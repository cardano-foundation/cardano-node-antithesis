#!/usr/bin/env bash
set -euo pipefail

# -------------------------- CONFIG --------------------------
REPO_URL="https://github.com/cardano-foundation/cardano-node-antithesis.git"
CLONE_DIR="/tmp/cardano-node-antithesis-build"
REGISTRY="ghcr.io/cardano-foundation/cardano-node-antithesis"
TEST="testnets/cardano_node_master"
# -----------------------------------------------------------

# Track every entry we couldn't process (missing git tag, missing
# component directory, …). The job MUST fail at the end if any
# entry was skipped — silent skips previously left CI green while
# the antithesis testnet ran against stale registry images.
SKIPPED=()

# 1. Extract "name tag" pairs  (e.g. configurator 5967670)
mapfile -t ENTRIES < <(
  grep -oP 'ghcr\.io/cardano-foundation/cardano-node-antithesis/\K[^ ]+' "$TEST/docker-compose.yaml" |
  sed 's|.*cardano-node-antithesis/||; s|:| |' |
  sort -u
)

# 2. Clone / update repo once
if [[ ! -d "$CLONE_DIR/.git" ]]; then
  echo "Cloning repository..."
  git clone "$REPO_URL" "$CLONE_DIR"
else
  echo "Updating existing clone..."
  git -C "$CLONE_DIR" fetch --tags --prune --quiet
  git -C "$CLONE_DIR" reset --hard --quiet
  git -C "$CLONE_DIR" clean -fdx --quiet
fi

# 3. Process **each** entry independently
for entry in "${ENTRIES[@]}"; do
  NAME="${entry%% *}"
  TAG="${entry#* }"
  BUILD_DIR="$CLONE_DIR/components/$NAME"

  echo
  echo "=================================================="
  echo "Processing: $NAME  (tag: $TAG)"
  echo "=================================================="

  # ---- Already-published fast path ----
  # If the registry already serves $NAME:$TAG, we don't need to
  # rebuild — the testnet will pull what's there. This handles
  # legacy components whose original git commit has since been
  # rebased away: the image is still served, we just can't build
  # it again. No need to resolve $TAG against the repo at all.
  if docker manifest inspect "$REGISTRY/$NAME:$TAG" >/dev/null 2>&1; then
    echo "→ $NAME:$TAG already in registry. Skipping rebuild."
    continue
  fi

  # ---- Need to build → must resolve tag to a commit ----
  COMMIT=$(git -C "$CLONE_DIR" rev-list -n 1 "$TAG" 2>/dev/null || true)
  if [[ -z "$COMMIT" ]]; then
    echo "Error: Tag '$TAG' is NOT in the registry AND cannot be" >&2
    echo "       resolved against this repo — nothing to build" >&2
    echo "       and nothing to publish." >&2
    SKIPPED+=("$NAME:$TAG (not in registry, tag not resolvable)")
    continue
  fi
  echo "→ Tag $TAG resolves to commit ${COMMIT:0:8}"

  # ---- Skip if commit-tagged image is also already published ----
  if docker manifest inspect "$REGISTRY/$NAME:$COMMIT" >/dev/null 2>&1; then
    echo "→ $NAME:$COMMIT already in registry. Re-tagging as $NAME:$TAG."
    docker pull "$REGISTRY/$NAME:$COMMIT"
    docker tag "$REGISTRY/$NAME:$COMMIT" "$REGISTRY/$NAME:$TAG"
    docker push "$REGISTRY/$NAME:$TAG"
    continue
  fi

  # ---- Checkout exact commit (detached HEAD) ----
  git -C "$CLONE_DIR" checkout "$COMMIT" --quiet

  # ---- Verify component directory exists ----
  if [[ ! -d "$BUILD_DIR" ]]; then
    echo "Warning: Directory $BUILD_DIR does not exist." >&2
    SKIPPED+=("$NAME:$TAG (component directory missing at commit ${COMMIT:0:8})")
    continue
  fi

  cd "$BUILD_DIR"
  echo "Current directory: $(pwd)"
  ls -la

  # try to use nix build .#docker-image if possible or fallback to docker build
  if nix eval --raw ".#docker-image" >/dev/null 2>&1; then
      echo "Building $NAME using from $CLONE_DIR using nix ..."
        nix build ".#docker-image" --out-link "/tmp/${NAME}-docker-image" --print-out-paths
        docker load -i "/tmp/${NAME}-docker-image"
        version=$(nix eval --raw ".#version")
        docker tag "$REGISTRY/$NAME:$version" "$REGISTRY/$NAME:$TAG"
        docker tag "$REGISTRY/$NAME:$version" "$REGISTRY/$NAME:$COMMIT"
    else
    # ---- Build image ----
    echo "Building $NAME from $BUILD_DIR using docker ..."
    docker build \
        --pull \
        -t "$REGISTRY/$NAME:$TAG" \
        -t "$REGISTRY/$NAME:$COMMIT" \
        .
  fi
  # ---- Push both tags ----
  echo "Pushing $NAME ..."
  docker push "$REGISTRY/$NAME:$TAG"
  docker push "$REGISTRY/$NAME:$COMMIT"

  echo "Done: $NAME"
done

echo
echo "All components processed."
echo "Images available at: $REGISTRY/<name>:<tag> and $REGISTRY/<name>:<commit>"

if (( ${#SKIPPED[@]} > 0 )); then
  echo
  echo "FAIL: ${#SKIPPED[@]} entr(y/ies) were skipped — the testnet docker-compose.yaml" >&2
  echo "       references images that this script could not build/push:" >&2
  for entry in "${SKIPPED[@]}"; do
    echo "         - $entry" >&2
  done
  echo >&2
  echo "Fix the offending image: tag in testnets/cardano_node_master/docker-compose.yaml" >&2
  echo "to a value that resolves via 'git rev-list -n 1 <tag>' (a real git tag, branch, or" >&2
  echo "commit SHA living in this repo) and that has the corresponding components/<name>/" >&2
  echo "build context at that commit." >&2
  exit 1
fi