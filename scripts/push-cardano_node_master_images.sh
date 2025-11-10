#!/usr/bin/env bash
set -euo pipefail

# -------------------------- CONFIG --------------------------
REPO_URL="https://github.com/cardano-foundation/cardano-node-antithesis.git"
CLONE_DIR="/tmp/cardano-node-antithesis-build"
REGISTRY="ghcr.io/cardano-foundation/cardano-node-antithesis"
TEST="testnets/cardano_node_master"
# -----------------------------------------------------------

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

  # ---- Resolve tag → commit ----
  COMMIT=$(git -C "$CLONE_DIR" rev-list -n 1 "$TAG" 2>/dev/null || true)
  if [[ -z "$COMMIT" ]]; then
    echo "Error: Tag '$TAG' not found in repo. Skipping $NAME."
    continue
  fi
  echo "→ Tag $TAG resolves to commit ${COMMIT:0:8}"

  # ---- Checkout exact commit (detached HEAD) ----
  git -C "$CLONE_DIR" checkout "$COMMIT" --quiet

  # ---- Verify component directory exists ----
  if [[ ! -d "$BUILD_DIR" ]]; then
    echo "Warning: Directory $BUILD_DIR does not exist. Skipping $NAME."
    continue
  fi

  # ---- Build image ----
  echo "Building $NAME from $BUILD_DIR ..."
  docker build \
    --pull \
    -t "$REGISTRY/$NAME:$TAG" \
    -t "$REGISTRY/$NAME:$COMMIT" \
    "$BUILD_DIR"

  # ---- Push both tags ----
  echo "Pushing $NAME ..."
  docker push "$REGISTRY/$NAME:$TAG"
  docker push "$REGISTRY/$NAME:$COMMIT"

  echo "Done: $NAME"
done

echo
echo "All components processed."
echo "Images available at: $REGISTRY/<name>:<tag> and $REGISTRY/<name>:<commit>"