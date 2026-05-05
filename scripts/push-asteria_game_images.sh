#!/usr/bin/env bash
# Sibling of push-cardano_node_master_images.sh — same logic,
# different testnet directory. Kept as a separate script so the
# canonical scheduled-test pipeline (cardano_node_master) is
# untouched while asteria iteration is in-flight.
#
# Resolves every `<tag>` in testnets/asteria_game/docker-compose.yaml
# under ghcr.io/cardano-foundation/cardano-node-antithesis/<name> to a
# commit SHA, builds components/<name>/ at that commit via
# `nix build .#docker-image`, and pushes both `:<tag>` and
# `:<commit>` to GHCR.
set -euo pipefail

# -------------------------- CONFIG --------------------------
REPO_URL="https://github.com/cardano-foundation/cardano-node-antithesis.git"
CLONE_DIR="/tmp/cardano-node-antithesis-build"
REGISTRY="ghcr.io/cardano-foundation/cardano-node-antithesis"
TEST="testnets/asteria_game"
# -----------------------------------------------------------

mapfile -t ENTRIES < <(
  grep -oP 'ghcr\.io/cardano-foundation/cardano-node-antithesis/\K[^ ]+' "$TEST/docker-compose.yaml" |
  sed 's|@sha256:[a-f0-9]\+||; s|:| |' |
  sort -u
)

if [[ ! -d "$CLONE_DIR/.git" ]]; then
  echo "Cloning repository..."
  git clone "$REPO_URL" "$CLONE_DIR"
else
  echo "Updating existing clone..."
  git -C "$CLONE_DIR" fetch --tags --prune --quiet
  git -C "$CLONE_DIR" reset --hard --quiet
  git -C "$CLONE_DIR" clean -fdx --quiet
fi

for entry in "${ENTRIES[@]}"; do
  if [[ "$entry" != *" "* ]]; then
    echo
    echo "=================================================="
    echo "Skipping: $entry  (digest-only pin, no tag to resolve)"
    echo "=================================================="
    continue
  fi
  NAME="${entry%% *}"
  TAG="${entry#* }"
  BUILD_DIR="$CLONE_DIR/components/$NAME"

  echo
  echo "=================================================="
  echo "Processing: $NAME  (tag: $TAG)"
  echo "=================================================="

  COMMIT=$(git -C "$CLONE_DIR" rev-list -n 1 "$TAG" 2>/dev/null || true)
  if [[ -z "$COMMIT" ]]; then
    echo "Error: Tag '$TAG' not found in repo. Skipping $NAME."
    continue
  fi
  echo "→ Tag $TAG resolves to commit ${COMMIT:0:8}"

  if docker manifest inspect "$REGISTRY/$NAME:$TAG" >/dev/null 2>&1 \
     && docker manifest inspect "$REGISTRY/$NAME:$COMMIT" >/dev/null 2>&1; then
    echo "→ $NAME:$TAG and $NAME:$COMMIT already in registry. Skipping."
    continue
  fi

  git -C "$CLONE_DIR" checkout "$COMMIT" --quiet

  if [[ ! -d "$BUILD_DIR" ]]; then
    echo "Warning: Directory $BUILD_DIR does not exist. Skipping $NAME."
    continue
  fi

  cd "$BUILD_DIR"
  echo "Current directory: $(pwd)"
  ls -la

  if nix eval --raw ".#docker-image" >/dev/null 2>&1; then
      echo "Building $NAME using from $CLONE_DIR using nix ..."
      nix build ".#docker-image" --out-link "/tmp/${NAME}-docker-image" --print-out-paths
      docker load -i "/tmp/${NAME}-docker-image"
      version=$(nix eval --raw ".#version")
      docker tag "$REGISTRY/$NAME:$version" "$REGISTRY/$NAME:$TAG"
      docker tag "$REGISTRY/$NAME:$version" "$REGISTRY/$NAME:$COMMIT"
  else
      echo "Building $NAME from $BUILD_DIR using docker ..."
      docker build \
          --pull \
          -t "$REGISTRY/$NAME:$TAG" \
          -t "$REGISTRY/$NAME:$COMMIT" \
          .
  fi

  echo "Pushing $NAME ..."
  docker push "$REGISTRY/$NAME:$TAG"
  docker push "$REGISTRY/$NAME:$COMMIT"

  echo "Done: $NAME"
done

echo
echo "All components processed."
echo "Images available at: $REGISTRY/<name>:<tag> and $REGISTRY/<name>:<commit>"
