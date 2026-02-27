#!/usr/bin/env bash
set -euo pipefail

# Build and push all component images from the current checkout.
# Images are tagged with both the short and full commit hash.
# Updates docker-compose.yaml to reference the new short hash tags.

REGISTRY="ghcr.io/cardano-foundation/cardano-node-antithesis"
COMPOSE="testnets/cardano_node_master/docker-compose.yaml"
COMPONENTS=(configurator sidecar tracer-sidecar adversary)
SHORT_SHA=$(git rev-parse --short HEAD)
FULL_SHA=$(git rev-parse HEAD)

echo "Building all components at ${SHORT_SHA}..."

for NAME in "${COMPONENTS[@]}"; do
  BUILD_DIR="components/$NAME"

  echo
  echo "=================================================="
  echo "Processing: $NAME (${SHORT_SHA})"
  echo "=================================================="

  if [[ ! -d "$BUILD_DIR" ]]; then
    echo "Warning: $BUILD_DIR not found, skipping."
    continue
  fi

  (
    cd "$BUILD_DIR"

    if nix eval --raw ".#docker-image" >/dev/null 2>&1; then
      echo "Building $NAME with nix..."
      nix build ".#docker-image" --out-link "/tmp/${NAME}-docker-image" --print-out-paths
      docker load -i "/tmp/${NAME}-docker-image"
      version=$(nix eval --raw ".#version")
      docker tag "$REGISTRY/$NAME:$version" "$REGISTRY/$NAME:$SHORT_SHA"
      docker tag "$REGISTRY/$NAME:$version" "$REGISTRY/$NAME:$FULL_SHA"
    else
      echo "Building $NAME with docker..."
      docker build --pull \
        -t "$REGISTRY/$NAME:$SHORT_SHA" \
        -t "$REGISTRY/$NAME:$FULL_SHA" \
        .
    fi

    echo "Pushing $NAME..."
    docker push "$REGISTRY/$NAME:$SHORT_SHA"
    docker push "$REGISTRY/$NAME:$FULL_SHA"
    echo "Done: $NAME"
  )
done

# Update docker-compose.yaml with new image tags
echo
echo "Updating $COMPOSE..."
for NAME in "${COMPONENTS[@]}"; do
  sed -i "s|\(${REGISTRY}/${NAME}\):[^ ]*|\1:${SHORT_SHA}|g" "$COMPOSE"
done

echo
echo "All components built and pushed."
echo "Images: $REGISTRY/<name>:$SHORT_SHA"
