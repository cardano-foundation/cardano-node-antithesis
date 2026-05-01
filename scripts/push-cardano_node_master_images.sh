#!/usr/bin/env bash
set -euo pipefail

# -------------------------- CONFIG --------------------------
REGISTRY="${REGISTRY:-ghcr.io/cardano-foundation/cardano-node-antithesis}"
TESTNETS="${TESTNETS:-cardano_node_master cardano_amaru cardano_amaru_epoch3600}"
ALWAYS_BUILD_COMPONENTS="${ALWAYS_BUILD_COMPONENTS:-sidecar}"
ARTIFACT_COMPONENTS="${ARTIFACT_COMPONENTS:-sidecar}"
REPO_ROOT="$(git rev-parse --show-toplevel)"
WORKTREE_ROOT="${WORKTREE_ROOT:-/tmp/cardano-node-antithesis-image-builds}"
HEAD_COMMIT="$(git -C "$REPO_ROOT" rev-parse HEAD)"
HEAD_TAG="$(git -C "$REPO_ROOT" rev-parse --short=7 HEAD)"
# -----------------------------------------------------------

mkdir -p "$WORKTREE_ROOT"

resolve_commit() {
  local ref="$1"
  git -C "$REPO_ROOT" rev-parse --verify "${ref}^{commit}" 2>/dev/null || true
}

entries_from_compose() {
  local testnet compose_file
  for testnet in $TESTNETS; do
    compose_file="$REPO_ROOT/testnets/$testnet/docker-compose.yaml"
    [[ -f "$compose_file" ]] || continue
    grep -oP 'ghcr\.io/cardano-foundation/cardano-node-antithesis/\K[^ ]+' "$compose_file" |
      sed 's|@sha256:[a-f0-9]\+||; s|:| |'
  done
}

all_entries() {
  local component
  for component in $ALWAYS_BUILD_COMPONENTS; do
    printf '%s %s\n' "$component" "$HEAD_TAG"
  done
  entries_from_compose
}

prepare_worktree() {
  local component="$1" commit="$2" dir
  dir="$WORKTREE_ROOT/$component-$commit"
  if [[ ! -e "$dir/.git" ]]; then
    rm -rf "$dir"
    git -C "$REPO_ROOT" worktree add --detach "$dir" "$commit" --quiet
  fi
  printf '%s' "$dir"
}

artifact_enabled_for() {
  local component="$1" artifact_component
  [[ -n "${IMAGE_ARTIFACT_DIR:-}" ]] || return 1
  for artifact_component in $ARTIFACT_COMPONENTS; do
    [[ "$artifact_component" == "$component" ]] && return 0
  done
  return 1
}

save_artifact() {
  local component="$1" tag="$2"
  artifact_enabled_for "$component" || return 0

  mkdir -p "$IMAGE_ARTIFACT_DIR"
  docker save "$REGISTRY/$component:$tag" \
    -o "$IMAGE_ARTIFACT_DIR/$component-$tag.tar"
}

build_component() {
  local name="$1" tag="$2" commit="$3"
  local checkout build_dir image_out version

  checkout="$(prepare_worktree "$name" "$commit")"
  build_dir="$checkout/components/$name"

  if [[ ! -d "$build_dir" ]]; then
    echo "Warning: Directory $build_dir does not exist. Skipping $name."
    return 0
  fi

  cd "$build_dir"
  echo "Current directory: $(pwd)"
  ls -la

  if nix flake show --json . 2>/dev/null | jq -e '.packages."x86_64-linux"."docker-image"' >/dev/null; then
    echo "Building $name from $commit using nix ..."
    image_out="/tmp/${name}-${tag}-docker-image"
    rm -f "$image_out"
    nix build ".#docker-image" --out-link "$image_out" --print-out-paths
    docker load -i "$image_out"
    version="$(nix eval --raw ".#version")"
    docker tag "$REGISTRY/$name:$version" "$REGISTRY/$name:$tag"
    docker tag "$REGISTRY/$name:$version" "$REGISTRY/$name:$commit"
  else
    echo "Building $name from $commit using docker ..."
    docker build \
      --pull \
      -t "$REGISTRY/$name:$tag" \
      -t "$REGISTRY/$name:$commit" \
      .
  fi
}

mapfile -t ENTRIES < <(all_entries | sort -u)

for entry in "${ENTRIES[@]}"; do
  # Compose entries pinned by digest-only (`name@sha256:digest`) leave no
  # tag side after sanitization. The image must already exist at that
  # digest — nothing to (re)build here.
  if [[ "$entry" != *" "* ]]; then
    echo
    echo "=================================================="
    echo "Skipping: $entry  (digest-only pin, no tag to resolve)"
    echo "=================================================="
    continue
  fi

  NAME="${entry%% *}"
  TAG="${entry#* }"
  COMMIT="$(resolve_commit "$TAG")"

  if [[ -z "$COMMIT" && "$TAG" == "$HEAD_TAG" ]]; then
    COMMIT="$HEAD_COMMIT"
  fi

  echo
  echo "=================================================="
  echo "Processing: $NAME  (tag: $TAG)"
  echo "=================================================="

  if [[ -z "$COMMIT" ]]; then
    echo "Error: tag/ref '$TAG' not found in current checkout. Skipping $NAME."
    continue
  fi
  echo "-> Tag $TAG resolves to commit ${COMMIT:0:8}"

  if docker manifest inspect "$REGISTRY/$NAME:$TAG" >/dev/null 2>&1 \
     && docker manifest inspect "$REGISTRY/$NAME:$COMMIT" >/dev/null 2>&1; then
    echo "-> $NAME:$TAG and $NAME:$COMMIT already in registry. Skipping."
    continue
  fi

  build_component "$NAME" "$TAG" "$COMMIT"

  echo "Pushing $NAME ..."
  docker push "$REGISTRY/$NAME:$TAG"
  docker push "$REGISTRY/$NAME:$COMMIT"
  save_artifact "$NAME" "$TAG"

  echo "Done: $NAME"
done

echo
echo "All components processed."
echo "Images available at: $REGISTRY/<name>:<tag> and $REGISTRY/<name>:<commit>"
