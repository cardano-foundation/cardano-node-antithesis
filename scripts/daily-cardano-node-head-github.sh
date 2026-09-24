#!/usr/bin/env bash
set -euo pipefail

# Real transport for the immutable Cardano Node master HEAD candidate (#215).
#
# One operation per invocation. scripts/daily-cardano-node-head.sh is the only
# caller and owns every policy decision; this transport observes and effects
# one boundary per operation and emits its observation on stdout. Diagnostics
# go to stderr so stdout carries operation values only.
#
# Boundaries crossed here (plan.md "Live boundaries"): the bare upstream
# remote, the exact-rev Nix dockerImage/node build, the image registry, the
# containerized cardano-node binary, and Compose's own model resolution.
#
# Hermetic proof: tests/test-daily-cardano-node-head-github.sh drives every
# operation through recording git/nix/docker stubs.

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
state_dir=${HEAD_CANDIDATE_STATE_DIR:?HEAD_CANDIDATE_STATE_DIR is required}
receipt_file=${HEAD_CANDIDATE_RECEIPT:?HEAD_CANDIDATE_RECEIPT is required}
source_model=${HEAD_CANDIDATE_SOURCE_MODEL:-$script_dir/../testnets/cardano_node_master/docker-compose.yaml}
upstream_flake=${HEAD_CANDIDATE_UPSTREAM_FLAKE:-github:IntersectMBO/cardano-node}

# The cardano_node_master network definition interpolates ${INTERNAL_NETWORK};
# an unset value is an invalid boolean for Compose, so every Compose
# invocation below defaults it and the caller may still override.
compose() {
  INTERNAL_NETWORK=${INTERNAL_NETWORK:-false} docker compose "$@"
}

die() {
  printf 'daily-cardano-node-head-github: %s\n' "$*" >&2
  exit 1
}

require_commands() {
  local command
  for command in "$@"; do
    command -v "$command" >/dev/null 2>&1 ||
      die "missing command: $command"
  done
}

# Every node image line the render must replace: the upstream repository with
# an optional tag, an optional digest, or both. Anchored so the
# cardano-foundation sidecars and the cardano-tracer image never match.
node_image_re='^([[:space:]]*image:[[:space:]]*)ghcr\.io/intersectmbo/cardano-node(:[^[:space:]@]+)?(@sha256:[0-9a-f]{64})?[[:space:]]*$'
# Detection is deliberately broader than substitution: any image line that
# still mentions the upstream node repository, in any quoting or tagging
# form, is stale after a render — a form the substitution does not recognize
# must fail closed, never render a mixed cluster.
stale_node_reference_re='^[[:space:]]*image:.*ghcr\.io/intersectmbo/cardano-node'

# Reserve the caller's stdout once, then make ordinary stdout diagnostic for
# the complete dispatch. Only emit can reach the operation-value channel.
exec {value_fd}>&1
exec 1>&2

emit() {
  printf '%s\n' "$@" >&"$value_fd"
}

operation=${1:?transport operation is required}
shift

case "$operation" in
  resolve-upstream)
    require_commands git
    origin=${1:?origin is required}
    ref=${2:?ref is required}
    # One row per matching remote head, relayed verbatim; the controller
    # enforces exactly-one and the origin/ref/SHA shape.
    git ls-remote "$origin" "$ref" |
      while IFS=$'\t' read -r sha observed_ref; do
        [ -n "${sha:-}" ] || continue
        emit "$origin|$observed_ref|$sha"
      done
    ;;

  publish-candidate)
    require_commands nix docker find sed head tr
    sha=${1:?upstream SHA is required}
    repository=${2:?repository is required}
    out=$(nix build --no-link --print-out-paths \
      "${upstream_flake}/${sha}#dockerImage/node")
    [ -n "$out" ] || die 'nix build produced no output path'
    image_source=$out
    if [ -d "$out" ]; then
      mapfile -t tarballs < <(find "$out" -maxdepth 1 -type f -name '*.tar*')
      [ "${#tarballs[@]}" -eq 1 ] ||
        die "expected exactly one image tarball under $out, found ${#tarballs[@]}"
      image_source=${tarballs[0]}
    fi
    load_output=$(docker load -i "$image_source")
    loaded=$(sed -nE 's/^Loaded image: (.+)$/\1/p' <<<"$load_output" | head -n 1)
    if [ -z "$loaded" ]; then
      loaded=$(sed -nE 's/^Loaded image ID: (sha256:[0-9a-f]{64})$/\1/p' \
        <<<"$load_output" | head -n 1)
    fi
    [ -n "$loaded" ] ||
      die "docker load reported no loadable image reference: $load_output"
    candidate_tag="$repository:$sha"
    docker tag "$loaded" "$candidate_tag"
    docker push "$candidate_tag"
    digest=$(docker buildx imagetools inspect "$candidate_tag" \
      --format '{{json .Manifest.Digest}}' | tr -d '"')
    [[ "$digest" =~ ^sha256:[0-9a-f]{64}$ ]] ||
      die "registry digest read-back is malformed: ${digest:-absent}"
    emit "$candidate_tag@$digest"
    ;;

  prove-revision)
    require_commands docker sed head
    candidate_ref=${1:?candidate ref is required}
    version_output=$(docker run --rm --entrypoint cardano-node \
      "$candidate_ref" --version)
    # Upstream has printed both `git revision <sha>` and `git rev <sha>`;
    # either form is the binary's own revision claim, nothing else is.
    revision=$(sed -nE 's/^git (revision|rev) ([0-9a-f]{40})$/\2/p' \
      <<<"$version_output" | head -n 1)
    [ -n "$revision" ] ||
      die "containerized cardano-node reported no git revision: $version_output"
    emit "$revision"
    ;;

  render-topology)
    require_commands sed find cp grep
    candidate_ref=${1:?candidate ref is required}
    [ -f "$source_model" ] || die "source model is absent: $source_model"
    rendered=$state_dir/docker-compose.yaml
    sed -E "s#${node_image_re}#\1${candidate_ref}#" \
      "$source_model" >"$rendered"
    # Fail closed if any node image line survived substitution.
    if grep -E "$stale_node_reference_re" "$rendered" |
      grep -Fv -- "image: $candidate_ref" | grep -q .; then
      die 'rendered model still references an upstream node image'
    fi
    # The model mounts side files relative to its own directory; the rendered
    # state dir must carry them so the model is self-contained.
    find "$(dirname "$source_model")" -maxdepth 1 -type f \
      ! -name docker-compose.yaml -exec cp -t "$state_dir" {} +
    emit "$rendered"
    ;;

  describe-topology)
    require_commands docker awk
    rendered_model=${1:?rendered model is required}
    [ -f "$rendered_model" ] || die "rendered model is absent: $rendered_model"
    # Compose-resolved rows only: the rendered file is never trusted as text.
    # Services sit at two spaces and their image at four in `config` output.
    config_output=$(compose -f "$rendered_model" config)
    [ -n "$config_output" ] || die 'compose resolution produced no model'
    while IFS= read -r row; do
      emit "$row"
    done < <(awk '
      /^  [A-Za-z0-9_.-]+:$/ {
        service = $0
        sub(/:$/, "", service)
        gsub(/^ +/, "", service)
        next
      }
      /^    image: / {
        image = $0
        sub(/^ *image: */, "", image)
        gsub(/^"|"$/, "", image)
        if (service != "") print service "|" image
        next
      }
    ' <<<"$config_output")
    ;;

  validate-compose)
    require_commands docker
    rendered_model=${1:?rendered model is required}
    [ -f "$rendered_model" ] || die "rendered model is absent: $rendered_model"
    compose -f "$rendered_model" config --quiet
    ;;

  fake-submit)
    require_commands grep
    rendered_model=${1:?rendered model is required}
    candidate_ref=${2:?candidate ref is required}
    sha=${3:?upstream SHA is required}
    # The rendered model is a witness, not an argument to trust: the fake
    # submission refuses a model that is not this candidate's model.
    [ -f "$rendered_model" ] || die "rendered model is absent: $rendered_model"
    grep -Fq "image: $candidate_ref" "$rendered_model" ||
      die 'rendered model does not carry the candidate image'
    if grep -E "$stale_node_reference_re" "$rendered_model" |
      grep -Fv -- "image: $candidate_ref" | grep -q .; then
      die 'rendered model still references an upstream node image'
    fi
    emit "fake://$sha"
    ;;

  receipt)
    # Receipt persistence is verbatim: the controller owns record content,
    # the transport owns durability. No record is ever invented here.
    mkdir -p "$(dirname "$receipt_file")"
    {
      printf '%s\n' "$@"
      printf '\n'
    } >>"$receipt_file"
    ;;

  *)
    die "unknown transport operation: $operation"
    ;;
esac
