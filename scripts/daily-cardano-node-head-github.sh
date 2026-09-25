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
consumer_repository=${HEAD_CANDIDATE_REPOSITORY:-${GITHUB_REPOSITORY:-cardano-foundation/cardano-node-antithesis}}

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

  prepare-consumer)
    # Local only: the consumer commit is created in the workspace and pushed
    # by nothing here. The day claim is the single ref-creating boundary.
    require_commands git cp find mkdir
    day=${1:?day is required}
    run_base=${2:?run base is required}
    run_mode=${3:?run mode is required}
    rendered_model=${4:?rendered model is required}
    candidate_ref=${5:?candidate ref is required}
    testnet=${6:?testnet is required}
    [[ "$run_base" =~ ^[0-9a-f]{40}$ ]] || die "invalid run base: $run_base"
    case "$run_mode" in
      daily | validation) ;;
      *) die "unknown run mode: $run_mode" ;;
    esac
    [ "$testnet" = cardano_node_head ] || die 'consumer directory is not the HEAD testnet'
    [ -f "$rendered_model" ] || die 'rendered model is absent'
    grep -Fq -- "image: $candidate_ref" "$rendered_model" ||
      die 'rendered model does not carry the candidate image'
    directory=$state_dir/consumer
    [ ! -e "$directory" ] || die "consumer workspace already exists: $directory"
    git clone --quiet --filter=blob:none --depth=1 \
      "https://github.com/${consumer_repository}.git" "$directory"
    # The consumer commit's parent is the exact commit this run started
    # from. If the default branch moved since the run began, fail closed
    # rather than pinning the moved main (I216-04).
    [ "$(git -C "$directory" rev-parse HEAD)" = "$run_base" ] ||
      die "start-sha-moved: the consumer base moved since the run started"
    target=$directory/testnets/$testnet
    mkdir -p "$target"
    cp "$rendered_model" "$target/docker-compose.yaml"
    # The same side-file set render-topology copied into the state directory
    # keeps the consumer model self-contained next to its compose file.
    find "$(dirname "$source_model")" -maxdepth 1 -type f \
      ! -name docker-compose.yaml -exec cp -t "$target" {} +
    git -C "$directory" add -- "testnets/$testnet"
    # The commit message carries the run mode and the UTC day, so a
    # validation consumer commit and a production consumer commit differ
    # by construction even on the same day from the same start SHA: they
    # can never share a SHA, and a validation lock can never occupy a
    # production submission.
    git -C "$directory" -c user.name='daily-cardano-node-head' \
      -c user.email='daily-cardano-node-head@users.noreply.github.com' \
      commit --quiet \
      -m "chore: pin the $run_mode cardano-node HEAD topology for $day"
    emit "$(git -C "$directory" rev-parse HEAD)"
    ;;

  claim-day)
    # One boundary: creation-only push of the day tag. An empty expected
    # value in --force-with-lease refuses any existing ref, so the tag is
    # created at most once and never re-pointed.
    require_commands git gh
    claim_ref=${1:?claim ref is required}
    consumer_sha=${2:?consumer SHA is required}
    [[ "$claim_ref" =~ ^refs/tags/daily-cardano-node-head(/validation)?/[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] ||
      die "claim ref is not a day tag: $claim_ref"
    [[ "$consumer_sha" =~ ^[0-9a-f]{40}$ ]] || die "invalid consumer SHA: $consumer_sha"
    directory=$state_dir/consumer
    [ -d "$directory/.git" ] || die 'consumer workspace is absent'
    [ "$(git -C "$directory" rev-parse HEAD)" = "$consumer_sha" ] ||
      die 'consumer workspace is not at the claimed commit'
    tag_name=${claim_ref#refs/tags/}
    if [ -n "$(git -C "$directory" ls-remote origin "$claim_ref")" ]; then
      emit 'BLOCKED day-already-claimed'
      die "day already claimed: $tag_name"
    fi
    if ! git -C "$directory" -c credential.helper='!gh auth git-credential' \
      push --force-with-lease="$claim_ref": origin "$consumer_sha:$claim_ref"; then
      # A concurrent claimant may have won the creation between the census
      # and the push; that refusal is the same day-already-claimed outcome.
      if [ -n "$(git -C "$directory" ls-remote origin "$claim_ref")" ]; then
        emit 'BLOCKED day-already-claimed'
        die "day already claimed: $tag_name"
      fi
      die 'claim push failed'
    fi
    confirmed=$(git -C "$directory" ls-remote origin "$claim_ref")
    [ "${confirmed%%$'\t'*}" = "$consumer_sha" ] ||
      die 'claim did not take effect'
    emit 'CLAIMED'
    ;;

  submit-run)
    # One boundary: dispatch the existing MOOG workflow at the immutable
    # claim tag, marked with a unique correlation value that the workflow
    # carries in its run title, then select exactly the run whose title
    # carries that marker. No MOOG client is embedded here (plan.md: reuse
    # cardano-node.yaml rather than a second client).
    require_commands gh sleep seq grep
    consumer_sha=${1:?consumer SHA is required}
    claim_ref=${2:?claim ref is required}
    testnet=${3:?testnet is required}
    duration=${4:?duration is required}
    no_faults=${5:?fault setting is required}
    [ "$testnet" = cardano_node_head ] || die 'submit target is not the HEAD testnet'
    case "$duration" in
      1 | 3) ;;
      *) die 'duration is outside the frozen contract' ;;
    esac
    [ "$no_faults" = false ] || die 'faults must stay enabled'
    correlation=${HEAD_CANDIDATE_CORRELATION:-${GITHUB_RUN_ID:-}}
    [[ "$correlation" =~ ^[A-Za-z0-9._-]+$ ]] ||
      die "correlation marker is absent or unusable: ${correlation:-empty}"
    tag_name=${claim_ref#refs/tags/}
    gh workflow run cardano-node.yaml -R "$consumer_repository" \
      --ref "$tag_name" -f "test=$testnet" -f "duration=$duration" \
      -f no-faults=false -f "correlation=$correlation"
    poll_attempts=${HEAD_CANDIDATE_RUN_POLL_ATTEMPTS:-30}
    poll_seconds=${HEAD_CANDIDATE_RUN_POLL_SECONDS:-2}
    matching=''
    for _ in $(seq 1 "$poll_attempts"); do
      rows=$(gh run list -R "$consumer_repository" \
        --workflow cardano-node.yaml --limit 30 \
        --json databaseId,displayTitle \
        --jq '.[] | (.databaseId | tostring) + "|" + .displayTitle')
      matching=$(grep -F "$correlation" <<<"$rows" || true)
      [ -n "$matching" ] && break
      sleep "$poll_seconds"
    done
    [ -n "$matching" ] || die 'dispatched run was not identifiable'
    match_count=$(grep -Ec . <<<"$matching")
    [ "$match_count" -eq 1 ] ||
      die "dispatched run selection is ambiguous: $match_count matches"
    run_id=${matching%%|*}
    [[ "$run_id" =~ ^[0-9]+$ ]] || die 'dispatched run id is malformed'
    emit "https://github.com/$consumer_repository/actions/runs/$run_id"
    ;;

  await-run)
    # One boundary: the correlation artifact the dispatched run exposed.
    require_commands gh awk rm mkdir
    consumer_sha=${1:?consumer SHA is required}
    run_url=${2:?run URL is required}
    [[ "$run_url" =~ ^https://github\.com/[^/[:space:]]+/[^/[:space:]]+/actions/runs/[0-9]+$ ]] ||
      die 'run URL is malformed'
    run_id=${run_url##*/}
    # Watch progress only; the artifact, not the conclusion, carries the
    # terminal state (a completed-but-failed test also ends this run red).
    gh run watch "$run_id" -R "$consumer_repository" ||
      printf 'run watch reported a non-zero exit; continuing to the artifact\n' >&2
    correlation_dir=$state_dir/correlation
    rm -rf "$correlation_dir"
    mkdir -p "$correlation_dir"
    gh run download "$run_id" -R "$consumer_repository" \
      -n moog-correlation -D "$correlation_dir" ||
      die 'correlation artifact is absent'
    record=$correlation_dir/moog-correlation
    [ -f "$record" ] || die 'correlation record is absent'
    correlation_field() {
      awk -F= -v key="$1" '$1 == key { sub(/^[^=]*=/, ""); print; exit }' "$record"
    }
    test_run_id=$(correlation_field test_run_id)
    phase=$(correlation_field phase)
    outcome=$(correlation_field outcome)
    report_url=$(correlation_field report_url)
    [ -n "$test_run_id" ] || die 'correlation record lacks test_run_id'
    [ -n "$phase" ] || die 'correlation record lacks phase'
    [ -n "$outcome" ] || die 'correlation record lacks outcome'
    [ -n "$report_url" ] || die 'correlation record lacks report_url'
    printf 'awaiting correlation for consumer %s\n' "$consumer_sha" >&2
    emit "$test_run_id|$report_url|$outcome|$phase"
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
