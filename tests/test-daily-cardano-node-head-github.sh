#!/usr/bin/env bash
# Hermetic proof for the real Cardano Node HEAD candidate transport
# (scripts/daily-cardano-node-head-github.sh), issue #215.
#
# No Docker daemon, no Nix build, no registry, no network: every external
# command the transport invokes (`git`, `nix`, `docker`) is a recording stub
# whose outputs come from this suite's own scenario constants. Expected values
# are constructed here, never derived from the transport's arguments.
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
transport="$repo_root/scripts/daily-cardano-node-head-github.sh"
fake_transport="$repo_root/tests/fixtures/daily-cardano-node-head/fake-transport.sh"
controller="$repo_root/scripts/daily-cardano-node-head.sh"

tmp_root=$(mktemp -d)
trap 'rm -rf "$tmp_root"' EXIT

upstream_origin=https://github.com/IntersectMBO/cardano-node.git
upstream_ref=refs/heads/master
upstream_sha=1e2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a
digest_hex=4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c
registry_digest="sha256:${digest_hex}"
image_repository=registry.example/cardano-node-head
candidate_ref="$image_repository:$upstream_sha@$registry_digest"
upstream_flake=github:IntersectMBO/cardano-node
loaded_image="ghcr.io/intersectmbo/cardano-node:${upstream_sha:0:7}"
expected_node_services=(p1 p2 p3 p4 relay1 relay2 relay3)

fail() {
  printf 'FAIL: %s\n' "$*" >&2
  exit 1
}

pass() {
  printf 'PASS %s\n' "$1"
}

assert_file_contains() {
  local file=$1
  local literal=$2
  grep -Fqx -- "$literal" "$file" ||
    fail "$file does not contain exact line: $literal"
}

assert_log_contains() {
  assert_file_contains "$stub_log" "$1"
}

assert_stderr_token() {
  grep -Fq -- "$1" "$case_stderr" ||
    fail "scenario=$case_name stderr lacks token: $1"
}

# ---------------------------------------------------------------------------
# Recording stubs. Every stub appends its full argument vector to $stub_log
# and answers from scenario files this suite writes.
# ---------------------------------------------------------------------------
stub_bin=$tmp_root/bin
stub_log=$tmp_root/stub.log
mkdir -p "$stub_bin"
: >"$stub_log"

real_git=$(command -v git)
cat >"$stub_bin/git" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf 'git %s\n' "$*" >>"${STUB_LOG:?}"
stub_lock_dir() {
  printf '%s\n' "${STUB_LOCK_DIR:-${STUB_LOG%/*}/locks}"
}
# Rows for refs created by an atomic push, derived from the marker dirs.
stub_lock_rows() {
  local marker ref
  for marker in "$(stub_lock_dir)"/*/; do
    [ -f "${marker}sha" ] || continue
    ref=${marker%/}
    ref=${ref##*/}
    ref=${ref//_/\/}
    printf '%s\t%s\n' "$(cat "${marker%/}/sha")" "$ref"
  done
  return 0
}
# Atomic create-once push: the first push of a ref wins the marker
# directory; every later push of the same ref is rejected, whatever value
# it carries. The remote state file and the porcelain line record the
# creation.
stub_handle_push() {
  if [ -n "${STUB_CLAIM_PUSH_FAIL:-}" ]; then
    printf 'stub push rejected\n' >&2
    exit 1
  fi
  refspec=${*: -1}
  pushed_sha=${refspec%%:*}
  pushed_ref=${refspec#*:}
  [ "${pushed_ref}" != "$refspec" ] || return 0
  lock_dir=$(stub_lock_dir)
  mkdir -p "$lock_dir"
  marker="$lock_dir/${pushed_ref//\//_}"
  if ! mkdir "$marker" 2>/dev/null; then
    printf 'stub push rejected (ref exists): %s\n' "$pushed_ref" >&2
    exit 1
  fi
  printf '%s\n' "$pushed_sha" >"$marker/sha"
  printf '%s\t%s\n' "$pushed_sha" "$pushed_ref" >>"${STUB_LSREMOTE_FILE:?}"
  printf '%s\n' "$pushed_sha" >>"$(stub_remote_shas)"
  printf '*\t%s:%s\t[new tag]\n' "$pushed_sha" "$pushed_ref"
}
# The remote's fetchable commit universe: everything the stub clone ever
# fabricated, plus everything pushed through it.
stub_remote_shas() {
  printf '%s\n' "$(stub_lock_dir)/remote-shas"
}
case "${1:-}" in
  ls-remote)
    if [ -n "${STUB_GIT_FAIL:-}" ]; then
      printf 'stub git remote unavailable\n' >&2
      exit 3
    fi
    cat "${STUB_LSREMOTE_FILE:?}"
    stub_lock_rows
    ;;
  push)
    stub_handle_push "$@"
    ;;
  fetch)
    # Fetchable = recorded in the stub remote's commit universe.
    requested=${*: -1}
    if ! grep -Fqx "$requested" "$(stub_remote_shas)" 2>/dev/null; then
      printf 'stub fetch rejected: %s\n' "$requested" >&2
      exit 1
    fi
    ;;
  clone)
    # Fabricate the cloned repository: fixed-date commits on main, no
    # network. STUB_CLONE_COMMITS=2 fabricates a second commit so the
    # default branch tip differs from the first (a moved main). Every
    # fabricated SHA is recorded as remotely fetchable.
    target=${!#}
    "${STUB_REAL_GIT:?}" init --quiet --initial-branch=main "$target"
    "${STUB_REAL_GIT}" -C "$target" config user.name stub
    "${STUB_REAL_GIT}" -C "$target" config user.email stub@example.invalid
    printf 'placeholder main\n' >"$target/README.stub"
    "${STUB_REAL_GIT}" -C "$target" add README.stub
    GIT_AUTHOR_DATE='2026-01-01T00:00:00Z' GIT_COMMITTER_DATE='2026-01-01T00:00:00Z' \
      "${STUB_REAL_GIT}" -C "$target" commit --quiet -m 'stub main'
    if [ "${STUB_CLONE_COMMITS:-1}" -ge 2 ]; then
      printf 'moved main\n' >>"$target/README.stub"
      GIT_AUTHOR_DATE='2026-01-02T00:00:00Z' GIT_COMMITTER_DATE='2026-01-02T00:00:00Z' \
        "${STUB_REAL_GIT}" -C "$target" commit --quiet -am 'stub main moved'
    fi
    mkdir -p "$(stub_lock_dir)"
    "${STUB_REAL_GIT}" -C "$target" rev-list HEAD | while IFS= read -r sha; do
      printf '%s\n' "$sha" >>"$(stub_remote_shas)"
    done
    ;;
  -C)
    case " $* " in
      *' push '*)
        stub_handle_push "$@"
        ;;
      *' fetch '*)
        requested=${*: -1}
        if ! grep -Fqx "$requested" "$(stub_remote_shas)" 2>/dev/null; then
          printf 'stub fetch rejected: %s\n' "$requested" >&2
          exit 1
        fi
        ;;
      *' ls-remote '*)
        cat "${STUB_LSREMOTE_FILE:?}"
        stub_lock_rows
        ;;
      *)
        exec "${STUB_REAL_GIT:?}" "$@"
        ;;
    esac
    ;;
  *)
    exec "${STUB_REAL_GIT:?}" "$@"
    ;;
esac
STUB

cat >"$stub_bin/nix" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf 'nix %s\n' "$*" >>"${STUB_LOG:?}"
[ "${1:-}" = build ] || exit 64
printf '%s\n' "${STUB_NIX_OUT:?}"
STUB

cat >"$stub_bin/docker" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf 'docker %s\n' "$*" >>"${STUB_LOG:?}"
case "${1:-}" in
  load)
    printf 'Loaded image: %s\n' "${STUB_LOADED_IMAGE:?}"
    ;;
  tag | push) ;;
  buildx)
    [ "$2 $3" = 'imagetools inspect' ] || exit 64
    printf '"%s"\n' "${STUB_REGISTRY_DIGEST:?}"
    ;;
  run)
    cat "${STUB_RUN_OUTPUT:?}"
    ;;
  compose)
    last=''
    for arg in "$@"; do
      if [ "${last:-}" = -f ]; then model=$arg; fi
      last=$arg
    done
    [ -n "${model:-}" ] || exit 64
    for arg in "$@"; do
      if [ "$arg" = --quiet ]; then exit 0; fi
    done
    cat "${STUB_COMPOSE_CONFIG:?}"
    ;;
  *) exit 64 ;;
esac
STUB

cat >"$stub_bin/gh" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf 'gh %s\n' "$*" >>"${STUB_LOG:?}"
case "${1:-}" in
  workflow)
    # gh workflow run <file> -R <repo> --ref <ref> -f k=v ...
    [ "${2:-}" = run ] || exit 64
    ;;
  run)
    case "${2:-}" in
      list)
        [ -z "${STUB_RUN_LIST_EMPTY:-}" ] || exit 0
        cat "${STUB_RUN_LIST_FILE:?}"
        ;;
      watch)
        if [ -n "${STUB_WATCH_FAIL:-}" ]; then
          printf 'stub watch failed\n' >&2
          exit 1
        fi
        ;;
      download)
        if [ -n "${STUB_DOWNLOAD_FAIL:-}" ]; then
          printf 'stub download failed\n' >&2
          exit 1
        fi
        target=''
        last=''
        for arg in "$@"; do
          if [ "$last" = -D ]; then target=$arg; fi
          last=$arg
        done
        [ -n "$target" ] || exit 64
        mkdir -p "$target"
        cp "${STUB_CORRELATION_FILE:?}" "$target/moog-correlation"
        ;;
      *) exit 64 ;;
    esac
    ;;
  auth)
    # credential helper probe: succeed with no credential material.
    ;;
  *) exit 64 ;;
esac
STUB

chmod +x "$stub_bin/git" "$stub_bin/nix" "$stub_bin/docker" "$stub_bin/gh"

stub_env() {
  printf 'STUB_LOG=%s\nSTUB_REAL_GIT=%s\nPATH=%s:%s\n' \
    "$stub_log" "$real_git" "$stub_bin" "$PATH"
}

# ---------------------------------------------------------------------------
# Scenario fixtures.
# ---------------------------------------------------------------------------
scenario_root=$tmp_root/scenarios
mkdir -p "$scenario_root"

make_nix_output() {
  local name=$1
  local out=$scenario_root/$name
  mkdir -p "$out"
  printf 'not a real tarball\n' >"$out/cardano-node-image.tar"
  printf '%s\n' "$out"
}

make_source_model() {
  local name=$1
  local dir=$scenario_root/$name
  mkdir -p "$dir"
  cat >"$dir/docker-compose.yaml" <<'YAML'
x-cardano-node: &cardano-node
  image: ghcr.io/intersectmbo/cardano-node@sha256:1111111111111111111111111111111111111111111111111111111111111111
  command: >
    run --config /configs/configs/config.json
  restart: always

services:
  tracer:
    image: ghcr.io/intersectmbo/cardano-tracer@sha256:2222222222222222222222222222222222222222222222222222222222222222
  p1:
    <<: *cardano-node
    volumes:
      - ./testnet.yaml:/testnet.yaml
  p2:
    <<: *cardano-node
    image: ghcr.io/intersectmbo/cardano-node@sha256:3333333333333333333333333333333333333333333333333333333333333333
  p3:
    <<: *cardano-node
    image: ghcr.io/intersectmbo/cardano-node@sha256:4444444444444444444444444444444444444444444444444444444444444444
  p4:
    <<: *cardano-node
    image: ghcr.io/intersectmbo/cardano-node@sha256:5555555555555555555555555555555555555555555555555555555555555555
  relay1:
    <<: *cardano-node
    image: ghcr.io/intersectmbo/cardano-node@sha256:3333333333333333333333333333333333333333333333333333333333333333
  relay2:
    <<: *cardano-node
    image: ghcr.io/intersectmbo/cardano-node@sha256:4444444444444444444444444444444444444444444444444444444444444444
  relay3:
    <<: *cardano-node
    image: ghcr.io/intersectmbo/cardano-node@sha256:5555555555555555555555555555555555555555555555555555555555555555
  sidecar:
    image: ghcr.io/cardano-foundation/cardano-node-antithesis/sidecar:1ff6913
YAML
  printf 'poolCount: 4\n' >"$dir/testnet.yaml"
  printf 'tracer config\n' >"$dir/tracer-config.yaml"
  printf '{}\n' >"$dir/relay-topology.json"
  printf 'model readme\n' >"$dir/README.md"
  printf '%s\n' "$dir/docker-compose.yaml"
}

make_compose_config() {
  # Stand-in for `docker compose -f <model> config` output: services at two
  # spaces, `image:` at four. Expected images come from this suite only.
  local name=$1
  local node_image=$2
  local file=$scenario_root/$name
  {
    printf 'name: rendered\nservices:\n'
    printf '  %s:\n    image: %s\n' p1 "$node_image"
    printf '  %s:\n    image: %s\n' p2 "$node_image"
    printf '  %s:\n    image: %s\n' p3 "$node_image"
    printf '  %s:\n    image: %s\n' p4 "$node_image"
    printf '  %s:\n    image: %s\n' relay1 "$node_image"
    printf '  %s:\n    image: %s\n' relay2 "$node_image"
    printf '  %s:\n    image: %s\n' relay3 "$node_image"
    printf '  sidecar:\n    image: %s\n' \
      'ghcr.io/cardano-foundation/cardano-node-antithesis/sidecar:1ff6913'
  } >"$file"
  printf '%s\n' "$file"
}

make_version_output() {
  local file=$scenario_root/$1
  {
    printf 'cardano-node 10.2.1 - linux-x86_64 - ghc-9.6\n'
    printf 'git revision %s\n' "$upstream_sha"
  } >"$file"
  printf '%s\n' "$file"
}

# ---------------------------------------------------------------------------
# Case runner.
# ---------------------------------------------------------------------------
case_number=0
run_transport() {
  case_name=$1
  shift
  case_number=$((case_number + 1))
  case_dir=$tmp_root/$case_number-$case_name
  mkdir -p "$case_dir/state"
  case_state=$case_dir/state
  case_receipt=$case_dir/receipt
  case_stderr=$case_dir/stderr
  case_stdout=$case_dir/stdout
  : >"$case_receipt"
  case_rc=0
  env -i \
    PATH="$stub_bin:$PATH" \
    STUB_LOG="$stub_log" \
    HEAD_CANDIDATE_STATE_DIR="$case_state" \
    HEAD_CANDIDATE_RECEIPT="$case_receipt" \
    "$@" >"$case_stdout" 2>"$case_stderr" || case_rc=$?
}

require_success() {
  [ "$case_rc" -eq 0 ] ||
    fail "scenario=$case_name expected success, exit=$case_rc: $(tr '\n' ' ' <"$case_stderr")"
}

require_failure() {
  [ "$case_rc" -ne 0 ] || fail "scenario=$case_name expected failure"
}

assert_stdout_line() {
  grep -Fqx -- "$1" "$case_stdout" ||
    fail "scenario=$case_name stdout lacks exact line: $1"
}

assert_stdout_empty() {
  [ ! -s "$case_stdout" ] ||
    fail "scenario=$case_name stdout unexpectedly non-empty: $(cat "$case_stdout")"
}

if [ ! -f "$fake_transport" ]; then
  fail "fake transport is absent: $fake_transport"
fi

if [ ! -x "$controller" ]; then
  fail "controller behavior absent: expected executable $controller"
fi

if [ ! -x "$transport" ]; then
  fail "real transport behavior absent: expected executable $transport"
fi

# ---------------------------------------------------------------------------
# resolve-upstream
# ---------------------------------------------------------------------------
lsremote_file=$scenario_root/ls-remote-one
printf '%s\t%s\n' "$upstream_sha" "$upstream_ref" >"$lsremote_file"

run_transport resolve-upstream-one env \
  STUB_LSREMOTE_FILE="$lsremote_file" \
  "$transport" resolve-upstream "$upstream_origin" "$upstream_ref"
require_success
assert_stdout_line "$upstream_origin|$upstream_ref|$upstream_sha"
assert_log_contains "git ls-remote $upstream_origin $upstream_ref"
pass resolve-upstream-relays-remote-row

lsremote_empty=$scenario_root/ls-remote-empty
: >"$lsremote_empty"
run_transport resolve-upstream-zero env \
  STUB_LSREMOTE_FILE="$lsremote_empty" \
  "$transport" resolve-upstream "$upstream_origin" "$upstream_ref"
require_success
assert_stdout_empty
pass resolve-upstream-relays-zero-rows

run_transport resolve-upstream-failure env \
  STUB_LSREMOTE_FILE="$lsremote_file" \
  STUB_GIT_FAIL=1 \
  "$transport" resolve-upstream "$upstream_origin" "$upstream_ref"
require_failure
assert_stderr_token 'stub git remote unavailable'
pass resolve-upstream-command-failure-propagates

# ---------------------------------------------------------------------------
# publish-candidate
# ---------------------------------------------------------------------------
nix_out=$(make_nix_output nix-out)

run_transport publish-ok env \
  STUB_NIX_OUT="$nix_out" \
  STUB_LOADED_IMAGE="$loaded_image" \
  STUB_REGISTRY_DIGEST="$registry_digest" \
  "$transport" publish-candidate "$upstream_sha" "$image_repository"
require_success
assert_stdout_line "$candidate_ref"
assert_log_contains "nix build --no-link --print-out-paths ${upstream_flake}/${upstream_sha}#dockerImage/node"
assert_log_contains "docker load -i $nix_out/cardano-node-image.tar"
assert_log_contains "docker tag $loaded_image $image_repository:$upstream_sha"
assert_log_contains "docker push $image_repository:$upstream_sha"
assert_log_contains \
  "docker buildx imagetools inspect $image_repository:$upstream_sha --format {{json .Manifest.Digest}}"
pass publish-candidate-exact-rev-build-tag-push-digest

run_transport publish-digest-malformed env \
  STUB_NIX_OUT="$nix_out" \
  STUB_LOADED_IMAGE="$loaded_image" \
  STUB_REGISTRY_DIGEST=not-a-digest \
  "$transport" publish-candidate "$upstream_sha" "$image_repository"
require_failure
assert_stderr_token 'registry digest read-back is malformed'
pass publish-candidate-rejects-malformed-digest

nix_out_empty=$scenario_root/nix-out-empty
mkdir -p "$nix_out_empty"
run_transport publish-no-tarball env \
  STUB_NIX_OUT="$nix_out_empty" \
  STUB_LOADED_IMAGE="$loaded_image" \
  STUB_REGISTRY_DIGEST="$registry_digest" \
  "$transport" publish-candidate "$upstream_sha" "$image_repository"
require_failure
assert_stderr_token 'expected exactly one image tarball'
pass publish-candidate-rejects-tarball-less-output

# ---------------------------------------------------------------------------
# prove-revision
# ---------------------------------------------------------------------------
version_output=$(make_version_output version-ok)

run_transport prove-revision-ok env \
  STUB_RUN_OUTPUT="$version_output" \
  "$transport" prove-revision "$candidate_ref"
require_success
assert_stdout_line "$upstream_sha"
assert_log_contains \
  "docker run --rm --entrypoint cardano-node $candidate_ref --version"
pass prove-revision-runs-containerized-binary

# Upstream's current output says `git rev`; both spellings are accepted.
version_rev_form=$scenario_root/version-rev-form
{
  printf 'cardano-node 11.1.1 - linux-x86_64 - ghc-9.6\n'
  printf 'git rev %s\n' "$upstream_sha"
} >"$version_rev_form"
run_transport prove-revision-rev-form env \
  STUB_RUN_OUTPUT="$version_rev_form" \
  "$transport" prove-revision "$candidate_ref"
require_success
assert_stdout_line "$upstream_sha"
pass prove-revision-accepts-git-rev-form

version_bare=$scenario_root/version-bare
printf 'cardano-node 10.2.1 - linux-x86_64 - ghc-9.6\n' >"$version_bare"
run_transport prove-revision-unparsable env \
  STUB_RUN_OUTPUT="$version_bare" \
  "$transport" prove-revision "$candidate_ref"
require_failure
assert_stderr_token 'reported no git revision'
pass prove-revision-rejects-revisionless-output

# ---------------------------------------------------------------------------
# render-topology
# ---------------------------------------------------------------------------
source_model=$(make_source_model source-model)

run_transport render-ok env \
  HEAD_CANDIDATE_SOURCE_MODEL="$source_model" \
  "$transport" render-topology "$candidate_ref"
require_success
rendered=$case_state/docker-compose.yaml
assert_stdout_line "$rendered"
[ -f "$rendered" ] || fail "rendered model was not written: $rendered"
node_lines=$(grep -Fc -- "image: $candidate_ref" "$rendered" || true)
[ "$node_lines" -eq 7 ] ||
  fail "rendered model expected 7 candidate image lines, found $node_lines"
if grep -E 'ghcr\.io/intersectmbo/cardano-node' "$rendered" >/dev/null; then
  fail 'rendered model still references an upstream node image'
fi
if ! grep -Fq -- \
  'image: ghcr.io/intersectmbo/cardano-tracer@sha256:2222222222222222222222222222222222222222222222222222222222222222' \
  "$rendered"; then
  fail 'rendered model lost the tracer image'
fi
for side_file in testnet.yaml tracer-config.yaml relay-topology.json README.md; do
  [ -f "$case_state/$side_file" ] ||
    fail "rendered state dir lacks side file: $side_file"
done
pass render-topology-renders-candidate-model

quoted_model_dir=$scenario_root/quoted-model
mkdir -p "$quoted_model_dir"
cat >"$quoted_model_dir/docker-compose.yaml" <<'YAML'
services:
  p1:
    image: "ghcr.io/intersectmbo/cardano-node@sha256:1111111111111111111111111111111111111111111111111111111111111111"
YAML
run_transport render-quoted-ref env \
  HEAD_CANDIDATE_SOURCE_MODEL="$quoted_model_dir/docker-compose.yaml" \
  "$transport" render-topology "$candidate_ref"
require_failure
assert_stderr_token 'still references an upstream node image'
pass render-topology-rejects-unmatchable-node-image

# ---------------------------------------------------------------------------
# describe-topology
# ---------------------------------------------------------------------------
compose_config=$(make_compose_config compose-config "$candidate_ref")

run_transport describe-ok env \
  STUB_COMPOSE_CONFIG="$compose_config" \
  "$transport" describe-topology "$rendered"
require_success
for service in "${expected_node_services[@]}" sidecar; do
  if [ "$service" = sidecar ]; then
    expected_image='ghcr.io/cardano-foundation/cardano-node-antithesis/sidecar:1ff6913'
  else
    expected_image=$candidate_ref
  fi
  assert_stdout_line "$service|$expected_image"
done
row_count=$(grep -Ec '^[^|]+\|[^|]+$' "$case_stdout" || true)
[ "$row_count" -eq 8 ] ||
  fail "describe-topology expected 8 rows, found $row_count"
assert_log_contains "docker compose -f $rendered config"
pass describe-topology-emits-compose-resolved-rows

run_transport describe-missing-model env \
  STUB_COMPOSE_CONFIG="$compose_config" \
  "$transport" describe-topology "$case_state/not-rendered.yaml"
require_failure
assert_stderr_token 'rendered model is absent'
pass describe-topology-requires-rendered-model

# ---------------------------------------------------------------------------
# validate-compose
# ---------------------------------------------------------------------------
run_transport validate-ok env \
  STUB_COMPOSE_CONFIG="$compose_config" \
  "$transport" validate-compose "$rendered"
require_success
assert_log_contains "docker compose -f $rendered config --quiet"
pass validate-compose-validates-rendered-model

run_transport validate-missing-model env \
  STUB_COMPOSE_CONFIG="$compose_config" \
  "$transport" validate-compose "$case_state/not-rendered.yaml"
require_failure
assert_stderr_token 'rendered model is absent'
pass validate-compose-requires-rendered-model

# ---------------------------------------------------------------------------
# fake-submit
# ---------------------------------------------------------------------------
run_transport fake-submit-ok env \
  HEAD_CANDIDATE_SOURCE_MODEL="$source_model" \
  "$transport" fake-submit "$rendered" "$candidate_ref" "$upstream_sha"
require_success
assert_stdout_line "fake://$upstream_sha"
pass fake-submit-witnesses-rendered-model

stale_upstream_ref='ghcr.io/intersectmbo/cardano-node@sha256:9999999999999999999999999999999999999999999999999999999999999999'

# Content is mutated at the SAME rendered path, so a transport that merely
# rejects unfamiliar paths cannot pass: the original path already passed the
# positive control above, and only its content changed.
sed "0,\#image: $candidate_ref#s##image: $stale_upstream_ref#" \
  "$rendered" >"$rendered.stale" && mv "$rendered.stale" "$rendered"
run_transport fake-submit-stale env \
  "$transport" fake-submit "$rendered" "$candidate_ref" "$upstream_sha"
require_failure
assert_stderr_token 'still references an upstream node image'
pass fake-submit-rejects-stale-upstream-reference

sed "s#image: $candidate_ref#image: ghcr.io/cardano-foundation/cardano-node-antithesis/sidecar:1ff6913#g" \
  "$rendered" >"$rendered.sidecar" && mv "$rendered.sidecar" "$rendered"
run_transport fake-submit-no-candidate env \
  "$transport" fake-submit "$rendered" "$candidate_ref" "$upstream_sha"
require_failure
assert_stderr_token 'does not carry the candidate image'
pass fake-submit-rejects-candidate-less-model

# ---------------------------------------------------------------------------
# receipt
# ---------------------------------------------------------------------------
receipt_fields=(
  schema=CandidateReceiptV1
  stage=validate-compose
  outcome=VALIDATED
  mode=manual
  "upstream_origin=$upstream_origin"
  "upstream_ref=$upstream_ref"
  "upstream_sha=$upstream_sha"
  "candidate_ref=$candidate_ref"
  "binary_revision=$upstream_sha"
  "rendered_model=$case_state/docker-compose.yaml"
  topology_services=7
  "topology_image=$candidate_ref"
)

run_transport receipt-nonterminal env \
  "$transport" receipt "${receipt_fields[@]}"
require_success
assert_file_contains "$case_receipt" 'outcome=VALIDATED'
record_count=$(grep -c '^schema=CandidateReceiptV1$' "$case_receipt" || true)
[ "$record_count" -eq 1 ] ||
  fail "expected exactly one persisted record, found $record_count"
pass receipt-persists-record-verbatim

terminal_fields=(
  schema=CandidateReceiptV1
  stage=submit-candidate
  outcome=PREPARED
  mode=manual
  "upstream_origin=$upstream_origin"
  "upstream_ref=$upstream_ref"
  "upstream_sha=$upstream_sha"
  "candidate_ref=$candidate_ref"
  "binary_revision=$upstream_sha"
  "rendered_model=$case_state/docker-compose.yaml"
  topology_services=7
  "topology_image=$candidate_ref"
  "submission=fake://$upstream_sha"
)

run_transport receipt-terminal env \
  "$transport" receipt "${terminal_fields[@]}"
require_success
assert_file_contains "$case_receipt" 'stage=submit-candidate'
assert_file_contains "$case_receipt" 'outcome=PREPARED'
assert_file_contains "$case_receipt" "upstream_sha=$upstream_sha"
assert_file_contains "$case_receipt" "candidate_ref=$candidate_ref"
assert_file_contains "$case_receipt" "binary_revision=$upstream_sha"
assert_file_contains "$case_receipt" "topology_image=$candidate_ref"
assert_file_contains "$case_receipt" "submission=fake://$upstream_sha"
record_count=$(grep -c '^schema=CandidateReceiptV1$' "$case_receipt" || true)
[ "$record_count" -eq 1 ] ||
  fail "terminal record persisted with extra records: $record_count"
if grep -Eq '^stage=complete$' "$case_receipt"; then
  fail 'receipt persistence invented a stage the controller never wrote'
fi
pass receipt-terminal-record-persists-verbatim

failed_fields=(
  schema=CandidateReceiptV1
  stage=submit-candidate
  outcome=FAILED
  'error=submission-failed'
  mode=manual
  "upstream_origin=$upstream_origin"
  "upstream_ref=$upstream_ref"
  "upstream_sha=$upstream_sha"
)

run_transport receipt-failed env \
  "$transport" receipt "${failed_fields[@]}"
require_success
assert_file_contains "$case_receipt" 'error=submission-failed'
assert_file_contains "$case_receipt" 'outcome=FAILED'
record_count=$(grep -c '^schema=CandidateReceiptV1$' "$case_receipt" || true)
[ "$record_count" -eq 1 ] ||
  fail "failed record persisted with extra records: $record_count"
pass receipt-failed-record-persists-verbatim

# ---------------------------------------------------------------------------
# Operation surface parity with the frozen fake transport.
# ---------------------------------------------------------------------------
transport_operations() {
  awk '
    /^case "\$operation" in$/ { in_op = 1; next }
    in_op && /^esac$/ { exit }
    in_op && /^  [a-z0-9-]+\)$/ {
      name = $1
      sub(/\)$/, "", name)
      print name
    }
  ' "$1" | sort
}

real_operations=$(transport_operations "$transport")
fake_operations=$(transport_operations "$fake_transport")
[ "$real_operations" = "$fake_operations" ] ||
  fail "real transport operation surface differs from the frozen fake surface
real:
$real_operations
fake:
$fake_operations"
pass operation-surface-parity

# ---------------------------------------------------------------------------
# Unknown operation.
# ---------------------------------------------------------------------------
run_transport unknown-operation env \
  "$transport" not-an-operation
require_failure
assert_stderr_token 'unknown transport operation: not-an-operation'
pass unknown-operation-rejected

# ---------------------------------------------------------------------------
# Full candidate path: the unchanged S1 controller driving the real transport
# through stubbed effectors.
# ---------------------------------------------------------------------------
path_dir=$tmp_root/full-path
mkdir -p "$path_dir/state"
path_state=$path_dir/state
path_receipt=$path_dir/receipt
path_stderr=$path_dir/stderr
path_stdout=$path_dir/stdout
: >"$path_receipt"

path_compose_config=$scenario_root/path-compose-config
cp "$compose_config" "$path_compose_config"

pushes_before=$(grep -c '^docker push ' "$stub_log" || true)

path_rc=0
env -i \
  PATH="$stub_bin:$PATH" \
  STUB_LOG="$stub_log" \
  STUB_LSREMOTE_FILE="$lsremote_file" \
  STUB_NIX_OUT="$nix_out" \
  STUB_LOADED_IMAGE="$loaded_image" \
  STUB_REGISTRY_DIGEST="$registry_digest" \
  STUB_RUN_OUTPUT="$version_output" \
  STUB_COMPOSE_CONFIG="$path_compose_config" \
  HEAD_CANDIDATE_SOURCE_MODEL="$source_model" \
  HEAD_CANDIDATE_TRANSPORT="$transport" \
  HEAD_CANDIDATE_MODE=manual \
  HEAD_CANDIDATE_STATE_DIR="$path_state" \
  HEAD_CANDIDATE_RECEIPT="$path_receipt" \
  HEAD_CANDIDATE_IMAGE_REPOSITORY="$image_repository" \
  "$controller" >"$path_stdout" 2>"$path_stderr" || path_rc=$?

[ "$path_rc" -eq 0 ] ||
  fail "full candidate path expected success, exit=$path_rc: $(tr '\n' ' ' <"$path_stderr")"
grep -Fqx -- "PREPARED $upstream_sha $candidate_ref fake://$upstream_sha" \
  "$path_stdout" ||
  fail "full candidate path stdout lacks PREPARED summary: $(cat "$path_stdout")"
assert_file_contains "$path_receipt" 'stage=submit-candidate'
assert_file_contains "$path_receipt" 'outcome=PREPARED'
assert_file_contains "$path_receipt" "candidate_ref=$candidate_ref"
assert_file_contains "$path_receipt" "binary_revision=$upstream_sha"
assert_file_contains "$path_receipt" 'topology_services=7'
receipt_records=$(grep -c '^schema=CandidateReceiptV1$' "$path_receipt" || true)
[ "$receipt_records" -eq 7 ] ||
  fail "full candidate path expected 7 receipt records, found $receipt_records"
if grep -Eq '^stage=complete$' "$path_receipt"; then
  fail 'receipt persistence invented a record the controller never wrote'
fi
submit_calls=$(( $(grep -c '^docker push ' "$stub_log" || true) - pushes_before ))
[ "$submit_calls" -eq 1 ] ||
  fail "full candidate path expected exactly one push, found $submit_calls"
pass full-path-controller-with-real-transport

# ---------------------------------------------------------------------------
# Daily operations (#216): consumer commit, day claim, dispatch, correlation.
# Each daily case drives the real transport through the recording stubs.
# ---------------------------------------------------------------------------
daily_day=2026-09-24
daily_claim_ref="refs/tags/daily-cardano-node-head/$daily_day"
daily_tag=daily-cardano-node-head/$daily_day
consumer_repository=cardano-foundation/cardano-node-antithesis
consumer_testnet=cardano_node_head

run_transport_in() {
  local state=$1
  local name=$2
  shift 2
  case_number=$((case_number + 1))
  case_dir=$tmp_root/$case_number-$name
  mkdir -p "$case_dir" "$state"
  case_state=$state
  case_receipt=$case_dir/receipt
  case_stderr=$case_dir/stderr
  case_stdout=$case_dir/stdout
  : >"$case_receipt"
  case_rc=0
  env -i \
    PATH="$stub_bin:$PATH" \
    STUB_LOG="$stub_log" \
    STUB_REAL_GIT="$real_git" \
    STUB_LOCK_DIR="$case_dir/locks" \
    HEAD_CANDIDATE_STATE_DIR="$case_state" \
    HEAD_CANDIDATE_RECEIPT="$case_receipt" \
    "$@" >"$case_stdout" 2>"$case_stderr" || case_rc=$?
}

# The deterministic fabricated main SHA the stub clone produces. The
# probe runs under the same env -i shape as the transport's own clone so
# both fabrications land on the identical commit.
fabricated_main_sha() {
  local dir=$tmp_root/fabricated-$RANDOM$RANDOM
  env -i \
    PATH="$stub_bin:$PATH" \
    STUB_LOG="$stub_log" \
    STUB_REAL_GIT="$real_git" \
    "$stub_bin/git" clone \
    "https://github.com/$consumer_repository.git" "$dir"
  "$real_git" -C "$dir" rev-parse HEAD
}

# Render + prepare a consumer commit inside one state directory; echoes the
# consumer commit SHA the transport produced.
seed_consumer_workspace() {
  local state=$1
  local name=$2
  local base=$3
  local run_mode=$4
  run_transport_in "$state" "$name-render" env \
    STUB_REAL_GIT="$real_git" \
    HEAD_CANDIDATE_SOURCE_MODEL="$source_model" \
    "$transport" render-topology "$candidate_ref"
  require_success
  run_transport_in "$state" "$name-prepare" env \
    STUB_REAL_GIT="$real_git" \
    HEAD_CANDIDATE_SOURCE_MODEL="$source_model" \
    "$transport" prepare-consumer "$daily_day" "$base" "$run_mode" \
    "$state/docker-compose.yaml" "$candidate_ref" "$consumer_testnet"
  require_success
  sed -n '1p' "$case_stdout"
}

# claim-day: creation-only push of the day tag.
prepare_state=$tmp_root/prepare-state
stub_main_sha=$(fabricated_main_sha)
consumer_commit=$(seed_consumer_workspace "$prepare_state" prepare-ok "$stub_main_sha" daily)
[[ "$consumer_commit" =~ ^[0-9a-f]{40}$ ]] ||
  fail "prepare-consumer emitted a non-SHA commit: $consumer_commit"

consumer_model=$prepare_state/consumer/testnets/$consumer_testnet/docker-compose.yaml
[ -f "$consumer_model" ] ||
  fail "prepare-consumer wrote no consumer model: $consumer_model"
grep -Fq -- "image: $candidate_ref" "$consumer_model" ||
  fail 'consumer model does not pin the candidate image'
for side_file in testnet.yaml tracer-config.yaml relay-topology.json README.md; do
  [ -f "$prepare_state/consumer/testnets/$consumer_testnet/$side_file" ] ||
    fail "consumer directory lacks side file: $side_file"
done
commit_count=$("$real_git" -C "$prepare_state/consumer" rev-list --count HEAD)
[ "$commit_count" -eq 2 ] ||
  fail "prepare-consumer expected one commit on top of main, found $commit_count"
parent_sha=$("$real_git" -C "$prepare_state/consumer" rev-parse HEAD^)
[ "$parent_sha" = "$stub_main_sha" ] ||
  fail "consumer commit is not pinned to the run's start SHA: parent=$parent_sha"
commit_subject=$("$real_git" -C "$prepare_state/consumer" log -1 --format=%s)
[ "$commit_subject" = "chore: pin the daily cardano-node HEAD topology for $daily_day" ] ||
  fail "consumer commit does not carry its mode and day: $commit_subject"
assert_log_contains \
  "git clone --quiet --filter=blob:none --no-checkout https://github.com/$consumer_repository.git $prepare_state/consumer"
if grep -Eq '^git -C [^ ]+ .* push ' "$stub_log"; then
  fail 'prepare-consumer pushed before the day was claimed'
fi
pass prepare-consumer-renders-immutable-commit

# Same day, same start SHA, different mode: the consumer commits differ by
# construction (the message carries the mode), so a validation consumer
# identity can never collide with a production one and production still
# submits.
validation_prepare_state=$tmp_root/validation-prepare-state
validation_commit=$(seed_consumer_workspace "$validation_prepare_state" \
  validation-prepare-ok "$stub_main_sha" validation)
[ "$validation_commit" != "$consumer_commit" ] ||
  fail 'validation and production consumer commits share a SHA'
validation_subject=$("$real_git" -C "$validation_prepare_state/consumer" log -1 --format=%s)
[ "$validation_subject" = "chore: pin the validation cardano-node HEAD topology for $daily_day" ] ||
  fail "validation consumer commit does not carry its mode: $validation_subject"
validation_parent=$("$real_git" -C "$validation_prepare_state/consumer" rev-parse HEAD^)
[ "$validation_parent" = "$stub_main_sha" ] ||
  fail 'validation consumer commit is not pinned to the same run base'
pass prepare-consumer-modes-distinct-by-construction

run_transport_in "$prepare_state" prepare-missing-model env \
  STUB_REAL_GIT="$real_git" \
  "$transport" prepare-consumer "$daily_day" "$stub_main_sha" daily \
  "$prepare_state/not-rendered.yaml" "$candidate_ref" "$consumer_testnet"
require_failure
assert_stderr_token 'rendered model is absent'
pass prepare-consumer-requires-rendered-model

stale_model=$scenario_root/stale-consumer-model
sed "s#image: $candidate_ref#image: $stale_upstream_ref#g" \
  "$prepare_state/docker-compose.yaml" >"$stale_model"
run_transport_in "$prepare_state" prepare-stale-model env \
  STUB_REAL_GIT="$real_git" \
  "$transport" prepare-consumer "$daily_day" "$stub_main_sha" daily \
  "$stale_model" "$candidate_ref" "$consumer_testnet"
require_failure
assert_stderr_token 'does not carry the candidate image'
pass prepare-consumer-rejects-stale-model

# Main moved between the run's start and consumer preparation: the consumer
# is still built on the exact run base — the moved tip is never pinned.
moved_state=$tmp_root/moved-state
# The two-commit fixture's tip differs from the run base by construction.
moved_probe=$tmp_root/moved-probe
env -i \
  PATH="$stub_bin:$PATH" \
  STUB_LOG="$stub_log" \
  STUB_REAL_GIT="$real_git" \
  STUB_CLONE_COMMITS=2 \
  "$stub_bin/git" clone \
  "https://github.com/$consumer_repository.git" "$moved_probe"
moved_tip=$("$real_git" -C "$moved_probe" rev-parse HEAD)
[ "$moved_tip" != "$stub_main_sha" ] ||
  fail 'the two-commit fixture did not move the default branch tip'
run_transport_in "$moved_state" prepare-moved-render env \
  STUB_REAL_GIT="$real_git" \
  HEAD_CANDIDATE_SOURCE_MODEL="$source_model" \
  "$transport" render-topology "$candidate_ref"
require_success
run_transport_in "$moved_state" prepare-moved-base env \
  STUB_REAL_GIT="$real_git" \
  STUB_CLONE_COMMITS=2 \
  HEAD_CANDIDATE_SOURCE_MODEL="$source_model" \
  "$transport" prepare-consumer "$daily_day" "$stub_main_sha" daily \
  "$moved_state/docker-compose.yaml" "$candidate_ref" "$consumer_testnet"
require_success
moved_parent=$("$real_git" -C "$moved_state/consumer" rev-parse HEAD^)
[ "$moved_parent" = "$stub_main_sha" ] ||
  fail "a moved main was pinned instead of the run base: parent=$moved_parent"
assert_log_contains "git -C $moved_state/consumer fetch --quiet --depth=1 origin $stub_main_sha"
assert_log_contains "git -C $moved_state/consumer checkout --quiet --detach $stub_main_sha"
pass prepare-consumer-builds-on-exact-run-base

# A run base the remote cannot fetch (history rewritten away) fails closed.
unfetchable_state=$tmp_root/unfetchable-state
run_transport_in "$unfetchable_state" prepare-unfetchable-render env \
  STUB_REAL_GIT="$real_git" \
  HEAD_CANDIDATE_SOURCE_MODEL="$source_model" \
  "$transport" render-topology "$candidate_ref"
require_success
run_transport_in "$unfetchable_state" prepare-unfetchable-base env \
  STUB_REAL_GIT="$real_git" \
  "$transport" prepare-consumer "$daily_day" 9999999999999999999999999999999999999999 daily \
  "$unfetchable_state/docker-compose.yaml" "$candidate_ref" "$consumer_testnet"
require_failure
assert_stderr_token 'run base is unfetchable'
[ ! -e "$unfetchable_state/consumer/testnets/$consumer_testnet" ] ||
  fail 'an unfetchable base still produced a consumer commit'
pass prepare-consumer-refuses-unfetchable-base

run_transport_in "$moved_state" prepare-bad-base env \
  STUB_REAL_GIT="$real_git" \
  "$transport" prepare-consumer "$daily_day" not-a-sha daily \
  "$moved_state/docker-compose.yaml" "$candidate_ref" "$consumer_testnet"
require_failure
assert_stderr_token 'invalid run base'
pass prepare-consumer-rejects-malformed-base

# claim-day: creation-only push of the day tag.
claim_ok_state=$tmp_root/claim-ok-state
claim_ok_fixture=$scenario_root/claim-ok-empty
: >"$claim_ok_fixture"
claim_commit=$(seed_consumer_workspace "$claim_ok_state" claim-ok "$stub_main_sha" daily)
run_transport_in "$claim_ok_state" claim-day-ok env \
  STUB_REAL_GIT="$real_git" \
  STUB_LSREMOTE_FILE="$claim_ok_fixture" \
  "$transport" claim-day "$daily_claim_ref" "$claim_commit"
require_success
assert_stdout_line 'CLAIMED'
assert_log_contains \
  "git -C $claim_ok_state/consumer -c credential.helper=!gh auth git-credential push --force-with-lease=$daily_claim_ref: origin $claim_commit:$daily_claim_ref"
if grep -Eq ' push (-f|--force)([[:space:]]|$)' "$stub_log"; then
  fail 'claim-day used a force push that can re-point the day ref'
fi
grep -Fq "$(printf '%s\t%s' "$claim_commit" "$daily_claim_ref")" "$claim_ok_fixture" ||
  fail 'claim-day did not confirm the created tag on the remote'
pass claim-day-creates-tag-once

claim_blocked_state=$tmp_root/claim-blocked-state
claim_blocked_fixture=$scenario_root/claim-blocked
: >"$claim_blocked_fixture"
blocked_commit=$(seed_consumer_workspace "$claim_blocked_state" claim-blocked "$stub_main_sha" daily)
printf '%s\t%s\n' "$blocked_commit" "$daily_claim_ref" >>"$claim_blocked_fixture"
blocked_marker_pushes=$(grep -Ec "push --force-with-lease=$daily_claim_ref:" "$stub_log" || true)
run_transport_in "$claim_blocked_state" claim-day-blocked env \
  STUB_REAL_GIT="$real_git" \
  STUB_LSREMOTE_FILE="$claim_blocked_fixture" \
  "$transport" claim-day "$daily_claim_ref" "$blocked_commit"
require_failure
assert_stdout_line 'BLOCKED day-already-claimed'
assert_stderr_token "day already claimed: $daily_tag"
blocked_pushes_before=$blocked_marker_pushes
blocked_pushes=$(grep -Ec "push --force-with-lease=$daily_claim_ref:" "$stub_log" || true)
[ "$blocked_pushes" -eq "$blocked_pushes_before" ] ||
  fail 'claim-day blocked case attempted a push'
pass claim-day-refuses-existing-tag

claim_fail_state=$tmp_root/claim-fail-state
claim_fail_fixture=$scenario_root/claim-fail-empty
: >"$claim_fail_fixture"
fail_commit=$(seed_consumer_workspace "$claim_fail_state" claim-fail "$stub_main_sha" daily)
case ${fail_commit:0:1} in
  0) wrong_commit="1${fail_commit:1}" ;;
  *) wrong_commit="0${fail_commit:1}" ;;
esac
run_transport_in "$claim_fail_state" claim-day-push-failure env \
  STUB_REAL_GIT="$real_git" \
  STUB_LSREMOTE_FILE="$claim_fail_fixture" \
  STUB_CLAIM_PUSH_FAIL=1 \
  "$transport" claim-day "$daily_claim_ref" "$fail_commit"
require_failure
assert_stdout_empty
assert_stderr_token 'claim push failed'
pass claim-day-push-failure-fails-closed

run_transport_in "$claim_fail_state" claim-day-bad-ref env \
  STUB_REAL_GIT="$real_git" \
  STUB_LSREMOTE_FILE="$claim_fail_fixture" \
  "$transport" claim-day refs/heads/daily-cardano-node-head "$fail_commit"
require_failure
assert_stderr_token 'claim ref is not a day tag'
pass claim-day-rejects-non-day-ref

run_transport_in "$claim_fail_state" claim-day-wrong-commit env \
  STUB_REAL_GIT="$real_git" \
  STUB_LSREMOTE_FILE="$claim_fail_fixture" \
  "$transport" claim-day "$daily_claim_ref" "$wrong_commit"
require_failure
assert_stderr_token 'consumer workspace is not at the claimed commit'
pass claim-day-requires-workspace-at-commit

# submit-run: dispatch the existing MOOG workflow at the claim tag, marked
# with a unique correlation value, then select exactly the marked run.
daily_marker=daily-cardano-node-head-$daily_day-run-4242
run_list_fixture=$scenario_root/run-list
printf '424242|cardano_node_head [%s]\n' "$daily_marker" >"$run_list_fixture"

run_transport claim-submit-daily env \
  STUB_REAL_GIT="$real_git" \
  STUB_RUN_LIST_FILE="$run_list_fixture" \
  HEAD_CANDIDATE_CORRELATION="$daily_marker" \
  "$transport" submit-run "$consumer_commit" "$daily_claim_ref" \
  "$consumer_testnet" 3 false
require_success
assert_stdout_line \
  "https://github.com/$consumer_repository/actions/runs/424242"
assert_log_contains \
  "gh workflow run cardano-node.yaml -R $consumer_repository --ref $daily_tag -f test=$consumer_testnet -f duration=3 -f no-faults=false -f correlation=$daily_marker"
pass submit-run-dispatches-moog-workflow-at-tag

# A competing eligible run in the list is not selected: only a run whose
# title EQUALS 'cardano_node_head [<marker>]' is chosen — a superstring
# title and a same-marker title for a different testnet are both refused.
competing_run_list=$scenario_root/run-list-competing
{
  printf '111111|cardano_node_master\n'
  printf '999999|cardano_node_head [x%s]\n' "$daily_marker"
  printf '888888|cardano_node_master [%s]\n' "$daily_marker"
  printf '424243|cardano_node_head [%s]\n' "$daily_marker"
} >"$competing_run_list"
run_transport submit-run-selects-by-marker env \
  STUB_REAL_GIT="$real_git" \
  STUB_RUN_LIST_FILE="$competing_run_list" \
  HEAD_CANDIDATE_CORRELATION="$daily_marker" \
  "$transport" submit-run "$consumer_commit" "$daily_claim_ref" \
  "$consumer_testnet" 3 false
require_success
assert_stdout_line \
  "https://github.com/$consumer_repository/actions/runs/424243"
pass submit-run-selects-exactly-its-run

# A superstring title alone is not selected: exact equality or refusal.
superstring_run_list=$scenario_root/run-list-superstring
printf '999999|cardano_node_head [x%s]\n' "$daily_marker" >"$superstring_run_list"
run_transport submit-run-refuses-superstring env \
  STUB_REAL_GIT="$real_git" \
  STUB_RUN_LIST_FILE="$superstring_run_list" \
  HEAD_CANDIDATE_RUN_POLL_ATTEMPTS=2 \
  HEAD_CANDIDATE_RUN_POLL_SECONDS=0 \
  HEAD_CANDIDATE_CORRELATION="$daily_marker" \
  "$transport" submit-run "$consumer_commit" "$daily_claim_ref" \
  "$consumer_testnet" 3 false
require_failure
assert_stderr_token 'dispatched run was not identifiable'
pass submit-run-refuses-superstring-title

# Two runs carrying the marker: ambiguous, refuses.
ambiguous_run_list=$scenario_root/run-list-ambiguous
{
  printf '424242|cardano_node_head [%s]\n' "$daily_marker"
  printf '424243|cardano_node_head [%s]\n' "$daily_marker"
} >"$ambiguous_run_list"
run_transport submit-run-ambiguous env \
  STUB_REAL_GIT="$real_git" \
  STUB_RUN_LIST_FILE="$ambiguous_run_list" \
  HEAD_CANDIDATE_RUN_POLL_ATTEMPTS=2 \
  HEAD_CANDIDATE_RUN_POLL_SECONDS=0 \
  HEAD_CANDIDATE_CORRELATION="$daily_marker" \
  "$transport" submit-run "$consumer_commit" "$daily_claim_ref" \
  "$consumer_testnet" 3 false
require_failure
assert_stderr_token 'dispatched run selection is ambiguous'
pass submit-run-refuses-ambiguous-selection

# No run carrying the marker: not identifiable, refuses.
unmatched_run_list=$scenario_root/run-list-unmatched
printf '111111|cardano_node_master\n' >"$unmatched_run_list"
run_transport submit-run-unidentifiable env \
  STUB_REAL_GIT="$real_git" \
  STUB_RUN_LIST_FILE="$unmatched_run_list" \
  HEAD_CANDIDATE_RUN_POLL_ATTEMPTS=2 \
  HEAD_CANDIDATE_RUN_POLL_SECONDS=0 \
  HEAD_CANDIDATE_CORRELATION="$daily_marker" \
  "$transport" submit-run "$consumer_commit" "$daily_claim_ref" \
  "$consumer_testnet" 3 false
require_failure
assert_stderr_token 'dispatched run was not identifiable'
pass submit-run-refuses-unidentifiable-run

# A dispatch without a correlation marker never leaves the ground.
dispatches_before_absent=$(grep -Ec '^gh workflow run' "$stub_log" || true)
run_transport submit-run-correlation-absent env \
  STUB_REAL_GIT="$real_git" \
  STUB_RUN_LIST_FILE="$run_list_fixture" \
  "$transport" submit-run "$consumer_commit" "$daily_claim_ref" \
  "$consumer_testnet" 3 false
require_failure
assert_stderr_token 'correlation marker is absent or unusable'
dispatches_after_absent=$(grep -Ec '^gh workflow run' "$stub_log" || true)
[ "$dispatches_after_absent" -eq "$dispatches_before_absent" ] ||
  fail 'the marker-less dispatch reached the workflow'
pass submit-run-refuses-absent-correlation

validation_claim_ref="refs/tags/daily-cardano-node-head/validation/$daily_day"
validation_tag=daily-cardano-node-head/validation/$daily_day
run_transport claim-submit-validation env \
  STUB_REAL_GIT="$real_git" \
  STUB_RUN_LIST_FILE="$run_list_fixture" \
  HEAD_CANDIDATE_CORRELATION="$daily_marker" \
  "$transport" submit-run "$consumer_commit" "$validation_claim_ref" \
  "$consumer_testnet" 1 false
require_success
assert_log_contains \
  "gh workflow run cardano-node.yaml -R $consumer_repository --ref $validation_tag -f test=$consumer_testnet -f duration=1 -f no-faults=false -f correlation=$daily_marker"
pass submit-run-dispatches-validation-at-own-tag

run_transport submit-duration-rejected env \
  STUB_REAL_GIT="$real_git" \
  STUB_RUN_LIST_FILE="$run_list_fixture" \
  HEAD_CANDIDATE_CORRELATION="$daily_marker" \
  "$transport" submit-run "$consumer_commit" "$daily_claim_ref" \
  "$consumer_testnet" 5 false
require_failure
assert_stderr_token 'duration is outside the frozen contract'
pass submit-run-rejects-foreign-duration

run_transport submit-faults-rejected env \
  STUB_REAL_GIT="$real_git" \
  STUB_RUN_LIST_FILE="$run_list_fixture" \
  HEAD_CANDIDATE_CORRELATION="$daily_marker" \
  "$transport" submit-run "$consumer_commit" "$daily_claim_ref" \
  "$consumer_testnet" 3 true
require_failure
assert_stderr_token 'faults must stay enabled'
pass submit-run-rejects-disabled-faults

run_transport submit-testnet-rejected env \
  STUB_REAL_GIT="$real_git" \
  STUB_RUN_LIST_FILE="$run_list_fixture" \
  HEAD_CANDIDATE_CORRELATION="$daily_marker" \
  "$transport" submit-run "$consumer_commit" "$daily_claim_ref" \
  cardano_node_master 3 false
require_failure
assert_stderr_token 'submit target is not the HEAD testnet'
pass submit-run-rejects-foreign-testnet

# await-run: read the MOOG correlation the dispatched run exposed.
correlation_fixture=$scenario_root/moog-correlation
{
  printf 'test_run_id=ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\n'
  printf 'phase=finished\n'
  printf 'outcome=success\n'
  printf 'report_url=https://amaru-cardano.antithesis.com/report/stub\n'
} >"$correlation_fixture"

run_transport await-reads-correlation env \
  STUB_REAL_GIT="$real_git" \
  STUB_CORRELATION_FILE="$correlation_fixture" \
  "$transport" await-run "$consumer_commit" \
  "https://github.com/$consumer_repository/actions/runs/424242"
require_success
assert_stdout_line \
  'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff|https://amaru-cardano.antithesis.com/report/stub|success|finished'
assert_log_contains "gh run watch 424242 -R $consumer_repository"
assert_log_contains \
  "gh run download 424242 -R $consumer_repository -n moog-correlation -D $case_dir/state/correlation"
pass await-run-reads-moog-correlation

run_transport await-watch-tolerated env \
  STUB_REAL_GIT="$real_git" \
  STUB_CORRELATION_FILE="$correlation_fixture" \
  STUB_WATCH_FAIL=1 \
  "$transport" await-run "$consumer_commit" \
  "https://github.com/$consumer_repository/actions/runs/424242"
require_success
assert_stdout_line \
  'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff|https://amaru-cardano.antithesis.com/report/stub|success|finished'
pass await-run-tolerates-watch-noise

run_transport await-artifact-absent env \
  STUB_REAL_GIT="$real_git" \
  STUB_CORRELATION_FILE="$correlation_fixture" \
  STUB_DOWNLOAD_FAIL=1 \
  "$transport" await-run "$consumer_commit" \
  "https://github.com/$consumer_repository/actions/runs/424242"
require_failure
assert_stderr_token 'correlation artifact is absent'
pass await-run-fails-without-correlation

correlation_incomplete=$scenario_root/moog-correlation-incomplete
{
  printf 'test_run_id=ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\n'
  printf 'phase=accepted\n'
} >"$correlation_incomplete"
run_transport await-record-incomplete env \
  STUB_REAL_GIT="$real_git" \
  STUB_CORRELATION_FILE="$correlation_incomplete" \
  "$transport" await-run "$consumer_commit" \
  "https://github.com/$consumer_repository/actions/runs/424242"
require_failure
assert_stderr_token 'correlation record lacks'
pass await-run-rejects-incomplete-record

# Workflow wiring: schedule, recovery, validation, correlation exposure.
# ---------------------------------------------------------------------------
daily_workflow=.github/workflows/daily-cardano-node-head.yaml
moog_workflow=.github/workflows/cardano-node.yaml

grep -Eq '^[[:space:]]+- cron:[[:space:]]+.[0-9]+ [0-9]+ \* \* \*.' "$daily_workflow" ||
  fail 'daily workflow lacks a once-per-UTC-day schedule'
grep -q 'workflow_dispatch' "$daily_workflow" ||
  fail 'daily workflow lacks a manual dispatch entrypoint'
pass workflow-schedule-once-per-utc-day

grep -Eq "^[[:space:]]+production:" "$daily_workflow" ||
  fail 'daily workflow lacks the production recovery input'
grep -Eq "^[[:space:]]+validation:" "$daily_workflow" ||
  fail 'daily workflow lacks the validation input'
grep -Eq "github.event_name == 'schedule'|\(github.event_name == 'workflow_dispatch' && inputs.production\)" "$daily_workflow" ||
  fail 'daily workflow does not route the schedule to the production job'
pass workflow-manual-recovery-and-validation-inputs

grep -Eq "github.event_name == 'workflow_dispatch' && inputs.validation" "$daily_workflow" ||
  fail 'validation job is not dispatched through its own input'
grep -Fq '!inputs.production && !inputs.validation' "$daily_workflow" ||
  fail 'the #215 manual candidate path lost its dispatch routing'
pass workflow-manual-candidate-path-preserved

grep -q 'name: moog-correlation' "$moog_workflow" ||
  fail 'cardano-node.yaml does not expose the moog-correlation artifact'
grep -Eq 'if: \$\{\{ always\(\) \}\}' "$moog_workflow" ||
  fail 'cardano-node.yaml correlation step is not always reached'
grep -q 'steps.request.outputs.id' "$moog_workflow" ||
  fail 'cardano-node.yaml correlation step does not bind the moog test id'
pass moog-workflow-exposes-correlation

# The run title carries the correlation input only when it is set; the
# fallback keeps the ordinary name for schedule and matrix runs.
grep -Fq "inputs.correlation != '' &&" "$moog_workflow" ||
  fail 'the run name does not test the correlation input'
grep -Fq "'Antithesis on cardano-node testnet'" "$moog_workflow" ||
  fail 'the run name fallback is not the ordinary workflow name'
grep -Eq '^[[:space:]]+correlation:' "$moog_workflow" ||
  fail 'cardano-node.yaml lacks the correlation input'
pass moog-workflow-run-name-correlation

# ---------------------------------------------------------------------------
# MOOG step guards: execute the real Submit test step text hermetically.
# ---------------------------------------------------------------------------
command -v jq >/dev/null 2>&1 ||
  fail 'jq is required to execute the extracted MOOG step'

moog_stub_bin=$tmp_root/moog-bin
moog_stub_log=$tmp_root/moog.log
mkdir -p "$moog_stub_bin"
: >"$moog_stub_log"
cat >"$moog_stub_bin/moog" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf 'moog %s\n' "$*" >>"${MOOG_STUB_LOG:?}"
case "$1 $2" in
  'facts test-runs')
    if [ -n "${MOOG_STUB_CENSUS_FAIL:-}" ]; then
      printf 'stub census unreadable\n' >&2
      exit 1
    fi
    cat "${MOOG_STUB_CENSUS:?}"
    ;;
  'requester create-test')
    printf '{"value":{"testRunId":"stub-test-run-id"},"txHash":"stub-tx"}\n'
    ;;
  *) exit 64 ;;
esac
STUB
chmod +x "$moog_stub_bin/moog"

# The step's run block, verbatim from the workflow, with the Actions
# expression substituted by a fixture commit (the only interpolation the
# step performs).
submit_step_script=$tmp_root/submit-test-step.sh
awk '
  /^    - name: Submit test$/ { in_step = 1; next }
  in_step && /^    - name: / { exit }
  in_step && /^      run: \|$/ { in_run = 1; next }
  in_run {
    if ($0 ~ /^        /) {
      line = $0
      sub(/^        /, "", line)
      print line
      next
    }
    if ($0 ~ /^[[:space:]]*$/) {
      print ""
      next
    }
    exit
  }
' "$moog_workflow" \
  | sed -e "s/\${{ github\.sha }}/$consumer_commit/" -e "s/\${{ inputs\.no-faults }}/false/" \
  >"$submit_step_script"
grep -Fq 'moog requester create-test' "$submit_step_script" ||
  fail 'the extracted Submit test step lost its MOOG request'
grep -Fq 'daily-head-rerun-refused' "$submit_step_script" ||
  fail 'the Submit test step lost its rerun guard'

run_submit_step() {
  local testnet=$1
  local attempt=$2
  local out=$3
  local census=$4
  local moog_log=${5:-$moog_stub_log}
  : >"$moog_log"
  : >"$census.lsremote"
  env -i \
    PATH="$moog_stub_bin:$stub_bin:$PATH" \
    STUB_LOG="$stub_log" \
    STUB_REAL_GIT="$real_git" \
    STUB_LSREMOTE_FILE="$census.lsremote" \
    MOOG_STUB_LOG="$moog_log" \
    MOOG_STUB_CENSUS="$census" \
    MOOG_REQUESTER=stub-requester \
    MOOG_PLATFORM=github \
    GITHUB_REPOSITORY="$consumer_repository" \
    GITHUB_OUTPUT="$out" \
    DURATION=3 \
    TESTNET="$testnet" \
    RUN_ATTEMPT="$attempt" \
    bash "$submit_step_script"
}

empty_census=$tmp_root/census-empty.json
printf '[]\n' >"$empty_census"
occupied_census=$tmp_root/census-occupied.json
{
  printf '[{"key":{"type":"test-run","commitId":"%s","directory":"testnets/cardano_node_head","platform":"github","repository":{"organization":"cardano-foundation","repo":"cardano-node-antithesis"},"requester":"stub-requester"}}]\n' "$consumer_commit"
} >"$occupied_census"
matrix_occupied_census=$tmp_root/census-matrix-occupied.json
{
  printf '[{"key":{"type":"test-run","commitId":"%s","directory":"testnets/cardano_node_master","platform":"github","repository":{"organization":"cardano-foundation","repo":"cardano-node-antithesis"},"requester":"stub-requester"}},{"key":{"type":"test-run","commitId":"%s","directory":"testnets/cardano_node_master","platform":"github","repository":{"organization":"cardano-foundation","repo":"cardano-node-antithesis"},"requester":"stub-requester"}}]\n' "$consumer_commit" "$consumer_commit"
} >"$matrix_occupied_census"


# A MOOG call log that never materialized is a broken control, not a
# passing one: every zero-call assertion requires the log to exist.
assert_no_create_test() {
  local log=$1
  [ -f "$log" ] ||
    fail "MOOG call log is missing (control broken): $log"
  [ "$(grep -Ec '^moog requester create-test' "$log" || true)" -eq 0 ] ||
    fail "a refused dispatch constructed a MOOG request: $log"
}

# A non-first workflow attempt is refused before anything else.
rerun_rc=0
run_submit_step cardano_node_head 2 "$tmp_root/rerun-output" "$empty_census" \
  >"$tmp_root/rerun-stdout" 2>"$tmp_root/rerun-stderr" || rerun_rc=$?
[ "$rerun_rc" -ne 0 ] ||
  fail 'a re-run of the daily HEAD test was not refused'
grep -Fq 'daily-head-rerun-refused: testnet=cardano_node_head attempt=2' \
  "$tmp_root/rerun-stderr" ||
  fail 'the rerun refusal lacks its stable reason token'
assert_no_create_test "$moog_stub_log"
pass moog-step-refuses-daily-head-rerun

# Matrix testnets are untouched by every daily HEAD guard.
matrix_touches_before=$(grep -Ec '^git (push|ls-remote)' "$stub_log" || true)
matrix_rc=0
run_submit_step cardano_node_master 2 "$tmp_root/matrix-output" "$empty_census" \
  >"$tmp_root/matrix-stdout" 2>"$tmp_root/matrix-stderr" || matrix_rc=$?
[ "$matrix_rc" -eq 0 ] ||
  fail "a matrix testnet re-run was refused: $(cat "$tmp_root/matrix-stderr")"
grep -Fq 'requester create-test' "$moog_stub_log" ||
  fail 'the matrix re-run did not construct its MOOG request'
grep -Fq 'id=stub-test-run-id' "$tmp_root/matrix-output" ||
  fail 'the matrix re-run did not export the test id'
matrix_touches_after=$(grep -Ec '^git (push|ls-remote)' "$stub_log" || true)
[ "$matrix_touches_after" -eq "$matrix_touches_before" ] ||
  fail 'a matrix dispatch touched the day-claim boundary'
pass moog-step-matrix-rerun-unchanged

# First fresh dispatch: empty census, submits.
first_rc=0
run_submit_step cardano_node_head 1 "$tmp_root/first-output" "$empty_census" \
  >"$tmp_root/first-stdout" 2>"$tmp_root/first-stderr" || first_rc=$?
[ "$first_rc" -eq 0 ] ||
  fail "the first fresh daily HEAD dispatch was refused: $(cat "$tmp_root/first-stderr")"
grep -Fq 'requester create-test' "$moog_stub_log" ||
  fail 'the first fresh daily HEAD dispatch did not construct its request'
grep -Fq 'id=stub-test-run-id' "$tmp_root/first-output" ||
  fail 'the first fresh daily HEAD dispatch did not export the test id'
pass moog-step-first-fresh-dispatch-submits

# A second sequential dispatch at the same consumer commit: the read-only
# MOOG census finds an existing test-run and refuses before create-test.
census_rc=0
run_submit_step cardano_node_head 1 "$tmp_root/census-output" "$occupied_census" \
  >"$tmp_root/census-stdout" 2>"$tmp_root/census-stderr" || census_rc=$?
[ "$census_rc" -ne 0 ] ||
  fail 'a census-occupied fresh dispatch was not refused'
grep -Fq 'daily-head-already-submitted: testnet=cardano_node_head existing=1' \
  "$tmp_root/census-stderr" ||
  fail 'the census refusal lacks its stable reason token'
assert_no_create_test "$moog_stub_log"
pass moog-step-second-sequential-dispatch-refused

# An unreadable census refuses the daily HEAD testnet before create-test.
: >"$empty_census.lsremote"
: >"$tmp_root/census-fail.moglog"
census_fail_rc=0
env -i \
  PATH="$moog_stub_bin:$stub_bin:$PATH" \
  STUB_LOG="$stub_log" \
  STUB_REAL_GIT="$real_git" \
  STUB_LSREMOTE_FILE="$empty_census.lsremote" \
  STUB_LOCK_DIR="$tmp_root/lock-census-fail" \
  MOOG_STUB_LOG="$tmp_root/census-fail.moglog" \
  MOOG_STUB_CENSUS_FAIL=1 \
  MOOG_STUB_CENSUS="$empty_census" \
  MOOG_REQUESTER=stub-requester \
  MOOG_PLATFORM=github \
  GITHUB_REPOSITORY="$consumer_repository" \
  GITHUB_OUTPUT="$tmp_root/census-fail-output" \
  DURATION=3 \
  TESTNET=cardano_node_head \
  RUN_ATTEMPT=1 \
  bash "$submit_step_script" >"$tmp_root/census-fail-stdout" 2>"$tmp_root/census-fail-stderr" ||
  census_fail_rc=$?
[ "$census_fail_rc" -ne 0 ] ||
  fail 'an unreadable census did not refuse the daily HEAD dispatch'
grep -Fq 'daily-head-census-unreadable: testnet=cardano_node_head' \
  "$tmp_root/census-fail-stderr" ||
  fail 'the unreadable-census refusal lacks its stable reason token'
assert_no_create_test "$tmp_root/census-fail.moglog"
pass moog-step-unreadable-census-refuses

# Matrix testnets keep today's behaviour: an occupied census only feeds the
# try counter, never a refusal.
matrix_occupied_rc=0
run_submit_step cardano_node_master 1 "$tmp_root/matrix-occupied-output" \
  "$matrix_occupied_census" \
  >"$tmp_root/matrix-occupied-stdout" 2>"$tmp_root/matrix-occupied-stderr" ||
  matrix_occupied_rc=$?
[ "$matrix_occupied_rc" -eq 0 ] ||
  fail "a matrix dispatch with occupied census was refused: $(cat "$tmp_root/matrix-occupied-stderr")"
grep -Fq 'TRY=3 for testnets/cardano_node_master' "$tmp_root/matrix-occupied-stdout" ||
  fail 'the occupied matrix census did not advance the try counter'
grep -Fq 'requester create-test' "$moog_stub_log" ||
  fail 'the matrix dispatch with occupied census did not construct its request'
pass moog-step-matrix-census-unchanged

# The widened G4 shape: no secret expression in any run text of the MOOG
# workflow either, including the wallet step.
if awk '/^[[:space:]]*run:/{r=1} /^[[:space:]]*(- name|uses|with|env):/{r=0} r' \
  "$moog_workflow" | grep -E '\$\{\{[[:space:]]*(secrets\.|github\.token)'; then
  fail 'cardano-node.yaml carries a secret expression inside run text'
fi
pass moog-workflow-run-text-secret-free
