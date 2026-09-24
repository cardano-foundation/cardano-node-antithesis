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

cat >"$stub_bin/git" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf 'git %s\n' "$*" >>"${STUB_LOG:?}"
[ "${1:-}" = ls-remote ] || exit 64
if [ -n "${STUB_GIT_FAIL:-}" ]; then
  printf 'stub git remote unavailable\n' >&2
  exit 3
fi
cat "${STUB_LSREMOTE_FILE:?}"
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

chmod +x "$stub_bin/git" "$stub_bin/nix" "$stub_bin/docker"

stub_env() {
  printf 'STUB_LOG=%s\nPATH=%s:%s\n' "$stub_log" "$stub_bin" "$PATH"
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
