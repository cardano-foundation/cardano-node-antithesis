#!/usr/bin/env bash
# Hermetic proof for the HEAD-candidate cluster command's teardown contract
# (scripts/head-candidate-cluster.sh, #215 AL-2): a failed teardown fails the
# command, PASS is reachable only after a strict teardown plus a zero-container
# census of the command's own project, and the success path records everything.
# Docker is a recording stub; no daemon is involved.
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
command_under_test="$repo_root/scripts/head-candidate-cluster.sh"

tmp_root=$(mktemp -d)
trap 'rm -rf "$tmp_root"' EXIT

candidate_sha=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
digest=sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
candidate_ref="registry.example/cardano-node-head:$candidate_sha@$digest"
candidate_image_id=sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

fail() {
  printf 'FAIL: %s\n' "$*" >&2
  exit 1
}

pass() {
  printf 'PASS %s\n' "$1"
}

# ---------------------------------------------------------------------------
# Fixtures: a rendered model with its side file, and the Compose resolution
# the stub reports for it.
# ---------------------------------------------------------------------------
model_dir=$tmp_root/model
mkdir -p "$model_dir"
{
  printf 'services:\n'
  for service in p1 p2 p3 p4 relay1 relay2 relay3; do
    printf '  %s:\n    image: %s\n' "$service" "$candidate_ref"
  done
  printf '  sidecar:\n'
  printf '    image: registry.example/sidecar:1\n'
} >"$model_dir/docker-compose.yaml"
printf 'poolCount: 4\n' >"$model_dir/testnet.yaml"

compose_config=$tmp_root/resolved.yaml
{
  printf 'name: rendered\nservices:\n'
  for service in p1 p2 p3 p4 relay1 relay2 relay3; do
    printf '  %s:\n    image: %s\n' "$service" "$candidate_ref"
  done
  printf '  sidecar:\n'
  printf '    image: registry.example/sidecar:1\n'
} >"$compose_config"

# ---------------------------------------------------------------------------
# Recording docker stub.
# ---------------------------------------------------------------------------
stub_bin=$tmp_root/bin
stub_state=$tmp_root/stub-state
stub_log=$tmp_root/stub.log
mkdir -p "$stub_bin" "$stub_state"
: >"$stub_log"

cat >"$stub_bin/docker" <<STUB
#!/usr/bin/env bash
set -euo pipefail
printf 'docker %s\n' "\$*" >>"\${STUB_LOG:?}"
if [ "\${1:-}" = compose ]; then
  sub=''
  svc=''
  prev=''
  verb=''
  for arg in "\$@"; do
    case "\$arg" in
      config | up | down | ps | exec | logs) sub=\$arg ;;
      ping | query) verb=\$arg ;;
    esac
    [ "\$prev" = -T ] && svc=\$arg
    prev=\$arg
  done
  case "\$sub" in
    config)
      for arg in "\$@"; do
        [ "\$arg" = --quiet ] && exit 0
      done
      cat "\${STUB_COMPOSE_CONFIG:?}"
      ;;
    up)
      : >"\${STUB_STATE:?}/up"
      ;;
    down)
      if [ -n "\${STUB_DOWN_FAILS:-}" ]; then
        printf 'stub docker down refused\n' >&2
        exit 1
      fi
      : >"\${STUB_STATE:?}/down"
      ;;
    ps)
      for arg in "\$@"; do
        if [ "\$arg" = -aq ]; then
          if [ -n "\${STUB_CENSUS_LEFTOVER:-}" ]; then
            printf 'leftovercontainer\n'
          fi
          exit 0
        fi
      done
      for arg in "\$@"; do
        [ "\$arg" = -q ] && {
          printf 'stubcontainerid\n'
          exit 0
        }
      done
      printf 'running\n'
      ;;
    exec)
      case "\$verb" in
        ping)
          printf 'stubhost,0.001,0xabc,7,90\n'
          ;;
        query)
          block_file="\${STUB_STATE:?}/block"
          if [ -f "\$block_file" ]; then
            block=\$(( \$(cat "\$block_file") + 2 ))
          else
            block=0
          fi
          printf '%s' "\$block" >"\$block_file"
          printf '{"block": %s, "slot": %s}\n' "\$block" "\$(( block * 10 ))"
          ;;
        *) exit 64 ;;
      esac
      ;;
    logs) ;;
    *) exit 64 ;;
  esac
  exit 0
fi
case "\${1:-}" in
  image)
    # docker image inspect --format {{.Id}} <ref>
    printf '%s\n' "\${STUB_CANDIDATE_IMAGE_ID:?}"
    ;;
  inspect)
    printf '%s\n' "\${STUB_CANDIDATE_IMAGE_ID:?}"
    ;;
  *) exit 64 ;;
esac
STUB
chmod +x "$stub_bin/docker"

run_command() {
  case_name=$1
  shift
  case_dir=$tmp_root/$case_name
  mkdir -p "$case_dir"
  case_stdout=$case_dir/stdout
  case_stderr=$case_dir/stderr
  case_rc=0
  env \
    PATH="$stub_bin:$PATH" \
    STUB_LOG="$stub_log" \
    STUB_STATE="$stub_state" \
    STUB_COMPOSE_CONFIG="$compose_config" \
    STUB_CANDIDATE_IMAGE_ID="$candidate_image_id" \
    HEAD_CANDIDATE_TIP_SAMPLE_SECONDS=1 \
    "$@" >"$case_stdout" 2>"$case_stderr" || case_rc=$?
}

require_failure() {
  [ "$case_rc" -ne 0 ] || fail "scenario=$case_name expected failure"
}

require_success() {
  [ "$case_rc" -eq 0 ] ||
    fail "scenario=$case_name expected success, exit=$case_rc: $(tr '\n' ' ' <"$case_stderr")"
}

assert_stdout_contains() {
  grep -Fq -- "$1" "$case_stdout" ||
    fail "scenario=$case_name stdout lacks: $1"
}

assert_stdout_lacks() {
  if grep -Fq -- "$1" "$case_stdout"; then
    fail "scenario=$case_name stdout unexpectedly contains: $1"
  fi
}

assert_stderr_token() {
  grep -Fq -- "$1" "$case_stderr" ||
    fail "scenario=$case_name stderr lacks token: $1"
}

assert_file_contains() {
  grep -Fqx -- "$2" "$1" ||
    fail "$1 does not contain exact line: $2"
}

[ -x "$command_under_test" ] ||
  fail "cluster command is absent or not executable: $command_under_test"

# ---------------------------------------------------------------------------
# Case 1: a failed teardown fails the command and no PASS is reachable.
# ---------------------------------------------------------------------------
: >"$stub_log"
images_out=$tmp_root/images-down-fails
run_command teardown-down-fails \
  STUB_DOWN_FAILS=1 \
  "$command_under_test" "$model_dir/docker-compose.yaml" 60 "$images_out"
require_failure
assert_stderr_token 'cluster teardown failed'
assert_stdout_lacks 'PASS:'
grep -q 'docker compose --progress quiet -f .* down --volumes --remove-orphans' \
  "$stub_log" ||
  fail 'teardown-down-fails: stub log lacks the down invocation'
pass teardown-failure-fails-the-command

# ---------------------------------------------------------------------------
# Case 2: a nonzero post-down census fails the command.
# ---------------------------------------------------------------------------
: >"$stub_log"
images_out=$tmp_root/images-census-leftover
run_command teardown-census-leftover \
  STUB_CENSUS_LEFTOVER=1 \
  "$command_under_test" "$model_dir/docker-compose.yaml" 60 "$images_out"
require_failure
assert_stderr_token 'teardown-census containers=1'
assert_stdout_lacks 'PASS:'
pass teardown-census-failure-fails-the-command

# ---------------------------------------------------------------------------
# Case 3: the success path tears down strictly, censuses zero, then PASS.
# ---------------------------------------------------------------------------
: >"$stub_log"
rm -rf "$stub_state"
mkdir -p "$stub_state"
images_out=$tmp_root/images-ok
run_command teardown-ok \
  "$command_under_test" "$model_dir/docker-compose.yaml" 60 "$images_out"
require_success
assert_stdout_contains 'teardown-census containers=0'
assert_stdout_contains 'PASS: 7 node services answering on the candidate image; chain advancing'
assert_stdout_contains "node-image relay3 $digest"
node_image_lines=$(grep -c '^node-image ' "$images_out" || true)
[ "$node_image_lines" -eq 7 ] ||
  fail "expected 7 node-image lines, found $node_image_lines"
for service in p1 p2 p3 p4 relay1 relay2 relay3; do
  assert_file_contains "$images_out" "node-image $service $digest"
done
# PASS is printed only after the strict down: the stub log must show a down
# before the census query that gates the PASS line.
down_line=$(grep -n 'down --volumes --remove-orphans' "$stub_log" | head -1 | cut -d: -f1)
census_line=$(grep -n 'ps -aq' "$stub_log" | head -1 | cut -d: -f1)
[ -n "$down_line" ] && [ -n "$census_line" ] &&
  [ "$census_line" -gt "$down_line" ] ||
  fail "stub log does not show the census after the teardown down"
pass teardown-strict-then-pass
