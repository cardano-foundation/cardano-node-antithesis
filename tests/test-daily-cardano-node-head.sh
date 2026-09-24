#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
controller="$repo_root/scripts/daily-cardano-node-head.sh"
fake_transport="$repo_root/tests/fixtures/daily-cardano-node-head/fake-transport.sh"
functions_model="$repo_root/specs/215-immutable-cardano-node-head-candidate/functions-model.md"
data_model="$repo_root/specs/215-immutable-cardano-node-head-candidate/data-model.md"

tmp_root=$(mktemp -d)
trap 'rm -rf "$tmp_root"' EXIT

upstream_sha=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
digest_hex=bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
candidate_ref="ghcr.io/cardano-foundation/cardano-node:${upstream_sha}@sha256:${digest_hex}"
image_repository=ghcr.io/cardano-foundation/cardano-node
upstream_origin=https://github.com/IntersectMBO/cardano-node.git
upstream_ref=refs/heads/master
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

assert_log_count() {
  local expected=$1
  local pattern=$2
  local actual
  actual=$(grep -Ec -- "$pattern" "$case_log" || true)
  [ "$actual" -eq "$expected" ] ||
    fail "scenario=$case_name expected $expected log matches for $pattern, found $actual"
}

assert_no_submit() {
  assert_log_count 0 '^fake-submit '
}

last_receipt_block() {
  awk '
    BEGIN { block = "" }
    /^schema=/ { block = $0 ORS; next }
    { block = block $0 ORS }
    END { printf "%s", block }
  ' "$case_receipt"
}

assert_last_receipt_contains() {
  local literal=$1
  local block
  block=$(last_receipt_block)
  grep -Fqx -- "$literal" <<<"$block" ||
    fail "last receipt record lacks exact line: $literal"
}

assert_last_receipt_lacks() {
  local pattern=$1
  local block
  block=$(last_receipt_block)
  if grep -Eq -- "$pattern" <<<"$block"; then
    fail "last receipt record unexpectedly matches: $pattern"
  fi
}

assert_honest_failure_receipt() {
  local stage=$1
  local reason=$2
  local forbidden=''
  assert_last_receipt_contains "stage=$stage"
  assert_last_receipt_contains 'outcome=FAILED'
  assert_last_receipt_contains "error=$reason"
  assert_last_receipt_contains 'schema=CandidateReceiptV1'
  assert_last_receipt_lacks \
    'outcome=(OBSERVED|PUBLISHED|PROVEN|RENDERED|VERIFIED|VALIDATED|PREPARED)'
  case "$stage" in
    resolve-upstream)
      forbidden='^(upstream_sha|candidate_ref|binary_revision|rendered_model|topology_services|topology_image|submission)='
      ;;
    publish-candidate)
      forbidden='^(candidate_ref|binary_revision|rendered_model|topology_services|topology_image|submission)='
      ;;
    prove-revision)
      forbidden='^(binary_revision|rendered_model|topology_services|topology_image|submission)='
      ;;
    render-topology)
      forbidden='^(rendered_model|topology_services|topology_image|submission)='
      ;;
    verify-topology)
      forbidden='^(topology_services|topology_image|submission)='
      ;;
    validate-compose | submit-candidate)
      forbidden='^submission='
      ;;
    *) fail "unknown failing receipt stage: $stage" ;;
  esac
  assert_last_receipt_lacks "$forbidden"
}

transport_operation_surface() {
  awk '
    /^case "\$operation" in$/ { in_op = 1; next }
    in_op && /^esac$/ { exit }
    in_op && /^  [a-z0-9-]+\)$/ {
      name = $1
      sub(/\)$/, "", name)
      print name
    }
  ' "$fake_transport" | sort
}

modeled_transport_operations() {
  awk -F'|' '
    /^## Transport operation surface/ { in_surface = 1; next }
    in_surface && /^Signature-level constraints:/ { exit }
    in_surface && $2 ~ /`[a-z0-9-]+`/ {
      operation = $2
      gsub(/[^a-z0-9-]/, "", operation)
      print operation
    }
  ' "$functions_model" | sort
}

modeled_rendered_model_operations() {
  awk -F'|' '
    $2 ~ /`[a-z0-9-]+`/ && $3 ~ /`rendered_model/ {
      operation = $2
      gsub(/[^a-z0-9-]/, "", operation)
      print operation
    }
  ' "$functions_model" | sort
}

modeled_receipt_fields() {
  awk -F'|' '
    /^## D-05 / { in_receipt = 1; next }
    in_receipt && /^State invariants:/ { exit }
    in_receipt && $2 ~ /`[a-z_]+`/ {
      cell = $2
      while (match(cell, /`[a-z_]+`/)) {
        print substr(cell, RSTART + 1, RLENGTH - 2)
        cell = substr(cell, RSTART + RLENGTH)
      }
    }
  ' "$data_model" | sort
}

assert_complete_receipt() {
  local expected_mode=$1
  local actual_fields expected_fields field modeled_fields
  declare -A expected=(
    [schema]=CandidateReceiptV1
    [stage]=submit-candidate
    [outcome]=PREPARED
    [mode]="$expected_mode"
    [upstream_origin]="$upstream_origin"
    [upstream_ref]="$upstream_ref"
    [upstream_sha]="$upstream_sha"
    [candidate_ref]="$candidate_ref"
    [binary_revision]="$upstream_sha"
    [rendered_model]="$case_state/rendered-model"
    [topology_services]="${#expected_node_services[@]}"
    [topology_image]="$candidate_ref"
    [submission]="fake://$upstream_sha"
  )
  modeled_fields=$(modeled_receipt_fields | grep -vx error)
  expected_fields=$(printf '%s\n' "${!expected[@]}" | sort)
  [ "$modeled_fields" = "$expected_fields" ] ||
    fail "complete receipt witness inventory differs from D-05
modeled:
$modeled_fields
witnessed:
$expected_fields"
  actual_fields=$(last_receipt_block | sed '/^$/d; s/=.*//' | sort)
  [ "$actual_fields" = "$modeled_fields" ] ||
    fail "complete receipt field inventory differs from D-05
modeled:
$modeled_fields
actual:
$actual_fields"
  while IFS= read -r field; do
    assert_last_receipt_contains "$field=${expected[$field]}"
  done <<<"$modeled_fields"
  assert_last_receipt_lacks '^error='
}

case_number=0
run_case() {
  case_name=$1
  local mode=${2:-test}
  local transport_path=${3:-$fake_transport}
  local repository=${4:-$image_repository}
  local -a controller_env=()
  case_number=$((case_number + 1))
  case_dir="$tmp_root/$case_number-$case_name"
  case_state="$case_dir/state"
  case_log="$case_dir/transport.log"
  case_receipt="$case_dir/receipt"
  case_stdout="$case_dir/stdout"
  case_stderr="$case_dir/stderr"
  mkdir -p "$case_state"
  : >"$case_log"
  : >"$case_receipt"

  controller_env=(
    "FAKE_SCENARIO=$case_name"
    "FAKE_LOG=$case_log"
    "HEAD_CANDIDATE_TRANSPORT=$transport_path"
    "HEAD_CANDIDATE_MODE=$mode"
    "HEAD_CANDIDATE_STATE_DIR=$case_state"
    "HEAD_CANDIDATE_RECEIPT=$case_receipt"
  )
  if [ "$repository" != DEFAULT ]; then
    controller_env+=("HEAD_CANDIDATE_IMAGE_REPOSITORY=$repository")
  fi

  case_rc=0
  env -u HEAD_CANDIDATE_IMAGE_REPOSITORY "${controller_env[@]}" \
    "$controller" >"$case_stdout" 2>"$case_stderr" || case_rc=$?
}

require_success() {
  [ "$case_rc" -eq 0 ] ||
    fail "scenario=$case_name expected success, exit=$case_rc: $(tr '\n' ' ' <"$case_stderr")"
}

require_failure() {
  [ "$case_rc" -ne 0 ] || fail "scenario=$case_name expected failure"
}

normalized_interaction_log() {
  local log_file=$1
  local state_path=$2
  sed \
    -e "s|$state_path|<state>|g" \
    -e 's/mode=\(test\|manual\)/mode=<mode>/g' \
    "$log_file"
}

run_rejection() {
  local scenario=$1
  local stage=$2
  local reason=$3
  shift 3
  run_case "$scenario"
  require_failure
  if [ "$stage" = submit-candidate ]; then
    assert_log_count 1 '^fake-submit '
  else
    assert_no_submit
  fi
  while [ "$#" -gt 0 ]; do
    assert_log_count 0 "^$1 "
    shift
  done
  assert_honest_failure_receipt "$stage" "$reason"
  pass "$scenario"
}

if [ ! -x "$fake_transport" ]; then
  fail "fake transport is absent or not executable: $fake_transport"
fi

if [ ! -x "$controller" ]; then
  fail "controller behavior absent: expected executable $controller"
fi

if [ ! -f "$functions_model" ]; then
  fail "functions model absent: $functions_model"
fi

if [ ! -f "$data_model" ]; then
  fail "data model absent: $data_model"
fi

run_case prepared
require_success
assert_complete_receipt test
assert_log_count 1 '^resolve-upstream '
assert_log_count 1 '^publish-candidate '
assert_log_count 1 '^prove-revision '
assert_log_count 1 '^render-topology '
assert_log_count 1 '^describe-topology '
assert_log_count 1 '^validate-compose '
assert_log_count 1 "^fake-submit .* ${upstream_sha}\$"
assert_log_count 7 '^receipt '
stage_records=$(grep -c '^stage=' "$case_receipt" || true)
[ "$stage_records" -eq 7 ] ||
  fail "prepared expected 7 append-only stage records, found $stage_records"
assert_file_contains "$case_receipt" 'stage=resolve-upstream'
assert_file_contains "$case_receipt" 'stage=publish-candidate'
assert_file_contains "$case_receipt" 'stage=prove-revision'
assert_file_contains "$case_receipt" 'stage=render-topology'
assert_file_contains "$case_receipt" 'stage=verify-topology'
assert_file_contains "$case_receipt" 'stage=validate-compose'
assert_file_contains "$case_receipt" 'stage=submit-candidate'
assert_file_contains "$case_stdout" \
  "PREPARED $upstream_sha $candidate_ref fake://$upstream_sha"
pass prepared

run_case prepared test "$fake_transport" DEFAULT
require_success
assert_log_count 1 "^publish-candidate $upstream_sha $image_repository\$"
pass default-image-repository

# A repository may carry a registry host:port (A-001); the candidate form is
# still repository:40hex-tag@sha256:digest and the whole path completes.
port_repository=localhost:5000/cardano-node-head
run_case prepared test "$fake_transport" "$port_repository"
require_success
assert_log_count 1 "^publish-candidate $upstream_sha $port_repository\$"
assert_file_contains "$case_receipt" \
  "candidate_ref=$port_repository:$upstream_sha@sha256:$digest_hex"
assert_file_contains "$case_receipt" 'topology_image='"$port_repository:$upstream_sha@sha256:$digest_hex"
pass port-bearing-repository-accepted

# The tag discipline does not relax with the port: a short tag after a
# port-bearing repository is still a malformed candidate form.
run_rejection short-tag-with-port publish-candidate malformed-candidate-form prove-revision

run_case prepared test
require_success
test_interactions=$(normalized_interaction_log "$case_log" "$case_state")
assert_log_count 1 '^fake-submit '

run_case prepared manual
require_success
assert_complete_receipt manual
manual_interactions=$(normalized_interaction_log "$case_log" "$case_state")

[ "$test_interactions" = "$manual_interactions" ] ||
  fail "mode-parity full interactions differ
test:
$test_interactions
manual:
$manual_interactions"
assert_log_count 1 '^fake-submit '
pass mode-parity

run_rejection wrong-ref resolve-upstream wrong-ref publish-candidate
run_rejection wrong-origin resolve-upstream wrong-origin publish-candidate
run_rejection resolve-failure resolve-upstream observation-command-failed publish-candidate
run_rejection malformed-observation resolve-upstream malformed-sha publish-candidate
run_rejection zero-observation resolve-upstream observation-count-0 publish-candidate
run_rejection ambiguous-observation resolve-upstream observation-count-2 publish-candidate
run_rejection trailing-field-observation resolve-upstream malformed-observation publish-candidate
run_rejection publish-failure publish-candidate publish-failed prove-revision
run_rejection malformed-candidate-form publish-candidate malformed-candidate-form prove-revision
run_rejection repository-whitespace publish-candidate malformed-candidate-form prove-revision
run_rejection tag-sha-mismatch publish-candidate tag-sha-mismatch prove-revision
run_rejection multiline-candidate-form publish-candidate multi-line-candidate prove-revision
run_rejection revision-mismatch prove-revision revision-mismatch render-topology
run_rejection revision-absent prove-revision revision-absent render-topology
run_rejection multiline-revision prove-revision multi-line-revision render-topology
run_rejection unparsable-revision prove-revision unparsable-revision render-topology
run_rejection render-failure render-topology render-failed describe-topology
run_rejection multiline-model-path render-topology multi-line-model-path describe-topology
run_rejection model-outside-state-dir render-topology model-outside-state-dir describe-topology
run_rejection describe-failure verify-topology describe-failed validate-compose
run_rejection stale-topology-override verify-topology stale-topology-override validate-compose
run_rejection missing-node-service verify-topology missing-node-service-relay3 validate-compose
run_rejection zero-topology-census verify-topology zero-topology-census validate-compose
run_rejection trailing-field-topology verify-topology malformed-topology-row validate-compose
run_rejection empty-topology-service verify-topology empty-service validate-compose
run_rejection empty-topology-image verify-topology empty-image validate-compose
run_rejection whitespace-topology-image verify-topology whitespace-image validate-compose
run_rejection duplicate-node-service verify-topology census-count-8 validate-compose
run_rejection rendered-model-candidate-mismatch verify-topology image-mismatch validate-compose
run_rejection compose-failure validate-compose compose-failed
run_rejection submission-failure submit-candidate submission-failed
run_rejection multiline-submission submit-candidate multi-line-submission
run_rejection malformed-submission submit-candidate malformed-submission

model_operations=(describe-topology validate-compose fake-submit)
modeled_operations=$(modeled_rendered_model_operations)
witnessed_operations=$(printf '%s\n' "${model_operations[@]}" | sort)
[ "$modeled_operations" = "$witnessed_operations" ] ||
  fail "rendered-model witness inventory differs from the operation surface
modeled:
$modeled_operations
witnessed:
$witnessed_operations"

for model_operation in "${model_operations[@]}"; do
  witness_dir="$tmp_root/model-witness-$model_operation"
  mkdir -p "$witness_dir/state"
  witness_log="$witness_dir/transport.log"
  : >"$witness_log"
  witness_args=("$witness_dir/not-rendered-model")
  if [ "$model_operation" = fake-submit ]; then
    witness_args+=("$candidate_ref" "$upstream_sha")
  fi
  if env \
    FAKE_SCENARIO=prepared \
    FAKE_LOG="$witness_log" \
    HEAD_CANDIDATE_STATE_DIR="$witness_dir/state" \
    HEAD_CANDIDATE_RECEIPT="$witness_dir/receipt" \
    "$fake_transport" "$model_operation" "${witness_args[@]}" \
    2>"$witness_dir/stderr"; then
    fail "$model_operation accepted a model it could not witness"
  fi
  assert_file_contains "$witness_log" \
    "$model_operation ${witness_args[*]}"
  grep -Fq 'rendered model missing:' "$witness_dir/stderr" ||
    fail "$model_operation did not report the missing rendered model"
done
pass rendered-model-witnesses

run_case unsupported-mode bogus
require_failure
assert_log_count 0 '.'
assert_no_submit
pass unsupported-mode

missing_transport_path="$tmp_root/non-executable-transport"
printf '#!/usr/bin/env bash\necho should-not-run\n' >"$missing_transport_path"
chmod a-x "$missing_transport_path"
run_case missing-transport test "$missing_transport_path"
require_failure
grep -Fq 'transport is not executable' "$case_stderr" ||
  fail "missing-transport stderr lacks preflight rejection token"
assert_no_submit
pass missing-transport

# Daily-run operations added by #216 on top of the frozen #215 surface.
daily_transport_operations=(prepare-consumer claim-day submit-run await-run)

actual_ops=$(transport_operation_surface)
expected_ops=$({ modeled_transport_operations; printf '%s\n' "${daily_transport_operations[@]}"; } | sort)
[ "$actual_ops" = "$expected_ops" ] ||
  fail "transport operation surface mismatch
expected:
$expected_ops
actual:
$actual_ops"
controller_ops=$(grep -Eoh 'transport_call [a-z0-9-]+' "$controller" |
  awk '{print $2}' | sort -u)
while IFS= read -r op; do
  [ -n "$op" ] || continue
  grep -Fqx -- "$op" <<<"$expected_ops" ||
    fail "controller invokes undocumented transport operation: $op"
done <<<"$controller_ops"
run_case prepared
require_success
assert_log_count 1 '^fake-submit '
pass no-real-submission

run_case revision-mismatch
require_failure
assert_honest_failure_receipt prove-revision revision-mismatch
assert_last_receipt_contains "candidate_ref=$candidate_ref"
assert_file_contains "$case_receipt" 'stage=resolve-upstream'
assert_file_contains "$case_receipt" 'outcome=OBSERVED'
assert_file_contains "$case_receipt" 'stage=publish-candidate'
assert_file_contains "$case_receipt" 'outcome=PUBLISHED'
assert_no_submit
pass receipt-honesty

# ---------------------------------------------------------------------------
# Daily run modes (#216): one real submission attempt per UTC day.
# ---------------------------------------------------------------------------
daily_day=2026-09-24
production_claim_ref="refs/tags/daily-cardano-node-head/$daily_day"
validation_claim_ref="refs/tags/daily-cardano-node-head/validation/$daily_day"
daily_consumer_sha=eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
daily_run_url=https://github.com/cardano-foundation/cardano-node-antithesis/actions/runs/424242
daily_moog_id=ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
daily_report_url=https://amaru-cardano.antithesis.com/report/00000000-0000-0000-0000-000000000000
daily_repository=cardano-foundation/cardano-node-antithesis

claim_marker_path() {
  printf '%s/claims/%s\n' "$1" "${2//\//_}"
}

count_log() {
  grep -Ec -- "$2" "$1" || true
}

assert_no_real_submission() {
  [ "$(count_log "$case_log" '^submit-run ')" -eq 0 ] ||
    fail "scenario=$case_name reached real submission: $(cat "$case_log")"
}

assert_daily_failure_receipt() {
  local stage=$1
  local reason=$2
  local forbidden=''
  assert_last_receipt_contains "stage=$stage"
  assert_last_receipt_contains 'outcome=FAILED'
  assert_last_receipt_contains "error=$reason"
  assert_last_receipt_contains 'schema=CandidateReceiptV1'
  case "$stage" in
    prepare-consumer)
      forbidden='^(consumer_sha|workflow_run|moog_test_id|report_url|terminal_outcome)='
      ;;
    claim-day | submit-run)
      forbidden='^(workflow_run|moog_test_id|report_url|terminal_outcome)='
      ;;
    await-run)
      forbidden='^(moog_test_id|report_url|terminal_outcome)='
      ;;
    *) fail "unknown daily failing receipt stage: $stage" ;;
  esac
  assert_last_receipt_lacks "$forbidden"
  assert_last_receipt_lacks 'outcome=TERMINAL'
}

# A complete daily receipt carries every identity the operator correlates.
assert_daily_complete_receipt() {
  local expected_mode=$1
  local expected_claim_ref=$2
  local expected_duration=$3
  assert_last_receipt_contains 'stage=await-run'
  assert_last_receipt_contains 'outcome=TERMINAL'
  assert_last_receipt_contains "mode=$expected_mode"
  assert_last_receipt_contains "day=$daily_day"
  assert_last_receipt_contains "claim_ref=$expected_claim_ref"
  assert_last_receipt_contains "duration=$expected_duration"
  assert_last_receipt_contains 'faults=enabled'
  assert_last_receipt_contains "consumer_repository=$daily_repository"
  assert_last_receipt_contains "upstream_sha=$upstream_sha"
  assert_last_receipt_contains "candidate_ref=$candidate_ref"
  assert_last_receipt_contains "binary_revision=$upstream_sha"
  assert_last_receipt_contains "topology_image=$candidate_ref"
  assert_last_receipt_contains "submission=fake://$upstream_sha"
  assert_last_receipt_contains "consumer_sha=$daily_consumer_sha"
  assert_last_receipt_contains "workflow_run=$daily_run_url"
  assert_last_receipt_contains "moog_test_id=$daily_moog_id"
  assert_last_receipt_contains "report_url=$daily_report_url"
  assert_last_receipt_contains 'terminal_outcome=success'
  assert_last_receipt_lacks '^error='
  if grep -Eiq 'ghs_|gho_|token=' "$case_receipt"; then
    fail 'daily receipt leaked credential-shaped text'
  fi
}

# One daily controller invocation. A shared state directory carries the fake
# transport's claim ledger across invocations; each invocation keeps its own
# transport log, stdout, stderr and receipt so per-invocation refusal is
# attributable. An empty day lets the controller derive the UTC day itself.
daily_case_number=1000
run_daily() {
  daily_case_number=$((daily_case_number + 1))
  local label=$1
  local scenario=$2
  local mode=${3:-daily}
  local shared_state=${4:-}
  local day=${5:-$daily_day}
  case_dir="$tmp_root/$daily_case_number-$label"
  mkdir -p "$case_dir"
  if [ -n "$shared_state" ]; then
    case_state=$shared_state/state
    case_receipt="$shared_state/receipt-$label"
  else
    case_state=$case_dir/state
    case_receipt=$case_dir/receipt
    mkdir -p "$case_state"
  fi
  case_log=$case_dir/transport.log
  case_stdout=$case_dir/stdout
  case_stderr=$case_dir/stderr
  : >"$case_log"
  local -a controller_env=(
    "FAKE_SCENARIO=$scenario"
    "FAKE_LOG=$case_log"
    "HEAD_CANDIDATE_TRANSPORT=$fake_transport"
    "HEAD_CANDIDATE_MODE=$mode"
    "HEAD_CANDIDATE_STATE_DIR=$case_state"
    "HEAD_CANDIDATE_RECEIPT=$case_receipt"
  )
  if [ -n "$day" ]; then
    controller_env+=("HEAD_CANDIDATE_DAY=$day")
  fi
  case_rc=0
  env -u HEAD_CANDIDATE_IMAGE_REPOSITORY -u HEAD_CANDIDATE_DAY \
    "${controller_env[@]}" \
    "$controller" >"$case_stdout" 2>"$case_stderr" || case_rc=$?
}

require_daily_success() {
  [ "$case_rc" -eq 0 ] ||
    fail "scenario=$1 expected success, exit=$case_rc: $(tr '\n' ' ' <"$case_stderr")"
}

require_daily_failure() {
  [ "$case_rc" -ne 0 ] || fail "scenario=$1 expected failure"
}

# --- happy paths -----------------------------------------------------------
run_daily daily-prepared prepared daily
require_daily_success daily-prepared
assert_file_contains "$case_stdout" \
  "RUN $daily_day $upstream_sha $daily_consumer_sha success"
assert_daily_complete_receipt daily "$production_claim_ref" 3
assert_log_count 1 '^prepare-consumer '
assert_log_count 1 '^claim-day '
assert_log_count 1 '^submit-run '
assert_log_count 1 '^await-run '
stage_records=$(grep -c '^stage=' "$case_receipt" || true)
[ "$stage_records" -eq 11 ] ||
  fail "daily-prepared expected 11 stage records, found $stage_records"
pass daily-prepared

# The exact real request: 3 hours, faults on, exact consumer commit, at the
# claim ref, in the HEAD testnet directory.
assert_file_contains "$case_log" \
  "submit-run $daily_consumer_sha $production_claim_ref cardano_node_head 3 false"
assert_file_contains "$case_log" "await-run $daily_consumer_sha $daily_run_url"
assert_file_contains "$case_log" \
  "prepare-consumer $daily_day $case_state/rendered-model $candidate_ref cardano_node_head"
assert_file_contains "$case_log" "claim-day $production_claim_ref $daily_consumer_sha"
pass request-3h-faults-exact-consumer

# Every identity in one terminal record, agreeing with the observed request.
assert_daily_complete_receipt daily "$production_claim_ref" 3
pass receipt-correlation

# The day is derived from the clock when the workflow supplies no day.
run_daily daily-clock-derived prepared daily '' ''
require_daily_success daily-clock-derived
expected_today=$(TZ=UTC0 printf '%(%Y-%m-%d)T' -1)
assert_file_contains "$case_receipt" "day=$expected_today"
pass daily-clock-derived

# A terminal failure outcome is an honest terminal run, not a controller error.
run_daily daily-terminal-failure daily-terminal-failure daily
require_daily_success daily-terminal-failure
assert_last_receipt_contains 'stage=await-run'
assert_last_receipt_contains 'outcome=TERMINAL'
assert_last_receipt_contains 'terminal_outcome=failure'
assert_log_count 1 '^submit-run '
pass daily-terminal-failure

# Validation mode: 1 hour, own claim namespace, production day untouched.
validation_state=$tmp_root/validation-state
mkdir -p "$validation_state/state"
run_daily validation-prepared prepared validation "$validation_state"
require_daily_success validation-prepared
assert_daily_complete_receipt validation "$validation_claim_ref" 1
assert_file_contains "$case_log" \
  "submit-run $daily_consumer_sha $validation_claim_ref cardano_node_head 1 false"
[ -d "$(claim_marker_path "$validation_state/state" "$validation_claim_ref")" ] ||
  fail 'validation run left no validation claim marker'
[ ! -d "$(claim_marker_path "$validation_state/state" "$production_claim_ref")" ] ||
  fail 'validation run consumed the production day claim'
# The same UTC day still allows a fresh production claim afterwards.
run_daily daily-after-validation prepared daily "$validation_state"
require_daily_success daily-after-validation
assert_file_contains "$case_receipt" "claim_ref=$production_claim_ref"
assert_file_contains "$case_receipt" 'stage=claim-day'
assert_file_contains "$case_receipt" 'outcome=CLAIMED'
pass validation-cannot-consume-day

# --- duplicate day ----------------------------------------------------------
duplicate_state=$tmp_root/duplicate-state
mkdir -p "$duplicate_state/state"
mkdir "$(claim_marker_path "$duplicate_state/state" "$production_claim_ref")"
run_daily duplicate-day-claim duplicate-day-claim daily "$duplicate_state"
require_daily_failure duplicate-day-claim
assert_daily_failure_receipt claim-day day-already-claimed
assert_file_contains "$case_receipt" "consumer_sha=$daily_consumer_sha"
assert_no_real_submission
pass duplicate-day-claim

# --- concurrent invocations: exactly one attempt wins -----------------------
concurrent_state=$tmp_root/concurrent-state
mkdir -p "$concurrent_state/state"
concurrent_log=$concurrent_state/transport.log
: >"$concurrent_log"
daily_pids=()
run_daily_concurrent() {
  local slot=$1
  local dir=$tmp_root/concurrent-$slot
  mkdir -p "$dir"
  env -u HEAD_CANDIDATE_IMAGE_REPOSITORY -u HEAD_CANDIDATE_DAY \
    FAKE_SCENARIO=prepared \
    FAKE_LOG="$concurrent_log" \
    HEAD_CANDIDATE_TRANSPORT="$fake_transport" \
    HEAD_CANDIDATE_MODE=daily \
    HEAD_CANDIDATE_DAY="$daily_day" \
    HEAD_CANDIDATE_STATE_DIR="$concurrent_state/state" \
    HEAD_CANDIDATE_RECEIPT="$dir/receipt" \
    "$controller" >"$dir/stdout" 2>"$dir/stderr" &
  daily_pids+=("$!")
}
run_daily_concurrent one
run_daily_concurrent two
concurrent_ok=0
concurrent_fail=0
for pid in "${daily_pids[@]}"; do
  if wait "$pid"; then
    concurrent_ok=$((concurrent_ok + 1))
  else
    concurrent_fail=$((concurrent_fail + 1))
  fi
done
[ "$concurrent_ok" -eq 1 ] && [ "$concurrent_fail" -eq 1 ] ||
  fail "concurrent-day-claim expected one winner and one refusal, got ok=$concurrent_ok fail=$concurrent_fail"
claim_attempts=$(count_log "$concurrent_log" '^claim-day ')
[ "$claim_attempts" -eq 2 ] ||
  fail "concurrent-day-claim expected 2 claim attempts, found $claim_attempts"
total_submissions=$(count_log "$concurrent_log" '^submit-run ')
[ "$total_submissions" -eq 1 ] ||
  fail "concurrent-day-claim expected exactly 1 submission, found $total_submissions"
blocked_found=0
for concurrent_receipt in "$tmp_root"/concurrent-one/receipt \
  "$tmp_root"/concurrent-two/receipt; do
  if grep -Fqx 'error=day-already-claimed' "$concurrent_receipt"; then
    blocked_found=$((blocked_found + 1))
  fi
done
[ "$blocked_found" -eq 1 ] ||
  fail 'concurrent-day-claim expected exactly one day-already-claimed receipt'
pass concurrent-day-claim

# --- no retry after a failed attempt ----------------------------------------
retry_state=$tmp_root/retry-state
mkdir -p "$retry_state/state"
run_daily no-retry-first daily-request-failure daily "$retry_state"
require_daily_failure no-retry-first
assert_daily_failure_receipt submit-run request-failed
assert_file_contains "$case_receipt" 'stage=claim-day'
assert_file_contains "$case_receipt" 'outcome=CLAIMED'
assert_log_count 1 '^submit-run '
run_daily no-retry-second prepared daily "$retry_state"
require_daily_failure no-retry-second
assert_daily_failure_receipt claim-day day-already-claimed
assert_no_real_submission
pass no-retry-after-failed-attempt

# --- the consumer ref is immutable once claimed ------------------------------
run_daily consumer-ref-immutable prepared daily
require_daily_success consumer-ref-immutable
immutable_state=$case_state
[ -d "$(claim_marker_path "$immutable_state" "$production_claim_ref")" ] ||
  fail 'happy daily run created no claim marker'
other_consumer=dddddddddddddddddddddddddddddddddddddddd
immutable_log=$case_dir/direct-claim.log
: >"$immutable_log"
immutable_rc=0
env FAKE_SCENARIO=prepared FAKE_LOG="$immutable_log" \
  HEAD_CANDIDATE_STATE_DIR="$immutable_state" \
  HEAD_CANDIDATE_RECEIPT="$case_dir/direct-receipt" \
  "$fake_transport" claim-day "$production_claim_ref" "$other_consumer" \
  >"$case_dir/direct-stdout" 2>"$case_dir/direct-stderr" || immutable_rc=$?
[ "$immutable_rc" -ne 0 ] ||
  fail 'consumer-ref-immutable: re-claim at a different commit succeeded'
assert_file_contains "$case_dir/direct-stdout" 'BLOCKED day-already-claimed'
assert_file_contains "$immutable_log" \
  "claim-day $production_claim_ref $other_consumer"
pass consumer-ref-immutable

# --- prerequisites: every failed stage stops before submission ---------------
run_daily prerequisite-publication publish-failure daily
require_daily_failure prerequisite-publication
assert_daily_failure_receipt publish-candidate publish-failed
assert_no_real_submission
pass prerequisite-publication-blocks-submission

run_daily prerequisite-provenance revision-mismatch daily
require_daily_failure prerequisite-provenance
assert_daily_failure_receipt prove-revision revision-mismatch
assert_no_real_submission
pass prerequisite-provenance-blocks-submission

run_daily prerequisite-compose compose-failure daily
require_daily_failure prerequisite-compose
assert_daily_failure_receipt validate-compose compose-failed
assert_no_real_submission
pass prerequisite-compose-blocks-submission

run_daily prerequisite-smoke submission-failure daily
require_daily_failure prerequisite-smoke
assert_daily_failure_receipt submit-candidate submission-failed
assert_no_real_submission
assert_log_count 0 '^prepare-consumer '
pass prerequisite-smoke-blocks-submission

run_daily prerequisite-claim daily-claim-failure daily
require_daily_failure prerequisite-claim
assert_daily_failure_receipt claim-day claim-failed
assert_file_contains "$case_receipt" "consumer_sha=$daily_consumer_sha"
assert_no_real_submission
pass prerequisite-claim-blocks-submission

run_daily prerequisite-request daily-request-failure daily
require_daily_failure prerequisite-request
assert_daily_failure_receipt submit-run request-failed
assert_no_real_submission
pass prerequisite-request-blocks-submission

# --- daily stage guards ------------------------------------------------------
run_daily daily-consumer-failure daily-consumer-failure daily
require_daily_failure daily-consumer-failure
assert_daily_failure_receipt prepare-consumer consumer-failed
assert_no_real_submission
pass daily-consumer-failure

run_daily daily-consumer-multiline daily-consumer-multiline daily
require_daily_failure daily-consumer-multiline
assert_daily_failure_receipt prepare-consumer multi-line-consumer
assert_no_real_submission
pass daily-consumer-multiline

run_daily daily-consumer-malformed-sha daily-consumer-malformed-sha daily
require_daily_failure daily-consumer-malformed-sha
assert_daily_failure_receipt prepare-consumer malformed-consumer-sha
assert_no_real_submission
pass daily-consumer-malformed-sha

run_daily daily-claim-malformed-verdict daily-claim-malformed-verdict daily
require_daily_failure daily-claim-malformed-verdict
assert_daily_failure_receipt claim-day malformed-claim-verdict
assert_no_real_submission
pass daily-claim-malformed-verdict

run_daily daily-run-url-multiline daily-run-url-multiline daily
require_daily_failure daily-run-url-multiline
assert_daily_failure_receipt submit-run multi-line-run-url
assert_file_contains "$case_receipt" "consumer_sha=$daily_consumer_sha"
assert_log_count 1 '^submit-run '
pass daily-run-url-multiline

run_daily daily-run-url-malformed daily-run-url-malformed daily
require_daily_failure daily-run-url-malformed
assert_daily_failure_receipt submit-run malformed-run-url
assert_log_count 1 '^submit-run '
pass daily-run-url-malformed

run_daily daily-await-failure daily-await-failure daily
require_daily_failure daily-await-failure
assert_daily_failure_receipt await-run await-failed
assert_file_contains "$case_receipt" "workflow_run=$daily_run_url"
assert_log_count 1 '^submit-run '
pass daily-await-failure

run_daily daily-correlation-multiline daily-correlation-multiline daily
require_daily_failure daily-correlation-multiline
assert_daily_failure_receipt await-run multi-line-correlation
assert_log_count 1 '^submit-run '
pass daily-correlation-multiline

run_daily daily-correlation-malformed daily-correlation-malformed daily
require_daily_failure daily-correlation-malformed
assert_daily_failure_receipt await-run malformed-correlation
assert_log_count 1 '^submit-run '
pass daily-correlation-malformed

run_daily daily-report-url-malformed daily-report-url-malformed daily
require_daily_failure daily-report-url-malformed
assert_daily_failure_receipt await-run malformed-report-url
assert_file_contains "$case_receipt" "workflow_run=$daily_run_url"
pass daily-report-url-malformed

run_daily daily-await-not-terminal daily-await-not-terminal daily
require_daily_failure daily-await-not-terminal
assert_daily_failure_receipt await-run run-not-terminal
assert_file_contains "$case_receipt" "workflow_run=$daily_run_url"
pass daily-await-not-terminal

run_daily daily-invalid-day prepared daily '' 2026-13-99
require_daily_failure daily-invalid-day
grep -Fq 'invalid UTC day' "$case_stderr" ||
  fail 'daily-invalid-day stderr lacks the preflight rejection token'
assert_log_count 0 '.'
assert_no_real_submission
pass daily-invalid-day
