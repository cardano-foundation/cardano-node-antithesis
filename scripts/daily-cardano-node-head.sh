#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

mode=${HEAD_CANDIDATE_MODE:-manual}
transport=${HEAD_CANDIDATE_TRANSPORT:-$script_dir/daily-cardano-node-head-github.sh}
state_dir=${HEAD_CANDIDATE_STATE_DIR:-${RUNNER_TEMP:-/tmp}/daily-cardano-node-head}
receipt_path=${HEAD_CANDIDATE_RECEIPT:-$state_dir/receipt}
image_repository=${HEAD_CANDIDATE_IMAGE_REPOSITORY:-ghcr.io/cardano-foundation/cardano-node}

origin=https://github.com/IntersectMBO/cardano-node.git
ref=refs/heads/master

die() {
  printf 'daily-cardano-node-head: %s\n' "$*" >&2
  exit 1
}

case "$mode" in
  test | manual | daily | validation) ;;
  *) die "unsupported mode: $mode" ;;
esac

# Preflight before any transport call; stderr token is the positive signal.
[ -x "$transport" ] || die "transport is not executable: $transport"
mkdir -p "$state_dir"
export HEAD_CANDIDATE_STATE_DIR=$state_dir
export HEAD_CANDIDATE_RECEIPT=$receipt_path

transport_call() {
  "$transport" "$@"
}

declare -A receipt=()
receipt[schema]=CandidateReceiptV1
receipt[mode]=$mode
receipt[upstream_origin]=$origin
receipt[upstream_ref]=$ref

# ---------------------------------------------------------------------------
# Daily run identity (daily/validation modes only; #215 receipts unchanged)
# ---------------------------------------------------------------------------
consumer_testnet=cardano_node_head
consumer_repository=${HEAD_CANDIDATE_REPOSITORY:-${GITHUB_REPOSITORY:-cardano-foundation/cardano-node-antithesis}}
day=''
claim_ref=''
duration=''
if [ "$mode" = daily ] || [ "$mode" = validation ]; then
  day=${HEAD_CANDIDATE_DAY:-$(TZ=UTC0 printf '%(%Y-%m-%d)T' -1)}
  [[ "$day" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] ||
    die "invalid UTC day: $day"
  receipt[day]=$day
  case "$mode" in
    daily)
      claim_ref="refs/tags/daily-cardano-node-head/$day"
      duration=3
      ;;
    validation)
      claim_ref="refs/tags/daily-cardano-node-head/validation/$day"
      duration=1
      ;;
  esac
  receipt[claim_ref]=$claim_ref
  receipt[duration]=$duration
  receipt[faults]=enabled
  receipt[consumer_repository]=$consumer_repository
fi

receipt_keys=(
  schema stage outcome error mode
  upstream_origin upstream_ref upstream_sha
  candidate_ref binary_revision
  rendered_model topology_services topology_image
  submission
  day claim_ref duration faults consumer_repository request
  consumer_sha workflow_run moog_test_id report_url terminal_outcome
)

write_receipt() {
  local stage=$1
  local outcome=$2
  local pair key value
  local -a fields=()
  shift 2

  receipt[stage]=$stage
  receipt[outcome]=$outcome
  unset 'receipt[error]'
  for pair in "$@"; do
    key=${pair%%=*}
    value=${pair#*=}
    receipt["$key"]=$value
  done
  for key in "${receipt_keys[@]}"; do
    if [[ -v "receipt[$key]" ]]; then
      fields+=("$key=${receipt[$key]}")
    fi
  done
  transport_call receipt "${fields[@]}"
}

fail_stage() {
  local stage=$1
  local message=$2
  write_receipt "$stage" FAILED "error=$message"
  die "$stage: $message"
}

# One non-empty result line; never join multi-line transport output.
require_single_line() {
  local raw=$1
  local -n _out=$2
  local -a lines=()
  mapfile -t lines < <(printf '%s\n' "$raw" | sed '/^$/d')
  if [ "${#lines[@]}" -ne 1 ]; then
    return 1
  fi
  _out=${lines[0]}
  return 0
}

# Pipe fields: trailing empty counts (awk NF; bash read -a drops it).
require_pipe_fields() {
  local line=$1
  local expected=$2
  local -n _fields=$3
  local nf
  local -a parsed=()
  nf=$(awk -F'|' '{print NF}' <<<"$line")
  if [ "$nf" -ne "$expected" ]; then
    return 1
  fi
  IFS='|' read -r -a parsed <<<"$line"
  _fields=("${parsed[@]}")
  return 0
}

# ---------------------------------------------------------------------------
# resolve-upstream
# ---------------------------------------------------------------------------
observation_output=''
if ! observation_output=$(transport_call resolve-upstream "$origin" "$ref"); then
  fail_stage resolve-upstream observation-command-failed
fi

mapfile -t observations < <(printf '%s\n' "$observation_output" | sed '/^$/d')
if [ "${#observations[@]}" -ne 1 ]; then
  fail_stage resolve-upstream "observation-count-${#observations[@]}"
fi

declare -a obs_fields=()
if ! require_pipe_fields "${observations[0]}" 3 obs_fields; then
  fail_stage resolve-upstream malformed-observation
fi
observed_origin=${obs_fields[0]}
observed_ref=${obs_fields[1]}
observed_sha=${obs_fields[2]}
[ "$observed_origin" = "$origin" ] || fail_stage resolve-upstream wrong-origin
[ "$observed_ref" = "$ref" ] || fail_stage resolve-upstream wrong-ref
[[ "$observed_sha" =~ ^[0-9a-f]{40}$ ]] ||
  fail_stage resolve-upstream malformed-sha

receipt[upstream_sha]=$observed_sha
write_receipt resolve-upstream OBSERVED

# ---------------------------------------------------------------------------
# publish-candidate
# ---------------------------------------------------------------------------
publish_output=''
if ! publish_output=$(transport_call publish-candidate "$observed_sha" "$image_repository"); then
  fail_stage publish-candidate publish-failed
fi
candidate_ref=''
if ! require_single_line "$publish_output" candidate_ref; then
  fail_stage publish-candidate multi-line-candidate
fi

# D-02: repository:tag@digest with tag == resolved SHA. The repository may
# carry a registry host:port (A-001); the tag stays exactly 40-hex anchored
# against the digest at end of line, so the split stays unique.
tag_part=''
if [[ "$candidate_ref" =~ ^([^[:space:]@]+):([0-9a-f]{40})@(sha256:[0-9a-f]{64})$ ]]; then
  tag_part=${BASH_REMATCH[2]}
else
  fail_stage publish-candidate malformed-candidate-form
fi
[ "$tag_part" = "$observed_sha" ] ||
  fail_stage publish-candidate tag-sha-mismatch

receipt[candidate_ref]=$candidate_ref
write_receipt publish-candidate PUBLISHED

# ---------------------------------------------------------------------------
# prove-revision
# ---------------------------------------------------------------------------
revision_output=''
if ! revision_output=$(transport_call prove-revision "$candidate_ref"); then
  fail_stage prove-revision revision-absent
fi
binary_revision=''
if ! require_single_line "$revision_output" binary_revision; then
  fail_stage prove-revision multi-line-revision
fi
[[ "$binary_revision" =~ ^[0-9a-f]{40}$ ]] ||
  fail_stage prove-revision unparsable-revision
[ "$binary_revision" = "$observed_sha" ] ||
  fail_stage prove-revision revision-mismatch

receipt[binary_revision]=$binary_revision
write_receipt prove-revision PROVEN

# ---------------------------------------------------------------------------
# render-topology
# ---------------------------------------------------------------------------
render_output=''
if ! render_output=$(transport_call render-topology "$candidate_ref"); then
  fail_stage render-topology render-failed
fi
rendered_model=''
if ! require_single_line "$render_output" rendered_model; then
  fail_stage render-topology multi-line-model-path
fi
# Absolute path inside the state directory.
case "$rendered_model" in
  "$state_dir"/*) ;;
  *) fail_stage render-topology model-outside-state-dir ;;
esac

receipt[rendered_model]=$rendered_model
write_receipt render-topology RENDERED

# ---------------------------------------------------------------------------
# verify-topology (describe-topology + census/equality policy)
# ---------------------------------------------------------------------------
topology_output=''
if ! topology_output=$(transport_call describe-topology "$rendered_model"); then
  fail_stage verify-topology describe-failed
fi

mapfile -t topology_rows < <(printf '%s\n' "$topology_output" | sed '/^$/d')
if [ "${#topology_rows[@]}" -eq 0 ]; then
  fail_stage verify-topology zero-topology-census
fi

declare -A seen=()
node_service_count=0

for row in "${topology_rows[@]}"; do
  declare -a row_fields=()
  if ! require_pipe_fields "$row" 2 row_fields; then
    fail_stage verify-topology malformed-topology-row
  fi
  service=${row_fields[0]-}
  image=${row_fields[1]-}
  [ -n "$service" ] || fail_stage verify-topology empty-service
  [ -n "$image" ] || fail_stage verify-topology empty-image
  [[ ! "$image" =~ [[:space:]] ]] || fail_stage verify-topology whitespace-image

  case "$image" in
    ghcr.io/intersectmbo/cardano-node*)
      [ "$image" = "$candidate_ref" ] ||
        fail_stage verify-topology stale-topology-override
      ;;
  esac

  if [[ "$service" =~ ^(p[1-4]|relay[1-3])$ ]]; then
    [ "$image" = "$candidate_ref" ] ||
      fail_stage verify-topology image-mismatch
    seen["$service"]=1
    node_service_count=$((node_service_count + 1))
  fi
done

for service in p1 p2 p3 p4 relay1 relay2 relay3; do
  [[ -v "seen[$service]" ]] ||
    fail_stage verify-topology "missing-node-service-$service"
done
[ "$node_service_count" -eq 7 ] ||
  fail_stage verify-topology "census-count-$node_service_count"

receipt[topology_services]=7
receipt[topology_image]=$candidate_ref
write_receipt verify-topology VERIFIED

# ---------------------------------------------------------------------------
# validate-compose
# ---------------------------------------------------------------------------
if ! transport_call validate-compose "$rendered_model"; then
  fail_stage validate-compose compose-failed
fi
write_receipt validate-compose VALIDATED

# ---------------------------------------------------------------------------
# submit-candidate (fake only in #215)
# ---------------------------------------------------------------------------
submit_output=''
if ! submit_output=$(transport_call fake-submit \
  "$rendered_model" "$candidate_ref" "$observed_sha"); then
  fail_stage submit-candidate submission-failed
fi
submission=''
if ! require_single_line "$submit_output" submission; then
  fail_stage submit-candidate multi-line-submission
fi
case "$submission" in
  fake://*) ;;
  *) fail_stage submit-candidate malformed-submission ;;
esac

receipt[submission]=$submission
write_receipt submit-candidate PREPARED

if [ "$mode" != daily ] && [ "$mode" != validation ]; then
  printf 'PREPARED %s %s %s\n' "$observed_sha" "$candidate_ref" "$submission"
  exit 0
fi

# ---------------------------------------------------------------------------
# prepare-consumer: the rendered topology becomes its own testnet directory
# on top of the exact consumer repository main (#216 daily modes only)
# ---------------------------------------------------------------------------
consumer_output=''
if ! consumer_output=$(transport_call prepare-consumer \
  "$day" "$rendered_model" "$candidate_ref" "$consumer_testnet"); then
  fail_stage prepare-consumer consumer-failed
fi
consumer_sha=''
if ! require_single_line "$consumer_output" consumer_sha; then
  fail_stage prepare-consumer multi-line-consumer
fi
[[ "$consumer_sha" =~ ^[0-9a-f]{40}$ ]] ||
  fail_stage prepare-consumer malformed-consumer-sha
receipt[consumer_sha]=$consumer_sha
write_receipt prepare-consumer CONSUMED

# ---------------------------------------------------------------------------
# claim-day: atomic creation of the day ref; at most one attempt per UTC day
# is possible even after a failed or incomplete attempt (I216-02, I216-06)
# ---------------------------------------------------------------------------
claim_verdict=''
if ! claim_verdict=$(transport_call claim-day "$claim_ref" "$consumer_sha"); then
  if [[ "$claim_verdict" =~ ^BLOCKED\ ([a-z-]+)$ ]]; then
    claim_reason=${BASH_REMATCH[1]}
  elif [ -z "$claim_verdict" ]; then
    claim_reason=claim-failed
  else
    claim_reason=malformed-claim-verdict
  fi
  fail_stage claim-day "$claim_reason"
fi
[ "$claim_verdict" = CLAIMED ] ||
  fail_stage claim-day malformed-claim-verdict
write_receipt claim-day CLAIMED

# ---------------------------------------------------------------------------
# construct-request: validate and render the exact dispatch identity before
# any submission operation is invoked (I216-05 request construction)
# ---------------------------------------------------------------------------
[[ "$consumer_repository" =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$ ]] ||
  fail_stage construct-request malformed-repository
request="cardano-node.yaml|${claim_ref#refs/tags/}|$consumer_testnet|$duration|no-faults=false"
receipt[request]=$request
write_receipt construct-request RENDERED

# ---------------------------------------------------------------------------
# submit-run: dispatch the existing MOOG workflow at the immutable claim ref
# (3 h, fault injection on; I216-03)
# ---------------------------------------------------------------------------
run_output=''
if ! run_output=$(transport_call submit-run \
  "$consumer_sha" "$claim_ref" "$consumer_testnet" "$duration" false); then
  fail_stage submit-run dispatch-failed
fi
workflow_run=''
if ! require_single_line "$run_output" workflow_run; then
  fail_stage submit-run multi-line-run-url
fi
[[ "$workflow_run" =~ ^https://github\.com/[^/[:space:]]+/[^/[:space:]]+/actions/runs/[0-9]+$ ]] ||
  fail_stage submit-run malformed-run-url
receipt[workflow_run]=$workflow_run
write_receipt submit-run SUBMITTED

# ---------------------------------------------------------------------------
# await-run: terminal correlation of the MOOG test identity (I216-07)
# ---------------------------------------------------------------------------
correlation_output=''
if ! correlation_output=$(transport_call await-run \
  "$consumer_sha" "$workflow_run"); then
  fail_stage await-run await-failed
fi
correlation=''
if ! require_single_line "$correlation_output" correlation; then
  fail_stage await-run multi-line-correlation
fi
declare -a correlation_fields=()
if ! require_pipe_fields "$correlation" 4 correlation_fields; then
  fail_stage await-run malformed-correlation
fi
moog_test_id=${correlation_fields[0]}
report_url=${correlation_fields[1]}
terminal_outcome=${correlation_fields[2]}
terminal_phase=${correlation_fields[3]}
[[ "$moog_test_id" =~ ^[^[:space:]]+$ ]] ||
  fail_stage await-run malformed-moog-id
[ "$terminal_phase" = finished ] ||
  fail_stage await-run run-not-terminal
case "$terminal_outcome" in
  success | failure) ;;
  *) fail_stage await-run run-not-terminal ;;
esac
[[ "$report_url" =~ ^https://[^/[:space:]]+/.+$|^antithesis://[^[:space:]]+/.+$ ]] ||
  fail_stage await-run malformed-report-url
receipt[moog_test_id]=$moog_test_id
receipt[report_url]=$report_url
receipt[terminal_outcome]=$terminal_outcome
write_receipt await-run TERMINAL

printf 'RUN %s %s %s %s\n' "$day" "$observed_sha" "$consumer_sha" "$terminal_outcome"
