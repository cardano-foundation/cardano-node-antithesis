#!/usr/bin/env bash
# shellcheck disable=SC2154,SC2034,SC2016,SC2015
# Sourced by test-transport-boundary.sh for scenario consumer-observation.
# Reuses the bootstrap observation's stand-ins (fake clock, sleeper, scripted
# `gh`) to drive the real `require-consumer-checks` operation through the
# lifecycle of a freshly created consumer PR.

declare -g workflow_head tmp_root fixture_root transport
declare -g case_root bin effects receipt stdout stderr state

consumer_suites=(
  '1|Build and push component images for cardano-node testnet|publish-images,Compose smoke test'
  '2|tracer-sidecar CI|Build,Run unit Tests,Check code quality'
  '3|Build documentation|build-docs'
  '4|PR preview|preview'
)

# One scripted poll. Arguments are `check=state` overrides over an all-success
# default; state is success|queued|in_progress|failure|absent|dup|stale. The
# variable consumer_gated names a workflow whose run awaits approval and has no
# jobs; `empty` as the only argument is a poll that sees no workflow run at all.
write_consumer_poll() {
  local n=$1 spec suite_spec suite workflow checks name state head sep override
  local -A overrides=()
  shift
  mkdir -p "$obs_root/polls/$n"
  if [ "${1:-}" = empty ]; then
    printf '{"workflow_runs":[]}\n' >"$obs_root/polls/$n/runs.json"
    return 0
  fi
  for spec in "$@"; do
    overrides["${spec%%=*}"]=${spec#*=}
  done
  {
    printf '{"workflow_runs":['
    sep=
    for suite_spec in "${consumer_suites[@]}"; do
      IFS='|' read -r suite workflow _ <<<"$suite_spec"
      if [ "${consumer_gated:-}" = "$workflow" ]; then
        printf '%s{"name":"%s","check_suite_id":%s,"head_sha":"%s","conclusion":"action_required"}' \
          "$sep" "$workflow" "$suite" "$obs_candidate"
      else
        printf '%s{"name":"%s","check_suite_id":%s,"head_sha":"%s"}' \
          "$sep" "$workflow" "$suite" "$obs_candidate"
      fi
      sep=,
    done
    printf ']}\n'
  } >"$obs_root/polls/$n/runs.json"
  for suite_spec in "${consumer_suites[@]}"; do
    IFS='|' read -r suite workflow checks <<<"$suite_spec"
    {
      printf '{"check_runs":['
      sep=
      IFS=',' read -r -a names <<<"$checks"
      for name in "${names[@]}"; do
        override=${overrides[$name]:-success}
        head=$obs_candidate
        case "$override" in
          absent) continue ;;
          stale) head=1111111111111111111111111111111111111111; state=success ;;
          dup)
            printf '%s{"name":"%s","head_sha":"%s","conclusion":"success"}' "$sep" "$name" "$head"
            sep=,
            state=success
            ;;
          *) state=$override ;;
        esac
        case "$state" in
          success | failure | cancelled | skipped | timed_out)
            printf '%s{"name":"%s","head_sha":"%s","conclusion":"%s"}' \
              "$sep" "$name" "$head" "$state"
            ;;
          *)
            printf '%s{"name":"%s","head_sha":"%s","status":"%s","conclusion":null}' \
              "$sep" "$name" "$head" "$state"
            ;;
        esac
        sep=,
      done
      printf ']}\n'
    } >"$obs_root/polls/$n/checks-$suite.json"
  done
}

expected_consumer_rows() {
  local suite_spec suite workflow checks name
  for suite_spec in "${consumer_suites[@]}"; do
    IFS='|' read -r suite workflow checks <<<"$suite_spec"
    IFS=',' read -r -a names <<<"$checks"
    for name in "${names[@]}"; do
      printf '%s|%s|%s|success\n' "$workflow" "$name" "$obs_candidate"
    done
  done
}

run_consumer_obs() {
  local extra_transport=${1:-$transport}
  transport_rc=0
  DAILY_AMARU_DAY=$day \
    DAILY_AMARU_CONSUMER_CHECK_MAX_SECONDS=$obs_ceiling \
    DAILY_AMARU_CONSUMER_CHECK_POLL_SECONDS=1 \
    DAILY_AMARU_IDENTITY=boundary-bootstrap-token \
    GH_TOKEN=boundary-repository-token \
    GIT_CONFIG_NOSYSTEM=1 \
    GIT_CONFIG_GLOBAL=/dev/null \
    DAILY_AMARU_STATE_DIR=$state \
    DAILY_AMARU_RECEIPT=$receipt \
    DAILY_AMARU_BOUNDARY_GH_LOG=$effects \
    DAILY_AMARU_OBSERVATION_ROOT=$obs_root \
    DAILY_AMARU_OBSERVATION_CLOCK=$clock \
    DAILY_AMARU_OBSERVATION_SLEEP_LOG=$sleep_log \
    run_obs_command "$bin" "$extra_transport" require-consumer-checks \
      "$obs_candidate" >"$stdout" 2>"$stderr" || transport_rc=$?
}

expect_consumer_failure() {
  local label=$1 fingerprint=$2 polls=$3
  [ "$transport_rc" -ne 0 ] || fail "consumer $label was accepted"
  grep -Fq -- "$fingerprint" "$stderr" ||
    fail "consumer $label failed for the wrong reason: $(tr '\n' ' ' <"$stderr")"
  [ ! -s "$stdout" ] || fail "consumer $label emitted success rows"
  [ "$(count_head_polls)" -eq "$polls" ] ||
    fail "consumer $label polled $(count_head_polls) times, expected $polls"
  assert_no_real_effects
}

assert_consumer_observation() {
  local n

  # Initially absent, then queued, then running, then every required check.
  setup_obs consumer-lifecycle
  write_consumer_poll 1 empty
  write_consumer_poll 2 publish-images=queued 'Compose smoke test=queued' Build=absent 'Run unit Tests=absent' 'Check code quality=absent' build-docs=absent preview=absent
  write_consumer_poll 3 publish-images=in_progress 'Compose smoke test=success' Build=in_progress 'Run unit Tests=queued' 'Check code quality=success' build-docs=in_progress preview=queued
  write_consumer_poll 4
  run_consumer_obs
  [ "$transport_rc" -eq 0 ] ||
    fail "consumer lifecycle did not reach success: $(tr '\n' ' ' <"$stderr")"
  expected_consumer_rows >"$case_root/expected"
  cmp -s "$case_root/expected" "$stdout" ||
    fail 'consumer lifecycle emitted non-exact success rows'
  grep -Fq 'consumer-check-observation polls=4' "$stderr" ||
    fail 'consumer lifecycle did not report four polls'
  [ "$(count_head_polls)" -eq 4 ] || fail 'consumer lifecycle polled a wrong number of times'
  [ "$(slept_total)" -eq 3 ] || fail 'consumer lifecycle did not wait between polls'
  assert_no_real_effects

  # A terminal failure ends the wait at the first poll that shows it.
  setup_obs consumer-failure
  write_consumer_poll 1 'Run unit Tests=failure' preview=queued
  write_consumer_poll 2
  run_consumer_obs
  expect_consumer_failure terminal-failure 'consumer check failed on' 1
  [ "$(slept_total)" -eq 0 ] || fail 'consumer terminal failure waited'

  # Skipped and cancelled required jobs are terminal failures, not pending.
  setup_obs consumer-skipped
  write_consumer_poll 1 build-docs=skipped
  run_consumer_obs
  expect_consumer_failure skipped-required-job 'consumer check failed on' 1

  # A run waiting for approval has no jobs: only the run can say so, at once.
  setup_obs consumer-approval-gate
  consumer_gated='PR preview' write_consumer_poll 1 preview=absent
  write_consumer_poll 2
  run_consumer_obs
  expect_consumer_failure approval-gate 'awaits approval (action_required)' 1

  # Rows for another head never satisfy the exact candidate.
  setup_obs consumer-wrong-head
  for n in 1 2 3 4 5 6 7 8 9 10 11 12; do
    write_consumer_poll "$n" publish-images=stale
  done
  run_consumer_obs
  [ "$transport_rc" -ne 0 ] || fail 'consumer stale head was accepted'
  grep -Fq 'never-reported' "$stderr" ||
    fail "consumer stale head was not reported absent: $(tr '\n' ' ' <"$stderr")"

  # A duplicate success is ambiguity, not success.
  setup_obs consumer-duplicate
  write_consumer_poll 1 preview=dup
  write_consumer_poll 2
  run_consumer_obs
  expect_consumer_failure duplicate-success 'consumer check failed on' 1

  # A complete set with one check never appearing fails at the ceiling.
  setup_obs consumer-never-reported
  for n in 1 2 3 4 5 6 7 8 9 10 11 12; do
    write_consumer_poll "$n" preview=absent
  done
  run_consumer_obs
  [ "$transport_rc" -ne 0 ] || fail 'consumer incomplete required set was accepted'
  grep -Fq 'never-reported on' "$stderr" && grep -Fq 'PR preview / preview' "$stderr" ||
    fail "consumer incomplete set was not named: $(tr '\n' ' ' <"$stderr")"

  # Finite timeout: a check that stays running is a failure at the ceiling.
  setup_obs consumer-timeout
  for n in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
    write_consumer_poll "$n" Build=in_progress
  done
  run_consumer_obs
  [ "$transport_rc" -ne 0 ] || fail 'consumer still-running check was accepted'
  grep -Fq 'still-running on' "$stderr" && grep -Fq "ceiling=$obs_ceiling" "$stderr" ||
    fail "consumer timeout lacks diagnostics: $(tr '\n' ' ' <"$stderr")"
  [ "$(slept_total)" -le "$obs_ceiling" ] ||
    fail 'consumer timeout waited past the ceiling'
  [ "$(count_head_polls)" -le $((obs_ceiling + 1)) ] ||
    fail 'consumer timeout polled without bound'

  # A transport error is a failure at once, naming the boundary.
  setup_obs consumer-transport
  write_consumer_poll 1 empty
  write_consumer_poll 2
  : >"$obs_root/fault-1"
  run_consumer_obs
  expect_consumer_failure transport-error 'consumer check transport-failed on' 1

  # The candidate must be an exact commit.
  setup_obs consumer-bad-candidate
  obs_candidate=not-a-sha run_consumer_obs
  [ "$transport_rc" -ne 0 ] && grep -Fq 'invalid consumer candidate' "$stderr" ||
    fail 'consumer observation accepted a malformed candidate'
  [ "$(count_head_polls)" -eq 0 ] || fail 'malformed candidate reached the API'

  printf 'CONSUMER-OBSERVATION lifecycle=4polls failure=1 skipped=1 approval=1 wrong_head=1 duplicate=1 incomplete=1 timeout=finite transport=1\n'
}

assert_consumer_observation_mutants() {
  local mutant_root="$tmp_root/consumer-observation-mutants" mutant
  mkdir -p "$mutant_root"
  # shellcheck disable=SC2016
  mutant="$mutant_root/immediate.sh"
  sed 's#^    observe_consumer_checks "\$candidate" "\$repository_identity"#    die "consumer check is not uniquely successful on $candidate: immediate"#' \
    "$transport" >"$mutant"
  chmod +x "$mutant"
  # shellcheck disable=SC2016
  ! grep -Fq 'observe_consumer_checks "$candidate" "$repository_identity"' "$mutant" ||
    fail 'immediate-read consumer mutation did not apply'
  reject_scenario_mutant consumer-immediate "$mutant" consumer-observation \
    'consumer lifecycle did not reach success'

  mutant="$mutant_root/stale-head.sh"
  sed 's#^    \[ "\$head" = "\$candidate" \] || continue#    :#' "$transport" >"$mutant"
  chmod +x "$mutant"
  # shellcheck disable=SC2016
  ! grep -Fq '[ "$head" = "$candidate" ] || continue' "$mutant" ||
    fail 'stale-head consumer mutation did not apply'
  reject_scenario_mutant consumer-stale-head "$mutant" consumer-observation \
    'consumer stale head was accepted'

  mutant="$mutant_root/duplicate.sh"
  sed 's#\[ "\$failed" -gt 0 \] || \[ "\$success" -gt 1 \]#[ "$failed" -gt 0 ]#' \
    "$transport" >"$mutant"
  chmod +x "$mutant"
  # shellcheck disable=SC2016
  ! grep -Fq '[ "$failed" -gt 0 ] || [ "$success" -gt 1 ]' "$mutant" ||
    fail 'duplicate-success consumer mutation did not apply'
  reject_scenario_mutant consumer-duplicate "$mutant" consumer-observation \
    'consumer duplicate-success was accepted'

  mutant="$mutant_root/no-gate.sh"
  sed 's#^    \[ -z "\$gated" \] ||#    :||#' "$transport" >"$mutant"
  chmod +x "$mutant"
  # shellcheck disable=SC2016
  ! grep -Fq '[ -z "$gated" ] ||' "$mutant" ||
    fail 'approval-gate consumer mutation did not apply'
  reject_scenario_mutant consumer-no-gate "$mutant" consumer-observation \
    'consumer approval-gate was accepted'
  printf 'CONSUMER-OBSERVATION-MUTANTS rejected=4\n'
}
