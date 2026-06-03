#!/usr/bin/env bash
# anytime_chain_progress.sh — the perturbation witness.
#
# gov-cli talks to the fault-excluded relay1, but relay1's TIP reflects
# p1/p2, which ARE under fault injection. So sampling whether the tip
# advances over a short window tells us whether the faults actually
# halted block production. This is the signal that answers "did we
# perturb governance enough":
#   chain_stalled_under_fault  green => faults stopped block production
#                                       at least once (perturbation bit)
#   chain_producing            green => and the chain also produced
#                                       (so it recovers)
# If chain_stalled_under_fault never goes green, the run never really
# exercised governance under a degraded chain.
#
# Runs as anytime_ (continuous, faults on). Self-contained: it measures
# a slot delta within one invocation, so there is no cross-instance race
# on the sample itself; it only publishes a verdict the create/vote
# drivers read for the gov_op_after_stall coverage.

set -u
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_sdk.sh"
# shellcheck disable=SC1091
source "$(dirname "$0")/helper_gov.sh"

readonly WINDOW="${CHAIN_PROBE_WINDOW:-20}"

sdk_reachable "chain_progress entered"
mkdir -p "$STATE_DIR"

s1="$(current_slot)"
sleep "$WINDOW"
s2="$(current_slot)"

# Relay unreachable on either sample -> not a stall verdict; the relay is
# fault-excluded so this is rare (cold start), absorb it.
if [ -z "$s1" ] || [ -z "$s2" ] || [ "$s1" = "0" ] || [ "$s2" = "0" ]; then
    sdk_unreachable "chain_progress_relay_unreachable"
    exit 0
fi

ds=$(( s2 - s1 ))
if [ "$ds" -le 0 ]; then
    set_chain_verdict stalled
    sdk_sometimes true "chain_stalled_under_fault" \
        "$(jq -nc --argjson w "$WINDOW" --argjson s "$s2" '{window_s:$w, slot:$s}')"
else
    set_chain_verdict producing
    sdk_sometimes true "chain_producing" \
        "$(jq -nc --argjson b "$ds" --argjson w "$WINDOW" '{blocks:$b, window_s:$w}')"
fi

# relay1 is excluded from faults, so it must keep answering tip queries.
sdk_always true "relay_reachable_under_fault"
exit 0
