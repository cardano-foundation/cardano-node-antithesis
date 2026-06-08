#!/usr/bin/env python3
"""anytime_chain_progress.py — the perturbation witness (Python).

gov-cli talks to the fault-excluded relay1, but relay1's TIP reflects
p1/p2, which ARE under fault injection. So sampling whether the tip
advances over a short window tells us whether the faults actually halted
block production. This is the signal that answers "did we perturb
governance enough":
  chain_stalled_under_fault  green => faults stopped block production at
                                      least once (perturbation bit)
  chain_producing            green => and the chain also produced
                                      (so it recovers)
If chain_stalled_under_fault never goes green, the run never really
exercised governance under a degraded chain.

Runs as anytime_ (continuous, faults on). Self-contained: it measures a
slot delta within one invocation, so there is no cross-instance race on
the sample itself; it only publishes a verdict the create/vote drivers
read for the gov_op_under_perturbation coverage.
"""

from __future__ import annotations

import os
import sys
import time

import helper_gov as g
import helper_sdk as sdk

WINDOW = int(os.environ.get("CHAIN_PROBE_WINDOW", "20"))


def main() -> int:
    sdk.reachable("chain_progress entered")
    g.ensure_dirs()

    cluster = g.make_cluster()

    # Relay unreachable on either sample -> not a stall verdict; the relay
    # is fault-excluded so this is rare (cold start), absorb it.
    try:
        s1 = cluster.g_query.get_slot_no()
        time.sleep(WINDOW)
        s2 = cluster.g_query.get_slot_no()
    except Exception:  # noqa: BLE001
        sdk.unreachable("chain_progress_relay_unreachable")
        return 0

    if not s1 or not s2:
        sdk.unreachable("chain_progress_relay_unreachable")
        return 0

    ds = s2 - s1
    if ds <= 0:
        g.set_chain_verdict("stalled")
        sdk.sometimes(True, "chain_stalled_under_fault", {"window_s": WINDOW, "slot": s2})
    else:
        g.set_chain_verdict("producing")
        sdk.sometimes(True, "chain_producing", {"blocks": ds, "window_s": WINDOW})

    # relay1 is excluded from faults, so it must keep answering tip queries.
    sdk.always(True, "relay_reachable_under_fault")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as exc:  # noqa: BLE001
        print(f"chain_progress aborted: {exc}", file=sys.stderr)
        sdk.unreachable("chain_progress_aborted")
        sys.exit(0)
