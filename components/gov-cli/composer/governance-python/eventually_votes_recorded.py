#!/usr/bin/env python3
"""eventually_votes_recorded.py — short post-fault recovery validation.

`eventually_` runs after a driver has started; Antithesis stops faults
and kills other drivers when it fires. Single-shot (not a polling
loop): settle, then assert at least one action carries DRep + SPO
votes. Cold-start guarded.
"""

from __future__ import annotations

import sys
import time

import helper_gov as g
import helper_sdk as sdk


def main() -> int:
    sdk.reachable("eventually_votes entered")
    time.sleep(15)  # let the chain settle after faults stop

    cluster = g.make_cluster()
    if not g.wait_for_node(cluster, tries=30):
        sdk.unreachable("eventually_cold_start")
        return 0

    try:
        gov_state = cluster.g_query.get_gov_state()
    except Exception:  # noqa: BLE001
        sdk.unreachable("eventually_cold_start")
        return 0

    voted = 0
    for prop in gov_state.get("proposals", []) or []:
        if len(prop.get("dRepVotes") or {}) >= 1 and len(prop.get("stakePoolVotes") or {}) >= 1:
            voted += 1

    sdk.sometimes(voted >= 1, "action_fully_voted_after_recovery")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as exc:  # noqa: BLE001
        print(f"eventually aborted: {exc}", file=sys.stderr)
        sdk.unreachable("eventually_aborted")
        sys.exit(0)
