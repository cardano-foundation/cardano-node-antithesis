#!/usr/bin/env python3
"""anytime_govstate_invariant.py — gov-state well-formedness invariant.

May run at any time, including under fault injection. Whenever the node
answers, its governance state must parse and expose a proposals array.
A missing node (mid-fault) is absorbed, not flagged.
"""

from __future__ import annotations

import sys

import helper_gov as g
import helper_sdk as sdk


def main() -> int:
    sdk.reachable("govstate_invariant entered")
    cluster = g.make_cluster()
    try:
        gov_state = cluster.g_query.get_gov_state()
    except Exception:  # noqa: BLE001
        sdk.unreachable("govstate_unavailable")
        return 0

    sdk.always(isinstance(gov_state, dict) and "proposals" in gov_state, "govstate_well_formed")

    # Invariant: once setup has authorized the committee, its quorum must
    # survive fault injection (CC auth is on-chain state, not a container,
    # so killing producers must never drop authorized members below
    # minSize). Bash counts `committee-state --active`, i.e. members whose
    # status is Active; mirror that against the full committee-state.
    if g.SETUP_MARKER.exists():
        try:
            cs = cluster.g_query.get_committee_state()
            members = (cs or {}).get("committee", {}) or {}
            authorized = sum(
                1 for m in members.values() if (m or {}).get("status") == "Active"
            )
            # committeeMinSize is 2 in the seeded Conway genesis.
            sdk.always(authorized >= 2, "committee_quorum_maintained", {"authorized": authorized, "min": 2})
        except Exception:  # noqa: BLE001
            pass

    # Lifecycle coverage (stateless, derived from gov-state): an action is
    # in its final epoch of life when expiresAfter == the current epoch.
    # Seeing this green proves the run lasted long enough for actions to
    # reach the end of their govActionLifetime — the ledger owns the
    # lifecycle, we just observe it.
    try:
        ep = cluster.g_query.get_epoch()
        near = sum(
            1
            for p in (gov_state.get("proposals", []) or [])
            if p.get("expiresAfter") == ep
        )
        sdk.sometimes(near >= 1, "action_near_expiry", {"near": near, "epoch": ep})
    except Exception:  # noqa: BLE001
        pass
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as exc:  # noqa: BLE001
        print(f"govstate_invariant aborted: {exc}", file=sys.stderr)
        sdk.unreachable("govstate_invariant_aborted")
        sys.exit(0)
