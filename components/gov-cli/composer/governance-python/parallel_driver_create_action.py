#!/usr/bin/env python3
"""parallel_driver_create_action.py — submit one InfoAction (Python).

One logical cardano-cli governance operation = one driver. Uses
cardano-clusterlib's g_governance.action.create_info to build an info
action (never enacts -> unbounded workload) and submits it. Stateless:
nothing is published locally — the vote driver finds votable actions
straight from gov-state, so a transient submit failure is harmless (a
later tick creates another action).
"""

from __future__ import annotations

import os
import sys
import time

import helper_gov as g
import helper_sdk as sdk


def main() -> int:
    sdk.reachable("create_action entered")
    g.ensure_dirs()

    if not g.SETUP_MARKER.exists():
        return 0

    cluster = g.make_cluster()
    if not g.wait_for_node(cluster, tries=30):
        sdk.unreachable("create_action_node_not_ready")
        return 0

    tok = f"{int(time.time())}_{os.getpid()}_{time.time_ns() % 100000}"
    deposit = cluster.g_query.get_gov_action_deposit()
    anchor_hash = cluster.g_governance.get_anchor_data_hash(text=g.ANCHOR_TEXT)

    info = cluster.g_governance.action.create_info(
        action_name=f"info_{tok}",
        deposit_amt=deposit,
        anchor_url=g.ANCHOR_URL,
        anchor_data_hash=anchor_hash,
        deposit_return_stake_vkey_file=g.GD / "vote_stake_addr1_stake.vkey",
        destination_dir=str(g.WORK),
    )

    try:
        txid = g.build_sign_submit(cluster, f"info_{tok}", proposal_files=[info.action_file])
    except Exception as exc:  # noqa: BLE001
        # Transient failure (faucet-lock timeout / stalled submit) —
        # harmless, a later tick creates another action. No local state to
        # update; the vote driver finds actions straight from gov-state.
        print(f"info action submit failed transiently: {exc} (will retry)", file=sys.stderr)
        sdk.sometimes(False, "info_action_created")
        return 0

    print(f"info action created: {txid}", file=sys.stderr)
    sdk.sometimes(True, "info_action_created")

    # Perturbation coverage: this action was created while the chain was
    # recently stalled by faults.
    if g.recent_stall(90):
        sdk.sometimes(True, "gov_op_under_perturbation", {"op": "create"})
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as exc:  # noqa: BLE001
        print(f"create_action aborted: {exc}", file=sys.stderr)
        sdk.unreachable("create_action_aborted")
        sys.exit(0)
