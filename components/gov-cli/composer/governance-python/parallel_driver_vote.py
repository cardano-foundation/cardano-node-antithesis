#!/usr/bin/env python3
"""parallel_driver_vote.py — cast one DRep/SPO/CC vote on one action.

One logical cardano-cli governance operation = one driver. Picks an
InfoAction from the create driver's published set (created.log MINUS
rejected.log) and submits ONE vote from ONE random voter — a random
DRep, stake pool (SPO) or constitutional committee (CC) member — with a
random decision (yes / no / abstain), using cardano-clusterlib's
g_governance.vote.create_{drep,spo,committee}. Every choice is steered
by the Antithesis RNG so the hypervisor explores the (action, voter,
decision) space. A vote that is rejected retires the action.
"""

from __future__ import annotations

import os
import sys
import time

import helper_gov as g
import helper_sdk as sdk
from cardano_clusterlib import clusterlib


def build_voters():
    """Build the voter roster: every DRep, SPO and CC member. Each entry
    is (kind, create_fn, vkey_kw, vkey_file, skey_file)."""
    voters = []
    for i in range(1, g.NUM_DREPS + 1):
        vkey = g.GD / f"default_drep_{i}_drep.vkey"
        skey = g.GD / f"default_drep_{i}_drep.skey"
        if vkey.exists():
            voters.append(("drep", "create_drep", "drep_vkey_file", vkey, skey))
    for i in range(1, g.NUM_POOLS + 1):
        vkey = g.GOV / "pools" / f"node-pool{i}" / "cold.vkey"
        skey = g.GOV / "pools" / f"node-pool{i}" / "cold.skey"
        if vkey.exists():
            voters.append(("spo", "create_spo", "cold_vkey_file", vkey, skey))
    for i in range(1, g.NUM_CC + 1):
        vkey = g.GD / f"cc_member{i}_committee_hot.vkey"
        skey = g.GD / f"cc_member{i}_committee_hot.skey"
        if vkey.exists():
            voters.append(("cc", "create_committee", "cc_hot_vkey_file", vkey, skey))
    return voters


def main() -> int:
    sdk.reachable("vote entered")
    g.ensure_dirs()

    if not g.SETUP_MARKER.exists():
        return 0

    cluster = g.make_cluster()
    if not g.wait_for_node(cluster, tries=30):
        sdk.unreachable("vote_node_not_ready")
        return 0

    # Work the set difference (created MINUS rejected) and let Antithesis'
    # RNG steer every choice: which action, which voter, and the decision.
    pend = g.pending_actions()
    if not pend:
        print("no pending actions", file=sys.stderr)
        return 0

    txid, ix = pend[g.rng_mod(len(pend))]
    if not txid:
        return 0

    # Retire it if it is no longer on-chain (expired/decided); another
    # invocation will pick a different one.
    if not g.action_onchain(cluster, txid):
        print(f"action {txid} no longer on-chain; retiring", file=sys.stderr)
        g.record_rejected(txid)
        sdk.sometimes(True, "stale_action_retired")
        return 0

    voters = build_voters()
    if not voters:
        sdk.sometimes(False, "votes_submitted")
        return 0

    kind, create_name, vkey_kw, vkey_file, skey_file = voters[g.rng_mod(len(voters))]

    # RNG-select the decision: yes, no or abstain.
    decisions = [
        (clusterlib.Votes.YES, "yes"),
        (clusterlib.Votes.NO, "no"),
        (clusterlib.Votes.ABSTAIN, "abstain"),
    ]
    vote_enum, decision = decisions[g.rng_mod(3)]

    tok = f"{int(time.time())}_{os.getpid()}_{time.time_ns() % 100000}"
    print(f"voting {decision} as {kind} on {txid}#{ix}", file=sys.stderr)

    create_fn = getattr(cluster.g_governance.vote, create_name)
    vote = create_fn(
        vote_name=f"{kind}_{tok}",
        action_txid=txid,
        action_ix=ix,
        vote=vote_enum,
        destination_dir=str(g.WORK),
        **{vkey_kw: vkey_file},
    )

    try:
        g.build_sign_submit(
            cluster,
            f"vote_{tok}",
            vote_files=[vote.vote_file],
            signing_key_files=[skey_file],
        )
    except Exception as exc:  # noqa: BLE001
        # Vote rejected (expired / already decided / invalid) — retire the
        # action so the created-minus-rejected difference stops offering it.
        print(f"vote rejected for {txid}: {exc}; retiring", file=sys.stderr)
        g.record_rejected(txid)
        sdk.sometimes(False, "votes_submitted")
        return 0

    # Verify the vote landed and emit per-kind / per-decision coverage.
    total = 0
    try:
        prop = g.lookup_proposal(cluster.g_query.get_gov_state(), txid) or {}
        total = (
            len(prop.get("dRepVotes") or {})
            + len(prop.get("stakePoolVotes") or {})
            + len(prop.get("committeeVotes") or {})
        )
    except Exception:  # noqa: BLE001
        pass

    sdk.sometimes(total >= 1, f"vote_recorded_{kind}")
    sdk.sometimes(True, f"vote_decision_{decision}")
    sdk.sometimes(True, "votes_submitted")

    print(
        f"vote submitted ({kind} {decision}; action now has {total} votes)",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as exc:  # noqa: BLE001
        print(f"vote aborted: {exc}", file=sys.stderr)
        sdk.unreachable("vote_aborted")
        sys.exit(0)
