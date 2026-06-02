#!/usr/bin/env python3
"""parallel_driver_vote.py — cast DRep + SPO + CC votes (Python).

One logical cardano-cli governance operation = one driver. Claims a
pending InfoAction and submits YES votes from every DRep, every stake
pool (SPO), and every constitutional committee member, using
cardano-clusterlib's g_governance.vote.create_{drep,spo,committee}.
"""

from __future__ import annotations

import os
import sys
import time

import helper_gov as g
import helper_sdk as sdk
from cardano_clusterlib import clusterlib


def main() -> int:
    sdk.reachable("vote entered")
    g.ensure_dirs()

    if not g.SETUP_MARKER.exists():
        return 0

    cluster = g.make_cluster()
    if not g.wait_for_node(cluster, tries=30):
        sdk.unreachable("vote_node_not_ready")
        return 0

    claim = g.claim_pending_action()
    if not claim:
        return 0
    txid, ix, action_path = claim
    tok = f"{int(time.time())}_{os.getpid()}_{time.time_ns() % 100000}"
    print(f"voting on {txid}#{ix}", file=sys.stderr)

    votes: list = []
    signing: list = []

    for i in range(1, g.NUM_DREPS + 1):
        votes.append(
            cluster.g_governance.vote.create_drep(
                vote_name=f"drep{i}_{tok}",
                action_txid=txid,
                action_ix=ix,
                vote=clusterlib.Votes.YES,
                drep_vkey_file=g.GD / f"default_drep_{i}_drep.vkey",
                destination_dir=str(g.WORK),
            )
        )
        signing.append(g.GD / f"default_drep_{i}_drep.skey")

    for i in range(1, g.NUM_POOLS + 1):
        cold_v = g.GOV / "pools" / f"node-pool{i}" / "cold.vkey"
        if not cold_v.exists():
            continue
        votes.append(
            cluster.g_governance.vote.create_spo(
                vote_name=f"spo{i}_{tok}",
                action_txid=txid,
                action_ix=ix,
                vote=clusterlib.Votes.YES,
                cold_vkey_file=cold_v,
                destination_dir=str(g.WORK),
            )
        )
        signing.append(g.GOV / "pools" / f"node-pool{i}" / "cold.skey")

    for i in range(1, g.NUM_CC + 1):
        hot_v = g.GD / f"cc_member{i}_committee_hot.vkey"
        if not hot_v.exists():
            continue
        votes.append(
            cluster.g_governance.vote.create_committee(
                vote_name=f"cc{i}_{tok}",
                action_txid=txid,
                action_ix=ix,
                vote=clusterlib.Votes.YES,
                cc_hot_vkey_file=hot_v,
                destination_dir=str(g.WORK),
            )
        )
        signing.append(g.GD / f"cc_member{i}_committee_hot.skey")

    if not votes:
        sdk.sometimes(False, "votes_submitted")
        return 0

    try:
        vote_files = [v.vote_file for v in votes]
        g.build_sign_submit(
            cluster, f"vote_{tok}", vote_files=vote_files, signing_key_files=signing
        )
    except Exception as exc:  # noqa: BLE001
        print(f"vote submit failed: {exc}", file=sys.stderr)
        sdk.sometimes(False, "votes_submitted")
        return 0

    drep_n = spo_n = cc_n = 0
    try:
        prop = g.lookup_proposal(cluster.g_query.get_gov_state(), txid) or {}
        drep_n = len(prop.get("dRepVotes") or {})
        spo_n = len(prop.get("stakePoolVotes") or {})
        cc_n = len(prop.get("committeeVotes") or {})
    except Exception:  # noqa: BLE001
        pass

    sdk.sometimes(drep_n >= 1, "drep_votes_recorded")
    sdk.sometimes(spo_n >= 1, "spo_votes_recorded")
    sdk.sometimes(cc_n >= 1, "cc_votes_recorded")
    sdk.sometimes(True, "votes_submitted")

    try:
        action_path.rename(g.STATE_DIR / f"voted_{txid}.json")
    except Exception:  # noqa: BLE001
        pass
    print(f"votes submitted (drep={drep_n} spo={spo_n} cc={cc_n})", file=sys.stderr)
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as exc:  # noqa: BLE001
        print(f"vote aborted: {exc}", file=sys.stderr)
        sdk.unreachable("vote_aborted")
        sys.exit(0)
