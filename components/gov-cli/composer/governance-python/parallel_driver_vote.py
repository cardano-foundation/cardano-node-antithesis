#!/usr/bin/env python3
"""parallel_driver_vote.py — cast one DRep/SPO/CC vote on one live action.

Stateless: the votable set comes straight from the chain via an N2C
gov-state query (relay1 is fault-excluded, so the query answers even
under a block-production stall). There is NO local created/rejected
ledger — an action leaves the set only when the LEDGER expires or enacts
it, so a transient submit/lock failure is self-healing (the action is
simply picked again next tick). Submits ONE vote from ONE random voter —
a random DRep, stake pool (SPO) or constitutional committee (CC) member —
with a random decision (yes / no / abstain), using cardano-clusterlib's
g_governance.vote.create_{drep,spo,committee}. The Antithesis RNG steers
every choice: which live action, which voter, and yes/no/abstain.
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

    # Pull the live InfoAction set from gov-state and RNG-select one. The
    # set is stateless and self-healing: an action only leaves it when the
    # ledger expires/enacts it, never on a transient local failure.
    props = g.live_info_actions(cluster)
    sdk.sometimes(len(props) >= 1, "actions_live", {"live": len(props)})
    if not props:
        print("no live actions in gov-state", file=sys.stderr)
        return 0

    pick = props[g.rng_mod(len(props))]
    txid = pick["actionId"]["txId"]
    ix = pick["actionId"]["govActionIx"]

    voters = build_voters()
    if not voters:
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
        # Transient failure (faucet-lock timeout / stalled submit). The
        # action is still in gov-state, so we do NOT retire it — a later
        # tick simply picks it again. Self-healing by construction; just
        # record coverage.
        print(f"vote submit failed transiently for {txid}: {exc} (will retry)", file=sys.stderr)
        sdk.sometimes(True, "vote_transient_failure", {"op": "vote", "voter": kind})
        return 0
    sdk.reachable("vote_submitted")

    # Coverage from the on-chain vote breakdown after this vote landed.
    drep_n = spo_n = cc_n = 0
    try:
        prop = g.lookup_proposal(cluster.g_query.get_gov_state(), txid) or {}
        drep_n = len(prop.get("dRepVotes") or {})
        spo_n = len(prop.get("stakePoolVotes") or {})
        cc_n = len(prop.get("committeeVotes") or {})
    except Exception:  # noqa: BLE001
        pass
    total = drep_n + spo_n + cc_n
    majority = (g.NUM_DREPS + g.NUM_POOLS + g.NUM_CC + 1) // 2

    # This vote was cast by $kind/$decision; per-role + per-decision coverage.
    sdk.sometimes(total >= 1, f"vote_recorded_{kind}")
    sdk.sometimes(True, f"vote_decision_{decision}")

    # Quorum-distribution coverage: an action voted by all three roles, and
    # one that crossed a majority of all eligible voters.
    all_roles = drep_n >= 1 and spo_n >= 1 and cc_n >= 1
    sdk.sometimes(
        all_roles, "action_voted_by_all_roles", {"drep": drep_n, "spo": spo_n, "cc": cc_n}
    )
    sdk.sometimes(
        total >= majority, "action_majority_reached", {"total": total, "majority": majority}
    )

    # Perturbation coverage: this vote landed while the chain was recently
    # stalled by faults (block production halted, yet governance progressed).
    if g.recent_stall(90):
        sdk.sometimes(True, "gov_op_under_perturbation", {"op": "vote", "voter": kind})

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
