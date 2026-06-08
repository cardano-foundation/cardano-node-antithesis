#!/usr/bin/env python3
"""first_setup.py — one-shot governance setup phase (Python).

`first_` runs after setup_complete, before any driver, with NO fault
injection. Brings the committee + DReps on-chain so the create/vote
drivers have real voters. Submits cardonnay's pre-generated certs via
cardano-clusterlib: CC hot-key authorizations, DRep registrations, and
vote-stake registration + delegation (funding each vote-stake address),
then waits one epoch for the DRep stake distribution to go live.
"""

from __future__ import annotations

import sys
import time

import helper_gov as g
import helper_sdk as sdk
from cardano_clusterlib import clusterlib

DREP_DELEGATED = 500_000_000_000


def main() -> int:
    sdk.reachable("first_setup entered")
    g.ensure_dirs()

    if g.SETUP_MARKER.exists():
        sdk.sometimes(True, "governance_setup_already_done")
        return 0

    cluster = g.make_cluster()
    if not g.wait_for_node(cluster):
        sdk.unreachable("setup_node_not_ready")
        return 1

    cert_files: list = []
    signing: list = []
    txouts: list = []

    for i in range(1, g.NUM_DREPS + 1):
        addr = (g.GD / f"vote_stake_addr{i}.addr").read_text().strip()
        cert_files += [
            g.GD / f"default_drep_{i}_drep_reg.cert",
            g.GD / f"vote_stake_addr{i}_stake.reg.cert",
            g.GD / f"vote_stake_addr{i}_stake.vote_deleg.cert",
        ]
        signing += [
            g.GD / f"default_drep_{i}_drep.skey",
            g.GD / f"vote_stake_addr{i}.skey",
            g.GD / f"vote_stake_addr{i}_stake.skey",
        ]
        txouts.append(clusterlib.TxOut(address=addr, amount=DREP_DELEGATED))

    for i in range(1, g.NUM_CC + 1):
        auth = g.GD / f"cc_member{i}_committee_hot_auth.cert"
        if not auth.exists():
            continue
        cert_files.append(auth)
        signing.append(g.GD / f"cc_member{i}_committee_cold.skey")

    try:
        txid = g.build_sign_submit(
            cluster,
            "setup_register",
            certificate_files=cert_files,
            signing_key_files=signing,
            txouts=txouts,
        )
    except Exception as exc:  # noqa: BLE001
        print(f"registration tx failed: {exc}", file=sys.stderr)
        sdk.unreachable("setup_registration_failed")
        return 1

    print(f"registration tx submitted: {txid}", file=sys.stderr)
    sdk.sometimes(True, "governance_registration_submitted")

    # DRep stake delegation takes effect at the next epoch boundary.
    try:
        start_epoch = g.current_epoch(cluster)
        # Epochs are ~16.7 min (epochLength 5000 × 0.2s), so allow > 1 epoch.
        g.wait_for_epoch(cluster, start_epoch + 1, 1800)
    except Exception:  # noqa: BLE001
        pass

    cc_active = 0
    try:
        cc_state = cluster.g_query.get_committee_state()
        members = (cc_state.get("committee") or cc_state.get("commitee") or {}).get("members", {})
        cc_active = sum(
            1
            for m in members.values()
            if m.get("hotCredsAuthStatus", {}).get("tag") == "MemberAuthorized"
        )
    except Exception:  # noqa: BLE001
        pass
    sdk.sometimes(cc_active >= 1, "committee_active_after_setup")

    dreps = 0
    try:
        dreps = len(cluster.g_query.get_drep_state() or [])
    except Exception:  # noqa: BLE001
        pass
    sdk.sometimes(dreps >= 1, "dreps_registered_after_setup")

    g.SETUP_MARKER.touch()
    sdk.sometimes(True, "governance_setup_complete")
    print(f"setup complete (cc_active={cc_active} dreps={dreps})", file=sys.stderr)
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as exc:  # noqa: BLE001
        # Absorb fault-induced failures into a coverage signal, exit 0.
        print(f"setup aborted: {exc}", file=sys.stderr)
        sdk.unreachable("setup_aborted")
        sys.exit(0)
