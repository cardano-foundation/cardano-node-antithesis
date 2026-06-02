#!/usr/bin/env python3
"""helper_gov.py — shared cardano governance helpers (Python).

`helper_`-prefixed: ignored by the Antithesis composer scheduler.
Wraps the standalone `cardano-clusterlib` library (which itself wraps
cardano-cli) — NOT the cardano-node-tests harness. The governance
assets are produced by the gov-configurator (cardonnay's conway_fast
genesis + governance_data) and mounted at $GOV; the cluster's network
parameters come from the genesis state dir at $GOV_STATE_DIR.
"""

from __future__ import annotations

import contextlib
import fcntl
import os
import pathlib
import time

from cardano_clusterlib import clusterlib

GOV = pathlib.Path(os.environ.get("GOV", "/gov-data"))
# State dir holding shelley/genesis.json so clusterlib can read the
# network magic / slot params. Populated by the gov-configurator.
GOV_STATE_DIR = pathlib.Path(os.environ.get("GOV_STATE_DIR", "/gov-state"))
SOCKET = os.environ.get("CARDANO_NODE_SOCKET_PATH", "/state/node.socket")
WORK = pathlib.Path(os.environ.get("WORK", "/work"))
NUM_DREPS = int(os.environ.get("NUM_DREPS", "5"))
NUM_CC = int(os.environ.get("NUM_CC", "5"))
NUM_POOLS = int(os.environ.get("NUM_POOLS", "2"))

GD = GOV / "governance_data"
STATE_DIR = GOV / "state"
ACTIONS_DIR = STATE_DIR / "actions"
SETUP_MARKER = STATE_DIR / "setup_done"
FAUCET_LOCK = STATE_DIR / "faucet.lock"

ANCHOR_URL = "https://example.com/governance.json"
ANCHOR_TEXT = '{"body":{"title":"antithesis governance workload"}}'


def ensure_dirs() -> None:
    for d in (WORK, STATE_DIR, ACTIONS_DIR):
        d.mkdir(parents=True, exist_ok=True)


def make_cluster() -> clusterlib.ClusterLib:
    return clusterlib.ClusterLib(
        state_dir=str(GOV_STATE_DIR),
        socket_path=SOCKET,
        command_era="conway",
    )


def faucet() -> clusterlib.AddressRecord:
    addr = (GOV / "faucet" / "genesis-utxo.addr").read_text().strip()
    return clusterlib.AddressRecord(
        address=addr,
        vkey_file=GOV / "faucet" / "genesis-utxo.vkey",
        skey_file=GOV / "faucet" / "genesis-utxo.skey",
    )


def wait_for_node(cluster: clusterlib.ClusterLib, tries: int = 150) -> bool:
    """Block until the node answers a tip query past slot 0."""
    for _ in range(tries):
        try:
            if cluster.g_query.get_slot_no() > 0:
                return True
        except Exception:
            pass
        time.sleep(2)
    return False


def current_epoch(cluster: clusterlib.ClusterLib) -> int:
    return cluster.g_query.get_epoch()


def wait_for_epoch(cluster: clusterlib.ClusterLib, target: int, max_seconds: int = 600) -> bool:
    waited = 0
    while waited < max_seconds:
        try:
            if cluster.g_query.get_epoch() >= target:
                return True
        except Exception:
            pass
        time.sleep(5)
        waited += 5
    return False


@contextlib.contextmanager
def faucet_lock(timeout: int = 120):
    """Serialize faucet spends — concurrent build_tx calls would
    otherwise select the same UTxO and conflict."""
    ensure_dirs()
    fh = FAUCET_LOCK.open("w")
    deadline = time.time() + timeout
    locked = False
    try:
        while time.time() < deadline:
            try:
                fcntl.flock(fh, fcntl.LOCK_EX | fcntl.LOCK_NB)
                locked = True
                break
            except OSError:
                time.sleep(1)
        if not locked:
            raise TimeoutError("faucet lock timeout")
        yield
    finally:
        if locked:
            fcntl.flock(fh, fcntl.LOCK_UN)
        fh.close()


def build_sign_submit(
    cluster: clusterlib.ClusterLib,
    name: str,
    *,
    certificate_files=(),
    proposal_files=(),
    vote_files=(),
    signing_key_files=(),
    txouts=(),
) -> str:
    """Build (auto-selecting faucet inputs), sign, submit. Returns txid.

    The faucet skey is always added to the witnesses; inputs and change
    go to the faucet address. Serialized on the faucet lock.
    """
    fa = faucet()
    all_signing = [*signing_key_files, fa.skey_file]
    tx_files = clusterlib.TxFiles(
        certificate_files=certificate_files,
        proposal_files=proposal_files,
        vote_files=vote_files,
        signing_key_files=all_signing,
    )
    with faucet_lock():
        out = cluster.g_transaction.build_tx(
            src_address=fa.address,
            tx_name=name,
            tx_files=tx_files,
            txouts=txouts,
            change_address=fa.address,
            witness_override=len(all_signing),
            destination_dir=str(WORK),
        )
        signed = cluster.g_transaction.sign_tx(
            tx_body_file=out.out_file,
            signing_key_files=all_signing,
            tx_name=name,
            destination_dir=str(WORK),
        )
        cluster.g_transaction.submit_tx(tx_file=signed, txins=out.txins)
    return cluster.g_transaction.get_txid(tx_body_file=out.out_file)


def lookup_proposal(gov_state: dict, action_txid: str):
    for prop in gov_state.get("proposals", []) or []:
        if prop.get("actionId", {}).get("txId") == action_txid:
            return prop
    return None


def claim_pending_action():
    """Atomically claim one un-voted action via a mkdir lock.

    Returns (txid, ix, path) or None.
    """
    ensure_dirs()
    for path in sorted(ACTIONS_DIR.glob("*.action.json")):
        lock = path.with_suffix(".claim")
        try:
            lock.mkdir()
        except FileExistsError:
            continue
        import json

        data = json.loads(path.read_text())
        return data["txid"], int(data["ix"]), path
    return None
