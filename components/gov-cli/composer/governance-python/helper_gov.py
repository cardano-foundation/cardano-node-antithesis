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
import subprocess
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
SETUP_MARKER = STATE_DIR / "setup_done"
FAUCET_LOCK = STATE_DIR / "faucet.lock"

# Action coordination between the independent create and vote drivers.
#
# The gov-cli container is excluded from fault injection, so its
# filesystem persists for the whole run — no need for locks or crash
# recovery. Two append-only logs are the shared state:
#   created.log   one "txid ix" line per action confirmed on-chain
#   rejected.log  one "txid" line per action a vote was rejected on
# The vote driver works the set difference (created MINUS rejected): an
# action stays votable until a vote on it is rejected, at which point it
# drops out for good. Append-only means concurrent driver instances
# never corrupt the state and a record is never lost.
CREATED_LOG = STATE_DIR / "created.log"
REJECTED_LOG = STATE_DIR / "rejected.log"

ANCHOR_URL = "https://example.com/governance.json"
ANCHOR_TEXT = '{"body":{"title":"antithesis governance workload"}}'


def ensure_dirs() -> None:
    for d in (WORK, STATE_DIR):
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


# --- Two-log action coordination (mirrors helper_gov.sh) -------------


def record_created(txid: str, ix: int) -> None:
    """Publish a confirmed action (append-only)."""
    ensure_dirs()
    with CREATED_LOG.open("a", encoding="utf-8") as fh:
        fh.write(f"{txid} {ix}\n")


def record_rejected(txid: str) -> None:
    """Retire an action a vote was rejected on (append-only)."""
    ensure_dirs()
    with REJECTED_LOG.open("a", encoding="utf-8") as fh:
        fh.write(f"{txid}\n")


def _rejected_set() -> set:
    if not REJECTED_LOG.exists():
        return set()
    out = set()
    for line in REJECTED_LOG.read_text(encoding="utf-8").splitlines():
        t = line.split()
        if t:
            out.add(t[0])
    return out


def pending_actions():
    """Return [(txid, ix), ...] for created actions whose txid is not in
    the rejected log (the set difference). Handles a missing/empty
    rejected log correctly."""
    if not CREATED_LOG.exists():
        return []
    rejected = _rejected_set()
    out = []
    for line in CREATED_LOG.read_text(encoding="utf-8").splitlines():
        parts = line.split()
        if not parts:
            continue
        txid = parts[0]
        if txid in rejected:
            continue
        ix = int(parts[1]) if len(parts) > 1 else 0
        out.append((txid, ix))
    return out


def action_onchain(cluster: clusterlib.ClusterLib, txid: str) -> bool:
    """True while the proposal still exists in gov-state (not yet
    expired/enacted/dropped)."""
    try:
        return lookup_proposal(cluster.g_query.get_gov_state(), txid) is not None
    except Exception:  # noqa: BLE001
        return False


def confirm_action(cluster: clusterlib.ClusterLib, txid: str, tries: int = 30):
    """Poll gov-state until the proposal with this txid appears, then
    return its real govActionIx. Returns None if it never shows up."""
    for _ in range(tries):
        try:
            prop = lookup_proposal(cluster.g_query.get_gov_state(), txid)
            if prop:
                return int(prop["actionId"]["govActionIx"])
        except Exception:  # noqa: BLE001
            pass
        time.sleep(2)
    return None


# --- Antithesis RNG (mirrors antithesis_rng / rng_mod in bash) -------


def antithesis_rng() -> int:
    """A random non-negative integer, steered by the Antithesis
    hypervisor when `antithesis_random` is present, otherwise from
    /dev/urandom. Every random choice the vote driver makes flows
    through here so the test surface is handed to Antithesis."""
    try:
        out = subprocess.run(
            ["antithesis_random"],
            capture_output=True,
            timeout=2,
            check=False,
        ).stdout.decode("ascii", "ignore")
        digits = "".join(c for c in out if c.isdigit())
        if digits:
            return int(digits)
    except Exception:  # noqa: BLE001
        pass
    return int.from_bytes(os.urandom(4), "big")


def rng_mod(n: int) -> int:
    """An index in [0, n) from antithesis_rng."""
    if n <= 0:
        return 0
    return antithesis_rng() % n
