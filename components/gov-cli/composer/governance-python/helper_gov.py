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

# Perturbation-witness state. The anytime_chain_progress probe writes a
# verdict here ("stalled <epoch>" / "producing <epoch>") each time it
# samples block production; the create/vote drivers read it to assert
# that a governance op landed while the chain was degraded.
CHAIN_VERDICT = STATE_DIR / "chain_verdict"

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


# --- Action selection: the chain IS the queue (mirrors helper_gov.sh) -
#
# Rather than maintaining a local created/rejected ledger (which a
# transient fault could corrupt into permanently dropping a still-live
# action), the vote driver reads the live set of governance actions
# straight from the node via an N2C gov-state query. relay1 is
# fault-excluded and a LocalStateQuery returns the last settled ledger
# state, so this answers correctly even while block production is
# stalled. An action leaves the set only when the LEDGER expires or
# enacts it — there is no local "retire", so a transient submit/lock
# failure is self-healing (the action reappears next tick).


def live_info_actions(cluster: clusterlib.ClusterLib):
    """Return the current InfoAction proposals (each a full gov-state
    proposal). Empty list if none or if the node can't be reached."""
    try:
        proposals = cluster.g_query.get_gov_state().get("proposals", []) or []
        return [
            p
            for p in proposals
            if p.get("proposalProcedure", {}).get("govAction", {}).get("tag") == "InfoAction"
        ]
    except Exception:  # noqa: BLE001
        return []


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


# --- Perturbation witness (mirrors helper_gov.sh) --------------------


def set_chain_verdict(kind: str) -> None:
    """Publish the latest chain-progress verdict: "<kind> <epoch>"."""
    ensure_dirs()
    try:
        CHAIN_VERDICT.write_text(f"{kind} {int(time.time())}\n", encoding="utf-8")
    except Exception:  # noqa: BLE001
        pass


def recent_stall(within: int = 90) -> bool:
    """True if the most recent chain sample was a stall within the window
    (i.e. faults were actively halting block production around now)."""
    if not CHAIN_VERDICT.exists():
        return False
    try:
        parts = CHAIN_VERDICT.read_text(encoding="utf-8").split()
    except Exception:  # noqa: BLE001
        return False
    if len(parts) < 2 or parts[0] != "stalled":
        return False
    try:
        ts = int(parts[1])
    except ValueError:
        return False
    return (int(time.time()) - ts) <= within
