#!/usr/bin/env python3
"""Transaction generator for Antithesis testnet.

Continuously submits ADA self-transfers using cardano-cli via N2C.
Uses the Antithesis SDK for deterministic randomness (falls back to
standard random outside Antithesis).
"""

import json
import logging
import os
import subprocess
import sys
import time

try:
    from antithesis.random import get_random
    from antithesis.lifecycle import setup_complete
except ImportError:
    # Fallback when antithesis SDK is not available
    import random

    def get_random():
        return random.getrandbits(64)

    def setup_complete(details=None):
        pass


logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(message)s",
    stream=sys.stdout,
)
log = logging.getLogger("tx-generator")

NETWORK_MAGIC = os.environ.get("NETWORK_MAGIC", "42")
NETWORK_ID = f"--testnet-magic {NETWORK_MAGIC}"
SOCKET_PATH = os.environ.get("CARDANO_NODE_SOCKET_PATH", "/state/node.socket")
WORK_DIR = "/tmp/tx-gen"
UTXO_KEYS_DIR = "/utxo-keys"
POOL_KEYS_DIR = "/configs/keys"
MIN_SLEEP = 5
MAX_SLEEP = 30


def run_cli(*args):
    """Run cardano-cli and return stdout."""
    cmd = ["cardano-cli"] + list(args)
    log.debug("Running: %s", " ".join(cmd))
    result = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
    if result.returncode != 0:
        raise RuntimeError(f"cardano-cli failed ({' '.join(cmd[:4])}): {result.stderr.strip()[:200]}")
    return result.stdout.strip()


def wait_for_socket():
    log.info("Waiting for node socket at %s", SOCKET_PATH)
    while not os.path.exists(SOCKET_PATH):
        time.sleep(2)
    log.info("Socket found")


def wait_for_sync():
    log.info("Waiting for node sync")
    while True:
        try:
            tip = json.loads(run_cli("query", "tip", *NETWORK_ID.split()))
            progress = tip.get("syncProgress", "0")
            if progress == "100.00":
                log.info("Node synced, slot %s", tip.get("slot", tip.get("slotInEpoch")))
                return tip
        except Exception as e:
            log.debug("Sync check failed: %s", e)
        time.sleep(5)


def get_era():
    tip = json.loads(run_cli("query", "tip", *NETWORK_ID.split()))
    return tip["era"].lower()


def find_key_file(name):
    """Find a key file in utxo-keys or pool keys directory."""
    for d in [UTXO_KEYS_DIR, POOL_KEYS_DIR]:
        path = os.path.join(d, name)
        if os.path.exists(path):
            return path
    raise FileNotFoundError(f"Key file {name} not found in {UTXO_KEYS_DIR} or {POOL_KEYS_DIR}")


def get_payment_address():
    """Get a funded payment address and its signing key."""
    # Look for genesis.N.addr.info + genesis.N.skey pairs in utxo-keys
    if os.path.isdir(UTXO_KEYS_DIR):
        for fname in sorted(os.listdir(UTXO_KEYS_DIR)):
            if fname.startswith("genesis.") and fname.endswith(".addr.info"):
                # e.g. genesis.1.addr.info → genesis.1.skey
                num = fname.split(".")[1]
                skey_name = f"genesis.{num}.skey"
                skey_path = os.path.join(UTXO_KEYS_DIR, skey_name)
                if not os.path.exists(skey_path):
                    continue
                addr_path = os.path.join(UTXO_KEYS_DIR, fname)
                with open(addr_path) as f:
                    info = json.load(f)
                addr = info["address"]
                utxos = query_utxos(addr)
                if utxos:
                    log.info("Found funded address: %s (key: %s)", addr, skey_path)
                    return addr, skey_path
    raise RuntimeError("No funded address found in " + UTXO_KEYS_DIR)


def get_protocol_params():
    path = os.path.join(WORK_DIR, "protocol-params.json")
    run_cli("query", "protocol-parameters", *NETWORK_ID.split(),
            "--out-file", path)
    return path


def query_utxos(address):
    path = os.path.join(WORK_DIR, "utxos.json")
    run_cli("query", "utxo", *NETWORK_ID.split(),
            "--address", address, "--out-file", path)
    with open(path) as f:
        return json.load(f)


def pick_utxo(utxos):
    """Pick a UTxO using Antithesis deterministic random. Returns (key, lovelace)."""
    keys = list(utxos.keys())
    if not keys:
        return None, 0
    idx = get_random() % len(keys)
    key = keys[idx]
    value = utxos[key].get("value", {})
    lovelace = value.get("lovelace", 0) if isinstance(value, dict) else 0
    return key, lovelace


def random_sleep():
    """Sleep for a random interval using Antithesis deterministic random."""
    seconds = MIN_SLEEP + (get_random() % (MAX_SLEEP - MIN_SLEEP + 1))
    log.info("Sleeping %ds", seconds)
    time.sleep(seconds)


MIN_SPLIT_OUTPUTS = 1
MAX_SPLIT_OUTPUTS = 5
SPLIT_AMOUNT = 2_000_000  # 2 ADA per extra output


def build_sign_submit(era, address, utxo_key, utxo_value, protocol_params_path, skey_path):
    """Build a transaction with random extra outputs to grow the UTxO set."""
    tip = json.loads(run_cli("query", "tip", *NETWORK_ID.split()))
    current_slot = tip.get("slot", tip.get("slotInEpoch", 0))
    ttl = current_slot + 200

    # Decide how many extra outputs (Antithesis deterministic random)
    n_extra = MIN_SPLIT_OUTPUTS + (get_random() % (MAX_SPLIT_OUTPUTS - MIN_SPLIT_OUTPUTS + 1))
    # Only split if the UTxO has enough funds
    needed = n_extra * SPLIT_AMOUNT + 5_000_000  # extra outputs + min change + fees
    if utxo_value < needed:
        n_extra = 0

    tx_raw = os.path.join(WORK_DIR, "tx.raw")
    tx_signed = os.path.join(WORK_DIR, "tx.signed")

    # Build with extra outputs
    build_args = [era, "transaction", "build",
                  *NETWORK_ID.split(),
                  "--tx-in", utxo_key]
    for _ in range(n_extra):
        build_args += ["--tx-out", f"{address}+{SPLIT_AMOUNT}"]
    build_args += ["--change-address", address,
                   "--invalid-hereafter", str(ttl),
                   "--out-file", tx_raw]
    run_cli(*build_args)

    # Sign
    run_cli(era, "transaction", "sign",
            *NETWORK_ID.split(),
            "--signing-key-file", skey_path,
            "--tx-body-file", tx_raw,
            "--out-file", tx_signed)

    # Get tx hash for logging
    tx_id = run_cli(era, "transaction", "txid", "--tx-file", tx_signed)

    # Submit
    run_cli(era, "transaction", "submit",
            *NETWORK_ID.split(),
            "--tx-file", tx_signed)

    return tx_id


def main():
    os.makedirs(WORK_DIR, exist_ok=True)

    wait_for_socket()
    wait_for_sync()

    era = get_era()
    address, skey_path = get_payment_address()
    log.info("Era: %s, Address: %s, Skey: %s", era, address, skey_path)

    setup_complete({"tx_generator": "ready", "era": era, "address": address})

    tx_count = 0
    error_count = 0

    while True:
        try:
            protocol_params = get_protocol_params()
            utxos = query_utxos(address)

            if not utxos:
                log.warning("No UTxOs available, waiting...")
                time.sleep(10)
                continue

            utxo_key, utxo_value = pick_utxo(utxos)
            log.info("Using UTxO: %s (%d lovelace, %d UTxOs total)", utxo_key, utxo_value, len(utxos))

            tx_id = build_sign_submit(era, address, utxo_key, utxo_value, protocol_params, skey_path)
            tx_count += 1
            log.info("Submitted tx %d: %s", tx_count, tx_id)

            # Wait for the consumed UTxO to disappear (tx included in block)
            for _ in range(60):
                time.sleep(2)
                fresh = query_utxos(address)
                if utxo_key not in fresh:
                    log.info("UTxO consumed, %d UTxOs now available", len(fresh))
                    break
            else:
                log.warning("UTxO still present after 120s, proceeding anyway")

        except Exception as e:
            error_count += 1
            log.error("Tx failed (error %d): %s", error_count, e)
            # Re-fetch era in case of hard fork
            try:
                era = get_era()
            except Exception:
                pass
            time.sleep(10)
            continue

        random_sleep()


if __name__ == "__main__":
    main()
