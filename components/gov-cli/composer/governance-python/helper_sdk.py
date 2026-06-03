#!/usr/bin/env python3
"""helper_sdk.py — Antithesis Fallback SDK emitter (Python).

`helper_`-prefixed: ignored by the Antithesis composer scheduler.
Sibling driver scripts import it. Emits the same sdk.jsonl format as
the bash helper_sdk.sh so reports are identical regardless of which
driver language a run uses.
"""

from __future__ import annotations

import json
import os
import pathlib


def _emit(obj: dict) -> None:
    try:
        out_dir = pathlib.Path(os.environ.get("ANTITHESIS_OUTPUT_DIR", "/tmp"))
        out_dir.mkdir(parents=True, exist_ok=True)
        with (out_dir / "sdk.jsonl").open("a", encoding="utf-8") as fh:
            fh.write(json.dumps(obj) + "\n")
    except Exception:
        # An SDK emit must never crash a driver.
        pass


def _assert(
    assert_id: str,
    display_type: str,
    assert_type: str,
    condition: bool,
    details: dict | None = None,
) -> None:
    _emit(
        {
            "antithesis_assert": {
                "id": assert_id,
                "message": assert_id,
                "condition": bool(condition),
                "display_type": display_type,
                "hit": True,
                "must_hit": True,
                "assert_type": assert_type,
                "location": {
                    "file": "",
                    "function": "",
                    "class": "",
                    "begin_line": 0,
                    "begin_column": 0,
                },
                "details": details,
            }
        }
    )


def reachable(assert_id: str) -> None:
    _assert(assert_id, "Reachable", "reachability", True)


def unreachable(assert_id: str) -> None:
    _assert(assert_id, "AlwaysOrUnreachable", "always", False)


def sometimes(condition: bool, assert_id: str, details: dict | None = None) -> None:
    _assert(assert_id, "Sometimes", "sometimes", condition, details)


def always(condition: bool, assert_id: str, details: dict | None = None) -> None:
    _assert(assert_id, "Always", "always", condition, details)


def setup_complete(details: dict | None = None) -> None:
    """Emit the Antithesis lifecycle "setup complete" signal: tells the
    hypervisor the system is healthy and it may START injecting faults."""
    _emit({"antithesis_setup": {"status": "complete", "details": details}})
