#!/usr/bin/env python3
"""finally_governance_summary.py — end-of-run coverage marker.

Emits a single Sometimes/Reachable pair proving the gov-cli lifecycle
reached end-of-test. No node calls, no sleep: always exits 0 even if a
fault delivers SIGTERM mid-run.
"""

from __future__ import annotations

import sys

import helper_sdk as sdk


def main() -> int:
    sdk.sometimes(True, "governance_run_completed")
    sdk.reachable("finally_governance_summary entered")
    return 0


if __name__ == "__main__":
    try:
        main()
    finally:
        sys.exit(0)
