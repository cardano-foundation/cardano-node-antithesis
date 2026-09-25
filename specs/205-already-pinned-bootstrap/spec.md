# Already-pinned bootstrap adoption

Internal child t-already-pinned of epic #205; controller context #210.
Frozen reproduction source: 8344022d0d5f1c418fcb5d844d0d75e8d0013eb5.
Ceiling: 100 lines / 6 KiB. Requirements are conditional on the reproduced RED.

The actual proposal path was reproduced with bootstrap main already pinning the
requested Amaru and paired configurations revisions. Both real resolver calls
verified all three networks; only the resolution timestamp changed. The atomic
proposal census rejected that resolution-only diff as a resolver failure.

## Requirements

R-1: A daily attempt may select an already-built producer from exact bootstrap
main when its committed source tuple matches the requested Amaru revision and
passes the existing source/resolver/anchor provenance contract.
R-2: Selection of existing source produces no redundant proposal commit, push or
PR. Newly changed-pin proposals retain the exact two-file, one-commit contract.
R-3: The daily path carries the selected exact source through its required checks
and immutable registry reference before consumer preparation. Selection alone is
provisional, not a check/image success or completed run.
R-4: Real check failures, wrong source identity, incoherent source records, invalid
image identity, unavailable provenance and resolver failures remain observable red.
R-5: Add permanent executable controls over distinguishable producer identities;
prove that wrong source/check/digest data cannot yield an accepted consumer input.
R-6: Document resulting behavior and proof limits. State explicitly whether this
repair generalizes to correct-no-op handling or is a narrow instance of that class.

## Invariants

All rows are BLOCKING: unattended producer inputs feed chain-consuming nodes.

| ID | Observable truth | Rejection evidence | Success evidence |
|---|---|---|---|
| INV-AP-1 | Existing-source selection is the observed exact bootstrap main commit with coherent requested Amaru/configuration/resolution provenance | A mismatched source tuple or requested revision cannot be adopted | Real already-pinned main is selected and its source tree remains unchanged |
| INV-AP-2 | Existing-source selection creates no redundant proposal effects; changed-pin and existing-branch atomicity remain enforced | Missing/additional paths, split history or foreign branch cannot become a valid atomic proposal | Existing source needs no bootstrap write; legitimate changed-pin proposal retains the two-file one-commit transaction |
| INV-AP-3 | One exact source identity links the daily proposal result, required successful checks and immutable image reference before consumer preparation | Wrong-head check rows, wrong source tag or malformed digest cannot reach accepted consumer preparation | The caller uses the selected source for checks and image resolution in order |
| INV-AP-4 | Required evidence failures remain typed, caller-visible failures without downstream effects | Failed checks, unavailable/malformed provenance and resolver failures remain red | Valid evidence alone advances the stage; no failure is recast as a successful no-op |

## Scope and limits

Production: scripts/daily-amaru-github.sh; scripts/daily-amaru.sh only where needed
for the candidate/value/failure contract. Proof: tests/test-daily-amaru.sh and
existing relevant tests/fixtures/daily-amaru/ boundary fixtures. Documentation:
docs/daily-amaru.md. No new shared provenance contract or no-op state-machine redesign.
No schedule, credentials, bootstrap source, consumer Compose, freshness, launch,
merge, external comments or contact. Full isolated-path proof is not a production
nightly, unattended consumer CI/integration proof, or streak acceptance.
