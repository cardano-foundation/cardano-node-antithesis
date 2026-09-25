# Modules
Ceiling: 60 lines / 3 KiB.

M-1 scripts/daily-amaru-github.sh owns source selection and proposal atomicity.
It may select existing bootstrap main under D-1 without constructing D-2, preserving
D-3 failure behavior. It consumes the external resolver; it does not own its rules.
M-2 scripts/daily-amaru.sh owns daily stage sequencing, provisional source identity,
required checks, image correspondence and caller-visible failure receipts.
M-3 bootstrap resolver/anchor is a read-only external dependency and retains all
source selection, peer-snapshot hashing and provenance rules.
M-4 tests/test-daily-amaru.sh and existing boundary fixtures exercise M-1/M-2 through
local Git repositories and effect adapters. Production does not depend on fixtures.
The dependency direction remains controller -> transport -> external resolver.
See data-model.md for identities and functions-model.md for unchanged public shapes.
