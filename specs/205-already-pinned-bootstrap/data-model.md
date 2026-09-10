# Data
Ceiling: 60 lines / 3 KiB.

D-1 Existing source: requested Amaru revision, exact observed bootstrap main commit,
committed lock and resolution tuple. Both revisions and resolver-owned snapshot
provenance must correspond; selection cannot manufacture a source identity.
D-2 New proposal: the existing one-commit history and exact flake.lock plus
nix/peer-snapshots/resolution.json path set with coherent source tuple. This contract
is retained; existing-source selection is not a relaxed D-2 proposal.
D-3 Provisional/verified pipeline: candidate SHA -> required exact-source successful
checks -> registry image repository:source-SHA@sha256:digest -> consumer input.
A missing, inconsistent or failed required observation is a terminal error at its
existing stage. A provisional SHA is not a verified producer or completed run.
No new receipt keys or global no-op state are required by this slice.
