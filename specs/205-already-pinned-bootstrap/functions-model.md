# Function contracts
Ceiling: 60 lines / 3 KiB. Internal helpers remain owner implementation choices.

F-1 propose-bootstrap(upstream_sha: 40-hex), with existing day/state/repository
environment -> candidate_sha: 40-hex or nonzero typed failure. It may now return
valid existing source D-1 with no bootstrap publication effects; new proposals D-2
retain existing effects only after their original checks.
F-2 require-bootstrap-checks(candidate_sha: 40-hex) -> successful exact-source rows
or nonzero failure. Its failure and observation semantics are preserved.
F-3 resolve-image(candidate_sha: 40-hex) -> immutable producer image reference or
nonzero failure. Image source tag corresponds to the selected candidate.
F-4 daily controller environment -> stage receipts / exit status. It preserves the
same source across F-1/F-2/F-3 and exposes required evidence failures to its caller.
No new public command, resolver signature, credential or schedule contract.
