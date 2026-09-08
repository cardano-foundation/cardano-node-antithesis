# One-hour Nix image provenance validation (#241)

This temporary profile copies `cardano_node_master` and replaces only
`tx-generator` and `tracer-sidecar` with digest-pinned images built from
PR #242 revision `36756d0efcf73f412df42a352afd7d6b0ac3c33d`.
Their OCI creation date is the source timestamp `2026-09-08T10:52:25Z`.

The first validation run requests one hour with fault injection enabled.
Check the Antithesis environment's actual image digests and creation dates,
as well as the freshness property. The other Nix component images retain
their existing pins; this run does not validate their corrected builds.

This profile is for pre-merge validation and is not a replacement for the
scheduled master profile.

see [docs](https://cardano-foundation.github.io/cardano-node-antithesis/testnets/cardano-node-master/)
