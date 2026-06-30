# Tasks: Score the amaru-consumer's convergence (#180)

## Slice 1 — conditional convergence property + RED→GREEN tests

- [X] T001 RED: add tracer-sidecar test cases (hspec/golden) for the new
  property — hit case (consumer advances + matches a producer tip), not-hit
  case (consumer stuck), no-consumer case (property not declared). Run and
  watch them FAIL (property absent). Record RED in WIP.md.
- [X] T002 `mkSpec` takes the consumer host (`Maybe Text`); `App.hs` reads
  `AMARU_CONSUMER_HOST` and passes it.
- [X] T003 Extend `State` (`producerTipHashes`, `consumerFirstLen`) + the
  updater rule + the `sometimes "amaru-served consumer reached producer tip"`
  property, added ONLY when a consumer host is configured.
- [X] T004 GREEN: `cd components/tracer-sidecar && cabal test` passes (all
  three cases); `fourmolu`/`hlint`/`cabal-fmt` clean; existing golden stable.
  One bisect-safe commit (property + tests).

## Slice 2 — wire into cardano_amaru + image bump

- [X] T005 `testnets/cardano_amaru/docker-compose.yaml`: set
  `AMARU_CONSUMER_HOST=amaru-consumer.example` on `tracer-sidecar`; leave
  `cardano_node_master` unset.
- [X] T006 Bump the `tracer-sidecar` image tag to a branch commit SHA so
  `publish-images` rebuilds with the property (image-tag hygiene).
- [X] T007 `INTERNAL_NETWORK=false docker compose -f
  testnets/cardano_amaru/docker-compose.yaml config` validates; image-spelling
  constraint preserved.

## Finalization — AC3 (one launched run, after S1+S2 green + image built)

- [ ] T008 Launch one `cardano_amaru` run; confirm the
  `amaru-served consumer reached producer tip` property appears in the report
  (`anti properties <run_id>`). Record the run_id + report link in the PR.
