# Quickstart: Amaru relays self-bootstrap

How to validate the change locally, then on Antithesis. Keep this
walkthrough open while reviewing the diff.

## Local validation

### Bring the testnet up

```bash
cd testnets/cardano_amaru_epoch3600
docker compose up -d
```

Expected within ~60 s:
- All containers in `Up`.
- `bootstrap-producer` is **gone** from `docker compose ps` output.
- `amaru-relay-1` and `amaru-relay-2` are `Up`. They MAY still be
  bootstrapping internally (look at `docker logs amaru-relay-1`); they
  MUST NOT be `Created` or `Restarting`.

### Watch the in-relay bootstrap

```bash
docker logs -f amaru-relay-1
```

Expected sequence:
1. Bootstrap loop output: snapshot copy + `/bin/bootstrap-producer …`
   attempt logs.
2. On success: `bootstrap-producer: committed /srv/amaru/testnet_42`
   (or the `wrote …` line from the binary).
3. Marker write (silent): `/startup/amaru-relay-1.started` appears.
4. `amaru run …` startup banner.

Same for `amaru-relay-2`.

### Confirm the sidecar still gates on Amaru readiness

```bash
docker logs sidecar | head -30
```

Expected: sidecar logs show it waiting for
`/amaru-startup/amaru-relay-1.started` and `…relay-2.started`. Once both
relays finish bootstrap, the sidecar's chainpoint workflow proceeds as
in the existing testnet.

### Tear down

```bash
docker compose down -v
```

## Antithesis validation (the binding gate)

### Dispatch a 1h run on the branch

```bash
gh workflow run cardano-node.yaml \
  --repo cardano-foundation/cardano-node-antithesis \
  --ref 080-amaru-self-bootstrap
```

Wait for the GitHub Actions run to print the test ID + Antithesis
report URL.

### What the report MUST show

- Status: `Completed` (not `IncompleteTimed out`, not `setup_error`).
- "Setup complete" event present in the timeline.
- `bootstrap-producer` container does NOT appear in the container list
  (because it no longer exists).
- `amaru-relay-1` and `amaru-relay-2` containers are `Running` for the
  duration.
- Property report renders (passed/failed mix; not the empty error-card
  view).

### What to compare against the baseline

`findings_new` on this branch's 1h run vs. the most recent green
1h-Amaru-testnet run on `main`. Use
`mcp__merge-guard__check-merge-ready` with the report URL to score the
gate.

## Rollback

If the Antithesis 1h run regresses Amaru properties (not just setup),
revert the single compose-file edit:

```bash
git revert <commit-sha>
```

The bootstrap-producer image and per-relay scratch volumes are
unchanged, so revert is a straight YAML rollback — no image rebuild, no
volume migration.
