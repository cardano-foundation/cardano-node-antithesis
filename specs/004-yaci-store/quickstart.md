# Quickstart: yaci-store on the Antithesis testnet

**Feature**: 004-yaci-store
**Date**: 2026-04-27

This is the operator recipe for bringing yaci-store up locally and verifying every Success Criterion in `spec.md`. All commands are run from the repo root inside the worktree (`/code/cardano-node-antithesis-issue-75`).

## Prerequisites

- Docker + Docker Compose (existing testnet requirements).
- `just` (existing testnet requirement).
- Network access to `docker.io/bloxbean/yaci-store` (no auth needed).

## 1. Bring the testnet up (cold)

```bash
just up cardano_node_master
```

Wait until all services report `running` (or `healthy` where they have healthchecks):

```bash
just ps cardano_node_master
```

Expected: yaci-store appears in the list and reaches `healthy` within ~1 minute.

## 2. Verify SC-001 — yaci-store reaches relay tip within 5 minutes

Get the relay tip slot:

```bash
docker compose -f testnets/cardano_node_master/docker-compose.yaml \
  exec -T relay1 cardano-cli query tip --testnet-magic 42 \
  --socket-path /state/node.socket | jq .slot
```

Watch yaci-store's reported tip from logs:

```bash
just logs yaci-store cardano_node_master | grep -i "block.*slot" | tail -5
```

Or query the actuator health + info endpoints from inside the container network:

```bash
docker compose -f testnets/cardano_node_master/docker-compose.yaml \
  exec -T yaci-store wget -qO- http://localhost:8080/actuator/health
```

Pass condition: yaci-store's reported tip slot ≥ relay's tip slot within 5 minutes of the network producing its first block.

## 3. Verify SC-003 — healthy within 1 minute on cold start

```bash
just down cardano_node_master
time just up cardano_node_master
# Watch yaci-store reach healthy:
docker compose -f testnets/cardano_node_master/docker-compose.yaml \
  ps yaci-store --format json | jq -r '.[0].Health'
```

Pass condition: `Health == "healthy"` within 60 seconds of `just up` returning.

## 4. Verify acceptance scenario — relay restart resilience

```bash
docker compose -f testnets/cardano_node_master/docker-compose.yaml \
  restart relay1
sleep 30
docker compose -f testnets/cardano_node_master/docker-compose.yaml \
  logs --tail 50 yaci-store
```

Pass condition: yaci-store logs show a reconnect cycle (disconnect → retry → connected) without container exit.

## 5. Verify SC-004 — clean teardown

```bash
just down cardano_node_master
docker volume ls | grep yaci
docker ps -a | grep yaci
```

Pass condition: both commands return empty.

## 6. Verify SIGTERM exit code (FR-009)

```bash
just up cardano_node_master
sleep 60
docker stop yaci-store
docker inspect yaci-store --format '{{.State.ExitCode}}'
```

Expected: exit code `0` (Spring Boot graceful shutdown). If non-zero, file as a finding and update the harness or compose `stop_signal` accordingly before merge.

## 7. Pre-merge: 1h Antithesis run (constitution hard gate)

```bash
gh workflow run cardano-node.yaml --ref 004-yaci-store \
  --field duration=1h
```

After completion:
- Open the run report (URL printed by `gh run view`).
- Compare findings to the most recent green 1h `main` baseline (use `antithesis-triage` skill).
- If `findings_new > 0`, attribute and either file a follow-up issue or whitelist with a documented reason.
- Only merge when `findings_new ≤ baseline`.

## Rollback

If yaci-store causes regressions and the bump must be reverted:

```bash
git revert <implementation-commit>
just smoke-test
```

A revert is a single-commit operation because the entire feature lives in one stanza of `docker-compose.yaml` plus this docs tree. No CI surface, no `components/`, no `publish-images` ripple effects.

## Image digest capture (for the implementation commit)

```bash
docker pull docker.io/bloxbean/yaci-store:2.0.0
docker inspect --format='{{index .RepoDigests 0}}' docker.io/bloxbean/yaci-store:2.0.0
```

Paste the resulting `bloxbean/yaci-store@sha256:<hex>` (replacing `bloxbean/` with `docker.io/bloxbean/` if needed) into the compose stanza.
