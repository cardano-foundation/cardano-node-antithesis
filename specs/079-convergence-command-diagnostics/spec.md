# Feature Specification: Improve Convergence Command Diagnostics

**Feature Branch**: `fix/improve-convergence-command-diagnostics`  
**Created**: 2026-04-28  
**Status**: Draft  
**Input**: GitHub issue #79

## User Scenarios & Testing

### User Story 1 - Classify Failed Tip Checks (Priority: P1)

As an operator reading an Antithesis report, I need failed convergence checks to say whether producers failed to return tips because of DNS, connection, or tip-protocol errors, or whether reachable producers returned divergent tips, so that I can triage network recovery separately from chain safety.

**Why this priority**: The latest failed main run reported `count=0 distinct=0`, which hid the actual transport or protocol failure.

**Independent Test**: Run the convergence command from the sidecar on a healthy local cluster and verify it records three successful producer probes with matching hashes.

**Acceptance Scenarios**:

1. **Given** all producers are reachable, **When** the convergence command runs, **Then** the report details include one successful result per producer.
2. **Given** a producer probe fails, **When** the convergence command reports failure, **Then** the details include the node name, exit code, a concrete failure reason such as `tip_protocol_deserialise_failure`, and bounded stderr/stdout context.
3. **Given** all producers are reachable but their hashes differ, **When** the final-tip command reports failure, **Then** the failure is classified as tip divergence rather than unreachability.

### User Story 2 - Exercise Sidecar Vantage Point Locally (Priority: P2)

As a maintainer, I need the smoke test to exercise the sidecar command path, so that compose-level CI covers the same network vantage point used by Antithesis.

**Why this priority**: The existing smoke test pings each node from inside itself, which does not validate sidecar-to-producer DNS/TCP reachability.

**Independent Test**: Run `scripts/smoke-test.sh cardano_node_master 120` and observe that it executes the sidecar convergence command after producer-local pings succeed.

### User Story 3 - Align Setup Readiness with Tip Probing (Priority: P2)

As a maintainer, I need Antithesis setup completion to wait for the same producer tip protocol path used by convergence checks, so that setup does not declare the system ready while `cardano-cli ping --tip` still fails.

**Why this priority**: Local reproduction showed plain producer ping can be weaker than `--tip`; the TCP endpoint can be reachable while the tip find-intersect path returns EOF.

**Independent Test**: Start `cardano_node_master` locally and verify the sidecar can complete setup and the convergence command can retrieve all producer tips.

**Acceptance Scenarios**:

1. **Given** producers are starting, **When** the sidecar signals Antithesis setup complete, **Then** every producer has already answered a sidecar-originated `cardano-cli ping --tip`.
2. **Given** a producer accepts TCP before tip probing is ready, **When** the sidecar setup loop checks readiness, **Then** setup continues polling instead of declaring completion.

## Requirements

### Functional Requirements

- **FR-001**: Convergence commands MUST preserve per-node probe outcomes in structured JSON details.
- **FR-002**: Failed probe details MUST include node name, exit code, failure reason, and bounded stdout/stderr context.
- **FR-003**: Final-tip checks MUST distinguish incomplete producer tip responses from reachable producer tip divergence.
- **FR-004**: Local smoke tests MUST execute a convergence command from the sidecar container.
- **FR-005**: The implementation MUST stay within the current sidecar runtime dependencies unless a follow-up issue replaces the shell command layer.
- **FR-006**: Sidecar setup readiness MUST use `cardano-cli ping --tip`, not plain ping, so setup and convergence share the same readiness definition.

## Success Criteria

### Measurable Outcomes

- **SC-001**: A failed no-response check produces JSON details showing why each producer probe failed.
- **SC-002**: A healthy local cluster passes sidecar convergence from `scripts/smoke-test.sh`.
- **SC-003**: Maintainers can tell from report text/details whether a failure is name resolution, connection failure, tip-protocol failure, missing tip data, or tip divergence.
- **SC-004**: Antithesis setup completion is not emitted until sidecar-originated `--tip` probes succeed for all producers.

## Assumptions

- Antithesis command discovery remains file-prefix based under `/opt/antithesis/test/v1`.
- Bash remains acceptable for this small diagnostic slice because the sidecar image already carries Bash, jq, and cardano-cli.
- A compiled probe may be preferable later if retries, protocol inspection, or richer health models outgrow shell.
