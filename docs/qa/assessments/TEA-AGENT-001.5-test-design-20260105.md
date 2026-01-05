# Test Design: Story TEA-AGENT-001.5 (Inter-Agent Communication)

Date: 2026-01-05
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 54
- **Unit tests:** 32 (59%)
- **Integration tests:** 16 (30%)
- **E2E tests:** 6 (11%)
- **Priority distribution:** P0: 28, P1: 18, P2: 8

## Test Level Justification

This story involves significant new infrastructure (message queues, shared state, agent discovery) with multiple component interactions. The test strategy emphasizes:

1. **Heavy unit testing** - Core message handling, serialization, timeout logic, and state management are pure business logic suitable for fast, isolated tests
2. **Targeted integration testing** - Multi-agent communication flows require verifying actual component interactions
3. **Focused E2E testing** - Only complete multi-agent workflows need full E2E validation

---

## Test Scenarios by Acceptance Criteria

### AC1: `a2a.send` Action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-001 | Unit | P0 | Validate message structure (type, payload, correlation_id) | Pure validation logic |
| 001.5-UNIT-002 | Unit | P0 | Send to named agent enqueues correctly | Core functionality with mock queue |
| 001.5-UNIT-003 | Unit | P1 | Fire-and-forget returns immediately | Async behavior verification |
| 001.5-UNIT-004 | Unit | P1 | Optional delivery confirmation flag handling | Configuration parsing logic |
| 001.5-INT-001 | Integration | P0 | Message delivered to target agent's inbox | Multi-component queue interaction |

### AC2: `a2a.receive` Action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-005 | Unit | P0 | Receive returns messages from queue | Core dequeue logic |
| 001.5-UNIT-006 | Unit | P0 | Timeout returns empty list when no messages | Timeout handling logic |
| 001.5-UNIT-007 | Unit | P0 | Filter by message type excludes non-matching | Filtering logic |
| 001.5-UNIT-008 | Unit | P0 | Filter by sender agent ID | Filtering logic |
| 001.5-UNIT-009 | Unit | P1 | Returns list of multiple messages | Batch retrieval |
| 001.5-UNIT-010 | Unit | P0 | `require_all` blocks until all specified agents respond | Wait logic with mock |
| 001.5-UNIT-011 | Unit | P1 | `require_all` times out if missing agent | Timeout + require_all interaction |
| 001.5-INT-002 | Integration | P0 | Send/receive round-trip between two agents | End-to-end message flow |
| 001.5-INT-003 | Integration | P0 | Multiple agents sending to same receiver | Concurrency handling |

### AC3: `a2a.broadcast` Action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-012 | Unit | P0 | Broadcast delivers to all agents in namespace | Routing logic |
| 001.5-UNIT-013 | Unit | P0 | Namespace isolation - agents in different namespaces don't receive | Security-critical isolation |
| 001.5-UNIT-014 | Unit | P1 | Agent type filter restricts to matching types | Filter logic |
| 001.5-UNIT-015 | Unit | P1 | Broadcast to empty namespace succeeds (no-op) | Edge case |
| 001.5-INT-004 | Integration | P1 | Three agents receive broadcast message | Multi-recipient validation |
| 001.5-INT-005 | Integration | P1 | Mixed namespace agents, only correct namespace receives | Isolation verification |

### AC4: `a2a.delegate` Action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-016 | Unit | P0 | Delegation sends task and waits for response | Core delegation logic |
| 001.5-UNIT-017 | Unit | P0 | Automatic correlation_id generated and matched | ID matching logic |
| 001.5-UNIT-018 | Unit | P0 | Timeout with `fallback_local` executes fallback action | Strategy execution |
| 001.5-UNIT-019 | Unit | P0 | Timeout with `retry` re-attempts delegation | Retry logic |
| 001.5-UNIT-020 | Unit | P0 | Timeout with `raise` throws DelegationTimeoutError | Error handling |
| 001.5-UNIT-021 | Unit | P1 | Mismatched correlation_id responses ignored | Security: prevent spoofing |
| 001.5-UNIT-022 | Unit | P1 | Configurable timeout value respected | Configuration handling |
| 001.5-INT-006 | Integration | P0 | Successful delegation round-trip | Complete flow |
| 001.5-INT-007 | Integration | P0 | Delegation with fallback executes fallback on timeout | Fallback flow |
| 001.5-E2E-001 | E2E | P0 | Coordinator delegates to worker, receives result | Critical user journey |

### AC5: Message Queue Backend

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-023 | Unit | P0 | InMemoryMessageQueue enqueue/dequeue | Core queue operations |
| 001.5-UNIT-024 | Unit | P0 | Message JSON serialization/deserialization | Data integrity |
| 001.5-UNIT-025 | Unit | P1 | Queue isolation by agent ID | Multi-agent correctness |
| 001.5-UNIT-026 | Unit | P1 | MessageQueue protocol/interface contracts | Interface compliance |
| 001.5-UNIT-027 | Unit | P2 | Empty queue returns empty list | Edge case |
| 001.5-UNIT-028 | Unit | P2 | Large payload serialization (>1MB) | Limit testing |

### AC6: Shared State Namespace

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-029 | Unit | P0 | `a2a.state.set` stores value by key | Core storage logic |
| 001.5-UNIT-030 | Unit | P0 | `a2a.state.get` retrieves stored value | Core retrieval logic |
| 001.5-UNIT-031 | Unit | P0 | Namespace scoping prevents cross-namespace access | Security-critical |
| 001.5-UNIT-032 | Unit | P0 | Optimistic locking detects concurrent update conflict | Data integrity |
| 001.5-UNIT-033 | Unit | P1 | TTL expiration removes stale data | Cleanup logic |
| 001.5-UNIT-034 | Unit | P1 | Get with default returns default when key missing | Edge case |
| 001.5-INT-008 | Integration | P0 | Two agents sharing state via namespace | Multi-agent state flow |
| 001.5-INT-009 | Integration | P1 | Concurrent writes trigger optimistic lock failure | Conflict detection |
| 001.5-E2E-002 | E2E | P1 | Multi-agent workflow with shared progress tracking | Real-world scenario |

### AC7: Agent Discovery

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-035 | Unit | P1 | `a2a.discover` returns agents in namespace | Core discovery logic |
| 001.5-UNIT-036 | Unit | P1 | Capability filter returns only matching agents | Filter logic |
| 001.5-UNIT-037 | Unit | P1 | Agent capability advertisement at registration | Registration flow |
| 001.5-UNIT-038 | Unit | P2 | Discovery mode: broadcast | Mode implementation |
| 001.5-UNIT-039 | Unit | P2 | Discovery mode: registry | Mode implementation |
| 001.5-UNIT-040 | Unit | P2 | Discovery mode: static | Mode implementation |
| 001.5-INT-010 | Integration | P1 | Dynamic agent registration/discovery | Real discovery flow |
| 001.5-INT-011 | Integration | P2 | Agent leaves namespace, no longer discoverable | Lifecycle handling |

### AC8: Python Implementation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-041 | Unit | P0 | All a2a actions registered in actions registry | Registration correctness |
| 001.5-INT-012 | Integration | P0 | YAML workflow using a2a.send/receive executes | YAML engine integration |
| 001.5-INT-013 | Integration | P0 | YAML workflow using a2a.delegate executes | YAML engine integration |
| 001.5-INT-014 | Integration | P1 | YAML workflow using a2a.broadcast executes | YAML engine integration |
| 001.5-INT-015 | Integration | P1 | YAML workflow using a2a.state actions executes | YAML engine integration |
| 001.5-INT-016 | Integration | P1 | YAML workflow using a2a.discover executes | YAML engine integration |
| 001.5-E2E-003 | E2E | P0 | coordinator-worker.yaml example runs successfully | Documentation accuracy |
| 001.5-E2E-004 | E2E | P1 | broadcast-consensus.yaml example runs successfully | Documentation accuracy |
| 001.5-E2E-005 | E2E | P1 | delegation-fallback.yaml example runs successfully | Documentation accuracy |
| 001.5-E2E-006 | E2E | P0 | Multi-agent timeout handling scenario | Robustness validation |

### AC9: Rust Design Document

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-042 | Unit | P2 | N/A - Design document only | No code to test |

---

## Risk Coverage Matrix

| Risk | Probability | Impact | Test Scenarios Mitigating |
|------|-------------|--------|---------------------------|
| Message loss in queue | Medium | High | 001.5-UNIT-023, 001.5-INT-002 |
| Namespace leak (security) | Low | Critical | 001.5-UNIT-013, 001.5-UNIT-031, 001.5-INT-005 |
| Delegation timeout not handled | Medium | High | 001.5-UNIT-018-020, 001.5-INT-007 |
| Correlation ID spoofing | Low | High | 001.5-UNIT-021 |
| Concurrent state corruption | Medium | High | 001.5-UNIT-032, 001.5-INT-009 |
| Broadcast storm (performance) | Low | Medium | 001.5-UNIT-015 (edge case coverage) |
| TTL cleanup failure | Low | Medium | 001.5-UNIT-033 |

---

## Recommended Execution Order

1. **P0 Unit tests (32 tests)** - Fail fast on core logic issues
2. **P0 Integration tests (8 tests)** - Verify component interactions
3. **P0 E2E tests (3 tests)** - Critical path validation
4. **P1 Unit tests (8 tests)** - Secondary logic validation
5. **P1 Integration tests (6 tests)** - Extended flow verification
6. **P1 E2E tests (3 tests)** - Additional journey coverage
7. **P2 tests (8 tests)** - As time permits

---

## Coverage Summary by Component

| Component | Unit | Integration | E2E | Total |
|-----------|------|-------------|-----|-------|
| a2a.send | 4 | 1 | 0 | 5 |
| a2a.receive | 7 | 2 | 0 | 9 |
| a2a.broadcast | 4 | 2 | 0 | 6 |
| a2a.delegate | 7 | 2 | 1 | 10 |
| MessageQueue | 6 | 0 | 0 | 6 |
| SharedState | 6 | 2 | 1 | 9 |
| Discovery | 6 | 2 | 0 | 8 |
| YAML Integration | 1 | 5 | 4 | 10 |
| **Total** | **41** | **16** | **6** | **63*** |

*Note: Some tests cover multiple ACs; unique scenario count is 54.

---

## Test Data Requirements

### Mock Agents
```python
MOCK_AGENTS = [
    {"agent_id": "coordinator", "namespace": "research-team", "capabilities": ["coordinate", "assign"]},
    {"agent_id": "worker-1", "namespace": "research-team", "capabilities": ["search", "summarize"]},
    {"agent_id": "worker-2", "namespace": "research-team", "capabilities": ["search", "analyze"]},
    {"agent_id": "external-agent", "namespace": "other-team", "capabilities": ["external"]},
]
```

### Message Fixtures
```python
SAMPLE_MESSAGES = [
    {"type": "task_assignment", "payload": {"task": "search", "query": "test"}},
    {"type": "status_update", "payload": {"progress": 50}},
    {"type": "task_result", "payload": {"result": "success", "data": {...}}},
]
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Security-critical paths have P0 coverage (namespace isolation, correlation ID)
- [x] Error/timeout handling thoroughly tested

---

## Gate YAML Block

```yaml
test_design:
  story_id: "TEA-AGENT-001.5"
  scenarios_total: 54
  by_level:
    unit: 32
    integration: 16
    e2e: 6
  by_priority:
    p0: 28
    p1: 18
    p2: 8
  coverage_gaps: []
  risk_mitigations:
    - namespace_isolation: 3 tests
    - timeout_handling: 5 tests
    - data_integrity: 4 tests
  critical_paths_covered:
    - send_receive_roundtrip
    - delegation_with_fallback
    - shared_state_concurrency
    - namespace_security
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-AGENT-001.5-test-design-20260105.md
P0 tests identified: 28
P1 tests identified: 18
P2 tests identified: 8
Total scenarios: 54
```
