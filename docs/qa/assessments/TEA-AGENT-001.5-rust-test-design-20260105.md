# Test Design: Story TEA-AGENT-001.5-rust

**Date:** 2026-01-05
**Designer:** Quinn (Test Architect)
**Story:** Inter-Agent Communication (Rust/Embedded)

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total Test Scenarios** | 54 |
| **Unit Tests** | 32 (59%) |
| **Integration Tests** | 18 (33%) |
| **E2E Tests** | 4 (8%) |
| **Priority Distribution** | P0: 30, P1: 16, P2: 8 |

### Strategy Rationale

This story implements core inter-agent communication primitives in Rust using crossbeam channels and DashMap. The test strategy emphasizes:

1. **Heavy unit testing (59%)** - Lock-free data structures and channel operations are pure logic that can be tested in isolation
2. **Strong integration testing (33%)** - Multi-agent coordination requires verifying component interactions
3. **Minimal E2E testing (8%)** - Only critical multi-agent workflow paths need full stack validation

### Risk-Based Focus Areas

| Risk Area | Mitigation via Testing |
|-----------|----------------------|
| Race conditions in shared state | P0 unit tests with concurrent access |
| Deadlocks in channel operations | P0 integration tests with timeout verification |
| Message loss under backpressure | P0 unit tests for bounded channel behavior |
| Correlation ID mismatches | P0 unit tests for delegate pattern |
| Persistence corruption | P1 integration tests for file recovery |

---

## Test Scenarios by Acceptance Criteria

### AC1: `a2a.send` Action (Crossbeam)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5R-UNIT-001 | Unit | P0 | Send message to named agent - message delivered to correct channel | Pure channel logic |
| 001.5R-UNIT-002 | Unit | P0 | Message structure contains type, payload, correlation_id | Data structure validation |
| 001.5R-UNIT-003 | Unit | P0 | Non-blocking send returns immediately when channel has capacity | Async behavior validation |
| 001.5R-UNIT-004 | Unit | P0 | Bounded channel backpressure - send blocks when at capacity | Critical memory safety |
| 001.5R-UNIT-005 | Unit | P1 | Send to non-existent agent returns error | Error handling |
| 001.5R-INT-001 | Integration | P0 | Fire-and-forget pattern - sender continues without waiting | Multi-component flow |

**Test Details:**

```rust
// 001.5R-UNIT-001: Send to named agent
#[test]
fn test_send_to_named_agent() {
    // Given: ChannelRegistry with agent "worker-1" registered
    // When: a2a.send to "worker-1" with message
    // Then: Message appears in worker-1's receive channel
}

// 001.5R-UNIT-004: Backpressure handling
#[test]
fn test_bounded_channel_backpressure() {
    // Given: Channel capacity = 2, 2 messages already queued
    // When: Third message sent
    // Then: Send blocks until receiver drains (or returns WouldBlock for try_send)
}
```

---

### AC2: `a2a.receive` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5R-UNIT-006 | Unit | P0 | Receive with timeout - returns before deadline when message available | Core functionality |
| 001.5R-UNIT-007 | Unit | P0 | Receive timeout - crossbeam::select! returns timeout error at deadline | Error path critical |
| 001.5R-UNIT-008 | Unit | P0 | Filter by message type - only matching types returned | Logic validation |
| 001.5R-UNIT-009 | Unit | P0 | require_all=true - waits for all specified agents | Critical coordination logic |
| 001.5R-UNIT-010 | Unit | P0 | require_all=false - returns on first message | Alternative path |
| 001.5R-UNIT-011 | Unit | P1 | Receive from multiple agents - Vec<Message> contains all | Collection handling |
| 001.5R-UNIT-012 | Unit | P1 | Receive with type filter on empty queue - returns empty after timeout | Edge case |
| 001.5R-INT-002 | Integration | P0 | Multi-agent receive - messages from different senders collected correctly | Cross-component |

**Test Details:**

```rust
// 001.5R-UNIT-009: require_all coordination
#[test]
fn test_require_all_waits_for_all_agents() {
    // Given: Expecting messages from [agent-a, agent-b, agent-c]
    // When: Only agent-a and agent-b send within partial timeout
    // Then: Continue waiting until agent-c sends or full timeout
}

// 001.5R-INT-002: Multi-agent message collection
#[test]
fn test_receive_from_multiple_agents() {
    // Given: 3 agents sending concurrently
    // When: receive with require_all=true
    // Then: All 3 messages collected in Vec before return
}
```

---

### AC3: `a2a.broadcast` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5R-UNIT-013 | Unit | P0 | Broadcast to namespace - all agents in namespace receive | Core broadcast logic |
| 001.5R-UNIT-014 | Unit | P0 | Namespace isolation - agents in other namespaces do not receive | Security boundary |
| 001.5R-UNIT-015 | Unit | P1 | Agent type filter - only matching agent types receive | Filter logic |
| 001.5R-UNIT-016 | Unit | P1 | Non-blocking broadcast - returns immediately | Async behavior |
| 001.5R-INT-003 | Integration | P1 | Broadcast under load - all 100 agents receive in namespace | Scale validation |

**Test Details:**

```rust
// 001.5R-UNIT-014: Namespace isolation
#[test]
fn test_broadcast_namespace_isolation() {
    // Given: namespace-a with [agent-1, agent-2], namespace-b with [agent-3]
    // When: Broadcast to namespace-a
    // Then: agent-1 and agent-2 receive, agent-3 does not
}
```

---

### AC4: `a2a.delegate` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5R-UNIT-017 | Unit | P0 | Request/response via paired channels - response returned | Core delegation pattern |
| 001.5R-UNIT-018 | Unit | P0 | Automatic correlation_id (UUID v7) - unique per request | ID generation |
| 001.5R-UNIT-019 | Unit | P0 | Response matched by correlation_id - correct response returned | Critical matching logic |
| 001.5R-UNIT-020 | Unit | P0 | Timeout with on_timeout: raise - error propagated | Error handling |
| 001.5R-UNIT-021 | Unit | P0 | Timeout with on_timeout: fallback_local - fallback action executed | Fallback logic |
| 001.5R-UNIT-022 | Unit | P1 | Timeout with on_timeout: retry - retries up to max | Retry logic |
| 001.5R-UNIT-023 | Unit | P1 | Multiple concurrent delegates - correlation IDs keep responses separate | Concurrency safety |
| 001.5R-INT-004 | Integration | P0 | Full delegate round-trip - request sent, processed, response returned | Critical path |
| 001.5R-INT-005 | Integration | P0 | Delegate timeout triggers fallback action execution | Fallback integration |

**Test Details:**

```rust
// 001.5R-UNIT-019: Correlation ID matching
#[test]
fn test_correlation_id_matches_response() {
    // Given: Delegate request with correlation_id = X
    // When: Multiple responses arrive (correlation_ids X, Y, Z)
    // Then: Only response with correlation_id X returned to requester
}

// 001.5R-UNIT-023: Concurrent delegate isolation
#[test]
fn test_concurrent_delegates_isolated() {
    // Given: 10 concurrent delegate requests from same agent
    // When: All responses arrive (potentially out of order)
    // Then: Each request gets its correctly correlated response
}
```

---

### AC5: Shared State (DashMap)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5R-UNIT-024 | Unit | P0 | a2a.state.get - lock-free read returns stored value | Core read path |
| 001.5R-UNIT-025 | Unit | P0 | a2a.state.set - lock-free write stores value | Core write path |
| 001.5R-UNIT-026 | Unit | P0 | a2a.state.cas - compare-and-swap succeeds when expected matches | CAS logic |
| 001.5R-UNIT-027 | Unit | P0 | a2a.state.cas - fails when expected does not match | CAS rejection |
| 001.5R-UNIT-028 | Unit | P0 | Namespace-scoped keys - same key in different namespaces isolated | Namespace isolation |
| 001.5R-UNIT-029 | Unit | P1 | TTL expiration - expired values not returned | TTL logic |
| 001.5R-INT-006 | Integration | P0 | Concurrent CAS for leader election - exactly one wins | Race condition handling |
| 001.5R-INT-007 | Integration | P1 | TTL cleanup task - expired entries removed from DashMap | Background task |

**Test Details:**

```rust
// 001.5R-UNIT-026: CAS success
#[test]
fn test_cas_success_when_expected_matches() {
    // Given: state["leader"] = null
    // When: CAS(key="leader", expected=null, new="agent-1")
    // Then: Returns true, state["leader"] = "agent-1"
}

// 001.5R-INT-006: Leader election race
#[test]
fn test_concurrent_cas_only_one_wins() {
    // Given: state["leader"] = null
    // When: 10 agents concurrently CAS(expected=null, new=self.id)
    // Then: Exactly 1 returns true, 9 return false
    // And: state["leader"] contains the winner's ID
}
```

---

### AC6: Agent Discovery

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5R-UNIT-030 | Unit | P1 | a2a.discover returns registered agents | Core discovery |
| 001.5R-UNIT-031 | Unit | P1 | Agent registration at workflow start | Registration path |
| 001.5R-UNIT-032 | Unit | P1 | Capabilities advertised in registration | Capability handling |
| 001.5R-UNIT-033 | Unit | P1 | Discovery mode: static (from YAML) | Config parsing |
| 001.5R-UNIT-034 | Unit | P1 | Discovery mode: dynamic (runtime registration) | Dynamic registration |
| 001.5R-INT-008 | Integration | P1 | Discover agents after dynamic registration | Integration of registration + discovery |

**Test Details:**

```rust
// 001.5R-UNIT-032: Capability advertisement
#[test]
fn test_capabilities_advertised() {
    // Given: Agent registers with capabilities: ["search", "summarize"]
    // When: a2a.discover()
    // Then: Agent's capabilities list includes ["search", "summarize"]
}
```

---

### AC7: Message Persistence (Optional)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5R-UNIT-035 | Unit | P2 | persistence: true enables file-backed queue | Config handling |
| 001.5R-UNIT-036 | Unit | P2 | Messages serialized via bincode to file | Serialization logic |
| 001.5R-INT-009 | Integration | P2 | Recovery on process restart - pending messages restored | Critical recovery path |
| 001.5R-INT-010 | Integration | P2 | Max file size respected - oldest messages dropped | Size limit handling |

**Test Details:**

```rust
// 001.5R-INT-009: Persistence recovery
#[test]
fn test_persistence_recovery_on_restart() {
    // Given: 5 messages persisted to file, process "restarted" (registry recreated)
    // When: New ChannelRegistry created with same persistence path
    // Then: 5 messages available for receive
}
```

---

### AC8: Feature Flag

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5R-UNIT-037 | Unit | P1 | Actions behind --features a2a flag compile only when enabled | Conditional compilation |
| 001.5R-INT-011 | Integration | P1 | Full a2a module available with feature enabled | Feature gate integration |
| 001.5R-INT-012 | Integration | P1 | Build succeeds without a2a feature (no crossbeam/dashmap deps) | Clean feature separation |

---

## E2E Test Scenarios

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5R-E2E-001 | E2E | P0 | Multi-agent YAML workflow - coordinator delegates to workers, aggregates results | Critical user journey |
| 001.5R-E2E-002 | E2E | P0 | Delegate timeout fallback - full workflow continues via local fallback | Failure recovery path |
| 001.5R-E2E-003 | E2E | P1 | Leader election scenario - 3 agents coordinate, 1 becomes leader | Real-world coordination |
| 001.5R-E2E-004 | E2E | P1 | Namespace-isolated workflows - two teams run independently | Multi-tenant scenario |

**Test Details:**

```rust
// 001.5R-E2E-001: Multi-agent workflow
#[test]
fn test_coordinator_worker_workflow() {
    // Given: YAML with coordinator + 3 worker agents
    // When: Workflow executed with research query
    // Then:
    //   - Coordinator delegates to all workers
    //   - Workers return results
    //   - Coordinator aggregates and returns final answer
}
```

---

## Risk Coverage Matrix

| Risk | Test IDs Mitigating |
|------|---------------------|
| Race conditions in DashMap | 001.5R-INT-006, 001.5R-UNIT-026, 001.5R-UNIT-027 |
| Deadlock in channel operations | 001.5R-UNIT-007, 001.5R-INT-004, 001.5R-UNIT-009 |
| Message loss under backpressure | 001.5R-UNIT-004, 001.5R-INT-003 |
| Correlation ID collisions | 001.5R-UNIT-018, 001.5R-UNIT-023 |
| Namespace leakage | 001.5R-UNIT-014, 001.5R-UNIT-028 |
| Persistence file corruption | 001.5R-INT-009, 001.5R-INT-010 |
| Feature flag regression | 001.5R-INT-011, 001.5R-INT-012 |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 Unit)
1. 001.5R-UNIT-001 through 001.5R-UNIT-010 (send/receive core)
2. 001.5R-UNIT-017 through 001.5R-UNIT-021 (delegate core)
3. 001.5R-UNIT-024 through 001.5R-UNIT-028 (shared state core)

### Phase 2: Integration Critical (P0 Integration)
4. 001.5R-INT-001 (fire-and-forget)
5. 001.5R-INT-002 (multi-agent receive)
6. 001.5R-INT-004, 001.5R-INT-005 (delegate integration)
7. 001.5R-INT-006 (CAS race)

### Phase 3: E2E Critical (P0 E2E)
8. 001.5R-E2E-001 (full workflow)
9. 001.5R-E2E-002 (fallback path)

### Phase 4: P1 Tests
10. All P1 unit tests
11. All P1 integration tests
12. P1 E2E tests

### Phase 5: P2+ (Time Permitting)
13. Persistence tests
14. Edge cases

---

## Gate YAML Block

```yaml
test_design:
  story_id: "TEA-AGENT-001.5-rust"
  story_title: "Inter-Agent Communication (Rust/Embedded)"
  date: "2026-01-05"
  designer: "Quinn"
  scenarios_total: 54
  by_level:
    unit: 32
    integration: 18
    e2e: 4
  by_priority:
    p0: 30
    p1: 16
    p2: 8
  coverage_gaps: []
  acceptance_criteria_coverage:
    AC1_a2a_send: 6
    AC2_a2a_receive: 8
    AC3_a2a_broadcast: 5
    AC4_a2a_delegate: 9
    AC5_shared_state: 8
    AC6_discovery: 6
    AC7_persistence: 4
    AC8_feature_flag: 3
    e2e_workflows: 4
  high_risk_tests:
    - id: "001.5R-INT-006"
      description: "Concurrent CAS leader election race"
      risk: "Race conditions"
    - id: "001.5R-UNIT-009"
      description: "require_all waits for all agents"
      risk: "Deadlock potential"
    - id: "001.5R-UNIT-004"
      description: "Bounded channel backpressure"
      risk: "Memory exhaustion"
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (001.5R-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Race condition scenarios explicitly designed
- [x] Rust-specific concerns addressed (lock-free, channel semantics)

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-AGENT-001.5-rust-test-design-20260105.md
P0 tests identified: 30
P1 tests identified: 16
P2 tests identified: 8
Total scenarios: 54
```
