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

**Given-When-Then Examples:**

```gherkin
# 001.5R-UNIT-001: Send to named agent
Given ChannelRegistry with agent "worker-1" registered (capacity=100)
When a2a.send to="worker-1" message={type: "task", payload: {"id": 1}}
Then message appears in worker-1's receive channel
And sender receives Ok(()) immediately

# 001.5R-UNIT-004: Backpressure handling
Given bounded channel with capacity=2 and 2 messages already queued
When third message sent via a2a.send
Then send blocks OR returns WouldBlock error (depending on try_send vs send)
And no message is lost

# 001.5R-INT-001: Fire-and-forget pattern
Given agent "sender" and agent "receiver" in same namespace
When sender calls a2a.send to="receiver" and immediately continues execution
Then sender completes its node without waiting for receiver to process
And receiver eventually processes the message
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

**Given-When-Then Examples:**

```gherkin
# 001.5R-UNIT-007: Timeout handling
Given empty channel (no messages pending)
When a2a.receive timeout_ms=100
Then crossbeam::select! triggers timeout branch after ~100ms
And Err(Timeout) is returned

# 001.5R-UNIT-009: require_all coordination
Given expecting messages from [agent-a, agent-b, agent-c]
And agent-a and agent-b have sent messages
And agent-c has not sent
When a2a.receive from=["agent-a","agent-b","agent-c"] require_all=true timeout_ms=1000
Then receiver waits beyond receiving agent-a and agent-b messages
And returns Timeout error after 1000ms if agent-c never sends

# 001.5R-INT-002: Multi-agent message collection
Given 3 agents [w1, w2, w3] sending concurrently
When coordinator calls a2a.receive from=["w1","w2","w3"] require_all=true
Then Vec<Message> with exactly 3 messages returned
And each message.from matches one of [w1, w2, w3]
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

**Given-When-Then Examples:**

```gherkin
# 001.5R-UNIT-014: Namespace isolation
Given namespace-a with agents [agent-1, agent-2]
And namespace-b with agents [agent-3]
When a2a.broadcast namespace="namespace-a" message={type: "alert"}
Then agent-1.receive() returns the message
And agent-2.receive() returns the message
And agent-3.receive(timeout_ms=100) returns Timeout (no message)

# 001.5R-UNIT-015: Agent type filter
Given namespace with agents:
  - {id: "w1", type: "worker"}
  - {id: "w2", type: "worker"}
  - {id: "c1", type: "coordinator"}
When a2a.broadcast type_filter="worker"
Then w1 and w2 receive the broadcast
And c1 does not receive (different type)
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

**Given-When-Then Examples:**

```gherkin
# 001.5R-UNIT-019: Correlation ID matching
Given coordinator has sent delegate request with correlation_id=X
And responder sends back 3 responses with correlation_ids [Y, X, Z]
When coordinator receives responses
Then only response with correlation_id=X is returned to coordinator's delegate call
And responses Y and Z are discarded or queued for other pending delegates

# 001.5R-UNIT-021: Fallback execution
Given responder agent is non-responsive
And delegate configured with:
  on_timeout: fallback_local
  timeout_ms: 100
  fallback: {action: "local_search", with: {query: "test"}}
When a2a.delegate to="responder"
Then after 100ms timeout fires
And fallback action "local_search" executes locally
And delegate returns fallback action's result

# 001.5R-UNIT-023: Concurrent delegate isolation
Given coordinator issues 10 delegate requests simultaneously to 10 workers
And each worker responds after random delay (10-100ms)
When all responses arrive (potentially out of order)
Then each delegate call receives exactly its correlated response
And no response is delivered to wrong delegate caller
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

**Given-When-Then Examples:**

```gherkin
# 001.5R-UNIT-026: CAS success
Given shared state with key "leader" = null
When agent-1 calls a2a.state.cas(key="leader", expected=null, new_value="agent-1")
Then CAS returns true (success)
And state.get("leader") returns "agent-1"

# 001.5R-UNIT-027: CAS failure
Given shared state with key "leader" = "agent-1"
When agent-2 calls a2a.state.cas(key="leader", expected=null, new_value="agent-2")
Then CAS returns false (current != expected)
And state.get("leader") still returns "agent-1"

# 001.5R-INT-006: Concurrent leader election [SECURITY-TEST: Race Condition]
Given shared state with key "leader" = null
And 10 agents spawn threads simultaneously
When all 10 call a2a.state.cas(key="leader", expected=null, new_value=self.id)
Then exactly 1 CAS returns true
And exactly 9 CAS return false
And state.get("leader") contains the winning agent's ID

# 001.5R-UNIT-029: TTL expiration
Given state.set(key="temp", value="data", ttl_seconds=1)
When 2 seconds elapse
Then state.get("temp") returns None (expired)
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

**Given-When-Then Examples:**

```gherkin
# 001.5R-UNIT-032: Capability advertisement
Given agent "search-worker" registers with capabilities: ["search", "summarize"]
When any agent calls a2a.discover(namespace="team")
Then result contains agent with id="search-worker"
And agent.capabilities == ["search", "summarize"]

# 001.5R-INT-008: Dynamic discovery
Given workflow starts with 2 static agents [a1, a2]
When third agent "a3" registers dynamically at runtime
And another agent calls a2a.discover()
Then result contains all 3 agents [a1, a2, a3]
```

---

### AC7: Message Persistence (Optional)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5R-UNIT-035 | Unit | P2 | persistence: true enables file-backed queue | Config handling |
| 001.5R-UNIT-036 | Unit | P2 | Messages serialized via bincode to file | Serialization logic |
| 001.5R-INT-009 | Integration | P2 | Recovery on process restart - pending messages restored | Critical recovery path |
| 001.5R-INT-010 | Integration | P2 | Max file size respected - oldest messages dropped | Size limit handling |

**Given-When-Then Examples:**

```gherkin
# 001.5R-INT-009: Persistence recovery
Given persistence=true and 5 messages sent before "process crash"
When ChannelRegistry recreated with same persistence_path
Then a2a.receive() returns the 5 previously persisted messages
And messages are in original send order (FIFO preserved)

# 001.5R-INT-010: Max file size enforcement
Given max_file_size=1KB and persistence enabled
And 2KB worth of messages sent
When checking persistence file
Then file size <= 1KB
And oldest messages were dropped to stay under limit
```

---

### AC8: Feature Flag

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5R-UNIT-037 | Unit | P1 | Actions behind --features a2a flag compile only when enabled | Conditional compilation |
| 001.5R-INT-011 | Integration | P1 | Full a2a module available with feature enabled | Feature gate integration |
| 001.5R-INT-012 | Integration | P1 | Build succeeds without a2a feature (no crossbeam/dashmap deps) | Clean feature separation |

**Given-When-Then Examples:**

```gherkin
# 001.5R-INT-011: Feature enabled
Given Cargo.toml with [features] a2a = ["crossbeam-channel", "dashmap"]
When cargo build --features a2a
Then all a2a.* actions compile successfully
And binary includes crossbeam and dashmap

# 001.5R-INT-012: Feature disabled
Given default features (no a2a)
When cargo build
Then a2a module is not compiled
And binary does not link crossbeam or dashmap
And cargo tree --features default shows no crossbeam/dashmap
```

---

## E2E Test Scenarios

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5R-E2E-001 | E2E | P0 | Multi-agent YAML workflow - coordinator delegates to workers, aggregates results | Critical user journey |
| 001.5R-E2E-002 | E2E | P0 | Delegate timeout fallback - full workflow continues via local fallback | Failure recovery path |
| 001.5R-E2E-003 | E2E | P1 | Leader election scenario - 3 agents coordinate, 1 becomes leader | Real-world coordination |
| 001.5R-E2E-004 | E2E | P1 | Namespace-isolated workflows - two teams run independently | Multi-tenant scenario |

**Given-When-Then Examples:**

```gherkin
# 001.5R-E2E-001: Multi-agent coordinator-worker workflow
Given YAML workflow defining:
  - coordinator agent with delegate logic
  - 3 worker agents with search capability
  - aggregation logic in coordinator
When workflow executed with input {query: "research topic"}
Then coordinator sends tasks to all 3 workers
And workers each return results
And coordinator aggregates into final output
And workflow output contains combined research results

# 001.5R-E2E-002: Delegate timeout with fallback
Given YAML workflow with delegate action:
  to: "offline-agent"
  timeout_ms: 100
  on_timeout: fallback_local
  fallback: {action: web.scrape, ...}
And "offline-agent" is not registered
When workflow executes the delegate node
Then timeout fires after 100ms
And fallback web.scrape action executes
And workflow continues with fallback result
And no error is raised to user

# 001.5R-E2E-004: Namespace isolation [SECURITY-TEST]
Given two YAML workflows:
  - team-alpha with namespace="alpha" and agent "worker"
  - team-beta with namespace="beta" and agent "worker" (same name)
When both workflows run concurrently
Then team-alpha's messages stay in namespace "alpha"
And team-beta's messages stay in namespace "beta"
And no cross-namespace message leakage occurs
And state keys are isolated between namespaces
```

---

## Risk Coverage Matrix

| Risk | Severity | Test IDs Mitigating | Notes |
|------|----------|---------------------|-------|
| Race conditions in DashMap | High | 001.5R-INT-006, 001.5R-UNIT-026, 001.5R-UNIT-027 | 10-agent concurrent CAS simulation |
| Deadlock in channel operations | High | 001.5R-UNIT-007, 001.5R-INT-004, 001.5R-UNIT-009 | Timeout + select! verification |
| Message loss under backpressure | High | 001.5R-UNIT-004, 001.5R-INT-003 | Bounded channel behavior |
| Correlation ID mismatches | Medium | 001.5R-UNIT-018, 001.5R-UNIT-019, 001.5R-UNIT-023 | Out-of-order response handling |
| Namespace leakage | Medium | 001.5R-UNIT-014, 001.5R-UNIT-028, 001.5R-E2E-004 | Security boundary tests |
| Persistence file corruption | Low | 001.5R-INT-009, 001.5R-INT-010 | Write/recovery path |
| Feature flag regression | Low | 001.5R-INT-011, 001.5R-INT-012 | Conditional compilation |

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
      annotation: "[SECURITY-TEST: Race Condition]"
    - id: "001.5R-UNIT-009"
      description: "require_all waits for all agents"
      risk: "Deadlock potential"
    - id: "001.5R-UNIT-004"
      description: "Bounded channel backpressure"
      risk: "Memory exhaustion"
    - id: "001.5R-E2E-004"
      description: "Namespace isolation"
      risk: "Security boundary"
      annotation: "[SECURITY-TEST]"
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
- [x] Given-When-Then examples provided for all ACs
- [x] Security test annotations added where applicable

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-AGENT-001.5-rust-test-design-20260105.md
P0 tests identified: 30
P1 tests identified: 16
P2 tests identified: 8
Total scenarios: 54
Story reference: docs/stories/TEA-AGENT-001.5-rust-a2a-communication.md
```
