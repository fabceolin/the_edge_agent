# Story TEA-AGENT-001.5-rust: Inter-Agent Communication (Rust/Embedded)

## Status

**Ready for Development**

*Updated 2026-01-05: QA validation passed - all acceptance criteria have test coverage, risk areas identified and addressed.*

## Story

**As a** developer deploying multi-agent systems to edge environments,
**I want** built-in inter-agent communication primitives in the Rust runtime,
**so that** agents can coordinate within a single embedded binary without external message brokers.

## Background

This is the Rust adaptation of TEA-AGENT-001.5, **particularly well-suited** for embedded/offline execution. The single-process, in-memory constraint of the Python version is actually a **feature** for embedded deployment.

| Aspect | Python Version | Rust Version |
|--------|---------------|--------------|
| **Message Queue** | Thread-safe dict | crossbeam channels |
| **Shared State** | Python dict + lock | DashMap (lock-free) |
| **Concurrency** | Threading | rayon + crossbeam |
| **Memory** | Python GC | Zero-copy where possible |
| **Persistence** | None (in-memory) | Optional bincode to file |

## Scope

### In Scope
- `a2a.send` - Send message via crossbeam channel
- `a2a.receive` - Receive with timeout
- `a2a.broadcast` - Broadcast to namespace
- `a2a.delegate` - Request/response pattern
- `a2a.state.get` / `a2a.state.set` - Lock-free shared state
- `a2a.discover` - Agent discovery within process
- In-memory message queue (crossbeam-channel)
- Optional message persistence (bincode to file)

### Out of Scope
- External message brokers (Redis, NATS)
- Cross-process communication
- Network-based agent discovery

## Acceptance Criteria

### AC1: `a2a.send` Action (Crossbeam)
1. Sends message to named agent via crossbeam channel
2. Message: `{type, payload, correlation_id}`
3. Non-blocking send (bounded channel with backpressure)
4. Returns immediately (fire-and-forget)

### AC2: `a2a.receive` Action
1. Receives from specified agents with timeout
2. Timeout via `crossbeam::select!` with deadline
3. Filter by message type
4. `require_all`: Wait for all agents or any
5. Returns `Vec<Message>` or timeout error

### AC3: `a2a.broadcast` Action
1. Sends to all agents in namespace
2. Namespace = separate channel registry
3. Agent type filter (optional)
4. Non-blocking broadcast

### AC4: `a2a.delegate` Action
1. Request/response via paired channels
2. Automatic correlation_id (UUID v7)
3. Timeout with configurable duration
4. `on_timeout`: `fallback_local`, `retry`, `raise`
5. Response matched by correlation_id

### AC5: Shared State (DashMap)
1. `a2a.state.get` - Lock-free read
2. `a2a.state.set` - Lock-free write with optional CAS
3. Namespace-scoped keys
4. TTL via background cleanup task
5. `a2a.state.cas` - Compare-and-swap for coordination

### AC6: Agent Discovery
1. `a2a.discover` returns registered agents
2. Agents register at workflow start
3. Capabilities advertised in registration
4. Discovery modes: `static` (from YAML), `dynamic` (runtime)

### AC7: Message Persistence (Optional)
1. `persistence: true` enables file-backed queue
2. Messages serialized via bincode
3. Recovery on process restart
4. Configurable max file size

### AC8: Feature Flag
1. Actions behind `--features a2a` cargo flag
2. `crossbeam-channel` and `dashmap` only when enabled

## Tasks / Subtasks

- [ ] **Task 1: Channel Registry** (AC: 1, 2)
  - [ ] Define `ChannelRegistry` with per-agent channels
  - [ ] Implement channel creation/lookup
  - [ ] Namespace isolation
  - [ ] Unit tests

- [ ] **Task 2: `a2a.send` Action** (AC: 1)
  - [ ] Implement send with bounded channel
  - [ ] Message serialization
  - [ ] Backpressure handling
  - [ ] Unit tests

- [ ] **Task 3: `a2a.receive` Action** (AC: 2)
  - [ ] Implement receive with crossbeam::select!
  - [ ] Timeout handling
  - [ ] Message type filtering
  - [ ] require_all logic
  - [ ] Unit tests

- [ ] **Task 4: `a2a.broadcast` Action** (AC: 3)
  - [ ] Implement broadcast to namespace
  - [ ] Agent type filtering
  - [ ] Unit tests

- [ ] **Task 5: `a2a.delegate` Action** (AC: 4)
  - [ ] Implement request/response pattern
  - [ ] Correlation ID generation (UUIDv7)
  - [ ] Response matching
  - [ ] Timeout strategies
  - [ ] Unit tests

- [ ] **Task 6: Shared State** (AC: 5)
  - [ ] Implement with DashMap
  - [ ] Namespace scoping
  - [ ] TTL cleanup task
  - [ ] CAS operation
  - [ ] Unit tests

- [ ] **Task 7: Agent Discovery** (AC: 6)
  - [ ] Agent registration
  - [ ] Capability advertisement
  - [ ] Discovery action
  - [ ] Unit tests

- [ ] **Task 8: Message Persistence** (AC: 7)
  - [ ] File-backed queue implementation
  - [ ] bincode serialization
  - [ ] Recovery logic
  - [ ] Unit tests

- [ ] **Task 9: Feature Flag & Integration** (AC: 8)
  - [ ] Add `a2a` feature to Cargo.toml
  - [ ] Conditional compilation
  - [ ] Integration tests

## Dev Notes

### Source Tree

```
rust/src/
├── engine/
│   ├── actions/
│   │   └── a2a.rs            # NEW: A2A actions
│   └── a2a/
│       ├── mod.rs
│       ├── channel.rs        # Channel registry
│       ├── message.rs        # Message types
│       ├── shared_state.rs   # DashMap wrapper
│       ├── discovery.rs      # Agent discovery
│       └── persistence.rs    # Optional file backing
```

### YAML Syntax

#### Agent Configuration
```yaml
name: worker-agent
settings:
  a2a:
    enabled: true
    namespace: research-team
    agent_id: worker-1
    capabilities: [search, summarize]
    channel_capacity: 100
    persistence: false
```

#### Send/Receive
```yaml
nodes:
  - name: send_result
    action: a2a.send
    with:
      to: coordinator
      message:
        type: task_complete
        payload: "{{ state.result }}"

  - name: wait_for_task
    action: a2a.receive
    with:
      from: [coordinator]
      type: task_assignment
      timeout_ms: 30000
```

#### Delegation
```yaml
nodes:
  - name: delegate_search
    action: a2a.delegate
    with:
      to: search-worker
      request:
        type: search
        query: "{{ state.query }}"
      timeout_ms: 60000
      on_timeout: fallback_local
      fallback:
        action: web.scrape
        with:
          url: "https://example.com/search?q={{ state.query }}"
```

#### Shared State
```yaml
nodes:
  - name: update_progress
    action: a2a.state.set
    with:
      namespace: research-team
      key: progress
      value:
        completed: "{{ state.completed }}"
        total: "{{ state.total }}"
      ttl_seconds: 3600

  - name: coordinate
    action: a2a.state.cas
    with:
      namespace: research-team
      key: leader
      expected: null
      new_value: "{{ state.agent_id }}"
      # Returns true if CAS succeeded (became leader)
```

### Rust Types

```rust
use crossbeam_channel::{Sender, Receiver, bounded, select};
use dashmap::DashMap;
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    pub id: Uuid,
    pub correlation_id: Option<Uuid>,
    pub from: String,
    pub to: String,  // Or "*" for broadcast
    pub namespace: String,
    pub msg_type: String,
    pub payload: serde_json::Value,
    pub timestamp: i64,
    pub ttl_ms: Option<u64>,
}

pub struct ChannelRegistry {
    channels: DashMap<String, (Sender<Message>, Receiver<Message>)>,
    namespace: String,
}

pub struct SharedState {
    data: DashMap<String, (serde_json::Value, Option<Instant>)>,
    namespace: String,
}

#[derive(Debug, Clone, Copy)]
pub enum OnTimeout {
    FallbackLocal,
    Retry { max_retries: u32 },
    Raise,
}
```

### Crossbeam Select Example

```rust
use crossbeam_channel::{select, after};
use std::time::Duration;

pub fn receive_with_timeout(
    receivers: &[Receiver<Message>],
    timeout: Duration,
    require_all: bool,
) -> Result<Vec<Message>, A2AError> {
    let deadline = after(timeout);
    let mut messages = Vec::new();
    let mut received_from = HashSet::new();

    loop {
        select! {
            recv(deadline) -> _ => {
                if require_all && received_from.len() < receivers.len() {
                    return Err(A2AError::Timeout);
                }
                return Ok(messages);
            }
            // Dynamic select over all receivers
            default => {
                for (i, rx) in receivers.iter().enumerate() {
                    if let Ok(msg) = rx.try_recv() {
                        messages.push(msg);
                        received_from.insert(i);

                        if !require_all {
                            return Ok(messages);
                        }
                        if received_from.len() == receivers.len() {
                            return Ok(messages);
                        }
                    }
                }
                std::thread::sleep(Duration::from_millis(1));
            }
        }
    }
}
```

### Dependencies

```toml
[dependencies]
crossbeam-channel = "0.5"
dashmap = "5.5"
uuid = { version = "1.6", features = ["v7"] }
bincode = "1.3"  # For persistence

[features]
a2a = ["crossbeam-channel", "dashmap", "uuid"]
```

### Embedded Advantages

| Feature | Why It's Good for Embedded |
|---------|---------------------------|
| **In-memory only** | No external broker needed |
| **Single binary** | All agents in one process |
| **Lock-free shared state** | Low latency coordination |
| **Bounded channels** | Predictable memory usage |
| **Optional persistence** | Survive restarts when needed |

### Related Stories
- TEA-RUST-001: Rust Migration Epic
- TEA-AGENT-001.5: Python version (reference)
- TEA-AGENT-001.1-rust: Multi-Agent (agent registry)

## Testing

### Test File Location
- `rust/tests/test_a2a_actions.rs`

### Test Categories

| Category | Count | Priority |
|----------|-------|----------|
| Channel Registry | 6 | P0 |
| a2a.send | 4 | P0 |
| a2a.receive | 8 | P0 |
| a2a.broadcast | 4 | P1 |
| a2a.delegate | 8 | P0 |
| Shared State | 6 | P0 |
| Discovery | 4 | P1 |
| Persistence | 4 | P2 |

### Key Test Scenarios

1. **Send/receive round-trip** - Message delivered correctly
2. **Timeout handling** - Receive times out as expected
3. **require_all** - Waits for all specified agents
4. **Delegation success** - Request/response completes
5. **Delegation timeout** - Fallback executed
6. **CAS coordination** - Only one agent becomes leader
7. **Namespace isolation** - Cross-namespace blocked
8. **Persistence recovery** - Messages survive restart

## QA Notes

**Date:** 2026-01-05
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| **Total Test Scenarios** | 54 |
| **Unit Tests** | 32 (59%) |
| **Integration Tests** | 18 (33%) |
| **E2E Tests** | 4 (8%) |
| **P0 (Critical)** | 30 |
| **P1 (High)** | 16 |
| **P2 (Medium)** | 8 |

All 8 acceptance criteria have explicit test coverage with no identified gaps.

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **Race conditions in DashMap** | High | P0 concurrent CAS tests (001.5R-INT-006) with 10-agent race simulation |
| **Deadlocks in channel operations** | High | P0 timeout tests with crossbeam::select! (001.5R-UNIT-007, 001.5R-UNIT-009) |
| **Message loss under backpressure** | High | P0 bounded channel tests (001.5R-UNIT-004) with capacity exhaustion |
| **Correlation ID mismatches** | Medium | P0 tests for concurrent delegates (001.5R-UNIT-023) with out-of-order responses |
| **Namespace leakage** | Medium | P0 isolation tests (001.5R-UNIT-014, 001.5R-UNIT-028) |
| **Persistence file corruption** | Low | P2 recovery tests (001.5R-INT-009, 001.5R-INT-010) |

### Recommended Test Scenarios (Priority Order)

**Phase 1 - Fail Fast (P0 Unit):**
- Send/receive core functionality (001.5R-UNIT-001 to 010)
- Delegate pattern with correlation matching (001.5R-UNIT-017 to 021)
- Shared state CAS operations (001.5R-UNIT-024 to 028)

**Phase 2 - Integration Critical (P0):**
- Fire-and-forget pattern validation (001.5R-INT-001)
- Multi-agent receive collection (001.5R-INT-002)
- Full delegate round-trip with fallback (001.5R-INT-004, 005)
- Concurrent CAS leader election (001.5R-INT-006)

**Phase 3 - E2E Critical (P0):**
- Multi-agent coordinator-worker workflow (001.5R-E2E-001)
- Delegate timeout fallback path (001.5R-E2E-002)

### Concerns and Blockers

| Type | Description | Recommendation |
|------|-------------|----------------|
| **Concern** | `receive_with_timeout` example uses busy-loop with 1ms sleep - may cause CPU spinning | Consider using crossbeam's `select!` with proper channel iteration instead of polling |
| **Concern** | No tests specified for channel cleanup on agent deregistration | Add P1 test for orphaned channel handling |
| **Concern** | UUID v7 dependency for correlation IDs - ensure monotonicity guarantees documented | Verify ordering requirements for correlation ID matching |
| **Note** | Persistence feature (AC7) is optional and lower priority (P2) - acceptable for MVP | Defer persistence tests to post-MVP validation |

### Test Design Assessment

**PASS** - Story is ready for development with comprehensive test coverage.

- All acceptance criteria mapped to test scenarios
- Risk-based prioritization aligns with embedded deployment concerns
- Rust-specific concurrency risks explicitly addressed
- Test levels appropriate (heavy unit testing for lock-free logic)

**Reference:** `docs/qa/assessments/TEA-AGENT-001.5-rust-test-design-20260105.md`

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-05 | 0.1 | Initial Rust adaptation from TEA-AGENT-001.5 | Sarah (PO) |
| 2026-01-05 | 0.2 | Added QA Notes with test design assessment | Quinn (QA) |
