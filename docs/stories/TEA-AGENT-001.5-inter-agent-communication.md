# Story TEA-AGENT-001.5: Inter-Agent Communication (A2A)

## Status

**Ready for Development**

## Story

**As a** YAML agent developer,
**I want** built-in inter-agent communication primitives,
**so that** I can build distributed agent systems with message passing, delegation, and shared memory.

## Background

Inter-agent communication (Chapter 15 of Agentic Design Patterns) enables agent collaboration beyond simple parallel execution. Currently, users must:

1. Implement custom message passing between agents
2. Handle timeouts and acknowledgments manually
3. Build shared memory abstractions from scratch
4. Coordinate multiple agent instances manually

This story introduces `a2a.*` actions for agent-to-agent communication within TEA workflows.

## Acceptance Criteria

### AC1: `a2a.send` Action
1. Sends message to a specific named agent
2. Messages have type, payload, and optional correlation_id
3. Fire-and-forget (no response wait)
4. Message delivery confirmation optional

### AC2: `a2a.receive` Action
1. Receives messages from specified agents
2. Configurable timeout
3. Filter by message type
4. Returns list of received messages
5. `require_all`: Wait for messages from all specified agents

### AC3: `a2a.broadcast` Action
1. Sends message to all agents in namespace
2. Namespace isolation prevents cross-workflow interference
3. Optional agent type filter (e.g., only "worker" agents)

### AC4: `a2a.delegate` Action
1. Request/response pattern: send task, wait for result
2. Configurable timeout
3. On-timeout strategies: `fallback_local`, `retry`, `raise`
4. Automatic correlation_id matching

### AC5: Message Queue Backend
1. Default: In-memory queue (single-process)
2. Pluggable backend abstraction
3. Future backends: Redis, Cloud Pub/Sub (out of scope for this story)

### AC6: Shared State Namespace
1. `a2a.state.get` / `a2a.state.set` for shared agent memory
2. Namespace-scoped to prevent conflicts
3. Optimistic locking for concurrent updates
4. TTL support for automatic cleanup

### AC7: Agent Discovery
1. `a2a.discover` returns available agents in namespace
2. Agents advertise capabilities at startup
3. Discovery modes: `broadcast`, `registry`, `static`

### AC8: Python Implementation
1. New module: `python/src/the_edge_agent/actions/a2a_actions.py`
2. In-memory message queue implementation
3. All actions registered in `build_actions_registry()`
4. Test coverage >90%

### AC9: Rust Implementation
1. Design document only (complex async/FFI requirements)
2. Future story for Rust implementation

## Tasks / Subtasks

- [ ] **Task 1: Message Queue Abstraction** (AC: 5)
  - [ ] Define `MessageQueue` protocol/interface
  - [ ] Implement `InMemoryMessageQueue`
  - [ ] Message serialization (JSON)
  - [ ] Unit tests

- [ ] **Task 2: `a2a.send` Action** (AC: 1)
  - [ ] Implement send action
  - [ ] Message type and payload handling
  - [ ] Optional delivery confirmation
  - [ ] Unit tests

- [ ] **Task 3: `a2a.receive` Action** (AC: 2)
  - [ ] Implement receive action
  - [ ] Timeout handling
  - [ ] Message type filtering
  - [ ] `require_all` logic
  - [ ] Unit tests

- [ ] **Task 4: `a2a.broadcast` Action** (AC: 3)
  - [ ] Implement broadcast action
  - [ ] Namespace isolation
  - [ ] Agent type filtering
  - [ ] Unit tests

- [ ] **Task 5: `a2a.delegate` Action** (AC: 4)
  - [ ] Implement delegate action
  - [ ] Correlation ID management
  - [ ] Timeout strategies
  - [ ] Unit tests

- [ ] **Task 6: Shared State** (AC: 6)
  - [ ] Implement `a2a.state.get`
  - [ ] Implement `a2a.state.set`
  - [ ] Optimistic locking
  - [ ] TTL support
  - [ ] Unit tests

- [ ] **Task 7: Agent Discovery** (AC: 7)
  - [ ] Implement `a2a.discover`
  - [ ] Capability advertisement
  - [ ] Discovery modes
  - [ ] Unit tests

- [ ] **Task 8: Integration Tests**
  - [ ] Multi-agent workflow test
  - [ ] Delegation round-trip test
  - [ ] Broadcast test
  - [ ] Timeout handling test

- [ ] **Task 9: Documentation & Examples**
  - [ ] Update YAML_REFERENCE.md
  - [ ] Create example: coordinator-worker.yaml
  - [ ] Create example: broadcast-consensus.yaml
  - [ ] Create example: delegation-fallback.yaml

- [ ] **Task 10: Rust Design Document** (AC: 9)
  - [ ] Document async requirements
  - [ ] FFI considerations
  - [ ] Proposed implementation approach

## Dev Notes

### Source Tree Context

**Python:**
```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py       # Add a2a_actions
│   ├── a2a_actions.py    # NEW: Inter-agent communication
│   └── ...
├── a2a/
│   ├── __init__.py
│   ├── message_queue.py  # Message queue abstraction
│   ├── shared_state.py   # Shared state namespace
│   └── discovery.py      # Agent discovery
```

**Rust (Design Doc Only):**
```
rust/src/engine/
├── actions/
│   └── a2a_design.md     # Design document for future impl
```

### YAML Syntax Reference

#### Agent Configuration
```yaml
name: worker-agent
settings:
  a2a:
    enabled: true
    namespace: research-team
    agent_id: worker-1            # Unique within namespace
    capabilities: [search, summarize]
    discovery: broadcast          # broadcast | registry | static
```

#### Send/Receive
```yaml
nodes:
  - name: send_status
    action: a2a.send
    with:
      to: coordinator
      message:
        type: status_update
        payload:
          progress: "{{ state.progress }}"

  - name: wait_for_work
    action: a2a.receive
    with:
      from: [coordinator]
      type: task_assignment
      timeout: 30s
```

#### Delegation
```yaml
nodes:
  - name: delegate_search
    action: a2a.delegate
    with:
      to: search-specialist
      task:
        type: search
        query: "{{ state.search_query }}"
      timeout: 60s
      on_timeout: fallback_local
      fallback:
        action: web.search
        with:
          query: "{{ state.search_query }}"
```

#### Broadcast
```yaml
nodes:
  - name: announce_completion
    action: a2a.broadcast
    with:
      namespace: research-team
      message:
        type: workflow_complete
        payload:
          result: "{{ state.final_result }}"
      agent_type_filter: worker  # Optional: only to workers
```

#### Shared State
```yaml
nodes:
  - name: update_shared_progress
    action: a2a.state.set
    with:
      key: team_progress
      value:
        completed: "{{ state.completed_tasks }}"
        total: "{{ state.total_tasks }}"
      ttl: 3600  # Seconds

  - name: read_team_progress
    action: a2a.state.get
    with:
      key: team_progress
      default: {completed: 0, total: 0}
```

#### Discovery
```yaml
nodes:
  - name: find_specialists
    action: a2a.discover
    with:
      namespace: research-team
      capability: summarize  # Optional filter
      # Returns: [{agent_id, capabilities, status}]
```

### Message Format

```json
{
  "id": "msg_uuid",
  "correlation_id": "req_uuid",  // For request/response
  "from": "worker-1",
  "to": "coordinator",          // Or "*" for broadcast
  "namespace": "research-team",
  "type": "task_result",
  "payload": {...},
  "timestamp": "2026-01-04T12:00:00Z",
  "ttl": 60
}
```

### State Variables Set by A2A Actions

| Variable | Type | Description |
|----------|------|-------------|
| `a2a_messages` | list | Received messages |
| `a2a_delegation_result` | any | Result from delegation |
| `a2a_agents` | list | Discovered agents |
| `a2a_shared_state` | dict | Retrieved shared state |

### Limitations

1. **Single-process only** - In-memory queue doesn't persist across processes
2. **No durability** - Messages lost on process termination
3. **No ordering guarantees** - Messages may arrive out of order
4. **Python-only initially** - Rust implementation deferred

### Future Extensions (Out of Scope)
- Redis message queue backend
- Cloud Pub/Sub backend
- Cross-process communication
- Message persistence
- Exactly-once delivery

### Dependencies
- TEA-AGENT-001.1: Multi-Agent Collaboration (agent definitions)
- TEA-AGENT-001.3: Planning Primitive (delegation patterns)

## Testing

### Test File Locations
- Python: `python/tests/test_a2a_actions.py`

### Test Categories

| Category | Count | Priority |
|----------|-------|----------|
| Message Queue | 8 | P0 |
| a2a.send | 4 | P0 |
| a2a.receive | 8 | P0 |
| a2a.broadcast | 4 | P1 |
| a2a.delegate | 8 | P0 |
| Shared State | 6 | P1 |
| Discovery | 4 | P1 |
| Integration | 4 | P0 |

### Key Test Scenarios

1. **Send/receive round-trip** - Message delivered correctly
2. **Timeout handling** - Receive times out as expected
3. **require_all** - Waits for all specified agents
4. **Delegation success** - Task delegated and result returned
5. **Delegation timeout** - Fallback action executed
6. **Broadcast delivery** - All agents in namespace receive
7. **Shared state isolation** - Namespaces don't leak
8. **Optimistic locking** - Concurrent update conflict detected

## QA Notes

**Test Design Review Date:** 2026-01-05
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Scenarios | 54 |
| Unit Tests | 32 (59%) |
| Integration Tests | 16 (30%) |
| E2E Tests | 6 (11%) |
| P0 (Critical) | 28 |
| P1 (High) | 18 |
| P2 (Medium) | 8 |

**Coverage by Component:**
- a2a.send: 5 tests
- a2a.receive: 9 tests
- a2a.broadcast: 6 tests
- a2a.delegate: 10 tests
- MessageQueue: 6 tests
- SharedState: 9 tests
- Discovery: 8 tests
- YAML Integration: 10 tests

### Risk Areas Identified

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Namespace leak (security) | Low | **Critical** | 3 dedicated isolation tests |
| Correlation ID spoofing | Low | High | Mismatched ID rejection test |
| Concurrent state corruption | Medium | High | Optimistic locking + conflict detection tests |
| Delegation timeout not handled | Medium | High | 5 timeout strategy tests |
| Message loss in queue | Medium | High | Queue integrity tests |
| TTL cleanup failure | Low | Medium | Expiration test coverage |

### Recommended Test Scenarios

**P0 Critical Path Tests:**
1. Send/receive round-trip between agents (001.5-INT-002)
2. Delegation success with result return (001.5-E2E-001)
3. Delegation timeout with fallback execution (001.5-INT-007)
4. Namespace isolation verification (001.5-UNIT-013, 001.5-INT-005)
5. Optimistic locking conflict detection (001.5-UNIT-032, 001.5-INT-009)
6. Multi-agent timeout handling scenario (001.5-E2E-006)

**P0 Security Tests:**
- Namespace scoping prevents cross-namespace access
- Mismatched correlation_id responses ignored (anti-spoofing)

**P0 YAML Integration:**
- coordinator-worker.yaml example validation
- All a2a actions registered in actions registry

### Concerns and Recommendations

**Concerns:**
1. **Single-process limitation** - In-memory queue doesn't persist. Document clearly that messages are lost on process termination.
2. **No ordering guarantees** - Out-of-order message delivery may cause subtle bugs in workflows expecting sequence.
3. **Rust deferred** - AC9 is design-only; ensure clear documentation that Rust A2A is future work.

**Recommendations:**
1. Run P0 tests first in CI pipeline for fast failure detection
2. Add load testing for broadcast to detect potential broadcast storms
3. Document timeout defaults and recommend conservative values for production
4. Consider adding a "message received" acknowledgment option for critical workflows

**Test Execution Order:**
1. P0 Unit → P0 Integration → P0 E2E
2. P1 Unit → P1 Integration → P1 E2E
3. P2 tests as time permits

### Gate Status

**Pre-Implementation Gate:** READY FOR DEVELOPMENT

All acceptance criteria have test coverage. No blocking concerns identified. Recommend proceeding with implementation following the task order in the story.

---

**Test Design Document:** `docs/qa/assessments/TEA-AGENT-001.5-test-design-20260105.md`

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-04 | 0.1 | Initial story draft | Sarah (PO) |
| 2026-01-05 | 0.2 | Added QA Notes section | Quinn (QA) |
