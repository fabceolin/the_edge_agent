# Test Design: Story TEA-BUILTIN-015.1

**Story:** Session Management in YAML
**Date:** 2026-01-05
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 32 |
| **Unit tests** | 16 (50%) |
| **Integration tests** | 12 (37.5%) |
| **E2E tests** | 4 (12.5%) |
| **Priority distribution** | P0: 8, P1: 14, P2: 8, P3: 2 |

### Strategy Rationale

This story introduces session management infrastructure with multiple backends (Memory, Firestore). The test strategy emphasizes:

1. **Heavy unit testing (50%)** - Session backends, settings models, and action logic are pure/isolated components ideal for unit tests
2. **Strong integration coverage (37.5%)** - Critical to verify backend persistence, YAML engine integration, and auto-save hooks
3. **Focused E2E tests (12.5%)** - Only for complete agent execution flows demonstrating session persistence across invocations

---

## Test Scenarios by Acceptance Criteria

### AC1: Settings Schema

`settings.session` section supports configuration of session backend, collection/table name, TTL, and auto-save behavior.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.1-UNIT-001 | Unit | P0 | Validate `SessionSettings` model accepts valid configuration | Core schema validation, pure Pydantic logic |
| 015.1-UNIT-002 | Unit | P0 | Validate `SessionSettings` rejects invalid backend type | Error handling for invalid enum |
| 015.1-UNIT-003 | Unit | P1 | Validate `SessionSettings` uses defaults when optional fields omitted | Default behavior is contract |
| 015.1-UNIT-004 | Unit | P1 | Validate `persist_fields` accepts list of field names | Optional field validation |
| 015.1-INT-001 | Integration | P1 | Parse YAML with `settings.session` block into Settings model | YAML → Pydantic integration |

**Scenario Details:**

```yaml
015.1-UNIT-001:
  component: SessionSettings
  input:
    backend: firestore
    collection: agent_sessions
    auto_save: true
    ttl: 3600
  expected: Valid SessionSettings instance
  mock_requirements: None

015.1-UNIT-002:
  component: SessionSettings
  input:
    backend: invalid_backend
  expected: ValidationError raised
  mock_requirements: None
```

---

### AC2: Firestore Backend

Session data can be stored in Firestore with configurable collection name.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.1-UNIT-005 | Unit | P1 | `FirestoreSessionBackend` initializes with collection name | Constructor validation |
| 015.1-INT-002 | Integration | P0 | Save and load round-trip with Firestore emulator | Critical data persistence path |
| 015.1-INT-003 | Integration | P0 | Delete session from Firestore | Data lifecycle management |
| 015.1-INT-004 | Integration | P1 | Handle Firestore connection errors gracefully | Error resilience |
| 015.1-INT-005 | Integration | P1 | Save with custom collection name uses correct collection | Configuration correctness |

**Scenario Details:**

```yaml
015.1-INT-002:
  components: [FirestoreSessionBackend, Firestore Emulator]
  flow: Save session → Load by ID → Verify data equality
  justification: Revenue-critical data persistence
  test_environment: Firebase Emulator Suite

015.1-INT-004:
  components: [FirestoreSessionBackend]
  flow: Attempt operation with unavailable Firestore
  expected: Graceful error with clear message, no crash
  justification: Production resilience
```

---

### AC3: Memory Backend

In-memory session storage for development/testing (default).

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.1-UNIT-006 | Unit | P0 | `MemorySessionBackend` save and load round-trip | Core functionality, pure logic |
| 015.1-UNIT-007 | Unit | P1 | `MemorySessionBackend` delete removes session | Data lifecycle |
| 015.1-UNIT-008 | Unit | P2 | `MemorySessionBackend` is default when no backend specified | Default behavior contract |
| 015.1-UNIT-009 | Unit | P2 | `MemorySessionBackend` handles concurrent access safely | Thread safety |

**Scenario Details:**

```yaml
015.1-UNIT-006:
  component: MemorySessionBackend
  flow:
    - save("sess-123", {"key": "value"})
    - load("sess-123")
  expected: {"key": "value"}
  mock_requirements: None
```

---

### AC4: Session Load Action

`session.load` action retrieves session by ID from configured backend.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.1-UNIT-010 | Unit | P0 | `session.load` returns session data when exists | Core action behavior |
| 015.1-UNIT-011 | Unit | P0 | `session.load` returns default when session not found | Error handling path |
| 015.1-UNIT-012 | Unit | P1 | `session.load` extracts session_id from state if not provided | Convenience feature |
| 015.1-INT-006 | Integration | P1 | `session.load` action registered in actions registry | Action discovery |

**Scenario Details:**

```yaml
015.1-UNIT-010:
  component: session_load action
  input:
    state: {session_id: "sess-123"}
    backend_data: {"sess-123": {"user": "alice"}}
  expected: {"user": "alice"} merged into state
  mock_requirements: Mock SessionBackend.load()

015.1-UNIT-011:
  component: session_load action
  input:
    state: {session_id: "nonexistent"}
    with: {default: {"empty": true}}
  expected: {"empty": true}
  mock_requirements: Mock SessionBackend.load() returning None
```

---

### AC5: Session Save Action

`session.save` action persists current state to configured backend.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.1-UNIT-013 | Unit | P0 | `session.save` persists full state when no fields specified | Core action behavior |
| 015.1-UNIT-014 | Unit | P1 | `session.save` persists only specified fields when provided | Field filtering feature |
| 015.1-INT-007 | Integration | P1 | `session.save` action registered in actions registry | Action discovery |

**Scenario Details:**

```yaml
015.1-UNIT-013:
  component: session_save action
  input:
    state: {session_id: "sess-123", user: "alice", count: 5}
  expected: Backend.save() called with full state dict
  mock_requirements: Mock SessionBackend.save()

015.1-UNIT-014:
  component: session_save action
  input:
    state: {session_id: "sess-123", user: "alice", count: 5, temp: "ignore"}
    with: {fields: ["user", "count"]}
  expected: Backend.save() called with {user: "alice", count: 5}
  mock_requirements: Mock SessionBackend.save()
```

---

### AC6: Auto-Save

When `auto_save: true`, state is automatically saved after each graph execution.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.1-INT-008 | Integration | P0 | Auto-save triggers after graph execution completes | Critical feature, multi-component |
| 015.1-INT-009 | Integration | P1 | Auto-save does NOT trigger when `auto_save: false` | Negative case verification |
| 015.1-INT-010 | Integration | P2 | Auto-save handles backend errors without crashing execution | Resilience |

**Scenario Details:**

```yaml
015.1-INT-008:
  components: [StateGraph, SessionBackend, YAML Engine]
  flow:
    - Configure agent with settings.session.auto_save: true
    - Execute graph with state
    - Verify backend.save() was called after completion
  justification: Core feature requiring graph + backend coordination
  test_environment: In-memory backend with spy
```

---

### AC7: TTL Support

Sessions expire after configured TTL (time-to-live) in seconds.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.1-UNIT-015 | Unit | P1 | `MemorySessionBackend` returns None for expired sessions | TTL logic, pure time check |
| 015.1-INT-011 | Integration | P1 | Firestore TTL field is set correctly on save | Backend-specific TTL implementation |
| 015.1-UNIT-016 | Unit | P2 | TTL of 0 means session never expires | Edge case |

**Scenario Details:**

```yaml
015.1-UNIT-015:
  component: MemorySessionBackend
  flow:
    - save("sess-123", data, ttl=1)
    - sleep(2 seconds)
    - load("sess-123")
  expected: None (expired)
  mock_requirements: Mock time.time() or use short TTL with actual wait
```

---

### AC8: State Injection

Loaded session data is automatically merged into initial state.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.1-INT-012 | Integration | P0 | Session data merged into initial state on graph init | Core feature requiring engine integration |
| 015.1-E2E-001 | E2E | P1 | Agent resumes conversation from previous session | Complete user journey |
| 015.1-E2E-002 | E2E | P2 | Explicit input values override session values | Precedence rules |

**Scenario Details:**

```yaml
015.1-INT-012:
  components: [YAMLEngine, SessionBackend, StateGraph]
  flow:
    - Pre-save session {"history": ["msg1"]}
    - Invoke agent with {session_id: "sess-123", new_input: "hello"}
    - Verify initial state has both history and new_input
  justification: Multi-component coordination, data flow
  test_environment: Memory backend

015.1-E2E-001:
  journey: Multi-turn conversation with session persistence
  flow:
    - Turn 1: User says "My name is Alice"
    - Save session
    - New agent invocation with same session_id
    - Turn 2: User says "What is my name?"
    - Verify agent remembers "Alice"
  justification: Critical user experience feature
  environment: Full agent execution with memory backend
```

---

### AC9: Backward Compatible

Agents without `settings.session` work unchanged.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.1-E2E-003 | E2E | P0 | Agent YAML without session settings executes normally | Regression prevention |
| 015.1-E2E-004 | E2E | P3 | Existing example agents continue to work | Backward compatibility |

**Scenario Details:**

```yaml
015.1-E2E-003:
  journey: Execute existing agent without session config
  flow:
    - Load examples/simple_agent.yaml (no session settings)
    - Execute with standard input
    - Verify successful completion
  justification: Regression prevention for existing users
  environment: Full agent execution
```

---

## Risk Coverage Matrix

| Risk | Probability | Impact | Test Coverage |
|------|-------------|--------|---------------|
| Session data loss on backend failure | Medium | High | 015.1-INT-004, 015.1-INT-010 |
| TTL calculation errors | Low | Medium | 015.1-UNIT-015, 015.1-UNIT-016 |
| State merge conflicts | Medium | High | 015.1-INT-012, 015.1-E2E-002 |
| Breaking existing agents | Low | Critical | 015.1-E2E-003, 015.1-E2E-004 |
| Firestore connection issues | Medium | High | 015.1-INT-004 |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0)
1. 015.1-UNIT-001 - Schema validation (foundational)
2. 015.1-UNIT-002 - Invalid backend rejection
3. 015.1-UNIT-006 - Memory backend core
4. 015.1-UNIT-010 - session.load action core
5. 015.1-UNIT-011 - session.load default handling
6. 015.1-UNIT-013 - session.save action core
7. 015.1-INT-002 - Firestore round-trip
8. 015.1-INT-003 - Firestore delete
9. 015.1-INT-008 - Auto-save trigger
10. 015.1-INT-012 - State injection
11. 015.1-E2E-003 - Backward compatibility

### Phase 2: Core Features (P1)
12. 015.1-UNIT-003 through 015.1-UNIT-005
13. 015.1-UNIT-007, 015.1-UNIT-012, 015.1-UNIT-014, 015.1-UNIT-015
14. 015.1-INT-001, 015.1-INT-004 through 015.1-INT-007
15. 015.1-INT-009, 015.1-INT-011
16. 015.1-E2E-001

### Phase 3: Secondary (P2)
17. 015.1-UNIT-008, 015.1-UNIT-009, 015.1-UNIT-016
18. 015.1-INT-010
19. 015.1-E2E-002

### Phase 4: Time Permitting (P3)
20. 015.1-E2E-004

---

## Test Implementation Notes

### Test File Structure

```
python/tests/
├── test_session_settings.py       # 015.1-UNIT-001 to 015.1-UNIT-004
├── test_memory_backend.py         # 015.1-UNIT-006 to 015.1-UNIT-009, 015.1-UNIT-015, 015.1-UNIT-016
├── test_firestore_backend.py      # 015.1-UNIT-005, 015.1-INT-002 to 015.1-INT-005, 015.1-INT-011
├── test_session_actions.py        # 015.1-UNIT-010 to 015.1-UNIT-014, 015.1-INT-006, 015.1-INT-007
├── test_session_integration.py    # 015.1-INT-001, 015.1-INT-008 to 015.1-INT-010, 015.1-INT-012
└── test_session_e2e.py            # 015.1-E2E-001 to 015.1-E2E-004
```

### Mock Requirements

| Component | Mock Strategy |
|-----------|---------------|
| Firestore | Firebase Emulator or `unittest.mock` |
| Time (TTL) | `freezegun` or mock `time.time()` |
| SessionBackend | Protocol-based mocking with `AsyncMock` |

### pytest Markers

```python
@pytest.mark.unit          # Fast, no I/O
@pytest.mark.integration   # May use emulators
@pytest.mark.e2e           # Full agent execution
@pytest.mark.asyncio       # Async tests
@pytest.mark.firestore     # Requires Firebase emulator
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (015.1-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
- [x] Backward compatibility explicitly tested

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 32
  by_level:
    unit: 16
    integration: 12
    e2e: 4
  by_priority:
    p0: 8
    p1: 14
    p2: 8
    p3: 2
  coverage_gaps: []
  design_date: "2026-01-05"
  designer: "Quinn (Test Architect)"
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-015.1-test-design-20260105.md
P0 tests identified: 8
Epic: TEA-BUILTIN-015
Story: 015.1 (Session Management)
```
