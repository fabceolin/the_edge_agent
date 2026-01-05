# Story TEA-BUILTIN-015.1: Session Management in YAML

## Status: Ready for Development

**Validation Notes (2026-01-05):**
- All acceptance criteria clearly defined (9 ACs)
- Tasks broken down with subtasks (8 tasks)
- Dev notes include schema, signatures, module structure
- Test design completed (32 scenarios, 90% coverage target)
- QA review completed with risk mitigation strategies

## Story

**As a** TEA agent developer,
**I want** to configure session persistence via YAML settings,
**so that** I can maintain stateful conversations without writing Python session management code.

## Acceptance Criteria

1. **AC1: Settings Schema** - `settings.session` section supports configuration of session backend, collection/table name, TTL, and auto-save behavior
2. **AC2: Firestore Backend** - Session data can be stored in Firestore with configurable collection name
3. **AC3: Memory Backend** - In-memory session storage for development/testing (default)
4. **AC4: Session Load Action** - `session.load` action retrieves session by ID from configured backend
5. **AC5: Session Save Action** - `session.save` action persists current state to configured backend
6. **AC6: Auto-Save** - When `auto_save: true`, state is automatically saved after each graph execution
7. **AC7: TTL Support** - Sessions expire after configured TTL (time-to-live) in seconds
8. **AC8: State Injection** - Loaded session data is automatically merged into initial state
9. **AC9: Backward Compatible** - Agents without `settings.session` work unchanged

## Tasks / Subtasks

- [ ] **Task 1: Define Session Settings Schema** (AC1)
  - [ ] Create `SessionSettings` Pydantic model in `python/src/the_edge_agent/settings/`
  - [ ] Add `session` field to main Settings model
  - [ ] Define supported backends enum: `firestore`, `memory`, `redis` (future)
  - [ ] Document schema in YAML Reference

- [ ] **Task 2: Implement Session Backend Protocol** (AC2, AC3)
  - [ ] Create `SessionBackend` abstract base class in `python/src/the_edge_agent/session/`
  - [ ] Implement `MemorySessionBackend` for in-memory storage
  - [ ] Implement `FirestoreSessionBackend` with firebase-admin integration
  - [ ] Add backend factory function `create_session_backend()`

- [ ] **Task 3: Implement Session Actions** (AC4, AC5)
  - [ ] Create `session_actions.py` in `python/src/the_edge_agent/actions/`
  - [ ] Implement `session_load` action with parameters: `session_id`, `default`
  - [ ] Implement `session_save` action with parameters: `session_id`, `fields` (optional)
  - [ ] Register actions in built-in actions registry

- [ ] **Task 4: Implement Auto-Save Hook** (AC6)
  - [ ] Add post-execution hook in `StateGraph.stream()` method
  - [ ] Check `settings.session.auto_save` flag
  - [ ] Save state if flag is true and session backend is configured

- [ ] **Task 5: Implement TTL Support** (AC7)
  - [ ] Add `ttl` field to session settings
  - [ ] Firestore: Use document TTL or custom expiry field
  - [ ] Memory: Use timestamp-based expiry check on load

- [ ] **Task 6: Implement State Injection** (AC8)
  - [ ] Modify graph initialization to check for `session_id` in input
  - [ ] Auto-load session if present and merge into initial state
  - [ ] Preserve explicit input values over session values

- [ ] **Task 7: Write Tests** (AC1-AC9)
  - [ ] Unit tests for `SessionSettings` model
  - [ ] Unit tests for `MemorySessionBackend`
  - [ ] Integration tests for `FirestoreSessionBackend` (with emulator)
  - [ ] Unit tests for `session.load` and `session.save` actions
  - [ ] Integration test for auto-save behavior
  - [ ] Regression test: agent without session settings

- [ ] **Task 8: Documentation**
  - [ ] Update `docs/shared/YAML_REFERENCE.md` with session settings
  - [ ] Update `docs/python/actions-reference.md` with session actions
  - [ ] Add example agent using session management

## Dev Notes

### Settings Schema

```yaml
settings:
  session:
    backend: firestore          # Required: firestore | memory
    collection: "agent_sessions" # Firestore collection name
    auto_save: true             # Save state after each execution
    ttl: 3600                   # Session expiry in seconds (0 = never)
    persist_fields:             # Optional: specific fields to persist
      - conversation_history
      - user_context
```

### Action Signatures

```yaml
# Load session
- name: load_session
  uses: session.load
  with:
    session_id: "{{ state.session_id }}"  # Optional if in state
    default: {}                            # Default if not found
  output: session_data

# Save session
- name: save_session
  uses: session.save
  with:
    session_id: "{{ state.session_id }}"
    fields:                                # Optional: specific fields
      - conversation_history
      - last_question
```

### Backend Protocol

```python
class SessionBackend(ABC):
    @abstractmethod
    async def load(self, session_id: str) -> Optional[dict]:
        """Load session data by ID."""
        pass

    @abstractmethod
    async def save(self, session_id: str, data: dict, ttl: Optional[int] = None) -> None:
        """Save session data."""
        pass

    @abstractmethod
    async def delete(self, session_id: str) -> None:
        """Delete session."""
        pass
```

### Module Structure (Files to Create)

```
python/src/the_edge_agent/
├── session/                        # NEW MODULE
│   ├── __init__.py                 # Exports: create_session_backend, SessionBackend
│   ├── base.py                     # SessionBackend ABC + SessionData model
│   ├── memory_backend.py           # MemorySessionBackend implementation
│   └── firestore_backend.py        # FirestoreSessionBackend implementation
│
├── actions/
│   └── session_actions.py          # NEW: session.load, session.save actions
│
├── settings.py                     # MODIFY: Add SessionSettings model
├── yaml_engine.py                  # MODIFY: Add _init_session() hook
└── stategraph.py                   # MODIFY: Add auto-save post-execution hook
```

### File Contents Overview

**session/__init__.py:**
```python
from .base import SessionBackend, SessionData
from .memory_backend import MemorySessionBackend
from .firestore_backend import FirestoreSessionBackend

def create_session_backend(config: dict) -> SessionBackend:
    """Factory function for session backends."""
    backend_type = config.get("backend", "memory")
    if backend_type == "memory":
        return MemorySessionBackend()
    elif backend_type == "firestore":
        return FirestoreSessionBackend(
            collection=config.get("collection", "agent_sessions"),
            ttl=config.get("ttl")
        )
    raise ValueError(f"Unknown session backend: {backend_type}")
```

**session/base.py:**
```python
from abc import ABC, abstractmethod
from typing import Optional
from pydantic import BaseModel

class SessionData(BaseModel):
    session_id: str
    data: dict
    created_at: str
    updated_at: str
    ttl: Optional[int] = None

class SessionBackend(ABC):
    @abstractmethod
    async def load(self, session_id: str) -> Optional[dict]:
        pass

    @abstractmethod
    async def save(self, session_id: str, data: dict, ttl: Optional[int] = None) -> None:
        pass

    @abstractmethod
    async def delete(self, session_id: str) -> None:
        pass
```

### Relevant Existing Files (Minimal Changes)

- `python/src/the_edge_agent/yaml_engine.py` - Add `_init_session()` hook (~10 lines)
- `python/src/the_edge_agent/stategraph.py` - Add auto-save in `stream()` (~15 lines)
- `python/src/the_edge_agent/settings.py` - Add `SessionSettings` Pydantic model

### Testing

**Test file location:** `python/tests/test_session_actions.py`

**Testing standards:**
- Use pytest with async support (`pytest-asyncio`)
- Mock Firestore with `unittest.mock` or use Firebase emulator
- Test both sync and async action variants
- Minimum 90% coverage for new code

**Test cases:**
1. Load non-existent session returns default
2. Save and load round-trip preserves data
3. TTL expiry removes session
4. Auto-save triggers after execution
5. State injection merges correctly
6. Backward compatibility (no session settings)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-05 | 1.0 | Initial story creation | Sarah (PO) |

## Dev Agent Record

### Agent Model Used
_To be filled by dev agent_

### Debug Log References
_To be filled by dev agent_

### Completion Notes List
_To be filled by dev agent_

### File List
_To be filled by dev agent_

## QA Results

### Test Design Review - 2026-01-05

**Reviewer:** Quinn (Test Architect)

**Test Design Document:** `docs/qa/assessments/TEA-BUILTIN-015.1-test-design-20260105.md`

#### Summary

| Metric | Value |
|--------|-------|
| Total Scenarios | 32 |
| Unit Tests | 16 (50%) |
| Integration Tests | 12 (37.5%) |
| E2E Tests | 4 (12.5%) |
| P0 (Critical) | 8 |
| P1 (High) | 14 |
| P2 (Medium) | 8 |
| P3 (Low) | 2 |

#### Coverage by Acceptance Criteria

| AC | Unit | Integration | E2E | Total |
|----|------|-------------|-----|-------|
| AC1: Settings Schema | 4 | 1 | - | 5 |
| AC2: Firestore Backend | 1 | 4 | - | 5 |
| AC3: Memory Backend | 4 | - | - | 4 |
| AC4: Session Load Action | 3 | 1 | - | 4 |
| AC5: Session Save Action | 2 | 1 | - | 3 |
| AC6: Auto-Save | - | 3 | - | 3 |
| AC7: TTL Support | 2 | 1 | - | 3 |
| AC8: State Injection | - | 1 | 2 | 3 |
| AC9: Backward Compatible | - | - | 2 | 2 |

#### Key Risks Addressed

- **Session data loss:** Integration tests for error handling (015.1-INT-004, 015.1-INT-010)
- **State merge conflicts:** Integration + E2E tests for precedence (015.1-INT-012, 015.1-E2E-002)
- **Backward compatibility:** E2E regression test (015.1-E2E-003)

#### Recommendations

1. Use Firebase Emulator for Firestore integration tests
2. Use `freezegun` for TTL time-based tests
3. Minimum 90% code coverage target aligns with test design
4. Execute P0 tests in CI pipeline as blocking gate

---

## QA Notes

### Test Coverage Summary

The test design provides **comprehensive coverage** across all 9 acceptance criteria with 32 total scenarios:

- **Unit Tests (16):** Focus on pure logic components - SessionSettings model validation, MemorySessionBackend operations, session.load/save action behavior
- **Integration Tests (12):** Cover multi-component interactions - Firestore persistence, YAML engine integration, auto-save hooks
- **E2E Tests (4):** Validate complete user journeys - multi-turn conversations, backward compatibility

**P0 Critical Tests (8):** Must pass before any release - core schema validation, backend round-trips, auto-save trigger, state injection, backward compatibility

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **Session data loss on Firestore failure** | HIGH | 015.1-INT-004 tests graceful error handling; 015.1-INT-010 ensures auto-save errors don't crash execution |
| **State merge conflicts (input vs session)** | HIGH | 015.1-INT-012 + 015.1-E2E-002 verify explicit input takes precedence over session values |
| **Breaking existing agents** | CRITICAL | 015.1-E2E-003 + 015.1-E2E-004 ensure agents without session settings work unchanged |
| **TTL race conditions** | MEDIUM | 015.1-UNIT-015 + 015.1-UNIT-016 cover expiry logic; recommend using `freezegun` for deterministic time tests |
| **Concurrent session access** | MEDIUM | 015.1-UNIT-009 covers MemorySessionBackend thread safety; Firestore has native concurrency |
| **Missing session_id handling** | MEDIUM | 015.1-UNIT-012 tests session_id extraction from state when not explicitly provided |

### Recommended Test Scenarios

**Must-Have (P0 - Blocking):**
1. `015.1-UNIT-001`: SessionSettings accepts valid config
2. `015.1-UNIT-002`: SessionSettings rejects invalid backend
3. `015.1-UNIT-006`: MemorySessionBackend save/load round-trip
4. `015.1-UNIT-010`: session.load returns existing data
5. `015.1-UNIT-011`: session.load returns default when not found
6. `015.1-UNIT-013`: session.save persists full state
7. `015.1-INT-002`: Firestore save/load with emulator
8. `015.1-INT-003`: Firestore delete session
9. `015.1-INT-008`: Auto-save triggers after execution
10. `015.1-INT-012`: Session data merged on graph init
11. `015.1-E2E-003`: Agent without session config works normally

**Should-Have (P1 - High Value):**
- Field-selective persistence (`persist_fields`)
- Auto-save negative case (`auto_save: false`)
- Firestore error resilience
- Multi-turn conversation resumption

**Nice-to-Have (P2/P3):**
- Default backend selection when none specified
- Thread safety stress test
- Legacy example agents compatibility

### Concerns and Blockers

| Concern | Type | Resolution |
|---------|------|------------|
| **Firebase Emulator required** | Infrastructure | Ensure CI pipeline has Firebase Emulator configured; document local setup in dev guide |
| **Async test complexity** | Technical | Use `pytest-asyncio` with proper fixtures; mock `SessionBackend` with `AsyncMock` |
| **TTL time-based tests are flaky** | Reliability | Use `freezegun` to mock time; avoid actual `sleep()` in tests |
| **Firestore SDK dependency** | Coupling | Make `firebase-admin` optional; skip Firestore tests if not installed |
| **No Redis backend in scope** | Feature Gap | Documented as future work in settings enum; no tests needed now |

### Test Implementation Guidance

**File Structure:**
```
python/tests/
├── test_session_settings.py       # AC1 unit tests
├── test_memory_backend.py         # AC3, AC7 unit tests
├── test_firestore_backend.py      # AC2 integration tests (requires emulator)
├── test_session_actions.py        # AC4, AC5 unit/integration tests
├── test_session_integration.py    # AC6, AC8 integration tests
└── test_session_e2e.py            # AC8, AC9 e2e tests
```

**pytest Markers Required:**
```python
@pytest.mark.unit          # Fast, no I/O
@pytest.mark.integration   # May use emulators
@pytest.mark.e2e           # Full agent execution
@pytest.mark.asyncio       # Async tests
@pytest.mark.firestore     # Requires Firebase emulator
```

**CI Pipeline Gate:** Execute all P0 tests as blocking gate; P1+ can be non-blocking initially

### QA Sign-Off Criteria

- [ ] All P0 tests implemented and passing
- [ ] 90% code coverage on new session module
- [ ] Firestore integration tests run in CI (with emulator)
- [ ] Backward compatibility confirmed with existing examples
- [ ] TTL edge cases covered with deterministic time mocking
- [ ] Error handling paths validated (no crashes on backend failures)
