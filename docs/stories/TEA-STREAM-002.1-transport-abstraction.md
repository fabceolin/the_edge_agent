# Story TEA-STREAM-002.1: Transport Abstraction Layer

## Status: Ready for Development

## Story

**As a** workflow developer,
**I want** a transport abstraction layer for stream channels,
**So that** I can switch between Unix pipes and ZeroMQ without changing workflow logic.

## Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `Transport` base class defines `bind()`, `connect()`, `send()`, `receive()`, `close()` interface | Unit test |
| AC2 | `TransportConfig` dataclass holds transport type, protocol, pattern, and options | Unit test |
| AC3 | `UnixPipeTransport` implements `Transport` interface using patterns from `StreamChannel` (reimplements pipe logic, does not delegate) | Unit test |
| AC4 | `TransportFactory.create()` returns appropriate transport based on config | Unit test |
| AC5 | Existing Unix pipe workflows work unchanged when `transport: unix` (default) | Integration test |
| AC6 | `StreamRegistry` accepts optional `transport_factory` parameter | Unit test |
| AC7 | Transport errors raise `TransportError` with helpful messages | Unit test |
| AC8 | `is_connected` property reflects connection state accurately | Unit test |
| AC9 | Context manager support (`with transport:`) for automatic cleanup | Unit test |
| AC10 | Graceful handling when transport is closed while operations pending | Unit test |

## Tasks / Subtasks

- [ ] **Task 1: Create transport base module** (AC: 1, 2, 7)
  - [ ] Create `python/src/the_edge_agent/transports/__init__.py`
  - [ ] Create `python/src/the_edge_agent/transports/base.py`
  - [ ] Define `Transport` abstract base class with all required methods
  - [ ] Define `TransportConfig` dataclass with type, protocol, pattern, options
  - [ ] Define `TransportError` exception class
  - [ ] Add docstrings with usage examples

- [ ] **Task 2: Implement UnixPipeTransport** (AC: 3, 8, 9, 10)
  - [ ] Create `python/src/the_edge_agent/transports/unix.py`
  - [ ] Wrap existing `StreamChannel` from `streams.py`
  - [ ] Implement `bind()` to create write-side of pipe
  - [ ] Implement `connect()` to connect to read-side of pipe
  - [ ] Implement `send()` using `os.write()`
  - [ ] Implement `receive()` using `os.read()`
  - [ ] Implement `close()` with idempotent cleanup
  - [ ] Implement `is_connected` property
  - [ ] Add `__enter__` and `__exit__` for context manager
  - [ ] Handle `BrokenPipeError` gracefully

- [ ] **Task 3: Implement TransportFactory** (AC: 4)
  - [ ] Create `python/src/the_edge_agent/transports/factory.py`
  - [ ] Implement `create_transport(config: TransportConfig) -> Transport`
  - [ ] Return `UnixPipeTransport` for `transport_type="unix"`
  - [ ] Raise `TransportError` for unknown transport types
  - [ ] Add lazy import for ZeroMQ transport (future story)

- [ ] **Task 4: Integrate with StreamRegistry** (AC: 5, 6)
  - [ ] Modify `python/src/the_edge_agent/streams.py`
  - [ ] Add optional `transport_factory` parameter to `StreamRegistry.__init__()`
  - [ ] Default to `UnixPipeTransport` factory when not specified
  - [ ] Ensure all existing `StreamRegistry` tests pass unchanged
  - [ ] Add new test for custom transport factory injection

- [ ] **Task 5: Write unit tests** (AC: 1-10)
  - [ ] Create `python/tests/test_transports.py`
  - [ ] Test `Transport` interface compliance
  - [ ] Test `TransportConfig` creation and validation
  - [ ] Test `UnixPipeTransport` send/receive cycle
  - [ ] Test `TransportFactory` dispatch logic
  - [ ] Test error scenarios and exception messages
  - [ ] Test context manager cleanup

- [ ] **Task 6: Write integration tests** (AC: 5)
  - [ ] Add tests to `python/tests/test_streams.py`
  - [ ] Verify existing stream workflows unchanged
  - [ ] Test `StreamRegistry` with explicit `transport: unix`
  - [ ] Test backward compatibility with no transport config

## Dev Notes

### Relevant Source Tree

```
python/src/the_edge_agent/
├── streams.py              # Existing StreamChannel, StreamRegistry
├── stream_broadcast.py     # Existing TeeOrchestrator
├── transports/             # NEW - Transport abstraction layer
│   ├── __init__.py
│   ├── base.py             # Transport ABC, TransportConfig, TransportError
│   ├── unix.py             # UnixPipeTransport
│   └── factory.py          # TransportFactory
```

### Existing Patterns to Follow

From `streams.py`:
- `StreamChannel` dataclass pattern for configuration
- `StreamRegistry` for managing multiple channels
- `StreamDirection` enum for type safety
- SIGPIPE handling pattern

### Key Design Decisions

1. **Transport reimplements pipe logic**: `UnixPipeTransport` uses patterns from `StreamChannel` but implements the `Transport` interface independently (does not delegate to `StreamChannel`)
2. **Factory pattern**: Enables runtime transport selection
3. **Lazy loading**: ZeroMQ imports only when `transport: zeromq`
4. **Backward compatible**: No changes to existing public API
5. **`is_connected` semantics for pipes**: Returns `True` after `bind()` or `connect()` successfully creates/opens the pipe; `False` after `close()`

### StreamRegistry Integration Notes

Since `StreamRegistry` is a `@dataclass`, adding `transport_factory` requires one of:
1. **Recommended**: Add as dataclass field with default factory:
   ```python
   transport_factory: Callable[[TransportConfig], Transport] = field(
       default_factory=lambda: create_transport
   )
   ```
2. **Alternative**: Use `__post_init__` for complex initialization logic

### Implementation Sketch

```python
# base.py
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Optional, Dict, Any

class TransportError(Exception):
    """Base exception for transport errors."""
    pass

@dataclass
class TransportConfig:
    """Configuration for a transport instance."""
    transport_type: str = "unix"  # "unix" | "zeromq"
    protocol: str = "ipc"         # "ipc" | "tcp"
    pattern: str = "direct"       # "direct" | "pub_sub" | "push_pull" | "req_rep"
    options: Dict[str, Any] = field(default_factory=dict)

class Transport(ABC):
    """Abstract base class for stream transports."""

    def __init__(self, config: TransportConfig):
        self.config = config
        self._connected = False

    @abstractmethod
    def bind(self, address: str) -> None:
        """Bind transport as producer."""
        pass

    @abstractmethod
    def connect(self, address: str) -> None:
        """Connect transport as consumer."""
        pass

    @abstractmethod
    def send(self, data: bytes, flags: int = 0) -> int:
        """Send data, return bytes sent."""
        pass

    @abstractmethod
    def receive(self, size: int = 4096, flags: int = 0) -> bytes:
        """Receive data."""
        pass

    @abstractmethod
    def close(self) -> None:
        """Close transport."""
        pass

    @property
    def is_connected(self) -> bool:
        return self._connected

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()
        return False
```

### Testing

| Test Category | Location | Framework |
|--------------|----------|-----------|
| Unit tests | `python/tests/test_transports.py` | pytest |
| Integration tests | `python/tests/test_streams.py` | pytest |

**Test Standards**:
- Follow existing pytest patterns in `tests/`
- Use fixtures for transport setup/teardown
- Mock OS-level pipe operations where needed
- Test both happy path and error scenarios

## Definition of Done

- [ ] All acceptance criteria have passing tests
- [ ] `Transport` ABC fully defined with docstrings
- [ ] `UnixPipeTransport` passes all unit tests
- [ ] `TransportFactory` correctly dispatches
- [ ] Existing `test_streams.py` tests pass unchanged
- [ ] No regressions in existing stream functionality
- [ ] Code follows project style (black, ruff)
- [ ] Type hints complete and mypy passes

## Dev Agent Record

_To be populated during implementation._

## QA Results

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 28 |
| Unit tests | 21 (75%) |
| Integration tests | 7 (25%) |
| E2E tests | 0 (0%) - Not needed for infrastructure code |
| Priority distribution | P0: 10, P1: 12, P2: 6 |

**Coverage by Acceptance Criteria:**
- AC1 (Transport ABC): 3 unit tests
- AC2 (TransportConfig): 3 unit tests
- AC3 (UnixPipeTransport): 4 unit + 2 integration tests
- AC4 (TransportFactory): 3 unit tests
- AC5 (Backward Compatibility): 3 integration tests (critical)
- AC6 (StreamRegistry Integration): 2 unit + 1 integration test
- AC7 (TransportError): 3 unit tests
- AC8 (is_connected): 3 unit tests
- AC9 (Context Manager): 3 unit tests
- AC10 (Graceful Pending): 3 unit + 1 integration test

### Risk Areas Identified

| Risk | Severity | Mitigating Tests |
|------|----------|------------------|
| **Backward compatibility regression** | HIGH | INT-003, INT-004, INT-005 - Must pass unchanged |
| **Resource leak (unclosed file descriptors)** | MEDIUM | UNIT-023, UNIT-024, UNIT-025 - Context manager coverage |
| **State inconsistency** | MEDIUM | UNIT-019, UNIT-020, UNIT-021 - Connection state tracking |
| **Factory dispatch failure** | MEDIUM | UNIT-011, UNIT-012 - Known/unknown transport types |
| **Error message quality** | LOW | UNIT-016, UNIT-017, UNIT-018 - Developer experience |

### Recommended Test Scenarios (Critical Path)

**P0 - Must Pass Before Merge:**
1. `TEA-STREAM-002.1-UNIT-001` - Transport ABC defines all abstract methods
2. `TEA-STREAM-002.1-UNIT-007` - bind() creates write-side fd
3. `TEA-STREAM-002.1-UNIT-008` - connect() opens read-side fd
4. `TEA-STREAM-002.1-UNIT-009` - send() writes data
5. `TEA-STREAM-002.1-UNIT-010` - receive() reads data
6. `TEA-STREAM-002.1-UNIT-011` - Factory returns UnixPipeTransport
7. `TEA-STREAM-002.1-UNIT-019` - is_connected False before bind
8. `TEA-STREAM-002.1-UNIT-020` - is_connected True after bind
9. `TEA-STREAM-002.1-INT-001` - Full send/receive cycle (real pipe)
10. `TEA-STREAM-002.1-INT-003` - All existing test_streams.py pass (regression)

**P1 - Required for Release:**
- Error handling scenarios (UNIT-002, 012, 016-018, 021, 025-26)
- Context manager lifecycle (UNIT-022, UNIT-023)
- Backward compatibility explicit configs (INT-004, INT-005)
- Multi-message streaming (INT-002)

### Concerns / Blockers

1. **No blockers identified** - Story is well-defined with clear acceptance criteria

2. **Implementation concern**: Task 2 subtask mentions "Wrap existing `StreamChannel`" but AC3 specifies "reimplements pipe logic, does not delegate". Ensure implementation follows AC3 exactly.

3. **Test fixture dependency**: Integration tests require actual Unix pipe creation - ensure CI environment supports `os.pipe()` operations

4. **Mutable default in TransportConfig**: UNIT-006 tests `options` dict isolation - implementation must use `field(default_factory=dict)` pattern

### QA Recommendation

**PASS with conditions** - Story is ready for development. Test design covers all ACs with appropriate depth. Key validations:
- Backward compatibility tests (INT-003) are non-negotiable
- P0 tests must be implemented alongside feature code
- Resource cleanup tests (context manager, idempotent close) prevent production issues

**Test Design Reference**: `docs/qa/assessments/TEA-STREAM-002.1-test-design-20260108.md`

---

## QA Notes

**Reviewed by:** Quinn (Test Architect)
**Review Date:** 2026-01-08

### Quality Gate Status: **PASS WITH CONDITIONS**

This story establishes the foundation for the entire TEA-STREAM-002 epic. Quality assessment confirms readiness for development.

### Test Implementation Guidance

**Recommended Test File:** `python/tests/test_transports.py`

**Mock Strategy:**
- Use `unittest.mock.patch` for `os.pipe()`, `os.read()`, `os.write()` in unit tests
- Use real pipes for integration tests
- Use `tmp_path` fixture for any file-based tests

**Fixtures Required:**
```python
@pytest.fixture
def transport_config():
    return TransportConfig(transport_type="unix")

@pytest.fixture
def unix_transport(transport_config):
    transport = UnixPipeTransport(transport_config)
    yield transport
    transport.close()
```

### Critical Implementation Notes

1. **AC3 Clarification**: `UnixPipeTransport` must reimplement pipe logic using `os.pipe()`, `os.read()`, `os.write()` directly. Do NOT delegate to `StreamChannel`.

2. **`is_connected` Semantics**: Returns `True` after successful `bind()` or `connect()`, `False` after `close()`.

3. **Mutable Default**: `TransportConfig.options` must use `field(default_factory=dict)` to avoid shared state.

4. **Context Manager**: Implement `__enter__` returning `self` and `__exit__` calling `close()`.

### Acceptance Criteria Traceability

| AC | Tests | Status |
|----|-------|--------|
| AC1 | UNIT-001, UNIT-002, UNIT-003 | ✅ Covered |
| AC2 | UNIT-004, UNIT-005, UNIT-006 | ✅ Covered |
| AC3 | UNIT-007 to UNIT-010, INT-001, INT-002 | ✅ Covered |
| AC4 | UNIT-011, UNIT-012, UNIT-013 | ✅ Covered |
| AC5 | INT-003, INT-004, INT-005 | ✅ Covered (Critical) |
| AC6 | UNIT-014, UNIT-015, INT-006 | ✅ Covered |
| AC7 | UNIT-016, UNIT-017, UNIT-018 | ✅ Covered |
| AC8 | UNIT-019, UNIT-020, UNIT-021 | ✅ Covered |
| AC9 | UNIT-022, UNIT-023, UNIT-024 | ✅ Covered |
| AC10 | UNIT-025, UNIT-026, INT-007 | ✅ Covered |

### Pre-Implementation Checklist

- [ ] Read existing `streams.py` to understand current patterns
- [ ] Verify `os.pipe()` works in CI environment
- [ ] Plan Transport ABC interface carefully (cannot change after dependents use it)
- [ ] Consider Windows compatibility (Unix pipes not available)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Initial story creation | Sarah (PO) |
| 2026-01-08 | 0.2 | Added clarifications per validation review | Sarah (PO) |
| 2026-01-08 | 0.3 | Added QA Notes with test implementation guidance | Quinn (QA) |
