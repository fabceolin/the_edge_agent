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

## QA Notes - Risk Profile

**Reviewed by:** Quinn (Test Architect)
**Review Date:** 2026-02-01
**Risk Profile:** `docs/qa/assessments/TEA-STREAM-002.1-risk-20260201.md`

### Risk Level: **MODERATE (60/100)**

### Risk Summary

| Severity | Count | IDs |
|----------|-------|-----|
| Critical | 0 | - |
| High | 3 | TECH-001, TECH-003, OPS-001 |
| Medium | 2 | TECH-002, TECH-004 |
| Low | 5 | PERF-001, DATA-001, OPS-002, BUS-001, SEC-001 |

### Key Risks Identified

| ID | Risk | Score | Mitigation |
|----|------|-------|------------|
| **TECH-001** | ABC Interface Lock-in | 6 | Review against zmq.Socket API before finalizing |
| **TECH-003** | Task/AC Conflict | 6 | **AC3 takes precedence** - fix Task 2 subtask wording |
| **OPS-001** | Resource Leak (FD Exhaustion) | 6 | Context manager tests are P0; idempotent close() |
| TECH-002 | Windows Incompatibility | 4 | Document limitation in docstring |
| TECH-004 | Mutable Default Bug | 4 | Use `field(default_factory=dict)` |

### Testing Priorities (Risk-Driven)

**P0 - Must Pass:**
1. Context manager cleanup (UNIT-022, 023, 024) - OPS-001
2. Idempotent close (UNIT-026) - OPS-001
3. Backward compatibility (INT-003, 004, 005) - BUS-001

**P1 - High Priority:**
1. Options dict isolation (UNIT-006) - TECH-004
2. is_connected state accuracy (UNIT-019, 020, 021) - DATA-001
3. Code review: verify no StreamChannel delegation - TECH-003

### Pre-Development Action Required

**TECH-003 Resolution:** Task 2 subtask says "Wrap existing `StreamChannel`" but AC3 explicitly states "reimplements pipe logic, does not delegate". Update Task 2 to align with AC3 before development begins.

### Gate Recommendation: **CONCERNS**

Story may proceed to development with the understanding that:
1. TECH-003 documentation conflict should be resolved
2. Context manager tests (OPS-001 mitigation) are non-negotiable P0
3. Transport ABC interface should be reviewed against ZeroMQ requirements before finalizing

## QA Notes - NFR Assessment

**Reviewed by:** Quinn (Test Architect)
**Review Date:** 2026-02-01
**Assessment:** `docs/qa/assessments/TEA-STREAM-002.1-nfr-20260201.md`

### NFR Summary

| NFR | Status | Notes |
|-----|--------|-------|
| Security | **PASS** | Infrastructure code, no security-sensitive operations |
| Performance | **CONCERNS** | Target unknown - no latency/throughput requirements defined |
| Reliability | **PASS** | Error handling, graceful degradation, context manager cleanup |
| Maintainability | **PASS** | Clear ABC interface, factory pattern, comprehensive test plan |

**Quality Score:** 90/100

### NFR Coverage Analysis

**Security (PASS):**
- Internal transport layer with no user-facing interfaces
- No authentication, authorization, or secret management concerns
- Pipe addresses are internal, not user-controlled

**Performance (CONCERNS - Target Unknown):**
- Uses OS-native pipe operations (good foundation)
- Missing: Explicit latency/throughput targets
- Missing: Benchmarking acceptance criteria
- Recommendation: Define targets before ZeroMQ story

**Reliability (PASS):**
- AC7: TransportError with helpful messages
- AC9: Context manager for automatic cleanup
- AC10: Graceful handling of pending operations on close
- Idempotent close() following existing StreamChannel pattern

**Maintainability (PASS):**
- Clear ABC interface with abstract methods
- Factory pattern for transport selection
- 28 test scenarios (75% unit, 25% integration)
- Type hints and docstrings required in DoD

### Missing Considerations

1. **Performance Targets:** No explicit latency/throughput requirements defined. Should specify:
   - Max latency for send()/receive(): <1ms for data <64KB
   - Throughput: >100MB/s for local pipe operations

2. **Cross-Platform:** Windows incompatibility documented in streams.py but not explicitly addressed in this story's acceptance criteria.

### Test Recommendations

**NFR-Specific Tests to Add:**

| ID | Test | NFR | Priority |
|----|------|-----|----------|
| NFR-PERF-001 | Measure send() latency for 1KB, 64KB, 1MB payloads | Performance | P1 |
| NFR-PERF-002 | Measure receive() latency for 1KB, 64KB, 1MB payloads | Performance | P1 |
| NFR-REL-001 | Verify TransportError message includes operation context | Reliability | P0 |
| NFR-REL-002 | Verify close() is idempotent (call twice, no error) | Reliability | P0 |
| NFR-REL-003 | Verify context manager calls close() on exception | Reliability | P0 |
| NFR-MAINT-001 | Verify Transport ABC has docstrings for all methods | Maintainability | P1 |
| NFR-MAINT-002 | Verify mypy passes with strict mode | Maintainability | P1 |

### Acceptance Criteria Gaps

| Gap | Recommendation |
|-----|----------------|
| No performance target defined | Add AC11: "Transport operations complete within 1ms for data <64KB" |
| Windows behavior undefined | Add note to AC3: "Raises PlatformError on Windows" |

### Gate Recommendation

**CONCERNS** - Story may proceed with the understanding that:
1. Performance targets should be defined before dependent stories
2. Performance tests (NFR-PERF-001, NFR-PERF-002) are recommended but not blocking

## SM Validation

**Validated by:** Bob (Scrum Master)
**Validation Date:** 2026-02-01
**Mode:** YOLO (Full autonomous validation)

### Checklist Results

| Category | Status | Issues |
|----------|--------|--------|
| 1. Goal & Context Clarity | **PASS** | Clear user story, epic relationship, dependencies identified |
| 2. Technical Implementation Guidance | **PASS** | Key files, APIs, implementation sketch all provided |
| 3. Reference Effectiveness | **PASS** | QA assessments referenced with specific paths |
| 4. Self-Containment Assessment | **PASS** | Core info included, edge cases addressed |
| 5. Testing Guidance | **PASS** | 34 test scenarios, fixtures, DoD checklist |
| 6. QA Sections Present | **PASS** | Risk Profile, NFR, Test Design, Requirements Trace all present |
| 7. Story Sizing | **PASS** | 6 tasks with subtasks, reasonable scope |

### Clarity Score: 9/10

### Known Issues (Non-Blocking)

1. **TECH-003 (Documented):** Task 2 subtask says "Wrap existing `StreamChannel`" but AC3 says "reimplements pipe logic, does not delegate". Resolution: **AC3 takes precedence**.
2. **NFR Performance:** No explicit latency targets defined. Recommendation: Define before ZeroMQ story.

### Definition of Ready Checklist

- [x] Story has clear title and description
- [x] Acceptance criteria are defined and testable (10 ACs)
- [x] Dependencies are identified (builds on `streams.py`)
- [x] Technical approach is documented (implementation sketch provided)
- [x] Story is properly sized (6 tasks, bounded scope)
- [x] QA notes sections present (Risk Profile, NFR, Test Design, Requirements Trace)
- [x] No blocking issues or unknowns

### Validation Result: **READY FOR DEVELOPMENT**

All Definition of Ready criteria met. Story provides comprehensive context for developer agent implementation.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Initial story creation | Sarah (PO) |
| 2026-01-08 | 0.2 | Added clarifications per validation review | Sarah (PO) |
| 2026-01-08 | 0.3 | Added QA Notes with test implementation guidance | Quinn (QA) |
| 2026-02-01 | 0.4 | Added QA Notes - Risk Profile section | Quinn (QA) |
| 2026-02-01 | 0.5 | Added QA Notes - NFR Assessment section | Quinn (QA) |
| 2026-02-01 | 0.6 | Added QA Notes - Test Design section | Quinn (QA) |
| 2026-02-01 | 0.7 | Added QA Notes - Requirements Trace section | Quinn (QA) |
| 2026-02-01 | 0.8 | SM Validation completed - Ready for Development | Bob (SM) |

## QA Notes - Test Design

**Reviewed by:** Quinn (Test Architect)
**Review Date:** 2026-02-01
**Test Design Document:** `docs/qa/assessments/TEA-STREAM-002.1-test-design-20260201.md`

### Test Coverage Matrix

| AC | Description | Unit | Integration | Total | Priority |
|----|-------------|------|-------------|-------|----------|
| AC1 | Transport ABC Interface | 3 | 0 | 3 | P0: 2, P1: 1 |
| AC2 | TransportConfig Dataclass | 3 | 0 | 3 | P0: 2, P1: 1 |
| AC3 | UnixPipeTransport Implementation | 5 | 3 | 8 | P0: 5, P1: 3 |
| AC4 | TransportFactory Dispatch | 3 | 0 | 3 | P0: 2, P1: 1 |
| AC5 | Backward Compatibility | 0 | 3 | 3 | P0: 2, P1: 1 |
| AC6 | StreamRegistry Integration | 2 | 1 | 3 | P1: 3 |
| AC7 | TransportError Exception | 3 | 0 | 3 | P0: 2, P1: 1 |
| AC8 | is_connected Property | 4 | 0 | 4 | P0: 4 |
| AC9 | Context Manager Support | 3 | 0 | 3 | P0: 3 |
| AC10 | Graceful Pending Ops | 3 | 1 | 4 | P0: 1, P1: 3 |
| NFR | Performance/Maintainability | 2 | 2 | 4 | P1: 2, P2: 2 |
| **Total** | | **24** | **10** | **34** | P0: 14, P1: 13, P2: 7 |

### Key Test Scenarios with Expected Results

#### P0 Critical Path Tests

| ID | Scenario | Expected Result |
|----|----------|-----------------|
| UNIT-001 | Transport ABC defines all abstract methods | ABC has bind, connect, send, receive, close |
| UNIT-006 | TransportConfig options dict isolation | Modifying options on instance A does not affect instance B |
| UNIT-007 | bind() creates pipe file descriptors | os.pipe() called, write fd stored |
| INT-001 | Full send/receive cycle | Data sent by producer received by consumer intact |
| INT-003 | UnixPipeTransport does NOT delegate to StreamChannel | No StreamChannel imports/usage in unix.py |
| INT-004 | **ALL existing test_streams.py tests pass** | Zero failures, zero regressions |
| UNIT-024 | Context manager __enter__ returns self | `with transport as t:` yields same object |
| UNIT-027 | close() is idempotent | Calling close() twice raises no exception |

#### P1 High Priority Tests

| ID | Scenario | Expected Result |
|----|----------|-----------------|
| UNIT-012 | Factory returns UnixPipeTransport for "unix" | Correct instance type |
| INT-002 | Multi-message streaming | 100 messages sent/received in order |
| UNIT-028 | send() on closed transport | TransportError with "closed" message |
| UNIT-030 | ABC has docstrings | All methods documented |

### Test Data Requirements

| Data Type | Values | Used In |
|-----------|--------|---------|
| Small payload | `b"test"` (4 bytes) | UNIT-009, UNIT-010, INT-001 |
| Medium payload | `b"x" * 64 * 1024` (64KB) | INT-009, INT-010 |
| Large payload | `b"x" * 1024 * 1024` (1MB) | Performance stress tests |
| Invalid transport type | `"invalid"`, `"zeromq"` (not yet impl) | UNIT-013 |
| Message count | 100 sequential messages | INT-002 |

### Test Environment Requirements

| Requirement | Details |
|-------------|---------|
| **Platform** | Unix/Linux (Windows not supported for Unix pipes) |
| **CI Compatibility** | Verify `os.pipe()` works in GitHub Actions container |
| **File Descriptor Limits** | Ensure ulimit allows pipe creation (~10 FDs per test) |
| **Python Version** | 3.9+ (per project requirements) |
| **Dependencies** | pytest, pytest-mock |

### Fixtures Required

```python
@pytest.fixture
def transport_config():
    return TransportConfig(transport_type="unix")

@pytest.fixture
def unix_transport(transport_config):
    transport = UnixPipeTransport(transport_config)
    yield transport
    if transport.is_connected:
        transport.close()

@pytest.fixture
def pipe_pair():
    """Producer/consumer pair for integration tests."""
    producer = UnixPipeTransport(TransportConfig())
    consumer = UnixPipeTransport(TransportConfig())
    yield producer, consumer
    producer.close()
    consumer.close()
```

### Risk Mitigation Coverage

| Risk ID | Risk | Mitigating Tests |
|---------|------|------------------|
| TECH-001 | ABC Interface Lock-in | UNIT-001, UNIT-002 |
| TECH-003 | Task/AC Conflict | INT-003 (code inspection) |
| TECH-004 | Mutable Default Bug | UNIT-006 |
| OPS-001 | Resource Leak | UNIT-011, UNIT-024, UNIT-025, UNIT-026, UNIT-027 |
| DATA-001 | State Inconsistency | UNIT-020, UNIT-021, UNIT-022, UNIT-023 |
| BUS-001 | Backward Compatibility | INT-004, INT-005 |

### Test Execution Order (CI Pipeline)

1. **P0 Unit Tests** - Fail fast on interface violations
2. **P0 Integration Tests** - Validate real pipe behavior + backward compat
3. **P1 Unit Tests** - Error handling, factory extensibility
4. **P1 Integration Tests** - Multi-message, custom factory
5. **P2 Integration Tests** - Performance baseline (optional)

### QA Recommendation

**Test design is COMPLETE and COMPREHENSIVE.** All 10 acceptance criteria are covered with 34 test scenarios (14 P0, 13 P1, 7 P2). No coverage gaps identified.

**Implementation Priority:**
1. Write P0 tests alongside feature code (TDD recommended)
2. INT-004 (backward compatibility) is the **single most critical test** - must pass before merge
3. Context manager tests (OPS-001 mitigation) are non-negotiable for production safety

## QA Notes - Requirements Trace

**Reviewed by:** Quinn (Test Architect)
**Review Date:** 2026-02-01
**Trace Mode:** YOLO (Full autonomous analysis)

---

### Requirements Coverage Summary

| Metric | Value |
|--------|-------|
| Total Acceptance Criteria | 10 |
| Fully Covered (design) | 10 (100%) |
| Partially Covered | 0 (0%) |
| Not Covered | 0 (0%) |
| Implementation Status | **NOT STARTED** |
| Tests Exist | **NO** - `python/tests/test_transports.py` not created |

---

### Traceability Matrix

| AC | Requirement | Test IDs (Planned) | Given | When | Then | Coverage |
|----|-------------|-------------------|-------|------|------|----------|
| **AC1** | `Transport` base class defines `bind()`, `connect()`, `send()`, `receive()`, `close()` interface | UNIT-001, UNIT-002, UNIT-003 | Transport ABC imported | Inspecting abstract methods | All 5 methods declared abstract | **PLANNED** |
| **AC2** | `TransportConfig` dataclass holds transport type, protocol, pattern, and options | UNIT-004, UNIT-005, UNIT-006 | TransportConfig instantiated with defaults | Accessing fields | type="unix", protocol="ipc", pattern="direct", options={} | **PLANNED** |
| **AC3** | `UnixPipeTransport` implements `Transport` interface using patterns from `StreamChannel` (reimplements pipe logic, does not delegate) | UNIT-007 to UNIT-010, INT-001, INT-002, INT-003 | UnixPipeTransport instantiated | Calling bind()/connect()/send()/receive() | Uses os.pipe() directly, NO StreamChannel import | **PLANNED** |
| **AC4** | `TransportFactory.create()` returns appropriate transport based on config | UNIT-011, UNIT-012, UNIT-013 | TransportConfig(type="unix") | Calling create_transport() | Returns UnixPipeTransport instance | **PLANNED** |
| **AC5** | Existing Unix pipe workflows work unchanged when `transport: unix` (default) | INT-004, INT-005 | Existing test_streams.py | Running pytest | Zero test failures, zero regressions | **CRITICAL** |
| **AC6** | `StreamRegistry` accepts optional `transport_factory` parameter | UNIT-014, UNIT-015, INT-006 | StreamRegistry initialized | Passing custom factory | Factory used for transport creation | **PLANNED** |
| **AC7** | Transport errors raise `TransportError` with helpful messages | UNIT-016, UNIT-017, UNIT-018 | TransportError raised | Inspecting exception | Contains operation context and helpful message | **PLANNED** |
| **AC8** | `is_connected` property reflects connection state accurately | UNIT-019, UNIT-020, UNIT-021, UNIT-022 | Transport instantiated | Before/after bind()/connect()/close() | False→True→False lifecycle | **PLANNED** |
| **AC9** | Context manager support (`with transport:`) for automatic cleanup | UNIT-023, UNIT-024, UNIT-025 | Transport used in `with` block | Exiting context (normal or exception) | close() called automatically | **PLANNED** |
| **AC10** | Graceful handling when transport is closed while operations pending | UNIT-026, UNIT-027, INT-007 | Transport with pending read/write | Calling close() | No crash, TransportError with context | **PLANNED** |

---

### Implementation Gap Analysis

#### Gap 1: No Implementation Exists
- **Status:** Transport abstraction layer has not been implemented
- **Missing Files:**
  - `python/src/the_edge_agent/transports/__init__.py`
  - `python/src/the_edge_agent/transports/base.py`
  - `python/src/the_edge_agent/transports/unix.py`
  - `python/src/the_edge_agent/transports/factory.py`
- **Missing Tests:**
  - `python/tests/test_transports.py`
- **Severity:** Expected (story is "Ready for Development")
- **Action:** Implement per task breakdown in story

#### Gap 2: Backward Compatibility Tests (Critical Path)
- **Requirement:** AC5 - Existing workflows unchanged
- **Existing Coverage:** `python/tests/test_streams.py` (18 tests for TEA-STREAM-001.1)
- **Gap:** No explicit test verifying transport layer doesn't break existing StreamChannel behavior
- **Recommendation:** INT-004 must explicitly run ALL existing test_streams.py tests post-implementation
- **Verification Command:** `pytest python/tests/test_streams.py -v`

#### Gap 3: Task 2 / AC3 Documentation Conflict
- **Issue:** Task 2 subtask says "Wrap existing `StreamChannel`" but AC3 says "reimplements pipe logic, does not delegate"
- **Resolution:** AC3 takes precedence (Acceptance Criteria are binding)
- **Action Required:** Update Task 2 subtask wording before development OR ensure dev follows AC3
- **Verification Test:** INT-003 must confirm no `StreamChannel` imports in `unix.py`

---

### Given-When-Then Detailed Mappings

#### AC1: Transport ABC Interface

**UNIT-001: Transport ABC defines all abstract methods**
```
Given: Transport ABC imported from base.py
When:  Inspecting abstract methods via inspect.isabstract()
Then:  bind(), connect(), send(), receive(), close() are all abstract
```

**UNIT-002: Transport ABC cannot be instantiated**
```
Given: Transport ABC class
When:  Attempting direct instantiation
Then:  TypeError raised indicating abstract methods
```

**UNIT-003: Transport ABC has is_connected property**
```
Given: Transport subclass implementation
When:  Accessing is_connected property
Then:  Returns boolean reflecting connection state
```

#### AC2: TransportConfig Dataclass

**UNIT-004: TransportConfig has required fields**
```
Given: TransportConfig class
When:  Inspecting dataclass fields
Then:  transport_type, protocol, pattern, options fields exist
```

**UNIT-005: TransportConfig defaults applied**
```
Given: TransportConfig instantiated with no arguments
When:  Accessing field values
Then:  transport_type="unix", protocol="ipc", pattern="direct"
```

**UNIT-006: TransportConfig options dict isolation (TECH-004 mitigation)**
```
Given: Two TransportConfig instances with default options
When:  Modifying options on instance A
Then:  Instance B options unchanged (no shared mutable state)
```

#### AC3: UnixPipeTransport Implementation

**UNIT-007: bind() creates write-side file descriptor**
```
Given: UnixPipeTransport instantiated
When:  Calling bind(address)
Then:  os.pipe() called, write_fd stored, is_connected=True
```

**UNIT-008: connect() opens read-side file descriptor**
```
Given: UnixPipeTransport with bound address
When:  Calling connect(address)
Then:  read_fd opened, is_connected=True
```

**UNIT-009: send() writes data to pipe**
```
Given: UnixPipeTransport after bind()
When:  Calling send(b"test data")
Then:  os.write() called with data, bytes written returned
```

**UNIT-010: receive() reads data from pipe**
```
Given: UnixPipeTransport after connect(), data available
When:  Calling receive(size)
Then:  os.read() called, data returned
```

**INT-001: Full send/receive cycle with real pipe**
```
Given: Producer and consumer UnixPipeTransport instances
When:  Producer binds, consumer connects, data sent/received
Then:  Data integrity verified, exact bytes match
```

**INT-002: Multi-message streaming**
```
Given: Connected transport pair
When:  Sending 100 sequential messages
Then:  All 100 messages received in order, no data loss
```

**INT-003: No StreamChannel delegation (TECH-003 mitigation)**
```
Given: python/src/the_edge_agent/transports/unix.py source
When:  Searching for StreamChannel imports/usage
Then:  Zero matches - pipe logic reimplemented directly
```

#### AC4: TransportFactory Dispatch

**UNIT-011: Factory returns UnixPipeTransport for "unix"**
```
Given: TransportConfig(transport_type="unix")
When:  Calling create_transport(config)
Then:  Instance of UnixPipeTransport returned
```

**UNIT-012: Factory raises TransportError for unknown type**
```
Given: TransportConfig(transport_type="unknown")
When:  Calling create_transport(config)
Then:  TransportError raised with helpful message
```

**UNIT-013: Factory lazy imports ZeroMQ (future)**
```
Given: TransportConfig(transport_type="zeromq")
When:  ZeroMQ not installed
Then:  TransportError raised with installation instructions
```

#### AC5: Backward Compatibility (CRITICAL)

**INT-004: ALL existing test_streams.py tests pass**
```
Given: Transport abstraction implemented
When:  Running pytest python/tests/test_streams.py
Then:  Zero failures, all 18 existing tests pass
```

**INT-005: Explicit transport: unix config works**
```
Given: YAML workflow with explicit transport: unix
When:  Executing workflow
Then:  Behavior identical to implicit default
```

#### AC6: StreamRegistry Integration

**UNIT-014: StreamRegistry accepts transport_factory parameter**
```
Given: Custom transport factory function
When:  Instantiating StreamRegistry(transport_factory=custom)
Then:  Factory stored and used for transport creation
```

**UNIT-015: StreamRegistry defaults to UnixPipeTransport factory**
```
Given: StreamRegistry instantiated without transport_factory
When:  Creating transports internally
Then:  UnixPipeTransport instances created
```

**INT-006: Custom factory integration end-to-end**
```
Given: Mock transport factory returning test transport
When:  StreamRegistry used in workflow
Then:  Mock transport used for all operations
```

#### AC7: TransportError Exception

**UNIT-016: TransportError includes operation context**
```
Given: send() called on closed transport
When:  Exception raised
Then:  Message includes "send" and "closed" context
```

**UNIT-017: TransportError is descriptive**
```
Given: Various error scenarios
When:  TransportError raised
Then:  Messages are actionable, not generic
```

**UNIT-018: TransportError hierarchy**
```
Given: TransportError class
When:  Checking inheritance
Then:  Inherits from Exception (not BaseException)
```

#### AC8: is_connected Property

**UNIT-019: is_connected False before bind/connect**
```
Given: Fresh UnixPipeTransport instance
When:  Accessing is_connected
Then:  Returns False
```

**UNIT-020: is_connected True after successful bind**
```
Given: UnixPipeTransport after bind() succeeds
When:  Accessing is_connected
Then:  Returns True
```

**UNIT-021: is_connected True after successful connect**
```
Given: UnixPipeTransport after connect() succeeds
When:  Accessing is_connected
Then:  Returns True
```

**UNIT-022: is_connected False after close**
```
Given: Connected UnixPipeTransport after close()
When:  Accessing is_connected
Then:  Returns False
```

#### AC9: Context Manager Support

**UNIT-023: __enter__ returns self**
```
Given: UnixPipeTransport
When:  Entering context with `with transport as t:`
Then:  t is transport (same object)
```

**UNIT-024: __exit__ calls close (normal exit)**
```
Given: Transport in context manager
When:  Exiting normally
Then:  close() called, is_connected=False
```

**UNIT-025: __exit__ calls close (exception exit)**
```
Given: Transport in context manager
When:  Exception raised inside block
Then:  close() still called before propagating exception
```

#### AC10: Graceful Pending Operations

**UNIT-026: send() on closed transport raises TransportError**
```
Given: Closed UnixPipeTransport
When:  Calling send(data)
Then:  TransportError with "closed" message, no crash
```

**UNIT-027: close() is idempotent (OPS-001 mitigation)**
```
Given: Transport already closed
When:  Calling close() again
Then:  No exception raised, no-op behavior
```

**INT-007: close() during pending read**
```
Given: Consumer transport blocking on receive()
When:  Another thread calls close()
Then:  Graceful interruption, no resource leak
```

---

### Coverage Gaps Identified

| Gap ID | Requirement | Gap Description | Severity | Recommended Action |
|--------|-------------|-----------------|----------|-------------------|
| GAP-001 | Implementation | Transport module not implemented | Expected | Implement per Tasks 1-6 |
| GAP-002 | Tests | test_transports.py not created | Expected | Create alongside implementation (TDD) |
| GAP-003 | AC3/Task2 | Documentation conflict | Medium | Clarify Task 2 wording, follow AC3 |
| GAP-004 | NFR-PERF | No performance targets defined | Low | Define latency targets before ZeroMQ story |
| GAP-005 | Windows | Platform behavior undefined | Low | Add note to docstrings |

---

### Recommendations

1. **Pre-Development:** Resolve Task 2 / AC3 conflict in story documentation
2. **TDD Approach:** Write P0 tests first, implementation second
3. **Critical Gate:** INT-004 (backward compatibility) must pass before any PR merge
4. **Resource Safety:** Context manager tests (UNIT-023, 024, 025) are non-negotiable
5. **Verification:** Run `grep -r "StreamChannel" python/src/the_edge_agent/transports/` to verify AC3 compliance

---

### Trace Quality Score: **95/100**

- Requirements clearly defined: ✅
- All ACs traceable to test scenarios: ✅
- Given-When-Then mappings complete: ✅
- Implementation gaps expected (pre-development): ✅
- One documentation conflict identified: ⚠️ (TECH-003)

---

**Trace Matrix Reference:** Inline above (this document)
**Test Design Reference:** `docs/qa/assessments/TEA-STREAM-002.1-test-design-20260201.md`
