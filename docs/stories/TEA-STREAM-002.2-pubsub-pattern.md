# Story TEA-STREAM-002.2: PUB/SUB Pattern Implementation

## Status: Ready for Development

## Story

**As a** workflow developer,
**I want** ZeroMQ PUB/SUB pattern for stream channels,
**So that** I can broadcast data to multiple subscribers without coordinator overhead.

## Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `ZeroMQPubSubTransport` implements `Transport` interface | Unit test |
| AC2 | Publisher calls `bind()`, subscribers call `connect()` | Unit test |
| AC3 | All subscribers receive all published messages (fan-out) | Integration test |
| AC4 | Topic filtering with `subscribe_filter` option | Unit test |
| AC5 | `high_water_mark` option prevents memory exhaustion | Unit test |
| AC6 | Works with IPC protocol on Linux/macOS | Integration test |
| AC7 | Works on Windows using `inproc://` (same-process) or `tcp://127.0.0.1` (cross-process) | Integration test |
| AC8 | Works with TCP protocol for network streaming | Integration test |
| AC9 | `linger` option controls close behavior | Unit test |
| AC10 | Slow subscriber warning when HWM reached | Unit test |

## Tasks / Subtasks

- [ ] **Task 1: Create ZeroMQ transport module** (AC: 1)
  - [ ] Create `python/src/the_edge_agent/transports/zeromq/__init__.py`
  - [ ] Create `python/src/the_edge_agent/transports/zeromq/base.py`
  - [ ] Define `ZeroMQTransport` base class extending `Transport`
  - [ ] Add shared context management (singleton pattern)
  - [ ] Add socket options helper methods

- [ ] **Task 2: Implement PUB/SUB transport** (AC: 1, 2, 3, 4, 5, 9)
  - [ ] Create `python/src/the_edge_agent/transports/zeromq/pubsub.py`
  - [ ] Implement `ZeroMQPubSubTransport` class
  - [ ] Implement `bind()` with `zmq.PUB` socket
  - [ ] Implement `connect()` with `zmq.SUB` socket
  - [ ] Implement `send()` for publishing
  - [ ] Implement `receive()` for subscribing
  - [ ] Add `subscribe_filter` support via `zmq.SUBSCRIBE`
  - [ ] Configure `SNDHWM` and `RCVHWM` from options
  - [ ] Configure `LINGER` from options
  - [ ] Implement `close()` with proper socket cleanup

- [ ] **Task 3: Add IPC protocol support** (AC: 6, 7)
  - [ ] Implement IPC address generation for Unix (`ipc:///tmp/tea-{name}.sock`)
  - [ ] Implement inproc fallback for Windows (`inproc://tea-{name}`)
  - [ ] Add platform detection helper
  - [ ] Test IPC on Linux/macOS
  - [ ] Test inproc on Windows (CI)

- [ ] **Task 4: Add TCP protocol support** (AC: 8)
  - [ ] Implement TCP address parsing and validation
  - [ ] Support `tcp://*:port` for bind
  - [ ] Support `tcp://host:port` for connect
  - [ ] Add connection timeout handling
  - [ ] Test with localhost TCP

- [ ] **Task 5: Add monitoring and diagnostics** (AC: 10)
  - [ ] Add logging for HWM events
  - [ ] Add `get_stats()` method for socket statistics
  - [ ] Add slow subscriber detection
  - [ ] Emit warning when messages dropped

- [ ] **Task 6: Update TransportFactory** (AC: 1)
  - [ ] Modify `python/src/the_edge_agent/transports/factory.py`
  - [ ] Add `zeromq` transport type handling
  - [ ] Dispatch to `ZeroMQPubSubTransport` for `pattern: pub_sub`
  - [ ] Add pyzmq import check with helpful error message

- [ ] **Task 7: Write unit tests** (AC: 1-5, 9, 10)
  - [ ] Create `python/tests/test_zeromq_pubsub.py`
  - [ ] Test Transport interface compliance
  - [ ] Test single publisher, single subscriber
  - [ ] Test single publisher, multiple subscribers
  - [ ] Test topic filtering
  - [ ] Test HWM configuration
  - [ ] Test linger behavior
  - [ ] Test slow subscriber warning

- [ ] **Task 8: Write integration tests** (AC: 3, 6, 7, 8)
  - [ ] Test fan-out with 3 subscribers
  - [ ] Test IPC on current platform
  - [ ] Test TCP with localhost
  - [ ] Test cross-process communication
  - [ ] Test message ordering guarantee

## Dev Notes

### Relevant Source Tree

```
python/src/the_edge_agent/transports/
├── __init__.py
├── base.py                 # From Story 002.1
├── unix.py                 # From Story 002.1
├── factory.py              # Modified in this story
└── zeromq/                 # NEW
    ├── __init__.py
    ├── base.py             # ZeroMQ base transport
    └── pubsub.py           # PUB/SUB implementation
```

### Dependencies

- **Runtime**: `pyzmq >= 25.0`
- **Story**: TEA-STREAM-002.1 (Transport Abstraction Layer)

### ZeroMQ PUB/SUB Semantics

```
Publisher (PUB)                 Subscribers (SUB)
      │                              │
      │ bind("tcp://*:5555")         │
      │◄────────────────────────────►│ connect("tcp://host:5555")
      │                              │
      │ send(b"topic:message")       │
      │──────────────────────────────►│ recv() → b"topic:message"
      │                              │
      │          Fan-out             │
      │──────────────────────────────►│ All subscribers get copy
      │──────────────────────────────►│
      │──────────────────────────────►│
```

### Key Design Decisions

1. **Singleton ZMQ Context**: One `zmq.Context` per process for efficiency
2. **Topic = stream name**: Use stream channel name as topic prefix
3. **Fire-and-forget**: PUB doesn't block if no subscribers
4. **Slow subscriber drop**: Messages dropped when HWM reached (configurable)

### Implementation Sketch

```python
# zeromq/pubsub.py
import zmq
from ..base import Transport, TransportConfig, TransportError

class ZeroMQPubSubTransport(Transport):
    """PUB/SUB pattern transport using ZeroMQ."""

    _context = None  # Shared context

    @classmethod
    def get_context(cls) -> zmq.Context:
        if cls._context is None:
            cls._context = zmq.Context.instance()
        return cls._context

    def __init__(self, config: TransportConfig):
        super().__init__(config)
        self.socket = None
        self._is_publisher = False

    def bind(self, address: str) -> None:
        """Bind as publisher."""
        self.socket = self.get_context().socket(zmq.PUB)
        self._configure_socket()
        self.socket.bind(address)
        self._is_publisher = True
        self._connected = True

    def connect(self, address: str) -> None:
        """Connect as subscriber."""
        self.socket = self.get_context().socket(zmq.SUB)
        self._configure_socket()

        # Subscribe filter (empty = all messages)
        topic = self.config.options.get("subscribe_filter", b"")
        if isinstance(topic, str):
            topic = topic.encode()
        self.socket.setsockopt(zmq.SUBSCRIBE, topic)

        self.socket.connect(address)
        self._connected = True

    def _configure_socket(self):
        hwm = self.config.options.get("high_water_mark", 1000)
        self.socket.setsockopt(zmq.SNDHWM, hwm)
        self.socket.setsockopt(zmq.RCVHWM, hwm)

        linger = self.config.options.get("linger", 1000)
        self.socket.setsockopt(zmq.LINGER, linger)

    def send(self, data: bytes, flags: int = 0) -> int:
        """Publish message."""
        self.socket.send(data, flags)
        return len(data)

    def receive(self, size: int = 4096, flags: int = 0) -> bytes:
        """Receive message."""
        return self.socket.recv(flags)

    def close(self) -> None:
        if self.socket:
            self.socket.close()
            self.socket = None
        self._connected = False
```

### Platform Considerations

| Platform | IPC Support | Recommended Protocol |
|----------|-------------|---------------------|
| Linux | `ipc://` | IPC for local, TCP for network |
| macOS | `ipc://` | IPC for local, TCP for network |
| Windows | `inproc://` (same-process only) | **TCP required for cross-process** |

**Important Windows Note**: `inproc://` only works within the same process (thread-to-thread). For cross-process communication on Windows, you **must** use `tcp://127.0.0.1:port`. This is a ZeroMQ limitation, not a TEA limitation.

### Testing

| Test Category | Location | Framework |
|--------------|----------|-----------|
| Unit tests | `python/tests/test_zeromq_pubsub.py` | pytest |
| Integration tests | `python/tests/test_zeromq_integration.py` | pytest |

**Test Standards**:
- Skip ZeroMQ tests if pyzmq not installed (`pytest.importorskip`)
- Use `inproc://` for unit tests (fastest, no cleanup)
- Use `tcp://127.0.0.1` for integration tests
- Clean up sockets in fixtures

## Definition of Done

- [ ] `ZeroMQPubSubTransport` fully implements `Transport` interface
- [ ] PUB/SUB fan-out verified with 3+ subscribers
- [ ] Topic filtering works correctly
- [ ] IPC works on Linux/macOS
- [ ] TCP works on all platforms
- [ ] Windows CI passes with inproc tests
- [ ] pyzmq optional dependency handled gracefully
- [ ] HWM and linger options configurable
- [ ] All unit and integration tests pass
- [ ] Code follows project style

## Dev Agent Record

_To be populated during implementation._

## QA Results

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 25 |
| Unit tests | 12 (48%) |
| Integration tests | 10 (40%) |
| E2E tests | 3 (12%) |
| Priority distribution | P0: 8, P1: 10, P2: 5, P3: 2 |

**Coverage by Acceptance Criteria:**
- AC1 (Transport Interface): 4 unit tests (interface compliance, factory routing)
- AC2 (bind/connect): 4 unit tests + 1 integration test (role differentiation)
- AC3 (Fan-out): 4 integration tests + 1 E2E test (core PUB/SUB semantics)
- AC4 (Topic Filtering): 2 unit + 2 integration tests (subscribe_filter option)
- AC5 (HWM): 2 unit + 1 integration tests (memory protection)
- AC6 (IPC Linux/macOS): 1 integration + 1 E2E test (Unix IPC)
- AC7 (Windows): 1 integration test (inproc fallback)
- AC8 (TCP): 1 E2E test (network streaming)
- AC9 (Linger): 2 unit tests (close behavior)
- AC10 (Slow Subscriber Warning): 1 unit + 1 integration test (diagnostics)

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **pyzmq import failures** | HIGH | All tests use `pytest.importorskip("zmq")` - graceful skip |
| **Socket/context resource leaks** | HIGH | UNIT-016 covers cleanup on exception; context manager pattern required |
| **Windows IPC incompatibility** | HIGH | AC7 clarifies inproc:// for same-process, TCP for cross-process |
| **Message ordering violations** | MEDIUM | INT-004 validates per-subscriber ordering guarantee |
| **HWM configuration confusion** | MEDIUM | UNIT-011/012 verify both SNDHWM and RCVHWM applied |
| **TCP port conflicts in CI** | MEDIUM | Use dynamic port allocation via `socket.bind(('', 0))` |
| **Singleton context lifecycle** | MEDIUM | UNIT-017 validates shared context pattern |

### Recommended Test Scenarios (Critical Path)

**P0 - Must Pass Before Merge:**
1. `TEA-STREAM-002.2-UNIT-001` - Interface inheritance verification
2. `TEA-STREAM-002.2-UNIT-002` - All abstract methods implemented
3. `TEA-STREAM-002.2-UNIT-005` - bind() creates zmq.PUB socket
4. `TEA-STREAM-002.2-UNIT-006` - connect() creates zmq.SUB socket
5. `TEA-STREAM-002.2-UNIT-011` - SNDHWM configured on publisher
6. `TEA-STREAM-002.2-UNIT-012` - RCVHWM configured on subscriber
7. `TEA-STREAM-002.2-UNIT-016` - Resource cleanup on exception
8. `TEA-STREAM-002.2-INT-002` - Single subscriber receives all messages
9. `TEA-STREAM-002.2-INT-003` - Multi-subscriber fan-out (3 subscribers)
10. `TEA-STREAM-002.2-INT-009/010` - Platform-specific IPC (Linux/macOS or Windows)
11. `TEA-STREAM-002.2-E2E-001` - Cross-process fan-out (5 subscribers)

**P1 - Required for Release:**
- Topic filtering end-to-end (INT-006)
- Late-joining subscriber semantics (INT-005)
- Message ordering validation (INT-004)
- TCP transport (E2E-003)
- Singleton context sharing (UNIT-017)

### Concerns / Blockers

1. **No blockers identified** - Story is well-defined with clear dependency on TEA-STREAM-002.1

2. **Platform testing requirement**: Windows CI **must** test inproc:// (same-process) and TCP (cross-process) paths since `ipc://` is not supported. Ensure CI matrix includes Windows runner.

3. **pyzmq version dependency**: Story specifies `pyzmq >= 25.0` - ensure minimum version is enforced in `setup.py` or `pyproject.toml` optional dependencies.

4. **Implementation concern**: The `_context = None` singleton pattern in the implementation sketch should handle termination carefully - `zmq.Context.instance()` manages this, but explicit termination during testing may cause issues.

5. **Slow subscriber detection (AC10)**: ZeroMQ doesn't provide direct notification when messages are dropped at HWM. Implementation may need to use `zmq.EVENTS` polling or statistics - clarify approach before implementation.

### QA Recommendation

**PASS** - Story is ready for development with comprehensive test coverage plan.

Key validations:
- Multi-subscriber fan-out tests (INT-003, E2E-001) are critical for PUB/SUB semantics
- Platform-specific tests must run on actual platform (not mocked)
- Resource cleanup tests prevent production socket leaks
- Topic filtering tests ensure selective subscription works correctly

**Test Design Reference**: `docs/qa/assessments/TEA-STREAM-002.2-test-design-20260108.md`

---

## QA Notes

**Reviewed by:** Quinn (Test Architect)
**Review Date:** 2026-01-08

### Quality Gate Status: **PASS**

PUB/SUB is the first ZeroMQ pattern implementation. Test coverage is comprehensive with appropriate focus on fan-out semantics.

### Test Implementation Guidance

**Recommended Test File:** `python/tests/test_zeromq_pubsub.py`

**Mock Strategy:**
- Use `pytest.importorskip("zmq")` at module level for graceful skip
- Use `inproc://` for unit tests (fastest, no cleanup needed)
- Use `tcp://127.0.0.1` for integration tests
- Mock `zmq.Context.instance()` only for singleton tests

**Fixtures Required:**
```python
import pytest
zmq = pytest.importorskip("zmq")

@pytest.fixture
def pub_socket():
    ctx = zmq.Context.instance()
    socket = ctx.socket(zmq.PUB)
    yield socket
    socket.close()

@pytest.fixture
def sub_socket():
    ctx = zmq.Context.instance()
    socket = ctx.socket(zmq.SUB)
    socket.setsockopt(zmq.SUBSCRIBE, b"")
    yield socket
    socket.close()
```

### Critical Implementation Notes

1. **Singleton Context**: Use `zmq.Context.instance()` for shared context across all transports.

2. **Subscribe Filter**: Empty filter (`b""`) subscribes to ALL messages. Non-empty filter matches message prefix.

3. **Late Subscriber**: Messages sent before subscriber connects are **lost** - this is expected PUB/SUB behavior.

4. **Slow Subscriber Detection (AC10)**: ZeroMQ drops messages silently when HWM reached. Options:
   - Use `zmq.EVENTS` polling to detect send would block
   - Use `zmq.XPUB` socket to receive subscription events
   - Log warning when `send()` returns immediately with HWM reached

### Platform-Specific Test Requirements

| Platform | IPC | inproc | TCP | CI Required |
|----------|-----|--------|-----|-------------|
| Linux | ✅ `ipc:///tmp/test.sock` | ✅ | ✅ | Yes |
| macOS | ✅ `ipc:///tmp/test.sock` | ✅ | ✅ | Yes |
| Windows | ❌ Not supported | ✅ (same-process) | ✅ | **TCP required** |

### Acceptance Criteria Traceability

| AC | Tests | Status |
|----|-------|--------|
| AC1 | UNIT-001 to UNIT-004 | ✅ Covered |
| AC2 | UNIT-005 to UNIT-008, INT-001 | ✅ Covered |
| AC3 | INT-002 to INT-005, E2E-001 | ✅ Covered (Critical) |
| AC4 | UNIT-009, UNIT-010, INT-006, INT-007 | ✅ Covered |
| AC5 | UNIT-011 to UNIT-013, INT-008 | ✅ Covered |
| AC6 | INT-009, E2E-002 | ✅ Covered |
| AC7 | INT-010 | ✅ Covered |
| AC8 | E2E-003 | ✅ Covered |
| AC9 | UNIT-014, UNIT-015 | ✅ Covered |
| AC10 | UNIT-016, INT-011 | ✅ Covered |

### Pre-Implementation Checklist

- [ ] Verify pyzmq >= 25.0 is installed
- [ ] Run `python -c "import zmq; print(zmq.zmq_version())"` to verify ZeroMQ version
- [ ] Plan for singleton context lifecycle in tests
- [ ] Consider slow subscriber detection approach before implementing AC10

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Initial story creation | Sarah (PO) |
| 2026-01-08 | 0.2 | Fixed AC7 Windows IPC clarification per validation | Sarah (PO) |
| 2026-01-08 | 0.3 | Added QA Notes with test implementation guidance | Quinn (QA) |
| 2026-01-08 | 0.4 | Status updated to Ready for Development | Auto-update |
