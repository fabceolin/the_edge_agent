# Story TEA-STREAM-002.3: PUSH/PULL Pattern Implementation

## Status: Ready for Development

## Story

**As a** workflow developer,
**I want** ZeroMQ PUSH/PULL pattern for stream channels,
**So that** I can distribute work across multiple workers with automatic load balancing.

## Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `ZeroMQPushPullTransport` implements `Transport` interface | Unit test |
| AC2 | PUSH socket distributes messages round-robin to PULL sockets | Integration test |
| AC3 | Each message is received by exactly one worker (no duplicates) | Integration test |
| AC4 | Load balancing (LRU distribution) when workers have different processing speeds | Integration test |
| AC5 | Works with 1:N topology (one pusher, many pullers) | Integration test |
| AC6 | Works with N:1 topology (many pushers, one puller) | Integration test |
| AC7 | `high_water_mark` prevents slow worker blocking fast producer | Unit test |
| AC8 | Works on Windows using `inproc://` (same-process) or `tcp://127.0.0.1` (cross-process) | Integration test |
| AC9 | Works with TCP protocol for network distribution | Integration test |
| AC10 | Graceful handling when all workers disconnect | Unit test |

## Tasks / Subtasks

- [ ] **Task 1: Implement PUSH/PULL transport** (AC: 1, 7)
  - [ ] Create `python/src/the_edge_agent/transports/zeromq/pushpull.py`
  - [ ] Implement `ZeroMQPushPullTransport` class
  - [ ] Implement `bind()` with `zmq.PUSH` socket (work distributor)
  - [ ] Implement `connect()` with `zmq.PULL` socket (worker)
  - [ ] Implement `send()` for pushing work items
  - [ ] Implement `receive()` for pulling work items
  - [ ] Configure `SNDHWM` and `RCVHWM` from options
  - [ ] Implement `close()` with proper cleanup

- [ ] **Task 2: Implement reverse topology support** (AC: 5, 6)
  - [ ] Support `bind()` on PULL socket (sink pattern)
  - [ ] Support `connect()` on PUSH socket (ventilator pattern)
  - [ ] Add `role` option: `"distributor"` vs `"sink"`
  - [ ] Document topology patterns in docstrings

- [ ] **Task 3: Implement fair queuing** (AC: 2, 3, 4)
  - [ ] Verify round-robin distribution behavior
  - [ ] Add test for uneven worker speeds
  - [ ] Document ZMQ's fair queuing guarantees
  - [ ] Add `immediate` option for connect-time behavior

- [ ] **Task 4: Add protocol support** (AC: 8, 9)
  - [ ] Reuse IPC/TCP helpers from PUB/SUB story
  - [ ] Test IPC on Unix platforms
  - [ ] Test inproc on Windows
  - [ ] Test TCP for distributed workers

- [ ] **Task 5: Handle edge cases** (AC: 10)
  - [ ] Handle `zmq.Again` when no workers connected
  - [ ] Add send timeout option
  - [ ] Add reconnect interval for workers
  - [ ] Emit warning when send blocks

- [ ] **Task 6: Update TransportFactory** (AC: 1)
  - [ ] Modify `python/src/the_edge_agent/transports/factory.py`
  - [ ] Dispatch to `ZeroMQPushPullTransport` for `pattern: push_pull`
  - [ ] Handle `stream_mode: load_balance` as alias

- [ ] **Task 7: Write unit tests** (AC: 1, 7, 10)
  - [ ] Create `python/tests/test_zeromq_pushpull.py`
  - [ ] Test Transport interface compliance
  - [ ] Test single pusher, single puller
  - [ ] Test HWM configuration
  - [ ] Test send timeout behavior
  - [ ] Test no-worker scenario

- [ ] **Task 8: Write integration tests** (AC: 2-6, 8, 9)
  - [ ] Test round-robin with 3 workers
  - [ ] Test no message duplication
  - [ ] Test slow worker handling
  - [ ] Test 1:N topology
  - [ ] Test N:1 topology
  - [ ] Test cross-platform compatibility

## Dev Notes

### Relevant Source Tree

```
python/src/the_edge_agent/transports/
├── __init__.py
├── base.py
├── unix.py
├── factory.py              # Modified
└── zeromq/
    ├── __init__.py
    ├── base.py             # From Story 002.2
    ├── pubsub.py           # From Story 002.2
    └── pushpull.py         # NEW
```

### Dependencies

- **Runtime**: `pyzmq >= 25.0`
- **Story**: TEA-STREAM-002.1, TEA-STREAM-002.2 (shared ZMQ base)

### ZeroMQ PUSH/PULL Semantics

```
Ventilator Pattern (1:N)          Sink Pattern (N:1)

    PUSH (bind)                   PUSH (connect) ────┐
       │                          PUSH (connect) ────┼──► PULL (bind)
       ├──► PULL (connect)        PUSH (connect) ────┘
       ├──► PULL (connect)
       └──► PULL (connect)        Many producers, one collector

  One producer, many workers
  Round-robin distribution
```

### Key Design Decisions

1. **LRU distribution (not strict round-robin)**: ZMQ sends to the worker that became available first (Least Recently Used), which effectively balances load but may not be strictly alternating
2. **Exactly-once delivery**: Each message goes to exactly one PULL socket
3. **Back-pressure via HWM**: PUSH blocks when all workers are slow (unless `NOBLOCK` flag used)
4. **Dual topology**: Support both ventilator (1:N) and sink (N:1) patterns

### Platform Considerations

| Platform | IPC Support | Recommended Protocol |
|----------|-------------|---------------------|
| Linux | `ipc://` | IPC for local, TCP for network |
| macOS | `ipc://` | IPC for local, TCP for network |
| Windows | `inproc://` (same-process only) | **TCP required for cross-process** |

**Important Windows Note**: `inproc://` only works within the same process. For cross-process communication on Windows, use `tcp://127.0.0.1:port`.

### Implementation Sketch

```python
# zeromq/pushpull.py
import zmq
from ..base import Transport, TransportConfig, TransportError

class ZeroMQPushPullTransport(Transport):
    """PUSH/PULL pattern transport for load-balanced work distribution."""

    def __init__(self, config: TransportConfig):
        super().__init__(config)
        self.socket = None
        self._role = config.options.get("role", "distributor")

    def bind(self, address: str) -> None:
        """Bind as work distributor (PUSH) or sink (PULL)."""
        if self._role == "sink":
            self.socket = self.get_context().socket(zmq.PULL)
        else:
            self.socket = self.get_context().socket(zmq.PUSH)

        self._configure_socket()
        self.socket.bind(address)
        self._connected = True

    def connect(self, address: str) -> None:
        """Connect as worker (PULL) or producer (PUSH)."""
        if self._role == "sink":
            self.socket = self.get_context().socket(zmq.PUSH)
        else:
            self.socket = self.get_context().socket(zmq.PULL)

        self._configure_socket()
        self.socket.connect(address)
        self._connected = True

    def _configure_socket(self):
        hwm = self.config.options.get("high_water_mark", 1000)
        self.socket.setsockopt(zmq.SNDHWM, hwm)
        self.socket.setsockopt(zmq.RCVHWM, hwm)

        linger = self.config.options.get("linger", 1000)
        self.socket.setsockopt(zmq.LINGER, linger)

        # Optional send timeout
        send_timeout = self.config.options.get("send_timeout")
        if send_timeout:
            self.socket.setsockopt(zmq.SNDTIMEO, send_timeout)

        # Optional immediate mode (drop if no peers)
        immediate = self.config.options.get("immediate", False)
        if immediate:
            self.socket.setsockopt(zmq.IMMEDIATE, 1)

    def send(self, data: bytes, flags: int = 0) -> int:
        """Push work item to a worker."""
        try:
            self.socket.send(data, flags)
            return len(data)
        except zmq.Again:
            raise TransportError("Send timeout - no workers available")

    def receive(self, size: int = 4096, flags: int = 0) -> bytes:
        """Pull work item."""
        return self.socket.recv(flags)

    def close(self) -> None:
        if self.socket:
            self.socket.close()
            self.socket = None
        self._connected = False
```

### Topology Configuration

```yaml
# 1:N Ventilator pattern (default)
settings:
  parallel:
    streams:
      transport: zeromq
      pattern: push_pull
      zeromq:
        role: distributor  # PUSH binds, PULLs connect

# N:1 Sink pattern
settings:
  parallel:
    streams:
      transport: zeromq
      pattern: push_pull
      zeromq:
        role: sink  # PULL binds, PUSHs connect
```

### Testing

| Test Category | Location | Framework |
|--------------|----------|-----------|
| Unit tests | `python/tests/test_zeromq_pushpull.py` | pytest |
| Integration tests | `python/tests/test_zeromq_integration.py` | pytest |

**Test Standards**:
- Verify exactly-once delivery with message counting
- Test with simulated slow workers (sleep)
- Use multiprocessing for realistic worker simulation
- Track message IDs to verify no duplicates

### Load Balancing Verification

```python
# Test to verify round-robin distribution
def test_round_robin_distribution():
    """Each worker should receive approximately equal share."""
    pusher = create_pusher()
    pullers = [create_puller() for _ in range(3)]

    # Send 300 messages
    for i in range(300):
        pusher.send(f"task-{i}".encode())

    # Each puller should get ~100 messages
    counts = [puller.count_received() for puller in pullers]
    assert all(90 <= c <= 110 for c in counts)
```

## Definition of Done

- [ ] `ZeroMQPushPullTransport` fully implements `Transport` interface
- [ ] Round-robin distribution verified with 3+ workers
- [ ] No message duplication (exactly-once delivery)
- [ ] Fair queuing with slow workers verified
- [ ] Both 1:N and N:1 topologies work
- [ ] IPC works on Linux/macOS
- [ ] TCP works on all platforms
- [ ] Windows CI passes
- [ ] All unit and integration tests pass
- [ ] Code follows project style

## Dev Agent Record

_To be populated during implementation._

## QA Results

**Assessment Date:** 2026-01-08
**Test Architect:** Quinn

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 34 |
| Unit tests | 18 (53%) |
| Integration tests | 14 (41%) |
| E2E tests | 2 (6%) |
| P0 (Critical) tests | 12 |
| P1 (High) tests | 14 |

**Coverage Status:** All 10 Acceptance Criteria have test coverage. No coverage gaps identified.

### Risk Areas Identified

| Risk | Severity | Impact | Mitigating Tests |
|------|----------|--------|------------------|
| **Message duplication** | HIGH | Data integrity failure - workflows produce incorrect results | INT-004, INT-005 |
| **Message loss** | HIGH | Work items silently dropped, incomplete processing | INT-005, INT-006 |
| **Uneven load distribution** | HIGH | Core load balancing promise broken | INT-001, INT-003 |
| **Worker starvation** | MEDIUM | Slow workers receive no work under high load | INT-007 |
| **Resource leak on close** | MEDIUM | Memory/socket leaks in long-running processes | UNIT-001 (close() compliance) |
| **Platform incompatibility** | MEDIUM | Windows CI failures, deployment issues | INT-012, INT-013 |

### Recommended Test Scenarios (Priority)

**P0 - Must Pass (Blocking)**
1. `TEA-STREAM-002.3-INT-001`: Round-robin distribution with 3 workers, 300 messages - Core load balancing verification
2. `TEA-STREAM-002.3-INT-004`: Message ID tracking - no duplicates across workers
3. `TEA-STREAM-002.3-INT-005`: Total received equals total sent - completeness validation
4. `TEA-STREAM-002.3-INT-006`: Fair queuing with slow worker (100ms delay) vs fast workers
5. `TEA-STREAM-002.3-INT-009`: Ventilator pattern (1:N) - PUSH bind, multiple PULL connect
6. `TEA-STREAM-002.3-INT-010`: Sink pattern (N:1) - multiple PUSH connect, PULL bind
7. `TEA-STREAM-002.3-UNIT-009`: HWM configures SNDHWM socket option
8. `TEA-STREAM-002.3-UNIT-013`: send() raises TransportError when no workers connected

**P1 - Should Pass (Important)**
- Topology socket type verification (UNIT-005, 006, 007, 008)
- Windows inproc protocol (INT-012)
- TCP with distributed workers E2E (E2E-001)
- Statistical distribution variance validation (INT-003)

### Concerns / Blockers

1. **Multi-process test complexity**: Tests require spawning multiple processes with proper cleanup. Recommend timeout guards and process termination in fixtures to prevent CI hangs.

2. **Timing sensitivity**: Fair queuing tests (INT-006, INT-007) depend on worker processing speeds. Use sufficient message counts and relaxed variance bounds to avoid flaky tests.

3. **Port conflicts**: TCP tests (INT-013, INT-014) should use ephemeral ports or unique port allocation to prevent CI race conditions.

4. **Dependency on prior stories**: This story depends on TEA-STREAM-002.1 (Transport Abstraction) and TEA-STREAM-002.2 (PUB/SUB base). Ensure those are complete before implementation.

### Test File Locations

| Level | Location |
|-------|----------|
| Unit | `python/tests/test_zeromq_pushpull.py` |
| Integration | `python/tests/test_zeromq_integration.py` |
| E2E | `python/tests/test_zeromq_e2e.py` |

### References

- Full test design: `docs/qa/assessments/TEA-STREAM-002.3-test-design-20260108.md`
- Story dependencies: TEA-STREAM-002.1, TEA-STREAM-002.2

---

## QA Notes

**Reviewed by:** Quinn (Test Architect)
**Review Date:** 2026-01-08

### Quality Gate Status: **PASS**

PUSH/PULL pattern is critical for load-balanced work distribution. Test coverage focuses on exactly-once delivery and fair queuing.

### Test Implementation Guidance

**Recommended Test File:** `python/tests/test_zeromq_pushpull.py`

**Multi-Process Test Pattern:**
```python
import pytest
import multiprocessing
from concurrent.futures import ProcessPoolExecutor

zmq = pytest.importorskip("zmq")

def worker_process(address, worker_id, result_queue):
    """Worker that pulls work items and reports count."""
    ctx = zmq.Context()
    socket = ctx.socket(zmq.PULL)
    socket.connect(address)

    count = 0
    while True:
        try:
            socket.recv(flags=zmq.NOBLOCK)
            count += 1
        except zmq.Again:
            break

    result_queue.put((worker_id, count))
    socket.close()
    ctx.term()

@pytest.fixture
def worker_pool():
    """Fixture for multi-worker tests."""
    workers = []
    result_queue = multiprocessing.Queue()
    yield workers, result_queue
    # Cleanup handled by test
```

### Critical Implementation Notes

1. **LRU Distribution (not round-robin)**: ZeroMQ PUSH/PULL uses Least Recently Used distribution. Messages go to the worker that became available first, not in strict alternation.

2. **No Message Duplication**: Each message is delivered to exactly ONE PULL socket. This is guaranteed by ZeroMQ.

3. **HWM Back-pressure**: When all workers are slow and HWM is reached, PUSH blocks (unless `zmq.NOBLOCK` flag used).

4. **Dual Topology Support**:
   - **Ventilator (1:N)**: PUSH binds, PULLs connect - default
   - **Sink (N:1)**: PULL binds, PUSHs connect - use `role: sink`

### Distribution Test Methodology

```python
def test_distribution_fairness():
    """Verify approximate fair distribution."""
    NUM_MESSAGES = 300
    NUM_WORKERS = 3

    # Send 300 messages
    for i in range(NUM_MESSAGES):
        pusher.send(f"task-{i}".encode())

    # Each worker should get ~100 ± 20%
    for count in worker_counts:
        assert 80 <= count <= 120, f"Unfair distribution: {count}"
```

### Acceptance Criteria Traceability

| AC | Tests | Status |
|----|-------|--------|
| AC1 | UNIT-001 to UNIT-004 | ✅ Covered |
| AC2 | INT-001, INT-002, INT-003 | ✅ Covered (Critical) |
| AC3 | INT-004, INT-005 | ✅ Covered (Critical) |
| AC4 | INT-006, INT-007 | ✅ Covered |
| AC5 | INT-009, UNIT-005, UNIT-006 | ✅ Covered |
| AC6 | INT-010, UNIT-007, UNIT-008 | ✅ Covered |
| AC7 | UNIT-009, UNIT-010 | ✅ Covered |
| AC8 | INT-012 | ✅ Covered |
| AC9 | INT-013, INT-014, E2E-001 | ✅ Covered |
| AC10 | UNIT-013, UNIT-014 | ✅ Covered |

### Pre-Implementation Checklist

- [ ] Verify TEA-STREAM-002.1 and 002.2 are complete
- [ ] Plan multi-process test fixtures with proper cleanup
- [ ] Allocate ephemeral ports for TCP tests (`socket.bind(('', 0))`)
- [ ] Consider using `inproc://` for deterministic unit tests

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Initial story creation | Sarah (PO) |
| 2026-01-08 | 0.2 | Fixed AC4 terminology, AC8 Windows clarification per validation | Sarah (PO) |
| 2026-01-08 | 0.3 | Added QA Results section with test coverage and risk assessment | Quinn (QA) |
| 2026-01-08 | 0.4 | Added QA Notes with test implementation guidance | Quinn (QA) |
| 2026-01-08 | 0.5 | Status updated to Ready for Development | Auto-update |
