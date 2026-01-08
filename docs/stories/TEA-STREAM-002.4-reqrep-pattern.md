# Story TEA-STREAM-002.4: REQ/REP Pattern Implementation

## Status: Ready for Development

## Story

**As a** workflow developer,
**I want** ZeroMQ REQ/REP pattern for stream channels,
**So that** I can implement synchronous request-response communication between nodes.

## Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `ZeroMQReqRepTransport` implements `Transport` interface | Unit test |
| AC2 | REQ socket enforces send-receive-send-receive order | Unit test |
| AC3 | REP socket enforces receive-send-receive-send order | Unit test |
| AC4 | `timeout` option on `receive()` prevents infinite blocking | Unit test |
| AC5 | Works for RPC-style node communication | Integration test |
| AC6 | Error handling for protocol violations (double send/recv) | Unit test |
| AC7 | Multiple REQ clients can connect to one REP server | Integration test |
| AC8 | Works on Windows using `inproc://` (same-process) or `tcp://127.0.0.1` (cross-process) | Integration test |
| AC9 | Works with TCP protocol for network RPC | Integration test |
| AC10 | `request()` convenience method combines send + receive | Unit test |

## Tasks / Subtasks

- [ ] **Task 1: Implement REQ/REP transport** (AC: 1, 2, 3)
  - [ ] Create `python/src/the_edge_agent/transports/zeromq/reqrep.py`
  - [ ] Implement `ZeroMQReqRepTransport` class
  - [ ] Implement `bind()` with `zmq.REP` socket (server)
  - [ ] Implement `connect()` with `zmq.REQ` socket (client)
  - [ ] Implement `send()` with state tracking
  - [ ] Implement `receive()` with state tracking
  - [ ] Track `_expecting_reply` state for REQ socket
  - [ ] Track `_has_request` state for REP socket

- [ ] **Task 2: Add timeout support** (AC: 4)
  - [ ] Add `timeout` option to config
  - [ ] Set `zmq.RCVTIMEO` socket option
  - [ ] Handle `zmq.Again` exception
  - [ ] Raise `TransportError` with clear message

- [ ] **Task 3: Add protocol violation handling** (AC: 6)
  - [ ] Detect double send on REQ socket
  - [ ] Detect double receive on REQ socket
  - [ ] Detect send before receive on REP socket
  - [ ] Detect double receive on REP socket
  - [ ] Raise `TransportError` with violation description

- [ ] **Task 4: Add convenience methods** (AC: 10)
  - [ ] Implement `request(data: bytes) -> bytes` on client
  - [ ] Implement `reply(handler: Callable) -> None` on server
  - [ ] Add async variants for non-blocking use

- [ ] **Task 5: Add multi-client support** (AC: 7)
  - [ ] Verify REP handles multiple REQ clients
  - [ ] Test round-trip with 3 concurrent clients
  - [ ] Document load balancing behavior

- [ ] **Task 6: Add protocol support** (AC: 8, 9)
  - [ ] Reuse IPC/TCP helpers from previous stories
  - [ ] Test IPC on Unix platforms
  - [ ] Test inproc on Windows
  - [ ] Test TCP for network RPC

- [ ] **Task 7: Update TransportFactory** (AC: 1)
  - [ ] Modify `python/src/the_edge_agent/transports/factory.py`
  - [ ] Dispatch to `ZeroMQReqRepTransport` for `pattern: req_rep`

- [ ] **Task 8: Write unit tests** (AC: 1-4, 6, 10)
  - [ ] Create `python/tests/test_zeromq_reqrep.py`
  - [ ] Test Transport interface compliance
  - [ ] Test REQ send-recv-send-recv cycle
  - [ ] Test REP recv-send-recv-send cycle
  - [ ] Test timeout behavior
  - [ ] Test protocol violation errors
  - [ ] Test `request()` convenience method

- [ ] **Task 9: Write integration tests** (AC: 5, 7, 8, 9)
  - [ ] Test RPC-style communication
  - [ ] Test multi-client scenario
  - [ ] Test cross-platform compatibility
  - [ ] Test TCP network RPC

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
    ├── base.py
    ├── pubsub.py
    ├── pushpull.py
    └── reqrep.py           # NEW
```

### Dependencies

- **Runtime**: `pyzmq >= 25.0`
- **Story**: TEA-STREAM-002.1, TEA-STREAM-002.2 (shared ZMQ base)

### ZeroMQ REQ/REP Semantics

```
Client (REQ)                    Server (REP)
     │                               │
     │ connect()                     │ bind()
     │                               │
     │ send(request)                 │
     │────────────────────────────►  │
     │                               │ recv() → request
     │                               │ process(request)
     │                               │ send(reply)
     │  ◄────────────────────────────│
     │ recv() → reply                │
     │                               │

Strict alternation: send-recv-send-recv (client)
                    recv-send-recv-send (server)
```

### Key Design Decisions

1. **Synchronous by default**: REQ/REP is blocking request-response
2. **State machine**: Track expected next operation to catch violations
3. **Timeout required**: Always set timeout to prevent deadlocks
4. **Multi-client**: One REP can serve many REQ clients (round-robin)

### Platform Considerations

| Platform | IPC Support | Recommended Protocol |
|----------|-------------|---------------------|
| Linux | `ipc://` | IPC for local, TCP for network |
| macOS | `ipc://` | IPC for local, TCP for network |
| Windows | `inproc://` (same-process only) | **TCP required for cross-process** |

**Important Windows Note**: `inproc://` only works within the same process. For cross-process RPC on Windows, use `tcp://127.0.0.1:port`.

### Inherited Methods

The implementation sketch uses `self.get_context()` which is inherited from `ZeroMQTransport` base class (defined in Story 002.2). This provides the shared `zmq.Context` singleton.

### Implementation Sketch

```python
# zeromq/reqrep.py
import zmq
from ..base import Transport, TransportConfig, TransportError

class ZeroMQReqRepTransport(Transport):
    """REQ/REP pattern transport for synchronous request-response."""

    def __init__(self, config: TransportConfig):
        super().__init__(config)
        self.socket = None
        self._is_server = False
        self._expecting_reply = False  # REQ state
        self._has_request = False      # REP state

    def bind(self, address: str) -> None:
        """Bind as REP server."""
        self.socket = self.get_context().socket(zmq.REP)
        self._configure_socket()
        self.socket.bind(address)
        self._is_server = True
        self._connected = True

    def connect(self, address: str) -> None:
        """Connect as REQ client."""
        self.socket = self.get_context().socket(zmq.REQ)
        self._configure_socket()
        self.socket.connect(address)
        self._is_server = False
        self._connected = True

    def _configure_socket(self):
        linger = self.config.options.get("linger", 1000)
        self.socket.setsockopt(zmq.LINGER, linger)

        timeout = self.config.options.get("timeout", 30000)  # 30s default
        self.socket.setsockopt(zmq.RCVTIMEO, timeout)
        self.socket.setsockopt(zmq.SNDTIMEO, timeout)

    def send(self, data: bytes, flags: int = 0) -> int:
        """Send request (client) or reply (server)."""
        if not self._is_server and self._expecting_reply:
            raise TransportError(
                "Protocol violation: must receive reply before sending next request"
            )
        if self._is_server and not self._has_request:
            raise TransportError(
                "Protocol violation: must receive request before sending reply"
            )

        try:
            self.socket.send(data, flags)
        except zmq.Again:
            raise TransportError("Send timeout")

        if not self._is_server:
            self._expecting_reply = True
        else:
            self._has_request = False

        return len(data)

    def receive(self, size: int = 4096, flags: int = 0) -> bytes:
        """Receive reply (client) or request (server)."""
        if not self._is_server and not self._expecting_reply:
            raise TransportError(
                "Protocol violation: must send request before receiving reply"
            )

        try:
            data = self.socket.recv(flags)
        except zmq.Again:
            raise TransportError("Receive timeout")

        if not self._is_server:
            self._expecting_reply = False
        else:
            self._has_request = True

        return data

    def request(self, data: bytes) -> bytes:
        """Convenience: send request and receive reply."""
        self.send(data)
        return self.receive()

    def close(self) -> None:
        if self.socket:
            self.socket.close()
            self.socket = None
        self._connected = False
        self._expecting_reply = False
        self._has_request = False
```

### Use Case: RPC-Style Node Communication

```yaml
nodes:
  - name: rpc_client
    run: |
      import zmq
      # Request-response cycle
      response = transport.request(b"get_status")
      return {"status": response.decode()}
    streams:
      stdout: rpc_channel
      protocol: tcp
      pattern: req_rep
      role: client

  - name: rpc_server
    run: |
      import zmq
      while True:
        request = transport.receive()
        if request == b"get_status":
          transport.send(b"OK")
        else:
          transport.send(b"UNKNOWN")
    streams:
      stdin: rpc_channel
      role: server
```

### Testing

| Test Category | Location | Framework |
|--------------|----------|-----------|
| Unit tests | `python/tests/test_zeromq_reqrep.py` | pytest |
| Integration tests | `python/tests/test_zeromq_integration.py` | pytest |

**Test Standards**:
- Test strict alternation enforcement
- Test timeout scenarios
- Use threading for multi-client tests
- Verify protocol violation errors are clear

### Protocol Violation Examples

```python
# REQ violations
client.send(b"request1")
client.send(b"request2")  # Error: must receive first

client.receive()  # Error: must send first

# REP violations
server.send(b"reply")  # Error: must receive first

server.receive()
server.receive()  # Error: must send reply first
```

## Definition of Done

- [ ] `ZeroMQReqRepTransport` fully implements `Transport` interface
- [ ] Strict send-recv alternation enforced on both sides
- [ ] Protocol violations raise clear `TransportError`
- [ ] Timeout prevents infinite blocking
- [ ] Multi-client scenario works correctly
- [ ] `request()` convenience method works
- [ ] IPC works on Linux/macOS
- [ ] TCP works on all platforms
- [ ] Windows CI passes
- [ ] All unit and integration tests pass
- [ ] Code follows project style

## QA Notes

**Reviewed:** 2026-01-08
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Level | Count | Percentage |
|-------|-------|------------|
| Unit | 16 | 57% |
| Integration | 12 | 43% |
| E2E | 0 | 0% |
| **Total** | **28** | 100% |

**Priority Distribution:** P0: 8, P1: 12, P2: 8

All 10 acceptance criteria have test coverage. No E2E tests required—this is infrastructure-level code where integration tests adequately cover RPC communication patterns.

### Risk Areas Identified

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Deadlock from infinite blocking | Medium | High | AC4 timeout tests (P0) |
| Protocol violation causing silent failures | Low | High | AC6 violation tests (P0-P1) |
| Multi-client race conditions | Medium | Medium | AC7 concurrent tests (P1) |
| Cross-platform incompatibility | Low | Medium | AC8 platform tests (P1-P2) |

### Recommended Test Scenarios

**P0 Critical (Must Pass):**
- `TEA-STREAM-002.4-UNIT-001`: Transport interface inheritance
- `TEA-STREAM-002.4-UNIT-005-007`: REQ send-recv alternation
- `TEA-STREAM-002.4-UNIT-008-009`: REP recv-send alternation
- `TEA-STREAM-002.4-UNIT-011`: Timeout raises TransportError
- `TEA-STREAM-002.4-UNIT-014`: Double send violation error
- `TEA-STREAM-002.4-INT-001`: Full REQ protocol cycle with live server

**P1 Important:**
- Multi-client concurrent scenarios (INT-007, INT-008)
- JSON payload round-trip (INT-005)
- TCP and inproc transport tests (INT-010, INT-012)
- `request()` convenience method (UNIT-017)

### Concerns / Blockers

1. **Threading complexity**: Multi-client tests require careful thread synchronization—recommend using `inproc://` transport for deterministic behavior in unit tests
2. **Windows CI**: Verify `inproc://` fallback works correctly since IPC (Unix domain sockets) is unavailable on Windows
3. **State reset on close()**: Ensure `_expecting_reply` and `_has_request` flags reset properly to avoid stale state on reconnection

### Recommendations

1. Implement P0 tests first to fail fast on core protocol issues
2. Use mock ZMQ context for unit tests to isolate state machine logic from network
3. Set short timeouts (5000ms) in integration tests to avoid slow CI on failures
4. Document fair-queuing behavior for multi-client scenarios (ZeroMQ handles this automatically but users should understand it)

**Test Design Assessment:** `docs/qa/assessments/TEA-STREAM-002.4-test-design-20260108.md`

### Quality Gate Status: **PASS**

REQ/REP pattern requires strict state machine enforcement. Test coverage properly emphasizes protocol compliance.

### Test Implementation Guidance

**Recommended Test File:** `python/tests/test_zeromq_reqrep.py`

**State Machine Test Pattern:**
```python
import pytest
zmq = pytest.importorskip("zmq")

class TestReqRepStateMachine:
    """Test strict send-recv alternation."""

    def test_req_must_send_before_recv(self, req_transport):
        """REQ: receive before send raises error."""
        with pytest.raises(TransportError, match="must send request"):
            req_transport.receive()

    def test_req_no_double_send(self, req_transport, rep_transport):
        """REQ: send-send raises error."""
        req_transport.send(b"first")
        with pytest.raises(TransportError, match="must receive reply"):
            req_transport.send(b"second")

    def test_rep_must_recv_before_send(self, rep_transport):
        """REP: send before receive raises error."""
        with pytest.raises(TransportError, match="must receive request"):
            rep_transport.send(b"reply")
```

**Fixtures Required:**
```python
@pytest.fixture
def req_rep_pair():
    """Create connected REQ/REP transport pair."""
    config = TransportConfig(transport_type="zeromq", pattern="req_rep")

    server = ZeroMQReqRepTransport(config)
    client = ZeroMQReqRepTransport(config)

    addr = "inproc://test-reqrep"
    server.bind(addr)
    client.connect(addr)

    yield client, server

    client.close()
    server.close()
```

### Critical Implementation Notes

1. **State Machine**: Track `_expecting_reply` (REQ) and `_has_request` (REP) flags to enforce alternation.

2. **Inherited Method**: `self.get_context()` is inherited from `ZeroMQTransport` base class (Story 002.2).

3. **Timeout Required**: Always set `zmq.RCVTIMEO` and `zmq.SNDTIMEO` to prevent deadlocks.

4. **Protocol Violations**: Raise `TransportError` with clear messages:
   - REQ double send: "must receive reply before sending next request"
   - REQ early recv: "must send request before receiving reply"
   - REP early send: "must receive request before sending reply"
   - REP double recv: "must send reply before receiving next request"

5. **Multi-Client**: REP handles multiple REQ clients via ZeroMQ's fair-queuing (LRU).

### Acceptance Criteria Traceability

| AC | Tests | Status |
|----|-------|--------|
| AC1 | UNIT-001 to UNIT-004 | ✅ Covered |
| AC2 | UNIT-005, UNIT-006, UNIT-007 | ✅ Covered |
| AC3 | UNIT-008, UNIT-009 | ✅ Covered |
| AC4 | UNIT-010, UNIT-011, UNIT-012 | ✅ Covered (Critical) |
| AC5 | INT-001, INT-002, INT-003 | ✅ Covered |
| AC6 | UNIT-013, UNIT-014, UNIT-015, UNIT-016 | ✅ Covered |
| AC7 | INT-007, INT-008 | ✅ Covered |
| AC8 | INT-010, INT-012 | ✅ Covered |
| AC9 | INT-009, INT-011 | ✅ Covered |
| AC10 | UNIT-017, UNIT-018 | ✅ Covered |

### Pre-Implementation Checklist

- [ ] Verify TEA-STREAM-002.1 and 002.2 are complete
- [ ] Plan state machine flag tracking carefully
- [ ] Implement timeout defaults (30s recommended)
- [ ] Test protocol violations before happy path

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Initial story creation | Sarah (PO) |
| 2026-01-08 | 0.2 | Added QA Notes | Quinn (QA) |
| 2026-01-08 | 0.3 | Fixed AC8 Windows clarification, added platform notes per validation | Sarah (PO) |
| 2026-01-08 | 0.4 | Enhanced QA Notes with test implementation guidance and AC traceability | Quinn (QA) |
