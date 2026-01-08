# Test Design: Story TEA-STREAM-002.4

**Date:** 2026-01-08
**Designer:** Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 28
- **Unit tests:** 16 (57%)
- **Integration tests:** 12 (43%)
- **E2E tests:** 0 (0%)
- **Priority distribution:** P0: 8, P1: 12, P2: 8

### Rationale

REQ/REP is a foundational transport pattern for synchronous RPC-style communication. The test strategy emphasizes:

1. **Protocol correctness**: Strict send-recv alternation is critical for ZeroMQ REQ/REP semantics
2. **Error handling**: Protocol violations must surface clear, actionable errors
3. **Timeout handling**: Preventing deadlocks is essential for production use
4. **Multi-client scenarios**: Real-world deployments have multiple clients

No E2E tests are required because:
- This is infrastructure-level code, not user-facing
- Integration tests adequately cover RPC communication patterns
- No cross-system or compliance requirements

---

## Test Scenarios by Acceptance Criteria

### AC1: `ZeroMQReqRepTransport` implements `Transport` interface

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.4-UNIT-001 | Unit | P0 | Verify class inherits from `Transport` base | Interface compliance is fundamental |
| TEA-STREAM-002.4-UNIT-002 | Unit | P1 | Verify all abstract methods implemented (`bind`, `connect`, `send`, `receive`, `close`) | Complete interface coverage |
| TEA-STREAM-002.4-UNIT-003 | Unit | P1 | Verify `TransportConfig` accepted in constructor | Configuration handling |
| TEA-STREAM-002.4-UNIT-004 | Unit | P2 | Verify `_connected` state tracking | Internal state consistency |

### AC2: REQ socket enforces send-receive-send-receive order

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.4-UNIT-005 | Unit | P0 | REQ client allows initial `send()` | Happy path start |
| TEA-STREAM-002.4-UNIT-006 | Unit | P0 | REQ client allows `receive()` after `send()` | Core protocol flow |
| TEA-STREAM-002.4-UNIT-007 | Unit | P0 | REQ client allows second `send()` after `receive()` | Complete cycle validation |
| TEA-STREAM-002.4-INT-001 | Integration | P0 | Full REQ send-recv-send-recv cycle with live REP server | End-to-end protocol flow |

### AC3: REP socket enforces receive-send-receive-send order

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.4-UNIT-008 | Unit | P0 | REP server allows initial `receive()` (blocks until request) | Happy path start |
| TEA-STREAM-002.4-UNIT-009 | Unit | P0 | REP server allows `send()` after `receive()` | Core protocol flow |
| TEA-STREAM-002.4-UNIT-010 | Unit | P1 | REP server allows second `receive()` after `send()` | Complete cycle validation |
| TEA-STREAM-002.4-INT-002 | Integration | P1 | Full REP recv-send-recv-send cycle with live REQ client | End-to-end protocol flow |

### AC4: `timeout` option on `receive()` prevents infinite blocking

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.4-UNIT-011 | Unit | P0 | `receive()` raises `TransportError` after timeout expires | Critical deadlock prevention |
| TEA-STREAM-002.4-UNIT-012 | Unit | P1 | Timeout value configurable via `config.options["timeout"]` | Configuration flexibility |
| TEA-STREAM-002.4-UNIT-013 | Unit | P1 | Default timeout is 30000ms (30 seconds) | Sensible defaults |
| TEA-STREAM-002.4-INT-003 | Integration | P1 | Timeout error includes clear message when server unresponsive | Debuggability |

### AC5: Works for RPC-style node communication

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.4-INT-004 | Integration | P1 | Client sends request, server processes, client receives response | Core RPC pattern |
| TEA-STREAM-002.4-INT-005 | Integration | P1 | Request/response with JSON payload (serialization round-trip) | Realistic data handling |
| TEA-STREAM-002.4-INT-006 | Integration | P2 | Server handles different request types (routing) | Practical use case |

### AC6: Error handling for protocol violations (double send/recv)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.4-UNIT-014 | Unit | P0 | REQ double `send()` raises `TransportError` with "must receive reply before sending" | Clear violation message |
| TEA-STREAM-002.4-UNIT-015 | Unit | P1 | REQ `receive()` before `send()` raises `TransportError` with "must send request before receiving" | State machine enforcement |
| TEA-STREAM-002.4-UNIT-016 | Unit | P1 | REP `send()` before `receive()` raises `TransportError` with "must receive request before sending" | State machine enforcement |

### AC7: Multiple REQ clients can connect to one REP server

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.4-INT-007 | Integration | P1 | Two REQ clients send requests to one REP server | Basic multi-client |
| TEA-STREAM-002.4-INT-008 | Integration | P1 | Three concurrent REQ clients with round-trip verification | Concurrent load |
| TEA-STREAM-002.4-INT-009 | Integration | P2 | Verify fair queuing behavior (requests served in order) | Load balancing semantics |

### AC8: Works on Windows with IPC/inproc transport

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.4-INT-010 | Integration | P1 | REQ/REP over `inproc://` transport | In-process communication |
| TEA-STREAM-002.4-INT-011 | Integration | P2 | REQ/REP over IPC on Unix, `inproc://` on Windows | Cross-platform compatibility |

### AC9: Works with TCP protocol for network RPC

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.4-INT-012 | Integration | P1 | REQ/REP over `tcp://127.0.0.1:PORT` | Network transport |

### AC10: `request()` convenience method combines send + receive

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.4-UNIT-017 | Unit | P1 | `request(data)` returns response bytes | Convenience API |
| TEA-STREAM-002.4-UNIT-018 | Unit | P2 | `request()` maintains state machine correctly | State consistency |
| TEA-STREAM-002.4-UNIT-019 | Unit | P2 | `request()` propagates timeout errors | Error handling |

---

## Risk Coverage

| Risk | Probability | Impact | Test Mitigation |
|------|-------------|--------|-----------------|
| Deadlock from infinite blocking | Medium | High | AC4 timeout tests (P0) |
| Protocol violation causing silent failures | Low | High | AC6 violation tests (P0-P1) |
| Multi-client race conditions | Medium | Medium | AC7 concurrent tests (P1) |
| Cross-platform incompatibility | Low | Medium | AC8 platform tests (P1-P2) |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core protocol issues)
   - TEA-STREAM-002.4-UNIT-001, 005-011, 014
2. **P0 Integration test**
   - TEA-STREAM-002.4-INT-001
3. **P1 Unit tests** (secondary protocol coverage)
   - TEA-STREAM-002.4-UNIT-002-003, 010, 012-013, 015-017
4. **P1 Integration tests** (RPC patterns, multi-client)
   - TEA-STREAM-002.4-INT-002-008, 010, 012
5. **P2 tests** (nice-to-have coverage)
   - Remaining tests as time permits

---

## Test Implementation Notes

### Unit Test Setup

```python
# Mock ZeroMQ context and sockets for state machine testing
@pytest.fixture
def mock_zmq_context():
    with patch("zmq.Context") as mock_ctx:
        yield mock_ctx

# Test state transitions without actual network
def test_req_state_machine(mock_zmq_context):
    transport = ZeroMQReqRepTransport(config)
    transport.connect("inproc://test")

    # Initial state allows send
    assert transport._expecting_reply == False
    transport.send(b"request")
    assert transport._expecting_reply == True
```

### Integration Test Setup

```python
# Use inproc for fast, reliable integration tests
@pytest.fixture
def reqrep_pair():
    """Create connected REQ-REP pair."""
    server = ZeroMQReqRepTransport(TransportConfig(
        address="inproc://test-reqrep",
        options={"timeout": 5000}
    ))
    server.bind("inproc://test-reqrep")

    client = ZeroMQReqRepTransport(TransportConfig(
        address="inproc://test-reqrep",
        options={"timeout": 5000}
    ))
    client.connect("inproc://test-reqrep")

    yield client, server

    client.close()
    server.close()
```

### Multi-Client Test Pattern

```python
# Use threading for concurrent client tests
def test_multi_client_concurrent(reqrep_server):
    results = []

    def client_task(client_id):
        client = create_client()
        response = client.request(f"request-{client_id}".encode())
        results.append((client_id, response))

    threads = [Thread(target=client_task, args=(i,)) for i in range(3)]
    for t in threads:
        t.start()

    # Server handles requests in separate thread
    for _ in range(3):
        req = server.receive()
        server.send(b"response")

    for t in threads:
        t.join()

    assert len(results) == 3
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (`TEA-STREAM-002.4-{LEVEL}-{SEQ}`)
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 28
  by_level:
    unit: 16
    integration: 12
    e2e: 0
  by_priority:
    p0: 8
    p1: 12
    p2: 8
  coverage_gaps: []
  key_risks_addressed:
    - Deadlock prevention via timeout (P0)
    - Protocol violation handling (P0)
    - Multi-client support (P1)
    - Cross-platform compatibility (P1-P2)
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-STREAM-002.4-test-design-20260108.md
P0 tests identified: 8
P1 tests identified: 12
P2 tests identified: 8
Total scenarios: 28
```
