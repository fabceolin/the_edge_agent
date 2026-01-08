# Test Design: Story TEA-STREAM-002.2

**Date:** 2026-01-08
**Designer:** Quinn (Test Architect)
**Story:** PUB/SUB Pattern Implementation
**Slug:** pubsub-pattern

---

## Test Strategy Overview

- **Total test scenarios:** 25
- **Unit tests:** 12 (48%)
- **Integration tests:** 10 (40%)
- **E2E tests:** 3 (12%)
- **Priority distribution:** P0: 8, P1: 10, P2: 5, P3: 2

### Risk Context

This story implements the ZeroMQ PUB/SUB pattern, which is:
- **Core transport functionality** - Foundation for broadcast streaming
- **Cross-platform critical** - Must work on Linux, macOS, and Windows
- **Resource management sensitive** - Socket/context leaks can cause memory exhaustion
- **Protocol-specific** - Topic filtering and HWM behavior are ZeroMQ semantics

---

## Test Scenarios by Acceptance Criterion

### AC1: `ZeroMQPubSubTransport` implements `Transport` interface

**Requirement:** Transport class must implement all abstract methods from base class.

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| TEA-STREAM-002.2-UNIT-001 | Unit | P0 | **Verify ZeroMQPubSubTransport inherits from Transport** | Interface compliance is foundational |
| TEA-STREAM-002.2-UNIT-002 | Unit | P0 | **Verify all abstract methods implemented (bind, connect, send, receive, close, is_connected)** | Must satisfy contract |
| TEA-STREAM-002.2-UNIT-003 | Unit | P1 | **Verify constructor accepts TransportConfig with pattern='pub_sub'** | Configuration integration |
| TEA-STREAM-002.2-UNIT-004 | Unit | P1 | **Verify TransportFactory dispatches to ZeroMQPubSubTransport for zeromq+pub_sub** | Factory routing |

---

### AC2: Publisher calls `bind()`, subscribers call `connect()`

**Requirement:** Role differentiation via bind/connect pattern.

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| TEA-STREAM-002.2-UNIT-005 | Unit | P0 | **Verify bind() creates zmq.PUB socket** | Publisher role established correctly |
| TEA-STREAM-002.2-UNIT-006 | Unit | P0 | **Verify connect() creates zmq.SUB socket** | Subscriber role established correctly |
| TEA-STREAM-002.2-UNIT-007 | Unit | P1 | **Verify bind() raises TransportError on invalid address** | Input validation |
| TEA-STREAM-002.2-UNIT-008 | Unit | P1 | **Verify connect() raises TransportError on invalid address** | Input validation |
| TEA-STREAM-002.2-INT-001 | Integration | P1 | **Verify bind() then connect() establishes connection** | Real socket connection works |

---

### AC3: All subscribers receive all published messages (fan-out)

**Requirement:** PUB/SUB semantics guarantee all subscribers get all messages.

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| TEA-STREAM-002.2-INT-002 | Integration | P0 | **Verify 1 publisher, 1 subscriber: all 100 messages received** | Basic fan-out |
| TEA-STREAM-002.2-INT-003 | Integration | P0 | **Verify 1 publisher, 3 subscribers: all 3 receive all 100 messages** | Multi-subscriber fan-out |
| TEA-STREAM-002.2-INT-004 | Integration | P1 | **Verify message ordering preserved per subscriber** | ZeroMQ ordering guarantee |
| TEA-STREAM-002.2-INT-005 | Integration | P1 | **Verify late-joining subscriber receives only new messages** | PUB/SUB subscription semantics |
| TEA-STREAM-002.2-E2E-001 | E2E | P0 | **Verify fan-out with 5 subscribers across 5 processes** | Real process isolation |

---

### AC4: Topic filtering with `subscribe_filter` option

**Requirement:** Subscribers can filter by topic prefix.

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| TEA-STREAM-002.2-UNIT-009 | Unit | P1 | **Verify subscribe_filter=b'' subscribes to all topics** | Default behavior |
| TEA-STREAM-002.2-UNIT-010 | Unit | P1 | **Verify subscribe_filter=b'topic_a' filters to matching prefix** | Topic filtering works |
| TEA-STREAM-002.2-INT-006 | Integration | P1 | **Verify subscriber with filter='metrics' receives only metrics messages** | End-to-end filtering |
| TEA-STREAM-002.2-INT-007 | Integration | P2 | **Verify subscriber can change filter after initial connect** | Dynamic resubscription |

---

### AC5: `high_water_mark` option prevents memory exhaustion

**Requirement:** Queue depth limits configurable via HWM.

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| TEA-STREAM-002.2-UNIT-011 | Unit | P0 | **Verify SNDHWM configured from options['high_water_mark'] on PUB** | HWM applied to publisher |
| TEA-STREAM-002.2-UNIT-012 | Unit | P0 | **Verify RCVHWM configured from options['high_water_mark'] on SUB** | HWM applied to subscriber |
| TEA-STREAM-002.2-INT-008 | Integration | P2 | **Verify messages dropped when subscriber HWM reached (slow consumer)** | Backpressure behavior |

---

### AC6: Works with IPC protocol on Linux/macOS

**Requirement:** Local IPC transport functional on Unix-like systems.

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| TEA-STREAM-002.2-INT-009 | Integration | P0 | **Verify IPC transport (ipc:///tmp/tea-test.sock) on Linux** | Linux IPC works |
| TEA-STREAM-002.2-E2E-002 | E2E | P1 | **Verify full workflow with IPC on macOS** | macOS IPC works |

**Platform Note:** Skip on Windows (covered by AC7).

---

### AC7: Works with IPC protocol on Windows (using `inproc://`)

**Requirement:** Windows compatibility via inproc protocol.

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| TEA-STREAM-002.2-INT-010 | Integration | P0 | **Verify inproc transport (inproc://tea-test) on Windows** | Windows compatibility |

**Platform Note:** Skip on non-Windows.

---

### AC8: Works with TCP protocol for network streaming

**Requirement:** TCP transport enables cross-machine communication.

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| TEA-STREAM-002.2-E2E-003 | E2E | P1 | **Verify TCP transport (tcp://127.0.0.1:5555) pub/sub** | TCP works end-to-end |

---

### AC9: `linger` option controls close behavior

**Requirement:** Socket linger timeout configurable.

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| TEA-STREAM-002.2-UNIT-013 | Unit | P2 | **Verify LINGER configured from options['linger']** | Linger applied |
| TEA-STREAM-002.2-UNIT-014 | Unit | P3 | **Verify default linger is 1000ms when not specified** | Sensible default |

---

### AC10: Slow subscriber warning when HWM reached

**Requirement:** Diagnostic warning for backpressure conditions.

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| TEA-STREAM-002.2-UNIT-015 | Unit | P2 | **Verify warning logged when HWM threshold reached** | Observability |
| TEA-STREAM-002.2-INT-011 | Integration | P3 | **Verify get_stats() returns queue depth metrics** | Monitoring integration |

---

## Risk Coverage Matrix

| Risk ID | Risk Description | Mitigating Tests |
|---------|------------------|------------------|
| R1 | pyzmq installation issues | All tests use `pytest.importorskip("zmq")` |
| R5 | Resource leaks on error | TEA-STREAM-002.2-UNIT-016 (resource cleanup on exception) |
| R6 | Windows IPC path issues | TEA-STREAM-002.2-INT-010 (inproc on Windows) |
| R4 | Message ordering issues | TEA-STREAM-002.2-INT-004 (ordering preserved) |
| R7 | High-water mark confusion | TEA-STREAM-002.2-UNIT-011/012 (HWM configuration) |
| NEW | Context lifecycle management | TEA-STREAM-002.2-UNIT-017 (singleton context) |
| NEW | TCP port conflicts | Use dynamic port allocation via `socket.bind(('', 0))` |

### Additional Risk-Mitigation Tests

| ID | Level | Priority | Test | Risk |
|---|---|---|---|---|
| TEA-STREAM-002.2-UNIT-016 | Unit | P0 | **Verify socket closed on exception during send/receive** | R5 - Resource leaks |
| TEA-STREAM-002.2-UNIT-017 | Unit | P1 | **Verify singleton context shared across instances** | Context management |

---

## Test Implementation Notes

### Pytest Fixtures Required

```python
@pytest.fixture
def zmq_context():
    """Fresh ZeroMQ context for test isolation."""
    ctx = zmq.Context()
    yield ctx
    ctx.term()

@pytest.fixture
def dynamic_tcp_port():
    """Allocate available TCP port."""
    import socket
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.bind(('', 0))
    port = s.getsockname()[1]
    s.close()
    return port

@pytest.fixture
def pubsub_transport_config():
    """Default PUB/SUB configuration."""
    return TransportConfig(
        transport_type="zeromq",
        protocol="ipc",
        pattern="pub_sub",
        options={"high_water_mark": 1000, "linger": 1000}
    )
```

### Platform Skip Markers

```python
import sys

is_windows = sys.platform == "win32"
is_macos = sys.platform == "darwin"
is_linux = sys.platform.startswith("linux")

skip_on_windows = pytest.mark.skipif(is_windows, reason="IPC not supported on Windows")
skip_unless_windows = pytest.mark.skipif(not is_windows, reason="Windows-only test")
```

### Test Organization

```
python/tests/
├── test_zeromq_pubsub.py           # Unit tests (UNIT-001 to UNIT-017)
├── test_zeromq_pubsub_integration.py # Integration tests (INT-001 to INT-011)
└── test_zeromq_pubsub_e2e.py       # E2E tests (E2E-001 to E2E-003)
```

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on interface/socket issues)
   - TEA-STREAM-002.2-UNIT-001, 002, 005, 006, 011, 012, 016
2. **P0 Integration tests** (verify real socket behavior)
   - TEA-STREAM-002.2-INT-002, 003, 009, 010
3. **P0 E2E tests** (verify process-level isolation)
   - TEA-STREAM-002.2-E2E-001
4. **P1 tests** (core functionality depth)
5. **P2+ tests** (as time permits)

---

## Coverage Gaps

| Gap | Severity | Recommendation |
|-----|----------|----------------|
| No stress test for high message rate | Low | Add P3 test for 10K msg/s throughput (deferred to Story 6) |
| No network partition simulation | Low | Consider chaos testing in future epic |
| No multicast test | None | Out of scope for this pattern |

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for sockets, e2e for processes)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (P0 for core functionality)
- [x] Test IDs follow naming convention `{story}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Risk mitigations addressed

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-STREAM-002.2
  story_slug: pubsub-pattern
  date: "2026-01-08"
  scenarios_total: 25
  by_level:
    unit: 12
    integration: 10
    e2e: 3
  by_priority:
    p0: 8
    p1: 10
    p2: 5
    p3: 2
  coverage_gaps:
    - high_message_rate_stress_test (deferred to Story 6)
  risk_coverage:
    - R1: pyzmq installation (import guards)
    - R4: message ordering (INT-004)
    - R5: resource leaks (UNIT-016)
    - R6: Windows IPC (INT-010)
    - R7: HWM confusion (UNIT-011/012)
  test_files:
    - python/tests/test_zeromq_pubsub.py
    - python/tests/test_zeromq_pubsub_integration.py
    - python/tests/test_zeromq_pubsub_e2e.py
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-STREAM-002.2-test-design-20260108.md
P0 tests identified: 8
P1 tests identified: 10
Total scenarios: 25
```
