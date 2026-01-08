# Test Design: Story TEA-STREAM-002.3

**Date:** 2026-01-08
**Designer:** Quinn (Test Architect)
**Story:** TEA-STREAM-002.3 - PUSH/PULL Pattern Implementation
**Epic:** TEA-STREAM-002 - ZeroMQ Transport

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 34 |
| Unit tests | 18 (53%) |
| Integration tests | 14 (41%) |
| E2E tests | 2 (6%) |
| Priority distribution | P0: 12, P1: 14, P2: 6, P3: 2 |

### Risk Profile

| Risk Factor | Assessment |
|-------------|------------|
| **Data integrity (exactly-once)** | HIGH - Message loss/duplication breaks workflows |
| **Load balancing correctness** | HIGH - Core feature promise |
| **Cross-platform compatibility** | MEDIUM - Windows IPC limitations |
| **Worker disconnect handling** | MEDIUM - Edge case in production |
| **Topology flexibility** | LOW - Clear patterns from ZMQ |

### Test Level Distribution Rationale

- **Unit (53%)**: Transport interface compliance, socket configuration, error handling
- **Integration (41%)**: Multi-worker scenarios, round-robin verification, topology patterns
- **E2E (6%)**: Critical path validation with real workflow orchestration

---

## Test Scenarios by Acceptance Criteria

### AC1: `ZeroMQPushPullTransport` implements `Transport` interface

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-STREAM-002.3-UNIT-001 | Unit | P0 | Transport ABC method compliance | Core contract validation |
| TEA-STREAM-002.3-UNIT-002 | Unit | P0 | Constructor accepts TransportConfig | Factory integration |
| TEA-STREAM-002.3-UNIT-003 | Unit | P1 | `__init__` sets role from config options | Configuration flexibility |

**Given-When-Then:**

```gherkin
# TEA-STREAM-002.3-UNIT-001
Given a ZeroMQPushPullTransport instance
When checking interface methods
Then bind(), connect(), send(), receive(), close() are all defined
And is_connected property exists

# TEA-STREAM-002.3-UNIT-002
Given a TransportConfig with pattern="push_pull"
When creating ZeroMQPushPullTransport(config)
Then transport is created without error
And transport.config equals provided config
```

---

### AC2: PUSH socket distributes messages round-robin to PULL sockets

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-STREAM-002.3-INT-001 | Integration | P0 | Round-robin with 3 workers, 300 messages | Core load balancing verification |
| TEA-STREAM-002.3-INT-002 | Integration | P1 | Round-robin with 2 workers, uneven message count | Edge case distribution |
| TEA-STREAM-002.3-INT-003 | Integration | P1 | Distribution variance within acceptable bounds (+-10%) | Statistical validation |

**Given-When-Then:**

```gherkin
# TEA-STREAM-002.3-INT-001
Given 1 PUSH socket bound to inproc://test-roundrobin
And 3 PULL sockets connected to the same address
When 300 messages are sent via PUSH socket
Then each PULL socket receives approximately 100 messages (90-110 range)
And total messages received equals 300

# TEA-STREAM-002.3-INT-003
Given 4 workers and 1000 messages
When messages are distributed
Then standard deviation of worker counts is < 5% of mean
```

---

### AC3: Each message is received by exactly one worker (no duplicates)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-STREAM-002.3-INT-004 | Integration | P0 | Message ID tracking - no duplicates | Data integrity critical |
| TEA-STREAM-002.3-INT-005 | Integration | P0 | Total received equals total sent | Completeness validation |
| TEA-STREAM-002.3-UNIT-004 | Unit | P1 | Single puller receives exactly what pusher sends | Basic correctness |

**Given-When-Then:**

```gherkin
# TEA-STREAM-002.3-INT-004
Given 3 PULL workers collecting messages with unique IDs
When PUSH sends messages task-0 through task-299
Then set of all received message IDs has exactly 300 unique entries
And no message ID appears more than once across all workers

# TEA-STREAM-002.3-INT-005
Given message counter on PUSH side
And message counters on each PULL side
When all messages are transmitted
Then sum(PULL counters) == PUSH counter
```

---

### AC4: Fair queuing when workers have different processing speeds

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-STREAM-002.3-INT-006 | Integration | P0 | Slow worker (100ms delay) vs fast workers | Real-world scenario |
| TEA-STREAM-002.3-INT-007 | Integration | P1 | Very slow worker doesn't starve fast workers | Back-pressure behavior |
| TEA-STREAM-002.3-INT-008 | Integration | P2 | Worker speed variance doesn't cause message loss | Reliability under stress |

**Given-When-Then:**

```gherkin
# TEA-STREAM-002.3-INT-006
Given 3 workers: Worker1 (no delay), Worker2 (50ms), Worker3 (100ms)
When 60 messages are sent over 3 seconds
Then Worker1 receives more messages than Worker3
And all messages are delivered (no loss)

# TEA-STREAM-002.3-INT-007
Given 1 worker with 500ms delay and 2 workers with no delay
When 100 messages are sent
Then fast workers receive majority of messages
And slow worker still receives some messages (not starved)
```

---

### AC5: Works with 1:N topology (one pusher, many pullers)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-STREAM-002.3-INT-009 | Integration | P0 | Ventilator pattern: 1 PUSH bind, 3 PULL connect | Primary use case |
| TEA-STREAM-002.3-UNIT-005 | Unit | P1 | bind() creates PUSH socket when role=distributor | Socket type verification |
| TEA-STREAM-002.3-UNIT-006 | Unit | P1 | connect() creates PULL socket when role=distributor | Socket type verification |

**Given-When-Then:**

```gherkin
# TEA-STREAM-002.3-INT-009
Given PUSH transport with role="distributor"
And 5 PULL transports with role="distributor"
When PUSH binds to tcp://127.0.0.1:5555
And all PULLs connect to tcp://127.0.0.1:5555
And 100 messages are sent
Then all messages are distributed among 5 workers
And each worker receives approximately 20 messages
```

---

### AC6: Works with N:1 topology (many pushers, one puller)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-STREAM-002.3-INT-010 | Integration | P0 | Sink pattern: 3 PUSH connect, 1 PULL bind | Result collection use case |
| TEA-STREAM-002.3-UNIT-007 | Unit | P1 | bind() creates PULL socket when role=sink | Socket type verification |
| TEA-STREAM-002.3-UNIT-008 | Unit | P1 | connect() creates PUSH socket when role=sink | Socket type verification |

**Given-When-Then:**

```gherkin
# TEA-STREAM-002.3-INT-010
Given PULL transport with role="sink" bound to ipc:///tmp/tea-sink.sock
And 3 PUSH transports with role="sink" connected to the address
When each PUSH sends 50 messages (150 total)
Then PULL sink receives all 150 messages
And message ordering within each producer is preserved
```

---

### AC7: `high_water_mark` prevents slow worker blocking fast producer

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-STREAM-002.3-UNIT-009 | Unit | P0 | HWM configures SNDHWM socket option | Core back-pressure config |
| TEA-STREAM-002.3-UNIT-010 | Unit | P0 | HWM configures RCVHWM socket option | Symmetric configuration |
| TEA-STREAM-002.3-UNIT-011 | Unit | P1 | Default HWM is 1000 | Sensible default |
| TEA-STREAM-002.3-INT-011 | Integration | P1 | Producer continues sending when worker blocked (up to HWM) | Non-blocking behavior |

**Given-When-Then:**

```gherkin
# TEA-STREAM-002.3-UNIT-009
Given TransportConfig with options={"high_water_mark": 500}
When ZeroMQPushPullTransport is created and socket configured
Then socket.getsockopt(zmq.SNDHWM) == 500

# TEA-STREAM-002.3-INT-011
Given HWM=10 and one worker with 1s delay
When producer rapidly sends 20 messages
Then first 10 messages are queued (up to HWM)
And producer blocks or receives EAGAIN after HWM exceeded
```

---

### AC8: Works on Windows with IPC/inproc transport

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-STREAM-002.3-INT-012 | Integration | P1 | inproc:// protocol on Windows | Windows CI requirement |
| TEA-STREAM-002.3-UNIT-012 | Unit | P2 | Windows platform detection for protocol selection | Platform abstraction |

**Given-When-Then:**

```gherkin
# TEA-STREAM-002.3-INT-012
Given Windows platform (or inproc:// on any platform)
When PUSH binds to inproc://tea-pushpull-test
And PULL connects to same address
And 10 messages are sent
Then all 10 messages are received correctly
```

---

### AC9: Works with TCP protocol for network distribution

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-STREAM-002.3-INT-013 | Integration | P0 | TCP protocol with localhost | Network transport |
| TEA-STREAM-002.3-INT-014 | Integration | P2 | TCP with ephemeral port binding | Port flexibility |
| TEA-STREAM-002.3-E2E-001 | E2E | P1 | Distributed workers via TCP in workflow | Real-world validation |

**Given-When-Then:**

```gherkin
# TEA-STREAM-002.3-INT-013
Given PUSH bound to tcp://127.0.0.1:5556
And 2 PULLs connected to tcp://127.0.0.1:5556
When 50 messages are sent
Then all messages are received across both workers

# TEA-STREAM-002.3-E2E-001
Given a YAML workflow with zeromq push_pull transport
And parallel workers configured via TCP
When workflow executes with distributed work items
Then all items are processed exactly once
```

---

### AC10: Graceful handling when all workers disconnect

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-STREAM-002.3-UNIT-013 | Unit | P0 | send() raises TransportError on zmq.Again (no workers) | Error handling |
| TEA-STREAM-002.3-UNIT-014 | Unit | P1 | Send timeout option configurable | User control |
| TEA-STREAM-002.3-UNIT-015 | Unit | P1 | Warning logged when send blocks | Observability |
| TEA-STREAM-002.3-UNIT-016 | Unit | P2 | immediate option drops messages when no peers | ZMQ feature support |
| TEA-STREAM-002.3-E2E-002 | E2E | P2 | Workflow handles worker crash gracefully | Production resilience |

**Given-When-Then:**

```gherkin
# TEA-STREAM-002.3-UNIT-013
Given PUSH socket with send_timeout=100ms
And no PULL workers connected
When send() is called
Then TransportError is raised with message "no workers available"

# TEA-STREAM-002.3-UNIT-014
Given TransportConfig with options={"send_timeout": 500}
When socket is configured
Then socket.getsockopt(zmq.SNDTIMEO) == 500
```

---

## Additional Test Scenarios

### TransportFactory Integration

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-STREAM-002.3-UNIT-017 | Unit | P0 | Factory dispatches to ZeroMQPushPullTransport for pattern=push_pull | Factory integration |
| TEA-STREAM-002.3-UNIT-018 | Unit | P2 | stream_mode=load_balance aliases to push_pull pattern | User convenience |

**Given-When-Then:**

```gherkin
# TEA-STREAM-002.3-UNIT-017
Given TransportConfig(transport_type="zeromq", pattern="push_pull")
When TransportFactory.create(config)
Then instance of ZeroMQPushPullTransport is returned
```

---

## Risk Coverage Matrix

| Risk | Severity | Mitigating Tests |
|------|----------|------------------|
| Message duplication | HIGH | INT-004, INT-005 |
| Message loss | HIGH | INT-005, INT-006 |
| Uneven distribution | HIGH | INT-001, INT-003 |
| Worker starvation | MEDIUM | INT-007 |
| Resource leak | MEDIUM | UNIT-001 (close() compliance) |
| Platform incompatibility | MEDIUM | INT-012, INT-013 |
| Timeout misconfiguration | LOW | UNIT-014, UNIT-016 |
| Factory routing error | LOW | UNIT-017 |

---

## Test Implementation Notes

### Test File Location

- **Unit tests**: `python/tests/test_zeromq_pushpull.py`
- **Integration tests**: `python/tests/test_zeromq_integration.py` (extend existing)
- **E2E tests**: `python/tests/test_zeromq_e2e.py`

### Test Dependencies

```python
import pytest
import zmq
from multiprocessing import Process, Queue
from the_edge_agent.transports.zeromq.pushpull import ZeroMQPushPullTransport
from the_edge_agent.transports.base import TransportConfig, TransportError
from the_edge_agent.transports.factory import create_transport

# Skip if pyzmq not installed
zmq = pytest.importorskip("zmq")
```

### Recommended Fixtures

```python
@pytest.fixture
def pushpull_config():
    return TransportConfig(
        transport_type="zeromq",
        pattern="push_pull",
        options={"high_water_mark": 100}
    )

@pytest.fixture
def inproc_address():
    return f"inproc://tea-test-{uuid.uuid4().hex[:8]}"

@pytest.fixture
def tcp_address():
    return "tcp://127.0.0.1:5557"
```

### Multi-Process Testing Pattern

```python
def worker_process(address: str, result_queue: Queue, delay_ms: int = 0):
    """Worker that pulls messages and reports count."""
    transport = ZeroMQPushPullTransport(TransportConfig(pattern="push_pull"))
    transport.connect(address)
    count = 0
    messages = []
    while True:
        try:
            msg = transport.receive(flags=zmq.NOBLOCK)
            messages.append(msg)
            count += 1
            if delay_ms:
                time.sleep(delay_ms / 1000)
        except zmq.Again:
            break
    transport.close()
    result_queue.put({"count": count, "messages": messages})
```

---

## Recommended Execution Order

1. **P0 Unit tests** - Interface compliance, socket configuration (fail fast)
2. **P0 Integration tests** - Round-robin, exactly-once delivery
3. **P1 Unit tests** - Error handling, timeout configuration
4. **P1 Integration tests** - Topology patterns, fair queuing
5. **P1 E2E tests** - Workflow integration
6. **P2+ tests** - Platform-specific, edge cases

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for config, integration for multi-worker)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (data integrity = P0)
- [x] Test IDs follow naming convention (TEA-STREAM-002.3-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Multi-process tests have proper cleanup

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-STREAM-002.3
  date: 2026-01-08
  scenarios_total: 34
  by_level:
    unit: 18
    integration: 14
    e2e: 2
  by_priority:
    p0: 12
    p1: 14
    p2: 6
    p3: 2
  coverage_gaps: []
  critical_tests:
    - TEA-STREAM-002.3-INT-001  # Round-robin distribution
    - TEA-STREAM-002.3-INT-004  # No message duplicates
    - TEA-STREAM-002.3-INT-005  # Total received = total sent
    - TEA-STREAM-002.3-INT-006  # Fair queuing with slow workers
    - TEA-STREAM-002.3-INT-009  # 1:N ventilator topology
    - TEA-STREAM-002.3-INT-010  # N:1 sink topology
    - TEA-STREAM-002.3-UNIT-009 # HWM configuration
    - TEA-STREAM-002.3-UNIT-013 # No-worker error handling
  risk_areas:
    - id: DATA_INTEGRITY
      severity: HIGH
      tests: [INT-004, INT-005]
    - id: LOAD_BALANCING
      severity: HIGH
      tests: [INT-001, INT-003, INT-006]
    - id: CROSS_PLATFORM
      severity: MEDIUM
      tests: [INT-012, INT-013]
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-STREAM-002.3-test-design-20260108.md
P0 tests identified: 12
Story dependency: TEA-STREAM-002.1 (Transport Abstraction), TEA-STREAM-002.2 (PUB/SUB)
Test file locations:
  - python/tests/test_zeromq_pushpull.py (unit)
  - python/tests/test_zeromq_integration.py (integration)
  - python/tests/test_zeromq_e2e.py (e2e)
```
