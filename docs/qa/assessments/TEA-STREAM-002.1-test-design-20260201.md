# Test Design: Story TEA-STREAM-002.1 - Transport Abstraction Layer

Date: 2026-02-01
Designer: Quinn (Test Architect)
Mode: YOLO (Full Execution)

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 34 |
| Unit tests | 24 (71%) |
| Integration tests | 10 (29%) |
| E2E tests | 0 (0%) - Infrastructure code, no user-facing journeys |
| Priority distribution | P0: 14, P1: 13, P2: 7 |

### Test Level Rationale

This story implements **infrastructure code** (transport abstraction layer). Per the Test Levels Framework:
- **Unit tests dominate** (71%): Pure logic, ABC interface validation, state management
- **Integration tests** (29%): Real pipe operations, StreamRegistry integration, backward compatibility
- **No E2E tests**: No user-facing journeys; this is internal plumbing

## Test Scenarios by Acceptance Criteria

### AC1: Transport Base Class Interface

**Criterion:** `Transport` base class defines `bind()`, `connect()`, `send()`, `receive()`, `close()` interface

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| TEA-STREAM-002.1-UNIT-001 | Unit | P0 | Verify Transport ABC has all 5 abstract methods | ABC completeness; foundation for all transports | TECH-001 |
| TEA-STREAM-002.1-UNIT-002 | Unit | P0 | Verify concrete class must implement all methods | Fails if method missing; prevents runtime errors | TECH-001 |
| TEA-STREAM-002.1-UNIT-003 | Unit | P1 | Verify ABC cannot be instantiated directly | Type safety; prevents misuse | - |

**Given-When-Then:**
```gherkin
# UNIT-001
Given the Transport ABC is defined
When I inspect its abstract methods
Then it contains bind(), connect(), send(), receive(), close()

# UNIT-002
Given a class inherits Transport
When it does not implement all abstract methods
Then instantiation raises TypeError

# UNIT-003
Given the Transport ABC
When I attempt to instantiate it directly
Then TypeError is raised
```

---

### AC2: TransportConfig Dataclass

**Criterion:** `TransportConfig` dataclass holds transport type, protocol, pattern, and options

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| TEA-STREAM-002.1-UNIT-004 | Unit | P0 | Verify TransportConfig has all required fields | Data structure correctness | - |
| TEA-STREAM-002.1-UNIT-005 | Unit | P1 | Verify default values (transport_type="unix", protocol="ipc", pattern="direct") | Configuration convenience | - |
| TEA-STREAM-002.1-UNIT-006 | Unit | P0 | Verify options dict uses default_factory (no shared mutable state) | **Critical**: Prevents cross-instance contamination | TECH-004 |

**Given-When-Then:**
```gherkin
# UNIT-004
Given TransportConfig dataclass
When I create an instance with all fields
Then all fields are accessible with correct values

# UNIT-005
Given TransportConfig dataclass
When I create an instance with no arguments
Then transport_type="unix", protocol="ipc", pattern="direct", options={}

# UNIT-006
Given two TransportConfig instances with no options
When I modify options on instance A
Then instance B.options remains empty
```

---

### AC3: UnixPipeTransport Implementation

**Criterion:** `UnixPipeTransport` implements `Transport` interface using patterns from `StreamChannel` (reimplements pipe logic, does not delegate)

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| TEA-STREAM-002.1-UNIT-007 | Unit | P0 | bind() creates file descriptors via os.pipe() | Core functionality | - |
| TEA-STREAM-002.1-UNIT-008 | Unit | P0 | connect() opens read-side file descriptor | Core functionality | - |
| TEA-STREAM-002.1-UNIT-009 | Unit | P0 | send() writes bytes via os.write() | Core functionality | - |
| TEA-STREAM-002.1-UNIT-010 | Unit | P0 | receive() reads bytes via os.read() | Core functionality | - |
| TEA-STREAM-002.1-UNIT-011 | Unit | P1 | close() releases file descriptors | Resource cleanup | OPS-001 |
| TEA-STREAM-002.1-INT-001 | Integration | P0 | Full send/receive cycle with real Unix pipe | End-to-end data flow | - |
| TEA-STREAM-002.1-INT-002 | Integration | P1 | Multi-message streaming through pipe | Data integrity over time | - |
| TEA-STREAM-002.1-INT-003 | Integration | P0 | Verify NO delegation to StreamChannel (code inspection) | **AC3 compliance** | TECH-003 |

**Given-When-Then:**
```gherkin
# UNIT-007
Given an UnixPipeTransport instance
When I call bind(address)
Then a pipe is created and write-side fd is held

# UNIT-008
Given an UnixPipeTransport instance
When I call connect(address)
Then read-side fd is opened

# UNIT-009
Given a bound UnixPipeTransport
When I call send(b"hello")
Then data is written to write-side fd

# UNIT-010
Given a connected UnixPipeTransport
When I call receive()
Then data is read from read-side fd

# INT-001
Given producer and consumer UnixPipeTransport instances
When producer sends b"test data"
Then consumer receives b"test data"

# INT-002
Given producer and consumer connected
When producer sends 100 sequential messages
Then consumer receives all 100 in order

# INT-003
Given the UnixPipeTransport source code
When I grep for StreamChannel import/usage
Then no references exist (reimplements, does not delegate)
```

---

### AC4: TransportFactory Dispatch

**Criterion:** `TransportFactory.create()` returns appropriate transport based on config

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| TEA-STREAM-002.1-UNIT-012 | Unit | P0 | Factory returns UnixPipeTransport for transport_type="unix" | Primary dispatch | - |
| TEA-STREAM-002.1-UNIT-013 | Unit | P0 | Factory raises TransportError for unknown transport type | Fail-fast for invalid config | - |
| TEA-STREAM-002.1-UNIT-014 | Unit | P1 | Factory is extensible (can register new transport types) | Future ZeroMQ support | - |

**Given-When-Then:**
```gherkin
# UNIT-012
Given TransportConfig(transport_type="unix")
When I call TransportFactory.create(config)
Then an UnixPipeTransport instance is returned

# UNIT-013
Given TransportConfig(transport_type="invalid")
When I call TransportFactory.create(config)
Then TransportError is raised with message containing "unknown transport type"

# UNIT-014
Given TransportFactory
When I register a new transport type "custom"
Then create(TransportConfig(transport_type="custom")) returns that transport
```

---

### AC5: Backward Compatibility

**Criterion:** Existing Unix pipe workflows work unchanged when `transport: unix` (default)

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| TEA-STREAM-002.1-INT-004 | Integration | P0 | **ALL existing test_streams.py tests pass unchanged** | **Regression prevention - NON-NEGOTIABLE** | BUS-001 |
| TEA-STREAM-002.1-INT-005 | Integration | P0 | StreamRegistry with no transport config uses Unix pipes | Backward compatible default | BUS-001 |
| TEA-STREAM-002.1-INT-006 | Integration | P1 | StreamRegistry with explicit transport="unix" works | Explicit config path | - |

**Given-When-Then:**
```gherkin
# INT-004
Given the existing test_streams.py test suite
When I run pytest tests/test_streams.py
Then all tests pass (zero failures, zero errors)

# INT-005
Given StreamRegistry initialized with no transport config
When I create a channel and send data
Then data flows through Unix pipes (default behavior preserved)

# INT-006
Given StreamRegistry initialized with transport_factory for "unix"
When I create a channel and send data
Then data flows through Unix pipes
```

---

### AC6: StreamRegistry Integration

**Criterion:** `StreamRegistry` accepts optional `transport_factory` parameter

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| TEA-STREAM-002.1-UNIT-015 | Unit | P1 | StreamRegistry accepts transport_factory parameter | Dependency injection | - |
| TEA-STREAM-002.1-UNIT-016 | Unit | P1 | StreamRegistry uses default factory when not specified | Backward compatibility | - |
| TEA-STREAM-002.1-INT-007 | Integration | P1 | Custom transport factory is invoked for channel creation | Factory integration verified | - |

**Given-When-Then:**
```gherkin
# UNIT-015
Given StreamRegistry class
When I instantiate with transport_factory=mock_factory
Then the instance holds reference to mock_factory

# UNIT-016
Given StreamRegistry class
When I instantiate with no transport_factory
Then default UnixPipeTransport factory is used

# INT-007
Given StreamRegistry with custom mock factory
When I create a new channel
Then mock factory.create() is called
```

---

### AC7: TransportError Exception

**Criterion:** Transport errors raise `TransportError` with helpful messages

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| TEA-STREAM-002.1-UNIT-017 | Unit | P0 | TransportError is raised on send failure | Error propagation | - |
| TEA-STREAM-002.1-UNIT-018 | Unit | P0 | TransportError message includes operation context | Developer experience | - |
| TEA-STREAM-002.1-UNIT-019 | Unit | P1 | TransportError wraps underlying OSError with cause chain | Debugging aid | - |

**Given-When-Then:**
```gherkin
# UNIT-017
Given an UnixPipeTransport with closed write fd
When I call send(data)
Then TransportError is raised

# UNIT-018
Given a TransportError for failed send
When I inspect error.message
Then it contains "send", "address", and "reason"

# UNIT-019
Given an OSError during pipe operation
When TransportError is raised
Then __cause__ is set to original OSError
```

---

### AC8: is_connected Property

**Criterion:** `is_connected` property reflects connection state accurately

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| TEA-STREAM-002.1-UNIT-020 | Unit | P0 | is_connected is False before bind/connect | Initial state correctness | DATA-001 |
| TEA-STREAM-002.1-UNIT-021 | Unit | P0 | is_connected is True after successful bind | State tracking | DATA-001 |
| TEA-STREAM-002.1-UNIT-022 | Unit | P0 | is_connected is True after successful connect | State tracking | DATA-001 |
| TEA-STREAM-002.1-UNIT-023 | Unit | P0 | is_connected is False after close | Cleanup state | DATA-001, OPS-001 |

**Given-When-Then:**
```gherkin
# UNIT-020
Given a new UnixPipeTransport instance
When I check is_connected
Then it returns False

# UNIT-021
Given an UnixPipeTransport instance
When I call bind(address) successfully
Then is_connected returns True

# UNIT-022
Given an UnixPipeTransport instance
When I call connect(address) successfully
Then is_connected returns True

# UNIT-023
Given a connected UnixPipeTransport
When I call close()
Then is_connected returns False
```

---

### AC9: Context Manager Support

**Criterion:** Context manager support (`with transport:`) for automatic cleanup

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| TEA-STREAM-002.1-UNIT-024 | Unit | P0 | __enter__ returns self | Context manager protocol | OPS-001 |
| TEA-STREAM-002.1-UNIT-025 | Unit | P0 | __exit__ calls close() | Automatic cleanup | OPS-001 |
| TEA-STREAM-002.1-UNIT-026 | Unit | P0 | __exit__ calls close() even on exception | Exception safety | OPS-001 |

**Given-When-Then:**
```gherkin
# UNIT-024
Given an UnixPipeTransport instance
When I use `with transport as t:`
Then t is the same object as transport

# UNIT-025
Given an UnixPipeTransport in `with` block
When the block exits normally
Then close() has been called

# UNIT-026
Given an UnixPipeTransport in `with` block
When an exception is raised inside the block
Then close() is still called before exception propagates
```

---

### AC10: Graceful Pending Operations Handling

**Criterion:** Graceful handling when transport is closed while operations pending

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| TEA-STREAM-002.1-UNIT-027 | Unit | P0 | close() is idempotent (call twice, no error) | Safe cleanup | OPS-001 |
| TEA-STREAM-002.1-UNIT-028 | Unit | P1 | send() on closed transport raises TransportError | Operation safety | - |
| TEA-STREAM-002.1-UNIT-029 | Unit | P1 | receive() on closed transport raises TransportError | Operation safety | - |
| TEA-STREAM-002.1-INT-008 | Integration | P1 | BrokenPipeError handled gracefully | Producer/consumer disconnect | - |

**Given-When-Then:**
```gherkin
# UNIT-027
Given a transport that has been closed
When I call close() again
Then no exception is raised

# UNIT-028
Given a closed transport
When I call send(data)
Then TransportError is raised with "closed" in message

# UNIT-029
Given a closed transport
When I call receive()
Then TransportError is raised with "closed" in message

# INT-008
Given producer and consumer transports
When consumer closes while producer sends
Then producer receives TransportError (not raw BrokenPipeError)
```

---

## NFR-Specific Test Scenarios

Based on the NFR Assessment, additional scenarios for non-functional requirements:

| ID | Level | Priority | NFR | Test | Justification |
|----|-------|----------|-----|------|---------------|
| TEA-STREAM-002.1-UNIT-030 | Unit | P1 | Maintainability | Transport ABC has docstrings for all methods | Code quality |
| TEA-STREAM-002.1-UNIT-031 | Unit | P1 | Maintainability | mypy passes with strict mode on transports/ | Type safety |
| TEA-STREAM-002.1-INT-009 | Integration | P2 | Performance | send() latency < 1ms for 64KB payload | Performance baseline |
| TEA-STREAM-002.1-INT-010 | Integration | P2 | Performance | receive() latency < 1ms for 64KB payload | Performance baseline |

---

## Risk Coverage Matrix

| Risk ID | Risk | Test Coverage |
|---------|------|---------------|
| TECH-001 | ABC Interface Lock-in | UNIT-001, UNIT-002 |
| TECH-003 | Task/AC Conflict (delegation) | INT-003 (code inspection) |
| TECH-004 | Mutable Default Bug | UNIT-006 |
| OPS-001 | Resource Leak (FD exhaustion) | UNIT-011, UNIT-024, UNIT-025, UNIT-026, UNIT-027 |
| DATA-001 | State Inconsistency | UNIT-020, UNIT-021, UNIT-022, UNIT-023 |
| BUS-001 | Backward Compatibility | INT-004, INT-005 |

---

## Test Data Requirements

### Unit Tests
- **No external test data required**: Use in-memory constructs and mocks
- **Payload sizes**: b"test" (small), b"x" * 64 * 1024 (64KB), b"x" * 1024 * 1024 (1MB for perf tests)

### Integration Tests
- **Real Unix pipes**: Tests must create actual pipes via `os.pipe()`
- **Temporary addresses**: Use `tmp_path` fixture for any file-based addressing
- **Multi-process tests**: May require `subprocess` or `multiprocessing` for INT-008

### Environment Requirements
- **Platform**: Unix/Linux (Windows not supported for Unix pipes)
- **CI compatibility**: Verify `os.pipe()` works in CI container
- **File descriptor limits**: Ensure ulimit allows pipe creation in tests

---

## Test Environment Setup

### Fixtures Required

```python
import pytest
from the_edge_agent.transports import Transport, TransportConfig, UnixPipeTransport, TransportFactory

@pytest.fixture
def transport_config():
    """Default Unix transport configuration."""
    return TransportConfig(transport_type="unix")

@pytest.fixture
def unix_transport(transport_config):
    """UnixPipeTransport with automatic cleanup."""
    transport = UnixPipeTransport(transport_config)
    yield transport
    if transport.is_connected:
        transport.close()

@pytest.fixture
def pipe_pair():
    """Producer/consumer pipe pair for integration tests."""
    producer = UnixPipeTransport(TransportConfig())
    consumer = UnixPipeTransport(TransportConfig())
    # Setup pipe connection...
    yield producer, consumer
    producer.close()
    consumer.close()
```

### Mock Strategy

| Component | Mock Approach |
|-----------|---------------|
| `os.pipe()` | `unittest.mock.patch("os.pipe")` for unit tests |
| `os.read()` | `unittest.mock.patch("os.read")` for unit tests |
| `os.write()` | `unittest.mock.patch("os.write")` for unit tests |
| `os.close()` | `unittest.mock.patch("os.close")` for cleanup verification |

---

## Recommended Test Execution Order

### CI Pipeline Order

1. **P0 Unit Tests** (fail fast on interface violations)
   - UNIT-001, UNIT-002, UNIT-004, UNIT-006, UNIT-007, UNIT-008, UNIT-009, UNIT-010
   - UNIT-017, UNIT-018, UNIT-020, UNIT-021, UNIT-022, UNIT-023
   - UNIT-024, UNIT-025, UNIT-026, UNIT-027

2. **P0 Integration Tests** (validate real behavior)
   - INT-001, INT-003, INT-004, INT-005

3. **P1 Unit Tests**
   - UNIT-003, UNIT-005, UNIT-011, UNIT-012, UNIT-014, UNIT-015, UNIT-016
   - UNIT-019, UNIT-028, UNIT-029, UNIT-030, UNIT-031

4. **P1 Integration Tests**
   - INT-002, INT-006, INT-007, INT-008

5. **P2 Integration Tests** (performance baseline)
   - INT-009, INT-010

---

## Coverage Summary by AC

| AC | Unit Tests | Integration Tests | Total | Status |
|----|------------|-------------------|-------|--------|
| AC1 | 3 | 0 | 3 | Covered |
| AC2 | 3 | 0 | 3 | Covered |
| AC3 | 5 | 3 | 8 | Covered |
| AC4 | 3 | 0 | 3 | Covered |
| AC5 | 0 | 3 | 3 | Covered (Critical) |
| AC6 | 2 | 1 | 3 | Covered |
| AC7 | 3 | 0 | 3 | Covered |
| AC8 | 4 | 0 | 4 | Covered |
| AC9 | 3 | 0 | 3 | Covered |
| AC10 | 3 | 1 | 4 | Covered |
| NFR | 2 | 2 | 4 | Covered |
| **Total** | **24** | **10** | **34** | **100% AC Coverage** |

---

## Quality Gate YAML Block

```yaml
test_design:
  scenarios_total: 34
  by_level:
    unit: 24
    integration: 10
    e2e: 0
  by_priority:
    p0: 14
    p1: 13
    p2: 7
  coverage_gaps: []
  risk_coverage:
    TECH-001: [UNIT-001, UNIT-002]
    TECH-003: [INT-003]
    TECH-004: [UNIT-006]
    OPS-001: [UNIT-011, UNIT-024, UNIT-025, UNIT-026, UNIT-027]
    DATA-001: [UNIT-020, UNIT-021, UNIT-022, UNIT-023]
    BUS-001: [INT-004, INT-005]
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-STREAM-002.1-test-design-20260201.md
P0 tests identified: 14
Total scenarios: 34
```
