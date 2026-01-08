# Test Design: Story TEA-STREAM-002.1

**Story:** Transport Abstraction Layer
**Date:** 2026-01-08
**Designer:** Quinn (Test Architect)

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 28 |
| Unit tests | 21 (75%) |
| Integration tests | 7 (25%) |
| E2E tests | 0 (0%) |
| Priority distribution | P0: 10, P1: 12, P2: 6, P3: 0 |

### Strategy Rationale

This story implements a **transport abstraction layer** - primarily an internal library component. The test strategy favors:

1. **Heavy unit testing** - Pure interface contracts, dataclass validation, error handling, and state management are ideal for unit tests
2. **Targeted integration tests** - Verify backward compatibility and real pipe I/O operations
3. **No E2E tests** - This is infrastructure code with no user-facing journeys; integration tests provide sufficient coverage for pipe workflows

---

## Test Scenarios by Acceptance Criteria

### AC1: Transport Base Class Interface

> `Transport` base class defines `bind()`, `connect()`, `send()`, `receive()`, `close()` interface

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.1-UNIT-001 | Unit | P0 | Verify `Transport` ABC defines all required abstract methods | Core interface contract validation |
| TEA-STREAM-002.1-UNIT-002 | Unit | P1 | Verify concrete class without all methods raises `TypeError` | Enforces proper implementation |
| TEA-STREAM-002.1-UNIT-003 | Unit | P1 | Verify `Transport.__init__` accepts `TransportConfig` | Constructor contract |

---

### AC2: TransportConfig Dataclass

> `TransportConfig` dataclass holds transport type, protocol, pattern, and options

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.1-UNIT-004 | Unit | P1 | Create `TransportConfig` with defaults (`unix`, `ipc`, `direct`, `{}`) | Default value validation |
| TEA-STREAM-002.1-UNIT-005 | Unit | P1 | Create `TransportConfig` with custom values | Custom configuration paths |
| TEA-STREAM-002.1-UNIT-006 | Unit | P2 | Verify `TransportConfig.options` mutable dict isolation | Prevent shared state bugs |

---

### AC3: UnixPipeTransport Implementation

> `UnixPipeTransport` implements `Transport` interface using patterns from `StreamChannel` (reimplements pipe logic)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.1-UNIT-007 | Unit | P0 | `UnixPipeTransport.bind()` creates write-side file descriptor | Core bind functionality |
| TEA-STREAM-002.1-UNIT-008 | Unit | P0 | `UnixPipeTransport.connect()` opens read-side file descriptor | Core connect functionality |
| TEA-STREAM-002.1-UNIT-009 | Unit | P0 | `UnixPipeTransport.send()` writes data to pipe | Core data transmission |
| TEA-STREAM-002.1-UNIT-010 | Unit | P0 | `UnixPipeTransport.receive()` reads data from pipe | Core data reception |
| TEA-STREAM-002.1-INT-001 | Integration | P0 | Full send/receive cycle through actual Unix pipe | Real I/O verification |
| TEA-STREAM-002.1-INT-002 | Integration | P1 | Multi-message send/receive sequence | Streaming data flow |

---

### AC4: TransportFactory Creation

> `TransportFactory.create()` returns appropriate transport based on config

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.1-UNIT-011 | Unit | P0 | `TransportFactory.create()` returns `UnixPipeTransport` for `transport_type="unix"` | Primary factory dispatch |
| TEA-STREAM-002.1-UNIT-012 | Unit | P1 | `TransportFactory.create()` raises `TransportError` for unknown type | Error handling for bad config |
| TEA-STREAM-002.1-UNIT-013 | Unit | P2 | Factory passes config options to transport constructor | Config propagation |

---

### AC5: Backward Compatibility

> Existing Unix pipe workflows work unchanged when `transport: unix` (default)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.1-INT-003 | Integration | P0 | All existing `test_streams.py` tests pass unchanged | Regression prevention - critical |
| TEA-STREAM-002.1-INT-004 | Integration | P1 | `StreamRegistry` with explicit `transport: unix` behaves identically | Explicit config backward compat |
| TEA-STREAM-002.1-INT-005 | Integration | P1 | `StreamRegistry` without transport config uses Unix pipes | Default config backward compat |

---

### AC6: StreamRegistry Integration

> `StreamRegistry` accepts optional `transport_factory` parameter

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.1-UNIT-014 | Unit | P1 | `StreamRegistry.__init__` accepts `transport_factory` parameter | API extension |
| TEA-STREAM-002.1-UNIT-015 | Unit | P1 | Custom `transport_factory` callable invoked during channel creation | Factory injection |
| TEA-STREAM-002.1-INT-006 | Integration | P2 | Mock transport factory integrated with real `StreamRegistry` | Factory customization |

---

### AC7: TransportError Exceptions

> Transport errors raise `TransportError` with helpful messages

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.1-UNIT-016 | Unit | P1 | `TransportError` includes descriptive message | Error usability |
| TEA-STREAM-002.1-UNIT-017 | Unit | P1 | Invalid bind address raises `TransportError` with context | Helpful error messages |
| TEA-STREAM-002.1-UNIT-018 | Unit | P2 | `TransportError` can wrap underlying OS exceptions | Exception chaining |

---

### AC8: Connection State Property

> `is_connected` property reflects connection state accurately

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.1-UNIT-019 | Unit | P0 | `is_connected` returns `False` before `bind()`/`connect()` | Initial state |
| TEA-STREAM-002.1-UNIT-020 | Unit | P0 | `is_connected` returns `True` after successful `bind()` | Post-bind state |
| TEA-STREAM-002.1-UNIT-021 | Unit | P1 | `is_connected` returns `False` after `close()` | Post-close state |

---

### AC9: Context Manager Support

> Context manager support (`with transport:`) for automatic cleanup

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.1-UNIT-022 | Unit | P1 | `with UnixPipeTransport(...) as t:` enters context | Context manager entry |
| TEA-STREAM-002.1-UNIT-023 | Unit | P1 | Context manager calls `close()` on exit | Automatic cleanup |
| TEA-STREAM-002.1-UNIT-024 | Unit | P2 | Context manager calls `close()` even on exception | Exception-safe cleanup |

---

### AC10: Graceful Pending Operation Handling

> Graceful handling when transport is closed while operations pending

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-STREAM-002.1-UNIT-025 | Unit | P1 | `close()` is idempotent (multiple calls safe) | Defensive cleanup |
| TEA-STREAM-002.1-UNIT-026 | Unit | P1 | `send()` after `close()` raises `TransportError` | Post-close error handling |
| TEA-STREAM-002.1-UNIT-027 | Unit | P2 | `BrokenPipeError` caught and wrapped in `TransportError` | OS error translation |
| TEA-STREAM-002.1-INT-007 | Integration | P2 | Close during blocked read does not hang | Real I/O timeout behavior |

---

## Test Implementation Details

### Unit Test Fixtures

```python
@pytest.fixture
def unix_config():
    """Default Unix pipe transport configuration."""
    return TransportConfig(transport_type="unix", protocol="ipc", pattern="direct")

@pytest.fixture
def mock_pipe():
    """Mock os.pipe() for isolated testing."""
    with patch('os.pipe') as mock:
        mock.return_value = (3, 4)  # mock fds
        yield mock

@pytest.fixture
def unix_transport(unix_config):
    """UnixPipeTransport instance for testing."""
    return UnixPipeTransport(unix_config)
```

### Integration Test Setup

Integration tests should use real pipes:

```python
@pytest.fixture
def pipe_pair():
    """Create actual Unix pipe for integration tests."""
    read_fd, write_fd = os.pipe()
    yield read_fd, write_fd
    # Cleanup
    try:
        os.close(read_fd)
    except OSError:
        pass
    try:
        os.close(write_fd)
    except OSError:
        pass
```

---

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| Backward compatibility regression | TEA-STREAM-002.1-INT-003, INT-004, INT-005 |
| Resource leak (unclosed file descriptors) | TEA-STREAM-002.1-UNIT-023, UNIT-024, UNIT-025 |
| State inconsistency | TEA-STREAM-002.1-UNIT-019, UNIT-020, UNIT-021 |
| Factory dispatch failure | TEA-STREAM-002.1-UNIT-011, UNIT-012 |
| Error message quality | TEA-STREAM-002.1-UNIT-016, UNIT-017, UNIT-018 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on interface contracts)
   - TEA-STREAM-002.1-UNIT-001 (ABC definition)
   - TEA-STREAM-002.1-UNIT-007 through UNIT-010 (core transport ops)
   - TEA-STREAM-002.1-UNIT-011 (factory dispatch)
   - TEA-STREAM-002.1-UNIT-019, UNIT-020 (connection state)

2. **P0 Integration tests**
   - TEA-STREAM-002.1-INT-001 (real pipe I/O)
   - TEA-STREAM-002.1-INT-003 (backward compat regression)

3. **P1 Unit tests**
   - All remaining UNIT tests at P1

4. **P1 Integration tests**
   - TEA-STREAM-002.1-INT-002, INT-004, INT-005

5. **P2 tests** (as time permits)
   - All P2 scenarios

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 28
  by_level:
    unit: 21
    integration: 7
    e2e: 0
  by_priority:
    p0: 10
    p1: 12
    p2: 6
    p3: 0
  coverage_gaps: []
  critical_scenarios:
    - TEA-STREAM-002.1-INT-003  # Backward compatibility
    - TEA-STREAM-002.1-INT-001  # Real pipe I/O
    - TEA-STREAM-002.1-UNIT-011 # Factory dispatch
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-STREAM-002.1-test-design-20260108.md
P0 tests identified: 10
Total scenarios: 28
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Backward compatibility explicitly tested (AC5)
- [x] Resource cleanup paths covered (AC9, AC10)
