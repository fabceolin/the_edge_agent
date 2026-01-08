# Test Design: Story TEA-STREAM-002.5

Date: 2026-01-08
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 34
- Unit tests: 24 (70%)
- Integration tests: 10 (30%)
- E2E tests: 0 (0%) - E2E coverage handled by epic-level integration tests
- Priority distribution: P0: 8, P1: 14, P2: 12

## Risk Analysis Summary

| Risk Area | Impact | Mitigation |
|-----------|--------|------------|
| YAML parsing regression | High | Comprehensive unit tests for all new fields |
| Environment variable injection | High | Security-focused unit tests for expansion |
| TCP address validation | Medium | Explicit validation error tests |
| pyzmq dependency detection | Medium | Mock-based availability tests |
| Per-node override conflicts | Medium | Edge case testing for merge behavior |

## Test Scenarios by Acceptance Criteria

### AC1: `settings.parallel.streams.transport: zeromq` enables ZeroMQ

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-001 | Unit | P0 | Parse `transport: zeromq` from YAML | Core feature enablement - must work |
| 002.5-UNIT-002 | Unit | P1 | Parse `transport: unix` (default) from YAML | Default behavior verification |
| 002.5-UNIT-003 | Unit | P1 | Reject invalid transport value | Error handling for user mistakes |

### AC2: `settings.parallel.streams.protocol: tcp` enables TCP transport

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-004 | Unit | P0 | Parse `protocol: tcp` from YAML | Critical for distributed workflows |
| 002.5-UNIT-005 | Unit | P1 | Parse `protocol: ipc` (default) from YAML | Default behavior verification |
| 002.5-UNIT-006 | Unit | P2 | Reject invalid protocol value | Error handling |

### AC3: `settings.parallel.streams.pattern: pub_sub|push_pull|req_rep` selects pattern

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-007 | Unit | P0 | Parse `pattern: pub_sub` from YAML | Pattern selection is core feature |
| 002.5-UNIT-008 | Unit | P1 | Parse `pattern: push_pull` from YAML | Load balancing pattern |
| 002.5-UNIT-009 | Unit | P1 | Parse `pattern: req_rep` from YAML | RPC pattern |
| 002.5-UNIT-010 | Unit | P2 | Reject invalid pattern value | Error handling |

### AC4: `stream_mode: load_balance` uses PUSH/PULL pattern

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-011 | Unit | P0 | Map `stream_mode: load_balance` to `pattern: push_pull` | User-friendly abstraction mapping |
| 002.5-INT-001 | Integration | P1 | Load balance mode creates PUSH/PULL transports | Verify end-to-end mapping works |

### AC5: `stream_mode: broadcast` uses PUB/SUB pattern

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-012 | Unit | P0 | Map `stream_mode: broadcast` to `pattern: pub_sub` | User-friendly abstraction mapping |
| 002.5-INT-002 | Integration | P1 | Broadcast mode creates PUB/SUB transports | Verify end-to-end mapping works |

### AC6: Per-stream protocol override via `streams.protocol` on node

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-013 | Unit | P1 | Node-level `protocol` overrides global | Per-node flexibility |
| 002.5-UNIT-014 | Unit | P1 | Node-level `bind_address` overrides global | Per-node flexibility |
| 002.5-UNIT-015 | Unit | P2 | Node-level `connect_addresses` overrides global | Per-node flexibility |
| 002.5-UNIT-016 | Unit | P2 | Node-level `high_water_mark` overrides global | Per-node flexibility |
| 002.5-INT-003 | Integration | P1 | Mixed protocol workflow (IPC + TCP nodes) | Complex real-world scenario |

### AC7: `zeromq.bind_address` and `zeromq.connect_addresses` configuration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-017 | Unit | P0 | Parse `bind_address` from zeromq section | Required for TCP |
| 002.5-UNIT-018 | Unit | P1 | Parse `connect_addresses` list from zeromq section | Required for workers |
| 002.5-INT-004 | Integration | P0 | TCP workflow with explicit addresses | Critical distributed scenario |
| 002.5-INT-005 | Integration | P1 | TCP workflow with multiple connect addresses | Multi-worker scenario |

### AC8: Auto-discovery via `zeromq.discovery: static|env`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-INT-006 | Integration | P1 | Static discovery uses explicit addresses | Baseline discovery mode |
| 002.5-INT-007 | Integration | P1 | Env discovery reads from environment | Dynamic configuration |
| 002.5-UNIT-019 | Unit | P2 | Discovery defaults to `static` | Default behavior |

### AC9: Validation: TCP requires explicit addresses

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-020 | Unit | P0 | Error when TCP lacks bind_address and connect_addresses | Prevent runtime failures |
| 002.5-UNIT-021 | Unit | P1 | Valid when TCP has bind_address only | Producer scenario |
| 002.5-UNIT-022 | Unit | P1 | Valid when TCP has connect_addresses only | Worker scenario |

### AC10: Environment variable expansion in addresses (`${VAR}`)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-023 | Unit | P0 | Expand `${VAR}` syntax in addresses | Core env var feature |
| 002.5-UNIT-024 | Unit | P1 | Expand `${VAR:-default}` with fallback | Robust configuration |
| 002.5-UNIT-025 | Unit | P1 | Missing env var with no default returns empty | Edge case handling |
| 002.5-INT-008 | Integration | P1 | Full workflow with env var addresses | Real-world scenario |

### AC11: Validation: ZeroMQ not available raises helpful error

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-026 | Unit | P0 | Helpful error when pyzmq not installed | User experience for missing dep |
| 002.5-UNIT-027 | Unit | P2 | Error includes installation instructions | Actionable error message |

### AC12: Default values for all ZeroMQ options are sensible

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-028 | Unit | P1 | Default high_water_mark is 1000 | Documented default |
| 002.5-UNIT-029 | Unit | P2 | Default linger is 1000ms | Documented default |
| 002.5-UNIT-030 | Unit | P2 | Default reconnect_interval is 100ms | Documented default |
| 002.5-UNIT-031 | Unit | P2 | Default send_timeout is 5000ms | Documented default |
| 002.5-UNIT-032 | Unit | P2 | Default recv_timeout is 30000ms | Documented default |
| 002.5-UNIT-033 | Unit | P2 | Default ipc_dir is /tmp/tea-zmq | Documented default |
| 002.5-INT-009 | Integration | P1 | Workflow runs with all defaults | Minimal config works |
| 002.5-INT-010 | Integration | P2 | Workflow with custom socket options | Override defaults |

## Additional Edge Case Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-034 | Unit | P2 | Windows + IPC falls back to inproc with warning | Platform compatibility |

## Test Implementation Guidance

### Unit Test File: `python/tests/test_yaml_zeromq.py`

```python
# Recommended test structure
class TestZeroMQYAMLParsing:
    """Tests for AC1-AC3: Basic YAML field parsing"""

class TestStreamModeMapping:
    """Tests for AC4-AC5: stream_mode to pattern mapping"""

class TestPerNodeOverrides:
    """Tests for AC6: Node-level configuration overrides"""

class TestZeroMQSettings:
    """Tests for AC7, AC12: ZeroMQ-specific settings and defaults"""

class TestAddressValidation:
    """Tests for AC9: TCP address requirements"""

class TestEnvVarExpansion:
    """Tests for AC10: ${VAR} and ${VAR:-default} expansion"""

class TestPyZMQAvailability:
    """Tests for AC11: pyzmq availability detection"""
```

### Integration Test File: `python/tests/test_yaml_zeromq_e2e.py`

```python
# Recommended test structure
class TestStaticDiscovery:
    """Tests for AC8: Static address discovery"""

class TestEnvDiscovery:
    """Tests for AC8: Environment-based discovery"""

class TestMixedProtocolWorkflows:
    """Tests for AC6: Mixed IPC + TCP workflows"""

class TestTCPWorkflows:
    """Tests for AC7: Full TCP workflow scenarios"""
```

### Mock Strategy

| Dependency | Mock Approach |
|------------|---------------|
| pyzmq | `unittest.mock.patch.dict(sys.modules, {'zmq': None})` |
| Environment variables | `monkeypatch.setenv()` / `monkeypatch.delenv()` |
| File system (IPC dir) | `tmp_path` fixture |
| Transport creation | Mock `TransportFactory` for unit tests |

### Test Data Fixtures

```yaml
# Minimal ZeroMQ config
minimal_zeromq:
  settings:
    parallel:
      streams:
        transport: zeromq

# Full TCP config
full_tcp_config:
  settings:
    parallel:
      streams:
        transport: zeromq
        protocol: tcp
        pattern: push_pull
        zeromq:
          bind_address: "tcp://*:5555"
          connect_addresses:
            - "tcp://worker1:5555"
          high_water_mark: 2000

# Env var config
env_var_config:
  settings:
    parallel:
      streams:
        transport: zeromq
        protocol: tcp
        zeromq:
          discovery: env
          bind_address: "${ZMQ_BIND:-tcp://*:5555}"
```

## Risk Coverage Matrix

| Risk | Test IDs | Coverage |
|------|----------|----------|
| YAML parsing breaks | 002.5-UNIT-001 through 010 | Full |
| Env var security | 002.5-UNIT-023 through 025 | Full |
| TCP validation bypass | 002.5-UNIT-020 through 022 | Full |
| pyzmq detection | 002.5-UNIT-026, 027 | Full |
| Override conflicts | 002.5-UNIT-013 through 016, INT-003 | Full |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core parsing)
   - 002.5-UNIT-001, 004, 007, 011, 012, 017, 020, 023, 026
2. **P0 Integration tests**
   - 002.5-INT-004
3. **P1 Unit tests**
   - Remaining parsing and validation tests
4. **P1 Integration tests**
   - Discovery and workflow tests
5. **P2 tests as time permits**
   - Default value verification, edge cases

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 34
  by_level:
    unit: 24
    integration: 10
    e2e: 0
  by_priority:
    p0: 8
    p1: 14
    p2: 12
  coverage_gaps: []
  notes:
    - E2E coverage deferred to epic-level TEA-STREAM-002 integration tests
    - All 12 acceptance criteria have test coverage
    - Focus on YAML parsing correctness and validation errors
```

## Quality Checklist

- [x] Every AC has test coverage (AC1-AC12 all covered)
- [x] Test levels are appropriate (unit for parsing, integration for workflows)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (P0 for core features, validation)
- [x] Test IDs follow naming convention (`{epic}.{story}-{LEVEL}-{SEQ}`)
- [x] Scenarios are atomic and independent
- [x] Mock strategy defined for external dependencies
- [x] Test data fixtures documented

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-STREAM-002.5-test-design-20260108.md
P0 tests identified: 8
Total scenarios: 34
Coverage: 100% of acceptance criteria
```
