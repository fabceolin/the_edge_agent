# Story TEA-STREAM-002.5: YAML Integration & TCP Discovery

## Status: Ready for Development

## Story

**As a** workflow developer,
**I want** to configure ZeroMQ transports in YAML,
**So that** I can declaratively define distributed streaming workflows.

## Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `settings.parallel.streams.transport: zeromq` enables ZeroMQ | Unit test |
| AC2 | `settings.parallel.streams.protocol: tcp` enables TCP transport | Unit test |
| AC3 | `settings.parallel.streams.pattern: pub_sub\|push_pull\|req_rep` selects pattern | Unit test |
| AC4 | `stream_mode: load_balance` uses PUSH/PULL pattern | Unit test |
| AC5 | `stream_mode: broadcast` uses PUB/SUB pattern | Unit test |
| AC6 | Per-stream protocol override via `streams.protocol` on node | Unit test |
| AC7 | `zeromq.bind_address` and `zeromq.connect_addresses` configuration | Integration test |
| AC8 | Auto-discovery via `zeromq.discovery: static\|env` | Integration test |
| AC9 | Validation: TCP requires explicit addresses | Unit test |
| AC10 | Environment variable expansion in addresses (`${VAR}`) | Unit test |
| AC11 | Validation: ZeroMQ not available raises helpful error | Unit test |
| AC12 | Default values for all ZeroMQ options are sensible | Unit test |

## Tasks / Subtasks

- [ ] **Task 1: Extend YAML schema for ZeroMQ** (AC: 1, 2, 3)
  - [ ] Modify `python/src/the_edge_agent/yaml_engine.py`
  - [ ] Add `transport` field to `settings.parallel.streams`
  - [ ] Add `protocol` field (ipc, tcp)
  - [ ] Add `pattern` field (pub_sub, push_pull, req_rep)
  - [ ] Parse and validate new fields

- [ ] **Task 2: Add ZeroMQ-specific settings** (AC: 7, 12)
  - [ ] Modify `python/src/the_edge_agent/yaml_engine.py`
  - [ ] Add `zeromq` subsection under `settings.parallel.streams`
  - [ ] Parse `bind_address`, `connect_addresses`
  - [ ] Parse `high_water_mark`, `linger`, `reconnect_interval`
  - [ ] Set sensible defaults (hwm=1000, linger=1000, etc.)

- [ ] **Task 3: Implement stream mode mapping** (AC: 4, 5)
  - [ ] Modify `python/src/the_edge_agent/yaml_engine.py` (in `_parse_parallel_settings` or similar)
  - [ ] Map `stream_mode: load_balance` to `pattern: push_pull`
  - [ ] Map `stream_mode: broadcast` to `pattern: pub_sub`
  - [ ] Keep `stream_mode: direct` for 1:1 connections
  - [ ] Add `stream_mode: rpc` for `pattern: req_rep`

- [ ] **Task 4: Implement per-node overrides** (AC: 6)
  - [ ] Modify `python/src/the_edge_agent/yaml_nodes.py`
  - [ ] Allow `protocol`, `bind_address` on node's `streams` block
  - [ ] Merge node-level settings with global settings
  - [ ] Validate override combinations

- [ ] **Task 5: Implement address discovery** (AC: 8, 10)
  - [ ] Create `python/src/the_edge_agent/transports/discovery.py`
  - [ ] Implement `static` discovery (explicit addresses)
  - [ ] Implement `env` discovery (from environment variables)
  - [ ] Expand `${VAR}` syntax in addresses
  - [ ] Add `${VAR:-default}` fallback syntax

- [ ] **Task 6: Add validation rules** (AC: 9, 11)
  - [ ] Validate TCP protocol requires addresses
  - [ ] Validate IPC on Windows falls back to inproc
  - [ ] Check pyzmq availability at YAML load time
  - [ ] Provide helpful error message if pyzmq missing

- [ ] **Task 7: Integrate with StreamRegistry** (AC: 1-6)
  - [ ] Modify `python/src/the_edge_agent/streams.py`
  - [ ] Create `TransportConfig` from YAML settings
  - [ ] Pass config to `TransportFactory`
  - [ ] Handle transport creation during workflow compile

- [ ] **Task 8: Write unit tests** (AC: 1-6, 9-12)
  - [ ] Create `python/tests/test_yaml_zeromq.py`
  - [ ] Test YAML parsing for all new fields
  - [ ] Test validation errors
  - [ ] Test environment variable expansion
  - [ ] Test stream_mode mapping
  - [ ] Test per-node overrides

- [ ] **Task 9: Write integration tests** (AC: 7, 8)
  - [ ] Test static address discovery
  - [ ] Test env-based discovery
  - [ ] Test TCP workflow end-to-end
  - [ ] Test mixed Unix + ZeroMQ workflow

## Dev Notes

### Relevant Source Tree

```
python/src/the_edge_agent/
├── yaml_engine.py          # Main YAML parsing - MODIFIED
├── yaml_nodes.py           # Node parsing - MODIFIED
├── yaml_edges.py           # Edge parsing - MODIFIED (if exists)
├── streams.py              # StreamRegistry - MODIFIED
└── transports/
    ├── __init__.py
    ├── base.py
    ├── factory.py
    ├── discovery.py        # NEW - Address discovery
    └── zeromq/
        ├── __init__.py
        ├── base.py
        ├── pubsub.py
        ├── pushpull.py
        └── reqrep.py
```

### Dependencies

- **Story**: TEA-STREAM-002.1 through 002.4 (all ZeroMQ patterns)
- **Blocked by**: TEA-STREAM-002.1 (Transport Abstraction), TEA-STREAM-002.2 (PUB/SUB), TEA-STREAM-002.3 (PUSH/PULL), TEA-STREAM-002.4 (REQ/REP) - all transport implementations must exist before YAML integration

### YAML Schema Additions

```yaml
# Full ZeroMQ configuration example
name: distributed-workflow

settings:
  parallel:
    strategy: process
    streams:
      enabled: true
      transport: zeromq           # NEW: unix (default) | zeromq
      protocol: ipc               # NEW: ipc (default) | tcp
      pattern: push_pull          # NEW: pub_sub | push_pull | req_rep

      # NEW: ZeroMQ-specific settings
      zeromq:
        # IPC settings
        ipc_dir: /tmp/tea-zmq     # Directory for IPC sockets

        # TCP settings (required if protocol: tcp)
        bind_address: "tcp://*:5555"
        connect_addresses:
          - "tcp://worker1:5555"
          - "tcp://worker2:5555"

        # Socket options
        high_water_mark: 1000     # Message queue depth
        linger: 1000              # Close timeout (ms)
        reconnect_interval: 100   # Reconnect delay (ms)
        send_timeout: 5000        # Send timeout (ms)
        recv_timeout: 30000       # Receive timeout (ms)

        # Discovery
        discovery: static         # static | env

# Node-level overrides
nodes:
  - name: distributed_producer
    run: |
      # Producer code
    streams:
      stdout: task_queue
      # Override global settings for this node
      protocol: tcp
      bind_address: "tcp://*:5556"

  - name: local_consumer
    run: |
      # Consumer code
    streams:
      stdin: task_queue
      # Use IPC for this consumer (local)
      protocol: ipc

# Edge-level stream mode (maps to pattern)
edges:
  - from: producer
    to: [worker_1, worker_2, worker_3]
    parallel: true
    parallel_strategy: process
    stream_mode: load_balance    # Maps to pattern: push_pull
    fan_in: aggregator
```

### Stream Mode to Pattern Mapping

| stream_mode | pattern | Description |
|-------------|---------|-------------|
| `direct` | N/A | 1:1 connection (Unix pipes or direct ZMQ) |
| `broadcast` | `pub_sub` | 1:N, all consumers get all messages |
| `load_balance` | `push_pull` | 1:N, round-robin distribution |
| `rpc` | `req_rep` | Request-response communication |

### Environment Variable Discovery

```yaml
settings:
  parallel:
    streams:
      transport: zeromq
      protocol: tcp
      zeromq:
        discovery: env
        bind_address: "${ZMQ_BIND_ADDR:-tcp://*:5555}"
        connect_addresses:
          - "${ZMQ_WORKER_1}"
          - "${ZMQ_WORKER_2}"
          - "${ZMQ_WORKER_3:-tcp://localhost:5557}"
```

### Implementation Sketch

```python
# yaml_engine.py additions
def _parse_zeromq_settings(self, streams_config: dict) -> dict:
    """Parse ZeroMQ-specific settings from YAML."""
    zeromq = streams_config.get("zeromq", {})

    # Apply defaults
    defaults = {
        "high_water_mark": 1000,
        "linger": 1000,
        "reconnect_interval": 100,
        "send_timeout": 5000,
        "recv_timeout": 30000,
        "ipc_dir": "/tmp/tea-zmq",
        "discovery": "static",
    }

    for key, default in defaults.items():
        zeromq.setdefault(key, default)

    # Expand environment variables
    zeromq = self._expand_env_vars(zeromq)

    # Validate TCP requirements
    protocol = streams_config.get("protocol", "ipc")
    if protocol == "tcp":
        if "bind_address" not in zeromq and "connect_addresses" not in zeromq:
            raise YAMLValidationError(
                "TCP protocol requires 'bind_address' or 'connect_addresses' "
                "in settings.parallel.streams.zeromq"
            )

    return zeromq

def _expand_env_vars(self, config: dict) -> dict:
    """Expand ${VAR} and ${VAR:-default} in string values."""
    import os
    import re

    pattern = r'\$\{([^}:]+)(?::-([^}]*))?\}'

    def expand(value):
        if isinstance(value, str):
            def replacer(match):
                var_name = match.group(1)
                default = match.group(2) or ""
                return os.environ.get(var_name, default)
            return re.sub(pattern, replacer, value)
        elif isinstance(value, dict):
            return {k: expand(v) for k, v in value.items()}
        elif isinstance(value, list):
            return [expand(v) for v in value]
        return value

    return expand(config)
```

### Per-Node Override Sketch (Task 4)

```python
# yaml_nodes.py additions
@dataclass
class NodeStreamsConfig:
    """Per-node stream configuration that can override global settings."""
    stdout: Optional[str] = None
    stdin: Optional[str] = None
    stderr: Optional[str] = None
    # Override fields
    protocol: Optional[str] = None        # Override global protocol
    bind_address: Optional[str] = None    # Override bind address
    connect_addresses: Optional[List[str]] = None  # Override connect addresses
    high_water_mark: Optional[int] = None # Override HWM

def _parse_node_streams(self, node_config: dict, global_streams: dict) -> NodeStreamsConfig:
    """Parse node-level streams with override merging."""
    streams = node_config.get("streams", {})

    # Start with global settings
    merged = dict(global_streams)

    # Override with node-level settings
    for key in ["protocol", "bind_address", "connect_addresses", "high_water_mark"]:
        if key in streams:
            merged[key] = streams[key]

    return NodeStreamsConfig(
        stdout=streams.get("stdout"),
        stdin=streams.get("stdin"),
        stderr=streams.get("stderr"),
        protocol=merged.get("protocol"),
        bind_address=merged.get("bind_address"),
        connect_addresses=merged.get("connect_addresses"),
        high_water_mark=merged.get("high_water_mark"),
    )
```

### Validation Rules

| Rule | Error Message |
|------|---------------|
| TCP without addresses | "TCP protocol requires 'bind_address' or 'connect_addresses'" |
| pyzmq not installed | "ZeroMQ transport requires pyzmq. Install with: pip install pyzmq" |
| Invalid pattern | "Unknown pattern 'X'. Valid: pub_sub, push_pull, req_rep" |
| Invalid protocol | "Unknown protocol 'X'. Valid: ipc, tcp" |
| Windows + IPC | "IPC protocol not supported on Windows. Using inproc instead." |

### Testing

| Test Category | Location | Framework |
|--------------|----------|-----------|
| Unit tests | `python/tests/test_yaml_zeromq.py` | pytest |
| Integration tests | `python/tests/test_yaml_zeromq_e2e.py` | pytest |

**Test Standards**:
- Test all YAML parsing scenarios
- Test validation error messages
- Mock pyzmq availability for error path
- Use temp directories for IPC paths

## Definition of Done

- [ ] All ZeroMQ settings parseable from YAML
- [ ] stream_mode correctly maps to ZeroMQ patterns
- [ ] Per-node overrides work correctly
- [ ] Environment variable expansion works
- [ ] TCP validation enforces address requirements
- [ ] Helpful error when pyzmq not installed
- [ ] All unit and integration tests pass
- [ ] Documentation updated in YAML_REFERENCE.md
- [ ] Code follows project style

## Dev Agent Record

_To be populated during implementation._

## QA Results

### Test Design Assessment (2026-01-08)

**Assessor:** Quinn (Test Architect)

**Test Design Document:** `docs/qa/assessments/TEA-STREAM-002.5-test-design-20260108.md`

**Summary:**
- Total test scenarios: 34
- Unit tests: 24 (70%)
- Integration tests: 10 (30%)
- Priority distribution: P0: 8, P1: 14, P2: 12

**Coverage:**
- All 12 acceptance criteria have test coverage
- P0 tests cover: transport enablement, protocol parsing, pattern selection, stream_mode mapping, TCP validation, env var expansion, pyzmq detection
- Integration tests cover: discovery modes, mixed protocol workflows, TCP end-to-end scenarios

**Key Test Areas:**
1. YAML parsing for all new fields (transport, protocol, pattern, zeromq settings)
2. stream_mode to pattern mapping (load_balance → push_pull, broadcast → pub_sub)
3. Per-node override merging behavior
4. Environment variable expansion (`${VAR}` and `${VAR:-default}`)
5. TCP address validation (requires bind_address or connect_addresses)
6. pyzmq availability detection with helpful error messages
7. Default value verification for all ZeroMQ socket options

**Recommendations:**
- Implement unit tests in `python/tests/test_yaml_zeromq.py`
- Implement integration tests in `python/tests/test_yaml_zeromq_e2e.py`
- Use mocking for pyzmq availability tests
- Use pytest fixtures for test YAML configurations

---

## QA Notes

**Reviewed by:** Quinn (Test Architect)
**Review Date:** 2026-01-08

### Test Coverage Summary

| Category | Count | Percentage |
|----------|-------|------------|
| Unit Tests | 24 | 70% |
| Integration Tests | 10 | 30% |
| E2E Tests | 0 | 0%* |
| **Total Scenarios** | **34** | **100%** |

*E2E coverage is deferred to epic-level TEA-STREAM-002 integration tests.

**AC Coverage:** All 12 acceptance criteria have complete test coverage with traceable test IDs.

### Risk Areas Identified

| Risk | Impact | Likelihood | Mitigation |
|------|--------|------------|------------|
| **YAML parsing regression** | High | Medium | Comprehensive unit tests (002.5-UNIT-001 through 010) for all new fields |
| **Environment variable injection** | High | Low | Security-focused tests for `${VAR}` expansion (002.5-UNIT-023 through 025) |
| **TCP address validation bypass** | Medium | Medium | Explicit validation error tests (002.5-UNIT-020 through 022) |
| **pyzmq dependency detection** | Medium | Low | Mock-based availability tests (002.5-UNIT-026, 027) |
| **Per-node override conflicts** | Medium | Medium | Edge case testing for merge behavior (002.5-UNIT-013 through 016) |
| **Windows platform compatibility** | Low | Low | IPC fallback to inproc test (002.5-UNIT-034) |

### Recommended Test Scenarios

#### P0 (Critical Path) - Must Pass Before Merge
1. **002.5-UNIT-001**: Parse `transport: zeromq` from YAML
2. **002.5-UNIT-004**: Parse `protocol: tcp` from YAML
3. **002.5-UNIT-007**: Parse `pattern: pub_sub` from YAML
4. **002.5-UNIT-011**: Map `stream_mode: load_balance` to `pattern: push_pull`
5. **002.5-UNIT-012**: Map `stream_mode: broadcast` to `pattern: pub_sub`
6. **002.5-UNIT-017**: Parse `bind_address` from zeromq section
7. **002.5-UNIT-020**: Error when TCP lacks bind_address and connect_addresses
8. **002.5-UNIT-023**: Expand `${VAR}` syntax in addresses
9. **002.5-UNIT-026**: Helpful error when pyzmq not installed
10. **002.5-INT-004**: TCP workflow with explicit addresses

#### P1 (Important) - Should Pass
- Default value tests for protocol, pattern (002.5-UNIT-002, 005, 008, 009)
- Per-node override tests (002.5-UNIT-013 through 016)
- Discovery mode tests (002.5-INT-006, 007)
- Multi-worker TCP scenarios (002.5-INT-005)

#### P2 (Nice to Have)
- Edge case validation (invalid values, Windows fallback)
- Socket option defaults verification
- Custom socket options workflow

### Concerns and Blockers

#### Blockers
1. **Hard Dependency Chain**: This story is blocked by TEA-STREAM-002.1 through 002.4. All transport implementations must exist before YAML integration can be tested end-to-end.

#### Concerns
1. **Security**: Environment variable expansion (`${VAR}`) must be sandboxed to prevent injection attacks. Tests should verify that only standard env var syntax is supported.
2. **Error Messages**: User-facing error messages for missing pyzmq or invalid TCP config must be actionable. Tests should verify exact error text.
3. **Default Drift**: If default values change in implementation, tests must be updated. Consider externalizing defaults to a constants file.
4. **Windows Testing**: Platform-specific behavior (IPC → inproc fallback) may require CI matrix expansion.

### Test Implementation Notes

**Mock Strategy:**
- Use `unittest.mock.patch.dict(sys.modules, {'zmq': None})` for pyzmq availability tests
- Use `monkeypatch.setenv()` / `monkeypatch.delenv()` for env var tests
- Use `tmp_path` fixture for IPC directory tests
- Mock `TransportFactory` for unit tests to avoid pyzmq dependency

**Recommended Test File Structure:**
```
python/tests/
├── test_yaml_zeromq.py          # Unit tests (24 scenarios)
└── test_yaml_zeromq_e2e.py      # Integration tests (10 scenarios)
```

### Quality Gate Readiness

| Criterion | Status |
|-----------|--------|
| Test design complete | ✅ |
| All ACs have test coverage | ✅ |
| Risk areas identified | ✅ |
| Mock strategy defined | ✅ |
| Test data fixtures documented | ✅ |
| Blocking dependencies identified | ✅ |

**Recommendation:** Story is ready for implementation. Test design provides comprehensive coverage. Ensure blocking dependencies (TEA-STREAM-002.1 through 002.4) are complete before integration testing.

### Quality Gate Status: **PASS**

YAML integration connects all transport patterns to the declarative workflow definition. Test coverage appropriately focuses on parsing and validation.

### Acceptance Criteria Traceability

| AC | Tests | Status |
|----|-------|--------|
| AC1 | UNIT-001, UNIT-002, UNIT-003 | ✅ Covered |
| AC2 | UNIT-004, UNIT-005, UNIT-006 | ✅ Covered |
| AC3 | UNIT-007, UNIT-008, UNIT-009, UNIT-010 | ✅ Covered |
| AC4 | UNIT-011, INT-001 | ✅ Covered |
| AC5 | UNIT-012, INT-002 | ✅ Covered |
| AC6 | UNIT-013, UNIT-014, UNIT-015, UNIT-016 | ✅ Covered |
| AC7 | UNIT-017, UNIT-018, UNIT-019, INT-003 | ✅ Covered |
| AC8 | INT-006, INT-007 | ✅ Covered |
| AC9 | UNIT-020, UNIT-021, UNIT-022 | ✅ Covered (Critical) |
| AC10 | UNIT-023, UNIT-024, UNIT-025 | ✅ Covered |
| AC11 | UNIT-026, UNIT-027 | ✅ Covered |
| AC12 | UNIT-028 to UNIT-034 | ✅ Covered |

### Pre-Implementation Checklist

- [ ] Verify TEA-STREAM-002.1 through 002.4 are complete
- [ ] Review existing YAML parsing patterns in `yaml_engine.py`
- [ ] Plan environment variable expansion security carefully
- [ ] Test error messages with actual pyzmq unavailable

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Initial story creation | Sarah (PO) |
| 2026-01-08 | 0.2 | Fixed Task 3 file path (yaml_engine.py), added Task 4 code sketch, blocking deps per validation | Sarah (PO) |
| 2026-01-08 | 0.3 | Added QA Notes with test design assessment | Quinn (QA) |
| 2026-01-08 | 0.4 | Added Quality Gate Status and AC traceability | Quinn (QA) |
