# Story TEA-STREAM-002.6: Integration Testing & Documentation

## Status: Ready for Development

## Story

**As a** workflow developer,
**I want** comprehensive documentation and examples for ZeroMQ streaming,
**So that** I can learn and implement distributed streaming patterns effectively.

## Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | E2E test: PUB/SUB broadcast with 3 subscribers | E2E test |
| AC2 | E2E test: PUSH/PULL load balancing with 3 workers | E2E test |
| AC3 | E2E test: REQ/REP request-response pattern | E2E test |
| AC4 | E2E test: TCP transport across processes | E2E test |
| AC5 | E2E test: Mixed Unix pipes + ZeroMQ in same workflow | E2E test |
| AC6 | Performance test: 10K msg/s throughput verified | Performance test |
| AC7 | Performance test: Latency under 1ms for local IPC | Performance test |
| AC8 | Documentation section in `YAML_REFERENCE.md` | Manual review |
| AC9 | New doc: `docs/shared/yaml-reference/zeromq.md` | Manual review |
| AC10 | Example: `examples/yaml/zeromq_pubsub.yaml` | Manual review |
| AC11 | Example: `examples/yaml/zeromq_pipeline.yaml` | Manual review |
| AC12 | Example: `examples/yaml/zeromq_distributed.yaml` | Manual review |
| AC13 | Troubleshooting guide for common ZeroMQ issues | Manual review |
| AC14 | Migration guide from Unix pipes to ZeroMQ | Manual review |
| AC15 | Cross-platform compatibility matrix documented | Manual review |

## Tasks / Subtasks

- [ ] **Task 1: Create E2E test suite** (AC: 1-5)
  - [ ] Create `python/tests/e2e/` directory if it doesn't exist
  - [ ] Create `python/tests/e2e/__init__.py`
  - [ ] Create `python/tests/e2e/test_zeromq_e2e.py`
  - [ ] Implement PUB/SUB broadcast test with 3 subscribers
  - [ ] Implement PUSH/PULL load balancing test with 3 workers
  - [ ] Implement REQ/REP request-response test
  - [ ] Implement TCP cross-process test
  - [ ] Implement mixed transport test (Unix + ZeroMQ)
  - [ ] Add proper cleanup and timeout handling

- [ ] **Task 2: Create performance benchmarks** (AC: 6, 7)
  - [ ] Create `python/tests/benchmarks/` directory if it doesn't exist
  - [ ] Create `python/tests/benchmarks/__init__.py`
  - [ ] Create `python/tests/benchmarks/conftest.py` with pytest-benchmark fixtures
  - [ ] Create `python/tests/benchmarks/test_zeromq_perf.py`
  - [ ] Benchmark PUB/SUB throughput (target: 10K msg/s)
  - [ ] Benchmark PUSH/PULL throughput
  - [ ] Benchmark IPC latency (target: <1ms)
  - [ ] Benchmark TCP latency (localhost)
  - [ ] Compare with Unix pipe baseline
  - [ ] Add pytest-benchmark markers

- [ ] **Task 3: Create ZeroMQ reference documentation** (AC: 9)
  - [ ] Create `docs/shared/yaml-reference/zeromq.md`
  - [ ] Document all configuration options
  - [ ] Document all patterns with diagrams
  - [ ] Document protocol selection guide
  - [ ] Add code examples for each pattern
  - [ ] Document socket options and tuning

- [ ] **Task 4: Update YAML_REFERENCE.md** (AC: 8)
  - [ ] Add ZeroMQ section to table of contents
  - [ ] Add quick start example
  - [ ] Link to detailed zeromq.md reference
  - [ ] Update stream_mode documentation

- [ ] **Task 5: Create example YAML files** (AC: 10, 11, 12)
  - [ ] Create `examples/yaml/zeromq_pubsub.yaml` - Event broadcast
  - [ ] Create `examples/yaml/zeromq_pipeline.yaml` - Work distribution
  - [ ] Create `examples/yaml/zeromq_distributed.yaml` - Multi-machine
  - [ ] Create `examples/yaml/zeromq_hybrid.yaml` - Mixed transports
  - [ ] Add README for each example
  - [ ] Ensure examples are runnable

- [ ] **Task 6: Create troubleshooting guide** (AC: 13)
  - [ ] Document common error messages and fixes
  - [ ] Document port binding issues
  - [ ] Document firewall configuration
  - [ ] Document HWM tuning
  - [ ] Document slow subscriber handling
  - [ ] Add debugging tips

- [ ] **Task 7: Create migration guide** (AC: 14)
  - [ ] Document when to migrate from Unix pipes
  - [ ] Provide step-by-step migration process
  - [ ] Document breaking changes (if any)
  - [ ] Add before/after YAML examples
  - [ ] Document rollback procedure

- [ ] **Task 8: Document platform compatibility** (AC: 15)
  - [ ] Create compatibility matrix table
  - [ ] Test and document Linux behavior
  - [ ] Test and document macOS behavior
  - [ ] Test and document Windows behavior
  - [ ] Document protocol recommendations per platform

- [ ] **Task 9: Add CI/CD integration** (AC: 1-7)
  - [ ] Modify `.github/workflows/python-tests.yaml` to include ZeroMQ tests
  - [ ] Add `pyzmq` to CI test dependencies (optional)
  - [ ] Configure Windows CI for `inproc://` and TCP tests (no `ipc://`)
  - [ ] Configure Linux/macOS CI for `ipc://` and TCP tests
  - [ ] Add pytest markers: `@pytest.mark.zeromq`, `@pytest.mark.e2e`, `@pytest.mark.benchmark`
  - [ ] Add CI job to run benchmarks without failing on regression (informational)
  - [ ] Ensure tests skip gracefully with `pytest.importorskip("zmq")`

## Dev Notes

### Relevant Source Tree

```
docs/
├── shared/
│   ├── YAML_REFERENCE.md           # MODIFIED - Add ZeroMQ section
│   └── yaml-reference/
│       ├── streams.md              # MODIFIED - Add transport selection
│       └── zeromq.md               # NEW - Full ZeroMQ reference
│
examples/yaml/
├── zeromq_pubsub.yaml              # NEW
├── zeromq_pipeline.yaml            # NEW
├── zeromq_distributed.yaml         # NEW
└── zeromq_hybrid.yaml              # NEW

python/tests/
├── e2e/
│   └── test_zeromq_e2e.py          # NEW
└── benchmarks/
    └── test_zeromq_perf.py         # NEW
```

### Dependencies

- **Story**: TEA-STREAM-002.1 through 002.5 (all implementation stories)
- **Blocked by**: TEA-STREAM-002.5 (YAML Integration) - all implementation must be complete before E2E tests

### E2E Test Structure

```python
# python/tests/e2e/test_zeromq_e2e.py
import pytest
import subprocess
import time
from pathlib import Path

pytestmark = pytest.mark.e2e

@pytest.fixture
def examples_dir():
    return Path(__file__).parent.parent.parent.parent / "examples" / "yaml"

class TestZeroMQPubSub:
    """E2E tests for PUB/SUB pattern."""

    def test_broadcast_to_3_subscribers(self, examples_dir):
        """Verify all 3 subscribers receive all messages."""
        # Launch producer and 3 consumers
        # Verify message counts match
        pass

class TestZeroMQPushPull:
    """E2E tests for PUSH/PULL pattern."""

    def test_load_balance_3_workers(self, examples_dir):
        """Verify work is distributed across workers."""
        # Launch producer and 3 workers
        # Verify total messages = sum of worker messages
        # Verify no duplicates
        pass

class TestZeroMQReqRep:
    """E2E tests for REQ/REP pattern."""

    def test_request_response(self, examples_dir):
        """Verify request-response cycle works."""
        pass

class TestZeroMQTCP:
    """E2E tests for TCP transport."""

    def test_tcp_across_processes(self, examples_dir):
        """Verify TCP works between separate processes."""
        pass

class TestMixedTransport:
    """E2E tests for mixed Unix + ZeroMQ."""

    def test_hybrid_workflow(self, examples_dir):
        """Verify mixed transport workflow works."""
        pass
```

### Performance Benchmark Structure

```python
# python/tests/benchmarks/test_zeromq_perf.py
import pytest

pytestmark = pytest.mark.benchmark

@pytest.mark.parametrize("transport,protocol", [
    ("zeromq", "ipc"),
    ("zeromq", "tcp"),
    ("unix", None),
])
def test_throughput(benchmark, transport, protocol):
    """Measure messages per second."""
    def send_10k_messages():
        # Send 10,000 messages
        pass

    result = benchmark(send_10k_messages)

    # Assert minimum throughput
    if transport == "zeromq":
        assert result.stats.mean < 1.0  # 10K msgs in <1 second

def test_ipc_latency(benchmark):
    """Measure round-trip latency for IPC."""
    def roundtrip():
        # Send and receive one message
        pass

    result = benchmark(roundtrip)
    assert result.stats.mean < 0.001  # <1ms latency
```

### Example YAML: PUB/SUB

```yaml
# examples/yaml/zeromq_pubsub.yaml
name: zeromq-pubsub-example
description: Demonstrates PUB/SUB pattern for event broadcasting

settings:
  parallel:
    strategy: process
    streams:
      enabled: true
      transport: zeromq
      protocol: ipc
      pattern: pub_sub
      zeromq:
        high_water_mark: 1000

state_schema:
  events: list
  subscriber_results: list

nodes:
  - name: event_publisher
    run: |
      import sys
      import time

      events = ["user_login", "page_view", "purchase", "logout"]
      for i, event in enumerate(events):
        print(f"{event}:{i}", file=sys.stdout, flush=True)
        time.sleep(0.1)

      return {"events_published": len(events)}
    streams:
      stdout: events

  - name: logger
    run: |
      import sys

      logged = []
      for line in sys.stdin:
        logged.append(f"[LOG] {line.strip()}")

      return {"logged": logged}
    streams:
      stdin: events

  - name: analytics
    run: |
      import sys

      counts = {}
      for line in sys.stdin:
        event = line.strip().split(":")[0]
        counts[event] = counts.get(event, 0) + 1

      return {"analytics": counts}
    streams:
      stdin: events

edges:
  - from: __start__
    to: event_publisher
  - from: event_publisher
    to: [logger, analytics]
    parallel: true
    parallel_strategy: process
    stream_mode: broadcast
    fan_in: summarize
  - from: summarize
    to: __end__
```

### Documentation Structure: zeromq.md

```markdown
# ZeroMQ Transport

## Overview
- What is ZeroMQ
- Why use ZeroMQ over Unix pipes
- When to use each pattern

## Configuration
- Global settings
- Per-node overrides
- Environment variables

## Patterns

### PUB/SUB (Broadcast)
- Use case
- Configuration
- Example
- Diagram

### PUSH/PULL (Load Balance)
- Use case
- Configuration
- Example
- Diagram

### REQ/REP (RPC)
- Use case
- Configuration
- Example
- Diagram

## Transport Protocols
- IPC vs TCP
- Platform considerations
- Performance characteristics

## Socket Options
- high_water_mark
- linger
- timeouts

## Troubleshooting
- Common errors
- Debugging tips
- Performance tuning

## Migration from Unix Pipes
- Step-by-step guide
- Breaking changes
- Rollback
```

### Platform Compatibility Matrix

| Feature | Linux | macOS | Windows |
|---------|-------|-------|---------|
| IPC (`ipc://`) | Yes | Yes | **No** |
| inproc (`inproc://`) | Yes | Yes | Yes (same-process only) |
| TCP (`tcp://`) | Yes | Yes | Yes |
| PUB/SUB | Yes | Yes | Yes |
| PUSH/PULL | Yes | Yes | Yes |
| REQ/REP | Yes | Yes | Yes |
| Unix pipes | Yes | Yes | No |
| Mixed transport | Yes | Yes | Partial |
| Cross-process local | IPC or TCP | IPC or TCP | **TCP only** |

**Windows Important Note**: `inproc://` only works within the same process (thread-to-thread). For cross-process communication on Windows, you **must** use `tcp://127.0.0.1:port`. This is a ZeroMQ platform limitation.

### Testing

| Test Category | Location | Framework |
|--------------|----------|-----------|
| E2E tests | `python/tests/e2e/test_zeromq_e2e.py` | pytest |
| Benchmarks | `python/tests/benchmarks/test_zeromq_perf.py` | pytest-benchmark |

**Test Standards**:
- E2E tests use real subprocesses
- Benchmarks run in isolated environment
- All tests skip gracefully if pyzmq not installed
- Windows tests use inproc/TCP only

## Definition of Done

- [ ] All E2E tests pass on Linux, macOS, Windows
- [ ] Performance benchmarks meet targets (10K msg/s, <1ms latency)
- [ ] `zeromq.md` documentation complete
- [ ] YAML_REFERENCE.md updated with ZeroMQ section
- [ ] All 4 example YAML files created and runnable
- [ ] Troubleshooting guide covers common issues
- [ ] Migration guide provides clear steps
- [ ] Platform compatibility matrix complete
- [ ] CI/CD includes ZeroMQ tests
- [ ] Documentation reviewed for clarity

## Dev Agent Record

_To be populated during implementation._

## QA Results

**Test Design Assessment:** 2026-01-08 by Quinn (Test Architect)

### Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 42 |
| Unit tests | 6 (14%) |
| Integration tests | 10 (24%) |
| E2E tests | 16 (38%) |
| Manual/Review tests | 10 (24%) |
| P0 (Critical) | 10 |
| P1 (High) | 18 |
| P2 (Medium) | 10 |
| P3 (Low) | 4 |

### Key Findings

1. **E2E-Heavy Design**: This story is appropriately E2E-focused (AC1-AC7 explicitly require E2E/performance tests)
2. **Platform Risk**: Windows compatibility requires special handling (no IPC, TCP-only for cross-process)
3. **Documentation Quality**: 10 manual review scenarios ensure documentation accuracy
4. **Performance Gates**: Clear targets (10K msg/s throughput, <1ms IPC latency)

### Critical P0 Tests

- PUB/SUB broadcast completeness (all subscribers receive all messages)
- PUSH/PULL data integrity (no loss, no duplicates)
- REQ/REP pattern completion
- TCP cross-process communication
- Mixed transport workflow completion
- Throughput benchmark (≥10K msg/s)
- IPC latency benchmark (<1ms)
- Documentation configuration completeness
- Compatibility matrix accuracy

### Platform-Specific Considerations

| Platform | Test Adjustments |
|----------|------------------|
| Linux/macOS | Full test suite |
| Windows | Skip IPC tests, use TCP for cross-process, skip Unix pipe tests |

### Assessment Document

Full test design: `docs/qa/assessments/TEA-STREAM-002.6-test-design-20260108.md`

---

## QA Notes

**Date:** 2026-01-08
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Category | Coverage | Notes |
|----------|----------|-------|
| **E2E Tests (AC1-AC7)** | Complete | 16 scenarios covering all messaging patterns |
| **Performance Tests (AC6-AC7)** | Complete | Throughput (10K msg/s) and latency (<1ms) gates defined |
| **Documentation (AC8-AC15)** | Complete | 10 manual review scenarios for doc quality |
| **Example Validation** | Complete | 6 scenarios validate example YAML files parse and execute |

**Total Acceptance Criteria:** 15
**Test Scenarios per AC:** ~2.8 average (appropriately weighted by risk)

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **Windows Platform Compatibility** | HIGH | No IPC support; requires TCP-only fallback path. CI must have separate Windows test matrix with IPC/Unix pipe tests skipped. |
| **Late-Joining Subscriber Message Loss** | MEDIUM | PUB/SUB pattern inherently loses messages for late joiners. Document as expected behavior, not a bug. |
| **Slow Worker Resilience** | MEDIUM | PUSH/PULL may back up if one worker is slow. Test scenario 002.6-E2E-007 validates behavior under load imbalance. |
| **Cross-Process TCP Port Conflicts** | LOW | E2E tests must use ephemeral ports or unique port ranges to avoid CI conflicts. |
| **Documentation Drift** | LOW | Documentation examples must be validated by integration tests (002.6-INT-008) to prevent drift from implementation. |

### Recommended Test Scenarios

#### Critical Path (Must Pass for Release)

1. **002.6-E2E-001**: PUB/SUB broadcast to 3 subscribers - all receive all messages
2. **002.6-E2E-004**: PUSH/PULL no message loss across 3 workers
3. **002.6-E2E-005**: PUSH/PULL no duplicate messages
4. **002.6-E2E-008**: REQ/REP synchronous cycle completes
5. **002.6-E2E-011**: TCP cross-process communication
6. **002.6-E2E-013**: Mixed Unix + ZeroMQ workflow completion
7. **002.6-PERF-001**: Throughput ≥10,000 msg/s
8. **002.6-PERF-003**: IPC latency <1ms (p95)
9. **002.6-DOC-004**: All configuration options documented
10. **002.6-DOC-011**: Compatibility matrix accuracy verified

#### High Priority Scenarios

- Message ordering preservation (002.6-E2E-002)
- Late-joining subscriber behavior documentation (002.6-E2E-003)
- Fair distribution across workers (002.6-E2E-006)
- TCP localhost functionality (002.6-E2E-012)
- All 4 example YAML files parse and execute (002.6-EXAMPLE-001 through 004)

#### Manual Review Checklist

- [ ] ZeroMQ section in YAML_REFERENCE.md is complete
- [ ] zeromq.md has diagrams for all 3 patterns
- [ ] Troubleshooting guide covers common errors
- [ ] Migration guide has working before/after examples
- [ ] Platform compatibility matrix is accurate

### Concerns and Blockers

| Type | Description | Status |
|------|-------------|--------|
| **Blocker** | Story depends on TEA-STREAM-002.1 through 002.5 completion | Must verify all dependencies are Done |
| **Concern** | Windows CI environment may not have pyzmq pre-installed | Add conditional install in CI workflow |
| **Concern** | Performance benchmarks may be flaky in CI due to shared resources | Run benchmarks as informational, not gating |
| **Recommendation** | Add pytest markers (@pytest.mark.zeromq, @pytest.mark.e2e, @pytest.mark.benchmark) | Enables selective test execution |

### CI/CD Recommendations

```yaml
# Suggested CI matrix structure
strategy:
  matrix:
    os: [ubuntu-latest, macos-latest, windows-latest]
    include:
      - os: ubuntu-latest
        skip_patterns: []
      - os: macos-latest
        skip_patterns: []
      - os: windows-latest
        skip_patterns: ["ipc", "unix_pipe", "mixed_transport"]
```

### Definition of Done Readiness

| Criterion | Ready | Notes |
|-----------|-------|-------|
| All E2E tests defined | Yes | 16 scenarios |
| Performance targets specified | Yes | 10K msg/s, <1ms |
| Platform matrix documented | Yes | Linux/macOS/Windows |
| Manual review checklist | Yes | 5 items |
| CI integration specified | Yes | Task 9 covers CI setup |

**Overall Assessment:** Story is well-defined for QA. Recommend proceeding to implementation once dependencies (TEA-STREAM-002.1-002.5) are verified complete.

### Quality Gate Status: **PASS**

This is the **capstone story** for TEA-STREAM-002 epic. E2E tests here validate the entire ZeroMQ transport implementation.

### Acceptance Criteria Traceability

| AC | Tests | Status |
|----|-------|--------|
| AC1 | E2E-001, E2E-002, E2E-003, INT-001 | ✅ Covered |
| AC2 | E2E-004, E2E-005, E2E-006, E2E-007, INT-002 | ✅ Covered (Critical) |
| AC3 | E2E-008, E2E-009, E2E-010, INT-003 | ✅ Covered |
| AC4 | E2E-011, E2E-012, E2E-013, INT-004 | ✅ Covered |
| AC5 | E2E-014, E2E-015, INT-005 | ✅ Covered |
| AC6 | PERF-001, PERF-002, INT-006 | ✅ Covered (Critical) |
| AC7 | PERF-003, PERF-004, INT-007 | ✅ Covered (Critical) |
| AC8 | DOC-001, DOC-002, DOC-003 | ✅ Covered |
| AC9 | DOC-004, DOC-005, DOC-006, INT-008 | ✅ Covered |
| AC10 | EXAMPLE-001, EXAMPLE-002 | ✅ Covered |
| AC11 | EXAMPLE-003, EXAMPLE-004 | ✅ Covered |
| AC12 | EXAMPLE-005, EXAMPLE-006 | ✅ Covered |
| AC13 | DOC-007, DOC-008 | ✅ Covered |
| AC14 | DOC-009, DOC-010 | ✅ Covered |
| AC15 | DOC-011, DOC-012 | ✅ Covered |

### Pre-Implementation Checklist

- [ ] Verify TEA-STREAM-002.1 through 002.5 are ALL complete
- [ ] Create `python/tests/e2e/` and `python/tests/benchmarks/` directories
- [ ] Install pytest-benchmark for performance tests
- [ ] Plan CI matrix for Windows-specific test skipping

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Initial story creation | Sarah (PO) |
| 2026-01-08 | 0.2 | Added directory creation subtasks, CI workflow path, Windows platform clarifications per validation | Sarah (PO) |
| 2026-01-08 | 0.3 | Added QA Results and comprehensive QA Notes | Quinn (QA) |
| 2026-01-08 | 0.4 | Added Quality Gate Status and AC traceability | Quinn (QA) |
