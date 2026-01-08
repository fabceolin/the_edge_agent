# Test Design: Story TEA-STREAM-002.6

**Date:** 2026-01-08
**Designer:** Quinn (Test Architect)
**Story:** TEA-STREAM-002.6 - Integration Testing & Documentation
**Epic:** TEA-STREAM-002 - ZeroMQ Transport

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 42 |
| **Unit tests** | 6 (14%) |
| **Integration tests** | 10 (24%) |
| **E2E tests** | 16 (38%) |
| **Manual/Review tests** | 10 (24%) |
| **Priority Distribution** | P0: 10, P1: 18, P2: 10, P3: 4 |

### Strategy Rationale

This story is unique in that it primarily focuses on **E2E validation** and **documentation** of previously implemented features (TEA-STREAM-002.1 through 002.5). The test design reflects this:

1. **E2E-Heavy**: AC1-AC7 explicitly require E2E and performance tests
2. **Documentation Validation**: AC8-AC15 require manual review of documentation quality
3. **Cross-Platform Focus**: Windows compatibility is critical and requires platform-specific test paths
4. **Performance Baseline**: Establishing benchmarks for future regression detection

---

## Test Scenarios by Acceptance Criteria

### AC1: E2E test - PUB/SUB broadcast with 3 subscribers

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 002.6-E2E-001 | E2E | P0 | Verify all 3 subscribers receive all published messages | Critical pattern validation - broadcast must be complete |
| 002.6-E2E-002 | E2E | P1 | Verify message ordering is preserved across subscribers | ZeroMQ ordering guarantees validation |
| 002.6-E2E-003 | E2E | P1 | Verify late-joining subscriber behavior (message loss expected) | Document expected behavior |
| 002.6-INT-001 | Integration | P1 | Verify PUB socket binding and SUB socket connection | Transport layer validation |
| 002.6-UNIT-001 | Unit | P2 | Verify topic filtering logic (if implemented) | Pure logic validation |

### AC2: E2E test - PUSH/PULL load balancing with 3 workers

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 002.6-E2E-004 | E2E | P0 | Verify total messages = sum of worker messages (no loss) | Data integrity critical |
| 002.6-E2E-005 | E2E | P0 | Verify no duplicate messages across workers | Load balancing correctness |
| 002.6-E2E-006 | E2E | P1 | Verify approximate fair distribution across workers | Pattern correctness validation |
| 002.6-E2E-007 | E2E | P2 | Verify behavior when one worker is slow | Resilience testing |
| 002.6-INT-002 | Integration | P1 | Verify PUSH socket connects to all PULL workers | Transport layer validation |

### AC3: E2E test - REQ/REP request-response pattern

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 002.6-E2E-008 | E2E | P0 | Verify synchronous request-response cycle completes | Core pattern validation |
| 002.6-E2E-009 | E2E | P1 | Verify multiple sequential requests work correctly | Pattern stability |
| 002.6-E2E-010 | E2E | P2 | Verify timeout handling on slow response | Error handling |
| 002.6-INT-003 | Integration | P1 | Verify REQ/REP socket pairing | Transport layer validation |

### AC4: E2E test - TCP transport across processes

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 002.6-E2E-011 | E2E | P0 | Verify TCP communication between separate processes | Cross-process critical |
| 002.6-E2E-012 | E2E | P1 | Verify TCP works on localhost (127.0.0.1) | Platform-neutral transport |
| 002.6-INT-004 | Integration | P1 | Verify port binding and connection establishment | Transport setup validation |
| 002.6-UNIT-002 | Unit | P2 | Verify endpoint URL parsing (tcp://host:port) | Input validation |

### AC5: E2E test - Mixed Unix pipes + ZeroMQ in same workflow

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 002.6-E2E-013 | E2E | P0 | Verify hybrid workflow with both transports completes | Critical integration test |
| 002.6-E2E-014 | E2E | P1 | Verify data integrity across transport boundaries | Data flow validation |
| 002.6-E2E-015 | E2E | P2 | Verify error in one transport doesn't corrupt other | Isolation testing |
| 002.6-INT-005 | Integration | P1 | Verify transport selection logic in workflow engine | Engine decision validation |

**Platform Note**: AC5 tests must skip or use TCP-only on Windows (no Unix pipes support).

### AC6: Performance test - 10K msg/s throughput verified

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 002.6-PERF-001 | E2E | P0 | Verify PUB/SUB achieves ≥10K msg/s throughput | Critical performance requirement |
| 002.6-PERF-002 | E2E | P1 | Verify PUSH/PULL achieves ≥10K msg/s throughput | Pattern parity validation |
| 002.6-INT-006 | Integration | P2 | Benchmark Unix pipe baseline for comparison | Comparative analysis |
| 002.6-UNIT-003 | Unit | P3 | Benchmark message serialization overhead | Component profiling |

### AC7: Performance test - Latency under 1ms for local IPC

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 002.6-PERF-003 | E2E | P0 | Verify IPC round-trip latency <1ms (Linux/macOS) | Critical performance requirement |
| 002.6-PERF-004 | E2E | P1 | Verify TCP localhost latency (all platforms) | Cross-platform baseline |
| 002.6-INT-007 | Integration | P2 | Measure inproc:// latency for same-process comparison | Overhead analysis |

**Platform Note**: IPC latency tests must use TCP on Windows.

### AC8: Documentation section in YAML_REFERENCE.md

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 002.6-DOC-001 | Manual | P1 | Verify ZeroMQ section added to table of contents | Documentation completeness |
| 002.6-DOC-002 | Manual | P1 | Verify quick start example is runnable | Example accuracy |
| 002.6-DOC-003 | Manual | P2 | Verify link to detailed zeromq.md works | Cross-reference validation |

### AC9: New doc - docs/shared/yaml-reference/zeromq.md

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 002.6-DOC-004 | Manual | P0 | Verify all configuration options documented | API documentation completeness |
| 002.6-DOC-005 | Manual | P1 | Verify all patterns have diagrams | Visual documentation |
| 002.6-DOC-006 | Manual | P1 | Verify code examples are syntactically correct | Example quality |
| 002.6-INT-008 | Integration | P2 | Validate all YAML examples in docs parse correctly | Documentation accuracy |

### AC10: Example - examples/yaml/zeromq_pubsub.yaml

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 002.6-EXAMPLE-001 | Integration | P1 | Verify zeromq_pubsub.yaml parses without errors | Example validity |
| 002.6-EXAMPLE-002 | E2E | P1 | Verify zeromq_pubsub.yaml executes successfully | Example runnability |

### AC11: Example - examples/yaml/zeromq_pipeline.yaml

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 002.6-EXAMPLE-003 | Integration | P1 | Verify zeromq_pipeline.yaml parses without errors | Example validity |
| 002.6-EXAMPLE-004 | E2E | P1 | Verify zeromq_pipeline.yaml executes successfully | Example runnability |

### AC12: Example - examples/yaml/zeromq_distributed.yaml

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 002.6-EXAMPLE-005 | Integration | P2 | Verify zeromq_distributed.yaml parses without errors | Example validity |
| 002.6-EXAMPLE-006 | Manual | P2 | Verify zeromq_distributed.yaml README explains multi-machine setup | Documentation clarity |

### AC13: Troubleshooting guide for common ZeroMQ issues

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 002.6-DOC-007 | Manual | P1 | Verify common error messages are documented | Developer experience |
| 002.6-DOC-008 | Manual | P2 | Verify HWM tuning guidance is accurate | Performance guidance |
| 002.6-UNIT-004 | Unit | P3 | Verify error message strings match documentation | Consistency check |

### AC14: Migration guide from Unix pipes to ZeroMQ

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 002.6-DOC-009 | Manual | P1 | Verify before/after YAML examples are valid | Migration accuracy |
| 002.6-DOC-010 | Manual | P2 | Verify step-by-step process is complete | Process completeness |
| 002.6-UNIT-005 | Unit | P3 | Verify both migration examples parse correctly | Example validity |

### AC15: Cross-platform compatibility matrix documented

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 002.6-DOC-011 | Manual | P0 | Verify compatibility matrix matches actual behavior | Critical accuracy |
| 002.6-DOC-012 | Manual | P1 | Verify Windows IPC limitation is clearly documented | Platform guidance |
| 002.6-UNIT-006 | Unit | P2 | Validate matrix entries against platform detection code | Code-doc alignment |

---

## Platform-Specific Test Matrix

| Test Category | Linux | macOS | Windows |
|---------------|-------|-------|---------|
| IPC tests | ✅ Run | ✅ Run | ⏭️ Skip (use TCP) |
| TCP tests | ✅ Run | ✅ Run | ✅ Run |
| inproc tests | ✅ Run | ✅ Run | ✅ Run (same-process only) |
| Unix pipe tests | ✅ Run | ✅ Run | ⏭️ Skip |
| Mixed transport tests | ✅ Run | ✅ Run | ⚠️ TCP + inproc only |

---

## Risk Coverage

| Risk | Mitigated By |
|------|--------------|
| Message loss in PUB/SUB | 002.6-E2E-001, 002.6-E2E-003 |
| Duplicate processing in PUSH/PULL | 002.6-E2E-005 |
| Cross-platform incompatibility | Platform-specific test matrix, 002.6-DOC-011 |
| Performance regression | 002.6-PERF-001 through PERF-004 |
| Documentation inaccuracy | 002.6-INT-008, 002.6-EXAMPLE-* tests |
| Example files don't work | 002.6-EXAMPLE-001 through 006 |

---

## Recommended Execution Order

### CI Pipeline Execution

1. **P0 Unit tests** - Fail fast on logic errors
2. **P0 Integration tests** - Validate component interactions
3. **P0 E2E tests** - Critical path validation
4. **P0 Performance tests** - Throughput/latency gates
5. **P1 tests in order** (Unit → Integration → E2E)
6. **P2+ tests** as time permits

### Manual Review Checklist

Before release, manually verify:

- [ ] All documentation renders correctly
- [ ] All examples are runnable with `tea run`
- [ ] Troubleshooting guide covers actual error messages
- [ ] Platform matrix matches tested behavior
- [ ] Migration guide works end-to-end

---

## Test Environment Requirements

### E2E Tests

```yaml
requirements:
  - pyzmq installed (pip install pyzmq)
  - pytest-benchmark for performance tests
  - Multi-process capability
  - Network ports available for TCP tests

platform_specific:
  linux:
    - IPC socket directory writable (/tmp)
  macos:
    - IPC socket directory writable
  windows:
    - TCP port range available (49152-65535)
    - No IPC/Unix pipe tests
```

### Performance Tests

```yaml
benchmarks:
  throughput:
    target: 10000  # msg/s
    warm_up: 1000  # messages
    iterations: 5
  latency:
    target: 0.001  # 1ms
    samples: 1000
    percentile: 95  # p95 must be <1ms
```

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-STREAM-002.6
  date: 2026-01-08
  designer: Quinn
  scenarios_total: 42
  by_level:
    unit: 6
    integration: 10
    e2e: 16
    manual: 10
  by_priority:
    p0: 10
    p1: 18
    p2: 10
    p3: 4
  coverage_gaps: []
  platform_considerations:
    - Windows requires TCP-only for cross-process tests
    - IPC latency tests skip on Windows
    - Unix pipe tests skip on Windows
  dependencies:
    - TEA-STREAM-002.1 (Transport Abstraction)
    - TEA-STREAM-002.2 (PUB/SUB Pattern)
    - TEA-STREAM-002.3 (PUSH/PULL Pattern)
    - TEA-STREAM-002.4 (REQ/REP Pattern)
    - TEA-STREAM-002.5 (YAML Integration)
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (E2E-heavy per story requirements)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Platform-specific handling documented
- [x] Performance targets clearly specified

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-STREAM-002.6-test-design-20260108.md
P0 tests identified: 10
Total scenarios: 42
```
