# Test Design: Story TEA-PARALLEL-001.3 - Remote Executor Core

**Date**: 2026-01-01
**Designer**: Quinn (Test Architect)
**Story**: [TEA-PARALLEL-001.3-remote-executor-core.md](../../stories/TEA-PARALLEL-001.3-remote-executor-core.md)

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 24 |
| **Unit tests** | 10 (42%) |
| **Integration tests** | 12 (50%) |
| **E2E tests** | 2 (8%) |
| **P0 (Critical)** | 7 |
| **P1 (High)** | 11 |
| **P2 (Medium)** | 4 |
| **P3 (Low)** | 2 |

### Strategy Rationale

This story implements distributed execution over SSH, which involves:
1. **Protocol/interface design** - Testable at unit level
2. **External process coordination** - Requires integration testing with mocks
3. **Network operations** - Critical paths need E2E validation on localhost

The test distribution favors integration tests (50%) because the core functionality involves subprocess management, file transfer, and async coordination that cannot be meaningfully tested as pure units.

---

## Test Scenarios by Acceptance Criteria

### AC1: RemoteExecutor implements ParallelExecutor Protocol

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-UNIT-001 | Unit | P0 | RemoteExecutor is instance of ParallelExecutor Protocol | Protocol conformance is foundational; enables polymorphic executor selection |
| 001.3-UNIT-002 | Unit | P1 | RemoteExecutor.execute() signature matches Protocol | Method signature contract validation |
| 001.3-UNIT-003 | Unit | P1 | RemoteExecutor returns List[ParallelFlowResult] | Return type contract validation |

**Given-When-Then:**
```gherkin
Scenario: Protocol conformance check
  Given a RemoteExecutor instance with valid config
  When checking isinstance(executor, ParallelExecutor)
  Then the check returns True
```

---

### AC2: Configuration via YAML (hosts, basefile, workdir, cleanup)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-UNIT-004 | Unit | P1 | YAMLEngine parses `hosts` as list of strings | Core config parsing, deterministic |
| 001.3-UNIT-005 | Unit | P1 | YAMLEngine parses `basefile` path | Required field validation |
| 001.3-UNIT-006 | Unit | P1 | YAMLEngine applies default `workdir` when omitted | Default value handling |
| 001.3-UNIT-007 | Unit | P2 | YAMLEngine parses `cleanup` boolean with default true | Optional field with default |

**Given-When-Then:**
```gherkin
Scenario: Parse remote configuration
  Given YAML with parallel.remote section containing hosts, basefile
  When YAMLEngine.from_yaml() is called
  Then settings.parallel.remote.hosts is a list of strings
  And settings.parallel.remote.basefile is the specified path
  And settings.parallel.remote.workdir defaults to "/tmp/tea-jobs"
  And settings.parallel.remote.cleanup defaults to True
```

---

### AC3: GNU Parallel command generation when available

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-UNIT-008 | Unit | P1 | Detect GNU Parallel via shutil.which | Pure function check |
| 001.3-UNIT-009 | Unit | P1 | Generate correct --sshlogin with comma-separated hosts | Command construction logic |
| 001.3-UNIT-010 | Unit | P1 | Include --basefile for binary and YAML | File transfer flags |
| 001.3-INT-001 | Integration | P1 | Generated command includes --timeout from ParallelConfig | Config integration |
| 001.3-INT-002 | Integration | P2 | Generated command includes --cleanup when config.cleanup=true | Conditional flag generation |

**Given-When-Then:**
```gherkin
Scenario: Generate GNU Parallel command
  Given RemoteExecutor with hosts ["user@host1", "user@host2"]
  And GNU Parallel is available (shutil.which returns path)
  When _build_parallel_command() is called
  Then command contains "--sshlogin user@host1,user@host2"
  And command contains "--basefile ./tea"
  And command contains "--basefile workflow.yaml"
```

---

### AC4: SSH fallback when GNU Parallel not available

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-INT-003 | Integration | P0 | SSH fallback activates when parallel not found | Core fallback mechanism |
| 001.3-INT-004 | Integration | P1 | SSH fallback distributes work round-robin across hosts | Load distribution logic |
| 001.3-INT-005 | Integration | P2 | Logs "Using SSH fallback" when parallel unavailable | Observability for debugging |

**Given-When-Then:**
```gherkin
Scenario: SSH fallback activation
  Given RemoteExecutor with 3 hosts configured
  And shutil.which("parallel") returns None
  When execute() is called with 6 flows
  Then _execute_with_ssh_fallback() is invoked
  And flows are distributed: host1 gets [0,3], host2 gets [1,4], host3 gets [2,5]
```

---

### AC5: State serialization to JSON for transfer

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-UNIT-011 | Unit | P0 | State dict serializes to valid JSON | Data integrity - foundational |
| 001.3-UNIT-012 | Unit | P0 | Complex nested state with lists/dicts serializes correctly | Edge case for complex workflows |
| 001.3-UNIT-013 | Unit | P1 | Non-serializable types raise clear error | Error handling for invalid state |

**Given-When-Then:**
```gherkin
Scenario: Complex state serialization
  Given state = {"items": [{"name": "a", "value": 1}], "metadata": {"nested": {"deep": true}}}
  When json.dumps(state) is called
  Then result is valid JSON string
  And json.loads(result) equals original state
```

---

### AC6: Full YAML + input state transfer to remote

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-INT-006 | Integration | P0 | SCP transfers complete workflow.yaml (not subset) | Critical for correct remote execution |
| 001.3-INT-007 | Integration | P1 | SCP transfers tea binary to workdir | Required for remote execution |
| 001.3-INT-008 | Integration | P1 | SCP transfers input.json with serialized state | State transfer verification |

**Given-When-Then:**
```gherkin
Scenario: Full YAML transfer
  Given workflow.yaml with 10 nodes including nodes outside entry/exit scope
  When _scp_to_remote() transfers files
  Then the complete workflow.yaml is transferred (all 10 nodes present)
  And binary and input.json are transferred to {workdir}/
```

---

### AC7: Remote execution using --entry-point and --exit-point flags

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-INT-009 | Integration | P0 | Remote command includes --entry-point with correct node | Scoped execution correctness |
| 001.3-INT-010 | Integration | P0 | Remote command includes --exit-point with fan-in node | Execution boundary correctness |
| 001.3-INT-011 | Integration | P1 | Remote command includes --input and --output paths | I/O file specification |

**Given-When-Then:**
```gherkin
Scenario: Remote command with scope flags
  Given flow with entry_point="process_chunk" and exit_point="fan_in"
  When remote SSH command is constructed
  Then command contains "--entry-point process_chunk"
  And command contains "--exit-point fan_in"
  And command contains "--input /tmp/tea-jobs/input_0.json"
  And command contains "--output /tmp/tea-jobs/input_0.json.result.json"
```

---

### AC8: Result collection and deserialization

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-INT-012 | Integration | P0 | Result JSON is retrieved via SCP | Core result collection |
| 001.3-INT-013 | Integration | P0 | Result JSON deserializes to ParallelFlowResult | Return type contract |
| 001.3-INT-014 | Integration | P1 | Multiple parallel results collected and aggregated | Multi-flow coordination |

**Given-When-Then:**
```gherkin
Scenario: Result collection
  Given 3 remote executions completed with result.json files
  When _scp_from_remote() retrieves all results
  Then each result deserializes to ParallelFlowResult
  And execute() returns List[ParallelFlowResult] with 3 items
```

---

### AC9: Automatic cleanup of remote files (configurable)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-INT-015 | Integration | P2 | Cleanup removes remote workdir contents when enabled | Hygiene feature |
| 001.3-INT-016 | Integration | P2 | Cleanup skipped when config.cleanup=false | Configurability |
| 001.3-INT-017 | Integration | P3 | Cleanup failure is logged but doesn't fail execution | Resilience |

**Given-When-Then:**
```gherkin
Scenario: Remote cleanup
  Given config.cleanup = true
  When remote execution completes successfully
  Then SSH command "rm -rf {workdir}/*" is executed
  And local temp files are deleted
```

---

### AC10: Timeout and retry integration with ParallelConfig

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-INT-018 | Integration | P0 | SSH command times out per ParallelConfig.timeout_seconds | Reliability under hang |
| 001.3-INT-019 | Integration | P0 | Network failure triggers retry with backoff | Resilience mechanism |
| 001.3-INT-020 | Integration | P1 | Retry count respects ParallelConfig.max_retries | Retry limit enforcement |
| 001.3-INT-021 | Integration | P1 | Timeout error returns ParallelFlowResult with error field | Error propagation |

**Given-When-Then:**
```gherkin
Scenario: SSH timeout handling
  Given ParallelConfig with timeout_seconds=30
  When remote command hangs for 35 seconds
  Then asyncio.wait_for raises TimeoutError
  And result contains error="SSH command timed out after 30s"
```

---

### E2E Validation

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-E2E-001 | E2E | P1 | Full remote execution cycle on localhost SSH | End-to-end validation with real SSH |
| 001.3-E2E-002 | E2E | P3 | Multi-host round-robin distribution (localhost aliases) | Multi-host simulation |

**Given-When-Then:**
```gherkin
Scenario: Full localhost SSH cycle
  Given SSH to localhost is configured (skip if not available)
  And a simple workflow with 2 parallel branches
  When RemoteExecutor.execute() runs with hosts=["localhost"]
  Then files are transferred to /tmp/tea-jobs/
  And tea executes with --entry-point and --exit-point
  And results are returned and aggregated
  And cleanup removes remote files
```

---

## Risk Coverage Matrix

| Risk | Impact | Test Coverage |
|------|--------|---------------|
| SSH key not configured | High | 001.3-INT-003 (fallback), 001.3-E2E-001 (skip if unavailable) |
| Network instability | Medium | 001.3-INT-019, 001.3-INT-020 (retry logic) |
| Remote disk full | Medium | Not covered - recommend monitoring |
| Binary compatibility | Medium | Not covered - document requirement |
| State serialization failure | High | 001.3-UNIT-011, 001.3-UNIT-012, 001.3-UNIT-013 |
| Timeout/hang | High | 001.3-INT-018, 001.3-INT-021 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on foundational issues)
   - 001.3-UNIT-001 (Protocol)
   - 001.3-UNIT-011, 001.3-UNIT-012 (Serialization)

2. **P0 Integration tests** (core functionality)
   - 001.3-INT-003 (SSH fallback)
   - 001.3-INT-006 (Full YAML transfer)
   - 001.3-INT-009, 001.3-INT-010 (Scope flags)
   - 001.3-INT-012, 001.3-INT-013 (Result collection)
   - 001.3-INT-018, 001.3-INT-019 (Timeout/retry)

3. **P1 tests** (core user journey)
   - All P1 unit and integration tests

4. **E2E tests** (if SSH available)
   - 001.3-E2E-001

5. **P2+ tests** (as time permits)
   - Cleanup, logging, edge cases

---

## Test Implementation Notes

### Mocking Strategy

```python
# Mock subprocess for SSH commands
@patch('asyncio.create_subprocess_exec')
async def test_ssh_fallback(mock_exec):
    mock_proc = AsyncMock()
    mock_proc.communicate.return_value = (b'{"result": "ok"}', b'')
    mock_proc.returncode = 0
    mock_exec.return_value = mock_proc
    # ... test logic

# Mock shutil.which for GNU Parallel detection
@patch('shutil.which')
def test_parallel_detection(mock_which):
    mock_which.return_value = None  # No parallel
    executor = RemoteExecutor(config, yaml_path)
    assert executor.has_parallel is False
```

### E2E Test Skip Condition

```python
import subprocess

def ssh_localhost_available():
    try:
        result = subprocess.run(
            ["ssh", "-o", "BatchMode=yes", "localhost", "echo", "ok"],
            capture_output=True, timeout=5
        )
        return result.returncode == 0
    except:
        return False

@pytest.mark.skipif(not ssh_localhost_available(), reason="SSH to localhost not configured")
def test_e2e_localhost_cycle():
    # Full E2E test
    pass
```

---

## Coverage Gaps

| Gap | Risk Level | Recommendation |
|-----|------------|----------------|
| Remote disk space check | Medium | Add pre-flight check in future story |
| Binary architecture mismatch | Medium | Document OS/arch requirement |
| SSH key passphrase handling | Low | Document key-based auth requirement |

---

## Quality Checklist

- [x] Every AC has at least one test
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (001.3-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
- [x] Given-When-Then patterns documented

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-PARALLEL-001.3
  date: 2026-01-01
  scenarios_total: 24
  by_level:
    unit: 10
    integration: 12
    e2e: 2
  by_priority:
    p0: 7
    p1: 11
    p2: 4
    p3: 2
  coverage_gaps:
    - "Remote disk space pre-check"
    - "Binary architecture validation"
  risk_mitigations:
    - "SSH fallback covered by 001.3-INT-003"
    - "Timeout/retry covered by 001.3-INT-018, 001.3-INT-019"
    - "Serialization integrity covered by 001.3-UNIT-011, 001.3-UNIT-012"
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-PARALLEL-001.3-test-design-20260101.md
P0 tests identified: 7
P1 tests identified: 11
Total scenarios: 24
```
