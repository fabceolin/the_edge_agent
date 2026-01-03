# Test Design: Story TEA-STREAM-001.2

**Date**: 2026-01-02
**Designer**: Quinn (Test Architect)
**Story**: Pipe Executor Extension
**Epic**: TEA-STREAM-001 - Unix Pipe Streaming

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 24 |
| **Unit tests** | 6 (25%) |
| **Integration tests** | 16 (67%) |
| **E2E tests** | 2 (8%) |

### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| **P0** | 11 | Critical functionality, data integrity, no-crash guarantees |
| **P1** | 10 | Core behavior, backward compat, expected defaults |
| **P2** | 3 | Edge cases, timeout behavior, advanced scenarios |

### Test Level Justification

- **Unit (25%)**: API signatures, pure validation logic, exit code handling
- **Integration (67%)**: Heavy focus due to pipe/FD wiring, process coordination, async IO complexity
- **E2E (8%)**: Full workflow validation with hybrid state+stream scenarios

---

## Test Scenarios by Acceptance Criteria

### AC1: StreamRegistry Integration in execute()

**Criterion**: `ProcessExecutor` accepts `StreamRegistry` in execute()

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-001 | Unit | P0 | `execute()` accepts `stream_registry` parameter | Method signature validation - API contract |
| 001.2-UNIT-002 | Unit | P1 | `execute()` works without `stream_registry` (backward compat) | Backward compatibility must not break existing usage |
| 001.2-UNIT-003 | Unit | P1 | Empty `stream_registry` uses standard execution path | Defensive behavior - empty registry should not trigger stream logic |

**Given-When-Then Scenarios:**

```gherkin
# 001.2-UNIT-001
Given a ProcessExecutor instance
When execute() is called with stream_registry=StreamRegistry()
Then the method accepts the parameter without TypeError

# 001.2-UNIT-002
Given a ProcessExecutor instance
When execute() is called without stream_registry parameter
Then execution completes using ProcessPoolExecutor path
And results match legacy behavior

# 001.2-UNIT-003
Given a ProcessExecutor instance
And a StreamRegistry with no channels
When execute() is called with the empty registry
Then _execute_standard() is invoked (not _execute_with_streams)
```

---

### AC2: Pipe Wiring to stdout/stdin

**Criterion**: Processes launched with stdout/stdin connected to pipes

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-INT-001 | Integration | P0 | Process stdout wired to channel write_fd | Core streaming functionality - data must flow |
| 001.2-INT-002 | Integration | P0 | Process stdin wired to channel read_fd | Core streaming functionality - input must reach process |
| 001.2-INT-003 | Integration | P1 | Unwired stdin defaults to `subprocess.PIPE` | Expected default behavior when no stream configured |
| 001.2-INT-004 | Integration | P1 | Unwired stdout defaults to `subprocess.PIPE` | Expected default behavior when no stream configured |

**Given-When-Then Scenarios:**

```gherkin
# 001.2-INT-001
Given a StreamRegistry with channel "data-pipe" (write_fd=5)
And a flow configured with streams.stdout="data-pipe"
When the process is launched
Then the process stdout is connected to fd 5
And data written to stdout appears on the pipe

# 001.2-INT-002
Given a StreamRegistry with channel "input-pipe" (read_fd=3)
And a flow configured with streams.stdin="input-pipe"
When the process is launched
Then the process stdin reads from fd 3
And data on the pipe is available as stdin input

# 001.2-INT-003
Given a StreamRegistry with no stdin channel for the flow
When the process is launched
Then stdin is set to asyncio.subprocess.PIPE
And process can still receive input via communicate()

# 001.2-INT-004
Given a StreamRegistry with no stdout channel for the flow
When the process is launched
Then stdout is set to asyncio.subprocess.PIPE
And output can be captured via communicate()
```

---

### AC3: Config-driven Wiring

**Criterion**: Pipe wiring follows `streams:` configuration

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-INT-005 | Integration | P0 | `streams.stdin` config determines input source | Config must be respected - contract validation |
| 001.2-INT-006 | Integration | P0 | `streams.stdout` config determines output dest | Config must be respected - contract validation |
| 001.2-INT-007 | Integration | P1 | Unknown channel name in config raises error | Fail-fast on misconfiguration - better than silent failure |

**Given-When-Then Scenarios:**

```gherkin
# 001.2-INT-005
Given a flow with streams: {stdin: "producer-output"}
And a StreamRegistry with channel "producer-output"
When execute() is called
Then the process stdin reads from "producer-output" channel

# 001.2-INT-006
Given a flow with streams: {stdout: "consumer-input"}
And a StreamRegistry with channel "consumer-input"
When execute() is called
Then the process stdout writes to "consumer-input" channel

# 001.2-INT-007
Given a flow with streams: {stdin: "nonexistent-channel"}
And a StreamRegistry without that channel
When execute() is called
Then a ValueError is raised with message containing "nonexistent-channel"
```

---

### AC4: SIGPIPE Handling

**Criterion**: SIGPIPE handled gracefully (consumer exits early)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-INT-008 | Integration | P0 | Consumer exits early doesn't crash producer | Critical stability - SIGPIPE must not terminate process |
| 001.2-INT-009 | Integration | P1 | Producer continues after consumer exit (if writing) | Graceful degradation - producer should handle broken pipe |
| 001.2-UNIT-004 | Unit | P1 | SIGPIPE handler installed before pipe creation | Ordering guarantee - handler must exist before any pipe write |

**Given-When-Then Scenarios:**

```gherkin
# 001.2-INT-008
Given a producer process writing to pipe "data-stream"
And a consumer process reading from pipe "data-stream"
When the consumer process exits early (before producer finishes)
Then the producer process does not crash with SIGPIPE
And execute() returns results without exception

# 001.2-INT-009
Given a producer process generating data to pipe
And a consumer that exits after reading 10 bytes
When the producer attempts to write more data
Then the producer receives EPIPE/BrokenPipeError
And the producer handles it gracefully (doesn't crash)

# 001.2-UNIT-004
Given a StreamRegistry with install_sigpipe_handler() method
When _execute_with_streams() is called
Then install_sigpipe_handler() is called BEFORE create_all_pipes()
```

---

### AC5: Deadlock Prevention

**Criterion**: Deadlock prevention via async pipe management

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-INT-010 | Integration | P0 | Large data (>64KB) doesn't deadlock | Pipe buffer (64KB on Linux) must not cause blocking |
| 001.2-INT-011 | Integration | P1 | Async IO prevents buffer-full blocking | Verify async reads prevent producer blockage |
| 001.2-INT-012 | Integration | P2 | Timeout kills stuck processes | Backstop safety - processes must not hang forever |

**Given-When-Then Scenarios:**

```gherkin
# 001.2-INT-010
Given a producer that writes 1MB of data to stdout pipe
And a consumer that reads all data from stdin pipe
When execute() is called with both flows
Then all data transfers successfully without deadlock
And execution completes within 30 seconds

# 001.2-INT-011
Given a producer writing data faster than consumer reads
And pipe buffer becoming full
When async event loop manages the pipe
Then producer is not blocked indefinitely
And data eventually flows through

# 001.2-INT-012
Given a consumer process that hangs (infinite loop)
And timeout_seconds=5 in config
When execute() is called
Then the stuck process is killed after 5 seconds
And result contains error="Process timed out after 5s"
And exit_code=-1
```

---

### AC6: Hybrid State+Stream Flow

**Criterion**: State still serialized/passed alongside streams

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-INT-013 | Integration | P0 | State passed via TEA_STATE env var | State must be accessible to subprocess |
| 001.2-INT-014 | Integration | P1 | Stream data flows while state is separate | Separation of concerns - state != stream data |
| 001.2-INT-015 | Integration | P2 | Large state (>128KB) falls back to temp file | Memory safety for env var size limits |
| 001.2-E2E-001 | E2E | P0 | Full workflow with state and streams | End-to-end validation of hybrid scenario |

**Given-When-Then Scenarios:**

```gherkin
# 001.2-INT-013
Given a flow with state={"user_id": 42, "mode": "stream"}
And streams: {stdout: "output-pipe"}
When the subprocess is launched
Then TEA_STATE environment variable contains JSON '{"user_id": 42, "mode": "stream"}'
And the subprocess can parse and use the state

# 001.2-INT-014
Given flow A writing stream data "STREAM_DATA" to pipe
And flow A having state={"source": "A"}
And flow B reading from the same pipe
And flow B having state={"target": "B"}
When execute() runs both flows
Then stream data transfers between processes
And each process sees only its own state (not each other's)

# 001.2-INT-015
Given a flow with state containing 200KB of data
When the subprocess is launched
Then state is written to a temp file
And TEA_STATE_FILE environment variable points to the temp file
And subprocess reads state from file

# 001.2-E2E-001
Given a YAML workflow with:
  - producer node (generates data, has state={count: 100})
  - transform node (reads stdin, writes stdout, has state={multiplier: 2})
  - consumer node (reads stdin, has state={format: "json"})
When the workflow executes with streaming enabled
Then data flows: producer → transform → consumer
And each node accesses its state correctly
And final output reflects all transformations
```

---

### AC7: Exit Codes

**Criterion**: Process exit codes captured and reported

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-005 | Unit | P0 | ParallelFlowResult includes exit_code field | API contract - exit code must be accessible |
| 001.2-INT-016 | Integration | P1 | Non-zero exit code captured correctly | Error detection - failed processes must be identifiable |
| 001.2-INT-017 | Integration | P1 | Timeout sets exit_code to -1 | Consistent timeout representation |
| 001.2-UNIT-006 | Unit | P0 | Exit code 0 for successful process | Success case must return 0 |

**Given-When-Then Scenarios:**

```gherkin
# 001.2-UNIT-005
Given a ParallelFlowResult class
When instantiated with exit_code=0
Then result.exit_code == 0
And exit_code is accessible as an attribute

# 001.2-INT-016
Given a flow that runs "exit 42"
When execute() collects results
Then result.exit_code == 42
And result.error is None (non-zero exit != exception)

# 001.2-INT-017
Given a flow that sleeps for 60 seconds
And config.timeout_seconds = 2
When execute() runs and times out
Then result.exit_code == -1
And result.error contains "timed out"

# 001.2-UNIT-006
Given a flow that completes successfully (exit 0)
When execute() collects results
Then result.exit_code == 0
And result.state contains the output
```

---

## Additional Scenarios (Edge Cases & Robustness)

### Error Handling & Cleanup

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-INT-018 | Integration | P0 | FD leak prevention on process crash | Resource safety - pipes must be cleaned up |
| 001.2-E2E-002 | E2E | P1 | Multi-process pipeline (A→B→C) works | Real-world chain validation |

**Given-When-Then Scenarios:**

```gherkin
# 001.2-INT-018
Given 3 flows with pipe connections
And process B crashes mid-execution
When execute() handles the crash
Then all file descriptors are closed
And no FD leaks occur (verified via /proc/self/fd or lsof)
And cleanup() is called in finally block

# 001.2-E2E-002
Given a 3-stage pipeline:
  - Stage A: generates lines 1-100
  - Stage B: filters even numbers
  - Stage C: sums the values
When execute() runs with full streaming
Then Stage A stdout → Stage B stdin (pipe 1)
And Stage B stdout → Stage C stdin (pipe 2)
And final result contains sum of even numbers 1-100
```

---

## Risk Coverage Matrix

| Risk | Impact | Mitigating Tests |
|------|--------|------------------|
| Deadlock on large data | High | 001.2-INT-010, 001.2-INT-011 |
| FD leak if process crashes | High | 001.2-INT-018 |
| SIGPIPE crash | High | 001.2-INT-008, 001.2-INT-009, 001.2-UNIT-004 |
| State too large for env var | Medium | 001.2-INT-015 |
| Backward compatibility break | Medium | 001.2-UNIT-002, 001.2-UNIT-003 |
| Config misconfiguration | Medium | 001.2-INT-007 |

---

## Test Implementation Notes

### Test File Structure

```
python/tests/test_pipe_executor.py
├── TestProcessExecutorSignature (Unit)
│   ├── test_execute_accepts_stream_registry
│   ├── test_execute_without_stream_registry
│   ├── test_execute_empty_registry_uses_standard
│   ├── test_sigpipe_handler_ordering
│   ├── test_parallel_flow_result_has_exit_code
│   └── test_exit_code_zero_for_success
├── TestPipeWiring (Integration)
│   ├── test_stdout_wired_to_channel
│   ├── test_stdin_wired_to_channel
│   ├── test_unwired_stdin_defaults_pipe
│   ├── test_unwired_stdout_defaults_pipe
│   ├── test_stdin_config_determines_source
│   ├── test_stdout_config_determines_dest
│   └── test_unknown_channel_raises_error
├── TestSigpipeHandling (Integration)
│   ├── test_consumer_early_exit_no_crash
│   └── test_producer_continues_after_broken_pipe
├── TestDeadlockPrevention (Integration)
│   ├── test_large_data_no_deadlock
│   ├── test_async_io_prevents_blocking
│   └── test_timeout_kills_stuck_process
├── TestHybridStateStream (Integration)
│   ├── test_state_via_env_var
│   ├── test_stream_data_separate_from_state
│   └── test_large_state_temp_file
├── TestExitCodes (Integration)
│   ├── test_nonzero_exit_code_captured
│   └── test_timeout_exit_code_minus_one
├── TestCleanup (Integration)
│   └── test_fd_cleanup_on_crash
└── TestE2EWorkflows (E2E)
    ├── test_full_workflow_state_and_streams
    └── test_multi_process_pipeline
```

### Testing Helpers Required

```python
# Helper subprocess scripts for testing
# python/tests/fixtures/echo_state.py - prints TEA_STATE
# python/tests/fixtures/large_writer.py - writes >64KB
# python/tests/fixtures/slow_reader.py - reads slowly (deadlock test)
# python/tests/fixtures/early_exit.py - exits after N bytes
# python/tests/fixtures/infinite_hang.py - hangs forever (timeout test)
```

### Mock Requirements

| Test Category | Mock Strategy |
|---------------|---------------|
| Unit tests | Mock subprocess creation, no real processes |
| Integration | Real processes with test scripts, in-memory pipes |
| E2E | Full subprocess execution, real file descriptors |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on API contract)
   - 001.2-UNIT-001, 001.2-UNIT-005, 001.2-UNIT-006

2. **P0 Integration tests** (core functionality)
   - 001.2-INT-001, 001.2-INT-002, 001.2-INT-005, 001.2-INT-006
   - 001.2-INT-008, 001.2-INT-010, 001.2-INT-013, 001.2-INT-018

3. **P0 E2E tests** (full validation)
   - 001.2-E2E-001

4. **P1 tests** (expected behavior)
   - All P1 scenarios in order

5. **P2 tests** (edge cases, as time permits)
   - 001.2-INT-012, 001.2-INT-015

---

## Gate YAML Block

```yaml
test_design:
  date: "2026-01-02"
  story_id: "TEA-STREAM-001.2"
  scenarios_total: 24
  by_level:
    unit: 6
    integration: 16
    e2e: 2
  by_priority:
    p0: 11
    p1: 10
    p2: 3
  coverage_gaps: []
  all_acs_covered: true
  risk_mitigations:
    - risk: "Deadlock on large data"
      tests: ["001.2-INT-010", "001.2-INT-011"]
    - risk: "FD leak on crash"
      tests: ["001.2-INT-018"]
    - risk: "SIGPIPE crash"
      tests: ["001.2-INT-008", "001.2-INT-009", "001.2-UNIT-004"]
    - risk: "State size limit"
      tests: ["001.2-INT-015"]
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-STREAM-001.2-test-design-20260102.md
P0 tests identified: 11
Total test scenarios: 24
Acceptance criteria coverage: 7/7 (100%)
```

---

## Quality Checklist

- [x] Every AC has test coverage (7/7 ACs covered)
- [x] Test levels are appropriate (heavy integration for pipe wiring)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (P0 = data integrity, crash prevention)
- [x] Test IDs follow naming convention (`{epic}.{story}-{LEVEL}-{SEQ}`)
- [x] Scenarios are atomic and independent
- [x] Risk mitigations mapped to tests
- [x] Given-When-Then format for all scenarios
