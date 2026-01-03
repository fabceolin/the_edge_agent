# Test Design: Story TEA-STREAM-001.3 - Stream Broadcasting (Tee)

Date: 2026-01-02
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios**: 18
- **Unit tests**: 10 (56%)
- **Integration tests**: 8 (44%)
- **E2E tests**: 0 (0%)
- **Priority distribution**: P0: 7, P1: 8, P2: 3

### Test Strategy Rationale

This story implements low-level Unix pipe broadcasting via FIFOs and the `tee` command. The testing approach heavily favors unit and integration tests because:

1. **No user-facing UI** - Pure backend infrastructure, no E2E needed
2. **System-level operations** - FIFO creation, process management require integration tests
3. **Complex state management** - TeeOrchestrator lifecycle requires thorough unit testing
4. **Data integrity critical** - Broadcast correctness is foundational for streaming workflows

### Test Environment Requirements

| Requirement | Details |
|-------------|---------|
| OS | Linux/macOS with FIFO support |
| Commands | `mkfifo`, `tee` (or Python fallback) |
| Python | asyncio support required |
| Temp Space | Writable temp directory for FIFOs |

---

## Test Scenarios by Acceptance Criteria

### AC1: `stream_mode: broadcast` duplicates stream to N consumers

**Requirement**: Broadcast stream must deliver identical data to all N consumers in correct order.

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.3-INT-001 | Integration | P0 | Verify 2 consumers receive identical data from single producer | Minimum viable broadcast; validates core tee functionality |
| 001.3-INT-002 | Integration | P0 | Verify 5 consumers receive identical data from single producer | Validates scaling beyond 2; catches FIFO ordering issues |
| 001.3-INT-003 | Integration | P1 | Verify data ordering preserved across all consumers | Critical for streaming semantics; order must match producer |
| 001.3-INT-004 | Integration | P1 | Verify large data chunks (64KB+) broadcast correctly | Tests buffer handling; catches truncation bugs |

**Test Design Details:**

```python
# 001.3-INT-001: 2 consumers receive identical data
async def test_broadcast_to_two_consumers():
    """
    Given: A TeeOrchestrator with 2-consumer broadcast
    When: Producer writes 1000 bytes of data
    Then: Both consumers receive identical 1000 bytes
    And: Data matches producer output exactly
    """

# 001.3-INT-002: 5 consumers receive identical data
async def test_broadcast_to_five_consumers():
    """
    Given: A TeeOrchestrator with 5-consumer broadcast
    When: Producer writes varied data chunks
    Then: All 5 consumers receive identical data
    And: Byte counts match across all consumers
    """

# 001.3-INT-003: Order preserved
async def test_broadcast_preserves_order():
    """
    Given: A broadcast to 3 consumers
    When: Producer writes numbered lines [1, 2, 3, ... 100]
    Then: Each consumer receives lines in exact order [1, 2, 3, ... 100]
    """

# 001.3-INT-004: Large chunks
async def test_broadcast_large_chunks():
    """
    Given: A broadcast to 2 consumers
    When: Producer writes 64KB data chunk
    Then: Both consumers receive complete 64KB
    And: No data corruption or truncation
    """
```

---

### AC2: Uses named FIFOs (`mkfifo`) for multi-consumer support

**Requirement**: System must create proper named pipes (FIFOs) for inter-process communication.

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.3-UNIT-001 | Unit | P0 | Verify `mkfifo` creates named pipe with correct permissions | Core FIFO creation; verifies OS integration |
| 001.3-UNIT-002 | Unit | P1 | Verify FIFO paths use unique names per broadcast | Prevents collisions between concurrent broadcasts |
| 001.3-UNIT-003 | Unit | P1 | Verify FIFO creation failure triggers rollback of partial FIFOs | Prevents resource leaks on partial failure |
| 001.3-UNIT-004 | Unit | P2 | Verify FIFO names include broadcast identifier | Aids debugging and cleanup identification |

**Test Design Details:**

```python
# 001.3-UNIT-001: mkfifo creates named pipe
def test_mkfifo_creates_named_pipe():
    """
    Given: A valid work directory
    When: create_broadcast("test", fd, 2) is called
    Then: 2 FIFO files exist in work directory
    And: Files have S_IFIFO type (verified via stat)
    """

# 001.3-UNIT-002: Unique names
def test_fifo_paths_unique_per_broadcast():
    """
    Given: TeeOrchestrator instance
    When: create_broadcast("a", fd, 2) and create_broadcast("b", fd, 2) called
    Then: All 4 FIFO paths are unique
    And: Paths contain broadcast name
    """

# 001.3-UNIT-003: Rollback on failure
def test_fifo_creation_rollback_on_failure(monkeypatch):
    """
    Given: mkfifo will fail on 3rd call (mocked)
    When: create_broadcast("test", fd, 5) is called
    Then: OSError is raised
    And: No FIFO files remain (rolled back)
    """

# 001.3-UNIT-004: Broadcast identifier in name
def test_fifo_names_include_broadcast_id():
    """
    Given: Broadcast named "mystream"
    When: create_broadcast creates FIFOs
    Then: All FIFO paths contain "mystream"
    """
```

---

### AC3: `TeeOrchestrator` manages FIFO lifecycle

**Requirement**: Orchestrator must properly manage creation, startup, shutdown, and cleanup of broadcast infrastructure.

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.3-UNIT-005 | Unit | P0 | Verify `create_broadcast` returns correct FIFO paths list | API contract; consumers need paths to connect |
| 001.3-UNIT-006 | Unit | P0 | Verify `start_broadcast` launches tee subprocess | Core functionality; validates process spawning |
| 001.3-INT-005 | Integration | P1 | Verify `stop_broadcast` terminates tee process cleanly | Prevents zombie processes; resource cleanup |
| 001.3-UNIT-007 | Unit | P1 | Verify context manager (`async with`) cleans up on exit | Ensures cleanup in all code paths |
| 001.3-UNIT-008 | Unit | P1 | Verify `get_fifo_paths` returns correct paths for named broadcast | API verification for path retrieval |
| 001.3-UNIT-009 | Unit | P2 | Verify error raised when starting non-existent broadcast | Error handling for invalid state |

**Test Design Details:**

```python
# 001.3-UNIT-005: create_broadcast returns paths
def test_create_broadcast_returns_fifo_paths():
    """
    Given: TeeOrchestrator instance
    When: create_broadcast("test", fd, 3) is called
    Then: Returns list of 3 Path objects
    And: All paths exist and are FIFOs
    """

# 001.3-UNIT-006: start_broadcast launches tee
async def test_start_broadcast_launches_tee_process():
    """
    Given: Broadcast "test" created with 2 consumers
    When: start_broadcast("test") is called (with consumers reading)
    Then: tee_process is not None
    And: Process is running (poll() returns None)
    """

# 001.3-INT-005: stop_broadcast terminates
async def test_stop_broadcast_terminates_tee():
    """
    Given: Running broadcast with active tee process
    When: stop_broadcast("test") is called
    Then: Process terminates within 5 seconds
    And: No zombie process remains
    """

# 001.3-UNIT-007: Context manager cleanup
async def test_context_manager_cleanup():
    """
    Given: TeeOrchestrator used with async with
    When: Context exits (normally or via exception)
    Then: All FIFOs are removed
    And: Work directory is cleaned up
    """

# 001.3-UNIT-008: get_fifo_paths
def test_get_fifo_paths_returns_correct_paths():
    """
    Given: Broadcast "stream1" created with 4 consumers
    When: get_fifo_paths("stream1") is called
    Then: Returns same 4 paths from create_broadcast
    """

# 001.3-UNIT-009: Error on non-existent
async def test_start_nonexistent_broadcast_raises():
    """
    Given: TeeOrchestrator with no broadcasts created
    When: start_broadcast("unknown") is called
    Then: KeyError is raised with message "Unknown broadcast: unknown"
    """
```

---

### AC4: Automatic FIFO cleanup on workflow completion

**Requirement**: FIFOs and temp directories must be cleaned up automatically to prevent resource leaks.

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.3-INT-006 | Integration | P0 | Verify `cleanup_all` removes all FIFOs and work directory | Prevents temp file accumulation |
| 001.3-INT-007 | Integration | P1 | Verify cleanup works after process crash simulation | Robustness; handles abnormal termination |

**Test Design Details:**

```python
# 001.3-INT-006: cleanup_all removes everything
def test_cleanup_all_removes_fifos_and_workdir():
    """
    Given: TeeOrchestrator with 2 broadcasts (3 consumers each)
    When: cleanup_all() is called
    Then: All 6 FIFO files are removed
    And: Work directory is removed
    And: broadcasts dict is empty
    """

# 001.3-INT-007: Cleanup after crash
async def test_cleanup_after_process_crash():
    """
    Given: Broadcast with tee process running
    When: tee process is killed externally (simulated crash)
    Then: cleanup_all_async() completes without error
    And: All resources are freed
    """
```

---

### AC5: Handles slow consumers without blocking producer

**Requirement**: Fast producer must not block when one consumer is slow; async forwarding prevents deadlock.

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.3-INT-008 | Integration | P1 | Verify fast producer continues when one consumer is slow | Prevents workflow deadlock; critical for real-world use |
| 001.3-INT-009 | Integration | P2 | Verify Python fallback (`start_broadcast_with_python`) handles slow consumers | Fallback path validation |

**Test Design Details:**

```python
# 001.3-INT-008: Fast producer doesn't block
async def test_fast_producer_not_blocked_by_slow_consumer():
    """
    Given: Broadcast to 2 consumers
    And: Consumer A reads immediately
    And: Consumer B sleeps 2 seconds before reading
    When: Producer writes 10KB data
    Then: Data transmission completes in < 1 second
    And: Both consumers eventually receive all data
    """

# 001.3-INT-009: Python fallback handles slow consumers
async def test_python_fallback_slow_consumers():
    """
    Given: Broadcast using start_broadcast_with_python()
    And: One consumer is slow
    When: Producer sends data
    Then: Fast consumer receives data without waiting
    """
```

---

### AC6: Error when consumer count exceeds system FIFO limit

**Requirement**: System must reject requests exceeding FIFO limits with clear error messages.

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.3-UNIT-010 | Unit | P0 | Verify error if consumer_count > MAX_FIFOS (256) | Prevents system resource exhaustion |
| 001.3-UNIT-011 | Unit | P1 | Verify error if consumer_count < 2 (requires at least 2 for broadcast) | Enforces correct usage; single consumer should use pipe |

**Test Design Details:**

```python
# 001.3-UNIT-010: Error on exceeding MAX_FIFOS
def test_error_when_exceeding_max_fifos():
    """
    Given: TeeOrchestrator with MAX_FIFOS = 256
    When: create_broadcast("test", fd, 300) is called
    Then: ValueError is raised
    And: Message contains "exceeds maximum 256"
    And: Suggests reducing parallel branches
    """

# 001.3-UNIT-011: Error on < 2 consumers
def test_error_when_less_than_two_consumers():
    """
    Given: TeeOrchestrator instance
    When: create_broadcast("test", fd, 1) is called
    Then: ValueError is raised
    And: Message suggests using direct pipe for single consumer
    """
```

---

## Risk Coverage Matrix

| Risk ID | Risk Description | Impact | Mitigating Tests |
|---------|------------------|--------|------------------|
| R1 | FIFO blocks on open (no reader) | High | 001.3-INT-008, 001.3-UNIT-006 |
| R2 | tee command not available | Medium | 001.3-INT-009 (Python fallback) |
| R3 | Slow consumer blocks others | Medium | 001.3-INT-008, 001.3-INT-009 |
| R4 | FIFO files left after crash | Low | 001.3-INT-006, 001.3-INT-007 |
| R5 | Resource exhaustion from too many FIFOs | Medium | 001.3-UNIT-010 |
| R6 | Data corruption in broadcast | High | 001.3-INT-001 through 001.3-INT-004 |

---

## Test Dependency Graph

```
                    001.3-UNIT-001 (FIFO creation)
                           │
           ┌───────────────┼───────────────┐
           ▼               ▼               ▼
    001.3-UNIT-002   001.3-UNIT-003   001.3-UNIT-004
    (unique names)   (rollback)       (naming)
           │
           ▼
    001.3-UNIT-005 (create_broadcast API)
           │
           ├───────────────┬───────────────┐
           ▼               ▼               ▼
    001.3-UNIT-006   001.3-UNIT-008   001.3-UNIT-010
    (start_broadcast) (get_paths)     (limit check)
           │                               │
           ▼                               ▼
    001.3-INT-001 through 001.3-INT-004   001.3-UNIT-011
    (broadcast correctness)                (min consumers)
           │
           ▼
    001.3-INT-005 (stop_broadcast)
           │
           ├───────────────┐
           ▼               ▼
    001.3-INT-006    001.3-UNIT-007
    (cleanup_all)    (context manager)
           │
           ▼
    001.3-INT-007 (crash cleanup)
           │
           └───────────────┐
                           ▼
              001.3-INT-008 (slow consumer)
                           │
                           ▼
              001.3-INT-009 (Python fallback)
```

---

## Recommended Execution Order

### Phase 1: P0 Unit Tests (Fail Fast)
1. 001.3-UNIT-001 - FIFO creation fundamentals
2. 001.3-UNIT-005 - create_broadcast API contract
3. 001.3-UNIT-006 - start_broadcast process spawning
4. 001.3-UNIT-010 - MAX_FIFOS limit enforcement

### Phase 2: P0 Integration Tests
5. 001.3-INT-001 - 2 consumers broadcast
6. 001.3-INT-002 - 5 consumers broadcast
7. 001.3-INT-006 - Complete cleanup verification

### Phase 3: P1 Tests (Core Functionality)
8. 001.3-UNIT-002 - Unique naming
9. 001.3-UNIT-003 - Rollback on failure
10. 001.3-UNIT-007 - Context manager
11. 001.3-UNIT-008 - get_fifo_paths API
12. 001.3-UNIT-011 - Minimum consumers validation
13. 001.3-INT-003 - Order preservation
14. 001.3-INT-004 - Large chunk handling
15. 001.3-INT-005 - Process termination
16. 001.3-INT-007 - Crash recovery
17. 001.3-INT-008 - Slow consumer handling

### Phase 4: P2 Tests (Time Permitting)
18. 001.3-UNIT-004 - Naming convention
19. 001.3-UNIT-009 - Error on non-existent
20. 001.3-INT-009 - Python fallback

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-STREAM-001.3
  date: 2026-01-02
  designer: Quinn
  scenarios_total: 18
  by_level:
    unit: 10
    integration: 8
    e2e: 0
  by_priority:
    p0: 7
    p1: 8
    p2: 3
  coverage_gaps: []
  risk_coverage:
    high_impact_risks: 2
    mitigated: 2
  recommended_test_file: python/tests/test_stream_broadcast.py
```

---

## Notes for Implementation

1. **Test Fixtures**: Create async fixtures for TeeOrchestrator that handle cleanup automatically
2. **Mock tee**: For unit tests, mock subprocess calls; for integration, use real processes
3. **Temp directories**: Use `pytest-tmp-path` or `tempfile.TemporaryDirectory`
4. **Async testing**: Use `pytest-asyncio` for all async test methods
5. **Platform considerations**: Skip tests on Windows (no FIFO support); mark with `@pytest.mark.skipif`

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-STREAM-001.3-test-design-20260102.md
P0 tests identified: 7
P1 tests identified: 8
P2 tests identified: 3
Story file: docs/stories/TEA-STREAM-001.3-stream-broadcasting.md
```
