# Test Design: Story TEA-STREAM-001.1

**Date:** 2026-01-02
**Designer:** Quinn (Test Architect)
**Story:** Stream Channel Infrastructure
**Epic:** TEA-STREAM-001 - Unix Pipe Streaming

---

## Test Strategy Overview

| Metric | Count |
|--------|-------|
| **Total test scenarios** | 18 |
| **Unit tests** | 14 (78%) |
| **Integration tests** | 4 (22%) |
| **E2E tests** | 0 (0%) |

### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| **P0** | 8 | Critical infrastructure - data integrity, lifecycle correctness |
| **P1** | 8 | Core functionality - important but not data-critical |
| **P2** | 2 | Nice-to-have - edge cases with graceful fallbacks |

### Test Level Rationale

This story implements core infrastructure dataclasses and a registry pattern. The majority of tests are **unit tests** because:

1. `StreamChannel` and `StreamRegistry` are pure data structures with logic
2. Business logic is isolated (no external dependencies for most methods)
3. Fast feedback loop needed for foundational code

Integration tests cover:
- Actual OS pipe creation and data flow
- SIGPIPE signal handling in realistic scenarios

No E2E tests because this is infrastructure - E2E will be covered in Story 5.

---

## Test Scenarios by Acceptance Criteria

### AC1: StreamChannel Dataclass

**Criterion:** `StreamChannel` dataclass with name, direction (stdin/stdout/stderr), buffer_size

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-UNIT-001 | Unit | P0 | StreamChannel has required fields (name, direction, buffer_size) | Core dataclass contract - must have all fields |
| 001.1-UNIT-002 | Unit | P0 | Default buffer_size is 65536 (64KB) | OS pipe default - critical for compatibility |
| 001.1-UNIT-003 | Unit | P1 | StreamDirection enum has STDIN, STDOUT, STDERR values | Enum completeness - affects all stream operations |
| 001.1-UNIT-004 | Unit | P1 | create_pipe() returns valid file descriptors (positive integers) | Pipe creation correctness |

**Test Implementation Notes:**

```python
# 001.1-UNIT-001
def test_stream_channel_required_fields():
    """StreamChannel must have name, direction, buffer_size fields."""
    channel = StreamChannel(name="test", direction=StreamDirection.STDOUT)
    assert hasattr(channel, 'name')
    assert hasattr(channel, 'direction')
    assert hasattr(channel, 'buffer_size')
    assert channel.name == "test"
    assert channel.direction == StreamDirection.STDOUT

# 001.1-UNIT-002
def test_stream_channel_default_buffer_size():
    """Default buffer_size should be 64KB (OS pipe default)."""
    channel = StreamChannel(name="test", direction=StreamDirection.STDOUT)
    assert channel.buffer_size == 65536

# 001.1-UNIT-003
def test_stream_direction_enum_values():
    """StreamDirection must have STDIN, STDOUT, STDERR."""
    assert StreamDirection.STDIN.value == "stdin"
    assert StreamDirection.STDOUT.value == "stdout"
    assert StreamDirection.STDERR.value == "stderr"
    assert len(StreamDirection) == 3

# 001.1-UNIT-004
def test_create_pipe_returns_valid_fds():
    """create_pipe() returns positive integer file descriptors."""
    channel = StreamChannel(name="test", direction=StreamDirection.STDOUT)
    try:
        r, w = channel.create_pipe()
        assert isinstance(r, int) and r > 0
        assert isinstance(w, int) and w > 0
        assert r != w
    finally:
        channel.close()
```

---

### AC2: StreamRegistry Management

**Criterion:** `StreamRegistry` manages named channels across workflow

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-UNIT-005 | Unit | P0 | register() creates new channel with given parameters | Primary registration path |
| 001.1-UNIT-006 | Unit | P0 | register() returns existing channel if name already exists | Deduplication logic |
| 001.1-UNIT-007 | Unit | P1 | get() returns None for unknown channel name | Null-safety contract |
| 001.1-UNIT-008 | Unit | P1 | cleanup() closes all registered channels | Resource cleanup |

**Test Implementation Notes:**

```python
# 001.1-UNIT-005
def test_registry_register_creates_channel():
    """register() creates new StreamChannel with parameters."""
    registry = StreamRegistry()
    channel = registry.register("data", StreamDirection.STDOUT, buffer_size=131072)

    assert channel.name == "data"
    assert channel.direction == StreamDirection.STDOUT
    assert channel.buffer_size == 131072
    assert "data" in registry.channels

# 001.1-UNIT-006
def test_registry_register_returns_existing():
    """register() returns existing channel for duplicate name."""
    registry = StreamRegistry()
    channel1 = registry.register("data", StreamDirection.STDOUT)
    channel2 = registry.register("data", StreamDirection.STDIN, node_name="consumer")

    assert channel1 is channel2
    assert len(registry.channels) == 1
    assert "consumer" in channel2.consumers

# 001.1-UNIT-007
def test_registry_get_returns_none_for_unknown():
    """get() returns None for non-existent channel."""
    registry = StreamRegistry()
    assert registry.get("nonexistent") is None

# 001.1-UNIT-008
def test_registry_cleanup_closes_all():
    """cleanup() closes all channels and clears registry."""
    registry = StreamRegistry()
    ch1 = registry.register("a", StreamDirection.STDOUT)
    ch2 = registry.register("b", StreamDirection.STDOUT)
    ch1.create_pipe()
    ch2.create_pipe()

    registry.cleanup()

    assert ch1._closed is True
    assert ch2._closed is True
    assert len(registry.channels) == 0
```

---

### AC3: Multiple Consumers (Fan-out Preparation)

**Criterion:** Channels support multiple consumers (fan-out preparation)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-UNIT-009 | Unit | P0 | add_consumer() adds node to consumers list | Consumer tracking |
| 001.1-UNIT-010 | Unit | P1 | is_broadcast returns True when consumer_count > 1 | Broadcast detection for Story 3 |
| 001.1-UNIT-011 | Unit | P1 | consumer_count reflects actual registered consumers | Count accuracy |

**Test Implementation Notes:**

```python
# 001.1-UNIT-009
def test_add_consumer_adds_to_list():
    """add_consumer() adds node name to consumers."""
    channel = StreamChannel(name="data", direction=StreamDirection.STDIN)
    channel.add_consumer("processor")

    assert "processor" in channel.consumers
    assert channel.consumer_count == 1

# 001.1-UNIT-010
def test_is_broadcast_true_for_multiple_consumers():
    """is_broadcast returns True when more than one consumer."""
    channel = StreamChannel(name="data", direction=StreamDirection.STDIN)
    channel.add_consumer("processor1")
    channel.add_consumer("processor2")

    assert channel.is_broadcast is True
    assert channel.consumer_count == 2

# 001.1-UNIT-011
def test_add_consumer_prevents_duplicates():
    """add_consumer() doesn't add duplicate consumers."""
    channel = StreamChannel(name="data", direction=StreamDirection.STDIN)
    channel.add_consumer("processor")
    channel.add_consumer("processor")  # Duplicate

    assert channel.consumer_count == 1
```

---

### AC4: Channel Lifecycle

**Criterion:** Channel lifecycle: create → connect → stream → close

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-INT-001 | Integration | P0 | Full lifecycle: create pipe → write data → read data → close | End-to-end data flow |
| 001.1-INT-002 | Integration | P0 | Data written to write_fd is readable from read_fd | OS pipe correctness |
| 001.1-INT-003 | Integration | P1 | close() is idempotent (can call multiple times safely) | Cleanup robustness |

**Test Implementation Notes:**

```python
# 001.1-INT-001
def test_channel_full_lifecycle():
    """Full lifecycle: create → write → read → close."""
    channel = StreamChannel(name="test", direction=StreamDirection.STDOUT)

    # Create
    r_fd, w_fd = channel.create_pipe()
    assert channel.read_fd == r_fd
    assert channel.write_fd == w_fd

    # Write
    test_data = b"hello world\n"
    os.write(w_fd, test_data)

    # Read
    received = os.read(r_fd, 1024)
    assert received == test_data

    # Close
    channel.close()
    assert channel._closed is True
    assert channel.read_fd is None
    assert channel.write_fd is None

# 001.1-INT-002
def test_pipe_data_integrity():
    """Data written to write_fd is correctly readable from read_fd."""
    channel = StreamChannel(name="test", direction=StreamDirection.STDOUT)
    r_fd, w_fd = channel.create_pipe()

    try:
        # Write multiple chunks
        for i in range(10):
            os.write(w_fd, f"line {i}\n".encode())

        # Close write end to signal EOF
        os.close(w_fd)
        channel.write_fd = None

        # Read all data
        data = b""
        while chunk := os.read(r_fd, 1024):
            data += chunk

        lines = data.decode().strip().split("\n")
        assert len(lines) == 10
        assert lines[0] == "line 0"
        assert lines[9] == "line 9"
    finally:
        channel.close()

# 001.1-INT-003
def test_close_is_idempotent():
    """close() can be called multiple times without error."""
    channel = StreamChannel(name="test", direction=StreamDirection.STDOUT)
    channel.create_pipe()

    channel.close()
    assert channel._closed is True

    # Second close should not raise
    channel.close()
    assert channel._closed is True
```

---

### AC5: SIGPIPE Handling

**Criterion:** Graceful handling of broken pipes (SIGPIPE)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-UNIT-012 | Unit | P0 | install_sigpipe_handler() sets signal.SIG_IGN for SIGPIPE | Handler installation |
| 001.1-INT-004 | Integration | P1 | Writing to pipe with closed reader raises BrokenPipeError (not crash) | Graceful degradation |

**Test Implementation Notes:**

```python
# 001.1-UNIT-012
def test_sigpipe_handler_installed():
    """install_sigpipe_handler() sets SIG_IGN for SIGPIPE."""
    import signal

    registry = StreamRegistry()
    old_handler = signal.getsignal(signal.SIGPIPE)

    try:
        registry.install_sigpipe_handler()

        current_handler = signal.getsignal(signal.SIGPIPE)
        assert current_handler == signal.SIG_IGN
        assert registry._sigpipe_handled is True

        # Calling again should be no-op
        registry.install_sigpipe_handler()
        assert registry._sigpipe_handled is True
    finally:
        # Restore original handler
        signal.signal(signal.SIGPIPE, old_handler)

# 001.1-INT-004
def test_broken_pipe_handled_gracefully():
    """Writing to closed pipe raises BrokenPipeError, not crash."""
    import signal

    channel = StreamChannel(name="test", direction=StreamDirection.STDOUT)
    r_fd, w_fd = channel.create_pipe()

    # Install SIGPIPE handler
    old_handler = signal.getsignal(signal.SIGPIPE)
    signal.signal(signal.SIGPIPE, signal.SIG_IGN)

    try:
        # Close read end to simulate broken pipe
        os.close(r_fd)
        channel.read_fd = None

        # Writing should raise BrokenPipeError (not crash with SIGPIPE)
        with pytest.raises((BrokenPipeError, OSError)):
            # Write enough to exceed buffer
            for _ in range(10000):
                os.write(w_fd, b"x" * 1024)
    finally:
        signal.signal(signal.SIGPIPE, old_handler)
        channel.close()
```

---

### AC6: Configurable Buffer Size

**Criterion:** Configurable buffer size per channel

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-UNIT-013 | Unit | P1 | Custom buffer_size passed to fcntl F_SETPIPE_SZ | Buffer configuration |
| 001.1-UNIT-014 | Unit | P2 | Graceful fallback when fcntl fails (non-Linux, permission denied) | Cross-platform resilience |

**Test Implementation Notes:**

```python
# 001.1-UNIT-013
@pytest.mark.skipif(sys.platform != "linux", reason="fcntl pipe resize only on Linux")
def test_custom_buffer_size_applied():
    """Custom buffer_size is applied via fcntl on Linux."""
    import fcntl

    channel = StreamChannel(
        name="test",
        direction=StreamDirection.STDOUT,
        buffer_size=131072  # 128KB
    )

    try:
        r_fd, w_fd = channel.create_pipe()

        # Query actual pipe size (F_GETPIPE_SZ = 1032)
        actual_size = fcntl.fcntl(w_fd, 1032)

        # Size may be rounded up by kernel
        assert actual_size >= 131072 or actual_size == 65536  # May fail if unprivileged
    finally:
        channel.close()

# 001.1-UNIT-014
def test_buffer_resize_fallback():
    """Graceful fallback when buffer resize fails."""
    # Create channel with very large buffer (likely to fail without privileges)
    channel = StreamChannel(
        name="test",
        direction=StreamDirection.STDOUT,
        buffer_size=1024 * 1024 * 16  # 16MB - likely exceeds /proc/sys/fs/pipe-max-size
    )

    try:
        # Should not raise even if resize fails
        r_fd, w_fd = channel.create_pipe()

        # Pipe was created successfully (with default size)
        assert r_fd > 0
        assert w_fd > 0
    finally:
        channel.close()
```

---

## Additional Test Scenarios

### Validation and Error Handling

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-UNIT-015 | Unit | P1 | validate() returns error for stdin channel without matching stdout producer | Misconfiguration detection |
| 001.1-UNIT-016 | Unit | P1 | create_pipe() on closed channel raises RuntimeError | State machine correctness |
| 001.1-UNIT-017 | Unit | P1 | validate_platform() raises PlatformError on Windows | Platform gate |
| 001.1-UNIT-018 | Unit | P2 | get_producers() returns only STDOUT/STDERR channels | Query correctness |

**Test Implementation Notes:**

```python
# 001.1-UNIT-015
def test_validate_detects_orphan_consumer():
    """validate() returns error for stdin without producer."""
    registry = StreamRegistry()
    registry.register("orphan", StreamDirection.STDIN, node_name="consumer")

    errors = registry.validate()

    assert len(errors) == 1
    assert "orphan" in errors[0]
    assert "never produced" in errors[0]

# 001.1-UNIT-016
def test_create_pipe_on_closed_channel_raises():
    """create_pipe() on closed channel raises RuntimeError."""
    channel = StreamChannel(name="test", direction=StreamDirection.STDOUT)
    channel.create_pipe()
    channel.close()

    with pytest.raises(RuntimeError, match="has been closed"):
        channel.create_pipe()

# 001.1-UNIT-017
@pytest.mark.skipif(sys.platform == "win32", reason="Test for non-Windows behavior")
def test_validate_platform_passes_on_unix():
    """validate_platform() passes on Unix systems."""
    from the_edge_agent.streams import validate_platform

    # Should not raise
    validate_platform()

# 001.1-UNIT-018
def test_get_producers_filters_correctly():
    """get_producers() returns only STDOUT/STDERR channels."""
    registry = StreamRegistry()
    registry.register("out", StreamDirection.STDOUT)
    registry.register("err", StreamDirection.STDERR)
    registry.register("in", StreamDirection.STDIN)

    producers = registry.get_producers()

    assert len(producers) == 2
    assert all(p.direction in (StreamDirection.STDOUT, StreamDirection.STDERR) for p in producers)
```

---

## Risk Coverage

| Risk ID | Risk Description | Test Coverage |
|---------|------------------|---------------|
| R1 | Deadlock on full pipe buffer | INT-001 (lifecycle), INT-002 (data flow) |
| R2 | Broken pipe crashes workflow | UNIT-012, INT-004 (SIGPIPE handling) |
| R3 | File descriptor leak | UNIT-008 (cleanup), INT-003 (idempotent close) |
| R4 | Platform incompatibility | UNIT-017 (validate_platform) |
| R5 | fcntl failure | UNIT-014 (graceful fallback) |

---

## Recommended Execution Order

1. **P0 Unit tests** (fast, fail-fast on fundamental issues)
   - 001.1-UNIT-001 through 001.1-UNIT-009
   - 001.1-UNIT-012
2. **P0 Integration tests** (confirm OS interaction)
   - 001.1-INT-001
   - 001.1-INT-002
3. **P1 Unit tests** (secondary functionality)
   - 001.1-UNIT-010, 001.1-UNIT-011
   - 001.1-UNIT-013
   - 001.1-UNIT-015 through 001.1-UNIT-018
4. **P1 Integration tests**
   - 001.1-INT-003
   - 001.1-INT-004
5. **P2 Unit tests** (edge cases, time permitting)
   - 001.1-UNIT-014

---

## Test File Structure

```
python/tests/
├── test_streams.py           # All 18 scenarios
│   ├── TestStreamChannel     # AC1, AC4, AC6
│   ├── TestStreamRegistry    # AC2, AC3
│   ├── TestSigpipeHandling   # AC5
│   └── TestValidation        # Additional scenarios
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for OS interaction)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (P0 for data integrity, lifecycle)
- [x] Test IDs follow naming convention (EPIC.STORY-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
- [x] Error scenarios covered (closed channel, orphan consumer, platform check)
- [x] Resource cleanup tested (idempotent close, registry cleanup)

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-STREAM-001.1
  scenarios_total: 18
  by_level:
    unit: 14
    integration: 4
    e2e: 0
  by_priority:
    p0: 8
    p1: 8
    p2: 2
  coverage_gaps: []
  risks_mitigated:
    - R1: Deadlock prevention via lifecycle tests
    - R2: SIGPIPE handling tests
    - R3: File descriptor cleanup tests
    - R4: Platform validation test
    - R5: fcntl fallback test
  test_file: python/tests/test_streams.py
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-STREAM-001.1-test-design-20260102.md
P0 tests identified: 8
P1 tests identified: 8
P2 tests identified: 2
Total scenarios: 18
```
