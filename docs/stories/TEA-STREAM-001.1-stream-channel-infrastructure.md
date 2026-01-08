# Story: TEA-STREAM-001.1 - Stream Channel Infrastructure

## Status: Done

**Epic**: [TEA-STREAM-001 - Unix Pipe Streaming](./TEA-STREAM-001-unix-pipe-streaming-epic.md)
**Estimated Tests**: 18 scenarios
**Dependencies**: TEA-PARALLEL-001.1 (Executor Abstraction)

---

## User Story

**As a** workflow developer,
**I want** named stream channels that can connect node outputs to inputs,
**So that** I can define streaming data flows alongside traditional state passing.

---

## Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `StreamChannel` dataclass with name, direction (stdin/stdout/stderr), buffer_size | Unit test: dataclass fields and defaults |
| AC2 | `StreamRegistry` manages named channels across workflow | Unit test: register, get, cleanup methods |
| AC3 | Channels support multiple consumers (fan-out preparation) | Unit test: add consumer to existing channel |
| AC4 | Channel lifecycle: create → connect → stream → close | Integration test: full lifecycle |
| AC5 | Graceful handling of broken pipes (SIGPIPE) | Unit test: signal handler installed |
| AC6 | Configurable buffer size per channel | Unit test: fcntl F_SETPIPE_SZ call |

---

## Technical Design

### Files to Create

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/streams.py` | Create | StreamChannel, StreamDirection, StreamRegistry |

### Implementation Approach

```python
from dataclasses import dataclass, field
from typing import Optional, List
from enum import Enum
import os
import signal


class StreamDirection(Enum):
    """Direction of stream data flow."""
    STDIN = "stdin"
    STDOUT = "stdout"
    STDERR = "stderr"


@dataclass
class StreamChannel:
    """Named stream channel for inter-process communication."""
    name: str
    direction: StreamDirection
    buffer_size: int = 65536  # 64KB default (OS pipe buffer)

    # Runtime state (set after create_pipe())
    read_fd: Optional[int] = None
    write_fd: Optional[int] = None
    consumers: List[str] = field(default_factory=list)
    _closed: bool = False

    def create_pipe(self) -> tuple[int, int]:
        """
        Create OS pipe, return (read_fd, write_fd).

        Optionally resizes pipe buffer on Linux if buffer_size differs from default.
        """
        if self._closed:
            raise RuntimeError(f"Channel '{self.name}' has been closed")

        r, w = os.pipe()

        # Resize pipe buffer on Linux (F_SETPIPE_SZ = 1031)
        if self.buffer_size != 65536:
            try:
                import fcntl
                fcntl.fcntl(w, 1031, self.buffer_size)
            except (ImportError, OSError, PermissionError):
                pass  # Not supported or insufficient permissions, use default

        self.read_fd, self.write_fd = r, w
        return r, w

    def close(self) -> None:
        """Close pipe file descriptors."""
        if self._closed:
            return

        if self.read_fd is not None:
            try:
                os.close(self.read_fd)
            except OSError:
                pass  # Already closed
            self.read_fd = None

        if self.write_fd is not None:
            try:
                os.close(self.write_fd)
            except OSError:
                pass  # Already closed
            self.write_fd = None

        self._closed = True

    def add_consumer(self, node_name: str) -> None:
        """Register a consumer node for this channel."""
        if node_name not in self.consumers:
            self.consumers.append(node_name)

    @property
    def consumer_count(self) -> int:
        """Number of registered consumers."""
        return len(self.consumers)

    @property
    def is_broadcast(self) -> bool:
        """True if channel has multiple consumers."""
        return len(self.consumers) > 1


@dataclass
class StreamRegistry:
    """Registry of named stream channels for a workflow execution."""
    channels: dict[str, StreamChannel] = field(default_factory=dict)
    _sigpipe_handled: bool = False

    def register(
        self,
        name: str,
        direction: StreamDirection,
        node_name: Optional[str] = None,
        buffer_size: int = 65536
    ) -> StreamChannel:
        """
        Register a new stream channel or add consumer to existing.

        Args:
            name: Unique channel name
            direction: stdin/stdout/stderr
            node_name: Node registering this channel
            buffer_size: Pipe buffer size in bytes

        Returns:
            The StreamChannel (new or existing)
        """
        if name in self.channels:
            # Channel exists - add consumer if stdin
            channel = self.channels[name]
            if direction == StreamDirection.STDIN and node_name:
                channel.add_consumer(node_name)
            return channel

        # Create new channel
        channel = StreamChannel(
            name=name,
            direction=direction,
            buffer_size=buffer_size
        )
        if node_name:
            channel.add_consumer(node_name)

        self.channels[name] = channel
        return channel

    def get(self, name: str) -> Optional[StreamChannel]:
        """Get channel by name, or None if not found."""
        return self.channels.get(name)

    def get_producers(self) -> List[StreamChannel]:
        """Get all channels with stdout/stderr direction (producers)."""
        return [
            c for c in self.channels.values()
            if c.direction in (StreamDirection.STDOUT, StreamDirection.STDERR)
        ]

    def get_consumers(self) -> List[StreamChannel]:
        """Get all channels with stdin direction (consumers)."""
        return [
            c for c in self.channels.values()
            if c.direction == StreamDirection.STDIN
        ]

    def create_all_pipes(self) -> None:
        """Create OS pipes for all registered channels."""
        for channel in self.channels.values():
            if channel.read_fd is None:
                channel.create_pipe()

    def cleanup(self) -> None:
        """Close all channels and release resources."""
        for channel in self.channels.values():
            channel.close()
        self.channels.clear()

    def install_sigpipe_handler(self) -> None:
        """Install SIGPIPE handler to prevent crashes on broken pipes."""
        if self._sigpipe_handled:
            return

        # Ignore SIGPIPE - allows graceful handling of broken pipes
        signal.signal(signal.SIGPIPE, signal.SIG_IGN)
        self._sigpipe_handled = True

    def validate(self) -> List[str]:
        """
        Validate channel configuration.

        Returns:
            List of validation error messages (empty if valid)
        """
        errors = []

        # Check that all stdin channels have a matching stdout producer
        producer_names = {
            c.name for c in self.channels.values()
            if c.direction == StreamDirection.STDOUT
        }

        for channel in self.channels.values():
            if channel.direction == StreamDirection.STDIN:
                if channel.name not in producer_names:
                    errors.append(
                        f"Stream '{channel.name}' is consumed but never produced. "
                        f"Add a node with 'streams.stdout: {channel.name}'"
                    )

        return errors


def validate_platform() -> None:
    """
    Validate that the current platform supports stream channels.

    Raises:
        PlatformError: If platform is not supported (Windows)
    """
    import sys
    if sys.platform == "win32":
        raise PlatformError(
            "Stream channels require Unix-like OS (Linux/macOS). "
            "Windows is not supported. Use parallel_strategy: thread instead."
        )


class PlatformError(Exception):
    """Raised when stream channels are used on unsupported platform."""
    pass
```

### Stream Channel States

```
┌─────────────┐    create_pipe()    ┌─────────────┐
│   CREATED   │ ──────────────────► │  CONNECTED  │
│ (no fds)    │                     │ (fds open)  │
└─────────────┘                     └──────┬──────┘
                                           │
                                           │ close()
                                           ▼
                                    ┌─────────────┐
                                    │   CLOSED    │
                                    │ (fds None)  │
                                    └─────────────┘
```

---

## Testing

### Test Location

`python/tests/test_streams.py`

### Test Scenarios (18 total)

#### AC1: StreamChannel Dataclass (4 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.1-UNIT-001 | Unit | P0 | StreamChannel has required fields (name, direction, buffer_size) |
| 001.1-UNIT-002 | Unit | P0 | Default buffer_size is 65536 (64KB) |
| 001.1-UNIT-003 | Unit | P1 | StreamDirection enum has STDIN, STDOUT, STDERR |
| 001.1-UNIT-004 | Unit | P1 | create_pipe() returns valid file descriptors |

#### AC2: StreamRegistry (4 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.1-UNIT-005 | Unit | P0 | register() creates new channel |
| 001.1-UNIT-006 | Unit | P0 | register() returns existing channel if name exists |
| 001.1-UNIT-007 | Unit | P1 | get() returns None for unknown channel |
| 001.1-UNIT-008 | Unit | P1 | cleanup() closes all channels |

#### AC3: Multiple Consumers (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.1-UNIT-009 | Unit | P0 | add_consumer() adds node to consumers list |
| 001.1-UNIT-010 | Unit | P1 | is_broadcast returns True for multiple consumers |
| 001.1-UNIT-011 | Unit | P1 | consumer_count reflects registered consumers |

#### AC4: Channel Lifecycle (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.1-INT-001 | Integration | P0 | Full lifecycle: create → write → read → close |
| 001.1-INT-002 | Integration | P0 | Data written to write_fd readable from read_fd |
| 001.1-INT-003 | Integration | P1 | close() is idempotent (can call multiple times) |

#### AC5: SIGPIPE Handling (2 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.1-UNIT-012 | Unit | P0 | install_sigpipe_handler() sets SIG_IGN |
| 001.1-INT-004 | Integration | P1 | Broken pipe doesn't crash process |

#### AC6: Buffer Size (2 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.1-UNIT-013 | Unit | P1 | Custom buffer_size passed to fcntl |
| 001.1-UNIT-014 | Unit | P2 | Graceful fallback when fcntl fails |

---

## Definition of Done

- [x] `StreamChannel` dataclass with all required fields
- [x] `StreamDirection` enum with STDIN, STDOUT, STDERR
- [x] `StreamRegistry` with register, get, cleanup methods
- [x] `validate_platform()` function for Windows check
- [x] SIGPIPE handler installation
- [x] All 18 test scenarios pass (27 tests total including edge cases)
- [ ] Code reviewed and merged

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| File descriptor leak | High | Ensure close() in finally blocks, add __del__ |
| SIGPIPE crashes | High | Install SIG_IGN handler before any pipe operations |
| fcntl not available (non-Linux) | Low | Graceful fallback to default buffer |

---

## Notes for Developer

1. **Platform check first**: Call `validate_platform()` early in any stream-enabled workflow.

2. **SIGPIPE handling**: Must install handler before creating any pipes. The SIG_IGN approach is simplest but means we need to check write return values.

3. **File descriptor cleanup**: Use try/finally pattern in all code that creates pipes.

4. **Testing broken pipes**: Create a pipe, close the read end, then write to trigger broken pipe.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-02 | 1.0 | Story created from epic | Sarah (PO) |

---

## QA Notes

**Reviewed by:** Quinn (Test Architect)
**Review Date:** 2026-01-02

### Test Coverage Summary

| Metric | Count |
|--------|-------|
| **Total scenarios** | 18 |
| **Unit tests** | 14 (78%) |
| **Integration tests** | 4 (22%) |
| **P0 (Critical)** | 8 |
| **P1 (Important)** | 8 |
| **P2 (Nice-to-have)** | 2 |

**Rationale:** Heavy unit test bias is appropriate for infrastructure dataclasses. Integration tests cover OS pipe interactions and signal handling.

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **File descriptor leak** | High | Tested via cleanup() and idempotent close() scenarios |
| **SIGPIPE crashes** | High | SIG_IGN handler installation tested; broken pipe scenario covered |
| **Deadlock on full buffer** | Medium | Lifecycle and data flow integration tests |
| **Platform incompatibility** | Low | validate_platform() guards Windows usage |
| **fcntl failure** | Low | Graceful fallback tested |

### Recommended Test Scenarios

**P0 Critical (must pass before merge):**
1. `StreamChannel` has required fields (name, direction, buffer_size)
2. Default buffer_size is 65536 (64KB)
3. `register()` creates new channel
4. `register()` returns existing channel for duplicate name
5. `add_consumer()` adds node to consumers list
6. Full lifecycle: create → write → read → close
7. Data integrity through pipe
8. SIGPIPE handler installation

**P1 Important:**
1. `StreamDirection` enum completeness
2. `create_pipe()` returns valid file descriptors
3. `get()` returns None for unknown channel
4. `cleanup()` closes all channels
5. `is_broadcast` detection
6. `close()` idempotency
7. Broken pipe graceful handling
8. Custom buffer_size via fcntl

**P2 Edge cases:**
1. fcntl fallback on failure
2. Orphan consumer validation

### Concerns / Blockers

1. **None blocking:** Test design is comprehensive and covers all acceptance criteria.

2. **Note:** Story 5 (Integration & Documentation) will provide E2E coverage for the full streaming pipeline. This story focuses on unit/integration testing of infrastructure primitives.

### Test File Location

`python/tests/test_streams.py`

### Gate Status

**PASS** - All 27 tests pass. Implementation complete with proper resource management.

---

## QA Results

**Review Date**: 2026-01-08
**Reviewer**: Quinn (Test Architect)
**Gate Decision**: PASS

### Summary

All acceptance criteria verified. Implementation exceeds test coverage requirements (27 tests vs 18 specified).

### Test Results

| Category | Count | Status |
|----------|-------|--------|
| Unit Tests | 14 | PASS |
| Integration Tests | 4 | PASS |
| Total | 27 | PASS |

### Acceptance Criteria Verification

| AC | Description | Verified |
|----|-------------|----------|
| AC1 | StreamChannel dataclass with name, direction, buffer_size | Yes |
| AC2 | StreamRegistry manages named channels | Yes |
| AC3 | Channels support multiple consumers | Yes |
| AC4 | Channel lifecycle: create -> connect -> stream -> close | Yes |
| AC5 | Graceful handling of broken pipes (SIGPIPE) | Yes |
| AC6 | Configurable buffer size per channel | Yes |

### Code Quality Assessment

- Clean dataclass design with proper defaults
- Idempotent close() prevents double-close errors
- __del__ ensures fd cleanup on garbage collection
- Platform validation fails fast with actionable message
- fcntl buffer resize has proper fallback for non-Linux systems

### Gate File

`docs/qa/gates/TEA-STREAM-001.1-stream-channel-infrastructure.yml`

---

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/streams.py` | Created | StreamChannel, StreamDirection, StreamRegistry, PlatformError, validate_platform() |
| `python/tests/test_streams.py` | Created | 27 test scenarios covering all acceptance criteria |

### Debug Log References
None - implementation completed without blocking issues.

### Completion Notes

1. **Implementation complete**: All acceptance criteria implemented as specified in the story.
2. **Test coverage**: 27 tests implemented (exceeding the 18 specified) covering:
   - AC1: 4 tests for StreamChannel dataclass
   - AC2: 4 tests for StreamRegistry
   - AC3: 3 tests for multiple consumers
   - AC4: 3 tests for channel lifecycle
   - AC5: 2 tests for SIGPIPE handling
   - AC6: 2 tests for buffer size
   - Additional edge case and validation tests
3. **All tests pass**: `pytest tests/test_streams.py` - 27 passed
4. **Regression verified**: Core tests (stategraph, yaml_engine) pass with new code.
5. **Note**: Pre-existing failure in `test_neo4j_backend.py` unrelated to this story.

### Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-02 | 1.0 | Story created from epic | Sarah (PO) |
| 2026-01-08 | 2.0 | Implementation complete, ready for review | James (Dev Agent) |
