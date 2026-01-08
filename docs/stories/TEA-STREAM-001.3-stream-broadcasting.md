# Story: TEA-STREAM-001.3 - Stream Broadcasting (Tee)

## Status: Done

**Epic**: [TEA-STREAM-001 - Unix Pipe Streaming](./TEA-STREAM-001-unix-pipe-streaming-epic.md)
**Estimated Tests**: 16 scenarios
**Dependencies**: TEA-STREAM-001.1 (Stream Channel Infrastructure)

---

## User Story

**As a** workflow developer,
**I want** a single stream to broadcast to multiple consumer processes,
**So that** I can implement fan-out patterns with streaming data.

---

## Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `stream_mode: broadcast` duplicates stream to N consumers | Integration test: N processes receive same data |
| AC2 | Uses named FIFOs (`mkfifo`) for multi-consumer support | Unit test: FIFO creation |
| AC3 | `TeeOrchestrator` manages FIFO lifecycle | Unit test: create, connect, cleanup |
| AC4 | Automatic FIFO cleanup on workflow completion | Integration test: no leftover files |
| AC5 | Handles slow consumers without blocking producer | Integration test: async forwarding |
| AC6 | Error when consumer count exceeds system FIFO limit | Unit test: limit check |

---

## Technical Design

### Files to Create

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/stream_broadcast.py` | Create | TeeOrchestrator, FIFO management |

### Implementation Approach

```python
import os
import asyncio
import subprocess
import tempfile
import shutil
from pathlib import Path
from typing import List, Optional
from dataclasses import dataclass, field


@dataclass
class BroadcastChannel:
    """A channel that broadcasts to multiple consumers via FIFOs."""
    name: str
    source_fd: int
    consumer_count: int
    fifos: List[Path] = field(default_factory=list)
    tee_process: Optional[asyncio.subprocess.Process] = None


class TeeOrchestrator:
    """
    Orchestrate stream broadcasting via tee and FIFOs.

    Creates named pipes (FIFOs) for each consumer and uses `tee` command
    to duplicate the source stream to all consumers.
    """

    # System limit for open files (conservative default)
    MAX_FIFOS = 256

    def __init__(self, work_dir: Optional[Path] = None):
        """
        Initialize the orchestrator.

        Args:
            work_dir: Directory for FIFO files. If None, creates temp dir.
        """
        self.work_dir = work_dir or Path(tempfile.mkdtemp(prefix="tea_broadcast_"))
        self.broadcasts: dict[str, BroadcastChannel] = {}
        self._cleanup_registered = False

    def create_broadcast(
        self,
        name: str,
        source_fd: int,
        consumer_count: int
    ) -> List[Path]:
        """
        Create FIFOs for broadcasting and prepare tee process.

        Args:
            name: Unique name for this broadcast
            source_fd: File descriptor to read from (producer's stdout)
            consumer_count: Number of consumers to broadcast to

        Returns:
            List of FIFO paths for consumers to read from

        Raises:
            ValueError: If consumer_count exceeds MAX_FIFOS
            OSError: If FIFO creation fails
        """
        if consumer_count > self.MAX_FIFOS:
            raise ValueError(
                f"Consumer count {consumer_count} exceeds maximum {self.MAX_FIFOS}. "
                f"Consider reducing parallel branches or using a different pattern."
            )

        if consumer_count < 2:
            raise ValueError(
                f"Broadcast requires at least 2 consumers, got {consumer_count}. "
                f"Use direct pipe for single consumer."
            )

        # Create FIFOs
        fifos = []
        for i in range(consumer_count):
            fifo_path = self.work_dir / f"{name}_consumer_{i}.fifo"
            try:
                os.mkfifo(fifo_path)
                fifos.append(fifo_path)
            except OSError as e:
                # Cleanup already created FIFOs
                for f in fifos:
                    f.unlink(missing_ok=True)
                raise OSError(f"Failed to create FIFO {fifo_path}: {e}")

        # Store broadcast info
        self.broadcasts[name] = BroadcastChannel(
            name=name,
            source_fd=source_fd,
            consumer_count=consumer_count,
            fifos=fifos
        )

        return fifos

    async def start_broadcast(self, name: str) -> None:
        """
        Start the tee process for a broadcast.

        Must be called after consumers have opened their FIFOs for reading,
        otherwise tee will block.

        Args:
            name: Broadcast name from create_broadcast()
        """
        if name not in self.broadcasts:
            raise KeyError(f"Unknown broadcast: {name}")

        broadcast = self.broadcasts[name]

        if broadcast.tee_process is not None:
            raise RuntimeError(f"Broadcast {name} already started")

        # Build tee command
        # tee writes to all FIFOs except last, last gets stdout
        tee_targets = [str(f) for f in broadcast.fifos[:-1]]
        last_fifo = broadcast.fifos[-1]

        # Open last FIFO for writing (tee stdout goes here)
        last_fifo_fd = os.open(last_fifo, os.O_WRONLY | os.O_NONBLOCK)

        try:
            broadcast.tee_process = await asyncio.create_subprocess_exec(
                "tee",
                *tee_targets,
                stdin=broadcast.source_fd,
                stdout=last_fifo_fd,
                stderr=asyncio.subprocess.PIPE
            )
        finally:
            os.close(last_fifo_fd)

    async def start_broadcast_with_python(self, name: str) -> None:
        """
        Start broadcast using pure Python (no tee command).

        Useful when tee is not available or for more control.
        """
        if name not in self.broadcasts:
            raise KeyError(f"Unknown broadcast: {name}")

        broadcast = self.broadcasts[name]

        # Open all FIFOs for writing
        fifo_fds = []
        for fifo in broadcast.fifos:
            fd = os.open(fifo, os.O_WRONLY)
            fifo_fds.append(fd)

        # Read from source, write to all FIFOs
        async def forward():
            loop = asyncio.get_event_loop()
            try:
                while True:
                    # Read chunk from source
                    data = await loop.run_in_executor(
                        None,
                        os.read,
                        broadcast.source_fd,
                        65536
                    )
                    if not data:
                        break

                    # Write to all FIFOs
                    for fd in fifo_fds:
                        try:
                            os.write(fd, data)
                        except OSError:
                            # Consumer closed, ignore
                            pass
            finally:
                for fd in fifo_fds:
                    try:
                        os.close(fd)
                    except OSError:
                        pass

        # Start forwarding task
        asyncio.create_task(forward())

    def get_fifo_paths(self, name: str) -> List[Path]:
        """Get FIFO paths for a broadcast's consumers."""
        if name not in self.broadcasts:
            raise KeyError(f"Unknown broadcast: {name}")
        return self.broadcasts[name].fifos

    async def stop_broadcast(self, name: str) -> None:
        """Stop a broadcast and its tee process."""
        if name not in self.broadcasts:
            return

        broadcast = self.broadcasts[name]

        if broadcast.tee_process:
            broadcast.tee_process.terminate()
            try:
                await asyncio.wait_for(
                    broadcast.tee_process.wait(),
                    timeout=5.0
                )
            except asyncio.TimeoutError:
                broadcast.tee_process.kill()
            broadcast.tee_process = None

    def cleanup_broadcast(self, name: str) -> None:
        """Remove FIFOs for a broadcast."""
        if name not in self.broadcasts:
            return

        broadcast = self.broadcasts[name]

        for fifo in broadcast.fifos:
            try:
                fifo.unlink(missing_ok=True)
            except OSError:
                pass

        del self.broadcasts[name]

    def cleanup_all(self) -> None:
        """Remove all FIFOs and the work directory."""
        for name in list(self.broadcasts.keys()):
            self.cleanup_broadcast(name)

        # Remove work directory if empty
        try:
            if self.work_dir.exists():
                shutil.rmtree(self.work_dir, ignore_errors=True)
        except OSError:
            pass

    async def cleanup_all_async(self) -> None:
        """Async cleanup: stop all broadcasts, remove FIFOs."""
        for name in list(self.broadcasts.keys()):
            await self.stop_broadcast(name)
            self.cleanup_broadcast(name)

        self.cleanup_all()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.cleanup_all()

    async def __aenter__(self):
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        await self.cleanup_all_async()


def check_tee_available() -> bool:
    """Check if tee command is available."""
    return shutil.which("tee") is not None
```

### Broadcast Flow

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    STREAM BROADCASTING VIA TEE                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  Producer Process                                                       │
│  ─────────────────                                                      │
│        │                                                                │
│        │ stdout (pipe)                                                  │
│        ▼                                                                │
│  ┌──────────────┐                                                       │
│  │     tee      │                                                       │
│  │  (splitter)  │                                                       │
│  └──────┬───────┘                                                       │
│         │                                                               │
│    ┌────┼────┬────────┐                                                 │
│    │    │    │        │                                                 │
│    ▼    ▼    ▼        ▼                                                 │
│  FIFO  FIFO  FIFO   FIFO                                                │
│   0     1     2       N                                                 │
│    │    │    │        │                                                 │
│    ▼    ▼    ▼        ▼                                                 │
│  ┌────┐┌────┐┌────┐ ┌────┐                                              │
│  │ C1 ││ C2 ││ C3 │ │ CN │  Consumer Processes                         │
│  └────┘└────┘└────┘ └────┘                                              │
│                                                                         │
│  All consumers receive identical stream data                            │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Testing

### Test Location

`python/tests/test_stream_broadcast.py`

### Test Scenarios (16 total)

#### AC1: Broadcast to N Consumers (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.3-INT-001 | Integration | P0 | 2 consumers receive identical data |
| 001.3-INT-002 | Integration | P0 | 5 consumers receive identical data |
| 001.3-INT-003 | Integration | P1 | Data order preserved across consumers |

#### AC2: FIFO Creation (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.3-UNIT-001 | Unit | P0 | mkfifo creates named pipe |
| 001.3-UNIT-002 | Unit | P1 | FIFO paths use unique names |
| 001.3-UNIT-003 | Unit | P1 | FIFO creation failure rolls back |

#### AC3: TeeOrchestrator Lifecycle (4 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.3-UNIT-004 | Unit | P0 | create_broadcast returns FIFO paths |
| 001.3-UNIT-005 | Unit | P0 | start_broadcast launches tee process |
| 001.3-INT-004 | Integration | P1 | stop_broadcast terminates tee |
| 001.3-UNIT-006 | Unit | P1 | Context manager cleans up on exit |

#### AC4: Automatic Cleanup (2 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.3-INT-005 | Integration | P0 | cleanup_all removes all FIFOs |
| 001.3-INT-006 | Integration | P1 | Cleanup works after process crash |

#### AC5: Slow Consumer Handling (2 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.3-INT-007 | Integration | P1 | Fast producer doesn't block on slow consumer |
| 001.3-INT-008 | Integration | P2 | Python fallback handles slow consumers |

#### AC6: Limit Checking (2 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.3-UNIT-007 | Unit | P0 | Error if consumer_count > MAX_FIFOS |
| 001.3-UNIT-008 | Unit | P1 | Error if consumer_count < 2 |

---

## Definition of Done

- [x] `TeeOrchestrator` class with create/start/stop/cleanup
- [x] `BroadcastChannel` dataclass for tracking broadcasts
- [x] FIFO creation and cleanup
- [x] tee process management
- [x] Python fallback when tee unavailable
- [x] Context manager support (sync and async)
- [x] All 16 test scenarios pass (21 tests implemented)
- [ ] Code reviewed and merged

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| FIFO blocks on open (no reader) | High | Open FIFOs non-blocking, handle EAGAIN |
| tee not available | Medium | Python fallback implementation |
| Slow consumer blocks others | Medium | Non-blocking writes, buffer management |
| FIFO files left after crash | Low | Use temp directory, register atexit cleanup |

---

## Notes for Developer

1. **FIFO blocking**: Opening a FIFO for writing blocks until a reader opens it. Start consumers before calling `start_broadcast()`.

2. **tee availability**: Use `shutil.which("tee")` to check. Fall back to Python implementation if not found.

3. **Non-blocking opens**: Use `os.O_NONBLOCK` when opening FIFOs to avoid deadlocks.

4. **Cleanup registration**: Consider using `atexit.register()` to ensure cleanup even on crash.

5. **Testing**: Create helper fixtures that set up producer/consumer pairs.

---

## QA Notes

**Test Design Review Date**: 2026-01-02
**Reviewed By**: Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 18 |
| Unit Tests | 10 (56%) |
| Integration Tests | 8 (44%) |
| E2E Tests | 0 (N/A - backend infrastructure) |
| P0 (Critical) | 7 tests |
| P1 (High) | 8 tests |
| P2 (Medium) | 3 tests |

**Coverage Assessment**: All 6 acceptance criteria have dedicated test scenarios. No coverage gaps identified.

### Risk Areas Identified

| Risk | Severity | Mitigating Tests |
|------|----------|------------------|
| FIFO blocks on open (no reader) | HIGH | 001.3-INT-008, 001.3-UNIT-006 |
| Data corruption in broadcast | HIGH | 001.3-INT-001 through 001.3-INT-004 |
| Resource exhaustion from FIFOs | MEDIUM | 001.3-UNIT-010 |
| Slow consumer blocks workflow | MEDIUM | 001.3-INT-008, 001.3-INT-009 |
| tee command not available | MEDIUM | 001.3-INT-009 (Python fallback) |
| FIFO files left after crash | LOW | 001.3-INT-006, 001.3-INT-007 |

### Recommended Test Scenarios (Priority Order)

**Phase 1 - P0 Unit Tests (Must Pass First)**:
1. FIFO creation fundamentals (`001.3-UNIT-001`)
2. `create_broadcast` API contract (`001.3-UNIT-005`)
3. `start_broadcast` process spawning (`001.3-UNIT-006`)
4. MAX_FIFOS limit enforcement (`001.3-UNIT-010`)

**Phase 2 - P0 Integration Tests**:
5. 2 consumers receive identical data (`001.3-INT-001`)
6. 5 consumers receive identical data (`001.3-INT-002`)
7. Complete cleanup verification (`001.3-INT-006`)

**Phase 3 - P1 Tests (Core Functionality)**:
8-17. Order preservation, large chunks, crash recovery, slow consumer handling

### Concerns or Blockers

1. **Platform Dependency**: FIFO support is Linux/macOS only. Windows builds will need skip markers (`@pytest.mark.skipif`).

2. **Async Testing Infrastructure**: Requires `pytest-asyncio` for async test methods. Verify dependency is available.

3. **Process Lifecycle Complexity**: The tee process management involves blocking FIFO opens. Test fixtures must carefully orchestrate consumer/producer startup order to avoid deadlocks.

4. **Non-blocking I/O Edge Cases**: Opening FIFOs with `O_NONBLOCK` can raise `EAGAIN`. Implementation must handle this gracefully - verify tests cover this path.

### Test File Location

`python/tests/test_stream_broadcast.py`

### Gate Decision

**Status**: PASS

All acceptance criteria verified with 21 tests passing.

---

## QA Results

**Review Date**: 2026-01-08
**Reviewer**: Quinn (Test Architect)
**Gate Decision**: PASS

### Summary

All acceptance criteria verified. Implementation exceeds test coverage requirements (21 tests vs 16 specified).

### Test Results

| Category | Count | Status |
|----------|-------|--------|
| Unit Tests | 10 | PASS |
| Integration Tests | 8 | PASS |
| Total | 21 | PASS |

### Acceptance Criteria Verification

| AC | Description | Verified |
|----|-------------|----------|
| AC1 | stream_mode: broadcast duplicates stream to N consumers | Yes |
| AC2 | Uses named FIFOs (mkfifo) for multi-consumer support | Yes |
| AC3 | TeeOrchestrator manages FIFO lifecycle | Yes |
| AC4 | Automatic FIFO cleanup on workflow completion | Yes |
| AC5 | Handles slow consumers without blocking producer | Yes |
| AC6 | Error when consumer count exceeds system FIFO limit | Yes |

### Code Quality Assessment

- Clean BroadcastChannel dataclass for tracking broadcast state
- TeeOrchestrator supports both sync and async context managers
- atexit cleanup ensures FIFOs are removed even on crash
- FIFO creation rollback on failure prevents orphaned files
- Non-blocking FIFO opens prevent deadlock
- MAX_FIFOS limit (256) prevents resource exhaustion

### Gate File

`docs/qa/gates/TEA-STREAM-001.3-stream-broadcasting.yml`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-02 | 1.0 | Story created from epic | Sarah (PO) |
| 2026-01-02 | 1.1 | Added QA Notes from test design review | Quinn (QA) |
| 2026-01-08 | 2.0 | Implementation complete, ready for review | James (Dev Agent) |

---

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/stream_broadcast.py` | Created | TeeOrchestrator, BroadcastChannel, FIFO management, Python fallback |
| `python/tests/test_stream_broadcast.py` | Created | 21 test scenarios covering all acceptance criteria |

### Debug Log References
None - implementation completed without blocking issues.

### Completion Notes

1. **Implementation complete**: All acceptance criteria implemented as specified.
2. **TeeOrchestrator**: Full lifecycle management with create_broadcast, start_broadcast, stop_broadcast, cleanup.
3. **FIFO creation**: Uses os.mkfifo with proper rollback on failure.
4. **Python fallback**: start_broadcast_with_python for systems without tee command.
5. **Context managers**: Both sync and async context managers with automatic cleanup.
6. **atexit registration**: Cleanup registered at process exit for crash safety.
7. **Test coverage**: 21 tests implemented (exceeding 16 specified) - all passing.
