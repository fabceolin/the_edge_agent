# Story: TEA-STREAM-001.2 - Pipe Executor Extension

## Status: Ready for Development

**Epic**: [TEA-STREAM-001 - Unix Pipe Streaming](./TEA-STREAM-001-unix-pipe-streaming-epic.md)
**Estimated Tests**: 22 scenarios
**Dependencies**:
- TEA-STREAM-001.1 (Stream Channel Infrastructure)
- TEA-PARALLEL-001.1 (Executor Abstraction)

---

## User Story

**As a** workflow developer,
**I want** `ProcessExecutor` to wire pipes between processes based on stream channels,
**So that** stdout from one process flows to stdin of another in real-time.

---

## Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `ProcessExecutor` accepts `StreamRegistry` in execute() | Unit test: method signature |
| AC2 | Processes launched with stdout/stdin connected to pipes | Integration test: fd wiring |
| AC3 | Pipe wiring follows `streams:` configuration | Integration test: config-driven wiring |
| AC4 | SIGPIPE handled gracefully (consumer exits early) | Integration test: no crash |
| AC5 | Deadlock prevention: async pipe management | Integration test: large data transfer |
| AC6 | State still serialized/passed alongside streams | Integration test: hybrid state+stream |
| AC7 | Process exit codes captured and reported | Unit test: exit code in result |

---

## Technical Design

### Files to Modify

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/parallel_executors.py` | Modify | Add stream support to ProcessExecutor |

### Implementation Approach

```python
import asyncio
import subprocess
from typing import List, Dict, Any, Optional
from concurrent.futures import ProcessPoolExecutor

from the_edge_agent.streams import StreamRegistry, StreamChannel, StreamDirection
from the_edge_agent.parallel import ParallelConfig, ParallelFlowResult


class ProcessExecutor:
    """Process-based executor with optional pipe streaming."""

    def __init__(self, max_workers: Optional[int] = None):
        self.max_workers = max_workers

    def execute(
        self,
        flows: List[Dict[str, Any]],
        config: ParallelConfig,
        stream_registry: Optional[StreamRegistry] = None
    ) -> List[ParallelFlowResult]:
        """
        Execute flows in parallel processes.

        If stream_registry is provided, connects process stdin/stdout
        to named stream channels.
        """
        if stream_registry and stream_registry.channels:
            return self._execute_with_streams(flows, config, stream_registry)
        else:
            return self._execute_standard(flows, config)

    def _execute_standard(
        self,
        flows: List[Dict[str, Any]],
        config: ParallelConfig
    ) -> List[ParallelFlowResult]:
        """Standard execution without streams (existing behavior)."""
        # Existing ProcessPoolExecutor logic
        self._validate_picklable([f.get("state", {}) for f in flows])

        with ProcessPoolExecutor(max_workers=self.max_workers) as executor:
            futures = [
                executor.submit(self._run_flow, flow)
                for flow in flows
            ]
            results = []
            for future in futures:
                try:
                    result = future.result(timeout=config.timeout_seconds)
                    results.append(ParallelFlowResult(state=result))
                except Exception as e:
                    results.append(ParallelFlowResult(error=str(e)))
            return results

    def _execute_with_streams(
        self,
        flows: List[Dict[str, Any]],
        config: ParallelConfig,
        stream_registry: StreamRegistry
    ) -> List[ParallelFlowResult]:
        """Execute with pipe streaming between processes."""
        # Install SIGPIPE handler
        stream_registry.install_sigpipe_handler()

        # Create all pipes
        stream_registry.create_all_pipes()

        try:
            # Run async event loop for non-blocking pipe management
            return asyncio.run(
                self._execute_async(flows, config, stream_registry)
            )
        finally:
            stream_registry.cleanup()

    async def _execute_async(
        self,
        flows: List[Dict[str, Any]],
        config: ParallelConfig,
        stream_registry: StreamRegistry
    ) -> List[ParallelFlowResult]:
        """Async execution with pipe management."""
        processes = []

        for flow in flows:
            proc = await self._launch_process(flow, stream_registry)
            processes.append((flow, proc))

        # Wait for all processes with timeout
        results = await self._collect_results_async(
            processes,
            timeout=config.timeout_seconds
        )

        return results

    async def _launch_process(
        self,
        flow: Dict[str, Any],
        stream_registry: StreamRegistry
    ) -> asyncio.subprocess.Process:
        """Launch a single process with stream wiring."""
        node_name = flow.get("node_name", "unknown")
        node_streams = flow.get("streams", {})

        # Determine stdin/stdout based on stream config
        stdin_fd = None
        stdout_fd = None

        # Wire stdin from stream channel
        if "stdin" in node_streams:
            channel_name = node_streams["stdin"]
            channel = stream_registry.get(channel_name)
            if channel and channel.read_fd is not None:
                stdin_fd = channel.read_fd

        # Wire stdout to stream channel
        if "stdout" in node_streams:
            channel_name = node_streams["stdout"]
            channel = stream_registry.get(channel_name)
            if channel and channel.write_fd is not None:
                stdout_fd = channel.write_fd

        # Build command
        cmd = self._build_subprocess_command(flow)

        # Launch process with wired file descriptors
        proc = await asyncio.create_subprocess_exec(
            *cmd,
            stdin=stdin_fd if stdin_fd else asyncio.subprocess.PIPE,
            stdout=stdout_fd if stdout_fd else asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            # Pass state via environment or temp file
            env=self._build_env_with_state(flow.get("state", {}))
        )

        # Close our copies of the fds (process has its own)
        if stdin_fd:
            # Don't close stdin_fd here - multiple consumers may need it
            pass
        if stdout_fd:
            # Close write end in parent - only child should write
            import os
            os.close(stdout_fd)

        return proc

    async def _collect_results_async(
        self,
        processes: List[tuple],
        timeout: Optional[float]
    ) -> List[ParallelFlowResult]:
        """Collect results from all processes."""
        results = []

        for flow, proc in processes:
            try:
                stdout, stderr = await asyncio.wait_for(
                    proc.communicate(),
                    timeout=timeout
                )

                # Parse result from stdout (if not piped to stream)
                if flow.get("streams", {}).get("stdout") is None:
                    result_state = self._parse_result(stdout)
                else:
                    result_state = flow.get("state", {})

                results.append(ParallelFlowResult(
                    state=result_state,
                    exit_code=proc.returncode
                ))

            except asyncio.TimeoutError:
                proc.kill()
                results.append(ParallelFlowResult(
                    error=f"Process timed out after {timeout}s",
                    exit_code=-1
                ))
            except Exception as e:
                results.append(ParallelFlowResult(
                    error=str(e),
                    exit_code=getattr(proc, 'returncode', -1)
                ))

        return results

    def _build_subprocess_command(self, flow: Dict[str, Any]) -> List[str]:
        """Build command to execute flow in subprocess."""
        # Execute via tea CLI with scoped execution
        return [
            "python", "-m", "the_edge_agent.cli",
            "run", flow.get("yaml_path", "workflow.yaml"),
            "--entry-point", flow.get("entry_point", "__start__"),
            "--exit-point", flow.get("exit_point", "__end__"),
            "--input", flow.get("input_file", "/dev/stdin"),
            "--output", flow.get("output_file", "/dev/stdout")
        ]

    def _build_env_with_state(self, state: Dict[str, Any]) -> Dict[str, str]:
        """Build environment with serialized state."""
        import json
        import os
        env = os.environ.copy()
        env["TEA_STATE"] = json.dumps(state)
        return env

    def _validate_picklable(self, states: List[Dict]) -> None:
        """Detect non-picklable objects before execution."""
        import pickle
        for i, state in enumerate(states):
            for key, value in state.items():
                try:
                    pickle.dumps(value)
                except (pickle.PicklingError, TypeError) as e:
                    raise ValueError(
                        f"State key '{key}' in flow {i} is not picklable: {e}"
                    )

    def _parse_result(self, stdout: bytes) -> Dict[str, Any]:
        """Parse JSON result from process stdout."""
        import json
        if not stdout:
            return {}
        try:
            return json.loads(stdout.decode())
        except json.JSONDecodeError:
            return {"_raw_output": stdout.decode()}

    @staticmethod
    def _run_flow(flow: Dict[str, Any]) -> Dict[str, Any]:
        """Run a single flow (for ProcessPoolExecutor)."""
        # This runs in a child process
        run_fn = flow.get("run")
        state = flow.get("state", {})
        if callable(run_fn):
            return run_fn(state)
        return state
```

### Hybrid State + Stream Flow

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    PROCESS EXECUTOR WITH STREAMS                        │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ProcessExecutor.execute()                                              │
│  ────────────────────────                                               │
│                                                                         │
│  1. Install SIGPIPE handler                                             │
│  2. Create all pipes (stream_registry.create_all_pipes())               │
│  3. For each flow:                                                      │
│     a. Wire stdin from channel (if configured)                          │
│     b. Wire stdout to channel (if configured)                           │
│     c. Launch subprocess with wired fds                                 │
│     d. Pass state via TEA_STATE env var                                 │
│  4. Wait for all processes (async with timeout)                         │
│  5. Collect exit codes and any stdout (if not piped)                    │
│  6. Cleanup all pipes                                                   │
│                                                                         │
│  ┌──────────────┐         ┌──────────────┐         ┌──────────────┐    │
│  │  Process A   │  pipe   │  Process B   │  pipe   │  Process C   │    │
│  │  (producer)  │────────►│ (transform)  │────────►│  (consumer)  │    │
│  │              │         │              │         │              │    │
│  │ TEA_STATE={} │         │ TEA_STATE={} │         │ TEA_STATE={} │    │
│  └──────────────┘         └──────────────┘         └──────────────┘    │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Testing

### Test Location

`python/tests/test_pipe_executor.py`

### Test Scenarios (22 total)

#### AC1: StreamRegistry Integration (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.2-UNIT-001 | Unit | P0 | execute() accepts stream_registry parameter |
| 001.2-UNIT-002 | Unit | P1 | execute() works without stream_registry (backward compat) |
| 001.2-UNIT-003 | Unit | P1 | Empty stream_registry uses standard execution |

#### AC2: Pipe Wiring (4 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.2-INT-001 | Integration | P0 | Process stdout wired to channel write_fd |
| 001.2-INT-002 | Integration | P0 | Process stdin wired to channel read_fd |
| 001.2-INT-003 | Integration | P1 | Unwired stdin defaults to subprocess.PIPE |
| 001.2-INT-004 | Integration | P1 | Unwired stdout defaults to subprocess.PIPE |

#### AC3: Config-driven Wiring (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.2-INT-005 | Integration | P0 | streams.stdin config determines input source |
| 001.2-INT-006 | Integration | P0 | streams.stdout config determines output dest |
| 001.2-INT-007 | Integration | P1 | Unknown channel name in config raises error |

#### AC4: SIGPIPE Handling (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.2-INT-008 | Integration | P0 | Consumer exits early doesn't crash producer |
| 001.2-INT-009 | Integration | P1 | Producer continues after consumer exit (if writing) |
| 001.2-UNIT-004 | Unit | P1 | SIGPIPE handler installed before pipe creation |

#### AC5: Deadlock Prevention (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.2-INT-010 | Integration | P0 | Large data (>64KB) doesn't deadlock |
| 001.2-INT-011 | Integration | P1 | Async IO prevents buffer full blocking |
| 001.2-INT-012 | Integration | P2 | Timeout kills stuck processes |

#### AC6: Hybrid State+Stream (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.2-INT-013 | Integration | P0 | State passed via TEA_STATE env var |
| 001.2-INT-014 | Integration | P1 | Stream data flows while state is separate |
| 001.2-E2E-001 | E2E | P0 | Full workflow with state and streams |

#### AC7: Exit Codes (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.2-UNIT-005 | Unit | P0 | ParallelFlowResult includes exit_code |
| 001.2-INT-015 | Integration | P1 | Non-zero exit code captured |
| 001.2-INT-016 | Integration | P1 | Timeout sets exit_code to -1 |

---

## Definition of Done

- [ ] `ProcessExecutor.execute()` accepts optional StreamRegistry
- [ ] Pipe wiring based on `streams:` config
- [ ] SIGPIPE handling prevents crashes
- [ ] Async execution prevents deadlocks
- [ ] State passed alongside streams
- [ ] Exit codes in ParallelFlowResult
- [ ] All 22 test scenarios pass
- [ ] Backward compatibility verified
- [ ] Code reviewed and merged

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Deadlock on large data | High | Async IO with non-blocking reads/writes |
| FD leak if process crashes | High | try/finally with cleanup |
| SIGPIPE crash | High | Install SIG_IGN before pipe creation |
| State too large for env var | Medium | Fall back to temp file for large state |

---

## Notes for Developer

1. **Async execution**: Use `asyncio.create_subprocess_exec` for non-blocking subprocess management. This prevents deadlocks when pipe buffers fill.

2. **FD inheritance**: Close FDs in parent after fork. Child process has its own copies.

3. **State passing**: Start with env var (TEA_STATE). If state > 128KB, write to temp file and pass path.

4. **Testing large data**: Create test that writes >64KB to ensure async IO works correctly.

5. **Backward compatibility**: If no stream_registry provided, use existing ProcessPoolExecutor path.

---

## QA Notes

**Date**: 2026-01-02
**Reviewer**: Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 24 |
| Unit tests | 6 (25%) |
| Integration tests | 16 (67%) |
| E2E tests | 2 (8%) |
| AC coverage | 7/7 (100%) |

**Priority Distribution**: 11 P0 (critical), 10 P1 (core), 3 P2 (edge cases)

### Risk Areas Identified

| Risk | Severity | Mitigating Tests |
|------|----------|------------------|
| Deadlock on large data (>64KB) | High | 001.2-INT-010, 001.2-INT-011 |
| FD leak if process crashes | High | 001.2-INT-018 |
| SIGPIPE crash on early consumer exit | High | 001.2-INT-008, 001.2-INT-009, 001.2-UNIT-004 |
| State too large for env var (>128KB) | Medium | 001.2-INT-015 |
| Backward compatibility break | Medium | 001.2-UNIT-002, 001.2-UNIT-003 |

### Recommended Test Scenarios

**Critical Path (P0)**:
1. StreamRegistry parameter acceptance (001.2-UNIT-001)
2. Pipe wiring: stdout→channel, stdin←channel (001.2-INT-001, 001.2-INT-002)
3. Config-driven stream routing (001.2-INT-005, 001.2-INT-006)
4. SIGPIPE handling: consumer early exit (001.2-INT-008)
5. Large data (>64KB) deadlock prevention (001.2-INT-010)
6. TEA_STATE env var state passing (001.2-INT-013)
7. FD cleanup on process crash (001.2-INT-018)
8. Full hybrid state+stream E2E workflow (001.2-E2E-001)

**Test Fixtures Required**:
- `echo_state.py` - prints TEA_STATE for verification
- `large_writer.py` - writes >64KB for deadlock testing
- `slow_reader.py` - reads slowly for async IO validation
- `early_exit.py` - exits after N bytes for SIGPIPE testing
- `infinite_hang.py` - hangs forever for timeout testing

### Concerns / Blockers

1. **Dependency on TEA-STREAM-001.1**: StreamRegistry must be complete before integration tests can run
2. **Dependency on TEA-PARALLEL-001.1**: Executor abstraction layer required
3. **Platform-specific behavior**: SIGPIPE handling differs on Windows (no SIGPIPE signal); tests should skip or mock on Windows
4. **Large state fallback**: Implementation mentions 128KB threshold for temp file fallback, but current design shows only env var; verify temp file path is implemented

### Test Design Reference

Full test design document: `docs/qa/assessments/TEA-STREAM-001.2-test-design-20260102.md`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-02 | 1.0 | Story created from epic | Sarah (PO) |
| 2026-01-02 | 1.1 | QA Notes added | Quinn (QA) |
