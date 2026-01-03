# Story: TEA-PARALLEL-001.3 - Remote Executor Core

## Status: Ready for Development

**SM Review**: 2026-01-01 - Story Checklist PASSED (23/23)
**QA Review**: 2026-01-01 - Test Design Complete (24 scenarios)

**Epic**: [TEA-PARALLEL-001 - Multi-Strategy Parallel Execution](./TEA-PARALLEL-001-multi-strategy-execution-epic.md)
**Estimated Tests**: 20 scenarios (QA expanded to 24)
**Dependencies**:
- [TEA-PARALLEL-001.1](./TEA-PARALLEL-001.1-executor-abstraction.md) (Executor Abstraction)
- [TEA-PARALLEL-001.2](./TEA-PARALLEL-001.2-cli-scoped-execution.md) (CLI Scoped Execution)

---

## User Story

**As a** workflow developer,
**I want** to configure `parallel_strategy: remote` to distribute work across SSH hosts,
**So that** I can scale workflows beyond a single machine.

---

## Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `RemoteExecutor` implements `ParallelExecutor` Protocol | Unit test: Protocol conformance |
| AC2 | Configuration via YAML: `hosts`, `basefile`, `workdir`, `cleanup` | Unit test: parsing |
| AC3 | GNU Parallel command generation when available | Unit test: command structure |
| AC4 | SSH fallback when GNU Parallel not available | Integration test: fallback works |
| AC5 | State serialization to JSON for transfer | Unit test: JSON serialization |
| AC6 | Full YAML + input state transfer to remote (not subset) | Integration test: files transferred |
| AC7 | Remote execution using `--entry-point` and `--exit-point` flags | Integration test: flags passed |
| AC8 | Result collection and deserialization | Integration test: results returned |
| AC9 | Automatic cleanup of remote files (configurable) | Integration test: cleanup runs |
| AC10 | Timeout and retry integration with `ParallelConfig` | Integration test: timeout/retry work |

---

## Technical Design

### Files to Create/Modify

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/parallel_executors.py` | Modify | Add RemoteExecutor class |
| `python/src/the_edge_agent/yaml_engine.py` | Modify | Parse remote settings |

### YAML Configuration

```yaml
settings:
  parallel:
    strategy: remote
    remote:
      hosts:
        - user@server1
        - user@server2
        - user@server3
      basefile: ./tea              # Binary to copy
      workdir: /tmp/tea-jobs       # Remote working directory
      cleanup: true                # Remove files after execution
```

### Remote Execution Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                   REMOTE EXECUTION MODEL                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  MAIN HOST                          REMOTE HOST                 │
│  ──────────                         ───────────                 │
│                                                                 │
│  1. Detect parallel edge            5. Receive files:           │
│     with strategy: remote              - tea binary             │
│                                        - workflow.yaml (FULL)   │
│  2. Serialize current state            - input.json             │
│     to input.json                                               │
│                                     6. Execute:                 │
│  3. Transfer files via SCP             tea run workflow.yaml \  │
│                                          --entry-point nodeX \  │
│  4. SSH execute command                  --exit-point fanIn \   │
│        ─────────────────────────►        --input input.json \   │
│                                          --output result.json   │
│                                                                 │
│  8. Deserialize result.json ◄──── 7. Write final state to      │
│                                       result.json               │
│  9. Continue to fan_in node                                     │
│     with collected results                                      │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### RemoteExecutor Implementation

```python
# parallel_executors.py
import asyncio
import json
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import List, Dict, Any, Optional

@dataclass
class RemoteConfig:
    hosts: List[str]
    basefile: str
    workdir: str = "/tmp/tea-jobs"
    cleanup: bool = True


class RemoteExecutor:
    """Remote executor using SSH/GNU Parallel."""

    def __init__(self, config: RemoteConfig, yaml_path: str):
        self.config = config
        self.yaml_path = yaml_path
        self.has_parallel = shutil.which("parallel") is not None

    def execute(
        self,
        flows: List[Dict],  # Each flow has entry_point, exit_point, state
        config: ParallelConfig
    ) -> List[ParallelFlowResult]:
        """Execute flows on remote hosts."""
        if self.has_parallel:
            return self._execute_with_gnu_parallel(flows, config)
        else:
            return asyncio.run(self._execute_with_ssh_fallback(flows, config))

    def _execute_with_gnu_parallel(
        self,
        flows: List[Dict],
        config: ParallelConfig
    ) -> List[ParallelFlowResult]:
        """Use GNU Parallel for efficient distribution."""
        # Write input files
        input_files = []
        for i, flow in enumerate(flows):
            input_path = f"/tmp/tea_input_{i}.json"
            with open(input_path, "w") as f:
                json.dump(flow["state"], f)
            input_files.append(input_path)

        # Build GNU Parallel command
        hosts = ",".join(self.config.hosts)
        cmd = [
            "parallel",
            f"--sshlogin {hosts}",
            f"--basefile {self.config.basefile}",
            f"--basefile {self.yaml_path}",
            "--transferfile {}",
            "--return {.}.result.json",
        ]
        if self.config.cleanup:
            cmd.append("--cleanup")

        # Add timeout
        if config.timeout_seconds:
            cmd.append(f"--timeout {config.timeout_seconds}")

        # Add command template
        cmd.append(
            f"'cd {self.config.workdir} && "
            f"./tea run workflow.yaml "
            f"--entry-point {{/.}} "
            f"--exit-point {{2}} "
            f"--input {{}} "
            f"--output {{}}.result.json'"
        )

        # Execute
        result = subprocess.run(
            " ".join(cmd),
            shell=True,
            capture_output=True,
            text=True
        )

        # Collect results
        results = []
        for input_path in input_files:
            result_path = f"{input_path}.result.json"
            with open(result_path) as f:
                results.append(ParallelFlowResult(state=json.load(f)))

        return results

    async def _execute_with_ssh_fallback(
        self,
        flows: List[Dict],
        config: ParallelConfig
    ) -> List[ParallelFlowResult]:
        """Fallback to direct SSH when GNU Parallel unavailable."""
        tasks = []
        for i, flow in enumerate(flows):
            host = self.config.hosts[i % len(self.config.hosts)]
            task = self._ssh_execute(
                host=host,
                entry_point=flow["entry_point"],
                exit_point=flow["exit_point"],
                state=flow["state"],
                config=config
            )
            tasks.append(task)

        results = await asyncio.gather(*tasks, return_exceptions=True)

        return [
            ParallelFlowResult(state=r) if not isinstance(r, Exception)
            else ParallelFlowResult(error=str(r))
            for r in results
        ]

    async def _ssh_execute(
        self,
        host: str,
        entry_point: str,
        exit_point: str,
        state: Dict,
        config: ParallelConfig
    ) -> Dict:
        """Execute on a single remote host via SSH."""
        import tempfile

        # Create temp input file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            json.dump(state, f)
            input_file = f.name

        try:
            # 1. Create remote workdir
            await self._ssh_run(host, f"mkdir -p {self.config.workdir}")

            # 2. SCP files to remote
            await self._scp_to_remote(host, [
                self.config.basefile,
                self.yaml_path,
                input_file
            ])

            # 3. Execute remotely
            remote_input = f"{self.config.workdir}/{Path(input_file).name}"
            remote_output = f"{remote_input}.result.json"

            cmd = (
                f"cd {self.config.workdir} && "
                f"./tea run workflow.yaml "
                f"--entry-point {entry_point} "
                f"--exit-point {exit_point} "
                f"--input {remote_input} "
                f"--output {remote_output}"
            )

            await self._ssh_run(host, cmd, timeout=config.timeout_seconds)

            # 4. SCP result back
            local_result = f"{input_file}.result.json"
            await self._scp_from_remote(host, remote_output, local_result)

            # 5. Read result
            with open(local_result) as f:
                result = json.load(f)

            # 6. Cleanup
            if self.config.cleanup:
                await self._ssh_run(
                    host,
                    f"rm -rf {self.config.workdir}/*"
                )

            return result

        finally:
            # Cleanup local temp files
            Path(input_file).unlink(missing_ok=True)
            Path(f"{input_file}.result.json").unlink(missing_ok=True)

    async def _ssh_run(
        self,
        host: str,
        command: str,
        timeout: Optional[int] = None
    ) -> str:
        """Run command on remote host via SSH."""
        ssh_cmd = ["ssh", host, command]

        proc = await asyncio.create_subprocess_exec(
            *ssh_cmd,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE
        )

        try:
            stdout, stderr = await asyncio.wait_for(
                proc.communicate(),
                timeout=timeout
            )
        except asyncio.TimeoutError:
            proc.kill()
            raise TimeoutError(f"SSH command timed out after {timeout}s")

        if proc.returncode != 0:
            raise RuntimeError(f"SSH command failed: {stderr.decode()}")

        return stdout.decode()

    async def _scp_to_remote(self, host: str, files: List[str]) -> None:
        """Copy files to remote host."""
        for file in files:
            cmd = ["scp", file, f"{host}:{self.config.workdir}/"]
            proc = await asyncio.create_subprocess_exec(*cmd)
            await proc.wait()
            if proc.returncode != 0:
                raise RuntimeError(f"SCP failed for {file}")

    async def _scp_from_remote(
        self,
        host: str,
        remote_path: str,
        local_path: str
    ) -> None:
        """Copy file from remote host."""
        cmd = ["scp", f"{host}:{remote_path}", local_path]
        proc = await asyncio.create_subprocess_exec(*cmd)
        await proc.wait()
        if proc.returncode != 0:
            raise RuntimeError(f"SCP failed for {remote_path}")
```

### Error Handling

| Error Type | Behavior | Retry? |
|------------|----------|--------|
| SSH auth failure | Fail fast, clear error | No |
| Network timeout | Retry with backoff | Yes |
| Remote command failure | Capture stderr, return error | Configurable |
| SCP failure | Retry transfer | Yes |
| JSON parse error | Return error result | No |

---

## Testing

### Test Location

`python/tests/test_remote_executor.py`

### Test Scenarios (20 total)

#### AC1-2: Protocol & Config (4 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.3-UNIT-001 | Unit | P0 | RemoteExecutor implements Protocol |
| 001.3-UNIT-002 | Unit | P1 | Initializes with required config |
| 001.3-UNIT-003 | Unit | P1 | YAMLEngine parses `hosts` as list |
| 001.3-UNIT-004 | Unit | P1 | YAMLEngine parses `workdir` with default |

#### AC3-4: GNU Parallel & SSH (4 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.3-UNIT-005 | Unit | P1 | Detect GNU Parallel availability |
| 001.3-UNIT-006 | Unit | P1 | Generate correct GNU Parallel command |
| 001.3-INT-001 | Integration | P1 | SSH fallback when Parallel unavailable |
| 001.3-INT-002 | Integration | P2 | Log which method is being used |

#### AC5-8: File Transfer & Execution (6 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.3-UNIT-007 | Unit | P0 | State serializes to valid JSON |
| 001.3-INT-003 | Integration | P1 | SCP transfers files (mocked) |
| 001.3-INT-004 | Integration | P1 | Full YAML transferred (not subset) |
| 001.3-INT-005 | Integration | P1 | Remote command uses scope flags |
| 001.3-INT-006 | Integration | P0 | Result JSON deserialized correctly |
| 001.3-E2E-001 | E2E | P1 | Full cycle with localhost SSH |

#### AC9: Cleanup (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.3-INT-007 | Integration | P2 | Cleanup removes remote files |
| 001.3-INT-008 | Integration | P2 | Cleanup skipped when disabled |
| 001.3-INT-009 | Integration | P3 | Cleanup failure logged, doesn't fail |

#### AC10: Timeout & Retry (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.3-INT-010 | Integration | P0 | SSH timeout from ParallelConfig |
| 001.3-INT-011 | Integration | P0 | Network failure triggers retry |
| 001.3-INT-012 | Integration | P1 | Circuit breaker per host |

---

## Definition of Done

- [ ] `RemoteExecutor` class implements `ParallelExecutor` Protocol
- [ ] YAML parsing for `settings.parallel.remote.*`
- [ ] GNU Parallel command generation
- [ ] SSH fallback implementation
- [ ] File transfer (SCP) for binary, YAML, and state
- [ ] Result collection and deserialization
- [ ] Configurable cleanup
- [ ] Timeout and retry integration
- [ ] All 20 test scenarios pass
- [ ] Code reviewed and merged

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| SSH key not configured | High | Clear error message with setup instructions |
| Network instability | Medium | Retry with exponential backoff |
| Remote disk full | Medium | Check disk space before transfer |
| Binary compatibility | Medium | Recommend same OS/arch |

---

## Notes for Developer

1. **Mock SSH for tests**: Use `unittest.mock` to mock `subprocess` and `asyncio.create_subprocess_exec` for unit tests.

2. **Localhost integration test**: One test (001.3-E2E-001) should use `localhost` SSH if available, otherwise skip with `@pytest.mark.skipif`.

3. **GNU Parallel detection**: Use `shutil.which("parallel")` to detect availability.

4. **Full YAML, not subset**: Transfer the complete workflow.yaml file. The `--entry-point` and `--exit-point` flags control what gets executed.

5. **Async execution**: Use `asyncio` for concurrent SSH connections in fallback mode.

---

## QA Results

### Test Design Review - 2026-01-01

**Reviewer**: Quinn (Test Architect)
**Assessment**: `docs/qa/assessments/TEA-PARALLEL-001.3-test-design-20260101.md`

#### Summary

| Metric | Story Estimate | QA Design |
|--------|----------------|-----------|
| Total scenarios | 20 | 24 |
| Unit tests | - | 10 (42%) |
| Integration tests | - | 12 (50%) |
| E2E tests | - | 2 (8%) |

#### Priority Distribution

| Priority | Count | Coverage |
|----------|-------|----------|
| P0 (Critical) | 7 | Protocol, serialization, core flow, timeout/retry |
| P1 (High) | 11 | Config parsing, command gen, file transfer, results |
| P2 (Medium) | 4 | Cleanup, logging |
| P3 (Low) | 2 | Edge cases |

#### Key P0 Tests (Must Pass)

1. `001.3-UNIT-001` - RemoteExecutor implements ParallelExecutor Protocol
2. `001.3-UNIT-011/012` - State JSON serialization integrity
3. `001.3-INT-003` - SSH fallback activates when GNU Parallel unavailable
4. `001.3-INT-006` - Full YAML transfer (complete file, not subset)
5. `001.3-INT-009/010` - Entry/exit point flags in remote command
6. `001.3-INT-012/013` - Result collection and deserialization
7. `001.3-INT-018/019` - Timeout and retry handling

#### Risk Mitigations Verified

| Risk | Test Coverage |
|------|---------------|
| SSH fallback | 001.3-INT-003, 001.3-INT-004 |
| Network timeout | 001.3-INT-018, 001.3-INT-021 |
| Retry logic | 001.3-INT-019, 001.3-INT-020 |
| Serialization integrity | 001.3-UNIT-011, 001.3-UNIT-012, 001.3-UNIT-013 |

#### Coverage Gaps (Documented)

- Remote disk space pre-check (recommend future enhancement)
- Binary architecture validation (document OS/arch requirement)

#### Recommendations

1. **Mocking strategy**: Use `unittest.mock` for subprocess and asyncio calls
2. **E2E skip condition**: Use `@pytest.mark.skipif` when localhost SSH unavailable
3. **Test execution order**: P0 unit tests first (fail fast), then P0 integration

#### Status: Ready for Development

Test design is comprehensive and covers all 10 acceptance criteria with appropriate test levels and priorities.

---

## QA Notes

### Test Coverage Summary

- **24 total test scenarios** designed (QA expanded from story's 20 estimate)
- **Distribution**: 10 unit (42%), 12 integration (50%), 2 E2E (8%)
- **Priority**: 7 P0, 11 P1, 4 P2, 2 P3
- **All 10 acceptance criteria** have dedicated test coverage
- **Given-When-Then patterns** documented for all major scenarios

### Risk Areas Identified

| Risk | Severity | Mitigation Status |
|------|----------|-------------------|
| SSH fallback failure | High | Covered by 001.3-INT-003, 001.3-INT-004 |
| Network timeout/hang | High | Covered by 001.3-INT-018, 001.3-INT-021 |
| State serialization corruption | High | Covered by 001.3-UNIT-011, 001.3-UNIT-012, 001.3-UNIT-013 |
| Retry exhaustion | Medium | Covered by 001.3-INT-019, 001.3-INT-020 |
| Remote disk full | Medium | **GAP** - Not covered, recommend pre-flight check |
| Binary architecture mismatch | Medium | **GAP** - Not covered, documented as requirement |
| SSH key passphrase prompt | Low | Documented key-based auth requirement |

### Recommended Test Scenarios

**Critical Path (P0 - Must Pass)**:
1. Protocol conformance: `001.3-UNIT-001`
2. JSON serialization integrity: `001.3-UNIT-011`, `001.3-UNIT-012`
3. SSH fallback activation: `001.3-INT-003`
4. Full YAML transfer (not subset): `001.3-INT-006`
5. Entry/exit scope flags: `001.3-INT-009`, `001.3-INT-010`
6. Result collection/deserialization: `001.3-INT-012`, `001.3-INT-013`
7. Timeout handling: `001.3-INT-018`, `001.3-INT-019`

**E2E Validation**:
- `001.3-E2E-001`: Full localhost SSH cycle (skip if SSH unavailable)
- Tests complete file transfer, remote execution, result collection, and cleanup

### Concerns / Blockers

1. **E2E test environment**: Requires SSH to localhost to be configured. Test must gracefully skip when unavailable using `@pytest.mark.skipif`.

2. **Mocking complexity**: Integration tests require mocking `asyncio.create_subprocess_exec` and `shutil.which`. Recommend using `AsyncMock` pattern documented in test design.

3. **Coverage gaps not blocking**:
   - Remote disk space pre-check is a nice-to-have enhancement
   - Binary architecture validation should be documented in user guide

4. **Test execution order matters**: P0 unit tests should run first (fail-fast strategy), followed by P0 integration tests.

### Testing Recommendations

- Use `unittest.mock.patch` for subprocess and asyncio calls
- Use `@pytest.mark.skipif` for E2E tests requiring SSH
- Ensure temp file cleanup in `finally` blocks to avoid test pollution
- Consider adding circuit breaker per-host for production resilience

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-01 | 1.0 | Story extracted from epic | Sarah (PO) |
| 2026-01-01 | 1.1 | Test design completed (24 scenarios) | Quinn (QA) |
| 2026-01-01 | 1.2 | Story checklist validated (23/23 PASS) | Bob (SM) |
