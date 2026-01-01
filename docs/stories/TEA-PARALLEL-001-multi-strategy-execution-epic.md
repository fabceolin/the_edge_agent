# Epic: TEA-PARALLEL-001 - Multi-Strategy Parallel Execution

## Status: Ready for Development

**QA Review Passed:** 2025-12-29 (updated 2026-01-01)
- All acceptance criteria have test coverage (82 scenarios)
- Quality checklist complete
- No blockers identified
- Risk mitigations mapped to tests

## Epic Title
Multi-Strategy Parallel Execution (thread | process | remote)

## Epic Goal
Permitir que workflows com fan-out utilizem diferentes estratégias de execução paralela: threads (atual), processos (multiprocessing) e remota (SSH/GNU Parallel), configurável por edge ou globalmente.

## Epic Description

### Existing System Context

- **Funcionalidade atual**: Fan-out paralelo via `ThreadPoolExecutor` no `stategraph.py`
- **Infraestrutura robusta existente**: `ParallelConfig`, `RetryPolicy`, `CircuitBreaker`, `CancellationToken` em `parallel.py`
- **YAML já suporta**: `parallel: true` e `fan_in:` nas edges
- **Integration points**: `_execute_parallel_flows()` no StateGraph

### Enhancement Details

- **O que será adicionado**: Nova propriedade `parallel_strategy` nas edges e settings globais
- **Como integra**: Abstração de executor que delega para Thread/Process/Remote backends
- **Success criteria**:
  - Testes existentes passam com strategy=thread (default)
  - Process backend funciona para CPU-bound tasks
  - Remote backend executa via SSH com state serialização

### YAML Configuration Preview

```yaml
# Global default strategy
settings:
  parallel:
    strategy: thread  # thread | process | remote

    # Remote-specific settings (only when strategy: remote)
    remote:
      hosts:
        - user@server1
        - user@server2
      basefile: ./tea          # Binary to copy
      workdir: /tmp/tea-jobs   # Remote working directory
      cleanup: true            # Remove files after execution

# Per-edge override
edges:
  - from: __start__
    to: [branch_a, branch_b, branch_c]
    parallel: true
    fan_in: merge_results
    parallel_strategy: process  # Override global setting for this edge
```

## Story Dependency Graph

```
                    ┌──────────────────────┐
                    │ Story 1: Executor    │
                    │ Abstraction + Process│
                    │ (TEA-PARALLEL-001.1) │
                    └──────────┬───────────┘
                               │
              ┌────────────────┴────────────────┐
              │                                 │
              ▼                                 │
┌─────────────────────────┐                     │
│ Story 2: CLI Scoped     │                     │
│ Execution               │                     │
│ (TEA-PARALLEL-001.2)    │                     │
└───────────┬─────────────┘                     │
            │                                   │
            ▼                                   │
┌─────────────────────────┐                     │
│ Story 3: Remote         │◄────────────────────┘
│ Executor Core           │
│ (TEA-PARALLEL-001.3)    │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│ Story 4: Remote         │
│ Environment & Security  │
│ (TEA-PARALLEL-001.4)    │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│ Story 5: Integration    │
│ Testing + Documentation │
│ (TEA-PARALLEL-001.5)    │
└─────────────────────────┘
```

## Stories

---

### Story 1: Parallel Executor Abstraction + Process Backend

**ID**: TEA-PARALLEL-001.1
**Estimated Tests**: 21 scenarios
**Dependencies**: None

**As a** workflow developer,
**I want** to configure `parallel_strategy: process` for CPU-bound parallel flows,
**So that** I can bypass Python's GIL for compute-intensive tasks.

#### Acceptance Criteria

1. Nova abstração `ParallelExecutor` com interface comum (Protocol)
2. `ThreadExecutor` encapsula implementação atual (sem mudanças de comportamento)
3. `ProcessExecutor` usa `ProcessPoolExecutor` do `concurrent.futures`
4. State é serializado via pickle entre processos
5. Todos os testes existentes de parallelism passam com default (thread)
6. YAML suporta `parallel_strategy: thread | process` em edges e settings

#### Technical Notes

**Files to Create/Modify**:
- Create: `python/src/the_edge_agent/parallel_executors.py`
- Modify: `python/src/the_edge_agent/stategraph.py` (use executor abstraction)
- Modify: `python/src/the_edge_agent/yaml_engine.py` (parse parallel_strategy)

**Implementation Approach**:
```python
from typing import Protocol, List, Dict, Any, Callable
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor

class ParallelExecutor(Protocol):
    """Protocol for parallel execution strategies."""

    def execute(
        self,
        flows: List[Callable],
        states: List[Dict[str, Any]],
        config: ParallelConfig
    ) -> List[ParallelFlowResult]:
        """Execute flows in parallel and return results."""
        ...

class ThreadExecutor:
    """Thread-based executor (current implementation)."""
    ...

class ProcessExecutor:
    """Process-based executor for CPU-bound tasks."""
    ...
```

**Serialization Handling**:
- Process executor must handle non-picklable objects gracefully
- Detect and report problematic state keys before execution
- Consider JSON serialization as fallback for complex objects

**Testing**:
- Location: `python/tests/test_parallel_executors.py`
- Verify thread executor matches current behavior exactly
- Test process executor with simple and complex states
- Test serialization error handling

---

### Story 2: CLI Scoped Execution

**ID**: TEA-PARALLEL-001.2
**Estimated Tests**: 15 scenarios
**Dependencies**: Story 1 (executor abstraction)

**As a** workflow developer,
**I want** CLI flags to execute a portion of a workflow graph,
**So that** remote executors can run specific branches without full workflow execution.

#### Acceptance Criteria

1. `--entry-point <node>` flag starts execution at specified node instead of `__start__`
2. `--exit-point <node>` flag stops execution BEFORE specified node (node not executed)
3. `--input <file.json>` flag loads initial state from JSON file
4. `--output <file.json>` flag writes final state to JSON file
5. `execute_scoped()` method in StateGraph for programmatic scoped execution
6. Validation: entry-point node must exist in graph
7. Validation: exit-point node must exist in graph
8. Validation: path must exist from entry-point to exit-point
9. Linear subgraph execution (chains of nodes) supported

#### Technical Notes

**Files to Create/Modify**:
- Modify: `python/src/the_edge_agent/cli.py` (add --entry-point, --exit-point, --input, --output flags)
- Modify: `python/src/the_edge_agent/stategraph.py` (add execute_scoped method)

**New CLI Flags**:
```bash
tea run <workflow.yaml> [OPTIONS]

Execution Scope Options:
  --entry-point <node>    Start execution at this node instead of __start__
  --exit-point <node>     Stop execution BEFORE this node (don't execute it)
  --input <file.json>     Load initial state from JSON file
  --output <file.json>    Write final state to JSON file

Examples:
  # Normal execution
  tea run workflow.yaml

  # Scoped execution (for remote parallel branches)
  tea run workflow.yaml --entry-point analyze --exit-point merge --input state.json --output result.json
```

**Scoped Execution Implementation**:
```python
# stategraph.py
def execute_scoped(
    self,
    initial_state: Dict[str, Any],
    entry_point: str,
    exit_point: str
) -> Dict[str, Any]:
    """
    Execute a portion of the graph from entry_point to exit_point.

    Args:
        initial_state: Starting state (loaded from --input)
        entry_point: Node to start execution (instead of __start__)
        exit_point: Node to stop BEFORE (not executed)

    Returns:
        Final state (written to --output)
    """
    # Validate scope
    if entry_point not in self.nodes:
        raise ValueError(f"Entry point '{entry_point}' not found in graph")
    if exit_point not in self.nodes and exit_point != "__end__":
        raise ValueError(f"Exit point '{exit_point}' not found in graph")

    # Verify path exists
    if not self._path_exists(entry_point, exit_point):
        raise ValueError(f"No execution path from '{entry_point}' to '{exit_point}'")

    # Execute with modified boundaries
    return self._run_graph(
        state=initial_state,
        start_node=entry_point,
        stop_before=exit_point
    )
```

**Linear Subgraph Support**:

Remote execution supports **linear subgraphs** (chains of nodes):
```yaml
# Original workflow with linear branch
edges:
  - from: fetch_data
    to: [calc_indicators, fetch_financials]
    parallel: true
    parallel_strategy: remote
    fan_in: merge_analysis

  # Branch A: Linear chain (all executed on one remote host)
  - from: calc_indicators
    to: identify_patterns
  - from: identify_patterns
    to: generate_signals

  # Branch B: Another chain
  - from: fetch_financials
    to: calc_ratios

# Remote execution for Branch A:
# tea run workflow.yaml --entry-point calc_indicators --exit-point merge_analysis
# This executes: calc_indicators → identify_patterns → generate_signals
```

**Testing**:
- Location: `python/tests/test_cli_scope_flags.py`
- Test each flag parsing
- Test validation errors (missing node, no path)
- Test state loading and saving
- Test scoped execution with linear chains

---

### Story 3: Remote Executor Core

**ID**: TEA-PARALLEL-001.3
**Estimated Tests**: 20 scenarios
**Dependencies**: Story 1 (executor abstraction), Story 2 (scoped execution)

**As a** workflow developer,
**I want** to configure `parallel_strategy: remote` to distribute work across SSH hosts,
**So that** I can scale workflows beyond a single machine.

#### Acceptance Criteria

1. `RemoteExecutor` implements `ParallelExecutor` Protocol
2. Configuration via YAML: `hosts`, `basefile`, `workdir`, `cleanup`
3. GNU Parallel command generation when available
4. SSH fallback when GNU Parallel not available
5. State serialization to JSON for transfer
6. Full YAML + input state transfer to remote (not subset)
7. Remote execution using `--entry-point` and `--exit-point` flags
8. Result collection and deserialization back to pipeline
9. Automatic cleanup of remote files (configurable)
10. Timeout and retry integration with existing `ParallelConfig`

#### Technical Notes

**Files to Create/Modify**:
- Modify: `python/src/the_edge_agent/parallel_executors.py` (add RemoteExecutor)
- Modify: `python/src/the_edge_agent/yaml_engine.py` (parse remote settings)

**Design Decision: Full YAML + Scope Execution**

Instead of generating a YAML subset, we copy the **full original YAML** to remote hosts and use CLI flags to specify the execution scope. This approach:
- Maximizes feature support (nested parallel, dynamic parallel, conditionals, LTM)
- Simplifies implementation (no subset extraction logic)
- Ensures remote execution uses the same engine as local

**Remote Execution Flow**:
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

**GNU Parallel Integration**:
```bash
# When GNU Parallel is available
parallel --sshlogin server1,server2 \
         --basefile ./tea \
         --basefile workflow.yaml \
         --transferfile {} \
         --return {.}.result.json \
         --cleanup \
         './tea run workflow.yaml --entry-point {/.} --exit-point merge_results --input {} --output {.}.result.json' \
         ::: branch_a.json branch_b.json branch_c.json
```

**SSH Fallback** (when parallel not available):
```python
async def _ssh_execute(
    self,
    host: str,
    entry_point: str,
    exit_point: str,
    input_file: str
) -> Dict:
    # 1. scp files to remote (FULL yaml, not subset)
    await self._scp_to_remote(host, [self.binary, self.yaml_path, input_file])

    # 2. Build SSH command
    ssh_cmd = self._build_ssh_command(host)

    # 3. ssh execute with scope flags
    remote_cmd = (
        f"cd {self.workdir} && "
        f"./tea run workflow.yaml "
        f"--entry-point {entry_point} "
        f"--exit-point {exit_point} "
        f"--input {input_file} "
        f"--output {input_file}.result.json"
    )
    result = await self._ssh_run(ssh_cmd, remote_cmd)

    # 4. scp result back
    await self._scp_from_remote(host, f"{input_file}.result.json")

    # 5. cleanup if configured
    if self.cleanup:
        await self._ssh_run(ssh_cmd, f"rm -f ./tea workflow.yaml {input_file}*")

    return json.load(open(f"{input_file}.result.json"))
```

**Error Handling**:
- Network failures: retry with exponential backoff
- SSH auth failures: fail fast with clear error message
- Remote execution failures: capture stderr, propagate to ParallelFlowResult
- Partial failures: configurable fail_fast or collect all results

**Testing**:
- Location: `python/tests/test_remote_executor.py`
- Mock SSH/scp for unit tests
- Integration tests with local "remote" (localhost via SSH)
- Test timeout handling
- Test cleanup behavior

---

### Story 4: Remote Environment & Security

**ID**: TEA-PARALLEL-001.4
**Estimated Tests**: 19 scenarios
**Dependencies**: Story 3 (remote executor core)

**As a** workflow developer,
**I want** secure environment variable transfer and validation for remote execution,
**So that** I can safely use API keys and secrets across distributed workflows.

#### Acceptance Criteria

1. Environment variable transfer via `env_vars` configuration:
   - `include`: Explicit whitelist of env vars to transfer
   - `exclude_patterns`: Glob patterns to exclude (e.g., `*_SECRET`)
   - `mode`: Transfer mechanism (`ssh_env` | `export_file` | `none`)
2. Secrets backend awareness: Log info when `secrets.backend != env`
3. Full engine features supported on remote:
   - Nested `parallel: true` edges (using local thread/process strategy)
   - Nested `dynamic_parallel` nodes
   - Conditional edges
   - LTM operations (with distributed backend)
   - Rate limiting (per-host limiters)
4. Interrupt point validation: Error if `interrupt_before`/`interrupt_after` in remote scope
5. Checkpoint behavior warnings for non-distributed backends
6. LTM backend warnings for local sqlite with remote strategy

#### Technical Notes

**Files to Create/Modify**:
- Create: `python/src/the_edge_agent/remote_env.py` (environment variable transfer logic)
- Modify: `python/src/the_edge_agent/parallel_executors.py` (add validation)
- Modify: `python/src/the_edge_agent/stategraph.py` (interrupt validation)

**YAML Configuration:**
```yaml
settings:
  parallel:
    strategy: remote
    remote:
      hosts:
        - user@server1
        - user@server2
      basefile: ./tea
      workdir: /tmp/tea-jobs
      cleanup: true

      # Environment variable transfer configuration
      env_vars:
        # Explicit whitelist
        include:
          - OPENAI_API_KEY
          - ANTHROPIC_API_KEY
          - LOG_LEVEL
        # Pattern-based exclusion (applied after include)
        exclude_patterns:
          - "*_SECRET"
          - "*_PASSWORD"
        # Transfer mode
        mode: ssh_env  # ssh_env | export_file | none
```

**Environment Variable Transfer Implementation**:
```python
# remote_env.py
from dataclasses import dataclass
from typing import List, Literal
from fnmatch import fnmatch
import os

@dataclass
class EnvVarsConfig:
    include: List[str]                    # Explicit whitelist
    exclude_patterns: List[str] = None    # Glob patterns to exclude
    mode: Literal["ssh_env", "export_file", "none"] = "ssh_env"

class RemoteEnvHandler:
    def __init__(self, config: EnvVarsConfig):
        self.config = config

    def get_filtered_env_vars(self) -> dict:
        """Get env vars matching include list, minus exclude patterns."""
        result = {}
        for var in self.config.include:
            if var in os.environ:
                # Check exclude patterns
                excluded = any(
                    fnmatch(var, pattern)
                    for pattern in (self.config.exclude_patterns or [])
                )
                if not excluded:
                    result[var] = os.environ[var]
        return result

    def build_ssh_options(self) -> List[str]:
        """Build SSH -o SendEnv options for ssh_env mode."""
        if self.config.mode != "ssh_env":
            return []
        env_vars = self.get_filtered_env_vars()
        return [f"-o SendEnv={var}" for var in env_vars.keys()]

    def generate_export_script(self, path: str) -> str:
        """Generate env.sh for export_file mode."""
        if self.config.mode != "export_file":
            return None
        env_vars = self.get_filtered_env_vars()
        lines = [f"export {k}='{v}'" for k, v in env_vars.items()]
        script = "#!/bin/bash\n" + "\n".join(lines)
        with open(path, "w") as f:
            f.write(script)
        return path
```

**Transfer Modes:**

| Mode | Mechanism | Security | Use Case |
|------|-----------|----------|----------|
| `ssh_env` | `ssh -o SendEnv=VAR` | Medium (requires server `AcceptEnv` config) | Standard deployment |
| `export_file` | Write to temp file, source on remote | Lower (file on disk briefly) | Servers without `AcceptEnv` |
| `none` | Remote uses own environment | High (no transfer) | Pre-configured servers |

**Interrupt Point Validation:**
```python
def _validate_remote_scope(self, entry_point: str, exit_point: str) -> List[str]:
    """Validate that remote execution scope doesn't contain interrupt points."""
    errors = []

    # Get all nodes in the execution path
    nodes_in_scope = self._get_nodes_between(entry_point, exit_point)

    # Check for interrupt points
    for node_name in nodes_in_scope:
        node = self.nodes[node_name]
        if hasattr(node, 'interrupt_before') and node.interrupt_before:
            errors.append(
                f"Node '{node_name}' has interrupt_before=True, which is not "
                f"supported in remote execution scope"
            )
        if hasattr(node, 'interrupt_after') and node.interrupt_after:
            errors.append(
                f"Node '{node_name}' has interrupt_after=True, which is not "
                f"supported in remote execution scope"
            )

    return errors
```

**Checkpoint Behavior with Remote Strategy**:

| Checkpoint Backend | Remote Behavior |
|--------------------|-----------------|
| In-memory (default) | ❌ Not shared - each host has own checkpoints |
| File-based (local) | ❌ Not shared - each host has own files |
| Distributed (S3/GCS) | ✅ Shared - all hosts access same checkpoint store |

**LTM Backend Requirements**:

| LTM Backend | Remote Support | Requirements |
|-------------|----------------|--------------|
| `sqlite` (default) | ❌ No (local file) | Each host has isolated LTM |
| `duckdb` | ✅ Yes | Shared cloud storage (S3, GCS, Azure Blob) |
| `litestream` | ✅ Yes | S3 replication target |
| `blob-sqlite` | ✅ Yes | Shared blob storage |

**Testing**:
- Location: `python/tests/test_remote_env.py`, `python/tests/test_remote_validation.py`
- Test env var filtering with include/exclude
- Test SSH SendEnv option generation
- Test export script generation
- Test interrupt point detection
- Test warnings for local backends

---

### Story 5: Integration Testing + Documentation

**ID**: TEA-PARALLEL-001.5
**Estimated Tests**: 7 scenarios
**Dependencies**: Stories 1-4

**As a** workflow developer,
**I want** comprehensive documentation and examples for parallel strategies,
**So that** I can choose the right strategy for my use case.

#### Acceptance Criteria

1. Integration tests for each strategy (mock SSH for remote)
2. Compatibility tests: verify thread remains default
3. Documentation in `YAML_REFERENCE.md` with trade-offs table
4. Example YAML demonstrating each strategy
5. Error handling documented for each failure mode

#### Technical Notes

**Documentation Updates**:
- File: `docs/shared/YAML_REFERENCE.md`
- Add section: "Parallel Execution Strategies"

**Trade-offs Table**:

| Strategy | Use Case | Pros | Cons |
|----------|----------|------|------|
| `thread` | I/O-bound tasks, API calls | Low overhead, shared memory | GIL limits CPU parallelism |
| `process` | CPU-bound tasks | True parallelism, bypasses GIL | Serialization overhead, no shared state |
| `remote` | Distributed execution | Horizontal scaling, heterogeneous nodes | Network latency, setup complexity |

**Example YAML**:
- Create: `examples/parallel_strategies_demo.yaml`

**Testing Matrix**:

| Test Case | Thread | Process | Remote |
|-----------|--------|---------|--------|
| Simple fan-out/fan-in | ✓ | ✓ | ✓ (mocked) |
| Complex state | ✓ | ✓ (pickle) | ✓ (JSON) |
| Timeout handling | ✓ | ✓ | ✓ |
| Retry on failure | ✓ | ✓ | ✓ |
| Circuit breaker | ✓ | ✓ | ✓ |
| Non-serializable state | N/A | Error | Error |
| Network failure | N/A | N/A | Retry/Error |

---

## Feature Interaction Limitations

This section documents how `parallel_strategy` interacts with other TEA features. These limitations arise from fundamental differences between execution contexts (threads share memory; processes and remote hosts do not).

### Interaction Matrix

| Feature | `thread` | `process` | `remote` |
|---------|----------|-----------|----------|
| **`ratelimit.wrap`** | ✅ Shared limiters | ⚠️ Per-process limiters | ⚠️ Per-host limiters |
| **`cache.wrap`** | ✅ Shared cache | ⚠️ Per-process cache | ⚠️ Per-host cache |
| **Environment Variables** | ✅ Inherited | ✅ Inherited at spawn | ⚙️ Explicit transfer |
| **Secrets (TEA-BUILTIN-012)** | ✅ Available | ✅ Inherited | ⚙️ Backend-dependent |
| **LTM (Long-Term Memory)** | ✅ Available | ✅ Inherited | ✅ With distributed backend |
| **Nested parallel** | ✅ Available | ✅ Available | ✅ Uses local strategy |
| **Dynamic parallel** | ✅ Available | ✅ Available | ✅ Available |
| **Conditional edges** | ✅ Available | ✅ Available | ✅ Available |
| **Interrupt points** | ✅ Available | ✅ Available | ❌ Not in remote scope |
| **State serialization** | N/A (shared memory) | Pickle required | JSON required |

### Rate Limiting Behavior

**Behavior:** Warn and degrade gracefully.

When `ratelimit.wrap` is used with `process` or `remote` strategies:

1. **Warning Logged:** `"Rate limiter '{name}' is per-{process|host}; global coordination unavailable with parallel_strategy={strategy}"`
2. **Limiter Scope:**
   - `thread`: Limiter shared across all parallel branches (coordinated)
   - `process`: Each process gets independent limiter (may exceed intended rate × N processes)
   - `remote`: Each host gets independent limiter (may exceed intended rate × N hosts)

### Cache Behavior

**Behavior:** Warn and degrade gracefully.

When `cache.wrap` is used with `process` or `remote` strategies:

1. **Warning Logged:** `"Cache '{key_strategy}' is per-{process|host}; cache sharing unavailable with parallel_strategy={strategy}"`
2. **Cache Scope:**
   - `thread`: Shared in-memory cache (deduplication works)
   - `process`: Each process has own cache (duplicate work possible)
   - `remote`: Each host has own filesystem cache (duplicate work possible)

### Serialization Requirements

| Strategy | State Serialization | Requirements |
|----------|---------------------|--------------|
| `thread` | None (shared memory) | State can contain any Python object |
| `process` | Pickle | State must be picklable (no lambdas, open files, connections) |
| `remote` | JSON | State must be JSON-serializable (no datetime, bytes, custom classes) |

---

## Compatibility Requirements

- [x] APIs existentes não mudam (parallel_strategy é opcional)
- [x] Default é `thread` (comportamento atual preservado)
- [x] Testes existentes passam sem modificação
- [x] Performance impact mínimo quando usando thread

## Risk Mitigation

| Risk | Impact | Mitigation |
|------|--------|------------|
| Process serialization failures | Medium | Detect non-picklable objects early, provide clear errors |
| SSH connection failures | High | Retry with backoff, circuit breaker per host |
| State size too large for transfer | Medium | Compression, threshold warning, streaming for large states |
| Orphan processes on remote | Low | Cleanup script, SSH connection timeout |
| Version mismatch (local vs remote binary) | Medium | Version check before execution |
| Interrupt in remote scope | High | Validation error at compile time |

**Rollback Plan**:
- Feature flag `TEA_PARALLEL_STRATEGY_ENABLED=false` to disable new strategies
- Default to thread if strategy parsing fails

---

## Definition of Done

- [ ] **Story 1**: Executor abstraction + Process backend implemented
- [ ] **Story 2**: CLI scoped execution flags implemented
- [ ] **Story 3**: Remote executor core with SSH/GNU Parallel support
- [ ] **Story 4**: Environment variable transfer and security validation
- [ ] **Story 5**: Documentation and integration tests
- [ ] All existing parallelism tests pass
- [ ] New tests for process and remote strategies
- [ ] YAML_REFERENCE.md updated
- [ ] No regression in existing functionality

## Dependencies

**Runtime Dependencies:**
- GNU Parallel (optional, for remote strategy optimization)
- SSH client configured on execution host
- Python 3.9+ (for ProcessPoolExecutor improvements)

**Related Epics:**
- **TEA-BUILTIN-012** (Cloud Secrets Backend) - Required for `secrets.backend` integration with remote strategy
- **TEA-BUILTIN-011** (Rate Limiting) - Interaction documented in "Feature Interaction Limitations"
- **TEA-BUILTIN-013** (Deferred) - [Distributed rate limiting](./TEA-BUILTIN-013-distributed-ratelimit-deferred.md) for coordinated limits across processes/hosts

## QA Notes

**Date:** 2026-01-01
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 82 |
| Unit tests | 32 (39%) |
| Integration tests | 34 (41%) |
| E2E tests | 16 (20%) |
| Priority distribution | P0: 24, P1: 37, P2: 16, P3: 5 |

**Coverage by Story:**
- TEA-PARALLEL-001.1 (Executor Abstraction): 21 scenarios
- TEA-PARALLEL-001.2 (CLI Scoped Execution): 15 scenarios
- TEA-PARALLEL-001.3 (Remote Executor Core): 20 scenarios
- TEA-PARALLEL-001.4 (Remote Env & Security): 19 scenarios
- TEA-PARALLEL-001.5 (Integration/Docs): 7 scenarios

### Risk Areas Identified

| Risk ID | Risk Description | Severity | Mitigation Tests |
|---------|------------------|----------|------------------|
| RISK-001 | Process serialization failures | High | 4 tests |
| RISK-002 | SSH connection failures | High | 3 tests |
| RISK-003 | State size too large | Medium | Scale test with >1MB |
| RISK-004 | Orphan processes | Medium | Cleanup tests |
| RISK-005 | Version mismatch | Medium | **Gap: Recommend test** |
| RISK-006 | Backward compatibility | Critical | 4 tests |
| RISK-007 | Env var leakage | High | 3 tests |
| RISK-008 | Credential exposure | High | 1 test |
| RISK-009 | Invalid scope | High | 5 tests |
| RISK-010 | Interrupt in remote | High | 3 tests |
| RISK-011 | Context init failure | Medium | 3 tests |
| RISK-012 | Checkpoint loss | Medium | 2 tests |

### Quality Gate Recommendation

**Status: READY FOR IMPLEMENTATION**

All acceptance criteria have test coverage. Stories are properly sized with clear dependencies.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-25 | 0.1 | Initial epic draft | Sarah (PO) |
| 2025-12-29 | 0.2 | Added QA Notes with test design review | Quinn (QA) |
| 2026-01-01 | 0.3 | Added Feature Interaction Limitations section | Sarah (PO) |
| 2026-01-01 | 0.4 | Updated Story 2 with env_vars configuration | Sarah (PO) |
| 2026-01-01 | 0.5 | Major revision: Full YAML + scope execution | Sarah (PO) |
| 2026-01-01 | 0.6 | Added context initialization and constraint docs | Sarah (PO) |
| 2026-01-01 | 1.0 | **RESTRUCTURED**: Split into 5 stories for manageable scope. Story 2 → Stories 2,3,4. Original Story 3 → Story 5. | Sarah (PO) |
