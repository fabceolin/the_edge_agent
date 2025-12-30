# Epic: TEA-PARALLEL-001 - Multi-Strategy Parallel Execution

## Status: Ready for Development

**QA Review Passed:** 2025-12-29
- All acceptance criteria have test coverage (47 scenarios)
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

## Stories

---

### Story 1: Parallel Executor Abstraction + Process Backend

**ID**: TEA-PARALLEL-001.1

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

### Story 2: Remote Executor via SSH/GNU Parallel

**ID**: TEA-PARALLEL-001.2

**As a** workflow developer,
**I want** to configure `parallel_strategy: remote` to distribute work across SSH hosts,
**So that** I can scale workflows beyond a single machine.

#### Acceptance Criteria

1. `RemoteExecutor` implementa interface `ParallelExecutor`
2. Configuração via YAML: `hosts`, `basefile`, `workdir`, `cleanup`
3. Gera comando GNU Parallel quando disponível, ou executa SSH direto (fallback)
4. Serializa input state para arquivo JSON temporário
5. Copia binário TEA + YAML subset + input para remote
6. Executa remotamente, coleta resultado, deserializa de volta ao pipeline
7. Limpeza automática de arquivos remotos (configurável)
8. Timeout e retry integrados com `ParallelConfig` existente

#### Technical Notes

**Files to Create/Modify**:
- Modify: `python/src/the_edge_agent/parallel_executors.py` (add RemoteExecutor)
- Create: `python/src/the_edge_agent/yaml_subset.py` (generate subset YAML)
- Modify: `python/src/the_edge_agent/yaml_engine.py` (parse remote settings)

**Remote Execution Flow**:
```
┌─────────────────────────────────────────────────────────────────┐
│                     RemoteExecutor.execute()                    │
└─────────────────────────────────────────────────────────────────┘
                              │
    ┌─────────────────────────┼─────────────────────────────┐
    ▼                         ▼                             ▼
┌──────────┐           ┌──────────┐                  ┌──────────┐
│ Branch A │           │ Branch B │                  │ Branch C │
│          │           │          │                  │          │
│ 1. serialize state   │ 1. serialize state          │ ...      │
│ 2. generate subset   │ 2. generate subset          │          │
│ 3. scp files         │ 3. scp files                │          │
│ 4. ssh execute       │ 4. ssh execute              │          │
│ 5. scp result        │ 5. scp result               │          │
│ 6. deserialize       │ 6. deserialize              │          │
│ 7. cleanup (opt)     │ 7. cleanup (opt)            │          │
└──────────┘           └──────────┘                  └──────────┘
                              │
                              ▼
                    ┌─────────────────┐
                    │    Fan-In       │
                    │  (merge results)│
                    └─────────────────┘
```

**GNU Parallel Integration**:
```bash
# When GNU Parallel is available
parallel --sshlogin server1,server2 \
         --basefile ./tea \
         --basefile subset.yaml \
         --transferfile {} \
         --return {.}.result.json \
         --cleanup \
         './tea run subset.yaml --input {} --output {.}.result.json' \
         ::: input1.json input2.json input3.json
```

**SSH Fallback** (when parallel not available):
```python
async def _ssh_execute(self, host: str, input_file: str) -> Dict:
    # 1. scp files to remote
    await self._scp_to_remote(host, [self.binary, self.yaml, input_file])
    # 2. ssh execute
    result = await self._ssh_run(host, f"./tea run subset.yaml --input {input_file}")
    # 3. scp result back
    await self._scp_from_remote(host, f"{input_file}.result.json")
    # 4. cleanup if configured
    if self.cleanup:
        await self._ssh_run(host, f"rm -f ./tea subset.yaml {input_file}*")
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

### Story 3: Integration Testing + Documentation

**ID**: TEA-PARALLEL-001.3

**As a** workflow developer,
**I want** comprehensive documentation and examples for parallel strategies,
**So that** I can choose the right strategy for my use case.

#### Acceptance Criteria

1. Testes de integração para cada strategy (mock SSH para remote)
2. Testes de compatibilidade: verificar que thread permanece default
3. Documentação em `YAML_REFERENCE.md` com tabela de trade-offs
4. Exemplo YAML demonstrando cada strategy
5. Error handling documentado para cada failure mode

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

**Rollback Plan**:
- Feature flag `TEA_PARALLEL_STRATEGY_ENABLED=false` to disable new strategies
- Default to thread if strategy parsing fails

## Definition of Done

- [ ] Story 1: Executor abstraction + Process backend implemented
- [ ] Story 2: Remote executor with SSH/GNU Parallel support
- [ ] Story 3: Documentation and integration tests
- [ ] All existing parallelism tests pass
- [ ] New tests for process and remote strategies
- [ ] YAML_REFERENCE.md updated
- [ ] No regression in existing functionality

## Dependencies

- GNU Parallel (optional, for remote strategy optimization)
- SSH client configured on execution host
- Python 3.9+ (for ProcessPoolExecutor improvements)

## QA Notes

**Date:** 2025-12-29
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 47 |
| Unit tests | 18 (38%) |
| Integration tests | 19 (40%) |
| E2E tests | 10 (21%) |
| Priority distribution | P0: 15, P1: 20, P2: 9, P3: 3 |

**Coverage by Story:**
- TEA-PARALLEL-001.1 (Executor Abstraction): 21 scenarios
- TEA-PARALLEL-001.2 (Remote Executor): 19 scenarios
- TEA-PARALLEL-001.3 (Integration/Docs): 7 scenarios

### Risk Areas Identified

| Risk ID | Risk Description | Severity | Mitigation Tests |
|---------|------------------|----------|------------------|
| RISK-001 | Process serialization failures (pickle incompatibility) | High | 4 tests covering detection, error messages, and fallback |
| RISK-002 | SSH connection failures in remote executor | High | 3 tests for timeout, retry, and circuit breaker |
| RISK-003 | State size too large for network transfer | Medium | Scale test with >1MB state |
| RISK-004 | Orphan processes on remote hosts | Medium | Cleanup tests with configurable behavior |
| RISK-005 | Version mismatch between local and remote binary | Medium | **Gap: Recommend adding explicit test** |
| RISK-006 | Backward compatibility regression | Critical | 4 tests ensuring ThreadExecutor behavioral parity |

### Recommended Test Scenarios (Priority Order)

**Critical P0 Tests (Must Pass Before Merge):**
1. `001.1-INT-002`: ThreadExecutor produces identical results to current implementation
2. `001.1-E2E-001`: Full existing parallel test suite passes with new abstraction
3. `001.1-E2E-002`: Default strategy is `thread` when not specified
4. `001.3-INT-001`: Legacy YAML without `parallel_strategy` uses ThreadExecutor

**Key P1 Tests (New Functionality):**
1. Process executor serialization round-trip
2. Remote executor command generation (GNU Parallel + SSH fallback)
3. YAML parsing for all strategy configurations
4. Error propagation for network/process failures

### Concerns and Blockers

**Concerns:**
1. **Version Mismatch Risk (RISK-005)** - No test scenario explicitly covers version checking between local and remote TEA binaries. Recommend adding a test in Story 2 implementation.

2. **Performance Baseline Test** - Test `001.3-INT-002` requires capturing timing baseline of current implementation before refactoring. Suggest running and recording this baseline before starting implementation.

3. **Remote Testing Complexity** - E2E tests for remote executor depend heavily on mocking. Consider adding one optional integration test with `localhost` SSH for CI environments that support it.

**No Blockers Identified** - Test design is comprehensive and implementation can proceed.

### Test File Organization

```
python/tests/
├── test_parallel_executors.py       # Unit + Integration (Story 1)
├── test_remote_executor.py          # Unit + Integration (Story 2)
├── test_yaml_parallel_strategy.py   # YAML parsing tests
└── test_parallel_integration.py     # E2E tests (Story 3)
```

### Quality Gate Recommendation

**Status: READY FOR IMPLEMENTATION**

All acceptance criteria have test coverage. Test design follows risk-based prioritization with backward compatibility as the highest priority. Implementation should execute P0 tests first to catch regressions early.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-25 | 0.1 | Initial epic draft | Sarah (PO) |
| 2025-12-29 | 0.2 | Added QA Notes with test design review | Quinn (QA) |
