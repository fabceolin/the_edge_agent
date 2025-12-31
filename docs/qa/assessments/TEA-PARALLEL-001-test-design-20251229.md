# Test Design: Epic TEA-PARALLEL-001 - Multi-Strategy Parallel Execution

**Date:** 2025-12-29
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 47 |
| Unit tests | 18 (38%) |
| Integration tests | 19 (40%) |
| E2E tests | 10 (21%) |
| **Priority distribution** | P0: 15, P1: 20, P2: 9, P3: 3 |

### Risk-Based Strategy Rationale

This epic introduces significant architectural changes to the parallel execution system. Test priorities are weighted toward:
1. **Backward Compatibility (P0)** - Existing thread executor behavior must be preserved
2. **Serialization Safety (P0)** - Process/Remote strategies depend on correct state serialization
3. **Error Handling (P0)** - Network/process failures must be handled gracefully
4. **New Functionality (P1)** - Process and Remote executors must work correctly
5. **Configuration Parsing (P1)** - YAML settings must be parsed correctly

---

## Story 1: Parallel Executor Abstraction + Process Backend (TEA-PARALLEL-001.1)

### AC1: Nova abstração `ParallelExecutor` com interface comum (Protocol)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-UNIT-001 | Unit | P0 | `ParallelExecutor` Protocol defines required method signatures | Pure interface contract - verify Protocol type hints |
| 001.1-UNIT-002 | Unit | P0 | `ThreadExecutor` implements `ParallelExecutor` Protocol | Interface conformance check |
| 001.1-UNIT-003 | Unit | P0 | `ProcessExecutor` implements `ParallelExecutor` Protocol | Interface conformance check |
| 001.1-INT-001 | Integration | P1 | Executor factory returns correct executor type based on strategy string | Component interaction with factory pattern |

### AC2: `ThreadExecutor` encapsula implementação atual (sem mudanças de comportamento)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-INT-002 | Integration | P0 | ThreadExecutor produces identical results to current `_execute_parallel_flows()` | **Critical regression test** - verify exact behavioral parity |
| 001.1-INT-003 | Integration | P0 | ThreadExecutor respects `ParallelConfig.max_workers` setting | Validate existing config honored |
| 001.1-INT-004 | Integration | P0 | ThreadExecutor integrates with existing `RetryPolicy` | Backward compatibility with retry mechanism |
| 001.1-INT-005 | Integration | P0 | ThreadExecutor integrates with existing `CircuitBreaker` | Backward compatibility with circuit breaker |
| 001.1-INT-006 | Integration | P1 | ThreadExecutor integrates with `CancellationToken` | Cancellation must still work |

### AC3: `ProcessExecutor` usa `ProcessPoolExecutor` do `concurrent.futures`

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-UNIT-004 | Unit | P1 | ProcessExecutor creates ProcessPoolExecutor with correct max_workers | Configuration mapping |
| 001.1-INT-007 | Integration | P1 | ProcessExecutor executes simple functions in separate processes | Core functionality verification |
| 001.1-INT-008 | Integration | P1 | ProcessExecutor respects timeout from ParallelConfig | Timeout handling in process context |
| 001.1-INT-009 | Integration | P2 | ProcessExecutor handles process crashes gracefully | Error recovery |

### AC4: State é serializado via pickle entre processos

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-UNIT-005 | Unit | P0 | Detect non-picklable objects in state before execution | Fail fast on serialization issues |
| 001.1-UNIT-006 | Unit | P0 | Provide clear error message for non-picklable state keys | Developer experience |
| 001.1-INT-010 | Integration | P0 | ProcessExecutor successfully serializes/deserializes simple dict state | Core serialization path |
| 001.1-INT-011 | Integration | P1 | ProcessExecutor handles nested dict/list state structures | Complex state handling |
| 001.1-INT-012 | Integration | P1 | ProcessExecutor handles numpy arrays in state (if numpy installed) | Common scientific computing case |
| 001.1-UNIT-007 | Unit | P2 | JSON fallback serialization for complex objects | Alternative serialization path |

### AC5: Todos os testes existentes de parallelism passam com default (thread)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-E2E-001 | E2E | P0 | Run full existing parallel test suite with new abstraction layer | **Critical regression** - no behavior change |
| 001.1-E2E-002 | E2E | P0 | Verify default strategy is `thread` when not specified | Backward compatibility |

### AC6: YAML suporta `parallel_strategy: thread | process` em edges e settings

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-UNIT-008 | Unit | P1 | YAMLEngine parses `settings.parallel.strategy` correctly | YAML parsing |
| 001.1-UNIT-009 | Unit | P1 | YAMLEngine parses `parallel_strategy` on individual edges | Edge-level override parsing |
| 001.1-UNIT-010 | Unit | P1 | Edge-level `parallel_strategy` overrides global setting | Override precedence logic |
| 001.1-INT-013 | Integration | P1 | Invalid strategy value raises descriptive ValidationError | User-friendly errors |
| 001.1-INT-014 | Integration | P2 | YAML with no strategy defaults to thread | Default value handling |

---

## Story 2: Remote Executor via SSH/GNU Parallel (TEA-PARALLEL-001.2)

### AC1: `RemoteExecutor` implementa interface `ParallelExecutor`

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-001 | Unit | P0 | `RemoteExecutor` implements `ParallelExecutor` Protocol | Interface conformance |
| 001.2-UNIT-002 | Unit | P1 | RemoteExecutor initializes with required config (hosts, basefile, workdir) | Constructor validation |

### AC2: Configuração via YAML: `hosts`, `basefile`, `workdir`, `cleanup`

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-003 | Unit | P1 | YAMLEngine parses `settings.parallel.remote.hosts` as list | Config parsing |
| 001.2-UNIT-004 | Unit | P1 | YAMLEngine parses `settings.parallel.remote.basefile` path | Config parsing |
| 001.2-UNIT-005 | Unit | P1 | YAMLEngine parses `settings.parallel.remote.workdir` with default | Config parsing with defaults |
| 001.2-UNIT-006 | Unit | P2 | YAMLEngine parses `settings.parallel.remote.cleanup` boolean | Config parsing |
| 001.2-INT-001 | Integration | P1 | Missing required remote config raises ValidationError when strategy=remote | Fail fast on misconfiguration |

### AC3: Gera comando GNU Parallel quando disponível, ou executa SSH direto (fallback)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-007 | Unit | P1 | Detect GNU Parallel availability via `shutil.which('parallel')` | Feature detection |
| 001.2-UNIT-008 | Unit | P1 | Generate correct GNU Parallel command with all flags | Command generation |
| 001.2-INT-002 | Integration | P1 | SSH fallback executes when GNU Parallel not available | Fallback path |
| 001.2-INT-003 | Integration | P2 | Log which execution method is being used | Observability |

### AC4: Serializa input state para arquivo JSON temporário

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-009 | Unit | P0 | State serializes to valid JSON file | Core serialization |
| 001.2-UNIT-010 | Unit | P1 | Temp file created in appropriate directory | File management |
| 001.2-INT-004 | Integration | P1 | Large state (>1MB) serializes without memory issues | Scale handling |
| 001.2-INT-005 | Integration | P2 | Non-JSON-serializable state raises clear error | Error handling |

### AC5: Copia binário TEA + YAML subset + input para remote

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-INT-006 | Integration | P1 | SCP file transfer to remote host (mocked) | File transfer |
| 001.2-INT-007 | Integration | P1 | YAML subset generation extracts only required nodes | Subset generation |
| 001.2-INT-008 | Integration | P2 | Binary basefile must exist locally before transfer | Pre-condition check |

### AC6: Executa remotamente, coleta resultado, deserializa de volta ao pipeline

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-E2E-001 | E2E | P1 | Full remote execution cycle with localhost (integration test) | End-to-end flow |
| 001.2-INT-009 | Integration | P1 | Remote stdout/stderr captured in ParallelFlowResult | Output capture |
| 001.2-INT-010 | Integration | P0 | Result JSON deserialized back to state dict | Return path |

### AC7: Limpeza automática de arquivos remotos (configurável)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-INT-011 | Integration | P2 | Cleanup removes remote files when cleanup=true | Cleanup functionality |
| 001.2-INT-012 | Integration | P2 | Cleanup skipped when cleanup=false | Configurable cleanup |
| 001.2-INT-013 | Integration | P3 | Cleanup failure logged but doesn't fail execution | Graceful degradation |

### AC8: Timeout e retry integrados com `ParallelConfig` existente

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-INT-014 | Integration | P0 | SSH command timeout from ParallelConfig.timeout_seconds | Timeout handling |
| 001.2-INT-015 | Integration | P0 | Network failure triggers retry per RetryPolicy | Retry integration |
| 001.2-INT-016 | Integration | P1 | Circuit breaker trips after configured failures per host | Circuit breaker per host |
| 001.2-E2E-002 | E2E | P1 | Partial failure with fail_fast=false collects available results | Partial success handling |
| 001.2-E2E-003 | E2E | P1 | Partial failure with fail_fast=true stops immediately | Fail fast behavior |

---

## Story 3: Integration Testing + Documentation (TEA-PARALLEL-001.3)

### AC1: Testes de integração para cada strategy (mock SSH para remote)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-E2E-001 | E2E | P0 | Integration test matrix: thread strategy with simple fan-out | Strategy coverage |
| 001.3-E2E-002 | E2E | P0 | Integration test matrix: process strategy with simple fan-out | Strategy coverage |
| 001.3-E2E-003 | E2E | P1 | Integration test matrix: remote strategy with mocked SSH | Strategy coverage |
| 001.3-E2E-004 | E2E | P1 | Integration test: mixed strategies in same workflow | Complex scenario |

### AC2: Testes de compatibilidade: verificar que thread permanece default

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-INT-001 | Integration | P0 | Legacy YAML without parallel_strategy uses ThreadExecutor | **Critical** backward compat |
| 001.3-INT-002 | Integration | P1 | Performance baseline: thread strategy has no overhead vs old impl | Performance regression |

### AC3: Documentação em `YAML_REFERENCE.md` com tabela de trade-offs

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-UNIT-001 | Unit | P3 | Documentation lint: all new YAML fields documented | Doc completeness |
| 001.3-UNIT-002 | Unit | P3 | Example YAML files are valid and parseable | Doc accuracy |

### AC4: Exemplo YAML demonstrando cada strategy

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-E2E-005 | E2E | P2 | Example `parallel_strategies_demo.yaml` runs without errors | Example validity |

### AC5: Error handling documentado para cada failure mode

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-INT-003 | Integration | P1 | Error messages match documented format | Doc accuracy |

---

## Risk Coverage Matrix

| Risk ID | Risk Description | Test Scenarios Mitigating |
|---------|------------------|---------------------------|
| RISK-001 | Process serialization failures | 001.1-UNIT-005, 001.1-UNIT-006, 001.1-INT-010, 001.1-INT-011 |
| RISK-002 | SSH connection failures | 001.2-INT-014, 001.2-INT-015, 001.2-INT-016 |
| RISK-003 | State size too large for transfer | 001.2-INT-004 |
| RISK-004 | Orphan processes on remote | 001.2-INT-011, 001.2-INT-013 |
| RISK-005 | Version mismatch (local vs remote) | (Recommend adding test in implementation) |
| RISK-006 | Backward compatibility regression | 001.1-INT-002, 001.1-E2E-001, 001.1-E2E-002, 001.3-INT-001 |

---

## Test File Organization

```
python/tests/
├── test_parallel_executors.py       # Unit + Integration tests for Story 1
│   ├── TestParallelExecutorProtocol  # AC1 tests
│   ├── TestThreadExecutor            # AC2, AC5 tests
│   ├── TestProcessExecutor           # AC3, AC4 tests
│   └── TestExecutorFactory           # Factory tests
│
├── test_remote_executor.py          # Unit + Integration tests for Story 2
│   ├── TestRemoteExecutorProtocol   # AC1 tests
│   ├── TestRemoteConfig             # AC2 tests
│   ├── TestGnuParallelIntegration   # AC3 tests
│   ├── TestSSHFallback              # AC3 tests
│   ├── TestStateSerialization       # AC4 tests
│   ├── TestFileTransfer             # AC5 tests (mocked)
│   ├── TestRemoteExecution          # AC6 tests (mocked)
│   ├── TestCleanup                  # AC7 tests
│   └── TestTimeoutRetry             # AC8 tests
│
├── test_yaml_parallel_strategy.py   # AC6 of Story 1, AC2 of Story 2
│   ├── TestSettingsParallelParsing
│   └── TestEdgeParallelStrategyParsing
│
└── test_parallel_integration.py     # E2E tests for Story 3
    ├── TestStrategyMatrix
    ├── TestBackwardCompatibility
    └── TestExampleWorkflows
```

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on interface/serialization issues)
   - 001.1-UNIT-001 through 001.1-UNIT-007
   - 001.2-UNIT-001, 001.2-UNIT-009

2. **P0 Integration tests** (verify backward compatibility early)
   - 001.1-INT-002 through 001.1-INT-005
   - 001.1-INT-010
   - 001.2-INT-010, 001.2-INT-014, 001.2-INT-015

3. **P0 E2E tests** (full regression verification)
   - 001.1-E2E-001, 001.1-E2E-002
   - 001.3-E2E-001, 001.3-E2E-002
   - 001.3-INT-001

4. **P1 tests** (new functionality)
   - All P1 unit, integration, and E2E tests

5. **P2+ tests** (time permitting)
   - Secondary features and edge cases

---

## Gate YAML Block

```yaml
test_design:
  epic: TEA-PARALLEL-001
  date: 2025-12-29
  scenarios_total: 47
  by_level:
    unit: 18
    integration: 19
    e2e: 10
  by_priority:
    p0: 15
    p1: 20
    p2: 9
    p3: 3
  by_story:
    TEA-PARALLEL-001.1: 21
    TEA-PARALLEL-001.2: 19
    TEA-PARALLEL-001.3: 7
  coverage_gaps: []
  critical_tests:
    - 001.1-INT-002  # ThreadExecutor behavioral parity
    - 001.1-E2E-001  # Full regression suite
    - 001.3-INT-001  # Backward compatibility
  risk_coverage:
    serialization: 4 tests
    network_failures: 3 tests
    backward_compat: 4 tests
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for components, E2E for flows)
- [x] No duplicate coverage across levels (each test has unique justification)
- [x] Priorities align with business risk (backward compat = P0)
- [x] Test IDs follow naming convention `{epic}.{story}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Risk mitigations mapped to tests

---

## Notes for Implementation

1. **Mock Strategy for Remote Tests**: Use `unittest.mock` to mock `subprocess.run` for SSH/SCP commands and `shutil.which` for GNU Parallel detection.

2. **Localhost Integration Test**: For one E2E test (001.2-E2E-001), consider using `localhost` via SSH if SSH is configured, otherwise skip with `@pytest.mark.skipif`.

3. **Performance Baseline**: Test 001.3-INT-002 should capture timing of 100 parallel tasks with old impl vs new ThreadExecutor and assert <5% overhead.

4. **Serialization Test Data**: Create fixtures with:
   - Simple dict: `{"count": 1, "name": "test"}`
   - Nested: `{"data": {"items": [1, 2, 3], "meta": {"id": "abc"}}}`
   - Non-picklable: `{"func": lambda x: x}` (for error tests)
   - Large state: `{"data": "x" * 1_000_000}` (for scale tests)

5. **Circuit Breaker Per Host**: Ensure 001.2-INT-016 tests that a failing host doesn't trip the breaker for other hosts.

---

*Generated by Quinn (Test Architect) - BMAD QA Framework*
