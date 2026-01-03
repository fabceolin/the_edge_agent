# Test Design: Epic TEA-PARALLEL-001 - Multi-Strategy Parallel Execution

**Date:** 2026-01-01 (Updated from 2025-12-29)
**Designer:** Quinn (Test Architect)
**Update Reason:**
- v1.1: Added test scenarios for AC9 (env_vars) and AC10 (secrets backend)
- v1.2: Added test scenarios for AC11 (CLI scope flags), AC12 (state I/O), AC13 (full engine on remote), context initialization, checkpoints, and interrupt validation

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 82 (+23 from v1.1) |
| Unit tests | 32 (39%) |
| Integration tests | 34 (41%) |
| E2E tests | 16 (20%) |
| **Priority distribution** | P0: 24, P1: 37, P2: 16, P3: 5 |

### Risk-Based Strategy Rationale

This epic introduces significant architectural changes to the parallel execution system. Test priorities are weighted toward:
1. **Backward Compatibility (P0)** - Existing thread executor behavior must be preserved
2. **Serialization Safety (P0)** - Process/Remote strategies depend on correct state serialization
3. **Error Handling (P0)** - Network/process failures must be handled gracefully
4. **Security (P0)** - Environment variable filtering and secrets handling must be secure
5. **Scope Validation (P0)** - CLI scope flags must be validated correctly
6. **Interrupt Safety (P0)** - Interrupt points must not be in remote branches
7. **New Functionality (P1)** - Process and Remote executors must work correctly
8. **Configuration Parsing (P1)** - YAML settings must be parsed correctly

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

### AC5: Copia binário TEA + YAML completo + input para remote (Revised)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-INT-006 | Integration | P1 | SCP file transfer to remote host (mocked) | File transfer |
| 001.2-INT-007 | Integration | P1 | Full YAML transferred (not subset) | Full engine support |
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

### AC9: Environment variable transfer via `env_vars` configuration (NEW)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-011 | Unit | P0 | `EnvVarsConfig` dataclass validates required fields | Config validation |
| 001.2-UNIT-012 | Unit | P0 | `get_filtered_env_vars()` returns only whitelisted vars | **Security critical** - whitelist enforcement |
| 001.2-UNIT-013 | Unit | P0 | `get_filtered_env_vars()` excludes vars matching `exclude_patterns` globs | **Security critical** - pattern exclusion |
| 001.2-UNIT-014 | Unit | P1 | `build_ssh_options()` generates correct `-o SendEnv=VAR` flags | SSH option generation |
| 001.2-UNIT-015 | Unit | P1 | `generate_export_script()` creates valid shell script with exports | Script generation |
| 001.2-INT-017 | Integration | P0 | `ssh_env` mode: SSH command includes SendEnv options for whitelisted vars | Mode integration |
| 001.2-INT-018 | Integration | P1 | `export_file` mode: env.sh transferred and sourced before execution | Mode integration |
| 001.2-INT-019 | Integration | P1 | `none` mode: No env var manipulation occurs | Mode integration |
| 001.2-INT-020 | Integration | P1 | Missing env var from `include` list handled gracefully (skip, don't fail) | Error handling |
| 001.2-INT-021 | Integration | P1 | YAMLEngine parses `settings.parallel.remote.env_vars` with all fields | YAML parsing |

### AC10: Secrets backend awareness (NEW)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-016 | Unit | P1 | Detect when `secrets.backend != env` | Backend detection |
| 001.2-INT-022 | Integration | P1 | Info log emitted when `secrets.backend != env` and `parallel_strategy == remote` | User guidance |
| 001.2-INT-023 | Integration | P2 | Cloud SDK credential env vars (`AWS_ACCESS_KEY_ID`, etc.) NOT auto-transferred | **Security** - intentional friction |

### AC11: CLI supports scoped execution via --entry-point and --exit-point (NEW)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-017 | Unit | P0 | `--entry-point` flag parsed correctly by CLI | CLI parsing |
| 001.2-UNIT-018 | Unit | P0 | `--exit-point` flag parsed correctly by CLI | CLI parsing |
| 001.2-UNIT-019 | Unit | P0 | Validation: entry-point node must exist in graph | **Fail fast** on invalid scope |
| 001.2-UNIT-020 | Unit | P0 | Validation: exit-point node must exist in graph | **Fail fast** on invalid scope |
| 001.2-UNIT-021 | Unit | P0 | Validation: path must exist from entry to exit | **Critical** - prevent unreachable scope |
| 001.2-INT-024 | Integration | P0 | Scoped execution starts at entry-point (not __start__) | Core scope functionality |
| 001.2-INT-025 | Integration | P0 | Scoped execution stops BEFORE exit-point (not executed) | **Critical** - fan-in on main host |
| 001.2-INT-026 | Integration | P1 | Linear subgraph: executes chain of nodes from entry to exit | Chain execution |
| 001.2-E2E-004 | E2E | P1 | Full scoped execution with --entry-point and --exit-point | End-to-end scope |

### AC12: CLI supports --input and --output for state serialization (NEW)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-022 | Unit | P1 | `--input` flag parsed correctly by CLI | CLI parsing |
| 001.2-UNIT-023 | Unit | P1 | `--output` flag parsed correctly by CLI | CLI parsing |
| 001.2-INT-027 | Integration | P0 | State loaded from `--input` JSON file | Core state loading |
| 001.2-INT-028 | Integration | P0 | Final state written to `--output` JSON file | Core state writing |
| 001.2-INT-029 | Integration | P1 | Invalid JSON in `--input` raises clear error | Error handling |
| 001.2-INT-030 | Integration | P2 | Missing `--input` with `--entry-point` uses empty state (with warning) | Edge case handling |

### AC13: Full engine features supported on remote execution (NEW)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-E2E-005 | E2E | P1 | Nested parallel (parallel within remote branch) works | Full engine |
| 001.2-E2E-006 | E2E | P1 | Dynamic parallel within remote branch works | Full engine |
| 001.2-E2E-007 | E2E | P1 | Conditional edges within remote branch work | Full engine |
| 001.2-INT-031 | Integration | P1 | LTM read/write works on remote with distributed backend | LTM integration |
| 001.2-INT-032 | Integration | P2 | LTM with sqlite backend emits warning for remote strategy | User guidance |
| 001.2-INT-033 | Integration | P1 | Rate limiters initialized fresh on remote (per-host) | Documented behavior |

### Context Initialization and Constraints (NEW)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-024 | Unit | P0 | `_validate_remote_scope()` detects interrupt_before in scope | **Critical** - prevent HITL in remote |
| 001.2-UNIT-025 | Unit | P0 | `_validate_remote_scope()` detects interrupt_after in scope | **Critical** - prevent HITL in remote |
| 001.2-INT-034 | Integration | P0 | Interrupt point in remote scope raises ValidationError | Fail fast |
| 001.2-INT-035 | Integration | P1 | Checkpoint with in-memory backend emits warning for remote | User guidance |
| 001.2-INT-036 | Integration | P2 | Checkpoint with distributed backend works on remote | Checkpoint support |
| 001.2-E2E-008 | E2E | P1 | Remote context initialized correctly (graph, state, settings) | Context initialization |

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
| **RISK-007** | **Env var leakage to remote hosts** | **001.2-UNIT-012, 001.2-UNIT-013, 001.2-INT-017** |
| **RISK-008** | **Cloud credential exposure via env transfer** | **001.2-INT-023** |
| **RISK-009** | **Invalid scope (entry/exit point)** | **001.2-UNIT-019, 001.2-UNIT-020, 001.2-UNIT-021, 001.2-INT-024, 001.2-INT-025** |
| **RISK-010** | **Interrupt point in remote branch** | **001.2-UNIT-024, 001.2-UNIT-025, 001.2-INT-034** |
| **RISK-011** | **Remote context initialization failure** | **001.2-INT-027, 001.2-INT-028, 001.2-E2E-008** |
| **RISK-012** | **Checkpoint loss on remote** | **001.2-INT-035, 001.2-INT-036** |

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
│   ├── TestTimeoutRetry             # AC8 tests
│   ├── TestEnvVarsTransfer          # AC9 tests
│   ├── TestSecretsBackendAwareness  # AC10 tests
│   ├── TestScopedExecution          # AC11 tests (NEW)
│   │   ├── test_entry_point_validation
│   │   ├── test_exit_point_validation
│   │   ├── test_path_validation
│   │   ├── test_scoped_execution_starts_at_entry
│   │   ├── test_scoped_execution_stops_before_exit
│   │   └── test_linear_subgraph_execution
│   ├── TestStateIO                  # AC12 tests (NEW)
│   │   ├── test_input_flag_parsing
│   │   ├── test_output_flag_parsing
│   │   ├── test_state_loaded_from_input
│   │   ├── test_state_written_to_output
│   │   └── test_invalid_json_error
│   ├── TestFullEngineOnRemote       # AC13 tests (NEW)
│   │   ├── test_nested_parallel_on_remote
│   │   ├── test_dynamic_parallel_on_remote
│   │   ├── test_conditional_edges_on_remote
│   │   ├── test_ltm_on_remote
│   │   └── test_rate_limiters_per_host
│   └── TestRemoteConstraints        # Context/Constraints tests (NEW)
│       ├── test_interrupt_before_validation
│       ├── test_interrupt_after_validation
│       ├── test_interrupt_in_scope_raises_error
│       ├── test_checkpoint_warning
│       └── test_context_initialization
│
├── test_cli_scope_flags.py          # CLI tests (NEW)
│   ├── TestEntryPointFlag
│   ├── TestExitPointFlag
│   ├── TestInputFlag
│   └── TestOutputFlag
│
├── test_yaml_parallel_strategy.py   # AC6 of Story 1, AC2 of Story 2
│   ├── TestSettingsParallelParsing
│   ├── TestEdgeParallelStrategyParsing
│   └── TestEnvVarsConfigParsing
│
└── test_parallel_integration.py     # E2E tests for Story 3
    ├── TestStrategyMatrix
    ├── TestBackwardCompatibility
    ├── TestExampleWorkflows
    └── TestRemoteFullEngine         # NEW
```

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on interface/serialization/security issues)
   - 001.1-UNIT-001 through 001.1-UNIT-007
   - 001.2-UNIT-001, 001.2-UNIT-009
   - **001.2-UNIT-011 through 001.2-UNIT-013** (NEW - env var security)

2. **P0 Integration tests** (verify backward compatibility and security early)
   - 001.1-INT-002 through 001.1-INT-005
   - 001.1-INT-010
   - 001.2-INT-010, 001.2-INT-014, 001.2-INT-015
   - **001.2-INT-017** (NEW - ssh_env mode security)

3. **P0 E2E tests** (full regression verification)
   - 001.1-E2E-001, 001.1-E2E-002
   - 001.3-E2E-001, 001.3-E2E-002
   - 001.3-INT-001

4. **P1 tests** (new functionality)
   - All P1 unit, integration, and E2E tests
   - **001.2-UNIT-014, 001.2-UNIT-015, 001.2-UNIT-016** (NEW)
   - **001.2-INT-018 through 001.2-INT-022** (NEW)

5. **P2+ tests** (time permitting)
   - Secondary features and edge cases
   - **001.2-INT-023** (NEW - cloud creds security)

---

## Gate YAML Block

```yaml
test_design:
  epic: TEA-PARALLEL-001
  date: 2026-01-01
  version: "1.2"
  previous_date: 2025-12-29
  scenarios_total: 82
  scenarios_added_v1_1: 12
  scenarios_added_v1_2: 23
  by_level:
    unit: 32
    integration: 34
    e2e: 16
  by_priority:
    p0: 24
    p1: 37
    p2: 16
    p3: 5
  by_story:
    TEA-PARALLEL-001.1: 21
    TEA-PARALLEL-001.2: 54  # +12 for AC9-10, +23 for AC11-13 and constraints
    TEA-PARALLEL-001.3: 7
  coverage_gaps: []
  critical_tests:
    - 001.1-INT-002  # ThreadExecutor behavioral parity
    - 001.1-E2E-001  # Full regression suite
    - 001.3-INT-001  # Backward compatibility
    - 001.2-UNIT-012  # Env var whitelist
    - 001.2-UNIT-013  # Exclude patterns
    - 001.2-UNIT-019  # Entry point validation (NEW)
    - 001.2-UNIT-021  # Path validation (NEW)
    - 001.2-INT-025   # Stop BEFORE exit (NEW)
    - 001.2-UNIT-024  # Interrupt validation (NEW)
    - 001.2-INT-034   # Interrupt in scope error (NEW)
  risk_coverage:
    serialization: 4 tests
    network_failures: 3 tests
    backward_compat: 4 tests
    env_var_security: 3 tests
    credential_exposure: 1 test
    scope_validation: 5 tests  # NEW
    interrupt_safety: 3 tests  # NEW
    context_init: 3 tests      # NEW
    checkpoint_remote: 2 tests # NEW
  new_scenarios_summary:
    ac9_env_vars: 10 scenarios (6 unit, 4 integration)
    ac10_secrets: 3 scenarios (1 unit, 2 integration)
    ac11_scope_flags: 9 scenarios (5 unit, 3 integration, 1 e2e)  # NEW
    ac12_state_io: 6 scenarios (2 unit, 4 integration)            # NEW
    ac13_full_engine: 6 scenarios (3 integration, 3 e2e)          # NEW
    constraints: 6 scenarios (2 unit, 3 integration, 1 e2e)       # NEW
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for components, E2E for flows)
- [x] No duplicate coverage across levels (each test has unique justification)
- [x] Priorities align with business risk (backward compat = P0, security = P0)
- [x] Test IDs follow naming convention `{epic}.{story}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Risk mitigations mapped to tests
- [x] Security-critical env var tests marked P0
- [x] Secrets backend interaction tests added
- [x] **NEW: CLI scope flag tests added (AC11-12)**
- [x] **NEW: Full engine on remote tests added (AC13)**
- [x] **NEW: Interrupt validation tests added (P0)**
- [x] **NEW: Checkpoint behavior tests added**
- [x] **NEW: Context initialization tests added**

---

## Notes for Implementation

### Existing Notes (from 2025-12-29)

1. **Mock Strategy for Remote Tests**: Use `unittest.mock` to mock `subprocess.run` for SSH/SCP commands and `shutil.which` for GNU Parallel detection.

2. **Localhost Integration Test**: For one E2E test (001.2-E2E-001), consider using `localhost` via SSH if SSH is configured, otherwise skip with `@pytest.mark.skipif`.

3. **Performance Baseline**: Test 001.3-INT-002 should capture timing of 100 parallel tasks with old impl vs new ThreadExecutor and assert <5% overhead.

4. **Serialization Test Data**: Create fixtures with:
   - Simple dict: `{"count": 1, "name": "test"}`
   - Nested: `{"data": {"items": [1, 2, 3], "meta": {"id": "abc"}}}`
   - Non-picklable: `{"func": lambda x: x}` (for error tests)
   - Large state: `{"data": "x" * 1_000_000}` (for scale tests)

5. **Circuit Breaker Per Host**: Ensure 001.2-INT-016 tests that a failing host doesn't trip the breaker for other hosts.

### New Notes (2026-01-01)

6. **Environment Variable Test Setup**: Use `monkeypatch.setenv()` to set test environment variables:
   ```python
   @pytest.fixture
   def env_setup(monkeypatch):
       monkeypatch.setenv("OPENAI_API_KEY", "test-key-123")
       monkeypatch.setenv("ANTHROPIC_API_KEY", "test-key-456")
       monkeypatch.setenv("DB_PASSWORD_SECRET", "should-be-excluded")
       monkeypatch.setenv("LOG_LEVEL", "DEBUG")
   ```

7. **Exclude Pattern Test Cases**: Test multiple glob patterns:
   ```python
   test_cases = [
       ("*_SECRET", ["DB_PASSWORD_SECRET"]),
       ("*_PASSWORD", ["DB_PASSWORD"]),
       ("AWS_*", ["AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY"]),
   ]
   ```

8. **SSH SendEnv Mock**: Mock subprocess to capture SSH command and verify `-o SendEnv=` flags:
   ```python
   def test_ssh_env_mode(mock_subprocess):
       # ...execute...
       call_args = mock_subprocess.call_args[0][0]
       assert "-o SendEnv=OPENAI_API_KEY" in call_args
       assert "-o SendEnv=DB_PASSWORD_SECRET" not in call_args
   ```

9. **Export Script Validation**: Verify generated env.sh:
   ```python
   def test_export_script_content(tmp_path):
       handler = RemoteEnvHandler(config)
       path = handler.generate_export_script(str(tmp_path / "env.sh"))
       content = Path(path).read_text()
       assert "export OPENAI_API_KEY='test-key-123'" in content
       assert "DB_PASSWORD_SECRET" not in content
   ```

10. **Secrets Backend Warning Log**: Use `caplog` fixture to verify info log:
    ```python
    def test_secrets_backend_info_log(caplog):
        with caplog.at_level(logging.INFO):
            executor = RemoteExecutor(secrets_backend="aws", ...)
        assert "Remote hosts should have cloud provider credentials" in caplog.text
    ```

11. **Cloud Credentials NOT Transferred**: Verify intentional friction:
    ```python
    def test_cloud_creds_not_auto_transferred():
        config = EnvVarsConfig(
            include=["OPENAI_API_KEY"],  # User's explicit list
            exclude_patterns=[],
            mode="ssh_env"
        )
        handler = RemoteEnvHandler(config)
        # Even if AWS_ACCESS_KEY_ID is in env, it should NOT be in output
        # unless explicitly in include list
        filtered = handler.get_filtered_env_vars()
        assert "AWS_ACCESS_KEY_ID" not in filtered
    ```

### New Notes (2026-01-01 v1.2)

12. **Scoped Execution Test Setup**: Create test fixtures for scope validation:
    ```python
    @pytest.fixture
    def sample_graph():
        """Graph with linear chain: start -> a -> b -> c -> end"""
        graph = StateGraph({"value": int})
        graph.add_node("a", run=lambda s: s)
        graph.add_node("b", run=lambda s: s)
        graph.add_node("c", run=lambda s: s)
        graph.set_entry_point("a")
        graph.add_edge("a", "b")
        graph.add_edge("b", "c")
        graph.set_finish_point("c")
        return graph.compile()
    ```

13. **Entry/Exit Point Validation Tests**:
    ```python
    def test_entry_point_not_found(sample_graph):
        with pytest.raises(ValueError, match="Entry point 'nonexistent' not found"):
            sample_graph.execute_scoped({}, entry_point="nonexistent", exit_point="c")

    def test_no_path_between_entry_exit(sample_graph):
        # Create graph where c cannot reach a
        with pytest.raises(ValueError, match="No execution path"):
            sample_graph.execute_scoped({}, entry_point="c", exit_point="a")
    ```

14. **Stop BEFORE Exit Point Test**:
    ```python
    def test_stops_before_exit_point(sample_graph, mocker):
        spy_c = mocker.spy(sample_graph.nodes["c"], "run")
        result = sample_graph.execute_scoped(
            {"value": 1},
            entry_point="a",
            exit_point="c"  # Should NOT execute c
        )
        spy_c.assert_not_called()
    ```

15. **Interrupt Point Validation Test**:
    ```python
    def test_interrupt_in_remote_scope_raises_error():
        graph = StateGraph({"value": int})
        graph.add_node("a", run=lambda s: s)
        graph.add_node("b", run=lambda s: s, interrupt_before=True)  # Interrupt!
        graph.add_node("c", run=lambda s: s)
        graph.add_edge("a", "b")
        graph.add_edge("b", "c")
        compiled = graph.compile()

        errors = compiled._validate_remote_scope("a", "c")
        assert len(errors) == 1
        assert "interrupt_before" in errors[0]
    ```

16. **State I/O Test with Temp Files**:
    ```python
    def test_state_loaded_from_input(tmp_path):
        input_file = tmp_path / "input.json"
        input_file.write_text('{"value": 42, "name": "test"}')

        result = run_cli([
            "run", "workflow.yaml",
            "--entry-point", "start_node",
            "--exit-point", "end_node",
            "--input", str(input_file)
        ])
        # Verify state was loaded
        assert result.initial_state["value"] == 42
    ```

17. **Checkpoint Warning Test**:
    ```python
    def test_checkpoint_warning_for_remote(caplog):
        config = ParallelConfig(strategy="remote", checkpoint_backend="memory")
        with caplog.at_level(logging.WARNING):
            validate_remote_config(config)
        assert "in-memory" in caplog.text
        assert "not shared" in caplog.text
    ```

18. **Full Engine on Remote E2E Test**:
    ```python
    @pytest.mark.e2e
    def test_nested_parallel_on_remote(mock_ssh):
        """Verify nested parallel (within remote branch) uses local strategy."""
        yaml_content = """
        nodes:
          - name: outer_branch
            run: |
              # This branch will be executed remotely
              return state
          - name: inner_a
            run: return {"inner": "a"}
          - name: inner_b
            run: return {"inner": "b"}
          - name: merge_inner
            run: return {"merged": parallel_results}

        edges:
          - from: outer_branch
            to: [inner_a, inner_b]
            parallel: true  # Nested parallel - should use thread on remote
            fan_in: merge_inner
        """
        # Execute scoped (simulating remote execution)
        result = execute_yaml_scoped(
            yaml_content,
            entry_point="outer_branch",
            exit_point="__end__"
        )
        assert "merged" in result
    ```

---

## Change Log

| Date | Version | Changes |
|------|---------|---------|
| 2025-12-29 | 1.0 | Initial test design with 47 scenarios |
| 2026-01-01 | 1.1 | Added 12 scenarios for AC9 (env_vars) and AC10 (secrets backend); Added RISK-007, RISK-008; Updated metrics |
| 2026-01-01 | 1.2 | **Major update**: Added 23 scenarios for AC11 (scope flags), AC12 (state I/O), AC13 (full engine on remote), context initialization, checkpoint behavior, and interrupt validation. Added RISK-009 through RISK-012. Total scenarios: 82. Updated AC5 from "subset" to "full YAML". |

---

*Generated by Quinn (Test Architect) - BMAD QA Framework*
