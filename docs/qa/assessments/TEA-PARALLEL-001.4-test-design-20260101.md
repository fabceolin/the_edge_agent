# Test Design: Story TEA-PARALLEL-001.4 - Remote Environment & Security

**Date:** 2026-01-01
**Designer:** Quinn (Test Architect)
**Story:** TEA-PARALLEL-001.4 - Remote Environment & Security
**Epic:** [TEA-PARALLEL-001 - Multi-Strategy Parallel Execution](../../stories/TEA-PARALLEL-001-multi-strategy-execution-epic.md)

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 21 |
| **Unit tests** | 10 (48%) |
| **Integration tests** | 8 (38%) |
| **E2E tests** | 3 (14%) |
| **P0 (Critical)** | 6 |
| **P1 (High)** | 13 |
| **P2 (Medium)** | 2 |

### Risk-Driven Focus Areas

This story involves **security-critical functionality** (environment variable and secrets handling). The test strategy emphasizes:

1. **Security-first testing**: Environment variable whitelist-only enforcement, credential leak prevention
2. **Fail-safe validation**: Interrupt point detection at compile-time to prevent runtime hangs
3. **Shift-left approach**: Heavy unit test coverage for pure logic (filtering, pattern matching, script generation)
4. **Integration verification**: End-to-end env transfer modes and backend warning emissions

---

## Test Scenarios by Acceptance Criteria

### AC1: Environment variable transfer via `env_vars.include` whitelist

**Requirement**: Only explicitly whitelisted environment variables are transferred to remote hosts.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.4-UNIT-001 | Unit | P0 | `EnvVarsConfig` validates required fields (include list, mode) | Pure validation logic, dataclass constraints |
| 001.4-UNIT-002 | Unit | P0 | **SECURITY**: Only vars in `include` list are returned; vars not in whitelist are never returned | Critical security boundary - pure function testing |
| 001.4-UNIT-003 | Unit | P1 | Missing env vars in `include` list logged at debug, not returned | Error handling in isolated component |

---

### AC2: Pattern exclusion via `env_vars.exclude_patterns`

**Requirement**: Glob patterns filter out matched variables from the include list.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.4-UNIT-004 | Unit | P0 | **SECURITY**: Exclude patterns filter vars matching glob (e.g., `*_SECRET`, `AWS_*`) | Critical security - pure function with glob matching |
| 001.4-UNIT-005 | Unit | P1 | Multiple exclude patterns applied cumulatively | Algorithm correctness for pattern matching |
| 001.4-UNIT-006 | Unit | P2 | Empty exclude_patterns list passes all included vars through | Edge case - empty list handling |

---

### AC3: Transfer mode `ssh_env` generates `-o SendEnv` flags

**Requirement**: When mode is `ssh_env`, SSH options are generated with SendEnv for each filtered variable.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.4-UNIT-007 | Unit | P1 | `build_ssh_options()` returns `-o SendEnv=VAR` for each filtered var | Pure function generating SSH options |
| 001.4-INT-001 | Integration | P1 | SSH command with SendEnv options transfers vars to remote | Component interaction (env handler + SSH subprocess) |

---

### AC4: Transfer mode `export_file` generates sourced script

**Requirement**: When mode is `export_file`, a bash script is generated with export statements.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.4-UNIT-008 | Unit | P1 | `generate_export_script()` creates valid bash with escaped single quotes | Pure function with string escaping logic |
| 001.4-UNIT-009 | Unit | P1 | Script contains `#!/bin/bash` header and `export KEY='value'` format | Output format validation |
| 001.4-INT-002 | Integration | P1 | Export script transferred to remote and sourced successfully | Multi-step file transfer and execution |

---

### AC5: Transfer mode `none` skips env transfer

**Requirement**: When mode is `none`, no environment manipulation occurs.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.4-UNIT-010 | Unit | P1 | Mode `none` returns empty SSH options and no script | Pure conditional logic |
| 001.4-INT-003 | Integration | P1 | Remote execution with `none` mode uses pre-existing remote environment | Full flow validation |

---

### AC6: Secrets backend awareness logging

**Requirement**: When `secrets.backend != 'env'`, log informational message about cloud credential requirements.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.4-INT-004 | Integration | P1 | Info log emitted when `secrets.backend` is `aws_ssm`, `gcp_secretmanager`, etc. | Log output verification, multi-component |
| 001.4-INT-005 | Integration | P2 | No log when `secrets.backend == 'env'` (default) | Negative case, log filtering |

---

### AC7: Full engine features on remote

**Requirement**: Remote branches support nested parallel, dynamic parallel, and conditional edges.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.4-E2E-001 | E2E | P1 | Nested parallel graph executes correctly on remote host | Critical path - complex workflow on remote |
| 001.4-E2E-002 | E2E | P1 | Dynamic parallel (`parallel_for`) executes on remote host | User journey - dynamic fan-out on remote |
| 001.4-E2E-003 | E2E | P1 | Conditional edges evaluate correctly on remote host | Cross-system workflow validation |

---

### AC8: Interrupt point validation

**Requirement**: Interrupt points (`interrupt_before`/`interrupt_after`) within remote execution scope raise validation error.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.4-UNIT-011 | Unit | P0 | **CRITICAL**: `_validate_remote_scope()` detects `interrupt_before=True` in scope | Pure graph traversal logic |
| 001.4-UNIT-012 | Unit | P0 | **CRITICAL**: `_validate_remote_scope()` detects `interrupt_after=True` in scope | Pure graph traversal logic |
| 001.4-INT-006 | Integration | P0 | Graph compilation with interrupt in remote scope raises `ValidationError` | Component boundary validation |

---

### AC9: Checkpoint warnings for non-distributed backends

**Requirement**: Log warning when checkpoint backend is `memory` or `file` with remote strategy.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.4-INT-007 | Integration | P1 | Warning logged when `checkpoint.backend` is `memory` with remote strategy | Log output verification |

---

### AC10: LTM warnings for local sqlite with remote strategy

**Requirement**: Log warning when LTM backend is `sqlite` with local path (not cloud storage).

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.4-INT-008 | Integration | P1 | Warning logged when `ltm.backend='sqlite'` with local path | Log output verification |

---

## Risk Coverage Matrix

| Risk ID | Risk Description | Mitigating Test IDs |
|---------|------------------|---------------------|
| RISK-001 | Env var leakage | 001.4-UNIT-002, 001.4-UNIT-004 |
| RISK-002 | Credential exposure | 001.4-UNIT-004, 001.4-INT-004 |
| RISK-003 | Interrupt in remote hangs | 001.4-UNIT-011, 001.4-UNIT-012, 001.4-INT-006 |
| RISK-004 | SSH AcceptEnv not configured | 001.4-INT-001 (documents server requirements) |
| RISK-005 | Backend state isolation | 001.4-INT-007, 001.4-INT-008 |

---

## Test Data Requirements

### Environment Variables for Testing

```python
# Positive test data
TEST_ENV_VARS = {
    "OPENAI_API_KEY": "test-key-openai",
    "ANTHROPIC_API_KEY": "test-key-anthropic",
    "LOG_LEVEL": "DEBUG",
    "DEBUG": "true",
    "AWS_SECRET_ACCESS_KEY": "should-be-excluded",
    "DB_PASSWORD": "should-be-excluded",
}

# Include whitelist
INCLUDE_LIST = ["OPENAI_API_KEY", "ANTHROPIC_API_KEY", "LOG_LEVEL", "DEBUG"]

# Exclude patterns
EXCLUDE_PATTERNS = ["*_SECRET*", "*_PASSWORD", "AWS_*"]
```

### YAML Configuration Fixtures

```yaml
# Fixture: ssh_env mode
env_vars_ssh_env:
  include: ["OPENAI_API_KEY", "LOG_LEVEL"]
  exclude_patterns: ["*_SECRET"]
  mode: ssh_env

# Fixture: export_file mode
env_vars_export_file:
  include: ["API_KEY", "DEBUG"]
  exclude_patterns: []
  mode: export_file

# Fixture: none mode
env_vars_none:
  include: []
  exclude_patterns: []
  mode: none
```

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 Security & Validation)

1. 001.4-UNIT-001 - Config validation
2. 001.4-UNIT-002 - Whitelist enforcement (SECURITY)
3. 001.4-UNIT-004 - Exclude pattern filtering (SECURITY)
4. 001.4-UNIT-011 - Interrupt detection (before)
5. 001.4-UNIT-012 - Interrupt detection (after)
6. 001.4-INT-006 - Interrupt validation error

### Phase 2: Core Functionality (P1)

7. 001.4-UNIT-003 - Missing var handling
8. 001.4-UNIT-005 - Multiple patterns
9. 001.4-UNIT-007 - SSH options generation
10. 001.4-UNIT-008 - Script generation
11. 001.4-UNIT-009 - Script format
12. 001.4-UNIT-010 - None mode
13. 001.4-INT-001 - SSH env transfer
14. 001.4-INT-002 - Export file transfer
15. 001.4-INT-003 - None mode flow
16. 001.4-INT-004 - Secrets backend log
17. 001.4-INT-007 - Checkpoint warning
18. 001.4-INT-008 - LTM warning

### Phase 3: E2E Validation (P1)

19. 001.4-E2E-001 - Nested parallel on remote
20. 001.4-E2E-002 - Dynamic parallel on remote
21. 001.4-E2E-003 - Conditional edges on remote

### Phase 4: Edge Cases (P2)

22. 001.4-UNIT-006 - Empty exclude patterns
23. 001.4-INT-005 - No log for default backend

---

## Test Implementation Notes

### Unit Test Structure (`test_remote_env.py`)

```python
import pytest
from the_edge_agent.remote_env import EnvVarsConfig, RemoteEnvHandler

class TestEnvVarsConfig:
    """001.4-UNIT-001: Config validation"""

    def test_default_values(self):
        config = EnvVarsConfig()
        assert config.include == []
        assert config.exclude_patterns == []
        assert config.mode == "ssh_env"

    def test_invalid_mode_raises(self):
        with pytest.raises(ValueError):
            EnvVarsConfig(mode="invalid")


class TestEnvVarFiltering:
    """001.4-UNIT-002, 001.4-UNIT-004: Security filtering"""

    @pytest.fixture
    def handler_with_whitelist(self, monkeypatch):
        monkeypatch.setenv("OPENAI_API_KEY", "test")
        monkeypatch.setenv("UNWANTED_VAR", "secret")
        config = EnvVarsConfig(include=["OPENAI_API_KEY"])
        return RemoteEnvHandler(config)

    def test_only_whitelisted_vars_returned(self, handler_with_whitelist):
        result = handler_with_whitelist.get_filtered_env_vars()
        assert "OPENAI_API_KEY" in result
        assert "UNWANTED_VAR" not in result  # SECURITY: Not in whitelist

    def test_exclude_patterns_filter(self, monkeypatch):
        monkeypatch.setenv("API_KEY", "test")
        monkeypatch.setenv("API_SECRET", "secret")
        config = EnvVarsConfig(
            include=["API_KEY", "API_SECRET"],
            exclude_patterns=["*_SECRET"]
        )
        handler = RemoteEnvHandler(config)
        result = handler.get_filtered_env_vars()
        assert "API_KEY" in result
        assert "API_SECRET" not in result  # SECURITY: Excluded by pattern
```

### Integration Test Structure (`test_remote_validation.py`)

```python
import pytest
from the_edge_agent import StateGraph

class TestInterruptValidation:
    """001.4-INT-006: Interrupt in remote scope raises ValidationError"""

    def test_interrupt_in_remote_scope_raises(self):
        graph = StateGraph({"value": int})
        graph.add_node("branch_a", interrupt_before=True, run=lambda s: s)
        graph.add_node("merge", run=lambda s: s)

        graph.set_entry_point("branch_a")
        graph.add_conditional_edges(
            "branch_a",
            "merge",
            parallel=True,
            parallel_strategy="remote"
        )

        with pytest.raises(ValidationError, match="interrupt_before"):
            graph.compile()
```

### E2E Test Requirements

E2E tests require:
- SSH access to test remote host (localhost or Docker container)
- Pre-installed `tea` binary on remote
- Network isolation for test execution

---

## Coverage Validation

### AC Coverage Summary

| AC | Test Count | P0 | P1 | P2 | Gaps |
|----|------------|----|----|----|----|
| AC1 | 3 | 2 | 1 | 0 | None |
| AC2 | 3 | 1 | 1 | 1 | None |
| AC3 | 2 | 0 | 2 | 0 | None |
| AC4 | 3 | 0 | 3 | 0 | None |
| AC5 | 2 | 0 | 2 | 0 | None |
| AC6 | 2 | 0 | 1 | 1 | None |
| AC7 | 3 | 0 | 3 | 0 | None |
| AC8 | 3 | 3 | 0 | 0 | None |
| AC9 | 1 | 0 | 1 | 0 | None |
| AC10 | 1 | 0 | 1 | 0 | None |

### Defense in Depth

Critical security paths have multiple test levels:
- **Env var whitelist**: Unit (001.4-UNIT-002) + Integration (001.4-INT-001)
- **Pattern exclusion**: Unit (001.4-UNIT-004) + covered by integration transfer tests
- **Interrupt validation**: Unit (001.4-UNIT-011, 012) + Integration (001.4-INT-006)

---

## Quality Checklist

- [x] Every AC has at least one test
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (security = P0)
- [x] Test IDs follow naming convention (`001.4-{LEVEL}-{SEQ}`)
- [x] Scenarios are atomic and independent
- [x] Security-critical paths have P0 priority
- [x] Fail-fast execution order defined

---

## Gate YAML Block

```yaml
test_design:
  date: "2026-01-01"
  story_id: "TEA-PARALLEL-001.4"
  scenarios_total: 21
  by_level:
    unit: 10
    integration: 8
    e2e: 3
  by_priority:
    p0: 6
    p1: 13
    p2: 2
  coverage_gaps: []
  security_tests:
    - 001.4-UNIT-002  # Whitelist-only enforcement
    - 001.4-UNIT-004  # Exclude pattern filtering
    - 001.4-UNIT-011  # Interrupt before detection
    - 001.4-UNIT-012  # Interrupt after detection
    - 001.4-INT-006   # Validation error raised
  recommended_execution:
    phase_1: [001.4-UNIT-001, 001.4-UNIT-002, 001.4-UNIT-004, 001.4-UNIT-011, 001.4-UNIT-012, 001.4-INT-006]
    phase_2: [001.4-UNIT-003, 001.4-UNIT-005, 001.4-UNIT-007, 001.4-UNIT-008, 001.4-UNIT-009, 001.4-UNIT-010, 001.4-INT-001, 001.4-INT-002, 001.4-INT-003, 001.4-INT-004, 001.4-INT-007, 001.4-INT-008]
    phase_3: [001.4-E2E-001, 001.4-E2E-002, 001.4-E2E-003]
    phase_4: [001.4-UNIT-006, 001.4-INT-005]
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-PARALLEL-001.4-test-design-20260101.md
P0 tests identified: 6
Security-critical tests: 5
Total scenarios: 21
```
