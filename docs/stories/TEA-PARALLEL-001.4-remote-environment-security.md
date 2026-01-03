# Story: TEA-PARALLEL-001.4 - Remote Environment & Security

## Status: Ready for Development (QA Approved)

**Epic**: [TEA-PARALLEL-001 - Multi-Strategy Parallel Execution](./TEA-PARALLEL-001-multi-strategy-execution-epic.md)
**Estimated Tests**: 21 scenarios (QA expanded from 19)
**Dependencies**: [TEA-PARALLEL-001.3](./TEA-PARALLEL-001.3-remote-executor-core.md) (Remote Executor Core)
**Story Checklist**: PASSED (2026-01-01)

---

## User Story

**As a** workflow developer,
**I want** secure environment variable transfer and validation for remote execution,
**So that** I can safely use API keys and secrets across distributed workflows.

---

## Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | Environment variable transfer via `env_vars.include` whitelist | Unit test: filtering |
| AC2 | Pattern exclusion via `env_vars.exclude_patterns` | Unit test: glob matching |
| AC3 | Transfer mode: `ssh_env` generates `-o SendEnv` flags | Unit test: SSH options |
| AC4 | Transfer mode: `export_file` generates sourced script | Unit test: script content |
| AC5 | Transfer mode: `none` skips env transfer | Unit test: no manipulation |
| AC6 | Secrets backend awareness: Log when `secrets.backend != env` | Integration test: log message |
| AC7 | Full engine features on remote (nested parallel, dynamic parallel, conditionals) | E2E test: features work |
| AC8 | Interrupt point validation: Error if in remote scope | Unit test: validation |
| AC9 | Checkpoint warnings for non-distributed backends | Integration test: warning logged |
| AC10 | LTM warnings for local sqlite with remote strategy | Integration test: warning logged |

---

## Technical Design

### Files to Create/Modify

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/remote_env.py` | Create | Environment variable transfer logic |
| `python/src/the_edge_agent/parallel_executors.py` | Modify | Add env handling to RemoteExecutor |
| `python/src/the_edge_agent/stategraph.py` | Modify | Add interrupt validation |
| `python/src/the_edge_agent/yaml_engine.py` | Modify | Parse env_vars config |

### YAML Configuration

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
        # Explicit whitelist - ONLY these vars are considered
        include:
          - OPENAI_API_KEY
          - ANTHROPIC_API_KEY
          - LOG_LEVEL
          - DEBUG

        # Pattern-based exclusion (applied after include)
        exclude_patterns:
          - "*_SECRET"
          - "*_PASSWORD"
          - "AWS_*"  # Don't auto-transfer cloud credentials

        # Transfer mode
        mode: ssh_env  # ssh_env | export_file | none
```

### Transfer Modes

| Mode | Mechanism | Security | Use Case |
|------|-----------|----------|----------|
| `ssh_env` | `ssh -o SendEnv=VAR` | Medium | Standard (requires server `AcceptEnv`) |
| `export_file` | Write script, source on remote | Lower | Servers without AcceptEnv |
| `none` | No transfer | High | Pre-configured servers |

### Environment Handler Implementation

```python
# remote_env.py
from dataclasses import dataclass, field
from typing import List, Literal, Dict
from fnmatch import fnmatch
import os
import logging

logger = logging.getLogger(__name__)


@dataclass
class EnvVarsConfig:
    include: List[str] = field(default_factory=list)
    exclude_patterns: List[str] = field(default_factory=list)
    mode: Literal["ssh_env", "export_file", "none"] = "ssh_env"


class RemoteEnvHandler:
    """Handle environment variable transfer to remote hosts."""

    def __init__(self, config: EnvVarsConfig):
        self.config = config

    def get_filtered_env_vars(self) -> Dict[str, str]:
        """
        Get env vars matching include list, minus exclude patterns.

        Security: Only vars explicitly in `include` are considered.
        Then `exclude_patterns` filters those out.
        """
        result = {}

        for var in self.config.include:
            if var not in os.environ:
                logger.debug(f"Env var '{var}' in include list not found in environment")
                continue

            # Check exclude patterns
            excluded = any(
                fnmatch(var, pattern)
                for pattern in self.config.exclude_patterns
            )

            if excluded:
                logger.debug(f"Env var '{var}' excluded by pattern")
                continue

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

        # Escape single quotes in values
        lines = []
        for key, value in env_vars.items():
            escaped_value = value.replace("'", "'\"'\"'")
            lines.append(f"export {key}='{escaped_value}'")

        script = "#!/bin/bash\n" + "\n".join(lines) + "\n"

        with open(path, "w") as f:
            f.write(script)

        return path

    def get_source_command(self, remote_script_path: str) -> str:
        """Get command to source env script on remote."""
        if self.config.mode != "export_file":
            return ""
        return f"source {remote_script_path} && "
```

### Interrupt Validation

```python
# stategraph.py
def _validate_remote_scope(
    self,
    entry_point: str,
    exit_point: str
) -> List[str]:
    """
    Validate that remote execution scope doesn't contain interrupt points.

    Interrupt points require human interaction, which can't happen on
    a remote host that's executing a branch.

    Returns:
        List of error messages (empty if valid)
    """
    errors = []

    # Get all nodes in the execution path
    nodes_in_scope = self._get_nodes_between(entry_point, exit_point)

    for node_name in nodes_in_scope:
        node = self.nodes.get(node_name)
        if not node:
            continue

        if getattr(node, 'interrupt_before', False):
            errors.append(
                f"Node '{node_name}' has interrupt_before=True, which is not "
                f"supported in remote execution scope. Move interrupt points "
                f"before the parallel fan-out."
            )

        if getattr(node, 'interrupt_after', False):
            errors.append(
                f"Node '{node_name}' has interrupt_after=True, which is not "
                f"supported in remote execution scope. Move interrupt points "
                f"after the parallel fan-in."
            )

    return errors


def _get_nodes_between(
    self,
    start: str,
    end: str
) -> List[str]:
    """Get all nodes reachable from start before reaching end."""
    visited = set()
    result = []
    queue = [start]

    while queue:
        current = queue.pop(0)

        if current == end or current in visited:
            continue

        visited.add(current)
        result.append(current)

        # Get next nodes
        for edge in self.edges:
            if edge.source == current:
                targets = edge.targets if isinstance(edge.targets, list) else [edge.targets]
                queue.extend(targets)

    return result
```

### Backend Warnings

```python
# Add to RemoteExecutor initialization
def _emit_backend_warnings(self, settings: Dict) -> None:
    """Emit warnings for backend configurations that may not work well with remote."""

    # Secrets backend warning
    secrets_backend = settings.get("secrets", {}).get("backend", "env")
    if secrets_backend != "env":
        logger.info(
            f"Remote hosts should have cloud provider credentials configured locally. "
            f"secrets.backend='{secrets_backend}' requires remote hosts to have "
            f"appropriate IAM roles or credentials."
        )

    # LTM backend warning
    ltm_backend = settings.get("ltm", {}).get("backend", "sqlite")
    ltm_path = settings.get("ltm", {}).get("path", "")

    if ltm_backend == "sqlite" and not ltm_path.startswith("s3://"):
        logger.warning(
            f"LTM backend 'sqlite' with local path is not shared across remote hosts. "
            f"Each remote host will have isolated LTM state. "
            f"Consider using 'duckdb' with cloud storage for shared LTM."
        )

    # Checkpoint backend warning
    checkpoint_backend = settings.get("checkpoint", {}).get("backend", "memory")
    if checkpoint_backend in ("memory", "file"):
        logger.warning(
            f"Checkpoint backend '{checkpoint_backend}' is not shared across remote hosts. "
            f"Checkpoints created on remote hosts will be local to that host. "
            f"Consider using a distributed checkpoint backend (S3/GCS) for shared checkpoints."
        )
```

### Recommended Workflow Pattern

```yaml
# ✅ CORRECT: Interrupt BEFORE parallel fan-out (on main host)
nodes:
  - name: human_review
    interrupt_after: true  # Pause here for human input
    run: |
      return state

  - name: branch_a
    run: |
      # This runs on remote - no interrupt allowed here
      return {"result": process(state)}

  - name: merge
    run: |
      return {"merged": parallel_results}

edges:
  - from: human_review
    to: [branch_a, branch_b]
    parallel: true
    parallel_strategy: remote
    fan_in: merge

# ❌ INCORRECT: Interrupt INSIDE parallel branch
nodes:
  - name: branch_a_step
    interrupt_before: true  # ERROR: Not supported in remote scope
    run: |
      return state
```

---

## Testing

### Test Location

- `python/tests/test_remote_env.py` - Environment variable handling
- `python/tests/test_remote_validation.py` - Interrupt and backend validation

### Test Scenarios (19 total)

#### AC1-5: Environment Variable Transfer (10 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.4-UNIT-001 | Unit | P0 | `EnvVarsConfig` validates fields |
| 001.4-UNIT-002 | Unit | P0 | **Security**: Only whitelisted vars returned |
| 001.4-UNIT-003 | Unit | P0 | **Security**: Exclude patterns filter |
| 001.4-UNIT-004 | Unit | P1 | SSH `-o SendEnv` options generated |
| 001.4-UNIT-005 | Unit | P1 | Export script generation |
| 001.4-INT-001 | Integration | P0 | `ssh_env` mode works end-to-end |
| 001.4-INT-002 | Integration | P1 | `export_file` mode transfers and sources |
| 001.4-INT-003 | Integration | P1 | `none` mode skips env transfer |
| 001.4-INT-004 | Integration | P1 | Missing env var handled gracefully |
| 001.4-INT-005 | Integration | P1 | YAML parsing for env_vars config |

#### AC6: Secrets Backend (2 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.4-UNIT-006 | Unit | P1 | Detect `secrets.backend != env` |
| 001.4-INT-006 | Integration | P1 | Info log emitted for cloud backends |

#### AC7: Full Engine on Remote (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.4-E2E-001 | E2E | P1 | Nested parallel works on remote |
| 001.4-E2E-002 | E2E | P1 | Dynamic parallel works on remote |
| 001.4-E2E-003 | E2E | P1 | Conditional edges work on remote |

#### AC8: Interrupt Validation (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.4-UNIT-007 | Unit | P0 | **Critical**: Detect `interrupt_before` in scope |
| 001.4-UNIT-008 | Unit | P0 | **Critical**: Detect `interrupt_after` in scope |
| 001.4-INT-007 | Integration | P0 | Interrupt in scope raises ValidationError |

#### AC9-10: Backend Warnings (1 test)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.4-INT-008 | Integration | P1 | Checkpoint/LTM warnings logged |

---

## Definition of Done

- [ ] `RemoteEnvHandler` class implemented
- [ ] `EnvVarsConfig` dataclass with validation
- [ ] `ssh_env` mode generates correct SSH options
- [ ] `export_file` mode generates and transfers script
- [ ] `none` mode skips environment handling
- [ ] `_validate_remote_scope()` detects interrupt points
- [ ] Backend warnings implemented
- [ ] YAML parsing for `env_vars` configuration
- [ ] All 19 test scenarios pass
- [ ] Code reviewed and merged

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Env var leakage | High | Explicit whitelist, never auto-transfer |
| Credential exposure | High | Log warning for cloud creds |
| Interrupt in remote hangs | High | Validation error at compile time |
| SSH AcceptEnv not configured | Medium | Document server requirements |

---

## Security Considerations

1. **Whitelist-only**: Only variables explicitly listed in `include` are considered. Never auto-discover env vars.

2. **Cloud credentials**: Do NOT auto-transfer `AWS_*`, `AZURE_*`, `GOOGLE_*` vars. User must explicitly add them.

3. **Pattern exclusion**: Always apply `exclude_patterns` as a safety net.

4. **No secrets in logs**: Never log env var values, only names.

5. **Export script cleanup**: Remove export script from remote after sourcing.

---

## Notes for Developer

1. **Test with monkeypatch**: Use `pytest`'s `monkeypatch.setenv()` to set test env vars.

2. **Escape values**: When generating export scripts, escape single quotes properly.

3. **Graceful missing vars**: If an `include` var isn't in env, log debug and skip (don't fail).

4. **Interrupt path traversal**: Use BFS to find all nodes between entry and exit for validation.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-01 | 1.0 | Story extracted from epic | Sarah (PO) |
| 2026-01-01 | 1.1 | Story checklist validation PASSED | Bob (SM) |

---

## QA Results

### Test Design Review - 2026-01-01

**Reviewer:** Quinn (Test Architect)
**Status:** Test Design Complete

#### Summary

| Metric | Story Estimate | QA Design |
|--------|----------------|-----------|
| **Total Scenarios** | 19 | 21 |
| **Unit Tests** | - | 10 (48%) |
| **Integration Tests** | - | 8 (38%) |
| **E2E Tests** | - | 3 (14%) |

#### Priority Distribution

| Priority | Count | Focus Areas |
|----------|-------|-------------|
| **P0 (Critical)** | 6 | Security boundaries, interrupt validation |
| **P1 (High)** | 13 | Core env transfer modes, backend warnings |
| **P2 (Medium)** | 2 | Edge cases |

#### Security-Critical Tests (P0)

- `001.4-UNIT-002`: Whitelist-only enforcement
- `001.4-UNIT-004`: Exclude pattern filtering (`*_SECRET`, `AWS_*`)
- `001.4-UNIT-011`: Interrupt before detection in remote scope
- `001.4-UNIT-012`: Interrupt after detection in remote scope
- `001.4-INT-006`: ValidationError raised at compile-time

#### Coverage Assessment

- All 10 ACs covered with no gaps
- Defense in depth: Critical security paths tested at unit + integration levels
- Shift-left achieved: 48% unit tests for fast feedback

#### Additional Scenarios (vs story estimate)

1. `001.4-UNIT-006`: Empty exclude_patterns edge case
2. `001.4-INT-005`: Negative case - no log when `secrets.backend == 'env'`

#### Test Design Document

`docs/qa/assessments/TEA-PARALLEL-001.4-test-design-20260101.md`

#### Recommendations

1. **Fail-fast execution**: Run P0 security tests first in CI
2. **E2E environment**: Requires SSH access to test host (localhost or Docker)
3. **Monkeypatch usage**: Use `pytest`'s `monkeypatch.setenv()` for env var tests

---

## QA Notes

### Test Coverage Summary

| Category | Coverage | Notes |
|----------|----------|-------|
| **Environment Variable Filtering** | Complete | 6 tests covering whitelist, exclude patterns, edge cases |
| **Transfer Modes** | Complete | 6 tests across ssh_env, export_file, none modes |
| **Security Validation** | Complete | 5 P0 tests for critical security boundaries |
| **Backend Warnings** | Complete | 3 tests for checkpoint, LTM, secrets backend awareness |
| **Interrupt Validation** | Complete | 3 tests ensuring compile-time detection |
| **Full Engine on Remote** | Complete | 3 E2E tests for nested/dynamic parallel and conditionals |

**Total: 21 scenarios across 10 ACs with 0 gaps identified.**

### Risk Areas Identified

| Risk Level | Area | Description | Mitigation |
|------------|------|-------------|------------|
| **HIGH** | Env Var Leakage | Accidental exposure of sensitive vars to remote | Whitelist-only design + P0 unit tests |
| **HIGH** | Credential Exposure | Cloud creds (AWS_*, etc.) transferred to remote | Explicit exclude patterns + warning logs |
| **HIGH** | Remote Interrupt Hang | Interrupt point in remote scope causes indefinite hang | Compile-time validation error |
| **MEDIUM** | SSH AcceptEnv Config | Server may not accept forwarded env vars | Documentation + fallback to export_file mode |
| **MEDIUM** | Backend State Isolation | Local checkpoint/LTM not shared across hosts | Warning logs + documentation |
| **LOW** | Script Injection | Malformed env values could break export script | Single-quote escaping in generation |

### Recommended Test Scenarios

#### Phase 1: Fail-Fast Security (Run First in CI)
1. `001.4-UNIT-001` - Config validation
2. `001.4-UNIT-002` - **SECURITY**: Whitelist-only enforcement
3. `001.4-UNIT-004` - **SECURITY**: Exclude pattern filtering
4. `001.4-UNIT-011` - **CRITICAL**: Interrupt before detection
5. `001.4-UNIT-012` - **CRITICAL**: Interrupt after detection
6. `001.4-INT-006` - ValidationError raised at compile-time

#### Phase 2: Core Functionality
7-18. Transfer mode tests, backend warnings, YAML parsing

#### Phase 3: E2E Validation
19. `001.4-E2E-001` - Nested parallel on remote
20. `001.4-E2E-002` - Dynamic parallel on remote
21. `001.4-E2E-003` - Conditional edges on remote

#### Phase 4: Edge Cases
22. `001.4-UNIT-006` - Empty exclude patterns
23. `001.4-INT-005` - No log for default backend

### Concerns and Blockers

| Type | Item | Status | Action Required |
|------|------|--------|-----------------|
| **Concern** | E2E tests require SSH infrastructure | Open | Setup Docker-based SSH test environment or use localhost |
| **Concern** | SSH AcceptEnv server config varies | Open | Document server requirements in user-facing docs |
| **Concern** | Export script cleanup on failure | Open | Ensure script removal in finally block |
| **None** | No blockers identified | N/A | Story ready for development |

### Test Infrastructure Requirements

1. **Unit Tests**: Standard pytest with monkeypatch for env var mocking
2. **Integration Tests**: Logging capture, subprocess mocking for SSH
3. **E2E Tests**: Docker container with SSH server or localhost SSH access

### QA Approval

**Status**: APPROVED for Development
**Date**: 2026-01-01
**Reviewer**: Quinn (Test Architect)

Test design is comprehensive with appropriate security focus. All acceptance criteria have corresponding test scenarios. Defense-in-depth achieved for critical security paths.
