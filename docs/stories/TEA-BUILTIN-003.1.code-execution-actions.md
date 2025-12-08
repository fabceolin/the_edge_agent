# Story TEA-BUILTIN-003.1: Code Execution Actions

## Status

Done

**QA Gate:** PASS
**QA Notes:** All 41 tests pass (20 P0 security, 21 P1 functional). Feature disabled by default. Documentation complete. All ACs met except AC4 (formally deferred to Phase 2).

## Story

**As a** YAML agent developer,
**I want** built-in code execution actions (execute, sandbox),
**so that** I can build agents that safely run generated code without writing Python infrastructure.

## Design Decisions

### Sandbox Technology: RestrictedPython

**Decision Date:** 2024-12-06

**Context:** Evaluated multiple sandbox approaches for Cloud Run 2nd gen / Firebase Functions compatibility.

**Options Considered:**
| Approach | Verdict | Reason |
|----------|---------|--------|
| RestrictedPython | ✅ **Selected** | Simple, no deps, Cloud Run compatible |
| RustPython WASM | ⏳ Future option | Better isolation, more complexity |
| Pyodide WASM | ❌ Deferred | Server-side complexity, cold start impact |
| Docker-in-Docker | ❌ Rejected | Not possible in Cloud Run |
| nsjail/firejail | ❌ Rejected | Requires kernel privileges |
| E2B Cloud | ⏳ Future option | External API, good for untrusted code |

**Cloud Run 2nd Gen Constraints:**
- No Docker-in-Docker
- No privileged containers
- No custom seccomp profiles
- Userspace execution only

**Phase Plan:**
- **Phase 1 (This Story):** RestrictedPython only, Python language
- **Phase 2 (Future):** Add E2B as "secure mode" for untrusted code
- **Phase 3 (Future):** Evaluate RustPython WASM if needed

## Acceptance Criteria

1. `code.execute` action runs Python code in RestrictedPython sandbox
2. `code.sandbox` action creates persistent sandbox sessions for multi-step execution
3. Sandboxed execution prevents filesystem, network, and system access by default
4. ~~Execution supports Python, JavaScript, and shell scripts~~ **Phase 1: Python only**
5. Resource limits enforced (CPU time via timeout, output size)
6. Execution captures stdout, stderr, return values, and exceptions
7. All actions follow existing `_setup_builtin_actions()` pattern
8. Actions are accessible via both `code.*` and `actions.code_*` namespaces
9. Comprehensive unit tests cover all code execution operations
10. Documentation updated in CLAUDE.md and docs/YAML_AGENTS.md

## Dependencies

**Blocked By**: None (can start independently)

**Blocks**: None

**Internal Dependencies**:
- ~~JavaScript execution requires optional PyMiniRacer~~ (Deferred to Phase 2+)
- ~~E2B integration requires optional e2b library~~ (Deferred to Phase 2)

## User Prerequisites

- [ ] **Required**: `pip install RestrictedPython` (for Python sandbox)
- [ ] ~~**Optional**: `pip install py_mini_racer` (for JavaScript execution)~~ (Deferred)
- [ ] ~~**Optional**: Obtain `E2B_API_KEY` from https://e2b.dev (for cloud execution)~~ (Deferred)
- [ ] **Security Warning**: Review security implications before enabling

## Tasks / Subtasks

### Phase 1: RestrictedPython Implementation (This Story)

- [x] Task 1: Implement `RestrictedPythonSandbox` class (AC: 1, 3, 5, 6)
  - [x] Create `RestrictedPythonSandbox` class with hardened configuration
  - [x] Block dangerous dunder attributes (`__class__`, `__mro__`, `__subclasses__`, etc.)
  - [x] Whitelist safe builtins only:
    ```python
    ALLOWED_BUILTINS = {
        # Math
        'abs', 'round', 'min', 'max', 'sum', 'pow', 'divmod',
        # Types
        'int', 'float', 'str', 'bool', 'list', 'dict', 'tuple', 'set',
        # Iteration
        'len', 'range', 'enumerate', 'zip', 'map', 'filter', 'sorted', 'reversed',
        # Predicates
        'all', 'any', 'isinstance',
        # String
        'chr', 'ord', 'repr',
        # Constants
        'True', 'False', 'None',
    }
    ```
  - [x] Implement `_guarded_getattr` to block attribute traversal attacks
  - [x] Implement timeout using `signal.SIGALRM` (Unix) with threading fallback (Windows)
  - [x] Capture stdout/stderr using `io.StringIO`
  - [x] Enforce `max_output_bytes` truncation
  - [x] Return structured result:
    ```python
    {
        "success": bool,
        "stdout": str,
        "stderr": str,
        "return_value": any,
        "error": Optional[str],
        "execution_time_ms": float
    }
    ```

- [x] Task 2: Implement `code.execute` action (AC: 1, 6, 7, 8)
  - [x] Define function signature: `code_execute(state, code, timeout=30, **kwargs)`
  - [x] Instantiate `RestrictedPythonSandbox` and execute code
  - [x] Register in actions dict with dual namespaces:
    - `actions["code.execute"]`
    - `actions["actions.code_execute"]`

- [x] Task 3: Implement `code.sandbox` action (AC: 2, 7, 8)
  - [x] Define function signature: `code_sandbox(state, action, sandbox_id=None, code=None, **kwargs)`
  - [x] Support actions: "create", "execute", "destroy", "list"
  - [x] Maintain sandbox sessions with persistent `_locals` dict
  - [x] Store sessions in `YAMLEngine._sandbox_sessions: Dict[str, RestrictedPythonSandbox]`
  - [x] Return appropriate result based on action:
    - create: `{"sandbox_id": str, "created": True}`
    - execute: `{"stdout": str, "stderr": str, ...}`
    - destroy: `{"destroyed": bool, "sandbox_id": str}`
    - list: `{"sandboxes": List[str]}`
  - [x] Register in actions dict with dual namespaces

- [x] Task 4: Write tests (AC: 9)
  - [x] Test code.execute with safe Python code
  - [x] Test code.execute blocks dangerous operations (P0 security tests)
  - [x] Test timeout enforcement
  - [x] Test output capture (stdout, stderr)
  - [x] Test code.sandbox session persistence
  - [x] Test max_output truncation

- [x] Task 5: Update documentation (AC: 10)
  - [x] Add code execution actions to CLAUDE.md
  - [x] Add examples in docs/YAML_AGENTS.md
  - [x] Document security model and limitations
  - [x] Create example YAML showing code agent

### Phase 2+ (Future Stories - Deferred)

- [ ] ~~Task: JavaScript sandbox implementation~~ → Future story TEA-BUILTIN-003.1.1
- [ ] ~~Task: Shell execution~~ → **Removed** (too risky, blocklist approach fundamentally flawed)
- [ ] ~~Task: E2B integration~~ → Future story TEA-BUILTIN-003.1.2

## Dev Notes

### Integration Points
- **File**: `src/the_edge_agent/yaml_engine.py`
- **Method**: `_setup_builtin_actions()` (lines 623-786)

### Dependencies
- **Required**: `RestrictedPython` (for Python sandbox)

### Security Model: RestrictedPython with Hardening

```
APPROACH: Bytecode transformation + guarded attribute access

BLOCKED OPERATIONS:
- No file system access (open, Path not in builtins)
- No network access (no imports allowed)
- No process spawning (no imports allowed)
- No code generation (exec, eval not in builtins)
- No module imports (import statement blocked by RestrictedPython)
- No dunder traversal (__class__, __mro__, __subclasses__ blocked)

ALLOWED BUILTINS (whitelist approach):
- Math: abs, round, min, max, sum, pow, divmod
- Types: int, float, str, bool, list, dict, tuple, set
- Iteration: len, range, enumerate, zip, map, filter, sorted, reversed
- Predicates: all, any, isinstance
- String: chr, ord, repr
- Constants: True, False, None

HARDENING MEASURES:
- _guarded_getattr blocks access to dangerous dunder attributes
- _guarded_getitem for safe indexing
- _guarded_getiter for safe iteration
- Timeout via signal.SIGALRM (Unix) / threading (Windows)
- Output truncation via max_output_bytes
```

### Configuration in YAML
```yaml
settings:
  code:
    default_timeout: 30
    max_output_bytes: 65536
    enabled: false  # Default OFF - must explicitly enable
```

### Key Constraints
- Security is paramount - whitelist approach, not blocklist
- RestrictedPython is the only supported sandbox (Phase 1)
- Default disabled - requires explicit `enable_code_execution=True`
- Cloud Run 2nd gen compatible (pure Python, no system deps)

### Blocked Dunder Attributes (Escape Prevention)
```python
BLOCKED_NAMES = frozenset([
    '__class__', '__bases__', '__mro__', '__subclasses__',
    '__code__', '__globals__', '__builtins__', '__import__',
    '__loader__', '__spec__', '__file__', '__cached__',
])
```

### Known Limitations
- **Not a true sandbox**: Runs in same process, bytecode transformation only
- **No memory limits**: Python's `resource` module unreliable cross-platform
- **Escape vectors may exist**: RestrictedPython is defense-in-depth, not absolute
- **For trusted code patterns only**: Do NOT use for arbitrary LLM-generated code

## Testing

**Test File Location**: `tests/test_yaml_engine_code.py` (new test file)

**Priority Levels**:
- **P0**: ALL security tests - MUST pass before any deployment
- **P1**: Core functionality - Required for basic usage

**Testing Standards**:
- Test security restrictions thoroughly
- Verify dangerous operations are blocked
- Test timeout enforcement

**Unit Test Cases**:
```python
class TestCodeExecutionActions(unittest.TestCase):
    # P1 - Core functionality
    def test_code_execute_simple(self): ...  # (P1)
    def test_code_execute_returns_value(self): ...  # (P1)
    def test_code_execute_captures_stdout(self): ...  # (P1)
    def test_code_execute_captures_stderr(self): ...  # (P1)
    def test_code_execute_timeout(self): ...  # (P1)
    def test_code_sandbox_create_destroy(self): ...  # (P1)
    def test_code_sandbox_session_persistence(self): ...  # (P1)
    def test_dual_namespace_access(self): ...  # (P1) - Verify code.* and actions.code_* work
    def test_max_output_bytes_truncation(self): ...  # (P1) - Output size limits enforced
    def test_execution_time_tracked(self): ...  # (P1) - execution_time_ms returned
```

**Integration Test Cases**:
```python
class TestCodeExecutionActionsIntegration(unittest.TestCase):
    def test_code_execute_in_yaml_workflow(self): ...  # (P1)
    def test_code_sandbox_with_checkpoint(self): ...  # (P1)
```

**Security Test Cases** (ALL P0 - Critical):
```python
class TestCodeExecutionSecurity(unittest.TestCase):
    # P0 - ALL security tests must pass
    def test_blocks_open_builtin(self): ...  # (P0)
    def test_blocks_import_statement(self): ...  # (P0)
    def test_blocks_exec_builtin(self): ...  # (P0)
    def test_blocks_eval_builtin(self): ...  # (P0)
    def test_blocks_dunder_class(self): ...  # (P0)
    def test_blocks_dunder_mro(self): ...  # (P0)
    def test_blocks_dunder_subclasses(self): ...  # (P0)
    def test_blocks_dunder_globals(self): ...  # (P0)
    def test_blocks_dunder_builtins(self): ...  # (P0)
    def test_blocks_getattr_builtin(self): ...  # (P0)
    def test_timeout_prevents_infinite_loop(self): ...  # (P0)
    def test_sandbox_isolation_between_sessions(self): ...  # (P0) - Sessions don't share state
```

**Test Summary**: 24 tests (10 unit + 2 integration + 12 security) | P0: 12 | P1: 12

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] All Phase 1 tasks completed
- [ ] Tests pass (existing and new) - all 24 tests
- [ ] All P0 security tests pass
- [ ] Documentation updated with security warnings
- [ ] Code follows existing patterns in yaml_engine.py
- [ ] Feature disabled by default (`enable_code_execution=False`)

## Rollback Procedure

If code execution actions cause issues in production:

1. **Immediate Rollback** (CRITICAL - Security sensitive):
   ```python
   # In _setup_builtin_actions(), comment out:
   # actions['code.execute'] = code_execute
   # actions['code.sandbox'] = code_sandbox
   ```

2. **State Cleanup**:
   - Sandbox sessions are isolated; safe to terminate
   - `YAMLEngine._sandbox_sessions` can be cleared
   - No persistent state to clean up

3. **Verification**:
   - Run: `pytest tests/test_yaml_engine_code.py`
   - Verify no security vulnerabilities introduced

4. **Gradual Rollout** (STRONGLY Recommended):
   - Feature flag: `YAMLEngine(enable_code_execution=False)` (default OFF)
   - Security review required before enabling
   - Only enable for trusted code patterns
   - Monitor execution logs

5. **Security Incident Response**:
   - If sandbox escape detected, disable immediately
   - Audit all code execution logs
   - Review blocked attribute list for gaps

## QA Results

### Review Date: 2025-12-06 (Updated: 2025-12-07)

### Reviewed By: Quinn (Test Architect)

### Test Design

- **Total Scenarios**: 40 (expanded from original 24)
- **Security Tests (P0)**: 20
- **Functional Tests (P1)**: 17
- **P0 Tests Total**: 23
- **Coverage**: All ACs except AC4 (deferred) and AC10 (docs pending)

Test Design Document: `docs/qa/assessments/TEA-BUILTIN-003.1-test-design-20251206.md`

### Findings

| ID | Severity | Finding | Status |
|----|----------|---------|--------|
| SEC-001 | Medium | RestrictedPython is bytecode transformation, not true isolation | Open |
| SEC-002 | Medium | No memory limits enforceable cross-platform | Open |
| SEC-003 | Low | Original story missing some escape vector tests | **RESOLVED** |
| DOC-001 | Low | Security warnings need prominence in docs | Open |

### Additional Security Tests (Added 2025-12-07)

| Test ID | Description | Priority |
|---------|-------------|----------|
| SEC-016 | Block `type()` builtin (metaclass attacks) | P0 |
| SEC-017 | Block `vars()` and `dir()` (information disclosure) | P0 |
| SEC-018 | Block `input()` builtin (STDIN hijacking) | P0 |
| SEC-019 | Block `help()` builtin (interactive shell access) | P0 |
| SEC-020 | Verify `os` module not accessible | P0 |

### Gate Status

Gate: **CONCERNS** → `docs/qa/gates/TEA-BUILTIN-003.1-code-execution-actions.yml`

### Recommendations

**Must Fix Before Deployment:**
- All 20 P0 security tests (SEC-001 to SEC-020) must pass
- Feature disabled by default (`enable_code_execution=False`)

**Monitor in Production:**
- Memory usage during execution
- Execution time patterns
- Error logs for escape attempts

---

### Review Date: 2025-12-07 (Implementation Review)

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

The implementation is well-structured and follows security best practices:

1. **Security Model**: Whitelist approach for builtins is the correct defensive posture. The `ALLOWED_BUILTINS`, `BLOCKED_NAMES`, and `BLOCKED_BUILTINS` frozensets provide clear, auditable security boundaries.

2. **Guard Functions**: Comprehensive guard implementation:
   - `_guarded_getattr` - Blocks dangerous dunder attributes with safe dunder whitelist
   - `_guarded_getitem/setitem/delitem` - Safe container operations
   - `_guarded_iter` - Safe iteration
   - `_guarded_write` - Allows only safe container types (dict, list, set)
   - `_inplacevar` - Safe in-place operations

3. **Timeout Enforcement**: Threading-based timeout is cross-platform compatible. The thread daemon flag ensures cleanup on timeout.

4. **Output Handling**: PrintCollector pattern for capturing print() output is correct. Output truncation at `max_output_bytes` prevents memory exhaustion.

5. **Session Management**: Sandbox sessions stored on engine instance with proper lifecycle (create/execute/destroy/list).

6. **Feature Flag**: `enable_code_execution=False` default correctly enforces opt-in security.

### Refactoring Performed

None required - implementation is clean and follows established patterns.

### Compliance Check

- Coding Standards: ✓ Follows existing `_setup_builtin_actions()` pattern
- Project Structure: ✓ Actions in `src/the_edge_agent/actions/code_actions.py`
- Testing Strategy: ✓ 41 tests (20 P0 security, 21 P1 functional) - all passing
- All ACs Met: ✓ See trace below

### Acceptance Criteria Trace

| AC | Test Coverage | Status |
|----|---------------|--------|
| AC1: code.execute in RestrictedPython | test_code_execute_simple, test_code_execute_returns_value | ✓ |
| AC2: code.sandbox persistent sessions | test_code_sandbox_session_persistence, test_code_sandbox_create_destroy | ✓ |
| AC3: Sandboxed execution blocks dangerous ops | TestCodeExecutionSecurity (20 tests) | ✓ |
| AC4: Multi-language support | Deferred to Phase 2 (Python only) | N/A |
| AC5: Resource limits (timeout, output) | test_code_execute_timeout, test_max_output_bytes_truncation | ✓ |
| AC6: Captures stdout/stderr/return/exceptions | test_code_execute_captures_stdout/stderr, test_code_execute_returns_value | ✓ |
| AC7: Follows _setup_builtin_actions pattern | Code review verified | ✓ |
| AC8: Dual namespace access | test_dual_namespace_access | ✓ |
| AC9: Comprehensive unit tests | 41 tests all passing | ✓ |
| AC10: Documentation updated | CLAUDE.md and YAML_AGENTS.md verified | ✓ |

### Security Review

**Verified Blocked Operations:**
- `open()`, `exec()`, `eval()`, `compile()` - Not in allowed builtins
- `import` statement - Blocked by RestrictedPython compiler
- `__class__`, `__mro__`, `__subclasses__`, `__globals__`, `__builtins__` - In BLOCKED_NAMES
- `getattr()`, `setattr()`, `delattr()`, `hasattr()` - In BLOCKED_BUILTINS
- `type()`, `object()`, `super()` - In BLOCKED_BUILTINS
- `vars()`, `dir()`, `input()`, `help()` - In BLOCKED_BUILTINS

**Known Limitations (Documented):**
- Not true process isolation - runs in same Python process
- No memory limits - Python resource module unreliable cross-platform
- Escape vectors may exist - RestrictedPython is defense-in-depth

### Performance Considerations

- Timeout prevents CPU exhaustion
- Output truncation prevents memory exhaustion
- Threading-based execution allows timeout enforcement without signal dependency

### Files Modified During Review

None - no refactoring required.

### Gate Status

Gate: **PASS** → `docs/qa/gates/TEA-BUILTIN-003.1-code-execution-actions.yml`

Previous status was CONCERNS (pre-implementation). Now PASS after verifying:
- All 41 tests pass (20 P0 security tests included)
- Feature disabled by default (`enable_code_execution=False`)
- Documentation complete with security warnings
- All ACs met (except AC4 which was formally deferred)

### Recommended Status

✓ Ready for Done

All acceptance criteria verified. Implementation is complete, tested, and documented.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-06 | 0.2 | Added Dependencies, User Prerequisites, Rollback, Integration/Security Tests | Sarah (PO Agent) |
| 2025-12-06 | 0.3 | **Major revision**: Scoped to RestrictedPython only (Phase 1). Removed JavaScript/shell support. Added Cloud Run 2nd gen constraints. Added hardening measures (dunder blocking). Updated security model to whitelist approach. | Security Review |
| 2025-12-06 | 0.4 | QA Review: Test design (35 scenarios), Gate decision (CONCERNS). | Quinn (Test Architect) |
| 2025-12-07 | 1.0 | Implementation complete: All 41 tests pass (20 P0 security, 21 P1 functional). | James (Dev Agent) |

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Status | Description |
|------|--------|-------------|
| `src/the_edge_agent/actions/code_actions.py` | Created | RestrictedPythonSandbox class and code.execute/code.sandbox actions |
| `src/the_edge_agent/actions/__init__.py` | Modified | Added code_actions import and registration |
| `src/the_edge_agent/yaml_engine.py` | Modified | Added enable_code_execution parameter |
| `tests/test_yaml_engine_code.py` | Created | 41 tests (20 P0 security, 21 P1 functional) |
| `setup.py` | Modified | Added RestrictedPython to extras_require |
| `CLAUDE.md` | Modified | Added Code Execution Actions documentation |
| `docs/YAML_AGENTS.md` | Modified | Added Code Execution Actions section with examples |

### Debug Log References
None - implementation completed without major debugging issues.

### Completion Notes
- All 41 tests pass (20 P0 security tests, 21 P1 functional tests)
- Feature disabled by default (`enable_code_execution=False`)
- RestrictedPython bytecode transformation with custom guards
- Print output capture via custom PrintCollector
- Dict/list/set mutation support via `_guarded_write`
- Timeout enforcement via threading
- Output truncation at max_output_bytes
