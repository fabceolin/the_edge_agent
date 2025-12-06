# Story TEA-BUILTIN-003.1: Code Execution Actions

## Status

Draft

## Story

**As a** YAML agent developer,
**I want** built-in code execution actions (execute, sandbox),
**so that** I can build agents that safely run generated code without writing Python infrastructure.

## Acceptance Criteria

1. `code.execute` action runs code in isolated environment with configurable language
2. `code.sandbox` action creates persistent sandbox sessions for multi-step execution
3. Sandboxed execution prevents filesystem, network, and system access by default
4. Execution supports Python, JavaScript, and shell scripts
5. Resource limits enforced (CPU time, memory, output size)
6. Execution captures stdout, stderr, return values, and exceptions
7. All actions follow existing `_setup_builtin_actions()` pattern
8. Actions are accessible via both `code.*` and `actions.code_*` namespaces
9. Comprehensive unit tests cover all code execution operations
10. Documentation updated in CLAUDE.md and docs/YAML_AGENTS.md

## Dependencies

**Blocked By**: None (can start independently)

**Blocks**: None

**Internal Dependencies**:
- JavaScript execution requires optional PyMiniRacer
- E2B integration requires optional e2b library

## User Prerequisites

- [ ] **Required**: `pip install RestrictedPython` (for Python sandbox)
- [ ] **Optional**: `pip install py_mini_racer` (for JavaScript execution)
- [ ] **Optional**: Obtain `E2B_API_KEY` from https://e2b.dev (for cloud execution)
- [ ] **Security Warning**: Review security implications before enabling

## Tasks / Subtasks

- [ ] Task 1: Design sandbox abstraction (AC: 2, 3, 5)
  - [ ] Create `CodeSandbox` protocol/interface
  - [ ] Implement `RestrictedPythonSandbox` as default
  - [ ] Design sandbox configuration:
    ```python
    {
        "language": str,
        "timeout_seconds": float,
        "max_memory_mb": int,
        "max_output_bytes": int,
        "allow_network": bool,
        "allow_filesystem": bool,
        "allowed_modules": List[str]
    }
    ```
  - [ ] Implement resource limit enforcement

- [ ] Task 2: Implement `code.execute` action (AC: 1, 4, 6, 7, 8)
  - [ ] Define function signature: `code_execute(state, code, language="python", timeout=30, **kwargs)`
  - [ ] Support languages: python, javascript, bash
  - [ ] Execute in isolated environment
  - [ ] Capture stdout, stderr, return value
  - [ ] Handle timeouts gracefully
  - [ ] Return:
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
  - [ ] Register in actions dict with namespaces

- [ ] Task 3: Implement `code.sandbox` action (AC: 2, 7, 8)
  - [ ] Define function signature: `code_sandbox(state, action, sandbox_id=None, code=None, **kwargs)`
  - [ ] Support actions: "create", "execute", "destroy", "list"
  - [ ] Maintain sandbox state across multiple calls
  - [ ] Allow variables to persist within sandbox session
  - [ ] Return appropriate result based on action:
    - create: `{"sandbox_id": str, "language": str}`
    - execute: `{"stdout": str, "stderr": str, ...}`
    - destroy: `{"destroyed": bool, "sandbox_id": str}`
    - list: `{"sandboxes": List[dict]}`
  - [ ] Register in actions dict with namespaces

- [ ] Task 4: Python sandbox implementation (AC: 3, 4, 5)
  - [ ] Use `RestrictedPython` for safe execution
  - [ ] Whitelist safe builtins (math, string ops, collections)
  - [ ] Block dangerous operations (exec, eval, import, open)
  - [ ] Implement timeout using signals or threading
  - [ ] Implement memory limit using resource module
  - [ ] Capture output using io.StringIO

- [ ] Task 5: JavaScript sandbox implementation (AC: 4)
  - [ ] Use `PyMiniRacer` or subprocess with Node.js
  - [ ] Apply same security restrictions as Python
  - [ ] Handle async JavaScript code
  - [ ] Make JavaScript support optional dependency

- [ ] Task 6: Shell execution (AC: 4)
  - [ ] Use subprocess with restricted shell
  - [ ] Block dangerous commands (rm, sudo, etc.)
  - [ ] Limit to read-only filesystem by default
  - [ ] Apply timeout and output limits

- [ ] Task 7: Optional E2B integration (AC: 1, 2)
  - [ ] Implement `E2BSandbox` for cloud-hosted execution
  - [ ] Make E2B an optional dependency
  - [ ] Document E2B setup and API key configuration

- [ ] Task 8: Write tests (AC: 9)
  - [ ] Test code.execute with safe Python code
  - [ ] Test code.execute blocks dangerous operations
  - [ ] Test timeout enforcement
  - [ ] Test output capture (stdout, stderr)
  - [ ] Test code.sandbox session persistence
  - [ ] Test resource limits
  - [ ] Test each supported language

- [ ] Task 9: Update documentation (AC: 10)
  - [ ] Add code execution actions to CLAUDE.md
  - [ ] Add examples in docs/YAML_AGENTS.md
  - [ ] Document security model and limitations
  - [ ] Create example YAML showing code agent

## Dev Notes

### Integration Points
- **File**: `src/the_edge_agent/yaml_engine.py`
- **Method**: `_setup_builtin_actions()` (lines 623-786)

### Dependencies
- **Required**: `RestrictedPython` (for Python sandbox)
- **Optional**: `py_mini_racer` (for JavaScript)
- **Optional**: `e2b` (for cloud execution)

### Security Model
```
DEFAULT RESTRICTIONS:
- No file system access (open, Path, os.*)
- No network access (socket, requests, urllib)
- No process spawning (subprocess, os.system)
- No code generation (exec, eval, compile)
- No module imports (import statement blocked)

ALLOWED BY DEFAULT:
- Math operations
- String manipulation
- List/dict operations
- Basic builtins (len, range, enumerate, zip, map, filter)
```

### Configuration in YAML
```yaml
settings:
  code:
    default_timeout: 30
    max_memory_mb: 256
    max_output_bytes: 65536
    sandbox_mode: restricted  # or permissive, e2b

secrets:
  E2B_API_KEY: ${E2B_API_KEY}  # Optional for cloud execution
```

### Key Constraints
- Security is paramount - default to maximum restrictions
- RestrictedPython is the recommended approach for Python
- JavaScript execution requires optional dependency
- Shell execution is inherently risky - document clearly

### Dangerous Operations Blocklist
```python
BLOCKED_BUILTINS = [
    'exec', 'eval', 'compile', 'open', 'input',
    '__import__', 'globals', 'locals', 'vars',
    'getattr', 'setattr', 'delattr', 'hasattr',
]

BLOCKED_MODULES = [
    'os', 'sys', 'subprocess', 'socket', 'shutil',
    'importlib', 'builtins', 'ctypes', 'pickle',
]
```

## Testing

**Test File Location**: `tests/test_yaml_engine.py` (add new test class)

**Priority Levels**:
- **P0**: ALL security tests - MUST pass before any deployment
- **P1**: Core functionality - Required for basic usage
- **P2**: Optional features - Can skip if dependencies unavailable

**Testing Standards**:
- Test security restrictions thoroughly
- Verify dangerous operations are blocked
- Test resource limits work correctly

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
    def test_code_execute_shell_simple(self): ...  # (P1) - AC4: Basic shell script execution
    def test_dual_namespace_access(self): ...  # (P1) - AC8: Verify code.* and actions.code_* work
    def test_concurrent_execution_safety(self): ...  # (P1) - Multiple concurrent executions
    def test_sandbox_cleanup_after_crash(self): ...  # (P1) - Resources cleaned on abnormal exit
    def test_invalid_language_raises_error(self): ...  # (P1) - Error for unsupported language
    def test_max_output_bytes_truncation(self): ...  # (P1) - Output size limits enforced

    # P2 - Optional features
    @unittest.skipUnless(has_minirace, "PyMiniRacer not installed")
    def test_code_execute_javascript(self): ...  # (P2)
```

**Integration Test Cases**:
```python
class TestCodeExecutionActionsIntegration(unittest.TestCase):
    def test_code_execute_in_yaml_workflow(self): ...  # (P1)
    def test_code_sandbox_with_checkpoint(self): ...  # (P1)
    def test_code_execute_with_llm_generated_code(self): ...  # (P1)
```

**Security Test Cases** (ALL P0 - Critical):
```python
class TestCodeExecutionSecurity(unittest.TestCase):
    # P0 - ALL security tests must pass
    def test_code_execute_blocks_file_access(self): ...  # (P0)
    def test_code_execute_blocks_network(self): ...  # (P0)
    def test_code_execute_blocks_imports(self): ...  # (P0)
    def test_code_execute_blocks_exec(self): ...  # (P0)
    def test_blocks_os_access(self): ...  # (P0)
    def test_blocks_network_socket(self): ...  # (P0)
    def test_blocks_file_write(self): ...  # (P0)
    def test_blocks_subprocess(self): ...  # (P0)
    def test_blocks_eval_exec(self): ...  # (P0)
    def test_resource_limit_memory(self): ...  # (P0)
    def test_resource_limit_cpu(self): ...  # (P0)
    def test_code_execute_shell_blocks_rm(self): ...  # (P0) - Block dangerous shell commands
    def test_code_execute_shell_blocks_sudo(self): ...  # (P0) - Block privilege escalation
    def test_code_execute_shell_readonly_filesystem(self): ...  # (P0) - Verify filesystem restrictions
    def test_sandbox_isolation_between_sessions(self): ...  # (P0) - Sessions don't share state
```

**Test Summary**: 31 tests (14 unit + 3 integration + 15 security) | P0: 15 | P1: 15 | P2: 1

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] All tasks completed
- [ ] Tests pass (existing and new)
- [ ] Security restrictions verified with adversarial tests
- [ ] Documentation updated with security warnings
- [ ] Code follows existing patterns in yaml_engine.py

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
   - No persistent state to clean up
   - E2B cloud sessions auto-terminate

3. **Verification**:
   - Run: `pytest tests/test_yaml_engine.py`
   - Verify no security vulnerabilities introduced

4. **Gradual Rollout** (STRONGLY Recommended):
   - Feature flag: `YAMLEngine(enable_code_execution=False)` (default OFF)
   - Security review required before enabling
   - Start with restricted Python only
   - Add JavaScript after security validation
   - Never enable shell by default

5. **Security Incident Response**:
   - If sandbox escape detected, disable immediately
   - Audit all code execution logs
   - Notify security team

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-06 | 0.2 | Added Dependencies, User Prerequisites, Rollback, Integration/Security Tests | Sarah (PO Agent) |
