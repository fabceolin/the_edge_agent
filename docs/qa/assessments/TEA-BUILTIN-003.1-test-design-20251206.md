# Test Design: Story TEA-BUILTIN-003.1

Date: 2025-12-06
Updated: 2025-12-07
Designer: Quinn (Test Architect)
Story: Code Execution Actions (RestrictedPython Phase 1)

## Risk Assessment

**Overall Risk: CRITICAL**

| Risk Factor | Assessment | Rationale |
|-------------|------------|-----------|
| Security Impact | CRITICAL | Sandbox escape = arbitrary code execution |
| Data Impact | HIGH | Could access filesystem, env vars |
| Availability Impact | MEDIUM | Infinite loops, resource exhaustion |
| Compliance Impact | HIGH | PCI/SOC2 if processing sensitive data |

**Conclusion**: All security tests are P0. No exceptions.

## Test Strategy Overview

- **Total test scenarios**: 40
- **Unit tests**: 15 (38%)
- **Integration tests**: 5 (12%)
- **Security tests**: 20 (50%)
- **Priority distribution**: P0: 23, P1: 17

### Test Pyramid

```
        /\
       /E2E\     (0 - deferred to integration)
      /------\
     /  INT   \  (5 tests)
    /----------\
   /   UNIT     \ (15 tests)
  /--------------\
 /   SECURITY    \ (20 tests - ALL P0)
/------------------\
```

## Test Scenarios by Acceptance Criteria

### AC1: `code.execute` runs Python code in RestrictedPython sandbox

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 003.1-UNIT-001 | Unit | P1 | Execute simple arithmetic expression | Core happy path - sandbox returns correct result |
| 003.1-UNIT-002 | Unit | P1 | Execute code with variable assignment | Verify `result` variable capture works |
| 003.1-UNIT-003 | Unit | P1 | Execute multi-line Python code | Complex code execution |
| 003.1-UNIT-004 | Unit | P1 | Execute code using allowed builtins (sum, len, etc.) | Whitelist verification |
| 003.1-UNIT-005 | Unit | P1 | Execute lambda functions | Functional programming support |
| 003.1-UNIT-006 | Unit | P1 | Execute list comprehensions | Python syntax support |
| 003.1-UNIT-007 | Unit | P1 | Handle syntax errors gracefully | Error reporting |
| 003.1-UNIT-008 | Unit | P1 | Handle runtime exceptions | Exception capture |

### AC2: `code.sandbox` creates persistent sandbox sessions

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 003.1-UNIT-009 | Unit | P1 | Create sandbox session returns valid ID | Session lifecycle |
| 003.1-UNIT-010 | Unit | P1 | Execute in sandbox preserves variables | State persistence |
| 003.1-UNIT-011 | Unit | P1 | Multiple executions build on previous state | Multi-step execution |
| 003.1-UNIT-012 | Unit | P1 | Destroy sandbox clears session | Cleanup verification |
| 003.1-UNIT-013 | Unit | P1 | List sandboxes returns active sessions | Session management |
| 003.1-SEC-012 | Security | P0 | Sessions are isolated from each other | Cross-session leakage prevention |

### AC3: Sandboxed execution prevents filesystem, network, system access

**CRITICAL SECTION - All P0**

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 003.1-SEC-001 | Security | P0 | Block `open()` builtin | File read prevention |
| 003.1-SEC-002 | Security | P0 | Block `import` statement | Module import prevention |
| 003.1-SEC-003 | Security | P0 | Block `exec()` builtin | Dynamic code execution prevention |
| 003.1-SEC-004 | Security | P0 | Block `eval()` builtin | Dynamic evaluation prevention |
| 003.1-SEC-005 | Security | P0 | Block `__class__` access | Object traversal prevention |
| 003.1-SEC-006 | Security | P0 | Block `__mro__` access | MRO traversal prevention |
| 003.1-SEC-007 | Security | P0 | Block `__subclasses__` access | Subclass enumeration prevention |
| 003.1-SEC-008 | Security | P0 | Block `__globals__` access | Global scope access prevention |
| 003.1-SEC-009 | Security | P0 | Block `__builtins__` access | Builtin override prevention |
| 003.1-SEC-010 | Security | P0 | Block `getattr()` builtin | Attribute access prevention |
| 003.1-SEC-011 | Security | P0 | Block `__import__()` function | Dynamic import prevention |
| 003.1-SEC-013 | Security | P0 | Block `compile()` builtin | Code object creation prevention |
| 003.1-SEC-014 | Security | P0 | Block `breakpoint()` builtin | Debugger access prevention |
| 003.1-SEC-015 | Security | P0 | Block attribute traversal attack chain | Combined escape attempt |

### AC5: Resource limits enforced (timeout, output size)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 003.1-UNIT-014 | Unit | P0 | Timeout kills infinite loop | CPU resource protection |
| 003.1-UNIT-015 | Unit | P1 | Output truncated at max_output_bytes | Memory protection |

### AC6: Execution captures stdout, stderr, return values, exceptions

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 003.1-INT-001 | Integration | P1 | Capture stdout from print statements | Output capture |
| 003.1-INT-002 | Integration | P1 | Capture stderr from error output | Error output capture |
| 003.1-INT-003 | Integration | P1 | Return value populated from `result` variable | Result extraction |
| 003.1-INT-004 | Integration | P1 | Exception details captured in error field | Error handling |
| 003.1-INT-005 | Integration | P1 | execution_time_ms tracked accurately | Performance monitoring |

### AC7 & AC8: Action registration and dual namespace access

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 003.1-UNIT-016 | Unit | P1 | `code.execute` registered in actions | Registration verification |
| 003.1-UNIT-017 | Unit | P1 | `actions.code_execute` registered in actions | Dual namespace |
| 003.1-UNIT-018 | Unit | P1 | `code.sandbox` registered in actions | Registration verification |
| 003.1-UNIT-019 | Unit | P1 | `actions.code_sandbox` registered in actions | Dual namespace |

### AC9 & AC10: Documentation (Non-automated)

These are verification tasks, not automated tests:
- [ ] CLAUDE.md updated with code execution actions
- [ ] YAML_AGENTS.md examples added
- [ ] Security model documented

## Security Test Details

### Escape Vector Tests (ALL P0)

Each security test must verify that attempting the blocked operation:
1. Does NOT succeed
2. Returns `success: false`
3. Contains appropriate error message
4. Does NOT leak stack trace information

#### Test: 003.1-SEC-015 - Attribute Traversal Attack Chain

```python
# Known RestrictedPython escape attempt
malicious_code = """
().__class__.__bases__[0].__subclasses__()[40]('/etc/passwd').read()
"""
# Must fail at first dunder access attempt
```

### AC3 (Extended): Additional Escape Vector Prevention

**CRITICAL SECTION - All P0**

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 003.1-SEC-016 | Security | P0 | Block `type()` builtin | Metaclass construction prevention |
| 003.1-SEC-017 | Security | P0 | Block `vars()` and `dir()` builtins | Introspection/information disclosure prevention |
| 003.1-SEC-018 | Security | P0 | Block `input()` builtin | STDIN hijacking prevention |
| 003.1-SEC-019 | Security | P0 | Block `help()` builtin | Interactive shell access prevention |
| 003.1-SEC-020 | Security | P0 | Verify `os` module not accessible via any path | System access prevention |

## Risk Coverage Matrix

| Risk | Test IDs | Coverage |
|------|----------|----------|
| Sandbox Escape | SEC-001 to SEC-020 | Full |
| Metaclass/Type Attacks | SEC-016 | Full |
| Information Disclosure | SEC-017 | Full |
| STDIN/Interactive Access | SEC-018, SEC-019 | Full |
| Module Import Bypass | SEC-020 | Full |
| Resource Exhaustion | UNIT-014, UNIT-015 | Full |
| Session Leakage | SEC-012 | Full |
| Output Injection | INT-001, INT-002 | Partial |
| Error Information Leak | All SEC tests | Full |

## Recommended Execution Order

### Phase 1: Security Gate (Must Pass Before Any Deployment)
1. 003.1-SEC-001 through 003.1-SEC-020 (all 20 security tests)
2. 003.1-UNIT-014 (timeout test)

### Phase 2: Core Functionality
1. 003.1-UNIT-001 through 003.1-UNIT-008 (code.execute tests)
2. 003.1-INT-001 through 003.1-INT-005 (output capture)

### Phase 3: Session Management
1. 003.1-UNIT-009 through 003.1-UNIT-013 (code.sandbox tests)
2. 003.1-SEC-012 (session isolation)

### Phase 4: Registration
1. 003.1-UNIT-016 through 003.1-UNIT-019 (namespace registration)

## Test Environment Requirements

```yaml
test_environment:
  python_version: ">=3.9"
  dependencies:
    - RestrictedPython
    - pytest
  isolation: "Each test in fresh sandbox instance"
  timeout_per_test: 10s  # Prevent hung tests
  special_requirements:
    - Unix for signal.SIGALRM tests
    - Windows fallback tests for threading timeout
```

## Coverage Gaps Identified

| Gap | Severity | Recommendation |
|-----|----------|----------------|
| No memory limit tests | MEDIUM | Document as known limitation (cross-platform unreliable) |
| No concurrent execution tests | LOW | Add if multithreading used |
| No Windows-specific tests | LOW | Add threading timeout tests |
| ~~SEC-016 to SEC-020 missing~~ | ~~HIGH~~ | ✓ RESOLVED - Added in 2025-12-07 update |

## Test Implementation Notes

### For Security Tests

```python
class TestCodeExecutionSecurity(unittest.TestCase):
    """All tests MUST:
    1. Verify operation fails
    2. Verify no side effects occurred
    3. Verify error message is safe (no stack traces)
    """

    def test_blocks_open_builtin(self):
        result = sandbox.execute("open('/etc/passwd')")
        self.assertFalse(result["success"])
        self.assertIn("not allowed", result["error"].lower())
        # Verify file was not accessed
```

### For Timeout Tests

```python
def test_timeout_prevents_infinite_loop(self):
    """CRITICAL: Must terminate within timeout + buffer"""
    start = time.time()
    result = sandbox.execute("while True: pass", timeout=1)
    elapsed = time.time() - start
    self.assertFalse(result["success"])
    self.assertLess(elapsed, 3)  # Timeout + 2s buffer
```

### For Extended Escape Vector Tests (SEC-016 to SEC-020)

```python
def test_blocks_type_builtin(self):
    """SEC-016: Prevent metaclass construction attacks"""
    result = sandbox.execute("type('Exploit', (), {})")
    self.assertFalse(result["success"])
    self.assertIn("not allowed", result["error"].lower())

def test_blocks_vars_and_dir_builtins(self):
    """SEC-017: Prevent introspection/information disclosure"""
    for func in ["vars()", "dir()"]:
        result = sandbox.execute(func)
        self.assertFalse(result["success"])

def test_blocks_input_builtin(self):
    """SEC-018: Prevent STDIN hijacking"""
    result = sandbox.execute("input('prompt')")
    self.assertFalse(result["success"])

def test_blocks_help_builtin(self):
    """SEC-019: Prevent interactive shell access"""
    result = sandbox.execute("help()")
    self.assertFalse(result["success"])

def test_blocks_os_module_access(self):
    """SEC-020: Verify os module not accessible via any path"""
    # Direct import blocked by RestrictedPython
    result = sandbox.execute("import os")
    self.assertFalse(result["success"])
    # Indirect access via builtins also blocked
    result = sandbox.execute("__builtins__['__import__']('os')")
    self.assertFalse(result["success"])
```

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels appropriate (unit for logic, integration for capture)
- [x] No duplicate coverage across levels
- [x] Priorities align with security risk (all SEC = P0)
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Security tests are exhaustive for known vectors

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-BUILTIN-003.1
  date: 2025-12-06
  updated: 2025-12-07
  scenarios_total: 40
  by_level:
    unit: 15
    integration: 5
    security: 20
  by_priority:
    p0: 23
    p1: 17
  security_gate: MANDATORY
  coverage_gaps:
    - memory_limits: "Documented as known limitation"
  recommendations:
    - "All 20 security tests (SEC-001 to SEC-020) must pass before merge"
    - "Run security tests in CI before any merge"
    - "Consider fuzzing with hypothesis for escape vectors"
```

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-003.1-test-design-20251206.md
P0 tests identified: 23
Security tests: 20 (all P0)
Functional tests: 17 (P1)
Total scenarios: 40
```
