# Story: TEA-PY-004 - Implement Prolog Scripting Support in Python TEA

## Status

**Approved** (Test Design Complete)

---

## Story

**As a** workflow developer building neurosymbolic AI applications,
**I want** Prolog scripting support in the Python TEA implementation,
**So that** I can combine neural network outputs with symbolic logic reasoning, constraint solving, and rule-based inference in my YAML agents.

---

## Story Context

**Parent Epic:** [TEA-PROLOG-001 - Prolog Integration for Neurosymbolic AI](TEA-PROLOG-001-prolog-integration-epic.md)

**Existing System Integration:**

- **Integrates with:** `YAMLEngine` class in `python/src/the_edge_agent/yaml_engine.py`
- **Technology:** Python 3.11+, `pyswip` library (SWI-Prolog bindings)
- **Follows pattern:** Existing `LuaRuntime` in `lua_runtime.py`
- **Touch points:** `_create_run_function()`, `_evaluate_condition()`, template processing

**Reference Implementation:**

The Lua implementation in `python/src/the_edge_agent/lua_runtime.py` provides the pattern:
- `LuaRuntime` class with `execute()`, `eval_condition()`, `execute_node_code()`
- Sandbox: removes `os`, `io`, `loadfile`, `dofile`, `debug` globals
- Timeout protection via debug hooks
- JSON ↔ Lua bidirectional conversion

---

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: GIVEN a node with `run: { type: prolog, code: "..." }`, WHEN the node executes, THEN the Prolog code runs with `state/2` predicate available for state access

2. **AC-2**: GIVEN a node with `run:` containing Prolog code (auto-detected via `%` comment or heuristics), WHEN the node executes, THEN the code executes in Prolog runtime

3. **AC-3**: GIVEN `language: prolog` specified in node config or globally in YAML, WHEN inline code executes, THEN Prolog is used instead of Python

4. **AC-4**: GIVEN Prolog code that exceeds configured timeout, WHEN execution runs, THEN `TimeoutError` is raised with "Prolog execution timeout" message

5. **AC-5**: GIVEN sandboxed mode (default), WHEN Prolog code attempts file/network/shell access, THEN operation fails safely with security error

6. **AC-6**: GIVEN Prolog query with multiple solutions, WHEN executed in node context, THEN first solution is returned (deterministic mode for workflow consistency)

7. **AC-7**: GIVEN CLP(FD) constraints in Prolog code, WHEN solved, THEN solutions are extracted and returned as state updates

### Integration Requirements

8. **AC-8**: Existing Python `run:` blocks continue to work unchanged (backward compatibility)

9. **AC-9**: Existing Lua `run:` blocks continue to work unchanged (backward compatibility)

10. **AC-10**: Existing Jinja2 condition evaluation (`when:` expressions) continues unchanged

11. **AC-11**: JSON ↔ Prolog term conversion handles all JSON types: null, boolean, number, string, array, object

12. **AC-12**: Prolog atoms map to Python strings, lists to lists, dicts to Prolog dicts/terms

### Quality Requirements

13. **AC-13**: Unit tests cover Prolog execution, timeout, sandbox, and JSON conversion

14. **AC-14**: Integration tests verify YAML agents with Prolog nodes execute correctly

15. **AC-15**: Documentation updated in YAML_REFERENCE.md with Prolog scripting section

16. **AC-16**: Clear error message with install instructions if `pyswip` not installed

---

## Technical Notes

### Implementation Approach

#### 1. Choose Python SWI-Prolog Binding Library

| Library | Pros | Cons | Decision |
|---------|------|------|----------|
| **pyswip** | Active, SWI-Prolog native | Requires SWI-Prolog installed | **Selected** |
| **pyDatalog** | Pure Python | Not full Prolog, limited | Not suitable |
| **kanren** | Pure Python, miniKanren | Different syntax | Not suitable |

**Selected: `pyswip`** - provides direct SWI-Prolog bindings, active maintenance, full Prolog support.

#### 2. PrologRuntime Class (Python)

```python
# python/src/the_edge_agent/prolog_runtime.py

from typing import Any, Dict, List, Optional, Union
import threading
import json

# Lazy import with availability flag
PYSWIP_AVAILABLE = False
_pyswip = None

try:
    from pyswip import Prolog, Functor, Variable, Query
    import pyswip
    _pyswip = pyswip
    PYSWIP_AVAILABLE = True
except ImportError:
    pass


class PrologRuntimeError(Exception):
    """Exception raised for Prolog syntax or runtime errors."""
    pass


class PrologTimeoutError(Exception):
    """Exception raised when Prolog execution timeout is exceeded."""
    pass


def _ensure_pyswip_installed():
    """Raise ImportError with installation instructions if pyswip is not installed."""
    if not PYSWIP_AVAILABLE:
        raise ImportError(
            "Prolog runtime requires the 'pyswip' package and SWI-Prolog.\n"
            "Install pyswip: pip install 'the_edge_agent[prolog]'\n"
            "Or directly: pip install pyswip\n\n"
            "Install SWI-Prolog:\n"
            "  Ubuntu/Debian: sudo apt install swi-prolog\n"
            "  macOS: brew install swi-prolog\n"
            "  Windows: Download from https://www.swi-prolog.org/download/stable"
        )


class PrologRuntime:
    """
    Prolog runtime for The Edge Agent.

    Provides a sandboxed SWI-Prolog environment with timeout protection
    for neurosymbolic AI workflows.

    Args:
        timeout: Maximum execution time in seconds (default: 30.0)
        sandbox: Enable sandboxed execution (default: True)

    Example:
        >>> runtime = PrologRuntime()
        >>> result = runtime.execute_query(
        ...     "state(value, V), V2 is V + 1, return(result, V2)",
        ...     {"value": 41}
        ... )
        >>> print(result)  # {"result": 42}
    """

    def __init__(self, timeout: float = 30.0, sandbox: bool = True):
        _ensure_pyswip_installed()

        self.timeout = timeout
        self.sandbox = sandbox
        self._prolog = Prolog()
        self._lock = threading.Lock()

        if sandbox:
            self._apply_sandbox()

        self._setup_state_predicates()

    def _apply_sandbox(self) -> None:
        """Enable SWI-Prolog sandbox mode."""
        # Load sandbox library
        self._prolog.assertz("sandbox_enabled")
        # Consult sandbox restrictions
        self._prolog.consult("library(sandbox)")
        # Add custom restrictions for file/network/shell
        restrictions = [
            ":- disallow(open(_,_,_))",
            ":- disallow(read(_))",
            ":- disallow(write(_))",
            ":- disallow(shell(_))",
            ":- disallow(shell(_,_))",
            ":- disallow(process_create(_,_,_))",
        ]
        for r in restrictions:
            try:
                self._prolog.assertz(r)
            except Exception:
                pass  # Some restrictions may not apply

    def _setup_state_predicates(self) -> None:
        """Set up state/2 and return/2 predicates for state access."""
        # Dynamic predicates for state access
        self._prolog.assertz(":- dynamic(state/2)")
        self._prolog.assertz(":- dynamic(return_value/2)")

    def _python_to_prolog(self, value: Any) -> str:
        """
        Convert Python value to Prolog term string.

        Type mapping:
        - None -> null (atom)
        - bool -> true/false (atoms)
        - int/float -> number
        - str -> atom (quoted if needed)
        - list -> Prolog list
        - dict -> Prolog dict or list of Key-Value pairs
        """
        if value is None:
            return "null"
        elif isinstance(value, bool):
            return "true" if value else "false"
        elif isinstance(value, (int, float)):
            return str(value)
        elif isinstance(value, str):
            # Escape single quotes and wrap in quotes
            escaped = value.replace("\\", "\\\\").replace("'", "\\'")
            return f"'{escaped}'"
        elif isinstance(value, list):
            items = [self._python_to_prolog(item) for item in value]
            return f"[{', '.join(items)}]"
        elif isinstance(value, dict):
            # Convert to Prolog dict syntax: key1: value1, key2: value2
            pairs = [f"{k}: {self._python_to_prolog(v)}" for k, v in value.items()]
            return f"_{{{', '.join(pairs)}}}"
        else:
            return f"'{str(value)}'"

    def _prolog_to_python(self, value: Any) -> Any:
        """
        Convert Prolog term to Python value.

        Handles pyswip types: Atom, Functor, Variable, lists, numbers.
        """
        if value is None:
            return None

        # Handle pyswip specific types
        type_name = type(value).__name__

        if type_name == 'Atom':
            atom_str = str(value)
            if atom_str == 'null':
                return None
            elif atom_str == 'true':
                return True
            elif atom_str == 'false':
                return False
            return atom_str

        elif isinstance(value, (int, float)):
            return value

        elif isinstance(value, str):
            if value == 'null':
                return None
            elif value == 'true':
                return True
            elif value == 'false':
                return False
            return value

        elif isinstance(value, list):
            return [self._prolog_to_python(item) for item in value]

        elif isinstance(value, dict):
            return {str(k): self._prolog_to_python(v) for k, v in value.items()}

        elif type_name == 'Functor':
            # Handle Prolog functors - convert to dict or appropriate structure
            return str(value)

        else:
            return str(value)

    def _set_state(self, state: Dict[str, Any]) -> None:
        """Assert state facts for Prolog access via state/2."""
        # Retract all existing state facts
        try:
            list(self._prolog.query("retractall(state(_,_))"))
        except Exception:
            pass

        # Assert new state facts
        for key, value in state.items():
            prolog_value = self._python_to_prolog(value)
            self._prolog.assertz(f"state('{key}', {prolog_value})")

    def _get_returns(self) -> Dict[str, Any]:
        """Extract return values from return_value/2 facts."""
        results = {}
        try:
            for sol in self._prolog.query("return_value(Key, Value)"):
                key = self._prolog_to_python(sol['Key'])
                value = self._prolog_to_python(sol['Value'])
                results[str(key)] = value
        except Exception:
            pass
        return results

    def _clear_returns(self) -> None:
        """Clear all return_value/2 facts."""
        try:
            list(self._prolog.query("retractall(return_value(_,_))"))
        except Exception:
            pass

    def execute_query(
        self,
        query: str,
        state: Dict[str, Any],
        first_only: bool = True
    ) -> Dict[str, Any]:
        """
        Execute a Prolog query with state access.

        The state is available via state/2 predicate.
        Use return/2 to set return values.

        Args:
            query: Prolog query to execute
            state: State dictionary accessible via state/2
            first_only: Return only first solution (default: True)

        Returns:
            Dictionary of return values

        Raises:
            PrologRuntimeError: If query has syntax or runtime errors
            PrologTimeoutError: If execution exceeds timeout
        """
        with self._lock:
            self._set_state(state)
            self._clear_returns()

            # Add return/2 predicate that asserts return_value/2
            self._prolog.assertz("return(Key, Value) :- assertz(return_value(Key, Value))")

            try:
                # Wrap query with timeout
                timed_query = f"call_with_time_limit({self.timeout}, ({query}))"

                solutions = list(self._prolog.query(timed_query))

                if not solutions:
                    # Query failed - no solutions
                    return {}

                # Get return values
                return self._get_returns()

            except Exception as e:
                error_msg = str(e).lower()
                if 'time_limit_exceeded' in error_msg or 'timeout' in error_msg:
                    raise PrologTimeoutError("Prolog execution timeout") from e
                raise PrologRuntimeError(str(e)) from e

    def execute_node_code(self, code: str, state: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute inline Prolog code for a node.

        The code should use state/2 to access state and return/2 to set return values.

        Args:
            code: Prolog code to execute
            state: Current node state

        Returns:
            State updates as dictionary

        Raises:
            PrologRuntimeError: If code has syntax or runtime errors
            PrologTimeoutError: If execution exceeds timeout
        """
        # Clean up code - remove leading/trailing whitespace
        code = code.strip()

        # If code contains rule definitions (:-), consult them first
        if ':-' in code and not code.startswith(':-'):
            # Split into rules and query
            lines = code.split('\n')
            rules = []
            query_parts = []

            for line in lines:
                line = line.strip()
                if not line:
                    continue
                if ':-' in line and not line.startswith(':-'):
                    rules.append(line)
                else:
                    query_parts.append(line)

            # Assert rules
            for rule in rules:
                try:
                    self._prolog.assertz(rule.rstrip('.'))
                except Exception as e:
                    raise PrologRuntimeError(f"Rule assertion failed: {e}")

            # Execute query parts
            if query_parts:
                query = ', '.join(q.rstrip('.,') for q in query_parts)
                return self.execute_query(query, state)
            return {}

        # Simple query execution
        query = code.rstrip('.')
        return self.execute_query(query, state)

    def eval_condition(self, expression: str, state: Dict[str, Any]) -> Optional[str]:
        """
        Evaluate a Prolog condition expression.

        The expression should succeed/fail or return a string via binding.

        Args:
            expression: Prolog expression to evaluate
            state: State dictionary accessible via state/2

        Returns:
            String result for routing, or None if query fails

        Raises:
            PrologRuntimeError: If expression has syntax or runtime errors
            PrologTimeoutError: If evaluation exceeds timeout
        """
        with self._lock:
            self._set_state(state)

            try:
                timed_query = f"call_with_time_limit({self.timeout}, ({expression}))"
                solutions = list(self._prolog.query(timed_query))

                if not solutions:
                    return None

                # If query succeeded, return "true"
                # If it bound a Result variable, return that
                first = solutions[0]
                if 'Result' in first:
                    return str(self._prolog_to_python(first['Result']))
                return "true"

            except Exception as e:
                error_msg = str(e).lower()
                if 'time_limit_exceeded' in error_msg:
                    raise PrologTimeoutError("Prolog execution timeout") from e
                if 'fail' in error_msg:
                    return None
                raise PrologRuntimeError(str(e)) from e

    def consult_file(self, path: str) -> None:
        """
        Consult a Prolog file to load rules.

        Args:
            path: Path to .pl file

        Raises:
            PrologRuntimeError: If file cannot be loaded
        """
        if self.sandbox:
            raise PrologRuntimeError("Cannot consult files in sandbox mode")

        try:
            self._prolog.consult(path)
        except Exception as e:
            raise PrologRuntimeError(f"Failed to consult {path}: {e}") from e

    def __repr__(self) -> str:
        return f"PrologRuntime(timeout={self.timeout}, sandbox={self.sandbox})"


# Convenience function to detect Prolog code
def detect_prolog_code(code: str) -> bool:
    """
    Detect if code block is Prolog.

    Detection rules:
    1. Explicit marker: code starts with '% prolog'
    2. Heuristic: contains Prolog-specific syntax

    Args:
        code: Code string to check

    Returns:
        True if code appears to be Prolog
    """
    import re

    stripped = code.strip()

    # Explicit marker
    if stripped.startswith('% prolog') or stripped.startswith('%prolog'):
        return True

    # Heuristic patterns unique to Prolog
    prolog_patterns = [
        r':-',                    # Rule operator
        r'\?-',                   # Query operator
        r'state\s*\(',            # state/2 predicate
        r'return\s*\(',           # return/2 predicate
        r'assertz?\s*\(',         # assert predicates
        r'retract\s*\(',          # retract predicate
        r'findall\s*\(',          # findall predicate
        r'forall\s*\(',           # forall predicate
        r'#=|#<|#>|#\\=',         # CLP(FD) operators
        r'\bin\b.*\.\.',          # CLP(FD) domain
        r'label\s*\(',            # CLP(FD) labeling
    ]

    return any(re.search(pattern, code) for pattern in prolog_patterns)
```

#### 3. YAML Syntax for Prolog

```yaml
# Option 1: Explicit type
nodes:
  - name: reason
    run:
      type: prolog
      code: |
        state(input, Input),
        process(Input, Output),
        return(result, Output).

# Option 2: Global language setting
language: prolog

nodes:
  - name: reason
    run: |
      state(input, Input),
      process(Input, Output),
      return(result, Output).

# Option 3: Node-level language override
nodes:
  - name: prolog_node
    language: prolog
    run: |
      state(value, V),
      V2 is V * 2,
      return(doubled, V2).

  - name: python_node
    language: python
    run: |
      return {"processed": True}
```

#### 4. Integration into YAMLEngine

```python
# In yaml_engine.py

def _create_run_function(self, node_config: Dict[str, Any]) -> Callable:
    """Create run function from node configuration."""

    # Determine language
    language = node_config.get('language', self.config.get('language', 'python'))

    # Option 1: Explicit run type
    if isinstance(node_config.get('run'), dict):
        run_config = node_config['run']
        if run_config.get('type') == 'prolog':
            return self._create_prolog_function(run_config['code'])
        if run_config.get('type') == 'lua':
            return self._create_lua_function(run_config['code'])
        # ... existing logic

    # Option 2: Inline code with language detection
    if 'run' in node_config and isinstance(node_config['run'], str):
        code = node_config['run']
        if language == 'prolog' or (language == 'auto' and detect_prolog_code(code)):
            return self._create_prolog_function(code)
        if language == 'lua' or (language == 'auto' and detect_lua_code(code)):
            return self._create_lua_function(code)
        # ... existing Python exec logic

    # ... rest of existing logic

def _create_prolog_function(self, code: str) -> Callable:
    """Create a function that executes Prolog code."""
    if self._prolog_runtime is None:
        self._prolog_runtime = PrologRuntime(
            timeout=self.config.get('prolog_timeout', 30.0),
            sandbox=self.config.get('prolog_sandbox', True)
        )

    def run_prolog(state: Dict[str, Any], **kwargs) -> Dict[str, Any]:
        return self._prolog_runtime.execute_node_code(code, state)

    return run_prolog
```

### Prolog State Interface

The `state/2` predicate provides read access to the workflow state:

```prolog
% Access state values
state(key, Value).        % Unify Value with state["key"]
state(nested, Dict),      % Access nested structures
get_dict(field, Dict, V). % Extract field from dict

% Return values to state
return(key, Value).       % Set state["key"] = Value

% Example usage
process_state :-
    state(input, Input),
    state(config, Config),
    get_dict(threshold, Config, T),
    Input > T,
    return(result, high).
```

### Error Types

| Error | When Raised |
|-------|-------------|
| `PrologRuntimeError` | Prolog syntax or runtime errors |
| `PrologTimeoutError` | Execution timeout exceeded |
| `ImportError` | pyswip not installed (with install instructions) |

### Key Constraints

- **Thread Safety**: PrologRuntime uses threading lock for concurrent access
- **Timeout**: Uses SWI-Prolog's built-in `call_with_time_limit/2`
- **Sandbox**: Uses SWI-Prolog's `library(sandbox)` for security
- **First Solution**: By default returns first solution only (deterministic mode)
- **System Dependency**: Requires SWI-Prolog installed on system

### Parallel Execution Isolation

**SWI-Prolog uses thread-local predicates for parallel branch isolation:**

Unlike Lua (which creates fresh VM instances per parallel branch), SWI-Prolog uses **thread-local facts** to isolate state between parallel branches:

```prolog
:- thread_local state/2.
:- thread_local return_value/2.
```

This means:
- Each parallel branch has its **own copy** of `state/2` facts
- `return_value/2` facts are private to each branch
- Shared rules (defined in consulted files) remain visible to all branches
- No engine creation overhead per branch

**Why not separate engines?**
- SWI-Prolog engine is heavier than LuaJIT (~50-100ms vs ~1-5ms to create)
- Thread-local predicates are the idiomatic SWI-Prolog solution
- Same behavior guaranteed in both Python (pyswip) and Rust (swipl-rs)

### Known Limitations

**Document in code:**
- `consult/1` disabled in sandbox mode (cannot load external files)
- CLP constraints require explicit `:- use_module(library(clpfd))` in code
- Complex Prolog terms may require explicit conversion helpers
- Tabling requires explicit `:- table predicate/arity` declarations
- Thread-local predicates are automatically cleaned up when thread ends

---

## Tasks / Subtasks

- [ ] **Task 1: Add `pyswip` dependency** (AC: 1, 2, 16)
  - [ ] Add `pyswip` to `requirements.txt` and `pyproject.toml` as optional dependency
  - [ ] Create optional dependency group `[prolog]` for pip install
  - [ ] Test import error handling with clear instructions

- [ ] **Task 2: Implement PrologRuntime class** (AC: 1-7, 11, 12)
  - [ ] Create `python/src/the_edge_agent/prolog_runtime.py`
  - [ ] Implement `__init__()` with timeout and sandbox options
  - [ ] Implement `_apply_sandbox()` using SWI-Prolog sandbox library
  - [ ] Implement `_python_to_prolog()` for Python → Prolog conversion
  - [ ] Implement `_prolog_to_python()` for Prolog → Python conversion
  - [ ] Implement `_set_state()` to assert state/2 facts
  - [ ] Implement `execute_query()` with timeout protection
  - [ ] Implement `execute_node_code()` for node inline code
  - [ ] Implement `eval_condition()` for conditional edges
  - [ ] Implement `consult_file()` for external rule files (non-sandbox)

- [ ] **Task 3: Implement timeout protection** (AC: 4)
  - [ ] Use SWI-Prolog `call_with_time_limit/2`
  - [ ] Handle `time_limit_exceeded` exception
  - [ ] Add timeout configuration to constructor

- [ ] **Task 4: Implement sandbox** (AC: 5)
  - [ ] Load SWI-Prolog `library(sandbox)`
  - [ ] Configure file/network/shell restrictions
  - [ ] Add sandbox toggle to constructor

- [ ] **Task 5: Integrate into YAMLEngine** (AC: 1-3, 8, 9, 10)
  - [ ] Add `_prolog_runtime` lazy initialization to `YAMLEngine`
  - [ ] Extend `_create_run_function()` to detect `type: prolog`
  - [ ] Add `prolog_enabled` and `prolog_timeout` config options
  - [ ] Implement `detect_prolog_code()` function
  - [ ] Ensure backward compatibility with Python and Lua blocks

- [ ] **Task 6: Unit tests for PrologRuntime** (AC: 13)
  - [ ] Test `_python_to_prolog` with all JSON types
  - [ ] Test `_prolog_to_python` with all Prolog types
  - [ ] Test `execute_query()` basic functionality
  - [ ] Test `execute_query()` timeout with infinite recursion
  - [ ] Test sandbox blocks dangerous predicates
  - [ ] Test `eval_condition()` with success/failure
  - [ ] Test `execute_node_code()` with state access
  - [ ] Test CLP(FD) constraint solving

- [ ] **Task 7: Integration tests for YAML** (AC: 14)
  - [ ] Create test YAML with `language: prolog`
  - [ ] Test mixed Python/Lua/Prolog nodes in same workflow
  - [ ] Test neurosymbolic pattern (Python neural → Prolog reasoning)
  - [ ] Create fixture `prolog_test_agent.yaml`

- [ ] **Task 8: Documentation** (AC: 15, 16)
  - [ ] Add Prolog scripting section to `docs/shared/YAML_REFERENCE.md`
  - [ ] Document state/2 and return/2 interface
  - [ ] Document CLP(FD) usage examples
  - [ ] Document installation: `pip install the-edge-agent[prolog]`
  - [ ] Document SWI-Prolog system installation
  - [ ] Add Prolog examples to examples directory

---

## Dev Notes

### Relevant Source Tree

```
the_edge_agent/
├── python/
│   ├── src/the_edge_agent/
│   │   ├── yaml_engine.py          # Modify: add Prolog dispatch
│   │   ├── lua_runtime.py          # Reference pattern
│   │   └── prolog_runtime.py       # NEW FILE to create
│   ├── tests/
│   │   ├── test_prolog_runtime.py  # NEW FILE to create
│   │   └── fixtures/
│   │       └── prolog_test_agent.yaml  # NEW fixture
│   ├── requirements.txt            # Add pyswip dependency
│   └── pyproject.toml              # Add [prolog] optional deps
│
├── docs/shared/
│   └── YAML_REFERENCE.md           # Update with Prolog section
│
└── examples/
    └── prolog/                      # NEW: Prolog examples
        ├── simple-prolog-agent.yaml
        ├── neurosymbolic-agent.yaml
        └── clpfd-constraints.yaml
```

### Testing

- **Test file location:** `python/tests/test_prolog_runtime.py`
- **Test frameworks:** pytest with standard assertions
- **Pattern:** Mirror Lua test cases structure
- **Key tests:**
  - `test_python_to_prolog_primitives`
  - `test_prolog_to_python_primitives`
  - `test_execute_query_basic`
  - `test_execute_query_state_access`
  - `test_execute_query_timeout`
  - `test_sandbox_blocks_file_access`
  - `test_clpfd_constraints`
  - `test_detect_prolog_code`

### pyswip API Reference

```python
from pyswip import Prolog

prolog = Prolog()

# Assert facts
prolog.assertz("fact(value)")

# Query
for result in prolog.query("fact(X)"):
    print(result['X'])

# Consult file
prolog.consult("rules.pl")

# Retract
prolog.retractall("fact(_)")
```

---

## Risk and Compatibility Check

**Minimal Risk Assessment:**

- **Primary Risk:** SWI-Prolog system installation required
- **Mitigation:** Clear documentation, Docker option, optional dependency
- **Rollback:** Remove `prolog_runtime.py`, revert YAMLEngine changes

**Compatibility Verification:**

- [ ] No breaking changes to existing Python `run:` blocks
- [ ] No breaking changes to existing Lua `run:` blocks
- [ ] No changes to Jinja2 condition evaluation
- [ ] Optional dependency - doesn't affect users not using Prolog

---

## Definition of Done

- [ ] Functional requirements met (AC 1-7)
- [ ] Integration requirements verified (AC 8-12)
- [ ] Existing functionality regression tested
- [ ] Code follows existing patterns and standards
- [ ] Tests pass (existing and new) (AC 13-14)
- [ ] Documentation updated with Prolog section (AC 15-16)

---

## QA Results

### Test Design Review

**Date:** 2025-12-21
**Reviewer:** Quinn (Test Architect)
**Status:** APPROVED

#### Test Strategy Summary

| Metric | Count |
|--------|-------|
| Total test scenarios | 42 |
| Unit tests | 26 (62%) |
| Integration tests | 12 (29%) |
| E2E tests | 4 (9%) |

#### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| P0 | 16 | Critical - security, timeout, core functionality |
| P1 | 18 | High - main features, backward compatibility |
| P2 | 8 | Medium - edge cases, documentation validation |

#### P0 Tests (Must Implement)

1. `TEA-PY-004-UNIT-001` - Core Prolog execution via `execute_node_code()`
2. `TEA-PY-004-UNIT-002` - `state/2` predicate unification
3. `TEA-PY-004-UNIT-010` to `UNIT-015` - Sandbox security (6 tests)
4. `TEA-PY-004-UNIT-016` to `UNIT-018` - Timeout protection (3 tests)
5. `TEA-PY-004-UNIT-026` to `UNIT-032` - JSON↔Prolog conversion (7 tests)
6. `TEA-PY-004-UNIT-035` - Import error with install instructions
7. `TEA-PY-004-INT-001` - YAMLEngine type dispatch
8. `TEA-PY-004-INT-007`, `INT-008` - Parallel isolation
9. `TEA-PY-004-INT-009` to `INT-011` - Backward compatibility (3 tests)

#### Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| Sandbox bypass | UNIT-010 through UNIT-015 |
| Infinite recursion | UNIT-016, UNIT-017, UNIT-018 |
| Backward compatibility | INT-009, INT-010, INT-011 |
| Parallel contamination | INT-007, INT-008 |
| JSON conversion | UNIT-026 through UNIT-032 |

#### Test Design Document

`docs/qa/assessments/TEA-PY-004-test-design-20251221.md`

#### Recommendation

**APPROVED** - Story is well-defined with clear acceptance criteria. Test design provides comprehensive coverage with appropriate shift-left strategy (62% unit tests). Security-critical sandbox and timeout tests are P0. All 16 acceptance criteria have test coverage.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-21 | 0.1 | Initial story draft | Sarah (PO) |
| 2025-12-21 | 0.2 | Test design complete, status → Approved | Quinn (QA) |
