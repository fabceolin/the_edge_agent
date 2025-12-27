# Test Design: TEA-PY-008.2 - Extract Node Factory Module

## Document Information

| Field | Value |
|-------|-------|
| Story ID | TEA-PY-008.2 |
| Test Designer | Quinn (Test Architect) |
| Date | 2025-12-27 |
| Status | Draft |
| Related Stories | TEA-PY-006 (Lua Thread Isolation), TEA-PY-008.1 (TemplateProcessor) |

## Executive Summary

This test design covers the extraction of ~700 lines of node factory logic from `yaml_engine.py` into a new `yaml_nodes.py` module. The refactor is **high risk** due to:
1. Critical Lua thread isolation logic (TEA-PY-006)
2. Complex while-loop validation
3. Prolog runtime thread-local state management
4. Multiple node types with different execution paths

The test strategy focuses on:
- **P0**: Backward compatibility (all 320+ existing tests must pass)
- **P0**: Runtime isolation (Lua thread isolation, Prolog thread-local state)
- **P0**: Node type functional parity (7 node types)
- **P1**: Code structure and maintainability

## Test Pyramid

```
                    E2E (5 scenarios)
                 ┌─────────────────┐
                 │  Full suite run │
                 │  Cross-runtime  │
                 └─────────────────┘
                         ▲
             Integration (18 scenarios)
          ┌──────────────────────────┐
          │  Runtime isolation       │
          │  Node type execution     │
          │  Engine integration      │
          └──────────────────────────┘
                         ▲
              Unit (25 scenarios)
     ┌─────────────────────────────────┐
     │  Factory structure              │
     │  Detection logic                │
     │  Wrapper behavior               │
     │  Individual node creation       │
     └─────────────────────────────────┘
```

**Total Scenarios**: 48 (25 unit + 18 integration + 5 e2e)

## Risk Analysis

### Critical Risks

| Risk ID | Risk | Impact | Likelihood | Mitigation |
|---------|------|--------|------------|------------|
| R1 | Lua thread isolation broken | **CRITICAL** | Medium | TEA-PY-008.2-integration-002, TEA-PY-008.2-integration-003 |
| R2 | Prolog thread-local state corruption | **HIGH** | Low | TEA-PY-008.2-integration-004 |
| R3 | While-loop validation regression | **HIGH** | Medium | TEA-PY-008.2-unit-018, TEA-PY-008.2-integration-008 |
| R4 | Circular import between modules | **MEDIUM** | High | TEA-PY-008.2-unit-001 |
| R5 | Context capture in wrappers broken | **HIGH** | Medium | TEA-PY-008.2-integration-009, TEA-PY-008.2-integration-010 |

### Testing Focus Areas

1. **Thread Safety** (R1, R2) - 30% of test effort
2. **Node Type Parity** (R3) - 25% of test effort
3. **Backward Compatibility** - 25% of test effort
4. **Integration Points** (R4, R5) - 20% of test effort

---

## Test Scenarios

### 1. Module Creation (AC 1-3)

#### TEA-PY-008.2-unit-001: Module Structure and Imports
**Priority**: P1
**Type**: Unit
**AC Coverage**: 1, 3

**Objective**: Verify the new `yaml_nodes.py` module is properly structured without circular imports.

**Preconditions**:
- Module file created at `python/src/the_edge_agent/yaml_nodes.py`

**Test Steps**:
1. Import the module: `import the_edge_agent.yaml_nodes`
2. Verify NodeFactory class exists: `hasattr(module, 'NodeFactory')`
3. Check TYPE_CHECKING import for YAMLEngine
4. Count total lines in file (excluding comments/blank lines)

**Expected Results**:
- Module imports successfully without circular dependency errors
- NodeFactory class is accessible
- Module under 750 lines total
- No runtime imports of YAMLEngine (only TYPE_CHECKING)

**Test Data**: N/A

---

#### TEA-PY-008.2-unit-002: Module Docstring Completeness
**Priority**: P2
**Type**: Unit
**AC Coverage**: 2

**Objective**: Verify module has comprehensive documentation.

**Test Steps**:
1. Read module `__doc__` attribute
2. Check for usage examples in docstring
3. Verify NodeFactory class docstring exists
4. Verify key methods have docstrings

**Expected Results**:
- Module docstring contains description and examples
- NodeFactory.__doc__ is not None
- Methods `add_node_from_config()` and `create_run_function()` have docstrings

**Test Data**: N/A

---

### 2. NodeFactory Class Structure (AC 4-8)

#### TEA-PY-008.2-unit-003: NodeFactory Constructor
**Priority**: P0
**Type**: Unit
**AC Coverage**: 4, 5

**Objective**: Verify NodeFactory initializes correctly with engine reference.

**Test Steps**:
1. Create mock YAMLEngine with required attributes
2. Instantiate NodeFactory with engine
3. Verify `_engine` attribute is set
4. Verify runtime config attributes copied (_lua_enabled, _prolog_enabled, etc.)
5. Verify runtime instances initialized to None

**Expected Results**:
- `factory._engine` references the engine
- `factory._lua_enabled` matches engine value
- `factory._prolog_enabled` matches engine value
- `factory._lua_timeout` matches engine value
- `factory._prolog_timeout` matches engine value
- `factory._prolog_sandbox` matches engine value
- `factory._lua_runtime` is None initially
- `factory._prolog_runtime` is None initially

**Test Data**:
```python
mock_engine = Mock(
    _lua_enabled=True,
    _lua_timeout=30.0,
    _prolog_enabled=True,
    _prolog_timeout=30.0,
    _prolog_sandbox=True
)
```

---

#### TEA-PY-008.2-unit-004: add_node_from_config Method Signature
**Priority**: P0
**Type**: Unit
**AC Coverage**: 6

**Objective**: Verify `add_node_from_config()` has correct signature.

**Test Steps**:
1. Get method signature using `inspect.signature()`
2. Verify parameters: `graph`, `node_config`
3. Verify return type annotation is None
4. Verify method is callable

**Expected Results**:
- Method exists on NodeFactory
- Parameters match: (self, graph: StateGraph, node_config: Dict[str, Any])
- Return type is None
- Method is callable

**Test Data**: N/A

---

#### TEA-PY-008.2-unit-005: create_run_function Method Signature
**Priority**: P0
**Type**: Unit
**AC Coverage**: 7

**Objective**: Verify `create_run_function()` has correct signature.

**Test Steps**:
1. Get method signature using `inspect.signature()`
2. Verify parameter: `node_config`
3. Verify return type annotation is Optional[Callable]
4. Verify method is callable

**Expected Results**:
- Method exists on NodeFactory
- Parameter matches: (self, node_config: Dict[str, Any])
- Return type is Optional[Callable]
- Method is callable

**Test Data**: N/A

---

#### TEA-PY-008.2-unit-006: All Node Creation Methods Present
**Priority**: P0
**Type**: Unit
**AC Coverage**: 8

**Objective**: Verify all 13 node creation methods were moved to NodeFactory.

**Test Steps**:
1. Check for presence of each method:
   - `_add_node_from_config()`
   - `_wrap_with_auto_trace()`
   - `_wrap_with_observability()`
   - `_create_run_function()`
   - `_detect_lua_code()`
   - `_detect_prolog_code()`
   - `_get_lua_runtime()`
   - `_get_prolog_runtime()`
   - `_create_inline_function()`
   - `_create_action_function()`
   - `_create_steps_function()`
   - `_create_expression_function()`
   - `_create_while_loop_function()`
2. Verify each method is callable

**Expected Results**:
- All 13 methods exist on NodeFactory class
- All methods are callable
- Method names match expected list

**Test Data**: List of 13 method names

---

### 3. Runtime Detection and Access (AC 9-13)

#### TEA-PY-008.2-unit-007: Lua Code Detection Logic
**Priority**: P0
**Type**: Unit
**AC Coverage**: 9

**Objective**: Verify `_detect_lua_code()` identifies Lua syntax correctly.

**Test Steps**:
1. Test Lua-specific patterns:
   - `function test()` (function keyword)
   - `local x = 10` (local keyword)
   - `for i = 1, 10 do` (for loop)
   - `if x then end` (if-then-end)
   - `return {key = value}` (table syntax)
2. Test Python code (should return False):
   - `def test():`
   - `import os`
   - `return {"key": "value"}`
3. Test edge cases:
   - Empty string
   - Comments only
   - Mixed Lua/Python-like syntax

**Expected Results**:
- Lua patterns return True
- Python patterns return False
- Edge cases handled gracefully

**Test Data**:
```python
lua_samples = [
    "function test() return 42 end",
    "local x = state.value",
    "for i = 1, 10 do print(i) end",
    "if state.condition then return {result = true} end"
]

python_samples = [
    "def test(): return 42",
    "import the_edge_agent",
    "return {'result': True}"
]
```

---

#### TEA-PY-008.2-unit-008: Prolog Code Detection Logic
**Priority**: P0
**Type**: Unit
**AC Coverage**: 10

**Objective**: Verify `_detect_prolog_code()` identifies Prolog syntax correctly.

**Test Steps**:
1. Test Prolog-specific patterns:
   - `parent(X, Y) :- father(X, Y).` (rule syntax)
   - `factorial(0, 1).` (fact syntax)
   - `?- query(X).` (query syntax)
   - `member(X, [H|T]).` (list syntax)
2. Test Python code (should return False)
3. Test edge cases

**Expected Results**:
- Prolog patterns return True
- Python patterns return False
- Edge cases handled gracefully

**Test Data**:
```python
prolog_samples = [
    "parent(X, Y) :- father(X, Y).",
    "factorial(0, 1).",
    "?- ancestor(X, john).",
    "append([], L, L)."
]
```

---

#### TEA-PY-008.2-integration-001: Lua Runtime Creation (Lua Disabled)
**Priority**: P1
**Type**: Integration
**AC Coverage**: 11

**Objective**: Verify Lua runtime raises error when disabled.

**Preconditions**:
- NodeFactory created with `_lua_enabled=False`

**Test Steps**:
1. Call `_get_lua_runtime()`
2. Catch exception

**Expected Results**:
- Raises RuntimeError
- Error message: "Lua support not enabled"

**Test Data**:
```python
engine = Mock(_lua_enabled=False)
factory = NodeFactory(engine)
```

---

#### TEA-PY-008.2-integration-002: Lua Runtime Thread Isolation (Main Thread)
**Priority**: P0 - **CRITICAL**
**Type**: Integration
**AC Coverage**: 11, 13

**Objective**: Verify main thread caches Lua runtime instance (TEA-PY-006).

**Preconditions**:
- NodeFactory created with `_lua_enabled=True`
- Running in main thread

**Test Steps**:
1. Call `_get_lua_runtime()` first time
2. Store runtime reference
3. Call `_get_lua_runtime()` second time
4. Compare references (should be identical)
5. Verify `_lua_runtime` attribute is set

**Expected Results**:
- First call creates new runtime
- Second call returns cached runtime
- Both references are the same object (id() matches)
- `factory._lua_runtime` is not None after first call

**Test Data**: N/A

**Notes**: This test verifies the fix from TEA-PY-006 where main thread must cache runtime.

---

#### TEA-PY-008.2-integration-003: Lua Runtime Thread Isolation (Worker Thread)
**Priority**: P0 - **CRITICAL**
**Type**: Integration
**AC Coverage**: 11, 13

**Objective**: Verify worker threads create fresh Lua runtime instances (TEA-PY-006).

**Preconditions**:
- NodeFactory created with `_lua_enabled=True`

**Test Steps**:
1. In main thread: Call `_get_lua_runtime()` and store reference
2. Create worker thread
3. In worker thread: Call `_get_lua_runtime()` and store reference
4. Compare references (should be different)
5. In worker thread: Call `_get_lua_runtime()` second time
6. Verify worker gets fresh runtime each time

**Expected Results**:
- Main thread runtime is cached
- Worker thread runtime is different from main thread
- Worker thread creates fresh runtime on each call (no caching)
- `threading.current_thread()` check works correctly

**Test Data**: N/A

**Notes**: This test verifies the fix from TEA-PY-006 where worker threads must NOT cache runtime to prevent state leakage in parallel branches.

**Test Code Sketch**:
```python
import threading

main_runtime = factory._get_lua_runtime()
worker_runtime = None

def worker_fn():
    nonlocal worker_runtime
    worker_runtime = factory._get_lua_runtime()

thread = threading.Thread(target=worker_fn)
thread.start()
thread.join()

assert main_runtime is not worker_runtime
```

---

#### TEA-PY-008.2-integration-004: Prolog Runtime Thread-Local State
**Priority**: P0 - **CRITICAL**
**Type**: Integration
**AC Coverage**: 12, 13

**Objective**: Verify Prolog runtime uses thread-local state correctly.

**Preconditions**:
- NodeFactory created with `_prolog_enabled=True`

**Test Steps**:
1. In main thread: Call `_get_prolog_runtime()` and add fact
2. Create worker thread
3. In worker thread: Call `_get_prolog_runtime()` and query fact
4. Verify worker thread doesn't see main thread's fact
5. In worker thread: Add different fact
6. In main thread: Verify main thread doesn't see worker's fact

**Expected Results**:
- Runtime instance is shared
- Thread-local state is isolated
- Facts added in one thread are not visible in another thread

**Test Data**:
```python
main_fact = "test_fact(main_thread)."
worker_fact = "test_fact(worker_thread)."
```

**Notes**: This test verifies Prolog's thread-local state management.

---

### 4. Wrapper Functions (AC 14-16)

#### TEA-PY-008.2-unit-009: Auto-Trace Wrapper Structure
**Priority**: P0
**Type**: Unit
**AC Coverage**: 14

**Objective**: Verify `_wrap_with_auto_trace()` creates correct wrapper.

**Test Steps**:
1. Create mock function: `lambda state: {"result": state["input"] * 2}`
2. Mock trace_context
3. Call `_wrap_with_auto_trace(mock_fn, "test_node")`
4. Verify wrapper is callable
5. Execute wrapper with test state
6. Verify original function was called

**Expected Results**:
- Wrapper function returned
- Wrapper is callable
- Wrapper executes original function
- Wrapper returns original function's result

**Test Data**:
```python
test_state = {"input": 5}
expected_result = {"result": 10}
```

---

#### TEA-PY-008.2-integration-009: Auto-Trace Wrapper Behavior
**Priority**: P0
**Type**: Integration
**AC Coverage**: 14, 16

**Objective**: Verify auto-trace wrapper logs events correctly.

**Preconditions**:
- NodeFactory created with `_auto_trace=True`
- Mock trace_context with event capture

**Test Steps**:
1. Create run function that takes 0.1s
2. Wrap with `_wrap_with_auto_trace()`
3. Execute wrapped function
4. Verify trace_context.log_event called with:
   - node_start event (timestamp, node_name)
   - node_end event (timestamp, duration, result)

**Expected Results**:
- node_start event logged before execution
- node_end event logged after execution
- Duration is approximately 0.1s
- Result is captured in node_end event

**Test Data**:
```python
def slow_fn(state):
    time.sleep(0.1)
    return {"result": "done"}
```

---

#### TEA-PY-008.2-unit-010: Observability Wrapper Structure
**Priority**: P0
**Type**: Unit
**AC Coverage**: 15

**Objective**: Verify `_wrap_with_observability()` creates correct wrapper.

**Test Steps**:
1. Create mock function
2. Mock observability_context
3. Call `_wrap_with_observability(mock_fn, "test_node")`
4. Verify wrapper is callable
5. Execute wrapper with test state

**Expected Results**:
- Wrapper function returned
- Wrapper is callable
- Wrapper executes original function
- Wrapper returns original function's result

**Test Data**: Similar to TEA-PY-008.2-unit-009

---

#### TEA-PY-008.2-integration-010: Observability Wrapper Events
**Priority**: P0
**Type**: Integration
**AC Coverage**: 15, 16

**Objective**: Verify observability wrapper emits correct events.

**Preconditions**:
- NodeFactory created with `_enable_observability=True`
- Mock observability_context with event capture

**Test Steps**:
1. Create run function
2. Wrap with `_wrap_with_observability()`
3. Execute wrapped function
4. Verify observability_context.emit called with:
   - node_execution_start event
   - node_execution_end event (with duration, output)

**Expected Results**:
- node_execution_start emitted before execution
- node_execution_end emitted after execution
- Events contain node_name, state, duration, output

**Test Data**: Similar to TEA-PY-008.2-integration-009

---

### 5. Node Type Support (AC 17-23)

#### TEA-PY-008.2-unit-011: Inline Python Code Node Creation
**Priority**: P0
**Type**: Unit
**AC Coverage**: 17

**Objective**: Verify inline Python code node creation.

**Test Steps**:
1. Create node_config with Python code in `run:`
2. Call `create_run_function(node_config)`
3. Verify function returned
4. Execute function with test state
5. Verify result

**Expected Results**:
- Run function created successfully
- Function executes Python code
- State is accessible in code
- Result is returned correctly

**Test Data**:
```python
node_config = {
    "name": "python_node",
    "run": "return {'result': state['input'] * 2}"
}
test_state = {"input": 21}
expected = {"result": 42}
```

---

#### TEA-PY-008.2-integration-005: Inline Lua Code Node Execution
**Priority**: P0
**Type**: Integration
**AC Coverage**: 18, 13

**Objective**: Verify inline Lua code executes correctly.

**Preconditions**:
- NodeFactory created with `_lua_enabled=True`

**Test Steps**:
1. Create node_config with Lua code
2. Call `create_run_function(node_config)`
3. Execute function with test state
4. Verify Lua code executed
5. Verify state passed to Lua
6. Verify result returned from Lua

**Expected Results**:
- Lua code detected correctly
- Lua runtime created
- State accessible as `state` table in Lua
- Result converted back to Python dict

**Test Data**:
```python
node_config = {
    "name": "lua_node",
    "run": """
        local result = state.input * 2
        return {result = result}
    """
}
test_state = {"input": 21}
expected = {"result": 42}
```

---

#### TEA-PY-008.2-integration-006: Inline Prolog Code Node Execution
**Priority**: P0
**Type**: Integration
**AC Coverage**: 19, 13

**Objective**: Verify inline Prolog code executes correctly.

**Preconditions**:
- NodeFactory created with `_prolog_enabled=True`

**Test Steps**:
1. Create node_config with Prolog code (facts + query)
2. Call `create_run_function(node_config)`
3. Execute function with test state
4. Verify Prolog code executed
5. Verify query results returned

**Expected Results**:
- Prolog code detected correctly
- Prolog runtime created
- Facts loaded successfully
- Query executed and results returned

**Test Data**:
```python
node_config = {
    "name": "prolog_node",
    "run": """
        parent(tom, bob).
        parent(tom, liz).
        ?- parent(tom, X).
    """
}
expected = {"results": [{"X": "bob"}, {"X": "liz"}]}
```

---

#### TEA-PY-008.2-unit-012: Action Node Creation
**Priority**: P0
**Type**: Unit
**AC Coverage**: 20

**Objective**: Verify action node creation with registry lookup.

**Test Steps**:
1. Register mock action in engine.actions_registry
2. Create node_config with `uses: mock_action`
3. Call `create_run_function(node_config)`
4. Verify function returned
5. Execute function
6. Verify action was called

**Expected Results**:
- Action looked up from registry
- Run function wraps action
- Action receives state parameter
- Action result returned

**Test Data**:
```python
def mock_action(state):
    return {"processed": True}

engine.actions_registry = {"mock_action": mock_action}
node_config = {
    "name": "action_node",
    "uses": "mock_action"
}
```

---

#### TEA-PY-008.2-integration-007: Multi-Step Node Execution
**Priority**: P0
**Type**: Integration
**AC Coverage**: 21

**Objective**: Verify multi-step nodes execute steps in order.

**Preconditions**:
- NodeFactory with multiple actions registered

**Test Steps**:
1. Create node_config with `steps:` list
2. Each step uses different action
3. Call `create_run_function(node_config)`
4. Execute function with initial state
5. Verify each step executed in order
6. Verify state accumulated across steps

**Expected Results**:
- Steps execute sequentially
- State passed through each step
- Final state contains results from all steps
- Execution order is preserved

**Test Data**:
```python
def step1(state):
    return {"step1_done": True}

def step2(state):
    return {"step2_done": True}

node_config = {
    "name": "multi_step",
    "steps": [
        {"uses": "step1"},
        {"uses": "step2"}
    ]
}
```

---

#### TEA-PY-008.2-unit-013: Expression Node Creation
**Priority**: P1
**Type**: Unit
**AC Coverage**: 22

**Objective**: Verify expression node evaluates expressions.

**Test Steps**:
1. Create node_config with `expression:`
2. Call `create_run_function(node_config)`
3. Execute with test state
4. Verify expression evaluated

**Expected Results**:
- Expression node created
- Expression uses `_evaluate_condition()` from engine
- Result returned in state

**Test Data**:
```python
node_config = {
    "name": "expr_node",
    "expression": "{{ state.value > 10 }}"
}
test_state = {"value": 42}
expected = True
```

---

#### TEA-PY-008.2-unit-014: While-Loop Node Creation
**Priority**: P0
**Type**: Unit
**AC Coverage**: 23

**Objective**: Verify while-loop node creation with validation.

**Test Steps**:
1. Create valid while-loop node_config
2. Call `create_run_function(node_config)`
3. Verify function returned
4. Verify all required fields validated:
   - `condition` exists
   - `body` exists (list of steps)
   - `max_iterations` is valid integer

**Expected Results**:
- While-loop node created successfully
- Validation passes for valid config
- Function is callable

**Test Data**:
```python
node_config = {
    "name": "while_loop",
    "while": {
        "condition": "{{ state.counter < 5 }}",
        "body": [
            {"run": "return {'counter': state.get('counter', 0) + 1}"}
        ],
        "max_iterations": 10
    }
}
```

---

#### TEA-PY-008.2-unit-015: While-Loop Validation (Missing Condition)
**Priority**: P0
**Type**: Unit
**AC Coverage**: 23

**Objective**: Verify while-loop validation rejects missing condition.

**Test Steps**:
1. Create while-loop config without `condition`
2. Call `create_run_function(node_config)`
3. Catch validation error

**Expected Results**:
- Raises ValueError or ConfigError
- Error message mentions "condition is required"

**Test Data**:
```python
node_config = {
    "name": "invalid_while",
    "while": {
        "body": [{"run": "return {}"}]
    }
}
```

---

#### TEA-PY-008.2-unit-016: While-Loop Validation (Missing Body)
**Priority**: P0
**Type**: Unit
**AC Coverage**: 23

**Objective**: Verify while-loop validation rejects missing body.

**Test Steps**:
1. Create while-loop config without `body`
2. Call `create_run_function(node_config)`
3. Catch validation error

**Expected Results**:
- Raises ValueError or ConfigError
- Error message mentions "body is required"

**Test Data**: Similar structure to TEA-PY-008.2-unit-015

---

#### TEA-PY-008.2-unit-017: While-Loop Validation (Empty Body)
**Priority**: P0
**Type**: Unit
**AC Coverage**: 23

**Objective**: Verify while-loop validation rejects empty body.

**Test Steps**:
1. Create while-loop config with empty body list
2. Call `create_run_function(node_config)`
3. Catch validation error

**Expected Results**:
- Raises ValueError or ConfigError
- Error message mentions "body must not be empty"

**Test Data**:
```python
node_config = {
    "name": "invalid_while",
    "while": {
        "condition": "{{ True }}",
        "body": []
    }
}
```

---

#### TEA-PY-008.2-unit-018: While-Loop Validation (Invalid max_iterations)
**Priority**: P0
**Type**: Unit
**AC Coverage**: 23

**Objective**: Verify while-loop validation rejects invalid max_iterations.

**Test Steps**:
1. Test with negative max_iterations
2. Test with zero max_iterations
3. Test with non-integer max_iterations
4. Verify all rejected with appropriate errors

**Expected Results**:
- Negative value raises error: "max_iterations must be positive"
- Zero value raises error: "max_iterations must be positive"
- Non-integer raises error: "max_iterations must be an integer"

**Test Data**:
```python
invalid_configs = [
    {"max_iterations": -1},
    {"max_iterations": 0},
    {"max_iterations": "not_a_number"},
    {"max_iterations": 3.14}
]
```

---

#### TEA-PY-008.2-integration-008: While-Loop Execution
**Priority**: P0
**Type**: Integration
**AC Coverage**: 23

**Objective**: Verify while-loop executes correctly with iteration limit.

**Preconditions**:
- Valid while-loop node config

**Test Steps**:
1. Create while-loop that increments counter
2. Set `max_iterations: 5`
3. Execute node
4. Verify loop stops at max_iterations
5. Verify state after each iteration

**Expected Results**:
- Loop executes exactly 5 times
- Counter increments on each iteration
- Final state has counter = 5
- No infinite loop

**Test Data**:
```python
node_config = {
    "name": "while_loop",
    "while": {
        "condition": "{{ state.counter < 100 }}",  # Would loop forever
        "body": [
            {"run": "return {'counter': state.get('counter', 0) + 1}"}
        ],
        "max_iterations": 5
    }
}
initial_state = {"counter": 0}
expected_final = {"counter": 5}
```

---

### 6. YAMLEngine Integration (AC 24-27)

#### TEA-PY-008.2-integration-011: YAMLEngine NodeFactory Initialization
**Priority**: P0
**Type**: Integration
**AC Coverage**: 24, 27

**Objective**: Verify YAMLEngine creates NodeFactory instance.

**Test Steps**:
1. Create YAMLEngine instance
2. Verify `_node_factory` attribute exists
3. Verify `_node_factory` is instance of NodeFactory
4. Verify factory has reference to engine
5. Verify factory has runtime configs

**Expected Results**:
- `engine._node_factory` is not None
- `isinstance(engine._node_factory, NodeFactory)` is True
- `engine._node_factory._engine` references engine
- Factory configs match engine configs

**Test Data**: N/A

---

#### TEA-PY-008.2-integration-012: YAMLEngine Delegates add_node_from_config
**Priority**: P0
**Type**: Integration
**AC Coverage**: 25

**Objective**: Verify YAMLEngine delegates node addition to factory.

**Preconditions**:
- YAMLEngine with mocked NodeFactory

**Test Steps**:
1. Create YAML with single node
2. Call `engine.load_from_dict(yaml_config)`
3. Verify `factory.add_node_from_config()` called
4. Verify call count matches number of nodes
5. Verify correct arguments passed

**Expected Results**:
- Factory method called for each node
- Arguments: graph instance, node_config dict
- Node added to graph successfully

**Test Data**:
```python
yaml_config = {
    "name": "test_agent",
    "state_schema": {"value": "int"},
    "nodes": [
        {"name": "node1", "run": "return {}"}
    ],
    "edges": [
        {"from": "__start__", "to": "node1"}
    ]
}
```

---

#### TEA-PY-008.2-integration-013: YAMLEngine Delegates create_run_function
**Priority**: P0
**Type**: Integration
**AC Coverage**: 26

**Objective**: Verify YAMLEngine delegates run function creation to factory.

**Preconditions**:
- YAMLEngine with mocked NodeFactory

**Test Steps**:
1. Call `engine._create_run_function(node_config)`
2. Verify `factory.create_run_function()` called
3. Verify return value passed through

**Expected Results**:
- Factory method called with node_config
- Return value from factory returned by engine method
- No transformation of arguments or result

**Test Data**:
```python
node_config = {
    "name": "test_node",
    "run": "return {'result': 42}"
}
```

---

#### TEA-PY-008.2-integration-014: Runtime Config Passing
**Priority**: P0
**Type**: Integration
**AC Coverage**: 27

**Objective**: Verify engine passes all runtime configs to factory.

**Test Steps**:
1. Create YAMLEngine with specific configs:
   - lua_enabled=True
   - lua_timeout=60.0
   - prolog_enabled=False
   - auto_trace=True
   - enable_observability=False
2. Verify factory receives all configs
3. Change engine config
4. Verify factory doesn't auto-update (no coupling)

**Expected Results**:
- Factory initialized with engine configs
- Factory stores copies of configs
- Factory doesn't reference engine configs dynamically

**Test Data**:
```python
engine_config = {
    "lua_enabled": True,
    "lua_timeout": 60.0,
    "prolog_enabled": False,
    "auto_trace": True,
    "enable_observability": False
}
```

---

### 7. Backward Compatibility (AC 28-30)

#### TEA-PY-008.2-e2e-001: Full Test Suite Execution
**Priority**: P0 - **CRITICAL**
**Type**: E2E
**AC Coverage**: 28, 29, 30

**Objective**: Verify all existing tests pass without modification.

**Preconditions**:
- Refactor completed
- No test files modified

**Test Steps**:
1. Run: `cd python && pytest tests/test_yaml_engine*.py -v`
2. Capture exit code and output
3. Count passed/failed/skipped tests
4. Compare with baseline (320+ tests)

**Expected Results**:
- All 320+ tests pass
- Zero test failures
- Zero test modifications required
- Exit code 0

**Test Data**: Existing test suite

**Notes**: This is the primary backward compatibility gate. All tests must pass.

---

#### TEA-PY-008.2-e2e-002: Lua Isolation Test Suite
**Priority**: P0 - **CRITICAL**
**Type**: E2E
**AC Coverage**: 28, 30, 13

**Objective**: Verify Lua thread isolation tests pass (TEA-PY-006).

**Preconditions**:
- Lua support enabled

**Test Steps**:
1. Run: `cd python && pytest tests/test_lua_isolation.py -v`
2. Verify all parallel branch tests pass
3. Verify state isolation verified

**Expected Results**:
- All Lua isolation tests pass
- Parallel branches don't leak state
- Main thread caching works
- Worker threads create fresh runtimes

**Test Data**: Existing Lua isolation tests

**Notes**: Critical for verifying R1 (Lua thread isolation) mitigation.

---

#### TEA-PY-008.2-e2e-003: Prolog Runtime Test Suite
**Priority**: P0 - **CRITICAL**
**Type**: E2E
**AC Coverage**: 28, 30, 12

**Objective**: Verify Prolog runtime tests pass.

**Preconditions**:
- Prolog support enabled

**Test Steps**:
1. Run: `cd python && pytest tests/test_prolog_runtime.py -v`
2. Verify thread-local state tests pass
3. Verify query execution tests pass

**Expected Results**:
- All Prolog tests pass
- Thread-local state isolation verified
- Query results correct

**Test Data**: Existing Prolog tests

---

#### TEA-PY-008.2-e2e-004: While-Loop Test Suite
**Priority**: P0 - **CRITICAL**
**Type**: E2E
**AC Coverage**: 28, 30, 23

**Objective**: Verify while-loop validation and execution tests pass.

**Preconditions**:
- While-loop nodes supported

**Test Steps**:
1. Run: `cd python && pytest tests/test_yaml_engine_while_loop.py -v`
2. Verify validation tests pass
3. Verify execution tests pass
4. Verify max_iterations enforcement

**Expected Results**:
- All while-loop tests pass
- Validation errors match expected
- Execution behavior unchanged

**Test Data**: Existing while-loop tests

**Notes**: Critical for verifying R3 (while-loop validation) mitigation.

---

#### TEA-PY-008.2-e2e-005: Cross-Runtime Integration
**Priority**: P1
**Type**: E2E
**AC Coverage**: 30

**Objective**: Verify workflows using multiple runtimes work correctly.

**Test Steps**:
1. Create YAML agent with:
   - Python node
   - Lua node (if enabled)
   - Prolog node (if enabled)
   - Action node
   - While-loop node
2. Execute agent end-to-end
3. Verify state flows correctly
4. Verify all nodes execute

**Expected Results**:
- Agent loads successfully
- All nodes execute in order
- State passes between different runtime nodes
- Final result is correct

**Test Data**:
```yaml
name: cross_runtime_test
state_schema:
  counter: int
  result: str

nodes:
  - name: python_init
    run: return {'counter': 0}

  - name: lua_increment
    run: |
      return {counter = state.counter + 1}

  - name: prolog_check
    run: |
      positive(X) :- X > 0.
      ?- positive({{ state.counter }}).

  - name: action_process
    uses: some_action

  - name: while_loop
    while:
      condition: "{{ state.counter < 5 }}"
      body:
        - run: return {'counter': state.counter + 1}
      max_iterations: 10

  - name: finalize
    run: return {'result': 'done'}

edges:
  - from: __start__
    to: python_init
  - from: python_init
    to: lua_increment
  - from: lua_increment
    to: prolog_check
  - from: prolog_check
    to: action_process
  - from: action_process
    to: while_loop
  - from: while_loop
    to: finalize
  - from: finalize
    to: __end__
```

---

### 8. Additional Quality Tests

#### TEA-PY-008.2-unit-019: No Circular Imports
**Priority**: P0
**Type**: Unit
**AC Coverage**: 1

**Objective**: Verify no circular import between yaml_nodes and yaml_engine.

**Test Steps**:
1. Import yaml_nodes first: `import the_edge_agent.yaml_nodes`
2. Import yaml_engine: `import the_edge_agent.yaml_engine`
3. Verify no ImportError
4. Check TYPE_CHECKING usage in yaml_nodes
5. Verify YAMLEngine import only in TYPE_CHECKING block

**Expected Results**:
- No circular import error
- Both modules import cleanly
- TYPE_CHECKING prevents runtime circular dependency

**Test Data**: N/A

---

#### TEA-PY-008.2-unit-020: Error Message Preservation
**Priority**: P1
**Type**: Unit
**AC Coverage**: 30

**Objective**: Verify error messages unchanged from original implementation.

**Test Steps**:
1. Trigger various error conditions:
   - Lua disabled but Lua code detected
   - Prolog disabled but Prolog code detected
   - Invalid while-loop config
   - Missing action in registry
2. Capture error messages
3. Compare with expected messages from original implementation

**Expected Results**:
- Error messages match original exactly
- Error types match original exactly
- Stack traces still useful

**Test Data**: Various invalid configs

---

#### TEA-PY-008.2-integration-015: Template Processing Delegation
**Priority**: P1
**Type**: Integration
**AC Coverage**: Dependency on TEA-PY-008.1

**Objective**: Verify NodeFactory delegates template processing to engine.

**Preconditions**:
- TEA-PY-008.1 (TemplateProcessor) completed

**Test Steps**:
1. Create node with template in run code: `{{ state.value }}`
2. Call create_run_function()
3. Verify `engine._process_template()` called
4. Verify template rendered correctly

**Expected Results**:
- Factory doesn't process templates directly
- Delegates to engine._process_template()
- Template rendering works correctly

**Test Data**:
```python
node_config = {
    "name": "template_node",
    "run": "return {'result': {{ state.value }} * 2}"
}
test_state = {"value": 21}
```

---

#### TEA-PY-008.2-integration-016: Condition Evaluation Delegation
**Priority**: P1
**Type**: Integration
**AC Coverage**: 22

**Objective**: Verify expression nodes delegate to engine._evaluate_condition().

**Test Steps**:
1. Create expression node
2. Execute node
3. Verify `engine._evaluate_condition()` called
4. Verify result correct

**Expected Results**:
- Factory doesn't evaluate conditions directly
- Delegates to engine._evaluate_condition()
- Condition evaluation works correctly

**Test Data**: Similar to TEA-PY-008.2-unit-013

---

#### TEA-PY-008.2-unit-021: Factory Immutability
**Priority**: P2
**Type**: Unit
**AC Coverage**: 5

**Objective**: Verify factory doesn't mutate engine state.

**Test Steps**:
1. Create engine with initial configs
2. Create factory
3. Modify factory attributes
4. Verify engine attributes unchanged

**Expected Results**:
- Factory has copies of configs, not references
- Modifying factory doesn't affect engine
- Proper encapsulation maintained

**Test Data**: N/A

---

#### TEA-PY-008.2-unit-022: Memory Cleanup
**Priority**: P2
**Type**: Unit
**AC Coverage**: N/A (Quality)

**Objective**: Verify factory cleans up runtimes properly.

**Test Steps**:
1. Create factory with Lua/Prolog enabled
2. Create runtimes
3. Delete factory
4. Verify no memory leaks (if possible)

**Expected Results**:
- Runtimes cleaned up on factory destruction
- No dangling references
- No resource leaks

**Test Data**: N/A

**Notes**: May require memory profiling tools.

---

#### TEA-PY-008.2-unit-023: Code Coverage
**Priority**: P2
**Type**: Unit
**AC Coverage**: N/A (Quality)

**Objective**: Measure test coverage of yaml_nodes.py.

**Test Steps**:
1. Run: `cd python && pytest --cov=the_edge_agent.yaml_nodes --cov-report=term --cov-report=html`
2. Check coverage percentage
3. Identify uncovered lines

**Expected Results**:
- Coverage > 90%
- All critical paths covered
- Only error handling paths may be uncovered

**Test Data**: N/A

---

#### TEA-PY-008.2-unit-024: Type Hints Validation
**Priority**: P2
**Type**: Unit
**AC Coverage**: N/A (Quality)

**Objective**: Verify type hints are correct and complete.

**Test Steps**:
1. Run: `cd python && mypy src/the_edge_agent/yaml_nodes.py`
2. Verify no type errors
3. Check all public methods have type hints

**Expected Results**:
- No mypy errors
- All parameters and return types annotated
- TYPE_CHECKING imports used correctly

**Test Data**: N/A

---

#### TEA-PY-008.2-unit-025: Documentation Completeness
**Priority**: P2
**Type**: Unit
**AC Coverage**: 2

**Objective**: Verify all public methods have docstrings.

**Test Steps**:
1. Parse yaml_nodes.py AST
2. Find all public methods (not starting with _)
3. Verify each has docstring
4. Verify docstrings have Args and Returns sections

**Expected Results**:
- All public methods documented
- Docstrings follow Google style
- Examples provided for main methods

**Test Data**: N/A

---

#### TEA-PY-008.2-integration-017: Parallel Node Execution
**Priority**: P1
**Type**: Integration
**AC Coverage**: 13, 30

**Objective**: Verify parallel nodes with different runtimes work correctly.

**Test Steps**:
1. Create agent with parallel branches:
   - Branch 1: Lua node
   - Branch 2: Python node
   - Branch 3: Prolog node
2. Execute agent
3. Verify all branches execute
4. Verify no state leakage between branches

**Expected Results**:
- All branches execute in parallel
- Each runtime isolated properly
- Fan-in collects results correctly
- No race conditions or state corruption

**Test Data**:
```yaml
nodes:
  - name: parallel_lua
    run: |
      return {lua_result = 42}

  - name: parallel_python
    run: return {'python_result': 42}

  - name: parallel_prolog
    run: |
      fact(42).
      ?- fact(X).

  - name: fan_in
    run: |
      # Collect parallel_results
      return {'combined': parallel_results}

edges:
  - from: __start__
    to: [parallel_lua, parallel_python, parallel_prolog]
  - from: [parallel_lua, parallel_python, parallel_prolog]
    to: fan_in
  - from: fan_in
    to: __end__
```

---

#### TEA-PY-008.2-integration-018: Error Propagation
**Priority**: P1
**Type**: Integration
**AC Coverage**: 30

**Objective**: Verify errors in factory propagate correctly to engine.

**Test Steps**:
1. Create node with invalid config
2. Try to load agent
3. Verify error propagates to caller
4. Verify stack trace is useful
5. Verify error message is clear

**Expected Results**:
- Errors don't get swallowed in factory
- Stack trace shows both factory and engine calls
- Error messages indicate source of problem
- Caller can catch and handle errors

**Test Data**: Various invalid node configs

---

## Test Execution Plan

### Phase 1: Unit Tests (Day 1)
**Focus**: Module structure, factory creation, detection logic, validation
**Tests**: TEA-PY-008.2-unit-001 through TEA-PY-008.2-unit-025
**Priority**: P0 and P1 unit tests
**Success Criteria**: All unit tests pass, coverage > 85%

### Phase 2: Integration Tests (Day 1-2)
**Focus**: Runtime creation, node execution, engine integration
**Tests**: TEA-PY-008.2-integration-001 through TEA-PY-008.2-integration-018
**Priority**: All P0 integration tests
**Success Criteria**: All integration tests pass, runtimes isolated correctly

### Phase 3: E2E Tests (Day 2)
**Focus**: Backward compatibility, full test suite
**Tests**: TEA-PY-008.2-e2e-001 through TEA-PY-008.2-e2e-005
**Priority**: All E2E tests (all P0)
**Success Criteria**: All 320+ existing tests pass without modification

### Phase 4: Quality & Regression (Day 2-3)
**Focus**: Edge cases, error handling, performance
**Tests**: Remaining P2 tests, exploratory testing
**Success Criteria**: No regressions found, quality metrics met

## Test Environment

### Prerequisites
- Python 3.10+
- pytest 7.0+
- lupa (for Lua support)
- janus-swi (for Prolog support)
- pytest-cov (for coverage)
- mypy (for type checking)

### Test Data Location
- `/home/fabricio/src/spa-base/the_edge_agent/python/tests/`
- Existing test files remain unchanged
- New test file: `tests/test_yaml_nodes.py` (if needed for unit tests)

### CI/CD Integration
- Tests run automatically on PR to main
- Coverage report generated
- All tests must pass before merge

## Success Metrics

| Metric | Target | Status |
|--------|--------|--------|
| Unit Test Pass Rate | 100% | TBD |
| Integration Test Pass Rate | 100% | TBD |
| E2E Test Pass Rate (Existing) | 100% (320+ tests) | TBD |
| Code Coverage | > 90% | TBD |
| No Circular Imports | 0 errors | TBD |
| Module Size | < 750 lines | TBD |
| Lua Thread Isolation | PASS | TBD |
| Prolog Thread-Local State | PASS | TBD |
| While-Loop Validation | PASS | TBD |

## Exit Criteria

This test design is considered complete when:

1. All 48 test scenarios defined
2. Test priorities assigned (P0-P3)
3. Test data specified
4. Risk mitigations mapped to tests
5. Execution plan created
6. Document reviewed by QA and PO

## Test Execution

To execute these tests during development:

```bash
# Phase 1: Unit tests
cd /home/fabricio/src/spa-base/the_edge_agent/python
pytest tests/test_yaml_nodes.py -v

# Phase 2: Integration tests (factory-specific)
pytest tests/test_yaml_engine.py::TestNodeFactory -v

# Phase 3: Full backward compatibility
pytest tests/test_yaml_engine*.py -v
pytest tests/test_lua_isolation.py -v
pytest tests/test_prolog_runtime.py -v
pytest tests/test_yaml_engine_while_loop.py -v

# Phase 4: Coverage report
pytest --cov=the_edge_agent.yaml_nodes --cov-report=term --cov-report=html
```

## Notes for Dev Agent

1. **Critical Path**: E2E tests (TEA-PY-008.2-e2e-001 through e2e-004) are the ultimate gate
2. **No Test Modification**: Existing tests must pass without changes (AC 28)
3. **Thread Safety**: Tests TEA-PY-008.2-integration-002 and -003 verify TEA-PY-006 fix
4. **While-Loop**: Tests TEA-PY-008.2-unit-014 through -018 and integration-008 are critical
5. **Delegation**: Factory should delegate, not duplicate, engine logic (template, condition eval)

## Approval

| Role | Name | Date | Signature |
|------|------|------|-----------|
| Test Architect | Quinn | 2025-12-27 | ________ |
| QA Lead | TBD | | |
| Product Owner | TBD | | |

---

**Document Version**: 1.0
**Last Updated**: 2025-12-27
**Next Review**: After Phase 3 completion
