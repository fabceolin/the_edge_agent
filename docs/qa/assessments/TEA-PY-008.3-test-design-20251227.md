# Test Design Assessment: TEA-PY-008.3

**Story**: Extract Edge Factory Module
**Date**: 2025-12-27
**Test Architect**: Quinn
**Story File**: `/home/fabricio/src/spa-base/the_edge_agent/docs/stories/TEA-PY-008.3-yaml-edges-module.md`

## Executive Summary

This test design covers the extraction of edge and goto processing logic (~350 lines) from `yaml_engine.py` into a dedicated `yaml_edges.py` module. The primary testing focus is ensuring backward compatibility while maintaining all functionality of the TEA-YAML-002 implicit graph syntax.

**Critical Testing Areas**:
1. EdgeFactory class isolation and initialization
2. Goto precedence rules (goto > edges > implicit)
3. Condition evaluation with template context
4. Legacy edge support (normal, parallel, conditional)
5. Backward compatibility with existing 320+ tests

## Test Coverage Matrix

| AC Group | Unit Tests | Integration Tests | E2E Tests | Total |
|----------|-----------|-------------------|-----------|-------|
| Module Creation (AC 1-3) | 3 | 0 | 0 | 3 |
| EdgeFactory Class (AC 4-7) | 8 | 2 | 0 | 10 |
| Goto Processing (AC 8-13) | 12 | 4 | 2 | 18 |
| Implicit Chaining (AC 14-17) | 8 | 2 | 1 | 11 |
| Condition Evaluation (AC 18-21) | 6 | 2 | 1 | 9 |
| Legacy Edge Support (AC 22-27) | 10 | 4 | 2 | 16 |
| YAMLEngine Integration (AC 28-31) | 4 | 4 | 0 | 8 |
| Backward Compatibility (AC 32-34) | 0 | 6 | 4 | 10 |
| **TOTAL** | **51** | **24** | **10** | **85** |

---

## Test Scenarios

### 1. Module Creation (AC 1-3)

#### TEA-PY-008.3-UNIT-001
**Title**: Verify yaml_edges.py module is created
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-1

**Preconditions**:
- None

**Test Steps**:
1. Check file exists at `python/src/the_edge_agent/yaml_edges.py`
2. Verify file is importable: `from the_edge_agent.yaml_edges import EdgeFactory`

**Expected Results**:
- File exists
- Import succeeds without errors

**Notes**: Static verification test

---

#### TEA-PY-008.3-UNIT-002
**Title**: Verify module has comprehensive docstring
**Level**: unit
**Priority**: P2
**AC Coverage**: AC-2

**Preconditions**:
- Module created

**Test Steps**:
1. Import module
2. Check `yaml_edges.__doc__` is not None
3. Verify docstring contains examples of usage
4. Verify docstring mentions TEA-YAML-002

**Expected Results**:
- Docstring exists and is comprehensive (>100 chars)
- Contains code examples
- References TEA-YAML-002

---

#### TEA-PY-008.3-UNIT-003
**Title**: Verify module is under 400 lines
**Level**: unit
**Priority**: P1
**AC Coverage**: AC-3

**Preconditions**:
- Module created

**Test Steps**:
1. Count lines in `yaml_edges.py`
2. Verify total <= 400

**Expected Results**:
- Line count <= 400
- Module stays maintainable

**Notes**: Can be automated with pre-commit hook

---

### 2. EdgeFactory Class (AC 4-7)

#### TEA-PY-008.3-UNIT-004
**Title**: EdgeFactory class exists and is importable
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-4

**Test Steps**:
1. Import EdgeFactory class
2. Check it's a class type
3. Verify it has expected methods

**Expected Results**:
- `EdgeFactory` class exists
- Has `__init__`, `process_goto_and_implicit_edges`, `add_edge_from_config`

---

#### TEA-PY-008.3-UNIT-005
**Title**: EdgeFactory constructor accepts engine reference
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-5

**Test Steps**:
1. Create mock YAMLEngine with required attributes:
   - `_jinja_env`
   - `_template_cache`
   - `variables`
   - `secrets`
   - `_evaluate_condition()`
2. Instantiate EdgeFactory with engine
3. Verify `_engine` attribute is set

**Expected Results**:
- EdgeFactory accepts engine parameter
- Stores engine reference internally

**Test Data**:
```python
mock_engine = Mock()
mock_engine._jinja_env = Mock()
mock_engine._template_cache = {}
mock_engine.variables = {}
mock_engine.secrets = {}
mock_engine._evaluate_condition = Mock(return_value=True)
```

---

#### TEA-PY-008.3-UNIT-006
**Title**: EdgeFactory initializes nodes_with_goto tracking
**Level**: unit
**Priority**: P1
**AC Coverage**: AC-4

**Test Steps**:
1. Create EdgeFactory instance
2. Verify `nodes_with_goto` property exists
3. Verify initial value is empty set

**Expected Results**:
- `nodes_with_goto` property returns `Set[str]`
- Initial value is `set()`

---

#### TEA-PY-008.3-UNIT-007
**Title**: process_goto_and_implicit_edges method signature correct
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-6

**Test Steps**:
1. Check method exists
2. Verify signature: `(graph: StateGraph, nodes_list: List[Dict], edges_list: List[Dict]) -> None`
3. Verify method is callable

**Expected Results**:
- Method signature matches specification
- Method accepts correct parameters

---

#### TEA-PY-008.3-UNIT-008
**Title**: add_edge_from_config method signature correct
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-7

**Test Steps**:
1. Check method exists
2. Verify signature: `(graph: StateGraph, edge_config: Dict[str, Any]) -> None`

**Expected Results**:
- Method signature matches specification

---

#### TEA-PY-008.3-UNIT-009
**Title**: EdgeFactory processes simple goto without errors
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-6, AC-8

**Test Steps**:
1. Create EdgeFactory with mock engine
2. Create StateGraph
3. Create nodes list with simple goto: `{"name": "a", "goto": "b"}`
4. Call `process_goto_and_implicit_edges()`
5. Verify no exceptions raised

**Expected Results**:
- Method executes without errors
- Graph edges are added correctly

---

#### TEA-PY-008.3-UNIT-010
**Title**: EdgeFactory handles empty nodes list gracefully
**Level**: unit
**Priority**: P2
**AC Coverage**: AC-6

**Test Steps**:
1. Create EdgeFactory
2. Call `process_goto_and_implicit_edges()` with empty nodes_list
3. Verify no errors

**Expected Results**:
- No errors for empty input
- Graceful handling of edge cases

---

#### TEA-PY-008.3-UNIT-011
**Title**: EdgeFactory handles empty edges list gracefully
**Level**: unit
**Priority**: P2
**AC Coverage**: AC-7

**Test Steps**:
1. Create EdgeFactory
2. Call `process_goto_and_implicit_edges()` with empty edges_list
3. Verify no errors

**Expected Results**:
- No errors for empty input
- Implicit chaining should still work

---

#### TEA-PY-008.3-INTG-001
**Title**: EdgeFactory integrates with real StateGraph for goto
**Level**: integration
**Priority**: P0
**AC Coverage**: AC-6, AC-8

**Test Steps**:
1. Create real YAMLEngine instance
2. Create EdgeFactory with engine
3. Create StateGraph with nodes
4. Process goto configuration
5. Execute graph and verify routing

**Test Data**:
```python
nodes = [
    {"name": "a", "goto": "c", "run": 'return {"path": "a"}'},
    {"name": "b", "run": 'return {"path": "b"}'},
    {"name": "c", "run": 'return {"path": "c"}'}
]
```

**Expected Results**:
- Graph executes a -> c (skipping b)
- State contains correct path

---

#### TEA-PY-008.3-INTG-002
**Title**: EdgeFactory integrates with StateGraph for legacy edges
**Level**: integration
**Priority**: P0
**AC Coverage**: AC-7, AC-22

**Test Steps**:
1. Create EdgeFactory
2. Process legacy edge configuration
3. Verify edges added to StateGraph

**Test Data**:
```python
edge_config = {"from": "node_a", "to": "node_b"}
```

**Expected Results**:
- Edge successfully added to graph
- Graph routing works correctly

---

### 3. Goto Processing - TEA-YAML-002 (AC 8-13)

#### TEA-PY-008.3-UNIT-012
**Title**: String goto creates unconditional edge
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-8

**Test Steps**:
1. Create EdgeFactory
2. Process node with `goto: "target_node"`
3. Verify edge added from source to target

**Test Data**:
```python
node = {"name": "source", "goto": "target"}
```

**Expected Results**:
- Unconditional edge created
- Routing always goes to target

**Notes**: Core goto functionality

---

#### TEA-PY-008.3-UNIT-013
**Title**: List goto creates conditional edges
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-9

**Test Steps**:
1. Create EdgeFactory
2. Process node with conditional goto list
3. Verify conditional edges added

**Test Data**:
```python
node = {
    "name": "check",
    "goto": [
        {"if": "state.x > 10", "to": "high"},
        {"if": "state.x > 0", "to": "low"}
    ]
}
```

**Expected Results**:
- Multiple conditional edges created
- First matching condition is used

---

#### TEA-PY-008.3-UNIT-014
**Title**: Fallback rule (no condition) works
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-10

**Test Steps**:
1. Create goto list with fallback rule (no `if` clause)
2. Process with state that matches no conditions
3. Verify fallback target is used

**Test Data**:
```python
goto = [
    {"if": "state.x > 100", "to": "very_high"},
    {"to": "default"}  # Fallback
]
state = {"x": 5}
```

**Expected Results**:
- Fallback rule executes when no conditions match
- Routes to "default" node

---

#### TEA-PY-008.3-UNIT-015
**Title**: Target validation at parse time - valid target
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-11

**Test Steps**:
1. Create nodes list: ["a", "b", "c"]
2. Create goto to existing target: `goto: "b"`
3. Process and verify no errors

**Expected Results**:
- No validation errors for valid target
- Processing succeeds

---

#### TEA-PY-008.3-UNIT-016
**Title**: Target validation at parse time - invalid target raises error
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-11

**Test Steps**:
1. Create nodes list: ["a", "b"]
2. Create goto to non-existent target: `goto: "non_existent"`
3. Verify ValueError is raised at parse time

**Expected Results**:
- `ValueError` raised
- Error message contains target name "non_existent"
- Error message indicates node does not exist

**Notes**: Validates TEA-YAML-002 AC-6

---

#### TEA-PY-008.3-UNIT-017
**Title**: Conditional goto invalid target raises error
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-11

**Test Steps**:
1. Create conditional goto with invalid target
2. Verify error at parse time

**Test Data**:
```python
goto = [{"if": "True", "to": "missing_node"}]
nodes = ["a", "b"]
```

**Expected Results**:
- ValueError raised
- Error mentions "missing_node"

---

#### TEA-PY-008.3-UNIT-018
**Title**: __end__ target works for finish points
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-12

**Test Steps**:
1. Create node with `goto: "__end__"`
2. Process and verify finish point is set
3. Execute graph and verify early termination

**Test Data**:
```python
nodes = [
    {"name": "a", "goto": "__end__"},
    {"name": "b"}  # Should never execute
]
```

**Expected Results**:
- Graph terminates after node "a"
- Node "b" is never executed

---

#### TEA-PY-008.3-UNIT-019
**Title**: Goto precedence over legacy edges enforced
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-13

**Test Steps**:
1. Create node with both goto and legacy edge
2. Process with EdgeFactory
3. Verify goto takes precedence

**Test Data**:
```python
node = {"name": "a", "goto": "c"}
edge = {"from": "a", "to": "b"}  # Should be ignored
```

**Expected Results**:
- Edge from "a" goes to "c" (goto target)
- Legacy edge to "b" is ignored
- `nodes_with_goto` contains "a"

**Notes**: Critical precedence rule from TEA-YAML-002

---

#### TEA-PY-008.3-UNIT-020
**Title**: nodes_with_goto tracking is maintained
**Level**: unit
**Priority**: P1
**AC Coverage**: AC-13

**Test Steps**:
1. Process multiple nodes, some with goto
2. Check `nodes_with_goto` property
3. Verify only goto nodes are tracked

**Test Data**:
```python
nodes = [
    {"name": "a", "goto": "c"},
    {"name": "b"},  # No goto
    {"name": "c", "goto": "__end__"}
]
```

**Expected Results**:
- `nodes_with_goto == {"a", "c"}`
- Node "b" not in set

---

#### TEA-PY-008.3-UNIT-021
**Title**: Missing 'to' field in goto rule raises error
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-11

**Test Steps**:
1. Create goto rule without 'to' field
2. Verify ValueError raised

**Test Data**:
```python
goto = [{"if": "state.x > 0"}]  # Missing 'to'
```

**Expected Results**:
- ValueError raised
- Error message mentions "'to'" field is required

---

#### TEA-PY-008.3-UNIT-022
**Title**: Invalid goto type (not string or list) raises error
**Level**: unit
**Priority**: P1
**AC Coverage**: AC-8, AC-9

**Test Steps**:
1. Create goto with invalid type (int, dict, etc.)
2. Verify error is raised

**Test Data**:
```python
goto = 123  # Invalid type
```

**Expected Results**:
- TypeError or ValueError raised
- Clear error message about goto type

---

#### TEA-PY-008.3-UNIT-023
**Title**: Goto list with all conditions false and no fallback
**Level**: unit
**Priority**: P1
**AC Coverage**: AC-10

**Test Steps**:
1. Create goto list with only conditional rules (no fallback)
2. Execute with state where all conditions are false
3. Verify behavior (should fall through to implicit chaining or error)

**Test Data**:
```python
goto = [
    {"if": "state.x > 100", "to": "high"},
    {"if": "state.x > 50", "to": "medium"}
]
state = {"x": 10}
```

**Expected Results**:
- Falls back to implicit chaining (next node)
- OR raises clear error about no matching condition

**Notes**: Edge case - spec should clarify expected behavior

---

#### TEA-PY-008.3-INTG-003
**Title**: Goto with complex conditional expressions
**Level**: integration
**Priority**: P1
**AC Coverage**: AC-9, AC-18

**Test Steps**:
1. Create goto with complex Jinja2 expressions
2. Verify expression evaluation works correctly
3. Test multiple state scenarios

**Test Data**:
```python
goto = [
    {"if": "state.score > 80 and state.verified", "to": "approved"},
    {"if": "state.score > 50 or state.retry < 3", "to": "review"},
    {"to": "reject"}
]
```

**Expected Results**:
- Complex AND/OR conditions evaluate correctly
- Routing matches expected logic

---

#### TEA-PY-008.3-INTG-004
**Title**: Goto loop with retry pattern
**Level**: integration
**Priority**: P1
**AC Coverage**: AC-9

**Test Steps**:
1. Create self-referencing goto for retry loop
2. Execute with incrementing counter
3. Verify loop executes and exits correctly

**Test Data**:
```python
goto = [
    {"if": "state.success", "to": "__end__"},
    {"if": "state.attempt < 3", "to": "retry_node"}  # Self-reference
]
```

**Expected Results**:
- Loop executes up to limit
- Exits on success or max attempts

**Notes**: Common pattern in LLM workflows

---

#### TEA-PY-008.3-INTG-005
**Title**: Multiple nodes with goto - complex routing graph
**Level**: integration
**Priority**: P1
**AC Coverage**: AC-8, AC-9

**Test Steps**:
1. Create 5+ nodes with various goto configurations
2. Execute different state scenarios
3. Verify all routing paths work correctly

**Expected Results**:
- All goto edges created correctly
- Complex routing graph executes as expected

---

#### TEA-PY-008.3-INTG-006
**Title**: Goto precedence in mixed configuration
**Level**: integration
**Priority**: P0
**AC Coverage**: AC-13

**Test Steps**:
1. Load YAML with nodes having goto AND legacy edges
2. Verify goto always wins
3. Confirm deprecation warnings logged for edges

**Expected Results**:
- Goto routing used
- Legacy edges ignored for goto nodes
- Warning logged about precedence

---

#### TEA-PY-008.3-E2E-001
**Title**: Full workflow with goto navigation
**Level**: e2e
**Priority**: P0
**AC Coverage**: AC-8, AC-9, AC-10, AC-12

**Test Steps**:
1. Load complete YAML file with goto navigation
2. Execute with YAMLEngine
3. Verify end-to-end execution

**Test Data**: Use existing `test_yaml_engine_goto.py::test_yaml002_goto_conditional_match`

**Expected Results**:
- Complete workflow executes
- All goto routing works
- Final state is correct

---

#### TEA-PY-008.3-E2E-002
**Title**: LLM self-correction pattern with goto
**Level**: e2e
**Priority**: P1
**AC Coverage**: AC-9

**Test Data**: Use existing `test_yaml_engine_goto.py::test_yaml002_self_correction_loop`

**Expected Results**:
- Multi-round retry loop works
- Conditional branching correct
- Final state shows success after retries

---

### 4. Implicit Chaining (AC 14-17)

#### TEA-PY-008.3-UNIT-024
**Title**: Entry point set to first node
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-14

**Test Steps**:
1. Create nodes list with no explicit entry edge
2. Process with EdgeFactory
3. Verify first node is set as entry point

**Test Data**:
```python
nodes = [
    {"name": "first"},
    {"name": "second"}
]
edges = []  # No __start__ edge
```

**Expected Results**:
- Graph entry point is "first"
- Workflow starts at first node

---

#### TEA-PY-008.3-UNIT-025
**Title**: Explicit entry edge overrides implicit entry
**Level**: unit
**Priority**: P1
**AC Coverage**: AC-14

**Test Steps**:
1. Create nodes with explicit `__start__` edge to middle node
2. Verify explicit edge takes precedence

**Test Data**:
```python
nodes = [{"name": "first"}, {"name": "second"}]
edges = [{"from": "__start__", "to": "second"}]
```

**Expected Results**:
- Entry point is "second", not "first"
- Explicit configuration wins

---

#### TEA-PY-008.3-UNIT-026
**Title**: Implicit chaining to next node for nodes without goto/edges
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-15

**Test Steps**:
1. Create 3 nodes with no goto or edges
2. Process with EdgeFactory
3. Verify implicit chains: node[0] -> node[1] -> node[2]

**Test Data**:
```python
nodes = [
    {"name": "a"},
    {"name": "b"},
    {"name": "c"}
]
edges = []
```

**Expected Results**:
- Edges added: a->b, b->c
- Linear execution order

---

#### TEA-PY-008.3-UNIT-027
**Title**: Finish point set for last node
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-16

**Test Steps**:
1. Create nodes with no explicit `__end__` edge
2. Verify last node has finish point

**Test Data**:
```python
nodes = [{"name": "a"}, {"name": "b"}, {"name": "last"}]
edges = []
```

**Expected Results**:
- Finish point set on "last" node
- Workflow terminates after last node

---

#### TEA-PY-008.3-UNIT-028
**Title**: Explicit finish edge overrides implicit finish
**Level**: unit
**Priority**: P1
**AC Coverage**: AC-16

**Test Steps**:
1. Create explicit edge from middle node to `__end__`
2. Verify explicit finish wins

**Test Data**:
```python
nodes = [{"name": "a"}, {"name": "b"}, {"name": "c"}]
edges = [{"from": "b", "to": "__end__"}]
```

**Expected Results**:
- Workflow can terminate at "b"
- "c" may not execute (depending on routing)

---

#### TEA-PY-008.3-UNIT-029
**Title**: Nodes with goto skip implicit chaining
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-17

**Test Steps**:
1. Create nodes where some have goto
2. Verify nodes with goto don't get implicit edges

**Test Data**:
```python
nodes = [
    {"name": "a", "goto": "c"},  # Has goto - skip implicit
    {"name": "b"},                # No goto - implicit to c
    {"name": "c"}
]
```

**Expected Results**:
- Node "a" edges only from goto (a->c)
- Node "b" gets implicit edge (b->c)

---

#### TEA-PY-008.3-UNIT-030
**Title**: Nodes with legacy edges skip implicit chaining
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-17

**Test Steps**:
1. Create node with legacy edge defined
2. Verify no implicit edge is added

**Test Data**:
```python
nodes = [
    {"name": "a"},     # No edge - implicit to b
    {"name": "b"},     # Has edge - no implicit
    {"name": "c"}
]
edges = [{"from": "b", "to": "c"}]
```

**Expected Results**:
- Node "a" gets implicit edge (a->b)
- Node "b" uses legacy edge only (b->c)

---

#### TEA-PY-008.3-UNIT-031
**Title**: Empty nodes list doesn't error on implicit chaining
**Level**: unit
**Priority**: P2
**AC Coverage**: AC-14, AC-15, AC-16

**Test Steps**:
1. Process empty nodes list
2. Verify no errors

**Expected Results**:
- No exceptions
- Graceful handling

---

#### TEA-PY-008.3-INTG-007
**Title**: Implicit chaining with real StateGraph execution
**Level**: integration
**Priority**: P0
**AC Coverage**: AC-14, AC-15, AC-16

**Test Steps**:
1. Load YAML with implicit chaining (no edges)
2. Execute with YAMLEngine
3. Verify linear execution order

**Test Data**: Use existing `test_yaml_engine_goto.py::test_yaml002_implicit_linear_flow`

**Expected Results**:
- Nodes execute in list order
- State accumulates correctly

---

#### TEA-PY-008.3-INTG-008
**Title**: Mixed implicit and explicit edges
**Level**: integration
**Priority**: P1
**AC Coverage**: AC-17

**Test Steps**:
1. Create nodes with mix of implicit, goto, and legacy edges
2. Execute and verify correct routing

**Test Data**:
```python
nodes = [
    {"name": "a"},                    # Implicit to b
    {"name": "b", "goto": "d"},       # Goto to d
    {"name": "c"},                    # Implicit to d
    {"name": "d"}                     # Implicit finish
]
```

**Expected Results**:
- Implicit edges only where no goto/edges
- Mixed routing works correctly

---

#### TEA-PY-008.3-E2E-003
**Title**: Complete workflow with only implicit chaining
**Level**: e2e
**Priority**: P1
**AC Coverage**: AC-14, AC-15, AC-16

**Test Data**: Use existing `test_yaml_engine_goto.py::test_yaml002_implicit_linear_flow`

**Expected Results**:
- No edges section needed
- Workflow executes in linear order
- Matches TEA-YAML-002 vision

---

### 5. Condition Evaluation (AC 18-21)

#### TEA-PY-008.3-UNIT-032
**Title**: _evaluate_goto_condition method exists
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-18

**Test Steps**:
1. Create EdgeFactory
2. Verify method exists
3. Check signature: `(expr: str, state: Dict, result: Optional[Dict]) -> bool`

**Expected Results**:
- Method exists and is callable
- Signature matches specification

---

#### TEA-PY-008.3-UNIT-033
**Title**: Condition evaluates with state access
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-19

**Test Steps**:
1. Create condition: `"state.x > 10"`
2. Evaluate with state `{"x": 15}`
3. Verify returns True

**Expected Results**:
- Condition evaluates correctly
- State is accessible in expression

---

#### TEA-PY-008.3-UNIT-034
**Title**: Condition evaluates with result access
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-19

**Test Steps**:
1. Create condition: `"result.success == True"`
2. Evaluate with result `{"success": True}`
3. Verify returns True

**Expected Results**:
- Result is accessible in expression
- Evaluation correct

---

#### TEA-PY-008.3-UNIT-035
**Title**: Condition evaluates with variables access
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-19

**Test Steps**:
1. Set engine variables: `{"threshold": 100}`
2. Create condition: `"state.score > variables.threshold"`
3. Evaluate and verify

**Expected Results**:
- Variables accessible as `variables.key`
- Evaluation works correctly

---

#### TEA-PY-008.3-UNIT-036
**Title**: Condition evaluates with secrets access
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-19

**Test Steps**:
1. Set engine secrets: `{"api_key": "secret123"}`
2. Create condition: `"secrets.api_key != ''"`
3. Evaluate and verify

**Expected Results**:
- Secrets accessible as `secrets.key`
- Evaluation correct

---

#### TEA-PY-008.3-UNIT-037
**Title**: Jinja2 expression syntax supported
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-20

**Test Steps**:
1. Create Jinja2 expression: `"state.items | length > 0"`
2. Evaluate with state
3. Verify filter works

**Expected Results**:
- Jinja2 filters work
- Expression evaluates correctly

**Notes**: Ensures Jinja2 template engine is used

---

#### TEA-PY-008.3-INTG-009
**Title**: Template caching for condition expressions
**Level**: integration
**Priority**: P1
**AC Coverage**: AC-21

**Test Steps**:
1. Create EdgeFactory with engine that has template cache
2. Evaluate same condition expression multiple times
3. Verify template is cached (check cache dict)

**Expected Results**:
- First evaluation compiles template
- Subsequent evaluations use cache
- Performance improved

**Notes**: Important for performance in loops

---

#### TEA-PY-008.3-INTG-010
**Title**: Condition evaluation with DotDict context
**Level**: integration
**Priority**: P1
**AC Coverage**: AC-19

**Test Steps**:
1. Verify condition context uses DotDict
2. Test dot notation access: `state.nested.field`
3. Test dict notation access: `state["nested"]["field"]`

**Expected Results**:
- Both access patterns work
- DotDict provides convenient access

---

#### TEA-PY-008.3-E2E-004
**Title**: Complex condition in full workflow
**Level**: e2e
**Priority**: P1
**AC Coverage**: AC-18, AC-19, AC-20

**Test Steps**:
1. Load YAML with complex goto conditions
2. Execute with various state scenarios
3. Verify routing based on conditions

**Test Data**: Use existing `test_yaml_engine_goto.py::test_yaml002_goto_conditional_match`

**Expected Results**:
- All condition types work in real workflow
- Routing matches expected logic

---

### 6. Legacy Edge Support (AC 22-27)

#### TEA-PY-008.3-UNIT-038
**Title**: Normal edges work
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-22

**Test Steps**:
1. Create simple edge config: `{"from": "a", "to": "b"}`
2. Call `add_edge_from_config()`
3. Verify edge added to graph

**Expected Results**:
- Edge added successfully
- No errors

---

#### TEA-PY-008.3-UNIT-039
**Title**: Parallel edges work
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-23

**Test Steps**:
1. Create parallel edge config: `{"from": "a", "to": ["b", "c"], "type": "parallel"}`
2. Call `add_edge_from_config()`
3. Verify parallel edges added

**Expected Results**:
- Multiple parallel edges created
- Fan-out pattern works

---

#### TEA-PY-008.3-UNIT-040
**Title**: Conditional edges with condition: work
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-24

**Test Steps**:
1. Create conditional edge: `{"from": "a", "to": "b", "condition": {"type": "expression", "value": "state.x > 0"}, "when": True}`
2. Process edge
3. Verify conditional edge added

**Expected Results**:
- Conditional edge created correctly
- Condition routing works

---

#### TEA-PY-008.3-UNIT-041
**Title**: when: clause syntactic sugar works
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-25

**Test Steps**:
1. Create edge with `when:` clause
2. Verify it's converted to condition

**Test Data**:
```python
edge = {"from": "a", "to": "b", "when": "state.ready"}
```

**Expected Results**:
- `when` converted to condition expression
- Routing works correctly

---

#### TEA-PY-008.3-UNIT-042
**Title**: when: with negation (!variable)
**Level**: unit
**Priority**: P1
**AC Coverage**: AC-25

**Test Steps**:
1. Create edge: `{"when": "!state.skip"}`
2. Verify negation is handled

**Expected Results**:
- Negation syntax works
- Condition evaluates correctly

---

#### TEA-PY-008.3-UNIT-043
**Title**: Entry edges from __start__ work
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-26

**Test Steps**:
1. Create edge: `{"from": "__start__", "to": "first"}`
2. Process edge
3. Verify entry point set

**Expected Results**:
- Entry point set to "first"
- Graph starts at correct node

---

#### TEA-PY-008.3-UNIT-044
**Title**: type: entry edge syntax works
**Level**: unit
**Priority**: P1
**AC Coverage**: AC-26

**Test Steps**:
1. Create edge: `{"type": "entry", "to": "first"}`
2. Verify equivalent to `from: __start__`

**Expected Results**:
- Entry point set correctly
- Alternative syntax works

---

#### TEA-PY-008.3-UNIT-045
**Title**: Finish edges to __end__ work
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-27

**Test Steps**:
1. Create edge: `{"from": "last", "to": "__end__"}`
2. Process edge
3. Verify finish point set

**Expected Results**:
- Finish point set on "last"
- Graph terminates correctly

---

#### TEA-PY-008.3-UNIT-046
**Title**: type: finish edge syntax works
**Level**: unit
**Priority**: P1
**AC Coverage**: AC-27

**Test Steps**:
1. Create edge: `{"type": "finish", "from": "last"}`
2. Verify equivalent to `to: __end__`

**Expected Results**:
- Finish point set correctly
- Alternative syntax works

---

#### TEA-PY-008.3-UNIT-047
**Title**: Multiple edges from same node
**Level**: unit
**Priority**: P1
**AC Coverage**: AC-22, AC-24

**Test Steps**:
1. Create multiple edges from same source
2. Process all edges
3. Verify conditional routing works

**Test Data**:
```python
edges = [
    {"from": "a", "to": "b", "when": "state.x > 0"},
    {"from": "a", "to": "c", "when": "state.x <= 0"}
]
```

**Expected Results**:
- Multiple outgoing edges created
- Conditional routing works

---

#### TEA-PY-008.3-INTG-011
**Title**: Legacy edges with real StateGraph
**Level**: integration
**Priority**: P0
**AC Coverage**: AC-22, AC-23, AC-24

**Test Steps**:
1. Load YAML with legacy edges section
2. Execute with YAMLEngine
3. Verify all edge types work

**Test Data**: Use existing `test_yaml_engine_edges.py::test_unit_030_simple_edge_creates_connection`

**Expected Results**:
- All legacy edge types work
- Execution matches expected flow

---

#### TEA-PY-008.3-INTG-012
**Title**: Parallel edges execute in parallel
**Level**: integration
**Priority**: P1
**AC Coverage**: AC-23

**Test Steps**:
1. Create parallel edge configuration
2. Execute graph
3. Verify parallel execution

**Expected Results**:
- Multiple branches execute
- Fan-out/fan-in works

---

#### TEA-PY-008.3-INTG-013
**Title**: Conditional start edges work
**Level**: integration
**Priority**: P1
**AC Coverage**: AC-26

**Test Data**: Use existing `test_yaml_engine_edges.py::test_ye7_simple_when_on_start_edge`

**Expected Results**:
- Conditional entry routing works
- Multiple start edge conditions supported

---

#### TEA-PY-008.3-INTG-014
**Title**: when clause in complex scenarios
**Level**: integration
**Priority**: P1
**AC Coverage**: AC-25

**Test Data**: Use existing `test_yaml_engine_edges.py::test_unit_039_when_negation`

**Expected Results**:
- when clause works in all contexts
- Negation syntax supported

---

#### TEA-PY-008.3-E2E-005
**Title**: Full workflow with legacy edges only
**Level**: e2e
**Priority**: P0
**AC Coverage**: AC-22, AC-26, AC-27

**Test Data**: Use existing `test_yaml_engine_goto.py::test_yaml002_legacy_edges_still_work`

**Expected Results**:
- Legacy YAML files work unchanged
- All edge types execute correctly

---

#### TEA-PY-008.3-E2E-006
**Title**: Multi-round workflow with conditional start edges
**Level**: e2e
**Priority**: P1
**AC Coverage**: AC-24, AC-26

**Test Data**: Use existing `test_yaml_engine_edges.py::test_ye7_integration_multi_round_workflow`

**Expected Results**:
- Conditional entry routing works across multiple runs
- Checkpoint/resume pattern works

---

### 7. YAMLEngine Integration (AC 28-31)

#### TEA-PY-008.3-UNIT-048
**Title**: YAMLEngine._edge_factory attribute added
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-28

**Test Steps**:
1. Create YAMLEngine instance
2. Verify `_edge_factory` attribute exists
3. Verify it's an EdgeFactory instance

**Expected Results**:
- `_edge_factory` attribute exists
- Is instance of EdgeFactory

---

#### TEA-PY-008.3-UNIT-049
**Title**: EdgeFactory initialized in YAMLEngine.__init__
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-28

**Test Steps**:
1. Create YAMLEngine
2. Verify EdgeFactory created with correct engine reference
3. Verify `_edge_factory._engine` points back to engine

**Expected Results**:
- EdgeFactory initialized on YAMLEngine creation
- Circular reference established correctly

---

#### TEA-PY-008.3-UNIT-050
**Title**: YAMLEngine._process_goto_and_implicit_edges delegates
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-29

**Test Steps**:
1. Mock EdgeFactory.process_goto_and_implicit_edges
2. Call YAMLEngine._process_goto_and_implicit_edges()
3. Verify delegation occurs

**Expected Results**:
- YAMLEngine method calls EdgeFactory method
- Parameters passed correctly

---

#### TEA-PY-008.3-UNIT-051
**Title**: YAMLEngine._add_edge_from_config delegates
**Level**: unit
**Priority**: P0
**AC Coverage**: AC-30

**Test Steps**:
1. Mock EdgeFactory.add_edge_from_config
2. Call YAMLEngine._add_edge_from_config()
3. Verify delegation

**Expected Results**:
- YAMLEngine method calls EdgeFactory method
- Edge config passed correctly

---

#### TEA-PY-008.3-INTG-015
**Title**: _nodes_with_goto tracking preserved
**Level**: integration
**Priority**: P0
**AC Coverage**: AC-31

**Test Steps**:
1. Load YAML with goto nodes
2. Process with YAMLEngine
3. Verify `YAMLEngine._nodes_with_goto` is populated

**Expected Results**:
- `_nodes_with_goto` attribute exists on YAMLEngine
- Contains correct node names
- Used for precedence logic

---

#### TEA-PY-008.3-INTG-016
**Title**: YAMLEngine.load_from_dict uses EdgeFactory
**Level**: integration
**Priority**: P0
**AC Coverage**: AC-29, AC-30

**Test Steps**:
1. Load complete YAML via load_from_dict
2. Verify EdgeFactory methods are called
3. Verify graph is built correctly

**Expected Results**:
- EdgeFactory processes all edges and gotos
- Graph structure matches configuration

---

#### TEA-PY-008.3-INTG-017
**Title**: No circular import issues
**Level**: integration
**Priority**: P0
**AC Coverage**: AC-28

**Test Steps**:
1. Import yaml_edges module
2. Import yaml_engine module
3. Verify no circular import errors

**Expected Results**:
- Both modules import cleanly
- TYPE_CHECKING pattern prevents circular imports

**Notes**: Critical for module separation

---

#### TEA-PY-008.3-INTG-018
**Title**: EdgeFactory has access to engine context
**Level**: integration
**Priority**: P1
**AC Coverage**: AC-28

**Test Steps**:
1. Set variables and secrets on YAMLEngine
2. Process goto with conditions using variables
3. Verify EdgeFactory can access engine context

**Expected Results**:
- EdgeFactory accesses engine._jinja_env
- Variables and secrets available in conditions

---

### 8. Backward Compatibility (AC 32-34)

#### TEA-PY-008.3-INTG-019
**Title**: All goto tests pass without modification
**Level**: integration
**Priority**: P0
**AC Coverage**: AC-32

**Test Steps**:
1. Run `pytest tests/test_yaml_engine_goto.py -v`
2. Verify all 16 tests pass
3. No test code modifications needed

**Expected Results**:
- All tests pass: ✓ 16/16
- Test behavior identical
- No regressions

**Notes**: Run command: `cd python && pytest tests/test_yaml_engine_goto.py -v`

---

#### TEA-PY-008.3-INTG-020
**Title**: All edge tests pass without modification
**Level**: integration
**Priority**: P0
**AC Coverage**: AC-32

**Test Steps**:
1. Run `pytest tests/test_yaml_engine_edges.py -v`
2. Verify all tests pass
3. No test code modifications

**Expected Results**:
- All tests pass
- Legacy edge functionality preserved

**Notes**: Run command: `cd python && pytest tests/test_yaml_engine_edges.py -v`

---

#### TEA-PY-008.3-INTG-021
**Title**: Full test suite passes
**Level**: integration
**Priority**: P0
**AC Coverage**: AC-32

**Test Steps**:
1. Run complete test suite: `cd python && pytest`
2. Verify 320+ tests pass
3. No new failures

**Expected Results**:
- All tests pass
- No regressions in any module

**Notes**: This is the ultimate backward compatibility check

---

#### TEA-PY-008.3-INTG-022
**Title**: Deprecation warnings still logged for edges section
**Level**: integration
**Priority**: P1
**AC Coverage**: AC-33

**Test Steps**:
1. Load YAML with legacy edges section
2. Capture log output
3. Verify deprecation warning is logged

**Expected Results**:
- Warning logged about edges section
- Message guides users to use goto

**Notes**: Important for migration path

---

#### TEA-PY-008.3-INTG-023
**Title**: Edge behavior identical before/after extraction - simple edges
**Level**: integration
**Priority**: P0
**AC Coverage**: AC-34

**Test Steps**:
1. Run same YAML through old and new code
2. Compare final states
3. Verify identical behavior

**Expected Results**:
- State values identical
- Execution order identical
- No functional changes

---

#### TEA-PY-008.3-INTG-024
**Title**: Edge behavior identical - conditional edges
**Level**: integration
**Priority**: P0
**AC Coverage**: AC-34

**Test Steps**:
1. Test conditional edge routing before/after
2. Compare behavior across multiple state scenarios

**Expected Results**:
- Routing decisions identical
- Condition evaluation unchanged

---

#### TEA-PY-008.3-E2E-007
**Title**: Existing YAML files work unchanged
**Level**: e2e
**Priority**: P0
**AC Coverage**: AC-32, AC-34

**Test Steps**:
1. Load all example YAML files from `examples/` directory
2. Execute each file
3. Verify no errors

**Expected Results**:
- All examples execute successfully
- No breaking changes to user-facing API

---

#### TEA-PY-008.3-E2E-008
**Title**: Goto precedence behavior unchanged
**Level**: e2e
**Priority**: P0
**AC Coverage**: AC-34

**Test Data**: Use existing `test_yaml_engine_goto.py::test_yaml002_goto_precedence_over_edges`

**Expected Results**:
- Precedence rules identical
- goto still wins over edges

---

#### TEA-PY-008.3-E2E-009
**Title**: Mixed goto and edges behavior unchanged
**Level**: e2e
**Priority**: P0
**AC Coverage**: AC-34

**Test Data**: Use existing `test_yaml_engine_goto.py::test_yaml002_mixed_goto_and_edges`

**Expected Results**:
- Mixed configuration works identically
- No behavior changes

---

#### TEA-PY-008.3-E2E-010
**Title**: Complex workflow patterns unchanged
**Level**: e2e
**Priority**: P1
**AC Coverage**: AC-32, AC-34

**Test Steps**:
1. Run integration tests from test_yaml_engine_goto.py
2. Verify self-correction loop pattern works
3. Verify branch-and-merge pattern works

**Test Data**:
- `test_yaml002_self_correction_loop`
- `test_yaml002_branch_and_merge`

**Expected Results**:
- All complex patterns work identically
- No regressions in real-world use cases

---

## Test Execution Strategy

### Phase 1: Unit Tests (Est: 2 hours)
**Priority**: P0 and P1 unit tests
**Goal**: Verify EdgeFactory class works in isolation

**Execution Plan**:
1. Create test file: `python/tests/test_yaml_edges.py`
2. Test EdgeFactory initialization (UNIT-004 to UNIT-011)
3. Test goto processing methods (UNIT-012 to UNIT-023)
4. Test implicit chaining logic (UNIT-024 to UNIT-031)
5. Test condition evaluation (UNIT-032 to UNIT-037)
6. Test legacy edge support (UNIT-038 to UNIT-047)
7. Test YAMLEngine integration points (UNIT-048 to UNIT-051)

**Success Criteria**: All unit tests pass with >90% code coverage of yaml_edges.py

---

### Phase 2: Integration Tests (Est: 1.5 hours)
**Priority**: P0 integration tests
**Goal**: Verify EdgeFactory integrates correctly with YAMLEngine and StateGraph

**Execution Plan**:
1. Run existing goto tests: `pytest tests/test_yaml_engine_goto.py -v`
2. Run existing edge tests: `pytest tests/test_yaml_engine_edges.py -v`
3. Add new integration tests for EdgeFactory (INTG-001 to INTG-018)
4. Verify backward compatibility (INTG-019 to INTG-024)

**Success Criteria**:
- All existing tests pass (16 goto + edge tests)
- New integration tests pass
- No regressions

---

### Phase 3: E2E Tests (Est: 1 hour)
**Priority**: P0 E2E tests
**Goal**: Verify complete workflows work end-to-end

**Execution Plan**:
1. Run full test suite: `cd python && pytest`
2. Execute E2E scenarios (E2E-001 to E2E-010)
3. Test with real YAML files from examples directory

**Success Criteria**:
- All 320+ tests pass
- All example YAML files execute successfully
- No functional changes to user-facing behavior

---

### Phase 4: Performance & Non-Functional (Est: 0.5 hours)
**Priority**: P2 and P3 tests

**Execution Plan**:
1. Verify module line count <= 400 (UNIT-003)
2. Test template caching performance (INTG-009)
3. Verify no circular imports (INTG-017)
4. Check deprecation warnings (INTG-022)

**Success Criteria**:
- Module stays under 400 lines
- Template caching improves performance
- Clean import structure

---

## Risk Matrix

| Risk | Impact | Probability | Mitigation | Test Coverage |
|------|--------|-------------|------------|---------------|
| **Breaking goto precedence** | HIGH | MEDIUM | INTG-006, E2E-008 - test precedence thoroughly | UNIT-019, INTG-006, E2E-008, E2E-009 |
| **Condition evaluation changes** | HIGH | LOW | UNIT-032 to UNIT-037 - test all context types | UNIT-032 to UNIT-037, INTG-009 |
| **Implicit chaining regression** | HIGH | MEDIUM | E2E-003, INTG-007 - compare before/after | UNIT-024 to UNIT-031, E2E-003 |
| **Legacy edge compatibility** | HIGH | LOW | INTG-019 to INTG-021 - run all existing tests | All backward compatibility tests |
| **Circular imports** | MEDIUM | LOW | INTG-017 - test import order | INTG-017 |
| **Performance degradation** | MEDIUM | LOW | INTG-009 - verify caching works | INTG-009 |
| **Missing edge cases** | MEDIUM | MEDIUM | UNIT-023 - test fallback scenarios | UNIT-023, edge case tests |

---

## Test Data Requirements

### Mock Data

**YAMLEngine Mock**:
```python
mock_engine = Mock()
mock_engine._jinja_env = Environment()
mock_engine._template_cache = {}
mock_engine.variables = {"threshold": 100}
mock_engine.secrets = {"api_key": "secret123"}
mock_engine._evaluate_condition = Mock(return_value=True)
```

**StateGraph Mock**:
```python
mock_graph = Mock(spec=StateGraph)
mock_graph.add_edge = Mock()
mock_graph.add_conditional_edges = Mock()
mock_graph.set_entry_point = Mock()
mock_graph.set_finish_point = Mock()
```

### Test YAML Files

**Simple Goto**:
```yaml
name: simple-goto-test
nodes:
  - name: start
    goto: end
    run: 'return {"visited": "start"}'
  - name: middle
    run: 'return {"visited": "middle"}'
  - name: end
    run: 'return {"visited": "end"}'
```

**Conditional Goto**:
```yaml
name: conditional-goto-test
nodes:
  - name: check
    run: 'return {"score": state.get("score", 0)}'
    goto:
      - if: "state.score > 80"
        to: high
      - if: "state.score > 50"
        to: medium
      - to: low
  - name: high
    goto: __end__
    run: 'return {"result": "high"}'
  - name: medium
    goto: __end__
    run: 'return {"result": "medium"}'
  - name: low
    run: 'return {"result": "low"}'
```

**Implicit Chaining**:
```yaml
name: implicit-chain-test
nodes:
  - name: a
    run: 'return {"path": "a"}'
  - name: b
    run: 'return {"path": state["path"] + "->b"}'
  - name: c
    run: 'return {"path": state["path"] + "->c"}'
```

---

## Coverage Goals

| Metric | Target | Measurement |
|--------|--------|-------------|
| **Line Coverage** | >95% | pytest-cov on yaml_edges.py |
| **Branch Coverage** | >90% | pytest-cov --branch |
| **AC Coverage** | 100% | All 34 ACs have tests |
| **Test Pass Rate** | 100% | All 85 scenarios pass |
| **Backward Compatibility** | 100% | All 320+ existing tests pass |

---

## Test Automation

### Pre-commit Hooks
```bash
# .pre-commit-config.yaml
- repo: local
  hooks:
    - id: test-yaml-edges
      name: Test yaml_edges module
      entry: bash -c 'cd python && pytest tests/test_yaml_edges.py -v'
      language: system
      pass_filenames: false

    - id: check-line-count
      name: Check yaml_edges.py line count
      entry: bash -c '[[ $(wc -l < python/src/the_edge_agent/yaml_edges.py) -le 400 ]]'
      language: system
      pass_filenames: false
```

### CI/CD Integration
```yaml
# .github/workflows/python-tests.yaml
- name: Test Edge Factory Module
  run: |
    cd python
    pytest tests/test_yaml_edges.py -v --cov=the_edge_agent.yaml_edges --cov-report=term-missing
    pytest tests/test_yaml_engine_goto.py tests/test_yaml_engine_edges.py -v
```

---

## Definition of Done (Testing Perspective)

- [ ] All 85 test scenarios documented
- [ ] All P0 tests (51 tests) implemented and passing
- [ ] All P1 tests (26 tests) implemented and passing
- [ ] All existing tests pass without modification (320+ tests)
- [ ] Code coverage >95% for yaml_edges.py
- [ ] No circular import issues
- [ ] Module line count <= 400
- [ ] Deprecation warnings working
- [ ] All example YAML files execute successfully
- [ ] Test execution time < 30 seconds for edge/goto tests

---

## Special Testing Considerations

### TEA-YAML-002 Goto Precedence
**Critical Test**: UNIT-019, INTG-006, E2E-008

The goto precedence rule is the most critical aspect of this extraction. We must verify:
1. Goto always wins over legacy edges
2. Legacy edges win over implicit chaining
3. Implicit chaining is the fallback
4. `_nodes_with_goto` tracking is accurate

**Test Strategy**:
- Create test cases with all three edge types competing for same node
- Verify only goto edge is used
- Check `_nodes_with_goto` set is correct

### Implicit Chaining
**Critical Test**: UNIT-026, INTG-007, E2E-003

Implicit chaining is the "magic" that makes YAML simpler. We must verify:
1. Entry point auto-set to first node (when no __start__ edge)
2. Nodes chain to next in list (when no goto/edges)
3. Finish point auto-set to last node (when no __end__ edge)
4. Nodes with goto/edges skip implicit edges

**Test Strategy**:
- Test empty edges section (pure implicit)
- Test mixed implicit and explicit
- Verify precedence rules

### Condition Evaluation Context
**Critical Test**: UNIT-033 to UNIT-036, INTG-010

Conditions must have access to:
- `state` (current state)
- `result` (node execution result)
- `variables` (global variables)
- `secrets` (secret values)

**Test Strategy**:
- Test each context variable independently
- Test combined access
- Verify DotDict for convenient access

---

## Appendix A: Test ID Reference

### By Priority

**P0 (Critical - Must Pass)**: 51 tests
- Module creation and basic functionality
- Core goto, implicit chaining, legacy edges
- Backward compatibility with existing tests

**P1 (High - Should Pass)**: 26 tests
- Edge cases and error handling
- Performance (template caching)
- Complex scenarios

**P2 (Medium - Nice to Have)**: 6 tests
- Documentation checks
- Empty input handling
- Deprecation warnings

**P3 (Low - Optional)**: 2 tests
- Additional validation checks

### By Level

**Unit**: 51 tests
**Integration**: 24 tests
**E2E**: 10 tests

---

## Appendix B: Test Dependencies

```
graph TD
    A[UNIT-004: EdgeFactory class exists] --> B[UNIT-005: Constructor]
    B --> C[UNIT-006: nodes_with_goto]
    B --> D[UNIT-007: process_goto method]
    B --> E[UNIT-008: add_edge method]

    D --> F[INTG-001: StateGraph integration]
    E --> G[INTG-002: Legacy edge integration]

    F --> H[E2E-001: Full goto workflow]
    G --> I[E2E-005: Full legacy workflow]

    H --> J[INTG-019: Backward compat - goto tests]
    I --> K[INTG-020: Backward compat - edge tests]

    J --> L[INTG-021: Full test suite]
    K --> L
```

---

## Appendix C: AC to Test Mapping

| AC # | Description | Test IDs |
|------|-------------|----------|
| AC-1 | Module created | UNIT-001 |
| AC-2 | Comprehensive docstring | UNIT-002 |
| AC-3 | Under 400 lines | UNIT-003 |
| AC-4 | EdgeFactory class | UNIT-004, UNIT-006 |
| AC-5 | Constructor with engine ref | UNIT-005 |
| AC-6 | process_goto method | UNIT-007, UNIT-009, INTG-001 |
| AC-7 | add_edge method | UNIT-008, INTG-002 |
| AC-8 | String goto | UNIT-012, E2E-001 |
| AC-9 | List goto | UNIT-013, INTG-003, INTG-004, E2E-002 |
| AC-10 | Fallback rule | UNIT-014 |
| AC-11 | Target validation | UNIT-015, UNIT-016, UNIT-017, UNIT-021 |
| AC-12 | __end__ target | UNIT-018, E2E-001 |
| AC-13 | Goto precedence | UNIT-019, UNIT-020, INTG-006, E2E-008 |
| AC-14 | Entry point first node | UNIT-024, UNIT-025, INTG-007, E2E-003 |
| AC-15 | Implicit chaining | UNIT-026, INTG-007, E2E-003 |
| AC-16 | Finish point last node | UNIT-027, UNIT-028, E2E-003 |
| AC-17 | Skip implicit for goto/edges | UNIT-029, UNIT-030, INTG-008 |
| AC-18 | evaluate_goto_condition | UNIT-032, E2E-004 |
| AC-19 | Context access | UNIT-033 to UNIT-036, INTG-010, E2E-004 |
| AC-20 | Jinja2 expressions | UNIT-037, E2E-004 |
| AC-21 | Template caching | INTG-009 |
| AC-22 | Normal edges | UNIT-038, UNIT-047, INTG-011, E2E-005 |
| AC-23 | Parallel edges | UNIT-039, INTG-011, INTG-012 |
| AC-24 | Conditional edges | UNIT-040, UNIT-047, INTG-011, INTG-024, E2E-006 |
| AC-25 | when clause | UNIT-041, UNIT-042, INTG-014 |
| AC-26 | Entry edges | UNIT-043, UNIT-044, INTG-013, E2E-005, E2E-006 |
| AC-27 | Finish edges | UNIT-045, UNIT-046, E2E-005 |
| AC-28 | _edge_factory attribute | UNIT-048, UNIT-049, INTG-017, INTG-018 |
| AC-29 | Delegate goto processing | UNIT-050, INTG-016 |
| AC-30 | Delegate edge processing | UNIT-051, INTG-016 |
| AC-31 | nodes_with_goto tracking | INTG-015 |
| AC-32 | Existing tests pass | INTG-019, INTG-020, INTG-021, E2E-007, E2E-010 |
| AC-33 | Deprecation warnings | INTG-022 |
| AC-34 | Identical behavior | INTG-023, INTG-024, E2E-007 to E2E-010 |

---

## Sign-off

**Test Architect**: Quinn
**Date**: 2025-12-27

**Review Status**: Ready for Development

**Next Steps**:
1. Developer implements `yaml_edges.py` module
2. QA implements test scenarios from this document
3. Run Phase 1 (Unit Tests)
4. Run Phase 2 (Integration Tests)
5. Run Phase 3 (E2E Tests)
6. QA Gate assessment

**Estimated Test Implementation Time**: 6 hours
**Estimated Test Execution Time**: 4.5 hours
**Total QA Effort**: 10.5 hours
