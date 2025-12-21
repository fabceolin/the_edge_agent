# TEA-PY-003: While-Loop Node for Autonomous Iteration (Python)

## Story Overview

| Field | Value |
|-------|-------|
| **ID** | TEA-PY-003 |
| **Type** | Story |
| **Priority** | Medium |
| **Estimated Effort** | 5 points |
| **Status** | Not Started |
| **Parent Epic** | TEA-RUST-001 (Python parity story) |
| **Depends On** | Core YamlEngine functionality |
| **Rust Parity** | TEA-RUST-033 |
| **Files to Modify** | `python/src/the_edge_agent/yaml_engine.py`, `python/src/the_edge_agent/stategraph.py` |

## Description

**As a** workflow author building autonomous agents,
**I want** a while-loop node type that iterates until a condition is met,
**So that** I can implement self-refining agents without requiring human intervention between iterations.

## Background

This is the Python implementation of TEA-RUST-033. Both implementations MUST have identical behavior to ensure cross-runtime parity.

### Current State

Python YamlEngine supports:
- Standard nodes with `run:` blocks (Python or Lua)
- `uses:` for built-in actions
- `when:` conditions on edges (Jinja2-based)
- Interrupt-based loops for human-in-the-loop workflows

### Target State

Add `type: while_loop` node support with:
- Jinja2-based condition evaluation
- Sequential body node execution
- `max_iterations` guard against infinite loops
- Cross-runtime parity with Rust implementation

## YAML Syntax

Identical to TEA-RUST-033:

```yaml
nodes:
  - name: refine_until_valid
    type: while_loop
    max_iterations: 10
    condition: "not state.get('is_valid', False)"
    body:
      - name: generate
        uses: llm.call
        with:
          model: gpt-4o
          messages:
            - role: user
              content: "Generate valid JSON for: {{ state.prompt }}"
        output: llm_response

      - name: validate
        run: |
          import json
          try:
              parsed = json.loads(state.get('llm_response', {}).get('content', '{}'))
              return {"parsed_result": parsed, "is_valid": True}
          except:
              return {"is_valid": False}

edges:
  - from: __start__
    to: refine_until_valid
  - from: refine_until_valid
    to: use_result
  - from: use_result
    to: __end__
```

## Acceptance Criteria

### Core Functionality (Must match TEA-RUST-033)

- [ ] **AC-1**: `type: while_loop` nodes are parsed from YAML
- [ ] **AC-2**: Loop condition is evaluated using Jinja2 (same as `when:` conditions)
- [ ] **AC-3**: Loop body nodes execute sequentially on each iteration
- [ ] **AC-4**: Loop exits when condition evaluates to `False`
- [ ] **AC-5**: Loop exits when `max_iterations` is reached (returns last state, no error)
- [ ] **AC-6**: State from each iteration is passed to the next iteration
- [ ] **AC-7**: Final state after loop completion is passed to downstream nodes

### Safety Guards

- [ ] **AC-8**: `max_iterations` is required; YAML parsing fails if missing
- [ ] **AC-9**: `max_iterations` must be positive integer (1-1000 range)
- [ ] **AC-10**: If loop body execution fails, error propagates immediately
- [ ] **AC-11**: Nested while-loops are NOT supported (validation error)

### Events and Observability

- [ ] **AC-12**: `LoopStart` event emitted with `{node_name, max_iterations}`
- [ ] **AC-13**: `LoopIteration` event emitted for each iteration
- [ ] **AC-14**: `LoopEnd` event emitted with `{node_name, iterations_completed, exit_reason}`
- [ ] **AC-15**: Body node events are emitted normally

### Cross-Runtime Parity

- [ ] **AC-16**: Python implementation matches Rust behavior exactly
- [ ] **AC-17**: Same YAML file produces identical results in both runtimes
- [ ] **AC-18**: Cross-runtime test suite validates parity

## Technical Design

### Node Configuration Extension

```python
# In yaml_engine.py

@dataclass
class WhileLoopConfig:
    """Configuration for while-loop nodes."""
    condition: str
    max_iterations: int
    body: List[Dict[str, Any]]

class YamlEngine:
    def _parse_node(self, node_config: Dict[str, Any]) -> Node:
        """Parse a node configuration, handling while_loop type."""
        node_type = node_config.get('type')

        if node_type == 'while_loop':
            return self._parse_while_loop_node(node_config)
        else:
            return self._parse_standard_node(node_config)

    def _parse_while_loop_node(self, config: Dict[str, Any]) -> Node:
        """Parse a while-loop node configuration."""
        name = config['name']
        condition = config.get('condition')
        max_iterations = config.get('max_iterations')
        body = config.get('body', [])

        if not condition:
            raise ValueError(f"while_loop node '{name}' requires 'condition'")
        if not max_iterations:
            raise ValueError(f"while_loop node '{name}' requires 'max_iterations'")
        if not isinstance(max_iterations, int) or max_iterations < 1 or max_iterations > 1000:
            raise ValueError(f"max_iterations must be integer between 1-1000")
        if not body:
            raise ValueError(f"while_loop node '{name}' requires 'body' with at least one node")

        # Parse body nodes
        body_nodes = [self._parse_node(n) for n in body]

        # Check for nested while-loops
        for body_node in body_nodes:
            if hasattr(body_node, 'node_type') and body_node.node_type == 'while_loop':
                raise ValueError(f"Nested while-loops not supported: {name} contains {body_node.name}")

        return Node(
            name=name,
            node_type='while_loop',
            while_loop_config=WhileLoopConfig(
                condition=condition,
                max_iterations=max_iterations,
                body=body_nodes
            )
        )
```

### Execution Logic

```python
# In yaml_engine.py or executor module

async def _execute_while_loop(
    self,
    node: Node,
    state: Dict[str, Any]
) -> Dict[str, Any]:
    """Execute a while-loop node."""
    config = node.while_loop_config

    # Emit LoopStart event
    self._emit_event('LoopStart', {
        'node_name': node.name,
        'max_iterations': config.max_iterations
    })

    iteration = 0
    current_state = state.copy()

    while iteration < config.max_iterations:
        # Evaluate condition using Jinja2
        condition_result = self._evaluate_condition(config.condition, current_state)

        # Emit LoopIteration event
        self._emit_event('LoopIteration', {
            'node_name': node.name,
            'iteration': iteration,
            'condition_result': condition_result
        })

        if not condition_result:
            break

        # Execute body nodes sequentially
        for body_node in config.body:
            current_state = await self._execute_node(body_node, current_state)

        iteration += 1

    # Determine exit reason
    exit_reason = 'max_iterations_reached' if iteration >= config.max_iterations else 'condition_false'

    # Emit LoopEnd event
    self._emit_event('LoopEnd', {
        'node_name': node.name,
        'iterations_completed': iteration,
        'exit_reason': exit_reason
    })

    return current_state
```

## Test Cases

### Unit Tests

```python
def test_while_loop_basic():
    """Test basic while-loop execution."""
    yaml = """
name: counter
nodes:
  - name: count_loop
    type: while_loop
    max_iterations: 5
    condition: "state.count < 3"
    body:
      - name: increment
        run: |
          return {"count": state.get("count", 0) + 1}
edges:
  - from: __start__
    to: count_loop
  - from: count_loop
    to: __end__
"""
    engine = YamlEngine()
    graph = engine.load_from_string(yaml)
    result = list(graph.compile().invoke({"count": 0}))[-1]
    assert result["count"] == 3  # Loop exits when count reaches 3


def test_while_loop_max_iterations():
    """Test max_iterations guard."""
    yaml = """
name: infinite_guard
nodes:
  - name: never_ends
    type: while_loop
    max_iterations: 5
    condition: "True"
    body:
      - name: noop
        run: |
          return {"iterations": state.get("iterations", 0) + 1}
edges:
  - from: __start__
    to: never_ends
  - from: never_ends
    to: __end__
"""
    engine = YamlEngine()
    graph = engine.load_from_string(yaml)
    result = list(graph.compile().invoke({}))[-1]
    assert result["iterations"] == 5  # Stopped by max_iterations


def test_while_loop_missing_max_iterations():
    """Test validation error when max_iterations is missing."""
    yaml = """
name: invalid
nodes:
  - name: no_guard
    type: while_loop
    condition: "True"
    body:
      - name: noop
        run: |
          return {}
"""
    engine = YamlEngine()
    with pytest.raises(ValueError, match="max_iterations"):
        engine.load_from_string(yaml)
```

### Cross-Runtime Parity Tests

```python
def test_cross_runtime_parity():
    """Test that Python and Rust produce identical results."""
    yaml = """
name: parity_test
nodes:
  - name: refine_loop
    type: while_loop
    max_iterations: 3
    condition: "state.quality < 0.8"
    body:
      - name: improve
        run: |
          return {"quality": state.get("quality", 0) + 0.3}
edges:
  - from: __start__
    to: refine_loop
  - from: refine_loop
    to: __end__
"""
    # Run in Python
    py_engine = YamlEngine()
    py_result = list(py_engine.load_from_string(yaml).compile().invoke({"quality": 0}))[-1]

    # Run in Rust (via CLI or FFI)
    rust_result = run_rust_engine(yaml, {"quality": 0})

    assert py_result == rust_result
```

## Dependencies

- Core YamlEngine parsing and execution
- Jinja2 condition evaluation (already implemented for `when:`)

## Related Stories

- **TEA-RUST-033**: Rust implementation (must have identical behavior)
- **TEA-YAML-001**: Jinja2 template engine (condition evaluation)

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2025-12-21 | 1.0 | Initial story creation for Python parity with TEA-RUST-033 | Sarah (PO Agent) |
