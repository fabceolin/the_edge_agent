# Navigation and Flow Control

> **Parent document:** [YAML Reference](../YAML_REFERENCE.md)
> **Epic:** [DOC-002](../../stories/DOC-002-yaml-reference-modularization.md)

## Overview

TEA supports two navigation approaches: the modern **implicit/goto** syntax (recommended) and the legacy **edges** section (deprecated for sequential flows). This document covers both approaches.

---

## Table of Contents

- [Implicit Chaining (Default)](#implicit-chaining-default)
- [The `goto` Property](#the-goto-property)
  - [Unconditional goto](#unconditional-goto)
  - [Special goto Targets](#special-goto-targets)
- [Conditional `goto`](#conditional-goto)
  - [Context Variables in Conditions](#context-variables-in-conditions)
- [Loops with `goto`](#loops-with-goto)
- [Navigation Precedence](#navigation-precedence)
- [Migration from Edges](#migration-from-edges)
- [Edge Specification (Deprecated)](#edge-specification-deprecated)
  - [Basic Structure](#basic-structure)
  - [Edge Types](#edge-types)
  - [Parallel Edges](#parallel-edges)

---

## Implicit Chaining (Default)

By default, nodes execute in the order they are defined. After a node completes:
1. If the node has a `goto` property, use it
2. Otherwise, proceed to the next node in the list
3. If it's the last node, the workflow ends (`__end__`)

```yaml
name: implicit-flow
nodes:
  - name: step_a
    run: |
      return {"message": "Step A done"}

  - name: step_b
    run: |
      return {"message": "Step B done"}

  - name: step_c
    run: |
      return {"message": "Step C done"}

# No edges needed - implicit flow: step_a -> step_b -> step_c -> __end__
```

---

## The `goto` Property

The `goto` property on a node specifies the next node to execute. It can be:
- **String**: Unconditional jump to a specific node
- **Array**: Conditional jump with if/to rules

### Unconditional `goto`

Jump directly to a named node:

```yaml
nodes:
  - name: start
    run: |
      return {"initialized": True}
    goto: validate  # Skip to validate, not next node

  - name: skipped_node
    run: |
      return {"this": "is skipped"}

  - name: validate
    run: |
      return {"validated": True}
```

### Special `goto` Targets

| Target | Description |
|--------|-------------|
| `"__end__"` | Terminate workflow immediately |
| Node name | Jump to the named node |
| (omitted) | Use implicit chaining (next in list) |

---

## Conditional `goto`

Use a list of rules for conditional branching. Each rule has:
- `if` (optional): Boolean expression to evaluate
- `to` (required): Target node if condition is true

Rules are evaluated in order; the first matching rule wins.

```yaml
nodes:
  - name: validate
    run: |
      score = check_quality(state["input"])
      return {"score": score}
    goto:
      - if: "state.score > 0.9"
        to: high_confidence
      - if: "state.score > 0.5"
        to: medium_confidence
      - to: low_confidence  # Fallback (no condition = always true)

  - name: high_confidence
    run: |
      return {"path": "high"}
    goto: __end__  # Terminate early

  - name: medium_confidence
    run: |
      return {"path": "medium"}

  - name: low_confidence
    run: |
      return {"path": "low"}
```

### Context Variables in Conditions

| Variable | Description |
|----------|-------------|
| `state` | Global agent state (includes merged node results) |
| `variables` | Template variables from YAML `variables:` section |
| `secrets` | Secret values (if configured) |

> **Note:** Node execution results are automatically merged into `state` before `goto` evaluation. Access returned values via `state.field_name` (e.g., if node returns `{"score": 90}`, use `state.score`).

---

## Loops with `goto`

Use conditional `goto` to create loops:

```yaml
nodes:
  - name: retry_step
    run: |
      result = attempt_operation()
      attempts = state.get("attempts", 0) + 1
      return {"status": result.status, "attempts": attempts}
    goto:
      - if: "state.status == 'error' and state.attempts < 3"
        to: retry_step  # Loop back
      - if: "state.status == 'ok'"
        to: success
      - to: failure  # Max retries exceeded

  - name: success
    run: |
      return {"final": "success"}
    goto: __end__

  - name: failure
    run: |
      return {"final": "failed"}
    goto: __end__
```

---

## Navigation Precedence

When multiple navigation methods are present, precedence is:

1. **`goto` property on node** (highest priority)
2. **`edges` section** (legacy, deprecated)
3. **Implicit chaining** (next node in list)

```yaml
# Example: goto takes precedence over edges
nodes:
  - name: step_a
    goto: step_c  # This wins
  - name: step_b
  - name: step_c
edges:
  - from: step_a
    to: step_b  # Ignored because goto exists
```

---

## Migration from Edges

To migrate from the legacy `edges` format:

| Legacy Pattern | New Syntax |
|----------------|------------|
| Linear edges (A→B→C) | Remove edges, use implicit chaining |
| `from: __start__` | Not needed, first node is entry point |
| `to: __end__` | Not needed for last node, or use `goto: __end__` |
| Conditional edges | Use `goto:` list with `if`/`to` rules |
| Unconditional jump | Use `goto: target_node` |
| **Parallel edges** | **Keep as-is** (not deprecated) |

> **Note:** Parallel edges with `parallel: true` and `fan_in:` are not deprecated and should remain in the `edges` section. Only sequential navigation edges are being migrated to `goto`.

See [TEA-YAML-002](../../stories/TEA-YAML-002-implicit-graph-goto-syntax.md) for the full migration guide and LLM prompt.

---

## Edge Specification (Deprecated)

> **Deprecation Notice:** Sequential edges are deprecated in favor of implicit chaining and `goto` properties. See sections above for the modern syntax.
>
> **Exception:** Parallel edges (`parallel: true`, `fan_in:`) are **not deprecated**. They remain the only way to define fan-out/fan-in execution patterns and will continue to be supported.
>
> **Deprecation Roadmap (Sequential Edges Only):**
> - **v0.8.x (Current)**: Sequential `edges` work normally, emit INFO-level warning
> - **v1.0.x**: Sequential `edges` work, emit WARNING-level warning with migration link
> - **v2.0.x**: Sequential `edges` rejected with error, must use `goto` or implicit

### Basic Structure

```yaml
edges:
  - from: string          # Source node (or __start__)
    to: string            # Target node (or __end__)

    # Optional:
    type: string          # Edge type: normal | parallel
    condition: object     # Conditional routing
    when: any             # Simple condition shorthand
    fan_in: string        # Fan-in node for parallel edges
```

### Edge Types

#### Simple Edge

```yaml
- from: node_a
  to: node_b
```

#### Entry Point

```yaml
- from: __start__
  to: first_node
```

#### Finish Point

```yaml
- from: last_node
  to: __end__
```

#### Conditional Edge

Route based on expression evaluation:

```yaml
# Method 1: Expression condition
- from: validate
  to: process
  condition:
    type: expression
    value: state["is_valid"] == True
  when: true

- from: validate
  to: error_handler
  condition:
    type: expression
    value: state["is_valid"] == True
  when: false

# Method 2: Simple when clause
- from: check
  to: proceed
  when: "state['count'] > 0"

# Method 3: Variable reference with negation
- from: check
  to: skip
  when: "!should_process"
```

### Parallel Edges

Execute flows concurrently (NOT deprecated):

```yaml
# Define parallel flows
- from: start
  to: flow_a
  type: parallel
  fan_in: combine

- from: start
  to: flow_b
  type: parallel
  fan_in: combine

- from: start
  to: flow_c
  type: parallel
  fan_in: combine

# Continue after fan-in
- from: combine
  to: next_step
```

Parallel edges require:
- `type: parallel` on each parallel branch
- `fan_in: node_name` specifying the node that collects results
- A fan-in node marked with `fan_in: true` in the nodes section

---

## See Also

- [Node Specification](./nodes.md) - Node configuration and execution methods
- [Template Syntax](./templates.md) - Variable interpolation in conditions
- [Actions Overview](./actions/README.md) - Built-in action reference
