# TEA-RUST-033: While-Loop Node for Autonomous Iteration

## Story Overview

| Field | Value |
|-------|-------|
| **ID** | TEA-RUST-033 |
| **Type** | Story |
| **Priority** | Medium |
| **Estimated Effort** | 5 points |
| **Status** | Not Started |
| **Parent Epic** | TEA-RUST-001 |
| **Depends On** | TEA-RUST-004 (Node Execution), TEA-RUST-029 (Tera Conditions) |
| **Python Parity** | TEA-PY-003 |
| **Files to Modify** | `rust/src/engine/graph.rs`, `rust/src/engine/executor.rs`, `rust/src/engine/yaml.rs` |

## Description

**As a** workflow author building autonomous agents,
**I want** a while-loop node type that iterates until a condition is met,
**So that** I can implement self-refining agents without requiring human intervention between iterations.

## Background

### Current State

TEA supports two iteration patterns:
1. **Interrupt-based loops** - Human-in-the-loop, state persisted between API calls (documented in epic)
2. **No native while-loop** - For autonomous iteration, users must manually unroll loops or use external orchestration

### Problem

Autonomous agents often need to iterate until a quality threshold is met:
- LLM refines output until it passes validation
- Data extraction retries until all fields are populated
- Research agents continue until sufficient sources are found

Without a while-loop construct, these patterns require:
- Complex edge routing with state flags
- Manual iteration counting in every node
- Risk of infinite loops without guards

### Solution

Add a `type: while_loop` node that:
1. Evaluates a condition before each iteration
2. Executes a body of sub-nodes
3. Enforces `max_iterations` to prevent infinite loops
4. Maintains DAG semantics (the loop is internal to the node)

## YAML Syntax

```yaml
nodes:
  - name: refine_until_valid
    type: while_loop
    max_iterations: 10           # Required: prevent infinite loops
    condition: "not state.get('is_valid', False)"  # Tera expression
    body:
      - name: generate
        uses: llm.call
        with:
          model: gpt-4o
          messages:
            - role: user
              content: "Generate a valid JSON response for: {{ state.prompt }}"
        output: llm_response

      - name: validate
        run: |
          import json
          try:
              parsed = json.loads(state.get('llm_response', {}).get('content', '{}'))
              return {"parsed_result": parsed, "is_valid": True}
          except:
              return {"is_valid": False, "validation_error": "Invalid JSON"}

edges:
  - from: __start__
    to: refine_until_valid
  - from: refine_until_valid
    to: use_result
  - from: use_result
    to: __end__
```

## Acceptance Criteria

### Core Functionality

- [ ] **AC-1**: `type: while_loop` nodes are parsed from YAML and stored in `Node` struct
- [ ] **AC-2**: Loop condition is evaluated using Tera (same as `when:` conditions)
- [ ] **AC-3**: Loop body nodes execute sequentially on each iteration
- [ ] **AC-4**: Loop exits when condition evaluates to `false`
- [ ] **AC-5**: Loop exits when `max_iterations` is reached (returns last state, no error)
- [ ] **AC-6**: State from each iteration is passed to the next iteration
- [ ] **AC-7**: Final state after loop completion is passed to downstream nodes

### Safety Guards

- [ ] **AC-8**: `max_iterations` is required; YAML parsing fails if missing
- [ ] **AC-9**: `max_iterations` must be positive integer (1-1000 range)
- [ ] **AC-10**: If loop body execution fails, error propagates immediately (no retry by loop itself)
- [ ] **AC-11**: Nested while-loops are NOT supported (validation error if attempted)

### Events and Observability

- [ ] **AC-12**: `LoopStart` event emitted with `{node_name, max_iterations}`
- [ ] **AC-13**: `LoopIteration` event emitted for each iteration `{node_name, iteration, condition_result}`
- [ ] **AC-14**: `LoopEnd` event emitted with `{node_name, iterations_completed, exit_reason}`
- [ ] **AC-15**: Body node events are emitted normally (NodeStart, NodeComplete, etc.)

### Cross-Runtime Parity

- [ ] **AC-16**: Rust implementation matches Python behavior exactly
- [ ] **AC-17**: Same YAML file produces identical results in both runtimes
- [ ] **AC-18**: Python implementation in TEA-PY-003 uses same syntax

## Technical Design

### Rust Implementation

#### Node Struct Extension

```rust
// In graph.rs
pub enum NodeType {
    Standard,
    WhileLoop {
        condition: String,      // Tera expression
        max_iterations: usize,
        body: Vec<Node>,        // Sub-nodes to execute
    },
}

pub struct Node {
    pub name: String,
    pub node_type: NodeType,
    // ... existing fields
}
```

#### YAML Parsing

```rust
// In yaml.rs
#[derive(Deserialize)]
struct NodeConfig {
    name: String,
    #[serde(rename = "type")]
    node_type: Option<String>,
    max_iterations: Option<usize>,
    condition: Option<String>,
    body: Option<Vec<NodeConfig>>,  // Sub-nodes for while_loop
    // ... existing fields
}

fn parse_node(&self, config: &NodeConfig) -> TeaResult<Node> {
    match config.node_type.as_deref() {
        Some("while_loop") => {
            let max_iterations = config.max_iterations
                .ok_or_else(|| TeaError::Yaml("while_loop requires max_iterations".into()))?;
            let condition = config.condition.clone()
                .ok_or_else(|| TeaError::Yaml("while_loop requires condition".into()))?;
            let body = config.body.as_ref()
                .ok_or_else(|| TeaError::Yaml("while_loop requires body".into()))?
                .iter()
                .map(|n| self.parse_node(n))
                .collect::<TeaResult<Vec<_>>>()?;

            Ok(Node {
                name: config.name.clone(),
                node_type: NodeType::WhileLoop { condition, max_iterations, body },
                ..Default::default()
            })
        }
        _ => self.parse_standard_node(config),
    }
}
```

#### Executor Loop Logic

```rust
// In executor.rs
async fn execute_node(&mut self, node: &Node, state: JsonValue) -> TeaResult<JsonValue> {
    match &node.node_type {
        NodeType::WhileLoop { condition, max_iterations, body } => {
            self.execute_while_loop(&node.name, condition, *max_iterations, body, state).await
        }
        NodeType::Standard => {
            self.execute_standard_node(node, state).await
        }
    }
}

async fn execute_while_loop(
    &mut self,
    name: &str,
    condition: &str,
    max_iterations: usize,
    body: &[Node],
    mut state: JsonValue,
) -> TeaResult<JsonValue> {
    self.emit_event(ExecutionEvent::LoopStart {
        node_name: name.to_string(),
        max_iterations,
    });

    let mut iteration = 0;

    while iteration < max_iterations {
        // Evaluate condition
        let condition_result = self.yaml_engine.evaluate_condition(condition, &state)?;

        self.emit_event(ExecutionEvent::LoopIteration {
            node_name: name.to_string(),
            iteration,
            condition_result,
        });

        if !condition_result {
            break;
        }

        // Execute body nodes sequentially
        for body_node in body {
            state = self.execute_node(body_node, state).await?;
        }

        iteration += 1;
    }

    let exit_reason = if iteration >= max_iterations {
        "max_iterations_reached"
    } else {
        "condition_false"
    };

    self.emit_event(ExecutionEvent::LoopEnd {
        node_name: name.to_string(),
        iterations_completed: iteration,
        exit_reason: exit_reason.to_string(),
    });

    Ok(state)
}
```

### Graph Validation

While-loop nodes are treated as atomic units for graph validation:
- The loop body is NOT added to the main graph
- Edges connect TO and FROM the while-loop node, not its body
- Cycle detection still applies to the main graph (while-loop is not a cycle)

```rust
fn validate(&self) -> TeaResult<()> {
    // Existing validation...

    // Check for nested while-loops
    for node in self.nodes() {
        if let NodeType::WhileLoop { body, .. } = &node.node_type {
            for body_node in body {
                if matches!(body_node.node_type, NodeType::WhileLoop { .. }) {
                    return Err(TeaError::Validation(
                        format!("Nested while-loops not supported: {} contains {}",
                                node.name, body_node.name)
                    ));
                }
            }
        }
    }

    Ok(())
}
```

## Test Cases

### Unit Tests

```rust
#[test]
fn test_while_loop_basic() {
    let yaml = r#"
name: counter
nodes:
  - name: count_loop
    type: while_loop
    max_iterations: 5
    condition: "state.count < 3"
    body:
      - name: increment
        run: |
          return { count = state.count + 1 }
edges:
  - from: __start__
    to: count_loop
  - from: count_loop
    to: __end__
"#;
    let graph = StateGraph::from_yaml(yaml).unwrap();
    let result = execute(graph, json!({"count": 0})).unwrap();
    assert_eq!(result["count"], 3);  // Loop exits when count reaches 3
}

#[test]
fn test_while_loop_max_iterations() {
    let yaml = r#"
name: infinite_guard
nodes:
  - name: never_ends
    type: while_loop
    max_iterations: 5
    condition: "true"  # Always true
    body:
      - name: noop
        run: |
          return { iterations = (state.iterations or 0) + 1 }
edges:
  - from: __start__
    to: never_ends
  - from: never_ends
    to: __end__
"#;
    let graph = StateGraph::from_yaml(yaml).unwrap();
    let result = execute(graph, json!({})).unwrap();
    assert_eq!(result["iterations"], 5);  // Stopped by max_iterations
}

#[test]
fn test_while_loop_missing_max_iterations() {
    let yaml = r#"
name: invalid
nodes:
  - name: no_guard
    type: while_loop
    condition: "true"
    body:
      - name: noop
        run: |
          return {}
"#;
    let result = StateGraph::from_yaml(yaml);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("max_iterations"));
}
```

### Integration Tests

```rust
#[test]
fn test_while_loop_with_llm() {
    // Test with mocked LLM that returns valid JSON on 3rd attempt
}

#[test]
fn test_while_loop_events() {
    // Verify LoopStart, LoopIteration, LoopEnd events are emitted
}
```

## Out of Scope

- **Nested while-loops**: Adds complexity, can be added later if needed
- **Break/continue statements**: Keep it simple for v1
- **Parallel loop iterations**: Each iteration is sequential
- **Loop-specific error handling**: Uses standard node error handling

## Dependencies

- **TEA-RUST-004** (Node Execution): Core node execution infrastructure
- **TEA-RUST-029** (Tera Conditions): Condition evaluation for loop condition

## Related Stories

- **TEA-PY-003**: Python implementation (must have identical behavior)
- **TEA-RUST-008**: Error handling (applies to loop body failures)

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2025-12-21 | 1.0 | Initial story creation from PO elicitation session | Sarah (PO Agent) |
