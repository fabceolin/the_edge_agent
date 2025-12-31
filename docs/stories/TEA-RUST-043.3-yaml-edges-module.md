# Story TEA-RUST-043.3: Extract Edge Factory Module

## Status
Ready for Dev

> **SM Validation**: ✅ PASS (story-draft-checklist) - 2025-12-27

**Parallel to**: TEA-PY-008.3 (Python equivalent)

## Story

**As a** developer maintaining the Rust YamlEngine codebase,
**I want** edge and goto processing logic extracted into a dedicated `yaml_edges.rs` module,
**so that** the ~380 lines of graph navigation code is isolated and the TEA-YAML-002 implicit graph syntax is maintainable.

## Context

### Existing System Integration

- **Integrates with**: `yaml.rs`, `graph.rs` StateGraph
- **Technology**: Rust 1.75+, petgraph-based StateGraph
- **Follows pattern**: Factory struct with template processor reference
- **Touch points**:
  - `build_graph()` - calls edge processing after node creation
  - `StateGraph.add_edge()`, `add_conditional_edges()`, `add_parallel_edge()`

### Methods to Extract (Lines 511-896, ~385 lines)

| Method | Lines | Purpose |
|--------|-------|---------|
| `process_goto_and_implicit_edges()` | 511-584 | TEA-YAML-002 implicit chaining |
| `process_node_goto()` | 586-666 | Per-node goto processing |
| `add_edge()` | 802-841 | Legacy edge configuration |
| `infer_entry_finish()` | 843-895 | Entry/finish point inference |

### Key Features to Preserve

1. **Precedence rules** (TEA-YAML-002):
   - goto property (highest)
   - edges section (legacy)
   - implicit chaining (lowest)

2. **Implicit entry/exit**:
   - First node is automatic entry point
   - Last node is automatic finish point

3. **Conditional routing**:
   - `goto: [{if: expr, to: target}, ...]`
   - Fallback rule (no condition)

## Acceptance Criteria

### Module Creation
1. New file `rust/src/engine/yaml_edges.rs` created
2. Module has comprehensive rustdoc documentation
3. Module under 450 lines total

### EdgeFactory Struct
4. `EdgeFactory` struct created
5. Constructor accepts template processor reference
6. `process_goto_and_implicit_edges(&self, graph, nodes, edges)` implemented
7. `add_edge(&self, graph, edge_config)` implemented
8. `infer_entry_finish(&self, graph, config)` implemented

### Goto Processing (TEA-YAML-002)
9. String goto (unconditional) works: `goto: "target_node"`
10. List goto (conditional) works: `goto: [{if: expr, to: target}, ...]`
11. Fallback rule (no condition) works
12. Target validation at parse time
13. `__end__` target works for finish points
14. Goto precedence over legacy edges enforced

### Implicit Chaining
15. Entry point set to first node (if no __start__ edge)
16. Implicit chaining to next node for nodes without goto/edges
17. Finish point set for last node (if no __end__ edge)
18. Nodes with explicit edges skip implicit chaining

### Edge Types Supported
19. Normal edges work
20. Parallel edges work
21. Conditional edges with `condition:` work
22. Entry edges from `__start__` work
23. Finish edges to `__end__` work

### YamlEngine Integration
24. `YamlEngine` uses `EdgeFactory` for edge processing
25. `process_goto_and_implicit_edges()` delegates to factory
26. `add_edge()` delegates to factory
27. `infer_entry_finish()` delegates to factory

### Backward Compatibility
28. All existing tests pass without modification
29. Edge behavior identical before/after extraction
30. Error messages unchanged

## Tasks / Subtasks

- [ ] **Task 1: Create yaml_edges.rs module** (AC: 1-3)
  - [ ] Create `rust/src/engine/yaml_edges.rs`
  - [ ] Add module documentation with examples
  - [ ] Add required imports (graph, yaml_config)
  - [ ] Add module to `mod.rs`

- [ ] **Task 2: Implement EdgeFactory struct** (AC: 4-8)
  - [ ] Create `EdgeFactory` struct
  - [ ] Add constructor with template processor reference
  - [ ] Move `process_goto_and_implicit_edges()` logic
  - [ ] Move `process_node_goto()` as private method
  - [ ] Move `add_edge()` logic
  - [ ] Move `infer_entry_finish()` logic

- [ ] **Task 3: Preserve goto processing** (AC: 9-14)
  - [ ] Test string goto (unconditional)
  - [ ] Test list goto (conditional)
  - [ ] Test fallback rules
  - [ ] Verify target validation
  - [ ] Test __end__ target
  - [ ] Verify precedence order

- [ ] **Task 4: Preserve implicit chaining** (AC: 15-18)
  - [ ] Test automatic entry point
  - [ ] Test implicit chaining
  - [ ] Test automatic finish point
  - [ ] Verify skip for explicit edges

- [ ] **Task 5: Support all edge types** (AC: 19-23)
  - [ ] Test normal edges
  - [ ] Test parallel edges
  - [ ] Test conditional edges
  - [ ] Test entry/finish edges

- [ ] **Task 6: Update YamlEngine integration** (AC: 24-27)
  - [ ] Import EdgeFactory in yaml.rs
  - [ ] Create EdgeFactory in `build_graph()`
  - [ ] Update all edge processing to delegate

- [ ] **Task 7: Verify backward compatibility** (AC: 28-30)
  - [ ] Run edge tests: `cargo test edge`
  - [ ] Run goto tests: `cargo test goto`
  - [ ] Run parallel tests: `cargo test parallel`

## Dev Notes

### EdgeFactory Pattern

```rust
// yaml_edges.rs
//! Edge and goto processing for YAML workflows.
//!
//! Implements TEA-YAML-002 implicit graph navigation syntax.

use std::collections::HashSet;

use crate::engine::graph::{Edge, StateGraph};
use crate::engine::yaml_config::{EdgeConfig, Goto, GotoRule, NodeConfig, YamlConfig};
use crate::engine::yaml_templates::TemplateProcessor;
use crate::error::{TeaError, TeaResult};
use crate::{END, START};

/// Factory for processing edges and goto navigation.
pub struct EdgeFactory<'a> {
    /// Reference to template processor for condition evaluation
    template_processor: &'a TemplateProcessor,
    /// Set of node names that have explicit goto definitions
    nodes_with_goto: HashSet<String>,
    /// Set of node names that have explicit edge definitions
    nodes_with_edges: HashSet<String>,
}

impl<'a> EdgeFactory<'a> {
    /// Create a new edge factory.
    pub fn new(template_processor: &'a TemplateProcessor) -> Self {
        Self {
            template_processor,
            nodes_with_goto: HashSet::new(),
            nodes_with_edges: HashSet::new(),
        }
    }

    /// Process goto properties and implicit chaining for all nodes.
    ///
    /// TEA-YAML-002: Implements the implicit graph navigation syntax.
    pub fn process_goto_and_implicit_edges(
        &mut self,
        graph: &mut StateGraph,
        nodes: &[NodeConfig],
        edges: &[EdgeConfig],
    ) -> TeaResult<()> {
        // Track which nodes have explicit edges
        for edge in edges {
            self.nodes_with_edges.insert(edge.from.clone());
        }

        let node_names: Vec<String> = nodes.iter().map(|n| n.name.clone()).collect();

        // Process goto for each node
        for (i, node) in nodes.iter().enumerate() {
            if let Some(ref goto) = node.goto {
                self.process_node_goto(graph, &node.name, goto, &node_names)?;
                self.nodes_with_goto.insert(node.name.clone());
            } else if !self.nodes_with_edges.contains(&node.name) {
                // Implicit chaining to next node
                if i + 1 < nodes.len() {
                    graph.add_edge(&node.name, &nodes[i + 1].name)?;
                } else {
                    // Last node goes to END
                    graph.add_edge(&node.name, END)?;
                }
            }
        }

        Ok(())
    }

    /// Process goto for a single node.
    fn process_node_goto(
        &self,
        graph: &mut StateGraph,
        node_name: &str,
        goto: &Goto,
        valid_nodes: &[String],
    ) -> TeaResult<()> {
        match goto {
            Goto::Simple(target) => {
                self.validate_target(target, valid_nodes)?;
                graph.add_edge(node_name, target)?;
            }
            Goto::Conditional(rules) => {
                let mut conditions = Vec::new();
                let mut fallback = None;

                for rule in rules {
                    self.validate_target(&rule.to, valid_nodes)?;

                    if let Some(ref condition) = rule.if_condition {
                        conditions.push((condition.clone(), rule.to.clone()));
                    } else {
                        // Fallback (no condition)
                        fallback = Some(rule.to.clone());
                    }
                }

                // Add conditional edges
                if !conditions.is_empty() {
                    graph.add_conditional_edges(node_name, conditions, fallback)?;
                } else if let Some(target) = fallback {
                    graph.add_edge(node_name, &target)?;
                }
            }
        }

        Ok(())
    }

    /// Validate that a target node exists.
    fn validate_target(&self, target: &str, valid_nodes: &[String]) -> TeaResult<()> {
        if target == END || target == "__end__" {
            return Ok(());
        }
        if !valid_nodes.contains(&target.to_string()) {
            return Err(TeaError::Validation(format!(
                "goto target '{}' does not exist",
                target
            )));
        }
        Ok(())
    }

    /// Add an edge from YAML configuration.
    pub fn add_edge(&self, graph: &mut StateGraph, config: &EdgeConfig) -> TeaResult<()> {
        // ... implementation
    }

    /// Infer entry and finish points if not explicit.
    pub fn infer_entry_finish(
        &self,
        graph: &mut StateGraph,
        config: &YamlConfig,
    ) -> TeaResult<()> {
        // Check for explicit __start__ edge
        let has_start_edge = config.edges.iter().any(|e| e.from == START || e.from == "__start__");

        if !has_start_edge && !config.nodes.is_empty() {
            // First node is entry point
            graph.set_entry_point(&config.nodes[0].name)?;
        }

        // Check for explicit __end__ edge
        let has_end_edge = config.edges.iter().any(|e| e.to == END || e.to == "__end__");

        if !has_end_edge && !config.nodes.is_empty() {
            // Last node (if not already handled) goes to END
            let last_node = &config.nodes.last().unwrap().name;
            if !self.nodes_with_goto.contains(last_node)
                && !self.nodes_with_edges.contains(last_node)
            {
                graph.set_finish_point(last_node)?;
            }
        }

        Ok(())
    }
}
```

### Integration in YamlEngine

```rust
// yaml.rs
mod yaml_edges;
use yaml_edges::EdgeFactory;

impl YamlEngine {
    fn build_graph(&self, config: YamlConfig) -> TeaResult<StateGraph> {
        let mut graph = StateGraph::new();
        let node_factory = NodeFactory::new(&self.template_processor);

        // Add nodes
        for node_config in &config.nodes {
            let node = node_factory.build_node(node_config)?;
            graph.add_node(node);
        }

        // Process edges
        let mut edge_factory = EdgeFactory::new(&self.template_processor);
        edge_factory.process_goto_and_implicit_edges(&mut graph, &config.nodes, &config.edges)?;

        // Add legacy edges
        for edge_config in &config.edges {
            edge_factory.add_edge(&mut graph, edge_config)?;
        }

        // Infer entry/finish
        edge_factory.infer_entry_finish(&mut graph, &config)?;

        Ok(graph)
    }
}
```

### Precedence Order

```
1. goto property (highest) - Always takes precedence
2. edges section - Legacy explicit edges
3. implicit chaining (lowest) - Only if no goto or edges
```

### Testing Requirements

- **Test file locations**: `rust/src/engine/yaml.rs` (inline tests)
- **Key test functions**: `test_conditional_start_edges`, `test_parse_parallel_edges`
- **Run command**: `cd rust && cargo test edge goto parallel`

## Definition of Done

- [ ] yaml_edges.rs created with EdgeFactory struct
- [ ] All edge processing methods moved and working
- [ ] YamlEngine delegates to EdgeFactory
- [ ] Goto precedence rules preserved
- [ ] All tests pass without modification
- [ ] No clippy warnings
- [ ] Module under 450 lines

## Risk Assessment

- **Primary Risk**: Breaking goto precedence logic
- **Mitigation**: Preserve exact precedence implementation, comprehensive tests

- **Secondary Risk**: Implicit chaining edge cases
- **Mitigation**: Test single-node, no-edges, and mixed scenarios

- **Rollback**: Git revert single commit

## Dependency

- **Depends on**: TEA-RUST-043.1 (TemplateProcessor for condition evaluation)
- **Depends on**: TEA-RUST-043.4 (EdgeConfig, Goto, GotoRule struct definitions)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-27 | 0.1 | Initial story creation from TEA-PY-008.3 | Sarah (PO) |
