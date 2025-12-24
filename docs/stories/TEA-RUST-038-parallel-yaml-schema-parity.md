# Story TEA-RUST-038: Parallel Edge YAML Schema Parity

## Status

Proposed

## Story

**As a** developer writing cross-runtime YAML agents,
**I want** the Rust TEA to accept the same parallel edge YAML syntax as Python,
**so that** my parallel workflow agents work identically in both runtimes without modification.

## Story Context

**Existing System Integration:**

- **Integrates with:** `EdgeConfig` struct in `rust/src/engine/yaml.rs`
- **Technology:** Rust + serde_yaml
- **Follows pattern:** Python's YAML parser behavior
- **Touch points:** `EdgeConfig.parallel` field parsing

**Background:**

The parity test `parallel-isolation.yaml` fails in Rust with:
```
YAML parse error: edges[1].parallel: invalid type: boolean `true`, expected a sequence
```

Python TEA accepts `parallel: true` on individual edges:
```yaml
edges:
  - from: setup
    to: branch_a
    parallel: true
    fan_in: fan_in
  - from: setup
    to: branch_b
    parallel: true
    fan_in: fan_in
```

Rust TEA expects `parallel` to be a list of target nodes:
```yaml
edges:
  - from: setup
    parallel:
      - branch_a
      - branch_b
    fan_in: fan_in
```

This schema incompatibility prevents cross-runtime portable YAML agents using parallel execution.

## Acceptance Criteria

**Functional Requirements:**

1. Rust accepts `parallel: true` on individual edges (Python syntax)
2. Rust continues to accept `parallel: [node1, node2]` list syntax (current Rust syntax)
3. Both syntaxes produce equivalent parallel execution behavior
4. `fan_in` attribute works with both syntaxes

**Parity Requirements:**

5. `examples/prolog/parity/parallel-isolation.yaml` passes in Rust
6. Existing Rust parallel tests continue to pass
7. No changes required to Python implementation

## Tasks / Subtasks

- [ ] **Task 1: Update EdgeConfig to accept both syntaxes** (AC: 1, 2)
  - [ ] Create `ParallelSpec` enum with `Bool(bool)` and `Nodes(Vec<String>)` variants
  - [ ] Use `#[serde(untagged)]` for automatic deserialization
  - [ ] Update `EdgeConfig.parallel` to use `Option<ParallelSpec>`

- [ ] **Task 2: Update edge processing logic** (AC: 3, 4)
  - [ ] In YAML engine, handle `ParallelSpec::Bool(true)` by collecting edges with same `from` node
  - [ ] Group edges by `from` node and `fan_in` target
  - [ ] Create single parallel edge with collected target nodes

- [ ] **Task 3: Update tests** (AC: 5, 6)
  - [ ] Remove `#[ignore]` from `test_parity_parallel_isolation`
  - [ ] Add unit test for `parallel: true` syntax
  - [ ] Verify existing parallel tests still pass

- [ ] **Task 4: Update documentation** (AC: 7)
  - [ ] Document both syntaxes in YAML_REFERENCE.md
  - [ ] Note cross-runtime compatibility

## Dev Notes

### Proposed Implementation

```rust
// In yaml.rs

/// Parallel edge specification - accepts both Python and Rust syntax
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ParallelSpec {
    /// Python syntax: parallel: true (marks edge as part of parallel group)
    Flag(bool),
    /// Rust syntax: parallel: [node1, node2, ...] (explicit target list)
    Nodes(Vec<String>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EdgeConfig {
    pub from: String,
    #[serde(default)]
    pub to: Option<String>,
    #[serde(default)]
    pub condition: Option<String>,
    #[serde(default)]
    pub targets: Option<HashMap<String, String>>,
    #[serde(default)]
    pub parallel: Option<ParallelSpec>,  // Changed from Option<Vec<String>>
    #[serde(default)]
    pub fan_in: Option<String>,
}
```

### Edge Grouping Logic

When processing edges with `parallel: true`:

```rust
// Pseudo-code for edge processing
fn process_edges(edges: Vec<EdgeConfig>) -> Vec<ProcessedEdge> {
    let mut parallel_groups: HashMap<(String, Option<String>), Vec<String>> = HashMap::new();
    let mut result = Vec::new();

    for edge in edges {
        match edge.parallel {
            Some(ParallelSpec::Flag(true)) => {
                // Group by (from_node, fan_in_target)
                let key = (edge.from.clone(), edge.fan_in.clone());
                parallel_groups.entry(key)
                    .or_default()
                    .push(edge.to.unwrap());
            }
            Some(ParallelSpec::Nodes(nodes)) => {
                // Direct parallel edge - use as-is
                result.push(ProcessedEdge::Parallel {
                    from: edge.from,
                    targets: nodes,
                    fan_in: edge.fan_in,
                });
            }
            _ => {
                // Regular edge
                result.push(ProcessedEdge::Simple { ... });
            }
        }
    }

    // Convert grouped parallel edges
    for ((from, fan_in), targets) in parallel_groups {
        result.push(ProcessedEdge::Parallel { from, targets, fan_in });
    }

    result
}
```

### Testing

**Test file:** `rust/tests/test_yaml_engine.rs`

**Key test cases:**
1. `parallel: true` syntax creates parallel execution
2. `parallel: [a, b, c]` syntax continues to work
3. Mixed syntax in same file works
4. `fan_in` grouping works correctly

## Risk and Compatibility Check

**Minimal Risk Assessment:**

- **Primary Risk:** Existing Rust YAML files using `parallel: [...]` might break
- **Mitigation:** `#[serde(untagged)]` tries variants in order; `Vec<String>` should parse first
- **Rollback:** Revert schema changes if issues found

**Compatibility Verification:**

- [ ] No breaking changes to existing Rust parallel syntax
- [ ] Python implementation unchanged
- [ ] Existing tests continue to pass

## Definition of Done

- [ ] Both `parallel: true` and `parallel: [nodes]` syntaxes work in Rust
- [ ] `test_parity_parallel_isolation` passes
- [ ] No regression in existing parallel execution tests
- [ ] Documentation updated

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-23 | 0.1 | Initial story draft from TEA-RUST-037 discovery | James (Dev) |
