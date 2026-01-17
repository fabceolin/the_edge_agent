# Story TEA-WASM-001.5: Simulated Parallel Execution

## Status
Draft

## Epic
[TEA-WASM-001: WASM YAML Engine Expansion](./TEA-WASM-001-yaml-engine-expansion.md)

## Story

**As a** browser-based agent developer,
**I want** tea-wasm-llm to support fan-out/fan-in patterns via async execution,
**so that** I can model parallel workflows even though they execute sequentially in WASM.

## Acceptance Criteria

### Core Functionality
1. Detect parallel edge patterns (multiple edges from same source node)
2. Execute "parallel" branches sequentially via async
3. Aggregate results into `parallel_results` array for fan-in node
4. Fan-in node receives combined state from all branches
5. Execution order is deterministic (edge definition order)
6. Works correctly with conditional parallel branches
7. Nested parallel patterns work correctly

### State Merge Strategy (Critical - TECH-001)
8. Default merge strategy is "isolated" - branches do NOT merge state, only `parallel_results` array is provided
9. Optional `merge_strategy` config allows: `isolated` (default), `last_write_wins`, `merge_deep`, `fail_on_conflict`
10. When `merge_strategy: fail_on_conflict`, conflicting keys produce clear error with branch names and key path
11. `parallel_results` array preserves complete branch state for manual merge in fan-in node
12. Documentation clearly explains merge behavior and recommends patterns

## Tasks / Subtasks

- [ ] Detect parallel patterns (AC: 1)
  - [ ] Identify nodes with multiple outgoing edges
  - [ ] Mark edges as part of parallel group
  - [ ] Find fan-in node (node with multiple incoming edges)

- [ ] Implement parallel execution (AC: 2, 5)
  - [ ] Execute each branch sequentially
  - [ ] Clone state for each branch
  - [ ] Maintain edge definition order

- [ ] Implement result aggregation (AC: 3, 4)
  - [ ] Create `parallel_results: Vec<JsonValue>` array
  - [ ] Collect final state from each branch
  - [ ] Inject `parallel_results` into fan-in node's state

- [ ] Handle conditional parallels (AC: 6)
  - [ ] Evaluate conditions for each parallel edge
  - [ ] Only execute branches where condition is true
  - [ ] Still aggregate results from executed branches

- [ ] Handle nested parallels (AC: 7)
  - [ ] Support parallel within parallel
  - [ ] Correctly scope `parallel_results` for each level
  - [ ] Test with nested patterns

- [ ] **Implement state merge strategy (AC: 8, 9, 10, 11) - CRITICAL (TECH-001)**
  - [ ] Define `MergeStrategy` enum: `Isolated`, `LastWriteWins`, `MergeDeep`, `FailOnConflict`
  - [ ] Add `merge_strategy` field to parallel group config (default: `Isolated`)
  - [ ] Implement `Isolated` strategy - fan-in receives original state + `parallel_results` only
  - [ ] Implement `LastWriteWins` strategy - branches merge in order, later overwrites earlier
  - [ ] Implement `MergeDeep` strategy - recursively merge objects, arrays concatenate
  - [ ] Implement `FailOnConflict` strategy - error if any key modified by multiple branches
  - [ ] Create `detect_conflicts(branch_states: &[JsonValue], base: &JsonValue) -> Vec<ConflictInfo>`
  - [ ] Error messages include: conflicting key path, branch names, conflicting values

- [ ] **Implement conflict detection (AC: 10)**
  - [ ] Track which keys each branch modified (diff from base state)
  - [ ] Compare modified key sets across branches
  - [ ] Generate `ConflictInfo` with full context for errors
  - [ ] Log warnings for `LastWriteWins` when conflicts occur

- [ ] **Document merge behavior (AC: 12)**
  - [ ] Add "Merge Strategy" section to YAML reference
  - [ ] Document each strategy with examples
  - [ ] Recommend `Isolated` + manual merge pattern for safety
  - [ ] Provide examples of fan-in node merge templates

## Dev Notes

### Relevant Source Tree
```
rust/tea-wasm-llm/
├── src/
│   ├── lib.rs
│   ├── executor.rs     # From Story 4 - extend with parallel
│   ├── routing.rs      # From Story 3
│   └── parallel.rs     # NEW: Parallel execution logic
└── Cargo.toml
```

### YAML Parallel Pattern
```yaml
name: parallel-example
nodes:
  - name: start
    action: return
    with:
      input: "process me"

  - name: branch_a
    action: llm.call
    with:
      prompt: "Summarize: {{ state.input }}"
    output: summary

  - name: branch_b
    action: llm.call
    with:
      prompt: "Translate: {{ state.input }}"
    output: translation

  - name: combine
    action: return
    with:
      combined: "{{ parallel_results | tojson }}"

edges:
  - from: start
    to: branch_a
  - from: start
    to: branch_b    # Multiple edges from 'start' = parallel
  - from: branch_a
    to: combine
  - from: branch_b
    to: combine     # Multiple edges to 'combine' = fan-in
```

### Detection Algorithm
```rust
pub struct ParallelGroup {
    pub source: String,
    pub branches: Vec<String>,
    pub fan_in: Option<String>,
}

pub fn detect_parallel_groups(config: &WasmYamlConfig) -> Vec<ParallelGroup> {
    let mut groups = Vec::new();

    // Find nodes with multiple outgoing edges
    let mut outgoing: HashMap<String, Vec<String>> = HashMap::new();
    for edge in &config.edges {
        outgoing.entry(edge.from.clone())
            .or_default()
            .push(edge.to.clone());
    }

    for (source, targets) in outgoing {
        if targets.len() > 1 {
            // Find fan-in (node where all branches converge)
            let fan_in = find_common_descendant(&targets, config);

            groups.push(ParallelGroup {
                source,
                branches: targets,
                fan_in,
            });
        }
    }

    groups
}
```

### Execution Pattern
```rust
async fn execute_parallel_group(
    group: &ParallelGroup,
    state: JsonValue,
    config: &WasmYamlConfig,
) -> Result<JsonValue, WasmError> {
    let mut parallel_results = Vec::new();

    // Execute each branch sequentially (simulated parallel)
    for branch_start in &group.branches {
        // Clone state for this branch
        let branch_state = state.clone();

        // Execute branch until fan-in node
        let branch_result = execute_until(
            branch_start,
            group.fan_in.as_deref(),
            branch_state,
            config,
        ).await?;

        parallel_results.push(branch_result);
    }

    // Combine results for fan-in node
    let mut combined_state = state;
    combined_state["parallel_results"] = JsonValue::Array(parallel_results);

    Ok(combined_state)
}

async fn execute_until(
    start: &str,
    end: Option<&str>,
    mut state: JsonValue,
    config: &WasmYamlConfig,
) -> Result<JsonValue, WasmError> {
    let mut current = Some(start.to_string());

    while let Some(node_name) = current {
        // Stop at fan-in node
        if Some(node_name.as_str()) == end {
            break;
        }

        if node_name == "__end__" {
            break;
        }

        let node = find_node(&node_name, config)?;
        state = execute_node_async(node, state, config).await?;
        current = resolve_next_node(&node_name, &state, config);
    }

    Ok(state)
}
```

### Conditional Parallel
```rust
async fn execute_conditional_parallel(
    group: &ParallelGroup,
    state: &JsonValue,
    config: &WasmYamlConfig,
) -> Result<JsonValue, WasmError> {
    let mut parallel_results = Vec::new();

    for branch_start in &group.branches {
        // Find edge for this branch
        let edge = config.edges.iter()
            .find(|e| e.from == group.source && e.to == *branch_start);

        // Check condition if present
        let should_execute = match edge.and_then(|e| e.when.as_ref()) {
            Some(condition) => evaluate_condition(condition, state)?,
            None => true,
        };

        if should_execute {
            let branch_result = execute_branch(branch_start, state.clone(), config).await?;
            parallel_results.push(branch_result);
        }
    }

    // Aggregate results
    let mut result = state.clone();
    result["parallel_results"] = JsonValue::Array(parallel_results);
    Ok(result)
}
```

### State Merge Strategy Architecture (TECH-001 Mitigation)

**Problem Statement:**
When multiple parallel branches modify state, merging their results is ambiguous:
- Branch A sets `state.result = "summary"`
- Branch B sets `state.result = "translation"`
- What should the fan-in node see?

**Solution: Configurable Merge Strategies**

```rust
/// Merge strategy for combining parallel branch results
#[derive(Debug, Clone, Copy, Default, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MergeStrategy {
    /// DEFAULT: Branches are isolated. Fan-in receives original state + parallel_results array.
    /// The fan-in node must explicitly merge using templates.
    /// This is the SAFEST option - no silent data loss.
    #[default]
    Isolated,

    /// Last branch to execute wins. Branches merge in definition order.
    /// Warning logged when conflicts detected.
    LastWriteWins,

    /// Deep merge objects recursively. Arrays are concatenated.
    /// Primitives use last-write-wins.
    MergeDeep,

    /// Error if any key is modified by multiple branches.
    /// Most strict - ensures no conflicts.
    FailOnConflict,
}

/// Information about a merge conflict
#[derive(Debug, Clone)]
pub struct ConflictInfo {
    pub key_path: String,           // e.g., "result.data.score"
    pub branch_a: String,           // e.g., "summarize"
    pub branch_b: String,           // e.g., "translate"
    pub value_a: JsonValue,
    pub value_b: JsonValue,
}

impl std::fmt::Display for ConflictInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Conflict at '{}': branch '{}' set {:?}, branch '{}' set {:?}",
            self.key_path, self.branch_a, self.value_a, self.branch_b, self.value_b
        )
    }
}
```

**YAML Configuration:**
```yaml
name: parallel-with-merge
settings:
  parallel:
    merge_strategy: isolated  # isolated | last_write_wins | merge_deep | fail_on_conflict

nodes:
  - name: fan_out
    # ...
  - name: branch_a
    output: summary
  - name: branch_b
    output: translation
  - name: fan_in
    action: return
    with:
      # With 'isolated' strategy, manually merge from parallel_results:
      combined:
        summary: "{{ parallel_results[0].summary }}"
        translation: "{{ parallel_results[1].translation }}"
```

**Implementation Pattern:**

```rust
async fn execute_parallel_group(
    group: &ParallelGroup,
    state: JsonValue,
    config: &WasmYamlConfig,
) -> Result<JsonValue, WasmError> {
    let merge_strategy = config.settings
        .as_ref()
        .and_then(|s| s.parallel.as_ref())
        .map(|p| p.merge_strategy)
        .unwrap_or_default();

    let mut branch_results: Vec<BranchResult> = Vec::new();

    // Execute each branch
    for (idx, branch_start) in group.branches.iter().enumerate() {
        let branch_state = state.clone();
        let final_state = execute_until(
            branch_start,
            group.fan_in.as_deref(),
            branch_state,
            config,
        ).await?;

        branch_results.push(BranchResult {
            branch_name: branch_start.clone(),
            branch_index: idx,
            final_state,
        });
    }

    // Apply merge strategy
    let merged_state = apply_merge_strategy(
        merge_strategy,
        &state,
        &branch_results,
        &group.branches,
    )?;

    Ok(merged_state)
}

fn apply_merge_strategy(
    strategy: MergeStrategy,
    base_state: &JsonValue,
    branch_results: &[BranchResult],
    branch_names: &[String],
) -> Result<JsonValue, WasmError> {
    // Extract just the final states for parallel_results
    let parallel_results: Vec<JsonValue> = branch_results
        .iter()
        .map(|br| br.final_state.clone())
        .collect();

    match strategy {
        MergeStrategy::Isolated => {
            // SAFEST: Only provide parallel_results, no state merge
            let mut result = base_state.clone();
            result["parallel_results"] = JsonValue::Array(parallel_results);
            Ok(result)
        }

        MergeStrategy::LastWriteWins => {
            let conflicts = detect_conflicts(base_state, branch_results);
            if !conflicts.is_empty() {
                log::warn!(
                    "Parallel merge conflicts (last_write_wins): {}",
                    conflicts.iter().map(|c| c.to_string()).collect::<Vec<_>>().join("; ")
                );
            }

            let mut result = base_state.clone();
            for br in branch_results {
                merge_json_last_write(&mut result, &br.final_state);
            }
            result["parallel_results"] = JsonValue::Array(parallel_results);
            Ok(result)
        }

        MergeStrategy::MergeDeep => {
            let mut result = base_state.clone();
            for br in branch_results {
                merge_json_deep(&mut result, &br.final_state);
            }
            result["parallel_results"] = JsonValue::Array(parallel_results);
            Ok(result)
        }

        MergeStrategy::FailOnConflict => {
            let conflicts = detect_conflicts(base_state, branch_results);
            if !conflicts.is_empty() {
                return Err(WasmError::MergeConflict {
                    conflicts: conflicts.clone(),
                    message: format!(
                        "Parallel execution conflict: {} keys modified by multiple branches. \
                         Use merge_strategy: 'isolated' or 'last_write_wins' to resolve.",
                        conflicts.len()
                    ),
                });
            }

            // No conflicts - safe to merge
            let mut result = base_state.clone();
            for br in branch_results {
                merge_json_last_write(&mut result, &br.final_state);
            }
            result["parallel_results"] = JsonValue::Array(parallel_results);
            Ok(result)
        }
    }
}
```

**Conflict Detection:**

```rust
fn detect_conflicts(
    base_state: &JsonValue,
    branch_results: &[BranchResult],
) -> Vec<ConflictInfo> {
    let mut conflicts = Vec::new();

    // Track which keys each branch modified
    let mut modifications: HashMap<String, Vec<(String, JsonValue)>> = HashMap::new();

    for br in branch_results {
        let changed_keys = diff_json_keys(base_state, &br.final_state);
        for (key_path, new_value) in changed_keys {
            modifications
                .entry(key_path)
                .or_default()
                .push((br.branch_name.clone(), new_value));
        }
    }

    // Find keys modified by multiple branches
    for (key_path, modifiers) in modifications {
        if modifiers.len() > 1 {
            // Check if values are actually different
            let first_value = &modifiers[0].1;
            for (branch_name, value) in modifiers.iter().skip(1) {
                if value != first_value {
                    conflicts.push(ConflictInfo {
                        key_path: key_path.clone(),
                        branch_a: modifiers[0].0.clone(),
                        branch_b: branch_name.clone(),
                        value_a: first_value.clone(),
                        value_b: value.clone(),
                    });
                }
            }
        }
    }

    conflicts
}

fn diff_json_keys(base: &JsonValue, modified: &JsonValue) -> Vec<(String, JsonValue)> {
    let mut changes = Vec::new();
    diff_json_recursive(base, modified, String::new(), &mut changes);
    changes
}

fn diff_json_recursive(
    base: &JsonValue,
    modified: &JsonValue,
    prefix: String,
    changes: &mut Vec<(String, JsonValue)>,
) {
    match (base, modified) {
        (JsonValue::Object(base_obj), JsonValue::Object(mod_obj)) => {
            for (key, mod_value) in mod_obj {
                let key_path = if prefix.is_empty() {
                    key.clone()
                } else {
                    format!("{}.{}", prefix, key)
                };

                match base_obj.get(key) {
                    Some(base_value) if base_value != mod_value => {
                        diff_json_recursive(base_value, mod_value, key_path, changes);
                    }
                    None => {
                        changes.push((key_path, mod_value.clone()));
                    }
                    _ => {} // No change
                }
            }
        }
        _ if base != modified => {
            changes.push((prefix, modified.clone()));
        }
        _ => {} // No change
    }
}
```

**Recommended Pattern (Isolated + Manual Merge):**

```yaml
name: safe-parallel-pattern
settings:
  parallel:
    merge_strategy: isolated  # Explicit (also the default)

nodes:
  - name: start
    action: return
    with:
      text: "{{ state.input }}"

  - name: summarize
    action: llm.call
    with:
      prompt: "Summarize: {{ state.text }}"
    output: summary

  - name: translate
    action: llm.call
    with:
      prompt: "Translate: {{ state.text }}"
    output: translation

  - name: combine
    action: return
    with:
      # Explicitly merge from parallel_results - NO ambiguity
      final_result:
        summary: "{{ parallel_results[0].summary }}"
        translation: "{{ parallel_results[1].translation }}"
        # Or use filters for more complex merging:
        all_outputs: "{{ parallel_results | map(attribute='output') | list | tojson }}"

edges:
  - from: start
    to: summarize
  - from: start
    to: translate
  - from: summarize
    to: combine
  - from: translate
    to: combine
```

## Testing

### Test Location
`rust/tea-wasm-llm/tests/test_parallel.rs`

### Test Cases
```rust
#[wasm_bindgen_test]
async fn test_parallel_detection() {
    let config = parse_yaml(r#"
        name: test
        nodes:
          - name: start
          - name: a
          - name: b
          - name: end
        edges:
          - from: start
            to: a
          - from: start
            to: b
          - from: a
            to: end
          - from: b
            to: end
    "#);

    let groups = detect_parallel_groups(&config);
    assert_eq!(groups.len(), 1);
    assert_eq!(groups[0].source, "start");
    assert_eq!(groups[0].branches, vec!["a", "b"]);
    assert_eq!(groups[0].fan_in, Some("end".to_string()));
}

#[wasm_bindgen_test]
async fn test_parallel_results_aggregation() {
    let yaml = r#"
        name: test
        nodes:
          - name: start
            action: return
            with:
              x: 1
          - name: add_one
            action: return
            with:
              result: "{{ state.x + 1 }}"
          - name: add_two
            action: return
            with:
              result: "{{ state.x + 2 }}"
          - name: combine
            action: return
            with:
              total: "{{ parallel_results | length }}"
        edges:
          - from: start
            to: add_one
          - from: start
            to: add_two
          - from: add_one
            to: combine
          - from: add_two
            to: combine
    "#;

    let result = execute_yaml_async(yaml, "{}").await.unwrap();
    let state: JsonValue = serde_json::from_str(&result).unwrap();

    assert_eq!(state["parallel_results"].as_array().unwrap().len(), 2);
}

#[wasm_bindgen_test]
async fn test_conditional_parallel() {
    let yaml = r#"
        name: test
        nodes:
          - name: start
            action: return
            with:
              mode: "fast"
          - name: slow_path
            action: return
            with:
              path: "slow"
          - name: fast_path
            action: return
            with:
              path: "fast"
          - name: combine
        edges:
          - from: start
            to: slow_path
            when: state.mode == "slow"
          - from: start
            to: fast_path
            when: state.mode == "fast"
          - from: slow_path
            to: combine
          - from: fast_path
            to: combine
    "#;

    let result = execute_yaml_async(yaml, "{}").await.unwrap();
    let state: JsonValue = serde_json::from_str(&result).unwrap();

    // Only fast_path should have executed
    assert_eq!(state["parallel_results"].as_array().unwrap().len(), 1);
    assert_eq!(state["parallel_results"][0]["path"], "fast");
}
```

### Merge Strategy Test Cases (TECH-001)
```rust
#[wasm_bindgen_test]
async fn test_isolated_strategy_no_state_merge() {
    // Default strategy: branches don't merge state, only parallel_results provided
    let yaml = r#"
        name: test
        settings:
          parallel:
            merge_strategy: isolated
        nodes:
          - name: start
            action: return
            with:
              original: "base"
          - name: branch_a
            action: return
            with:
              result: "from_a"
            output: result
          - name: branch_b
            action: return
            with:
              result: "from_b"
            output: result
          - name: combine
            action: return
        edges:
          - from: start
            to: branch_a
          - from: start
            to: branch_b
          - from: branch_a
            to: combine
          - from: branch_b
            to: combine
    "#;

    let result = execute_yaml_async(yaml, "{}").await.unwrap();
    let state: JsonValue = serde_json::from_str(&result).unwrap();

    // With 'isolated', the fan-in node should NOT see merged 'result' key
    // It should only have original state + parallel_results
    assert!(state.get("result").is_none(), "Isolated strategy should not merge branch state");
    assert_eq!(state["original"], "base");
    assert_eq!(state["parallel_results"].as_array().unwrap().len(), 2);
    assert_eq!(state["parallel_results"][0]["result"], "from_a");
    assert_eq!(state["parallel_results"][1]["result"], "from_b");
}

#[wasm_bindgen_test]
async fn test_last_write_wins_strategy() {
    let yaml = r#"
        name: test
        settings:
          parallel:
            merge_strategy: last_write_wins
        nodes:
          - name: start
            action: return
          - name: branch_a
            action: return
            with:
              shared_key: "value_a"
            output: shared_key
          - name: branch_b
            action: return
            with:
              shared_key: "value_b"
            output: shared_key
          - name: combine
        edges:
          - from: start
            to: branch_a
          - from: start
            to: branch_b
          - from: branch_a
            to: combine
          - from: branch_b
            to: combine
    "#;

    let result = execute_yaml_async(yaml, "{}").await.unwrap();
    let state: JsonValue = serde_json::from_str(&result).unwrap();

    // Branch B executes after A, so B's value wins
    assert_eq!(state["shared_key"], "value_b");
}

#[wasm_bindgen_test]
async fn test_fail_on_conflict_strategy_errors() {
    let yaml = r#"
        name: test
        settings:
          parallel:
            merge_strategy: fail_on_conflict
        nodes:
          - name: start
            action: return
          - name: branch_a
            action: return
            with:
              conflict_key: "value_a"
            output: conflict_key
          - name: branch_b
            action: return
            with:
              conflict_key: "value_b"
            output: conflict_key
          - name: combine
        edges:
          - from: start
            to: branch_a
          - from: start
            to: branch_b
          - from: branch_a
            to: combine
          - from: branch_b
            to: combine
    "#;

    let result = execute_yaml_async(yaml, "{}").await;

    // Should error due to conflict
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("conflict"), "Error should mention conflict");
    assert!(err.contains("conflict_key"), "Error should include key path");
    assert!(err.contains("branch_a") || err.contains("branch_b"), "Error should include branch names");
}

#[wasm_bindgen_test]
async fn test_fail_on_conflict_passes_with_no_conflicts() {
    let yaml = r#"
        name: test
        settings:
          parallel:
            merge_strategy: fail_on_conflict
        nodes:
          - name: start
            action: return
          - name: branch_a
            action: return
            with:
              result_a: "value_a"
            output: result_a
          - name: branch_b
            action: return
            with:
              result_b: "value_b"
            output: result_b
          - name: combine
        edges:
          - from: start
            to: branch_a
          - from: start
            to: branch_b
          - from: branch_a
            to: combine
          - from: branch_b
            to: combine
    "#;

    let result = execute_yaml_async(yaml, "{}").await.unwrap();
    let state: JsonValue = serde_json::from_str(&result).unwrap();

    // No conflicts - both values should be merged
    assert_eq!(state["result_a"], "value_a");
    assert_eq!(state["result_b"], "value_b");
}

#[wasm_bindgen_test]
async fn test_merge_deep_strategy() {
    let yaml = r#"
        name: test
        settings:
          parallel:
            merge_strategy: merge_deep
        nodes:
          - name: start
            action: return
            with:
              data:
                existing: "base"
          - name: branch_a
            action: return
            with:
              data:
                from_a: "value_a"
            output: data
          - name: branch_b
            action: return
            with:
              data:
                from_b: "value_b"
            output: data
          - name: combine
        edges:
          - from: start
            to: branch_a
          - from: start
            to: branch_b
          - from: branch_a
            to: combine
          - from: branch_b
            to: combine
    "#;

    let result = execute_yaml_async(yaml, "{}").await.unwrap();
    let state: JsonValue = serde_json::from_str(&result).unwrap();

    // Deep merge should combine all nested keys
    assert_eq!(state["data"]["existing"], "base");
    assert_eq!(state["data"]["from_a"], "value_a");
    assert_eq!(state["data"]["from_b"], "value_b");
}

#[wasm_bindgen_test]
async fn test_conflict_info_includes_context() {
    let yaml = r#"
        name: test
        settings:
          parallel:
            merge_strategy: fail_on_conflict
        nodes:
          - name: start
          - name: branch_a
            action: return
            with:
              nested:
                deep:
                  key: "a"
            output: nested
          - name: branch_b
            action: return
            with:
              nested:
                deep:
                  key: "b"
            output: nested
          - name: combine
        edges:
          - from: start
            to: branch_a
          - from: start
            to: branch_b
          - from: branch_a
            to: combine
          - from: branch_b
            to: combine
    "#;

    let result = execute_yaml_async(yaml, "{}").await;
    let err = result.unwrap_err();

    // Error should include full key path
    assert!(err.contains("nested.deep.key") || err.contains("nested"),
        "Error should include nested key path");
}

#[test]
fn test_detect_conflicts_finds_modified_keys() {
    let base = json!({"x": 1, "y": 2});
    let branch_results = vec![
        BranchResult {
            branch_name: "a".to_string(),
            branch_index: 0,
            final_state: json!({"x": 1, "y": 2, "z": 10}),  // Added z
        },
        BranchResult {
            branch_name: "b".to_string(),
            branch_index: 1,
            final_state: json!({"x": 1, "y": 2, "z": 20}),  // Also added z with different value
        },
    ];

    let conflicts = detect_conflicts(&base, &branch_results);
    assert_eq!(conflicts.len(), 1);
    assert_eq!(conflicts[0].key_path, "z");
    assert_eq!(conflicts[0].branch_a, "a");
    assert_eq!(conflicts[0].branch_b, "b");
}

#[test]
fn test_no_conflict_when_same_value() {
    let base = json!({});
    let branch_results = vec![
        BranchResult {
            branch_name: "a".to_string(),
            branch_index: 0,
            final_state: json!({"result": "same"}),
        },
        BranchResult {
            branch_name: "b".to_string(),
            branch_index: 1,
            final_state: json!({"result": "same"}),  // Same value - no conflict
        },
    ];

    let conflicts = detect_conflicts(&base, &branch_results);
    assert!(conflicts.is_empty(), "Same values should not be a conflict");
}
```

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-17 | 0.1 | Initial story creation | Sarah (PO) |
| 2026-01-17 | 0.2 | Added state merge strategy requirements (AC 8-12), MergeStrategy enum, conflict detection, and 9 merge strategy test cases per TECH-001 risk assessment | Quinn (QA) |

## Dev Agent Record

### Agent Model Used
_To be filled during implementation_

### Debug Log References
_To be filled during implementation_

### Completion Notes List
_To be filled during implementation_

### File List
_To be filled during implementation_

## QA Results

### QA Review Date
2026-01-17

### Reviewer
Quinn (Test Architect)

### Test Design Summary
| Metric | Count |
|--------|-------|
| Total Scenarios | 18 |
| Unit Tests | 10 |
| Integration Tests | 6 |
| E2E Tests | 2 |
| P0 (Critical) | 8 |
| P1 (Important) | 7 |
| P2 (Edge cases) | 3 |

### Risk Assessment
| Risk ID | Score | Description |
|---------|-------|-------------|
| TECH-001 | 9 (Critical) | Fan-in state merge conflicts - **MITIGATED** via MergeStrategy |
| TECH-005 | 4 (Medium) | Nested parallel complexity |

### Key Test Scenarios
- `1.5-UNIT-001`: Detect parallel edge patterns (P0)
- `1.5-UNIT-004`: `parallel_results` array populated correctly (P0)
- `1.5-UNIT-007`: Execution order matches edge definition order (P0)
- `1.5-UNIT-009`: `merge_strategy: isolated` provides only `parallel_results` (P0, TECH-001)
- `1.5-UNIT-010`: `merge_strategy: fail_on_conflict` errors on conflicts (P0, TECH-001)
- `1.5-INT-002`: Conditional parallel branches (P0)
- `1.5-INT-004`: Nested parallel patterns (P1)

### Recommendations
1. Default `merge_strategy: isolated` is correct - document clearly
2. Add extensive logging for merge conflicts in `last_write_wins`
3. Consider adding `parallel_results` metadata (branch names, execution times)

### Gate Status
**PASS** - Story is ready for implementation. Critical risk TECH-001 fully mitigated by configurable MergeStrategy with safe default.

### Reference
Test design: `docs/qa/assessments/TEA-WASM-001-test-design-20260117.md`
