//! Deep Merge Algorithm for YAML Values.
//!
//! This module provides kubectl-style deep merge semantics for combining YAML configurations:
//! - Objects (Mappings) are recursively merged
//! - Arrays with identifying keys are merged by key (YE.9):
//!   - nodes: merged by "name"
//!   - edges: merged by ("from", "to")
//!   - goto: merged by "to"
//! - Arrays without keys are replaced (not concatenated)
//! - Scalars use last-wins semantics
//! - Null can override non-null values
//! - Elements with __delete__: true are removed (YE.9)
//!
//! YE.8: YAML Overlay Merge Support
//! YE.9: Deep Array Merge by Identifying Key

use serde_yaml::Value;
use std::collections::HashMap;

/// Key for goto arrays (nested in nodes), merge by "to" field.
const GOTO_MERGE_KEY: &[&str] = &["to"];

/// Special marker for element deletion.
const DELETE_MARKER: &str = "__delete__";

/// Get merge keys for an array field name.
/// Returns Some(keys) for known arrays, None otherwise.
fn get_array_merge_keys(array_name: &str) -> Option<&'static [&'static str]> {
    match array_name {
        "nodes" => Some(&["name"]),
        "edges" => Some(&["from", "to"]),
        _ => None,
    }
}

/// Extract composite key from an array element.
///
/// Returns Some(Vec<String>) with key values if element is a mapping and has all required keys,
/// None otherwise.
fn get_element_key(element: &Value, keys: &[&str]) -> Option<Vec<String>> {
    let mapping = element.as_mapping()?;
    let mut key_values = Vec::with_capacity(keys.len());
    for key in keys {
        let value = mapping.get(*key)?;
        let str_value = match value {
            Value::String(s) => s.clone(),
            Value::Number(n) => n.to_string(),
            Value::Bool(b) => b.to_string(),
            _ => return None,
        };
        key_values.push(str_value);
    }
    Some(key_values)
}

/// Merge two arrays by identifying key(s).
///
/// YE.9: Strategic merge patch for arrays using identifying keys.
///
/// Merge rules:
/// - Elements with matching keys are deep-merged recursively
/// - Elements with __delete__: true marker are removed
/// - Non-matching overlay elements are appended
///
/// # Arguments
/// * `base` - Base array (mutable, will be modified in place)
/// * `overlay` - Overlay array elements
/// * `keys` - Field names that identify unique elements
/// * `path` - Current path for nested merge context
fn merge_arrays_by_key(base: &mut Vec<Value>, overlay: Vec<Value>, keys: &[&str], path: &str) {
    // Build index of base elements by key
    let mut base_index: HashMap<Vec<String>, usize> = HashMap::new();
    for (i, elem) in base.iter().enumerate() {
        if let Some(key_tuple) = get_element_key(elem, keys) {
            base_index.insert(key_tuple, i);
        }
    }

    // Track indices to delete (process deletions after all merges)
    let mut indices_to_delete: Vec<usize> = Vec::new();

    // Process overlay elements
    for overlay_elem in overlay {
        if !overlay_elem.is_mapping() {
            // Non-mapping elements: append as-is
            base.push(overlay_elem);
            continue;
        }

        let key_tuple = get_element_key(&overlay_elem, keys);

        // Check for delete marker
        if let Some(mapping) = overlay_elem.as_mapping() {
            if let Some(delete_val) = mapping.get(DELETE_MARKER) {
                if delete_val.as_bool() == Some(true) {
                    if let Some(ref kt) = key_tuple {
                        if let Some(&idx) = base_index.get(kt) {
                            indices_to_delete.push(idx);
                        }
                    }
                    continue;
                }
            }
        }

        if let Some(ref kt) = key_tuple {
            if let Some(&idx) = base_index.get(kt) {
                // Merge with existing element
                deep_merge_impl(&mut base[idx], overlay_elem, path);
                continue;
            }
        }

        // Append new element
        let new_idx = base.len();
        base.push(overlay_elem);
        // Update index for newly added element
        if let Some(kt) = key_tuple {
            base_index.insert(kt, new_idx);
        }
    }

    // Remove deleted elements (in reverse order to maintain indices)
    indices_to_delete.sort_unstable();
    indices_to_delete.reverse();
    for idx in indices_to_delete {
        base.remove(idx);
    }
}

/// Internal implementation of deep merge with path tracking.
fn deep_merge_impl(base: &mut Value, overlay: Value, path: &str) {
    match (base, overlay) {
        (Value::Mapping(base_map), Value::Mapping(overlay_map)) => {
            // Both are mappings: recursively merge
            for (key, overlay_value) in overlay_map {
                let key_str = key.as_str().unwrap_or("");

                // Skip delete marker (handled at array level)
                if key_str == DELETE_MARKER {
                    continue;
                }

                let new_path = if path.is_empty() {
                    key_str.to_string()
                } else {
                    format!("{}.{}", path, key_str)
                };

                match base_map.get_mut(&key) {
                    Some(base_value) => {
                        // Both have the key - recurse
                        deep_merge_impl(base_value, overlay_value, &new_path);
                    }
                    None => {
                        // New key from overlay
                        base_map.insert(key, overlay_value);
                    }
                }
            }
        }
        (Value::Sequence(base_seq), Value::Sequence(overlay_seq)) => {
            // Both are arrays: check for array merge by key
            let array_name = path.rsplit('.').next().unwrap_or("");

            if let Some(keys) = get_array_merge_keys(array_name) {
                merge_arrays_by_key(base_seq, overlay_seq, keys, path);
            } else if array_name == "goto" {
                merge_arrays_by_key(base_seq, overlay_seq, GOTO_MERGE_KEY, path);
            } else {
                // Default: replace (existing YE.8 behavior for unknown arrays)
                *base_seq = overlay_seq;
            }
        }
        (base, overlay) => {
            // Type mismatch or scalars: overlay wins
            *base = overlay;
        }
    }
}

/// Deep merge two YAML Values with kubectl-style semantics.
///
/// Merge rules:
/// - Objects (Mappings) are recursively merged
/// - Arrays with identifying keys are merged by key (YE.9):
///   - nodes: merged by "name"
///   - edges: merged by ("from", "to")
///   - goto: merged by "to"
/// - Arrays without keys are replaced, not concatenated
/// - Scalars use last-wins (overlay overrides base)
/// - Null values can override non-null values
/// - Elements with __delete__: true are removed (YE.9)
///
/// # Arguments
/// * `base` - Base value (mutable, will be modified in place)
/// * `overlay` - Overlay value (takes precedence)
///
/// # Examples
///
/// ```
/// use serde_yaml::Value;
/// use the_edge_agent::engine::deep_merge::deep_merge;
///
/// let mut base: Value = serde_yaml::from_str(r#"
/// a: 1
/// b:
///   x: 10
///   y: 20
/// "#).unwrap();
///
/// let overlay: Value = serde_yaml::from_str(r#"
/// b:
///   y: 30
///   z: 40
/// c: 3
/// "#).unwrap();
///
/// deep_merge(&mut base, overlay);
/// // Result: a=1, b.x=10, b.y=30, b.z=40, c=3
/// ```
pub fn deep_merge(base: &mut Value, overlay: Value) {
    deep_merge_impl(base, overlay, "");
}

/// Merge multiple YAML Values in order (first is lowest priority, last is highest).
///
/// This applies kubectl-style merge semantics across a list of values.
/// Later values in the list override earlier ones.
///
/// # Arguments
/// * `values` - List of YAML Values to merge
///
/// # Returns
/// Merged Value. Returns empty Mapping if input is empty.
///
/// # Example
///
/// ```
/// use serde_yaml::Value;
/// use the_edge_agent::engine::deep_merge::merge_all;
///
/// let base: Value = serde_yaml::from_str("a: 1").unwrap();
/// let overlay1: Value = serde_yaml::from_str("b: 2").unwrap();
/// let overlay2: Value = serde_yaml::from_str("a: 3").unwrap();
///
/// let result = merge_all(vec![base, overlay1, overlay2]);
/// // Result: a=3, b=2
/// ```
pub fn merge_all(values: Vec<Value>) -> Value {
    if values.is_empty() {
        return Value::Mapping(serde_yaml::Mapping::new());
    }

    let mut iter = values.into_iter();
    let mut result = iter.next().unwrap();

    for overlay in iter {
        deep_merge(&mut result, overlay);
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_object_recursive_merge() {
        let mut base: Value = serde_yaml::from_str(
            r#"
a: 1
b:
  x: 10
  y: 20
"#,
        )
        .unwrap();

        let overlay: Value = serde_yaml::from_str(
            r#"
b:
  y: 30
  z: 40
c: 3
"#,
        )
        .unwrap();

        deep_merge(&mut base, overlay);

        assert_eq!(base["a"], Value::Number(1.into()));
        assert_eq!(base["b"]["x"], Value::Number(10.into()));
        assert_eq!(base["b"]["y"], Value::Number(30.into()));
        assert_eq!(base["b"]["z"], Value::Number(40.into()));
        assert_eq!(base["c"], Value::Number(3.into()));
    }

    #[test]
    fn test_array_replacement_not_concat() {
        let mut base: Value = serde_yaml::from_str(
            r#"
items:
  - 1
  - 2
  - 3
"#,
        )
        .unwrap();

        let overlay: Value = serde_yaml::from_str(
            r#"
items:
  - 4
  - 5
"#,
        )
        .unwrap();

        deep_merge(&mut base, overlay);

        let items = base["items"].as_sequence().unwrap();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0], Value::Number(4.into()));
        assert_eq!(items[1], Value::Number(5.into()));
    }

    #[test]
    fn test_scalar_override() {
        let mut base: Value = serde_yaml::from_str(
            r#"
count: 10
name: base
"#,
        )
        .unwrap();

        let overlay: Value = serde_yaml::from_str(
            r#"
count: 20
"#,
        )
        .unwrap();

        deep_merge(&mut base, overlay);

        assert_eq!(base["count"], Value::Number(20.into()));
        assert_eq!(base["name"], Value::String("base".to_string()));
    }

    #[test]
    fn test_null_can_override() {
        let mut base: Value = serde_yaml::from_str(
            r#"
enabled: true
value: 42
"#,
        )
        .unwrap();

        let overlay: Value = serde_yaml::from_str(
            r#"
enabled: null
"#,
        )
        .unwrap();

        deep_merge(&mut base, overlay);

        assert_eq!(base["enabled"], Value::Null);
        assert_eq!(base["value"], Value::Number(42.into()));
    }

    #[test]
    fn test_nested_object_merge() {
        let mut base: Value = serde_yaml::from_str(
            r#"
a:
  b:
    c:
      d: 1
"#,
        )
        .unwrap();

        let overlay: Value = serde_yaml::from_str(
            r#"
a:
  b:
    c:
      e: 2
"#,
        )
        .unwrap();

        deep_merge(&mut base, overlay);

        assert_eq!(base["a"]["b"]["c"]["d"], Value::Number(1.into()));
        assert_eq!(base["a"]["b"]["c"]["e"], Value::Number(2.into()));
    }

    #[test]
    fn test_type_mismatch_overlay_wins() {
        let mut base: Value = serde_yaml::from_str(
            r#"
value:
  nested: true
"#,
        )
        .unwrap();

        let overlay: Value = serde_yaml::from_str(
            r#"
value: "string"
"#,
        )
        .unwrap();

        deep_merge(&mut base, overlay);

        assert_eq!(base["value"], Value::String("string".to_string()));
    }

    #[test]
    fn test_merge_all_multiple_overlays() {
        let base: Value = serde_yaml::from_str("a: 1\nb: 2").unwrap();
        let overlay1: Value = serde_yaml::from_str("b: 3\nc: 4").unwrap();
        let overlay2: Value = serde_yaml::from_str("c: 5\nd: 6").unwrap();

        let result = merge_all(vec![base, overlay1, overlay2]);

        assert_eq!(result["a"], Value::Number(1.into()));
        assert_eq!(result["b"], Value::Number(3.into()));
        assert_eq!(result["c"], Value::Number(5.into()));
        assert_eq!(result["d"], Value::Number(6.into()));
    }

    #[test]
    fn test_merge_all_empty_list() {
        let result = merge_all(vec![]);
        assert!(result.is_mapping());
        assert!(result.as_mapping().unwrap().is_empty());
    }

    #[test]
    fn test_merge_all_single_value() {
        let base: Value = serde_yaml::from_str("a: 1").unwrap();
        let result = merge_all(vec![base.clone()]);
        assert_eq!(result["a"], Value::Number(1.into()));
    }

    // === YE.9: Array Merge by Key Tests ===

    #[test]
    fn test_nodes_merge_by_name() {
        let mut base: Value = serde_yaml::from_str(
            r#"
nodes:
  - name: fetch
    uses: http.get
  - name: process
    uses: llm.call
    with:
      model: gpt-4
"#,
        )
        .unwrap();

        let overlay: Value = serde_yaml::from_str(
            r#"
nodes:
  - name: process
    with:
      model: claude-3
"#,
        )
        .unwrap();

        deep_merge(&mut base, overlay);

        let nodes = base["nodes"].as_sequence().unwrap();
        assert_eq!(nodes.len(), 2);

        // First node unchanged
        assert_eq!(nodes[0]["name"], Value::String("fetch".to_string()));
        assert_eq!(nodes[0]["uses"], Value::String("http.get".to_string()));

        // Second node merged
        assert_eq!(nodes[1]["name"], Value::String("process".to_string()));
        assert_eq!(nodes[1]["uses"], Value::String("llm.call".to_string())); // Preserved
        assert_eq!(
            nodes[1]["with"]["model"],
            Value::String("claude-3".to_string())
        ); // Updated
    }

    #[test]
    fn test_edges_merge_by_from_to() {
        let mut base: Value = serde_yaml::from_str(
            r#"
edges:
  - from: a
    to: b
    weight: 1
  - from: b
    to: c
    weight: 2
"#,
        )
        .unwrap();

        let overlay: Value = serde_yaml::from_str(
            r#"
edges:
  - from: a
    to: b
    weight: 10
    retry: 3
"#,
        )
        .unwrap();

        deep_merge(&mut base, overlay);

        let edges = base["edges"].as_sequence().unwrap();
        assert_eq!(edges.len(), 2);

        // First edge merged
        assert_eq!(edges[0]["weight"], Value::Number(10.into()));
        assert_eq!(edges[0]["retry"], Value::Number(3.into()));

        // Second edge unchanged
        assert_eq!(edges[1]["weight"], Value::Number(2.into()));
    }

    #[test]
    fn test_goto_merge_by_to() {
        let mut base: Value = serde_yaml::from_str(
            r#"
nodes:
  - name: router
    goto:
      - if: "score > 0.9"
        to: high
      - if: "score > 0.5"
        to: medium
      - to: low
"#,
        )
        .unwrap();

        let overlay: Value = serde_yaml::from_str(
            r#"
nodes:
  - name: router
    goto:
      - if: "score > 0.95"
        to: high
"#,
        )
        .unwrap();

        deep_merge(&mut base, overlay);

        let nodes = base["nodes"].as_sequence().unwrap();
        let goto = nodes[0]["goto"].as_sequence().unwrap();

        assert_eq!(goto.len(), 3);
        // High route updated
        assert_eq!(goto[0]["if"], Value::String("score > 0.95".to_string()));
        assert_eq!(goto[0]["to"], Value::String("high".to_string()));
        // Medium route preserved
        assert_eq!(goto[1]["to"], Value::String("medium".to_string()));
        // Low route preserved
        assert_eq!(goto[2]["to"], Value::String("low".to_string()));
    }

    #[test]
    fn test_delete_marker_removes_node() {
        let mut base: Value = serde_yaml::from_str(
            r#"
nodes:
  - name: keep_me
    val: 1
  - name: delete_me
    val: 2
  - name: also_keep
    val: 3
"#,
        )
        .unwrap();

        let overlay: Value = serde_yaml::from_str(
            r#"
nodes:
  - name: delete_me
    __delete__: true
"#,
        )
        .unwrap();

        deep_merge(&mut base, overlay);

        let nodes = base["nodes"].as_sequence().unwrap();
        assert_eq!(nodes.len(), 2);

        // Verify delete_me is gone
        for node in nodes {
            assert_ne!(node["name"], Value::String("delete_me".to_string()));
        }
    }

    #[test]
    fn test_delete_marker_removes_edge() {
        let mut base: Value = serde_yaml::from_str(
            r#"
edges:
  - from: a
    to: b
  - from: b
    to: c
  - from: c
    to: d
"#,
        )
        .unwrap();

        let overlay: Value = serde_yaml::from_str(
            r#"
edges:
  - from: b
    to: c
    __delete__: true
"#,
        )
        .unwrap();

        deep_merge(&mut base, overlay);

        let edges = base["edges"].as_sequence().unwrap();
        assert_eq!(edges.len(), 2);

        // Verify b->c is gone
        for edge in edges {
            if edge["from"] == Value::String("b".to_string()) {
                assert_ne!(edge["to"], Value::String("c".to_string()));
            }
        }
    }

    #[test]
    fn test_non_matching_elements_appended() {
        let mut base: Value = serde_yaml::from_str(
            r#"
nodes:
  - name: a
    val: 1
"#,
        )
        .unwrap();

        let overlay: Value = serde_yaml::from_str(
            r#"
nodes:
  - name: b
    val: 2
"#,
        )
        .unwrap();

        deep_merge(&mut base, overlay);

        let nodes = base["nodes"].as_sequence().unwrap();
        assert_eq!(nodes.len(), 2);
        assert_eq!(nodes[0]["name"], Value::String("a".to_string()));
        assert_eq!(nodes[1]["name"], Value::String("b".to_string()));
    }

    #[test]
    fn test_nested_deep_merge_in_matched_element() {
        let mut base: Value = serde_yaml::from_str(
            r#"
nodes:
  - name: llm
    with:
      model: gpt-4
      temperature: 0.7
      params:
        max_tokens: 1000
        top_p: 0.9
"#,
        )
        .unwrap();

        let overlay: Value = serde_yaml::from_str(
            r#"
nodes:
  - name: llm
    with:
      model: claude-3
      params:
        max_tokens: 2000
"#,
        )
        .unwrap();

        deep_merge(&mut base, overlay);

        let nodes = base["nodes"].as_sequence().unwrap();
        let node = &nodes[0];

        assert_eq!(node["with"]["model"], Value::String("claude-3".to_string())); // Updated
        assert_eq!(
            node["with"]["temperature"],
            Value::Number(serde_yaml::Number::from(0.7))
        ); // Preserved
        assert_eq!(
            node["with"]["params"]["max_tokens"],
            Value::Number(2000.into())
        ); // Updated
        assert_eq!(
            node["with"]["params"]["top_p"],
            Value::Number(serde_yaml::Number::from(0.9))
        ); // Preserved
    }

    #[test]
    fn test_unknown_arrays_still_replace() {
        let mut base: Value = serde_yaml::from_str(
            r#"
items:
  - 1
  - 2
  - 3
tags:
  - a
  - b
"#,
        )
        .unwrap();

        let overlay: Value = serde_yaml::from_str(
            r#"
items:
  - 4
  - 5
tags:
  - c
"#,
        )
        .unwrap();

        deep_merge(&mut base, overlay);

        let items = base["items"].as_sequence().unwrap();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0], Value::Number(4.into()));
        assert_eq!(items[1], Value::Number(5.into()));

        let tags = base["tags"].as_sequence().unwrap();
        assert_eq!(tags.len(), 1);
        assert_eq!(tags[0], Value::String("c".to_string()));
    }

    #[test]
    fn test_complex_add_modify_delete() {
        let mut base: Value = serde_yaml::from_str(
            r#"
nodes:
  - name: a
    val: 1
  - name: b
    val: 2
  - name: c
    val: 3
  - name: d
    val: 4
"#,
        )
        .unwrap();

        let overlay: Value = serde_yaml::from_str(
            r#"
nodes:
  - name: a
    val: 10
  - name: c
    __delete__: true
  - name: e
    val: 5
"#,
        )
        .unwrap();

        deep_merge(&mut base, overlay);

        let nodes = base["nodes"].as_sequence().unwrap();
        assert_eq!(nodes.len(), 4); // 4 - 1 delete + 1 add = 4

        // Check names present
        let names: Vec<String> = nodes
            .iter()
            .map(|n| n["name"].as_str().unwrap().to_string())
            .collect();
        assert!(names.contains(&"a".to_string()));
        assert!(names.contains(&"b".to_string()));
        assert!(!names.contains(&"c".to_string())); // Deleted
        assert!(names.contains(&"d".to_string()));
        assert!(names.contains(&"e".to_string())); // Added

        // Check a was modified
        let a_node = nodes
            .iter()
            .find(|n| n["name"].as_str() == Some("a"))
            .unwrap();
        assert_eq!(a_node["val"], Value::Number(10.into()));
    }
}
