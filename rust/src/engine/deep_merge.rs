//! Deep Merge Algorithm for YAML Values.
//!
//! This module provides kubectl-style deep merge semantics for combining YAML configurations:
//! - Objects (Mappings) are recursively merged
//! - Arrays (Sequences) are replaced (not concatenated)
//! - Scalars use last-wins semantics
//! - Null can override non-null values
//!
//! YE.8: YAML Overlay Merge Support

use serde_yaml::Value;

/// Deep merge two YAML Values with kubectl-style semantics.
///
/// Merge rules:
/// - Objects (Mappings) are recursively merged
/// - Arrays (Sequences) are replaced, not concatenated
/// - Scalars use last-wins (overlay overrides base)
/// - Null values can override non-null values
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
    match (base, overlay) {
        (Value::Mapping(base_map), Value::Mapping(overlay_map)) => {
            // Both are mappings: recursively merge
            for (key, overlay_value) in overlay_map {
                match base_map.get_mut(&key) {
                    Some(base_value) => {
                        // Both have the key
                        if base_value.is_mapping() && overlay_value.is_mapping() {
                            // Both are mappings: recurse
                            deep_merge(base_value, overlay_value);
                        } else {
                            // Arrays, scalars, or type mismatch: overlay wins
                            *base_value = overlay_value;
                        }
                    }
                    None => {
                        // New key from overlay
                        base_map.insert(key, overlay_value);
                    }
                }
            }
        }
        (base, overlay) => {
            // Non-mapping: overlay wins (replaces base entirely)
            *base = overlay;
        }
    }
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
}
