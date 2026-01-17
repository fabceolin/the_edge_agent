//! Integration tests for TEA-WASM-001.5: Simulated Parallel Execution

use serde_json::json;
use tea_wasm_llm::{
    apply_merge_strategy, detect_conflicts, detect_parallel_groups, find_common_descendant,
    is_parallel_source, parse_yaml_config, BranchResult, MergeStrategy,
};

#[test]
fn test_detect_parallel_groups_in_workflow() {
    let yaml = r#"
name: parallel-workflow
nodes:
  - name: start
    action: return
    with:
      input: "test"
  - name: branch_a
    action: return
    with:
      result_a: "from a"
    output: result_a
  - name: branch_b
    action: return
    with:
      result_b: "from b"
    output: result_b
  - name: combine
    action: return
    with:
      combined: true
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
    let config = parse_yaml_config(yaml).unwrap();
    let groups = detect_parallel_groups(&config);

    assert_eq!(groups.len(), 1, "Should detect one parallel group");
    assert_eq!(groups[0].source, "start");
    assert_eq!(groups[0].branches.len(), 2);
    assert!(groups[0].branches.contains(&"branch_a".to_string()));
    assert!(groups[0].branches.contains(&"branch_b".to_string()));
    assert_eq!(groups[0].fan_in, Some("combine".to_string()));
}

#[test]
fn test_find_common_descendant_simple() {
    let yaml = r#"
name: test
nodes:
  - name: a
  - name: b
  - name: c
  - name: end
edges:
  - from: a
    to: c
  - from: b
    to: c
  - from: c
    to: end
"#;
    let config = parse_yaml_config(yaml).unwrap();
    let branches = vec!["a".to_string(), "b".to_string()];
    let fan_in = find_common_descendant(&branches, &config);

    assert_eq!(fan_in, Some("c".to_string()));
}

#[test]
fn test_find_common_descendant_nested() {
    let yaml = r#"
name: test
nodes:
  - name: a1
  - name: a2
  - name: b1
  - name: b2
  - name: merge
edges:
  - from: a1
    to: a2
  - from: b1
    to: b2
  - from: a2
    to: merge
  - from: b2
    to: merge
"#;
    let config = parse_yaml_config(yaml).unwrap();
    let branches = vec!["a1".to_string(), "b1".to_string()];
    let fan_in = find_common_descendant(&branches, &config);

    assert_eq!(fan_in, Some("merge".to_string()));
}

#[test]
fn test_is_parallel_source_detection() {
    let yaml = r#"
name: test
nodes:
  - name: single_out
  - name: multi_out
  - name: target1
  - name: target2
edges:
  - from: single_out
    to: target1
  - from: multi_out
    to: target1
  - from: multi_out
    to: target2
"#;
    let config = parse_yaml_config(yaml).unwrap();

    assert!(!is_parallel_source("single_out", &config));
    assert!(is_parallel_source("multi_out", &config));
    assert!(!is_parallel_source("target1", &config));
}

#[test]
fn test_merge_strategy_isolated_preserves_base_state() {
    let base = json!({"original": "base", "count": 10});
    let branch_results = vec![
        BranchResult {
            branch_name: "branch_a".to_string(),
            branch_index: 0,
            final_state: json!({"original": "base", "count": 10, "from_a": "value_a"}),
        },
        BranchResult {
            branch_name: "branch_b".to_string(),
            branch_index: 1,
            final_state: json!({"original": "base", "count": 10, "from_b": "value_b"}),
        },
    ];

    let result = apply_merge_strategy(MergeStrategy::Isolated, &base, &branch_results).unwrap();

    // Isolated strategy should NOT merge branch keys
    assert!(result.get("from_a").is_none());
    assert!(result.get("from_b").is_none());

    // Original state preserved
    assert_eq!(result["original"], "base");
    assert_eq!(result["count"], 10);

    // parallel_results should contain both branch states
    let pr = result["parallel_results"].as_array().unwrap();
    assert_eq!(pr.len(), 2);
    assert_eq!(pr[0]["from_a"], "value_a");
    assert_eq!(pr[1]["from_b"], "value_b");
}

#[test]
fn test_merge_strategy_last_write_wins_order() {
    let base = json!({});
    let branch_results = vec![
        BranchResult {
            branch_name: "first".to_string(),
            branch_index: 0,
            final_state: json!({"shared": "first_value", "unique_first": 1}),
        },
        BranchResult {
            branch_name: "second".to_string(),
            branch_index: 1,
            final_state: json!({"shared": "second_value", "unique_second": 2}),
        },
        BranchResult {
            branch_name: "third".to_string(),
            branch_index: 2,
            final_state: json!({"shared": "third_value", "unique_third": 3}),
        },
    ];

    let result =
        apply_merge_strategy(MergeStrategy::LastWriteWins, &base, &branch_results).unwrap();

    // Last branch wins for shared key
    assert_eq!(result["shared"], "third_value");

    // All unique keys present
    assert_eq!(result["unique_first"], 1);
    assert_eq!(result["unique_second"], 2);
    assert_eq!(result["unique_third"], 3);
}

#[test]
fn test_merge_strategy_fail_on_conflict_with_error_details() {
    let base = json!({});
    let branch_results = vec![
        BranchResult {
            branch_name: "branch_a".to_string(),
            branch_index: 0,
            final_state: json!({"conflict": "value_a"}),
        },
        BranchResult {
            branch_name: "branch_b".to_string(),
            branch_index: 1,
            final_state: json!({"conflict": "value_b"}),
        },
    ];

    let result = apply_merge_strategy(MergeStrategy::FailOnConflict, &base, &branch_results);

    assert!(result.is_err());
    let err = result.unwrap_err().to_string();

    // Error should contain useful context
    assert!(err.contains("conflict"), "Error should mention conflict");
    assert!(
        err.contains("branch_a") || err.contains("branch_b"),
        "Error should include branch names"
    );
}

#[test]
fn test_merge_strategy_merge_deep_nested_objects() {
    let base = json!({
        "data": {
            "level1": {
                "original": "value"
            }
        }
    });
    let branch_results = vec![
        BranchResult {
            branch_name: "a".to_string(),
            branch_index: 0,
            final_state: json!({
                "data": {
                    "level1": {
                        "original": "value",
                        "from_a": "added_by_a"
                    }
                }
            }),
        },
        BranchResult {
            branch_name: "b".to_string(),
            branch_index: 1,
            final_state: json!({
                "data": {
                    "level1": {
                        "original": "value",
                        "from_b": "added_by_b"
                    }
                }
            }),
        },
    ];

    let result = apply_merge_strategy(MergeStrategy::MergeDeep, &base, &branch_results).unwrap();

    // Deep merge should preserve all nested additions
    assert_eq!(result["data"]["level1"]["original"], "value");
    assert_eq!(result["data"]["level1"]["from_a"], "added_by_a");
    assert_eq!(result["data"]["level1"]["from_b"], "added_by_b");
}

#[test]
fn test_detect_conflicts_nested_keys() {
    let base = json!({"nested": {"deep": {}}});
    let branch_results = vec![
        BranchResult {
            branch_name: "a".to_string(),
            branch_index: 0,
            final_state: json!({"nested": {"deep": {"key": "value_a"}}}),
        },
        BranchResult {
            branch_name: "b".to_string(),
            branch_index: 1,
            final_state: json!({"nested": {"deep": {"key": "value_b"}}}),
        },
    ];

    let conflicts = detect_conflicts(&base, &branch_results);

    assert!(!conflicts.is_empty(), "Should detect nested conflict");
    // The conflict should be at the nested path
    assert!(
        conflicts.iter().any(|c| c.key_path.contains("nested")),
        "Conflict path should include nested structure"
    );
}

#[test]
fn test_no_conflict_for_identical_values() {
    let base = json!({});
    let branch_results = vec![
        BranchResult {
            branch_name: "a".to_string(),
            branch_index: 0,
            final_state: json!({"result": {"score": 0.95, "label": "positive"}}),
        },
        BranchResult {
            branch_name: "b".to_string(),
            branch_index: 1,
            final_state: json!({"result": {"score": 0.95, "label": "positive"}}),
        },
    ];

    let conflicts = detect_conflicts(&base, &branch_results);

    assert!(
        conflicts.is_empty(),
        "Identical values should not create conflicts"
    );
}

#[test]
fn test_parse_parallel_settings_from_yaml() {
    let yaml = r#"
name: test-with-settings
settings:
  parallel:
    merge_strategy: fail_on_conflict
nodes:
  - name: start
"#;
    let config = parse_yaml_config(yaml).unwrap();
    let settings = config.effective_settings();

    assert!(settings.parallel.is_some());
    assert_eq!(
        settings.parallel.unwrap().merge_strategy,
        MergeStrategy::FailOnConflict
    );
}

#[test]
fn test_parse_parallel_settings_defaults_to_isolated() {
    let yaml = r#"
name: test-without-settings
nodes:
  - name: start
"#;
    let config = parse_yaml_config(yaml).unwrap();
    let settings = config.effective_settings();

    // Default merge strategy is Isolated
    let strategy = settings
        .parallel
        .map(|p| p.merge_strategy)
        .unwrap_or_default();

    assert_eq!(strategy, MergeStrategy::Isolated);
}

#[test]
fn test_parse_all_merge_strategies() {
    for (strategy_str, expected) in [
        ("isolated", MergeStrategy::Isolated),
        ("last_write_wins", MergeStrategy::LastWriteWins),
        ("merge_deep", MergeStrategy::MergeDeep),
        ("fail_on_conflict", MergeStrategy::FailOnConflict),
    ] {
        let yaml = format!(
            r#"
name: test
settings:
  parallel:
    merge_strategy: {}
nodes:
  - name: start
"#,
            strategy_str
        );
        let config = parse_yaml_config(&yaml).unwrap();
        let strategy = config
            .effective_settings()
            .parallel
            .unwrap()
            .merge_strategy;

        assert_eq!(
            strategy, expected,
            "Failed to parse strategy: {}",
            strategy_str
        );
    }
}
