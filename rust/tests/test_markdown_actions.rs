//! Integration tests for markdown actions (TEA-RALPHY-001.2)

use serde_json::json;
use std::collections::HashMap;
use the_edge_agent::actions::markdown;
use the_edge_agent::engine::executor::ActionRegistry;

/// Test markdown.parse action registration and invocation
#[test]
fn test_markdown_parse_action_registration() {
    let registry = ActionRegistry::new();
    markdown::register(&registry);

    assert!(registry.has("markdown.parse"));
    assert!(registry.has("markdown.extract_tasks"));
    assert!(registry.has("markdown.extract_variables"));
}

/// Test markdown.parse with basic document
#[test]
fn test_markdown_parse_basic_document() {
    let state = json!({});
    let markdown_content = r#"# My Document

## Introduction
This is the introduction paragraph.

## Tasks
- [ ] Task 1 (AC: 1, 2)
- [x] Task 2 (AC: 3)
  - [x] Subtask 2.1

## Code Example
```rust
fn main() {
    println!("Hello");
}
```

Text with {{variable1}} and {{variable2}}.
"#;

    let mut params = HashMap::new();
    params.insert("input".to_string(), json!(markdown_content));

    let registry = ActionRegistry::new();
    markdown::register(&registry);

    let action = registry.get("markdown.parse").unwrap();
    let result = action(&state, &params).unwrap();

    // Verify title extraction
    assert_eq!(result["title"], "My Document");

    // Verify sections
    let sections = result["sections"].as_array().unwrap();
    assert!(!sections.is_empty());

    // Verify checklist items
    let tasks = result["tasks"].as_array().unwrap();
    assert_eq!(tasks.len(), 3);
    assert_eq!(tasks[0]["checked"], false);
    assert_eq!(tasks[1]["checked"], true);
    assert_eq!(tasks[2]["checked"], true);
    assert_eq!(tasks[2]["indent"], 1);

    // Verify AC refs extraction (md-parser returns strings)
    assert_eq!(tasks[0]["ac_refs"], json!(["1", "2"]));
    assert_eq!(tasks[1]["ac_refs"], json!(["3"]));

    // Verify variables
    let vars = result["variables"].as_array().unwrap();
    assert!(vars.contains(&json!("variable1")));
    assert!(vars.contains(&json!("variable2")));

    // Verify task summary
    assert_eq!(result["task_summary"]["total"], 3);
    assert_eq!(result["task_summary"]["completed"], 2);
    assert_eq!(result["task_summary"]["pending"], 1);
}

/// Test markdown.extract_tasks action
#[test]
fn test_markdown_extract_tasks_action() {
    let state = json!({});
    let markdown_content = r#"
- [ ] First task
- [x] Second task (completed)
- [ ] Third task (AC: 1)
  - [x] Nested task
"#;

    let mut params = HashMap::new();
    params.insert("input".to_string(), json!(markdown_content));

    let registry = ActionRegistry::new();
    markdown::register(&registry);

    let action = registry.get("markdown.extract_tasks").unwrap();
    let result = action(&state, &params).unwrap();

    let tasks = result["tasks"].as_array().unwrap();
    assert_eq!(tasks.len(), 4);

    // Verify summary
    assert_eq!(result["task_summary"]["total"], 4);
    assert_eq!(result["task_summary"]["completed"], 2);
    assert_eq!(result["task_summary"]["percentage"], 50.0);
}

/// Test markdown.extract_variables action
#[test]
fn test_markdown_extract_variables_action() {
    let state = json!({});
    let markdown_content = "Hello {{name}}, your order {{order_id}} is ready. Amount: {{amount}}";

    let mut params = HashMap::new();
    params.insert("input".to_string(), json!(markdown_content));

    let registry = ActionRegistry::new();
    markdown::register(&registry);

    let action = registry.get("markdown.extract_variables").unwrap();
    let result = action(&state, &params).unwrap();

    let vars = result["variables"].as_array().unwrap();
    assert_eq!(vars.len(), 3);
    assert!(vars.contains(&json!("name")));
    assert!(vars.contains(&json!("order_id")));
    assert!(vars.contains(&json!("amount")));
}

/// Test markdown.parse with frontmatter
#[test]
fn test_markdown_parse_with_frontmatter() {
    let state = json!({});
    let markdown_content = r#"---
title: Test Document
version: 1.0
author: Test Author
---

# Document Content

Some paragraph text.
"#;

    let mut params = HashMap::new();
    params.insert("input".to_string(), json!(markdown_content));

    let registry = ActionRegistry::new();
    markdown::register(&registry);

    let action = registry.get("markdown.parse").unwrap();
    let result = action(&state, &params).unwrap();

    // Frontmatter should be parsed
    assert!(result.get("frontmatter").is_some());
    // The exact content depends on whether frontmatter feature is enabled
}

/// Test markdown.parse with empty input
#[test]
fn test_markdown_parse_empty_input() {
    let state = json!({});

    let mut params = HashMap::new();
    params.insert("input".to_string(), json!(""));

    let registry = ActionRegistry::new();
    markdown::register(&registry);

    let action = registry.get("markdown.parse").unwrap();
    let result = action(&state, &params).unwrap();

    assert!(result["title"].is_null());
    assert_eq!(result["tasks"].as_array().unwrap().len(), 0);
    assert_eq!(result["variables"].as_array().unwrap().len(), 0);
}

/// Test markdown.parse with malformed markdown (graceful handling)
#[test]
fn test_markdown_parse_malformed_input() {
    let state = json!({});
    let markdown_content = r#"
# Unbalanced heading

Some text

## Another heading without content

- Incomplete list item
  - [incomplete checkbox
  - [ ] Valid checkbox
"#;

    let mut params = HashMap::new();
    params.insert("input".to_string(), json!(markdown_content));

    let registry = ActionRegistry::new();
    markdown::register(&registry);

    let action = registry.get("markdown.parse").unwrap();
    let result = action(&state, &params);

    // Should not error, just return partial results
    assert!(result.is_ok());
    let result = result.unwrap();
    assert!(result.get("sections").is_some());
}

/// Test markdown.parse with BMad story format
#[test]
fn test_markdown_parse_bmad_story_format() {
    let state = json!({});
    let markdown_content = r#"# Story TEA-EXAMPLE-001: Example Story

## Status
Draft

## Story

**As a** developer,
**I want** example functionality,
**So that** I can test parsing.

## Acceptance Criteria

1. First criterion
2. Second criterion
3. Third criterion

## Tasks / Subtasks

### Phase 1: Setup

- [ ] Initialize project (AC: 1)
  - [x] Create directory structure
  - [ ] Add configuration files
- [x] Setup CI/CD (AC: 2)

### Phase 2: Implementation

- [ ] Implement feature (AC: 3)
  - [ ] Core logic
  - [ ] Tests

## Dev Notes

Some notes with {{variable}} references.

## Change Log

| Date | Version | Description |
|------|---------|-------------|
| 2025-01-19 | 0.1 | Initial |
"#;

    let mut params = HashMap::new();
    params.insert("input".to_string(), json!(markdown_content));

    let registry = ActionRegistry::new();
    markdown::register(&registry);

    let action = registry.get("markdown.parse").unwrap();
    let result = action(&state, &params).unwrap();

    // Verify title extraction
    assert_eq!(result["title"], "Story TEA-EXAMPLE-001: Example Story");

    // Verify tasks
    let tasks = result["tasks"].as_array().unwrap();
    assert_eq!(tasks.len(), 7); // 4 top-level + 3 subtasks

    // Verify nested structure
    let phase1_setup = &tasks[0];
    assert_eq!(phase1_setup["checked"], false);
    assert_eq!(phase1_setup["indent"], 0);
    assert_eq!(phase1_setup["ac_refs"], json!(["1"]));

    // Verify variable detection
    let vars = result["variables"].as_array().unwrap();
    assert!(vars.contains(&json!("variable")));

    // Verify completion stats
    let summary = &result["task_summary"];
    assert_eq!(summary["total"], 7);
}

/// Test markdown.parse preserves state
#[test]
fn test_markdown_parse_preserves_state() {
    let state = json!({
        "existing_key": "existing_value",
        "another_key": 42
    });

    let mut params = HashMap::new();
    params.insert("input".to_string(), json!("# Test\n\nContent"));

    let registry = ActionRegistry::new();
    markdown::register(&registry);

    let action = registry.get("markdown.parse").unwrap();
    let result = action(&state, &params).unwrap();

    // Original state should be preserved
    assert_eq!(result["existing_key"], "existing_value");
    assert_eq!(result["another_key"], 42);

    // New fields should be added
    assert_eq!(result["title"], "Test");
}

/// Test deeply nested checklist items
#[test]
fn test_markdown_parse_deeply_nested_checklist() {
    let state = json!({});
    let markdown_content = r#"
- [ ] Level 0
  - [ ] Level 1
    - [ ] Level 2
      - [x] Level 3
        - [x] Level 4
"#;

    let mut params = HashMap::new();
    params.insert("input".to_string(), json!(markdown_content));

    let registry = ActionRegistry::new();
    markdown::register(&registry);

    let action = registry.get("markdown.parse").unwrap();
    let result = action(&state, &params).unwrap();

    let tasks = result["tasks"].as_array().unwrap();
    assert_eq!(tasks.len(), 5);

    // Verify indent levels
    assert_eq!(tasks[0]["indent"], 0);
    assert_eq!(tasks[1]["indent"], 1);
    assert_eq!(tasks[2]["indent"], 2);
    assert_eq!(tasks[3]["indent"], 3);
    assert_eq!(tasks[4]["indent"], 4);
}

/// Test error handling for missing input parameter
#[test]
fn test_markdown_parse_missing_input() {
    let state = json!({});
    let params = HashMap::new();

    let registry = ActionRegistry::new();
    markdown::register(&registry);

    let action = registry.get("markdown.parse").unwrap();
    let result = action(&state, &params);

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("Missing"));
}
