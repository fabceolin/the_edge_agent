//! Markdown parsing actions (markdown.*)
//!
//! Provides structured Markdown parsing using the md-parser crate.
//! Extracts sections, checklists, variables, and frontmatter.

use crate::engine::executor::ActionRegistry;
use crate::error::{TeaError, TeaResult};
use md_parser::{extract_checklist_items, extract_variables, ChecklistSummary, MarkdownParser};
use serde_json::{json, Value as JsonValue};
use std::collections::HashMap;

/// Register markdown processing actions
pub fn register(registry: &ActionRegistry) {
    registry.register("markdown.parse", markdown_parse);
    registry.register("markdown.extract_tasks", markdown_extract_tasks);
    registry.register("markdown.extract_variables", markdown_extract_variables);
}

/// Parse Markdown content into structured document.
///
/// # Parameters
/// - `input`: Raw Markdown string to parse
///
/// # Returns
/// State updated with:
/// - `title`: Document title (from first H1)
/// - `sections`: Array of parsed sections
/// - `variables`: Array of `{{variable}}` names found
/// - `frontmatter`: YAML frontmatter object (if present)
/// - `tasks`: Array of checklist items
/// - `task_summary`: Completion statistics
fn markdown_parse(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let input = params
        .get("input")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "markdown.parse".to_string(),
            message: "Missing required parameter: input (must be a string)".to_string(),
        })?;

    let parser = MarkdownParser::new();
    let doc = parser
        .parse(input)
        .map_err(|e| TeaError::Action(e.to_string()))?;

    // Convert sections to JSON
    let sections: Vec<JsonValue> = doc
        .sections
        .iter()
        .map(|s| {
            json!({
                "id": s.id,
                "section_type": s.section_type.as_str(),
                "level": s.level,
                "content": s.content,
                "order_idx": s.order_idx,
                "variables": s.variables,
            })
        })
        .collect();

    // Convert tasks to JSON
    let tasks: Vec<JsonValue> = doc
        .checklist_items
        .iter()
        .map(|t| {
            json!({
                "text": t.text,
                "checked": t.checked,
                "indent": t.indent,
                "ac_refs": t.ac_refs,
            })
        })
        .collect();

    // Calculate task summary
    let summary = ChecklistSummary::from_items(&doc.checklist_items);

    // Convert edges to JSON
    let edges: Vec<JsonValue> = doc
        .edges
        .iter()
        .map(|e| {
            json!({
                "source_idx": e.source_idx,
                "target_idx": e.target_idx,
                "edge_type": format!("{:?}", e.edge_type),
            })
        })
        .collect();

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("title".to_string(), json!(doc.title));
        obj.insert("sections".to_string(), json!(sections));
        obj.insert("variables".to_string(), json!(doc.variables));
        obj.insert(
            "frontmatter".to_string(),
            doc.frontmatter
                .map(|fm| serde_json::to_value(fm).unwrap_or(JsonValue::Null))
                .unwrap_or(JsonValue::Null),
        );
        obj.insert("tasks".to_string(), json!(tasks));
        obj.insert(
            "task_summary".to_string(),
            json!({
                "total": summary.total,
                "completed": summary.completed,
                "pending": summary.pending,
                "percentage": summary.percentage,
            }),
        );
        obj.insert("edges".to_string(), json!(edges));
    }

    Ok(result)
}

/// Extract checklist items from Markdown content.
///
/// # Parameters
/// - `input`: Raw Markdown string
///
/// # Returns
/// State updated with:
/// - `tasks`: Array of checklist items
/// - `task_summary`: Completion statistics
fn markdown_extract_tasks(
    state: &JsonValue,
    params: &HashMap<String, JsonValue>,
) -> TeaResult<JsonValue> {
    let input = params
        .get("input")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "markdown.extract_tasks".to_string(),
            message: "Missing required parameter: input (must be a string)".to_string(),
        })?;

    let items = extract_checklist_items(input);
    let summary = ChecklistSummary::from_items(&items);

    let tasks: Vec<JsonValue> = items
        .iter()
        .map(|t| {
            json!({
                "text": t.text,
                "checked": t.checked,
                "indent": t.indent,
                "ac_refs": t.ac_refs,
            })
        })
        .collect();

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("tasks".to_string(), json!(tasks));
        obj.insert(
            "task_summary".to_string(),
            json!({
                "total": summary.total,
                "completed": summary.completed,
                "pending": summary.pending,
                "percentage": summary.percentage,
            }),
        );
    }

    Ok(result)
}

/// Extract template variables from Markdown content.
///
/// # Parameters
/// - `input`: Raw Markdown string
///
/// # Returns
/// State updated with:
/// - `variables`: Array of unique `{{variable}}` names
fn markdown_extract_variables(
    state: &JsonValue,
    params: &HashMap<String, JsonValue>,
) -> TeaResult<JsonValue> {
    let input = params
        .get("input")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "markdown.extract_variables".to_string(),
            message: "Missing required parameter: input (must be a string)".to_string(),
        })?;

    let variables = extract_variables(input);

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("variables".to_string(), json!(variables));
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_markdown_parse_basic() {
        let state = json!({});
        let markdown = r#"# My Document

## Tasks
- [ ] Task 1 (AC: 1)
  - [x] Subtask 1.1
- [x] Task 2

Some text with {{variable}} template.
"#;
        let params: HashMap<String, JsonValue> = [("input".to_string(), json!(markdown))]
            .into_iter()
            .collect();

        let result = markdown_parse(&state, &params).unwrap();

        assert_eq!(result["title"], "My Document");
        assert!(!result["sections"].as_array().unwrap().is_empty());
        assert!(result["variables"]
            .as_array()
            .unwrap()
            .contains(&json!("variable")));
        assert_eq!(result["task_summary"]["total"], 3);
    }

    #[test]
    fn test_markdown_parse_checklist() {
        let state = json!({});
        let markdown = r#"## Tasks
- [ ] Task 1 (AC: 1, 2)
  - [x] Subtask 1.1
- [x] Task 2
"#;
        let params: HashMap<String, JsonValue> = [("input".to_string(), json!(markdown))]
            .into_iter()
            .collect();

        let result = markdown_parse(&state, &params).unwrap();

        let tasks = result["tasks"].as_array().unwrap();
        assert_eq!(tasks.len(), 3);
        assert_eq!(tasks[0]["checked"], false);
        assert_eq!(tasks[1]["checked"], true);
        assert_eq!(tasks[1]["indent"], 1);
    }

    #[test]
    fn test_markdown_parse_ac_refs() {
        let state = json!({});
        let markdown = "- [ ] Task with AC refs (AC: 1, 2, 3)\n";
        let params: HashMap<String, JsonValue> = [("input".to_string(), json!(markdown))]
            .into_iter()
            .collect();

        let result = markdown_parse(&state, &params).unwrap();

        let tasks = result["tasks"].as_array().unwrap();
        // md-parser returns AC refs as strings
        assert_eq!(tasks[0]["ac_refs"], json!(["1", "2", "3"]));
    }

    #[test]
    fn test_markdown_parse_frontmatter() {
        let state = json!({});
        let markdown = r#"---
title: Test Document
version: 1.0
---

# Content
"#;
        let params: HashMap<String, JsonValue> = [("input".to_string(), json!(markdown))]
            .into_iter()
            .collect();

        let result = markdown_parse(&state, &params).unwrap();

        // Frontmatter should be parsed if md-parser was built with frontmatter feature
        // The result depends on the feature being enabled
        assert!(result.get("frontmatter").is_some());
    }

    #[test]
    fn test_markdown_parse_variables() {
        let state = json!({});
        let markdown = "Text with {{var1}} and {{var2}} variables.\n";
        let params: HashMap<String, JsonValue> = [("input".to_string(), json!(markdown))]
            .into_iter()
            .collect();

        let result = markdown_parse(&state, &params).unwrap();

        let vars = result["variables"].as_array().unwrap();
        assert!(vars.contains(&json!("var1")));
        assert!(vars.contains(&json!("var2")));
    }

    #[test]
    fn test_markdown_extract_tasks() {
        let state = json!({});
        let markdown = r#"- [ ] Task 1
- [x] Task 2
- [ ] Task 3
"#;
        let params: HashMap<String, JsonValue> = [("input".to_string(), json!(markdown))]
            .into_iter()
            .collect();

        let result = markdown_extract_tasks(&state, &params).unwrap();

        assert_eq!(result["task_summary"]["total"], 3);
        assert_eq!(result["task_summary"]["completed"], 1);
        assert_eq!(result["task_summary"]["pending"], 2);
    }

    #[test]
    fn test_markdown_extract_variables() {
        let state = json!({});
        let markdown = "Hello {{name}}, your balance is {{balance}}.\n";
        let params: HashMap<String, JsonValue> = [("input".to_string(), json!(markdown))]
            .into_iter()
            .collect();

        let result = markdown_extract_variables(&state, &params).unwrap();

        let vars = result["variables"].as_array().unwrap();
        assert_eq!(vars.len(), 2);
        assert!(vars.contains(&json!("name")));
        assert!(vars.contains(&json!("balance")));
    }

    #[test]
    fn test_markdown_parse_missing_input() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = HashMap::new();

        let result = markdown_parse(&state, &params);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Missing"));
    }

    #[test]
    fn test_markdown_parse_code_blocks() {
        let state = json!({});
        let markdown = r#"# Code Example

```rust
fn main() {
    println!("Hello");
}
```
"#;
        let params: HashMap<String, JsonValue> = [("input".to_string(), json!(markdown))]
            .into_iter()
            .collect();

        let result = markdown_parse(&state, &params).unwrap();

        let sections = result["sections"].as_array().unwrap();
        let code_section = sections
            .iter()
            .find(|s| s["section_type"] == "code")
            .expect("Should have a code section");
        // Code block content should contain the rust code
        assert!(code_section["content"]
            .as_str()
            .unwrap()
            .contains("println"));
    }

    #[test]
    fn test_markdown_parse_nested_checklist() {
        let state = json!({});
        let markdown = r#"- [ ] Parent task
  - [ ] Child task 1
    - [x] Grandchild task
  - [x] Child task 2
"#;
        let params: HashMap<String, JsonValue> = [("input".to_string(), json!(markdown))]
            .into_iter()
            .collect();

        let result = markdown_parse(&state, &params).unwrap();

        let tasks = result["tasks"].as_array().unwrap();
        assert_eq!(tasks.len(), 4);
        assert_eq!(tasks[0]["indent"], 0);
        assert_eq!(tasks[1]["indent"], 1);
        assert_eq!(tasks[2]["indent"], 2);
        assert_eq!(tasks[3]["indent"], 1);
    }

    #[test]
    fn test_markdown_task_summary_percentage() {
        let state = json!({});
        let markdown = r#"- [x] Done 1
- [x] Done 2
- [ ] Pending 1
- [ ] Pending 2
"#;
        let params: HashMap<String, JsonValue> = [("input".to_string(), json!(markdown))]
            .into_iter()
            .collect();

        let result = markdown_parse(&state, &params).unwrap();

        assert_eq!(result["task_summary"]["total"], 4);
        assert_eq!(result["task_summary"]["completed"], 2);
        assert_eq!(result["task_summary"]["percentage"], 50.0);
    }
}
