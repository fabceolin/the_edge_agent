//! Markdown parsing WASM bindings (TEA-RALPHY-001.2)
//!
//! Provides markdown.parse action for WASM environments.

use md_parser::{extract_checklist_items, extract_variables, ChecklistSummary, MarkdownParser};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};
use wasm_bindgen::prelude::*;

/// Parsed document result for JavaScript
#[derive(Debug, Clone, Serialize, Deserialize)]
#[wasm_bindgen(getter_with_clone)]
pub struct MarkdownParseResult {
    /// Document title (from first H1)
    pub title: Option<String>,
    /// JSON string of parsed sections
    pub sections_json: String,
    /// JSON string of variable names
    pub variables_json: String,
    /// JSON string of frontmatter (if present)
    pub frontmatter_json: String,
    /// JSON string of checklist items
    pub tasks_json: String,
    /// JSON string of task summary
    pub task_summary_json: String,
    /// JSON string of section edges
    pub edges_json: String,
}

/// Parse Markdown content into structured document.
///
/// # Arguments
/// * `content` - Raw Markdown string to parse
///
/// # Returns
/// MarkdownParseResult with JSON strings for each component
#[wasm_bindgen]
pub fn markdown_parse(content: &str) -> Result<MarkdownParseResult, JsError> {
    let parser = MarkdownParser::new();
    let doc = parser
        .parse(content)
        .map_err(|e| JsError::new(&e.to_string()))?;

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

    let frontmatter_json = doc
        .frontmatter
        .map(|fm| serde_json::to_string(&fm).unwrap_or_else(|_| "null".to_string()))
        .unwrap_or_else(|| "null".to_string());

    Ok(MarkdownParseResult {
        title: doc.title,
        sections_json: serde_json::to_string(&sections).unwrap_or_else(|_| "[]".to_string()),
        variables_json: serde_json::to_string(&doc.variables).unwrap_or_else(|_| "[]".to_string()),
        frontmatter_json,
        tasks_json: serde_json::to_string(&tasks).unwrap_or_else(|_| "[]".to_string()),
        task_summary_json: serde_json::to_string(&json!({
            "total": summary.total,
            "completed": summary.completed,
            "pending": summary.pending,
            "percentage": summary.percentage,
        }))
        .unwrap_or_else(|_| "{}".to_string()),
        edges_json: serde_json::to_string(&edges).unwrap_or_else(|_| "[]".to_string()),
    })
}

/// Parse Markdown and return complete result as JSON string.
///
/// This is a convenience function for JavaScript that returns everything
/// as a single JSON object.
#[wasm_bindgen]
pub fn markdown_parse_json(content: &str) -> Result<String, JsError> {
    let result = markdown_parse(content)?;

    let json_result = json!({
        "title": result.title,
        "sections": serde_json::from_str::<JsonValue>(&result.sections_json).unwrap_or(JsonValue::Null),
        "variables": serde_json::from_str::<JsonValue>(&result.variables_json).unwrap_or(JsonValue::Null),
        "frontmatter": serde_json::from_str::<JsonValue>(&result.frontmatter_json).unwrap_or(JsonValue::Null),
        "tasks": serde_json::from_str::<JsonValue>(&result.tasks_json).unwrap_or(JsonValue::Null),
        "task_summary": serde_json::from_str::<JsonValue>(&result.task_summary_json).unwrap_or(JsonValue::Null),
        "edges": serde_json::from_str::<JsonValue>(&result.edges_json).unwrap_or(JsonValue::Null),
    });

    serde_json::to_string(&json_result).map_err(|e| JsError::new(&e.to_string()))
}

/// Extract checklist items from Markdown content.
///
/// # Arguments
/// * `content` - Raw Markdown string
///
/// # Returns
/// JSON string with tasks array and summary
#[wasm_bindgen]
pub fn markdown_extract_tasks(content: &str) -> Result<String, JsError> {
    let items = extract_checklist_items(content);
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

    let result = json!({
        "tasks": tasks,
        "task_summary": {
            "total": summary.total,
            "completed": summary.completed,
            "pending": summary.pending,
            "percentage": summary.percentage,
        }
    });

    serde_json::to_string(&result).map_err(|e| JsError::new(&e.to_string()))
}

/// Extract template variables from Markdown content.
///
/// # Arguments
/// * `content` - Raw Markdown string
///
/// # Returns
/// JSON array of unique variable names
#[wasm_bindgen]
pub fn markdown_extract_variables(content: &str) -> Result<String, JsError> {
    let variables = extract_variables(content);
    serde_json::to_string(&variables).map_err(|e| JsError::new(&e.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wasm_markdown_parse() {
        let content = r#"# Test Document

## Tasks
- [ ] Task 1 (AC: 1)
- [x] Task 2

Text with {{variable}}.
"#;
        let result = markdown_parse(content).unwrap();
        assert_eq!(result.title, Some("Test Document".to_string()));

        let tasks: Vec<JsonValue> = serde_json::from_str(&result.tasks_json).unwrap();
        assert_eq!(tasks.len(), 2);

        let vars: Vec<String> = serde_json::from_str(&result.variables_json).unwrap();
        assert!(vars.contains(&"variable".to_string()));
    }

    #[test]
    fn test_wasm_markdown_parse_json() {
        let content = "# Hello\n\nWorld";
        let json_str = markdown_parse_json(content).unwrap();
        let parsed: JsonValue = serde_json::from_str(&json_str).unwrap();
        assert_eq!(parsed["title"], "Hello");
    }

    #[test]
    fn test_wasm_extract_tasks() {
        let content = "- [ ] Task 1\n- [x] Task 2\n- [ ] Task 3";
        let json_str = markdown_extract_tasks(content).unwrap();
        let parsed: JsonValue = serde_json::from_str(&json_str).unwrap();
        assert_eq!(parsed["task_summary"]["total"], 3);
        assert_eq!(parsed["task_summary"]["completed"], 1);
    }

    #[test]
    fn test_wasm_extract_variables() {
        let content = "Hello {{name}}, your balance is {{balance}}.";
        let json_str = markdown_extract_variables(content).unwrap();
        let vars: Vec<String> = serde_json::from_str(&json_str).unwrap();
        assert_eq!(vars.len(), 2);
        assert!(vars.contains(&"name".to_string()));
        assert!(vars.contains(&"balance".to_string()));
    }
}
