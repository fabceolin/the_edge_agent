//! Integration tests for Tera template engine
//!
//! TEA-WASM-001.2: Tests template rendering with real-world scenarios.

use serde_json::json;
use std::collections::HashMap;
use tea_wasm_llm::{
    render_template, render_template_with_config, sanitize_context_value, TemplateSecurityConfig,
};

/// Test rendering prompts with state interpolation
#[test]
fn test_render_llm_prompt() {
    let state = json!({
        "question": "What is the capital of France?",
        "context": "France is a country in Western Europe."
    });
    let template = "Based on this context: {{ state.context }}\n\nAnswer: {{ state.question }}";
    let result = render_template(template, &state, &HashMap::new()).unwrap();
    assert!(result.as_str().unwrap().contains("France"));
    assert!(result.as_str().unwrap().contains("capital"));
}

/// Test variables interpolation for configuration
#[test]
fn test_render_with_variables() {
    let state = json!({"input": "Hello"});
    let mut variables = HashMap::new();
    variables.insert("model".to_string(), json!("gpt-4"));
    variables.insert("max_tokens".to_string(), json!(100));

    let template = "Model: {{ variables.model }}, Tokens: {{ variables.max_tokens }}";
    let result = render_template(template, &state, &variables).unwrap();
    assert!(result.as_str().unwrap().contains("gpt-4"));
    assert!(result.as_str().unwrap().contains("100"));
}

/// Test conditional rendering for workflow logic
#[test]
fn test_conditional_workflow() {
    let state = json!({"has_results": true, "count": 5});
    let template = r#"{% if state.has_results %}Found {{ state.count }} results{% else %}No results{% endif %}"#;
    let result = render_template(template, &state, &HashMap::new()).unwrap();
    assert_eq!(result.as_str().unwrap(), "Found 5 results");
}

/// Test loop rendering for lists
#[test]
fn test_loop_rendering() {
    let state = json!({
        "items": [
            {"name": "Alice", "score": 95},
            {"name": "Bob", "score": 87},
            {"name": "Carol", "score": 92}
        ]
    });
    let template =
        r#"{% for item in state.items %}{{ item.name }}: {{ item.score }}{% if not loop.last %}, {% endif %}{% endfor %}"#;
    let result = render_template(template, &state, &HashMap::new()).unwrap();
    let s = result.as_str().unwrap();
    assert!(s.contains("Alice: 95"));
    assert!(s.contains("Bob: 87"));
    assert!(s.contains("Carol: 92"));
}

/// Test deeply nested state access
#[test]
fn test_deeply_nested_access() {
    let state = json!({
        "user": {
            "profile": {
                "settings": {
                    "theme": "dark"
                }
            }
        }
    });
    let template = "Theme: {{ state.user.profile.settings.theme }}";
    let result = render_template(template, &state, &HashMap::new()).unwrap();
    assert_eq!(result.as_str().unwrap(), "Theme: dark");
}

/// Test filter chaining
#[test]
fn test_filter_chaining() {
    let state = json!({"name": "  alice  "});
    let template = "{{ state.name | trim | upper }}";
    let result = render_template(template, &state, &HashMap::new()).unwrap();
    assert_eq!(result.as_str().unwrap(), "ALICE");
}

/// Test tojson filter for JSON serialization
#[test]
fn test_tojson_for_serialization() {
    let state = json!({"config": {"key": "value", "num": 42}});
    let template = "Config: {{ state.config | tojson }}";
    let result = render_template(template, &state, &HashMap::new()).unwrap();
    let s = result.as_str().unwrap();
    assert!(s.contains("key"));
    assert!(s.contains("value"));
}

/// Test default filter for missing values
#[test]
fn test_default_for_missing() {
    let state = json!({});
    let template = "Name: {{ state.name | default(value='Unknown') }}";
    let result = render_template(template, &state, &HashMap::new()).unwrap();
    assert_eq!(result.as_str().unwrap(), "Name: Unknown");
}

/// Test multiline template rendering
#[test]
fn test_multiline_template() {
    let state = json!({
        "title": "Report",
        "sections": ["Introduction", "Body", "Conclusion"]
    });
    let template = r#"# {{ state.title }}

## Table of Contents
{% for section in state.sections %}
- {{ section }}
{% endfor %}"#;
    let result = render_template(template, &state, &HashMap::new()).unwrap();
    let s = result.as_str().unwrap();
    assert!(s.contains("# Report"));
    assert!(s.contains("- Introduction"));
    assert!(s.contains("- Conclusion"));
}

// Security Tests

/// Test that prototype pollution keys are blocked
#[test]
fn test_security_prototype_pollution_blocked() {
    let state = json!({
        "__proto__": {"polluted": true},
        "safe": "value"
    });
    let config = TemplateSecurityConfig::default();
    let sanitized = sanitize_context_value(&state, &config).unwrap();
    assert!(!sanitized.as_object().unwrap().contains_key("__proto__"));
    assert!(sanitized.as_object().unwrap().contains_key("safe"));
}

/// Test that deep nesting is blocked
#[test]
fn test_security_deep_nesting_blocked() {
    let mut deep = json!("leaf");
    for _ in 0..50 {
        deep = json!({"nested": deep});
    }
    let config = TemplateSecurityConfig::default();
    let result = sanitize_context_value(&deep, &config);
    assert!(result.is_err());
}

/// Test custom security config
#[test]
fn test_custom_security_config() {
    let state = json!({"name": "test"});
    let config = TemplateSecurityConfig {
        max_iterations: 100,
        max_recursion: 10,
        max_state_depth: 5,
        ..Default::default()
    };
    let result = render_template_with_config("{{ state.name }}", &state, &HashMap::new(), config);
    assert!(result.is_ok());
}

/// Test error handling for undefined variables
#[test]
fn test_undefined_variable_handling() {
    let state = json!({});
    // Tera errors on undefined variables when accessing nested fields
    let result = render_template("Hello {{ state.missing }}", &state, &HashMap::new());
    // Should error (Tera doesn't silently ignore undefined)
    assert!(result.is_err(), "Expected error for undefined variable access");
}

/// Test complex real-world template
#[test]
fn test_real_world_qa_template() {
    let state = json!({
        "question": "What is 2+2?",
        "think": {
            "content": "Let me calculate: 2+2=4"
        },
        "prolog_result": {
            "bindings": {"Category": "math"}
        }
    });

    let template = r#"Question: {{ state.question }}
Reasoning: {{ state.think.content }}
Category: {{ state.prolog_result.bindings.Category }}"#;

    let result = render_template(template, &state, &HashMap::new()).unwrap();
    let s = result.as_str().unwrap();
    assert!(s.contains("What is 2+2?"));
    assert!(s.contains("2+2=4"));
    assert!(s.contains("math"));
}
