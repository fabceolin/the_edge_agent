//! Integration tests for TEA-WASM-001.6: Action Parameter Standardization

use serde_json::json;
use std::collections::HashMap;
use tea_wasm_llm::{
    apply_defaults, extract_params, get_action_def, get_at_path, parse_yaml_config, set_at_path,
    validate_params, ActionDef, ParamError, WasmNodeConfig,
};

#[test]
fn test_set_at_path_deeply_nested() {
    let mut state = json!({});
    set_at_path(
        &mut state,
        "a.b.c.d.e",
        json!({"nested": "very deeply"}),
    );

    assert_eq!(state["a"]["b"]["c"]["d"]["e"]["nested"], "very deeply");
}

#[test]
fn test_set_at_path_with_existing_structure() {
    let mut state = json!({
        "result": {
            "existing": "value",
            "data": {
                "old": "data"
            }
        }
    });

    set_at_path(&mut state, "result.data.new", json!("added"));

    // Should preserve existing structure
    assert_eq!(state["result"]["existing"], "value");
    assert_eq!(state["result"]["data"]["old"], "data");
    assert_eq!(state["result"]["data"]["new"], "added");
}

#[test]
fn test_get_at_path_various_types() {
    let state = json!({
        "string": "hello",
        "number": 42,
        "boolean": true,
        "array": [1, 2, 3],
        "object": {"nested": "value"}
    });

    assert_eq!(get_at_path(&state, "string"), Some(&json!("hello")));
    assert_eq!(get_at_path(&state, "number"), Some(&json!(42)));
    assert_eq!(get_at_path(&state, "boolean"), Some(&json!(true)));
    assert_eq!(get_at_path(&state, "array"), Some(&json!([1, 2, 3])));
    assert_eq!(
        get_at_path(&state, "object.nested"),
        Some(&json!("value"))
    );
}

#[test]
fn test_validate_all_llm_call_params() {
    let action = get_action_def("llm.call").unwrap();

    // Missing prompt should fail
    let empty_params: HashMap<String, serde_json::Value> = HashMap::new();
    assert!(validate_params(action, &empty_params).is_err());

    // With prompt should pass
    let with_prompt: HashMap<String, serde_json::Value> =
        [("prompt".to_string(), json!("test"))].into();
    assert!(validate_params(action, &with_prompt).is_ok());
}

#[test]
fn test_validate_storage_read_params() {
    let action = get_action_def("storage.read").unwrap();

    // Missing uri should fail
    let empty_params: HashMap<String, serde_json::Value> = HashMap::new();
    let result = validate_params(action, &empty_params);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("uri"));

    // With uri should pass
    let with_uri: HashMap<String, serde_json::Value> =
        [("uri".to_string(), json!("memory://test.txt"))].into();
    assert!(validate_params(action, &with_uri).is_ok());
}

#[test]
fn test_validate_storage_write_params() {
    let action = get_action_def("storage.write").unwrap();

    // Missing both should mention first missing
    let empty_params: HashMap<String, serde_json::Value> = HashMap::new();
    let result = validate_params(action, &empty_params);
    assert!(result.is_err());

    // With only uri should still fail (needs content too)
    let with_uri: HashMap<String, serde_json::Value> =
        [("uri".to_string(), json!("memory://test.txt"))].into();
    let result = validate_params(action, &with_uri);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("content"));

    // With both should pass
    let complete: HashMap<String, serde_json::Value> = [
        ("uri".to_string(), json!("memory://test.txt")),
        ("content".to_string(), json!("hello")),
    ]
    .into();
    assert!(validate_params(action, &complete).is_ok());
}

#[test]
fn test_apply_defaults_llm_call() {
    let action = get_action_def("llm.call").unwrap();
    let mut params: HashMap<String, serde_json::Value> =
        [("prompt".to_string(), json!("test"))].into();

    apply_defaults(action, &mut params);

    // Check defaults are applied
    assert!(params.contains_key("temperature"));
    assert!(params.contains_key("max_tokens"));
    assert!(params.contains_key("top_p"));
    assert!(params.contains_key("top_k"));

    // Check values are reasonable
    let temp = params["temperature"].as_f64().unwrap();
    assert!(temp >= 0.0 && temp <= 2.0);

    let max_tokens = params["max_tokens"].as_u64().unwrap();
    assert!(max_tokens > 0);
}

#[test]
fn test_apply_defaults_storage_list() {
    let action = get_action_def("storage.list").unwrap();
    let mut params: HashMap<String, serde_json::Value> =
        [("uri".to_string(), json!("memory://"))].into();

    apply_defaults(action, &mut params);

    // Check limit default is applied
    assert!(params.contains_key("limit"));
    assert_eq!(params["limit"], 1000);
}

#[test]
fn test_extract_params_complex_template() {
    let node = WasmNodeConfig {
        name: "test".to_string(),
        params: Some(
            [
                ("prompt".to_string(), json!("Summarize: {{ state.text }}")),
                ("max_tokens".to_string(), json!(100)),
                (
                    "metadata".to_string(),
                    json!({
                        "source": "{{ state.source }}",
                        "timestamp": "{{ state.time }}"
                    }),
                ),
            ]
            .into(),
        ),
        action: None,
        uses: None,
        output: None,
        run: None,
        language: None,
        goto: None,
        steps: None,
    };

    let state = json!({
        "text": "Hello world",
        "source": "user-input",
        "time": "2026-01-17"
    });

    let params = extract_params(&node, &state, &HashMap::new()).unwrap();

    assert_eq!(params["prompt"], "Summarize: Hello world");
    assert_eq!(params["max_tokens"], 100);
    assert_eq!(params["metadata"]["source"], "user-input");
    assert_eq!(params["metadata"]["timestamp"], "2026-01-17");
}

#[test]
fn test_extract_params_with_variables() {
    let node = WasmNodeConfig {
        name: "test".to_string(),
        params: Some(
            [(
                "message".to_string(),
                json!("{{ variables.greeting }}, {{ state.name }}!"),
            )]
            .into(),
        ),
        action: None,
        uses: None,
        output: None,
        run: None,
        language: None,
        goto: None,
        steps: None,
    };

    let state = json!({"name": "Alice"});
    let variables: HashMap<String, serde_json::Value> =
        [("greeting".to_string(), json!("Hello"))].into();

    let params = extract_params(&node, &state, &variables).unwrap();

    assert_eq!(params["message"], "Hello, Alice!");
}

#[test]
fn test_extract_params_array_with_templates() {
    let node = WasmNodeConfig {
        name: "test".to_string(),
        params: Some(
            [(
                "items".to_string(),
                json!(["First: {{ state.a }}", "Second: {{ state.b }}"]),
            )]
            .into(),
        ),
        action: None,
        uses: None,
        output: None,
        run: None,
        language: None,
        goto: None,
        steps: None,
    };

    let state = json!({"a": "alpha", "b": "beta"});
    let params = extract_params(&node, &state, &HashMap::new()).unwrap();

    let items = params["items"].as_array().unwrap();
    assert_eq!(items[0], "First: alpha");
    assert_eq!(items[1], "Second: beta");
}

#[test]
fn test_action_def_passthrough() {
    let action = get_action_def("passthrough").unwrap();

    // Passthrough has no required params
    assert!(action.required.is_empty());

    // Should validate with empty params
    let empty: HashMap<String, serde_json::Value> = HashMap::new();
    assert!(validate_params(action, &empty).is_ok());
}

#[test]
fn test_action_def_return() {
    let action = get_action_def("return").unwrap();

    // Return has no required params
    assert!(action.required.is_empty());
}

#[test]
fn test_param_error_types() {
    // Test MissingRequired error
    let err = ParamError::MissingRequired {
        action: "llm.call".to_string(),
        param: "prompt".to_string(),
    };
    assert!(err.to_string().contains("Missing required"));
    assert!(err.to_string().contains("prompt"));
    assert!(err.to_string().contains("llm.call"));

    // Test InvalidType error
    let err = ParamError::InvalidType {
        param: "temperature".to_string(),
        expected: "number".to_string(),
        actual: "string".to_string(),
    };
    assert!(err.to_string().contains("Invalid parameter type"));
    assert!(err.to_string().contains("temperature"));

    // Test TemplateError
    let err = ParamError::TemplateError {
        param: "prompt".to_string(),
        message: "undefined variable".to_string(),
    };
    assert!(err.to_string().contains("Template error"));
    assert!(err.to_string().contains("prompt"));
}

#[test]
fn test_yaml_config_with_output_path() {
    let yaml = r#"
name: test-output-path
nodes:
  - name: process
    action: return
    with:
      value: "processed data"
    output: result.data.items
"#;
    let config = parse_yaml_config(yaml).unwrap();

    assert_eq!(config.nodes[0].output, Some("result.data.items".to_string()));
}

#[test]
fn test_yaml_config_multiple_params() {
    let yaml = r#"
name: test-params
nodes:
  - name: llm_call
    action: llm.call
    with:
      prompt: "Hello {{ state.name }}"
      temperature: 0.8
      max_tokens: 500
      model: gpt-4
    output: response
"#;
    let config = parse_yaml_config(yaml).unwrap();

    let params = config.nodes[0].params.as_ref().unwrap();
    assert!(params.contains_key("prompt"));
    assert!(params.contains_key("temperature"));
    assert!(params.contains_key("max_tokens"));
    assert!(params.contains_key("model"));
}
