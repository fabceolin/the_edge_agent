//! Integration tests for YAML config parsing
//!
//! TEA-WASM-001.1: Tests parsing of example YAML files from the repository.

use tea_wasm_llm::{parse_yaml_config, WasmYamlConfig};

/// Test parsing the qa-neurosymbolic-wasm.yaml example
#[test]
fn test_parse_qa_neurosymbolic_wasm() {
    let yaml = include_str!("../../../examples/qa-neurosymbolic-wasm.yaml");
    let config = parse_yaml_config(yaml).expect("Failed to parse qa-neurosymbolic-wasm.yaml");

    assert_eq!(config.name, "qa-neurosymbolic");
    assert!(config.description.is_some());
    assert_eq!(config.nodes.len(), 5);
    assert_eq!(config.edges.len(), 6);

    // Verify node names
    let node_names: Vec<&str> = config.nodes.iter().map(|n| n.name.as_str()).collect();
    assert!(node_names.contains(&"think"));
    assert!(node_names.contains(&"validate_prolog"));
    assert!(node_names.contains(&"validate_lua"));
    assert!(node_names.contains(&"extract_answer"));
    assert!(node_names.contains(&"format_answer"));

    // Verify actions
    let think_node = config.nodes.iter().find(|n| n.name == "think").unwrap();
    assert_eq!(WasmYamlConfig::get_node_action(think_node), Some("llm.call"));

    let prolog_node = config.nodes.iter().find(|n| n.name == "validate_prolog").unwrap();
    assert_eq!(WasmYamlConfig::get_node_action(prolog_node), Some("prolog.query"));

    let lua_node = config.nodes.iter().find(|n| n.name == "validate_lua").unwrap();
    assert_eq!(WasmYamlConfig::get_node_action(lua_node), Some("lua.eval"));
}

/// Test parsing simple-prolog-agent.yaml
#[test]
fn test_parse_simple_prolog_agent() {
    let yaml = include_str!("../../../examples/prolog/simple-prolog-agent.yaml");
    let config = parse_yaml_config(yaml).expect("Failed to parse simple-prolog-agent.yaml");

    assert_eq!(config.name, "simple-prolog-agent");
    assert!(config.description.is_some());
    assert_eq!(config.nodes.len(), 3);

    // Check state schema
    assert!(config.state_schema.is_some());
    let schema = config.state_schema.as_ref().unwrap();
    assert!(schema.contains_key("value"));
    assert!(schema.contains_key("doubled"));
    assert!(schema.contains_key("result"));
    assert!(schema.contains_key("category"));

    // Check language field
    for node in &config.nodes {
        assert_eq!(node.language.as_deref(), Some("prolog"));
    }
}

/// Test parsing parallel_strategies_demo.yaml
#[test]
fn test_parse_parallel_strategies_demo() {
    let yaml = include_str!("../../../examples/parallel_strategies_demo.yaml");
    let config = parse_yaml_config(yaml).expect("Failed to parse parallel_strategies_demo.yaml");

    assert!(!config.name.is_empty());
    assert!(!config.nodes.is_empty());
}

/// Test that parsed config can be accessed by executor
#[test]
fn test_parsed_config_accessible_for_executor() {
    let yaml = r#"
name: executor-test
description: Test config accessibility

state_schema:
  input: str
  output: str

variables:
  timeout: 30
  model: gpt-4

nodes:
  - name: process
    action: llm.call
    with:
      prompt: "{{ state.input }}"
      model: "{{ variables.model }}"
      max_tokens: 100
    output: llm_response

settings:
  llm:
    model: gpt-4
    temperature: 0.7
  raise_exceptions: true

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"#;

    let config = parse_yaml_config(yaml).expect("Failed to parse config");

    // Verify all fields are accessible
    assert_eq!(config.name, "executor-test");
    assert!(config.description.is_some());
    assert!(config.state_schema.is_some());
    assert!(!config.variables.is_empty());
    assert!(!config.nodes.is_empty());
    assert!(!config.edges.is_empty());

    // Verify effective settings
    let settings = config.effective_settings();
    assert!(settings.llm.is_some());
    assert_eq!(settings.raise_exceptions, Some(true));

    // Verify node params are accessible
    let node = &config.nodes[0];
    assert!(node.params.is_some());
    let params = node.params.as_ref().unwrap();
    assert!(params.contains_key("prompt"));
    assert!(params.contains_key("model"));
}

/// Test parsing of qa-neurosymbolic-fixed.yaml
#[test]
fn test_parse_qa_neurosymbolic_fixed() {
    let yaml = include_str!("../../../examples/qa-neurosymbolic-fixed.yaml");
    let config = parse_yaml_config(yaml).expect("Failed to parse qa-neurosymbolic-fixed.yaml");

    assert!(!config.name.is_empty());
    assert!(!config.nodes.is_empty());
}

/// Test that we can iterate over all nodes
#[test]
fn test_iterate_all_nodes() {
    let yaml = r#"
name: iterate-test
nodes:
  - name: node1
    action: return
  - name: node2
    action: llm.call
    with:
      prompt: test
  - name: node3
    language: lua
    run: return {}
"#;

    let config = parse_yaml_config(yaml).expect("Failed to parse config");

    let mut count = 0;
    for node in &config.nodes {
        assert!(!node.name.is_empty());
        count += 1;
    }
    assert_eq!(count, 3);
}

/// Test parsing retail_sales_interviewer.yaml
#[test]
fn test_parse_retail_sales_interviewer() {
    let yaml = include_str!("../../../examples/retail_sales_interviewer.yaml");
    let config = parse_yaml_config(yaml).expect("Failed to parse retail_sales_interviewer.yaml");

    assert!(!config.name.is_empty());
    assert!(!config.nodes.is_empty());

    // This file may have goto configurations
    let has_goto = config.nodes.iter().any(|n| n.goto.is_some());
    // Just verify parsing worked - goto presence depends on file content
    assert!(config.nodes.len() > 0 || !has_goto);
}

/// Test error messages contain useful context
#[test]
fn test_error_messages_have_context() {
    let yaml = r#"
name: test
nodes:
  - name: a
edges:
  - from: nonexistent
    to: a
"#;

    let result = parse_yaml_config(yaml);
    assert!(result.is_err());
    let err = result.unwrap_err();
    let msg = err.to_string();

    // Error should mention the invalid reference
    assert!(msg.contains("nonexistent") || msg.contains("reference"));
}

/// Test that YAML syntax errors include line information
#[test]
fn test_yaml_syntax_error_has_line() {
    let yaml = r#"
name: test
nodes: [
  invalid yaml here
"#;

    let result = parse_yaml_config(yaml);
    assert!(result.is_err());
    let err = result.unwrap_err();
    let msg = err.to_string();

    // Error should be a YAML parse error
    assert!(msg.contains("YAML") || msg.contains("parse"));
}

/// Test parsing a complex workflow with conditional edges
#[test]
fn test_parse_complex_workflow_with_conditions() {
    let yaml = r#"
name: conditional-workflow
nodes:
  - name: check
    action: lua.eval
    with:
      code: return { ready = true }
  - name: process_ready
    action: return
  - name: process_not_ready
    action: return

edges:
  - from: __start__
    to: check
  - from: check
    to: process_ready
    when: "state.ready == true"
  - from: check
    to: process_not_ready
    when: "state.ready == false"
  - from: process_ready
    to: __end__
  - from: process_not_ready
    to: __end__
"#;

    let config = parse_yaml_config(yaml).expect("Failed to parse config");

    // Count conditional edges
    let conditional_edges = config.edges.iter().filter(|e| e.condition.is_some()).count();
    assert_eq!(conditional_edges, 2);

    // Verify conditions
    let ready_edge = config.edges.iter().find(|e| e.to == "process_ready").unwrap();
    assert!(ready_edge.condition.as_ref().unwrap().contains("true"));
}
