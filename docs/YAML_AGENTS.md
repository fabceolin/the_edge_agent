# YAML-Based Agent Configuration

The Edge Agent supports declarative agent configuration using YAML files, inspired by GitHub Actions and GitLab CI/CD pipelines.

## Table of Contents

- [Overview](#overview)
- [Security Notice](#security-notice)
- [Basic Structure](#basic-structure)
- [Node Types](#node-types)
- [Edge Types](#edge-types)
- [Template Variables](#template-variables)
- [Built-in Actions](#built-in-actions)
  - [Data Processing Actions](#data-processing-actions)
- [Checkpoint Persistence](#checkpoint-persistence)
- [Examples](#examples)

## Overview

Instead of writing Python code to construct your StateGraph, you can define the entire workflow in a YAML file. This makes agent configuration:

- **Declarative**: Define what you want, not how to build it
- **Portable**: Configuration can be version-controlled and shared
- **Accessible**: Non-programmers can create and modify agents
- **Inspectable**: Easy to understand workflow at a glance

## Security Notice

**YAML files execute arbitrary Python code.** Only load YAML configurations from trusted sources.

This is similar to running any Python script—the YAML author has full access to:
- The Python runtime and all importable modules
- The file system (read/write)
- Network access
- Environment variables and secrets passed to the engine

Unlike GitHub Actions (which runs in isolated VMs with a limited expression language), YAML agents execute directly in your Python process using `exec()` and `eval()`.

**Safe usage:**
- Only load YAML files you wrote or reviewed
- Treat YAML agent files like executable code in code reviews
- Do not load YAML from untrusted user input
- Consider running untrusted agents in a container/sandbox

## Basic Structure

```yaml
name: my-agent
description: What this agent does

# Global variables accessible throughout the workflow
variables:
  max_retries: 3
  api_endpoint: https://api.example.com

# Define the state schema
state_schema:
  input: str
  result: str
  count: int

# Define nodes (workflow steps)
nodes:
  - name: step1
    run: |
      # Python code here
      return {"result": "processed"}

# Define edges (transitions between nodes)
edges:
  - from: __start__
    to: step1
  - from: step1
    to: __end__

# Configuration
config:
  raise_exceptions: true
  interrupt_before: []
  interrupt_after: []
```

## Node Types

### 1. Inline Python Code

Execute Python code directly in the YAML:

```yaml
nodes:
  - name: process_data
    run: |
      # Full Python code with state access
      data = state["input"]
      processed = data.upper()
      return {"result": processed}
```

Or using `script` (GitLab CI style):

```yaml
nodes:
  - name: process_data
    script: |
      result = state["input"] * 2
      return {"result": result}
```

### 2. Built-in Actions

Use pre-defined actions for common operations:

```yaml
nodes:
  # Call an LLM
  - name: call_llm
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: system
          content: You are a helpful assistant
        - role: user
          content: "{{ state.query }}"
      temperature: 0.7
    output: llm_response

  # HTTP GET request
  - name: fetch_data
    uses: http.get
    with:
      url: https://api.example.com/data
      headers:
        Authorization: Bearer {{ secrets.api_key }}
    output: api_data

  # Write to file
  - name: save_result
    uses: file.write
    with:
      path: ./output/{{ state.filename }}.txt
      content: "{{ state.result }}"
```

### 3. Multi-Step Nodes (GitHub Actions Style)

Define multiple steps within a single node:

```yaml
nodes:
  - name: multi_step_process
    steps:
      - name: Step 1 - Fetch data
        uses: http.get
        with:
          url: https://api.example.com/data

      - name: Step 2 - Process
        run: |
          data = state["api_data"]
          processed = [item["value"] for item in data]
          return {"processed": processed}

      - name: Step 3 - Save
        uses: file.write
        with:
          path: ./result.json
          content: "{{ state.processed | json }}"
```

### 4. Expression Evaluation

Simple expression-based nodes:

```yaml
nodes:
  - name: check_condition
    run:
      type: expression
      value: len(state.get("items", [])) > 0
      output_key: has_items
```

### 5. Fan-in Nodes (for Parallel Flows)

Nodes that collect results from parallel execution:

```yaml
nodes:
  - name: combine_results
    fan_in: true
    run: |
      # Access parallel_results parameter
      all_results = [r["data"] for r in parallel_results]
      combined = "\n".join(all_results)
      return {"combined": combined}
```

## Edge Types

### 1. Simple Edge

Unconditional transition:

```yaml
edges:
  - from: step1
    to: step2
```

### 2. Entry Point

Define the starting node:

```yaml
edges:
  - from: __start__
    to: first_step
```

### 3. Finish Point

Define how to reach the end:

```yaml
edges:
  - from: final_step
    to: __end__
```

### 4. Conditional Edge

Route based on a condition:

```yaml
edges:
  - from: check_input
    to: process_valid
    condition:
      type: expression
      value: state["count"] > 10
    when: true

  - from: check_input
    to: process_invalid
    condition:
      type: expression
      value: state["count"] > 10
    when: false
```

### 5. Simple When Clause

Syntactic sugar for simple conditions:

```yaml
edges:
  # Boolean expressions
  - from: validate
    to: process
    when: "state['is_valid']"

  # Negation
  - from: check
    to: skip
    when: "!should_process"
```

### 6. Parallel Edge

Execute flows in parallel:

```yaml
edges:
  - from: start_parallel
    to: flow1
    type: parallel
    fan_in: combine_node

  - from: start_parallel
    to: flow2
    type: parallel
    fan_in: combine_node

  - from: start_parallel
    to: flow3
    type: parallel
    fan_in: combine_node

  - from: combine_node
    to: next_step
```

## Template Variables

Use template syntax to access state, variables, and secrets:

### State Access

```yaml
nodes:
  - name: example
    uses: file.write
    with:
      path: ./{{ state.filename }}.txt
      content: "{{ state.result }}"
```

### Global Variables

```yaml
variables:
  api_url: https://api.example.com

nodes:
  - name: fetch
    uses: http.get
    with:
      url: "{{ variables.api_url }}/endpoint"
```

### Secrets (GitLab CI style)

```yaml
nodes:
  - name: auth_request
    uses: http.get
    with:
      url: https://api.example.com
      headers:
        Authorization: Bearer ${{ secrets.api_key }}
```

### Filters

Apply transformations to values:

```yaml
nodes:
  - name: example
    run: |
      # {{ state.data | json }} - Convert to JSON string
      # {{ state.text | upper }} - Convert to uppercase
      # {{ state.text | lower }} - Convert to lowercase
      data_json = "{{ state.items | json }}"
      return {"json_string": data_json}
```

## Built-in Actions

### LLM Actions

```yaml
# Call OpenAI/compatible API
- name: llm_call
  uses: llm.call
  with:
    model: gpt-4
    messages:
      - role: system
        content: System prompt
      - role: user
        content: User message
    temperature: 0.7
  output: llm_response
```

### HTTP Actions

```yaml
# GET request
- name: fetch
  uses: http.get
  with:
    url: https://api.example.com/data
    headers:
      Authorization: Bearer token

# POST request
- name: send
  uses: http.post
  with:
    url: https://api.example.com/data
    json:
      key: value
    headers:
      Content-Type: application/json
```

### File Actions

```yaml
# Write file
- name: save
  uses: file.write
  with:
    path: ./output.txt
    content: "{{ state.data }}"

# Read file
- name: load
  uses: file.read
  with:
    path: ./input.txt
  output: file_content
```

### Notification Actions

```yaml
# Send notification
- name: notify
  uses: actions.notify
  with:
    channel: slack
    message: "Task completed: {{ state.task_id }}"
```

### Checkpoint Actions

Save and load workflow checkpoints for persistence and recovery:

```yaml
# Save checkpoint to file
- name: save_progress
  uses: checkpoint.save
  with:
    path: ./checkpoints/{{ state.step_name }}.pkl
  output: save_result

# Load checkpoint from file
- name: load_previous
  uses: checkpoint.load
  with:
    path: ./checkpoints/previous.pkl
  output: loaded_checkpoint
```

**checkpoint.save** returns:
- `{"checkpoint_path": str, "saved": True}` on success
- `{"checkpoint_path": str, "saved": False, "error": str}` on failure

**checkpoint.load** returns:
- `{"checkpoint_state": dict, "checkpoint_node": str, "checkpoint_config": dict, "checkpoint_timestamp": float, "checkpoint_version": str}` on success
- `{"error": str}` on failure

### Data Processing Actions

Actions for parsing, transforming, and validating data. Useful for building ETL pipelines and data-driven workflows.

#### JSON Actions

```yaml
# Parse JSON string to Python object
- name: parse_api_response
  uses: json.parse
  with:
    text: "{{ state.raw_response }}"
    strict: true  # Set false to allow trailing commas and comments
    # default: {}  # Fallback value if parsing fails (requires strict: false)
```

**json.parse** returns:
- `{"data": any, "success": true}` on success
- `{"error": str, "success": false, "error_type": "parse", "position": {"line": int, "column": int}}` on failure

```yaml
# Transform data with JMESPath expressions (pip install jmespath)
- name: extract_users
  uses: json.transform
  with:
    data: "{{ state.api_response }}"
    expression: "users[?status=='active'].{name: name, email: email}"
    engine: jmespath  # or "jsonpath" (pip install jsonpath-ng)
```

**json.transform** returns:
- `{"result": any, "expression": str, "success": true}` on success
- `{"error": str, "success": false, "error_type": "transform"}` on failure

Common JMESPath expressions:
- `user.profile.name` - Extract nested value
- `users[?status=='active']` - Filter array by condition
- `users[*].name` - Extract all names from array
- `{names: users[].name, count: length(users)}` - Project new structure

```yaml
# Convert Python object to JSON string
- name: serialize_result
  uses: json.stringify
  with:
    data: "{{ state.result }}"
    indent: 2      # Pretty print with indentation
    sort_keys: true  # Sort dictionary keys alphabetically
```

**json.stringify** returns:
- `{"text": str, "success": true}` on success
- `{"error": str, "success": false, "error_type": "serialize"}` on failure

#### CSV Actions

```yaml
# Parse CSV from text
- name: parse_csv_data
  uses: csv.parse
  with:
    text: "{{ state.csv_content }}"
    delimiter: ","   # Default comma, use ";" for semicolon-separated
    has_header: true # First row contains column names

# Parse CSV from file
- name: load_csv_file
  uses: csv.parse
  with:
    path: ./data/input.csv
    delimiter: ","
    has_header: true
```

**csv.parse** returns:
- With `has_header: true`: `{"data": [{"col1": "val1", ...}, ...], "headers": ["col1", ...], "row_count": int, "success": true}`
- With `has_header: false`: `{"data": [["val1", "val2", ...], ...], "headers": null, "row_count": int, "success": true}`
- `{"error": str, "success": false, "error_type": "parse"|"io"}` on failure

```yaml
# Convert list to CSV string
- name: export_csv
  uses: csv.stringify
  with:
    data: "{{ state.records }}"  # List of dicts or list of lists
    headers: ["name", "email", "status"]  # Optional, auto-detected from dicts
    delimiter: ","
```

**csv.stringify** returns:
- `{"text": str, "row_count": int, "success": true}` on success
- `{"error": str, "success": false, "error_type": "serialize"}` on failure

#### Data Validation Actions

```yaml
# Validate data against JSON Schema (pip install jsonschema)
- name: validate_input
  uses: data.validate
  with:
    data: "{{ state.user_input }}"
    schema:
      type: object
      properties:
        name:
          type: string
          minLength: 1
        email:
          type: string
          format: email
        age:
          type: integer
          minimum: 0
      required: ["name", "email"]
```

**data.validate** returns:
- `{"valid": true, "errors": [], "success": true}` when valid
- `{"valid": false, "errors": [{"path": str, "message": str}, ...], "success": true}` when invalid
- `{"error": str, "success": false, "error_type": "validate"}` on schema error

#### Data Merge Actions

```yaml
# Merge multiple dictionaries
- name: combine_configs
  uses: data.merge
  with:
    sources:
      - "{{ state.default_config }}"
      - "{{ state.user_config }}"
      - "{{ state.override_config }}"
    strategy: deep  # "deep", "shallow", or "replace"
```

Merge strategies:
- `deep`: Recursively merge nested dictionaries
- `shallow`: Only merge top-level keys (nested dicts replaced entirely)
- `replace`: Later sources completely replace earlier ones

**data.merge** returns:
- `{"result": dict, "source_count": int, "success": true}` on success
- `{"error": str, "success": false, "error_type": "merge"}` on failure

#### Data Filter Actions

```yaml
# Filter list items with predicates
- name: filter_active_users
  uses: data.filter
  with:
    data: "{{ state.users }}"
    predicate:
      field: status
      op: eq
      value: active

# Multiple predicates (AND logic)
- name: filter_premium_active
  uses: data.filter
  with:
    data: "{{ state.users }}"
    predicate:
      - field: status
        op: eq
        value: active
      - field: subscription
        op: in
        value: ["premium", "enterprise"]
      - field: last_login_days
        op: lt
        value: 30
```

Supported operators:
- `eq`, `ne`: Equal, not equal
- `gt`, `gte`, `lt`, `lte`: Comparison operators
- `in`, `not_in`: Membership in list
- `contains`, `startswith`, `endswith`: String operations

**data.filter** returns:
- `{"result": list, "original_count": int, "filtered_count": int, "success": true}` on success
- `{"error": str, "success": false, "error_type": "filter"}` on failure

#### Example: Data Processing Pipeline

```yaml
name: data-pipeline
description: ETL pipeline using data processing actions

nodes:
  - name: fetch_data
    uses: http.get
    with:
      url: https://api.example.com/users
    output: raw_response

  - name: parse_response
    uses: json.parse
    with:
      text: "{{ state.raw_response }}"

  - name: filter_active
    uses: data.filter
    with:
      data: "{{ state.data }}"
      predicate:
        field: status
        op: eq
        value: active

  - name: transform_data
    uses: json.transform
    with:
      data: "{{ state.result }}"
      expression: "[*].{id: id, name: name, email: email}"

  - name: export_csv
    uses: csv.stringify
    with:
      data: "{{ state.result }}"

  - name: save_file
    uses: file.write
    with:
      path: ./output/active_users.csv
      content: "{{ state.text }}"

edges:
  - from: __start__
    to: fetch_data
  - from: fetch_data
    to: parse_response
  - from: parse_response
    to: filter_active
  - from: filter_active
    to: transform_data
  - from: transform_data
    to: export_csv
  - from: export_csv
    to: save_file
  - from: save_file
    to: __end__
```

## Checkpoint Persistence

YAML agents support checkpoint persistence for saving and resuming workflow execution. This is useful for:

- Long-running workflows that may be interrupted
- Debugging by stopping at specific points
- Recovering from failures
- Implementing human-in-the-loop workflows

### Configuration

Enable checkpoint features in the `config` section:

```yaml
config:
  # Directory for auto-save checkpoints at interrupt points
  checkpoint_dir: ./checkpoints

  # Resume from a specific checkpoint on load
  checkpoint: ./checkpoints/resume_point.pkl

  # Interrupt at specific nodes (triggers auto-save)
  interrupt_before: [critical_node]
  interrupt_after: [validation_node]
```

### Auto-Save at Interrupts

When `checkpoint_dir` is configured, checkpoints are automatically saved before yielding interrupt events:

```yaml
config:
  checkpoint_dir: ./checkpoints
  interrupt_before: [review_node]
```

Checkpoint files are saved as `{checkpoint_dir}/{node}_{timestamp_ms}.pkl`.

### Resume from Checkpoint

Resume execution from a saved checkpoint:

```yaml
config:
  checkpoint: ./checkpoints/review_node_1733500000.pkl
```

Or in Python:

```python
engine = YAMLEngine()
# Load with checkpoint parameter (overrides config.checkpoint)
graph = engine.load_from_file("agent.yaml", checkpoint="./checkpoints/state.pkl")
# Resume from checkpoint
for event in graph.resume_from_checkpoint("./checkpoints/state.pkl"):
    print(event)
```

### Manual Checkpoints with Actions

Use `checkpoint.save` and `checkpoint.load` actions for manual control:

```yaml
nodes:
  - name: prepare_data
    run: |
      return {"data": "processed", "step": "prepare"}

  - name: save_before_expensive
    uses: checkpoint.save
    with:
      path: "{{ checkpoint.dir }}/before_expensive.pkl"
    output: checkpoint_info

  - name: expensive_operation
    run: |
      # Long-running operation
      return {"result": "completed"}
```

### Template Variables for Checkpoints

Two template variables are available for checkpoint paths:

- `{{ checkpoint.dir }}` - The configured `checkpoint_dir` value (empty string if not set)
- `{{ checkpoint.last }}` - Path to the most recent checkpoint saved via `checkpoint.save` action

```yaml
config:
  checkpoint_dir: ./checkpoints

nodes:
  - name: save_checkpoint
    uses: checkpoint.save
    with:
      path: "{{ checkpoint.dir }}/{{ state.run_id }}.pkl"

  - name: reference_last
    run: |
      last_checkpoint = "{{ checkpoint.last }}"
      return {"last_saved": last_checkpoint}
```

### Example: Workflow with Checkpoints

```yaml
name: long-running-agent
description: Agent with checkpoint persistence

config:
  checkpoint_dir: ./checkpoints
  interrupt_before: [human_review]

variables:
  output_dir: ./output

state_schema:
  data: list
  processed: list
  approved: bool
  result: str

nodes:
  - name: fetch_data
    run: |
      data = [{"id": i, "value": i * 10} for i in range(100)]
      return {"data": data}

  - name: process_batch
    run: |
      processed = [{"id": d["id"], "result": d["value"] * 2}
                   for d in state["data"]]
      return {"processed": processed}

  - name: human_review
    # Execution pauses here with auto-save checkpoint
    # Resume later with: graph.resume_from_checkpoint("checkpoint.pkl")
    run: |
      # Check for approval flag set after resume
      return {"approved": state.get("approved", False)}

  - name: finalize
    run: |
      result = f"Processed {len(state['processed'])} items"
      return {"result": result}

edges:
  - from: __start__
    to: fetch_data
  - from: fetch_data
    to: process_batch
  - from: process_batch
    to: human_review
  - from: human_review
    to: finalize
    when: "state['approved']"
  - from: finalize
    to: __end__
```

### Python API

```python
from the_edge_agent import YAMLEngine

engine = YAMLEngine()

# Load with checkpoint_dir configured in YAML
graph = engine.load_from_file("agent.yaml")

# Or load with explicit checkpoint to resume
graph = engine.load_from_file("agent.yaml", checkpoint="./chk/state.pkl")

# Or use load_from_dict with checkpoint parameter
config = {...}
graph = engine.load_from_dict(config, checkpoint="./chk/state.pkl")

# Resume using the engine's convenience method
for event in engine.resume_from_checkpoint(
    "agent.yaml",
    "./checkpoints/human_review_1733500000.pkl",
    config={"approved": True}  # Override config for resume
):
    if event["type"] == "final":
        print("Completed:", event["state"]["result"])
```

## Examples

### Example 1: Simple Research Agent

```yaml
name: research-agent
description: Search and summarize information

state_schema:
  query: str
  results: list
  summary: str

nodes:
  - name: search
    run: |
      # Simulate search
      results = [{"title": f"Result {i}", "text": "..."}
                 for i in range(5)]
      return {"results": results}

  - name: summarize
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: system
          content: Summarize search results
        - role: user
          content: "{{ state.results | json }}"
    output: summary

  - name: save
    uses: file.write
    with:
      path: ./{{ state.query }}.md
      content: "{{ state.summary }}"

edges:
  - from: __start__
    to: search
  - from: search
    to: summarize
  - from: summarize
    to: save
  - from: save
    to: __end__
```

### Example 2: Conditional Workflow

```yaml
name: data-processor
description: Process data with validation

state_schema:
  data: list
  valid: bool
  result: str

nodes:
  - name: validate
    run: |
      valid = len(state.get("data", [])) > 0
      return {"valid": valid}

  - name: process
    run: |
      result = f"Processed {len(state['data'])} items"
      return {"result": result}

  - name: error_handler
    run: |
      return {"result": "No data to process"}

edges:
  - from: __start__
    to: validate

  - from: validate
    to: process
    when: "state['valid']"

  - from: validate
    to: error_handler
    when: "!state['valid']"

  - from: process
    to: __end__

  - from: error_handler
    to: __end__
```

### Example 3: Parallel Processing

```yaml
name: parallel-processor
description: Process multiple items in parallel

state_schema:
  items: list
  results: list

nodes:
  - name: process_a
    run: |
      return {"result": "A processed"}

  - name: process_b
    run: |
      return {"result": "B processed"}

  - name: process_c
    run: |
      return {"result": "C processed"}

  - name: combine
    fan_in: true
    run: |
      results = [r["result"] for r in parallel_results]
      return {"results": results}

edges:
  - from: __start__
    to: process_a
    type: parallel
    fan_in: combine

  - from: __start__
    to: process_b
    type: parallel
    fan_in: combine

  - from: __start__
    to: process_c
    type: parallel
    fan_in: combine

  - from: combine
    to: __end__
```

## Usage

### In Python

```python
from the_edge_agent import YAMLEngine

# Load and run agent
engine = YAMLEngine()
graph = engine.load_from_file("my_agent.yaml")

# Execute
for event in graph.stream({"input": "hello"}):
    if event["type"] == "final":
        print(event["state"])
```

### From Command Line

```bash
python -m the_edge_agent.yaml_engine my_agent.yaml '{"input": "hello"}'
```

## Advanced Features

### Custom Actions

Register your own actions:

```python
def custom_action(state, param1, param2, **kwargs):
    # Your logic here
    return {"result": f"{param1} + {param2}"}

engine = YAMLEngine(actions_registry={
    "custom.action": custom_action
})
```

Then use in YAML:

```yaml
nodes:
  - name: use_custom
    uses: custom.action
    with:
      param1: value1
      param2: value2
```

### Interrupts for Debugging

```yaml
config:
  interrupt_before: [critical_node]
  interrupt_after: [validation_node]
```

### Error Handling

```yaml
config:
  raise_exceptions: true  # Raise errors instead of yielding them
```

## Best Practices

1. **Keep nodes focused**: Each node should do one thing well
2. **Use meaningful names**: Node and edge names should be descriptive
3. **Leverage built-in actions**: Don't reinvent common operations
4. **Document with comments**: YAML supports `# comments`
5. **Version control**: Keep YAML configs in git
6. **Test incrementally**: Use interrupts to debug complex flows
7. **Validate state schema**: Define expected state structure upfront

## Comparison with GitHub Actions

| Feature | GitHub Actions | YAML Agents |
|---------|---------------|-------------|
| Workflow definition | YAML | YAML |
| Steps/Jobs | `steps:` | `nodes:` |
| Triggers | `on:` | `edges:` with conditions |
| Variables | `${{ }}` | `{{ }}` |
| Actions | `uses:` | `uses:` |
| Inline code | `run:` | `run:` or `script:` |
| Parallel | `matrix:` | `type: parallel` |
| Conditionals | `if:` | `condition:` or `when:` |

## Troubleshooting

### Common Issues

**Issue**: Template variables not replaced
- **Solution**: Ensure you're using correct syntax: `{{ state.key }}` not `${{ state.key }}`

**Issue**: Node function not found
- **Solution**: Check that custom actions are registered in the engine

**Issue**: Parallel flows not working
- **Solution**: Ensure fan-in node is defined and all parallel edges reference it

**Issue**: Conditional edges not routing correctly
- **Solution**: Debug by adding `interrupt_after` at the decision node

## Future Enhancements

Planned features:

- [ ] Matrix strategy for parallel execution
- [ ] Retry logic with backoff
- [ ] Caching of node results
- [ ] Workflow composition (import/include)
- [ ] Environment-specific configs
- [ ] Schema validation for YAML files
- [ ] Visual workflow editor
- [ ] Metrics and monitoring hooks

Recently implemented:

- [x] Checkpoint persistence for save/resume workflows (v0.5.0)
