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
  - [LLM Enhanced Actions](#llm-enhanced-actions-tea-builtin-0012)
  - [Data Processing Actions](#data-processing-actions)
  - [Code Execution Actions](#code-execution-actions-tea-builtin-0031)
  - [Observability Actions](#observability-actions)
  - [Memory Actions](#memory-actions)
  - [Web Actions](#web-actions-tea-builtin-0021)
  - [RAG Actions](#rag-actions-tea-builtin-0022)
  - [Tools Bridge Actions](#tools-bridge-actions-tea-builtin-0023)
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

### LLM Enhanced Actions (TEA-BUILTIN-001.2)

Production-grade LLM actions with streaming, retry, and tool calling support.

#### llm.stream

Stream LLM responses with chunk aggregation:

```yaml
- name: stream_response
  uses: llm.stream
  with:
    model: gpt-4
    messages:
      - role: user
        content: "{{ state.query }}"
    temperature: 0.7
  output: stream_result
```

**llm.stream** returns:
- `{"content": str, "usage": dict, "streamed": true, "chunk_count": int}` on success
- `{"error": str, "success": false}` on failure

The action uses OpenAI's streaming API internally, collecting chunks and returning the aggregated result. This provides the benefits of streaming (faster time to first token) while fitting within the action return model.

#### llm.retry

LLM calls with exponential backoff retry logic:

```yaml
- name: resilient_call
  uses: llm.retry
  with:
    model: gpt-4
    messages:
      - role: user
        content: "{{ state.query }}"
    max_retries: 3      # Maximum retry attempts (default: 3)
    base_delay: 1.0     # Initial delay in seconds (default: 1.0)
    max_delay: 60.0     # Maximum delay between retries (default: 60.0)
    temperature: 0.7
  output: retry_result
```

**llm.retry** returns:
- `{"content": str, "usage": dict, "attempts": int, "total_delay": float}` on success
- `{"error": str, "success": false, "attempts": int, "total_delay": float}` on failure

Retry behavior:
- **Retryable errors**: HTTP 429 (rate limit), HTTP 5xx, timeouts, connection errors
- **Non-retryable errors**: HTTP 4xx (except 429) - immediate failure
- **Retry-After header**: Respected when present in rate limit responses
- **Exponential backoff**: `delay = min(base_delay * 2^attempt, max_delay)`

#### llm.tools

Function/tool calling with automatic action dispatch:

```yaml
- name: agent_with_tools
  uses: llm.tools
  with:
    model: gpt-4
    messages:
      - role: system
        content: You are a helpful assistant with access to tools.
      - role: user
        content: "{{ state.query }}"
    tools:
      - name: search_web
        description: Search the web for information
        parameters:
          query:
            type: string
            description: Search query
            required: true
        action: http.get  # Maps to registered action
      - name: save_note
        description: Save a note to file
        parameters:
          filename:
            type: string
            required: true
          content:
            type: string
            required: true
        action: file.write
    tool_choice: auto   # "auto", "none", or specific tool name
    max_tool_rounds: 10 # Maximum tool call iterations
  output: tools_result
```

**Tool Definition Schema:**

Tools can be defined in YAML-style (recommended) or OpenAI format:

```yaml
# YAML-style (recommended)
tools:
  - name: tool_name
    description: What the tool does
    parameters:
      param1:
        type: string       # string, number, integer, boolean, array, object
        description: Parameter description
        required: true     # Mark as required
        enum: [opt1, opt2] # Optional: restrict values
    action: registered.action  # Maps to action registry

# OpenAI format (also supported)
tools:
  - type: function
    function:
      name: tool_name
      description: What the tool does
      parameters:
        type: object
        properties:
          param1:
            type: string
            description: Parameter description
        required: [param1]
```

**llm.tools** returns:
- Success: `{"content": str, "tool_calls": list, "tool_results": list, "rounds": int}`
- Failure: `{"error": str, "success": false, "tool_calls": list, "tool_results": list}`

Tool dispatch:
- When the LLM requests a tool call, the action specified in `action` is invoked
- Tool arguments are passed as keyword arguments to the action
- Results are automatically fed back to the LLM for multi-turn conversations
- Tools without `action` mapping return info about the call for manual handling

**Security:** Action references are validated against the registry. Path traversal attempts (e.g., `../etc/passwd`) are blocked.

#### Example: Agent with Tools

```yaml
name: research-agent
description: Agent that can search and save results

variables:
  api_base: https://api.example.com

nodes:
  - name: research
    uses: llm.tools
    with:
      model: gpt-4
      messages:
        - role: system
          content: |
            You are a research assistant. Use the search tool to find
            information, then save important findings to files.
        - role: user
          content: "{{ state.query }}"
      tools:
        - name: search
          description: Search for information on a topic
          parameters:
            query:
              type: string
              description: The search query
              required: true
          action: http.get
        - name: save_finding
          description: Save an important finding to a file
          parameters:
            filename:
              type: string
              required: true
            content:
              type: string
              required: true
          action: file.write
      max_tool_rounds: 5

edges:
  - from: __start__
    to: research
  - from: research
    to: __end__
```

All LLM enhanced actions are available via dual namespaces: `llm.*` and `actions.llm_*`.

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

### Code Execution Actions (TEA-BUILTIN-003.1)

Actions for sandboxed Python code execution. Uses RestrictedPython for security.

**SECURITY WARNING**: Code execution is DISABLED by default. Only enable for trusted code patterns.

**Required:**
- `pip install RestrictedPython`
- `YAMLEngine(enable_code_execution=True)` to enable

#### code.execute

Execute Python code in a sandboxed environment:

```yaml
# Simple arithmetic
- name: compute
  uses: code.execute
  with:
    code: |
      x = 1 + 2
      y = x * 10
      result = y  # Set 'result' to return a value
    timeout: 30  # Optional: max execution time in seconds (default: 30)
    max_output_bytes: 65536  # Optional: max output size (default: 64KB)

# Using state values
- name: process_data
  uses: code.execute
  with:
    code: |
      data = {{ state.input_data | json }}
      total = sum(item['value'] for item in data)
      result = {'total': total, 'count': len(data)}

# Capture print output
- name: generate_report
  uses: code.execute
  with:
    code: |
      print("Processing started...")
      for i in range(5):
          print(f"Step {i + 1} complete")
      result = "done"
```

**code.execute** returns:
- `{"success": true, "stdout": str, "stderr": str, "return_value": any, "execution_time_ms": float}` on success
- `{"success": false, "error": str, "stdout": "", "stderr": "", "return_value": null}` on failure

**Security model (whitelist approach):**

Allowed operations:
- Math: `abs`, `round`, `min`, `max`, `sum`, `pow`, `divmod`
- Types: `int`, `float`, `str`, `bool`, `list`, `dict`, `tuple`, `set`
- Iteration: `len`, `range`, `enumerate`, `zip`, `map`, `filter`, `sorted`, `reversed`
- Predicates: `all`, `any`, `isinstance`
- String: `chr`, `ord`, `repr`
- Exceptions: `try`/`except` blocks work

Blocked operations:
- Imports (`import os`, `from sys import ...`)
- File access (`open()`)
- Code generation (`exec`, `eval`, `compile`)
- Dangerous introspection (`__class__`, `__mro__`, `__subclasses__`, `getattr`)
- System access (`input`, `help`, `vars`, `dir`)

#### code.sandbox

Manage persistent sandbox sessions for multi-step code execution:

```yaml
# Create a new sandbox session
- name: create_session
  uses: code.sandbox
  with:
    action: create
  output: sandbox_info
# Returns: {"sandbox_id": str, "created": true, "success": true}

# Execute code in the session (variables persist)
- name: init_counter
  uses: code.sandbox
  with:
    action: execute
    sandbox_id: "{{ state.sandbox_info.sandbox_id }}"
    code: "counter = 0"

- name: increment_counter
  uses: code.sandbox
  with:
    action: execute
    sandbox_id: "{{ state.sandbox_info.sandbox_id }}"
    code: |
      counter += 1
      result = counter

# List active sessions
- name: list_sessions
  uses: code.sandbox
  with:
    action: list
# Returns: {"sandboxes": [str, ...], "count": int, "success": true}

# Destroy session when done
- name: cleanup
  uses: code.sandbox
  with:
    action: destroy
    sandbox_id: "{{ state.sandbox_info.sandbox_id }}"
# Returns: {"destroyed": true, "sandbox_id": str, "success": true}
```

Sandbox actions:
- `create`: Create new session, returns `sandbox_id`
- `execute`: Run code in session, variables persist
- `list`: Get all active session IDs
- `destroy`: Clean up session

#### Example: Code Agent

```yaml
name: code-agent
description: Agent that executes generated Python code safely

nodes:
  - name: generate_code
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: system
          content: |
            Generate Python code to solve the user's math problem.
            Only use basic operations: +, -, *, /, **, //, %.
            Store the answer in a variable called 'result'.
        - role: user
          content: "{{ state.problem }}"
    output: llm_response

  - name: execute_code
    uses: code.execute
    with:
      code: "{{ state.llm_response.content }}"
      timeout: 5

  - name: format_result
    run: |
      if state.get('success'):
          return {"answer": state.get('return_value'), "error": None}
      else:
          return {"answer": None, "error": state.get('error')}

edges:
  - from: __start__
    to: generate_code
  - from: generate_code
    to: execute_code
  - from: execute_code
    to: format_result
  - from: format_result
    to: __end__

config:
  raise_exceptions: false
```

**Usage:**
```python
from the_edge_agent import YAMLEngine

# IMPORTANT: Enable code execution explicitly
engine = YAMLEngine(enable_code_execution=True)
graph = engine.load_from_file("code_agent.yaml")

result = list(graph.invoke({"problem": "Calculate the sum of squares from 1 to 10"}))
print(result[-1]["state"]["answer"])  # 385
```

### Observability Actions

Actions for tracing, logging, and debugging workflow execution. Useful for debugging, performance analysis, and observability.

#### trace.start

Start a new trace span to track an operation:

```yaml
- name: start_trace
  uses: trace.start
  with:
    name: "process_data"  # Required: operation name
    metadata:             # Optional: attach metadata
      user_id: "{{ state.user_id }}"
      operation: "data_processing"
    parent_id: "{{ state.parent_span }}"  # Optional: explicit parent span
```

**trace.start** returns:
- `{"span_id": str, "name": str, "parent_id": str | null, "success": true}` on success
- `{"error": str, "success": false}` if tracing is disabled

#### trace.log

Log events, metrics, or state snapshots to the current span:

```yaml
# Log a message
- name: log_progress
  uses: trace.log
  with:
    message: "Processing step completed"

# Log metrics
- name: log_metrics
  uses: trace.log
  with:
    metrics:
      items_processed: 100
      duration_ms: 250

# Snapshot state (with sensitive key redaction)
- name: log_state
  uses: trace.log
  with:
    message: "Before API call"
    snapshot_state: true
    sanitize_keys: ["api_key", "password", "token"]
```

**trace.log** returns:
- `{"logged": true, "span_id": str, "event_count": int, "success": true}` on success
- `{"error": str, "success": false, "logged": false}` if no active span

#### trace.end

End the current trace span:

```yaml
# End successfully
- name: end_trace
  uses: trace.end
  with:
    status: ok

# End with error
- name: end_trace_error
  uses: trace.end
  with:
    status: error
    error: "API call failed: {{ state.error_message }}"
```

**trace.end** returns:
- `{"span_id": str, "duration_ms": float, "status": str, "success": true}` on success
- `{"error": str, "success": false}` if no active span

#### Auto-Instrumentation

Enable automatic tracing of all nodes via YAML settings:

```yaml
settings:
  auto_trace: true         # Auto-wrap all nodes with tracing
  trace_exporter: console  # "console", "file"
  trace_file: ./traces.jsonl  # For file exporter
```

When `auto_trace: true`:
- Every node automatically starts/ends a span
- Node execution time is captured
- LLM token usage is auto-captured from `llm.call` results
- HTTP latency is auto-captured from `http.*` results
- Errors are automatically recorded

#### Example: Traced Workflow

```yaml
name: traced-workflow
description: Workflow with manual tracing for debugging

settings:
  auto_trace: false  # We'll trace manually

nodes:
  - name: process
    steps:
      - name: start_trace
        uses: trace.start
        with:
          name: "process_user_request"
          metadata:
            request_id: "{{ state.request_id }}"

      - name: fetch_data
        uses: http.get
        with:
          url: "https://api.example.com/data"

      - name: log_fetch
        uses: trace.log
        with:
          message: "Data fetched successfully"
          metrics:
            response_size: "{{ state.fetch_data.content_length }}"

      - name: process_data
        run: |
          return {"result": process(state["fetch_data"])}

      - name: end_trace
        uses: trace.end
        with:
          status: ok

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
```

#### Python Configuration

Configure trace exporters when creating the engine:

```python
from the_edge_agent import YAMLEngine, ConsoleExporter, FileExporter, CallbackExporter

# Console exporter (verbose mode shows full span details)
engine = YAMLEngine(
    trace_exporter="console",
    trace_verbose=True
)

# File exporter (JSON lines format)
engine = YAMLEngine(
    trace_exporter="file",
    trace_file="./traces.jsonl"
)

# Custom callback exporter
def handle_span(span):
    print(f"Completed: {span['name']} in {span['duration_ms']:.2f}ms")

engine = YAMLEngine(
    trace_exporter="callback",
    trace_callback=handle_span
)

# Multiple exporters
engine = YAMLEngine(
    trace_exporter=[
        ConsoleExporter(verbose=False),
        FileExporter("./traces.jsonl"),
        CallbackExporter(lambda s: send_to_observability_platform(s))
    ]
)

# Disable tracing entirely
engine = YAMLEngine(enable_tracing=False)
```

### Memory Actions

Actions for storing and retrieving data across graph invocations. Useful for building conversational agents with persistent context.

#### memory.store

Store a key-value pair in memory with optional TTL:

```yaml
- name: remember_user
  uses: memory.store
  with:
    key: "user_name"
    value: "{{ state.name }}"
    ttl: 3600        # Optional: Time-to-live in seconds (null = no expiration)
    namespace: "session_123"  # Optional: Namespace for key isolation
```

**memory.store** returns:
- `{"stored": true, "key": str, "namespace": str}` on success
- `{"stored": false, "key": str, "error": str}` on failure

#### memory.retrieve

Retrieve a value from memory:

```yaml
- name: recall_user
  uses: memory.retrieve
  with:
    key: "user_name"
    default: "Guest"       # Optional: Default value if key not found/expired
    namespace: "session_123"  # Optional: Namespace to look in
```

**memory.retrieve** returns:
- `{"value": any, "found": true, "key": str}` if key exists and not expired
- `{"value": default, "found": false, "key": str}` if key not found or expired

#### memory.summarize

Summarize conversation history using LLM to fit token windows:

```yaml
- name: compress_history
  uses: memory.summarize
  with:
    messages_key: "conversation"  # State key containing messages list
    max_tokens: 1000              # Maximum tokens for summary
    model: "gpt-3.5-turbo"        # Optional: Model to use
```

**memory.summarize** returns:
- `{"summary": str, "original_count": int, "token_estimate": int, "success": true}` on success
- `{"error": str, "success": false}` on failure

Messages should be in the format: `[{"role": "user", "content": "..."}, {"role": "assistant", "content": "..."}]`

#### Example: Conversational Agent with Memory

```yaml
name: conversational-agent
description: Agent with persistent memory across invocations

state_schema:
  user_input: str
  conversation: list
  response: str

nodes:
  - name: load_context
    uses: memory.retrieve
    with:
      key: "conversation_history"
      default: []

  - name: update_conversation
    run: |
      history = state.get("value", [])
      history.append({"role": "user", "content": state["user_input"]})
      return {"conversation": history}

  - name: generate_response
    uses: llm.call
    with:
      model: gpt-4
      messages: "{{ state.conversation }}"
      temperature: 0.7
    output: llm_result

  - name: save_context
    steps:
      - name: append_response
        run: |
          history = state["conversation"]
          history.append({"role": "assistant", "content": state["llm_result"]["content"]})
          return {"conversation": history, "response": state["llm_result"]["content"]}

      - name: persist_history
        uses: memory.store
        with:
          key: "conversation_history"
          value: "{{ state.conversation }}"

edges:
  - from: __start__
    to: load_context
  - from: load_context
    to: update_conversation
  - from: update_conversation
    to: generate_response
  - from: generate_response
    to: save_context
  - from: save_context
    to: __end__
```

#### Python Configuration

```python
from the_edge_agent import YAMLEngine, InMemoryBackend

# Default in-memory backend (persists across invocations within same engine)
engine = YAMLEngine()

# Custom backend injection
custom_backend = InMemoryBackend()
engine = YAMLEngine(memory_backend=custom_backend)

# Memory state serialization for checkpoints
memory_state = engine.get_memory_state()
# ... save to checkpoint ...
# Restore later
engine.restore_memory_state(memory_state)

# Clear memory
engine.clear_memory()  # Clear all namespaces
engine.clear_memory(namespace="session_123")  # Clear specific namespace
```

### Long-Term Memory Actions (TEA-BUILTIN-001.4)

Long-term memory provides persistent storage using SQLite with FTS5 full-text search.
Unlike session memory, data persists across engine restarts when using file-based storage.

All LTM actions are available via dual namespaces: `ltm.*` and `actions.ltm_*`.

#### ltm.store

Store key-value pairs persistently with optional metadata:

```yaml
- name: store_knowledge
  uses: ltm.store
  with:
    key: "user_profile"
    value: "{{ state.profile_data }}"
    metadata:
      type: "profile"
      source: "onboarding"
```

**ltm.store** returns:
- `{"success": true, "stored": true, "key": str, "created": true/false}` on success
- `{"success": false, "error": str, "error_type": str}` on failure

#### ltm.retrieve

Retrieve values by key from persistent storage:

```yaml
- name: load_knowledge
  uses: ltm.retrieve
  with:
    key: "user_profile"
    default: {}  # Optional default if not found
```

**ltm.retrieve** returns:
- `{"success": true, "value": any, "found": true, "metadata": dict}` if key exists
- `{"success": true, "value": default, "found": false, "metadata": null}` if not found
- `{"success": false, "error": str, "error_type": str}` on failure

#### ltm.delete

Delete a key from persistent storage:

```yaml
- name: remove_old_data
  uses: ltm.delete
  with:
    key: "deprecated_key"
```

**ltm.delete** returns:
- `{"success": true, "deleted": true, "key": str}` if key was deleted
- `{"success": true, "deleted": false, "key": str}` if key didn't exist
- `{"success": false, "error": str, "error_type": str}` on failure

#### ltm.search

Full-text search across stored values using FTS5:

```yaml
- name: search_knowledge
  uses: ltm.search
  with:
    query: "coding preferences"
    limit: 10
    metadata_filter:  # Optional
      type: "profile"
```

**ltm.search** returns:
- `{"success": true, "results": list, "count": int}` with results containing `{key, value, metadata, score}`
- `{"success": false, "error": str, "error_type": str}` on failure

#### Example: Persistent Knowledge Agent

```yaml
name: knowledge-agent
description: Agent that builds and searches a persistent knowledge base

state_schema:
  query: str
  results: list
  answer: str

nodes:
  - name: search_knowledge
    uses: ltm.search
    with:
      query: "{{ state.query }}"
      limit: 5

  - name: generate_answer
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: system
          content: "Answer based on this knowledge: {{ state.results }}"
        - role: user
          content: "{{ state.query }}"
    output: llm_result

  - name: format_response
    run: |
      return {"answer": state["llm_result"]["content"]}

edges:
  - from: __start__
    to: search_knowledge
  - from: search_knowledge
    to: generate_answer
  - from: generate_answer
    to: format_response
  - from: format_response
    to: __end__
```

#### Python Configuration

```python
from the_edge_agent import YAMLEngine, SQLiteBackend

# Default in-memory LTM (data lost when engine closes)
engine = YAMLEngine()

# File-based persistent LTM
engine = YAMLEngine(ltm_path="./agent_memory.db")

# Custom backend injection
custom_backend = SQLiteBackend("./custom.db")
engine = YAMLEngine(ltm_backend=custom_backend)

# Disable LTM
engine = YAMLEngine(enable_ltm=False)

# Close engine to release resources
engine.close()
```

### Graph Database Actions (TEA-BUILTIN-001.4)

Graph actions provide entity-relationship storage using CozoDB (optional dependency).
When CozoDB is not installed, graph actions return informative error messages.

Required dependency (optional):
- `pip install 'pycozo[embedded]'` - For CozoDB graph backend

All graph actions are available via dual namespaces: `graph.*` and `actions.graph_*`.

#### graph.store_entity

Store entities with type, properties, and optional embeddings:

```yaml
- name: store_user
  uses: graph.store_entity
  with:
    entity_id: "{{ state.user_id }}"
    entity_type: "User"
    properties:
      name: "{{ state.user_name }}"
      role: "{{ state.user_role }}"
```

**graph.store_entity** returns:
- `{"success": true, "entity_id": str, "entity_type": str}` on success
- `{"success": false, "error": str, "error_type": str}` on failure

#### graph.store_relation

Create relationships between entities:

```yaml
- name: create_ownership
  uses: graph.store_relation
  with:
    source_id: "{{ state.user_id }}"
    target_id: "{{ state.project_id }}"
    relation_type: "owns"
    properties:
      since: "{{ state.created_date }}"
```

**graph.store_relation** returns:
- `{"success": true, "source_id": str, "target_id": str, "relation_type": str}` on success
- `{"success": false, "error": str, "error_type": str}` on failure

#### graph.query

Execute Datalog queries against the graph:

```yaml
- name: find_owned_projects
  uses: graph.query
  with:
    query: |
      ?[project_id, project_name] :=
        *relation{source_id: '{{ state.user_id }}', relation_type: 'owns', target_id: project_id},
        *entity{entity_id: project_id, properties: props},
        project_name = get(props, 'name')
```

**graph.query** returns:
- `{"success": true, "results": list, "count": int}` with query results
- `{"success": false, "error": str, "error_type": str}` on failure

#### graph.retrieve_context

Retrieve contextual information for an entity:

```yaml
- name: get_user_context
  uses: graph.retrieve_context
  with:
    entity_id: "{{ state.user_id }}"
    max_depth: 2
```

**graph.retrieve_context** returns:
- `{"success": true, "entity": dict, "relations": list, "related_entities": list}` on success
- `{"success": false, "error": str, "error_type": str}` on failure

#### Example: Knowledge Graph Agent

```yaml
name: knowledge-graph-agent
description: Agent that builds and queries a knowledge graph

state_schema:
  entity_type: str
  entity_data: dict
  query_result: list

nodes:
  - name: store_entity
    uses: graph.store_entity
    with:
      entity_id: "{{ state.entity_data.id }}"
      entity_type: "{{ state.entity_type }}"
      properties: "{{ state.entity_data.properties }}"

  - name: find_related
    uses: graph.retrieve_context
    with:
      entity_id: "{{ state.entity_data.id }}"
      max_depth: 2
    output: context

  - name: format_result
    run: |
      return {"query_result": state["context"]["relations"]}

edges:
  - from: __start__
    to: store_entity
  - from: store_entity
    to: find_related
  - from: find_related
    to: format_result
  - from: format_result
    to: __end__
```

#### Python Configuration

```python
from the_edge_agent import YAMLEngine

# Graph backend auto-enabled if CozoDB installed
engine = YAMLEngine()

# File-based persistent graph
engine = YAMLEngine(graph_path="./agent_graph.db")

# Disable graph (even if CozoDB available)
engine = YAMLEngine(enable_graph=False)

# Check if CozoDB is available
from the_edge_agent import COZO_AVAILABLE
if COZO_AVAILABLE:
    print("CozoDB is installed")
else:
    print("Install with: pip install 'pycozo[embedded]'")
```

### Web Actions (TEA-BUILTIN-002.1)

Web actions for scraping, crawling, and searching the web. Designed for Firebase Cloud Functions compatibility using external API delegation.

- **Firecrawl** (https://firecrawl.dev): Handles scraping/crawling with JS rendering
- **Perplexity** (https://perplexity.ai): Handles web search with AI-powered answers

All web actions are available via dual namespaces: `web.*` and `actions.web_*`.

#### web.scrape

Scrape web content via Firecrawl API (returns LLM-ready markdown):

```yaml
- name: fetch_article
  uses: web.scrape
  with:
    url: "{{ state.target_url }}"
    formats: ["markdown", "links"]  # Optional: markdown, html, links, screenshot, extract
    only_main_content: true         # Exclude headers/footers/nav
    timeout: 30000                  # Request timeout in ms
  output: scraped_content
```

**web.scrape** returns:
- Success: `{"success": true, "url": str, "markdown": str, "links": list, "metadata": {...}}`
- Failure: `{"success": false, "error": str, "error_type": str}`

Error types: `configuration`, `rate_limit`, `payment_required`, `timeout`, `connection`, `api_error`

Advanced features:
```yaml
# Structured extraction with JSON schema
- name: extract_products
  uses: web.scrape
  with:
    url: "https://example.com/products"
    extract_schema:
      type: object
      properties:
        products:
          type: array
          items:
            type: object
            properties:
              name: { type: string }
              price: { type: number }

# Natural language extraction prompt
- name: extract_with_prompt
  uses: web.scrape
  with:
    url: "https://example.com/article"
    extract_prompt: "Extract the author name and publication date"

# Browser actions before scraping (for interactive pages)
- name: scrape_with_interaction
  uses: web.scrape
  with:
    url: "https://example.com/lazy-load"
    actions:
      - type: click
        selector: "#load-more-btn"
      - type: wait
        milliseconds: 2000
    formats: ["markdown"]

# Mobile viewport and tag filtering
- name: scrape_mobile
  uses: web.scrape
  with:
    url: "https://example.com"
    mobile: true
    include_tags: ["article", "main"]
    exclude_tags: ["nav", "footer"]
```

#### web.crawl

Crawl multiple pages from a starting URL:

```yaml
- name: crawl_docs
  uses: web.crawl
  with:
    url: "https://docs.example.com"
    max_depth: 2                # Maximum crawl depth (default: 2)
    limit: 20                   # Maximum pages to crawl (default: 10)
    include_paths: ["/api/*"]   # Only crawl matching paths
    exclude_paths: ["/admin/*"] # Skip matching paths
    allow_external_links: false # Follow external links (default: false)
    poll_interval: 2.0          # Seconds between status checks (default: 2.0)
    max_poll_time: 300.0        # Max seconds to wait (default: 300.0)
  output: crawled_pages
```

**web.crawl** returns:
- Success: `{"success": true, "pages": [{"url": str, "markdown": str, "metadata": dict}, ...], "total_pages": int, "job_id": str}`
- Failure: `{"success": false, "error": str, "error_type": str, "job_id": str}`

#### web.search

Search the web via Perplexity API:

```yaml
- name: search_topic
  uses: web.search
  with:
    query: "{{ state.topic }} latest news 2025"
    num_results: 10
  output: search_results
```

**web.search** returns:
- Success: `{"success": true, "results": [{"title": str, "url": str, "snippet": str, "position": int}], "query": str, "total_results": int, "answer": str}`
- Failure: `{"success": false, "error": str, "error_type": str}`

The `answer` field contains Perplexity's AI-synthesized answer based on search results.

#### Environment Variables

Web actions require external API keys:

```bash
# Required for web.scrape and web.crawl
export FIRECRAWL_API_KEY="your-firecrawl-api-key"  # From https://firecrawl.dev

# Required for web.search
export PERPLEXITY_API_KEY="your-perplexity-api-key"  # From https://perplexity.ai
```

#### Example: Research Agent

```yaml
name: research-agent
description: Agent that researches a topic and extracts structured data

state_schema:
  topic: str
  search_results: dict
  article_content: dict
  summary: str

nodes:
  - name: search
    uses: web.search
    with:
      query: "{{ state.topic }}"
      num_results: 5
    output: search_results

  - name: scrape_top_result
    uses: web.scrape
    with:
      url: "{{ state.search_results.results[0].url }}"
      formats: ["markdown"]
      only_main_content: true
    output: article_content

  - name: summarize
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: system
          content: Summarize the following article concisely.
        - role: user
          content: "{{ state.article_content.markdown }}"
    output: summary

edges:
  - from: __start__
    to: search
  - from: search
    to: scrape_top_result
  - from: scrape_top_result
    to: summarize
  - from: summarize
    to: __end__
```

#### Python Usage

```python
from the_edge_agent import YAMLEngine

engine = YAMLEngine()

# Scrape a page
result = engine.actions_registry['web.scrape'](
    state={},
    url="https://example.com",
    formats=["markdown", "links"],
    only_main_content=True
)

if result['success']:
    print(result['markdown'])
else:
    print(f"Error ({result['error_type']}): {result['error']}")

# Search the web
result = engine.actions_registry['web.search'](
    state={},
    query="AI developments 2025",
    num_results=5
)

if result['success']:
    print(f"Answer: {result['answer']}")
    for r in result['results']:
        print(f"- {r['title']}: {r['url']}")
```

### RAG Actions (TEA-BUILTIN-002.2)

RAG (Retrieval-Augmented Generation) actions provide embedding creation, vector storage, and semantic search capabilities for building knowledge-augmented agents.

#### Embedding Providers

RAG actions support pluggable embedding providers:

| Provider | Models | Dimensions | Notes |
|----------|--------|------------|-------|
| **OpenAI** | text-embedding-3-small (default) | 1536 | Remote API, requires OPENAI_API_KEY |
| | text-embedding-3-large | 3072 | Higher quality, larger vectors |
| | text-embedding-ada-002 | 1536 | Legacy model |
| **Ollama** | nomic-embed-text (default) | 768 | Local, 8K context |
| | mxbai-embed-large | 1024 | High accuracy |
| | all-minilm | 384 | Lightweight, fast |
| | bge-m3 | 1024 | Highest retrieval accuracy |

#### Vector Stores

| Store | Dependencies | Persistence | Notes |
|-------|--------------|-------------|-------|
| **InMemoryVectorStore** | None | Via checkpoints | Default, pure Python |
| **ChromaVectorStore** | chromadb | Automatic | Persistent storage |

#### embedding.create

Generate embeddings from text:

```yaml
# Single text
- name: embed_query
  uses: embedding.create
  with:
    text: "{{ state.query }}"
    model: text-embedding-3-small  # optional
  output: embedding_result

# Batch embedding
- name: embed_documents
  uses: embedding.create
  with:
    text: "{{ state.documents }}"  # List of texts
  output: embeddings_result
```

**embedding.create** returns:
- Single: `{"embedding": List[float], "model": str, "dimensions": int}`
- Batch: `{"embeddings": List[List[float]], "model": str, "count": int, "dimensions": int}`
- Error: `{"error": str, "success": false}`

#### vector.store

Store documents with embeddings:

```yaml
# Store with auto-generated embeddings
- name: store_docs
  uses: vector.store
  with:
    texts:
      - "First document content"
      - "Second document content"
    metadata:
      - type: article
        date: "2024-01-15"
      - type: blog
        date: "2024-01-20"
    collection: my_knowledge_base
  output: store_result

# Store with pre-computed embeddings
- name: store_with_embeddings
  uses: vector.store
  with:
    texts: "{{ state.texts }}"
    embeddings: "{{ state.embeddings }}"
    ids: "{{ state.doc_ids }}"
    collection: my_collection
  output: store_result
```

**vector.store** returns:
- Success: `{"stored": int, "collection": str, "ids": List[str]}`
- Error: `{"error": str, "success": false}`

#### vector.query

Semantic similarity search:

```yaml
# Basic query
- name: search_knowledge
  uses: vector.query
  with:
    query: "{{ state.question }}"
    k: 5
    collection: my_knowledge_base
  output: search_results

# Query with metadata filter
- name: search_filtered
  uses: vector.query
  with:
    query: "{{ state.question }}"
    k: 10
    collection: my_knowledge_base
    filter:
      type: article
      date_gte: "2024-01-01"
  output: search_results
```

**vector.query** returns:
- Success: `{"results": [{"id": str, "text": str, "score": float, "metadata": dict}], "query": str, "collection": str, "k": int}`
- Error: `{"error": str, "success": false}`

**Metadata Filter Operators**:
- `field`: Exact match (`{"type": "article"}`)
- `field_gte`: Greater than or equal (`{"count_gte": 10}`)
- `field_lte`: Less than or equal (`{"date_lte": "2024-12-31"}`)
- `field_gt`: Greater than (`{"score_gt": 0.8}`)
- `field_lt`: Less than (`{"priority_lt": 3}`)
- `field_ne`: Not equal (`{"status_ne": "deleted"}`)
- `field_in`: In list (`{"category_in": ["tech", "science"]}`)

#### RAG Configuration in Settings

```yaml
settings:
  rag:
    # Embedding provider: "openai" (default) or "ollama"
    embedding_provider: openai
    embedding_model: text-embedding-3-small

    # For OpenAI with custom base URL (LocalAI, vLLM, etc.)
    # openai_base_url: http://localhost:8080/v1

    # For Ollama provider
    # embedding_provider: ollama
    # embedding_model: nomic-embed-text
    # ollama_base_url: http://localhost:11434
    # ollama_timeout: 60.0

    # Vector store: "memory" (default) or "chroma"
    vector_store: memory
    # chroma_path: ./chroma_db  # For persistent Chroma
```

#### RAG Agent Example

A complete example of a knowledge-augmented agent:

```yaml
name: knowledge-agent
description: Agent that answers questions using a knowledge base

state_schema:
  question: str
  context: list
  answer: str

settings:
  rag:
    embedding_provider: openai
    embedding_model: text-embedding-3-small
    vector_store: memory

nodes:
  - name: search_knowledge
    uses: vector.query
    with:
      query: "{{ state.question }}"
      k: 3
      collection: knowledge_base
    output: search_results

  - name: build_context
    run: |
      results = state.get('search_results', {}).get('results', [])
      context = [r['text'] for r in results]
      return {"context": context}

  - name: generate_answer
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: system
          content: |
            Answer the question using only the provided context.
            If the context doesn't contain the answer, say "I don't know."
        - role: user
          content: |
            Context:
            {% for doc in state.context %}
            - {{ doc }}
            {% endfor %}

            Question: {{ state.question }}
    output: llm_result

  - name: extract_answer
    run: |
      return {"answer": state.get('llm_result', {}).get('content', 'Error')}

edges:
  - from: __start__
    to: search_knowledge
  - from: search_knowledge
    to: build_context
  - from: build_context
    to: generate_answer
  - from: generate_answer
    to: extract_answer
  - from: extract_answer
    to: __end__
```

#### Python Usage

```python
from the_edge_agent import YAMLEngine

engine = YAMLEngine()

# Store knowledge (typically done once during setup)
engine.actions_registry['vector.store'](
    state={},
    texts=[
        "The capital of France is Paris.",
        "Python was created by Guido van Rossum in 1991.",
        "The speed of light is approximately 299,792,458 m/s."
    ],
    metadata=[
        {"topic": "geography"},
        {"topic": "programming"},
        {"topic": "physics"}
    ],
    collection="knowledge_base"
)

# Query the knowledge base
result = engine.actions_registry['vector.query'](
    state={},
    query="Who created Python?",
    k=2,
    collection="knowledge_base"
)

for r in result['results']:
    print(f"Score: {r['score']:.3f} - {r['text']}")

# Use Ollama for local embeddings (no API key needed)
result = engine.actions_registry['embedding.create'](
    state={},
    text="Hello world",
    provider="ollama",
    model="nomic-embed-text"
)
print(f"Embedding dimensions: {result['dimensions']}")
```

### Tools Bridge Actions (TEA-BUILTIN-002.3)

Tools bridge actions provide access to external tool ecosystems (CrewAI, MCP, LangChain) without writing Python code. All bridges are optional - they gracefully degrade if dependencies are not installed.

#### Dependencies

All dependencies are optional - install only what you need:

```bash
# For CrewAI tools (700+ available)
pip install crewai crewai-tools

# For MCP (Model Context Protocol) servers
pip install mcp

# For LangChain tools
pip install langchain langchain-community
```

#### tools.crewai

Execute CrewAI tools by name:

```yaml
- name: search_web
  uses: tools.crewai
  with:
    tool: SerperDevTool
    query: "{{ state.search_query }}"
    timeout: 30.0  # Optional timeout in seconds
  output: search_result
```

**tools.crewai** returns:
- Success: `{"result": any, "tool": str, "success": true}`
- Failure: `{"error": str, "error_type": str, "tool": str, "success": false}`

Error types: `import` (library not installed), `execution`, `timeout`

Available CrewAI tools include: `SerperDevTool`, `ScrapeWebsiteTool`, `WebsiteSearchTool`, `FileReadTool`, `DirectoryReadTool`, `CodeDocsSearchTool`, `YoutubeVideoSearchTool`, `GithubSearchTool`, `PDFSearchTool`, and many more.

#### tools.mcp

Connect to MCP servers and execute their tools:

```yaml
- name: read_file
  uses: tools.mcp
  with:
    server:
      command: npx
      args: ["-y", "@anthropic/mcp-server-filesystem"]
    tool: read_file
    path: "/tmp/data.txt"
    timeout: 30.0
  output: file_result
```

Or use a named server from settings:

```yaml
settings:
  tools:
    mcp:
      servers:
        - name: filesystem
          command: npx
          args: ["-y", "@anthropic/mcp-server-filesystem"]

nodes:
  - name: read_file
    uses: tools.mcp
    with:
      server: filesystem  # Reference by name
      tool: read_file
      path: "/tmp/data.txt"
```

**tools.mcp** returns:
- Success: `{"result": any, "tool": str, "server": str, "success": true}`
- Failure: `{"error": str, "error_type": str, "tool": str, "success": false}`

#### tools.langchain

Execute LangChain tools:

```yaml
- name: search
  uses: tools.langchain
  with:
    tool: DuckDuckGoSearchRun
    query: "{{ state.query }}"
    timeout: 30.0
  output: search_result
```

**tools.langchain** returns:
- Success: `{"result": any, "tool": str, "success": true}`
- Failure: `{"error": str, "error_type": str, "tool": str, "success": false}`

Available LangChain tools include: `DuckDuckGoSearchRun`, `WikipediaQueryRun`, `ArxivQueryRun`, `PubmedQueryRun`, `TavilySearchResults`, `GoogleSearchRun`, `ReadFileTool`, `WriteFileTool`, and many more.

#### tools.discover

Discover available tools from configured sources:

```yaml
- name: list_tools
  uses: tools.discover
  with:
    source: all  # "crewai", "mcp", "langchain", or "all"
    filter: search  # Optional: filter by name
    use_cache: true  # Use cached discovery results
  output: available_tools
```

**tools.discover** returns:
```json
{
  "tools": [
    {
      "name": "SerperDevTool",
      "description": "Search the web using Serper API",
      "parameters": {"query": {"type": "string", "required": true}},
      "source": "crewai"
    }
  ],
  "sources": ["crewai", "langchain", "mcp"],
  "count": 15,
  "success": true
}
```

#### Tools Configuration in Settings

Configure tools bridges globally in your YAML settings:

```yaml
settings:
  tools:
    crewai:
      enabled: true
      tools: [SerperDevTool, ScrapeWebsiteTool]
    mcp:
      servers:
        - name: filesystem
          command: npx
          args: ["-y", "@anthropic/mcp-server-filesystem"]
        - name: brave-search
          command: npx
          args: ["-y", "@anthropic/mcp-server-brave-search"]
          env:
            BRAVE_API_KEY: "{{ secrets.brave_api_key }}"
    langchain:
      enabled: true
      tools: [DuckDuckGoSearchRun, WikipediaQueryRun]
```

#### Example: Multi-Tool Research Agent

```yaml
name: research-agent
description: Agent that uses multiple tool ecosystems

state_schema:
  topic: str
  search_results: dict
  wiki_results: dict
  summary: str

settings:
  tools:
    crewai:
      enabled: true
    langchain:
      enabled: true

nodes:
  - name: discover_tools
    uses: tools.discover
    with:
      source: all
      filter: search
    output: available_tools

  - name: web_search
    uses: tools.crewai
    with:
      tool: SerperDevTool
      query: "{{ state.topic }}"
    output: search_results

  - name: wiki_search
    uses: tools.langchain
    with:
      tool: WikipediaQueryRun
      query: "{{ state.topic }}"
    output: wiki_results

  - name: summarize
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: system
          content: Summarize the research findings.
        - role: user
          content: |
            Web results: {{ state.search_results.result }}
            Wiki results: {{ state.wiki_results.result }}
    output: summary

edges:
  - from: __start__
    to: discover_tools
  - from: discover_tools
    to: web_search
  - from: web_search
    to: wiki_search
  - from: wiki_search
    to: summarize
  - from: summarize
    to: __end__
```

#### Python Usage

```python
from the_edge_agent import YAMLEngine

engine = YAMLEngine()

# Use CrewAI tool
result = engine.actions_registry['tools.crewai'](
    state={},
    tool="SerperDevTool",
    query="AI developments 2025"
)
print(f"Search result: {result['result']}")

# Use LangChain tool
result = engine.actions_registry['tools.langchain'](
    state={},
    tool="DuckDuckGoSearchRun",
    query="Python programming"
)
print(f"DDG result: {result['result']}")

# Discover all available tools
result = engine.actions_registry['tools.discover'](
    state={},
    source="all"
)
for tool in result['tools']:
    print(f"[{tool['source']}] {tool['name']}: {tool['description'][:50]}...")

# Clear discovery cache
engine.actions_registry['tools.clear_cache'](state={})
```

All tools bridge actions are available via dual namespaces: `tools.*` and `actions.tools_*`.

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
- [x] Retry logic with backoff (llm.retry)
- [ ] Caching of node results
- [ ] Workflow composition (import/include)
- [ ] Environment-specific configs
- [ ] Schema validation for YAML files
- [ ] Visual workflow editor
- [ ] Metrics and monitoring hooks

Recently implemented:

- [x] RAG Actions: embedding.create, vector.store, vector.query (TEA-BUILTIN-002.2)
- [x] Web Actions: web.scrape, web.crawl, web.search (TEA-BUILTIN-002.1)
- [x] LLM Enhanced Actions: llm.stream, llm.retry, llm.tools (TEA-BUILTIN-001.2)
- [x] Checkpoint persistence for save/resume workflows (v0.5.0)
