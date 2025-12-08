# YAML Agent Specification

Version: 1.0.0

This document defines the formal specification for YAML-based agent configuration in The Edge Agent.

## Table of Contents

- [Overview](#overview)
- [State and Variable Passing](#state-and-variable-passing)
- [Document Structure](#document-structure)
- [Top-Level Keys](#top-level-keys)
- [Node Specification](#node-specification)
- [Edge Specification](#edge-specification)
- [Template Syntax](#template-syntax)
- [Built-in Actions](#built-in-actions)
- [Complete Examples](#complete-examples)

---

## Overview

YAML agents define declarative workflows that compile to `StateGraph` instances. The YAML structure maps directly to the Python API:

| YAML Concept | Python Equivalent |
|--------------|-------------------|
| `nodes:` | `graph.add_node()` |
| `edges:` | `graph.add_edge()`, `add_conditional_edges()`, `add_parallel_edge()` |
| `state_schema:` | `StateGraph(state_schema={...})` |
| `config:` | `graph.compile(...)` |

---

## State and Variable Passing

### How State Works

**State is the mechanism for passing data between nodes.** Every node:
1. Receives the current state as input
2. Returns a dictionary of updates
3. Updates are merged into state for the next node

```yaml
nodes:
  - name: step1
    run: |
      # Access input from initial state
      query = state["user_query"]
      # Return updates - these become available to subsequent nodes
      return {"processed_query": query.strip().lower()}

  - name: step2
    run: |
      # Access data from step1
      processed = state["processed_query"]
      results = search(processed)
      return {"search_results": results}

  - name: step3
    run: |
      # Access data from both step1 and step2
      query = state["processed_query"]
      results = state["search_results"]
      return {"summary": f"Found {len(results)} results for '{query}'"}
```

### State Flow Diagram

```
Initial State          Node 1 Output         Node 2 Output         Final State
{user_query: "AI"}  →  {processed_query}  →  {search_results}  →  All keys merged
                       merged into state     merged into state
```

### Variable Scopes

| Scope | Syntax | Description |
|-------|--------|-------------|
| State | `state["key"]` or `{{ state.key }}` | Runtime data passed between nodes |
| Variables | `variables["key"]` or `{{ variables.key }}` | Global constants defined in YAML |
| Secrets | `secrets["key"]` or `{{ secrets.key }}` | Sensitive values (API keys, etc.) |

### Example: Complete Data Flow

```yaml
name: data-flow-example
description: Demonstrates variable passing between nodes

variables:
  max_items: 10
  api_version: "v2"

state_schema:
  input: str
  items: list
  filtered: list
  output: str

nodes:
  - name: fetch
    run: |
      # Access global variable
      version = "{{ variables.api_version }}"
      # Return data for next node
      return {
        "items": [{"id": i, "value": f"item-{i}"} for i in range(20)]
      }

  - name: filter
    run: |
      # Access data from fetch node via state
      items = state["items"]
      # Access global variable
      max_items = {{ variables.max_items }}
      # Return filtered data
      return {"filtered": items[:max_items]}

  - name: format
    run: |
      # Access data from filter node
      filtered = state["filtered"]
      # Access original input
      original_input = state["input"]
      # Combine everything
      output = f"Query: {original_input}\nResults: {len(filtered)} items"
      return {"output": output}

edges:
  - from: __start__
    to: fetch
  - from: fetch
    to: filter
  - from: filter
    to: format
  - from: format
    to: __end__
```

---

## Document Structure

```yaml
# Metadata (optional)
name: string                    # Agent identifier
description: string             # Human-readable description

# Global Variables (optional)
variables:
  key: value

# State Schema (optional but recommended)
state_schema:
  field_name: type

# Node Definitions (required)
nodes:
  - name: string
    # ... node configuration

# Edge Definitions (required)
edges:
  - from: string
    to: string
    # ... edge configuration

# Compilation Options (optional)
config:
  raise_exceptions: boolean
  interrupt_before: [string]
  interrupt_after: [string]
```

---

## Top-Level Keys

### `name` (optional)
```yaml
name: my-research-agent
```
Identifier for the agent. Used for logging and debugging.

### `description` (optional)
```yaml
description: An agent that searches and summarizes research papers
```
Human-readable description of what the agent does.

### `variables` (optional)
```yaml
variables:
  api_endpoint: https://api.example.com
  max_retries: 3
  timeout_seconds: 30
```
Global constants accessible throughout the workflow via `{{ variables.key }}` or `variables["key"]`.

### `state_schema` (optional)
```yaml
state_schema:
  query: str
  results: list
  count: int
  is_valid: bool
```
Defines the expected structure of the state object. Types are Python type names as strings.

### `nodes` (required)
Array of node definitions. See [Node Specification](#node-specification).

### `edges` (required)
Array of edge definitions. See [Edge Specification](#edge-specification).

### `config` (optional)
```yaml
config:
  raise_exceptions: true        # Raise errors vs yield error events (default: false)
  interrupt_before: [node_name] # Pause before these nodes
  interrupt_after: [node_name]  # Pause after these nodes
```

---

## Node Specification

### Basic Structure

```yaml
nodes:
  - name: string          # Required: unique node identifier
    # One of the following execution methods:
    run: string           # Inline Python code
    script: string        # Alias for run (GitLab CI style)
    uses: string          # Built-in or custom action
    steps: array          # Multi-step execution

    # Additional options:
    fan_in: boolean       # Mark as fan-in node for parallel flows
    with: object          # Parameters for 'uses' actions
    output: string        # Key name for action result
```

### Execution Methods

#### Method 1: Inline Python Code (`run:`)

```yaml
- name: process_data
  run: |
    # Full Python with state access
    data = state["input"]
    processed = data.upper()
    count = len(data.split())

    # Must return a dict to update state
    return {
      "processed": processed,
      "word_count": count
    }
```

**Available in execution context:**
- `state` - Current state dictionary
- `json` - Python json module
- `requests` - Auto-imported if referenced
- `datetime` - Auto-imported if referenced
- `OpenAI` - Auto-imported if referenced

#### Method 2: Script (`script:`)

Alias for `run:`, inspired by GitLab CI:

```yaml
- name: process_data
  script: |
    result = state["value"] * 2
    return {"doubled": result}
```

#### Method 3: Built-in Actions (`uses:`)

```yaml
- name: call_api
  uses: http.get
  with:
    url: "{{ variables.api_url }}/data"
    headers:
      Authorization: "Bearer {{ secrets.api_key }}"
  output: api_response
```

**Parameters:**
- `uses:` - Action name (see [Built-in Actions](#built-in-actions))
- `with:` - Action parameters (template-processed)
- `output:` - State key for result (optional)

#### Method 4: Expression (`run.type: expression`)

For simple evaluations:

```yaml
- name: check_count
  run:
    type: expression
    value: len(state.get("items", [])) > 0
    output_key: has_items
```

#### Method 5: Multi-Step (`steps:`)

GitHub Actions-style sequential steps within a node:

```yaml
- name: multi_step_process
  steps:
    - name: step1
      run: |
        return {"intermediate": state["input"] + " processed"}

    - name: step2
      uses: http.post
      with:
        url: https://api.example.com/submit
        json:
          data: "{{ state.intermediate }}"

    - name: step3
      run: |
        # Can access results from step1 and step2
        return {"final": f"Submitted: {state['intermediate']}"}
```

### Fan-In Nodes

For collecting results from parallel flows:

```yaml
- name: aggregate
  fan_in: true
  run: |
    # parallel_results contains list of states from parallel flows
    all_data = [r.get("data") for r in parallel_results]
    combined = "\n".join(all_data)
    return {"combined_results": combined}
```

---

## Edge Specification

### Basic Structure

```yaml
edges:
  - from: string          # Source node (or __start__)
    to: string            # Target node (or __end__)

    # Optional:
    type: string          # Edge type: normal | parallel
    condition: object     # Conditional routing
    when: any             # Simple condition shorthand
    fan_in: string        # Fan-in node for parallel edges
```

### Edge Types

#### Simple Edge

```yaml
- from: node_a
  to: node_b
```

#### Entry Point

```yaml
- from: __start__
  to: first_node
```

#### Finish Point

```yaml
- from: last_node
  to: __end__
```

#### Conditional Edge

Route based on expression evaluation:

```yaml
# Method 1: Expression condition
- from: validate
  to: process
  condition:
    type: expression
    value: state["is_valid"] == True
  when: true

- from: validate
  to: error_handler
  condition:
    type: expression
    value: state["is_valid"] == True
  when: false

# Method 2: Simple when clause
- from: check
  to: proceed
  when: "state['count'] > 0"

# Method 3: Variable reference
- from: check
  to: skip
  when: "!should_process"    # Negation with !
```

#### Parallel Edge

Execute flows concurrently:

```yaml
# Define parallel flows
- from: start
  to: flow_a
  type: parallel
  fan_in: combine

- from: start
  to: flow_b
  type: parallel
  fan_in: combine

- from: start
  to: flow_c
  type: parallel
  fan_in: combine

# Continue after fan-in
- from: combine
  to: next_step
```

---

## Template Syntax

### Basic Substitution

| Syntax | Description | Example |
|--------|-------------|---------|
| `{{ state.key }}` | State value | `{{ state.user_name }}` |
| `{{ variables.key }}` | Global variable | `{{ variables.api_url }}` |
| `{{ secrets.key }}` | Secret value | `{{ secrets.api_key }}` |
| `${ key }` | GitLab CI style | `${ CI_COMMIT_SHA }` |

### Filters

Apply transformations to values:

| Filter | Description | Example |
|--------|-------------|---------|
| `json` | JSON serialize | `{{ state.data \| json }}` |
| `upper` | Uppercase | `{{ state.name \| upper }}` |
| `lower` | Lowercase | `{{ state.name \| lower }}` |

### Template in Different Contexts

```yaml
variables:
  base_url: https://api.example.com

nodes:
  - name: example
    uses: http.post
    with:
      # Template in URL
      url: "{{ variables.base_url }}/users/{{ state.user_id }}"

      # Template in headers
      headers:
        Authorization: "Bearer {{ secrets.token }}"
        X-Request-ID: "{{ state.request_id }}"

      # Template in JSON body
      json:
        name: "{{ state.name | upper }}"
        data: "{{ state.payload | json }}"
```

### Nested Access

```yaml
# Access nested state values
{{ state.user.profile.name }}

# Access nested variables
{{ variables.config.timeout }}
```

---

## Built-in Actions

### LLM Actions

#### `llm.call`

Call OpenAI-compatible LLM API:

```yaml
- name: generate
  uses: llm.call
  with:
    model: gpt-4                    # Required
    messages:                       # Required
      - role: system
        content: You are helpful
      - role: user
        content: "{{ state.prompt }}"
    temperature: 0.7                # Optional (default: 0.7)
  output: llm_response
```

**Returns:**
```python
{
  "content": "LLM response text",
  "usage": {"prompt_tokens": N, "completion_tokens": N, ...}
}
```

### HTTP Actions

#### `http.get`

```yaml
- name: fetch
  uses: http.get
  with:
    url: https://api.example.com/data    # Required
    headers:                              # Optional
      Authorization: Bearer token
  output: response_data
```

#### `http.post`

```yaml
- name: submit
  uses: http.post
  with:
    url: https://api.example.com/submit  # Required
    json:                                 # Optional: JSON body
      key: value
    headers:                              # Optional
      Content-Type: application/json
  output: response_data
```

### File Actions

File actions support both local paths and remote URIs via fsspec (S3, GCS, Azure, etc.).

#### `file.read`

```yaml
# Local file
- name: load_local
  uses: file.read
  with:
    path: ./data/input.txt               # Required
  output: file_content

# Remote file (S3)
- name: load_s3
  uses: file.read
  with:
    path: s3://my-bucket/data/input.txt
    cache: simple                        # Optional: "simple", "file", "block"
  output: file_content
```

**Returns:**
- Success: `{"content": str, "success": true}`
- Failure: `{"success": false, "error": str, "error_type": str}`

#### `file.write`

```yaml
# Local file
- name: save_local
  uses: file.write
  with:
    path: "./output/{{ state.filename }}.txt"  # Required
    content: "{{ state.data }}"                 # Required

# Remote file (GCS)
- name: save_gcs
  uses: file.write
  with:
    path: gs://my-bucket/output/result.json
    content: "{{ state.data | json }}"
```

**Returns:**
- Success: `{"path": str, "success": true}`
- Failure: `{"success": false, "error": str, "error_type": str}`

**Supported URI schemes:**
- Local: `./path`, `/abs/path`, `file:///path`
- AWS S3: `s3://bucket/path` (requires `pip install s3fs`)
- GCS: `gs://bucket/path` (requires `pip install gcsfs`)
- Azure: `az://container/path` (requires `pip install adlfs`)
- Memory: `memory://path` (for testing)

### Notification Actions

#### `actions.notify`

```yaml
- name: alert
  uses: actions.notify
  with:
    channel: slack                       # Required
    message: "Task completed!"           # Required
```

**Returns:** `{"sent": true}`

### Checkpoint Actions

#### `checkpoint.save`

Save workflow checkpoint for persistence and recovery:

```yaml
- name: save_progress
  uses: checkpoint.save
  with:
    path: ./checkpoints/{{ state.step_name }}.pkl  # Required
  output: save_result
```

**Returns:**
- Success: `{"checkpoint_path": str, "saved": true}`
- Failure: `{"checkpoint_path": str, "saved": false, "error": str}`

#### `checkpoint.load`

Load checkpoint from file:

```yaml
- name: load_previous
  uses: checkpoint.load
  with:
    path: ./checkpoints/previous.pkl               # Required
  output: loaded_checkpoint
```

**Returns:**
```python
{
  "checkpoint_state": dict,      # Saved state
  "checkpoint_node": str,        # Node where checkpoint was saved
  "checkpoint_config": dict,     # Saved config
  "checkpoint_timestamp": float, # Unix timestamp
  "checkpoint_version": str      # Checkpoint format version
}
```

### LLM Enhanced Actions

#### `llm.stream`

Stream LLM responses with chunk aggregation:

```yaml
- name: stream_response
  uses: llm.stream
  with:
    model: gpt-4                    # Required
    messages:                       # Required
      - role: user
        content: "{{ state.query }}"
    temperature: 0.7                # Optional
  output: stream_result
```

**Returns:**
```python
{
  "content": "Full aggregated response",
  "usage": {"prompt_tokens": N, "completion_tokens": N},
  "streamed": true,
  "chunk_count": N
}
```

#### `llm.retry`

LLM calls with exponential backoff retry logic:

```yaml
- name: resilient_call
  uses: llm.retry
  with:
    model: gpt-4                    # Required
    messages:                       # Required
      - role: user
        content: "{{ state.query }}"
    max_retries: 3                  # Optional (default: 3)
    base_delay: 1.0                 # Optional (default: 1.0)
    max_delay: 60.0                 # Optional (default: 60.0)
    temperature: 0.7                # Optional
  output: retry_result
```

**Returns:**
- Success: `{"content": str, "usage": dict, "attempts": int, "total_delay": float}`
- Failure: `{"error": str, "success": false, "attempts": int, "total_delay": float}`

**Retry behavior:**
- Retryable: HTTP 429 (rate limit), HTTP 5xx, timeouts, connection errors
- Non-retryable: HTTP 4xx (except 429)
- Respects `Retry-After` header when present

#### `llm.tools`

Function/tool calling with automatic action dispatch:

```yaml
- name: agent_with_tools
  uses: llm.tools
  with:
    model: gpt-4                    # Required
    messages:                       # Required
      - role: system
        content: You are a helpful assistant with access to tools.
      - role: user
        content: "{{ state.query }}"
    tools:                          # Required
      - name: search_web
        description: Search the web for information
        parameters:
          query:
            type: string
            description: Search query
            required: true
        action: http.get            # Maps to registered action
    tool_choice: auto               # Optional: "auto", "none", or tool name
    max_tool_rounds: 10             # Optional (default: 10)
  output: tools_result
```

**Returns:**
- Success: `{"content": str, "tool_calls": list, "tool_results": list, "rounds": int}`
- Failure: `{"error": str, "success": false, "tool_calls": list, "tool_results": list}`

### Data Processing Actions

#### `json.parse`

Parse JSON string to Python object:

```yaml
- name: parse_response
  uses: json.parse
  with:
    text: "{{ state.raw_response }}"  # Required
    strict: true                       # Optional (default: true)
    default: {}                        # Optional fallback (requires strict: false)
  output: parsed_data
```

**Returns:**
- Success: `{"data": any, "success": true}`
- Failure: `{"error": str, "success": false, "error_type": "parse", "position": {"line": int, "column": int}}`

#### `json.transform`

Transform data with JMESPath or JSONPath expressions:

```yaml
- name: extract_users
  uses: json.transform
  with:
    data: "{{ state.api_response }}"                          # Required
    expression: "users[?status=='active'].{name: name, email: email}"  # Required
    engine: jmespath                                           # Optional: "jmespath" or "jsonpath"
  output: transformed_data
```

**Common expressions:**
- `user.profile.name` - Extract nested value
- `users[?status=='active']` - Filter array
- `users[*].name` - Extract all names
- `{names: users[].name, count: length(users)}` - Project new structure

**Returns:** `{"result": any, "expression": str, "success": true}`

#### `json.stringify`

Convert Python object to JSON string:

```yaml
- name: serialize
  uses: json.stringify
  with:
    data: "{{ state.result }}"    # Required
    indent: 2                      # Optional
    sort_keys: true                # Optional
  output: json_string
```

**Returns:** `{"text": str, "success": true}`

#### `csv.parse`

Parse CSV from text or file:

```yaml
- name: parse_csv
  uses: csv.parse
  with:
    text: "{{ state.csv_content }}"  # Required (or use path)
    # path: ./data/input.csv         # Alternative: read from file
    delimiter: ","                    # Optional (default: ",")
    has_header: true                  # Optional (default: true)
  output: csv_data
```

**Returns:**
- With header: `{"data": [{"col1": "val1", ...}], "headers": ["col1", ...], "row_count": int, "success": true}`
- Without header: `{"data": [["val1", "val2"]], "headers": null, "row_count": int, "success": true}`

#### `csv.stringify`

Convert list to CSV string:

```yaml
- name: export_csv
  uses: csv.stringify
  with:
    data: "{{ state.records }}"           # Required
    headers: ["name", "email", "status"]  # Optional (auto-detected from dicts)
    delimiter: ","                         # Optional
  output: csv_text
```

**Returns:** `{"text": str, "row_count": int, "success": true}`

#### `data.validate`

Validate data against JSON Schema:

```yaml
- name: validate_input
  uses: data.validate
  with:
    data: "{{ state.user_input }}"        # Required
    schema:                                # Required
      type: object
      properties:
        name:
          type: string
          minLength: 1
        email:
          type: string
          format: email
      required: ["name", "email"]
  output: validation_result
```

**Returns:**
- Valid: `{"valid": true, "errors": [], "success": true}`
- Invalid: `{"valid": false, "errors": [{"path": str, "message": str}], "success": true}`

#### `data.merge`

Merge multiple dictionaries:

```yaml
- name: combine_configs
  uses: data.merge
  with:
    sources:                              # Required
      - "{{ state.default_config }}"
      - "{{ state.user_config }}"
      - "{{ state.override_config }}"
    strategy: deep                        # Optional: "deep", "shallow", "replace"
  output: merged_config
```

**Strategies:**
- `deep`: Recursively merge nested dictionaries
- `shallow`: Only merge top-level keys
- `replace`: Later sources completely replace earlier ones

**Returns:** `{"result": dict, "source_count": int, "success": true}`

#### `data.filter`

Filter list items with predicates:

```yaml
- name: filter_users
  uses: data.filter
  with:
    data: "{{ state.users }}"             # Required
    predicate:                             # Required
      field: status
      op: eq
      value: active
  output: filtered_users

# Multiple predicates (AND logic)
- name: filter_premium
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
```

**Operators:** `eq`, `ne`, `gt`, `gte`, `lt`, `lte`, `in`, `not_in`, `contains`, `startswith`, `endswith`

**Returns:** `{"result": list, "original_count": int, "filtered_count": int, "success": true}`

### Code Execution Actions

> **Security Warning:** Code execution is DISABLED by default. Enable with `YAMLEngine(enable_code_execution=True)`.
> Uses RestrictedPython sandbox - not suitable for arbitrary untrusted code.

#### `code.execute`

Execute Python code in sandboxed environment:

```yaml
- name: compute
  uses: code.execute
  with:
    code: |                               # Required
      x = 1 + 2
      y = x * 10
      result = y  # Set 'result' to return a value
    timeout: 30                           # Optional (default: 30 seconds)
    max_output_bytes: 65536               # Optional (default: 64KB)
  output: execution_result
```

**Returns:**
- Success: `{"success": true, "stdout": str, "stderr": str, "return_value": any, "execution_time_ms": float}`
- Failure: `{"success": false, "error": str, "stdout": "", "stderr": "", "return_value": null}`

**Allowed:** Math, types, iteration, list/dict operations, try/except
**Blocked:** imports, file access, network, exec/eval, dangerous dunders

#### `code.sandbox`

Manage persistent sandbox sessions:

```yaml
# Create session
- name: create_session
  uses: code.sandbox
  with:
    action: create
  output: sandbox_info

# Execute in session (variables persist)
- name: run_code
  uses: code.sandbox
  with:
    action: execute
    sandbox_id: "{{ state.sandbox_info.sandbox_id }}"
    code: |
      counter += 1
      result = counter

# Destroy session
- name: cleanup
  uses: code.sandbox
  with:
    action: destroy
    sandbox_id: "{{ state.sandbox_info.sandbox_id }}"
```

**Actions:** `create`, `execute`, `list`, `destroy`

### Observability Actions

#### `trace.start`

Start a new trace span:

```yaml
- name: start_trace
  uses: trace.start
  with:
    name: "process_data"                  # Required
    metadata:                             # Optional
      user_id: "{{ state.user_id }}"
      operation: "data_processing"
    parent_id: "{{ state.parent_span }}"  # Optional
  output: span_info
```

**Returns:** `{"span_id": str, "name": str, "parent_id": str | null, "success": true}`

#### `trace.log`

Log events, metrics, or state snapshots:

```yaml
# Log message
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

# Snapshot state
- name: log_state
  uses: trace.log
  with:
    message: "Before API call"
    snapshot_state: true
    sanitize_keys: ["api_key", "password"]
```

**Returns:** `{"logged": true, "span_id": str, "event_count": int, "success": true}`

#### `trace.end`

End current trace span:

```yaml
- name: end_trace
  uses: trace.end
  with:
    status: ok                            # "ok" or "error"
    error: "{{ state.error_message }}"    # Optional (for status: error)
```

**Returns:** `{"span_id": str, "duration_ms": float, "status": str, "success": true}`

### Memory Actions

#### `memory.store`

Store key-value pair with optional TTL:

```yaml
- name: remember_user
  uses: memory.store
  with:
    key: "user_name"                      # Required
    value: "{{ state.name }}"             # Required
    ttl: 3600                             # Optional (seconds, null = no expiration)
    namespace: "session_123"              # Optional
  output: store_result
```

**Returns:** `{"stored": true, "key": str, "namespace": str}`

#### `memory.retrieve`

Retrieve value from memory:

```yaml
- name: recall_user
  uses: memory.retrieve
  with:
    key: "user_name"                      # Required
    default: "Guest"                      # Optional
    namespace: "session_123"              # Optional
  output: retrieved_value
```

**Returns:** `{"value": any, "found": bool, "key": str}`

#### `memory.summarize`

Summarize conversation history using LLM:

```yaml
- name: compress_history
  uses: memory.summarize
  with:
    messages_key: "conversation"          # Required (state key with messages)
    max_tokens: 1000                      # Optional
    model: "gpt-3.5-turbo"                # Optional
  output: summary_result
```

**Returns:** `{"summary": str, "original_count": int, "token_estimate": int, "success": true}`

### Long-Term Memory Actions

Persistent storage using SQLite with FTS5 full-text search.

#### `ltm.store`

Store key-value pair persistently:

```yaml
- name: store_knowledge
  uses: ltm.store
  with:
    key: "user_profile"                   # Required
    value: "{{ state.profile_data }}"     # Required
    metadata:                             # Optional
      type: "profile"
      source: "onboarding"
  output: store_result
```

**Returns:** `{"success": true, "stored": true, "key": str, "created": bool}`

#### `ltm.retrieve`

Retrieve value from persistent storage:

```yaml
- name: load_knowledge
  uses: ltm.retrieve
  with:
    key: "user_profile"                   # Required
    default: {}                           # Optional
  output: retrieved_value
```

**Returns:** `{"success": true, "value": any, "found": bool, "metadata": dict}`

#### `ltm.delete`

Delete key from persistent storage:

```yaml
- name: remove_data
  uses: ltm.delete
  with:
    key: "deprecated_key"                 # Required
  output: delete_result
```

**Returns:** `{"success": true, "deleted": bool, "key": str}`

#### `ltm.search`

Full-text search across stored values:

```yaml
- name: search_knowledge
  uses: ltm.search
  with:
    query: "coding preferences"           # Required
    limit: 10                             # Optional (default: 10)
    metadata_filter:                      # Optional
      type: "profile"
  output: search_results
```

**Returns:** `{"success": true, "results": [{"key": str, "value": any, "metadata": dict, "score": float}], "count": int}`

### Storage Actions

Remote storage operations via fsspec (S3, GCS, Azure, etc.).

#### `storage.list`

List files/objects at path:

```yaml
- name: list_files
  uses: storage.list
  with:
    path: s3://my-bucket/data/            # Required
    detail: true                          # Optional (include metadata)
    max_results: 100                      # Optional
  output: files_list
```

**Returns:** `{"files": list, "count": int, "success": true}`

#### `storage.exists`

Check if file/object exists:

```yaml
- name: check_file
  uses: storage.exists
  with:
    path: s3://my-bucket/data/file.json   # Required
  output: exists_result
```

**Returns:** `{"exists": bool, "path": str, "success": true}`

#### `storage.info`

Get file/object metadata:

```yaml
- name: get_info
  uses: storage.info
  with:
    path: s3://my-bucket/data/file.json   # Required
  output: file_info
```

**Returns:** `{"info": {"name": str, "size": int, "type": str, ...}, "success": true}`

#### `storage.copy`

Copy file between locations:

```yaml
- name: copy_to_gcs
  uses: storage.copy
  with:
    source: s3://source-bucket/file.json       # Required
    destination: gs://dest-bucket/file.json    # Required
  output: copy_result
```

**Returns:** `{"copied": true, "source": str, "destination": str, "success": true}`

#### `storage.delete`

Delete file/object:

```yaml
- name: cleanup
  uses: storage.delete
  with:
    path: s3://my-bucket/temp/file.json   # Required
    recursive: false                       # Optional (for directories)
  output: delete_result
```

**Returns:** `{"deleted": true, "path": str, "success": true}`

#### `storage.mkdir`

Create directory/prefix:

```yaml
- name: make_dir
  uses: storage.mkdir
  with:
    path: s3://my-bucket/new-folder/      # Required
    exist_ok: true                         # Optional
  output: mkdir_result
```

**Returns:** `{"created": true, "path": str, "success": true}`

#### `storage.native`

Access provider-specific operations:

```yaml
- name: set_acl
  uses: storage.native
  with:
    path: s3://my-bucket/file.json        # Required
    operation: put_object_acl             # Required
    ACL: public-read                      # Operation-specific params
  output: native_result
```

**Returns:** `{"result": any, "operation": str, "success": true}`

**Supported URI schemes:**
- Local: `./path`, `/abs/path`, `file:///path`
- AWS S3: `s3://bucket/path` (requires `s3fs`)
- GCS: `gs://bucket/path` (requires `gcsfs`)
- Azure: `az://container/path` (requires `adlfs`)
- Memory: `memory://path` (for testing)

### Graph Database Actions

Entity-relationship storage using CozoDB or Kuzu.

#### `graph.store_entity`

Store entity with properties:

```yaml
- name: store_user
  uses: graph.store_entity
  with:
    entity_id: "{{ state.user_id }}"      # Required
    entity_type: "User"                   # Required
    properties:                           # Optional
      name: "{{ state.user_name }}"
      role: "{{ state.user_role }}"
  output: entity_result
```

**Returns:** `{"success": true, "entity_id": str, "type": str, "created": bool}`

#### `graph.store_relation`

Create relationship between entities:

```yaml
- name: create_ownership
  uses: graph.store_relation
  with:
    from_entity: "{{ state.user_id }}"    # Required
    to_entity: "{{ state.project_id }}"   # Required
    relation_type: "owns"                 # Required
    properties:                           # Optional
      since: "{{ state.created_date }}"
  output: relation_result
```

**Returns:** `{"success": true, "from": str, "to": str, "type": str}`

#### `graph.query`

Execute graph queries:

```yaml
# Cypher query (Kuzu backend)
- name: find_projects
  uses: graph.query
  with:
    cypher: |
      MATCH (u:Entity {id: '{{ state.user_id }}'})
      -[r:owns]->(p:Entity)
      RETURN p.id, p.properties
  output: query_result

# Pattern query (works with both backends)
- name: find_users
  uses: graph.query
  with:
    pattern:
      entity_type: "User"
  output: pattern_result
```

**Returns:** `{"success": true, "results": list, "count": int, "query": str}`

#### `graph.retrieve_context`

Retrieve contextual information for entity:

```yaml
- name: get_context
  uses: graph.retrieve_context
  with:
    entity_id: "{{ state.user_id }}"      # Required
    hops: 2                               # Optional (default: 2)
    limit: 20                             # Optional
  output: context_result
```

**Returns:** `{"success": true, "entities": list, "relations": list, "context_summary": str}`

### Web Actions

Web scraping and search via external APIs (Firecrawl, Perplexity).

#### `web.scrape`

Scrape web content:

```yaml
- name: fetch_article
  uses: web.scrape
  with:
    url: "{{ state.target_url }}"         # Required
    formats: ["markdown", "links"]        # Optional
    only_main_content: true               # Optional
    timeout: 30000                        # Optional (ms)
  output: scraped_content
```

**Returns:** `{"success": true, "url": str, "markdown": str, "links": list, "metadata": dict}`

**Requires:** `FIRECRAWL_API_KEY` environment variable

#### `web.crawl`

Crawl multiple pages:

```yaml
- name: crawl_docs
  uses: web.crawl
  with:
    url: "https://docs.example.com"       # Required
    max_depth: 2                          # Optional (default: 2)
    limit: 20                             # Optional (default: 10)
    include_paths: ["/api/*"]             # Optional
    exclude_paths: ["/admin/*"]           # Optional
  output: crawled_pages
```

**Returns:** `{"success": true, "pages": list, "total_pages": int, "job_id": str}`

#### `web.search`

Web search via Perplexity:

```yaml
- name: search_topic
  uses: web.search
  with:
    query: "{{ state.topic }}"            # Required
    num_results: 10                       # Optional
  output: search_results
```

**Returns:** `{"success": true, "results": list, "query": str, "total_results": int, "answer": str}`

**Requires:** `PERPLEXITY_API_KEY` environment variable

### RAG Actions

Retrieval-Augmented Generation with embeddings and vector search.

#### `embedding.create`

Generate embeddings from text:

```yaml
# Single text
- name: embed_query
  uses: embedding.create
  with:
    text: "{{ state.query }}"             # Required
    model: text-embedding-3-small         # Optional
    provider: openai                      # Optional: "openai" or "ollama"
  output: embedding_result

# Batch embedding
- name: embed_documents
  uses: embedding.create
  with:
    text: "{{ state.documents }}"         # List of texts
  output: embeddings_result
```

**Returns:**
- Single: `{"embedding": list[float], "model": str, "dimensions": int}`
- Batch: `{"embeddings": list[list[float]], "model": str, "count": int, "dimensions": int}`

**Providers:**
- OpenAI: `text-embedding-3-small`, `text-embedding-3-large`, `text-embedding-ada-002`
- Ollama: `nomic-embed-text`, `mxbai-embed-large`, `all-minilm`, `bge-m3`

#### `vector.store`

Store documents with embeddings:

```yaml
- name: store_docs
  uses: vector.store
  with:
    texts:                                # Required
      - "First document content"
      - "Second document content"
    metadata:                             # Optional
      - type: article
      - type: blog
    collection: my_knowledge_base         # Optional
  output: store_result
```

**Returns:** `{"stored": int, "collection": str, "ids": list[str]}`

#### `vector.query`

Semantic similarity search:

```yaml
- name: search_knowledge
  uses: vector.query
  with:
    query: "{{ state.question }}"         # Required
    k: 5                                  # Optional (default: 5)
    collection: my_knowledge_base         # Optional
    filter:                               # Optional
      type: article
  output: search_results
```

**Returns:** `{"results": [{"id": str, "text": str, "score": float, "metadata": dict}], "query": str, "collection": str, "k": int}`

**Filter operators:** `field` (exact), `field_gte`, `field_lte`, `field_gt`, `field_lt`, `field_ne`, `field_in`

### Tools Bridge Actions

Access external tool ecosystems (CrewAI, MCP, LangChain).

#### `tools.crewai`

Execute CrewAI tools:

```yaml
- name: search_web
  uses: tools.crewai
  with:
    tool: SerperDevTool                   # Required
    query: "{{ state.search_query }}"     # Tool-specific params
    timeout: 30.0                         # Optional
  output: search_result
```

**Returns:** `{"result": any, "tool": str, "success": true}`

**Requires:** `pip install crewai crewai-tools`

#### `tools.mcp`

Execute MCP server tools:

```yaml
- name: read_file
  uses: tools.mcp
  with:
    server:                               # Required
      command: npx
      args: ["-y", "@anthropic/mcp-server-filesystem"]
    tool: read_file                       # Required
    path: "/tmp/data.txt"                 # Tool-specific params
  output: file_result
```

**Returns:** `{"result": any, "tool": str, "server": str, "success": true}`

**Requires:** `pip install mcp`

#### `tools.langchain`

Execute LangChain tools:

```yaml
- name: wiki_search
  uses: tools.langchain
  with:
    tool: WikipediaQueryRun               # Required
    query: "{{ state.query }}"            # Tool-specific params
  output: wiki_result
```

**Returns:** `{"result": any, "tool": str, "success": true}`

**Requires:** `pip install langchain langchain-community`

#### `tools.discover`

Discover available tools:

```yaml
- name: list_tools
  uses: tools.discover
  with:
    source: all                           # "crewai", "mcp", "langchain", or "all"
    filter: search                        # Optional
    use_cache: true                       # Optional (default: true)
  output: available_tools
```

**Returns:** `{"tools": list, "sources": list, "count": int, "success": true}`

#### `tools.clear_cache`

Clear discovery cache:

```yaml
- name: clear_cache
  uses: tools.clear_cache
  with:
    source: crewai                        # Optional (null = clear all)
  output: clear_result
```

**Returns:** `{"cleared": true, "source": str | null}`

### Custom Actions

Register custom actions in Python:

```python
def my_custom_action(state, param1, param2, **kwargs):
    # Your logic here
    result = do_something(param1, param2)
    return {"result": result}

engine = YAMLEngine(actions_registry={
    "custom.my_action": my_custom_action
})
```

Use in YAML:

```yaml
- name: custom_step
  uses: custom.my_action
  with:
    param1: value1
    param2: "{{ state.dynamic_value }}"
  output: custom_result
```

---

## Complete Examples

### Example 1: Research Agent with Conditional Routing

```yaml
name: research-agent
description: Search, validate, and summarize research results

variables:
  min_results: 3
  output_dir: ./reports

state_schema:
  query: str
  results: list
  has_enough: bool
  summary: str
  report_path: str

nodes:
  - name: search
    run: |
      query = state["query"]
      # Simulate search
      results = [
        {"title": f"Result {i}", "snippet": f"Info about {query}"}
        for i in range(5)
      ]
      return {"results": results}

  - name: validate
    run:
      type: expression
      value: len(state.get("results", [])) >= {{ variables.min_results }}
      output_key: has_enough

  - name: summarize
    run: |
      results = state["results"]
      summary = f"Found {len(results)} results for '{state['query']}':\n"
      for r in results:
        summary += f"- {r['title']}: {r['snippet']}\n"
      return {"summary": summary}

  - name: save_report
    uses: file.write
    with:
      path: "{{ variables.output_dir }}/{{ state.query }}.md"
      content: "# Research Report\n\n{{ state.summary }}"
    output: report_path

  - name: insufficient_results
    run: |
      return {
        "summary": f"Insufficient results for query: {state['query']}",
        "report_path": None
      }

edges:
  - from: __start__
    to: search

  - from: search
    to: validate

  - from: validate
    to: summarize
    condition:
      type: expression
      value: state["has_enough"]
    when: true

  - from: validate
    to: insufficient_results
    condition:
      type: expression
      value: state["has_enough"]
    when: false

  - from: summarize
    to: save_report

  - from: save_report
    to: __end__

  - from: insufficient_results
    to: __end__

config:
  raise_exceptions: true
```

### Example 2: Parallel Processing Pipeline

```yaml
name: parallel-analyzer
description: Analyze data from multiple sources in parallel

state_schema:
  input: str
  analysis_a: dict
  analysis_b: dict
  analysis_c: dict
  combined: dict

nodes:
  - name: prepare
    run: |
      return {"prepared_input": state["input"].strip()}

  - name: analyze_sentiment
    run: |
      text = state["prepared_input"]
      # Simulate sentiment analysis
      return {
        "analysis_a": {
          "type": "sentiment",
          "score": 0.75,
          "label": "positive"
        }
      }

  - name: analyze_entities
    run: |
      text = state["prepared_input"]
      # Simulate entity extraction
      return {
        "analysis_b": {
          "type": "entities",
          "entities": ["AI", "machine learning", "Python"]
        }
      }

  - name: analyze_topics
    run: |
      text = state["prepared_input"]
      # Simulate topic modeling
      return {
        "analysis_c": {
          "type": "topics",
          "topics": ["technology", "programming"]
        }
      }

  - name: combine_results
    fan_in: true
    run: |
      # Collect all analysis results from parallel flows
      combined = {}
      for result in parallel_results:
        for key in ["analysis_a", "analysis_b", "analysis_c"]:
          if key in result:
            combined[key] = result[key]

      return {"combined": combined}

  - name: generate_report
    run: |
      combined = state["combined"]
      report = {
        "input": state["prepared_input"],
        "analyses": combined,
        "summary": f"Completed {len(combined)} analyses"
      }
      return {"final_report": report}

edges:
  - from: __start__
    to: prepare

  # Parallel fan-out
  - from: prepare
    to: analyze_sentiment
    type: parallel
    fan_in: combine_results

  - from: prepare
    to: analyze_entities
    type: parallel
    fan_in: combine_results

  - from: prepare
    to: analyze_topics
    type: parallel
    fan_in: combine_results

  # Continue after fan-in
  - from: combine_results
    to: generate_report

  - from: generate_report
    to: __end__

config:
  raise_exceptions: true
```

### Example 3: Multi-Step Customer Support Agent

```yaml
name: customer-support-agent
description: Classify and route customer inquiries

variables:
  support_email: support@example.com

state_schema:
  customer_id: str
  message: str
  intent: str
  response: str
  ticket_id: str

nodes:
  - name: classify_intent
    run: |
      message = state["message"].lower()

      if "bill" in message or "charge" in message or "payment" in message:
        intent = "billing"
      elif "cancel" in message or "refund" in message:
        intent = "cancellation"
      elif "bug" in message or "error" in message or "broken" in message:
        intent = "technical"
      else:
        intent = "general"

      return {"intent": intent}

  - name: handle_billing
    steps:
      - name: lookup_account
        run: |
          # Simulate account lookup
          return {
            "account_status": "active",
            "last_payment": "2025-01-01"
          }

      - name: generate_response
        run: |
          return {
            "response": f"Hello! I see your account is {state['account_status']}. "
                       f"Your last payment was on {state['last_payment']}. "
                       "How can I help with your billing question?",
            "ticket_id": f"BILL-{state['customer_id']}"
          }

  - name: handle_cancellation
    run: |
      return {
        "response": "I understand you'd like to cancel. "
                   "Let me connect you with our retention team who can help.",
        "ticket_id": f"CANCEL-{state['customer_id']}"
      }

  - name: handle_technical
    run: |
      return {
        "response": "I'm sorry you're experiencing technical issues. "
                   "I've created a support ticket for our technical team.",
        "ticket_id": f"TECH-{state['customer_id']}"
      }

  - name: handle_general
    run: |
      return {
        "response": f"Thank you for contacting us! "
                   f"For assistance, please email {{ variables.support_email }}",
        "ticket_id": f"GEN-{state['customer_id']}"
      }

edges:
  - from: __start__
    to: classify_intent

  # Conditional routing based on intent
  - from: classify_intent
    to: handle_billing
    when: "state['intent'] == 'billing'"

  - from: classify_intent
    to: handle_cancellation
    when: "state['intent'] == 'cancellation'"

  - from: classify_intent
    to: handle_technical
    when: "state['intent'] == 'technical'"

  - from: classify_intent
    to: handle_general
    when: "state['intent'] == 'general'"

  # All handlers lead to end
  - from: handle_billing
    to: __end__

  - from: handle_cancellation
    to: __end__

  - from: handle_technical
    to: __end__

  - from: handle_general
    to: __end__

config:
  raise_exceptions: true
  interrupt_after: [classify_intent]  # Debug: pause after classification
```

---

## Execution

### Python API

```python
from the_edge_agent import YAMLEngine

# Load agent
engine = YAMLEngine()
graph = engine.load_from_file("agent.yaml")

# Execute with initial state
initial_state = {"query": "machine learning"}

# Stream execution (recommended)
for event in graph.stream(initial_state):
    if event["type"] == "state":
        print(f"Node: {event['node']}")
        print(f"State: {event['state']}")
    elif event["type"] == "final":
        print(f"Final result: {event['state']}")

# Or use invoke for final state only
result = list(graph.invoke(initial_state))
final_state = result[-1]["state"]
```

### With Custom Actions

```python
def custom_search(state, query, max_results=10, **kwargs):
    # Your search implementation
    return {"results": [...]}

engine = YAMLEngine(actions_registry={
    "search.web": custom_search
})
```

---

## Security Considerations

YAML agents execute arbitrary Python code via `exec()` and `eval()`. This means:

- **Trust model:** Only load YAML files from trusted sources
- **Attack surface:** YAML can access the file system, network, and all Python modules
- **Not sandboxed:** Unlike GitHub Actions, there is no VM isolation

See `docs/YAML_AGENTS.md` for detailed security guidance.
