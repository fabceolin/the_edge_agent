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

#### `file.read`

```yaml
- name: load
  uses: file.read
  with:
    path: ./data/input.txt               # Required
  output: file_content
```

**Returns:** `{"content": "file contents..."}`

#### `file.write`

```yaml
- name: save
  uses: file.write
  with:
    path: "./output/{{ state.filename }}.txt"  # Required
    content: "{{ state.data }}"                 # Required
```

**Returns:** `{"path": "resolved/path/to/file"}`

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
