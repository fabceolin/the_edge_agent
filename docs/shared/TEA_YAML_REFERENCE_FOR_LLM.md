# TEA YAML Agent Reference (LLM-Optimized)

Declarative agent configuration for The Edge Agent. Nodes execute sequentially by default.

## Basic Structure

```yaml
name: agent-name
description: What this agent does

variables:          # Global constants
  api_url: https://api.example.com

state_schema:       # Expected state fields
  query: str
  result: str

nodes:              # Workflow steps (execute in order)
  - name: step1
    run: |
      return {"result": state["query"].upper()}
```

## Node Types

```yaml
# INLINE PYTHON - Full code access
- name: process
  run: |
    data = state["input"]
    return {"output": data.strip(), "count": len(data)}

# BUILT-IN ACTION - Pre-built operations
- name: call_llm
  uses: llm.call
  with:
    model: gpt-4
    prompt: "Summarize: {{ state.text }}"
    temperature: 0.7
  output: summary  # result -> state["summary"]

# MULTI-STEP - Sequential steps in one node
- name: pipeline
  steps:
    - uses: http.get
      with:
        url: "{{ variables.api_url }}/data"
      output: raw_data
    - run: |
        return {"processed": state["raw_data"]["items"]}
```

## Navigation & Flow Control

```yaml
# UNCONDITIONAL JUMP
- name: start
  run: |
    return {"ready": True}
  goto: process_data  # jump to specific node

# CONDITIONAL BRANCHING
- name: classify
  run: |
    category = "billing" if "invoice" in state["query"] else "support"
    return {"category": category}
  goto:
    - if: "state['category'] == 'billing'"
      to: handle_billing
    - if: "state['category'] == 'support'"
      to: handle_support
    - to: handle_default  # fallback (no condition)

# LOOP (goto earlier node)
- name: retry_loop
  run: |
    attempts = state.get("attempts", 0) + 1
    return {"attempts": attempts, "success": attempts >= 3}
  goto:
    - if: "state['success']"
      to: finish
    - to: retry_loop  # loop back
```

## Template Syntax (Jinja2)

```yaml
# Variable interpolation
prompt: "User asked: {{ state.query }}"
url: "{{ variables.api_url }}/users/{{ state.user_id }}"
auth: "Bearer {{ secrets.api_key }}"

# Filters
data: "{{ state.items | tojson }}"
name: "{{ state.name | upper }}"
fallback: "{{ state.value | default('none') }}"

# Conditionals in templates
prompt: |
  {% if state.context %}
  Context: {{ state.context }}
  {% endif %}
  Question: {{ state.query }}
```

## Parallel Execution

```yaml
nodes:
  - name: prepare
    run: |
      return {"items": ["a", "b", "c"]}

  - name: process_a
    run: |
      return {"result_a": "processed"}

  - name: process_b
    run: |
      return {"result_b": "processed"}

  - name: combine
    fan_in: true  # receives parallel_results list
    run: |
      results = [r for r in parallel_results]
      return {"combined": results}

edges:
  - from: prepare
    to: [process_a, process_b]
    parallel: true
    fan_in: combine
```

## Common Actions

```yaml
# LLM CALL
- uses: llm.call
  with:
    model: gpt-4              # or: claude-3-opus, ollama/llama3
    prompt: "{{ state.query }}"
    system: "You are helpful."
    temperature: 0.7
    max_tokens: 1000
  output: response

# HTTP REQUESTS
- uses: http.get
  with:
    url: "https://api.example.com/data"
    headers:
      Authorization: "Bearer {{ secrets.token }}"
  output: response

- uses: http.post
  with:
    url: "{{ variables.api_url }}"
    body: "{{ state.payload | tojson }}"
    headers:
      Content-Type: application/json
  output: result

# FILE OPERATIONS
- uses: file.read
  with:
    path: "./data/input.json"
  output: file_content

- uses: file.write
  with:
    path: "./output/result.json"
    content: "{{ state.result | tojson }}"

# DATA PROCESSING
- uses: json.parse
  with:
    text: "{{ state.raw_json }}"
  output: parsed

- uses: data.filter
  with:
    items: "{{ state.items }}"
    condition: "item['status'] == 'active'"
  output: filtered
```

## Input Validation

```yaml
input_schema:
  query:
    type: str
    required: true
    min_length: 1
    max_length: 1000
  max_results:
    type: int
    default: 10
    min: 1
    max: 100
  format:
    type: str
    choices: ["json", "text", "markdown"]
```

## HTTP Endpoint (API exposure)

```yaml
endpoint:
  path: "/api/v1/search"
  method: POST
  summary: "Search endpoint"
  auth:
    required: true
  query_params:
    limit:
      type: int
      default: 10
```

## Variable Scopes

| Scope | Access | Description |
|-------|--------|-------------|
| `state["key"]` | `{{ state.key }}` | Runtime data between nodes |
| `variables["key"]` | `{{ variables.key }}` | YAML-defined constants |
| `secrets["key"]` | `{{ secrets.key }}` | Sensitive values (env/config) |

## Special Nodes

- `__start__` - Entry point (implicit)
- `__end__` - Terminal node (implicit after last node, or explicit via `goto: __end__`)

## Minimal Complete Example

```yaml
name: research-agent
description: Search and summarize information

variables:
  model: gpt-4

input_schema:
  query:
    type: str
    required: true

nodes:
  - name: search
    uses: http.get
    with:
      url: "https://api.search.com/q={{ state.query }}"
    output: results

  - name: summarize
    uses: llm.call
    with:
      model: "{{ variables.model }}"
      prompt: |
        Summarize these search results for: {{ state.query }}

        Results: {{ state.results | tojson }}
    output: summary

  - name: format
    run: |
      return {
          "answer": state["summary"],
          "source_count": len(state["results"].get("items", []))
      }
```
