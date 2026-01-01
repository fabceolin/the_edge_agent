# Node Specification

> **Parent document:** [YAML Reference](../YAML_REFERENCE.md)
> **Epic:** [DOC-002](../../stories/DOC-002-yaml-reference-modularization.md)

## Overview

Nodes are the fundamental building blocks of YAML agents. Each node represents a discrete step in the workflow that performs an action and returns state updates.

---

## Table of Contents

- [Basic Structure](#basic-structure)
- [Execution Methods](#execution-methods)
  - [Method 1: Inline Python Code](#method-1-inline-python-code-run)
  - [Method 2: Script](#method-2-script-script)
  - [Method 2b: Lua Code](#method-2b-lua-code)
  - [Method 2c: Prolog Code](#method-2c-prolog-code)
  - [Method 3: Built-in Actions](#method-3-built-in-actions-uses)
  - [Method 4: Expression](#method-4-expression)
  - [Method 5: Multi-Step](#method-5-multi-step-steps)
  - [Method 6: While-Loop](#method-6-while-loop)
  - [Method 7: Dynamic Parallel Fan-Out](#method-7-dynamic-parallel-fan-out)
- [Fan-In Nodes](#fan-in-nodes)

---

## Basic Structure

```yaml
nodes:
  - name: string          # Required: unique node identifier
    # One of the following execution methods:
    run: string           # Inline Python code
    script: string        # Alias for run (GitLab CI style)
    uses: string          # Built-in or custom action
    steps: array          # Multi-step execution

    # Navigation (optional, replaces edges section):
    goto: string | array  # Next node: string for unconditional, array for conditional

    # Additional options:
    fan_in: boolean       # Mark as fan-in node for parallel flows
    with: object          # Parameters for 'uses' actions
    output: string        # Key name for action result
```

---

## Execution Methods

### Method 1: Inline Python Code (`run:`)

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
- `variables` - Global variables from YAML `variables:` section
- `secrets` - Secrets from configured backend (see [Secrets Actions](./actions/specialized.md#secrets-actions))
- `json` - Python json module
- `requests` - Auto-imported if referenced
- `datetime` - Auto-imported if referenced
- `OpenAI` - Auto-imported if referenced

---

### Method 2: Script (`script:`)

Alias for `run:`, inspired by GitLab CI:

```yaml
- name: process_data
  script: |
    result = state["value"] * 2
    return {"doubled": result}
```

---

### Method 2b: Lua Code

Execute Lua code instead of Python for cross-runtime compatibility with the Rust implementation.

```yaml
- name: process_lua
  run: |
    -- lua
    local result = {}
    result.value = state.value * 2
    result.message = state.name .. "!"
    return result
```

> **Complete Lua documentation:** [Advanced Runtimes - Lua](./advanced-runtimes.md#lua-runtime)
> Includes sandbox details, cross-runtime compatibility, and portable syntax guide.

---

### Method 2c: Prolog Code

Execute Prolog code for neurosymbolic AI workflows combining neural network outputs with symbolic reasoning.

```yaml
- name: validate
  language: prolog
  run: |
    state(value, V),
    V2 is V * 2,
    return(result, V2).
```

> **Complete Prolog documentation:** [Advanced Runtimes - Prolog](./advanced-runtimes.md#prolog-runtime)
> Includes CLP(FD), module pre-loading, and runtime comparison.

---

### Method 3: Built-in Actions (`uses:`)

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
- `uses:` - Action name (see [Built-in Actions](./actions/README.md))
- `with:` - Action parameters (template-processed)
- `output:` - State key for result (optional)

---

### Method 4: Expression

For simple evaluations:

```yaml
- name: check_count
  run:
    type: expression
    value: len(state.get("items", [])) > 0
    output_key: has_items
```

---

### Method 5: Multi-Step (`steps:`)

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
        return {"final": f"Submitted: {state['intermediate']}"}
```

---

### Method 6: While-Loop

Execute a loop body repeatedly until a condition becomes false or max iterations is reached:

```yaml
- name: refine_until_valid
  type: while_loop
  condition: "not state.get('is_valid', False)"  # Jinja2/Tera expression
  max_iterations: 10                              # Required: 1-1000
  body:
    - name: generate
      uses: llm.call
      with:
        model: gpt-4o
        messages:
          - role: user
            content: "Generate valid JSON for: {{ state.prompt }}"
      output: llm_response

    - name: validate
      run: |
        import json
        try:
            parsed = json.loads(state.get('llm_response', {}).get('content', '{}'))
            return {"parsed_result": parsed, "is_valid": True}
        except:
            return {"is_valid": False}
```

**Required fields:**
- `condition`: Jinja2 (Python) or Tera (Rust) expression evaluated before each iteration
- `max_iterations`: Safety guard (integer 1-1000) to prevent infinite loops
- `body`: List of nodes to execute sequentially on each iteration

**Behavior:**
1. Evaluate `condition` before each iteration
2. If `true`, execute all body nodes sequentially
3. State from each iteration passes to the next
4. Loop exits when condition is `false` or `max_iterations` reached
5. Final state passes to downstream nodes

**Safety guards:**
- `max_iterations` is **required** — YAML parsing fails if missing
- Range must be 1-1000 (validation error otherwise)
- Nested while-loops are **NOT supported** (validation error if attempted)
- Body execution errors propagate immediately (no automatic retry)

**Events emitted:**

| Event | Payload |
|-------|---------|
| `LoopStart` | `{node_name, max_iterations}` |
| `LoopIteration` | `{node_name, iteration, condition_result}` |
| `LoopEnd` | `{node_name, iterations_completed, exit_reason}` |

`exit_reason` is either `"condition_false"` or `"max_iterations_reached"`.

**Use cases:**
- LLM refinement until output passes validation
- Data extraction with retry until all fields populated
- Research agents that continue until sufficient sources found

**Cross-runtime parity:**
The while-loop syntax works identically in both Python and Rust TEA implementations. The same YAML file produces identical results in both runtimes.

**Simple counter example:**

```yaml
name: counter-demo
nodes:
  - name: count_loop
    type: while_loop
    condition: "state.count < 5"
    max_iterations: 10
    body:
      - name: increment
        run: |
          -- lua
          local count = state.count or 0
          local sum = state.sum or 0
          return { count = count + 1, sum = sum + count + 1 }

edges:
  - from: __start__
    to: count_loop
  - from: count_loop
    to: __end__
```

**Result** with initial state `{count: 0, sum: 0}`:
- Loop runs 5 iterations (count goes 0→1→2→3→4→5)
- Final state: `{count: 5, sum: 15}`
- Exit reason: `condition_false` (count is no longer < 5)

---

### Method 7: Dynamic Parallel Fan-Out

Execute branches in parallel over a runtime-evaluated collection, with built-in fan-in:

```yaml
- name: process_items
  type: dynamic_parallel
  items: "{{ state.urls }}"           # Jinja2 expression → list at runtime
  item_var: url                        # Variable name for each item (default: "item")
  index_var: idx                       # Variable name for index (default: "index")
  max_concurrency: 5                   # Optional: throttle parallel execution
  fail_fast: true                      # Optional: cancel remaining on first failure
  action:                              # Option A: single action per item
    uses: http.get
    with:
      url: "{{ url }}"
    output: response
  output: all_responses                # Results collected here
```

**Three execution modes (mutually exclusive):**

1. **Action mode** (`action:`): Execute a single action per item
2. **Steps mode** (`steps:`): Execute sequential steps per item
3. **Subgraph mode** (`subgraph:`): Load and execute an external YAML file per item

**Required fields:**
- `items`: Jinja2 expression that evaluates to an iterable at runtime
- One of: `action:`, `steps:`, or `subgraph:`

**Optional fields:**
- `item_var`: Name of the variable holding each item (default: `"item"`)
- `index_var`: Name of the variable holding the 0-based index (default: `"index"`)
- `max_concurrency`: Maximum parallel branches (default: unlimited)
- `fail_fast`: Stop remaining branches on first error (default: `false`)
- `output`: State key to store collected results (default: `"parallel_results"`)

**Action mode example:**

```yaml
- name: fetch_all_urls
  type: dynamic_parallel
  items: "{{ state.urls }}"
  item_var: url
  max_concurrency: 10
  action:
    uses: http.get
    with:
      url: "{{ url }}"
    output: response
  output: responses
```

**Steps mode example:**

```yaml
- name: process_documents
  type: dynamic_parallel
  items: "{{ state.documents }}"
  item_var: doc
  index_var: i
  steps:
    - name: extract
      uses: llm.call
      with:
        model: gpt-4o
        messages:
          - role: user
            content: "Extract key points from: {{ doc.content }}"
      output: extraction

    - name: summarize
      uses: llm.call
      with:
        model: gpt-4o
        messages:
          - role: user
            content: "Summarize: {{ state.extraction.content }}"
      output: summary
  output: processed_docs
```

**Subgraph mode example:**

```yaml
- name: run_analysis_per_item
  type: dynamic_parallel
  items: "{{ state.data_sources }}"
  item_var: source
  max_concurrency: 3
  fail_fast: true
  subgraph: "./analysis-workflow.yaml"  # Supports local, s3://, gs://, az://, http://
  input:
    data_source: "{{ source }}"
    config: "{{ state.analysis_config }}"
  output: analysis_results
```

**Behavior:**
1. Evaluate `items` expression to get the collection
2. For each item, spawn a parallel branch with `item_var` and `index_var` injected
3. Execute branches concurrently (throttled by `max_concurrency` if set)
4. Collect results from all branches into `output` as a list of `ParallelFlowResult`
5. If `fail_fast: true`, cancel remaining branches on first failure

**Result format:**

Each result in the output list is a `ParallelFlowResult` with:
- `state`: The final state from that branch
- `source_node`: The dynamic_parallel node name
- `index`: The branch index (0-based)

**Safety guards:**
- `items` must evaluate to a list/iterable (runtime error otherwise)
- Empty `items` list results in empty output (no branches executed)
- `max_concurrency` must be positive integer if specified
- Branch errors are captured in results unless `fail_fast` cancels execution

**Events emitted:**

| Event | Payload |
|-------|---------|
| `DynamicParallelStart` | `{node_name, item_count, max_concurrency}` |
| `DynamicParallelBranchStart` | `{node_name, index, item}` |
| `DynamicParallelBranchEnd` | `{node_name, index, success, error?}` |
| `DynamicParallelEnd` | `{node_name, total_branches, successful, failed}` |

**Use cases:**
- Process a batch of URLs, files, or API endpoints in parallel
- Run the same analysis workflow on multiple data sources
- Fan-out LLM calls across a list of prompts with rate limiting
- Parallel document processing pipelines with controlled concurrency

**Comparison with static parallel edges:**

| Feature | Static Parallel (edges) | Dynamic Parallel |
|---------|------------------------|------------------|
| Branch count | Fixed at YAML parse time | Determined at runtime |
| Fan-in | Explicit fan-in node | Built-in, automatic |
| Item iteration | Manual | Automatic with item_var/index_var |
| Concurrency control | None | max_concurrency + fail_fast |
| Subgraph loading | Not supported | Supported with fsspec |

---

## Fan-In Nodes

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

Fan-in nodes are used with parallel edges to collect and combine results from multiple concurrent execution paths.

---

## See Also

- [Navigation & Flow](./navigation.md) - Routing between nodes
- [Template Syntax](./templates.md) - Variable interpolation
- [Advanced Runtimes](./advanced-runtimes.md) - Lua and Prolog details
- [Actions Overview](./actions/README.md) - Built-in action reference
