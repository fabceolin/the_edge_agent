# YAML Agent Reference

Version: 0.8.10

Complete reference for declarative agent configuration in The Edge Agent using YAML files.

## Table of Contents

- [Overview](#overview)
- [Security Notice](#security-notice)
- [Basic Structure](#basic-structure)
- [State and Variable Passing](#state-and-variable-passing)
- [Document Structure](#document-structure)
- [Top-Level Keys](#top-level-keys)
  - [imports](#imports)
- [Node Specification](#node-specification)
- [Navigation and Flow Control](#navigation-and-flow-control)
  - [Implicit Chaining (Default)](#implicit-chaining-default)
  - [The `goto` Property](#the-goto-property)
  - [Conditional `goto`](#conditional-goto)
  - [Navigation Precedence](#navigation-precedence)
- [Edge Specification](#edge-specification-deprecated) (sequential edges deprecated, parallel edges supported)
- [Template Syntax](#template-syntax)
- [Built-in Actions](#built-in-actions)
  - [LLM Actions](#llm-actions)
  - [HTTP Actions](#http-actions)
  - [File Actions](#file-actions)
  - [Storage Actions](#storage-actions)
  - [Data Processing Actions](#data-processing-actions)
  - [Code Execution Actions](#code-execution-actions)
  - [Observability Actions](#observability-actions)
    - [Opik Integration](#opik-integration)
  - [Memory Actions](#memory-actions)
  - [Long-Term Memory Actions](#long-term-memory-actions)
  - [Cache and Memoization Actions](#cache-and-memoization-actions)
  - [Firebase Agent Memory Actions](#firebase-agent-memory-actions)
  - [Tabular Data Actions](#tabular-data-actions)
  - [Graph Database Actions](#graph-database-actions)
  - [Web Actions](#web-actions)
  - [RAG Actions](#rag-actions)
  - [Tools Bridge Actions](#tools-bridge-actions)
  - [Notification Actions](#notification-actions)
  - [Checkpoint Actions](#checkpoint-actions)
  - [Schema Actions](#schema-actions)
  - [LlamaExtract Actions](#llamaextract-actions)
  - [Validation Actions](#validation-actions)
  - [Custom Actions](#custom-actions)
- [Extraction Validation](#extraction-validation)
  - [Extraction Schema](#extraction-schema)
  - [Validation Constraints](#validation-constraints)
  - [Semantic Probes](#semantic-probes)
  - [Validation Logging](#validation-logging)
- [Checkpoint Persistence](#checkpoint-persistence)
- [Complete Examples](#complete-examples)
- [Python API](#python-api)
- [Best Practices](#best-practices)
- [Troubleshooting](#troubleshooting)
- [Comparison with GitHub Actions](#comparison-with-github-actions)

---

## Overview

The Edge Agent supports declarative agent configuration using YAML files, inspired by GitHub Actions and GitLab CI/CD pipelines. Instead of writing Python code to construct your StateGraph, you can define the entire workflow in a YAML file.

YAML agents compile to `StateGraph` instances with this mapping:

| YAML Concept | Python Equivalent |
|--------------|-------------------|
| `nodes:` | `graph.add_node()` |
| `edges:` | `graph.add_edge()`, `add_conditional_edges()`, `add_parallel_edge()` |
| `state_schema:` | `StateGraph(state_schema={...})` |
| `config:` | `graph.compile(...)` |

Benefits:
- **Declarative**: Define what you want, not how to build it
- **Portable**: Configuration can be version-controlled and shared
- **Accessible**: Non-programmers can create and modify agents
- **Inspectable**: Easy to understand workflow at a glance

---

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

**Lua sandbox (when `lua_enabled=True`):**
Lua code blocks are sandboxed with dangerous globals removed (`os`, `io`, `debug`, `loadfile`, `dofile`). However, this is not a complete security boundary—the Python host process still has full access.

**Prolog sandbox (when `prolog_enabled=True`):**
Prolog code blocks use SWI-Prolog's sandbox library with dangerous predicates restricted (file I/O, shell execution, network access). Timeouts prevent runaway queries. This is not a complete security boundary—the Python host process still has full access.

---

## Basic Structure

```yaml
name: my-agent
description: What this agent does

# Global variables accessible throughout the workflow
variables:
  max_retries: 3
  api_endpoint: https://api.example.com

# External action modules (optional)
imports:
  - path: ./actions/custom.py
    namespace: custom

# Define the state schema
state_schema:
  input: str
  result: str
  count: int

# Define nodes (workflow steps) - implicit flow: step1 -> step2 -> __end__
nodes:
  - name: step1
    run: |
      return {"result": "processed"}

  - name: step2
    run: |
      return {"final": state["result"]}
    # goto: __end__  # Optional: explicit termination

# Configuration
config:
  raise_exceptions: true
  interrupt_before: []
  interrupt_after: []
```

> **Note:** The `edges` section is deprecated. Use implicit chaining (node order) and `goto` properties instead. See [Navigation and Flow Control](#navigation-and-flow-control).

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

---

## Document Structure

```yaml
# Metadata (optional)
name: string                    # Agent identifier
description: string             # Human-readable description

# External Imports (optional)
imports:
  - path: string                # Local file path
    namespace: string           # Action namespace prefix
  - package: string             # Installed Python package
    namespace: string

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
    goto: string | array  # Optional: navigation control

# Edge Definitions (deprecated - use goto on nodes instead)
edges:  # Optional - legacy syntax
  - from: string
    to: string
    # ... edge configuration

# Compilation Options (optional)
config:
  raise_exceptions: boolean
  interrupt_before: [string]
  interrupt_after: [string]
  checkpoint_dir: string
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

### `imports`

External action modules to load. Enables modular action organization and code reuse.

```yaml
imports:
  # Local file (relative to YAML file)
  - path: ./actions/my_custom.py
    namespace: custom

  # Installed Python package
  - package: tea_actions_slack
    namespace: slack
```

Each import requires:
- `path` OR `package`: Source of the action module
- `namespace`: Prefix for all actions from this module

**Local File Import** (`path:`):
- Relative paths resolve from YAML file location
- Absolute paths used as-is
- Module must define `register_actions(registry, engine)` function

**Package Import** (`package:`):
- Uses `importlib.import_module()` to load installed packages
- Supports dotted package names (e.g., `tea_actions.slack`)
- Package must define `register_actions(registry, engine)` function

**Action Registration Contract**:

```python
# my_actions.py
from typing import Any, Callable, Dict

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """Register actions into the provided registry."""
    def my_action(state, param1, param2=None, **kwargs):
        return {"result": "value", "success": True}

    registry['my_action'] = my_action

# Optional metadata for discovery
__tea_actions__ = {
    "version": "1.0.0",
    "description": "My custom actions",
    "actions": ["my_action"],
}
```

**Usage in Nodes**:
```yaml
nodes:
  - name: process
    uses: custom.my_action  # namespace.action_name
    with:
      param1: "{{ state.input }}"
```

**Features**:
- Namespace prefixing prevents action name collisions
- Circular import detection (same module loaded once, skipped on duplicates)
- Clear error messages include file path or package name

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

### `edges` (deprecated)

> ⚠️ **Deprecation Notice (v0.8.x):** Sequential edges are deprecated in favor of implicit chaining and `goto` properties on nodes. See [Navigation and Flow Control](#navigation-and-flow-control) for the new syntax.
>
> **Exception:** Parallel edges (`parallel: true`, `fan_in:`) are **not deprecated** and remain the only way to define fan-out/fan-in patterns.

Array of edge definitions. See [Edge Specification](#edge-specification-deprecated).

### `config` (optional)
```yaml
config:
  raise_exceptions: true        # Raise errors vs yield error events (default: false)
  interrupt_before: [node_name] # Pause before these nodes
  interrupt_after: [node_name]  # Pause after these nodes
  checkpoint_dir: ./checkpoints # Directory for auto-save checkpoints
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

    # Navigation (optional, replaces edges section):
    goto: string | array  # Next node: string for unconditional, array for conditional

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

#### Method 2b: Lua Code (`run:` with `-- lua` marker)

Execute Lua code instead of Python for cross-runtime compatibility with the Rust implementation.

**Enabling Lua:**
```python
engine = YAMLEngine(lua_enabled=True)
```

**Explicit marker (recommended):**
```yaml
- name: process_lua
  run: |
    -- lua
    local result = {}
    result.value = state.value * 2
    result.message = state.name .. "!"
    return result
```

**Auto-detection (heuristic):**
Lua code is auto-detected when it contains Lua-specific syntax:
- `local` keyword (variable declaration)
- `then` / `end` keywords (control flow)
- `elseif` keyword (Lua uses elseif, Python uses elif)
- `..` operator (string concatenation)

```yaml
- name: auto_detected_lua
  run: |
    local count = state.count + 1
    local doubled = state.count * 2
    return {count = count, doubled = doubled}
```

**Lua Sandbox:**
For security, the following Lua globals are removed:
- `os` - Operating system access
- `io` - File I/O operations
- `debug` - Debugging facilities
- `loadfile`, `dofile` - File loading

Safe libraries remain available: `string`, `math`, `table`, `pairs`, `ipairs`, `type`, `tostring`, `tonumber`.

**Timeout Protection:**
Lua code execution has a configurable timeout (default: 30 seconds):
```python
engine = YAMLEngine(lua_enabled=True, lua_timeout=10.0)  # 10 second timeout
```

**Installation:**
```bash
pip install 'the_edge_agent[lua]'
# or
pip install lupa>=2.0
```

**Cross-Runtime Compatibility:**
Lua code in YAML agents runs identically in both Python and Rust TEA implementations, enabling portable agents.

**Lua Version Compatibility (LuaJIT 2.1 vs Lua 5.4):**

The Python implementation uses **LuaJIT 2.1** (via `lupa`) while the Rust implementation uses **Lua 5.4** (via `mlua`). For cross-runtime compatibility, use the **portable subset** of Lua syntax.

| Feature | LuaJIT 2.1 (Python) | Lua 5.4 (Rust) | Portable Alternative |
|---------|---------------------|----------------|---------------------|
| Integer division | `math.floor(a/b)` | `a // b` | Use `math.floor(a/b)` |
| Bitwise ops | `bit.band()`, `bit.bor()` | `&`, `\|`, `~` | Avoid bitwise; use math |
| Const variables | ❌ Not supported | `local x <const>` | Use `local x = ...` |
| Close variables | ❌ Not supported | `local f <close>` | Avoid `<close>` |
| UTF-8 library | ❌ Not built-in | `utf8.*` | Use `string.len` for ASCII |
| Warning system | ❌ Not available | `warn()` | Avoid `warn()` |

**Portable Syntax Examples (works in both):**
```lua
-- State access
local count = state.count + 1

-- Conditionals (ternary style)
local result = state.value > 5 and "high" or "low"

-- Table creation
return { count = count, status = "done" }

-- Loops
for i, v in ipairs(state.items) do
    -- process
end

-- String and math operations
local upper = string.upper(state.name)
local avg = math.floor(total / count)
```

**Syntax to Avoid (Lua 5.4 only - will fail in Python):**
```lua
-- Integer division operator (Lua 5.4 only)
local quotient = 17 // 5  -- Use: math.floor(17/5)

-- Bitwise operators (Lua 5.4 only)
local flags = a & b | c  -- Use: bit.band/bit.bor in LuaJIT

-- Const/close attributes (Lua 5.4 only)
local x <const> = 10  -- Use: local x = 10
```

#### Method 2c: Prolog Code (`run:` with `% prolog` marker or `language: prolog`)

Execute Prolog code for neurosymbolic AI workflows combining neural network outputs with symbolic reasoning.

**Enabling Prolog:**

*Python:*
```python
engine = YAMLEngine(prolog_enabled=True)
```

*Rust:*
```bash
# Build with Prolog feature
cargo build --features prolog

# Run agent with Prolog feature
cargo run --features prolog -- run my-agent.yaml

# Prolog requires SWI-Prolog 9.1+ system installation:
# Ubuntu/Debian: sudo apt install swi-prolog swi-prolog-nox
# macOS: brew install swi-prolog
# Windows: choco install swi-prolog
```

**Important:** In Rust TEA, Prolog is an optional feature requiring:
1. The `--features prolog` flag during build/run
2. SWI-Prolog 9.1+ installed on the system

If Prolog feature is not enabled, nodes with `language: prolog` will fail with a clear error message explaining how to enable it.

**Module Pre-Loading:**
Both Python and Rust runtimes automatically pre-load common modules at initialization:
- `lists` - List manipulation predicates (`member/2`, `append/3`, `reverse/2`, etc.)
- `clpfd` - Finite domain constraints (no `:- use_module` needed!)
- `apply` - Higher-order predicates (`maplist/2`, `include/3`, etc.)
- `aggregate` - Aggregation predicates (`aggregate_all/3`, etc.)

This means CLP(FD) constraints and list predicates work immediately in **both Python and Rust** without explicit module imports. YAML agents using these predicates are fully portable across runtimes.

**Explicit marker (recommended):**
```yaml
- name: process_prolog
  run: |
    % prolog
    state(value, V),
    V2 is V * 2,
    return(result, V2).
```

**Language attribute (explicit):**
```yaml
- name: process_prolog
  language: prolog
  run: |
    state(value, V),
    V2 is V + 10,
    return(result, V2).
```

**Explicit type in run config:**
```yaml
- name: compute
  run:
    type: prolog
    code: |
      state(value, V),
      V2 is V * 3,
      return(result, V2).
```

**Auto-detection (heuristic):**
Prolog code is auto-detected when it contains Prolog-specific syntax:
- `:-` rule operator or directive
- `state(` or `return(` predicates (TEA convention)
- `assertz`, `findall`, `forall`, `aggregate_all` predicates
- CLP(FD) operators (`#=`, `#<`, `#>`, `in`)
- `use_module` directive

```yaml
- name: auto_detected_prolog
  run: |
    state(count, C),
    C2 is C + 1,
    return(count, C2).
```

**State Interface:**

Access state via `state/2` predicate and set return values via `return/2`:

```prolog
% Read state["value"] into V
state(value, V),

% Compute result
Result is V * 2,

% Set state["doubled"] = Result
return(doubled, Result).
```

**⚠️ Rust Runtime Limitation:** The `return/2` predicate is recognized by auto-detection but **does not currently update the state** in the Rust runtime due to swipl-rs crate constraints. Workarounds for Rust:
1. Use Lua nodes for state manipulation
2. Use CLP(FD) constraints where labeled values can be extracted
3. Use Prolog purely for validation/logic checks (success/failure)

The Python runtime (janus-swi) fully supports `return/2` for state updates.

**CLP(FD) Constraint Solving:**

Prolog integration includes support for CLP(FD) finite domain constraints. The `clpfd` module is pre-loaded automatically—no explicit import needed:

```yaml
- name: solve_constraints
  language: prolog
  run: |
    % No :- use_module(library(clpfd)) needed - it's pre-loaded!
    X in 1..10,
    Y in 1..10,
    X + Y #= 15,
    X #< Y,
    label([X, Y]),
    return(x, X),
    return(y, Y).
```

**Prolog Sandbox:**
For security, the sandbox restricts:
- File I/O operations (`open/3`, `read/1`, `write/1`)
- Shell execution (`shell/1`, `process_create/3`)
- Network access

Safe predicates remain available: arithmetic, list operations, `findall`, `aggregate_all`, CLP(FD), etc.

**Timeout Protection:**
Prolog code execution has a configurable timeout (default: 30 seconds):
```python
engine = YAMLEngine(prolog_enabled=True, prolog_timeout=10.0)  # 10 second timeout
```

**Installation:**

*Python Runtime:*
```bash
# Install janus-swi Python binding (requires SWI-Prolog 9.1+)
pip install 'the_edge_agent[prolog]'
# or directly
pip install janus-swi

# Install SWI-Prolog 9.1+ system dependency
# Ubuntu/Debian (PPA recommended for 9.1+):
sudo apt-add-repository ppa:swi-prolog/stable
sudo apt update
sudo apt install swi-prolog

# macOS:
brew install swi-prolog

# Verify version (must be 9.1+):
swipl --version

# Windows: Download from https://www.swi-prolog.org/download/stable
```

*Rust Runtime:*
```bash
# Build with Prolog feature
cargo build --features prolog

# SWI-Prolog 9.1+ must be installed system-wide:
# Ubuntu/Debian:
sudo apt-add-repository ppa:swi-prolog/stable
sudo apt update
sudo apt install swi-prolog swi-prolog-nox

# macOS:
brew install swi-prolog

# Fedora:
sudo dnf install pl

# Windows: Download from https://www.swi-prolog.org/download/stable
# or: choco install swi-prolog
```

**Cross-Runtime Compatibility:**

Prolog code in YAML agents runs in both Python and Rust TEA implementations (using janus-swi and swipl-rs respectively).

**Runtime Comparison Table:**

| Feature | Python (janus-swi) | Rust (swipl-rs) |
|---------|-------------------|-----------------|
| **Bindings** | janus-swi (official) | swipl-rs (community) |
| **SWI-Prolog Version** | 9.1+ required | 9.1+ recommended |
| **`return/2` support** | ✅ Full support | ⚠️ Limited (doesn't update state) |
| **Module pre-loading** | ✅ Auto (clpfd, lists, apply, aggregate) | ✅ Auto (clpfd, lists, apply, aggregate) |
| **`state/2` support** | ✅ Full support | ✅ Full support |
| **Sandbox** | ✅ Default enabled | ✅ Default enabled |
| **Timeout protection** | ✅ 30s default | ✅ 30s default |
| **Thread safety** | Thread-local predicates | RwLock + state caching |

**Best Practice for Portable Agents:**
- Use `state/2` for reading input values (works in both)
- Use Prolog for logic/validation that succeeds or fails
- For state updates: use Lua nodes or Python `run:` blocks
- CLP(FD) predicates work without explicit imports in both runtimes

**Migration from pyswip (TEA-PY-005):**
The Python runtime was migrated from pyswip to janus-swi for:
- Proper timeout exception handling (no segfaults)
- Native directive support via `consult()`
- Full CLP(FD) module loading
- Official SWI-Prolog 9.1+ bindings

**Neurosymbolic AI Use Cases:**

1. **Validate LLM outputs** with logical rules
2. **Constraint-based reasoning** for scheduling, planning
3. **Knowledge graph queries** with Datalog-style rules
4. **Formal verification** of neural network predictions
5. **Ontology reasoning** with OWL/RDF-style inference

**Example - LLM + Prolog Validation:**
```yaml
nodes:
  - name: generate
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: user
          content: "Generate a valid schedule for {{ state.constraints }}"
    output: llm_response

  - name: validate_with_prolog
    language: prolog
    run: |
      % Define scheduling constraints
      valid_schedule(Start, End) :-
        Start >= 9,   % No earlier than 9 AM
        End =< 17,    % No later than 5 PM
        End > Start.  % End after start

      % Parse and validate LLM output
      state(llm_response, Response),
      % Extract times from response (simplified)
      Start = 10, End = 14,
      (valid_schedule(Start, End)
        -> return(valid, true), return(schedule, [Start, End])
        ; return(valid, false), return(error, 'Invalid schedule')).
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
        return {"final": f"Submitted: {state['intermediate']}"}
```

#### Method 6: While-Loop (`type: while_loop`)

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

## Navigation and Flow Control

TEA supports two navigation approaches: the modern **implicit/goto** syntax (recommended) and the legacy **edges** section (deprecated). This section covers the modern approach.

### Implicit Chaining (Default)

By default, nodes execute in the order they are defined. After a node completes:
1. If the node has a `goto` property, use it
2. Otherwise, proceed to the next node in the list
3. If it's the last node, the workflow ends (`__end__`)

```yaml
name: implicit-flow
nodes:
  - name: step_a
    run: |
      return {"message": "Step A done"}

  - name: step_b
    run: |
      return {"message": "Step B done"}

  - name: step_c
    run: |
      return {"message": "Step C done"}

# No edges needed - implicit flow: step_a -> step_b -> step_c -> __end__
```

### The `goto` Property

The `goto` property on a node specifies the next node to execute. It can be:
- **String**: Unconditional jump to a specific node
- **Array**: Conditional jump with if/to rules

#### Unconditional `goto`

Jump directly to a named node:

```yaml
nodes:
  - name: start
    run: |
      return {"initialized": True}
    goto: validate  # Skip to validate, not next node

  - name: skipped_node
    run: |
      return {"this": "is skipped"}

  - name: validate
    run: |
      return {"validated": True}
```

#### Special `goto` Targets

| Target | Description |
|--------|-------------|
| `"__end__"` | Terminate workflow immediately |
| Node name | Jump to the named node |
| (omitted) | Use implicit chaining (next in list) |

### Conditional `goto`

Use a list of rules for conditional branching. Each rule has:
- `if` (optional): Boolean expression to evaluate
- `to` (required): Target node if condition is true

Rules are evaluated in order; the first matching rule wins.

```yaml
nodes:
  - name: validate
    run: |
      score = check_quality(state["input"])
      return {"score": score}
    goto:
      - if: "state.score > 0.9"
        to: high_confidence
      - if: "state.score > 0.5"
        to: medium_confidence
      - to: low_confidence  # Fallback (no condition = always true)

  - name: high_confidence
    run: |
      return {"path": "high"}
    goto: __end__  # Terminate early

  - name: medium_confidence
    run: |
      return {"path": "medium"}

  - name: low_confidence
    run: |
      return {"path": "low"}
```

#### Context Variables in Conditions

| Variable | Description |
|----------|-------------|
| `state` | Global agent state (includes merged node results) |
| `variables` | Template variables from YAML `variables:` section |
| `secrets` | Secret values (if configured) |

> **Note:** Node execution results are automatically merged into `state` before `goto` evaluation. Access returned values via `state.field_name` (e.g., if node returns `{"score": 90}`, use `state.score`).

### Loops with `goto`

Use conditional `goto` to create loops:

```yaml
nodes:
  - name: retry_step
    run: |
      result = attempt_operation()
      attempts = state.get("attempts", 0) + 1
      return {"status": result.status, "attempts": attempts}
    goto:
      - if: "state.status == 'error' and state.attempts < 3"
        to: retry_step  # Loop back
      - if: "state.status == 'ok'"
        to: success
      - to: failure  # Max retries exceeded

  - name: success
    run: |
      return {"final": "success"}
    goto: __end__

  - name: failure
    run: |
      return {"final": "failed"}
    goto: __end__
```

### Navigation Precedence

When multiple navigation methods are present, precedence is:

1. **`goto` property on node** (highest priority)
2. **`edges` section** (legacy, deprecated)
3. **Implicit chaining** (next node in list)

```yaml
# Example: goto takes precedence over edges
nodes:
  - name: step_a
    goto: step_c  # This wins
  - name: step_b
  - name: step_c
edges:
  - from: step_a
    to: step_b  # Ignored because goto exists
```

### Migration from Edges

To migrate from the legacy `edges` format:

| Legacy Pattern | New Syntax |
|----------------|------------|
| Linear edges (A→B→C) | Remove edges, use implicit chaining |
| `from: __start__` | Not needed, first node is entry point |
| `to: __end__` | Not needed for last node, or use `goto: __end__` |
| Conditional edges | Use `goto:` list with `if`/`to` rules |
| Unconditional jump | Use `goto: target_node` |
| **Parallel edges** | **Keep as-is** (not deprecated) |

> **Note:** Parallel edges with `parallel: true` and `fan_in:` are not deprecated and should remain in the `edges` section. Only sequential navigation edges are being migrated to `goto`.

See [TEA-YAML-002](../stories/TEA-YAML-002-implicit-graph-goto-syntax.md) for the full migration guide and LLM prompt.

---

## Edge Specification (Deprecated)

> ⚠️ **Deprecation Notice:** Sequential edges are deprecated in favor of implicit chaining and `goto` properties. See [Navigation and Flow Control](#navigation-and-flow-control) for the modern syntax.
>
> **Exception:** Parallel edges (`parallel: true`, `fan_in:`) are **not deprecated**. They remain the only way to define fan-out/fan-in execution patterns and will continue to be supported.
>
> **Deprecation Roadmap (Sequential Edges Only):**
> - **v0.8.x (Current)**: Sequential `edges` work normally, emit INFO-level warning
> - **v1.0.x**: Sequential `edges` work, emit WARNING-level warning with migration link
> - **v2.0.x**: Sequential `edges` rejected with error, must use `goto` or implicit

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

# Method 3: Variable reference with negation
- from: check
  to: skip
  when: "!should_process"
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

Templates use **Jinja2** (TEA-YAML-001), providing familiar syntax used in Flask, Ansible, and dbt.

### Basic Substitution

| Syntax | Description | Example |
|--------|-------------|---------|
| `{{ state.key }}` | State value | `{{ state.user_name }}` |
| `{{ variables.key }}` | Global variable | `{{ variables.api_url }}` |
| `{{ secrets.key }}` | Secret value | `{{ secrets.api_key }}` |
| `{{ checkpoint.dir }}` | Checkpoint directory | `{{ checkpoint.dir }}/backup.pkl` |
| `{{ checkpoint.last }}` | Last checkpoint path | `{{ checkpoint.last }}` |
| `${ key }` | GitLab CI style | `${ CI_COMMIT_SHA }` |

### Jinja2 Filters

All standard Jinja2 filters are available, plus custom filters:

| Filter | Description | Example |
|--------|-------------|---------|
| `tojson` | JSON serialize | `{{ state.data \| tojson }}` |
| `json` | Alias for tojson | `{{ state.data \| json }}` |
| `fromjson` | Parse JSON string | `{{ state.json_str \| fromjson }}` |
| `upper` | Uppercase | `{{ state.name \| upper }}` |
| `lower` | Lowercase | `{{ state.name \| lower }}` |
| `length` | Get length | `{{ state.items \| length }}` |
| `default` | Default for undefined | `{{ state.missing \| default("N/A") }}` |
| `truncate` | Truncate string | `{{ state.text \| truncate(50) }}` |
| `join` | Join list items | `{{ state.tags \| join(", ") }}` |
| `first` | First item | `{{ state.items \| first }}` |
| `last` | Last item | `{{ state.items \| last }}` |

### Jinja2 Constructs

#### Conditionals

```yaml
- name: format_message
  uses: template.render
  with:
    template: |
      {% if state.priority == 'high' %}
      🚨 URGENT: {{ state.message }}
      {% elif state.priority == 'medium' %}
      ⚠️ {{ state.message }}
      {% else %}
      {{ state.message }}
      {% endif %}
```

#### Loops

```yaml
- name: format_report
  uses: template.render
  with:
    template: |
      Report Items:
      {% for item in state.items %}
      - {{ item.name }}: {{ item.value | tojson }}
      {% endfor %}
      Total: {{ state.items | length }} items
```

### GitHub Actions to Jinja2 Equivalents

| GitHub Actions Style | Jinja2 Native Equivalent |
|---------------------|--------------------------|
| `contains(s, v)` | `'v' in s` |
| `startsWith(s, p)` | `s.startswith('p')` |
| `endsWith(s, x)` | `s.endswith('x')` |
| `join(arr, sep)` | `arr \| join(sep)` |
| `toJSON(v)` | `v \| tojson` |
| `fromJSON(s)` | `s \| fromjson` |
| `len(c)` | `c \| length` |
| `format('{0}', a)` | `'%s' % a` or inline `{{ a }}` |

### Object Passthrough

When a template is a single expression (e.g., `"{{ state.data }}"`), it returns the actual Python object, not a string representation. This enables passing complex objects between actions:

```yaml
- name: get_data
  uses: http.get
  with:
    url: "{{ variables.api_url }}"
  output: response

- name: process
  uses: json.transform
  with:
    # data receives the actual dict, not a string
    data: "{{ state.response }}"
    expression: "items[*].name"
```

### Undefined Variable Handling

Templates use `StrictUndefined` mode. Undefined variables:
- **Single-level access** (`{{ state.missing }}`) returns `None`
- **Nested access** (`{{ state.missing.deep }}`) raises `ValueError`
- Use `| default("fallback")` filter for graceful fallbacks:

```yaml
url: "{{ state.custom_url | default(variables.default_url) }}"
```

### Security Note

Template processing uses Jinja2's sandboxed environment. Unlike the old `eval()` approach:
- `__import__` and dangerous builtins are **blocked** in templates
- This improves security for template expressions
- `run:` blocks still use `exec()` with full Python access (by design)

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
{"content": "LLM response text", "usage": {"prompt_tokens": N, "completion_tokens": N}}
```

#### LLM Provider Configuration

LLM actions support multiple providers: **OpenAI**, **Azure OpenAI**, **Ollama**, and **LiteLLM**.

**Provider Detection Priority:**
1. Explicit `provider` parameter (highest priority)
2. Environment variable detection:
   - `OLLAMA_API_BASE` → Ollama
   - `AZURE_OPENAI_API_KEY` + `AZURE_OPENAI_ENDPOINT` → Azure OpenAI
3. Default → OpenAI

**Ollama Example (local LLMs):**

```yaml
# Explicit provider parameter
- name: ask_local_llm
  uses: llm.call
  with:
    provider: ollama                # Use local Ollama
    model: llama3.2                 # Ollama model name
    api_base: http://localhost:11434/v1  # Optional, this is the default
    messages:
      - role: user
        content: "{{ state.question }}"
  output: response

# Environment variable fallback (set OLLAMA_API_BASE)
- name: ask_llm
  uses: llm.call
  with:
    model: llama3.2
    messages:
      - role: user
        content: "{{ state.question }}"
  output: response
```

**Provider Parameters:**

| Parameter | Description | Default |
|-----------|-------------|---------|
| `provider` | Provider selection: `auto`, `openai`, `azure`, `ollama`, `litellm` | `auto` |
| `api_base` | Custom API base URL | Provider default |

**Environment Variables:**

| Variable | Provider | Description |
|----------|----------|-------------|
| `OPENAI_API_KEY` | OpenAI | OpenAI API key |
| `AZURE_OPENAI_API_KEY` | Azure | Azure OpenAI API key |
| `AZURE_OPENAI_ENDPOINT` | Azure | Azure endpoint URL |
| `OLLAMA_API_BASE` | Ollama | Ollama API URL (default: `http://localhost:11434/v1`) |

**Ollama Notes:**
- No API key required (uses dummy value internally)
- No cost calculation (local/free)
- Tool calling requires compatible models: `llama3.1+`, `mistral-nemo`, `qwen2.5`

#### LiteLLM Provider (TEA-LLM-003)

LiteLLM provides access to 100+ LLM providers through a unified OpenAI-compatible interface. Install with:

```bash
pip install the_edge_agent[litellm]
```

**LiteLLM Example:**

```yaml
# Use Anthropic Claude via LiteLLM
- name: ask_claude
  uses: llm.call
  with:
    provider: litellm
    model: anthropic/claude-3-opus-20240229
    messages:
      - role: user
        content: "{{ state.question }}"
  output: response

# Use Google Gemini via LiteLLM
- name: ask_gemini
  uses: llm.call
  with:
    provider: litellm
    model: gemini/gemini-pro
    messages:
      - role: user
        content: "{{ state.question }}"
  output: response

# Use AWS Bedrock via LiteLLM
- name: ask_bedrock
  uses: llm.call
  with:
    provider: litellm
    model: bedrock/anthropic.claude-v2
    messages:
      - role: user
        content: "{{ state.question }}"
  output: response
```

**LiteLLM Model Format:**

LiteLLM uses `provider/model-name` format:

| Provider | Model Example |
|----------|---------------|
| Anthropic | `anthropic/claude-3-opus-20240229` |
| AWS Bedrock | `bedrock/anthropic.claude-v2` |
| Google Gemini | `gemini/gemini-pro` |
| Azure OpenAI | `azure/gpt-4` |
| Ollama (via LiteLLM) | `ollama/llama3.2` |
| Cohere | `cohere/command-r-plus` |
| Mistral | `mistral/mistral-large-latest` |

**LiteLLM Environment Variables:**

Each provider requires its own API key. Common examples:

| Variable | Provider |
|----------|----------|
| `ANTHROPIC_API_KEY` | Anthropic Claude |
| `GOOGLE_API_KEY` | Google Gemini |
| `COHERE_API_KEY` | Cohere |
| `MISTRAL_API_KEY` | Mistral AI |
| `AWS_ACCESS_KEY_ID` + `AWS_SECRET_ACCESS_KEY` | AWS Bedrock |

See [LiteLLM Providers](https://docs.litellm.ai/docs/providers) for complete list.

**LiteLLM Features:**
- Built-in cost tracking via `cost_usd` in response
- Automatic retry with exponential backoff (`max_retries` parameter)
- Opik observability integration (`opik_trace=True`)
- Streaming support (`llm.stream`)
- Tool calling support (`llm.tools`) for compatible models

**LiteLLM with Opik Tracing:**

```yaml
- name: traced_call
  uses: llm.call
  with:
    provider: litellm
    model: anthropic/claude-3-opus-20240229
    opik_trace: true  # Enable Opik logging
    messages:
      - role: user
        content: "{{ state.question }}"
  output: response
```

#### `llm.stream`

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

**Returns:**
```python
{"content": str, "usage": dict, "streamed": true, "chunk_count": int}
```

#### `llm.retry`

LLM calls with exponential backoff retry logic:

```yaml
- name: resilient_call
  uses: llm.retry
  with:
    model: gpt-4
    messages:
      - role: user
        content: "{{ state.query }}"
    max_retries: 3          # Optional (default: 3)
    base_delay: 1.0         # Optional (default: 1.0)
    max_delay: 60.0         # Optional (default: 60.0)
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
        action: http.get            # Maps to registered action
    tool_choice: auto               # Optional: "auto", "none", or tool name
    max_tool_rounds: 10             # Optional (default: 10)
  output: tools_result
```

**Returns:**
- Success: `{"content": str, "tool_calls": list, "tool_results": list, "rounds": int}`
- Failure: `{"error": str, "success": false, "tool_calls": list, "tool_results": list}`

All LLM actions are available via dual namespaces: `llm.*` and `actions.llm_*`.

---

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

---

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

---

### Storage Actions

Advanced storage operations for cloud and local filesystems.

#### `storage.list`

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

```yaml
- name: check_file
  uses: storage.exists
  with:
    path: s3://my-bucket/data/file.json   # Required
  output: exists_result
```

**Returns:** `{"exists": bool, "path": str, "success": true}`

#### `storage.info`

```yaml
- name: get_info
  uses: storage.info
  with:
    path: s3://my-bucket/data/file.json   # Required
  output: file_info
```

**Returns:** `{"info": {"name": str, "size": int, "type": str, ...}, "success": true}`

#### `storage.copy`

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

All storage actions are available via dual namespaces: `storage.*` and `actions.storage_*`.

---

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

**Common JMESPath expressions:**
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

---

### Code Execution Actions

> **Security Warning:** Code execution is DISABLED by default. Enable with `YAMLEngine(enable_code_execution=True)`.
> Uses RestrictedPython sandbox - not suitable for arbitrary untrusted code.

**Required:** `pip install RestrictedPython`

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

All code actions are available via dual namespaces: `code.*` and `actions.code_*`.

---

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

#### Auto-Instrumentation

Enable automatic tracing via YAML settings:

```yaml
settings:
  auto_trace: true         # Auto-wrap all nodes with tracing
  trace_exporter: console  # "console", "file"
  trace_file: ./traces.jsonl
```

All trace actions are available via dual namespaces: `trace.*` and `actions.trace_*`.

#### Opik Integration

Export traces to [Comet Opik](https://www.comet.com/site/products/opik/) for visualization, analysis, and LLM observability.

**Required:** `pip install opik` or `pip install the-edge-agent[opik]`

##### Basic Configuration

```yaml
settings:
  # Enable Opik trace export
  trace_exporter: opik

  # Full Opik configuration
  opik:
    enabled: true
    api_key: "${OPIK_API_KEY}"          # Supports env var interpolation
    workspace: my-team                   # Optional: Opik workspace/organization
    project_name: my-agent-production    # Project for grouping traces
    url: https://opik.mycompany.com/api  # Optional: Self-hosted Opik URL
    llm_tracing: true                    # Wrap OpenAI clients with track_openai()
    trace_export: true                   # Export TEA trace spans to Opik
```

##### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `OPIK_API_KEY` | API key for Opik Cloud | None (required for Cloud) |
| `OPIK_WORKSPACE` | Workspace/organization name | User's default workspace |
| `OPIK_PROJECT_NAME` | Project for grouping traces | `"the-edge-agent"` |
| `OPIK_URL_OVERRIDE` | Self-hosted Opik endpoint URL | Opik Cloud URL |

##### Native LLM Tracing

Enable automatic instrumentation of LLM calls with rich telemetry (tokens, latency, cost):

```yaml
settings:
  opik:
    llm_tracing: true    # Wraps OpenAI clients with track_openai()

nodes:
  - name: generate
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: user
          content: "{{ state.prompt }}"
      opik_trace: true   # Per-call override (optional)
    output: response
```

When `llm_tracing: true` is set:
- OpenAI client is wrapped with `track_openai()` for rich telemetry
- Captures: model, tokens (prompt/completion), latency, model parameters
- Automatically calculates `cost_usd` based on token usage and model pricing
- Works with both `llm.call` and `llm.stream` actions

##### `opik.healthcheck`

Validate Opik connectivity before running workflows:

```yaml
nodes:
  - name: validate_opik
    uses: opik.healthcheck
    output: opik_status

  - name: process
    run: |
      if not state.get('opik_status', {}).get('success'):
          return {"error": "Opik not available", "skip_tracing": True}
      # Continue with tracing-enabled processing
      return {"ready": True}

edges:
  - from: __start__
    to: validate_opik
  - from: validate_opik
    to: process
  - from: process
    to: __end__
```

**Returns:**
- Success: `{"success": true, "latency_ms": float, "project": str, "workspace": str}`
- Failure: `{"success": false, "message": str, "error_type": str}`

**Common error messages:**
- `"Opik SDK not installed. Install with: pip install opik"`
- `"OPIK_API_KEY not set. Get your API key at https://www.comet.com/opik"`
- `"Invalid API key. Please verify your key at https://www.comet.com/opik/account"`
- `"Cannot connect to Opik at {url}. Check network connectivity."`

##### Configuration Examples

**Cloud Setup (Recommended):**
```yaml
settings:
  opik:
    enabled: true
    api_key: "${OPIK_API_KEY}"
    project_name: my-production-agent
    llm_tracing: true
```

**Self-Hosted Setup:**
```yaml
settings:
  opik:
    enabled: true
    url: https://opik.mycompany.com/api
    project_name: internal-agent
    # API key may not be required for self-hosted
```

**Minimal Local Development:**
```yaml
settings:
  trace_exporter: opik
  # Uses defaults - traces stored locally
```

**Combined with File Tracing:**
```yaml
settings:
  auto_trace: true
  trace_exporter:
    - opik                    # Export to Opik Cloud
    - file                    # Also save locally
  trace_file: ./traces.jsonl
  opik:
    project_name: my-agent
    llm_tracing: true
```

##### Python API Configuration

```python
from the_edge_agent import YAMLEngine

# Simple usage - uses environment variables
engine = YAMLEngine(trace_exporter="opik")

# Full configuration
engine = YAMLEngine(
    trace_exporter="opik",
    opik_project_name="my-agent",
    opik_workspace="my-team",
    opik_api_key="your-api-key",
    opik_url="https://opik.mycompany.com",  # Self-hosted
    opik_llm_tracing=True,    # Native LLM instrumentation
    opik_trace_export=True    # Export TEA spans to Opik
)

# Access resolved configuration
print(engine.opik_config)
# {'enabled': True, 'api_key': '...', 'workspace': 'my-team',
#  'project_name': 'my-agent', 'url': '...', 'llm_tracing': True, 'trace_export': True}
```

##### Troubleshooting

| Issue | Solution |
|-------|----------|
| `ImportError: Opik SDK not installed` | `pip install opik` or `pip install the-edge-agent[opik]` |
| `OPIK_API_KEY not set` | Set env var or use `api_key` in settings |
| `Invalid API key` | Verify key at https://www.comet.com/opik/account |
| `Cannot connect to Opik` | Check network/firewall, verify URL for self-hosted |
| Project not visible in Opik | Project is auto-created on first trace export |
| LLM costs not showing | Ensure `llm_tracing: true` is set |

All Opik actions are available via dual namespaces: `opik.*` and `actions.opik_*`.

---

### Memory Actions

Session memory for storing data across graph invocations within the same engine instance.

> **Warning: Global State**
>
> Memory actions use a **process-global** in-memory store:
>
> - All workflows in the same process share the same memory store
> - `memory.clear` wipes ALL stored data, affecting all concurrent workflows
> - Data does **not** persist across process restarts
> - Use key prefixes (e.g., `session_123:user_name`) to namespace data
>
> For workflow-scoped storage, use state variables. For persistent storage, use [Long-Term Memory Actions](#long-term-memory-actions).

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

All memory actions are available via dual namespaces: `memory.*` and `actions.memory_*`.

---

### Long-Term Memory Configuration

Configure LTM backend in the `settings.ltm` section. Supports multiple backend types and catalog options.

#### Basic Configuration

```yaml
settings:
  ltm:
    backend: duckdb              # "sqlite" (default), "duckdb", "litestream", "blob-sqlite"
    catalog:
      type: sqlite               # "sqlite", "firestore", "postgres", "supabase"
      path: ":memory:"           # For sqlite catalog
    storage:
      uri: "${LTM_STORAGE:-./ltm_data/}"  # Cloud or local storage path
    inline_threshold: 1024       # Inline data < this size in catalog (bytes)
    lazy: true                   # Lazy initialization for serverless
    enable_fts: true             # Full-text search (default: true)
```

#### Backend Types

| Backend | Description | Best For |
|---------|-------------|----------|
| `sqlite` | Local SQLite with FTS5 | Development, single-node |
| `duckdb` | DuckDB + catalog + cloud storage | Analytics, cloud storage, caching |
| `litestream` | SQLite with S3 replication | Disaster recovery, edge sync |
| `blob-sqlite` | SQLite on blob storage | Distributed, multi-node |

#### Catalog Types (for DuckDB backend)

| Catalog | Description | Best For |
|---------|-------------|----------|
| `sqlite` | Local SQLite | Development, testing |
| `firestore` | Firebase Firestore | Serverless, Firebase ecosystem |
| `postgres` | PostgreSQL | Self-hosted, SQL compatibility |
| `supabase` | Supabase REST API | Edge, managed Postgres |

#### Environment Variable Expansion

LTM configuration supports `${VAR}` and `${VAR:-default}` syntax:

```yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: "${CATALOG_TYPE:-sqlite}"
      path: "${CATALOG_PATH:-:memory:}"
    storage:
      uri: "${STORAGE_URI:-./ltm_data/}"
```

#### Minimal Configuration

For simple use cases, use the default SQLite backend:

```yaml
settings:
  ltm:
    backend: sqlite  # Uses in-memory database by default
```

#### Migration Guide

##### Migrating from SQLite to DuckDB Backend

The DuckDB backend provides catalog-aware storage with automatic inlining for small data and cloud storage for large data. Migration is seamless:

**Before (SQLite):**
```yaml
settings:
  ltm:
    backend: sqlite
```
Or using Python:
```python
engine = YAMLEngine(ltm_backend_type="sqlite", ltm_path="./memory.db")
```

**After (DuckDB):**
```yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: sqlite
      path: "./memory.db"  # Reuse existing SQLite as catalog
    storage:
      uri: "./ltm_data/"
```
Or using Python:
```python
engine = YAMLEngine(
    ltm_backend_type="duckdb",
    ltm_config={
        "catalog_config": {"type": "sqlite", "path": "./memory.db"},
        "storage_uri": "./ltm_data/"
    }
)
```

**Data Migration:** Existing SQLite data is compatible - DuckDB uses the SQLite file as its catalog.

##### Migrating from Blob-SQLite to DuckDB

For users currently using `blob-sqlite` backend:

**Before (Blob-SQLite):**
```python
engine = YAMLEngine(
    ltm_backend_type="blob-sqlite",
    ltm_config={
        "blob_uri": "s3://bucket/prefix/",
        "lock_backend": "firestore"
    }
)
```

**After (DuckDB):**
```yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: firestore  # Use Firestore for distributed coordination
    storage:
      uri: "s3://bucket/prefix/"  # Same cloud storage
```

**Key Differences:**
- DuckDB separates catalog (metadata) from storage (values)
- Automatic inlining for small values (<1KB by default)
- Content-hash deduplication built-in
- FTS search via DuckDB extension

### Long-Term Memory Actions

Persistent storage using the configured LTM backend with full-text search. Unlike session memory, data persists across engine restarts.

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

All LTM actions are available via dual namespaces: `ltm.*` and `actions.ltm_*`.

---

### Cache and Memoization Actions

Automatic caching of action results in Long-Term Memory with configurable TTL and cache key strategies. Avoids redundant expensive operations like API calls, file processing, and LLM inference.

**Required dependencies:**
- LTM backend configured (SQLite by default)
- `pip install fsspec` - For remote file hashing

#### `cache.wrap`

Wrap any action with automatic caching:

```yaml
# Cache LLM call by arguments hash
- name: translate_cached
  uses: cache.wrap
  with:
    action: llm.call
    key_strategy: args                     # Hash all arguments
    ttl_days: 30                           # Cache for 30 days
    args:
      model: gpt-4o
      messages:
        - role: user
          content: "Translate to Spanish: {{ state.text }}"
  output: translation_result

# Cache document extraction by file content hash
- name: extract_cached
  uses: cache.wrap
  with:
    action: llamaextract.extract
    key_strategy: file_content             # Hash file content
    key_source: file                       # Argument containing file path
    ttl_days: 60
    args:
      file: "{{ state.file_path }}"
      agent_name: "{{ state.agent_name }}"
  output: extraction_result

# Custom cache key with Jinja expression
- name: search_cached
  uses: cache.wrap
  with:
    action: web.search
    key: "search:{{ state.query | lower | sha256 }}"
    ttl_hours: 24
    args:
      query: "{{ state.query }}"
  output: search_result
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `action` | string | Yes | - | Action to wrap (e.g., `llm.call`, `llamaextract.extract`) |
| `args` | dict | Yes | - | Arguments to pass to wrapped action |
| `key` | string | No | - | Custom cache key or Jinja expression |
| `key_strategy` | string | No | `args` | One of: `sha256`, `args`, `custom`, `file_content` |
| `key_source` | string | No | - | Argument name for `file_content`/`sha256` strategy |
| `ttl_days` | int | No | 60 | Cache TTL in days |
| `ttl_hours` | int | No | - | TTL in hours (overrides `ttl_days`) |
| `ttl_seconds` | int | No | - | TTL in seconds (overrides `ttl_hours`) |
| `skip_cache` | bool | No | false | Bypass cache lookup, force fresh execution |
| `cache_enabled` | bool | No | true | Enable/disable caching entirely |
| `cleanup_probability` | float | No | 0.05 | Probability of cleanup after cache miss |
| `cleanup_limit` | int | No | 5 | Max expired entries to delete per cleanup |

**Returns:**
```json
{
  "success": true,
  "result": {...},        // Wrapped action result
  "_cache_hit": true,     // Whether result came from cache
  "_cache_key": "cache:llm.call:abc123...",
  "_cache_created_at": "2025-01-15T10:30:00+00:00"  // If cache hit
}
```

#### `cache.get`

Retrieve cached value without executing action (for debugging/inspection):

```yaml
- name: check_cache
  uses: cache.get
  with:
    key: "cache:llm.call:abc123..."        # Cache key to retrieve
    include_metadata: true                  # Include cache metadata
  output: cache_entry
```

**Returns:** `{"success": true, "found": bool, "value": any, "expired": bool, "metadata": dict}`

#### `cache.invalidate`

Delete cached entries by key or pattern:

```yaml
# Invalidate by exact key
- name: clear_entry
  uses: cache.invalidate
  with:
    key: "cache:llm.call:abc123..."
  output: invalidate_result

# Invalidate by pattern
- name: clear_all_llm_cache
  uses: cache.invalidate
  with:
    pattern: "cache:llm.call:*"
  output: bulk_invalidate

# Invalidate by metadata filter
- name: clear_extraction_cache
  uses: cache.invalidate
  with:
    metadata_filter:
      _cache_action: llamaextract.extract
  output: filtered_invalidate
```

**Returns:** `{"success": true, "deleted_count": int, "deleted_keys": list}`

#### `storage.hash`

Compute SHA256 hash of file content from any URI:

```yaml
- name: hash_document
  uses: storage.hash
  with:
    path: "s3://bucket/document.pdf"       # Any fsspec URI
    algorithm: sha256                       # sha256, md5, or blake2b
  output: hash_result
```

**Returns:** `{"success": true, "hash": str, "algorithm": str, "size_bytes": int, "path": str}`

#### sha256 Jinja Filter

Compute SHA256 hash inline in templates:

```yaml
# Hash string content
key: "{{ state.content | sha256 }}"

# Combine with other filters
key: "cache:{{ state.query | lower | sha256 }}"
```

All cache actions are available via dual namespaces: `cache.*` and `actions.cache_*`.

---

### Firebase Agent Memory Actions

Cloud-native agent memory layer with DuckDB search, vector similarity, and session management.
Uses abstract backend interfaces for portability across Firebase, PostgreSQL, and S3.

**Required dependencies:**
- `pip install firebase-admin` - For Firestore/GCS backends
- `pip install duckdb` - For query engine and vector search
- `pip install sqlglot` - For SQL validation
- `pip install tiktoken` - For token counting

Or install with: `pip install the-edge-agent[firebase]`

#### `memory.cloud_store`

Store content to cloud storage with metadata:

```yaml
- name: store_document
  uses: memory.cloud_store
  with:
    path: "firms/acme/profile.yaml"        # Required
    content: "{{ state.yaml_content }}"     # Required
    metadata:                               # Optional
      status: "active"
      summary: "Company profile"
    skip_embedding: false                   # Optional (default: false)
  output: store_result
```

**Returns:** `{"success": true, "storage_uri": str, "content_hash": "sha256:...", "doc_id": str}`

#### `memory.cloud_retrieve`

Retrieve content from cloud storage:

```yaml
- name: load_document
  uses: memory.cloud_retrieve
  with:
    path: "firms/acme/profile.yaml"        # Required
  output: retrieved_doc
```

**Returns:** `{"success": true, "content": str, "metadata": dict}`

#### `memory.cloud_list`

List files with filtering:

```yaml
- name: list_firm_docs
  uses: memory.cloud_list
  with:
    prefix: "firms/"                        # Optional
    limit: 100                              # Optional (default: 100)
  output: file_list
```

**Returns:** `{"success": true, "files": list, "count": int}`

#### `memory.grep`

Deterministic text search across memory files:

```yaml
- name: search_todos
  uses: memory.grep
  with:
    pattern: "TODO"                         # Required
    path_filter: "*.yaml"                   # Optional
    case_sensitive: false                   # Optional (default: true)
  output: grep_results
```

**Returns:** `{"success": true, "results": list, "count": int}`

#### `memory.sql_query`

SQL query with safety controls (SELECT only):

```yaml
- name: query_active_docs
  uses: memory.sql_query
  with:
    query: "SELECT file_path, summary FROM agent_memory WHERE status = 'active' LIMIT 10"
    path_filter: "firms/*"                  # Optional
  output: query_results
```

**Returns:** `{"success": true, "results": list, "count": int, "columns": list}`

**Security:** Only SELECT queries allowed. Dangerous functions (read_csv, etc.) blocked.

#### `memory.embed`

Generate embedding for content:

```yaml
- name: embed_content
  uses: memory.embed
  with:
    content: "{{ state.document_text }}"    # Required
    model: "text-embedding-3-small"         # Optional (default)
  output: embedding_result
```

**Returns:** `{"success": true, "embedding": list, "model": str, "dimensions": 1536}`

#### `memory.vector_search`

Semantic similarity search:

```yaml
- name: semantic_search
  uses: memory.vector_search
  with:
    query: "legal contract analysis"        # Required
    top_k: 10                               # Optional (default: 10)
    threshold: 0.7                          # Optional (default: 0.0)
    content_type: "yaml"                    # Optional filter
  output: search_results
```

**Returns:** `{"success": true, "results": [{"id": str, "score": float, "content": str, "metadata": dict}], "count": int}`

#### `session.create`

Create session with TTL:

```yaml
- name: start_session
  uses: session.create
  with:
    session_id: "interview-123"             # Required
    user_id: "user-456"                     # Required
    ttl_hours: 24                           # Optional (default: 24)
    metadata:                               # Optional
      type: "interview"
      firm: "acme"
  output: session_result
```

**Returns:** `{"success": true, "session_id": str, "expires_at": datetime}`

#### `session.end`

End session and archive its memory:

```yaml
- name: end_session
  uses: session.end
  with:
    session_id: "interview-123"             # Required
  output: archive_result
```

**Returns:** `{"success": true, "archived": true, "archive_path": str}`

#### `session.restore`

Restore archived session:

```yaml
- name: restore_session
  uses: session.restore
  with:
    session_id: "interview-123"             # Required
  output: restore_result
```

**Returns:** `{"success": true, "restored": true, "session_id": str}`

#### `catalog.register_table`

Register table in DuckLake catalog:

```yaml
- name: register_memory_table
  uses: catalog.register_table
  with:
    name: "agent_memory"                    # Required
    table_type: "memory"                    # Required (memory | tabular)
    source_prefix: "agent-memory/"          # Required
    schema:                                 # Required
      file_path: "VARCHAR"
      content: "VARCHAR"
      embedding: "FLOAT[1536]"
  output: table_result
```

**Returns:** `{"success": true, "table_id": str, "name": str}`

#### `catalog.create_snapshot`

Create point-in-time snapshot:

```yaml
- name: snapshot_catalog
  uses: catalog.create_snapshot
  with:
    table: "agent_memory"                   # Required
  output: snapshot_result
```

**Returns:** `{"success": true, "snapshot_id": str, "created_at": datetime}`

All Firebase memory actions use dual namespaces: `memory.*`, `session.*`, `catalog.*` and `actions.memory_*`, etc.

---

### Tabular Data Actions

Hybrid storage for structured tabular data using DuckLake catalog.

**Required:**
- `pip install duckdb` - For SQL queries and Parquet I/O

**Storage Strategy:**
- **Inline** (<1KB): Small batches stored in metadata store for low-latency access
- **Parquet** (≥1KB): Large batches stored as Parquet files in blob storage
- **LWW Merge**: Queries merge both sources, keeping highest `_version` per primary key

#### `data.create_table`

Create table with schema and primary key:

```yaml
- name: create_scores_table
  uses: data.create_table
  with:
    name: "firm_scores"                       # Required
    schema:                                   # Required
      firm_id: "string"
      score: "float"
      category: "string"
    primary_key:                              # Required
      - "firm_id"
  output: table_result
```

**Schema types:** `string`, `integer`, `float`, `boolean`, `timestamp`, `json`

**Returns:** `{"success": true, "table": str, "schema": dict}`

#### `data.insert`

Insert rows (auto-selects inline vs Parquet based on size):

```yaml
- name: insert_scores
  uses: data.insert
  with:
    table: "firm_scores"                      # Required
    rows:                                     # Required
      - firm_id: "f1"
        score: 85.5
        category: "A"
      - firm_id: "f2"
        score: 92.0
        category: "A+"
  output: insert_result
```

**Returns:** `{"success": true, "table": str, "row_count": int, "storage": "inlined"|"parquet"}`

#### `data.update`

Update rows by primary key (append-only versioning):

```yaml
- name: update_score
  uses: data.update
  with:
    table: "firm_scores"                      # Required
    where:                                    # Required (must include PK)
      firm_id: "f1"
    updates:                                  # Required
      score: 90.0
      category: "A+"
  output: update_result
```

**Returns:** `{"success": true, "table": str, "status": "updated", "row_count": int}`

#### `data.delete`

Delete rows by primary key (creates tombstone):

```yaml
- name: delete_firm
  uses: data.delete
  with:
    table: "firm_scores"                      # Required
    where:                                    # Required (must include PK)
      firm_id: "f2"
  output: delete_result
```

**Returns:** `{"success": true, "table": str, "status": "deleted", "row_count": int}`

#### `data.query`

SQL query with Last-Write-Wins merge:

```yaml
- name: query_top_scores
  uses: data.query
  with:
    table: "firm_scores"                      # Required
    sql: "SELECT * FROM data WHERE score > 80 ORDER BY score DESC"  # Required
  output: query_result
```

**Note:** Table is aliased as `data` in SQL queries.

**Returns:** `{"success": true, "table": str, "rows": list, "row_count": int}`

#### `data.consolidate`

Compact inlined rows into Parquet files:

```yaml
- name: consolidate_scores
  uses: data.consolidate
  with:
    table: "firm_scores"                      # Required
  output: consolidate_result
```

**Returns:** `{"success": true, "table": str, "status": "consolidated", "parquet_path": str}`

All tabular data actions use dual namespaces: `data.*` and `actions.data_*`.

---

### Graph Database Actions

Entity-relationship storage using CozoDB or Kuzu backends.

**Required (optional):**
- `pip install 'pycozo[embedded]'` - For CozoDB backend
- `pip install kuzu` - For Kuzu backend

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

All graph actions are available via dual namespaces: `graph.*` and `actions.graph_*`.

---

### Web Actions

Web scraping and search via external APIs.

**Required environment variables:**
- `FIRECRAWL_API_KEY` - For web.scrape and web.crawl
- `PERPLEXITY_API_KEY` - For web.search
- `SCRAPEGRAPH_API_KEY` - For web.ai_scrape (AI-powered extraction)

#### `web.scrape`

Scrape web content via Firecrawl API:

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

Web search via Perplexity API:

```yaml
- name: search_topic
  uses: web.search
  with:
    query: "{{ state.topic }}"            # Required
    num_results: 10                       # Optional
  output: search_results
```

**Returns:** `{"success": true, "results": list, "query": str, "total_results": int, "answer": str}`

#### `web.ai_scrape`

AI-powered structured data extraction via ScrapeGraphAI API (TEA-BUILTIN-008.4).

**Required:** `SCRAPEGRAPH_API_KEY` environment variable.

**Optional package:** `pip install scrapegraph-py pydantic`

```yaml
# Basic usage with inline schema
- name: extract_products
  uses: web.ai_scrape
  with:
    url: "{{ state.target_url }}"         # Required
    prompt: "Extract all products"        # Required
    output_schema:                         # Schema (inline dict)
      type: object
      properties:
        products:
          type: array
          items:
            type: object
            properties:
              name: { type: string }
              price: { type: string }
    max_retries: 3                         # Optional (default: 3)
  output: extracted_data
```

```yaml
# Load schema from Git reference (Story 008.2)
- name: extract_invoice
  uses: web.ai_scrape
  with:
    url: "{{ state.invoice_url }}"
    prompt: "Extract invoice data"
    schema:
      uses: company/schemas@v1.0.0#invoice/schema.json
  output: invoice_data
```

```yaml
# Merge schemas from multiple sources (Story 008.3)
- name: extract_complex
  uses: web.ai_scrape
  with:
    url: "{{ state.url }}"
    prompt: "Extract all data"
    schema:
      uses:
        - base/schemas@v1#common.json       # Git ref (lowest priority)
        - s3://bucket/overlay.json           # S3 (fsspec)
        - gs://shared/final.json             # GCS
        - company/private@main#override.json # Git (highest priority)
  output: merged_data
```

```yaml
# With caching to avoid redundant API calls (Story 008.7)
- name: extract_cached
  uses: web.ai_scrape
  with:
    url: "{{ state.url }}"
    prompt: "Extract product data"
    output_schema:
      type: object
      properties:
        name: { type: string }
        price: { type: string }
    cache:
      enabled: true              # Enable LTM caching
      ttl_days: 30               # Cache for 30 days
      key_strategy: "url"        # Cache by URL only
  output: product_data
```

**Schema Sources (via Story 008.2):**
- Inline dict via `output_schema` parameter
- Git short refs: `owner/repo@ref#path/to/schema.json`
- Git full URLs: `git+https://...` or `git+ssh://...`
- fsspec URIs: `s3://`, `gs://`, `az://`, `https://`, `file://`

**Schema Merging (via Story 008.3):**
When `schema.uses` is a list, schemas are merged with kubectl-style semantics (last wins).

**Caching (via Story 008.7):**
When `cache.enabled=true`, results are stored in Long-Term Memory with configurable TTL:

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `enabled` | bool | false | Enable caching |
| `ttl_days` | int | 60 | Cache TTL in days |
| `ttl_hours` | int | - | Cache TTL in hours (overrides ttl_days) |
| `ttl_seconds` | int | - | Cache TTL in seconds (overrides ttl_hours) |
| `key_strategy` | str | "url" | Cache key strategy (see below) |
| `skip_cache` | bool | false | Force fresh scrape, ignore cache |

**Cache Key Strategies:**
- `"url"` - Hash URL only (same URL always returns cached result)
- `"url+prompt"` - Hash URL + prompt (different prompts get separate entries)
- `"url+schema"` - Hash URL + schema (different schemas get separate entries)
- `"url+prompt+schema"` - Hash all three (most granular)

**Returns:**
```python
# Success
{
    "success": True,
    "data": {...},           # Extracted data matching schema
    "url": str,
    "schema_used": {...},    # Final merged schema
    "_cache_hit": bool,      # True if result came from cache
    "_cache_key": str,       # Cache key used (when caching enabled)
    "_cache_created_at": str # ISO timestamp (on cache hit)
}

# Failure
{
    "success": False,
    "error": str,
    "error_type": str  # configuration, api_error, schema_error,
                       # timeout, rate_limit, dependency, authentication
}
```

**Retry Logic:** Automatically retries on rate limits (429) and server errors (5xx) with exponential backoff (1s, 2s, 4s...).

All web actions are available via dual namespaces: `web.*` and `actions.web_*`.

---

### RAG Actions

Retrieval-Augmented Generation with embeddings and vector search.

**Providers:**
- OpenAI: `text-embedding-3-small`, `text-embedding-3-large`, `text-embedding-ada-002`
- Ollama: `nomic-embed-text`, `mxbai-embed-large`, `all-minilm`, `bge-m3`

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

All RAG actions are available via dual namespaces: `embedding.*`, `vector.*` and `actions.embedding_*`, `actions.vector_*`.

---

### Tools Bridge Actions

Access external tool ecosystems (CrewAI, MCP, LangChain).

**Dependencies (all optional):**
```bash
pip install crewai crewai-tools     # For CrewAI
pip install mcp                      # For MCP
pip install langchain langchain-community  # For LangChain
```

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

#### `tools.discover`

Discover available tools:

```yaml
- name: list_tools
  uses: tools.discover
  with:
    source: all                           # "crewai", "mcp", "langchain", or "all"
    filter: search                        # Optional
  output: available_tools
```

**Returns:** `{"tools": list, "sources": list, "count": int, "success": true}`

All tools bridge actions are available via dual namespaces: `tools.*` and `actions.tools_*`.

---

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

---

### Checkpoint Actions

#### `checkpoint.save`

Save workflow checkpoint:

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
  "checkpoint_state": dict,
  "checkpoint_node": str,
  "checkpoint_config": dict,
  "checkpoint_timestamp": float,
  "checkpoint_version": str
}
```

---

### Schema Actions

Schema manipulation actions for merging and loading JSON Schemas.

#### `schema.merge`

Deep merge multiple JSON Schemas using kubectl-style semantics:

```yaml
- name: merge_schemas
  uses: schema.merge
  with:
    schemas:
      - path: ./base-schema.json
      - uses: company/schemas@v1.0.0#overlay.json
      - inline:
          properties:
            custom_field:
              type: string
    validate: true  # Optional: validate output schema
    output_key: merged  # Optional: default "merged_schema"
  output: schema_result
```

**Merge Semantics:**
- Objects: Recursively merged (overlay adds/overrides properties)
- Arrays: Last-wins (overlay replaces base array)
- Scalars: Last-wins
- `null`: Explicit null removes the key

**Schema Sources:**
- `path`: Local file path
- `uses`: Git reference (`owner/repo@ref#path`) or fsspec URI (`s3://bucket/path`)
- `inline`: Inline JSON Schema object

**Returns:**
```python
{
  "merged_schema": dict,  # The merged schema
  "success": true
}
```

---

### LlamaExtract Actions

Document extraction using LlamaCloud's LlamaExtract service.

**Requirements:**
- `requests` package (for REST API - default)
- `llama-cloud-services` package (optional - only for `agent_name` lookup or `use_sdk=true`)
- `LLAMAEXTRACT_API_KEY` or `LLAMAPARSE_API_KEY` environment variable

#### `llamaextract.extract`

Extract structured data from documents using the LlamaExtract REST API.

> **TEA-BUILTIN-008.5**: Uses direct REST API calls for better control and reliability.
> For large documents (30+ pages), see Phase 2 async support in TEA-BUILTIN-008.6.

```yaml
- name: extract_invoice
  uses: llamaextract.extract
  with:
    file: https://example.com/invoice.pdf  # URL, local path, or base64
    schema:
      type: object
      properties:
        total: { type: number }
        vendor: { type: string }
    mode: BALANCED  # BALANCED, MULTIMODAL, PREMIUM, FAST
    timeout: 300    # Optional: HTTP timeout in seconds (default: 300)
    max_retries: 3  # Optional: default 3
  output: extracted_data
```

**Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `file` | string | Yes | URL, local file path, or base64-encoded content |
| `schema` | dict | One of these | JSON Schema for extraction |
| `agent_id` | string | One of these | Use existing LlamaExtract agent by ID |
| `agent_name` | string | One of these | Use existing LlamaExtract agent by name (uses SDK) |
| `mode` | string | No | Extraction mode (default: BALANCED) |
| `timeout` | int | No | HTTP request timeout in seconds (default: 300) |
| `max_retries` | int | No | Max retry attempts for 429/5xx errors (default: 3) |
| `use_rest` | bool | No | Use direct REST API instead of SDK (default: false) |

**Extraction Modes:**
- `BALANCED`: Good balance of speed and accuracy (default)
- `MULTIMODAL`: Uses vision models for complex layouts
- `PREMIUM`: Highest accuracy, slower processing
- `FAST`: Fastest processing, may sacrifice accuracy

**Error Handling:**
- HTTP 429 (rate limit): Retries with exponential backoff
- HTTP 5xx (server error): Retries with exponential backoff
- HTTP 4xx (client error): Returns immediately without retry
- Timeout: Returns `error_type: "timeout"`

**Returns:**
```python
{
  "success": true,
  "data": {...},  # Extracted data matching schema
  "status": "completed"
}
```

**Error Response:**
```python
{
  "success": false,
  "error": "Error message",
  "error_type": "rate_limit" | "timeout" | "api_error" | "validation" | "configuration"
}
```

#### `llamaextract.upload_agent`

Create or update a LlamaExtract agent:

```yaml
- name: create_agent
  uses: llamaextract.upload_agent
  with:
    name: invoice-extractor
    schema:
      type: object
      properties:
        total: { type: number }
    mode: BALANCED
    force: false  # Optional: update if exists
  output: agent_result
```

**Returns:**
```python
{
  "success": true,
  "agent_id": str,
  "agent_name": str
}
```

#### `llamaextract.list_agents`

List available extraction agents:

```yaml
- name: list_all
  uses: llamaextract.list_agents
  with:
    name_filter: invoice  # Optional: filter by name
  output: agents_list
```

**Returns:**
```python
{
  "success": true,
  "agents": [{"id": str, "name": str}, ...]
}
```

#### `llamaextract.get_agent`

Get details of a specific agent:

```yaml
- name: get_agent
  uses: llamaextract.get_agent
  with:
    agent_id: abc123  # or agent_name
  output: agent_info
```

#### `llamaextract.delete_agent`

Delete an extraction agent:

```yaml
- name: remove_agent
  uses: llamaextract.delete_agent
  with:
    agent_id: abc123  # or agent_name
  output: delete_result
```

---

### Validation Actions

Actions for validating LLM-extracted data using structural schemas, Prolog constraints, and semantic probes.

#### validate.extraction

Validates extracted entities and relationships through 3 layers:

```yaml
- name: validate_data
  action: validate.extraction
  inputs:
    entities: "{{ state.entities }}"
    relationships: "{{ state.relationships }}"
    source_text: "{{ state.input_text }}"  # For semantic probes
  output: validation_result
```

The validation result includes:
- `valid`: Boolean indicating overall validation success
- `errors`: List of validation errors with details
- `validated_at`: ISO timestamp

#### validate.generate_prompt

Generates a schema-guided extraction prompt from the `extraction_schema`:

```yaml
- name: get_prompt
  action: validate.generate_prompt
  output: extraction_prompt
```

This is automatically done when `guide_extraction: true` is set in the schema.

---

### Custom Actions

Register custom actions in Python:

```python
def my_custom_action(state, param1, param2, **kwargs):
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

## Extraction Validation

The Edge Agent provides a powerful 3-layer validation framework for LLM-extracted data:

1. **Layer 1: Structural Validation** - Schema-based field requirements
2. **Layer 2: Semantic Validation** - Prolog constraint rules
3. **Layer 3: Grounding Validation** - LLM-verified semantic probes

This neurosymbolic approach combines symbolic reasoning (Prolog) with neural verification (LLM probes) for robust extraction validation.

### Extraction Schema

Defines the structure of expected entities and relationships:

```yaml
extraction_schema:
  entities:
    required_fields: [name, type]       # Fields that must be present
    optional_fields: [birth_date]       # Fields that may be present
    type_field: type                    # Field containing entity type

  relationships:
    types: [mother, father, spouse]     # Allowed relationship types
    required_fields: [type, subject, object]
    optional_fields: [confidence]
    type_requirements:                  # Type-specific requirements
      affair:
        - start_date
        - end_date

  guide_extraction: true                # Generate extraction prompt
  confidence_tracking: true             # Include confidence in Prolog facts
```

**Field Descriptions:**

| Field | Type | Description |
|-------|------|-------------|
| `entities.required_fields` | list | Fields every entity must have |
| `entities.optional_fields` | list | Fields entities may have |
| `entities.type_field` | string | Field name containing entity type (default: `type`) |
| `relationships.types` | list | Allowed relationship type values (empty = any) |
| `relationships.required_fields` | list | Fields every relationship must have |
| `relationships.type_requirements` | dict | Extra required fields per relationship type |
| `guide_extraction` | bool | If true, generates `extraction_prompt` variable |
| `confidence_tracking` | bool | If true, passes confidence to Prolog facts |

**Error Types:**
- `missing_required_field`: Required field is missing or null
- `invalid_relationship_type`: Relationship type not in allowed list

### Validation Constraints

Prolog rules for semantic validation:

```yaml
validation_constraints:
  language: prolog                      # Only "prolog" supported
  rules: |
    % A person cannot be their own parent
    validation_error(self_parent, Person) :-
        relationship(Type, Person, Person),
        member(Type, ['mother', 'father']).

    % Each child can have at most one mother
    validation_error(multiple_mothers, Child) :-
        relationship('mother', M1, Child),
        relationship('mother', M2, Child),
        M1 \= M2.

    % Warn on low confidence (using entity/3 with confidence)
    validation_error(low_confidence, Entity) :-
        entity(Entity, _, Conf),
        Conf < 0.5.
```

**Available Prolog Predicates:**

| Predicate | Description |
|-----------|-------------|
| `entity(Name, Type)` | Entity without confidence |
| `entity(Name, Type, Confidence)` | Entity with confidence (when `confidence_tracking: true`) |
| `relationship(Type, Subject, Object)` | Basic relationship |
| `relationship(Type, Subject, Object, Confidence)` | With confidence |
| `relationship(Type, Subject, Object, Start, End)` | With date range |
| `validation_error(ErrorType, Context)` | Define a constraint violation |

When any `validation_error/2` predicate succeeds, validation fails.

### Semantic Probes

LLM-verified grounding checks:

```yaml
semantic_probes:
  - for_each: relationship              # "entity" or "relationship"
    where: "type == 'mother'"           # Optional filter condition
    probe: |
      Based on the text, is {{ subject }} explicitly stated
      to be the mother of {{ object }}?

      Text: "{{ state.input_text }}"
    on_fail: reject                     # "reject" (fail-fast) or "warn"

  - for_each: entity
    probe: "Is {{ name }} a real person mentioned in the text?"
    on_fail: warn
```

**Field Descriptions:**

| Field | Type | Description |
|-------|------|-------------|
| `for_each` | string | Item type to iterate: `entity` or `relationship` |
| `where` | string | Filter expression (e.g., `type == 'mother'`) |
| `probe` | string | Jinja2 template for yes/no question |
| `on_fail` | string | `reject` (default) for fail-fast, `warn` to continue |

**Template Variables:**

- All fields from the current entity/relationship
- `{{ state.field }}` - Access full state
- `{{ text }}` - Source text passed to validation

**Error Types:**
- `semantic_probe_failed`: LLM answered "no" to probe
- `probe_execution_error`: LLM call failed
- `configuration_error`: No LLM configured for probes

### Validation Logging

Log failures for analysis and model improvement:

```yaml
validation_logging:
  enabled: true
  log_path: "${VALIDATION_LOG_PATH:-./validation_failures.jsonl}"
  include_source: true                  # Include source text
  include_timestamp: true               # Include ISO timestamp
```

**JSONL Output Format:**

```json
{
  "agent_name": "family-extractor",
  "source_hash": "a1b2c3...",
  "source_text": "Mary is the mother of John.",
  "entities": [...],
  "relationships": [...],
  "validation_result": {"valid": false, "errors": [...]},
  "timestamp": "2024-01-15T10:30:00Z"
}
```

### Complete Example

```yaml
name: family-validator

extraction_schema:
  entities:
    required_fields: [name, type]
  relationships:
    types: [mother, father]
    required_fields: [type, subject, object]
  guide_extraction: true
  confidence_tracking: true

validation_constraints:
  language: prolog
  rules: |
    validation_error(self_parent, P) :-
        relationship(T, P, P), member(T, ['mother', 'father']).

semantic_probes:
  - for_each: relationship
    probe: "Is {{ subject }} the {{ type }} of {{ object }}?"
    on_fail: reject

validation_logging:
  enabled: true
  log_path: ./failures.jsonl

nodes:
  - name: validate
    action: validate.extraction
    inputs:
      entities: "{{ state.entities }}"
      relationships: "{{ state.relationships }}"
      source_text: "{{ state.text }}"
    output: result

edges:
  - from: __start__
    to: validate
  - from: validate
    to: __end__
```

---

## Checkpoint Persistence

YAML agents support checkpoint persistence for saving and resuming workflow execution.

### Configuration

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

When `checkpoint_dir` is configured, checkpoints are automatically saved before yielding interrupt events. Files are saved as `{checkpoint_dir}/{node}_{timestamp_ms}.pkl`.

### Resume from Checkpoint

```yaml
config:
  checkpoint: ./checkpoints/review_node_1733500000.pkl
```

Or in Python:

```python
graph = engine.load_from_file("agent.yaml", checkpoint="./checkpoints/state.pkl")
```

### Template Variables for Checkpoints

- `{{ checkpoint.dir }}` - The configured `checkpoint_dir` value
- `{{ checkpoint.last }}` - Path to the most recent checkpoint saved

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

nodes:
  - name: search
    run: |
      query = state["query"]
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

  - name: insufficient_results
    run: |
      return {"summary": f"Insufficient results for query: {state['query']}"}

edges:
  - from: __start__
    to: search
  - from: search
    to: validate
  - from: validate
    to: summarize
    when: "state['has_enough']"
  - from: validate
    to: insufficient_results
    when: "!state['has_enough']"
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
  combined: dict

nodes:
  - name: prepare
    run: |
      return {"prepared_input": state["input"].strip()}

  - name: analyze_sentiment
    run: |
      return {"analysis_a": {"type": "sentiment", "score": 0.75}}

  - name: analyze_entities
    run: |
      return {"analysis_b": {"type": "entities", "entities": ["AI", "Python"]}}

  - name: analyze_topics
    run: |
      return {"analysis_c": {"type": "topics", "topics": ["technology"]}}

  - name: combine_results
    fan_in: true
    run: |
      combined = {}
      for result in parallel_results:
        for key in ["analysis_a", "analysis_b", "analysis_c"]:
          if key in result:
            combined[key] = result[key]
      return {"combined": combined}

  - name: generate_report
    run: |
      combined = state["combined"]
      return {"final_report": {"analyses": combined, "count": len(combined)}}

edges:
  - from: __start__
    to: prepare
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
  - from: combine_results
    to: generate_report
  - from: generate_report
    to: __end__
```

### Example 3: Customer Support Agent

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
      if "bill" in message or "payment" in message:
        intent = "billing"
      elif "cancel" in message or "refund" in message:
        intent = "cancellation"
      elif "bug" in message or "error" in message:
        intent = "technical"
      else:
        intent = "general"
      return {"intent": intent}

  - name: handle_billing
    steps:
      - name: lookup_account
        run: |
          return {"account_status": "active", "last_payment": "2025-01-01"}
      - name: generate_response
        run: |
          return {
            "response": f"Your account is {state['account_status']}.",
            "ticket_id": f"BILL-{state['customer_id']}"
          }

  - name: handle_cancellation
    run: |
      return {
        "response": "Let me connect you with our retention team.",
        "ticket_id": f"CANCEL-{state['customer_id']}"
      }

  - name: handle_technical
    run: |
      return {
        "response": "I've created a support ticket for our technical team.",
        "ticket_id": f"TECH-{state['customer_id']}"
      }

  - name: handle_general
    run: |
      return {
        "response": f"Please email {{ variables.support_email }}",
        "ticket_id": f"GEN-{state['customer_id']}"
      }

edges:
  - from: __start__
    to: classify_intent
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
```

---

## Python API

### Basic Usage

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
        print(f"Node: {event['node']}, State: {event['state']}")
    elif event["type"] == "final":
        print(f"Final result: {event['state']}")

# Or use invoke for final state only
result = list(graph.invoke(initial_state))
final_state = result[-1]["state"]
```

### With Custom Actions

```python
def custom_search(state, query, max_results=10, **kwargs):
    return {"results": [...]}

engine = YAMLEngine(actions_registry={
    "search.web": custom_search
})
```

### Engine Configuration

```python
# Enable code execution (disabled by default)
engine = YAMLEngine(enable_code_execution=True)

# Configure tracing
engine = YAMLEngine(
    trace_exporter="console",
    trace_verbose=True
)

# File-based LTM
engine = YAMLEngine(ltm_path="./agent_memory.db")

# Custom memory backend
from the_edge_agent import InMemoryBackend
engine = YAMLEngine(memory_backend=InMemoryBackend())
```

### Resume from Checkpoint

```python
# Load with checkpoint
graph = engine.load_from_file("agent.yaml", checkpoint="./chk/state.pkl")

# Or use convenience method
for event in engine.resume_from_checkpoint(
    "agent.yaml",
    "./checkpoints/review_node_1733500000.pkl",
    config={"approved": True}
):
    print(event)
```

---

## Best Practices

1. **Keep nodes focused**: Each node should do one thing well
2. **Use meaningful names**: Node and edge names should be descriptive
3. **Leverage built-in actions**: Don't reinvent common operations
4. **Document with comments**: YAML supports `# comments`
5. **Version control**: Keep YAML configs in git
6. **Test incrementally**: Use interrupts to debug complex flows
7. **Validate state schema**: Define expected state structure upfront
8. **Use namespaces for imports**: Prevent action name collisions

---

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

**Issue**: Import module not found
- **Solution**: Check path is relative to YAML file location, not working directory

### Prolog Integration Issues

**Issue**: "SWI-Prolog not found" or "janus-swi not found"
- **Solution (Python)**: Install SWI-Prolog 9.1+ and janus-swi:
  ```bash
  # Ubuntu/Debian
  sudo apt-add-repository ppa:swi-prolog/stable
  sudo apt update && sudo apt install swi-prolog
  pip install janus-swi
  ```

**Issue**: "Prolog feature not enabled" (Rust)
- **Solution**: Build with Prolog feature:
  ```bash
  cargo build --features prolog
  cargo run --features prolog -- run my-agent.yaml
  ```

**Issue**: "Arguments are not sufficiently instantiated"
- **Solution**: Ensure variables are bound before arithmetic operations:
  ```prolog
  % Wrong
  Result is X + 1.  % X not bound

  % Correct
  state(value, X),
  Result is X + 1.
  ```

**Issue**: "Unknown procedure: predicate/N"
- **Solution**: Define predicates before use, or use inline conditional logic:
  ```prolog
  % Instead of calling undefined helper/1
  (X > 0 -> Result = positive ; Result = non_positive).
  ```

**Issue**: `return/2` not updating state (Rust only)
- **Solution**: This is a known limitation in Rust TEA. Use Lua nodes for state updates:
  ```yaml
  - name: prolog_validate
    language: prolog
    run: |
      state(value, V), V > 0.  % Just validate

  - name: lua_update
    language: lua
    run: |
      return { validated = true }  % Update state here
  ```

**Issue**: Prolog query timeout
- **Solution**: Increase timeout for complex constraint solving:
  ```python
  engine = YAMLEngine(prolog_enabled=True, prolog_timeout=60.0)
  ```

**Issue**: CLP(FD) constraints not working
- **Solution**: CLP(FD) is pre-loaded in both Python and Rust. Ensure you use proper CLP(FD) syntax:
  ```prolog
  X in 1..10,           % Domain declaration
  X + Y #= 15,          % Constraint (use #= not =)
  label([X, Y]).        % Find concrete values
  ```

---

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
| Imports | N/A | `imports:` |
