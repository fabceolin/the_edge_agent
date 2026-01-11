# YAML Agent Reference

Version: 0.9.36

Complete reference for declarative agent configuration in The Edge Agent using YAML files.

## Table of Contents

- [Overview](#overview)
- [Security Notice](#security-notice)
- [Basic Structure](#basic-structure)
- [State and Variable Passing](#state-and-variable-passing)
- [Document Structure](#document-structure)
- [Top-Level Keys](#top-level-keys)
  - [imports](#imports)
  - [endpoint](#endpoint-optional) (HTTP API configuration)
- [Node Specification](#node-specification)
- [Navigation and Flow Control](#navigation-and-flow-control)
- [Edge Specification](#edge-specification-deprecated) (sequential edges deprecated, parallel edges supported)
- [Template Syntax](#template-syntax)
- [Built-in Actions](#built-in-actions)
- [Extraction Validation](#extraction-validation)
- [Checkpoint Persistence](#checkpoint-persistence)
- [Complete Examples](#complete-examples)
- [Python API](#python-api)
- [YAML Overlay Merging](#yaml-overlay-merging)
- [Best Practices](#best-practices)
- [Troubleshooting](#troubleshooting)
- [Parallel Execution Strategies](#parallel-execution-strategies)
- [Stream Channels](#stream-channels)
- [Comparison with GitHub Actions](#comparison-with-github-actions)

---

## Documentation Structure

This reference is organized into focused modules for easier navigation:

### Core Concepts (this document)
- Overview, Security, Basic Structure
- State and Variable Passing
- Document Structure, Top-Level Keys

### Detailed References
| Topic | Document |
|-------|----------|
| Node Specification | [yaml-reference/nodes.md](./yaml-reference/nodes.md) |
| Navigation & Flow | [yaml-reference/navigation.md](./yaml-reference/navigation.md) |
| Template Syntax | [yaml-reference/templates.md](./yaml-reference/templates.md) |
| Lua & Prolog | [yaml-reference/advanced-runtimes.md](./yaml-reference/advanced-runtimes.md) |
| Extraction Validation | [yaml-reference/actions/specialized.md](./yaml-reference/actions/specialized.md#validation-actions) |
| Stream Channels | [yaml-reference/streams.md](./yaml-reference/streams.md) |

### Built-in Actions
| Category | Document |
|----------|----------|
| Overview | [yaml-reference/actions/README.md](./yaml-reference/actions/README.md) |
| LLM Actions | [yaml-reference/actions/llm.md](./yaml-reference/actions/llm.md) |
| I/O Actions | [yaml-reference/actions/io.md](./yaml-reference/actions/io.md) |
| Data Processing | [yaml-reference/actions/data.md](./yaml-reference/actions/data.md) |
| Memory Actions | [yaml-reference/actions/memory.md](./yaml-reference/actions/memory.md) |
| Reasoning Actions | [yaml-reference/actions/reasoning.md](./yaml-reference/actions/reasoning.md) |
| Planning Actions | [yaml-reference/actions/planning.md](./yaml-reference/actions/planning.md) |
| Integrations | [yaml-reference/actions/integrations.md](./yaml-reference/actions/integrations.md) |
| Specialized (incl. Secrets) | [yaml-reference/actions/specialized.md](./yaml-reference/actions/specialized.md) |

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

This is similar to running any Python script--the YAML author has full access to:
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
Lua code blocks are sandboxed with dangerous globals removed (`os`, `io`, `debug`, `loadfile`, `dofile`). However, this is not a complete security boundary--the Python host process still has full access.

**Prolog sandbox (when `prolog_enabled=True`):**
Prolog code blocks use SWI-Prolog's sandbox library with dangerous predicates restricted (file I/O, shell execution, network access). Timeouts prevent runaway queries. This is not a complete security boundary--the Python host process still has full access.

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
{user_query: "AI"}  ->  {processed_query}  ->  {search_results}  ->  All keys merged
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

### `input_schema` (optional)

Defines validation rules for input data. When provided, input is validated before graph execution, returning a 422 error if validation fails. Supports type coercion, required fields, defaults, and constraints.

```yaml
input_schema:
  # Required string with length constraints
  query:
    type: str
    required: true
    min_length: 1
    max_length: 1000

  # Optional string with regex pattern
  session_id:
    type: str
    pattern: "^[a-f0-9-]{36}$"

  # Number with range and default
  max_results:
    type: int
    default: 5
    min: 1
    max: 100

  # Enum choices
  output_format:
    type: str
    default: "json"
    choices: ["json", "text", "markdown"]

  # Nested object validation
  options:
    type: dict
    properties:
      temperature:
        type: float
        default: 0.7
        min: 0.0
        max: 2.0
      model:
        type: str
        default: "gpt-4o-mini"

  # List with item validation
  tags:
    type: list
    items:
      type: str
      max_length: 50
```

**Supported Types:** `str`, `int`, `float`, `bool`, `list`, `dict`

**Constraints:**
| Constraint | Types | Description |
|------------|-------|-------------|
| `required` | all | Error if field is missing |
| `default` | all | Value to use when field is missing |
| `min_length` | str | Minimum string length |
| `max_length` | str | Maximum string length |
| `pattern` | str | Regex pattern to match |
| `min` | int, float | Minimum numeric value |
| `max` | int, float | Maximum numeric value |
| `choices` | all | List of allowed values |
| `properties` | dict | Nested field schemas |
| `items` | list | Schema for list elements |

**Type Coercion:** String values are automatically coerced to the target type when possible (e.g., `"123"` → `123`, `"true"` → `True`).

**Validation Error Response:**
```json
{
  "type": "validation_error",
  "status_code": 422,
  "errors": [
    {
      "field": "query",
      "error": "required",
      "message": "Field 'query' is required"
    },
    {
      "field": "options.temperature",
      "error": "max",
      "message": "Field 'options.temperature' must be at most 2.0",
      "value": 3.0,
      "constraint": 2.0
    }
  ]
}
```

### `endpoint` (optional)

Defines the HTTP endpoint configuration for exposing this agent as a REST API. When present, the agent can be auto-registered with a route registry and its OpenAPI specification is automatically generated.

```yaml
endpoint:
  # Required: URL path (must start with /)
  path: "/api/v1/research"

  # HTTP method (default: POST)
  method: POST

  # OpenAPI metadata
  summary: "Execute research query"
  description: "Searches the web and synthesizes an answer with citations."
  tags:
    - Research
    - AI Agents

  # Authentication override (default: required)
  auth:
    required: true
    roles:
      - user
      - admin

  # Path parameters (extracted from URL)
  path_params:
    user_id:
      type: int
      description: "User identifier"
      pattern: "^[0-9]+$"

  # Query parameters (URL ?param=value)
  query_params:
    limit:
      type: int
      required: false
      default: 10
      min: 1
      max: 100
      description: "Maximum results"

    format:
      type: str
      default: "json"
      map_to: output_format  # Maps to different state key

  # Request body configuration
  request:
    content_type: "application/json"
    schema_ref: input_schema  # Links to input_schema section

  # Response configuration
  response:
    content_type: "application/json"
    schema_ref: output_schema
    examples:
      success:
        summary: "Successful research result"
        value:
          status: "success"
          result: "Research findings..."
          citations: ["https://example.com"]
```

**HTTP Methods:** `GET`, `POST`, `PUT`, `DELETE`, `PATCH`

**Path Parameters:**
- Extracted from `{param_name}` placeholders in path
- Auto-created as `str` type if not explicitly defined
- Type-converted and validated before graph execution

**Query Parameters:**
| Property | Type | Description |
|----------|------|-------------|
| `type` | str | `str`, `int`, `float`, `bool`, `list` |
| `required` | bool | Error if missing (default: false) |
| `default` | any | Value when not provided |
| `min`/`max` | number | Range constraints (int/float) |
| `min_length`/`max_length` | int | Length constraints (str/list) |
| `map_to` | str | State key to store value (default: param name) |
| `description` | str | OpenAPI description |

**Auth Configuration:**
| Property | Default | Description |
|----------|---------|-------------|
| `required` | true | Require authentication |
| `roles` | [] | Required roles (empty = any authenticated user) |

**OpenAPI Generation:**
When an agent has an `endpoint` section, its OpenAPI specification includes:
- Path/query parameters with types and validation
- Request body schema (from `input_schema`)
- Response schema (from `output_schema`)
- Security requirements (from `auth`)
- Tags and operation metadata

**Python Integration:**
```python
from the_edge_agent import YAMLEngine
from the_edge_agent.http import RouteRegistry, generate_openapi_from_routes

# Load agent with endpoint config
engine = YAMLEngine()
graph = engine.load_from_file("agent.yaml")

# Access endpoint config
if engine.endpoint_config:
    print(f"Endpoint: {engine.endpoint_config.method} {engine.endpoint_config.path}")

# Register with route registry
registry = RouteRegistry()
registry.register(
    agent_name=graph._agent_name,
    config=graph._endpoint_config,
    handler=lambda state: list(graph.invoke(state)),
    input_schema=agent_config.get("input_schema"),
    output_schema=agent_config.get("output_schema"),
)

# Generate OpenAPI spec
spec = generate_openapi_from_routes(registry, "my-api", "1.0.0")
```

### `nodes` (required)
Array of node definitions. See [Node Specification](#node-specification).

### `edges` (deprecated)

> **Deprecation Notice (v0.8.x):** Sequential edges are deprecated in favor of implicit chaining and `goto` properties on nodes. See [Navigation and Flow Control](#navigation-and-flow-control) for the new syntax.
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

### `settings` (optional)

Runtime configuration for the agent including authentication, LLM defaults, and infrastructure.

#### `settings.auth` - Authentication (TEA-BUILTIN-015.3)

Configure authentication for securing agent endpoints:

```yaml
settings:
  auth:
    provider: firebase         # Required: firebase | jwt | api_key | none
    token_header: "Authorization"  # Header containing token (default)
    # token_query_param: "api_key"  # Alternative: token from query param
    required: true             # 401 if auth fails (default: true)
    inject_user: true          # Inject user into state (default: true)
    user_state_key: "__user__" # State key for user info (default: __user__)

    # Claims mapping (optional) - map token claims to user fields
    claims_mapping:
      user_id: uid             # state.__user__.user_id = token.uid
      user_email: email
      roles: custom_claims.roles

    # Provider-specific config
    firebase:
      project_id: "${FIREBASE_PROJECT_ID}"

    jwt:
      secret: "${JWT_SECRET}"
      # public_key_path: "/path/to/public.pem"  # For RS256
      algorithms: ["HS256"]
      issuer: "https://your-issuer.com"
      audience: "your-api"

    api_key:
      keys:
        - "${API_KEY_1}"
        - "${API_KEY_2}"
      # keys_file: "/path/to/keys.txt"  # Load from file
```

**Provider Types:**

| Provider | Description | Dependencies |
|----------|-------------|--------------|
| `firebase` | Firebase ID token verification | `firebase-admin>=6.2.0` |
| `jwt` | Generic JWT verification | `PyJWT>=2.8.0` |
| `api_key` | Simple API key matching | None |
| `none` | No authentication (default) | None |

**User Info Injection:**

When `inject_user: true`, the authenticated user is available in state:

```yaml
nodes:
  - name: greet_user
    run: |
      user = state.get("__user__")
      if user:
          return {"greeting": f"Hello, {user['email']}!"}
      return {"greeting": "Hello, anonymous!"}
```

**Required vs Optional Auth:**

- `required: true` (default): Returns 401 error if token is missing/invalid
- `required: false`: Sets `__user__` to None and continues execution

See [Cloud Production Actions](./yaml-reference/actions/cloud-production.md) for `auth.verify` and `auth.get_user` actions.

#### `settings.llm` - LLM Configuration (TEA-RELEASE-004)

Configure LLM defaults for the workflow, including backend selection and model settings:

```yaml
settings:
  llm:
    # Backend selection: "local", "api", or "auto" (default)
    backend: auto

    # Local model configuration
    model_path: ~/.cache/tea/models/gemma-3n-E4B-it-Q4_K_M.gguf
    n_ctx: 2048              # Context window size (default: 2048)
    n_gpu_layers: 0          # GPU layers: 0=CPU only, -1=all GPU

    # API backend defaults
    api_key: "${OPENAI_API_KEY}"  # API key (env var expansion supported)
    api_base: https://api.openai.com/v1  # Base URL

    # Shell providers (for shell CLI backend)
    shell_providers:
      claude:
        command: claude
        args: ["--model", "claude-3-opus"]
        timeout: 600
```

**Backend Selection:**

| Backend | Description |
|---------|-------------|
| `auto` | Prefer local if model available, fallback to API |
| `local` | Force local llama.cpp model, error if not found |
| `api` | Force API call (requires API key) |

**Model Path Resolution Order:**

1. `TEA_MODEL_PATH` environment variable
2. `params.model_path` in action
3. `settings.llm.model_path` in YAML
4. `$APPDIR/usr/share/models/` (AppImage bundle)
5. `~/.cache/tea/models/` (default cache)

**Example with Local Model:**

```yaml
name: offline-chat
settings:
  llm:
    backend: local
    model_path: ~/models/phi-4-mini.gguf
    n_gpu_layers: -1  # Use all GPU layers

nodes:
  - name: chat
    action: llm.chat
    params:
      prompt: "{{ state.question }}"
      max_tokens: 200
```

**Example with Auto Backend:**

```yaml
name: portable-agent
settings:
  llm:
    backend: auto  # Works offline with AppImage, online with API

nodes:
  - name: respond
    action: llm.chat
    params:
      prompt: "{{ state.input }}"
      system: "You are a helpful assistant."
```

See [Python Actions Reference](../python/actions-reference.md) and [Rust Actions Reference](../rust/actions-reference.md) for complete action documentation (search for "Local LLM Provider").

---

## Node Specification

> **Full documentation:** [yaml-reference/nodes.md](./yaml-reference/nodes.md)

Nodes are the building blocks of YAML agents. Each node represents a workflow step with one of these execution methods:

| Method | Syntax | Description |
|--------|--------|-------------|
| Inline Python | `run:` | Full Python code with state access |
| Script | `script:` | Alias for run (GitLab CI style) |
| Lua | `run:` + `-- lua` | Cross-runtime Lua code |
| Prolog | `language: prolog` | Neurosymbolic AI logic |
| Built-in Action | `uses:` | Pre-built actions |
| Multi-Step | `steps:` | Sequential steps within node |
| While Loop | `type: while_loop` | Iterative execution |
| Dynamic Parallel | `type: dynamic_parallel` | Fan-out/fan-in patterns |

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

For complete node specification including all execution methods, options, and examples, see [Node Specification](./yaml-reference/nodes.md).

---

## Navigation and Flow Control

> **Full documentation:** [yaml-reference/navigation.md](./yaml-reference/navigation.md)

TEA supports implicit chaining (nodes execute in order) and explicit `goto` properties for branching:

- **Implicit flow:** node1 -> node2 -> node3 -> __end__
- **Unconditional goto:** `goto: target_node`
- **Conditional goto:** `goto:` list with `if`/`to` rules
- **Loops:** Conditional goto back to earlier nodes

### Quick Example

```yaml
nodes:
  - name: classify
    run: |
      return {"category": "billing" if "bill" in state["query"] else "general"}
    goto:
      - if: "state['category'] == 'billing'"
        to: handle_billing
      - to: handle_general  # default case

  - name: handle_billing
    run: |
      return {"response": "Billing inquiry handled"}

  - name: handle_general
    run: |
      return {"response": "General inquiry handled"}
```

For complete navigation reference including deprecated edges, see [Navigation & Flow Control](./yaml-reference/navigation.md).

---

## Edge Specification (Deprecated)

> **Full documentation:** [yaml-reference/navigation.md](./yaml-reference/navigation.md)

> **Deprecation Notice:** Sequential edges are deprecated in favor of implicit chaining and `goto` properties.

**Exception:** Parallel edges (`parallel: true`, `fan_in:`) are **not deprecated** and remain the only way to define fan-out/fan-in patterns:

```yaml
edges:
  - from: prepare
    to: analyze_sentiment
    parallel: true
    fan_in: combine_results
  - from: prepare
    to: analyze_entities
    parallel: true
    fan_in: combine_results
```

See [Navigation & Flow Control](./yaml-reference/navigation.md) for migration guide and parallel edge syntax.

---

## Template Syntax

> **Full documentation:** [yaml-reference/templates.md](./yaml-reference/templates.md)

Templates use **Jinja2** syntax for variable interpolation:

| Syntax | Description |
|--------|-------------|
| `{{ state.key }}` | Access state value |
| `{{ variables.key }}` | Access global variable |
| `{{ secrets.key }}` | Access secret |
| `{{ value \| filter }}` | Apply filter |

### Common Filters

| Filter | Description |
|--------|-------------|
| `tojson` | Convert to JSON string |
| `fromjson` | Parse JSON string |
| `upper` / `lower` | Case conversion |
| `default(value)` | Fallback value |
| `length` | Collection length |
| `join(sep)` | Join list elements |

### Quick Example

```yaml
- name: format_output
  uses: http.post
  with:
    url: "{{ variables.api_endpoint }}/submit"
    body: "{{ state.results | tojson }}"
    headers:
      Authorization: "Bearer {{ secrets.api_key }}"
```

For complete template reference including conditionals and loops, see [Template Syntax](./yaml-reference/templates.md).

---

## Built-in Actions

> **Full documentation:** [Actions Index](./yaml-reference/actions/README.md)

TEA provides 100+ built-in actions organized by category:

| Category | Actions | Documentation |
|----------|---------|---------------|
| LLM | llm.call, llm.stream, llm.retry, llm.tools | [actions/llm.md](./yaml-reference/actions/llm.md) |
| I/O | http.*, file.*, storage.* | [actions/io.md](./yaml-reference/actions/io.md) |
| Data | json.*, csv.*, data.* | [actions/data.md](./yaml-reference/actions/data.md) |
| Memory | memory.*, ltm.*, cache.*, graph.* | [actions/memory.md](./yaml-reference/actions/memory.md) |
| Reasoning | reason.cot, reason.react, reason.self_correct, reason.decompose | [actions/reasoning.md](./yaml-reference/actions/reasoning.md) |
| Planning | plan.decompose, plan.execute, plan.replan, plan.status | [actions/planning.md](./yaml-reference/actions/planning.md) |
| Multi-Agent | agent.dispatch, agent.parallel, agent.sequential, agent.coordinate | [actions/agent.md](./yaml-reference/actions/agent.md) |
| A2A | a2a.send, a2a.receive, a2a.broadcast, a2a.delegate, a2a.state.* | [actions/a2a.md](./yaml-reference/actions/a2a.md) |
| Reflection | reflection.loop, reflection.evaluate, reflection.correct | [actions/reflection.md](./yaml-reference/actions/reflection.md) |
| Cloud Production | session.*, firestore.*, auth.*, validate.*, error.* | [actions/cloud-production.md](./yaml-reference/actions/cloud-production.md) |
| Integrations | web.*, vector.*, tools.* | [actions/integrations.md](./yaml-reference/actions/integrations.md) |
| Specialized | checkpoint.*, schema.*, secrets.*, ratelimit.* | [actions/specialized.md](./yaml-reference/actions/specialized.md) |

### Quick Examples

```yaml
# LLM call
- name: analyze
  uses: llm.call
  with:
    model: gpt-4
    prompt: "Analyze: {{ state.text }}"
  output: analysis

# HTTP request
- name: fetch_data
  uses: http.get
  with:
    url: "{{ variables.api_url }}"
  output: response

# Rate limiting
- name: limited_call
  uses: ratelimit.wrap
  with:
    limiter_name: api_limiter
    rate: 10
    period: 60
    action: llm.call
    action_params:
      model: gpt-4
      prompt: "{{ state.query }}"
```

For complete action reference with parameters and examples, see the [Actions Index](./yaml-reference/actions/README.md).

---

## Extraction Validation

> **Full documentation:** [yaml-reference/actions/specialized.md#validation-actions](./yaml-reference/actions/specialized.md#validation-actions)

TEA provides a 3-layer validation framework for LLM-extracted data:

### 1. Structural Validation

Schema-based field requirements using `extraction_schema`:

```yaml
extraction_schema:
  entities:
    required_fields: [name, type]
  relationships:
    types: [mother, father]
    required_fields: [type, subject, object]
```

### 2. Semantic Validation

Prolog constraint rules using `validation_constraints`:

```yaml
validation_constraints:
  language: prolog
  rules: |
    validation_error(self_parent, P) :-
        relationship(T, P, P), member(T, ['mother', 'father']).
```

### 3. Grounding Validation

LLM-verified semantic probes:

```yaml
semantic_probes:
  - for_each: relationship
    probe: "Is {{ subject }} the {{ type }} of {{ object }}?"
    on_fail: reject
```

For complete extraction validation reference including logging and examples, see [Specialized Actions](./yaml-reference/actions/specialized.md#validation-actions).

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

> **Full documentation:** [yaml-reference/examples.md](./yaml-reference/examples.md)

Complete, runnable example agents demonstrating various patterns:

| Example | Pattern | Features |
|---------|---------|----------|
| Research Agent | Conditional routing | State validation, file output, variables |
| Parallel Analyzer | Fan-out/Fan-in | Parallel edges, result collection |
| Customer Support | Intent classification | Multi-step nodes, conditional handlers |

For complete examples with full code, see [Complete Examples](./yaml-reference/examples.md).

See also: [examples/yaml/](https://github.com/fabceolin/the_edge_agent/tree/main/examples/yaml) for additional example agents.

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

## YAML Overlay Merging

YE.8: The Edge Agent supports kubectl-style overlay merging for composing YAML configurations.

### Overview

Overlay merging allows you to:
- Create a base agent configuration
- Apply environment-specific overrides (dev, staging, prod)
- Keep sensitive settings in separate files
- Compose configurations from reusable components

### CLI Usage

Both Python and Rust CLIs support the same overlay options:

```bash
# Apply single overlay
tea run base.yaml -f overlay.yaml

# Apply multiple overlays (applied in order, last wins)
tea run base.yaml -f dev.yaml -f secrets.yaml

# Preview merged config without executing
tea run base.yaml -f overlay.yaml --dump-merged
```

### Merge Semantics

The merge follows **kubectl-style strategic merge patch** semantics:

| Type | Behavior | Example |
|------|----------|---------|
| **Objects** | Recursively merged | Base `{a: 1}` + Overlay `{b: 2}` = `{a: 1, b: 2}` |
| **Arrays** | Replaced (not concatenated) | Base `[1,2,3]` + Overlay `[4,5]` = `[4,5]` |
| **Scalars** | Last wins | Base `count: 10` + Overlay `count: 20` = `count: 20` |
| **Null** | Can override non-null | Base `enabled: true` + Overlay `enabled: null` = `enabled: null` |

### Example: Environment Overlays

**base.yaml** (shared configuration):
```yaml
name: my-agent
settings:
  ltm:
    backend: sqlite
    path: ./data/
  model: gpt-4o-mini
  temperature: 0.7
nodes:
  - name: process
    uses: llm
```

**prod-overlay.yaml** (production overrides):
```yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: firestore
    storage:
      uri: gs://my-bucket/ltm/
  model: gpt-4o
  temperature: 0.3
```

**Run with overlay:**
```bash
tea run base.yaml -f prod-overlay.yaml
```

**Merged result:**
```yaml
name: my-agent  # Preserved from base
settings:
  ltm:
    backend: duckdb      # From overlay (replaces)
    path: ./data/        # Preserved from base (not in overlay)
    catalog:             # Added from overlay
      type: firestore
    storage:             # Added from overlay
      uri: gs://my-bucket/ltm/
  model: gpt-4o          # From overlay (replaces)
  temperature: 0.3       # From overlay (replaces)
nodes:                   # Preserved from base (arrays replace, but no nodes in overlay)
  - name: process
    uses: llm
```

### Debug Merged Configuration

Use `--dump-merged` to preview the final configuration:

```bash
# Output merged YAML without executing
tea run base.yaml -f overlay.yaml --dump-merged > merged.yaml

# Validate the merged result
tea validate merged.yaml
```

### Cross-Implementation Parity

Both Python and Rust implementations produce **identical merged output** for the same inputs. This is verified by the parity test suite.

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

> **Full documentation:** [yaml-reference/troubleshooting.md](./yaml-reference/troubleshooting.md)

Quick reference for common issues:

| Issue | Likely Cause | Quick Fix |
|-------|--------------|-----------|
| Template not rendering | Wrong syntax | Use `{{ state.key }}` not `${{ }}` |
| Node not found | Typo in goto | Check node names match exactly |
| Parallel not working | Missing fan-in | Define fan-in node, reference in edges |
| Prolog not found | Not installed | Install SWI-Prolog 9.1+ and janus-swi |

For complete troubleshooting guide including Prolog issues, see [Troubleshooting](./yaml-reference/troubleshooting.md).

---

## Parallel Execution Strategies

TEA supports three parallel execution strategies for fan-out edges, allowing you to choose the optimal execution backend for your workload.

### Strategy Comparison

| Strategy | Use Case | Pros | Cons |
|----------|----------|------|------|
| `thread` | I/O-bound tasks, API calls | Low overhead, shared memory | GIL limits CPU parallelism |
| `process` | CPU-bound tasks | True parallelism, bypasses GIL | Serialization overhead |
| `remote` | Distributed execution | Horizontal scaling | Network latency, setup complexity |

### Configuration

```yaml
# Global default
settings:
  parallel:
    strategy: thread  # thread | process | remote
    max_workers: 4

# Per-edge override
edges:
  - from: prepare
    to: [branch_a, branch_b]
    parallel: true
    parallel_strategy: process  # Overrides global
    fan_in: merge
```

### Thread Strategy (Default)

Best for I/O-bound operations like API calls, file reads, or network requests.

```yaml
settings:
  parallel:
    strategy: thread
    max_workers: 10  # Concurrent threads

edges:
  - from: prepare
    to: [fetch_api_a, fetch_api_b, fetch_api_c]
    parallel: true
    fan_in: combine_results
```

**Characteristics:**
- Shared memory (state modifications visible across threads)
- GIL prevents true CPU parallelism
- Low overhead, fast context switching
- Rate limiters and caches are shared

### Process Strategy

Best for CPU-bound operations like data processing, calculations, or transformations.

```yaml
settings:
  parallel:
    strategy: process
    max_workers: 4  # Concurrent processes

edges:
  - from: load_data
    to: [process_chunk_1, process_chunk_2, process_chunk_3]
    parallel: true
    parallel_strategy: process
    fan_in: merge_results
```

**Characteristics:**
- True parallelism, bypasses GIL
- State must be picklable (no lambdas, connections, file handles)
- Higher memory usage (process isolation)
- Rate limiters and caches are per-process

**Serialization Requirements:**
- ✅ `dict`, `list`, `str`, `int`, `float`, `bool`
- ✅ `dataclass` (with picklable fields)
- ❌ `lambda` functions
- ❌ Open file handles
- ❌ Database connections

### Remote Strategy

Best for distributed execution across multiple machines.

```yaml
settings:
  parallel:
    strategy: remote
    remote:
      hosts:
        - user@server1
        - user@server2
      basefile: ./tea
      workdir: /tmp/tea-jobs
      cleanup: true
      env_vars:
        include:
          - OPENAI_API_KEY
          - LOG_LEVEL
        exclude_patterns:
          - "*_SECRET"
        mode: ssh_env

edges:
  - from: prepare
    to: [analyze_region_1, analyze_region_2, analyze_region_3]
    parallel: true
    parallel_strategy: remote
    fan_in: aggregate
```

**Characteristics:**
- Horizontal scaling across machines
- State must be JSON-serializable
- Requires SSH access to remote hosts
- Full TEA engine runs on each remote
- Rate limiters and caches are per-host

**Requirements:**
- SSH key authentication configured
- TEA binary compatible with remote OS/arch
- Sufficient disk space on remotes

### Error Handling

| Strategy | Error Type | Behavior |
|----------|------------|----------|
| All | Timeout | Configurable via `ParallelConfig.timeout_seconds` |
| `process` | Pickle error | Fail fast with clear message before execution |
| `remote` | SSH auth failure | Fail fast with setup instructions |
| `remote` | Network timeout | Retry with exponential backoff |
| All | Partial failure | Configurable `fail_fast` or collect all results |

### Feature Interactions

| Feature | `thread` | `process` | `remote` |
|---------|----------|-----------|----------|
| Rate limiting | Shared | Per-process | Per-host |
| Caching | Shared | Per-process | Per-host |
| LTM | Shared | Shared | Distributed backend required |
| Interrupts | ✅ | ✅ | ❌ Not in remote scope |

---

## Stream Channels

> **Version**: 0.9.0+
> **Platforms**: Linux, macOS (Windows not supported)
> **Full documentation**: [yaml-reference/streams.md](./yaml-reference/streams.md)

Stream channels enable Unix-style pipe streaming between nodes for real-time data flow.

### Quick Example

```yaml
settings:
  parallel:
    strategy: process    # Required
    streams:
      enabled: true

nodes:
  - name: producer
    run: |
      import sys
      for i in range(1000):
        print(f"record_{i}", file=sys.stdout, flush=True)
      return {"count": 1000}
    streams:
      stdout: data_stream

  - name: consumer
    run: |
      import sys
      for line in sys.stdin:
        process(line)
      return {"status": "done"}
    streams:
      stdin: data_stream

edges:
  - from: __start__
    to: producer
  - from: producer
    to: consumer
  - from: consumer
    to: __end__
```

### Stream vs State

| Model | Transfer | Serialization | Checkpoint | Use Case |
|-------|----------|---------------|------------|----------|
| **State** | Discrete batches | JSON/Pickle | Yes | Structured data, metadata |
| **Stream** | Continuous flow | Raw bytes | No | Large files, logs, events |

### Broadcasting

```yaml
edges:
  - from: producer
    to: [consumer_a, consumer_b]
    parallel: true
    parallel_strategy: process
    stream_mode: broadcast  # All consumers get same data
    fan_in: merger
```

### Limitations

1. **No checkpointing** on stream nodes (`interrupt_before`/`interrupt_after` not allowed)
2. **Process strategy only** (`parallel_strategy: thread` not supported)
3. **Unix only** (Windows not supported)
4. **Single producer** per stream channel

For complete documentation including troubleshooting, see [Stream Channels Reference](./yaml-reference/streams.md).

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
