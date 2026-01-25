# Advanced Runtimes: Lua & Prolog

> **Parent document:** [YAML Reference](../YAML_REFERENCE.md)
> **Related:** [Node Specification](./nodes.md)
> **Epic:** DOC-002 (YAML Reference Modularization)

## Overview

TEA supports alternative runtimes for specific use cases:
- **Lua**: Cross-runtime compatibility with the Rust implementation
- **Prolog**: Neurosymbolic AI workflows combining neural network outputs with symbolic reasoning

Both runtimes are sandboxed for security and have timeout protection.

---

## Table of Contents

- [Lua Runtime](#lua-runtime)
  - [Enabling Lua](#enabling-lua)
  - [Syntax Options](#syntax-options)
  - [Auto-Detection](#auto-detection)
  - [Lua Sandbox](#lua-sandbox)
  - [Timeout Protection](#timeout-protection)
  - [Cross-Runtime Compatibility](#cross-runtime-compatibility)
- [Prolog Runtime](#prolog-runtime)
  - [Enabling Prolog](#enabling-prolog)
  - [Module Pre-Loading](#module-pre-loading)
  - [State Interface](#state-interface)
  - [CLP(FD) Constraint Solving](#clpfd-constraint-solving)
  - [Prolog Sandbox](#prolog-sandbox)
  - [Timeout Protection](#timeout-protection-1)
  - [Runtime Comparison Table](#runtime-comparison-table)
  - [Neurosymbolic AI Use Cases](#neurosymbolic-ai-use-cases)
  - [Example: LLM + Prolog Validation](#example-llm--prolog-validation)

---

## Lua Runtime

### Enabling Lua

```python
engine = YAMLEngine(lua_enabled=True)
```

**Installation:**
```bash
pip install 'the_edge_agent[lua]'
# or
pip install lupa>=2.0
```

### Syntax Options

**Language attribute (recommended):**
```yaml
- name: process_lua
  language: lua
  run: |
    local result = {}
    result.value = state.value * 2
    result.message = state.name .. "!"
    return result
```

**Explicit marker (alternative):**
```yaml
- name: process_lua
  run: |
    -- lua
    local result = {}
    result.value = state.value * 2
    return result
```

### Auto-Detection

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

### Lua Sandbox

For security, the following Lua globals are removed:
- `os` - Operating system access
- `io` - File I/O operations
- `debug` - Debugging facilities
- `loadfile`, `dofile` - File loading

Safe libraries remain available: `string`, `math`, `table`, `pairs`, `ipairs`, `type`, `tostring`, `tonumber`.

### Timeout Protection

Lua code execution has a configurable timeout (default: 30 seconds):
```python
engine = YAMLEngine(lua_enabled=True, lua_timeout=10.0)  # 10 second timeout
```

### Cross-Runtime Compatibility

Lua code in YAML agents runs identically in both Python and Rust TEA implementations, enabling portable agents.

**Lua Version Compatibility (LuaJIT 2.1 vs Lua 5.4):**

The Python implementation uses **LuaJIT 2.1** (via `lupa`) while the Rust implementation uses **Lua 5.4** (via `mlua`). For cross-runtime compatibility, use the **portable subset** of Lua syntax.

| Feature | LuaJIT 2.1 (Python) | Lua 5.4 (Rust) | Portable Alternative |
|---------|---------------------|----------------|---------------------|
| Integer division | `math.floor(a/b)` | `a // b` | Use `math.floor(a/b)` |
| Bitwise ops | `bit.band()`, `bit.bor()` | `&`, `\|`, `~` | Avoid bitwise; use math |
| Const variables | Not supported | `local x <const>` | Use `local x = ...` |
| Close variables | Not supported | `local f <close>` | Avoid `<close>` |
| UTF-8 library | Not built-in | `utf8.*` | Use `string.len` for ASCII |
| Warning system | Not available | `warn()` | Avoid `warn()` |

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

---

## Prolog Runtime

### Enabling Prolog

**Python:**
```python
engine = YAMLEngine(prolog_enabled=True)
```

**Rust:**
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

**Installation (Python):**
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

**Installation (Rust):**
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

### Module Pre-Loading

Both Python and Rust runtimes automatically pre-load common modules at initialization:
- `lists` - List manipulation predicates (`member/2`, `append/3`, `reverse/2`, etc.)
- `clpfd` - Finite domain constraints (no `:- use_module` needed!)
- `apply` - Higher-order predicates (`maplist/2`, `include/3`, etc.)
- `aggregate` - Aggregation predicates (`aggregate_all/3`, etc.)

This means CLP(FD) constraints and list predicates work immediately in **both Python and Rust** without explicit module imports. YAML agents using these predicates are fully portable across runtimes.

### State Interface

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

Access state via `state/2` predicate and set return values via `return/2`:

```prolog
% Read state["value"] into V
state(value, V),

% Compute result
Result is V * 2,

% Set state["doubled"] = Result
return(doubled, Result).
```

**Rust Runtime Limitation:** The `return/2` predicate is recognized by auto-detection but **does not currently update the state** in the Rust runtime due to swipl-rs crate constraints. Workarounds for Rust:
1. Use Lua nodes for state manipulation
2. Use CLP(FD) constraints where labeled values can be extracted
3. Use Prolog purely for validation/logic checks (success/failure)

The Python runtime (janus-swi) fully supports `return/2` for state updates.

### CLP(FD) Constraint Solving

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

### Prolog Sandbox

For security, the sandbox restricts:
- File I/O operations (`open/3`, `read/1`, `write/1`)
- Shell execution (`shell/1`, `process_create/3`)
- Network access

Safe predicates remain available: arithmetic, list operations, `findall`, `aggregate_all`, CLP(FD), etc.

### Timeout Protection

Prolog code execution has a configurable timeout (default: 30 seconds):
```python
engine = YAMLEngine(prolog_enabled=True, prolog_timeout=10.0)  # 10 second timeout
```

### Runtime Comparison Table

| Feature | Python (janus-swi) | Rust (swipl-rs) |
|---------|-------------------|-----------------|
| **Bindings** | janus-swi (official) | swipl-rs (community) |
| **SWI-Prolog Version** | 9.1+ required | 9.1+ recommended |
| **`return/2` support** | Full support | Limited (doesn't update state) |
| **Module pre-loading** | Auto (clpfd, lists, apply, aggregate) | Auto (clpfd, lists, apply, aggregate) |
| **`state/2` support** | Full support | Full support |
| **Sandbox** | Default enabled | Default enabled |
| **Timeout protection** | 30s default | 30s default |
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

### Neurosymbolic AI Use Cases

1. **Validate LLM outputs** with logical rules
2. **Constraint-based reasoning** for scheduling, planning
3. **Knowledge graph queries** with Datalog-style rules
4. **Formal verification** of neural network predictions
5. **Ontology reasoning** with OWL/RDF-style inference

### Example: LLM + Prolog Validation

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

---

## See Also

- [Node Specification](./nodes.md) - All execution methods
- `examples/prolog/neurosymbolic/` - Example agents
- [Navigation & Flow](./navigation.md) - Workflow routing
- [Actions Overview](./actions/README.md) - Built-in action reference
