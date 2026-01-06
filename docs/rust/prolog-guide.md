# Rust Prolog Integration Guide

This guide covers Prolog scripting support in Rust TEA, enabling neurosymbolic AI workflows that combine Rust's performance with Prolog's logical reasoning capabilities.

## Prerequisites

### System Requirements

1. **Rust 1.70+** with Cargo
2. **SWI-Prolog 9.1+** (system-wide installation required)
3. **pkg-config** (for locating SWI-Prolog libraries)

### Installing SWI-Prolog

```bash
# Ubuntu/Debian (recommended PPA for 9.1+)
sudo apt-add-repository ppa:swi-prolog/stable
sudo apt update
sudo apt install swi-prolog swi-prolog-nox

# macOS
brew install swi-prolog

# Fedora
sudo dnf install pl

# Windows
# Download from https://www.swi-prolog.org/download/stable
# or: choco install swi-prolog

# Verify version (must be 9.1+)
swipl --version
```

### Verify pkg-config Can Find SWI-Prolog

```bash
# Check if swipl is found
pkg-config --modversion swipl

# If not found, you may need to set PKG_CONFIG_PATH
export PKG_CONFIG_PATH=$(swipl --dump-runtime-variables | grep PLBASE | cut -d= -f2 | tr -d '"')/lib/pkgconfig:$PKG_CONFIG_PATH
```

## Enabling Prolog in Rust TEA

Prolog is an **optional feature** in Rust TEA. Enable it during build:

```bash
# Build with Prolog support
cargo build --features prolog

# Run with Prolog support
cargo run --features prolog -- run my-agent.yaml

# Run tests with Prolog
cargo test --features prolog
```

### Cargo.toml Configuration

If using TEA as a dependency:

```toml
[dependencies]
the_edge_agent = { version = "0.7", features = ["prolog"] }
```

## Architecture: Prolog-Side Parsing

As of v0.7.8, Rust TEA uses **Prolog-side parsing** via `tea_load_code/1`, achieving full parity with Python TEA.

### How It Works

1. TEA predicates are loaded from `tea_prolog_predicates.pl` via `consult/1`
2. User code is passed to `tea_load_code/1` which uses Prolog's native `read_term/3`
3. Each term is classified by Prolog itself using `tea_process_term/1`:
   - **Directives**: `(:-Body)` → executed immediately
   - **Rules**: `(Head :- Body)` → asserted with `assertz/1`
   - **Facts**: Ground compound terms (detected by `tea_is_fact/1`)
   - **Queries**: Everything else → called directly
4. User-asserted facts are cleaned up via `tea_cleanup_facts/0`

### Why Prolog-Side Parsing?

This approach provides:
- **100% accurate parsing** - Prolog parses Prolog syntax correctly
- **No edge case bugs** - Handles commas in quotes, operators, etc.
- **Cross-runtime parity** - Same architecture as Python TEA
- **Simpler maintenance** - No regex patterns to maintain

### TEA Predicates

The following predicates are defined in `rust/src/engine/tea_prolog_predicates.pl`:
- `tea_load_code/1` - Entry point: loads code from string
- `tea_process_term/1` - Classifies and processes each term
- `tea_is_fact/1` - Determines if a term should be asserted
- `tea_action_predicate/1` - Lists predicates that should be called, not asserted
- `tea_cleanup_facts/0` - Cleans up user-asserted facts

### Historical Note

Before v0.7.8, Rust used host-side heuristic parsing with regex patterns.
This legacy code is preserved as `execute_node_code_legacy()` for reference.

## Writing Prolog Nodes

### Method 1: Language Attribute (Recommended)

```yaml
- name: process
  language: prolog
  run: |
    state(value, V),
    Result is V * 2,
    return(result, Result).
```

### Method 2: Marker Comment

```yaml
- name: process
  run: |
    % prolog
    state(value, V),
    Result is V * 2,
    return(result, Result).
```

### Method 3: Explicit Type

```yaml
- name: process
  run:
    type: prolog
    code: |
      state(value, V),
      Result is V * 2,
      return(result, Result).
```

## State Interface

### Reading State: `state/2`

```prolog
% Read a single value
state(key_name, Value),

% Read multiple values
state(input, Input),
state(count, Count),
```

### Writing State: `return/2`

Use `return/2` to update state from Prolog nodes:

```prolog
% Return a computed value
state(value, V),
Result is V * 2,
return(result, Result).

% Return multiple values
return(a, 1),
return(b, 2),
return(status, 'completed').

% Last-write-wins for duplicate keys
return(x, 1),
return(x, 2),
return(x, 3).  % State will have x = 3
```

**Cross-Runtime Parity:** The `return/2` predicate works identically in both Python and Rust TEA implementations.

**Security Note:** Using `return/2` or `state/2` disables sandbox protection for that query, since these predicates require dynamic assertions that the sandbox blocks. Avoid mixing state/return predicates with potentially dangerous operations.

## Inline Rule Definitions

You can define custom Prolog rules inline within a node's code block. Rules are automatically detected and asserted before the main query executes, then cleaned up afterward.

### Basic Syntax

```yaml
- name: apply_rule
  language: prolog
  run: |
    % Define a rule inline
    add_ten(X, Y) :- Y is X + 10.

    % Use state and apply rule
    state(doubled, D),
    add_ten(D, R),
    return(result, R).
```

### Multiple Rules

```yaml
- name: compute_with_rules
  language: prolog
  run: |
    % Multiple rule definitions
    double(X, Y) :- Y is X * 2.
    add_five(X, Y) :- Y is X + 5.

    % Chain the rules
    state(value, V),
    double(V, D),
    add_five(D, R),
    return(result, R).
```

### Complex Rule Bodies

Rules can have complex bodies with multiple goals, cuts, and conditionals:

```yaml
- name: categorize
  language: prolog
  run: |
    categorize(X, large) :- X >= 100.
    categorize(X, medium) :- X >= 50, X < 100.
    categorize(X, small) :- X < 50.

    state(value, V),
    categorize(V, Category),
    return(category, Category).
```

### Multi-Line Rules

Rules can span multiple lines until the period terminator:

```yaml
- name: complex_rule
  language: prolog
  run: |
    validate_and_process(Input, Output) :-
        Input > 0,
        Input < 1000,
        Temp is Input * 2,
        Output is Temp + 10.

    state(value, V),
    validate_and_process(V, R),
    return(result, R).
```

### How It Works (Prolog-Side Parsing)

As of v0.7.8, the Rust implementation uses **Prolog-side parsing** via the
`tea_load_code/1` predicate, which delegates parsing to SWI-Prolog's native
`read_term/3`. This ensures 100% accurate parsing of all Prolog syntax,
including edge cases like commas in quoted strings.

1. **Code Submission**: User code is passed to `tea_load_code/1`
2. **Native Parsing**: SWI-Prolog's `read_term/3` parses each term
3. **Classification**: Each term is classified as:
   - **Directive**: `(:-Body)` - executed immediately
   - **Rule**: `(Head :- Body)` - asserted using `assertz/1`
   - **Fact**: Ground compound terms - asserted to knowledge base
   - **Query**: Everything else - executed immediately
4. **Cleanup**: User-asserted facts are cleaned up after execution

This approach matches Python's janus-swi integration for cross-runtime parity.

### Isolation Between Nodes

Rules are cleaned up after each node execution, ensuring:
- No pollution between nodes
- Parallel execution remains thread-safe (rules are per-engine)
- Same code works in different nodes without conflicts

## Pre-Loaded Modules

The following modules are automatically available without explicit imports:

| Module | Description | Example Predicates |
|--------|-------------|-------------------|
| `lists` | List manipulation | `member/2`, `append/3`, `reverse/2`, `length/2` |
| `clpfd` | Finite domain constraints | `#=`, `#<`, `in`, `label/1` |
| `apply` | Higher-order predicates | `maplist/2`, `include/3`, `foldl/4` |
| `aggregate` | Aggregation | `aggregate_all/3` |

This means CLP(FD) constraints work immediately:

```yaml
- name: solve
  language: prolog
  run: |
    % No explicit import needed!
    X in 1..10,
    Y in 1..10,
    X + Y #= 15,
    label([X, Y]).
```

## Complete Examples

### Example 1: Compute and Return

Use `return/2` to update state directly from Prolog:

```yaml
name: prolog-compute
state_schema:
  value: int
  result: int
  doubled: int

nodes:
  - name: compute
    language: prolog
    run: |
      state(value, V),
      Result is V * 2,
      Doubled is V + V,
      return(result, Result),
      return(doubled, Doubled).

edges:
  - from: __start__
    to: compute
  - from: compute
    to: __end__
```

### Example 2: Validation Pattern

Use Prolog for validation with conditional edges:

```yaml
name: prolog-validation
state_schema:
  value: int
  is_valid: bool

nodes:
  - name: validate
    language: prolog
    run: |
      state(value, V),
      V > 0,        % Must be positive
      V < 1000,     % Must be < 1000
      0 is V mod 2, % Must be even
      return(is_valid, true).

  - name: on_invalid
    language: lua
    run: |
      return { is_valid = false }

edges:
  - from: __start__
    to: validate
  - from: validate
    to: __end__
    condition: true  # Prolog succeeded
  - from: validate
    to: on_invalid
    condition: false # Prolog failed
  - from: on_invalid
    to: __end__
```

### Example 3: CLP(FD) Constraint Solving

```yaml
name: constraint-solver
state_schema:
  x: int
  y: int
  target_sum: int

nodes:
  - name: solve
    language: prolog
    run: |
      state(target_sum, Sum),
      X in 1..100,
      Y in 1..100,
      X + Y #= Sum,
      X #< Y,
      label([X, Y]).

edges:
  - from: __start__
    to: solve
  - from: solve
    to: __end__
```

### Example 4: Knowledge Graph Validation

```yaml
name: kg-validation
state_schema:
  person: str
  relationship: str
  target: str
  valid: bool

nodes:
  - name: check_relationship
    language: prolog
    run: |
      state(person, P),
      state(relationship, R),
      state(target, T),

      % Valid relationships in our domain
      (R = parent ; R = sibling ; R = spouse),

      % Convert to atoms for matching
      (atom(P) -> PA = P ; atom_string(PA, P)),
      (atom(T) -> TA = T ; atom_string(TA, T)),

      % Simple validation
      PA \= TA.  % Can't relate to self

  - name: mark_valid
    language: lua
    run: |
      return { valid = true }

  - name: mark_invalid
    language: lua
    run: |
      return { valid = false }

edges:
  - from: __start__
    to: check_relationship
  - from: check_relationship
    to: mark_valid
    condition: true
  - from: check_relationship
    to: mark_invalid
    condition: false
```

## Rust API Usage

### Programmatic Loading

```rust
use the_edge_agent::{YamlEngine, Executor};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load YAML agent
    let engine = YamlEngine::new();
    let graph = engine.load_from_file("my-agent.yaml")?;

    // Compile the graph
    let compiled = graph.compile()?;

    // Create executor
    let executor = Executor::new(compiled)?;

    // Run with initial state
    let initial_state = serde_json::json!({
        "value": 42
    });

    let final_state = executor.invoke(initial_state)?;
    println!("Result: {:?}", final_state);

    Ok(())
}
```

### Feature Detection

```rust
#[cfg(feature = "prolog")]
fn run_prolog_agent() {
    // Prolog-specific code
}

#[cfg(not(feature = "prolog"))]
fn run_prolog_agent() {
    eprintln!("Prolog feature not enabled. Build with: cargo build --features prolog");
}
```

## Sandbox and Security

Prolog code runs in a sandboxed environment. The following are **restricted**:

- File I/O (`open/3`, `read/1`, `write/1`, `read_term/2`)
- Shell execution (`shell/1`, `process_create/3`)
- Network access
- Module loading from filesystem

**Safe predicates** remain available:
- Arithmetic operations
- List manipulation
- CLP(FD) constraints
- `findall/3`, `aggregate_all/3`
- Term manipulation

## Timeout Protection

All Prolog queries have timeout protection (default: 30 seconds).

If a query exceeds the timeout, execution fails gracefully.

## Runtime Comparison: Python vs Rust

| Feature | Python (janus-swi) | Rust (swipl-rs) |
|---------|-------------------|-----------------|
| **Bindings** | janus-swi (official) | swipl-rs (community) |
| **SWI-Prolog Version** | 9.1+ required | 9.1+ recommended |
| **`return/2` support** | Full support | Full support |
| **Module pre-loading** | Auto (clpfd, lists, apply, aggregate) | Auto (clpfd, lists, apply, aggregate) |
| **`state/2` support** | Full support | Full support |
| **Sandbox** | Default enabled | Default enabled* |
| **Timeout protection** | 30s default | 30s default |
| **Thread safety** | Thread-local predicates | RwLock + state caching |

\* Sandbox is automatically disabled when using `state/2` or `return/2` predicates due to technical constraints with dynamic assertions.

## Best Practices for Portable Agents

To write YAML agents that work in both Python and Rust:

1. **Use `state/2` for reading** - Works identically in both runtimes
2. **Use `return/2` for writing** - Now supported in both runtimes
3. **Use Prolog for validation** - Success/failure patterns work everywhere
4. **Keep security in mind** - Using TEA predicates disables sandbox

```yaml
# Portable pattern: Pure Prolog compute and return
nodes:
  - name: compute
    language: prolog
    run: |
      state(value, V),
      V >= 0,
      V =< 100,
      Doubled is V * 2,
      return(doubled, Doubled),
      return(status, 'processed').

edges:
  - from: __start__
    to: compute
  - from: compute
    to: __end__
```

## Troubleshooting

### "Prolog feature not enabled"

Build with the Prolog feature:

```bash
cargo build --features prolog
cargo run --features prolog -- run my-agent.yaml
```

### "Cannot find SWI-Prolog"

1. Verify SWI-Prolog is installed:
   ```bash
   swipl --version
   ```

2. Check pkg-config can find it:
   ```bash
   pkg-config --modversion swipl
   ```

3. Set PKG_CONFIG_PATH if needed:
   ```bash
   export PKG_CONFIG_PATH=$(swipl --dump-runtime-variables | grep PLBASE | cut -d= -f2 | tr -d '"')/lib/pkgconfig:$PKG_CONFIG_PATH
   ```

### "Failed to parse Prolog code: Exception"

This error indicates a Prolog exception occurred during execution. Common causes:

1. **Missing period** at the end of clauses
2. **Unbalanced parentheses**
3. **Invalid predicate names** (must start with lowercase)
4. **Undefined predicates**
5. **Trying to assert built-in predicates** (e.g., conjunctions like `foo, bar` as facts)

Debug by testing Prolog code directly:

```bash
swipl
?- state(value, V), V2 is V * 2, return(result, V2).
```

### "No permission to modify static procedure"

This error occurs when `tea_load_code/1` tries to assert a term that looks like a fact
but is actually a built-in operator. Common cases:

1. **Conjunctions without definitions**: `5 > 3.` (comparison without assignment)
2. **Operators as top-level terms**: `X = 5.` (unification without context)

**Solution**: Ensure your code uses `return/2` to return values:

```prolog
% Wrong - tries to assert (>) which is a built-in
5 > 3.

% Correct - executes comparison and returns result
(5 > 3 -> R = true ; R = false), return(result, R).
```

### "Unknown procedure: state/2" or "Unknown procedure: return/2"

This indicates TEA predicates weren't loaded properly. Causes:

1. **Missing tea_prolog_predicates.pl file** - Rust looks for this at:
   - `$CARGO_MANIFEST_DIR/src/engine/tea_prolog_predicates.pl` (dev/test)
   - Falls back to embedded predicates

2. **consult/1 failed** - The file exists but couldn't be loaded

**Debug steps**:
```bash
# Verify the predicates file exists
ls rust/src/engine/tea_prolog_predicates.pl

# Test loading it directly
swipl -g "consult('rust/src/engine/tea_prolog_predicates.pl'), listing(tea_load_code/1)" -t halt
```

### Sandbox Violations in YAML Agents

Note: As of v0.7.8, sandbox is NOT enforced in `execute_node_code` to ensure
compatibility with TEA predicates (`state/2`, `return/2`). This matches Python's
janus-swi behavior.

If you need sandboxed execution for untrusted code, use the `execute()` method
directly with `sandbox=true` in the runtime configuration.

### Build Fails on Linux

Ensure development headers are installed:

```bash
# Ubuntu/Debian
sudo apt install swi-prolog-nox

# Fedora
sudo dnf install pl-devel
```

### Build Fails on macOS

Ensure Homebrew paths are correct:

```bash
brew install swi-prolog
export PKG_CONFIG_PATH=/opt/homebrew/lib/pkgconfig:$PKG_CONFIG_PATH
```

## Build from Source

If building TEA from source with Prolog:

```bash
# Clone repository
git clone https://github.com/your-org/the_edge_agent.git
cd the_edge_agent/rust

# Build with Prolog
cargo build --features prolog

# Run tests
cargo test --features prolog

# Install CLI
cargo install --path . --features prolog
```

## Related Documentation

- [YAML Reference](../shared/YAML_REFERENCE.md)
- [Neurosymbolic Patterns Guide](https://github.com/fabceolin/the_edge_agent/blob/main/docs/shared/architecture/neurosymbolic-patterns.md)
- [Prolog Examples](https://github.com/fabceolin/the_edge_agent/tree/main/examples/prolog)
- [Python Prolog Guide](../python/prolog-guide.md)
- [Rust Development Guide](./development-guide.md)
