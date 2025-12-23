# Python Prolog Integration Guide

This guide covers Prolog scripting support in Python TEA, enabling neurosymbolic AI workflows that combine Python's machine learning ecosystem with Prolog's logical reasoning capabilities.

## Prerequisites

### System Requirements

1. **Python 3.9+** with TEA installed
2. **SWI-Prolog 9.1+** (system-wide installation required)

### Installing SWI-Prolog

```bash
# Ubuntu/Debian (recommended PPA for 9.1+)
sudo apt-add-repository ppa:swi-prolog/stable
sudo apt update
sudo apt install swi-prolog

# macOS
brew install swi-prolog

# Windows
# Download from https://www.swi-prolog.org/download/stable
# or: choco install swi-prolog

# Verify version (must be 9.1+)
swipl --version
```

### Installing Python Bindings

```bash
# Install TEA with Prolog support
pip install 'the_edge_agent[prolog]'

# Or install janus-swi directly
pip install janus-swi
```

## Enabling Prolog in YAMLEngine

```python
from the_edge_agent import YAMLEngine

# Enable Prolog support
engine = YAMLEngine(prolog_enabled=True)

# Optional: customize timeout (default: 30 seconds)
engine = YAMLEngine(prolog_enabled=True, prolog_timeout=10.0)
```

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

```prolog
% Return single value
return(result, 42).

% Return multiple values
return(status, ok),
return(message, 'Success').

% Return lists
return(items, [1, 2, 3]).

% Return compound terms (become dicts in Python)
return(person, _{name: 'Alice', age: 30}).
```

## Pre-Loaded Modules

The following modules are automatically available without explicit imports:

| Module | Description | Example Predicates |
|--------|-------------|-------------------|
| `lists` | List manipulation | `member/2`, `append/3`, `reverse/2`, `length/2` |
| `clpfd` | Finite domain constraints | `#=`, `#<`, `in`, `label/1` |
| `apply` | Higher-order predicates | `maplist/2`, `include/3`, `foldl/4` |
| `aggregate` | Aggregation | `aggregate_all/3` |

```yaml
# CLP(FD) works without explicit import
- name: solve
  language: prolog
  run: |
    X in 1..10,
    Y in 1..10,
    X + Y #= 15,
    label([X, Y]),
    return(x, X),
    return(y, Y).
```

## Complete Examples

### Example 1: Basic Arithmetic

```yaml
name: prolog-arithmetic
state_schema:
  value: int
  doubled: int
  squared: int

initial_state:
  value: 5
  doubled: 0
  squared: 0

nodes:
  - name: compute
    language: prolog
    run: |
      state(value, V),
      D is V * 2,
      S is V * V,
      return(doubled, D),
      return(squared, S).

edges:
  - from: __start__
    to: compute
  - from: compute
    to: __end__
```

**Run:**
```python
from the_edge_agent import YAMLEngine

engine = YAMLEngine(prolog_enabled=True)
graph = engine.load_from_file("prolog-arithmetic.yaml")
compiled = graph.compile()

for event in compiled.invoke({"value": 5, "doubled": 0, "squared": 0}):
    result = event

print(result["state"])
# {"value": 5, "doubled": 10, "squared": 25}
```

### Example 2: List Processing

```yaml
name: prolog-lists
state_schema:
  numbers: list
  total: int
  count: int
  has_even: bool

nodes:
  - name: analyze
    language: prolog
    run: |
      state(numbers, Numbers),

      % Sum all numbers
      sum_list(Numbers, Total),

      % Count elements
      length(Numbers, Count),

      % Check for even numbers
      (member(X, Numbers), 0 is X mod 2 -> HasEven = true ; HasEven = false),

      return(total, Total),
      return(count, Count),
      return(has_even, HasEven).

edges:
  - from: __start__
    to: analyze
  - from: analyze
    to: __end__
```

### Example 3: CLP(FD) Constraint Solving

```yaml
name: scheduling-agent
state_schema:
  num_tasks: int
  max_time: int
  schedule: list

nodes:
  - name: solve_schedule
    language: prolog
    run: |
      state(max_time, MaxTime),

      % Define task variables
      [T1, T2, T3] ins 0..MaxTime,

      % Task durations
      D1 = 2, D2 = 3, D3 = 2,

      % Precedence: T1 before T2, T2 before T3
      T1 + D1 #=< T2,
      T2 + D2 #=< T3,

      % Must complete within time limit
      T3 + D3 #=< MaxTime,

      % Find solution
      label([T1, T2, T3]),

      % Build schedule
      E1 is T1 + D1, E2 is T2 + D2, E3 is T3 + D3,
      Schedule = [
        _{task: 'A', start: T1, end: E1},
        _{task: 'B', start: T2, end: E2},
        _{task: 'C', start: T3, end: E3}
      ],

      return(schedule, Schedule).

edges:
  - from: __start__
    to: solve_schedule
  - from: solve_schedule
    to: __end__
```

### Example 4: Knowledge Graph Reasoning

```yaml
name: family-reasoning
state_schema:
  query_person: str
  parents: list
  siblings: list

nodes:
  - name: infer_relationships
    language: prolog
    run: |
      state(query_person, Person),

      % Embedded knowledge base
      (Person = alice ->
        (Parents = [george, mary], Siblings = [bob, carol])
      ; Person = bob ->
        (Parents = [george, mary], Siblings = [alice, carol])
      ; Person = carol ->
        (Parents = [george, mary], Siblings = [alice, bob])
      ; (Parents = [], Siblings = [])
      ),

      return(parents, Parents),
      return(siblings, Siblings).

edges:
  - from: __start__
    to: infer_relationships
  - from: infer_relationships
    to: __end__
```

## Neurosymbolic Patterns

### Pattern 1: Neural → Symbolic Pipeline

```yaml
nodes:
  # Step 1: Python classifier
  - name: classify
    run: |
      text = state["input_text"].lower()
      if "urgent" in text:
          return {"classification": "high_priority", "confidence": 0.95}
      return {"classification": "normal", "confidence": 0.7}

  # Step 2: Prolog rule engine
  - name: apply_rules
    language: prolog
    run: |
      state(classification, Class),
      state(confidence, Conf),

      (Class = high_priority, Conf > 0.8 ->
        Decision = escalate
      ; Conf < 0.5 ->
        Decision = human_review
      ;
        Decision = auto_process
      ),

      return(decision, Decision).
```

### Pattern 2: Multi-Step Reasoning Chain

```yaml
nodes:
  # Step 1: Identify conditions
  - name: identify
    language: prolog
    run: |
      state(symptoms, Symptoms),
      (member(fever, Symptoms), member(cough, Symptoms) ->
        Condition = flu
      ;
        Condition = unknown
      ),
      return(condition, Condition).

  # Step 2: Assess severity
  - name: assess
    language: prolog
    run: |
      state(condition, Condition),
      state(age, Age),
      ((Condition = flu, Age > 65) ->
        Severity = high
      ;
        Severity = medium
      ),
      return(severity, Severity).

  # Step 3: Generate recommendation
  - name: recommend
    language: prolog
    run: |
      state(severity, Severity),
      (Severity = high ->
        Recommendation = 'Seek medical attention'
      ;
        Recommendation = 'Rest and hydrate'
      ),
      return(recommendation, Recommendation).
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

All Prolog queries have timeout protection:

```python
# Default: 30 seconds
engine = YAMLEngine(prolog_enabled=True)

# Custom timeout: 10 seconds
engine = YAMLEngine(prolog_enabled=True, prolog_timeout=10.0)
```

If a query exceeds the timeout, it raises `PrologTimeoutError`.

## Troubleshooting

### "SWI-Prolog not found"

```bash
# Verify SWI-Prolog is installed
swipl --version

# Should show 9.1.x or higher
```

### "janus-swi not found"

```bash
pip install janus-swi
# or
pip install 'the_edge_agent[prolog]'
```

### "Arguments are not sufficiently instantiated"

This Prolog error occurs when variables aren't bound before use:

```prolog
% Wrong: X not bound
Result is X + 1.

% Correct: Read X from state first
state(value, X),
Result is X + 1.
```

### "Unknown procedure"

Ensure predicates are defined before use:

```prolog
% Wrong: helper/1 not defined
helper(X), return(result, X).

% Correct: Define helper first (or use inline logic)
(X = 42 -> return(result, X) ; return(result, 0)).
```

### Timeout Issues

For complex CLP(FD) problems:

```python
# Increase timeout for complex constraint solving
engine = YAMLEngine(prolog_enabled=True, prolog_timeout=60.0)
```

## Python API Reference

### YAMLEngine Options

```python
engine = YAMLEngine(
    prolog_enabled=True,       # Enable Prolog scripting
    prolog_timeout=30.0,       # Query timeout in seconds
)
```

### Programmatic Prolog Execution

For direct Prolog queries without YAML:

```python
import janus_swi as janus

# Initialize
janus.consult("my_rules.pl")

# Query
for result in janus.query("parent(X, bob)"):
    print(result["X"])

# Assert facts
janus.assertz("likes(alice, prolog)")
```

## Related Documentation

- [YAML Reference - Prolog Section](../shared/YAML_REFERENCE.md#method-2c-prolog-code)
- [Neurosymbolic Patterns Guide](../shared/architecture/neurosymbolic-patterns.md)
- [Prolog Examples](../../examples/prolog/)
- [Rust Prolog Guide](../rust/prolog-guide.md)
