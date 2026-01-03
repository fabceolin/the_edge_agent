# Troubleshooting

> **Parent document:** [YAML Reference](../YAML_REFERENCE.md)
> **Related:** [Node Specification](./nodes.md) | [Advanced Runtimes](./advanced-runtimes.md)

## Overview

This guide covers common issues and solutions when working with YAML agents.

---

## Table of Contents

- [Common Issues](#common-issues)
- [Prolog Integration Issues](#prolog-integration-issues)

---

## Common Issues

### Template variables not replaced

**Problem:** Template expressions like `{{ state.key }}` appear literally in output.

**Solution:** Ensure you're using correct Jinja2 syntax:
- ✅ Correct: `{{ state.key }}`
- ❌ Wrong: `${{ state.key }}` (GitHub Actions syntax)

---

### Node function not found

**Problem:** Custom action not recognized by the engine.

**Solution:** Check that custom actions are registered in the engine before loading the YAML:
```python
engine = YAMLEngine()
engine.register_action("my.action", my_action_function)
```

---

### Parallel flows not working

**Problem:** Parallel branches don't execute or results not collected.

**Solution:** Ensure:
1. Fan-in node is defined with `fan_in: true`
2. All parallel edges reference the same fan-in node
3. Fan-in node accesses `parallel_results` parameter

---

### Conditional edges not routing correctly

**Problem:** Workflow doesn't take expected branch.

**Solution:** Debug by adding `interrupt_after` at the decision node to inspect state:
```yaml
config:
  interrupt_after:
    - classify_intent  # Pause here to check state
```

---

### Import module not found

**Problem:** `imports:` section fails to load external actions.

**Solution:** Check path is relative to YAML file location, not working directory:
```yaml
imports:
  - path: ./actions/custom.py  # Relative to YAML file
    actions:
      - my_action
```

---

## Prolog Integration Issues

### SWI-Prolog not found

**Problem:** "SWI-Prolog not found" or "janus-swi not found"

**Solution (Python):** Install SWI-Prolog 9.1+ and janus-swi:
```bash
# Ubuntu/Debian
sudo apt-add-repository ppa:swi-prolog/stable
sudo apt update && sudo apt install swi-prolog
pip install janus-swi
```

---

### Prolog feature not enabled (Rust)

**Problem:** "Prolog feature not enabled" error in Rust runtime.

**Solution:** Build with Prolog feature:
```bash
cargo build --features prolog
cargo run --features prolog -- run my-agent.yaml
```

---

### Arguments are not sufficiently instantiated

**Problem:** Prolog arithmetic fails with unbound variables.

**Solution:** Ensure variables are bound before arithmetic operations:
```prolog
% Wrong
Result is X + 1.  % X not bound

% Correct
state(value, X),
Result is X + 1.
```

---

### Unknown procedure

**Problem:** "Unknown procedure: predicate/N" error.

**Solution:** Define predicates before use, or use inline conditional logic:
```prolog
% Instead of calling undefined helper/1
(X > 0 -> Result = positive ; Result = non_positive).
```

---

### return/2 not updating state (Rust only)

**Problem:** Prolog `return/2` predicate doesn't update state in Rust TEA.

**Solution:** This is a known limitation. Use Lua nodes for state updates:
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

---

### Prolog query timeout

**Problem:** Complex queries timeout before completion.

**Solution:** Increase timeout for complex constraint solving:
```python
engine = YAMLEngine(prolog_enabled=True, prolog_timeout=60.0)
```

---

### CLP(FD) constraints not working

**Problem:** Constraint logic programming constraints fail.

**Solution:** CLP(FD) is pre-loaded in both Python and Rust. Ensure proper syntax:
```prolog
X in 1..10,           % Domain declaration
X + Y #= 15,          % Constraint (use #= not =)
label([X, Y]).        % Find concrete values
```

---

## See Also

- [Node Specification](./nodes.md) - Execution methods
- [Advanced Runtimes](./advanced-runtimes.md) - Lua and Prolog details
- [Navigation](./navigation.md) - Flow control
