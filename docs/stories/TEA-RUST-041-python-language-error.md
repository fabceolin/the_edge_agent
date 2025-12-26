# Story: TEA-RUST-041 - Clear Error for Unsupported Python Language in Rust Runtime

## Status

**Draft** (Created 2025-12-26)

---

## Story

**As a** workflow developer using TEA with YAML agents,
**I want** the Rust runtime to provide a clear, actionable error when `language: python` is specified,
**So that** I understand immediately that Python is not supported and know what alternatives are available.

---

## Story Context

**Parent Epic:** TEA-RUST-001 - Rust Migration

**Related Stories:**
- TEA-PY-001 (Done): Lua scripting support in Python
- TEA-RUST-035 (Done): Prolog scripting support in Rust

**Change Trigger:** User confusion when Rust runtime attempts to parse Python syntax as Lua, producing cryptic syntax errors like `'}' expected near ':'`.

---

## Background

### Language Support by Runtime

| Language | Python Runtime | Rust Runtime |
|----------|----------------|--------------|
| `python` | Supported | **Not Supported** |
| `lua` | Supported | Supported |
| `prolog` | Supported | Supported |
| *(default)* | Lua | Lua |

**Key Difference:** The Python runtime supports all three scripting languages (Python, Lua, Prolog), while the Rust runtime supports only Lua and Prolog.

Both implementations should:
1. **Default to Lua** when no `language:` field is specified
2. Support `language: lua` and `language: prolog` identically
3. **Python runtime only**: Also supports `language: python` for full Python scripting
4. **Rust runtime**: Must explicitly reject `language: python` with a helpful error message

Currently, when `language: python` is specified in the Rust runtime, the behavior is undefined - either it attempts to parse as Lua (producing confusing errors) or silently fails.

---

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: GIVEN a node with `language: python` specified, WHEN the Rust runtime parses/executes the agent, THEN it returns error: `"Python scripting is not supported in the Rust runtime. Supported languages: lua, prolog. Use language: lua for portable cross-runtime agents."`

2. **AC-2**: GIVEN a node with no `language:` field specified, WHEN executed in either Python or Rust runtime, THEN it defaults to Lua

3. **AC-3**: GIVEN the error occurs, WHEN displayed to the user, THEN it includes the node name where the error occurred

### Cross-Runtime Parity

4. **AC-4**: GIVEN the same YAML agent with `language: lua` or `language: prolog`, WHEN executed in both Python and Rust, THEN behavior is identical

5. **AC-5**: GIVEN a node with no language specified using Lua syntax, WHEN executed in both runtimes, THEN it works correctly (consistent Lua default)

### Example Updates

6. **AC-6**: ALL example YAML files in `examples/` directory MUST have explicit `language:` specification on every node with a `run:` block

7. **AC-7**: Examples intended for Rust compatibility MUST use `language: lua` or `language: prolog` only

8. **AC-8**: Examples using Python syntax MUST specify `language: python` and include a comment noting Rust incompatibility

---

## Technical Notes

### Files to Modify

```
rust/src/engine/yaml_engine.rs     # Add language validation
rust/src/engine/mod.rs             # Error type if needed
examples/**/*.yaml                 # Add explicit language fields
```

### Implementation Approach

1. **Parse-time validation** (preferred): Check `language` field when loading YAML node definition
2. Add a new error variant or use existing `TeaError::InvalidConfig`
3. Error should be returned before attempting any Lua/Prolog execution

### Error Message Template

```
Error at node '{node_name}': Python scripting is not supported in the Rust runtime.
Supported languages: lua, prolog

Tip: Use `language: lua` for portable agents that work in both Python and Rust runtimes.
```

---

## Examples to Update

The following files need explicit `language:` specification:

### Pure Prolog Examples (already have `language: prolog`)
- `examples/prolog/*.yaml` - Review for completeness

### Mixed Language Examples (need updates)
- `examples/prolog/clpfd-constraints.yaml` - `validate` node needs `language: python` or convert to Lua
- Any example using Python syntax without explicit language

### Lua Examples
- All examples defaulting to Lua should add explicit `language: lua` for clarity

---

## Testing

### Unit Tests

```rust
#[test]
fn test_python_language_returns_clear_error() {
    let yaml = r#"
name: test-agent
nodes:
  - name: python_node
    language: python
    run: |
      return {"key": "value"}
"#;
    let result = YamlEngine::from_yaml(yaml);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Python scripting is not supported"));
    assert!(err.contains("python_node"));
}

#[test]
fn test_no_language_defaults_to_lua() {
    let yaml = r#"
name: test-agent
nodes:
  - name: lua_node
    run: |
      return { result = "ok" }
"#;
    let engine = YamlEngine::from_yaml(yaml).unwrap();
    // Should parse and execute as Lua
}
```

### Integration Tests

- Run all examples through Rust runtime, verify appropriate errors for Python-only examples
- Verify Lua/Prolog examples work unchanged

---

## Definition of Done

- [ ] Rust runtime returns clear error for `language: python`
- [ ] Error includes node name and actionable guidance
- [ ] All examples updated with explicit `language:` fields
- [ ] Python-only examples documented as such
- [ ] Unit tests for error case
- [ ] Cross-runtime parity verified (Lua default behavior)
- [ ] Documentation updated if needed

---

## Effort Estimate

**Story Points:** 3

- 1 point: Error handling in Rust
- 2 points: Update all example files with explicit language specification

---

## Notes

This is a developer experience improvement. The Rust runtime intentionally does not support Python to maintain:
- Single-binary deployment (no Python interpreter needed)
- Minimal dependencies for edge computing
- Consistent Lua/Prolog neurosymbolic capabilities


---

## Sub-Tasks: Example Files Requiring Updates

*Generated: 2025-12-26*

### Summary

- **16 files** need `language:` specification
- **18 files** already compliant
- **3 files** have no `run:` blocks (schemas only)

---

### Priority 1: Python-Only Examples (Add `language: python`)

These use Python syntax and will **only work in Python runtime**:

| Status | File | Missing Nodes | Action |
|--------|------|---------------|--------|
| [ ] | `examples/yaml_agent_example.yaml` | 6 | Add `language: python` + Rust incompatibility note |
| [ ] | `examples/yaml_customer_support_example.yaml` | 7 | Add `language: python` + Rust incompatibility note |
| [ ] | `examples/yaml_perplexity_example.yaml` | 5 | Add `language: python` + Rust incompatibility note |
| [ ] | `examples/yaml/extraction_validation_example.yaml` | 2 | Add `language: python` + Rust incompatibility note |
| [ ] | `examples/prolog/clpfd-constraints.yaml` | 1 (`validate`) | Add `language: python` OR convert to Lua |

---

### Priority 2: Mixed/Prolog Examples (Need Investigation)

| Status | File | Missing Nodes | Action |
|--------|------|---------------|--------|
| [ ] | `examples/prolog/neurosymbolic/llm-prolog-family-reasoning-interview.yaml` | 4 | Check syntax, add appropriate language |
| [ ] | `examples/prolog/neurosymbolic/llm-prolog-family-reasoning.yaml` | 3 | Check syntax, add appropriate language |
| [ ] | `examples/prolog/parity/parallel-isolation.yaml` | 2 | Should be `language: prolog` (parity test) |

---

### Priority 3: LlamaExtract/Web Examples (Investigate Syntax)

| Status | File | Missing Nodes | Action |
|--------|------|---------------|--------|
| [ ] | `examples/llamaextract/extract-arxiv-paper.yaml` | 1 | Check syntax, add language |
| [ ] | `examples/llamaextract/extract-invoice.yaml` | 1 | Check syntax, add language |
| [ ] | `examples/llamaextract/extract-invoice-external-schema.yaml` | 1 | Check syntax, add language |
| [ ] | `examples/llamaextract/extract-with-merged-schemas.yaml` | 1 | Check syntax, add language |
| [ ] | `examples/web/deep-research-crawler.yaml` | 1 | Check syntax, add language |
| [ ] | `examples/web/scrapegraph-simple-test.yaml` | 2 | Check syntax, add language |
| [ ] | `examples/web/test-scrapegraph-production.yaml` | 3 | Check syntax, add language |

---

### Priority 4: Parity Test (Quick Fix)

| Status | File | Missing Nodes | Action |
|--------|------|---------------|--------|
| [ ] | `examples/while-loop-parity-test.yaml` | 1 | Add `language: lua` (cross-runtime parity test) |

---

### Already Compliant (18 files) ✓

- `examples/prolog/simple-prolog-agent.yaml`
- `examples/prolog/neurosymbolic/classifier-rules.yaml`
- `examples/prolog/neurosymbolic/clpfd-scheduling.yaml`
- `examples/prolog/neurosymbolic/flight-safety-temporal-agent.yaml`
- `examples/prolog/neurosymbolic/knowledge-graph.yaml`
- `examples/prolog/neurosymbolic/reasoning-chain.yaml`
- `examples/prolog/parity/basic-state-access.yaml`
- `examples/prolog/parity/basic_arithmetic.yaml`
- `examples/prolog/parity/clpfd-deterministic.yaml`
- `examples/prolog/parity/clpfd-multiple-solutions.yaml`
- `examples/prolog/parity/comparison_logic.yaml`
- `examples/prolog/parity/empty-collections.yaml`
- `examples/prolog/parity/error-sandbox.yaml`
- `examples/prolog/parity/error-syntax.yaml`
- `examples/prolog/parity/error-timeout.yaml`
- `examples/prolog/parity/nested-objects.yaml`
- `examples/prolog/parity/type-coercion.yaml`
- `examples/prolog/parity/unicode-strings.yaml`
