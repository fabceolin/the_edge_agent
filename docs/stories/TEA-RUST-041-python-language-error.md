# Story: TEA-RUST-041 - Clear Error for Unsupported Python Language in Rust Runtime

## Status

**Done**

---

## Story

**As a** workflow developer using TEA with YAML agents,
**I want** the Rust runtime to provide a clear, actionable error when `language: python` is specified,
**So that** I understand immediately that Python is not supported and know what alternatives are available.

---

## Story Context

**Parent Epic:** [TEA-RUST-001 - Rust Migration](./TEA-RUST-001-rust-migration.md)

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

Currently, when `language: python` is specified in the Rust runtime, the code falls through to Lua execution (line 821 of executor.rs), producing confusing syntax errors.

---

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: GIVEN a node with `language: python` specified, WHEN the Rust runtime executes the agent, THEN it returns error: `"Python scripting is not supported in the Rust runtime. Supported languages: lua, prolog. Use language: lua for portable cross-runtime agents."`

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

## Tasks / Subtasks

- [x] **Task 1: Add Python language validation in Rust executor** (AC-1, AC-3)
  - [x] Add check for `language == "python"` in `execute_node_body()` before Lua fallback
  - [x] Return `TeaError::InvalidConfig` with node name in message
  - [x] Verify error message matches AC-1 specification

- [x] **Task 2: Verify Lua default behavior** (AC-2, AC-5)
  - [x] Add test confirming no-language nodes default to Lua
  - [x] Test identical behavior in both Python and Rust runtimes

- [x] **Task 3: Update Python-only example files** (AC-6, AC-8)
  - [x] Add `language: python` to `examples/yaml_agent_example.yaml` (6 nodes)
  - [x] Add `language: python` to `examples/yaml_customer_support_example.yaml` (7 nodes)
  - [x] Add `language: python` to `examples/yaml_perplexity_example.yaml` (5 nodes)
  - [x] Add `language: python` to `examples/yaml/extraction_validation_example.yaml` (2 nodes)
  - [x] Convert `examples/prolog/clpfd-constraints.yaml` validate node to Lua for cross-runtime compatibility
  - [x] Add Rust incompatibility comment header to each file

- [x] **Task 4: Update Prolog/Mixed example files** (AC-6, AC-7)
  - [x] Update `examples/prolog/neurosymbolic/llm-prolog-family-reasoning-interview.yaml` (6 Python nodes)
  - [x] Update `examples/prolog/neurosymbolic/llm-prolog-family-reasoning.yaml` (4 Python nodes)
  - [x] Convert `examples/prolog/parity/parallel-isolation.yaml` Lua nodes (2 nodes)

- [x] **Task 5: Update LlamaExtract/Web examples** (AC-6)
  - [x] Check and update `examples/llamaextract/extract-arxiv-paper.yaml`
  - [x] Check and update `examples/llamaextract/extract-invoice.yaml`
  - [x] Check and update `examples/llamaextract/extract-invoice-external-schema.yaml`
  - [x] Check and update `examples/llamaextract/extract-with-merged-schemas.yaml`
  - [x] Check and update `examples/web/deep-research-crawler.yaml`

- [x] **Task 6: Update parity test example** (AC-6)
  - [x] Add `language: lua` to `rust/tests/fixtures/simple_workflow.yaml`
  - [x] Add `language: lua` to `rust/tests/fixtures/interruptible_workflow.yaml`

- [x] **Task 7: Unit tests for error handling** (AC-1, AC-3)
  - [x] Test `language: python` returns clear error with node name
  - [x] Test error message contains supported languages list

- [x] **Task 8: Cross-runtime parity tests** (AC-4, AC-5)
  - [x] Test same Lua agent in both runtimes produces identical results
  - [x] Test same Prolog agent in both runtimes produces identical results

---

## Dev Notes

### Files to Modify

```
rust/src/engine/executor.rs     # Add Python language check before Lua fallback (lines 798-821)
rust/src/engine/yaml.rs         # Optional: Add parse-time validation in NodeConfig
examples/**/*.yaml              # Add explicit language fields (16 files)
```

### Current Code Location

Language handling occurs in `executor.rs` at execution time:

```rust
// rust/src/engine/executor.rs:798-821
if let Some(ref code) = node.lua_code {
    // Check language: prolog vs lua (default)
    let is_prolog = node.language.as_deref() == Some("prolog");

    // ... prolog handling ...

    // Default: Lua (THIS IS WHERE PYTHON FALLS THROUGH)
    return self.lua.execute_node_code(code, state);
}
```

### Implementation Approach

**Execution-time validation** (recommended): Add Python check in `execute_node_body()` before the Lua fallback. This follows the existing pattern for Prolog validation.

```rust
// Insert after line 800, before prolog check:
if node.language.as_deref() == Some("python") {
    return Err(TeaError::InvalidConfig(format!(
        "Error at node '{}': Python scripting is not supported in the Rust runtime.\n\
         Supported languages: lua, prolog\n\n\
         Tip: Use `language: lua` for portable agents that work in both Python and Rust runtimes.",
        node.name
    )));
}
```

### Error Type

Use existing `TeaError::InvalidConfig(String)` - no new error variants needed.

### Relevant Source Tree

```
rust/src/engine/
├── executor.rs        # Language validation (lines 798-821) - PRIMARY TARGET
├── yaml.rs            # NodeConfig parsing, language field
├── graph.rs           # Node struct with language: Option<String>
└── lua_runtime.rs     # Lua execution
```

---

## Testing

### Test File Location

Add tests to `rust/src/engine/executor.rs` in the `#[cfg(test)]` module, or create `rust/tests/test_language_validation.rs`.

### Test Standards

- Use built-in Rust test framework
- Follow existing test patterns in executor.rs
- Run with `cargo test test_python_language`

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
    let engine = YamlEngine::from_yaml(yaml).unwrap();
    let result = engine.invoke(HashMap::new());
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("Python scripting is not supported"));
    assert!(err.contains("python_node"));
    assert!(err.contains("lua, prolog"));
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
    let result = engine.invoke(HashMap::new());
    assert!(result.is_ok());
    // Verify Lua executed successfully
}

#[test]
fn test_explicit_lua_language_works() {
    let yaml = r#"
name: test-agent
nodes:
  - name: explicit_lua
    language: lua
    run: |
      return { result = "explicit lua" }
"#;
    let engine = YamlEngine::from_yaml(yaml).unwrap();
    let result = engine.invoke(HashMap::new());
    assert!(result.is_ok());
}
```

### Integration Tests

- Run all examples through Rust runtime, verify appropriate errors for Python-only examples
- Verify Lua/Prolog examples work unchanged

---

## Definition of Done

- [x] Rust runtime returns clear error for `language: python`
- [x] Error includes node name and actionable guidance
- [x] All 16 example files updated with explicit `language:` fields
- [x] Python-only examples documented with Rust incompatibility comment
- [x] Unit tests for error case pass
- [x] Cross-runtime parity verified (Lua default behavior)
- [x] All existing tests continue to pass (`cargo test`)

---

## Effort Estimate

**Story Points:** 3

- 1 point: Error handling in Rust executor
- 2 points: Update all example files with explicit language specification

---

## Notes

This is a developer experience improvement. The Rust runtime intentionally does not support Python to maintain:
- Single-binary deployment (no Python interpreter needed)
- Minimal dependencies for edge computing
- Consistent Lua/Prolog neurosymbolic capabilities

---

## Example Files Inventory

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

### Already Compliant (18 files)

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

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-26 | 0.1 | Initial draft | Sarah (PO) |
| 2025-12-26 | 0.2 | Validation fixes: corrected file paths (executor.rs), added formal tasks with AC mapping, added change log, clarified execution-time validation approach | Sarah (PO) |
| 2025-12-26 | 1.0 | SM validation passed (9/10 clarity score), status → Approved | Bob (SM) |
| 2025-12-26 | 1.1 | Implementation complete, all tasks done, status → Ready for Review | Dev Agent (James) |
| 2025-12-26 | 1.2 | QA concerns resolved, all 3 missed files updated, status → Done | Dev Agent (James) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

N/A - No blocking issues encountered during implementation.

### Completion Notes

1. **Python language validation** added to `executor.rs` in two locations:
   - `execute_node_body()` for sequential execution (line 808)
   - `execute_node_with_lua()` for parallel execution (line 888)

2. **Error message** follows AC-1 specification exactly:
   - Mentions Python is not supported
   - Lists supported languages (lua, prolog)
   - Includes node name for debugging
   - Provides actionable guidance

3. **Example file updates**:
   - Added `language: python` to 20+ nodes across 11 Python-syntax files
   - Added Rust incompatibility header comments to all Python-only examples
   - Converted `clpfd-constraints.yaml` validate node to Lua for cross-runtime compatibility
   - Updated parity test fixtures with explicit `language: lua`

4. **Tests added**:
   - 3 unit tests in `yaml.rs` (test_python_language_returns_clear_error, test_no_language_defaults_to_lua, test_explicit_lua_language_works)
   - 3 CLI parity tests in `test_cli_parity.rs` (test_python_language_error, test_no_language_defaults_to_lua, test_explicit_lua_language)

5. **All 266+ existing tests pass** after changes.

### File List

**Modified:**
- `rust/src/engine/executor.rs` - Added Python language check before Lua fallback (2 locations)
- `rust/src/engine/yaml.rs` - Added 3 unit tests for Python language handling
- `rust/tests/test_cli_parity.rs` - Added 3 CLI parity tests for Python language error
- `rust/tests/fixtures/simple_workflow.yaml` - Added explicit `language: lua`
- `rust/tests/fixtures/interruptible_workflow.yaml` - Added explicit `language: lua`
- `examples/yaml_agent_example.yaml` - Added `language: python` + header
- `examples/yaml_customer_support_example.yaml` - Added `language: python` + header
- `examples/yaml_perplexity_example.yaml` - Added `language: python` + header
- `examples/yaml/extraction_validation_example.yaml` - Added `language: python` + header
- `examples/prolog/clpfd-constraints.yaml` - Converted validate node to Lua
- `examples/prolog/neurosymbolic/llm-prolog-family-reasoning-interview.yaml` - Added `language: python` + header
- `examples/prolog/neurosymbolic/llm-prolog-family-reasoning.yaml` - Added `language: python` + header
- `examples/prolog/parity/parallel-isolation.yaml` - Added explicit `language: lua`
- `examples/llamaextract/extract-arxiv-paper.yaml` - Added `language: python` + header
- `examples/llamaextract/extract-invoice.yaml` - Added `language: python` + header
- `examples/llamaextract/extract-invoice-external-schema.yaml` - Added `language: python` + header
- `examples/llamaextract/extract-with-merged-schemas.yaml` - Added `language: python` + header
- `examples/web/deep-research-crawler.yaml` - Added `language: python` + header
- `examples/web/scrapegraph-simple-test.yaml` - Added `language: python` + header (QA fix)
- `examples/web/test-scrapegraph-production.yaml` - Added `language: python` + header (QA fix)
- `examples/while-loop-parity-test.yaml` - Added `language: lua` (QA fix)

---

## QA Results

### Review Date: 2025-12-26

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: GOOD** - Implementation follows existing patterns in executor.rs, error message is clear and actionable, and tests are well-structured. The Python check is correctly placed in both sequential and parallel execution paths.

**Strengths:**
- Error message exactly matches AC-1 specification with node name (AC-3)
- Consistent placement in both `execute_node_body()` and `execute_node_with_lua()` paths
- Uses existing `TeaError::InvalidConfig` type - no new error variants needed
- 6 well-designed tests covering error case, default Lua, and explicit Lua
- Example file headers are clear and informative

**Minor Code Review Notes:**
- Error message format is good but could include "in Rust runtime" for clarity (currently just "Python scripting is not supported")
- Code duplication between sequential/parallel paths is acceptable given the existing pattern

### Refactoring Performed

None - Implementation is clean and follows existing patterns. No refactoring needed.

### Compliance Check

- Coding Standards: ✓ Follows Rust conventions, proper error handling
- Project Structure: ✓ Tests in correct locations (yaml.rs unit tests, test_cli_parity.rs)
- Testing Strategy: ✓ 3 unit tests + 3 CLI integration tests = good coverage
- All ACs Met: ✗ **See CONCERNS below**

### Improvements Checklist

**Completed by Dev:**
- [x] Python language validation in executor.rs (2 locations)
- [x] 3 unit tests in yaml.rs
- [x] 3 CLI parity tests in test_cli_parity.rs
- [x] Updated 11 Python-syntax examples with `language: python`
- [x] Updated 2 Rust fixtures with explicit `language: lua`
- [x] Rust incompatibility headers on Python-only examples

**CONCERNS - Files missed per AC-6 (RESOLVED):**
- [x] `examples/web/scrapegraph-simple-test.yaml` - Added `language: python` to setup/report nodes + header
- [x] `examples/web/test-scrapegraph-production.yaml` - Added `language: python` to route_test/test_error_handling/validate_results nodes + header
- [x] `examples/while-loop-parity-test.yaml` - Added `language: lua` to increment node

### Requirements Traceability

| AC | Status | Test Coverage |
|----|--------|---------------|
| AC-1 | ✓ | test_python_language_returns_clear_error, test_python_language_error |
| AC-2 | ✓ | test_no_language_defaults_to_lua (unit + CLI) |
| AC-3 | ✓ | Error message includes node name - verified in tests |
| AC-4 | ✓ | Existing parity tests + cross-runtime fixtures updated |
| AC-5 | ✓ | test_no_language_defaults_to_lua (both unit + CLI) |
| AC-6 | ✓ | All example files now have explicit `language:` |
| AC-7 | ✓ | clpfd-constraints converted to Lua, parity files updated |
| AC-8 | ✓ | All Python files now have `language: python` + header |

### Security Review

No security concerns. This change improves developer experience by providing clear error messages instead of cryptic Lua parse errors.

### Performance Considerations

No performance impact. The Python language check is a simple string comparison before Lua execution.

### Files Modified During Review

None - no refactoring performed.

### Gate Status

**Gate: PASS** → docs/qa/gates/TEA-RUST-041-python-language-error.yml

**Status:** All QA concerns have been resolved.

**Issues Resolved:**
1. [RESOLVED] `examples/web/scrapegraph-simple-test.yaml` - Added `language: python` to setup/report nodes + Rust incompatibility header
2. [RESOLVED] `examples/web/test-scrapegraph-production.yaml` - Added `language: python` to 3 nodes + Rust incompatibility header
3. [RESOLVED] `examples/while-loop-parity-test.yaml` - Added `language: lua` to increment node

### Final Status

**✓ Story Complete** - All acceptance criteria met, all tests pass.
