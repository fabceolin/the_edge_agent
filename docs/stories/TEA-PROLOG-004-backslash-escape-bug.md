# Story TEA-PROLOG-004: Fix Backslash Escape in tea_load_code

## Status

Draft

## Story

**As a** developer using Prolog nodes with operators like `\=`, `\+`, or `\==`,
**I want** backslashes to be properly escaped when code is passed to `tea_load_code/1`,
**so that** my Prolog code executes correctly without syntax errors.

## Story Context

**Parent Epic:** [TEA-PROLOG-001 - Prolog Integration for Neurosymbolic AI](TEA-PROLOG-001-prolog-integration-epic.md)

**Discovered During:** TEA-RUST-039 QA review

**Affects:** Both Python and Rust runtimes

## Problem Statement

When Prolog code containing backslash operators (e.g., `\=`, `\+`, `\==`) is passed to `tea_load_code/1`, the backslash is interpreted as an escape character in the quoted atom, causing syntax errors.

### Root Cause

Both Python and Rust implementations escape single quotes when building the `tea_load_code('...')` query:

**Python (`prolog_runtime.py` line 593):**
```python
escaped_code = code.replace("'", "''")
```

**Rust (`prolog_runtime.rs` line 1187):**
```rust
let escaped_code = code_with_period.replace('\'', "''");
```

Neither escapes backslashes, which are also special characters inside Prolog quoted atoms.

### Reproduction

```yaml
# knowledge-graph.yaml - FAILS in both runtimes
nodes:
  - name: test
    language: prolog
    run: |
      sibling(X, Y) :- parent(P, X), parent(P, Y), X \= Y.
      parent(alice, bob).
      parent(alice, carol).
      findall(S, sibling(bob, S), R), return(results, R).
```

**Error:**
```
Syntax error: Unknown character escape in quoted atom or string: `\='
```

### Affected Operators

- `\=` (not unifiable)
- `\+` (negation as failure)
- `\==` (not structurally equal)
- `=\=` (arithmetic not equal)

---

## Acceptance Criteria

1. **AC-1**: Backslash characters in Prolog code are escaped before passing to `tea_load_code/1`
2. **AC-2**: The `\=` operator works correctly in Prolog nodes
3. **AC-3**: The `\+` operator works correctly in Prolog nodes
4. **AC-4**: The `\==` operator works correctly in Prolog nodes
5. **AC-5**: The `=\=` operator works correctly in Prolog nodes
6. **AC-6**: Existing tests continue to pass
7. **AC-7**: Both Python and Rust implementations are fixed

---

## Tasks / Subtasks

### Python Implementation

- [ ] **Task 1: Fix Python backslash escaping** (AC: 1, 7)
  - [ ] Update `execute_node_code()` in `prolog_runtime.py`
  - [ ] Change: `escaped_code = code.replace("\\", "\\\\").replace("'", "''")`
  - [ ] Add test for `\=` operator
  - [ ] Add test for `\+` operator

### Rust Implementation

- [ ] **Task 2: Fix Rust backslash escaping** (AC: 1, 7)
  - [ ] Update `execute_node_code()` in `prolog_runtime.rs`
  - [ ] Change: `let escaped_code = code_with_period.replace('\\', "\\\\").replace('\'', "''");`
  - [ ] Add test for `\=` operator
  - [ ] Add test for `\+` operator

### Validation

- [ ] **Task 3: Verify knowledge-graph.yaml works** (AC: 2, 3, 4, 5)
  - [ ] Run `knowledge-graph.yaml` in Python
  - [ ] Run `knowledge-graph.yaml` in Rust
  - [ ] Verify correct output

- [ ] **Task 4: Run regression tests** (AC: 6)
  - [ ] Run Python test suite
  - [ ] Run Rust test suite
  - [ ] Verify no regressions

---

## Dev Notes

### The Fix

Both runtimes need the same fix - escape backslashes before single quotes:

**Before:**
```python
escaped_code = code.replace("'", "''")
```

**After:**
```python
escaped_code = code.replace("\\", "\\\\").replace("'", "''")
```

### Why Order Matters

Backslashes must be escaped FIRST, otherwise we'd double-escape the backslashes used to escape quotes.

### Test Case

```prolog
% Test the \= operator
X = foo, Y = bar, X \= Y, return(result, yes).
```

---

## Risk and Compatibility Check

**Risk Assessment:**

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Breaking existing code | Low | Medium | Comprehensive test suite |
| Edge cases with escaping | Low | Low | Test with nested escapes |

**Compatibility:**
- Existing YAML agents without backslashes: No change
- Agents with backslash operators: Will now work correctly

---

## Definition of Done

- [ ] Python `execute_node_code` escapes backslashes
- [ ] Rust `execute_node_code` escapes backslashes
- [ ] `knowledge-graph.yaml` example works in both runtimes
- [ ] Tests for `\=`, `\+`, `\==`, `=\=` operators pass
- [ ] No regression in existing tests
- [ ] Code reviewed and approved
- [ ] Merged to main branch

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-23 | 0.1 | Initial bug story from QA review of TEA-RUST-039 | Quinn (QA) |
