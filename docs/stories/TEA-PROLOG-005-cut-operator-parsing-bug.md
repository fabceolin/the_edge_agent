# Story TEA-PROLOG-005: Fix Cut Operator Parsing in tea_load_code

## Status

Draft

## Story

**As a** developer using Prolog nodes with the cut operator (`!`),
**I want** multi-clause predicates with cuts to be parsed correctly by `tea_load_code/1`,
**so that** I can use standard Prolog idioms without unexpected failures.

## Story Context

**Parent Epic:** [TEA-PROLOG-001 - Prolog Integration for Neurosymbolic AI](TEA-PROLOG-001-prolog-integration-epic.md)

**Discovered During:** Neurosymbolic LLM+Prolog testing (llm-prolog-family-reasoning example)

**Affects:** Both Python and Rust runtimes (likely same root cause as `tea_load_code/1` implementation)

**Related Story:** [TEA-PROLOG-004](TEA-PROLOG-004-backslash-escape-bug.md) - Similar category (parsing issues in `tea_load_code/1`)

## Problem Statement

When Prolog code contains multi-clause predicates with the cut operator (`!`), `tea_load_code/1` fails silently or produces incorrect behavior. The predicates are not properly asserted.

### Reproduction

```yaml
# test-cut.yaml - FAILS
nodes:
  - name: test
    language: prolog
    run: |
      convert(32, 95) :- !.  % space -> underscore
      convert(X, X).         % default: keep char

      maplist(convert, [97, 32, 98], Results),
      return(results, Results).
```

**Expected Output:** `Results = [97, 95, 98]`
**Actual Output:** `Results = None` (execution fails silently)

### Workaround (Current)

Replace multi-clause predicates with if-then-else:

```prolog
% Instead of:
convert(32, 95) :- !.
convert(X, X).

% Use:
convert(X, Y) :- (X = 32 -> Y = 95 ; Y = X).
```

**Documented in:** `examples/prolog/neurosymbolic/llm-prolog-family-reasoning.yaml` (lines 163-178)

### Affected Patterns

- Multi-clause predicates with cut (`!`)
- Guard clauses using cut for efficiency
- DCG rules with cuts
- Any predicate using `!` as part of clause body

---

## Acceptance Criteria

1. **AC-1**: Multi-clause predicates with cuts are correctly asserted by `tea_load_code/1`
2. **AC-2**: The cut operator (`!`) works correctly within clause bodies
3. **AC-3**: Guard clause patterns like `foo(X) :- condition, !. foo(X) :- other.` work
4. **AC-4**: Existing tests continue to pass (no regression)
5. **AC-5**: Both Python and Rust implementations are fixed (if both affected)

---

## Tasks / Subtasks

### Investigation

- [ ] **Task 1: Diagnose root cause** (AC: 1)
  - [ ] Debug `tea_load_code/1` with cut-containing code
  - [ ] Identify if issue is in term parsing or term processing
  - [ ] Determine if `tea_process_term/1` mishandles cuts
  - [ ] Check if sandbox mode blocks cuts

### Python Implementation

- [ ] **Task 2: Fix Python implementation** (AC: 1, 2, 5)
  - [ ] Apply fix to `_TEA_PREDICATES` in `prolog_runtime.py`
  - [ ] Add test for multi-clause with cuts
  - [ ] Add test for guard clause pattern

### Rust Implementation

- [ ] **Task 3: Fix Rust implementation** (AC: 1, 2, 5)
  - [ ] Apply fix to `tea_prolog_predicates.pl`
  - [ ] Add test for multi-clause with cuts
  - [ ] Add test for guard clause pattern

### Validation

- [ ] **Task 4: Verify patterns work** (AC: 2, 3)
  - [ ] Test multi-clause with cut
  - [ ] Test guard clause pattern
  - [ ] Verify workaround is no longer needed

- [ ] **Task 5: Run regression tests** (AC: 4)
  - [ ] Run Python test suite
  - [ ] Run Rust test suite
  - [ ] Verify no regressions

---

## Dev Notes

### Existing System Context

**Integration Point:** `tea_load_code/1` in `_TEA_PREDICATES` (Python: `prolog_runtime.py`, Rust: `tea_prolog_predicates.pl`)

**Existing Pattern:** The `tea_process_term/1` predicate routes terms to appropriate handlers:
- `(:-Body)` → execute as directive
- `(Head :- Body)` → assertz as rule
- Other → check if fact and assert, or call as goal

**Possible Causes:**
1. Cut in clause body might confuse term classification
2. Sandbox mode might block cut execution
3. Multiple clauses for same predicate might not be handled correctly
4. Cut might be interpreted as a special term

### Testing

**Test Location:** `python/tests/test_prolog_runtime.py`, `rust/tests/test_prolog_runtime.rs`

**Testing Framework:** pytest (Python), cargo test (Rust)

**Relevant Existing Tests:** `TestBackslashOperators` class in Python

### Source Files

- `python/src/the_edge_agent/prolog_runtime.py` (lines 180-300 for `_TEA_PREDICATES`)
- `rust/src/engine/tea_prolog_predicates.pl`
- `rust/src/engine/prolog_runtime.rs`

---

## Risk and Compatibility Check

**Risk Assessment:**

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Breaking existing code | Low | Medium | Comprehensive test suite |
| Sandbox mode conflict | Medium | High | Check sandbox safe_primitive list |
| Edge cases with nested cuts | Low | Low | Test with complex patterns |

**Compatibility:**
- Existing YAML agents without cuts: No change
- Agents with cuts: Will now work correctly
- Workaround code: Will continue to work

---

## Definition of Done

- [ ] Root cause identified and documented
- [ ] Python `tea_load_code/1` handles cuts correctly
- [ ] Rust `tea_load_code/1` handles cuts correctly
- [ ] Multi-clause with cut patterns work
- [ ] Guard clause patterns work
- [ ] Tests for cut operator pass
- [ ] No regression in existing tests
- [ ] Workaround documented as no longer needed
- [ ] Merged to main branch

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-23 | 0.1 | Initial story created from neurosymbolic testing | Sarah (PO) |
