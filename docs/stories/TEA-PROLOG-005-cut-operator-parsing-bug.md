# Story TEA-PROLOG-005: Fix Cut Operator Parsing in tea_load_code

## Status

Done

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

- [x] **Task 1: Diagnose root cause** (AC: 1)
  - [x] Debug `tea_load_code/1` with cut-containing code
  - [x] Identify if issue is in term parsing or term processing
  - [x] Determine if `tea_process_term/1` mishandles cuts
  - [x] Check if sandbox mode blocks cuts

### Python Implementation

- [x] **Task 2: Fix Python implementation** (AC: 1, 2, 5)
  - [x] Apply fix to `_TEA_PREDICATES` in `prolog_runtime.py`
  - [x] Add test for multi-clause with cuts
  - [x] Add test for guard clause pattern

### Rust Implementation

- [x] **Task 3: Fix Rust implementation** (AC: 1, 2, 5)
  - [x] Apply fix to `tea_prolog_predicates.pl`
  - [x] Add test for multi-clause with cuts
  - [x] Add test for guard clause pattern

### Validation

- [x] **Task 4: Verify patterns work** (AC: 2, 3)
  - [x] Test multi-clause with cut
  - [x] Test guard clause pattern
  - [x] Verify workaround is no longer needed

- [x] **Task 5: Run regression tests** (AC: 4)
  - [x] Run Python test suite
  - [x] Run Rust test suite
  - [x] Verify no regressions

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

- [x] Root cause identified and documented
- [x] Python `tea_load_code/1` handles cuts correctly
- [x] Rust `tea_load_code/1` handles cuts correctly
- [x] Multi-clause with cut patterns work
- [x] Guard clause patterns work
- [x] Tests for cut operator pass
- [x] No regression in existing tests
- [x] Workaround documented as no longer needed
- [ ] Merged to main branch

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5

### Root Cause Analysis

The issue was in `tea_is_fact/1` predicate which required `ground(Term)` to be true. Facts with variables like `convert(X, X).` are NOT ground, so they were treated as queries and called instead of asserted.

**Original code (broken):**
```prolog
tea_is_fact(Term) :-
    compound(Term),
    ground(Term),  % <-- PROBLEM: facts with variables fail here
    functor(Term, F, _),
    atom(F),
    \+ tea_action_predicate(F).
```

**Fixed code:**
```prolog
tea_is_fact(Term) :-
    compound(Term),
    % ground(Term) removed - facts with variables should be asserted
    functor(Term, F, _),
    atom(F),
    \+ tea_action_predicate(F).
```

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/prolog_runtime.py` | Modified | Removed `ground(Term)` from `tea_is_fact/1` |
| `python/tests/test_prolog_runtime.py` | Modified | Added `TestCutOperator` class with 6 tests |
| `rust/src/engine/tea_prolog_predicates.pl` | Modified | Removed `ground(Term)` from `tea_is_fact/1` |
| `rust/tests/test_prolog_runtime.rs` | Modified | Added 6 cut operator tests |
| `examples/prolog/neurosymbolic/llm-prolog-family-reasoning.yaml` | Modified | Replaced if-then-else workaround with native multi-clause cuts |

### Completion Notes

1. **Root cause identified**: `tea_is_fact/1` required `ground(Term)` which rejected facts with variables
2. **Fix applied**: Removed `ground(Term)` requirement from both Python and Rust implementations
3. **Tests added**: 6 new tests in each runtime (12 total) covering:
   - Multi-clause with cut (`convert(32, 95) :- !. convert(X, X).`)
   - Guard clause patterns (`classify(X, negative) :- X < 0, !.`)
   - Facts with variables (`identity(X, X).`)
   - Multiple clauses for same predicate
   - Cut preventing backtracking
   - If-then-else alternative (workaround) still works
4. **Regression tests passed**: All 81 Python tests + 106 Rust tests pass
5. **Workaround no longer needed**: The original multi-clause with cut pattern now works correctly

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-23 | 0.1 | Initial story created from neurosymbolic testing | Sarah (PO) |
| 2025-12-23 | 1.0 | Fix implemented: removed ground(Term) from tea_is_fact/1 | James (Dev) |
| 2025-12-23 | 1.1 | QA review passed, example updated to native cuts, status → Done | Quinn (QA) |

---

## QA Results

### Review Date: 2025-12-23

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Excellent implementation.** The fix is minimal, targeted, and addresses the root cause correctly. The developer identified that `ground(Term)` was incorrectly rejecting facts with variables like `convert(X, X).`, causing them to be called as queries instead of asserted as facts.

The fix is consistent across both Python and Rust implementations, maintaining cross-runtime parity. The added comments clearly explain the rationale for removing the `ground()` check.

### Refactoring Performed

None required. The implementation is clean and minimal.

### Compliance Check

- Coding Standards: ✓ Code follows existing patterns in both runtimes
- Project Structure: ✓ Tests added to appropriate test files
- Testing Strategy: ✓ Comprehensive test coverage with 6 tests per runtime
- All ACs Met: ✓ All 5 acceptance criteria verified with tests

### Requirements Traceability

| AC | Description | Test Coverage |
|----|-------------|---------------|
| AC-1 | Multi-clause predicates with cuts correctly asserted | `test_multi_clause_with_cut` |
| AC-2 | Cut operator works within clause bodies | `test_cut_prevents_backtracking` |
| AC-3 | Guard clause patterns work | `test_guard_clause_pattern` |
| AC-4 | No regression | All 81 Python + 106 Rust tests pass |
| AC-5 | Both Python and Rust fixed | 6 tests each runtime |

### Improvements Checklist

- [x] Root cause correctly identified and documented
- [x] Fix applied consistently to Python and Rust
- [x] Comprehensive tests added (12 total)
- [x] All acceptance criteria covered
- [x] Regression tests pass
- [x] Updated `llm-prolog-family-reasoning.yaml` to use native cut syntax

### Security Review

No security concerns. The fix removes a constraint (`ground(Term)`) from the `tea_is_fact/1` predicate. This change allows facts with variables to be properly asserted, which is the expected Prolog behavior. No new attack vectors introduced.

### Performance Considerations

No performance concerns. The fix actually simplifies the predicate logic by removing one check (`ground(Term)`), which may marginally improve performance.

### Test Quality Assessment

**Test coverage is excellent:**

1. **test_multi_clause_with_cut** - Validates the exact reproduction case from the story
2. **test_guard_clause_pattern** - Validates guard clause idiom with multiple cut clauses
3. **test_fact_with_variables_asserted** - Validates root cause fix (facts with variables)
4. **test_multiple_clauses_same_predicate** - Validates multi-clause facts work
5. **test_cut_prevents_backtracking** - Validates cut semantics are correct
6. **test_if_then_else_alternative** - Validates workaround still works

### Gate Status

**Gate: PASS** → `docs/qa/gates/TEA-PROLOG-005-cut-operator-parsing-bug.yml`

### Recommended Status

✓ **Ready for Done**

All acceptance criteria are met, comprehensive tests added, no regressions, and the fix is clean and well-documented.

---

### QA Final Notes (2025-12-23)

**Example Updated:** The workaround in `llm-prolog-family-reasoning.yaml` has been replaced with native multi-clause cut syntax. Tested and verified working:
- `"John Smith"` → `"john_smith"` ✓
- `"Mary-Jane"` → `"mary_jane"` ✓
- `"Dr. Alice!"` → `"dr_alice"` ✓

**Final Gate:** PASS - Story is complete and ready for merge.
