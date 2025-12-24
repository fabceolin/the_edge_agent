# Story TEA-PROLOG-004: Fix Backslash Escape in tea_load_code

## Status

Done

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

- [x] **Task 1: Fix Python backslash escaping** (AC: 1, 7)
  - [x] Update `execute_node_code()` in `prolog_runtime.py`
  - [x] Change: `escaped_code = code.replace("\\", "\\\\").replace("'", "''")`
  - [x] Add test for `\=` operator
  - [x] Add test for `\+` operator

### Rust Implementation

- [x] **Task 2: Fix Rust backslash escaping** (AC: 1, 7)
  - [x] Update `execute_node_code()` in `prolog_runtime.rs`
  - [x] Change: `let escaped_code = code_with_period.replace('\\', "\\\\").replace('\'', "''");`
  - [x] Add test for `\=` operator
  - [x] Add test for `\+` operator

### Validation

- [x] **Task 3: Verify knowledge-graph.yaml works** (AC: 2, 3, 4, 5)
  - [x] Run `knowledge-graph.yaml` in Python
  - [x] Run `knowledge-graph.yaml` in Rust
  - [x] Verify correct output

- [x] **Task 4: Run regression tests** (AC: 6)
  - [x] Run Python test suite
  - [x] Run Rust test suite
  - [x] Verify no regressions

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

- [x] Python `execute_node_code` escapes backslashes
- [x] Rust `execute_node_code` escapes backslashes
- [x] `knowledge-graph.yaml` example works in both runtimes
- [x] Tests for `\=`, `\+`, `\==`, `=\=` operators pass
- [x] No regression in existing tests
- [x] Code reviewed and approved
- [ ] Merged to main branch

---

## QA Results

**Test Design Review:** 2025-12-23
**Reviewer:** Quinn (Test Architect)

### Test Design Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 14 |
| Unit tests | 6 (43%) |
| Integration tests | 6 (43%) |
| E2E tests | 2 (14%) |
| P0 (Critical) | 4 |
| P1 (High) | 8 |
| P2 (Medium) | 2 |

### Test Coverage Assessment

| AC | Coverage | Tests |
|----|----------|-------|
| AC-1 (Backslash escaping) | Full | PROLOG-004-UNIT-001, UNIT-002, UNIT-003 |
| AC-2 (`\=` operator) | Full | PROLOG-004-INT-001, INT-002 |
| AC-3 (`\+` operator) | Full | PROLOG-004-INT-003, UNIT-004 |
| AC-4 (`\==` operator) | Full | PROLOG-004-INT-004, UNIT-005 |
| AC-5 (`=\=` operator) | Full | PROLOG-004-INT-005, UNIT-006 |
| AC-6 (Regression) | Full | PROLOG-004-E2E-001, E2E-002 |
| AC-7 (Both runtimes) | Full | PROLOG-004-INT-006 (parity test) |

### P0 Critical Tests

1. **PROLOG-004-UNIT-001**: Basic backslash escaping (`\` -> `\\`)
2. **PROLOG-004-UNIT-002**: Escape order verification (backslash before quote)
3. **PROLOG-004-INT-001**: `\=` operator runtime execution
4. **PROLOG-004-INT-003**: `\+` operator (negation as failure)

### Risk Assessment

- **Fix complexity**: Low (single line change per runtime)
- **Regression risk**: Low (fix is additive)
- **Cross-runtime parity**: Explicitly tested

### Recommendations

1. Implement unit tests first for fast feedback on escaping logic
2. Run parity test to ensure Python and Rust behave identically
3. Full regression suite run before merge

### Test Design Document

`docs/qa/assessments/TEA-PROLOG-004-test-design-20251223.md`

---

### Implementation Review: 2025-12-23

**Reviewed By:** Quinn (Test Architect)

#### Code Quality Assessment

**Overall: Excellent** - Clean, minimal fix with proper documentation. The implementation demonstrates good engineering practices:

1. **Correct fix approach**: Backslash escaping added BEFORE single quote escaping, which is critical to avoid double-escaping issues
2. **Identical implementation across runtimes**: Python and Rust use the same escaping logic, ensuring cross-runtime parity
3. **Well-documented**: Comments explain WHY order matters, aiding future maintainability
4. **Minimal change surface**: Single-line change in each runtime reduces regression risk

#### Requirements Traceability

| AC | Test Coverage | Status |
|----|---------------|--------|
| AC-1: Backslash escaping | `test_combined_backslash_operators` | ✅ Full |
| AC-2: `\=` operator | `test_not_unifiable_operator`, `test_not_unifiable_fails_when_equal` | ✅ Full |
| AC-3: `\+` operator | `test_negation_as_failure_operator`, `test_negation_as_failure_succeeds` | ✅ Full |
| AC-4: `\==` operator | `test_not_structurally_equal_operator` | ✅ Full |
| AC-5: `=\=` operator | `test_arithmetic_not_equal_operator` | ✅ Full |
| AC-6: No regression | Full test suite run (75 Python Prolog, 100 Rust Prolog) | ✅ Full |
| AC-7: Both runtimes | Identical tests in Python and Rust | ✅ Full |

#### Compliance Check

- Coding Standards: ✅ Comments explain escape order rationale
- Project Structure: ✅ Tests added to existing test files
- Testing Strategy: ✅ Unit tests cover each operator individually + combined
- All ACs Met: ✅ All 7 acceptance criteria verified

#### Test Architecture Assessment

**Strengths:**
- Tests cover both success and failure cases (e.g., `\=` succeeds when different, fails when same)
- Integration test with `knowledge-graph.yaml` validates real-world usage
- Test docstrings trace to specific ACs for traceability
- Combined operator test validates no interference between escapes

**Test Levels:**
- Unit: 8 Python + 8 Rust tests covering individual operators
- Integration: knowledge-graph.yaml E2E validation
- Parity: Both runtimes produce identical results

#### Security Review

No security concerns. Fix is additive and only affects string escaping in Prolog code transmission.

#### Performance Considerations

Negligible impact. One additional `.replace()` call per Prolog node execution. String replacement is O(n) where n is code length.

#### Refactoring Performed

None required. Implementation is clean and minimal.

#### Improvements Checklist

All items addressed by developer:
- [x] Python escape order fixed (backslash before quote)
- [x] Rust escape order fixed (backslash before quote)
- [x] Comments explaining order importance
- [x] Tests for all 4 operators (`\=`, `\+`, `\==`, `=\=`)
- [x] Edge case: combined operators in single code block
- [x] Edge case: operator failure when condition not met
- [x] knowledge-graph.yaml integration verification

No outstanding items.

#### Files Modified During Review

None. No refactoring needed.

#### Gate Status

**Gate: PASS** → `docs/qa/gates/TEA-PROLOG-004-backslash-escape-bug.yml`

#### Recommended Status

**✅ Ready for Done** - All acceptance criteria met, comprehensive test coverage, no outstanding issues.

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/prolog_runtime.py` | Modified | Added backslash escaping before single quote escaping (line 624) |
| `python/tests/test_prolog_runtime.py` | Modified | Added `TestBackslashOperators` class with 8 tests |
| `rust/src/engine/prolog_runtime.rs` | Modified | Added backslash escaping before single quote escaping (line 1189) |
| `rust/tests/test_prolog_runtime.rs` | Modified | Added 8 backslash operator tests (TEA-PROLOG-004) |

### Debug Log References

No debugging issues encountered.

### Completion Notes

- **Python fix**: Line 624 in `prolog_runtime.py` - `escaped_code = code_with_period.replace("\\", "\\\\").replace("'", "''")`
- **Rust fix**: Line 1189 in `prolog_runtime.rs` - `let escaped_code = code_with_period.replace('\\', "\\\\").replace('\'', "''")`
- **Tests added**: 8 Python tests + 8 Rust tests covering all 4 backslash operators
- **Regression**: 75 Python Prolog tests passed, 100 Rust Prolog tests passed, 496 other Rust tests passed
- **knowledge-graph.yaml**: Verified working in both runtimes with output `["david", "eve", "frank"]`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-23 | 0.1 | Initial bug story from QA review of TEA-RUST-039 | Quinn (QA) |
| 2025-12-23 | 0.2 | Added QA Results section with test design (14 scenarios) | Quinn (QA) |
| 2025-12-23 | 0.3 | Story checklist validated (21/21), status → Ready | Bob (SM) |
| 2025-12-23 | 0.4 | Implementation complete, all tests pass, status → Ready for Review | James (Dev) |
| 2025-12-23 | 0.5 | QA review PASS, gate file created, status → Done | Quinn (QA) |
