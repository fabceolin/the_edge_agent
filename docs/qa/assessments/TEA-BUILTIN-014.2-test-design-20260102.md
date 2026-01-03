# Test Design: Story TEA-BUILTIN-014.2

**Story:** Rust Semantic Search Actions
**Date:** 2026-01-02
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 38 | 100% |
| **Unit tests** | 22 | 58% |
| **Integration tests** | 12 | 31% |
| **E2E tests** | 4 | 11% |

**Priority Distribution:**
- P0 (Critical): 14
- P1 (High): 16
- P2 (Medium): 6
- P3 (Low): 2

---

## Test Scenarios by Acceptance Criteria

### AC1: `search.semantic` action searches files/directories

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-UNIT-001 | Unit | P0 | `search_files` returns results for matching content | Core functionality |
| 014.2-UNIT-002 | Unit | P1 | `search_files` handles empty path list gracefully | Edge case |
| 014.2-UNIT-003 | Unit | P1 | `search_files` handles non-existent paths | Error handling |
| 014.2-INT-001 | Integration | P0 | Action invoked via YAML `uses: search.semantic` | Action registration |
| 014.2-INT-002 | Integration | P1 | Action returns results in expected schema | API contract |

### AC2: `search.semantic_content` action searches string content

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-UNIT-004 | Unit | P0 | `search_content` returns ranked results | Core algorithm |
| 014.2-UNIT-005 | Unit | P1 | `search_content` handles empty string | Edge case |
| 014.2-UNIT-006 | Unit | P1 | `search_content` handles single-line content | Boundary |
| 014.2-INT-003 | Integration | P0 | Action invoked via YAML `uses: search.semantic_content` | Action registration |

### AC3: Paths support local filesystem only

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-UNIT-007 | Unit | P1 | `list_files` handles absolute paths | Baseline |
| 014.2-UNIT-008 | Unit | P1 | `list_files` handles relative paths | Baseline |
| 014.2-INT-004 | Integration | P1 | Real filesystem traversal works correctly | Integration |

### AC4: Recursive directory traversal enabled by default

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-UNIT-009 | Unit | P1 | `list_files` with `recursive=true` traverses subdirectories | Default behavior |
| 014.2-UNIT-010 | Unit | P1 | `list_files` with `recursive=false` stays in top directory | Non-recursive mode |
| 014.2-INT-005 | Integration | P1 | Nested directory structure searched correctly | Real filesystem |

### AC5: Extension filtering limits search to specified file types

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-UNIT-011 | Unit | P1 | `list_files` with `extensions=[".rs"]` filters correctly | Filter logic |
| 014.2-UNIT-012 | Unit | P1 | `list_files` with multiple extensions filters correctly | Multiple filters |
| 014.2-UNIT-013 | Unit | P2 | `list_files` with `extensions=None` includes all files | Default behavior |

### AC6: Results schema matches Python implementation exactly

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-UNIT-014 | Unit | P0 | `SearchResult` struct has all required fields | Data structure |
| 014.2-UNIT-015 | Unit | P0 | `SearchResult` serializes to correct JSON schema | Serialization |
| 014.2-INT-006 | Integration | P0 | Action output matches Python action output schema | Cross-runtime parity |

### AC7: `top_k` limits number of results

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-UNIT-016 | Unit | P0 | `search_content` respects `top_k=5` limit | Core parameter |
| 014.2-UNIT-017 | Unit | P1 | `search_content` returns fewer than `top_k` if insufficient | Boundary |

### AC8: `max_distance` threshold filters low-confidence matches

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-UNIT-018 | Unit | P0 | Results above `max_distance` threshold excluded | Core parameter |
| 014.2-UNIT-019 | Unit | P1 | `max_distance=None` includes all results | Default behavior |

### AC9: `n_lines` parameter controls context window

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-UNIT-020 | Unit | P0 | Context includes `n_lines` before and after match | Core parameter |
| 014.2-UNIT-021 | Unit | P1 | Context clamps to file start boundary (`saturating_sub`) | Edge case |
| 014.2-UNIT-022 | Unit | P1 | Context clamps to file end boundary (`std::cmp::min`) | Edge case |

### AC10: `ignore_case` enables case-insensitive search

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-UNIT-023 | Unit | P1 | Case-insensitive mode matches "Error" with "error" | Core parameter |
| 014.2-UNIT-024 | Unit | P2 | Case-sensitive mode distinguishes "Error" from "error" | Default behavior |

### AC11: Model is lazy-loaded using `OnceLock`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-INT-007 | Integration | P1 | Model not loaded at binary startup | Lazy loading |
| 014.2-INT-008 | Integration | P1 | `get_model()` loads model on first call | Initialization |

### AC12: Model is cached globally

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-INT-009 | Integration | P1 | Multiple calls to `get_model()` return same instance | Caching |
| 014.2-INT-010 | Integration | P2 | Model shared across multiple executor instances | Memory efficiency |

### AC13: Actions register as `search.semantic` and `search.semantic_content`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-UNIT-025 | Unit | P0 | `register()` adds both actions to registry | Registration |
| 014.2-INT-011 | Integration | P0 | Actions callable by name from executor | Integration |

### AC14: Works with YAML `uses:` syntax and `output:` key mapping

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-E2E-001 | E2E | P0 | Full YAML workflow with `search.semantic` executes | End-to-end |
| 014.2-E2E-002 | E2E | P0 | Output mapped to state via `output:` key | YAML integration |

### AC15: Existing actions remain unaffected

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-INT-012 | Integration | P0 | Existing action tests pass with `search` feature enabled | Regression |

### AC16-19: Cross-Runtime Parity

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-UNIT-026 | Unit | P0 | Model name constant matches Python (`minishlab/potion-multilingual-128M`) | Parity |
| 014.2-UNIT-027 | Unit | P0 | `cosine_distance` using simsimd matches Python numpy implementation | Parity |
| 014.2-E2E-003 | E2E | P0 | Same query on same content produces same results as Python | Cross-runtime |
| 014.2-E2E-004 | E2E | P1 | Default parameter values match Python (top_k=10, n_lines=3, etc.) | Parity |

### AC20-24: Quality Requirements

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.2-UNIT-028 | Unit | P0 | `f32::cosine(a, a)` returns ~0.0 | Math correctness |
| 014.2-UNIT-029 | Unit | P0 | `f32::cosine(a, -a)` returns ~2.0 | Math correctness |
| 014.2-UNIT-030 | Unit | P1 | Build with `--features search` compiles successfully | Feature gating |
| 014.2-UNIT-031 | Unit | P1 | Build without `search` feature excludes search module | Feature isolation |
| 014.2-UNIT-032 | Unit | P1 | Missing file returns `TeaError::Action` | Error handling |
| 014.2-UNIT-033 | Unit | P2 | Non-UTF8 file returns `TeaError::Action` | Error handling |

---

## Risk Coverage

| Risk | Severity | Mitigating Tests |
|------|----------|------------------|
| Cross-runtime result mismatch | CRITICAL | 014.2-E2E-003, 014.2-UNIT-026, 014.2-UNIT-027 |
| Incorrect similarity ranking | HIGH | 014.2-UNIT-016, 014.2-UNIT-018, 014.2-UNIT-028, 014.2-UNIT-029 |
| Context window off-by-one | MEDIUM | 014.2-UNIT-020, 014.2-UNIT-021, 014.2-UNIT-022 |
| Model not cached (memory bloat) | MEDIUM | 014.2-INT-009, 014.2-INT-010 |
| Feature flag breaks default build | HIGH | 014.2-UNIT-030, 014.2-UNIT-031 |
| Action registration failure | HIGH | 014.2-UNIT-025, 014.2-INT-011, 014.2-E2E-001 |
| Existing actions broken | HIGH | 014.2-INT-012 |
| Binary size bloat | LOW | Documentation only (not testable) |

---

## Recommended Execution Order

1. **P0 Unit tests first** (fail fast on core logic)
   - 014.2-UNIT-001, 014.2-UNIT-004, 014.2-UNIT-014, 014.2-UNIT-015, 014.2-UNIT-016, 014.2-UNIT-018, 014.2-UNIT-020, 014.2-UNIT-025, 014.2-UNIT-026, 014.2-UNIT-027, 014.2-UNIT-028, 014.2-UNIT-029

2. **P0 Integration tests** (verify action registration and parity)
   - 014.2-INT-001, 014.2-INT-003, 014.2-INT-006, 014.2-INT-011, 014.2-INT-012

3. **P0 E2E tests** (verify workflow and cross-runtime parity)
   - 014.2-E2E-001, 014.2-E2E-002, 014.2-E2E-003

4. **P1 tests in level order** (unit → integration → e2e)

5. **P2+ tests as time permits**

---

## Cross-Runtime Parity Testing

### Reference Data Set

To verify cross-runtime parity, create a canonical test dataset:

**File:** `tests/fixtures/search/parity_test.txt`
```text
The quick brown fox jumps over the lazy dog.
Machine learning algorithms can identify patterns in data.
Authentication errors should be handled gracefully.
The database connection timed out unexpectedly.
User session management requires careful security considerations.
```

**Query:** `"authentication error handling"`

**Expected Results (both runtimes must match):**
1. Line 3: "Authentication errors should be handled gracefully." - distance ~0.15
2. Line 5: "User session management requires careful security considerations." - distance ~0.35
3. Line 2: "Machine learning algorithms can identify patterns in data." - distance ~0.55

### Parity Test Implementation

```rust
#[test]
fn test_cross_runtime_parity() {
    // Load reference data from shared fixtures
    let content = include_str!("fixtures/search/parity_test.txt");
    let query = "authentication error handling";
    let config = SearchConfig::default();

    let results = search_content(content, query, &config);

    // Compare against Python-generated reference results
    let expected = load_python_reference_results("parity_test_expected.json");

    for (rust_result, python_result) in results.iter().zip(expected.iter()) {
        assert_eq!(rust_result.match_line, python_result.match_line);
        assert!((rust_result.distance - python_result.distance).abs() < 0.01);
    }
}
```

---

## Feature Gating Tests

### Build Tests

```bash
# Test 1: Default build without search feature
cargo build --no-default-features
# Expected: Compiles, no search module

# Test 2: Build with search feature
cargo build --features search
# Expected: Compiles with search module

# Test 3: Test without search feature (existing tests pass)
cargo test
# Expected: All existing tests pass

# Test 4: Test with search feature
cargo test --features search
# Expected: All tests pass including search tests
```

---

## Test Data Requirements

### Sample Files (to create in `tests/fixtures/search/`)

```
tests/fixtures/search/
├── simple.rs           # Single Rust file with varied content
├── nested/
│   ├── auth.rs        # Authentication-related code
│   └── db/
│       └── models.rs  # Database models
├── empty.txt          # Empty file for edge case
├── binary.bin         # Binary file for error handling
├── parity_test.txt    # Cross-runtime parity test data
└── parity_expected.json # Python-generated reference results
```

### Sample Content (simple.rs)
```rust
// Authentication module
pub fn login(username: &str, password: &str) -> Result<Session, AuthError> {
    if verify_password(username, password)? {
        Ok(create_session(username))
    } else {
        Err(AuthError::InvalidCredentials)
    }
}

pub fn logout(session: &Session) {
    session.invalidate();
}
```

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-BUILTIN-014.2
  scenarios_total: 38
  by_level:
    unit: 22
    integration: 12
    e2e: 4
  by_priority:
    p0: 14
    p1: 16
    p2: 6
    p3: 2
  coverage_gaps: []
  critical_paths:
    - Cross-runtime parity with Python
    - Feature flag isolation
    - OnceLock model caching
  special_considerations:
    - Requires Python implementation completed first for reference results
    - Feature-gated tests require `cargo test --features search`
    - E2E parity tests require model download (~500MB)
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-014.2-test-design-20260102.md
P0 tests identified: 14
Total AC coverage: 24/24 (100%)
Cross-runtime parity tests: 4 (014.2-UNIT-026, 014.2-UNIT-027, 014.2-E2E-003, 014.2-E2E-004)
```

---

## Dependency on TEA-BUILTIN-014.1

**Critical:** This story's E2E parity tests (014.2-E2E-003, 014.2-E2E-004) require the Python implementation to be completed first. The Python implementation generates reference results that the Rust tests validate against.

**Recommended workflow:**
1. Complete TEA-BUILTIN-014.1 (Python)
2. Generate reference results from Python tests
3. Save to `tests/fixtures/search/parity_expected.json`
4. Implement TEA-BUILTIN-014.2 (Rust)
5. Run parity tests to verify cross-runtime consistency
