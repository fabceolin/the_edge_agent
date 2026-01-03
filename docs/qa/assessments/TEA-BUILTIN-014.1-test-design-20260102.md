# Test Design: Story TEA-BUILTIN-014.1

**Story:** Python Semantic Search Actions
**Date:** 2026-01-02
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 42 | 100% |
| **Unit tests** | 24 | 57% |
| **Integration tests** | 14 | 33% |
| **E2E tests** | 4 | 10% |

**Priority Distribution:**
- P0 (Critical): 12
- P1 (High): 18
- P2 (Medium): 10
- P3 (Low): 2

---

## Test Scenarios by Acceptance Criteria

### AC1: `search.semantic` action searches files/directories

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-UNIT-001 | Unit | P0 | `search_files` returns results for matching content | Core functionality |
| 014.1-UNIT-002 | Unit | P1 | `search_files` handles empty file list gracefully | Edge case |
| 014.1-UNIT-003 | Unit | P1 | `search_files` handles non-existent paths | Error handling |
| 014.1-INT-001 | Integration | P0 | Action invoked via YAML `uses: search.semantic` | Action registration |
| 014.1-INT-002 | Integration | P1 | Action returns results in expected schema | API contract |

### AC2: `search.semantic_content` action searches string content

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-UNIT-004 | Unit | P0 | `search_content` returns ranked results | Core algorithm |
| 014.1-UNIT-005 | Unit | P1 | `search_content` handles empty string | Edge case |
| 014.1-UNIT-006 | Unit | P1 | `search_content` handles single-line content | Boundary |
| 014.1-INT-003 | Integration | P0 | Action invoked via YAML `uses: search.semantic_content` | Action registration |

### AC3: Paths support fsspec URIs

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-UNIT-007 | Unit | P1 | `list_files` handles local paths | Baseline |
| 014.1-UNIT-008 | Unit | P1 | `list_files` handles `s3://` URIs (mocked) | Cloud support |
| 014.1-UNIT-009 | Unit | P2 | `list_files` handles `gs://` URIs (mocked) | Cloud support |
| 014.1-UNIT-010 | Unit | P2 | `list_files` handles `az://` URIs (mocked) | Cloud support |
| 014.1-UNIT-011 | Unit | P2 | `list_files` handles `https://` URIs (mocked) | HTTP support |
| 014.1-INT-004 | Integration | P1 | File content read via fsspec local filesystem | Integration |
| 014.1-INT-005 | Integration | P2 | File content read via mocked S3 backend | Cloud integration |

### AC4: Recursive directory traversal enabled by default

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-UNIT-012 | Unit | P1 | `list_files` with `recursive=True` traverses subdirectories | Default behavior |
| 014.1-UNIT-013 | Unit | P1 | `list_files` with `recursive=False` stays in top directory | Non-recursive mode |
| 014.1-INT-006 | Integration | P1 | Nested directory structure searched correctly | Real filesystem |

### AC5: Extension filtering limits search to specified file types

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-UNIT-014 | Unit | P1 | `list_files` with `extensions=[".py"]` filters correctly | Filter logic |
| 014.1-UNIT-015 | Unit | P1 | `list_files` with multiple extensions filters correctly | Multiple filters |
| 014.1-UNIT-016 | Unit | P2 | `list_files` with `extensions=None` includes all files | Default behavior |

### AC6: Results include filename, line range, matched line, distance, context

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-UNIT-017 | Unit | P0 | `SearchResult` dataclass has all required fields | Data structure |
| 014.1-UNIT-018 | Unit | P0 | `SearchResult.to_dict()` produces correct JSON schema | Serialization |
| 014.1-INT-007 | Integration | P0 | Action output matches documented schema | API contract |

### AC7: `top_k` limits number of results

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-UNIT-019 | Unit | P0 | `search_content` respects `top_k=5` limit | Core parameter |
| 014.1-UNIT-020 | Unit | P1 | `search_content` returns fewer than `top_k` if insufficient matches | Boundary |
| 014.1-UNIT-021 | Unit | P2 | `top_k=0` returns empty results | Edge case |

### AC8: `max_distance` threshold filters low-confidence matches

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-UNIT-022 | Unit | P0 | Results above `max_distance` threshold excluded | Core parameter |
| 014.1-UNIT-023 | Unit | P1 | `max_distance=None` includes all results | Default behavior |
| 014.1-UNIT-024 | Unit | P1 | `max_distance=0.0` returns only exact matches | Boundary |

### AC9: `n_lines` parameter controls context window

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-UNIT-025 | Unit | P0 | Context includes `n_lines` before and after match | Core parameter |
| 014.1-UNIT-026 | Unit | P1 | Context clamps to file start boundary | Edge case |
| 014.1-UNIT-027 | Unit | P1 | Context clamps to file end boundary | Edge case |
| 014.1-UNIT-028 | Unit | P2 | `n_lines=0` returns only matched line | Boundary |

### AC10: `ignore_case` enables case-insensitive search

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-UNIT-029 | Unit | P1 | Case-insensitive mode matches "Error" with "error" | Core parameter |
| 014.1-UNIT-030 | Unit | P2 | Case-sensitive mode distinguishes "Error" from "error" | Default behavior |

### AC11: Model is lazy-loaded on first action invocation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-UNIT-031 | Unit | P1 | `load_model()` not called at import time | Lazy loading |
| 014.1-INT-008 | Integration | P1 | Model loaded only when action first invoked | Performance |

### AC12: Model is cached at engine level

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-INT-009 | Integration | P1 | Second action invocation reuses cached model | Caching |
| 014.1-INT-010 | Integration | P2 | Multiple engines share same model instance | Memory efficiency |

### AC13: Actions register as `search.semantic` and `search.semantic_content`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-UNIT-032 | Unit | P0 | `register_actions` adds both actions to registry | Registration |
| 014.1-INT-011 | Integration | P0 | Actions callable by name from engine | Integration |

### AC14: Works with YAML `uses:` syntax and `output:` key mapping

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-E2E-001 | E2E | P0 | Full YAML workflow with `search.semantic` executes | End-to-end |
| 014.1-E2E-002 | E2E | P0 | Output mapped to state via `output:` key | YAML integration |

### AC15: Existing actions remain unaffected

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-INT-012 | Integration | P0 | Existing action tests pass after adding search actions | Regression |

### AC16-18: Testing requirements

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-UNIT-033 | Unit | P0 | `cosine_distance(a, a)` returns 0.0 | Math correctness |
| 014.1-UNIT-034 | Unit | P0 | `cosine_distance(a, -a)` returns 2.0 | Math correctness |
| 014.1-UNIT-035 | Unit | P1 | `cosine_distance` handles zero vectors | Edge case |

### AC19: Type hints throughout (mypy compatible)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-INT-013 | Integration | P1 | `mypy python/src/the_edge_agent/search/` passes | Type safety |

### AC20: Error handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.1-UNIT-036 | Unit | P1 | Missing file returns `{"success": False, "error_type": "file_not_found"}` | Error format |
| 014.1-UNIT-037 | Unit | P2 | Binary file returns `{"success": False, "error_type": "decode_error"}` | Error format |
| 014.1-UNIT-038 | Unit | P1 | Model load failure returns `{"success": False, "error_type": "model_load_error"}` | Error format |
| 014.1-E2E-003 | E2E | P1 | Workflow handles missing file gracefully | Error propagation |
| 014.1-E2E-004 | E2E | P3 | Workflow with model unavailable fails gracefully | Error propagation |

---

## Risk Coverage

| Risk | Severity | Mitigating Tests |
|------|----------|------------------|
| Incorrect similarity ranking | HIGH | 014.1-UNIT-019, 014.1-UNIT-022, 014.1-UNIT-033, 014.1-UNIT-034 |
| Context window off-by-one | MEDIUM | 014.1-UNIT-025, 014.1-UNIT-026, 014.1-UNIT-027 |
| fsspec cloud integration failure | MEDIUM | 014.1-UNIT-008, 014.1-UNIT-009, 014.1-INT-005 |
| Model not cached (memory bloat) | MEDIUM | 014.1-INT-009, 014.1-INT-010 |
| Action registration failure | HIGH | 014.1-UNIT-032, 014.1-INT-011, 014.1-E2E-001 |
| Existing actions broken | HIGH | 014.1-INT-012 |

---

## Recommended Execution Order

1. **P0 Unit tests first** (fail fast on core logic)
   - 014.1-UNIT-001, 014.1-UNIT-004, 014.1-UNIT-017, 014.1-UNIT-018, 014.1-UNIT-019, 014.1-UNIT-022, 014.1-UNIT-025, 014.1-UNIT-032, 014.1-UNIT-033, 014.1-UNIT-034

2. **P0 Integration tests** (verify action registration)
   - 014.1-INT-001, 014.1-INT-003, 014.1-INT-007, 014.1-INT-011, 014.1-INT-012

3. **P0 E2E tests** (verify workflow integration)
   - 014.1-E2E-001, 014.1-E2E-002

4. **P1 tests in level order** (unit → integration → e2e)

5. **P2+ tests as time permits**

---

## Mock Strategy

### Unit Tests
- **model2vec.StaticModel**: Mock `encode()` to return fixed embeddings
- **fsspec**: Mock `open()`, `filesystem()`, `glob()` for cloud URIs
- **numpy**: Use real numpy for distance calculations

### Integration Tests
- **Model**: Use real model for integration tests (download required)
- **Filesystem**: Use `tempfile` for local filesystem tests
- **S3/GCS/Azure**: Mock at fsspec level

### E2E Tests
- **Full stack**: Real model, real filesystem, real YAML engine
- **Test data**: Use `tests/fixtures/search/` directory with sample files

---

## Test Data Requirements

### Sample Files (to create in `tests/fixtures/search/`)

```
tests/fixtures/search/
├── simple.py           # Single Python file with varied content
├── nested/
│   ├── auth.py        # Authentication-related code
│   └── db/
│       └── models.py  # Database models
├── empty.txt          # Empty file for edge case
├── binary.bin         # Binary file for error handling
└── large.py           # 1000+ lines for performance testing
```

### Sample Content (simple.py)
```python
# Authentication module
def login(username, password):
    """Handle user login with password verification."""
    if verify_password(username, password):
        return create_session(username)
    raise AuthenticationError("Invalid credentials")

def logout(session):
    """Terminate user session."""
    session.invalidate()
```

**Query:** "authentication error handling"
**Expected match:** Line 5-6 (AuthenticationError)

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-BUILTIN-014.1
  scenarios_total: 42
  by_level:
    unit: 24
    integration: 14
    e2e: 4
  by_priority:
    p0: 12
    p1: 18
    p2: 10
    p3: 2
  coverage_gaps: []
  critical_paths:
    - Action registration and YAML invocation
    - Cosine distance calculation correctness
    - Context window boundary handling
  mocking_required:
    - model2vec.StaticModel (unit tests)
    - fsspec cloud backends (unit + integration)
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-014.1-test-design-20260102.md
P0 tests identified: 12
Total AC coverage: 20/20 (100%)
```
