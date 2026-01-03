# Test Design: Story TEA-BUILTIN-002.3

**Title:** vector.index_files Action
**Date:** 2026-01-02
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total Test Scenarios** | 26 |
| **Unit Tests** | 15 (58%) |
| **Integration Tests** | 8 (31%) |
| **E2E Tests** | 3 (11%) |

**Priority Distribution:**
- P0 (Critical): 12
- P1 (High): 10
- P2 (Medium): 4

---

## Test Scenarios by Acceptance Criteria

### AC1: `vector.index_files` action indexes files/directories into vector store

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-INT-001 | Integration | P0 | Basic action indexes single file | Core functionality - must work |
| 002.3-INT-002 | Integration | P0 | Action indexes directory with multiple files | Multi-file indexing is primary use case |
| 002.3-INT-003 | Integration | P0 | Action returns correct result structure | Output structure critical for workflow chaining |

---

### AC2: Supports glob patterns for file filtering

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-UNIT-001 | Unit | P0 | Glob pattern `**/*.py` matches Python files | Core file discovery logic |
| 002.3-UNIT-002 | Unit | P1 | Glob pattern `*.md` matches only markdown | Single-level glob behavior |
| 002.3-UNIT-003 | Unit | P1 | Multiple glob patterns combined | Complex pattern usage |
| 002.3-UNIT-004 | Unit | P1 | Invalid glob pattern handled gracefully | Error boundary |

---

### AC3: Supports fsspec URIs (local, s3://, gs://, az://, https://)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-UNIT-005 | Unit | P0 | Local file path resolution | Primary use case |
| 002.3-UNIT-006 | Unit | P2 | S3 URI parsing (mocked) | Cloud storage support |
| 002.3-UNIT-007 | Unit | P2 | GCS URI parsing (mocked) | Cloud storage support |
| 002.3-INT-004 | Integration | P1 | fsspec local filesystem read | Validates fsspec integration |

---

### AC4: `chunk_by` parameter controls granularity

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-UNIT-008 | Unit | P0 | `chunk_by=line` splits by newline with line numbers | Core chunking logic |
| 002.3-UNIT-009 | Unit | P0 | `chunk_by=paragraph` splits by double newline | Core chunking logic |
| 002.3-UNIT-010 | Unit | P0 | `chunk_by=document` returns entire file as one chunk | Core chunking logic |
| 002.3-UNIT-011 | Unit | P1 | Empty file handled (no chunks) | Edge case |
| 002.3-UNIT-012 | Unit | P1 | Single line file with `chunk_by=paragraph` | Edge case |

---

### AC5: `recursive` parameter controls directory traversal

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-UNIT-013 | Unit | P1 | `recursive=true` includes subdirectories | Default behavior |
| 002.3-UNIT-014 | Unit | P1 | `recursive=false` excludes subdirectories | Non-recursive mode |

---

### AC6: `extensions` parameter filters by file type

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-UNIT-015 | Unit | P1 | Extensions filter case-insensitive (`.PY` == `.py`) | User convenience |
| 002.3-INT-005 | Integration | P1 | Extension filtering with multiple types | Combined filtering |

---

### AC7: Results include file/line metadata for line-level chunks

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-INT-006 | Integration | P0 | Metadata contains `file`, `line`, `chunk_type` | Required for search result context |

---

### AC8: Incremental updates - only re-index changed files

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-UNIT-016 | Unit | P0 | New file detected (not in state) | Change detection core |
| 002.3-UNIT-017 | Unit | P0 | Modified file detected (mtime/size changed) | Change detection core |
| 002.3-UNIT-018 | Unit | P0 | Unchanged file skipped | Performance optimization |
| 002.3-INT-007 | Integration | P0 | Re-indexing updates only changed files | End-to-end change detection |

---

### AC9-13: Integration Requirements

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-INT-008 | Integration | P0 | Uses configured embedding provider | Settings integration |
| 002.3-E2E-001 | E2E | P0 | Index files then query returns matches | Complete RAG workflow |
| 002.3-E2E-002 | E2E | P1 | YAML workflow with `uses: vector.index_files` | YAML engine integration |

---

### AC14-17: Quality Requirements

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.3-UNIT-019 | Unit | P1 | Missing file returns error in result | Error handling |
| 002.3-UNIT-020 | Unit | P1 | Binary file skipped gracefully | Error handling |
| 002.3-UNIT-021 | Unit | P1 | Permission error handled | Error handling |
| 002.3-E2E-003 | E2E | P2 | Sample directory indexing end-to-end | Full integration validation |

---

## Test Summary Table

| Level | P0 | P1 | P2 | Total |
|-------|----|----|----|----|
| Unit | 7 | 10 | 2 | 15 |
| Integration | 5 | 3 | 0 | 8 |
| E2E | 1 | 1 | 1 | 3 |
| **Total** | **12** | **10** | **4** | **26** |

---

## Recommended Execution Order

### Phase 1: P0 Unit Tests (Fail Fast)
1. 002.3-UNIT-001: Glob pattern matching
2. 002.3-UNIT-005: Local path resolution
3. 002.3-UNIT-008: Line chunking
4. 002.3-UNIT-009: Paragraph chunking
5. 002.3-UNIT-010: Document chunking
6. 002.3-UNIT-016: New file detection
7. 002.3-UNIT-017: Modified file detection
8. 002.3-UNIT-018: Unchanged file skip

### Phase 2: P0 Integration Tests
9. 002.3-INT-001: Basic file indexing
10. 002.3-INT-002: Directory indexing
11. 002.3-INT-003: Result structure
12. 002.3-INT-006: Metadata verification
13. 002.3-INT-007: Incremental update
14. 002.3-INT-008: Embedding provider integration

### Phase 3: P0 E2E Tests
15. 002.3-E2E-001: Index-then-query workflow

### Phase 4: P1 Tests
16-25. All P1 tests in order

### Phase 5: P2 Tests (As Time Permits)
26. Remaining P2 tests

---

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| Incorrect line numbers in metadata | 002.3-UNIT-008, 002.3-INT-006 | Unit + integration coverage |
| Change detection misses modified files | 002.3-UNIT-017, 002.3-INT-007 | Explicit mtime/size testing |
| Glob patterns fail silently | 002.3-UNIT-004 | Error handling test |
| Binary files corrupt index | 002.3-UNIT-020 | Skip binary validation |
| fsspec cloud URIs fail | 002.3-UNIT-006, 002.3-UNIT-007 | Mocked URI tests |
| Query returns no context | 002.3-E2E-001 | Full workflow validation |

---

## Test Data Requirements

### Sample Directory Structure
```
test_fixtures/
├── python/
│   ├── main.py           # 50 lines
│   ├── utils.py          # 30 lines
│   └── sub/
│       └── helper.py     # 20 lines
├── docs/
│   ├── README.md         # Paragraphs for paragraph chunking
│   └── guide.md
├── data/
│   └── binary.bin        # Binary file for skip test
└── empty/
    └── empty.txt         # Empty file edge case
```

### Mock Requirements
- fsspec filesystem mock for cloud URIs
- Embedding provider mock returning fixed vectors
- Vector store mock for storage verification

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for components)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  story_id: "TEA-BUILTIN-002.3"
  story_title: "vector.index_files Action"
  scenarios_total: 26
  by_level:
    unit: 15
    integration: 8
    e2e: 3
  by_priority:
    p0: 12
    p1: 10
    p2: 4
  coverage_gaps: []
  test_file: "python/tests/test_vector_index_files.py"
  design_date: "2026-01-02"
  designer: "Quinn (Test Architect)"
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-002.3-test-design-20260102.md
P0 tests identified: 12
Story file: docs/stories/TEA-BUILTIN-002.3-vector-index-files.md
Test file location: python/tests/test_vector_index_files.py
```
