# Test Design: Story TEA-BUILTIN-014.3

**Story:** Python Workspace with LanceDB Persistence
**Date:** 2026-01-02
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 48 | 100% |
| **Unit tests** | 28 | 58% |
| **Integration tests** | 16 | 33% |
| **E2E tests** | 4 | 9% |

**Priority Distribution:**
- P0 (Critical): 16
- P1 (High): 20
- P2 (Medium): 10
- P3 (Low): 2

---

## Test Scenarios by Acceptance Criteria

### AC1: `search.workspace_create` action creates/opens workspace

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.3-UNIT-001 | Unit | P0 | `Workspace.create()` creates directory and config | Core functionality |
| 014.3-UNIT-002 | Unit | P1 | `Workspace.open()` opens existing workspace | Core functionality |
| 014.3-UNIT-003 | Unit | P1 | `Workspace.create()` fails on invalid path | Error handling |
| 014.3-INT-001 | Integration | P0 | Action creates workspace at specified path | Action integration |
| 014.3-INT-002 | Integration | P1 | Action returns `created=true` for new workspace | API contract |
| 014.3-INT-003 | Integration | P1 | Action returns `created=false` for existing workspace | API contract |

### AC2: `search.workspace_index` action indexes files

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.3-UNIT-004 | Unit | P0 | `Store.upsert_line_embeddings()` stores embeddings | Core functionality |
| 014.3-UNIT-005 | Unit | P1 | `Store.upsert_document_meta()` stores document metadata | Core functionality |
| 014.3-INT-004 | Integration | P0 | Action indexes new files | Action integration |
| 014.3-INT-005 | Integration | P0 | Action returns indexed/updated/skipped counts | API contract |
| 014.3-E2E-001 | E2E | P0 | Full workflow: create workspace, index files | End-to-end |

### AC3: `search.workspace_search` action searches indexed embeddings

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.3-UNIT-006 | Unit | P0 | `Store.search_line_embeddings()` returns ranked results | Core algorithm |
| 014.3-UNIT-007 | Unit | P1 | Search respects `top_k` limit | Core parameter |
| 014.3-UNIT-008 | Unit | P1 | Search respects `max_distance` threshold | Core parameter |
| 014.3-UNIT-009 | Unit | P1 | Search respects path filter | Core parameter |
| 014.3-INT-006 | Integration | P0 | Action returns results matching query | Action integration |
| 014.3-INT-007 | Integration | P1 | Action results include context lines | API contract |

### AC4: `search.workspace_stats` action returns statistics

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.3-UNIT-010 | Unit | P1 | `Store.get_stats()` returns document count | Core functionality |
| 014.3-UNIT-011 | Unit | P1 | `Store.get_stats()` returns line count | Core functionality |
| 014.3-UNIT-012 | Unit | P1 | `Store.get_stats()` returns index status | Core functionality |
| 014.3-INT-008 | Integration | P1 | Action returns accurate statistics | Action integration |

### AC5: Document state detection (New, Changed, Unchanged)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.3-UNIT-013 | Unit | P0 | New file detected as `DocumentState.NEW` | Core logic |
| 014.3-UNIT-014 | Unit | P0 | File with size change detected as `DocumentState.CHANGED` | Core logic |
| 014.3-UNIT-015 | Unit | P0 | File with mtime change detected as `DocumentState.CHANGED` | Core logic |
| 014.3-UNIT-016 | Unit | P1 | File with version mismatch detected as `DocumentState.CHANGED` | Core logic |
| 014.3-UNIT-017 | Unit | P0 | Unchanged file detected as `DocumentState.UNCHANGED` | Core logic |
| 014.3-INT-009 | Integration | P0 | `analyze_document_states()` processes file list correctly | Integration |

### AC6: Changed files trigger re-embedding, unchanged skipped

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.3-INT-010 | Integration | P0 | Modified file re-indexed on second run | Incremental update |
| 014.3-INT-011 | Integration | P0 | Unchanged file skipped on second run | Incremental update |
| 014.3-E2E-002 | E2E | P0 | Workflow: index, modify file, re-index, verify counts | End-to-end |

### AC7: LanceDB vector index auto-created at 256+ rows

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.3-UNIT-018 | Unit | P1 | Index not created below 256 rows | Threshold logic |
| 014.3-UNIT-019 | Unit | P1 | Index created at/above 256 rows | Threshold logic |
| 014.3-INT-012 | Integration | P2 | Index creation reflected in stats | Integration |

### AC8: Workspace actions integrate with engine lifecycle

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.3-INT-013 | Integration | P1 | Workspace state persists across action invocations | Lifecycle |
| 014.3-INT-014 | Integration | P2 | Workspace connection closed on engine shutdown | Cleanup |

### AC9: Workspace path supports fsspec URIs

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.3-UNIT-020 | Unit | P1 | Local path resolved correctly | Baseline |
| 014.3-UNIT-021 | Unit | P2 | S3 path parsed correctly (mocked) | Cloud support |
| 014.3-INT-015 | Integration | P1 | Local filesystem workspace works | Integration |
| 014.3-INT-016 | Integration | P2 | Mocked S3 workspace works | Cloud integration |

### AC10: `search.semantic` action optionally uses workspace

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.3-UNIT-022 | Unit | P1 | `search.semantic` with `workspace` param uses store | Optional param |
| 014.3-UNIT-023 | Unit | P1 | `search.semantic` without `workspace` uses direct mode | Backward compat |
| 014.3-E2E-003 | E2E | P1 | Workflow: index files, search via `search.semantic` with workspace | End-to-end |

### AC11: Works with YAML `uses:` syntax

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.3-E2E-004 | E2E | P0 | Full YAML workflow with all workspace actions | End-to-end |

### AC12: Existing search actions remain unaffected

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.3-INT-017 | Integration | P0 | Existing `search.semantic` tests pass | Regression |
| 014.3-INT-018 | Integration | P0 | Existing `search.semantic_content` tests pass | Regression |

### AC13-17: Quality Requirements

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 014.3-UNIT-024 | Unit | P0 | `DocMeta` dataclass has all fields | Data structure |
| 014.3-UNIT-025 | Unit | P0 | `DocumentState` enum has all values | Data structure |
| 014.3-UNIT-026 | Unit | P1 | `LineEmbedding` dataclass serializes correctly | Data structure |
| 014.3-UNIT-027 | Unit | P1 | LanceDB connection failure returns error dict | Error handling |
| 014.3-UNIT-028 | Unit | P2 | Corrupt workspace returns error dict | Error handling |
| 014.3-UNIT-029 | Unit | P1 | `mypy` passes on workspace module | Type safety |

---

## Risk Coverage

| Risk | Severity | Mitigating Tests |
|------|----------|------------------|
| Incorrect change detection | CRITICAL | 014.3-UNIT-013, 014.3-UNIT-014, 014.3-UNIT-015, 014.3-UNIT-016, 014.3-UNIT-017 |
| Unchanged files re-embedded (performance) | HIGH | 014.3-INT-011, 014.3-E2E-002 |
| LanceDB index not created | MEDIUM | 014.3-UNIT-018, 014.3-UNIT-019, 014.3-INT-012 |
| Workspace corruption | MEDIUM | 014.3-UNIT-028 |
| Existing search actions broken | HIGH | 014.3-INT-017, 014.3-INT-018 |
| fsspec cloud integration failure | MEDIUM | 014.3-UNIT-021, 014.3-INT-016 |

---

## Recommended Execution Order

1. **P0 Unit tests first** (fail fast on core logic)
   - 014.3-UNIT-001, 014.3-UNIT-004, 014.3-UNIT-006, 014.3-UNIT-013, 014.3-UNIT-014, 014.3-UNIT-015, 014.3-UNIT-017, 014.3-UNIT-024, 014.3-UNIT-025

2. **P0 Integration tests** (verify action integration)
   - 014.3-INT-001, 014.3-INT-004, 014.3-INT-005, 014.3-INT-006, 014.3-INT-009, 014.3-INT-010, 014.3-INT-011, 014.3-INT-017, 014.3-INT-018

3. **P0 E2E tests** (verify full workflow)
   - 014.3-E2E-001, 014.3-E2E-002, 014.3-E2E-004

4. **P1 tests in level order** (unit → integration → e2e)

5. **P2+ tests as time permits**

---

## LanceDB Test Infrastructure

### Fixtures Required

```python
import pytest
import tempfile
from pathlib import Path

@pytest.fixture
def temp_workspace():
    """Create temporary workspace directory."""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield Path(tmpdir) / "workspace"

@pytest.fixture
def sample_files(temp_workspace):
    """Create sample files for indexing."""
    files_dir = temp_workspace.parent / "files"
    files_dir.mkdir()

    (files_dir / "auth.py").write_text("""
def login(user, password):
    if verify(user, password):
        return create_session(user)
    raise AuthenticationError("Invalid")
""")

    (files_dir / "db.py").write_text("""
def connect():
    return Database.connect(config.DB_URL)

def query(sql):
    return connection.execute(sql)
""")

    return files_dir

@pytest.fixture
def mock_embeddings():
    """Mock embeddings to avoid model download in unit tests."""
    import numpy as np
    def mock_encode(lines):
        return np.random.rand(len(lines), 128).astype(np.float32)
    return mock_encode
```

### LanceDB Schema Validation

```python
def test_documents_table_schema():
    """Verify documents table has correct columns."""
    store = Store.open(workspace_path)
    table = store.db.open_table("documents")
    schema = table.schema

    assert "id" in schema.names
    assert "path" in schema.names
    assert "size_bytes" in schema.names
    assert "mtime" in schema.names
    assert "version" in schema.names

def test_line_embeddings_table_schema():
    """Verify line_embeddings table has correct columns."""
    store = Store.open(workspace_path)
    table = store.db.open_table("line_embeddings")
    schema = table.schema

    assert "id" in schema.names
    assert "path" in schema.names
    assert "line_number" in schema.names
    assert "vector" in schema.names
    # Vector should be 128-dimensional
    vector_field = schema.field("vector")
    assert vector_field.type.list_size == 128
```

---

## Incremental Update Test Scenario

**Critical Test: 014.3-E2E-002**

```python
def test_incremental_update_workflow():
    """Test that modified files are re-indexed, unchanged skipped."""
    # Setup
    workspace = Workspace.create(temp_path)
    files = create_sample_files()

    # First index
    result1 = workspace_index(workspace, files)
    assert result1["indexed"] == 3
    assert result1["updated"] == 0
    assert result1["skipped"] == 0

    # Modify one file
    modify_file(files[0])

    # Second index
    result2 = workspace_index(workspace, files)
    assert result2["indexed"] == 0  # No new files
    assert result2["updated"] == 1  # One modified
    assert result2["skipped"] == 2  # Two unchanged

    # Verify search still works
    results = workspace_search(workspace, "authentication")
    assert len(results) > 0
```

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-BUILTIN-014.3
  scenarios_total: 48
  by_level:
    unit: 28
    integration: 16
    e2e: 4
  by_priority:
    p0: 16
    p1: 20
    p2: 10
    p3: 2
  coverage_gaps: []
  critical_paths:
    - Document state detection (New/Changed/Unchanged)
    - Incremental indexing (skip unchanged files)
    - LanceDB store operations
    - Regression on existing search actions
  mocking_required:
    - model2vec.StaticModel (unit tests)
    - fsspec cloud backends (unit + integration)
  special_considerations:
    - LanceDB requires actual file I/O (use tempfile)
    - Index creation threshold requires 256+ rows
    - Cloud workspace tests should be mocked
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-014.3-test-design-20260102.md
P0 tests identified: 16
Total AC coverage: 17/17 (100%)
Dependencies: TEA-BUILTIN-014.1 must be complete before running
```
