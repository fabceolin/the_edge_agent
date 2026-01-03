# Story TEA-BUILTIN-014.3: Python Workspace with LanceDB Persistence

> **DEPRECATED**: This story has been superseded by:
> - TEA-BUILTIN-002.5 (lancedb vector store backend)
>
> LanceDB is now integrated as a pluggable vector store backend in the
> unified RAG system rather than a separate workspace module.
>
> See TEA-BUILTIN-014-semantic-search-epic.md for rationale.

## Status
**Superseded**

## Story

**As a** workflow developer searching large document collections repeatedly,
**I want** persistent embedding storage with incremental updates,
**so that** subsequent searches are fast without re-embedding unchanged files.

## Story Context

**Existing System Integration:**
- Integrates with: TEA-BUILTIN-014.1 Python semantic search actions
- Technology: Python 3.9+, lancedb, pyarrow, fsspec
- Follows pattern: `memory/` module with backend abstraction
- Touch points: `search/` module, `actions/search_actions.py`, `yaml_engine.py`

**Reference Implementation:** [semtools](https://github.com/run-llama/semtools)
- Workspace story: [`docs/stories/1.2.python-workspace-lancedb.md`](https://github.com/run-llama/semtools/blob/main/docs/stories/1.2.python-workspace-lancedb.md)
- Store implementation: [`src/workspace/store.rs`](https://github.com/run-llama/semtools/blob/main/src/workspace/store.rs) (lines 88-761)
- Change detection: [`src/workspace/store.rs`](https://github.com/run-llama/semtools/blob/main/src/workspace/store.rs) (lines 763-828)

**Dependency:** Requires TEA-BUILTIN-014.1 (Python Semantic Search) completed first.

## Acceptance Criteria

**Functional Requirements:**

1. `search.workspace_create` action creates/opens workspace at specified path
2. `search.workspace_index` action indexes files/directories, storing embeddings
3. `search.workspace_search` action searches indexed embeddings with path filtering
4. `search.workspace_stats` action returns workspace statistics (document count, has_index)
5. Can detect document state: `New`, `Changed`, `Unchanged` (by size, mtime, version)
6. Changed files trigger re-embedding on index; unchanged files skip embedding
7. LanceDB vector index created automatically when 256+ rows exist

**Integration Requirements:**

8. Workspace actions integrate with engine lifecycle
9. Workspace path supports fsspec URIs (local, s3://, etc.)
10. `search.semantic` action can optionally use workspace for acceleration
11. Works with YAML `uses:` syntax and `output:` key mapping
12. Existing search actions remain unaffected (workspace is opt-in)

**Quality Requirements:**

13. Unit tests for document state detection (new, changed, unchanged)
14. Unit tests for LanceDB store operations (upsert, search, delete)
15. Integration test for full workspace search flow
16. Type hints throughout (mypy compatible)
17. Error handling for LanceDB connection failures, corrupt indices

## Tasks / Subtasks

- [ ] **Task 1: Create workspace module** (AC: 1, 8, 9)
  - [ ] Create `python/src/the_edge_agent/search/workspace/__init__.py`
  - [ ] Create `python/src/the_edge_agent/search/workspace/config.py` with WorkspaceConfig dataclass
  - [ ] Create `python/src/the_edge_agent/search/workspace/store.py` with Store class
  - [ ] Support fsspec URIs for workspace path (s3://, gs://, etc.)

- [ ] **Task 2: Document state tracking** (AC: 5, 6)
  - [ ] Create `DocMeta` dataclass (path, size_bytes, mtime, version)
  - [ ] Create `DocumentState` enum (New, Changed, Unchanged)
  - [ ] Implement `analyze_document_states(paths) -> list[tuple[str, DocumentState]]`
  - [ ] Define `CURRENT_EMBEDDING_VERSION = 1` constant

- [ ] **Task 3: LanceDB store implementation** (AC: 2, 3, 7)
  - [ ] Implement `Store.open(workspace_path)` with LanceDB connection
  - [ ] Implement `documents` table schema (id, path, size_bytes, mtime, version)
  - [ ] Implement `line_embeddings` table schema (id, path, line_number, vector)
  - [ ] Implement `upsert_line_embeddings(embeddings)` with delete-then-insert
  - [ ] Implement `search_line_embeddings(query_vec, paths, top_k, max_distance)`
  - [ ] Auto-create IVF_PQ index when table reaches 256 rows

- [ ] **Task 4: Create workspace actions** (AC: 1, 2, 3, 4, 10, 11)
  - [ ] Create `python/src/the_edge_agent/actions/workspace_actions.py`
  - [ ] Implement `search_workspace_create(state, path, ...)` action
  - [ ] Implement `search_workspace_index(state, workspace, paths, ...)` action
  - [ ] Implement `search_workspace_search(state, workspace, query, ...)` action
  - [ ] Implement `search_workspace_stats(state, workspace)` action
  - [ ] Add optional `workspace` parameter to `search.semantic` action
  - [ ] Register actions in `actions/__init__.py`

- [ ] **Task 5: Add dependencies** (AC: 16)
  - [ ] Add `lancedb` to optional `[search]` extra in setup.py
  - [ ] Add `pyarrow` to optional `[search]` extra
  - [ ] Verify fsspec is already a core dependency

- [ ] **Task 6: Testing** (AC: 13, 14, 15, 17)
  - [ ] Test document state detection (new file, modified file, unchanged file)
  - [ ] Test LanceDB table creation and schema
  - [ ] Test upsert with delete-then-insert pattern
  - [ ] Test vector search with path filtering
  - [ ] Test index creation threshold (256 rows)
  - [ ] Integration test: index files, modify one, re-index, search
  - [ ] Test error handling for corrupt workspace

## Dev Notes

### Key Implementation References

**From semtools [`src/workspace/store.rs`](https://github.com/run-llama/semtools/blob/main/src/workspace/store.rs):**

**Version constant:**
```python
CURRENT_EMBEDDING_VERSION = 1  # Bump when embedding model/pipeline changes
```

**Document State Detection Logic:**
```python
def analyze_document_states(paths: list[str], store: Store) -> list[tuple[str, DocumentState]]:
    results = []
    for path in paths:
        file_meta = get_file_metadata(path)  # size, mtime
        stored_meta = store.get_document_meta(path)

        if stored_meta is None:
            results.append((path, DocumentState.NEW))
        elif (file_meta.size != stored_meta.size or
              file_meta.mtime != stored_meta.mtime or
              stored_meta.version != CURRENT_EMBEDDING_VERSION):
            results.append((path, DocumentState.CHANGED))
        else:
            results.append((path, DocumentState.UNCHANGED))

    return results
```

**LanceDB Python API:**
```python
import lancedb

# Open/create database
db = lancedb.connect("~/.tea/workspaces/myproject/")

# Create table with embeddings
table = db.create_table("line_embeddings", [
    {"id": hash(f"{path}:{line_num}"), "path": path, "line_number": line_num, "vector": emb}
    for path, line_num, emb in embeddings
])

# Search with path filter
results = table.search(query_vec) \
    .where(f"path IN {tuple(paths)}") \
    .metric("cosine") \
    .limit(top_k) \
    .to_list()

# Create vector index (when 256+ rows)
if table.count_rows() >= 256:
    table.create_index(metric="cosine")
```

### Data Structures

```python
from dataclasses import dataclass
from enum import Enum
from typing import Optional

CURRENT_EMBEDDING_VERSION = 1

class DocumentState(Enum):
    NEW = "new"
    CHANGED = "changed"
    UNCHANGED = "unchanged"

@dataclass
class DocMeta:
    path: str
    size_bytes: int
    mtime: float
    version: int

@dataclass
class WorkspaceStats:
    total_documents: int
    total_lines: int
    has_index: bool
    index_type: Optional[str]  # "IVF_PQ" or None

@dataclass
class LineEmbedding:
    path: str
    line_number: int
    embedding: list[float]
```

### Table Schemas

**`documents` table:**
| Column | Type | Description |
|--------|------|-------------|
| id | Int32 | Hash of path |
| path | String | File path |
| size_bytes | UInt64 | File size |
| mtime | Float64 | Modification time |
| version | UInt32 | Embedding version |

**`line_embeddings` table:**
| Column | Type | Description |
|--------|------|-------------|
| id | Int32 | Hash of path + line_number |
| path | String | File path |
| line_number | Int32 | 0-based line index |
| vector | FixedSizeList[Float32, 128] | Line embedding |

### Module Structure

```
python/src/the_edge_agent/
├── search/
│   ├── __init__.py
│   ├── core.py              # Existing search logic
│   ├── model.py             # Existing model loading
│   └── workspace/
│       ├── __init__.py      # Exports
│       ├── config.py        # WorkspaceConfig
│       └── store.py         # Store, DocMeta, DocumentState
├── actions/
│   ├── search_actions.py    # Existing actions
│   └── workspace_actions.py # New workspace actions
```

### Action Signatures

```python
def search_workspace_create(
    state: dict,
    path: str,
    **kwargs
) -> dict:
    """Create or open a workspace at the given path."""
    return {
        "workspace": workspace_path,
        "success": True,
        "created": was_created
    }

def search_workspace_index(
    state: dict,
    workspace: str,
    paths: list[str],
    recursive: bool = True,
    extensions: Optional[list[str]] = None,
    **kwargs
) -> dict:
    """Index files into the workspace."""
    return {
        "success": True,
        "indexed": new_count,
        "updated": changed_count,
        "skipped": unchanged_count,
        "total_lines": total_lines
    }

def search_workspace_search(
    state: dict,
    workspace: str,
    query: str,
    paths: Optional[list[str]] = None,  # Filter to subset
    top_k: int = 10,
    max_distance: Optional[float] = None,
    n_lines: int = 3,
    **kwargs
) -> dict:
    """Search the workspace for semantically similar content."""
    return {
        "results": [...],
        "success": True,
        "query": query
    }

def search_workspace_stats(
    state: dict,
    workspace: str,
    **kwargs
) -> dict:
    """Get workspace statistics."""
    return {
        "total_documents": 42,
        "total_lines": 1234,
        "has_index": True,
        "index_type": "IVF_PQ",
        "success": True
    }
```

### YAML Usage Example

```yaml
nodes:
  - name: setup_workspace
    uses: search.workspace_create
    with:
      path: "~/.tea/workspaces/myproject/"
    output: ws

  - name: index_codebase
    uses: search.workspace_index
    with:
      workspace: "{{ ws.workspace }}"
      paths:
        - src/
        - lib/
      extensions: [".py", ".rs"]
    output: index_result

  - name: search_codebase
    uses: search.workspace_search
    with:
      workspace: "{{ ws.workspace }}"
      query: "authentication error handling"
      top_k: 10
    output: search_results
```

### Error Handling

```python
{
    "success": False,
    "error": "Error message",
    "error_type": "workspace_not_found | lancedb_error | index_corrupt | ..."
}
```

## Testing

- **Test location:** `python/tests/test_workspace_actions.py`, `python/tests/test_workspace_store.py`
- **Framework:** pytest with tempfile fixtures
- **Key test cases:**

| Test Case | Priority | Description |
|-----------|----------|-------------|
| `test_document_state_new` | P0 | New file detected correctly |
| `test_document_state_changed_size` | P0 | Size change detected |
| `test_document_state_changed_mtime` | P0 | Mtime change detected |
| `test_document_state_changed_version` | P1 | Version bump triggers re-embed |
| `test_document_state_unchanged` | P0 | Unchanged file skipped |
| `test_store_upsert_embeddings` | P0 | Embeddings stored correctly |
| `test_store_search_path_filter` | P0 | Path filtering works |
| `test_store_search_distance_filter` | P1 | Distance threshold applied |
| `test_store_delete_document` | P1 | Document deletion works |
| `test_index_creation_threshold` | P1 | Index created at 256 rows |
| `test_action_workspace_create` | P0 | Action creates workspace |
| `test_action_workspace_index` | P0 | Action indexes files |
| `test_action_workspace_search` | P0 | Action searches correctly |
| `test_action_workspace_stats` | P1 | Stats action works |
| `test_incremental_update` | P0 | Modified file re-indexed, unchanged skipped |
| `test_error_workspace_not_found` | P1 | Error for invalid workspace path |
| `test_fsspec_workspace_path` | P2 | S3 workspace path works (mocked) |

**Mock Strategy:**
- Mock `model2vec.StaticModel` for unit tests
- Use `tempfile.TemporaryDirectory` for LanceDB workspace
- Mock fsspec for cloud workspace paths

## Definition of Done

- [ ] All functional requirements met
- [ ] Workspace actions registered and working in YAML workflows
- [ ] Incremental indexing only re-embeds changed files
- [ ] Vector index auto-created at 256 rows
- [ ] fsspec support for workspace paths
- [ ] Unit tests pass (mocked model)
- [ ] Integration test passes (real LanceDB)
- [ ] Type hints pass mypy check
- [ ] Dependencies added to setup.py `[search]` extra
- [ ] Existing search tests still pass

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-02 | 0.1 | Initial draft | Sarah (PO) |
