# Story TEA-BUILTIN-014.1: Python Semantic Search Actions

## Status
**Draft**

## Story

**As a** workflow developer using the Python runtime,
**I want** built-in semantic search actions to find relevant content in files,
**so that** I can build intelligent document retrieval and code analysis workflows.

## Story Context

**Existing System Integration:**
- Integrates with: Python actions system (`actions/` directory)
- Technology: Python 3.9+, model2vec, numpy, fsspec
- Follows pattern: `actions/ratelimit_actions.py` (engine-level resource caching)
- Touch points: `actions/__init__.py`, `yaml_engine.py` (action invocation)

**Reference Implementation:** [semtools](https://github.com/run-llama/semtools)
- Python story: [`docs/stories/1.1.python-search-core.md`](https://github.com/run-llama/semtools/blob/main/docs/stories/1.1.python-search-core.md)
- Rust search logic: [`src/search/mod.rs`](https://github.com/run-llama/semtools/blob/main/src/search/mod.rs) (reference implementation)
- Key functions: `search_documents()` (lines 77-120), `create_document_from_content()` (lines 40-75)

## Acceptance Criteria

**Functional Requirements:**

1. `search.semantic` action searches files/directories for semantically similar content
2. `search.semantic_content` action searches string content (no file I/O)
3. Paths support fsspec URIs (local, s3://, gs://, az://, https://)
4. Recursive directory traversal enabled by default
5. Extension filtering limits search to specified file types
6. Results include filename, line range, matched line, distance, and context lines
7. `top_k` limits number of results returned
8. `max_distance` threshold filters low-confidence matches
9. `n_lines` parameter controls context window around matches
10. `ignore_case` enables case-insensitive search

**Integration Requirements:**

11. Model is lazy-loaded on first action invocation
12. Model is cached at engine level (not reloaded per invocation)
13. Actions register as `search.semantic` and `search.semantic_content`
14. Works with YAML `uses:` syntax and `output:` key mapping
15. Existing actions remain unaffected

**Quality Requirements:**

16. Unit tests for search logic (similarity, ranking, context window)
17. Unit tests for fsspec file listing and reading
18. Integration test with sample directory search
19. Type hints throughout (mypy compatible)
20. Error handling for missing files, unreadable content, model loading failures

## Tasks / Subtasks

- [ ] **Task 1: Create search module** (AC: 11, 12)
  - [ ] Create `python/src/the_edge_agent/search/__init__.py`
  - [ ] Create `python/src/the_edge_agent/search/core.py` with SearchConfig, SearchResult dataclasses
  - [ ] Create `python/src/the_edge_agent/search/model.py` with lazy model loading
  - [ ] Implement model caching using module-level `_model` variable
  - [ ] Add `load_model()` function for explicit loading

- [ ] **Task 2: Implement core search logic** (AC: 2, 6, 7, 8, 9, 10)
  - [ ] Implement `embed_lines(lines: list[str]) -> np.ndarray`
  - [ ] Implement `cosine_distance(a: np.ndarray, b: np.ndarray) -> float`
  - [ ] Implement `search_content(content: str, query: str, config: SearchConfig) -> list[SearchResult]`
  - [ ] Handle context window calculation with boundary clamping
  - [ ] Handle case-insensitive mode (lowercase before embedding)

- [ ] **Task 3: Implement file/directory search** (AC: 1, 3, 4, 5)
  - [ ] Implement `list_files(paths: list[str], recursive: bool, extensions: list[str]) -> list[str]`
  - [ ] Use `fsspec.open()` for file reading
  - [ ] Use `fsspec.filesystem()` for directory listing
  - [ ] Implement `search_files(paths, query, config) -> list[SearchResult]`
  - [ ] Handle mixed paths (some files, some directories)

- [ ] **Task 4: Create action module** (AC: 13, 14, 15)
  - [ ] Create `python/src/the_edge_agent/actions/search_actions.py`
  - [ ] Implement `search_semantic(state, query, paths, ...)` action
  - [ ] Implement `search_semantic_content(state, query, content, ...)` action
  - [ ] Implement `register_actions(registry, engine)` function
  - [ ] Register in `actions/__init__.py`
  - [ ] Store model reference on engine for caching

- [ ] **Task 5: Add dependencies** (AC: 19)
  - [ ] Add `model2vec` to optional `[search]` extra in setup.py
  - [ ] Add `numpy` to optional `[search]` extra
  - [ ] Verify fsspec is already a core dependency

- [ ] **Task 6: Testing** (AC: 16, 17, 18, 20)
  - [ ] Test cosine distance calculation
  - [ ] Test context window edge cases (first/last lines of file)
  - [ ] Test case-insensitive search
  - [ ] Test top_k limiting
  - [ ] Test max_distance threshold filtering
  - [ ] Test extension filtering
  - [ ] Test recursive vs non-recursive directory search
  - [ ] Test fsspec file reading (mock for cloud URIs)
  - [ ] Test error handling for missing files
  - [ ] Test error handling for binary files
  - [ ] Integration test: search sample code directory

## Dev Notes

### Key Implementation References

**From semtools [`src/search/mod.rs`](https://github.com/run-llama/semtools/blob/main/src/search/mod.rs):**
- Model: `minishlab/potion-multilingual-128M`
- Vector dimension: 128
- Cosine distance: `1 - dot(a,b) / (norm(a) * norm(b))`
- Context window clamping: `max(0, idx - n_lines)` to `min(len, idx + n_lines + 1)`

**Python model2vec Usage:**
```python
from model2vec import StaticModel

# Load model (cached in ~/.cache/huggingface/)
model = StaticModel.from_pretrained("minishlab/potion-multilingual-128M")

# Embed multiple lines - returns np.ndarray shape (n, 128)
embeddings = model.encode(lines)

# Embed single query
query_embedding = model.encode([query])[0]
```

**Cosine Distance:**
```python
import numpy as np

def cosine_distance(a: np.ndarray, b: np.ndarray) -> float:
    """Compute cosine distance (0 = identical, 2 = opposite)."""
    dot = np.dot(a, b)
    norm_a = np.linalg.norm(a)
    norm_b = np.linalg.norm(b)
    if norm_a == 0 or norm_b == 0:
        return 1.0  # Undefined, treat as orthogonal
    return 1.0 - (dot / (norm_a * norm_b))
```

**fsspec File Operations:**
```python
import fsspec

# List files in directory (local or cloud)
fs, path = fsspec.core.url_to_fs("s3://bucket/prefix/")
files = fs.glob(f"{path}/**/*.py")  # Recursive glob

# Read file content
with fsspec.open("s3://bucket/file.txt", "r") as f:
    content = f.read()

# Check if path is directory
fs.isdir(path)
```

### Data Structures

```python
from dataclasses import dataclass
from typing import Optional

@dataclass
class SearchConfig:
    n_lines: int = 3
    top_k: int = 10
    max_distance: Optional[float] = None
    ignore_case: bool = False
    recursive: bool = True
    extensions: Optional[list[str]] = None

@dataclass
class SearchResult:
    filename: str
    lines: list[str]
    start: int          # 0-based line index
    end: int            # Exclusive end index
    match_line: int     # 0-based index of matched line
    distance: float
```

### Module Structure

```
python/src/the_edge_agent/
├── search/
│   ├── __init__.py          # Public exports: SearchConfig, SearchResult, search_files, search_content
│   ├── core.py              # Core search logic
│   └── model.py             # Lazy model loading with caching
├── actions/
│   └── search_actions.py    # Action wrappers for YAML integration
```

### Action Signatures

```python
def search_semantic(
    state: dict,
    query: str,
    paths: list[str],
    recursive: bool = True,
    top_k: int = 10,
    max_distance: Optional[float] = None,
    n_lines: int = 3,
    ignore_case: bool = False,
    extensions: Optional[list[str]] = None,
    **kwargs
) -> dict:
    """Search files/directories for semantically similar content."""
    ...
    return {
        "results": [result.to_dict() for result in results],
        "success": True,
        "query": query,
        "files_searched": len(files)
    }

def search_semantic_content(
    state: dict,
    query: str,
    content: str,
    top_k: int = 10,
    max_distance: Optional[float] = None,
    n_lines: int = 3,
    ignore_case: bool = False,
    **kwargs
) -> dict:
    """Search string content for semantically similar lines."""
    ...
    return {
        "results": [result.to_dict() for result in results],
        "success": True,
        "query": query,
        "lines_searched": len(lines)
    }
```

### Error Handling

Return error dict format (consistent with other actions):
```python
{
    "success": False,
    "error": "Error message",
    "error_type": "model_load_error | file_not_found | permission_error | ..."
}
```

## Testing

- **Test location:** `python/tests/test_search_actions.py`, `python/tests/test_search_core.py`
- **Framework:** pytest
- **Key test cases:**

| Test Case | Priority | Description |
|-----------|----------|-------------|
| `test_cosine_distance_identical` | P0 | Identical vectors return 0.0 |
| `test_cosine_distance_orthogonal` | P0 | Orthogonal vectors return 1.0 |
| `test_search_content_basic` | P0 | Basic content search returns results |
| `test_search_content_top_k` | P1 | top_k limits results |
| `test_search_content_max_distance` | P1 | max_distance filters results |
| `test_search_content_context_window` | P1 | Context includes n_lines before/after |
| `test_search_content_boundary_clamp` | P1 | Context clamps to file boundaries |
| `test_search_content_ignore_case` | P2 | Case-insensitive mode works |
| `test_search_files_directory` | P1 | Directory search finds files |
| `test_search_files_recursive` | P1 | Recursive search traverses subdirs |
| `test_search_files_extensions` | P1 | Extension filter limits files |
| `test_search_files_fsspec_local` | P1 | Local fsspec paths work |
| `test_search_files_fsspec_mock_s3` | P2 | S3 paths work (mocked) |
| `test_action_search_semantic` | P0 | Action integrates correctly |
| `test_action_search_semantic_content` | P0 | Content action integrates correctly |
| `test_model_lazy_loading` | P1 | Model not loaded until first use |
| `test_model_cached` | P1 | Model reused across calls |
| `test_error_missing_file` | P1 | Missing file returns error dict |
| `test_error_binary_file` | P2 | Binary file handled gracefully |

**Mock Strategy:**
- Mock `model2vec.StaticModel` for unit tests (avoid model download)
- Use real model for integration tests only
- Mock fsspec for cloud URIs

## Definition of Done

- [ ] All functional requirements met
- [ ] Actions registered and working in YAML workflows
- [ ] Model lazy loading and caching verified
- [ ] fsspec support for local and cloud URIs
- [ ] Unit tests pass (mocked model)
- [ ] Integration test passes (real model)
- [ ] Type hints pass mypy check
- [ ] Dependencies added to setup.py `[search]` extra
- [ ] Existing tests still pass

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-02 | 0.1 | Initial draft | Sarah (PO) |
