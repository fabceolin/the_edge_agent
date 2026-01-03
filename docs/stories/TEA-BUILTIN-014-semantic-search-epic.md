# TEA-BUILTIN-014: Semantic Search Integration - Epic

## Epic Overview

| Field | Value |
|-------|-------|
| **ID** | TEA-BUILTIN-014 |
| **Type** | Epic |
| **Priority** | Medium |
| **Status** | Draft |
| **Estimated Stories** | 3 |

## Epic Goal

Provide built-in semantic search actions for both Python and Rust runtimes, enabling workflows to find semantically similar content within files or directory trees using the `minishlab/potion-multilingual-128M` embedding model.

## Epic Description

### Existing System Context

- **Reference Implementation:** [semtools](https://github.com/run-llama/semtools) - Rust CLI for semantic search
  - Core search logic: [`src/search/mod.rs`](https://github.com/run-llama/semtools/blob/main/src/search/mod.rs)
  - Workspace/store: [`src/workspace/store.rs`](https://github.com/run-llama/semtools/blob/main/src/workspace/store.rs)
  - CLI binary: [`src/bin/search.rs`](https://github.com/run-llama/semtools/blob/main/src/bin/search.rs)
- **Technology Stack:**
  - **Python:** model2vec, numpy
  - **Rust:** model2vec (Rust bindings), simsimd
- **Integration Points:**
  - `python/src/the_edge_agent/actions/` - Python action modules
  - `rust/src/actions/` - Rust action modules
  - YAML nodes using `uses:` action syntax
- **Related Work:**
  - Story 1.1 in semtools: Python Semantic Search Module (Core)
  - Story 1.2 in semtools: Workspace Integration with LanceDB (deferred)

### Enhancement Details

**What's Being Added:**

1. **Python `search.semantic` Action** - Line-by-line semantic search over files/directories
2. **Python `search.semantic_content` Action** - Semantic search over string content
3. **Rust `search.semantic` Action** - Equivalent Rust implementation for cross-runtime parity
4. **Rust `search.semantic_content` Action** - Content-based semantic search in Rust
5. **Engine-Level Model Caching** - Lazy-loaded embedding model shared across invocations

**How It Integrates:**

```yaml
nodes:
  - name: find_relevant_code
    uses: search.semantic
    with:
      query: "authentication error handling"
      paths:
        - src/
        - lib/
      recursive: true
      top_k: 5
      max_distance: 0.3
      n_lines: 3
      extensions:
        - .py
        - .rs
    output: search_results

  - name: search_text
    uses: search.semantic_content
    with:
      query: "database connection"
      content: "{{ state.document_text }}"
      top_k: 3
    output: matches
```

**Success Criteria:**

- [ ] `search.semantic` action searches files/directories recursively
- [ ] `search.semantic_content` action searches string content
- [ ] Results include filename, line range, matched line, distance, and context
- [ ] Model loading is lazy and cached at engine level
- [ ] Extension filtering supported for file search
- [ ] Both Python and Rust implementations produce equivalent results
- [ ] Distance threshold (`max_distance`) filters low-confidence matches

---

## Stories

### Story 1: TEA-BUILTIN-014.1 - Python Semantic Search Actions

**Goal:** Implement semantic search actions for the Python runtime.

**Scope:**
- Create `python/src/the_edge_agent/actions/search_actions.py`
- Implement `search.semantic` action for file/directory search
- Implement `search.semantic_content` action for content search
- Add `model2vec` and `numpy` to optional dependencies
- Lazy model loading cached on engine instance

**Key Implementation Details:**
- Uses `minishlab/potion-multilingual-128M` model (128-dimensional embeddings)
- Line-by-line embedding strategy (not chunking)
- Cosine distance for similarity (lower = more similar)
- Context window: `n_lines` before/after matched line

---

### Story 2: TEA-BUILTIN-014.2 - Rust Semantic Search Actions

**Goal:** Implement semantic search actions for the Rust runtime with cross-runtime parity.

**Scope:**
- Create `rust/src/actions/search.rs`
- Implement `search.semantic` action handler
- Implement `search.semantic_content` action handler
- Add `model2vec` and `simsimd` crate dependencies
- Lazy model loading with `OnceLock` pattern

**Key Implementation Details:**
- Same embedding model as Python for result parity
- Uses `simsimd::f32::cosine()` for distance calculation
- Thread-safe model caching with `Arc<StaticModel>`

---

### Story 3: TEA-BUILTIN-014.3 - Python Workspace with LanceDB Persistence

**Goal:** Add persistent embedding storage with incremental updates for large document collections.

**Scope:**
- Create `python/src/the_edge_agent/search/workspace/` module
- Implement `Store` class with LanceDB backend
- Implement document state tracking (New, Changed, Unchanged)
- Create workspace actions: `search.workspace_create`, `search.workspace_index`, `search.workspace_search`, `search.workspace_stats`
- Add `lancedb` and `pyarrow` dependencies

**Key Implementation Details:**
- Change detection by file size, mtime, and embedding version
- Incremental indexing: only re-embed changed/new files
- Auto-create IVF_PQ vector index when 256+ rows
- Optional workspace acceleration for `search.semantic` action

**Dependency:** Requires TEA-BUILTIN-014.1 completed first.

---

## Compatibility Requirements

- [x] Both runtimes support identical YAML configuration
- [x] Result schema is identical across runtimes
- [x] Existing actions unaffected
- [x] Optional dependency - core package works without model2vec
- [x] No changes to checkpoint format

## Dependencies

**Python Dependencies:**

| Dependency | Purpose | Install Extra |
|------------|---------|---------------|
| `model2vec` | Embedding model | `[search]` |
| `numpy` | Vector operations | `[search]` |
| `lancedb` | Vector database for workspace | `[search]` |
| `pyarrow` | LanceDB dependency | `[search]` |
| `fsspec` | Cloud/remote filesystem abstraction | Core (already present) |

**Note:** Python implementation supports `fsspec` URIs for paths, enabling search over:
- Local files: `./src/`, `/home/user/project/`
- S3: `s3://bucket/prefix/`
- GCS: `gs://bucket/prefix/`
- Azure Blob: `az://container/prefix/`
- HTTP/HTTPS: `https://example.com/file.txt`

**Rust Dependencies:**

| Dependency | Purpose | Feature Flag |
|------------|---------|--------------|
| `model2vec` | Embedding model | `search` |
| `simsimd` | Fast cosine distance | `search` |
| `walkdir` | Directory traversal | `search` |

## Risk Mitigation

- **Primary Risk:** Large model download on first use (~500MB)
- **Mitigation:**
  - Lazy loading only when action is invoked
  - Clear error message if download fails
  - Document model caching location (`~/.cache/huggingface/`)
- **Rollback Plan:** Remove `search.*` actions; no impact on existing workflows

## Technical Design Notes

### Result Schema

```python
@dataclass
class SearchResult:
    filename: str          # Source file path
    lines: list[str]       # Context lines (n_lines before/after)
    start: int             # Start line number (0-based)
    end: int               # End line number (exclusive)
    match_line: int        # Matched line number
    distance: float        # Cosine distance (0.0 = identical, 2.0 = opposite)
```

**YAML Output Example:**

```yaml
search_results:
  - filename: "src/auth/login.py"
    lines:
      - "def handle_auth_error(e):"
      - "    logger.error(f'Auth failed: {e}')"
      - "    raise AuthenticationError(e)"
    start: 42
    end: 45
    match_line: 43
    distance: 0.18
```

### Python Module Structure

```
python/src/the_edge_agent/
├── search/
│   ├── __init__.py          # Exports
│   ├── core.py              # SearchConfig, SearchResult, search logic
│   └── model.py             # Lazy model loading
├── actions/
│   └── search_actions.py    # search.semantic, search.semantic_content
```

### Rust Module Structure

```
rust/src/
├── actions/
│   ├── mod.rs               # Add search::register()
│   └── search.rs            # search.semantic, search.semantic_content
├── search/
│   ├── mod.rs               # SearchConfig, SearchResult
│   └── model.rs             # OnceLock model loading
```

### Action Configuration Schema

```yaml
# search.semantic - File/directory search
- uses: search.semantic
  with:
    query: string           # Required: search query
    paths: list[string]     # Required: files/dirs (Python: supports fsspec URIs like s3://)
    recursive: bool         # Optional: recurse into directories (default: true)
    top_k: int              # Optional: max results (default: 10)
    max_distance: float     # Optional: distance threshold (default: none)
    n_lines: int            # Optional: context lines (default: 3)
    ignore_case: bool       # Optional: case-insensitive (default: false)
    extensions: list[str]   # Optional: file extensions to include (e.g., [".py", ".rs"])
  output: results

# search.semantic_content - Content search
- uses: search.semantic_content
  with:
    query: string           # Required: search query
    content: string         # Required: text content to search
    top_k: int              # Optional: max results (default: 10)
    max_distance: float     # Optional: distance threshold
    n_lines: int            # Optional: context lines (default: 3)
    ignore_case: bool       # Optional: case-insensitive (default: false)
  output: results
```

---

## Definition of Done

- [ ] All 3 stories completed with acceptance criteria met
- [ ] Python and Rust implementations produce identical results
- [ ] Actions work in YAML workflows with `uses:` syntax
- [ ] Optional dependencies properly configured
- [ ] Unit tests for search logic in both runtimes
- [ ] Integration test with sample files
- [ ] `docs/shared/YAML_REFERENCE.md` updated with search actions
- [ ] `docs/python/actions-reference.md` updated
- [ ] `docs/rust/actions-reference.md` updated
- [ ] No regression in existing features

---

## Story Manager Handoff

"Please develop detailed user stories for this brownfield epic. Key considerations:

- This enhancement adds semantic search to both Python and Rust runtimes
- Integration points: `actions/` directories, YAML engine action invocation
- Existing patterns to follow:
  - `actions/` module pattern with `register_actions()` function
  - Lazy resource loading (see `ratelimit_actions.py` for engine-level registry pattern)
  - Optional dependencies as extras in `pyproject.toml` / feature flags in `Cargo.toml`
- Critical compatibility requirements:
  - YAML configuration must be identical across runtimes
  - Result schema must match for workflow portability
- Each story must include verification that existing functionality remains intact
- Reference semtools stories 1.1 and 1.2 for implementation details

The epic should maintain system integrity while delivering semantic search capabilities to both runtimes."

---

## Reference Implementation Notes

**Repository:** https://github.com/run-llama/semtools

### From semtools [`src/search/mod.rs`](https://github.com/run-llama/semtools/blob/main/src/search/mod.rs)

**Key Constants:**
- Model: `minishlab/potion-multilingual-128M`
- Vector dimension: 128
- Batch size: 2048 (for `encode_with_args`)
- Max tokens: 16384

**Core Algorithm:**
1. Split content by newlines
2. Embed each line separately
3. Embed query
4. Compute cosine distance between query and each line
5. Filter by `max_distance` threshold (if set)
6. Sort by distance ascending
7. Take `top_k` results
8. Add context lines (clamped to file boundaries)

**Cosine Distance Formula:**
```python
distance = 1 - dot(a, b) / (norm(a) * norm(b))
```
- 0.0 = identical vectors
- 1.0 = orthogonal
- 2.0 = opposite directions

### From semtools Python story 1.1

**Directory Structure:**
```
semtools-py/
├── pyproject.toml
├── src/
│   └── semtools/
│       ├── __init__.py      # Public API exports
│       ├── search.py        # Core search logic
│       └── cli.py           # Click CLI
└── tests/
    └── test_search.py
```

**Python Equivalents:**
```python
from model2vec import StaticModel

# Lazy loading
model = StaticModel.from_pretrained("minishlab/potion-multilingual-128M")

# Embed lines
embeddings = model.encode(lines)  # shape: (n_lines, 128)

# Single query
query_vec = model.encode([query])[0]
```

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-02 | 0.1 | Initial epic draft | Sarah (PO) |
