# Story TEA-BUILTIN-014.2: Rust Semantic Search Actions

> **DEPRECATED**: This story has been superseded by unified RAG extensions.
> Rust parity will be addressed when porting the unified `vector.*` actions.
>
> See TEA-BUILTIN-014-semantic-search-epic.md for rationale.

## Status
**Superseded**

## Story

**As a** workflow developer using the Rust runtime,
**I want** built-in semantic search actions equivalent to the Python version,
**so that** my workflows produce consistent results regardless of runtime choice.

## Story Context

**Existing System Integration:**
- Integrates with: Rust actions system (`rust/src/actions/` directory)
- Technology: Rust, model2vec (Rust bindings), simsimd, walkdir
- Follows pattern: `rust/src/actions/ratelimit.rs` (global registry with OnceLock)
- Touch points: `actions/mod.rs`, `engine/executor.rs` (action invocation)

**Reference Implementation:** [semtools](https://github.com/run-llama/semtools)
- Core search: [`src/search/mod.rs`](https://github.com/run-llama/semtools/blob/main/src/search/mod.rs)
  - `search_documents()` lines 77-120, `create_document_from_content()` lines 40-75
- Store patterns: [`src/workspace/store.rs`](https://github.com/run-llama/semtools/blob/main/src/workspace/store.rs)
  - `analyze_document_states()` lines 763-828
- CLI: [`src/bin/search.rs`](https://github.com/run-llama/semtools/blob/main/src/bin/search.rs)

**Dependency:** Requires TEA-BUILTIN-014.1 (Python) completed first for API alignment verification.

## Acceptance Criteria

**Functional Requirements:**

1. `search.semantic` action searches files/directories for semantically similar content
2. `search.semantic_content` action searches string content (no file I/O)
3. Paths support local filesystem only (cloud URIs out of scope for Rust)
4. Recursive directory traversal enabled by default
5. Extension filtering limits search to specified file types
6. Results schema matches Python implementation exactly
7. `top_k` limits number of results returned
8. `max_distance` threshold filters low-confidence matches
9. `n_lines` parameter controls context window around matches
10. `ignore_case` enables case-insensitive search

**Integration Requirements:**

11. Model is lazy-loaded on first action invocation using `OnceLock`
12. Model is cached globally (process-level, shared across all executors)
13. Actions register as `search.semantic` and `search.semantic_content`
14. Works with YAML `uses:` syntax and `output:` key mapping
15. Existing actions remain unaffected

**Cross-Runtime Parity:**

16. Same embedding model as Python (`minishlab/potion-multilingual-128M`)
17. Same cosine distance calculation
18. Same result schema and field names
19. Same default values for optional parameters

**Quality Requirements:**

20. Unit tests for search logic (similarity, ranking, context window)
21. Unit tests for file listing and reading
22. Integration test with sample directory search
23. Feature-gated behind `search` Cargo feature
24. Error handling for missing files, unreadable content, model loading failures

## Tasks / Subtasks

- [ ] **Task 1: Add dependencies** (AC: 16, 23)
  - [ ] Add `model2vec` crate to Cargo.toml under `[dependencies]` with `optional = true`
  - [ ] Add `simsimd` crate for fast cosine distance
  - [ ] Add `walkdir` crate for directory traversal
  - [ ] Create `search` feature flag enabling these dependencies
  - [ ] Update `rust/Cargo.toml` with feature definitions

- [ ] **Task 2: Create search module** (AC: 11, 12)
  - [ ] Create `rust/src/search/mod.rs` with SearchConfig, SearchResult structs
  - [ ] Create `rust/src/search/model.rs` with OnceLock model loading
  - [ ] Implement `load_model() -> &'static StaticModel`
  - [ ] Export search module from `lib.rs` (feature-gated)

- [ ] **Task 3: Implement core search logic** (AC: 2, 6, 7, 8, 9, 10, 17)
  - [ ] Implement `embed_lines(lines: &[String]) -> Vec<Vec<f32>>`
  - [ ] Implement `cosine_distance(a: &[f32], b: &[f32]) -> f32` using simsimd
  - [ ] Implement `search_content(content: &str, query: &str, config: &SearchConfig) -> Vec<SearchResult>`
  - [ ] Handle context window calculation with boundary clamping
  - [ ] Handle case-insensitive mode (lowercase before embedding)

- [ ] **Task 4: Implement file/directory search** (AC: 1, 3, 4, 5)
  - [ ] Implement `list_files(paths: &[String], recursive: bool, extensions: &[String]) -> Vec<String>`
  - [ ] Use `walkdir` for directory traversal
  - [ ] Use `std::fs::read_to_string()` for file reading
  - [ ] Implement `search_files(paths, query, config) -> Vec<SearchResult>`
  - [ ] Handle mixed paths (some files, some directories)

- [ ] **Task 5: Create action handlers** (AC: 13, 14, 15)
  - [ ] Create `rust/src/actions/search.rs`
  - [ ] Implement `search_semantic(state, params) -> TeaResult<JsonValue>` handler
  - [ ] Implement `search_semantic_content(state, params) -> TeaResult<JsonValue>` handler
  - [ ] Implement `register(registry: &ActionRegistry)` function
  - [ ] Register in `actions/mod.rs` (feature-gated)

- [ ] **Task 6: Ensure cross-runtime parity** (AC: 16, 17, 18, 19)
  - [ ] Verify same model name constant
  - [ ] Verify cosine distance formula matches Python
  - [ ] Verify result JSON schema matches Python output
  - [ ] Verify default parameter values match Python

- [ ] **Task 7: Testing** (AC: 20, 21, 22, 24)
  - [ ] Test cosine distance calculation
  - [ ] Test context window edge cases (first/last lines of file)
  - [ ] Test case-insensitive search
  - [ ] Test top_k limiting
  - [ ] Test max_distance threshold filtering
  - [ ] Test extension filtering
  - [ ] Test recursive vs non-recursive directory search
  - [ ] Test error handling for missing files
  - [ ] Integration test: search sample code directory

## Dev Notes

### Key Implementation References

**From semtools [`src/search/mod.rs`](https://github.com/run-llama/semtools/blob/main/src/search/mod.rs):**

```rust
use model2vec::StaticModel;
use simsimd::f32::cosine;

pub const MODEL_NAME: &str = "minishlab/potion-multilingual-128M";

// Load model (downloads on first use, cached in ~/.cache/huggingface/)
let model = StaticModel::from_pretrained(MODEL_NAME).unwrap();

// Embed lines
let embeddings = model.encode_with_args(&lines, Some(2048), 16384);

// Embed single query
let query_embedding = model.encode_single(query);

// Cosine distance using simsimd
let distance = f32::cosine(&query_embedding, &line_embedding).unwrap();
```

**Context Window Calculation:**
```rust
let start = idx.saturating_sub(config.n_lines);
let end = std::cmp::min(lines.len(), idx + config.n_lines + 1);
```

### Data Structures

```rust
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchConfig {
    pub n_lines: usize,
    pub top_k: usize,
    pub max_distance: Option<f64>,
    pub ignore_case: bool,
    pub recursive: bool,
    pub extensions: Option<Vec<String>>,
}

impl Default for SearchConfig {
    fn default() -> Self {
        Self {
            n_lines: 3,
            top_k: 10,
            max_distance: None,
            ignore_case: false,
            recursive: true,
            extensions: None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResult {
    pub filename: String,
    pub lines: Vec<String>,
    pub start: usize,       // 0-based
    pub end: usize,         // Exclusive
    pub match_line: usize,  // 0-based index of matched line
    pub distance: f32,
}
```

### Module Structure

```
rust/src/
├── search/
│   ├── mod.rs               # SearchConfig, SearchResult, search_files, search_content
│   └── model.rs             # OnceLock model loading
├── actions/
│   ├── mod.rs               # Add search::register() (feature-gated)
│   └── search.rs            # Action handlers
```

### Action Handler Signatures

```rust
fn search_semantic(
    state: &JsonValue,
    params: &HashMap<String, JsonValue>,
) -> TeaResult<JsonValue> {
    // Extract parameters
    let query = params.get("query")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "search.semantic".to_string(),
            message: "Missing required parameter: query".to_string(),
        })?;

    let paths = params.get("paths")
        .and_then(|v| v.as_array())
        .map(|arr| arr.iter().filter_map(|v| v.as_str().map(String::from)).collect())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "search.semantic".to_string(),
            message: "Missing required parameter: paths".to_string(),
        })?;

    // ... extract optional parameters with defaults ...

    let results = search_files(&paths, query, &config)?;

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("results".to_string(), serde_json::to_value(&results)?);
        obj.insert("success".to_string(), json!(true));
        obj.insert("query".to_string(), json!(query));
        obj.insert("files_searched".to_string(), json!(files_count));
    }
    Ok(result)
}
```

### Model Caching Pattern

```rust
use std::sync::OnceLock;
use model2vec::StaticModel;

static MODEL: OnceLock<StaticModel> = OnceLock::new();

pub fn get_model() -> &'static StaticModel {
    MODEL.get_or_init(|| {
        StaticModel::from_pretrained("minishlab/potion-multilingual-128M")
            .expect("Failed to load embedding model")
    })
}
```

### Feature Gating

**Cargo.toml:**
```toml
[features]
default = []
search = ["model2vec", "simsimd", "walkdir"]

[dependencies]
model2vec = { version = "0.x", optional = true }
simsimd = { version = "0.x", optional = true }
walkdir = { version = "2", optional = true }
```

**actions/mod.rs:**
```rust
#[cfg(feature = "search")]
pub mod search;

pub fn register_defaults(registry: &ActionRegistry) {
    // ... other registrations ...

    #[cfg(feature = "search")]
    search::register(registry);
}
```

### Error Handling

Use existing TeaError variants:
```rust
TeaError::InvalidInput { action, message }  // Missing/invalid parameters
TeaError::Action(String)                     // Runtime errors
```

## Testing

- **Test location:** `rust/tests/test_search.rs`
- **Framework:** Built-in Rust testing (`cargo test`)
- **Key test cases:**

| Test Case | Priority | Description |
|-----------|----------|-------------|
| `test_cosine_distance_identical` | P0 | Identical vectors return 0.0 |
| `test_cosine_distance_orthogonal` | P0 | Orthogonal vectors return ~1.0 |
| `test_search_content_basic` | P0 | Basic content search returns results |
| `test_search_content_top_k` | P1 | top_k limits results |
| `test_search_content_max_distance` | P1 | max_distance filters results |
| `test_search_content_context_window` | P1 | Context includes n_lines before/after |
| `test_search_content_boundary_clamp` | P1 | Context clamps to file boundaries |
| `test_search_content_ignore_case` | P2 | Case-insensitive mode works |
| `test_search_files_directory` | P1 | Directory search finds files |
| `test_search_files_recursive` | P1 | Recursive search traverses subdirs |
| `test_search_files_extensions` | P1 | Extension filter limits files |
| `test_action_search_semantic` | P0 | Action integrates correctly |
| `test_action_search_semantic_content` | P0 | Content action integrates correctly |
| `test_model_lazy_loading` | P1 | Model not loaded until first use |
| `test_model_cached` | P1 | Model reused across calls |
| `test_error_missing_file` | P1 | Missing file returns error |
| `test_cross_runtime_parity` | P0 | Same input produces same output as Python |

**Test with feature flag:**
```bash
cargo test --features search
```

## Risk Assessment

**Primary Risk:** model2vec Rust crate may have different behavior than Python version
**Mitigation:** Cross-runtime parity test with fixed input/output comparison

**Secondary Risk:** Large binary size increase when `search` feature enabled
**Mitigation:** Feature-gated, not included in default build

**Rollback:** Disable `search` feature; no impact on existing workflows

## Definition of Done

- [ ] All functional requirements met
- [ ] Actions registered and working in YAML workflows
- [ ] Model lazy loading and caching verified
- [ ] Cross-runtime parity with Python verified
- [ ] Unit tests pass (`cargo test --features search`)
- [ ] Integration test passes with real model
- [ ] Dependencies feature-gated correctly
- [ ] Existing tests still pass without feature
- [ ] Clippy passes with no warnings

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-02 | 0.1 | Initial draft | Sarah (PO) |
