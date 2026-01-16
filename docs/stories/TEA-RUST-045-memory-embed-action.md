# Story TEA-RUST-045: Rust Memory Embed Action - Cross-Runtime Parity

## Status

Ready for Review

## Story

**As a** developer using The Edge Agent Rust runtime with local LLM support,
**I want** `memory.embed` and `llm.embed` actions available in Rust,
**so that** I can generate vector embeddings for RAG workflows with cross-runtime parity to Python.

## Story Context

**Existing System Integration:**

- Integrates with: `rust/src/actions/llm.rs`, `rust/src/actions/llm_local.rs`
- Technology: Rust `llama-cpp-2` crate for local embeddings
- Follows pattern: Existing `LocalLlmBackend::embed()` method (already implemented)
- Touch points: Action registry, LLM backend trait, feature gates

**Current State:**

| Action | Python | Rust | Gap |
|--------|--------|------|-----|
| `memory.embed` | Full (all LLM backends) | Backend exists, action missing | **This story** |
| `llm.embed` | N/A (alias needed) | Backend exists, action missing | **This story** |
| `llm.chat` | Full | Full (local LLM) | None |
| `llm.call` | Full | Full | None |

**Python Reference Implementation:**

- `memory.embed`: Lines 1045-1140 of `vector_actions.py` - Embedding with multiple backends

**Rust Backend Implementation:**

- `LocalLlmBackend::embed()`: Lines 312-351 of `llm_local.rs` - Already implemented, returns `EmbeddingResult`

## Acceptance Criteria

**Functional Requirements:**

1. `llm.embed` action generates vector embeddings using local LLM when `llm-local` feature is enabled
2. `memory.embed` action is registered as alias to `llm.embed` for Python parity
3. Action returns `embedding` (Vec<f32>), `model`, `dimensions`, and optionally `tokens_used`
4. Action requires `text` parameter (string to embed)
5. Action supports optional `model_path` parameter to override default model
6. Graceful error when `llm-local` feature is not compiled

**Integration Requirements:**

7. Existing `llm.*` actions continue unchanged
8. YAML syntax matches Python `memory.embed` for cross-runtime parity
9. Actions registered in `register()` function in `llm.rs`
10. Error handling follows existing `TeaError` patterns
11. Feature-gated behind `llm-local` to keep base binary lightweight

**Quality Requirements:**

12. Unit tests for embedding parameter validation
13. Integration tests with `#[ignore]` for live embedding tests (requires model)
14. Documentation updated in `docs/rust/actions-reference.md`

## Technical Notes

**Embedding Flow:**

```
YAML action call → llm_embed() → LocalLlmBackend::from_settings() → embed(text) → EmbeddingResult
```

**Existing Backend Method (llm_local.rs:312-351):**

```rust
fn embed(&self, text: &str) -> LlmResult<EmbeddingResult> {
    let ctx_params = LlamaContextParams::default()
        .with_n_ctx(NonZeroU32::new(self.config.n_ctx))
        .with_embeddings(true);

    let mut ctx = self.model.new_context(&self._backend, ctx_params)?;
    let tokens = self.tokenize(text, true)?;

    // ... batch processing ...

    let embeddings = ctx.embeddings_seq_ith(0)?;

    Ok(EmbeddingResult {
        embedding: embeddings.to_vec(),
        model: self.model_name.clone(),
        tokens_used: Some(tokens.len()),
    })
}
```

**YAML Syntax Examples:**

```yaml
# Generate embedding for semantic search
- name: embed_query
  uses: memory.embed
  with:
    text: "{{ state.query }}"
  outputs:
    query_embedding: embedding

# Using llm.embed alias
- name: embed_document
  uses: llm.embed
  with:
    text: "{{ state.document }}"
  outputs:
    doc_embedding: embedding
    embedding_dim: dimensions
```

**Expected Output State:**

```json
{
  "embedding": [0.123, -0.456, 0.789, ...],
  "model": "phi-4-mini",
  "dimensions": 768,
  "tokens_used": 42
}
```

**Key Constraints:**

- Feature-gated: Must compile with `--features llm-local` or `llm-local-vulkan`
- Model required: TEA_MODEL_PATH environment variable or `model_path` param
- Thread-safe: Backend creation per-call (no shared state issues)

## Tasks / Subtasks

- [x] **Task 1: Add llm_embed action function** (AC: 1, 3, 4, 5, 10)
  - [x] Add `llm_embed()` function with `#[cfg(feature = "llm-local")]`
  - [x] Extract `text` parameter (required)
  - [x] Support optional `model_path` parameter
  - [x] Use `LocalLlmBackend::from_settings()` to create backend
  - [x] Call `backend.embed(text)` and return result
  - [x] Map `EmbeddingResult` to output state

- [x] **Task 2: Add stub for non-llm-local builds** (AC: 6)
  - [x] Add `#[cfg(not(feature = "llm-local"))]` version of `llm_embed`
  - [x] Return clear error: "Local LLM not available. Compile with --features llm-local"

- [x] **Task 3: Register actions** (AC: 2, 9)
  - [x] Add `registry.register("llm.embed", llm_embed)`
  - [x] Add `registry.register("memory.embed", llm_embed)` (alias for Python parity)

- [x] **Task 4: Add unit tests** (AC: 12)
  - [x] Test missing `text` parameter returns error
  - [x] Test parameter extraction logic
  - [x] Test `#[cfg(not(feature = "llm-local"))]` returns proper error

- [x] **Task 5: Add integration tests** (AC: 13)
  - [x] Add `#[ignore]` test for live embedding with local model
  - [x] Test embedding returns valid vector with correct dimensions
  - [x] Document test execution with model path

- [x] **Task 6: Update documentation** (AC: 14)
  - [x] Add `llm.embed` / `memory.embed` to `docs/rust/actions-reference.md`
  - [x] Update action comparison table (Rust vs Python)
  - [x] Add example YAML snippet

- [x] **Task 7: Add CI smoke test** (AC: 7)
  - [x] Add embedding smoke test to `build-rust-llm.yaml` workflow
  - [x] Verify AppImage with bundled model can generate embeddings

## Definition of Done

- [x] Functional requirements met (`llm.embed`, `memory.embed` implemented)
- [x] Feature gate working (error without `llm-local`, success with it)
- [x] Existing `llm.*` actions unchanged (no regression)
- [x] Code follows existing Rust patterns and standards
- [x] Tests pass (`cargo test --features llm`)
- [x] Clippy warnings resolved (`cargo clippy --features llm`)
- [x] Documentation updated

## Risk and Compatibility Check

**Minimal Risk Assessment:**

- **Primary Risk:** Model path resolution edge cases
- **Mitigation:** Use existing `resolve_model_path()` helper from `llm_backend.rs`
- **Rollback:** New actions only - no changes to existing functionality

**Compatibility Verification:**

- [x] No breaking changes to existing `llm.*` API
- [x] Database changes: N/A
- [x] UI changes: N/A
- [x] Performance impact: Negligible (embedding is fast operation)

## Dev Notes

**Relevant Source Tree:**

```
rust/src/actions/
├── mod.rs              # Action registry exports
├── llm.rs              # Main file to modify (~1800 lines)
├── llm_local.rs        # LocalLlmBackend::embed() already implemented
└── llm_backend.rs      # LlmBackend trait, EmbeddingResult struct

.github/workflows/
└── build-rust-llm.yaml # Add smoke test for embedding
```

**Existing Helpers Available:**

- `LocalLlmBackend::from_settings()` - Creates backend from settings
- `resolve_model_path()` - Finds model file from env var or default paths
- `get_llm_settings()` - Extracts LLM settings from state

**Dependencies:**

No new dependencies - uses existing `llama-cpp-2` via `llm-local` feature.

**Environment Variables:**

- `TEA_MODEL_PATH` - Path to GGUF model file (existing)

### Testing

**Test file location:** `rust/src/actions/llm.rs` (inline tests)

**Test standards:**
- Unit tests in `#[cfg(test)]` module with `#[cfg(feature = "llm-local")]`
- Integration tests with `#[ignore]` attribute
- Run with `TEA_MODEL_PATH=model.gguf cargo test --features llm-local -- --ignored`

**Recommended Test Models:**
- **Embedding:** Any GGUF model - embedding uses model's internal vectors
- Suggest: Same model as bundled in LLM AppImage (Phi-4 mini or Gemma 3 1B)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-16 | 0.1 | Initial draft from Correct Course analysis | Sarah (PO Agent) |
| 2026-01-16 | 1.0 | Implementation complete - all tasks done | James (Dev Agent) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Action | Description |
|------|--------|-------------|
| `rust/src/actions/llm.rs` | Modified | Added `llm_embed()` function, registered `llm.embed` and `memory.embed` actions, added unit tests |
| `rust/src/actions/llm_local.rs` | Modified | Added integration tests for embedding generation |
| `docs/rust/actions-reference.md` | Modified | Updated documentation for `llm.embed`/`memory.embed`, updated LLM and Memory action tables |
| `.github/workflows/build-rust-llm.yaml` | Modified | Added embedding smoke test to Gemma 3 1B AppImage builds (x86_64 and aarch64) |

### Completion Notes

1. Implemented `llm_embed()` function with full feature-gate support (lines 1198-1294 of llm.rs)
2. Both `llm.embed` and `memory.embed` actions registered for Python parity
3. Returns `embedding`, `model`, `dimensions`, and `tokens_used` matching Python output format
4. 5 unit tests added and passing (test_llm_embed_*)
5. 3 integration tests added with `#[ignore]` attribute for live model testing
6. CI smoke test added to Gemma 3 1B AppImage workflows
7. All existing tests pass with no regressions

### Debug Log References

No debug issues encountered during implementation.

---

## Sprint Change Proposal Reference

This story was created from Sprint Change Proposal analysis performed on 2025-01-16.

**Issue:** `memory.embed` action not implemented in Rust, causing `Action not found: memory.embed` errors.

**Root Cause:** Backend `LocalLlmBackend::embed()` exists but no action registered to expose it.

**Solution:** Register `llm.embed` and `memory.embed` actions that delegate to existing backend.

---

## QA Results

### Review Date: 2026-01-16

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: Excellent implementation.** The code follows existing patterns consistently, has comprehensive documentation, and includes appropriate test coverage at multiple levels. The implementation is clean, well-structured, and properly feature-gated.

**Strengths:**
- Clean function signature matching existing `llm_*` action patterns
- Comprehensive doc comments with YAML examples
- Proper error handling using existing `TeaError` types
- Feature-gate implementation handles both `#[cfg]` branches correctly
- Unit tests cover all parameter validation scenarios
- Integration tests verify actual embedding behavior with cosine similarity checks
- CI smoke test ensures AppImage deployment works

### Refactoring Performed

No refactoring required. Implementation quality is production-ready.

### Compliance Check

- Coding Standards: ✓ Follows existing patterns in llm.rs
- Project Structure: ✓ Changes isolated to appropriate files
- Testing Strategy: ✓ Unit + integration + CI smoke tests
- All ACs Met: ✓ All 14 acceptance criteria verified

### AC Traceability

| AC | Description | Test Coverage |
|----|-------------|---------------|
| 1 | llm.embed generates embeddings | test_llm_embed_text_param_extraction, test_embedding_generation |
| 2 | memory.embed registered as alias | Verified in register() function |
| 3 | Returns embedding, model, dimensions, tokens_used | test_embedding_generation assertions |
| 4 | Requires text parameter | test_llm_embed_missing_text, test_llm_embed_missing_text_with_other_params |
| 5 | Supports optional model_path | build_llm_settings() reuse, documented in params |
| 6 | Graceful error without llm-local | test_llm_embed_without_feature_returns_error |
| 7 | Existing llm.* unchanged | No regression - all existing tests pass |
| 8 | YAML syntax matches Python | Documentation verified in actions-reference.md |
| 9 | Actions registered in register() | Lines 19-20 of llm.rs |
| 10 | Error handling uses TeaError | Lines 1230, 1242, 1252, 1260, 1265 |
| 11 | Feature-gated behind llm-local | #[cfg(feature = "llm-local")] at line 1236 |
| 12 | Unit tests for validation | 5 unit tests in test module |
| 13 | Integration tests with #[ignore] | 3 integration tests in llm_local.rs |
| 14 | Documentation updated | actions-reference.md lines 219-276 |

### Improvements Checklist

All items handled - no outstanding issues:

- [x] Implementation follows existing patterns
- [x] All acceptance criteria met
- [x] Unit tests comprehensive
- [x] Integration tests properly marked with #[ignore]
- [x] CI smoke test added to AppImage builds
- [x] Documentation complete with examples
- [x] No security concerns (text input is passed to existing backend)
- [x] No performance concerns (embedding is single operation)

### Security Review

**No security concerns.** The action:
- Only accepts text input (string)
- Delegates to existing trusted `LocalLlmBackend::embed()` method
- No filesystem access beyond existing model loading
- No network access
- Feature-gated to prevent accidental exposure

### Performance Considerations

**No performance concerns.** Embedding generation is:
- O(n) on input token count
- Single synchronous operation (no batching needed for single text)
- Backend creation per-call is expected pattern (same as llm.chat)

### Files Modified During Review

None - implementation quality was production-ready.

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-RUST-045-memory-embed-action.yml

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, tests pass, documentation complete.
