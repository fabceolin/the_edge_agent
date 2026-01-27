# Epic: TEA-RUST-043 - LLM Module Modularization

## Status: Draft

## Epic Goal

Split `rust/src/actions/llm.rs` (2,986 lines, 102KB) into smaller, focused modules to improve maintainability, reduce compilation times, and make the codebase easier to navigate and test.

## Epic Description

### Existing System Context

- **Current relevant functionality:** `llm.rs` provides all LLM integration actions including basic calls, streaming, tool-calling, chat, and embeddings
- **Technology stack:** Rust, reqwest for HTTP, serde for serialization, existing `llm_backend.rs` and `llm_local.rs` modules
- **Integration points:**
  - `mod.rs` registers LLM actions via `register()` function
  - `llm_backend.rs` provides shared LlmSettings and backend logic
  - `llm_local.rs` provides local LLM execution

### Current File Structure Analysis

| Section | Lines | Description |
|---------|-------|-------------|
| Core Types | 1-63 | Message, CompletionRequest/Response, Choice, Usage |
| Streaming Types | 64-100 | Delta, StreamChoice, StreamChunk |
| Tools Types | 102-206 | FunctionDefinition, Tool, ToolCall, MessageWithTools |
| Provider Config | 208-290 | ProviderConfig, build_provider_config() |
| llm_call | 291-438 | Basic LLM call implementation |
| llm_stream | 439-616 | Streaming LLM implementation |
| llm_tools | 617-961 | Tool-calling LLM implementation |
| llm_chat | 962-1307 | Chat/conversational LLM |
| llm_embed | 1308-1390 | Embeddings |
| Tests | 1391-2986 | ~1,600 lines of tests |

### Enhancement Details

- **What's being changed:** Refactoring monolithic `llm.rs` into a modular `llm/` directory structure
- **How it integrates:** Public API remains unchanged; internal reorganization only
- **Success criteria:**
  - No single file exceeds 800 lines
  - All existing tests pass
  - `cargo fmt --check` and `cargo clippy` pass
  - Public API unchanged (no breaking changes)

### Target Structure

```
rust/src/actions/
├── llm/
│   ├── mod.rs          # Re-exports, register(), llm_call()
│   ├── types.rs        # All structs: Message, CompletionRequest, ProviderConfig, etc.
│   ├── provider.rs     # build_provider_config() and provider logic
│   ├── stream.rs       # llm_stream(), parse_sse_response()
│   ├── tools.rs        # llm_tools(), convert_tool_to_openai_format()
│   ├── chat.rs         # llm_chat(), execute_local_chat(), execute_api_chat()
│   └── embed.rs        # llm_embed()
├── llm_backend.rs      # (unchanged)
├── llm_local.rs        # (unchanged)
└── mod.rs              # Update to use `mod llm;` instead of single file
```

## Stories

### Story 1: TEA-RUST-043.1 - Extract Types and Provider Configuration

**Goal:** Create `llm/` directory structure with `types.rs` and `provider.rs`

**Scope:**
- Create `rust/src/actions/llm/` directory
- Extract all struct definitions to `types.rs` (~200 lines)
- Extract `ProviderConfig` and `build_provider_config()` to `provider.rs` (~100 lines)
- Create `mod.rs` that re-exports everything and contains `register()` and `llm_call()`
- Update `actions/mod.rs` to use the new module structure
- Ensure all tests pass

**Acceptance Criteria:**
1. `llm/types.rs` contains all struct definitions
2. `llm/provider.rs` contains provider configuration logic
3. `llm/mod.rs` re-exports public items maintaining API compatibility
4. `cargo test` passes
5. `cargo fmt --check` passes
6. `cargo clippy -- -D warnings` passes

---

### Story 2: TEA-RUST-043.2 - Extract Streaming and Tools Modules

**Goal:** Extract streaming and tool-calling functionality into separate modules

**Scope:**
- Create `llm/stream.rs` with `llm_stream()` and `parse_sse_response()` (~180 lines)
- Create `llm/tools.rs` with `llm_tools()` and `convert_tool_to_openai_format()` (~350 lines)
- Move relevant tests to each module
- Update `mod.rs` imports and re-exports

**Acceptance Criteria:**
1. `llm/stream.rs` contains all streaming functionality
2. `llm/tools.rs` contains all tool-calling functionality
3. Tests for streaming are in `stream.rs`
4. Tests for tools are in `tools.rs`
5. All tests pass
6. CI checks pass

---

### Story 3: TEA-RUST-043.3 - Extract Chat and Embed Modules

**Goal:** Complete modularization by extracting chat and embed functionality

**Scope:**
- Create `llm/chat.rs` with `llm_chat()`, `execute_local_chat()`, `execute_api_chat()` (~350 lines)
- Create `llm/embed.rs` with `llm_embed()` (~90 lines)
- Move remaining tests to appropriate modules
- Final cleanup of `mod.rs` (should be <200 lines)
- Verify no file exceeds 800 lines

**Acceptance Criteria:**
1. `llm/chat.rs` contains all chat functionality
2. `llm/embed.rs` contains embeddings functionality
3. `llm/mod.rs` is <200 lines (only register() and re-exports)
4. No individual module exceeds 800 lines
5. All tests pass
6. CI checks pass (fmt, clippy, doc)
7. Binary size unchanged (within 1%)

## Compatibility Requirements

- [x] Existing APIs remain unchanged (all functions remain public with same signatures)
- [x] No database/schema changes
- [x] No UI changes
- [x] Performance impact is minimal (compile time should improve)

## Risk Mitigation

- **Primary Risk:** Breaking public API or internal references during refactoring
- **Mitigation:**
  - Each story includes running full test suite
  - Use `pub use` re-exports to maintain API compatibility
  - Run `cargo doc` to verify documentation links still work
- **Rollback Plan:** Git revert; each story is a single commit

## Definition of Done

- [x] All 3 stories completed with acceptance criteria met
- [x] Existing functionality verified through testing (all tests pass)
- [x] Integration points working correctly (llm_backend.rs, llm_local.rs unchanged)
- [x] No regression in existing features
- [x] CI pipeline passes (fmt, clippy, tests, doc)

## Technical Notes

### Module Visibility Strategy

```rust
// In llm/mod.rs
mod types;
mod provider;
mod stream;
mod tools;
mod chat;
mod embed;

// Re-export public items
pub use types::*;
pub(crate) use provider::build_provider_config;

// Keep register() here
pub fn register(registry: &ActionRegistry) { ... }
```

### Test Organization

Each module should contain its own `#[cfg(test)] mod tests { }` block with tests specific to that module's functionality. Integration tests that span modules remain in `mod.rs` or move to `rust/tests/`.

## Story Dependencies

```
TEA-RUST-043.1 (Types/Provider)
       │
       ▼
TEA-RUST-043.2 (Stream/Tools)
       │
       ▼
TEA-RUST-043.3 (Chat/Embed)
```

Stories must be completed in order as each builds on the module structure established by the previous.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-27 | 1.0 | Initial epic creation | Sarah (PO Agent) |
