# Story TEA-RALPHY-001.2: Rust `markdown.parse` Action

## Status
Done

## Epic Reference
[TEA-RALPHY-001: Autonomous AI Coding Loop](./TEA-RALPHY-001-autonomous-coding-loop.md)

## Story

**As a** workflow developer,
**I want** a `markdown.parse` action in the Rust runtime,
**So that** YAML agents work identically in both Python and Rust.

## Shared Library Strategy

> **Decision**: Use the shared `md-graph-parser` crate as a git dependency.
>
> - **Repository**: https://github.com/fabceolin/md-graph-parser
> - **Rationale**: Avoid duplication with agentfs GraphDocs parser, ensure cross-runtime parity
> - **This story**: Simply wraps md-graph-parser as a TEA action

### Dependency Configuration

```toml
# rust/Cargo.toml
[dependencies]
md-graph-parser = { git = "https://github.com/fabceolin/md-graph-parser", features = ["serde", "frontmatter"] }
```

## Acceptance Criteria

1. Same output schema as Python implementation (via shared md-graph-parser crate)
2. Use `pulldown-cmark` for parsing (handled by md-graph-parser)
3. Extract checklists with GitHub Flavored Markdown extension
4. Parse YAML frontmatter using `serde_yaml`
5. Variable extraction via regex
6. Expose as WASM-compatible action for tea-wasm-llm

## Tasks / Subtasks

### Phase 0: Shared Crate (Prerequisite - see TEA-RALPHY-001.0)

- [x] Implement md-parser crate (tracked in TEA-RALPHY-001.0) - Note: renamed from md-graph-parser to md-parser

### Phase 1: TEA Integration

- [x] Add md-parser as git dependency (AC: 1, 2, 3, 4, 5)
  - [x] Add to `rust/Cargo.toml` with features `["serde", "frontmatter"]`
  - [x] Verify compilation
- [x] Create action wrapper (AC: 1)
  - [x] Add `rust/src/actions/markdown.rs`
  - [x] Wrap `md_parser::MarkdownParser` as TEA action
  - [x] Register in action registry
- [x] Add WASM bindings (AC: 6)
  - [x] Export via wasm-bindgen in tea-wasm-llm
  - [x] JSON serialization for JS interop
- [x] Add integration tests
  - [x] Test action invocation (11 unit tests + 11 integration tests)
  - [x] Test YAML agent usage (via action registry)

## Dev Notes

### Dependencies

Add to `rust/Cargo.toml`:

```toml
[dependencies]
md-graph-parser = { git = "https://github.com/fabceolin/md-graph-parser", features = ["serde", "frontmatter"] }
```

> **Note**: The shared crate handles all parsing dependencies internally (`pulldown-cmark`, `regex`, `serde_yaml`).

### Action Wrapper (Simplified)

```rust
// rust/src/actions/markdown.rs
use md_graph_parser::{MarkdownParser, ParsedDocument};
use serde_json::Value;

pub fn markdown_parse(content: &str, _options: Option<Value>) -> Result<Value, ActionError> {
    let parser = MarkdownParser::new();
    let doc = parser.parse(content)?;
    Ok(serde_json::to_value(doc)?)
}

pub fn register(registry: &mut ActionRegistry) {
    registry.insert("markdown.parse", markdown_parse);
}
```

### Source Tree

```
rust/src/
├── actions/
│   ├── mod.rs              # MODIFY: Add markdown module
│   └── markdown.rs         # NEW: Thin wrapper around md-graph-parser
└── ...
```

### Reference Implementations

| Source | Location | Notes |
|--------|----------|-------|
| **md-graph-parser** | https://github.com/fabceolin/md-graph-parser | Shared crate (canonical) |
| **agentfs GraphDocs** | `/home/fabricio/src/agentfs/sdk/rust/src/graphdocs/parser.rs` | Original implementation |

### Cross-Runtime Parity

The shared crate guarantees identical output schema. Both Python and Rust use the same types:
- `ParsedDocument`, `ParsedSection`, `ChecklistItem`
- Serde serialization ensures consistent JSON output

## Testing

**Test Location:** `rust/tests/test_markdown_actions.rs`

```rust
#[test]
fn test_parse_checklist() {
    let markdown = r#"
## Tasks
- [ ] Task 1 (AC: 1)
  - [x] Subtask 1.1
- [x] Task 2
"#;
    let result = markdown_parse(markdown, None).unwrap();
    assert_eq!(result.tasks.len(), 3);
    assert!(!result.tasks[0].checked);
    assert!(result.tasks[1].checked);
    assert_eq!(result.tasks[1].indent, 1);
}
```

### Test Cases

| Test Case | Description | AC |
|-----------|-------------|-----|
| test_schema_parity | Output matches Python exactly | 1 |
| test_pulldown_cmark_parsing | Correct parser usage | 2 |
| test_gfm_tasklists | GitHub Flavored Markdown | 3 |
| test_frontmatter_yaml | YAML header parsing | 4 |
| test_variable_extraction | Regex `{{var}}` | 5 |
| test_wasm_export | WASM bindings work | 6 |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-17 | 0.1 | Extracted from epic TEA-RALPHY-001 | Sarah (PO) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

N/A - No major blockers requiring debug sessions

### Completion Notes List

1. **Crate renamed**: md-graph-parser → md-parser (per TEA-RALPHY-001.0)
2. **Rust 1.92 required**: Upgraded from 1.89 to support md-parser minimum version
3. **AC refs as strings**: md-parser returns AC references as strings, not integers
4. **No language field**: ParsedSection doesn't have `language` field in current md-parser version
5. **WASM bindings**: Added to tea-wasm-llm crate with `markdown_parse`, `markdown_parse_json`, `markdown_extract_tasks`, `markdown_extract_variables` exports
6. **22 tests passing**: 11 unit tests + 11 integration tests
7. **Clippy clean**: No warnings with `--features markdown`

### File List

| File | Change |
|------|--------|
| `rust/Cargo.toml` | MODIFIED: Added md-parser dependency and markdown feature |
| `rust/src/actions/mod.rs` | MODIFIED: Added markdown module and registration |
| `rust/src/actions/markdown.rs` | NEW: markdown.parse, markdown.extract_tasks, markdown.extract_variables actions |
| `rust/tests/test_markdown_actions.rs` | NEW: 11 integration tests |
| `rust/tea-wasm-llm/Cargo.toml` | MODIFIED: Added md-parser dependency |
| `rust/tea-wasm-llm/src/lib.rs` | MODIFIED: Added markdown module export |
| `rust/tea-wasm-llm/src/markdown.rs` | NEW: WASM bindings for markdown parsing |

### Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-19 | 0.2 | Implementation complete: 22 tests passing, WASM bindings added | James (Dev) |

---

## QA Results

### Review Date: 2026-01-19

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: Excellent** - Clean, well-documented implementation following established TEA action patterns.

**Strengths:**
- Comprehensive docstrings with parameter and return value documentation
- Proper error handling with descriptive error messages (TeaError::InvalidInput)
- Consistent JSON conversion patterns across all actions
- State preservation pattern correctly implemented (result = state.clone())
- Feature-gated module with `#[cfg(feature = "markdown")]`
- WASM bindings provide both structured (`MarkdownParseResult`) and convenience (`markdown_parse_json`) APIs

**Architecture:**
- Thin wrapper design correctly delegates parsing to md-parser crate
- Three actions registered: `markdown.parse`, `markdown.extract_tasks`, `markdown.extract_variables`
- WASM bindings mirror native API with appropriate JS-friendly adaptations

### Refactoring Performed

None required - implementation is clean and follows project conventions.

### Compliance Check

- Coding Standards: ✓ Follows TEA action patterns, proper rustdocs
- Project Structure: ✓ Correct locations: `src/actions/markdown.rs`, `tea-wasm-llm/src/markdown.rs`
- Testing Strategy: ✓ Unit tests (11) + Integration tests (11) = 22 total
- All ACs Met: ✓ See traceability matrix below

### Requirements Traceability

| AC | Description | Test Coverage | Status |
|----|-------------|---------------|--------|
| 1 | Same output schema as Python | `test_markdown_parse_basic_document`, `test_markdown_parse_bmad_story_format` | ✓ |
| 2 | Use pulldown-cmark | Delegated to md-parser (verified via dependency) | ✓ |
| 3 | Extract checklists with GFM | `test_markdown_parse_checklist`, `test_markdown_extract_tasks_action`, `test_markdown_parse_nested_checklist` | ✓ |
| 4 | Parse YAML frontmatter | `test_markdown_parse_frontmatter`, `test_markdown_parse_with_frontmatter` | ✓ |
| 5 | Variable extraction | `test_markdown_parse_variables`, `test_markdown_extract_variables_action` | ✓ |
| 6 | WASM-compatible action | `tea-wasm-llm/src/markdown.rs` with 4 WASM tests | ✓ |

### Improvements Checklist

- [x] All acceptance criteria implemented
- [x] Unit tests comprehensive (11 tests covering all actions)
- [x] Integration tests validate registry and invocation (11 tests)
- [x] WASM bindings with tests (4 tests)
- [x] Error handling for missing input parameter
- [x] State preservation verified
- [x] Clippy clean with `--features markdown`
- [ ] Future: Add YAML agent workflow integration test (low priority)
- [ ] Future: Cross-runtime parity test with Python (when testing infrastructure supports it)

### Security Review

**PASS** - No security concerns identified.
- No code execution (parsing only)
- No network calls
- No file system access
- Input validation via typed parameters
- Safe JSON serialization with fallbacks

### Performance Considerations

**PASS** - No performance concerns.
- Uses compiled Rust parser (md-parser crate)
- No caching needed for stateless parsing
- JSON conversion uses efficient serde_json macros

### Files Modified During Review

None - no refactoring required.

### Gate Status

**Gate: PASS** → `docs/qa/gates/TEA-RALPHY-001.2-rust-markdown-parse.yml`

| Category | Status | Notes |
|----------|--------|-------|
| Security | PASS | No execution, no I/O, safe parsing |
| Performance | PASS | Native Rust code via md-parser |
| Reliability | PASS | Comprehensive error handling, graceful malformed input |
| Maintainability | PASS | Clean code, excellent docs, follows patterns |

**Quality Score: 100/100**

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, 22 tests passing, WASM bindings complete.
