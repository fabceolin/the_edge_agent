# Story TEA-RALPHY-001.2: Rust `markdown.parse` Action

## Status
Draft

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

### Phase 0: Shared Crate (Prerequisite - see TEA-RALPHY-001.1)

- [ ] Implement md-graph-parser crate (tracked in TEA-RALPHY-001.1)

### Phase 1: TEA Integration

- [ ] Add md-graph-parser as git dependency (AC: 1, 2, 3, 4, 5)
  - [ ] Add to `rust/Cargo.toml` with features `["serde", "frontmatter"]`
  - [ ] Verify compilation
- [ ] Create action wrapper (AC: 1)
  - [ ] Add `rust/src/actions/markdown.rs`
  - [ ] Wrap `md_graph_parser::MarkdownParser` as TEA action
  - [ ] Register in action registry
- [ ] Add WASM bindings (AC: 6)
  - [ ] Export via wasm-bindgen
  - [ ] JSON serialization for JS interop
- [ ] Add integration tests
  - [ ] Test action invocation
  - [ ] Test YAML agent usage

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

_To be filled by development agent_

### Debug Log References

_To be filled by development agent_

### Completion Notes List

_To be filled by development agent_

### File List

_To be filled by development agent_

---

## QA Results

_To be filled by QA agent after implementation review_
