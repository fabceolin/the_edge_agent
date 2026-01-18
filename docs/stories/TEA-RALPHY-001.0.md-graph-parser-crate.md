# Story TEA-RALPHY-001.0: md-graph-parser Shared Crate

## Status
Draft

## Epic Reference
[TEA-RALPHY-001: Autonomous AI Coding Loop](./TEA-RALPHY-001-autonomous-coding-loop.md)

## Story

**As a** library maintainer,
**I want** a shared Rust crate for structured Markdown parsing,
**So that** both TEA and agentfs can use the same parser with guaranteed schema parity.

## Repository

- **URL**: https://github.com/fabceolin/md-graph-parser
- **Local scaffold**: `/home/fabricio/src/md-graph-parser/`

## Acceptance Criteria

### Core Parsing (P0)
1. Parse Markdown into structured sections (heading, paragraph, list, code, blockquote, hr)
2. Extract document title from first H1
3. Generate section IDs (UUID or sequential)
4. Track section order index
5. Create "follows" edges between sequential sections

### Checklist Extraction (P0)
6. Extract `- [ ]` and `- [x]` checklist items with completion status
7. Support nested items with indent tracking (2 spaces = 1 level)
8. Extract AC references from `(AC: 1, 2)` pattern
9. Provide `ChecklistSummary` with completion percentage

### Variable Detection (P0)
10. Detect `{{variable_name}}` template variables
11. Collect unique variable names per section and document-wide

### Frontmatter (P1 - Feature-gated)
12. Parse YAML frontmatter (`---` delimited)
13. Return as `HashMap<String, Value>`

### Quality
14. Serde serialization for all types (feature-gated)
15. Comprehensive unit tests (>90% coverage)
16. No unsafe code
17. WASM-compatible (no std dependencies that break wasm32)

## Tasks / Subtasks

### Phase 1: Repository Setup
- [ ] Create GitHub repository (AC: N/A)
  - [ ] Initialize at https://github.com/fabceolin/md-graph-parser
  - [ ] Add MIT license
  - [ ] Add .gitignore for Rust
- [ ] Set up Cargo.toml (AC: 14)
  - [ ] Define features: `default`, `serde`, `frontmatter`
  - [ ] Add dependencies: `pulldown-cmark`, `regex`, `thiserror`
  - [ ] Add optional deps: `serde`, `serde_yaml`

### Phase 2: Core Types
- [ ] Create `section.rs` (AC: 1, 3, 4)
  - [ ] `SectionType` enum with `as_str()` method
  - [ ] `ParsedSection` struct with builder pattern
  - [ ] Serde derives behind feature flag
- [ ] Create `document.rs` (AC: 2, 5)
  - [ ] `ParsedDocument` struct
  - [ ] `ParsedEdge` struct with `EdgeType` enum
- [ ] Create `checklist.rs` (AC: 6, 7, 8, 9)
  - [ ] `ChecklistItem` struct
  - [ ] `ChecklistSummary` struct
  - [ ] `extract_checklist_items()` function
  - [ ] AC reference regex extraction
- [ ] Create `variables.rs` (AC: 10, 11)
  - [ ] `extract_variables()` function
  - [ ] Regex for `{{variable_name}}`

### Phase 3: Parser Implementation
- [ ] Create `parser.rs` (AC: 1, 2, 3, 4, 5)
  - [ ] Port from agentfs `graphdocs/parser.rs`
  - [ ] `MarkdownParser` struct with `parse()` method
  - [ ] Section flushing logic
  - [ ] Edge generation
- [ ] Create `frontmatter.rs` (AC: 12, 13)
  - [ ] Feature-gated behind `frontmatter`
  - [ ] Detect and strip YAML header
  - [ ] Parse with `serde_yaml`

### Phase 4: Public API
- [ ] Create `lib.rs` (AC: 14)
  - [ ] Re-export all public types
  - [ ] Module organization
  - [ ] Crate-level documentation
- [ ] Create `error.rs`
  - [ ] `ParseError` enum with thiserror

### Phase 5: Testing & Release
- [ ] Add unit tests (AC: 15)
  - [ ] Test each section type
  - [ ] Test checklist extraction
  - [ ] Test AC reference parsing
  - [ ] Test variable detection
  - [ ] Test frontmatter parsing
  - [ ] Test malformed input handling
- [ ] Add integration tests
  - [ ] Parse real BMad story files
  - [ ] Parse agentfs GraphDocs examples
- [ ] Verify WASM compatibility (AC: 17)
  - [ ] Build with `--target wasm32-unknown-unknown`
- [ ] Tag v0.1.0 release

## Dev Notes

### Crate Structure

```
md-graph-parser/
├── Cargo.toml
├── LICENSE
├── README.md
├── src/
│   ├── lib.rs              # Public API exports
│   ├── error.rs            # ParseError
│   ├── section.rs          # ParsedSection, SectionType
│   ├── document.rs         # ParsedDocument, ParsedEdge
│   ├── parser.rs           # MarkdownParser
│   ├── checklist.rs        # ChecklistItem, extraction
│   ├── variables.rs        # Variable extraction
│   └── frontmatter.rs      # YAML frontmatter (feature-gated)
└── tests/
    ├── integration_tests.rs
    └── fixtures/
        ├── simple.md
        ├── bmad_story.md
        └── with_frontmatter.md
```

### Cargo.toml

```toml
[package]
name = "md-graph-parser"
version = "0.1.0"
edition = "2021"
description = "Structured Markdown parsing with sections, variables, and checklists"
license = "MIT"
repository = "https://github.com/fabceolin/md-graph-parser"
keywords = ["markdown", "parser", "sections", "checklist", "frontmatter"]
categories = ["parsing", "text-processing"]

[dependencies]
pulldown-cmark = { version = "0.12", default-features = false }
regex = "1"
thiserror = "1"

# Optional dependencies
serde = { version = "1", features = ["derive"], optional = true }
serde_yaml = { version = "0.9", optional = true }

[dev-dependencies]
pretty_assertions = "1"

[features]
default = []
serde = ["dep:serde"]
frontmatter = ["dep:serde_yaml", "serde"]
```

### Public API

```rust
// lib.rs
pub use parser::MarkdownParser;
pub use document::{ParsedDocument, ParsedEdge, EdgeType};
pub use section::{ParsedSection, SectionType};
pub use checklist::{ChecklistItem, ChecklistSummary, extract_checklist_items};
pub use variables::extract_variables;
pub use error::ParseError;

#[cfg(feature = "frontmatter")]
pub use frontmatter::parse_frontmatter;
```

### Usage Example

```rust
use md_graph_parser::{MarkdownParser, extract_checklist_items, ChecklistSummary};

let content = r#"
# My Document

## Tasks
- [ ] Task 1 (AC: 1)
  - [x] Subtask 1.1
- [x] Task 2 (AC: 2, 3)
"#;

// Full document parsing
let parser = MarkdownParser::new();
let doc = parser.parse(content).unwrap();

println!("Title: {:?}", doc.title);
println!("Sections: {}", doc.sections.len());
println!("Variables: {:?}", doc.variables);

// Standalone checklist extraction
let items = extract_checklist_items(content);
let summary = ChecklistSummary::from_items(&items);
println!("Completion: {:.1}%", summary.percentage);
```

### Reference Implementation

| Source | Location | Notes |
|--------|----------|-------|
| **agentfs GraphDocs** | `/home/fabricio/src/agentfs/sdk/rust/src/graphdocs/parser.rs` | Original implementation to port |
| **Local scaffold** | `/home/fabricio/src/md-graph-parser/` | Initial structure (Cargo.toml, section.rs, checklist.rs) |

## Dependencies

This story has no dependencies. It is a prerequisite for:
- TEA-RALPHY-001.1 (Python markdown.parse action)
- TEA-RALPHY-001.2 (Rust markdown.parse action)

## Testing

### Test Cases

| Test Case | Description | AC |
|-----------|-------------|-----|
| test_parse_headings | Parse H1-H6 correctly | 1 |
| test_extract_title | Title from first H1 | 2 |
| test_section_ids | UUIDs generated | 3 |
| test_order_idx | Sequential ordering | 4 |
| test_edges | Follows relationships | 5 |
| test_checklist_basic | Extract checkboxes | 6 |
| test_checklist_nested | Indent tracking | 7 |
| test_ac_refs | AC reference extraction | 8 |
| test_checklist_summary | Completion percentage | 9 |
| test_variables | `{{var}}` detection | 10, 11 |
| test_frontmatter | YAML header parsing | 12, 13 |
| test_serde | JSON serialization | 14 |
| test_malformed | Graceful error handling | 15 |
| test_wasm_build | Compiles for wasm32 | 17 |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-18 | 0.1 | Initial story creation | Sarah (PO) |

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
