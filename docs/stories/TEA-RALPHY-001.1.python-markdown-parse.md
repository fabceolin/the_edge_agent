# Story TEA-RALPHY-001.1: Python `markdown.parse` Action

## Status
Draft

## Epic Reference
[TEA-RALPHY-001: Autonomous AI Coding Loop](./TEA-RALPHY-001-autonomous-coding-loop.md)

## Story

**As a** workflow developer,
**I want** a `markdown.parse` action in the Python runtime,
**So that** I can extract tasks, sections, and variables from Markdown documents in YAML agents.

## Shared Library Strategy

> **Decision**: Use the shared `md-parser` crate as a git dependency with PyO3 bindings.
>
> - **Repository**: https://github.com/fabceolin/md-parser
> - **Rationale**: Avoid duplication with agentfs GraphDocs parser, ensure cross-runtime parity
> - **Rust**: Direct dependency on the crate
> - **Python**: PyO3 bindings (feature-gated in md-parser crate)

### md-parser Crate Plan

The shared crate will be implemented in a separate project before this story:

| Component | Description | Priority |
|-----------|-------------|----------|
| `section.rs` | `ParsedSection`, `SectionType` enums | P0 |
| `document.rs` | `ParsedDocument`, `ParsedEdge` structs | P0 |
| `parser.rs` | Core `MarkdownParser` using `pulldown-cmark` | P0 |
| `checklist.rs` | `ChecklistItem`, AC reference extraction | P0 |
| `frontmatter.rs` | YAML frontmatter parsing (feature-gated) | P1 |
| `variables.rs` | `{{variable}}` extraction | P0 |

**Crate Features:**
- `default`: Core parsing only
- `serde`: Serialization support
- `frontmatter`: YAML frontmatter parsing

**Git Dependency (Rust):**
```toml
[dependencies]
md-parser = { git = "https://github.com/fabceolin/md-parser", features = ["frontmatter"] }
```

**Python Integration:**
- **PyO3 bindings** via `pyo3` feature in md-parser crate
- Pre-built wheels available via GitHub Release (no PyPI)
- Install: `pip install https://github.com/fabceolin/md-parser/releases/download/v0.1.0/md_parser-0.1.0-cp311-cp311-manylinux_2_17_x86_64.whl`

## Acceptance Criteria

1. Parse Markdown into structured sections (heading, paragraph, list, code, checklist)
2. Extract `- [ ]` and `- [x]` checklist items with completion status
3. Detect `{{variable}}` template variables
4. Parse YAML frontmatter if present
5. Support nested list items with indent tracking
6. Return standardized schema compatible with Rust implementation (from md-parser)
7. Handle malformed Markdown gracefully with partial results

## Tasks / Subtasks

### Phase 0: Shared Crate (Prerequisite)

- [ ] Implement md-parser crate with PyO3 bindings (TEA-RALPHY-001.0)
  - [ ] Create repository at https://github.com/fabceolin/md-parser
  - [ ] Port `parser.rs` from agentfs GraphDocs
  - [ ] Add `checklist.rs` with AC reference extraction
  - [ ] Add `frontmatter.rs` with YAML parsing
  - [ ] Add `pyo3` feature with Python bindings
  - [ ] Add comprehensive tests
  - [ ] Tag v0.1.0 release (GitHub Release with wheels)

### Phase 1: Python Action

- [ ] Install md-parser PyO3 bindings from GitHub Release
  - [ ] Add `md-parser` wheel URL to `setup.py` dependencies
  - [ ] Verify import works: `from md_parser import MarkdownParser`
- [ ] Create thin wrapper action (AC: 1, 2, 3, 4, 5, 6)
  - [ ] Wrap PyO3 classes in TEA action interface
  - [ ] Convert Rust types to Python dicts for state compatibility
- [ ] Register as built-in action (AC: 1)
  - [ ] Add to `actions/markdown_actions.py`
  - [ ] Export in action registry
- [ ] Add unit tests (AC: 7)
  - [ ] Test each section type
  - [ ] Test malformed input handling
  - [ ] Test BMad story format
  - [ ] Cross-validate with Rust implementation

## Dev Notes

### Shared Crate Schema (from md-parser)

The Python implementation must match this Rust schema exactly:

```rust
// From md-parser crate
pub enum SectionType {
    Heading, Paragraph, List, Code, Table,
    Blockquote, HorizontalRule, Checklist, Choice,
}

pub struct ParsedSection {
    pub id: String,
    pub section_type: SectionType,
    pub level: Option<u8>,
    pub content: String,
    pub order_idx: u32,
    pub variables: Vec<String>,
    pub language: Option<String>,
}

pub struct ChecklistItem {
    pub text: String,
    pub checked: bool,
    pub indent: u32,
    pub ac_refs: Vec<u32>,
    pub line_number: Option<u32>,
}

pub struct ParsedDocument {
    pub title: Option<String>,
    pub sections: Vec<ParsedSection>,
    pub variables: Vec<String>,
    pub frontmatter: Option<HashMap<String, Value>>,
    pub tasks: Vec<ChecklistItem>,
    pub edges: Vec<ParsedEdge>,
}
```

### Python Equivalent Schema

```python
from dataclasses import dataclass
from typing import List, Optional, Dict, Any
from enum import Enum

class SectionType(str, Enum):
    HEADING = "heading"
    PARAGRAPH = "paragraph"
    LIST = "list"
    CODE = "code"
    TABLE = "table"
    BLOCKQUOTE = "blockquote"
    HORIZONTAL_RULE = "hr"
    CHECKLIST = "checklist"
    CHOICE = "choice"

@dataclass
class ChecklistItem:
    text: str
    checked: bool
    indent: int
    ac_refs: List[int]  # Extracted from "(AC: 1, 2)"
    line_number: Optional[int] = None

@dataclass
class ParsedSection:
    id: str
    section_type: SectionType
    content: str
    order_idx: int
    variables: List[str]
    level: Optional[int] = None
    language: Optional[str] = None

@dataclass
class ParsedDocument:
    title: Optional[str]
    sections: List[ParsedSection]
    variables: List[str]
    frontmatter: Optional[Dict[str, Any]]
    tasks: List[ChecklistItem]
```

### Action Signature

```python
def markdown_parse(
    content: str,
    extract: List[str] = None,  # ["tasks", "sections", "variables", "frontmatter"]
    **kwargs
) -> Dict[str, Any]:
    """
    Parse Markdown content into structured document.

    Args:
        content: Raw Markdown string
        extract: Optional list of components to extract (default: all)

    Returns:
        {
            "title": "Document Title",
            "sections": [...],
            "variables": ["var1", "var2"],
            "frontmatter": {...},
            "tasks": [...]
        }
    """
```

### Source Tree

```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py          # MODIFY: Add markdown_actions import
│   └── markdown_actions.py  # NEW: markdown.parse action
└── ...
```

### Reference Implementations

| Source | Location | Notes |
|--------|----------|-------|
| **md-parser** | https://github.com/fabceolin/md-parser | Shared crate with PyO3 bindings (canonical) |
| **agentfs GraphDocs** | `/home/fabricio/src/agentfs/sdk/rust/src/graphdocs/parser.rs` | Original implementation |
| **Local scaffold** | `/home/fabricio/src/md-parser/` | Initial structure created |

## Testing

**Test Location:** `python/tests/test_markdown_actions.py`

```python
def test_parse_checklist():
    result = markdown_parse("""
## Tasks
- [ ] Task 1 (AC: 1)
  - [x] Subtask 1.1
- [x] Task 2
""")
    assert len(result["tasks"]) == 3
    assert result["tasks"][0]["checked"] == False
    assert result["tasks"][1]["checked"] == True
    assert result["tasks"][1]["indent"] == 1
```

### Test Cases

| Test Case | Description | AC |
|-----------|-------------|-----|
| test_parse_headings | Parse H1-H6 headings correctly | 1 |
| test_parse_paragraphs | Extract paragraph content | 1 |
| test_parse_code_blocks | Extract code with language | 1 |
| test_parse_checklist | Extract checkboxes with state | 2 |
| test_parse_nested_checklist | Handle indented items | 5 |
| test_extract_variables | Find `{{var}}` patterns | 3 |
| test_parse_frontmatter | Extract YAML header | 4 |
| test_malformed_markdown | Return partial results | 7 |
| test_bmad_story_format | Parse full BMad story | 1-7 |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-17 | 0.1 | Extracted from epic TEA-RALPHY-001 | Sarah (PO) |
| 2025-01-19 | 0.2 | Updated to use md-parser with PyO3 bindings (was md-graph-parser) | Sarah (PO) |
| 2025-01-19 | 0.3 | Updated to install from GitHub Release (no PyPI) | Sarah (PO) |

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
