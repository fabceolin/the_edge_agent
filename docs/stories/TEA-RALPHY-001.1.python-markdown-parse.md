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

> **Decision**: Use the shared `md-graph-parser` crate as a git dependency.
>
> - **Repository**: https://github.com/fabceolin/md-graph-parser
> - **Rationale**: Avoid duplication with agentfs GraphDocs parser, ensure cross-runtime parity
> - **Rust**: Direct dependency on the crate
> - **Python**: PyO3 bindings or pure Python port following the same schema

### md-graph-parser Crate Plan

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
md-graph-parser = { git = "https://github.com/fabceolin/md-graph-parser", features = ["frontmatter"] }
```

**Python Integration Options:**
1. **PyO3 bindings** - Wrap Rust crate for Python (best performance)
2. **Pure Python port** - Implement same schema in Python (simpler distribution)
3. **Subprocess** - Call Rust binary (least coupling)

## Acceptance Criteria

1. Parse Markdown into structured sections (heading, paragraph, list, code, checklist)
2. Extract `- [ ]` and `- [x]` checklist items with completion status
3. Detect `{{variable}}` template variables
4. Parse YAML frontmatter if present
5. Support nested list items with indent tracking
6. Return standardized schema compatible with Rust implementation (from md-graph-parser)
7. Handle malformed Markdown gracefully with partial results

## Tasks / Subtasks

### Phase 0: Shared Crate (Prerequisite)

- [ ] Implement md-graph-parser crate (separate project)
  - [ ] Create repository at https://github.com/fabceolin/md-graph-parser
  - [ ] Port `parser.rs` from agentfs GraphDocs
  - [ ] Add `checklist.rs` with AC reference extraction
  - [ ] Add `frontmatter.rs` with YAML parsing
  - [ ] Add comprehensive tests
  - [ ] Tag v0.1.0 release

### Phase 1: Python Action

- [ ] Define Python output schema matching Rust (AC: 6)
  - [ ] `ParsedDocument` dataclass with title, sections, variables, frontmatter
  - [ ] `ParsedSection` with type, level, content, items
  - [ ] `ChecklistItem` with text, checked, indent, ac_refs
- [ ] Implement Python parser (AC: 1, 2, 3, 4, 5)
  - [ ] Option A: PyO3 bindings to md-graph-parser
  - [ ] Option B: Pure Python using `mistune` following same schema
- [ ] Register as built-in action (AC: 1)
  - [ ] Add to `actions/markdown_actions.py`
  - [ ] Export in action registry
- [ ] Add unit tests (AC: 7)
  - [ ] Test each section type
  - [ ] Test malformed input handling
  - [ ] Test BMad story format
  - [ ] Cross-validate with Rust implementation

## Dev Notes

### Shared Crate Schema (from md-graph-parser)

The Python implementation must match this Rust schema exactly:

```rust
// From md-graph-parser crate
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
| **md-graph-parser** | https://github.com/fabceolin/md-graph-parser | Shared crate (canonical) |
| **agentfs GraphDocs** | `/home/fabricio/src/agentfs/sdk/rust/src/graphdocs/parser.rs` | Original implementation |
| **Local scaffold** | `/home/fabricio/src/md-graph-parser/` | Initial structure created |

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
