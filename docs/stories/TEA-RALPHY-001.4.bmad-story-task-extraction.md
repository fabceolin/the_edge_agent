# Story TEA-RALPHY-001.4: BMad Story Task Extraction

## Status
Draft

## Epic Reference
[TEA-RALPHY-001: Autonomous AI Coding Loop](./TEA-RALPHY-001-autonomous-coding-loop.md)

## Dependencies

- TEA-RALPHY-001.1 (Python `markdown.parse` Action)
- TEA-RALPHY-001.2 (Rust `markdown.parse` Action)
  - Both transitively depend on TEA-RALPHY-001.0 (md-graph-parser shared crate)

## Story

**As a** BMad workflow user,
**I want** to extract tasks from BMad story files,
**So that** I can use existing stories as task sources for autonomous execution.

## Acceptance Criteria

1. Parse BMad story markdown format (see `docs/stories/*.md`)
2. Extract `## Tasks / Subtasks` section
3. Extract `## Acceptance Criteria` section
4. Link tasks to AC references (e.g., "(AC: 1)")
5. Detect story status from `## Status` section
6. Support BMad template validation

## Tasks / Subtasks

- [ ] Create `bmad.parse_story` action (AC: 1-5)
  - [ ] Use `markdown.parse` as foundation
  - [ ] Extract BMad-specific sections
  - [ ] Map AC references
- [ ] Add template conformance check (AC: 6)
  - [ ] Load template from `.bmad-core/templates/story-tmpl.yaml`
  - [ ] Validate required sections present

## Dev Notes

### BMad Story Structure

```markdown
# Story TEA-XXX-001.1: Title

## Status
Draft | Approved | InProgress | Review | Done

## Acceptance Criteria
1. First criterion
2. Second criterion

## Tasks / Subtasks
- [ ] Task 1 (AC: 1)
  - [ ] Subtask 1.1
  - [x] Subtask 1.2 <- Completed
- [x] Task 2 (AC: 2)
```

### Output Schema

```python
{
    "story_id": "TEA-XXX-001.1",
    "title": "Title",
    "status": "Draft",
    "acceptance_criteria": [
        {"number": 1, "text": "First criterion"},
        {"number": 2, "text": "Second criterion"}
    ],
    "tasks": [
        {
            "text": "Task 1",
            "checked": False,
            "ac_refs": [1],
            "subtasks": [
                {"text": "Subtask 1.1", "checked": False},
                {"text": "Subtask 1.2", "checked": True}
            ]
        }
    ],
    "completion": {
        "total": 4,
        "completed": 2,
        "percentage": 50.0
    }
}
```

### Action Signature

```python
def bmad_parse_story(
    content: str,
    validate_template: bool = False,
    template_path: str = ".bmad-core/templates/story-tmpl.yaml",
    **kwargs
) -> Dict[str, Any]:
    """
    Parse a BMad story file.

    Args:
        content: Raw markdown content of the story
        validate_template: If True, validate against BMad template
        template_path: Path to story template for validation

    Returns:
        Parsed story with tasks, ACs, status, and completion metrics
    """
```

### Source Tree

```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py          # MODIFY: Add bmad_actions import
│   ├── markdown_actions.py  # Dependency from 001.1
│   └── bmad_actions.py      # NEW: bmad.* actions
└── ...
```

### YAML Usage

```yaml
nodes:
  - name: parse_story
    uses: bmad.parse_story
    with:
      content: "{{ state.story_content }}"
      validate_template: true
    output: parsed_story

  - name: extract_incomplete_tasks
    run: |
      tasks = state["parsed_story"]["tasks"]
      incomplete = [t for t in tasks if not t["checked"]]
      return {"incomplete_tasks": incomplete}
```

### AC Reference Extraction

```python
import re

def extract_ac_refs(text: str) -> List[int]:
    """Extract AC references like (AC: 1) or (AC: 1, 2, 3)."""
    match = re.search(r'\(AC:\s*([\d,\s]+)\)', text)
    if match:
        refs = match.group(1).split(',')
        return [int(r.strip()) for r in refs]
    return []
```

## Testing

**Test Location:** `python/tests/test_bmad_actions.py`

```python
def test_parse_bmad_story():
    content = """
# Story TEA-TEST-001.1: Test Story

## Status
Draft

## Acceptance Criteria
1. First AC
2. Second AC

## Tasks / Subtasks
- [ ] Task 1 (AC: 1)
  - [x] Subtask 1.1
- [x] Task 2 (AC: 2)
"""
    result = bmad_parse_story(content)

    assert result["story_id"] == "TEA-TEST-001.1"
    assert result["title"] == "Test Story"
    assert result["status"] == "Draft"
    assert len(result["acceptance_criteria"]) == 2
    assert len(result["tasks"]) == 2
    assert result["tasks"][0]["ac_refs"] == [1]
    assert result["completion"]["percentage"] == 50.0

def test_template_validation():
    # Missing required section
    content = """
# Story TEA-TEST-001.1: Test Story

## Status
Draft
"""
    result = bmad_parse_story(content, validate_template=True)
    assert "validation_errors" in result
    assert "Tasks / Subtasks" in str(result["validation_errors"])
```

### Test Cases

| Test Case | Description | AC |
|-----------|-------------|-----|
| test_parse_story_format | Parse standard BMad format | 1 |
| test_extract_tasks | Extract Tasks / Subtasks section | 2 |
| test_extract_acceptance_criteria | Extract AC section | 3 |
| test_link_ac_refs | Parse (AC: 1) references | 4 |
| test_detect_status | Read Status section | 5 |
| test_template_validation | Validate against template | 6 |
| test_completion_calculation | Calculate % complete | 1-5 |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-17 | 0.1 | Extracted from epic TEA-RALPHY-001 | Sarah (PO) |
| 2025-01-18 | 0.2 | Added transitive dependency note for 001.0 (md-graph-parser) | Sarah (PO) |

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
