# Story TEA-RALPHY-001.4: BMad Story Task Extraction

## Status
Done

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

- [x] Create `bmad.parse_story` action (AC: 1-5)
  - [x] Use `markdown.parse` as foundation
  - [x] Extract BMad-specific sections
  - [x] Map AC references
- [x] Add template conformance check (AC: 6)
  - [x] Load template from `.bmad-core/templates/story-tmpl.yaml`
  - [x] Validate required sections present

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
| 2026-01-19 | 1.0 | Implementation complete - bmad.parse_story action with 29 tests | James (Dev) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

No debug logs required - implementation completed without issues.

### Completion Notes List

- Created `bmad_actions.py` with `bmad.parse_story` action following existing action patterns
- Implemented regex-based parsing for BMad story sections:
  - Story header (ID and title extraction)
  - Status section
  - Acceptance Criteria (numbered list parsing)
  - Tasks / Subtasks (checklist with nested subtasks)
  - AC reference extraction with range support (e.g., "AC: 1-5")
- Added template validation against required BMad sections
- Completion metrics calculation (total, completed, percentage)
- Registered action with both naming conventions (`bmad.parse_story` and `bmad_parse_story`)
- Full test coverage with 29 passing tests covering all ACs

### File List

**Created:**
- `python/src/the_edge_agent/actions/bmad_actions.py` - BMad story parsing action
- `python/tests/test_bmad_actions.py` - Unit and integration tests (29 tests)

**Modified:**
- `python/src/the_edge_agent/actions/__init__.py` - Added bmad_actions import and registration

---

## QA Results

### Review Date: 2026-01-19

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: Excellent** - Clean, well-structured implementation following established patterns in the codebase.

**Strengths:**
- Clear module organization with helper functions at module level
- Comprehensive docstrings with examples
- Proper error handling with graceful degradation
- Good separation of concerns (parsing, validation, calculation)
- Follows existing action registration patterns exactly

**Code Observations:**
- Regex patterns are well-documented and handle edge cases
- AC range support (e.g., "1-5") is a nice enhancement beyond the spec
- Template validation is extensible for future additions

### Refactoring Performed

None required - code quality meets standards.

### Compliance Check

- Coding Standards: ✓ Follows Python docstring conventions, type hints, naming
- Project Structure: ✓ File placement matches documented source tree
- Testing Strategy: ✓ Unit tests with clear AC traceability
- All ACs Met: ✓ All 6 acceptance criteria verified

### Requirements Traceability

| AC | Requirement | Test Coverage | Status |
|----|-------------|---------------|--------|
| 1 | Parse BMad story markdown format | TestBmadParseStory::test_parse_complete_story | ✓ |
| 2 | Extract Tasks / Subtasks section | TestParseTasksSection (4 tests) | ✓ |
| 3 | Extract Acceptance Criteria section | TestParseAcceptanceCriteria (3 tests) | ✓ |
| 4 | Link tasks to AC references | TestExtractAcRefs (5 tests), test_task_ac_refs | ✓ |
| 5 | Detect story status | TestExtractSection::test_extract_status | ✓ |
| 6 | Support BMad template validation | TestValidateTemplate (3 tests) | ✓ |

### Improvements Checklist

- [x] All ACs have corresponding test coverage
- [x] Error handling for edge cases (empty/None content)
- [x] Both naming conventions registered (`bmad.parse_story`, `bmad_parse_story`)
- [x] Module docstring with usage examples
- [ ] Consider: Future enhancement to parse Dev Notes section for richer context
- [ ] Consider: Add support for markdown bullet variations (*, +) in task parsing

### Security Review

No security concerns - this is a parsing-only module with no external I/O, no code execution, and no sensitive data handling.

### Performance Considerations

No concerns - regex-based parsing is efficient for typical story file sizes (<100KB). No optimization needed.

### Test Architecture Assessment

**Test Coverage: Comprehensive (29 tests)**

| Test Class | Tests | Focus |
|------------|-------|-------|
| TestExtractAcRefs | 5 | AC reference extraction edge cases |
| TestParseStoryHeader | 3 | Story ID/title parsing |
| TestParseAcceptanceCriteria | 3 | Numbered list parsing |
| TestParseTasksSection | 4 | Task/subtask hierarchy |
| TestCalculateCompletion | 3 | Completion metrics |
| TestExtractSection | 3 | Markdown section extraction |
| TestValidateTemplate | 3 | Required sections validation |
| TestBmadParseStory | 4 | Integration tests |
| TestRegisterActions | 1 | Registration verification |

**Test Quality:**
- Good use of unittest assertions
- Clear test method naming
- Integration test verifies full workflow

### Files Modified During Review

None - no modifications required.

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-RALPHY-001.4-bmad-story-task-extraction.yml

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, comprehensive test coverage, code quality excellent.
