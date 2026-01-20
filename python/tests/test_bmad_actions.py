"""
Tests for BMad Story Task Extraction Actions (TEA-RALPHY-001.4).

Test Cases:
- test_parse_story_format: Parse standard BMad format (AC: 1)
- test_extract_tasks: Extract Tasks / Subtasks section (AC: 2)
- test_extract_acceptance_criteria: Extract AC section (AC: 3)
- test_link_ac_refs: Parse (AC: 1) references (AC: 4)
- test_detect_status: Read Status section (AC: 5)
- test_template_validation: Validate against template (AC: 6)
- test_completion_calculation: Calculate % complete (AC: 1-5)
"""

import unittest

from the_edge_agent.actions.bmad_actions import (
    extract_ac_refs,
    parse_acceptance_criteria,
    parse_story_header,
    parse_tasks_section,
    calculate_completion,
    validate_template,
    extract_section,
    register_actions,
)


class TestExtractAcRefs(unittest.TestCase):
    """Test AC reference extraction from task text."""

    def test_single_ac_ref(self):
        """Extract single AC reference."""
        result = extract_ac_refs("Task 1 (AC: 1)")
        self.assertEqual(result, [1])

    def test_multiple_ac_refs(self):
        """Extract multiple comma-separated AC references."""
        result = extract_ac_refs("Task 1 (AC: 1, 2, 3)")
        self.assertEqual(result, [1, 2, 3])

    def test_ac_range(self):
        """Extract AC range reference."""
        result = extract_ac_refs("Task 1 (AC: 1-5)")
        self.assertEqual(result, [1, 2, 3, 4, 5])

    def test_no_ac_ref(self):
        """Return empty list when no AC reference."""
        result = extract_ac_refs("Task without AC reference")
        self.assertEqual(result, [])

    def test_ac_with_spaces(self):
        """Handle AC references with extra spaces."""
        result = extract_ac_refs("Task (AC:  1 ,  2 )")
        self.assertEqual(result, [1, 2])


class TestParseStoryHeader(unittest.TestCase):
    """Test story header parsing."""

    def test_standard_header(self):
        """Parse standard story header format."""
        content = "# Story TEA-TEST-001.1: Test Story Title\n\nContent..."
        result = parse_story_header(content)
        self.assertEqual(result["story_id"], "TEA-TEST-001.1")
        self.assertEqual(result["title"], "Test Story Title")

    def test_header_with_special_chars(self):
        """Parse header with special characters in title."""
        content = "# Story TEA-RALPHY-001.4: BMad Story Task Extraction\n"
        result = parse_story_header(content)
        self.assertEqual(result["story_id"], "TEA-RALPHY-001.4")
        self.assertEqual(result["title"], "BMad Story Task Extraction")

    def test_no_header(self):
        """Return None values when no header found."""
        content = "Some content without a header"
        result = parse_story_header(content)
        self.assertIsNone(result["story_id"])
        self.assertIsNone(result["title"])


class TestParseAcceptanceCriteria(unittest.TestCase):
    """Test acceptance criteria parsing."""

    def test_numbered_list(self):
        """Parse numbered acceptance criteria."""
        content = """1. First criterion
2. Second criterion
3. Third criterion"""
        result = parse_acceptance_criteria(content)
        self.assertEqual(len(result), 3)
        self.assertEqual(result[0], {"number": 1, "text": "First criterion"})
        self.assertEqual(result[1], {"number": 2, "text": "Second criterion"})
        self.assertEqual(result[2], {"number": 3, "text": "Third criterion"})

    def test_numbered_with_parenthesis(self):
        """Parse criteria with parenthesis notation."""
        content = """1) First AC
2) Second AC"""
        result = parse_acceptance_criteria(content)
        self.assertEqual(len(result), 2)
        self.assertEqual(result[0]["text"], "First AC")

    def test_empty_content(self):
        """Return empty list for empty content."""
        result = parse_acceptance_criteria("")
        self.assertEqual(result, [])


class TestParseTasksSection(unittest.TestCase):
    """Test tasks section parsing."""

    def test_simple_tasks(self):
        """Parse simple task list."""
        content = """- [ ] Task 1
- [x] Task 2
- [ ] Task 3"""
        result = parse_tasks_section(content)
        self.assertEqual(len(result), 3)
        self.assertFalse(result[0]["checked"])
        self.assertTrue(result[1]["checked"])
        self.assertFalse(result[2]["checked"])

    def test_tasks_with_subtasks(self):
        """Parse tasks with nested subtasks."""
        content = """- [ ] Task 1 (AC: 1)
  - [x] Subtask 1.1
  - [ ] Subtask 1.2
- [x] Task 2 (AC: 2)"""
        result = parse_tasks_section(content)
        self.assertEqual(len(result), 2)
        self.assertEqual(len(result[0]["subtasks"]), 2)
        self.assertTrue(result[0]["subtasks"][0]["checked"])
        self.assertFalse(result[0]["subtasks"][1]["checked"])

    def test_task_ac_refs(self):
        """Extract AC references from tasks."""
        content = """- [ ] Task 1 (AC: 1, 2)
- [ ] Task 2 (AC: 3)"""
        result = parse_tasks_section(content)
        self.assertEqual(result[0]["ac_refs"], [1, 2])
        self.assertEqual(result[1]["ac_refs"], [3])

    def test_task_text_without_ac_ref(self):
        """Task text should not include AC reference."""
        content = "- [ ] Implement feature (AC: 1)"
        result = parse_tasks_section(content)
        self.assertEqual(result[0]["text"], "Implement feature")


class TestCalculateCompletion(unittest.TestCase):
    """Test completion calculation."""

    def test_all_complete(self):
        """Calculate 100% completion."""
        tasks = [
            {"checked": True, "subtasks": [{"checked": True}]},
            {"checked": True, "subtasks": []},
        ]
        result = calculate_completion(tasks)
        self.assertEqual(result["total"], 3)
        self.assertEqual(result["completed"], 3)
        self.assertEqual(result["percentage"], 100.0)

    def test_partial_complete(self):
        """Calculate partial completion."""
        tasks = [
            {"checked": False, "subtasks": [{"checked": True}]},
            {"checked": True, "subtasks": []},
        ]
        result = calculate_completion(tasks)
        self.assertEqual(result["total"], 3)
        self.assertEqual(result["completed"], 2)
        self.assertAlmostEqual(result["percentage"], 66.7, places=1)

    def test_empty_tasks(self):
        """Handle empty task list."""
        result = calculate_completion([])
        self.assertEqual(result["total"], 0)
        self.assertEqual(result["completed"], 0)
        self.assertEqual(result["percentage"], 0.0)


class TestExtractSection(unittest.TestCase):
    """Test section extraction from markdown."""

    def test_extract_status(self):
        """Extract Status section."""
        content = """# Story

## Status
Draft

## Story
As a user..."""
        result = extract_section(content, "Status")
        self.assertEqual(result, "Draft")

    def test_extract_tasks(self):
        """Extract Tasks / Subtasks section."""
        content = """## Tasks / Subtasks
- [ ] Task 1
- [x] Task 2

## Dev Notes"""
        result = extract_section(content, "Tasks / Subtasks")
        self.assertIn("Task 1", result)
        self.assertIn("Task 2", result)

    def test_missing_section(self):
        """Return None for missing section."""
        content = "## Status\nDraft"
        result = extract_section(content, "NonExistent Section")
        self.assertIsNone(result)


class TestValidateTemplate(unittest.TestCase):
    """Test template validation."""

    def test_valid_story(self):
        """Validate complete story structure."""
        content = """# Story TEA-TEST-001.1: Test

## Status
Draft

## Story
**As a** user...

## Acceptance Criteria
1. First AC

## Tasks / Subtasks
- [ ] Task 1"""
        errors = validate_template(content)
        self.assertEqual(errors, [])

    def test_missing_status(self):
        """Detect missing Status section."""
        content = """# Story TEA-TEST-001.1: Test

## Story
As a user...

## Acceptance Criteria
1. First AC

## Tasks / Subtasks
- [ ] Task 1"""
        errors = validate_template(content)
        self.assertTrue(any("Status" in e for e in errors))

    def test_missing_tasks(self):
        """Detect missing Tasks section."""
        content = """# Story TEA-TEST-001.1: Test

## Status
Draft

## Story
As a user...

## Acceptance Criteria
1. First AC"""
        errors = validate_template(content)
        self.assertTrue(any("Tasks" in e for e in errors))


class TestBmadParseStory(unittest.TestCase):
    """Integration tests for bmad_parse_story action."""

    def test_parse_complete_story(self):
        """Parse a complete BMad story file."""
        content = """# Story TEA-TEST-001.1: Test Story

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
        # Call the action function through registry
        registry = {}
        register_actions(registry, None)

        result = registry["bmad.parse_story"](state={}, content=content)

        self.assertTrue(result["success"])
        self.assertEqual(result["story_id"], "TEA-TEST-001.1")
        self.assertEqual(result["title"], "Test Story")
        self.assertEqual(result["status"], "Draft")
        self.assertEqual(len(result["acceptance_criteria"]), 2)
        self.assertEqual(len(result["tasks"]), 2)
        self.assertEqual(result["tasks"][0]["ac_refs"], [1])
        # 3 items total (Task 1, Subtask 1.1, Task 2), 2 completed (Subtask 1.1, Task 2) = 66.7%
        self.assertAlmostEqual(result["completion"]["percentage"], 66.7, places=1)

    def test_parse_with_validation(self):
        """Parse with template validation enabled."""
        content = """# Story TEA-TEST-001.1: Test Story

## Status
Draft
"""
        registry = {}
        register_actions(registry, None)

        result = registry["bmad.parse_story"](
            state={}, content=content, validate_template_flag=True
        )

        self.assertTrue(result["success"])
        self.assertIn("validation_errors", result)
        self.assertTrue(len(result["validation_errors"]) > 0)

    def test_parse_empty_content(self):
        """Handle empty content gracefully."""
        registry = {}
        register_actions(registry, None)

        result = registry["bmad.parse_story"](state={}, content="")

        self.assertFalse(result["success"])
        self.assertIn("error", result)

    def test_parse_none_content(self):
        """Handle None content gracefully."""
        registry = {}
        register_actions(registry, None)

        result = registry["bmad.parse_story"](state={}, content=None)

        self.assertFalse(result["success"])
        self.assertIn("error", result)


class TestRegisterActions(unittest.TestCase):
    """Test action registration."""

    def test_register_both_naming_conventions(self):
        """Actions registered with both naming conventions."""
        registry = {}
        register_actions(registry, None)

        self.assertIn("bmad.parse_story", registry)
        self.assertIn("bmad_parse_story", registry)


if __name__ == "__main__":
    unittest.main()
