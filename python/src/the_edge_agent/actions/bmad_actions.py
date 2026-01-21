"""
BMad Story Task Extraction Actions for YAMLEngine (TEA-RALPHY-001.4).

This module provides actions for parsing BMad story files and extracting
tasks, acceptance criteria, status, and completion metrics.

Actions:
    - bmad.parse_story: Parse BMad story markdown into structured data

Example:
    >>> result = registry['bmad.parse_story'](state={}, content='''
    ... # Story TEA-TEST-001.1: Test Story
    ...
    ... ## Status
    ... Draft
    ...
    ... ## Acceptance Criteria
    ... 1. First AC
    ... 2. Second AC
    ...
    ... ## Tasks / Subtasks
    ... - [ ] Task 1 (AC: 1)
    ...   - [x] Subtask 1.1
    ... - [x] Task 2 (AC: 2)
    ... ''')
    >>> print(result['story_id'])  # 'TEA-TEST-001.1'
    >>> print(result['status'])    # 'Draft'
    >>> print(result['completion']['percentage'])  # 50.0
"""

import re
from typing import Any, Callable, Dict, List, Optional

# Valid BMad story statuses
VALID_STATUSES = ["Draft", "Approved", "InProgress", "Review", "Done"]


def extract_ac_refs(text: str) -> List[int]:
    """
    Extract AC references from task text.

    Parses patterns like "(AC: 1)" or "(AC: 1, 2, 3)" or "(AC: 1-5)".

    Args:
        text: Task text potentially containing AC references

    Returns:
        List of AC numbers referenced
    """
    match = re.search(r"\(AC:\s*([\d,\s\-]+)\)", text)
    if not match:
        return []

    refs_str = match.group(1)
    refs = []

    # Handle ranges like "1-5"
    range_match = re.match(r"(\d+)\s*-\s*(\d+)", refs_str)
    if range_match:
        start = int(range_match.group(1))
        end = int(range_match.group(2))
        refs.extend(range(start, end + 1))
    else:
        # Handle comma-separated values
        for part in refs_str.split(","):
            part = part.strip()
            if part.isdigit():
                refs.append(int(part))

    return refs


def parse_tasks_section(content: str) -> List[Dict[str, Any]]:
    """
    Parse the Tasks / Subtasks section from BMad story content.

    Args:
        content: The content of the Tasks / Subtasks section

    Returns:
        List of task dictionaries with text, checked status, ac_refs, and subtasks
    """
    tasks = []
    lines = content.strip().split("\n")

    current_task = None
    task_pattern = re.compile(r"^(\s*)-\s*\[([ xX])\]\s*(.+)$")

    for line in lines:
        match = task_pattern.match(line)
        if not match:
            continue

        indent = len(match.group(1))
        checked = match.group(2).lower() == "x"
        text = match.group(3).strip()

        # Extract AC references from text
        ac_refs = extract_ac_refs(text)
        # Remove AC reference from display text
        clean_text = re.sub(r"\s*\(AC:\s*[\d,\s\-]+\)\s*", "", text).strip()

        task_item = {
            "text": clean_text,
            "checked": checked,
            "ac_refs": ac_refs,
            "indent": indent,
        }

        # Top-level task (no indentation or minimal indentation)
        if indent == 0:
            task_item["subtasks"] = []
            tasks.append(task_item)
            current_task = task_item
        elif current_task is not None:
            # This is a subtask
            current_task["subtasks"].append(task_item)

    return tasks


def parse_acceptance_criteria(content: str) -> List[Dict[str, Any]]:
    """
    Parse the Acceptance Criteria section.

    Args:
        content: The content of the Acceptance Criteria section

    Returns:
        List of AC dictionaries with number and text
    """
    criteria = []
    lines = content.strip().split("\n")

    # Match numbered list items: "1. Some text" or "1) Some text"
    pattern = re.compile(r"^(\d+)[.)]\s*(.+)$")

    for line in lines:
        line = line.strip()
        match = pattern.match(line)
        if match:
            number = int(match.group(1))
            text = match.group(2).strip()
            criteria.append({"number": number, "text": text})

    return criteria


def extract_section(content: str, section_title: str) -> Optional[str]:
    """
    Extract a section's content from markdown.

    Args:
        content: Full markdown content
        section_title: Title of the section to extract (without ##)

    Returns:
        Section content or None if not found
    """
    # Build pattern to match section header and capture content until next section
    pattern = re.compile(
        rf"^##\s+{re.escape(section_title)}\s*\n(.*?)(?=^##\s|\Z)",
        re.MULTILINE | re.DOTALL,
    )
    match = pattern.search(content)
    if match:
        return match.group(1).strip()
    return None


def parse_story_header(content: str) -> Dict[str, Optional[str]]:
    """
    Parse the story header to extract ID and title.

    Args:
        content: Full markdown content

    Returns:
        Dict with story_id and title
    """
    # Match: # Story TEA-XXX-001.1: Title
    pattern = re.compile(r"^#\s+Story\s+([\w\-\.]+):\s*(.+)$", re.MULTILINE)
    match = pattern.search(content)
    if match:
        return {"story_id": match.group(1), "title": match.group(2).strip()}
    return {"story_id": None, "title": None}


def calculate_completion(tasks: List[Dict[str, Any]]) -> Dict[str, Any]:
    """
    Calculate completion metrics from tasks.

    Args:
        tasks: List of task dictionaries with checked status and subtasks

    Returns:
        Dict with total, completed, and percentage
    """
    total = 0
    completed = 0

    for task in tasks:
        total += 1
        if task["checked"]:
            completed += 1

        for subtask in task.get("subtasks", []):
            total += 1
            if subtask["checked"]:
                completed += 1

    percentage = (completed / total * 100) if total > 0 else 0.0

    return {"total": total, "completed": completed, "percentage": round(percentage, 1)}


def validate_template(
    content: str, _template_path: str = ".bmad-core/templates/story-tmpl.yaml"
) -> List[str]:
    """
    Validate that required sections are present.

    Args:
        content: Full markdown content
        template_path: Path to BMad story template

    Returns:
        List of validation errors (empty if valid)
    """
    errors = []

    # Required sections based on BMad template
    required_sections = [
        "Status",
        "Story",
        "Acceptance Criteria",
        "Tasks / Subtasks",
    ]

    for section in required_sections:
        section_content = extract_section(content, section)
        if section_content is None or len(section_content.strip()) == 0:
            errors.append(f"Missing or empty required section: '{section}'")

    return errors


def register_actions(registry: Dict[str, Callable], _engine: Any) -> None:
    """
    Register BMad story parsing actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    def bmad_parse_story(
        state: Dict[str, Any],
        content: str,
        validate_template_flag: bool = False,
        template_path: str = ".bmad-core/templates/story-tmpl.yaml",
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Parse a BMad story file into structured data.

        Uses regex-based parsing to extract BMad-specific sections including
        tasks, acceptance criteria, status, and completion metrics.

        Args:
            state: Current state dictionary
            content: Raw markdown content of the story
            validate_template_flag: If True, validate against BMad template
            template_path: Path to story template for validation

        Returns:
            {
                "story_id": str,                 # Story identifier
                "title": str,                    # Story title
                "status": str,                   # Current status
                "acceptance_criteria": List,     # Numbered AC items
                "tasks": List,                   # Task tree with subtasks
                "completion": {                  # Completion metrics
                    "total": int,
                    "completed": int,
                    "percentage": float
                },
                "validation_errors": List[str],  # If validate_template=True
                "success": True
            }
            Or {"error": str, "success": False} on failure

        Example:
            >>> result = bmad_parse_story(
            ...     state={},
            ...     content="# Story TEA-TEST-001.1: Test\\n\\n## Status\\nDraft\\n..."
            ... )
            >>> assert result['story_id'] == 'TEA-TEST-001.1'
            >>> assert result['status'] == 'Draft'
        """
        if content is None or not content.strip():
            return {
                "success": False,
                "error": "Content is required",
                "error_type": "parse",
            }

        try:
            # Parse header
            header = parse_story_header(content)

            # Extract status
            status_content = extract_section(content, "Status")
            status = status_content.strip() if status_content else None

            # Extract and parse acceptance criteria
            ac_content = extract_section(content, "Acceptance Criteria")
            acceptance_criteria = (
                parse_acceptance_criteria(ac_content) if ac_content else []
            )

            # Extract and parse tasks
            tasks_content = extract_section(content, "Tasks / Subtasks")
            tasks = parse_tasks_section(tasks_content) if tasks_content else []

            # Calculate completion
            completion = calculate_completion(tasks)

            # Build result
            result: Dict[str, Any] = {
                "success": True,
                "story_id": header["story_id"],
                "title": header["title"],
                "status": status,
                "acceptance_criteria": acceptance_criteria,
                "tasks": tasks,
                "completion": completion,
            }

            # Validate template if requested
            if validate_template_flag:
                validation_errors = validate_template(content, template_path)
                result["validation_errors"] = validation_errors

            return result

        except Exception as e:
            return {
                "success": False,
                "error": f"BMad story parse error: {str(e)}",
                "error_type": "parse",
            }

    # Register with both naming conventions
    registry["bmad.parse_story"] = bmad_parse_story
    registry["bmad_parse_story"] = bmad_parse_story
