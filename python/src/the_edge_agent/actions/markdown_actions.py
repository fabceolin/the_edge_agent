"""
Markdown Parsing Actions for YAMLEngine (TEA-RALPHY-001.1).

This module provides Markdown parsing actions using the md-parser
crate with PyO3 bindings. It extracts structured sections, variables,
checklists, and frontmatter from Markdown documents.

Actions:
    - markdown.parse: Parse Markdown content into structured document

Installation:
    The md-parser PyO3 bindings must be installed from GitHub Release:

    # Linux x86_64 (Python 3.11)
    pip install https://github.com/fabceolin/md-parser/releases/download/v0.1.0/md_parser-0.1.0-cp311-cp311-manylinux_2_17_x86_64.manylinux2014_x86_64.whl

    # See https://github.com/fabceolin/md-parser/releases for other platforms

Example:
    >>> result = registry['markdown.parse'](state={}, content='''
    ... # My Document
    ...
    ... ## Tasks
    ... - [ ] Task 1 (AC: 1)
    ...   - [x] Subtask 1.1
    ... - [x] Task 2
    ...
    ... Some text with {{variable}} template.
    ... ''')
    >>> print(result['title'])  # 'My Document'
    >>> print(len(result['tasks']))  # 3
    >>> print(result['variables'])  # ['variable']
"""

from typing import Any, Callable, Dict, List, Optional


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register Markdown parsing actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    def markdown_parse(
        state: Dict[str, Any],
        content: str,
        extract: Optional[List[str]] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Parse Markdown content into a structured document.

        Uses the md-parser crate with PyO3 bindings for fast, accurate parsing
        with guaranteed schema parity with the Rust implementation.

        Args:
            state: Current state dictionary
            content: Raw Markdown string to parse
            extract: Optional list of components to extract. If None, extracts all.
                     Supported: ["tasks", "sections", "variables", "frontmatter"]

        Returns:
            {
                "title": Optional[str],        # Document title from first H1
                "sections": List[dict],        # Structured sections
                "variables": List[str],        # Template variables found
                "frontmatter": Optional[dict], # YAML frontmatter if present
                "tasks": List[dict],           # Checklist items
                "success": True
            }
            Or {"error": str, "success": False, "error_type": "parse"|"import"} on failure

        Section dict schema:
            {
                "id": str,                     # Unique section ID
                "section_type": str,           # heading, paragraph, list, code, etc.
                "content": str,                # Section content
                "order_idx": int,              # Order in document
                "variables": List[str],        # Variables in this section
                "level": Optional[int],        # Heading level (1-6)
                "language": Optional[str]      # Code block language
            }

        Task/ChecklistItem dict schema:
            {
                "text": str,                   # Task text
                "checked": bool,               # Completion status
                "indent": int,                 # Nesting level (0 = top level)
                "ac_refs": List[int],          # AC references from "(AC: 1, 2)"
                "line_number": Optional[int]   # Line number in source
            }

        Example:
            >>> result = markdown_parse(
            ...     state={},
            ...     content="# Title\\n\\n- [ ] Task 1 (AC: 1)\\n- [x] Task 2"
            ... )
            >>> assert result['title'] == 'Title'
            >>> assert len(result['tasks']) == 2
            >>> assert result['tasks'][0]['checked'] == False
            >>> assert result['tasks'][0]['ac_refs'] == [1]
        """
        if content is None:
            return {
                "success": False,
                "error": "Content is required",
                "error_type": "parse",
            }

        # Lazy import to provide helpful error if not installed
        try:
            from md_parser import MarkdownParser
        except ImportError:
            return {
                "success": False,
                "error": (
                    "md-parser PyO3 bindings not installed. "
                    "Install from GitHub Release: "
                    "pip install https://github.com/fabceolin/md-parser/releases/download/v0.1.0/"
                    "md_parser-0.1.0-cp311-cp311-manylinux_2_17_x86_64.manylinux2014_x86_64.whl "
                    "(adjust for your Python version and platform)"
                ),
                "error_type": "import",
            }

        try:
            # Parse the Markdown content
            parser = MarkdownParser()
            doc = parser.parse(content)

            # Convert PyO3 objects to Python dicts for state compatibility
            # The PyO3 bindings expose attributes directly

            # Build sections list
            sections = []
            if extract is None or "sections" in extract:
                for section in doc.sections:
                    sections.append(
                        {
                            "id": section.id,
                            "section_type": section.section_type,
                            "content": section.content,
                            "order_idx": section.order_idx,
                            "variables": list(section.variables),
                            "level": section.level,
                            # language may not be present in all versions
                            "language": getattr(section, "language", None),
                        }
                    )

            # Build tasks list from checklist_items
            tasks = []
            if extract is None or "tasks" in extract:
                # Use checklist_items attribute (PyO3 binding name)
                checklist_items = getattr(doc, "checklist_items", [])
                for task in checklist_items:
                    # Convert ac_refs from strings to integers
                    ac_refs_raw = list(task.ac_refs) if hasattr(task, "ac_refs") else []
                    ac_refs = []
                    for ref in ac_refs_raw:
                        try:
                            ac_refs.append(int(ref))
                        except (ValueError, TypeError):
                            pass  # Skip non-integer refs

                    tasks.append(
                        {
                            "text": task.text,
                            "checked": task.checked,
                            "indent": task.indent,
                            "ac_refs": ac_refs,
                            # line_number may not be present in all versions
                            "line_number": getattr(task, "line_number", None),
                        }
                    )

            # Build result dict with all fields
            result: Dict[str, Any] = {
                "success": True,
            }

            if extract is None or "title" in extract:
                result["title"] = doc.title

            if extract is None or "sections" in extract:
                result["sections"] = sections

            if extract is None or "variables" in extract:
                result["variables"] = list(doc.variables)

            if extract is None or "frontmatter" in extract:
                # frontmatter may not be present if feature not compiled
                result["frontmatter"] = getattr(doc, "frontmatter", None)

            if extract is None or "tasks" in extract:
                result["tasks"] = tasks

            return result

        except Exception as e:
            return {
                "success": False,
                "error": f"Markdown parse error: {str(e)}",
                "error_type": "parse",
            }

    registry["markdown.parse"] = markdown_parse
    registry["markdown_parse"] = markdown_parse
