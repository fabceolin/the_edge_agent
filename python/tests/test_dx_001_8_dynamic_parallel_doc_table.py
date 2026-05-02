"""
Tests for TEA-DX-001.8: dynamic_parallel mode comparison doc table.

Adds a comparison table in YAML_REFERENCE.md showing when to use
`action:` vs `steps:` vs `subgraph:` in dynamic_parallel nodes.

Coverage map (per docs/qa/assessments/TEA-DX-001.8-test-design-20260501.md):
- UNIT-001 / UNIT-002: section heading present (AC-1)
- UNIT-003: comparison table headers contain the required columns (AC-2)
- UNIT-004: exactly one row per mode (AC-2)
- UNIT-005: each mode has a 5–10 line minimal example (AC-3)
- UNIT-006: mutual-exclusion sentence present (AC-4)
- UNIT-007: fan_in: requirement sentence present (AC-5)
- UNIT-008: anchor pinned via {#dynamic-parallel-branch-body-modes} OR
            preceded by a load-bearing-comment naming the slug (AC-7, AC-13)
- INT-006:  cross-link from actions-reference.md to the new section (AC-6)
"""

import re
import unittest
from pathlib import Path


REPO = Path(__file__).resolve().parents[2]
YAML_REF = REPO / "docs" / "shared" / "YAML_REFERENCE.md"
ACTIONS_REF = REPO / "docs" / "python" / "actions-reference.md"

EXAMPLES_DIR = REPO / "examples" / "yaml"


# --------------------------------------------------------------------------- #
# Helpers
# --------------------------------------------------------------------------- #


def _section_text(text: str, heading_pattern: str) -> str:
    """Return the text of the first section whose heading matches the pattern.

    The section runs from the matched heading up to the next heading of equal
    or shallower depth (or end-of-file).
    """
    m = re.search(heading_pattern, text, flags=re.MULTILINE)
    if not m:
        return ""
    start = m.start()
    # Determine the heading depth (number of leading '#').
    line = text[start : text.index("\n", start)]
    depth = len(line) - len(line.lstrip("#"))
    # Find next heading of same-or-shallower depth.
    after = text[m.end() :]
    next_heading = re.search(
        rf"(?m)^#{{1,{depth}}}\s+\S", after
    )
    end = m.end() + (next_heading.start() if next_heading else len(after))
    return text[start:end]


# --------------------------------------------------------------------------- #
# Tests
# --------------------------------------------------------------------------- #


class TestDynamicParallelDocTable(unittest.TestCase):
    """TEA-DX-001.8 — comparison table presence in YAML_REFERENCE.md."""

    @classmethod
    def setUpClass(cls):
        cls.text = YAML_REF.read_text(encoding="utf-8")
        cls.actions_text = ACTIONS_REF.read_text(encoding="utf-8")
        cls.section = _section_text(
            cls.text, r"^#{2,4}\s+Dynamic Parallel(:?\s+Branch Body Modes)?"
        )
        # Narrow down to the Branch Body Modes subsection specifically.
        cls.modes_section = _section_text(
            cls.text, r"^#{2,4}\s+Dynamic Parallel:\s+Branch Body Modes"
        )

    # ----------------- AC-1 (section exists) ------------------------------ #

    def test_doc_file_exists(self):
        self.assertTrue(YAML_REF.exists(), f"Missing: {YAML_REF}")

    def test_dynamic_parallel_section_present(self):
        """AC-1 / UNIT-001: a heading naming the section exists."""
        self.assertRegex(self.text, r"(?im)^#{1,3}\s+Dynamic Parallel\b")

    def test_branch_body_modes_subsection_present(self):
        """AC-1 / UNIT-002: the dedicated Branch-Body-Modes section is named."""
        self.assertRegex(
            self.text,
            r"(?m)^#{2,4}\s+Dynamic Parallel:\s+Branch Body Modes",
        )

    # ----------------- AC-2 (comparison table) ---------------------------- #

    def test_table_columns_describe_three_modes(self):
        """AC-2: the comparison table mentions all three branch-body keys."""
        self.assertIn("`action:`", self.modes_section)
        self.assertIn("`steps:`", self.modes_section)
        self.assertIn("`subgraph:`", self.modes_section)

    def test_table_uses_markdown_table_syntax(self):
        """AC-2: at least one Markdown table separator inside the section."""
        self.assertRegex(
            self.modes_section,
            r"(?m)^\|[ \-:|]+\|",
        )

    def test_table_headers_include_required_columns(self):
        """AC-2 / UNIT-003: header row covers the AC-2 column set."""
        # Find the first table header line in the modes section.
        header_match = re.search(
            r"(?m)^\|(.+)\|\s*$", self.modes_section
        )
        self.assertIsNotNone(header_match, "No table header found")
        headers = [
            h.strip().lower() for h in header_match.group(1).split("|") if h.strip()
        ]
        required = {
            "mode",
            "when to use",
            "branch state",
            "reusability",
            "where it's defined",
            "example link",
        }
        # Allow superset; require all required columns.
        missing = required - set(headers)
        self.assertFalse(
            missing,
            f"Comparison table missing required columns: {missing}; got {headers}",
        )

    def test_table_has_exactly_three_mode_rows(self):
        """AC-2 / UNIT-004: exactly one row per mode (action, steps, subgraph)."""
        # Count data rows whose first cell mentions one of the three modes.
        # We match cells that contain `action:`, `steps:`, or `subgraph:`
        # (with optional surrounding link / formatting).
        action_rows = re.findall(
            r"(?m)^\|[^|]*`action:`[^|]*\|", self.modes_section
        )
        steps_rows = re.findall(
            r"(?m)^\|[^|]*`steps:`[^|]*\|", self.modes_section
        )
        subgraph_rows = re.findall(
            r"(?m)^\|[^|]*`subgraph:`[^|]*\|", self.modes_section
        )
        self.assertEqual(
            len(action_rows), 1, f"Expected 1 action row, found {len(action_rows)}"
        )
        self.assertEqual(
            len(steps_rows), 1, f"Expected 1 steps row, found {len(steps_rows)}"
        )
        self.assertEqual(
            len(subgraph_rows),
            1,
            f"Expected 1 subgraph row, found {len(subgraph_rows)}",
        )

    # ----------------- AC-3 (minimal examples) ---------------------------- #

    def test_each_mode_has_minimal_example(self):
        """AC-3 / UNIT-005: a fenced YAML example exists for each mode.

        Each example is between 5 and 15 lines so it stays minimal but readable.
        """
        # Extract all fenced ```yaml ... ``` blocks within the section.
        blocks = re.findall(
            r"```yaml\s*\n(.*?)```", self.modes_section, flags=re.DOTALL
        )
        self.assertGreaterEqual(
            len(blocks),
            3,
            f"Expected >=3 yaml example blocks, found {len(blocks)}",
        )

        # Identify which block belongs to which mode by looking for the key.
        modes_seen = {"action": False, "steps": False, "subgraph": False}
        for block in blocks:
            line_count = len([ln for ln in block.splitlines() if ln.strip()])
            self.assertGreaterEqual(
                line_count,
                5,
                f"Example too short ({line_count} non-blank lines): {block!r}",
            )
            self.assertLessEqual(
                line_count,
                15,
                f"Example too long ({line_count} non-blank lines): {block!r}",
            )
            if re.search(r"^\s*action:\s*$", block, flags=re.MULTILINE):
                modes_seen["action"] = True
            if re.search(r"^\s*steps:\s*$", block, flags=re.MULTILINE):
                modes_seen["steps"] = True
            if re.search(r"^\s*subgraph:\s*['\"]?", block, flags=re.MULTILINE):
                modes_seen["subgraph"] = True
        for mode, found in modes_seen.items():
            self.assertTrue(
                found, f"No minimal example detected for mode '{mode}'"
            )

    # ----------------- AC-4 (mutual-exclusion sentence) ------------------- #

    def test_mentions_mutual_exclusion(self):
        """AC-4 / UNIT-006: doc spells out exactly-one / mutual-exclusion rule."""
        self.assertRegex(
            self.modes_section,
            r"(?i)mutually exclusive|exactly one of",
        )
        # And the sentence should name all three modes.
        self.assertIn("action", self.modes_section)
        self.assertIn("steps", self.modes_section)
        self.assertIn("subgraph", self.modes_section)

    # ----------------- AC-5 (fan_in: requirement) ------------------------- #

    def test_mentions_fan_in_requirement(self):
        """AC-5 / UNIT-007: doc states fan_in: is required and is a sibling."""
        # Look for a paragraph (non-empty lines around it) containing both
        # `fan_in` and a "required" / "must reference" wording.
        # Easier: assert the substring exists in the same section.
        self.assertIn("fan_in", self.modes_section)
        self.assertRegex(
            self.modes_section,
            r"(?is)fan_in.{0,200}(required|must reference|sibling|missing)",
        )

    # ----------------- AC-7 / AC-13 (anchor stability) -------------------- #

    def test_anchor_is_pinned(self):
        """AC-7 / UNIT-008: heading carries explicit anchor OR a load-bearing
        comment naming the slug.
        """
        slug = "dynamic-parallel-branch-body-modes"
        explicit_attr = re.search(
            rf"(?m)^#{{2,4}}\s+Dynamic Parallel:\s+Branch Body Modes\s*\{{#{slug}\}}",
            self.text,
        )
        load_bearing_comment = re.search(
            rf"(?is)<!--\s*LOAD-BEARING ANCHOR.*?{slug}.*?-->",
            self.text,
        )
        self.assertTrue(
            explicit_attr or load_bearing_comment,
            "Expected either an explicit {#dynamic-parallel-branch-body-modes} "
            "attribute on the heading OR a load-bearing comment naming the slug "
            "above the heading.",
        )

    # ----------------- AC-6 (cross-link from actions-reference.md) -------- #

    def test_cross_link_from_actions_reference(self):
        """AC-6 / INT-006: actions-reference.md links to the new section."""
        self.assertTrue(ACTIONS_REF.exists(), f"Missing: {ACTIONS_REF}")
        # The link must resolve to the YAML_REFERENCE.md anchor.
        self.assertIn(
            "YAML_REFERENCE.md#dynamic-parallel-branch-body-modes",
            self.actions_text,
            "actions-reference.md is missing a link to "
            "YAML_REFERENCE.md#dynamic-parallel-branch-body-modes",
        )

    # ----------------- AC-9 (existing examples reused) -------------------- #

    def test_section_links_to_existing_example_files(self):
        """AC-9: the new section references the existing example YAML files
        rather than introducing duplicates.
        """
        for fname in (
            "dynamic_parallel_action_mode.yaml",
            "dynamic_parallel_steps_mode.yaml",
            "dynamic_parallel_subgraph_mode.yaml",
        ):
            self.assertIn(
                fname,
                self.modes_section,
                f"Section does not reference existing example {fname}",
            )
            self.assertTrue(
                (EXAMPLES_DIR / fname).exists(),
                f"Referenced example file is missing: {EXAMPLES_DIR / fname}",
            )

    # ----------------- AC-10 (no broken markdown table) ------------------- #

    def test_table_rows_have_consistent_column_count(self):
        """AC-10: every data row in the comparison table has the same number of
        cells as the header row.
        """
        # Capture every line that starts with `|` inside the section, ignoring
        # lines that live inside ```fences```.
        section_lines = self.modes_section.splitlines()
        in_fence = False
        table_lines = []
        for ln in section_lines:
            if ln.lstrip().startswith("```"):
                in_fence = not in_fence
                continue
            if in_fence:
                continue
            if ln.lstrip().startswith("|"):
                table_lines.append(ln)
        # Drop separator rows (--- only).
        table_lines = [
            ln for ln in table_lines if not re.match(r"^\|[ \-:|]+\|$", ln.strip())
        ]
        self.assertGreaterEqual(len(table_lines), 4)  # 1 header + >=3 data rows
        header_cells = [c for c in table_lines[0].strip().strip("|").split("|")]
        for ln in table_lines[1:]:
            cells = [c for c in ln.strip().strip("|").split("|")]
            self.assertEqual(
                len(cells),
                len(header_cells),
                f"Row column count mismatch: {ln!r} has {len(cells)} cells, "
                f"header has {len(header_cells)}",
            )


if __name__ == "__main__":
    unittest.main()
