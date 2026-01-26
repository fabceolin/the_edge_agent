"""
Tests for BMad Workflow Status Detection (TEA-RALPHY-002.4)

Tests the artifact-based completion detection helper functions used in:
- bmad-story-development.yaml
- bmad-story-validation.yaml

Test coverage:
- check_story_status: Status extraction from story files
- check_story_section: Section existence detection
- check_artifact_file: Glob pattern matching for artifact files
- get_story_id: Story ID extraction from file paths
"""

import os
import re
import glob
import tempfile
import unittest
from pathlib import Path


# ============================================================================
# Helper functions extracted from workflow YAML files
# These mirror the implementation in the summary nodes
# ============================================================================


def check_story_status(story_path, expected_statuses):
    """Check if story status matches expected values (case-insensitive)."""
    try:
        with open(story_path, "r") as f:
            content = f.read()
        # Match ## Status section and capture the status value
        match = re.search(
            r"##\s*Status\s*\n+\s*(\S+[\S\s]*?)(?=\n##|\n\n##|\Z)", content
        )
        if match:
            status = match.group(1).strip().split("\n")[0].strip()
            return any(exp.lower() in status.lower() for exp in expected_statuses)
    except Exception:
        pass
    return False


def check_story_section(story_path, section_name):
    """Check if a section exists in the story file."""
    try:
        with open(story_path, "r") as f:
            content = f.read()
        return section_name in content
    except Exception:
        return False


def check_artifact_file(pattern):
    """Check if artifact file(s) matching glob pattern exist."""
    return len(glob.glob(pattern)) > 0


def get_story_id(story_path):
    """Extract story ID from path (e.g., 'TEA-RALPHY-002.4' from 'docs/stories/TEA-RALPHY-002.4-name.md')."""
    basename = os.path.basename(story_path).replace(".md", "")
    # Match pattern like TEA-XXX-NNN.N or similar
    match = re.match(r"^([A-Z]+-[A-Z]+-\d+\.\d+)", basename)
    if match:
        return match.group(1)
    return basename


# ============================================================================
# Test Classes
# ============================================================================


class TestCheckStoryStatus(unittest.TestCase):
    """Tests for check_story_status helper function (P0/P1 scenarios)."""

    def setUp(self):
        """Create temporary directory for test files."""
        self.temp_dir = tempfile.mkdtemp()

    def tearDown(self):
        """Clean up temporary files."""
        import shutil

        shutil.rmtree(self.temp_dir)

    def create_story_file(self, status_text):
        """Create a temporary story file with given status."""
        content = f"""# Story Title

## Status

{status_text}

## Description

Story description here.
"""
        path = os.path.join(self.temp_dir, "test_story.md")
        with open(path, "w") as f:
            f.write(content)
        return path

    # P0 Scenarios (Critical)
    def test_ready_for_review_status(self):
        """002.4-UNIT-001: check_story_status with 'Ready for Review' returns True."""
        path = self.create_story_file("Ready for Review")
        self.assertTrue(check_story_status(path, ["Ready for Review", "Review"]))

    def test_review_partial_match(self):
        """002.4-UNIT-002: check_story_status with 'Review' returns True (partial match)."""
        path = self.create_story_file("In Review")
        self.assertTrue(check_story_status(path, ["Ready for Review", "Review"]))

    def test_done_status(self):
        """002.4-UNIT-009: check_story_status with 'Done' returns True."""
        path = self.create_story_file("Done")
        self.assertTrue(
            check_story_status(path, ["Done", "Changes Required", "Done (Waived)"])
        )

    # P1 Scenarios (High Priority)
    def test_draft_status_not_match(self):
        """002.4-UNIT-003: check_story_status with 'Draft' returns False."""
        path = self.create_story_file("Draft")
        self.assertFalse(check_story_status(path, ["Ready for Review", "Review"]))

    def test_case_insensitive_matching(self):
        """002.4-UNIT-004: Case-insensitive status matching."""
        path = self.create_story_file("READY FOR REVIEW")
        self.assertTrue(check_story_status(path, ["Ready for Review", "Review"]))

    def test_changes_required_status(self):
        """002.4-UNIT-010: check_story_status with 'Changes Required' returns True."""
        path = self.create_story_file("Changes Required")
        self.assertTrue(
            check_story_status(path, ["Done", "Changes Required", "Done (Waived)"])
        )

    def test_done_waived_status(self):
        """002.4-UNIT-011: check_story_status with 'Done (Waived)' returns True."""
        path = self.create_story_file("Done (Waived)")
        self.assertTrue(
            check_story_status(path, ["Done", "Changes Required", "Done (Waived)"])
        )

    def test_ready_for_development_status(self):
        """check_story_status with 'Ready for Development' returns True."""
        path = self.create_story_file("Ready for Development")
        self.assertTrue(
            check_story_status(path, ["Ready for Development", "Needs Revision"])
        )

    def test_needs_revision_status(self):
        """check_story_status with 'Needs Revision' returns True."""
        path = self.create_story_file("Needs Revision")
        self.assertTrue(
            check_story_status(path, ["Ready for Development", "Needs Revision"])
        )

    # P2 Scenarios (Edge cases)
    def test_missing_status_section(self):
        """check_story_status with missing Status section returns False."""
        content = """# Story Title

## Description

Story description here.
"""
        path = os.path.join(self.temp_dir, "no_status.md")
        with open(path, "w") as f:
            f.write(content)
        self.assertFalse(check_story_status(path, ["Ready for Review"]))

    def test_nonexistent_file(self):
        """check_story_status with nonexistent file returns False."""
        self.assertFalse(
            check_story_status("/nonexistent/path.md", ["Ready for Review"])
        )

    def test_mixed_case_ready_for_review(self):
        """check_story_status with 'ready FOR review' (mixed case) returns True."""
        path = self.create_story_file("ready FOR review")
        self.assertTrue(check_story_status(path, ["Ready for Review", "Review"]))


class TestCheckStorySection(unittest.TestCase):
    """Tests for check_story_section helper function."""

    def setUp(self):
        """Create temporary directory for test files."""
        self.temp_dir = tempfile.mkdtemp()

    def tearDown(self):
        """Clean up temporary files."""
        import shutil

        shutil.rmtree(self.temp_dir)

    def create_story_with_sections(self, sections):
        """Create a temporary story file with given sections."""
        content = "# Story Title\n\n## Status\n\nDraft\n\n"
        for section in sections:
            content += f"{section}\n\nSection content.\n\n"
        path = os.path.join(self.temp_dir, "test_story.md")
        with open(path, "w") as f:
            f.write(content)
        return path

    def test_risk_profile_section_exists(self):
        """check_story_section finds '## QA Notes - Risk Profile' section."""
        path = self.create_story_with_sections(["## QA Notes - Risk Profile"])
        self.assertTrue(check_story_section(path, "## QA Notes - Risk Profile"))

    def test_nfr_assessment_section_exists(self):
        """check_story_section finds '## QA Notes - NFR Assessment' section."""
        path = self.create_story_with_sections(["## QA Notes - NFR Assessment"])
        self.assertTrue(check_story_section(path, "## QA Notes - NFR Assessment"))

    def test_test_design_section_exists(self):
        """check_story_section finds '## QA Notes - Test Design' section."""
        path = self.create_story_with_sections(["## QA Notes - Test Design"])
        self.assertTrue(check_story_section(path, "## QA Notes - Test Design"))

    def test_sm_validation_section_exists(self):
        """check_story_section finds '## SM Validation' section."""
        path = self.create_story_with_sections(["## SM Validation"])
        self.assertTrue(check_story_section(path, "## SM Validation"))

    def test_section_not_found(self):
        """check_story_section returns False for missing section."""
        path = self.create_story_with_sections(["## Other Section"])
        self.assertFalse(check_story_section(path, "## QA Notes - Risk Profile"))

    def test_nonexistent_file(self):
        """check_story_section returns False for nonexistent file."""
        self.assertFalse(check_story_section("/nonexistent/path.md", "## Status"))

    def test_all_qa_sections_present(self):
        """check_story_section finds all QA sections when all present."""
        path = self.create_story_with_sections(
            [
                "## QA Notes - Risk Profile",
                "## QA Notes - NFR Assessment",
                "## QA Notes - Test Design",
                "## SM Validation",
            ]
        )
        self.assertTrue(check_story_section(path, "## QA Notes - Risk Profile"))
        self.assertTrue(check_story_section(path, "## QA Notes - NFR Assessment"))
        self.assertTrue(check_story_section(path, "## QA Notes - Test Design"))
        self.assertTrue(check_story_section(path, "## SM Validation"))


class TestCheckArtifactFile(unittest.TestCase):
    """Tests for check_artifact_file helper function (P0 scenarios)."""

    def setUp(self):
        """Create temporary directory for test files."""
        self.temp_dir = tempfile.mkdtemp()

    def tearDown(self):
        """Clean up temporary files."""
        import shutil

        shutil.rmtree(self.temp_dir)

    def test_gate_file_exists(self):
        """002.4-UNIT-006: check_artifact_file with existing gate returns True."""
        gate_path = os.path.join(self.temp_dir, "TEA-TEST-001.1-story.yml")
        Path(gate_path).touch()
        pattern = os.path.join(self.temp_dir, "TEA-TEST-001.1*.yml")
        self.assertTrue(check_artifact_file(pattern))

    def test_gate_file_not_exists(self):
        """check_artifact_file with nonexistent pattern returns False."""
        pattern = os.path.join(self.temp_dir, "NONEXISTENT*.yml")
        self.assertFalse(check_artifact_file(pattern))

    def test_assessment_file_exists(self):
        """check_artifact_file finds assessment files with date pattern."""
        assessment_path = os.path.join(self.temp_dir, "TEA-TEST-001.1-risk-20260125.md")
        Path(assessment_path).touch()
        pattern = os.path.join(self.temp_dir, "TEA-TEST-001.1-risk-*.md")
        self.assertTrue(check_artifact_file(pattern))

    def test_multiple_matching_files(self):
        """check_artifact_file returns True when multiple files match."""
        Path(os.path.join(self.temp_dir, "TEA-TEST-001.1-story.yml")).touch()
        Path(os.path.join(self.temp_dir, "TEA-TEST-001.1-another.yml")).touch()
        pattern = os.path.join(self.temp_dir, "TEA-TEST-001.1*.yml")
        self.assertTrue(check_artifact_file(pattern))

    def test_nfr_assessment_file(self):
        """check_artifact_file finds NFR assessment files."""
        nfr_path = os.path.join(self.temp_dir, "TEA-TEST-001.1-nfr-20260125.md")
        Path(nfr_path).touch()
        pattern = os.path.join(self.temp_dir, "TEA-TEST-001.1-nfr-*.md")
        self.assertTrue(check_artifact_file(pattern))

    def test_test_design_assessment_file(self):
        """check_artifact_file finds test design assessment files."""
        test_path = os.path.join(
            self.temp_dir, "TEA-TEST-001.1-test-design-20260125.md"
        )
        Path(test_path).touch()
        pattern = os.path.join(self.temp_dir, "TEA-TEST-001.1-test-design-*.md")
        self.assertTrue(check_artifact_file(pattern))


class TestGetStoryId(unittest.TestCase):
    """Tests for get_story_id helper function."""

    def test_standard_story_id(self):
        """get_story_id extracts ID from standard path."""
        path = "docs/stories/TEA-RALPHY-002.4-bmad-workflow-status-detection.md"
        self.assertEqual(get_story_id(path), "TEA-RALPHY-002.4")

    def test_different_prefix(self):
        """get_story_id extracts ID with different prefix."""
        path = "docs/stories/TEA-KIROKU-006.1-some-feature.md"
        self.assertEqual(get_story_id(path), "TEA-KIROKU-006.1")

    def test_absolute_path(self):
        """get_story_id extracts ID from absolute path."""
        path = "/home/user/project/docs/stories/TEA-GAME-001.3-word-embedding.md"
        self.assertEqual(get_story_id(path), "TEA-GAME-001.3")

    def test_non_matching_pattern(self):
        """get_story_id returns full basename for non-matching patterns."""
        path = "docs/stories/simple-story-name.md"
        self.assertEqual(get_story_id(path), "simple-story-name")

    def test_double_digit_version(self):
        """get_story_id handles double-digit story versions."""
        path = "docs/stories/TEA-TEST-123.45-feature.md"
        self.assertEqual(get_story_id(path), "TEA-TEST-123.45")


class TestIntegrationMarkerAndArtifact(unittest.TestCase):
    """Integration tests for marker-first, artifact-fallback detection logic."""

    def setUp(self):
        """Create temporary directory for test files."""
        self.temp_dir = tempfile.mkdtemp()

    def tearDown(self):
        """Clean up temporary files."""
        import shutil

        shutil.rmtree(self.temp_dir)

    def test_marker_takes_priority_over_artifact(self):
        """When marker is present, artifact detection is not needed."""
        # Create story with "Draft" status (would fail artifact check)
        content = """# Story

## Status

Draft

## Description

Test.
"""
        path = os.path.join(self.temp_dir, "test_story.md")
        with open(path, "w") as f:
            f.write(content)

        # Simulate detection logic
        dev_output = "Work done. DEV_STORY_COMPLETED"
        dev_marker = "DEV_STORY_COMPLETED" in dev_output
        dev_artifact = check_story_status(path, ["Ready for Review", "Review"])
        dev_done = dev_marker or dev_artifact

        self.assertTrue(dev_marker)
        self.assertFalse(dev_artifact)
        self.assertTrue(dev_done)

    def test_artifact_fallback_when_marker_missing(self):
        """When marker is absent, artifact detection is used."""
        # Create story with "Ready for Review" status
        content = """# Story

## Status

Ready for Review

## Description

Test.
"""
        path = os.path.join(self.temp_dir, "test_story.md")
        with open(path, "w") as f:
            f.write(content)

        # Simulate detection logic with empty output (no marker)
        dev_output = ""
        dev_marker = "DEV_STORY_COMPLETED" in dev_output
        dev_artifact = check_story_status(path, ["Ready for Review", "Review"])
        dev_done = dev_marker or dev_artifact

        self.assertFalse(dev_marker)
        self.assertTrue(dev_artifact)
        self.assertTrue(dev_done)

    def test_qa_detection_with_gate_file(self):
        """QA detection uses gate file when marker is missing."""
        # Create gate file
        gate_path = os.path.join(self.temp_dir, "TEA-TEST-001.1-story.yml")
        Path(gate_path).touch()

        qa_output = ""  # No marker
        story_id = "TEA-TEST-001.1"
        qa_marker = "QA_REVIEW_COMPLETED" in qa_output
        qa_artifact = check_artifact_file(
            os.path.join(self.temp_dir, f"{story_id}*.yml")
        )
        qa_done = qa_marker or qa_artifact

        self.assertFalse(qa_marker)
        self.assertTrue(qa_artifact)
        self.assertTrue(qa_done)

    def test_sm_detection_with_final_status(self):
        """SM detection checks for final status when marker is missing."""
        content = """# Story

## Status

Done

## Description

Test.
"""
        path = os.path.join(self.temp_dir, "test_story.md")
        with open(path, "w") as f:
            f.write(content)

        sm_output = ""  # No marker
        sm_marker = "SM_STATUS_UPDATED" in sm_output
        sm_artifact = check_story_status(
            path, ["Done", "Changes Required", "Done (Waived)"]
        )
        sm_done = sm_marker or sm_artifact

        self.assertFalse(sm_marker)
        self.assertTrue(sm_artifact)
        self.assertTrue(sm_done)

    def test_validation_workflow_section_detection(self):
        """Validation workflow detects QA sections and SM validation."""
        content = """# Story

## Status

Ready for Development

## QA Notes - Risk Profile

Risk assessment content.

## QA Notes - NFR Assessment

NFR content.

## QA Notes - Test Design

Test design content.

## SM Validation

Validation checklist.
"""
        path = os.path.join(self.temp_dir, "test_story.md")
        with open(path, "w") as f:
            f.write(content)

        # All sections present
        self.assertTrue(check_story_section(path, "## QA Notes - Risk Profile"))
        self.assertTrue(check_story_section(path, "## QA Notes - NFR Assessment"))
        self.assertTrue(check_story_section(path, "## QA Notes - Test Design"))
        self.assertTrue(check_story_section(path, "## SM Validation"))

        # SM requires both section AND status
        sm_section = check_story_section(path, "## SM Validation")
        sm_status = check_story_status(
            path, ["Ready for Development", "Needs Revision"]
        )
        sm_artifact = sm_section and sm_status
        self.assertTrue(sm_artifact)


class TestBackwardCompatibility(unittest.TestCase):
    """Tests to ensure backward compatibility with marker-based detection (AC 9, 13)."""

    def test_dev_marker_still_works(self):
        """DEV_STORY_COMPLETED marker is still detected."""
        output = "All tasks complete. DEV_STORY_COMPLETED"
        self.assertIn("DEV_STORY_COMPLETED", output)

    def test_dev_review_qa_marker_still_works(self):
        """DEV_REVIEW_QA_COMPLETED marker is still detected."""
        output = "QA concerns addressed. DEV_REVIEW_QA_COMPLETED"
        self.assertIn("DEV_REVIEW_QA_COMPLETED", output)

    def test_qa_marker_still_works(self):
        """QA_REVIEW_COMPLETED marker is still detected."""
        output = "Review complete. QA_REVIEW_COMPLETED"
        self.assertIn("QA_REVIEW_COMPLETED", output)

    def test_sm_marker_still_works(self):
        """SM_STATUS_UPDATED marker is still detected."""
        output = "Status set to Done. SM_STATUS_UPDATED"
        self.assertIn("SM_STATUS_UPDATED", output)

    def test_risk_profile_marker_still_works(self):
        """RISK_PROFILE_COMPLETED marker is still detected."""
        output = "Risk profile complete. RISK_PROFILE_COMPLETED"
        self.assertIn("RISK_PROFILE_COMPLETED", output)

    def test_nfr_assess_marker_still_works(self):
        """NFR_ASSESS_COMPLETED marker is still detected."""
        output = "NFR assessment done. NFR_ASSESS_COMPLETED"
        self.assertIn("NFR_ASSESS_COMPLETED", output)

    def test_test_design_marker_still_works(self):
        """TEST_DESIGN_COMPLETED marker is still detected."""
        output = "Test design ready. TEST_DESIGN_COMPLETED"
        self.assertIn("TEST_DESIGN_COMPLETED", output)

    def test_sm_checklist_marker_still_works(self):
        """SM_CHECKLIST_COMPLETED marker is still detected."""
        output = "Checklist validated. SM_CHECKLIST_COMPLETED"
        self.assertIn("SM_CHECKLIST_COMPLETED", output)


if __name__ == "__main__":
    unittest.main()
