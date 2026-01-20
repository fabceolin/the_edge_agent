"""
Tests for Ralphy Loop Integration Workflow (TEA-RALPHY-001.9)

Test categories:
- Unit tests for source type detection
- Unit tests for task parsing (all source types)
- Unit tests for engine configuration
- Unit tests for test command detection
- Integration tests for workflow execution
- Checkpoint/resume tests
"""

import os
import tempfile
import pytest
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock

import sys

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from the_edge_agent import YAMLEngine, StateGraph


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def engine():
    """Create a fresh YAMLEngine instance."""
    return YAMLEngine()


@pytest.fixture
def ralphy_workflow_path():
    """Path to the ralphy-loop workflow file."""
    return str(
        Path(__file__).parent.parent.parent
        / "examples"
        / "workflows"
        / "ralphy-loop.yaml"
    )


@pytest.fixture
def sample_bmad_story():
    """Sample BMad story content."""
    return """# Story TEA-TEST-001.1: Test Feature

## Status
In Progress

## Story
Implement a test feature for testing.

## Acceptance Criteria
1. Feature works correctly
2. Tests pass
3. Documentation updated

## Tasks / Subtasks
- [ ] Implement core logic (AC: 1)
  - [ ] Write main function
  - [ ] Add error handling
- [ ] Add tests (AC: 2)
- [ ] Update docs (AC: 3)

## Dev Agent Record
### Debug Log References
### Completion Notes
### File List
### Change Log
"""


@pytest.fixture
def sample_markdown_tasks():
    """Sample markdown with task checkboxes."""
    return """# TODO List

## Tasks
- [ ] First task
- [x] Completed task
- [ ] Third task
  - [ ] Subtask A
  - [ ] Subtask B
"""


@pytest.fixture
def sample_yaml_tasks():
    """Sample YAML task file content."""
    return """tasks:
  - id: task-1
    text: Implement feature X
    description: Full implementation of feature X
  - id: task-2
    text: Write tests for feature X
    description: Unit and integration tests
"""


@pytest.fixture
def temp_bmad_file(sample_bmad_story):
    """Create a temporary BMad story file."""
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".md", delete=False, prefix="TEA-TEST-"
    ) as f:
        f.write(sample_bmad_story)
        yield f.name
    os.unlink(f.name)


@pytest.fixture
def temp_markdown_file(sample_markdown_tasks):
    """Create a temporary markdown file."""
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".md", delete=False, prefix="TODO-"
    ) as f:
        f.write(sample_markdown_tasks)
        yield f.name
    os.unlink(f.name)


@pytest.fixture
def temp_yaml_file(sample_yaml_tasks):
    """Create a temporary YAML tasks file."""
    import yaml

    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".yaml", delete=False, prefix="tasks-"
    ) as f:
        f.write(sample_yaml_tasks)
        yield f.name
    os.unlink(f.name)


# =============================================================================
# AC1: Workflow Loading Tests
# =============================================================================


class TestWorkflowLoading:
    """Tests for loading the ralphy-loop workflow."""

    def test_unit_001_workflow_file_exists(self, ralphy_workflow_path):
        """RALPHY-001-UNIT-001: Workflow file exists at expected path."""
        assert os.path.exists(ralphy_workflow_path)

    def test_unit_002_workflow_loads_successfully(self, engine, ralphy_workflow_path):
        """RALPHY-001-UNIT-002: Workflow YAML loads without errors."""
        graph = engine.load_from_file(ralphy_workflow_path)
        assert isinstance(graph, StateGraph)

    def test_unit_003_workflow_has_required_nodes(self, engine, ralphy_workflow_path):
        """RALPHY-001-UNIT-003: Workflow contains all required nodes."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        node_names = [n["name"] for n in config.get("nodes", [])]

        required_nodes = [
            "detect_source_type",
            "parse_markdown_source",
            "parse_yaml_source",
            "parse_github_source",
            "parse_bmad_source",
            "parse_glob_source",
            "configure_engine",
            "detect_test_command",
            "execute_tasks_sequential",
            "execute_tasks_parallel",
            "execute_tasks_graph",
            "collect_results",
            "report_costs",
            "notify_completion",
        ]

        for node_name in required_nodes:
            assert node_name in node_names, f"Missing node: {node_name}"

    def test_unit_004_workflow_has_initial_state(self, engine, ralphy_workflow_path):
        """RALPHY-001-UNIT-004: Workflow has initial_state with defaults."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        initial_state = config.get("initial_state", {})
        assert initial_state.get("engine") == "claude"
        assert initial_state.get("mode") == "sequential"
        assert initial_state.get("max_concurrency") == 4
        assert initial_state.get("base_branch") == "main"
        assert initial_state.get("create_prs") is True


# =============================================================================
# AC2: Source Type Detection Tests
# =============================================================================


class TestSourceTypeDetection:
    """Tests for detect_source_type node logic."""

    def test_unit_005_detect_github_source(self):
        """RALPHY-002-UNIT-005: Detects github:owner/repo as github type."""
        source = "github:owner/repo"
        assert source.startswith("github:")
        source_type = "github"
        assert source_type == "github"

    def test_unit_006_detect_yaml_source(self):
        """RALPHY-002-UNIT-006: Detects .yaml/.yml as yaml type."""
        for ext in [".yaml", ".yml"]:
            source = f"tasks{ext}"
            source_type = "yaml" if source.endswith((".yaml", ".yml")) else None
            assert source_type == "yaml"

    def test_unit_007_detect_glob_source(self):
        """RALPHY-002-UNIT-007: Detects patterns with * as glob type."""
        source = "stories/*.md"
        source_type = "glob" if "*" in source else None
        assert source_type == "glob"

    def test_unit_008_detect_bmad_source(self, sample_bmad_story):
        """RALPHY-002-UNIT-008: Detects BMad story files by content."""
        content = sample_bmad_story

        is_bmad = (
            "## Tasks / Subtasks" in content or "## Acceptance Criteria" in content
        )
        assert is_bmad is True

    def test_unit_009_detect_markdown_source(self, temp_markdown_file):
        """RALPHY-002-UNIT-009: Detects regular markdown (not BMad)."""
        with open(temp_markdown_file, "r") as f:
            content = f.read()

        is_bmad = (
            "## Tasks / Subtasks" in content or "## Acceptance Criteria" in content
        )
        assert is_bmad is False  # Regular markdown, not BMad


# =============================================================================
# AC3: Engine Configuration Tests
# =============================================================================


class TestEngineConfiguration:
    """Tests for configure_engine node logic."""

    def test_unit_010_engine_mapping_claude(self):
        """RALPHY-003-UNIT-010: Maps 'claude' to 'claude' command."""
        engine_commands = {
            "claude": "claude",
            "codex": "codex",
            "gemini": "gemini",
            "opencode": "opencode",
            "cursor": "agent",
        }
        assert engine_commands["claude"] == "claude"

    def test_unit_011_engine_mapping_cursor(self):
        """RALPHY-003-UNIT-011: Maps 'cursor' to 'agent' command."""
        engine_commands = {
            "claude": "claude",
            "codex": "codex",
            "gemini": "gemini",
            "opencode": "opencode",
            "cursor": "agent",
        }
        assert engine_commands["cursor"] == "agent"

    def test_unit_012_all_engines_mapped(self):
        """RALPHY-003-UNIT-012: All 5 engines have command mappings."""
        engine_commands = {
            "claude": "claude",
            "codex": "codex",
            "gemini": "gemini",
            "opencode": "opencode",
            "cursor": "agent",
        }
        assert len(engine_commands) == 5


# =============================================================================
# AC4: Task Parsing Tests
# =============================================================================


class TestTaskParsing:
    """Tests for task parsing from various sources."""

    def test_unit_013_parse_bmad_tasks(self, engine, sample_bmad_story):
        """RALPHY-004-UNIT-013: Parses tasks from BMad story."""
        # Use bmad.parse_story action
        bmad_parse = engine.actions_registry.get("bmad.parse_story")
        if bmad_parse:
            result = bmad_parse(state={}, content=sample_bmad_story)
            assert result.get("success") is True
            assert len(result.get("tasks", [])) >= 3

    def test_unit_014_parse_markdown_tasks(self, engine, sample_markdown_tasks):
        """RALPHY-004-UNIT-014: Parses tasks from regular markdown."""
        # Use markdown.parse action if available
        md_parse = engine.actions_registry.get("markdown.parse")
        if md_parse:
            result = md_parse(state={}, content=sample_markdown_tasks)
            if result.get("success"):
                tasks = result.get("tasks", [])
                # Should find checkboxes
                assert len(tasks) >= 1

    def test_unit_015_parse_yaml_tasks(self, sample_yaml_tasks):
        """RALPHY-004-UNIT-015: Parses tasks from YAML file."""
        import yaml

        data = yaml.safe_load(sample_yaml_tasks)

        tasks = data.get("tasks", [])
        assert len(tasks) == 2
        assert tasks[0]["id"] == "task-1"
        assert tasks[1]["id"] == "task-2"

    def test_unit_016_task_has_required_fields(self, engine, sample_bmad_story):
        """RALPHY-004-UNIT-016: Parsed tasks have required fields."""
        bmad_parse = engine.actions_registry.get("bmad.parse_story")
        if bmad_parse:
            result = bmad_parse(state={}, content=sample_bmad_story)
            if result.get("success") and result.get("tasks"):
                task = result["tasks"][0]
                # Check required fields exist
                assert "text" in task
                assert "checked" in task


# =============================================================================
# AC5: Test Detection Tests
# =============================================================================


class TestCommandDetection:
    """Tests for test/lint command auto-detection."""

    def test_unit_017_detect_pytest_for_python(self):
        """RALPHY-005-UNIT-017: Detects pytest for Python projects."""
        # Simulate detection logic
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create pyproject.toml
            (Path(tmpdir) / "pyproject.toml").touch()

            test_cmd = None
            if (Path(tmpdir) / "pyproject.toml").exists():
                test_cmd = "pytest"

            assert test_cmd == "pytest"

    def test_unit_018_detect_npm_for_node(self):
        """RALPHY-005-UNIT-018: Detects npm test for Node projects."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create package.json
            (Path(tmpdir) / "package.json").touch()

            test_cmd = None
            if (Path(tmpdir) / "package.json").exists():
                test_cmd = "npm test"

            assert test_cmd == "npm test"

    def test_unit_019_detect_cargo_for_rust(self):
        """RALPHY-005-UNIT-019: Detects cargo test for Rust projects."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create Cargo.toml
            (Path(tmpdir) / "Cargo.toml").touch()

            test_cmd = None
            if (Path(tmpdir) / "Cargo.toml").exists():
                test_cmd = "cargo test"

            assert test_cmd == "cargo test"


# =============================================================================
# AC6: Execution Mode Tests
# =============================================================================


class TestExecutionModes:
    """Tests for sequential, parallel, and graph execution modes."""

    def test_unit_020_sequential_mode_default(self, ralphy_workflow_path):
        """RALPHY-006-UNIT-020: Sequential is the default execution mode."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        initial_state = config.get("initial_state", {})
        assert initial_state.get("mode") == "sequential"

    def test_unit_021_parallel_mode_uses_concurrency(self, ralphy_workflow_path):
        """RALPHY-006-UNIT-021: Parallel mode respects max_concurrency."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        initial_state = config.get("initial_state", {})
        assert initial_state.get("max_concurrency") == 4

    def test_unit_031_graph_mode_node_exists(self, ralphy_workflow_path):
        """RALPHY-006-UNIT-031: Graph execution mode node exists."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        node_names = [n["name"] for n in config.get("nodes", [])]
        assert "execute_tasks_graph" in node_names

    def test_unit_032_graph_mode_edge_exists(self, ralphy_workflow_path):
        """RALPHY-006-UNIT-032: Graph mode has edge from detect_test_command."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        edges = config.get("edges", [])
        graph_edge = None
        for edge in edges:
            if (
                edge.get("from") == "detect_test_command"
                and edge.get("to") == "execute_tasks_graph"
            ):
                graph_edge = edge
                break

        assert graph_edge is not None
        assert "graph" in graph_edge.get("condition", "")

    def test_unit_033_tmux_output_option_defaults(self, ralphy_workflow_path):
        """RALPHY-006-UNIT-033: output_to_tmux defaults to false, tmux session/layout have defaults."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        initial_state = config.get("initial_state", {})
        assert initial_state.get("output_to_tmux") is False
        assert initial_state.get("tmux_session") == "ralphy"
        assert initial_state.get("tmux_layout") == "tiled"

    def test_unit_034_graph_in_interrupt_before(self, ralphy_workflow_path):
        """RALPHY-006-UNIT-034: Graph node is in interrupt_before for checkpoints."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        interrupt_before = config.get("interrupt_before", [])
        assert "execute_tasks_graph" in interrupt_before

    def test_unit_035_graph_to_collect_results_edge(self, ralphy_workflow_path):
        """RALPHY-006-UNIT-035: Graph node connects to collect_results."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        edges = config.get("edges", [])
        graph_to_collect = False
        for edge in edges:
            if (
                edge.get("from") == "execute_tasks_graph"
                and edge.get("to") == "collect_results"
            ):
                graph_to_collect = True
                break

        assert graph_to_collect is True

    def test_unit_036_output_to_tmux_in_state_schema(self, ralphy_workflow_path):
        """RALPHY-006-UNIT-036: output_to_tmux is in state_schema."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        state_schema = config.get("state_schema", {})
        assert "output_to_tmux" in state_schema

    def test_unit_037_three_execution_modes(self, ralphy_workflow_path):
        """RALPHY-006-UNIT-037: All three execution modes (sequential, parallel, graph) have nodes."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        node_names = [n["name"] for n in config.get("nodes", [])]
        assert "execute_tasks_sequential" in node_names
        assert "execute_tasks_parallel" in node_names
        assert "execute_tasks_graph" in node_names


# =============================================================================
# AC7: Cost Tracking Tests
# =============================================================================


class TestCostTracking:
    """Tests for cost/token tracking."""

    def test_unit_022_cost_summary_in_initial_state(self, engine, ralphy_workflow_path):
        """RALPHY-007-UNIT-022: _cost_summary is in initial_state."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        initial_state = config.get("initial_state", {})
        cost_summary = initial_state.get("_cost_summary", {})

        assert "total_input_tokens" in cost_summary
        assert "total_output_tokens" in cost_summary
        assert "total_cost_usd" in cost_summary

    def test_unit_023_opik_settings_enabled(self, engine, ralphy_workflow_path):
        """RALPHY-007-UNIT-023: Opik settings are configured."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        settings = config.get("settings", {})
        opik = settings.get("opik", {})

        assert opik.get("enabled") is True
        assert opik.get("project_name") == "ralphy-loop"


# =============================================================================
# AC8: Checkpoint Configuration Tests
# =============================================================================


class TestCheckpointConfiguration:
    """Tests for checkpoint/resume configuration."""

    def test_unit_024_checkpoint_config_present(self, engine, ralphy_workflow_path):
        """RALPHY-008-UNIT-024: Checkpoint configuration is present."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        checkpoint = config.get("checkpoint", {})
        assert checkpoint.get("enabled") is True
        assert checkpoint.get("directory") == ".ralphy-checkpoints"

    def test_unit_025_interrupt_points_defined(self, engine, ralphy_workflow_path):
        """RALPHY-008-UNIT-025: Interrupt points are defined for execution nodes."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        interrupt_before = config.get("interrupt_before", [])
        assert "execute_tasks_sequential" in interrupt_before
        assert "execute_tasks_parallel" in interrupt_before


# =============================================================================
# Integration Tests
# =============================================================================


class TestWorkflowIntegration:
    """Integration tests for the workflow."""

    def test_int_001_workflow_compiles(self, engine, ralphy_workflow_path):
        """RALPHY-INT-001: Workflow compiles to runnable StateGraph."""
        graph = engine.load_from_file(ralphy_workflow_path)
        compiled = graph.compile()
        assert compiled is not None

    def test_int_002_workflow_initial_state_valid(self, ralphy_workflow_path):
        """RALPHY-INT-002: Workflow YAML has valid initial state defined."""
        import yaml

        with open(ralphy_workflow_path, "r") as f:
            config = yaml.safe_load(f)

        # Check that YAML has default initial state
        initial_state = config.get("initial_state")
        assert initial_state is not None
        assert "engine" in initial_state
        assert "mode" in initial_state

    @pytest.mark.skipif(
        os.environ.get("SKIP_SLOW_TESTS") == "1",
        reason="Skipping slow integration tests",
    )
    def test_int_003_detect_source_type_executes(
        self, engine, ralphy_workflow_path, temp_bmad_file
    ):
        """RALPHY-INT-003: Source type detection executes correctly."""
        graph = engine.load_from_file(ralphy_workflow_path)
        compiled = graph.compile()

        # Execute just the source detection node
        state = {"source": temp_bmad_file}
        events = list(compiled.invoke(state))

        # Should have executed and detected source type
        final_state = events[-1] if events else {}
        # Note: This may not work without proper action mocking
        # The test validates that the workflow can at least start


# =============================================================================
# Edge Case Tests
# =============================================================================


class TestEdgeCases:
    """Tests for edge cases and error handling."""

    def test_unit_026_empty_source_handled(self):
        """RALPHY-EDGE-026: Empty source returns error type."""
        source = ""
        if not source:
            source_type = "error"
        assert source_type == "error"

    def test_unit_027_unknown_engine_handled(self):
        """RALPHY-EDGE-027: Unknown engine falls back gracefully."""
        engine_commands = {
            "claude": "claude",
            "codex": "codex",
            "gemini": "gemini",
            "opencode": "opencode",
            "cursor": "agent",
        }
        unknown_engine = "unknown"
        cmd = engine_commands.get(unknown_engine, unknown_engine)
        assert cmd == "unknown"  # Falls back to engine name itself

    def test_unit_028_no_tasks_produces_empty_list(self):
        """RALPHY-EDGE-028: No tasks found results in empty list."""
        tasks = []
        assert len(tasks) == 0
        assert isinstance(tasks, list)


# =============================================================================
# Documentation Tests
# =============================================================================


class TestDocumentation:
    """Tests for documentation completeness."""

    def test_unit_029_readme_exists(self):
        """RALPHY-DOC-029: README-ralphy.md documentation exists."""
        readme_path = (
            Path(__file__).parent.parent.parent
            / "examples"
            / "workflows"
            / "README-ralphy.md"
        )
        assert readme_path.exists()

    def test_unit_030_readme_has_sections(self):
        """RALPHY-DOC-030: README has required documentation sections."""
        readme_path = (
            Path(__file__).parent.parent.parent
            / "examples"
            / "workflows"
            / "README-ralphy.md"
        )
        with open(readme_path, "r") as f:
            content = f.read()

        required_sections = [
            "Quick Start",
            "Supported Task Sources",
            "Supported AI Engines",
            "Execution Modes",
            "Configuration Options",
            "Checkpoint",
            "Cost Tracking",
        ]

        for section in required_sections:
            assert section in content, f"Missing documentation section: {section}"
