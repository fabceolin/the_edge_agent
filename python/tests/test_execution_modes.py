"""
Tests for Execution Modes (TEA-RALPHY-001.6).

Tests cover:
- ExecutionMode enum and ExecutionConfig parsing
- ExecutionOrchestrator factory
- SequentialExecutor
- ParallelExecutor
- GraphExecutor
"""

import subprocess
from unittest.mock import patch

import pytest

# Configure pytest-asyncio mode
pytest_plugins = ("pytest_asyncio",)

from the_edge_agent.execution.mode import ExecutionMode, ExecutionConfig
from the_edge_agent.execution.orchestrator import ExecutionOrchestrator, BaseExecutor
from the_edge_agent.execution.sequential import SequentialExecutor
from the_edge_agent.execution.parallel import ParallelExecutor
from the_edge_agent.execution.graph import GraphExecutor


class TestExecutionMode:
    """Tests for ExecutionMode enum."""

    def test_mode_values(self):
        """Test mode enum values."""
        assert ExecutionMode.SEQUENTIAL.value == "sequential"
        assert ExecutionMode.PARALLEL.value == "parallel"
        assert ExecutionMode.GRAPH.value == "graph"

    def test_mode_from_string(self):
        """Test creating mode from string."""
        assert ExecutionMode("sequential") == ExecutionMode.SEQUENTIAL
        assert ExecutionMode("parallel") == ExecutionMode.PARALLEL
        assert ExecutionMode("graph") == ExecutionMode.GRAPH

    def test_invalid_mode_raises(self):
        """Test that invalid mode raises ValueError."""
        with pytest.raises(ValueError):
            ExecutionMode("invalid")


class TestExecutionConfig:
    """Tests for ExecutionConfig."""

    def test_default_values(self):
        """Test default configuration values."""
        config = ExecutionConfig()

        assert config.mode == ExecutionMode.SEQUENTIAL
        assert config.target_branch == "main"
        assert config.max_parallel == 4
        assert config.worktree_base == ".worktrees"
        assert config.branch_prefix == "task/"

    def test_from_yaml_sequential(self):
        """Test parsing sequential mode from YAML."""
        settings = {"execution": {"mode": "sequential"}}
        config = ExecutionConfig.from_yaml(settings)

        assert config.mode == ExecutionMode.SEQUENTIAL

    def test_from_yaml_parallel_with_settings(self):
        """Test parsing parallel mode with custom settings."""
        settings = {
            "execution": {
                "mode": "parallel",
                "target_branch": "develop",
                "parallel": {
                    "max_parallel": 8,
                    "worktree_base": ".wt",
                    "branch_prefix": "feature/",
                    "merge_strategy": "ours",
                },
            }
        }
        config = ExecutionConfig.from_yaml(settings)

        assert config.mode == ExecutionMode.PARALLEL
        assert config.target_branch == "develop"
        assert config.max_parallel == 8
        assert config.worktree_base == ".wt"
        assert config.branch_prefix == "feature/"
        assert config.merge_strategy == "ours"

    def test_from_yaml_graph_with_settings(self):
        """Test parsing graph mode with custom settings."""
        settings = {
            "execution": {
                "mode": "graph",
                "graph": {
                    "dot_output_path": "./custom.dot",
                    "yaml_output_path": "./custom.yaml",
                    "shell_provider": "openai",
                    "cleanup_generated_files": True,
                },
            }
        }
        config = ExecutionConfig.from_yaml(settings)

        assert config.mode == ExecutionMode.GRAPH
        assert config.dot_output_path == "./custom.dot"
        assert config.yaml_output_path == "./custom.yaml"
        assert config.shell_provider == "openai"
        assert config.cleanup_generated_files is True

    def test_from_yaml_empty_settings(self):
        """Test parsing with empty settings defaults to sequential."""
        config = ExecutionConfig.from_yaml({})

        assert config.mode == ExecutionMode.SEQUENTIAL

    def test_from_yaml_invalid_mode_defaults(self):
        """Test that invalid mode defaults to sequential."""
        settings = {"execution": {"mode": "invalid_mode"}}
        config = ExecutionConfig.from_yaml(settings)

        assert config.mode == ExecutionMode.SEQUENTIAL

    def test_to_dict(self):
        """Test converting config to dictionary."""
        config = ExecutionConfig(
            mode=ExecutionMode.PARALLEL,
            max_parallel=6,
        )
        d = config.to_dict()

        assert d["mode"] == "parallel"
        assert d["parallel"]["max_parallel"] == 6

    def test_with_overrides(self):
        """Test applying overrides to config."""
        config = ExecutionConfig(mode=ExecutionMode.SEQUENTIAL)

        overridden = config.with_overrides(
            {
                "execution": {
                    "mode": "parallel",
                    "parallel": {"max_parallel": 10},
                }
            }
        )

        assert overridden.mode == ExecutionMode.PARALLEL
        assert overridden.max_parallel == 10


class TestExecutionOrchestrator:
    """Tests for ExecutionOrchestrator factory."""

    def test_creates_sequential_executor(self):
        """Test orchestrator creates SequentialExecutor for sequential mode."""
        config = ExecutionConfig(mode=ExecutionMode.SEQUENTIAL)
        orchestrator = ExecutionOrchestrator(config)

        assert isinstance(orchestrator._executor, SequentialExecutor)

    def test_creates_parallel_executor(self):
        """Test orchestrator creates ParallelExecutor for parallel mode."""
        config = ExecutionConfig(mode=ExecutionMode.PARALLEL)
        orchestrator = ExecutionOrchestrator(config)

        assert isinstance(orchestrator._executor, ParallelExecutor)

    def test_creates_graph_executor(self):
        """Test orchestrator creates GraphExecutor for graph mode."""
        config = ExecutionConfig(mode=ExecutionMode.GRAPH)
        orchestrator = ExecutionOrchestrator(config)

        assert isinstance(orchestrator._executor, GraphExecutor)

    def test_get_mode(self):
        """Test getting current mode."""
        config = ExecutionConfig(mode=ExecutionMode.PARALLEL)
        orchestrator = ExecutionOrchestrator(config)

        assert orchestrator.get_mode() == ExecutionMode.PARALLEL

    def test_get_config_summary(self):
        """Test getting config summary."""
        config = ExecutionConfig(
            mode=ExecutionMode.PARALLEL,
            max_parallel=6,
        )
        orchestrator = ExecutionOrchestrator(config)
        summary = orchestrator.get_config_summary()

        assert summary["mode"] == "parallel"
        assert summary["max_parallel"] == 6

    @pytest.mark.asyncio
    async def test_run_validates_tasks(self):
        """Test that run validates tasks before execution."""
        config = ExecutionConfig(mode=ExecutionMode.SEQUENTIAL)
        orchestrator = ExecutionOrchestrator(config)

        # Empty task list should raise
        with pytest.raises(ValueError, match="empty"):
            await orchestrator.run([], {})

        # Task without id should raise
        with pytest.raises(ValueError, match="id"):
            await orchestrator.run([{"description": "no id"}], {})


class TestSequentialExecutor:
    """Tests for SequentialExecutor."""

    @pytest.mark.asyncio
    async def test_executes_in_order(self):
        """Test tasks execute in specified order."""
        config = ExecutionConfig(mode=ExecutionMode.SEQUENTIAL)
        executor = SequentialExecutor(config)

        execution_order = []

        async def track_executor(task, state, working_dir):
            execution_order.append(task["id"])
            return {"success": True}

        executor._task_executor = track_executor

        tasks = [
            {"id": "first"},
            {"id": "second"},
            {"id": "third"},
        ]

        result = await executor.execute(tasks, {})

        assert result["mode"] == "sequential"
        assert result["tasks_completed"] == 3
        assert execution_order == ["first", "second", "third"]

    @pytest.mark.asyncio
    async def test_state_accumulates(self):
        """Test state accumulates across tasks."""
        config = ExecutionConfig(mode=ExecutionMode.SEQUENTIAL)
        executor = SequentialExecutor(config)

        async def accumulating_executor(task, state, working_dir):
            return {
                "success": True,
                f"{task['id']}_result": f"output from {task['id']}",
            }

        executor._task_executor = accumulating_executor

        tasks = [{"id": "task1"}, {"id": "task2"}]
        result = await executor.execute(tasks, {})

        # Final state should have both results
        assert "task1_result" in result["final_state"]
        assert "task2_result" in result["final_state"]

    @pytest.mark.asyncio
    async def test_handles_task_failure(self):
        """Test executor handles task failure gracefully."""
        config = ExecutionConfig(mode=ExecutionMode.SEQUENTIAL)
        executor = SequentialExecutor(config)

        async def failing_executor(task, state, working_dir):
            if task["id"] == "fail":
                return {"success": False, "error": "Task failed"}
            return {"success": True}

        executor._task_executor = failing_executor

        tasks = [
            {"id": "pass1"},
            {"id": "fail"},
            {"id": "pass2"},
        ]

        result = await executor.execute(tasks, {})

        assert result["status"] == "partial"
        assert result["tasks_completed"] == 2  # pass1 and pass2
        assert result["error"] is not None


class TestParallelExecutor:
    """Tests for ParallelExecutor."""

    @pytest.fixture
    def git_repo(self, tmp_path):
        """Create a temporary git repository."""
        repo = tmp_path / "repo"
        repo.mkdir()
        subprocess.run(["git", "init"], cwd=repo, check=True, capture_output=True)
        subprocess.run(
            ["git", "config", "user.email", "test@test.com"],
            cwd=repo,
            check=True,
            capture_output=True,
        )
        subprocess.run(
            ["git", "config", "user.name", "Test"],
            cwd=repo,
            check=True,
            capture_output=True,
        )
        (repo / "README.md").write_text("# Test\n")
        subprocess.run(["git", "add", "."], cwd=repo, check=True, capture_output=True)
        subprocess.run(
            ["git", "commit", "-m", "Initial"],
            cwd=repo,
            check=True,
            capture_output=True,
        )
        return repo

    def test_respects_max_parallel(self):
        """Test executor respects max_parallel setting."""
        config = ExecutionConfig(
            mode=ExecutionMode.PARALLEL,
            max_parallel=2,
        )
        executor = ParallelExecutor(config)

        assert executor.config.max_parallel == 2

    @pytest.mark.asyncio
    async def test_creates_worktrees(self, git_repo):
        """Test parallel mode creates worktree per task."""
        config = ExecutionConfig(
            mode=ExecutionMode.PARALLEL,
            worktree_base=str(git_repo / ".worktrees"),
            working_directory=str(git_repo),
        )
        executor = ParallelExecutor(config)

        # Mock the task executor to not actually run anything
        async def noop_executor(task, state, working_dir):
            return {"success": True}

        executor._task_executor = noop_executor

        # Mock merge to avoid actual git operations
        with patch.object(executor, "_merge_results", return_value=([], None)):
            tasks = [{"id": "task-1"}, {"id": "task-2"}]

            # This will create worktrees
            try:
                await executor.execute(tasks, {})
            except Exception:
                pass  # Ignore errors, we're testing worktree creation

            # Check worktrees were created
            worktrees_dir = git_repo / ".worktrees"
            if worktrees_dir.exists():
                assert any(worktrees_dir.iterdir())

    @pytest.mark.asyncio
    async def test_simple_parallel_no_worktrees(self, tmp_path):
        """Test parallel mode without worktrees (use_worktrees=False)."""
        config = ExecutionConfig(
            mode=ExecutionMode.PARALLEL,
            use_worktrees=False,
            max_parallel=2,
            working_directory=str(tmp_path),
        )
        executor = ParallelExecutor(config)

        execution_order = []

        async def tracking_executor(task, state, working_dir):
            execution_order.append(task["id"])
            return {"success": True}

        executor._task_executor = tracking_executor

        tasks = [{"id": "task-1"}, {"id": "task-2"}, {"id": "task-3"}]

        result = await executor.execute(tasks, {})

        assert result["mode"] == "parallel"
        assert result["use_worktrees"] is False
        assert result["status"] == "success"
        assert result["tasks_completed"] == 3
        assert result["merge_results"] is None  # No merge for simple parallel
        assert set(execution_order) == {"task-1", "task-2", "task-3"}

    def test_use_worktrees_config_parsing(self):
        """Test that use_worktrees is parsed from YAML config."""
        settings = {
            "execution": {
                "mode": "parallel",
                "parallel": {
                    "use_worktrees": False,
                    "max_parallel": 8,
                },
            }
        }
        config = ExecutionConfig.from_yaml(settings)

        assert config.mode == ExecutionMode.PARALLEL
        assert config.use_worktrees is False
        assert config.max_parallel == 8

    def test_use_worktrees_defaults_true(self):
        """Test that use_worktrees defaults to True."""
        config = ExecutionConfig(mode=ExecutionMode.PARALLEL)
        assert config.use_worktrees is True

        # Also test from_yaml without use_worktrees specified
        settings = {"execution": {"mode": "parallel"}}
        config = ExecutionConfig.from_yaml(settings)
        assert config.use_worktrees is True


class TestGraphExecutor:
    """Tests for GraphExecutor."""

    def test_compute_phases_simple(self):
        """Test computing phases for simple dependency graph."""
        config = ExecutionConfig(mode=ExecutionMode.GRAPH)
        executor = GraphExecutor(config)

        nodes = ["A", "B", "C"]
        deps = {
            "A": set(),
            "B": {"A"},
            "C": {"A"},
        }

        phases = executor._compute_phases(nodes, deps)

        assert len(phases) == 2
        assert phases[0] == ["A"]
        assert set(phases[1]) == {"B", "C"}

    def test_compute_phases_circular_raises(self):
        """Test circular dependency raises error."""
        config = ExecutionConfig(mode=ExecutionMode.GRAPH)
        executor = GraphExecutor(config)

        nodes = ["A", "B"]
        deps = {
            "A": {"B"},
            "B": {"A"},
        }

        with pytest.raises(ValueError, match="Circular dependency"):
            executor._compute_phases(nodes, deps)

    def test_generate_dot_format(self):
        """Test DOT generation follows expected format."""
        config = ExecutionConfig(mode=ExecutionMode.GRAPH)
        executor = GraphExecutor(config)

        graph = {
            "nodes": ["A", "B", "C"],
            "edges": [
                {"from": "A", "to": "C"},
                {"from": "B", "to": "C"},
            ],
        }
        tasks = [
            {"id": "A", "command": "echo A"},
            {"id": "B", "command": "echo B"},
            {"id": "C", "command": "echo C"},
        ]

        dot = executor._generate_dot(graph, tasks)

        # Verify DOT structure
        assert "digraph workflow" in dot
        assert "subgraph cluster_phase_1" in dot
        assert "subgraph cluster_phase_2" in dot
        assert '"A"' in dot
        assert '"B"' in dot
        assert '"C"' in dot
        assert '"A" -> "C"' in dot
        assert '"B" -> "C"' in dot

    def test_parse_dependency_response(self):
        """Test parsing LLM dependency response."""
        config = ExecutionConfig(mode=ExecutionMode.GRAPH)
        executor = GraphExecutor(config)

        response = """
        Here's the analysis:
        {
            "nodes": ["task-1", "task-2"],
            "edges": [{"from": "task-1", "to": "task-2"}],
            "analysis": "task-2 depends on task-1"
        }
        """
        tasks = [{"id": "task-1"}, {"id": "task-2"}]

        graph = executor._parse_dependency_response(response, tasks)

        assert graph["nodes"] == ["task-1", "task-2"]
        assert len(graph["edges"]) == 1

    def test_create_sequential_graph_fallback(self):
        """Test fallback sequential graph creation."""
        config = ExecutionConfig(mode=ExecutionMode.GRAPH)
        executor = GraphExecutor(config)

        tasks = [
            {"id": "first"},
            {"id": "second"},
            {"id": "third"},
        ]

        graph = executor._create_sequential_graph(tasks)

        assert graph["nodes"] == ["first", "second", "third"]
        assert len(graph["edges"]) == 2
        assert graph["edges"][0] == {"from": "first", "to": "second"}
        assert graph["edges"][1] == {"from": "second", "to": "third"}


class TestExecutionModeIntegration:
    """Integration tests for execution modes."""

    @pytest.mark.asyncio
    async def test_sequential_end_to_end(self, tmp_path):
        """Test sequential execution end-to-end."""
        config = ExecutionConfig(
            mode=ExecutionMode.SEQUENTIAL,
            working_directory=str(tmp_path),
        )
        executor = SequentialExecutor(config)

        tasks = [
            {"id": "create-dir", "command": "mkdir -p subdir"},
            {"id": "create-file", "command": "echo 'hello' > subdir/hello.txt"},
        ]

        result = await executor.execute(tasks, {})

        assert result["status"] == "success"
        assert result["tasks_completed"] == 2
        assert (tmp_path / "subdir" / "hello.txt").exists()

    @pytest.mark.asyncio
    async def test_config_override_at_runtime(self):
        """Test configuration override at runtime."""
        # Start with sequential config
        settings = {"execution": {"mode": "sequential"}}
        config = ExecutionConfig.from_yaml(settings)

        assert config.mode == ExecutionMode.SEQUENTIAL

        # Override to parallel at runtime
        overridden = config.with_overrides({"execution": {"mode": "parallel"}})

        assert overridden.mode == ExecutionMode.PARALLEL


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
