"""
Execution Mode Configuration for TEA.

This module defines the ExecutionMode enum and ExecutionConfig dataclass
for configuring how multi-task workflows execute.

Story: TEA-RALPHY-001.6 (Execution Modes)

Example YAML Configuration:
    settings:
      execution:
        mode: parallel
        target_branch: main
        parallel:
          max_parallel: 4
          use_worktrees: true  # Set to false for simple parallel (no git isolation)
          worktree_base: .worktrees
          branch_prefix: "task/"
        graph:
          dot_output_path: "./generated-workflow.dot"
          shell_provider: claude
"""

from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Dict, Optional


class ExecutionMode(Enum):
    """
    Execution mode for multi-task workflows.

    - SEQUENTIAL: Tasks execute one at a time in order. No worktrees created.
    - PARALLEL: Each task gets isolated worktree with dedicated branch.
    - GRAPH: LLM analyzes dependencies, generates DOT. Git worktree/branching DISABLED.
    """

    SEQUENTIAL = "sequential"
    PARALLEL = "parallel"
    GRAPH = "graph"


@dataclass
class ExecutionConfig:
    """
    Configuration for execution mode and related settings.

    This configuration is typically parsed from YAML settings and can be
    overridden at runtime via --input JSON or --config file.

    Attributes:
        mode: Execution mode (sequential, parallel, graph)
        target_branch: Branch to merge/commit changes to
        max_parallel: Max concurrent tasks in parallel mode
        use_worktrees: Enable git worktree isolation (parallel mode, default True)
        worktree_base: Directory for worktrees (parallel mode)
        branch_prefix: Prefix for task branches (parallel mode)
        merge_strategy: Git merge strategy (parallel mode)
        dot_output_path: Path for generated DOT file (graph mode)
        yaml_output_path: Path for generated YAML (graph mode)
        shell_provider: LLM provider for dependency analysis (graph mode)
        prompt_template: Template for dependency analysis (graph mode)
        cleanup_generated_files: Remove DOT/YAML after execution (graph mode)
    """

    # Core settings
    mode: ExecutionMode = ExecutionMode.SEQUENTIAL
    target_branch: str = "main"

    # Parallel mode settings
    max_parallel: int = 4
    use_worktrees: bool = (
        True  # Set to False for simpler parallel without git isolation
    )
    worktree_base: str = ".worktrees"
    branch_prefix: str = "task/"
    merge_strategy: str = "recursive"

    # Graph mode settings
    dot_output_path: str = "./generated-workflow.dot"
    yaml_output_path: str = "./generated-workflow.yaml"
    shell_provider: str = "claude"
    prompt_template: str = "prompts/analyze-task-dependencies.md"
    cleanup_generated_files: bool = False

    # Runtime context (set during execution)
    working_directory: Optional[str] = None
    engine: Optional[Any] = field(default=None, repr=False)

    @classmethod
    def from_yaml(cls, settings: Dict[str, Any]) -> "ExecutionConfig":
        """
        Parse execution config from YAML settings dict.

        The settings dict should have the structure:
            {
                "execution": {
                    "mode": "sequential",
                    "target_branch": "main",
                    "parallel": {...},
                    "graph": {...}
                }
            }

        Args:
            settings: Dictionary from YAML settings section

        Returns:
            ExecutionConfig instance with parsed values
        """
        exec_settings = settings.get("execution", {})

        # Parse mode with default
        mode_str = exec_settings.get("mode", "sequential")
        try:
            mode = ExecutionMode(mode_str)
        except ValueError:
            # Invalid mode, default to sequential
            mode = ExecutionMode.SEQUENTIAL

        # Parse parallel settings
        parallel_settings = exec_settings.get("parallel", {})

        # Parse graph settings
        graph_settings = exec_settings.get("graph", {})

        return cls(
            mode=mode,
            target_branch=exec_settings.get("target_branch", "main"),
            # Parallel mode
            max_parallel=parallel_settings.get("max_parallel", 4),
            use_worktrees=parallel_settings.get("use_worktrees", True),
            worktree_base=parallel_settings.get("worktree_base", ".worktrees"),
            branch_prefix=parallel_settings.get("branch_prefix", "task/"),
            merge_strategy=parallel_settings.get("merge_strategy", "recursive"),
            # Graph mode
            dot_output_path=graph_settings.get(
                "dot_output_path", "./generated-workflow.dot"
            ),
            yaml_output_path=graph_settings.get(
                "yaml_output_path", "./generated-workflow.yaml"
            ),
            shell_provider=graph_settings.get("shell_provider", "claude"),
            prompt_template=graph_settings.get(
                "prompt_template", "prompts/analyze-task-dependencies.md"
            ),
            cleanup_generated_files=graph_settings.get(
                "cleanup_generated_files", False
            ),
        )

    def to_dict(self) -> Dict[str, Any]:
        """
        Convert config to dictionary representation.

        Returns:
            Dictionary suitable for serialization or logging
        """
        return {
            "mode": self.mode.value,
            "target_branch": self.target_branch,
            "parallel": {
                "max_parallel": self.max_parallel,
                "use_worktrees": self.use_worktrees,
                "worktree_base": self.worktree_base,
                "branch_prefix": self.branch_prefix,
                "merge_strategy": self.merge_strategy,
            },
            "graph": {
                "dot_output_path": self.dot_output_path,
                "yaml_output_path": self.yaml_output_path,
                "shell_provider": self.shell_provider,
                "prompt_template": self.prompt_template,
                "cleanup_generated_files": self.cleanup_generated_files,
            },
        }

    def with_overrides(self, overrides: Dict[str, Any]) -> "ExecutionConfig":
        """
        Create a new config with overridden values.

        This is used to apply --input or --config overrides at runtime.

        Args:
            overrides: Dictionary with override values (same structure as from_yaml)

        Returns:
            New ExecutionConfig with overrides applied
        """
        # Start with current config as dict
        current = self.to_dict()

        # Deep merge overrides
        exec_overrides = overrides.get("execution", overrides)

        # Override mode
        if "mode" in exec_overrides:
            try:
                current["mode"] = ExecutionMode(exec_overrides["mode"]).value
            except ValueError:
                pass  # Keep current mode

        # Override target_branch
        if "target_branch" in exec_overrides:
            current["target_branch"] = exec_overrides["target_branch"]

        # Override parallel settings
        if "parallel" in exec_overrides:
            current["parallel"].update(exec_overrides["parallel"])

        # Override graph settings
        if "graph" in exec_overrides:
            current["graph"].update(exec_overrides["graph"])

        # Create new config from merged dict
        return ExecutionConfig.from_yaml({"execution": current})
