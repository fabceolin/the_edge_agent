# Story TEA-RALPHY-001.6: Execution Modes (Sequential, Parallel, Graph)

## Status
Draft

## Epic Reference
[TEA-RALPHY-001: Autonomous AI Coding Loop](./TEA-RALPHY-001-autonomous-coding-loop.md)

## Dependencies

- TEA-RALPHY-001.10 (Dynamic Dependency Analysis - for graph mode DOT generation)

## Story

**As a** workflow developer,
**I want** configurable execution modes (sequential, parallel, graph) for multi-task workflows,
**So that** I can choose the optimal strategy for task isolation, parallelization, and dependency management.

## Acceptance Criteria

### Core Worktree Operations
1. Create isolated worktree per parallel task
2. Automatic branch creation from base branch
3. Execute task within worktree context
4. Merge worktree changes back to main
5. Cleanup worktree after merge
6. Handle merge conflicts by delegating to AI

### Execution Mode Selection
7. Support three execution modes via `settings.execution.mode`: `sequential`, `parallel`, `graph`
8. Default mode is `sequential` when not specified
9. Settings can be overridden at runtime via `--input` JSON/YAML or `--config` file (generic approach)

### Sequential Mode (AC: 10-12)
10. Tasks execute one at a time in order specified
11. All tasks share single working directory (no worktrees created)
12. Single branch used for all changes (current branch or specified `target_branch`)

### Parallel Mode (AC: 13-16)
13. Each task gets isolated worktree with dedicated branch
14. Tasks execute concurrently (limited by `max_parallel` setting)
15. Results merged back to target branch in dependency order
16. Merge conflicts halt execution and return conflict info for AI resolution

### Graph Mode (AC: 17-22)
17. LLM analyzes task dependencies and generates DOT file
18. DOT file follows `DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md` format
19. Execute via `tea from dot --use-node-commands` for maximum parallelization
20. Git worktree/branching is DISABLED in graph mode (DOT handles orchestration)
21. All changes made directly to current working directory
22. DOT file saved to configurable location for debugging/reuse

## Tasks / Subtasks

### Core Worktree Actions
- [ ] Implement `git.worktree_create` action (AC: 1, 2)
  - [ ] Create `python/src/the_edge_agent/actions/git_actions.py`
  - [ ] Accept `branch_name` and `worktree_path` parameters
  - [ ] Create branch from `base_branch` (default: current branch)
  - [ ] Return worktree path for downstream nodes
- [ ] Implement `git.worktree_remove` action (AC: 5)
  - [ ] Remove worktree safely
  - [ ] Optionally delete branch
  - [ ] Handle uncommitted changes gracefully
- [ ] Implement `git.worktree_merge` action (AC: 4, 6)
  - [ ] Merge worktree branch back to target branch
  - [ ] Detect merge conflicts
  - [ ] On conflict: return conflict info for AI resolution

### Execution Mode Framework
- [ ] Create `ExecutionMode` enum and configuration (AC: 7, 8, 9)
  - [ ] Create `python/src/the_edge_agent/execution/mode.py`
  - [ ] Define `ExecutionMode` enum: `SEQUENTIAL`, `PARALLEL`, `GRAPH`
  - [ ] Parse mode from `settings.execution.mode` in YAML
  - [ ] Support override via `--input` JSON/YAML (e.g., `--input '{"settings": {"execution": {"mode": "graph"}}}'`)
  - [ ] Support override via `--config` YAML file for complex overrides
  - [ ] Default to `SEQUENTIAL` when not specified
- [ ] Create `ExecutionOrchestrator` class (AC: 7)
  - [ ] Create `python/src/the_edge_agent/execution/orchestrator.py`
  - [ ] Factory method to select strategy based on mode
  - [ ] Common interface for all execution strategies

### Sequential Mode Implementation
- [ ] Implement `SequentialExecutor` (AC: 10, 11, 12)
  - [ ] Execute tasks in order from task list
  - [ ] Use current working directory (no worktree creation)
  - [ ] Commit changes to current/target branch after each task
  - [ ] Return aggregated results

### Parallel Mode Implementation
- [ ] Implement `ParallelExecutor` (AC: 13, 14, 15, 16)
  - [ ] Create worktree per task using `git.worktree_create`
  - [ ] Execute tasks concurrently with `asyncio.gather` or `ThreadPoolExecutor`
  - [ ] Respect `max_parallel` setting
  - [ ] Merge results in dependency order
  - [ ] Handle merge conflicts with structured conflict info

### Graph Mode Implementation
- [ ] Implement `GraphExecutor` (AC: 17, 18, 19, 20, 21, 22)
  - [ ] Invoke LLM to analyze task dependencies
  - [ ] Generate DOT file using `DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md` format
  - [ ] Save DOT file to `settings.execution.graph.dot_output_path`
  - [ ] Convert DOT to YAML via `tea from dot --use-node-commands`
  - [ ] Execute generated workflow
  - [ ] SKIP all git worktree/branching operations
- [ ] Create dependency analysis prompt template (AC: 17)
  - [ ] Create `examples/prompts/analyze-task-dependencies.md`
  - [ ] Include rules for identifying implicit dependencies
  - [ ] Output JSON dependency graph format

### Workflow Patterns and Examples
- [ ] Create workflow pattern for each mode (AC: 3)
  - [ ] `examples/workflows/sequential-execution.yaml`
  - [ ] `examples/workflows/parallel-worktree-pattern.yaml`
  - [ ] `examples/workflows/graph-orchestrated-pattern.yaml`
- [ ] Document mode selection in README

### Integration Tests
- [ ] Add integration tests for all modes
  - [ ] Create `python/tests/test_git_actions.py`
  - [ ] Create `python/tests/test_execution_modes.py`
  - [ ] Test worktree lifecycle
  - [ ] Test merge with and without conflicts
  - [ ] Test mode selection and override
  - [ ] Test graph mode DOT generation

## Dev Notes

### Execution Mode Configuration

```yaml
# YAML Configuration for Execution Modes
settings:
  execution:
    mode: sequential  # sequential | parallel | graph
    target_branch: main

    # Parallel mode settings
    parallel:
      max_parallel: 4
      worktree_base: .worktrees
      branch_prefix: "task/"
      merge_strategy: recursive  # recursive | ours | theirs

    # Graph mode settings
    graph:
      dot_output_path: "./generated-workflow.dot"
      yaml_output_path: "./generated-workflow.yaml"
      shell_provider: claude  # LLM for dependency analysis
      prompt_template: "prompts/analyze-task-dependencies.md"
      cleanup_generated_files: false  # Keep for debugging
```

### Execution Mode Comparison

| Feature | Sequential | Parallel | Graph |
|---------|------------|----------|-------|
| **Git Worktrees** | ❌ No | ✅ Yes | ❌ No |
| **Git Branching** | Single branch | Per-task branches | ❌ Disabled |
| **Parallelization** | ❌ None | ✅ Concurrent | ✅ DOT-optimized |
| **Dependency Analysis** | Manual order | Manual order | LLM-analyzed |
| **Merge Handling** | N/A | Conflict resolution | N/A |
| **Best For** | Simple tasks | Isolated changes | Complex dependencies |

### ExecutionMode Enum

```python
# python/src/the_edge_agent/execution/mode.py
from enum import Enum
from dataclasses import dataclass
from typing import Optional, Dict, Any

class ExecutionMode(Enum):
    SEQUENTIAL = "sequential"
    PARALLEL = "parallel"
    GRAPH = "graph"

@dataclass
class ExecutionConfig:
    mode: ExecutionMode = ExecutionMode.SEQUENTIAL
    target_branch: str = "main"

    # Parallel mode
    max_parallel: int = 4
    worktree_base: str = ".worktrees"
    branch_prefix: str = "task/"
    merge_strategy: str = "recursive"

    # Graph mode
    dot_output_path: str = "./generated-workflow.dot"
    yaml_output_path: str = "./generated-workflow.yaml"
    shell_provider: str = "claude"
    prompt_template: str = "prompts/analyze-task-dependencies.md"
    cleanup_generated_files: bool = False

    @classmethod
    def from_yaml(cls, settings: Dict[str, Any]) -> "ExecutionConfig":
        """Parse execution config from YAML settings."""
        exec_settings = settings.get("execution", {})
        mode_str = exec_settings.get("mode", "sequential")

        return cls(
            mode=ExecutionMode(mode_str),
            target_branch=exec_settings.get("target_branch", "main"),
            max_parallel=exec_settings.get("parallel", {}).get("max_parallel", 4),
            worktree_base=exec_settings.get("parallel", {}).get("worktree_base", ".worktrees"),
            branch_prefix=exec_settings.get("parallel", {}).get("branch_prefix", "task/"),
            merge_strategy=exec_settings.get("parallel", {}).get("merge_strategy", "recursive"),
            dot_output_path=exec_settings.get("graph", {}).get("dot_output_path", "./generated-workflow.dot"),
            yaml_output_path=exec_settings.get("graph", {}).get("yaml_output_path", "./generated-workflow.yaml"),
            shell_provider=exec_settings.get("graph", {}).get("shell_provider", "claude"),
            prompt_template=exec_settings.get("graph", {}).get("prompt_template", "prompts/analyze-task-dependencies.md"),
            cleanup_generated_files=exec_settings.get("graph", {}).get("cleanup_generated_files", False),
        )
```

### ExecutionOrchestrator

```python
# python/src/the_edge_agent/execution/orchestrator.py
from abc import ABC, abstractmethod
from typing import List, Dict, Any
from .mode import ExecutionMode, ExecutionConfig

class BaseExecutor(ABC):
    """Abstract base class for execution strategies."""

    def __init__(self, config: ExecutionConfig):
        self.config = config

    @abstractmethod
    async def execute(self, tasks: List[Dict[str, Any]], state: Dict[str, Any]) -> Dict[str, Any]:
        """Execute tasks according to the strategy."""
        pass

class ExecutionOrchestrator:
    """Factory for selecting execution strategy based on mode."""

    def __init__(self, config: ExecutionConfig):
        self.config = config
        self._executor = self._create_executor()

    def _create_executor(self) -> BaseExecutor:
        if self.config.mode == ExecutionMode.SEQUENTIAL:
            from .sequential import SequentialExecutor
            return SequentialExecutor(self.config)
        elif self.config.mode == ExecutionMode.PARALLEL:
            from .parallel import ParallelExecutor
            return ParallelExecutor(self.config)
        elif self.config.mode == ExecutionMode.GRAPH:
            from .graph import GraphExecutor
            return GraphExecutor(self.config)
        else:
            raise ValueError(f"Unknown execution mode: {self.config.mode}")

    async def run(self, tasks: List[Dict[str, Any]], state: Dict[str, Any]) -> Dict[str, Any]:
        return await self._executor.execute(tasks, state)
```

### Sequential Executor

```python
# python/src/the_edge_agent/execution/sequential.py
from typing import List, Dict, Any
from .orchestrator import BaseExecutor

class SequentialExecutor(BaseExecutor):
    """Execute tasks one at a time in specified order."""

    async def execute(self, tasks: List[Dict[str, Any]], state: Dict[str, Any]) -> Dict[str, Any]:
        results = []

        for task in tasks:
            # Execute in current working directory
            result = await self._execute_task(task, state)
            results.append(result)

            # Update state with result for next task
            state = {**state, **result}

        return {
            "mode": "sequential",
            "tasks_completed": len(results),
            "results": results,
        }

    async def _execute_task(self, task: Dict[str, Any], state: Dict[str, Any]) -> Dict[str, Any]:
        # Execute task using shell provider
        # No worktree creation - uses current directory
        pass
```

### Parallel Executor

```python
# python/src/the_edge_agent/execution/parallel.py
import asyncio
from typing import List, Dict, Any
from .orchestrator import BaseExecutor
from ..actions.git_actions import git_worktree_create, git_worktree_merge, git_worktree_remove

class ParallelExecutor(BaseExecutor):
    """Execute tasks in parallel with isolated worktrees."""

    async def execute(self, tasks: List[Dict[str, Any]], state: Dict[str, Any]) -> Dict[str, Any]:
        # Create worktrees for each task
        worktrees = []
        for task in tasks:
            wt = git_worktree_create(
                branch_name=f"{self.config.branch_prefix}{task['id']}",
                worktree_path=f"{self.config.worktree_base}/{task['id']}",
            )
            worktrees.append({**wt, "task": task})

        # Execute in parallel with semaphore for max_parallel
        semaphore = asyncio.Semaphore(self.config.max_parallel)

        async def execute_with_limit(wt_info):
            async with semaphore:
                return await self._execute_in_worktree(wt_info, state)

        results = await asyncio.gather(*[execute_with_limit(wt) for wt in worktrees])

        # Merge results back
        merge_results = []
        for wt_info, result in zip(worktrees, results):
            if result.get("success"):
                merge_result = git_worktree_merge(
                    worktree_path=wt_info["worktree_path"],
                    target_branch=self.config.target_branch,
                )
                merge_results.append(merge_result)

                if merge_result.get("conflicts"):
                    return {
                        "mode": "parallel",
                        "status": "conflict",
                        "conflicts": merge_result["conflicts"],
                        "partial_results": results,
                    }

        return {
            "mode": "parallel",
            "status": "success",
            "tasks_completed": len(results),
            "results": results,
            "merge_results": merge_results,
        }

    async def _execute_in_worktree(self, wt_info: Dict[str, Any], state: Dict[str, Any]) -> Dict[str, Any]:
        # Execute task with working_dir set to worktree path
        pass
```

### Graph Executor

```python
# python/src/the_edge_agent/execution/graph.py
import subprocess
from typing import List, Dict, Any
from .orchestrator import BaseExecutor

class GraphExecutor(BaseExecutor):
    """Execute tasks via DOT-based orchestration. Git worktree/branching DISABLED."""

    async def execute(self, tasks: List[Dict[str, Any]], state: Dict[str, Any]) -> Dict[str, Any]:
        # Step 1: Analyze dependencies with LLM
        dependency_graph = await self._analyze_dependencies(tasks, state)

        # Step 2: Generate DOT file
        dot_content = self._generate_dot(dependency_graph)
        with open(self.config.dot_output_path, "w") as f:
            f.write(dot_content)

        # Step 3: Convert DOT to YAML via tea from dot
        subprocess.run([
            "tea", "from", "dot", self.config.dot_output_path,
            "--use-node-commands",
            "-o", self.config.yaml_output_path,
        ], check=True)

        # Step 4: Execute generated workflow
        result = subprocess.run([
            "tea", "run", self.config.yaml_output_path,
        ], capture_output=True, text=True)

        # Step 5: Cleanup if configured
        if self.config.cleanup_generated_files:
            import os
            os.remove(self.config.dot_output_path)
            os.remove(self.config.yaml_output_path)

        return {
            "mode": "graph",
            "dot_path": self.config.dot_output_path,
            "yaml_path": self.config.yaml_output_path,
            "dependency_graph": dependency_graph,
            "stdout": result.stdout,
            "stderr": result.stderr,
            "returncode": result.returncode,
        }

    async def _analyze_dependencies(self, tasks: List[Dict[str, Any]], state: Dict[str, Any]) -> Dict[str, Any]:
        """Use LLM to analyze task dependencies."""
        # Load prompt template
        # Call shell provider (claude, codex, etc.)
        # Parse JSON response: {"nodes": [...], "edges": [...]}
        pass

    def _generate_dot(self, graph: Dict[str, Any]) -> str:
        """Generate DOT file following DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md format."""
        from collections import defaultdict

        nodes = graph["nodes"]
        edges = graph["edges"]

        # Build adjacency list for topological sort
        deps = defaultdict(set)
        for edge in edges:
            deps[edge["to"]].add(edge["from"])

        # Group into parallel phases
        phases = []
        remaining = set(nodes)
        completed = set()

        while remaining:
            ready = [n for n in remaining if deps[n].issubset(completed)]
            if not ready:
                raise ValueError("Circular dependency detected")
            phases.append(ready)
            completed.update(ready)
            remaining -= set(ready)

        # Generate DOT with clusters
        lines = [
            "digraph workflow {",
            "    rankdir=TB;",
            "    compound=true;",
            "",
        ]

        for i, phase in enumerate(phases, 1):
            parallel_label = " (Parallel)" if len(phase) > 1 else ""
            lines.append(f"    // Phase {i}{parallel_label}")
            lines.append(f"    subgraph cluster_phase_{i} {{")
            lines.append(f'        label="Phase {i}";')
            lines.append("        style=dashed;")
            lines.append("")

            for node in phase:
                task = next(t for t in graph.get("task_details", []) if t["id"] == node)
                cmd = task.get("command", f'tea run task.yaml --input \'{{\"task\": \"{node}\"}}\'')
                lines.append(f'        "{node}" [command="{cmd}"];')

            lines.append("    }")
            lines.append("")

        # Add edges
        lines.append("    // Edges")
        for edge in edges:
            lines.append(f'    "{edge["from"]}" -> "{edge["to"]}";')

        lines.append("}")
        return "\n".join(lines)
```

### Action Signatures (Git Worktree)

```python
def git_worktree_create(
    branch_name: str,
    worktree_path: str = None,  # Default: .worktrees/{branch_name}
    base_branch: str = None,    # Default: current branch
    **kwargs
) -> Dict[str, Any]:
    """
    Create an isolated git worktree with a new branch.

    Returns:
        {
            "worktree_path": "/path/to/.worktrees/task-123",
            "branch": "task/task-123",
            "base_branch": "main",
            "created": True
        }
    """

def git_worktree_remove(
    worktree_path: str,
    delete_branch: bool = False,
    force: bool = False,
    **kwargs
) -> Dict[str, Any]:
    """
    Remove a git worktree.

    Returns:
        {"removed": True, "branch_deleted": False}
    """

def git_worktree_merge(
    worktree_path: str,
    target_branch: str = None,  # Default: base branch used in create
    delete_after_merge: bool = True,
    **kwargs
) -> Dict[str, Any]:
    """
    Merge worktree branch back to target branch.

    Returns:
        {
            "merged": True,
            "conflicts": None,  # Or list of conflict files
            "worktree_removed": True
        }
    """
```

### Source Tree

```
python/src/the_edge_agent/
├── execution/                    # NEW: Execution mode framework
│   ├── __init__.py
│   ├── mode.py                   # ExecutionMode enum, ExecutionConfig
│   ├── orchestrator.py           # ExecutionOrchestrator, BaseExecutor
│   ├── sequential.py             # SequentialExecutor
│   ├── parallel.py               # ParallelExecutor
│   └── graph.py                  # GraphExecutor (no git worktree)
├── actions/
│   ├── __init__.py               # MODIFY: Add git_actions import
│   └── git_actions.py            # NEW: git.* actions
└── ...

examples/
├── workflows/
│   ├── sequential-execution.yaml       # NEW: Sequential mode example
│   ├── parallel-worktree-pattern.yaml  # NEW: Parallel mode example
│   └── graph-orchestrated-pattern.yaml # NEW: Graph mode example
└── prompts/
    └── analyze-task-dependencies.md    # NEW: LLM prompt for graph mode
```

### Workflow Examples

#### Sequential Mode

```yaml
# examples/workflows/sequential-execution.yaml
name: sequential-task-runner
description: Execute tasks sequentially in current directory

settings:
  execution:
    mode: sequential
    target_branch: main

initial_state:
  tasks:
    - id: task-1
      description: "Implement feature A"
    - id: task-2
      description: "Implement feature B (uses A)"

nodes:
  - name: run_tasks
    uses: execution.run
    with:
      tasks: "{{ state.tasks }}"
    output: results
```

#### Parallel Mode

```yaml
# examples/workflows/parallel-worktree-pattern.yaml
name: parallel-task-runner
description: Execute tasks in parallel with isolated worktrees

settings:
  execution:
    mode: parallel
    target_branch: main
    parallel:
      max_parallel: 3
      worktree_base: .worktrees
      branch_prefix: "feature/"

initial_state:
  tasks:
    - id: feature-a
      description: "Implement feature A"
    - id: feature-b
      description: "Implement feature B"
    - id: feature-c
      description: "Implement feature C"

nodes:
  - name: run_tasks
    uses: execution.run
    with:
      tasks: "{{ state.tasks }}"
    output: results

  - name: handle_conflicts
    condition: "{{ state.results.status == 'conflict' }}"
    uses: llm.call
    with:
      provider: shell
      shell_provider: claude
      messages:
        - role: user
          content: |
            Resolve these merge conflicts:
            {{ state.results.conflicts | tojson }}
```

#### Graph Mode

```yaml
# examples/workflows/graph-orchestrated-pattern.yaml
name: graph-task-runner
description: LLM analyzes dependencies, generates DOT, maximizes parallelization

settings:
  execution:
    mode: graph
    graph:
      dot_output_path: "./generated-workflow.dot"
      yaml_output_path: "./generated-workflow.yaml"
      shell_provider: claude
      prompt_template: "prompts/analyze-task-dependencies.md"
      cleanup_generated_files: false

initial_state:
  tasks:
    - id: setup-db
      description: "Create database schema"
    - id: api-auth
      description: "Implement authentication API"
    - id: api-users
      description: "Implement users API (needs auth)"
    - id: api-products
      description: "Implement products API (needs db)"
    - id: frontend
      description: "Build frontend (needs all APIs)"

nodes:
  - name: run_tasks
    uses: execution.run
    with:
      tasks: "{{ state.tasks }}"
    output: results
```

### Git Commands Used (Parallel Mode Only)

```bash
# Create worktree with new branch
git worktree add -b task/123 .worktrees/task-123 main

# List worktrees
git worktree list

# Remove worktree
git worktree remove .worktrees/task-123

# Force remove (if uncommitted changes)
git worktree remove --force .worktrees/task-123

# Merge back
git checkout main
git merge task/123
```

### Conflict Handling (Parallel Mode Only)

When merge conflicts occur, return structured conflict info:

```python
{
    "merged": False,
    "conflicts": [
        {
            "file": "src/main.py",
            "ours": "...",
            "theirs": "...",
            "base": "..."
        }
    ],
    "resolution_prompt": "Resolve these merge conflicts..."
}
```

The downstream node can pass this to an AI for resolution.

### CLI Usage

```bash
# Run with default mode (sequential - defined in workflow.yaml settings)
tea run workflow.yaml

# Override mode at runtime via --input (generic approach)
tea run workflow.yaml --input '{"settings": {"execution": {"mode": "parallel"}}}'
tea run workflow.yaml --input '{"settings": {"execution": {"mode": "graph"}}}'

# Override via config file (for complex overrides)
tea run workflow.yaml --config overrides.yaml

# Example overrides.yaml:
# settings:
#   execution:
#     mode: graph
#     graph:
#       dot_output_path: "./my-workflow.dot"
#       shell_provider: gemini

# Graph mode generates and executes DOT
# → Generates: ./generated-workflow.dot
# → Converts:  tea from dot ./generated-workflow.dot --use-node-commands
# → Executes:  tea run ./generated-workflow.yaml
```

> **Architectural Note:** The `--input` and `--config` flags are generic mechanisms that can override any workflow setting, not just execution mode. This follows the principle of having a single, extensible configuration system rather than adding specific CLI flags for each feature.

## Testing

### Test Locations

- `python/tests/test_git_actions.py` - Git worktree actions
- `python/tests/test_execution_modes.py` - Execution mode framework

### Git Actions Tests

```python
# python/tests/test_git_actions.py
import pytest
import tempfile
import subprocess
from pathlib import Path
from the_edge_agent.actions.git_actions import (
    git_worktree_create,
    git_worktree_remove,
    git_worktree_merge,
)

@pytest.fixture
def git_repo(tmp_path):
    """Create a temporary git repository."""
    repo = tmp_path / "repo"
    repo.mkdir()
    subprocess.run(["git", "init"], cwd=repo, check=True)
    subprocess.run(["git", "config", "user.email", "test@test.com"], cwd=repo)
    subprocess.run(["git", "config", "user.name", "Test"], cwd=repo)
    (repo / "README.md").write_text("# Test")
    subprocess.run(["git", "add", "."], cwd=repo, check=True)
    subprocess.run(["git", "commit", "-m", "Initial"], cwd=repo, check=True)
    return repo

def test_worktree_create(git_repo):
    """Test creating a worktree."""
    result = git_worktree_create(
        branch_name="task-123",
        worktree_path=str(git_repo / ".worktrees" / "task-123"),
    )
    assert result["created"] == True
    assert Path(result["worktree_path"]).exists()

def test_worktree_remove(git_repo):
    """Test removing a worktree."""
    wt_path = str(git_repo / ".worktrees" / "task-123")
    git_worktree_create(branch_name="task-123", worktree_path=wt_path)

    result = git_worktree_remove(worktree_path=wt_path)
    assert result["removed"] == True
    assert not Path(wt_path).exists()

def test_worktree_merge_success(git_repo):
    """Test merging a worktree branch."""
    wt_path = str(git_repo / ".worktrees" / "task-123")
    git_worktree_create(branch_name="task-123", worktree_path=wt_path)

    (Path(wt_path) / "new_file.py").write_text("# New file")
    subprocess.run(["git", "add", "."], cwd=wt_path, check=True)
    subprocess.run(["git", "commit", "-m", "Add file"], cwd=wt_path, check=True)

    result = git_worktree_merge(worktree_path=wt_path)
    assert result["merged"] == True
    assert result["conflicts"] is None

def test_worktree_merge_conflict(git_repo):
    """Test handling merge conflicts."""
    # Setup conflict scenario
    # ... create conflicting changes
    pass
```

### Execution Mode Tests

```python
# python/tests/test_execution_modes.py
import pytest
from pathlib import Path
from the_edge_agent.execution.mode import ExecutionMode, ExecutionConfig
from the_edge_agent.execution.orchestrator import ExecutionOrchestrator
from the_edge_agent.execution.sequential import SequentialExecutor
from the_edge_agent.execution.parallel import ParallelExecutor
from the_edge_agent.execution.graph import GraphExecutor

class TestExecutionConfig:
    def test_default_mode_is_sequential(self):
        """Default mode should be sequential."""
        config = ExecutionConfig()
        assert config.mode == ExecutionMode.SEQUENTIAL

    def test_from_yaml_sequential(self):
        """Parse sequential mode from YAML."""
        settings = {"execution": {"mode": "sequential"}}
        config = ExecutionConfig.from_yaml(settings)
        assert config.mode == ExecutionMode.SEQUENTIAL

    def test_from_yaml_parallel(self):
        """Parse parallel mode with settings."""
        settings = {
            "execution": {
                "mode": "parallel",
                "parallel": {
                    "max_parallel": 8,
                    "worktree_base": ".wt",
                    "branch_prefix": "feat/",
                }
            }
        }
        config = ExecutionConfig.from_yaml(settings)
        assert config.mode == ExecutionMode.PARALLEL
        assert config.max_parallel == 8
        assert config.worktree_base == ".wt"
        assert config.branch_prefix == "feat/"

    def test_from_yaml_graph(self):
        """Parse graph mode with settings."""
        settings = {
            "execution": {
                "mode": "graph",
                "graph": {
                    "dot_output_path": "./custom.dot",
                    "shell_provider": "gemini",
                }
            }
        }
        config = ExecutionConfig.from_yaml(settings)
        assert config.mode == ExecutionMode.GRAPH
        assert config.dot_output_path == "./custom.dot"
        assert config.shell_provider == "gemini"


class TestExecutionOrchestrator:
    def test_creates_sequential_executor(self):
        """Orchestrator creates SequentialExecutor for sequential mode."""
        config = ExecutionConfig(mode=ExecutionMode.SEQUENTIAL)
        orchestrator = ExecutionOrchestrator(config)
        assert isinstance(orchestrator._executor, SequentialExecutor)

    def test_creates_parallel_executor(self):
        """Orchestrator creates ParallelExecutor for parallel mode."""
        config = ExecutionConfig(mode=ExecutionMode.PARALLEL)
        orchestrator = ExecutionOrchestrator(config)
        assert isinstance(orchestrator._executor, ParallelExecutor)

    def test_creates_graph_executor(self):
        """Orchestrator creates GraphExecutor for graph mode."""
        config = ExecutionConfig(mode=ExecutionMode.GRAPH)
        orchestrator = ExecutionOrchestrator(config)
        assert isinstance(orchestrator._executor, GraphExecutor)


class TestSequentialExecutor:
    @pytest.mark.asyncio
    async def test_executes_in_order(self):
        """Tasks execute in specified order."""
        config = ExecutionConfig(mode=ExecutionMode.SEQUENTIAL)
        executor = SequentialExecutor(config)

        tasks = [
            {"id": "task-1", "description": "First"},
            {"id": "task-2", "description": "Second"},
        ]

        result = await executor.execute(tasks, {})
        assert result["mode"] == "sequential"
        assert result["tasks_completed"] == 2

    @pytest.mark.asyncio
    async def test_no_worktrees_created(self, tmp_path):
        """Sequential mode does not create worktrees."""
        config = ExecutionConfig(mode=ExecutionMode.SEQUENTIAL, worktree_base=str(tmp_path / ".wt"))
        executor = SequentialExecutor(config)

        tasks = [{"id": "task-1"}]
        await executor.execute(tasks, {})

        assert not (tmp_path / ".wt").exists()


class TestParallelExecutor:
    @pytest.mark.asyncio
    async def test_creates_worktrees(self, git_repo):
        """Parallel mode creates worktree per task."""
        config = ExecutionConfig(
            mode=ExecutionMode.PARALLEL,
            worktree_base=str(git_repo / ".worktrees"),
        )
        executor = ParallelExecutor(config)

        tasks = [
            {"id": "task-1"},
            {"id": "task-2"},
        ]

        result = await executor.execute(tasks, {})
        assert result["mode"] == "parallel"

    @pytest.mark.asyncio
    async def test_respects_max_parallel(self):
        """Parallel mode respects max_parallel setting."""
        config = ExecutionConfig(mode=ExecutionMode.PARALLEL, max_parallel=2)
        executor = ParallelExecutor(config)
        # Verify semaphore limits concurrent execution
        assert executor.config.max_parallel == 2


class TestGraphExecutor:
    @pytest.mark.asyncio
    async def test_generates_dot_file(self, tmp_path):
        """Graph mode generates DOT file."""
        dot_path = str(tmp_path / "workflow.dot")
        config = ExecutionConfig(
            mode=ExecutionMode.GRAPH,
            dot_output_path=dot_path,
        )
        executor = GraphExecutor(config)

        # Mock dependency analysis
        # ...

    @pytest.mark.asyncio
    async def test_no_git_operations(self, git_repo):
        """Graph mode does NOT create worktrees or branches."""
        config = ExecutionConfig(
            mode=ExecutionMode.GRAPH,
            worktree_base=str(git_repo / ".worktrees"),
        )
        executor = GraphExecutor(config)

        tasks = [{"id": "task-1"}]
        # Execute and verify no .worktrees directory created
        # This confirms git worktree/branching is DISABLED

    def test_dot_format_follows_guide(self):
        """Generated DOT follows DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md format."""
        config = ExecutionConfig(mode=ExecutionMode.GRAPH)
        executor = GraphExecutor(config)

        graph = {
            "nodes": ["A", "B", "C"],
            "edges": [
                {"from": "A", "to": "C"},
                {"from": "B", "to": "C"},
            ],
            "task_details": [
                {"id": "A", "command": "echo A"},
                {"id": "B", "command": "echo B"},
                {"id": "C", "command": "echo C"},
            ],
        }

        dot = executor._generate_dot(graph)

        assert "digraph workflow" in dot
        assert "subgraph cluster_phase_1" in dot
        assert "subgraph cluster_phase_2" in dot
        assert '"A"' in dot and '"B"' in dot
        assert '"A" -> "C"' in dot
        assert '"B" -> "C"' in dot

    def test_circular_dependency_detection(self):
        """Circular dependencies raise error."""
        config = ExecutionConfig(mode=ExecutionMode.GRAPH)
        executor = GraphExecutor(config)

        graph = {
            "nodes": ["A", "B"],
            "edges": [
                {"from": "A", "to": "B"},
                {"from": "B", "to": "A"},
            ],
        }

        with pytest.raises(ValueError, match="Circular dependency"):
            executor._generate_dot(graph)
```

### Test Cases Summary

| Test Case | Description | AC |
|-----------|-------------|-----|
| **Core Worktree Tests** | | |
| test_worktree_create | Create isolated worktree | 1 |
| test_worktree_auto_branch | Branch created from base | 2 |
| test_execute_in_worktree | Commands run in worktree | 3 |
| test_worktree_merge_success | Merge without conflicts | 4 |
| test_worktree_cleanup | Worktree removed after merge | 5 |
| test_worktree_merge_conflict | Conflict info returned | 6 |
| **Mode Selection Tests** | | |
| test_default_mode_is_sequential | Default mode is sequential | 7, 8 |
| test_from_yaml_sequential | Parse sequential from YAML | 7 |
| test_from_yaml_parallel | Parse parallel from YAML | 7 |
| test_from_yaml_graph | Parse graph from YAML | 7 |
| test_input_override | --input JSON overrides settings | 9 |
| test_config_file_override | --config YAML file overrides settings | 9 |
| **Sequential Mode Tests** | | |
| test_executes_in_order | Tasks run in order | 10 |
| test_no_worktrees_created | No worktrees in sequential | 11 |
| test_single_branch | Uses single branch | 12 |
| **Parallel Mode Tests** | | |
| test_creates_worktrees | Creates worktree per task | 13 |
| test_respects_max_parallel | Concurrency limit | 14 |
| test_merges_in_order | Merge in dependency order | 15 |
| test_conflict_halts_execution | Conflicts halt and return info | 16 |
| **Graph Mode Tests** | | |
| test_analyzes_dependencies | LLM analyzes dependencies | 17 |
| test_dot_format_follows_guide | DOT follows guide format | 18 |
| test_tea_from_dot_execution | Executes via tea from dot | 19 |
| test_no_git_operations | No worktrees/branches | 20 |
| test_changes_in_current_dir | Changes in cwd | 21 |
| test_dot_file_saved | DOT saved to config path | 22 |
| test_circular_dependency_detection | Circular deps error | 17 |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-17 | 0.1 | Extracted from epic TEA-RALPHY-001 | Sarah (PO) |
| 2026-01-18 | 0.2 | Added three execution modes: sequential, parallel, graph. Graph mode uses LLM dependency analysis and DOT orchestration with git worktree/branching disabled | Sarah (PO) |
| 2026-01-18 | 0.3 | Replaced `--mode` CLI flag with generic `--input` JSON/YAML and `--config` file approach per architect feedback | Sarah (PO) |

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
