# Story TEA-RALPHY-001.9: Integration Workflow Agent

## Status
Deferred

> **Note:** Superseded by [TEA-RALPHY-002](./TEA-RALPHY-002-minimal-ralphy.md) which takes a simpler two-command approach: `tea run dependency-analyzer.yaml | tea from dot - --tmux`.

## Epic Reference
[TEA-RALPHY-001: Autonomous AI Coding Loop](./TEA-RALPHY-001-autonomous-coding-loop.md)

## Dependencies

### Direct Dependencies
- TEA-RALPHY-001.1 (Python `markdown.parse` Action)
- TEA-RALPHY-001.2 (Rust `markdown.parse` Action)
- TEA-RALPHY-001.3 (GitHub Issues Integration)
- TEA-RALPHY-001.4 (BMad Story Task Extraction)
- TEA-RALPHY-001.5 (Additional Shell Providers)
- TEA-RALPHY-001.6 (Execution Modes)

### Transitive Dependencies
- TEA-RALPHY-001.0 (md-graph-parser shared crate) - via 001.1, 001.2

## Story

**As a** developer,
**I want** a complete Ralphy-compatible workflow agent,
**So that** I can run autonomous coding loops with a single YAML file.

## Acceptance Criteria

1. Single YAML agent that orchestrates full loop
2. Support all task sources (Markdown, YAML, GitHub, BMad)
3. Configurable AI engine selection
4. Parallel or sequential execution mode
5. Automatic test and lint execution
6. Git branch and PR workflow
7. Token tracking and cost reporting
8. Checkpoint/resume support

## Tasks / Subtasks

- [x] Create main workflow file (AC: 1)
  - [x] Create `examples/workflows/ralphy-loop.yaml`
  - [x] Define `state_schema` for all inputs
  - [x] Define `initial_state` with sensible defaults
  - [x] Configure `settings.opik.enabled: true` (Opik handles cost tracking)
  - [x] Configure `settings.progress.track: true`
- [x] Implement task source routing node (AC: 2)
  - [x] Create `detect_source_type` node
  - [x] Detect source type from input: `markdown`, `yaml`, `github`, `bmad`, `glob`
  - [x] Route to appropriate parser based on type (conditional edges)
  - [x] Create `parse_markdown_source` node using `markdown.parse`
  - [x] Create `parse_yaml_source` node for direct YAML task files
  - [x] Create `parse_github_source` node using `github.list_issues`
  - [x] Create `parse_bmad_source` node using `bmad.parse_story`
  - [x] Unify output to `state.tasks[]` array
- [x] Implement engine selection node (AC: 3)
  - [x] Create `configure_engine` node
  - [x] Support engines: `claude`, `codex`, `gemini`, `opencode`, `cursor`
  - [x] Validate engine is available (check CLI exists via shutil.which)
  - [x] Store in `state.engine_config`
- [x] Implement execution mode selection (AC: 4)
  - [x] Create `execute_tasks_sequential` node
  - [x] Create `execute_tasks_parallel` node
  - [x] Create `execute_tasks_graph` node (dependency-aware execution)
  - [x] Support modes: `sequential`, `parallel`, `graph` via conditional edges
  - [x] For parallel: use ThreadPoolExecutor with git worktrees
  - [x] For sequential: linear task loop within node
  - [x] For graph: topological execution respecting task dependencies
  - [x] Add `output_to_tmux` option for visual feedback (works with any mode)
  - [x] Respect `max_concurrency` setting (default: 4)
- [x] Implement single task execution node (AC: 3, 5, 6)
  - [x] Execute tasks in `execute_tasks_sequential` and `execute_tasks_parallel` nodes
  - [x] Create git branch: `git checkout -b task/{task_id}`
  - [x] Call AI engine via `llm.call` with shell provider
  - [x] Run tests: use detected test command
  - [x] Run linter: use detected lint command
  - [x] Commit changes: `git add -A && git commit -m "..."`
  - [x] Handle test failures: track in test_results
- [x] Implement test detection (AC: 5)
  - [x] Create `detect_test_command` node
  - [x] Check for `package.json` -> `npm test`
  - [x] Check for `pyproject.toml` / `setup.py` -> `pytest`
  - [x] Check for `Cargo.toml` -> `cargo test`
  - [x] Check for `Makefile` -> `make test`
  - [x] Store in `state.test_command`
- [x] Implement PR creation flow (AC: 6)
  - [x] Integrated into execution nodes
  - [x] Use `gh pr create` via subprocess
  - [x] Generate PR title from task description
  - [x] Generate PR body with task details and test results
  - [x] Support draft PRs via `--draft` flag
  - [x] Store PR URLs in `state.pr_urls`
- [x] Implement cost reporting (AC: 7)
  - [x] Create `report_costs` node (uses Opik integration)
  - [x] Output formatted cost summary in `notify_completion`
  - [x] Track tokens via `_cost_summary` state
- [x] Implement checkpoint configuration (AC: 8)
  - [x] Add `interrupt_before` for `execute_tasks_sequential`, `execute_tasks_parallel`, `execute_tasks_graph`
  - [x] Configure checkpoint directory: `.ralphy-checkpoints`
  - [x] Enable auto_save
- [x] Add documentation
  - [x] Create `examples/workflows/README-ralphy.md`
  - [x] Document all input parameters
  - [x] Document output state
  - [x] Add troubleshooting section
- [x] Add unit and integration tests
  - [x] Create `python/tests/test_ralphy_workflow.py`
  - [x] Test source detection (5 tests)
  - [x] Test execution mode selection (7 tests including 5 tmux tests)
  - [x] Test engine configuration (3 tests)
  - [x] Test task parsing (4 tests)
  - [x] Test checkpoint configuration (2 tests)
  - [x] Test cost tracking (2 tests)
  - [x] Test documentation (2 tests)
  - [x] Total: 38 tests passing

## Dev Notes

### Source Tree

```
examples/workflows/
├── ralphy-loop.yaml              # NEW: Main integration workflow
├── ralphy-task-executor.yaml     # NEW: Single task execution sub-workflow
├── ralphy-pr-creator.yaml        # NEW: PR creation sub-workflow
└── README-ralphy.md              # NEW: Documentation

python/tests/
└── test_ralphy_workflow.py       # NEW: Integration tests
```

### State Schema

```yaml
state_schema:
  # Input parameters
  source: str                    # Task source (file path, glob, github:owner/repo)
  engine: str                    # AI engine name (claude, codex, gemini)
  mode: str                      # Execution mode (sequential, parallel, graph)
  max_concurrency: int           # Max parallel tasks (default: 4)
  base_branch: str               # Git base branch (default: main)
  create_prs: bool               # Create PRs for each task (default: true)
  draft_prs: bool                # Create as draft PRs (default: false)
  run_tests: bool                # Run tests after each task (default: true)
  test_command: str              # Override test command
  lint_command: str              # Override lint command
  output_to_tmux: bool           # Output task execution to tmux panes (default: false)
  tmux_session: str              # Tmux session name (default: ralphy)
  tmux_layout: str               # Tmux layout: tiled, even-horizontal, even-vertical

  # Internal state
  source_type: str               # Detected source type
  tasks: list                    # Parsed tasks
  engine_config: dict            # Engine configuration
  test_results: list             # Test results per task
  completed_tasks: list          # Successfully completed tasks
  failed_tasks: list             # Failed tasks
  pr_urls: list                  # Created PR URLs

  # Tracking
  _token_usage: dict             # From 001.7
  _cost_summary: dict            # From 001.7
  _progress: dict                # From 001.8
```

### Main Workflow Structure

```yaml
# examples/workflows/ralphy-loop.yaml
name: ralphy-loop
description: Autonomous AI Coding Loop - Ralphy-compatible workflow

initial_state:
  engine: claude
  mode: sequential
  max_concurrency: 4
  base_branch: main
  create_prs: true
  draft_prs: false
  run_tests: true
  output_to_tmux: false
  tmux_session: ralphy
  tmux_layout: tiled

settings:
  cost_tracking:
    enabled: true
  progress:
    track: true

nodes:
  # ============================================
  # Phase 1: Source Detection & Parsing
  # ============================================
  - name: detect_source_type
    description: Detect the type of task source
    run: |
      source = state.get("source", "")

      if source.startswith("github:"):
          source_type = "github"
      elif source.endswith(".yaml") or source.endswith(".yml"):
          source_type = "yaml"
      elif source.endswith(".md"):
          source_type = "markdown"
      elif "*" in source:
          source_type = "glob"
      else:
          source_type = "bmad"  # Default to BMad story

      return {"source_type": source_type}

  - name: parse_source
    description: Parse tasks from source
    run: |
      source_type = state["source_type"]
      source = state["source"]
      tasks = []

      if source_type == "glob":
          import glob
          files = glob.glob(source)
          for f in files:
              # Each file becomes a task
              tasks.append({
                  "id": os.path.basename(f).replace(".md", ""),
                  "source_file": f,
                  "type": "bmad"
              })
      elif source_type == "github":
          # Will be handled by github.list_issues action
          pass
      elif source_type == "markdown":
          # Will be handled by markdown.parse action
          pass

      return {"tasks": tasks, "total_tasks": len(tasks)}

  # ============================================
  # Phase 2: Engine Configuration
  # ============================================
  - name: configure_engine
    description: Configure the AI engine
    run: |
      import shutil
      engine = state.get("engine", "claude")

      # Verify engine CLI exists
      engine_cmd = {
          "claude": "claude",
          "codex": "codex",
          "gemini": "gemini",
          "opencode": "opencode",
          "cursor": "cursor"
      }.get(engine, engine)

      if not shutil.which(engine_cmd):
          raise RuntimeError(f"Engine CLI not found: {engine_cmd}")

      return {
          "engine_config": {
              "name": engine,
              "command": engine_cmd,
              "available": True
          }
      }

  # ============================================
  # Phase 3: Test Command Detection
  # ============================================
  - name: detect_test_command
    description: Auto-detect test command for the project
    run: |
      import os

      test_cmd = state.get("test_command")
      lint_cmd = state.get("lint_command")

      if not test_cmd:
          if os.path.exists("package.json"):
              test_cmd = "npm test"
          elif os.path.exists("pyproject.toml") or os.path.exists("setup.py"):
              test_cmd = "pytest"
          elif os.path.exists("Cargo.toml"):
              test_cmd = "cargo test"
          elif os.path.exists("Makefile"):
              test_cmd = "make test"
          else:
              test_cmd = "echo 'No test command detected'"

      if not lint_cmd:
          if os.path.exists("package.json"):
              lint_cmd = "npm run lint"
          elif os.path.exists("pyproject.toml"):
              lint_cmd = "ruff check ."
          elif os.path.exists("Cargo.toml"):
              lint_cmd = "cargo clippy"
          else:
              lint_cmd = "echo 'No lint command detected'"

      return {"test_command": test_cmd, "lint_command": lint_cmd}

  # ============================================
  # Phase 4: Task Execution
  # ============================================
  - name: execute_tasks_sequential
    description: Execute tasks sequentially
    condition: "{{ state.mode == 'sequential' }}"
    run: |
      # Sequential execution handled by loop
      return {"execution_mode": "sequential"}

  - name: execute_tasks_parallel
    description: Execute tasks in parallel
    condition: "{{ state.mode == 'parallel' }}"
    parallel_for: "{{ state.tasks[:state.max_concurrency] }}"
    uses: workflow.invoke
    with:
      workflow: examples/workflows/ralphy-task-executor.yaml
      input:
        task: "{{ item }}"
        engine_config: "{{ state.engine_config }}"
        test_command: "{{ state.test_command }}"
        base_branch: "{{ state.base_branch }}"
    output: task_results

  - name: execute_tasks_graph
    description: Execute tasks based on dependency graph
    condition: "{{ state.mode == 'graph' }}"
    run: |
      from concurrent.futures import ThreadPoolExecutor
      # Analyzes task dependencies (depends_on field)
      # Executes tasks in topological order
      # Parallelizes independent tasks up to max_concurrency
      # Optional: outputs to tmux panes when output_to_tmux=true
      return {"execution_mode": "graph", "task_results": [...]}

  # ============================================
  # Phase 5: Results Collection
  # ============================================
  - name: collect_results
    description: Collect results from all tasks
    run: |
      results = state.get("task_results", [])
      completed = [r for r in results if r.get("success")]
      failed = [r for r in results if not r.get("success")]

      return {
          "completed_tasks": completed,
          "failed_tasks": failed,
          "success_rate": len(completed) / max(len(results), 1) * 100
      }

  # ============================================
  # Phase 6: Cost Reporting
  # ============================================
  - name: report_costs
    description: Generate cost summary
    uses: ralphy.token_summary
    output: cost_report

  - name: notify_completion
    description: Send completion notification
    uses: notify.send
    with:
      title: "Ralphy Loop Complete"
      message: |
        Tasks: {{ state.completed_tasks | length }}/{{ state.total_tasks }} completed
        Cost: ${{ state._cost_summary.total_cost_usd | round(4) }}

edges:
  - from: __start__
    to: detect_source_type
  - from: detect_source_type
    to: parse_source
  - from: parse_source
    to: configure_engine
  - from: configure_engine
    to: detect_test_command
  - from: detect_test_command
    to: execute_tasks_sequential
    condition: "{{ state.mode == 'sequential' }}"
  - from: detect_test_command
    to: execute_tasks_parallel
    condition: "{{ state.mode == 'parallel' }}"
  - from: detect_test_command
    to: execute_tasks_graph
    condition: "{{ state.mode == 'graph' }}"
  - from: execute_tasks_sequential
    to: collect_results
  - from: execute_tasks_parallel
    to: collect_results
  - from: execute_tasks_graph
    to: collect_results
  - from: collect_results
    to: report_costs
  - from: report_costs
    to: notify_completion
  - from: notify_completion
    to: __end__
```

### Task Executor Sub-Workflow

```yaml
# examples/workflows/ralphy-task-executor.yaml
name: ralphy-task-executor
description: Execute a single task with AI engine

state_schema:
  task: dict
  engine_config: dict
  test_command: str
  base_branch: str

nodes:
  - name: create_branch
    run: |
      import subprocess
      task_id = state["task"]["id"]
      branch = f"task/{task_id}"

      subprocess.run(["git", "checkout", state["base_branch"]], check=True)
      subprocess.run(["git", "checkout", "-b", branch], check=True)

      return {"branch": branch}

  - name: execute_with_ai
    uses: llm.call
    with:
      provider: shell
      shell_provider: "{{ state.engine_config.name }}"
      timeout: 3600
      messages:
        - role: user
          content: |
            Implement the following task:

            Task ID: {{ state.task.id }}
            Source: {{ state.task.source_file }}

            Instructions:
            1. Read the source file for requirements
            2. Implement all acceptance criteria
            3. Write tests for your implementation
            4. Do not ask for confirmation, just implement

            When done, output: TASK_COMPLETED
    output: ai_output

  - name: run_tests
    run: |
      import subprocess

      result = subprocess.run(
          state["test_command"],
          shell=True,
          capture_output=True,
          text=True
      )

      return {
          "test_passed": result.returncode == 0,
          "test_output": result.stdout + result.stderr
      }

  - name: commit_changes
    run: |
      import subprocess

      task_id = state["task"]["id"]
      subprocess.run(["git", "add", "-A"], check=True)
      subprocess.run([
          "git", "commit", "-m",
          f"feat: implement {task_id}\n\nAutonomous implementation by Ralphy"
      ], check=True)

      return {"committed": True}

  - name: create_pr
    run: |
      import subprocess

      task_id = state["task"]["id"]
      result = subprocess.run([
          "gh", "pr", "create",
          "--title", f"feat: {task_id}",
          "--body", f"Autonomous implementation of {task_id}\n\nTest results: {'Pass' if state['test_passed'] else 'Fail'}",
          "--draft" if state.get("draft_prs") else ""
      ], capture_output=True, text=True)

      pr_url = result.stdout.strip()
      return {"pr_url": pr_url, "success": True}

edges:
  - from: __start__
    to: create_branch
  - from: create_branch
    to: execute_with_ai
  - from: execute_with_ai
    to: run_tests
  - from: run_tests
    to: commit_changes
  - from: commit_changes
    to: create_pr
  - from: create_pr
    to: __end__
```

### Target Usage

```bash
# Run from Markdown PRD
tea run examples/workflows/ralphy-loop.yaml \
  --input '{"source": "./docs/prd.md", "engine": "claude", "parallel": 4}'

# Run from GitHub Issues
tea run examples/workflows/ralphy-loop.yaml \
  --input '{"source": "github:owner/repo", "labels": ["ready"], "engine": "claude"}'

# Run from BMad stories (using input file)
echo '{"source": "./docs/stories/TEA-*.md", "engine": "claude"}' > vars.json
tea run examples/workflows/ralphy-loop.yaml --input @vars.json

# Sequential mode (default)
tea run examples/workflows/ralphy-loop.yaml \
  --input '{"source": "./docs/stories/TEA-001.md", "mode": "sequential"}'

# With visual progress
tea run examples/workflows/ralphy-loop.yaml \
  --input '{"source": "./docs/stories/*.md"}' --show-graph
```

## Testing

**Test Location:** `python/tests/test_ralphy_workflow.py`

```python
import pytest
import tempfile
import os
from the_edge_agent import YAMLEngine

class TestRalphyWorkflow:
    @pytest.fixture
    def engine(self):
        return YAMLEngine()

    def test_source_type_detection(self, engine):
        """Test that source types are correctly detected."""
        graph = engine.load_from_file("examples/workflows/ralphy-loop.yaml")
        compiled = graph.compile()

        # Test GitHub source
        result = list(compiled.invoke({"source": "github:owner/repo"}))
        final_state = result[-1]["state"]
        assert final_state["source_type"] == "github"

        # Test glob source
        result = list(compiled.invoke({"source": "docs/*.md"}))
        final_state = result[-1]["state"]
        assert final_state["source_type"] == "glob"

    def test_test_command_detection(self, engine):
        """Test auto-detection of test commands."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create package.json
            with open(os.path.join(tmpdir, "package.json"), "w") as f:
                f.write("{}")

            os.chdir(tmpdir)
            graph = engine.load_from_file("examples/workflows/ralphy-loop.yaml")
            # ... test that npm test is detected

    def test_engine_validation(self, engine):
        """Test that invalid engines raise errors."""
        graph = engine.load_from_file("examples/workflows/ralphy-loop.yaml")
        compiled = graph.compile()

        with pytest.raises(RuntimeError, match="Engine CLI not found"):
            list(compiled.invoke({
                "source": "test.md",
                "engine": "nonexistent-engine"
            }))
```

### Test Cases

| Test Case | Description | AC |
|-----------|-------------|-----|
| test_single_yaml_agent | Complete workflow in one file | 1 |
| test_source_type_detection | All source types detected | 2 |
| test_engine_selection | Engine config validated | 3 |
| test_parallel_mode | Parallel execution works | 4 |
| test_sequential_mode | Sequential execution works | 4 |
| test_graph_mode | Graph-based execution works | 4 |
| test_output_to_tmux | Tmux output option works | 4 |
| test_test_detection | Auto-detect test command | 5 |
| test_git_workflow | Branch and commit flow | 6 |
| test_cost_reporting | Token summary generated | 7 |
| test_checkpoint_resume | Resume from checkpoint | 8 |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-17 | 0.1 | Extracted from epic TEA-RALPHY-001 | Sarah (PO) |
| 2025-01-18 | 0.2 | Added transitive dependency on 001.0 (md-graph-parser) | Sarah (PO) |
| 2026-01-19 | 1.0 | Implementation complete - workflow, docs, 33 tests passing | James (Dev) |
| 2026-01-19 | 1.1 | Added graph mode and output_to_tmux option, 40 tests | James (Dev) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

- Fixed YAML syntax errors caused by multiline f-strings containing numbered lists and colons
- Converted multiline f-strings with numbered instructions to list-based prompt construction
- Converted multiline print statements to single-line prints to avoid YAML interpretation issues

### Completion Notes List

- Created single YAML workflow file `ralphy-loop.yaml` with 14 nodes covering all workflow phases
- Implemented source type detection supporting markdown, yaml, github, bmad, and glob patterns
- Added conditional routing to appropriate parsers based on detected source type
- Integrated all 5 shell providers (claude, codex, gemini, opencode, cursor)
- Implemented three execution modes: sequential, parallel, and graph
- Sequential mode executes tasks one at a time with git branch workflow
- Parallel mode uses ThreadPoolExecutor with git worktrees for isolated task execution
- Graph mode respects task dependencies, executing in topological order with parallelism
- Added `output_to_tmux` option for visual real-time feedback (works with any execution mode)
- Auto-detection of test commands (npm test, pytest, cargo test, make test)
- Auto-detection of lint commands (npm run lint, ruff check, cargo clippy)
- Integrated PR creation with gh CLI and draft PR support
- Cost tracking uses Opik integration (settings.opik.enabled)
- Checkpoint configuration with interrupt_before on all execution nodes
- Created comprehensive README-ralphy.md documentation with graph mode and tmux examples
- 40 unit and integration tests all passing

### File List

| File | Status | Description |
|------|--------|-------------|
| `examples/workflows/ralphy-loop.yaml` | Added | Main integration workflow with 14 nodes (~900 lines) |
| `examples/workflows/README-ralphy.md` | Added | Comprehensive documentation with graph mode and tmux examples |
| `python/tests/test_ralphy_workflow.py` | Added | 40 unit/integration tests |

---

## QA Results

_To be filled by QA agent after implementation review_
