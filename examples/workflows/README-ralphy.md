# Ralphy Workflows - Autonomous AI Coding

The Ralphy workflows provide autonomous coding capabilities using multiple AI engines.

## Workflow Files

| Workflow | File | Purpose |
|----------|------|---------|
| **Develop** | `ralphy-develop.yaml` | Implements tasks from stories using AI engines |
| **Validate** | `ralphy-validate.yaml` | Validates story structure before development |
| **Loop** | `ralphy-loop.yaml` | Alias for develop (original name) |

## Quick Comparison

```bash
# Validate a story before development
tea run examples/workflows/ralphy-validate.yaml \
    --input '{"source": "docs/stories/TEA-001.md"}'

# Develop/implement tasks from a story
tea run examples/workflows/ralphy-develop.yaml \
    --input '{"source": "docs/stories/TEA-001.md"}'

# Validate all stories in an epic
tea run examples/workflows/ralphy-validate.yaml \
    --input '{"source": "docs/stories/TEA-RALPHY-001.*.md"}'
```

---

# Ralphy Develop - Development Workflow

The development workflow (`ralphy-develop.yaml`) orchestrates autonomous coding sessions using multiple AI engines. It automatically parses tasks from various sources, executes them in sequential, parallel, or graph mode, runs tests, and creates pull requests. Optionally outputs to tmux panes for visual real-time feedback.

## Quick Start

```bash
# Run with default settings (Claude, sequential mode)
tea run examples/workflows/ralphy-loop.yaml \
    --input '{"source": "docs/stories/my-story.md"}'

# Run with a different engine in parallel mode
tea run examples/workflows/ralphy-loop.yaml \
    --input '{"source": "docs/stories/my-story.md", "engine": "codex", "mode": "parallel"}'

# Run from GitHub Issues
tea run examples/workflows/ralphy-loop.yaml \
    --input '{"source": "github:owner/repo", "engine": "gemini"}'
```

## Supported Task Sources

| Source Type | Format | Example |
|-------------|--------|---------|
| **BMad Story** | `.md` with `## Tasks / Subtasks` | `docs/stories/TEA-001.md` |
| **Markdown** | `.md` with checkboxes | `TODO.md` |
| **YAML** | `.yaml` with `tasks:` key | `tasks.yaml` |
| **GitHub** | `github:owner/repo` | `github:myorg/myrepo` |
| **Glob** | Pattern matching files | `stories/*.md` |

### BMad Story Format

BMad story files are automatically detected when they contain `## Acceptance Criteria` or `## Tasks / Subtasks` sections:

```markdown
# Story TEA-001.1: Feature Implementation

## Status
In Progress

## Acceptance Criteria
1. Feature works correctly
2. Tests pass

## Tasks / Subtasks
- [ ] Implement core logic (AC: 1)
  - [ ] Write main function
  - [ ] Add error handling
- [ ] Add tests (AC: 2)
```

### YAML Task Format

```yaml
tasks:
  - id: task-1
    text: Implement feature X
    description: Full description here
  - id: task-2
    text: Write tests for feature X
```

## Supported AI Engines

| Engine | CLI Command | Description |
|--------|-------------|-------------|
| `claude` | `claude` | Anthropic Claude Code CLI |
| `codex` | `codex` | OpenAI Codex CLI |
| `gemini` | `gemini` | Google Gemini CLI |
| `opencode` | `opencode` | Opencode CLI |
| `cursor` | `agent` | Cursor Agent CLI |

The workflow auto-detects if the engine CLI is available on your system.

## Execution Modes

### Sequential Mode (Default)

Tasks are executed one at a time. Each task:
1. Creates a new git branch
2. Runs the AI to implement the task
3. Runs tests
4. Creates a PR (optional)
5. Returns to base branch

```yaml
mode: sequential
```

### Parallel Mode

Tasks are executed concurrently using git worktrees for isolation:
1. Creates isolated worktrees for each task
2. Runs multiple AI instances in parallel
3. Merges completed work back to base branch
4. Creates PRs for successful tasks

```yaml
mode: parallel
max_concurrency: 4  # Maximum parallel tasks
```

### Graph Mode

Tasks are executed based on their dependency graph, respecting task dependencies:
1. Analyzes task dependencies (`depends_on` field)
2. Executes tasks in topological order
3. Parallelizes independent tasks up to max_concurrency
4. Creates isolated worktrees for each task
5. Creates PRs for successful tasks

```yaml
mode: graph
max_concurrency: 4  # Maximum parallel tasks
```

Tasks can specify dependencies:
```yaml
tasks:
  - id: task-1
    text: Create database schema
  - id: task-2
    text: Write API endpoints
    depends_on: [task-1]
  - id: task-3
    text: Write frontend
    depends_on: [task-2]
```

### Tmux Output (Optional)

Any execution mode can output to tmux panes for visual real-time feedback:

```yaml
mode: parallel           # Or sequential, or graph
output_to_tmux: true     # Enable tmux output
tmux_session: ralphy     # Session name (default: ralphy)
tmux_layout: tiled       # Layout: tiled, even-horizontal, even-vertical
```

**Requirements:**
- Must have `tmux` installed
- Works best when already inside a tmux session
- If not in tmux, creates a new detached session

**Use cases:**
- Debugging task execution in real-time
- Monitoring multiple AI agents working simultaneously
- Interactive workflows where you want to see all output

## Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `source` | string | *required* | Task source (file path, glob, or github:owner/repo) |
| `engine` | string | `claude` | AI engine to use |
| `mode` | string | `sequential` | Execution mode: sequential, parallel, or graph |
| `max_concurrency` | int | `4` | Max parallel tasks |
| `base_branch` | string | `main` | Git base branch |
| `create_prs` | bool | `true` | Create PRs for completed tasks |
| `draft_prs` | bool | `false` | Create PRs as drafts |
| `run_tests` | bool | `true` | Run tests after each task |
| `test_command` | string | *auto-detect* | Override test command |
| `lint_command` | string | *auto-detect* | Override lint command |
| `output_to_tmux` | bool | `false` | Output task execution to tmux panes |
| `tmux_session` | string | `ralphy` | Tmux session name (when output_to_tmux=true) |
| `tmux_layout` | string | `tiled` | Tmux layout (when output_to_tmux=true) |

## Test Command Auto-Detection

The workflow automatically detects test commands based on project files:

| File | Test Command | Lint Command |
|------|--------------|--------------|
| `package.json` | `npm test` | `npm run lint` |
| `pyproject.toml` | `pytest` | `ruff check .` |
| `Cargo.toml` | `cargo test` | `cargo clippy` |
| `Makefile` | `make test` | - |

Override with explicit commands:

```json
{
  "source": "story.md",
  "test_command": "pytest -x -v",
  "lint_command": "black --check ."
}
```

## Checkpoint & Resume

The workflow supports checkpoint/resume for long-running sessions:

```yaml
checkpoint:
  enabled: true
  directory: ".ralphy-checkpoints"
  auto_save: true

interrupt_before:
  - execute_tasks_sequential
  - execute_tasks_parallel
  - execute_tasks_graph
```

Resume a checkpointed workflow:

```bash
tea resume --checkpoint .ralphy-checkpoints/latest
```

## Cost Tracking

Token usage and costs are tracked via Opik integration:

```yaml
settings:
  opik:
    enabled: true
    project_name: "ralphy-loop"
```

The `report_costs` node outputs a summary:

```
===============================================================================
                        RALPHY LOOP COMPLETE
===============================================================================
Tasks:    5/6 completed (83.3%)
Failed:   1
PRs:      5

Cost:     ~$0.0234 USD
Tokens:   12500 in / 8200 out
===============================================================================
```

View detailed traces in the Opik dashboard when `opik.enabled=true`.

## Workflow Diagram

```
                    +-----------------+
                    |   __start__     |
                    +-----------------+
                            |
                            v
                +-----------------------+
                | detect_source_type    |
                +-----------------------+
                   /    |    |    \
                  /     |    |     \
                 v      v    v      v
           +-------+ +----+ +------+ +------+
           |markdown| |yaml| |github| | bmad |
           +-------+ +----+ +------+ +------+
                 \     |    |     /
                  \    |    |    /
                   v   v    v   v
                +-----------------+
                | configure_engine|
                +-----------------+
                        |
                        v
              +---------------------+
              | detect_test_command |
              +---------------------+
                    /    |    \
                   /     |     \
                  v      v      v
        +-----------+ +-------+ +-----------+
        | sequential| | graph | | parallel  |
        +-----------+ +-------+ +-----------+
                 \       |       /
                  \      |      /
                   v     v     v
              +---------------+
              | collect_results|
              +---------------+
                      |
                      v
              +---------------+
              | report_costs  |
              +---------------+
                      |
                      v
              +-------------------+
              | notify_completion |
              +-------------------+
                      |
                      v
                 +----------+
                 | __end__  |
                 +----------+
```

## Examples

### Example 1: Implement a BMad Story

```bash
tea run examples/workflows/ralphy-loop.yaml \
    --input '{
      "source": "docs/stories/TEA-FEATURE-001.md",
      "engine": "claude",
      "mode": "sequential",
      "run_tests": true,
      "create_prs": true
    }'
```

### Example 2: Process GitHub Issues in Parallel

```bash
tea run examples/workflows/ralphy-loop.yaml \
    --input '{
      "source": "github:myorg/myrepo",
      "engine": "codex",
      "mode": "parallel",
      "max_concurrency": 3,
      "draft_prs": true
    }'
```

### Example 3: Batch Process Stories

```bash
tea run examples/workflows/ralphy-loop.yaml \
    --input '{
      "source": "docs/stories/*.md",
      "engine": "gemini",
      "mode": "parallel",
      "max_concurrency": 2
    }'
```

### Example 4: Custom Test Configuration

```bash
tea run examples/workflows/ralphy-loop.yaml \
    --input '{
      "source": "tasks.yaml",
      "engine": "opencode",
      "test_command": "make test-unit && make test-integration",
      "lint_command": "golangci-lint run"
    }'
```

### Example 5: Graph Mode with Dependencies

```bash
tea run examples/workflows/ralphy-loop.yaml \
    --input '{
      "source": "tasks-with-deps.yaml",
      "engine": "claude",
      "mode": "graph",
      "max_concurrency": 4
    }'
```

Graph mode respects task dependencies, executing tasks in topological order while parallelizing independent tasks.

### Example 6: Parallel Mode with Tmux Output

```bash
# Run inside tmux for best experience
tea run examples/workflows/ralphy-loop.yaml \
    --input '{
      "source": "docs/stories/TEA-001.md",
      "engine": "claude",
      "mode": "parallel",
      "output_to_tmux": true,
      "tmux_session": "ralphy-dev",
      "tmux_layout": "tiled",
      "max_concurrency": 4
    }'
```

This creates 4 tmux panes, each running a task with visible real-time output. Works with any execution mode (sequential, parallel, or graph).

## Troubleshooting

### Engine Not Available

If the workflow reports "Engine CLI not available", install the required CLI:

```bash
# Claude
npm install -g @anthropic/claude-code

# Codex
pip install openai-codex

# Gemini
pip install google-generativeai-cli

# Opencode
pip install opencode

# Cursor
# Requires Cursor IDE installation
```

### Git Worktree Conflicts (Parallel Mode)

If parallel execution fails due to worktree conflicts:

```bash
# Clean up orphaned worktrees
git worktree prune

# List existing worktrees
git worktree list

# Remove specific worktree
git worktree remove .worktrees/task-123
```

### Checkpoint Recovery

If the workflow was interrupted:

```bash
# List available checkpoints
ls -la .ralphy-checkpoints/

# Resume from latest
tea resume --checkpoint .ralphy-checkpoints/latest

# Resume from specific checkpoint
tea resume --checkpoint .ralphy-checkpoints/2024-01-15-123456
```

---

# Ralphy Validate - Story Validation Workflow

The validation workflow (`ralphy-validate.yaml`) validates BMad stories before development, checking structure, acceptance criteria, and task completion.

## Quick Start

```bash
# Validate a single story
tea run examples/workflows/ralphy-validate.yaml \
    --input '{"source": "docs/stories/TEA-001.md"}'

# Validate all stories in an epic
tea run examples/workflows/ralphy-validate.yaml \
    --input '{"source": "docs/stories/TEA-RALPHY-001.*.md"}'

# Strict mode (fail on warnings)
tea run examples/workflows/ralphy-validate.yaml \
    --input '{"source": "docs/stories/TEA-001.md", "strict": true}'

# JSON output
tea run examples/workflows/ralphy-validate.yaml \
    --input '{"source": "docs/stories/TEA-001.md", "output_format": "json"}'
```

## What It Validates

| Check | Type | Description |
|-------|------|-------------|
| Required sections | Error | `## Status`, `## Story`, `## Acceptance Criteria`, `## Tasks / Subtasks` |
| Valid status | Warning | Must be: Draft, Ready, In Progress, Ready for Review, Done, Blocked, Deferred |
| Acceptance criteria | Warning | Must have numbered items (1. 2. 3.) |
| Task-AC linkage | Warning | Tasks should reference ACs with `(AC: 1, 2)` |
| Task completion | Info | Shows completion percentage |

## Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `source` | string | *required* | Story file path or glob pattern |
| `strict` | bool | `false` | Fail validation on warnings |
| `output_format` | string | `text` | Output format: text, json |

## Example Output

```
===============================================================================
                    RALPHY VALIDATE - Story Validation Report
===============================================================================
Stories: 3 | Passed: 2 | Failed: 1
Errors: 1 | Warnings: 3
===============================================================================

[PASS] TEA-RALPHY-001.1.md
  ℹ Status: Done
  ℹ Acceptance Criteria: 7
  ℹ Tasks: 5/5 (100.0%)
  ⚠ Only 3/5 tasks have AC references

[FAIL] TEA-RALPHY-001.2.md
  ℹ Status: Draft
  ✗ Missing required section: ## Acceptance Criteria

[PASS] TEA-RALPHY-001.3.md
  ℹ Status: In Progress
  ℹ Acceptance Criteria: 5
  ℹ Tasks: 3/8 (37.5%)
===============================================================================
```

## Workflow Diagram

```
    +------------------+
    |    __start__     |
    +------------------+
            |
            v
  +---------------------+
  | detect_source_type  |
  +---------------------+
        /       \
       v         v
  +--------+ +-----------+
  |  file  | |   glob    |
  +--------+ +-----------+
       \         /
        v       v
  +------------------+
  | validate_stories |
  +------------------+
            |
            v
  +------------------+
  | generate_summary |
  +------------------+
            |
            v
  +------------------+
  |  output_report   |
  +------------------+
            |
            v
     +----------+
     | __end__  |
     +----------+
```

---

## Related Documentation

- [YAML Reference](../../docs/shared/YAML_REFERENCE.md)
- [Checkpoint Guide](../../docs/shared/architecture/checkpoint-guide.md)
- [Actions Reference (Python)](../../docs/python/actions-reference.md)
- [Experiments Guide](../../docs/python/experiments-guide.md)
