# Story TEA-RALPHY-002.3: Custom Executable Parameter for DOT Workflow Execution

## Status
Done

## Epic Reference
[TEA-RALPHY-002: Minimal Ralphy](./TEA-RALPHY-002-minimal-ralphy.md)

## Story

**As a** workflow developer,
**I want** to specify a custom executable for running workflows in DOT orchestration mode,
**So that** I can use alternative TEA installations, virtual environments, or custom runners.

## Acceptance Criteria

1. `tea-python run-from-dot` accepts `--exec`/`-e` parameter to specify workflow executable
2. `tea-python run --from-dot` accepts `--dot-exec` parameter to specify workflow executable
3. Default executable is `tea-python` when parameter is not provided
4. Custom executables like `python -m the_edge_agent` or `/path/to/tea` work correctly
5. Documentation is updated in `DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md`

## Tasks / Subtasks

- [x] Add `--exec`/`-e` parameter to `run-from-dot` command in `cli.py` (AC: 1)
- [x] Add `--dot-exec` parameter to `run --from-dot` option in `cli.py` (AC: 2)
- [x] Modify command construction to use the executable variable (AC: 3, 4)
- [x] Update `DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md` with new parameters (AC: 5)

## Implementation

### CLI Changes

```python
# python/src/the_edge_agent/cli.py

# For run-from-dot command (around line 2018)
@app.command("run-from-dot")
def run_from_dot(
    dot_file: str = typer.Argument(..., help="Path to DOT file"),
    workflow: str = typer.Option(..., "--workflow", "-w", help="Workflow YAML to run for each node"),
    max_parallel: int = typer.Option(3, "--max-parallel", "-m", help="Max parallel processes"),
    executable: str = typer.Option(
        "tea-python",
        "--exec",
        "-e",
        help="Executable to use for running workflows (default: tea-python). "
        "Examples: 'tea-python', 'python -m the_edge_agent', '/path/to/tea'",
    ),
    # ... other options
):
    # Command construction uses executable variable
    cmd = f"{executable} run {workflow} --input '{input_json}'"
```

```python
# For run --from-dot option (around line 573)
@app.command("run")
def run(
    workflow_file: str = typer.Argument(...),
    from_dot: Optional[str] = typer.Option(None, "--from-dot", help="Execute from DOT file"),
    dot_workflow: Optional[str] = typer.Option(None, "--dot-workflow", help="Workflow for --from-dot mode"),
    dot_max_parallel: int = typer.Option(3, "--dot-max-parallel", help="Max parallel for --from-dot"),
    dot_exec: str = typer.Option(
        "tea-python",
        "--dot-exec",
        help="Executable to use for running workflows in --from-dot mode (default: tea-python). "
        "Examples: 'tea-python', 'python -m the_edge_agent', '/path/to/tea'",
    ),
    # ... other options
):
    # When from_dot is specified, command construction uses dot_exec variable
    cmd = f"{dot_exec} run {dot_workflow} --input '{input_json}'"
```

## Usage Examples

### Using Default Executable

```bash
# Default uses tea-python
tea-python run-from-dot workflow.dot --workflow bmad-story-validation.yaml
```

### Using Custom Executable

```bash
# Use python module directly
tea-python run-from-dot workflow.dot --workflow bmad-story-validation.yaml \
  --exec "python -m the_edge_agent"

# Use absolute path
tea-python run-from-dot workflow.dot --workflow bmad-story-validation.yaml \
  --exec "/home/user/.venv/bin/tea-python"

# Use via run --from-dot
tea-python run agent.yaml --from-dot workflow.dot --dot-workflow bmad.yaml \
  --dot-exec "python -m the_edge_agent"
```

## Parameter Mapping

| `run-from-dot` | `run --from-dot` | Purpose |
|----------------|------------------|---------|
| `--workflow`/`-w` | `--dot-workflow` | Workflow YAML file |
| `--max-parallel`/`-m` | `--dot-max-parallel` | Max parallel processes |
| `--exec`/`-e` | `--dot-exec` | Workflow executable |

## Testing

### Manual Test Cases

| Test Case | Command | Expected Result |
|-----------|---------|-----------------|
| Default executable | `tea-python run-from-dot test.dot -w workflow.yaml` | Uses `tea-python` |
| Custom executable | `tea-python run-from-dot test.dot -w workflow.yaml -e "python -m the_edge_agent"` | Uses custom command |
| Absolute path | `tea-python run-from-dot test.dot -w workflow.yaml -e "/path/to/tea"` | Uses absolute path |
| Via run command | `tea-python run x.yaml --from-dot test.dot --dot-exec "tea"` | Uses `tea` executable |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-24 | 0.1 | Initial story creation from implemented feature | Claude |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Completion Notes List

- Added `--exec`/`-e` parameter to `run-from-dot` command
- Added `--dot-exec` parameter to `run --from-dot` option
- Updated command construction in both code paths to use the executable variable
- Updated `DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md` with new parameters

### File List

- `python/src/the_edge_agent/cli.py` - Added executable parameters
- `docs/llm-prompts/DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md` - Updated documentation
- `docs/stories/TEA-RALPHY-002.3.dot-exec-parameter.md` - This story

---

## QA Results

Feature tested manually during DOT workflow execution with 46 stories.
