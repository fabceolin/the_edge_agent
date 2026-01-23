# Story TEA-RALPHY-002.2: DOT Stdin Support for Piping

## Status
Done

## Epic Reference
[TEA-RALPHY-002: Minimal Ralphy](./TEA-RALPHY-002-minimal-ralphy.md)

## Story

**As a** workflow developer,
**I want** `tea from dot` to accept DOT content from stdin,
**So that** I can pipe the output of the dependency analyzer directly to execution.

## Acceptance Criteria

1. `tea from dot -` reads DOT content from stdin
2. `tea from dot -` works with all existing flags (`--tmux`, `--use-node-commands`, etc.)
3. Pipeline works: `tea run agent.yaml | tea from dot - --tmux`
4. Helpful error message when stdin is empty or invalid
5. Existing file path behavior is unchanged

## Tasks / Subtasks

- [ ] Modify `python/src/the_edge_agent/cli.py` `from_dot` function (AC: 1, 2, 5)
  - [ ] Check if file argument is `-`
  - [ ] If `-`, read DOT content from `sys.stdin`
  - [ ] Create temporary file or use `parse_dot_string` directly
  - [ ] Pass content through existing pipeline
- [ ] Modify `python/src/the_edge_agent/dot_parser.py` if needed (AC: 1)
  - [ ] Ensure `parse_dot_string` is exposed and works correctly
- [ ] Add error handling for stdin edge cases (AC: 4)
  - [ ] Empty stdin
  - [ ] Invalid DOT content
  - [ ] Non-TTY detection
- [ ] Add unit tests (AC: 1-5)
- [ ] Add integration test for full pipeline (AC: 3)

## Implementation

### CLI Changes

```python
# python/src/the_edge_agent/cli.py

@from_app.command("dot")
def from_dot(
    file: str = typer.Argument(
        ...,
        help="Path to DOT/Graphviz file, or '-' to read from stdin"
    ),
    # ... existing options ...
):
    """
    Convert DOT/Graphviz diagram to TEA YAML workflow.

    Use '-' as the file argument to read DOT content from stdin:

        tea run analyzer.yaml | tea from dot - --use-node-commands --tmux
    """
    from the_edge_agent.dot_parser import (
        dot_to_yaml,
        dot_to_yaml_from_string,  # NEW: Add this function
        DotParseError,
        CircularDependencyError,
    )

    # Handle stdin
    if file == "-":
        import sys

        if sys.stdin.isatty():
            typer.echo("Error: No input from stdin. Pipe DOT content or use a file path.", err=True)
            raise typer.Exit(1)

        dot_content = sys.stdin.read()

        if not dot_content.strip():
            typer.echo("Error: Empty input from stdin.", err=True)
            raise typer.Exit(1)

        try:
            yaml_content = dot_to_yaml_from_string(
                dot_content=dot_content,
                command_template=command or "echo {{ item }}",
                max_concurrency=max_concurrency,
                workflow_name=name,
                use_tmux=tmux,
                tmux_session=session,
                validate=validate_output,
                use_node_commands=use_node_commands,
                allow_cycles=allow_cycles,
                tea_executable=tea_executable,
                subprocess_timeout=timeout,
            )
        except DotParseError as e:
            typer.echo(f"Error parsing DOT from stdin: {e}", err=True)
            raise typer.Exit(1)

    else:
        # Existing file-based logic
        file_path = Path(file)
        if not file_path.exists():
            typer.echo(f"Error: DOT file not found: {file}", err=True)
            raise typer.Exit(1)

        # ... rest of existing implementation ...
```

### DOT Parser Changes

```python
# python/src/the_edge_agent/dot_parser.py

def dot_to_yaml_from_string(
    dot_content: str,
    command_template: str,
    output_path: Optional[str] = None,
    max_concurrency: int = 3,
    workflow_name: Optional[str] = None,
    use_tmux: bool = False,
    tmux_session: Optional[str] = None,
    validate: bool = False,
    use_node_commands: bool = False,
    allow_cycles: bool = False,
    tea_executable: Optional[str] = None,
    subprocess_timeout: int = DEFAULT_SUBPROCESS_TIMEOUT,
) -> str:
    """
    Convert DOT content string to TEA YAML workflow.

    Same as dot_to_yaml but accepts string content instead of file path.

    Args:
        dot_content: DOT format string
        ... (same as dot_to_yaml)

    Returns:
        Generated YAML string
    """
    # Parse DOT string
    parsed = parse_dot_string(dot_content, default_name=workflow_name or "stdin-workflow")

    # Analyze graph structure
    analyzed = analyze_graph(parsed, allow_cycles=allow_cycles)

    # Validate commands if needed
    if use_node_commands:
        all_labels = set()
        for phase in analyzed.phases:
            all_labels.update(phase.items)
        for node_id in analyzed.standalone_nodes:
            node = parsed.nodes.get(node_id)
            if node and node.shape in ("ellipse", "circle", "point", "doublecircle"):
                continue
            all_labels.add(analyzed.node_labels.get(node_id, node_id))

        missing = all_labels - set(analyzed.node_commands.keys())
        if missing:
            missing_list = ", ".join(sorted(missing))
            raise ValueError(
                f"--use-node-commands requires ALL nodes to have command attribute. "
                f"Missing commands for: {missing_list}"
            )

    # Generate YAML
    yaml_content = generate_yaml(
        analyzed=analyzed,
        command_template=command_template,
        max_concurrency=max_concurrency,
        workflow_name=workflow_name,
        use_tmux=use_tmux,
        tmux_session=tmux_session,
        use_node_commands=use_node_commands,
        tea_executable=tea_executable,
        subprocess_timeout=subprocess_timeout,
    )

    # Optionally validate
    if validate:
        _validate_yaml(yaml_content)

    # Optionally write to file
    if output_path:
        Path(output_path).write_text(yaml_content)

    return yaml_content
```

## Testing

**Test Location:** `python/tests/test_dot_stdin.py`

```python
import pytest
import subprocess
import tempfile
from pathlib import Path

class TestDotStdin:
    """Tests for stdin support in tea from dot."""

    def test_stdin_basic(self):
        """Test reading DOT from stdin."""
        dot_content = '''
        digraph test {
            "A" [command="echo A"];
            "B" [command="echo B"];
            "A" -> "B";
        }
        '''

        result = subprocess.run(
            ["tea", "from", "dot", "-"],
            input=dot_content,
            capture_output=True,
            text=True,
        )

        assert result.returncode == 0
        assert "name:" in result.stdout
        assert "nodes:" in result.stdout

    def test_stdin_with_tmux_flag(self):
        """Test stdin works with --tmux flag."""
        dot_content = '''
        digraph test {
            "A" [command="echo A"];
        }
        '''

        result = subprocess.run(
            ["tea", "from", "dot", "-", "--tmux", "-s", "test-session"],
            input=dot_content,
            capture_output=True,
            text=True,
        )

        assert result.returncode == 0
        assert "tmux" in result.stdout.lower()

    def test_stdin_empty_error(self):
        """Test error when stdin is empty."""
        result = subprocess.run(
            ["tea", "from", "dot", "-"],
            input="",
            capture_output=True,
            text=True,
        )

        assert result.returncode != 0
        assert "empty" in result.stderr.lower()

    def test_stdin_invalid_dot_error(self):
        """Test error when stdin contains invalid DOT."""
        result = subprocess.run(
            ["tea", "from", "dot", "-"],
            input="this is not valid DOT",
            capture_output=True,
            text=True,
        )

        assert result.returncode != 0
        assert "error" in result.stderr.lower()

    def test_pipeline_integration(self):
        """Test full pipeline with agent output piped to from dot."""
        # This would require the dependency-analyzer agent to be installed
        # For unit testing, we can mock the first command

        dot_content = '''
        digraph workflow {
            subgraph cluster_phase_1 {
                "task1" [command="echo task1"];
                "task2" [command="echo task2"];
            }
        }
        '''

        # Simulate: echo "$dot_content" | tea from dot -
        result = subprocess.run(
            ["tea", "from", "dot", "-", "--use-node-commands"],
            input=dot_content,
            capture_output=True,
            text=True,
        )

        assert result.returncode == 0
        assert "dynamic_parallel" in result.stdout or "task1" in result.stdout

    def test_file_path_still_works(self):
        """Test that file path argument still works."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.dot', delete=False) as f:
            f.write('''
            digraph test {
                "A" [command="echo A"];
            }
            ''')
            f.flush()

            result = subprocess.run(
                ["tea", "from", "dot", f.name],
                capture_output=True,
                text=True,
            )

            assert result.returncode == 0
            Path(f.name).unlink()
```

### Test Cases

| Test Case | Description | AC |
|-----------|-------------|-----|
| test_stdin_basic | `-` reads from stdin | 1 |
| test_stdin_with_tmux_flag | Works with `--tmux` | 2 |
| test_stdin_with_use_node_commands | Works with `--use-node-commands` | 2 |
| test_pipeline_integration | Full pipeline works | 3 |
| test_stdin_empty_error | Error on empty stdin | 4 |
| test_stdin_invalid_dot_error | Error on invalid DOT | 4 |
| test_file_path_still_works | File paths unchanged | 5 |

## Dev Notes

### Type Annotation Change

The `file` parameter type needs to change from `Path` to `str` to support `-`:

```python
# Before
file: Path = typer.Argument(...)

# After
file: str = typer.Argument(..., help="Path to DOT file, or '-' for stdin")
```

Then convert to Path only when not `-`:
```python
if file != "-":
    file_path = Path(file)
```

### TTY Detection

When stdin is a TTY (interactive terminal), there's no piped input:

```python
import sys
if sys.stdin.isatty():
    typer.echo("Error: No input from stdin...", err=True)
```

### parse_dot_string Already Exists

The `parse_dot_string` function already exists in `dot_parser.py:119`. We just need to add a `dot_to_yaml_from_string` wrapper or modify `dot_to_yaml` to accept either path or content.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-22 | 0.1 | Initial story creation | Winston (Architect) |

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
