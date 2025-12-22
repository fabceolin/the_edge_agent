# TEA-CLI-005c: Interactive Mode - Python Port & Parity Tests

## Status

**Approved**

## Story

**As a** workflow author who may use either Python or Rust CLI,
**I want** the Python CLI to have identical `--interactive` mode behavior,
**So that** I can switch between runtimes without relearning the interactive workflow.

## Story Context

### Existing System Integration

| Aspect | Details |
|--------|---------|
| **Integrates with** | Python `tea run` command, existing `YAMLEngine` |
| **Technology** | Python (typer), reference: `run_standalone.py` |
| **Follows pattern** | Port Rust implementation from TEA-CLI-005a/b |
| **Touch points** | `python/src/the_edge_agent/cli.py` |

### Depends On

- **TEA-CLI-005a**: Rust core (defines the spec)
- **TEA-CLI-005b**: Rust UX (defines display format)

### Reference

The existing `firebase/functions-agents/run_standalone.py` already has much of this logic. This story ports the Rust-defined interface to Python while leveraging existing Python patterns.

## Acceptance Criteria

### Python CLI Flags (Matching Rust)

- [ ] **AC-1**: `--interactive` / `-I` flag on `tea run` command
- [ ] **AC-2**: `--question-key KEY` (default: `question,prompt,message,ask,next_question`)
- [ ] **AC-3**: `--response-key KEY` (default: `response`)
- [ ] **AC-4**: `--complete-key KEY` (default: `complete,done,finished`)
- [ ] **AC-5**: `--skip-response TEXT` (default: "I don't have information about this.")
- [ ] **AC-6**: `--display-key KEY` (optional, comma-separated)
- [ ] **AC-7**: `--display-format FORMAT` (`pretty`, `json`, `raw`)
- [ ] **AC-8**: `--input-timeout SECONDS` (optional)

### Behavior Parity

- [ ] **AC-9**: Same double-enter input behavior as Rust
- [ ] **AC-10**: Same welcome banner format as Rust
- [ ] **AC-11**: Same question display format as Rust
- [ ] **AC-12**: Same `quit`/`exit`/`skip` command handling
- [ ] **AC-13**: Same Ctrl+C behavior (save checkpoint, exit gracefully)
- [ ] **AC-14**: Same checkpoint auto-management

### Parity Tests

- [ ] **AC-15**: Shared test workflow runs identically on both CLIs
- [ ] **AC-16**: Same workflow produces same question sequence
- [ ] **AC-17**: Same input produces same state mutations
- [ ] **AC-18**: `--help` output structure matches between CLIs

## Technical Notes

### Python CLI Arguments (Typer)

```python
@app.command()
def run(
    file: Path = typer.Argument(..., help="Path to workflow YAML file"),
    # ... existing args ...

    # Interactive mode
    interactive: bool = typer.Option(
        False, "--interactive", "-I",
        help="Enable interactive human-in-the-loop mode"
    ),
    question_key: str = typer.Option(
        "question,prompt,message,ask,next_question",
        "--question-key",
        help="State key(s) for question extraction (comma-separated)"
    ),
    response_key: str = typer.Option(
        "response",
        "--response-key",
        help="State key to inject user response"
    ),
    complete_key: str = typer.Option(
        "complete,done,finished",
        "--complete-key",
        help="State key(s) that signal completion (comma-separated)"
    ),
    skip_response: str = typer.Option(
        "I don't have information about this.",
        "--skip-response",
        help="Response to use when user types 'skip'"
    ),
    display_key: Optional[str] = typer.Option(
        None,
        "--display-key",
        help="State key(s) to display (comma-separated)"
    ),
    display_format: str = typer.Option(
        "pretty",
        "--display-format",
        help="Display format: pretty, json, raw"
    ),
    input_timeout: Optional[int] = typer.Option(
        None,
        "--input-timeout",
        help="Input timeout in seconds (for testing)"
    ),
):
```

### Leverage Existing Code

The `run_standalone.py` has `InteractiveAgentRunner` and `InteractiveCLI` classes. Extract and adapt:

```python
# python/src/the_edge_agent/interactive.py

class InteractiveRunner:
    """Manages interactive execution loop."""

    def __init__(
        self,
        engine: YAMLEngine,
        graph,
        question_keys: list[str],
        response_key: str,
        complete_keys: list[str],
        skip_response: str,
        display_keys: list[str] | None,
        display_format: str,
        checkpoint_dir: Path,
    ):
        self.engine = engine
        self.graph = graph
        self.question_keys = question_keys
        self.response_key = response_key
        self.complete_keys = complete_keys
        self.skip_response = skip_response
        self.display_keys = display_keys
        self.display_format = display_format
        self.checkpoint_dir = checkpoint_dir

    def extract_question(self, state: dict) -> str | None:
        for key in self.question_keys:
            if key in state and state[key]:
                return str(state[key])
        return None

    def is_complete(self, state: dict) -> bool:
        return any(state.get(key) for key in self.complete_keys)

    def inject_response(self, state: dict, response: str) -> dict:
        return {**state, self.response_key: response}

    def run(self, initial_state: dict) -> dict:
        """Run interactive loop, return final state."""
        # Implementation mirrors Rust
        pass
```

### Double-Enter Input (Python)

```python
def read_multiline_input(timeout: int | None = None) -> str | None:
    """Read input until double-enter. Returns None if quit/exit."""
    import sys
    import select

    lines = []
    print("> ", end="", flush=True)

    while True:
        if timeout:
            ready, _, _ = select.select([sys.stdin], [], [], timeout)
            if not ready:
                raise TimeoutError("Input timeout")

        line = sys.stdin.readline()
        if not line:  # EOF
            return None

        line = line.rstrip('\n')

        # Check for quit
        if line.lower() in ('quit', 'exit', 'q'):
            return None

        # Check for double-enter
        if line == '' and lines and lines[-1] == '':
            lines.pop()
            break

        lines.append(line)
        print("> ", end="", flush=True)

    return '\n'.join(lines).strip()
```

### Parity Test Structure

```bash
# tests/cli_parity/test_interactive_parity.sh

#!/bin/bash
set -e

WORKFLOW="examples/parity-test-interactive.yaml"
INPUT='{"test_id": "parity-001"}'

# Run same workflow on both CLIs with scripted input
echo -e "test answer\n\nquit\n" | tea run "$WORKFLOW" --interactive --input "$INPUT" --impl > /tmp/rust_output.txt
echo -e "test answer\n\nquit\n" | python -m the_edge_agent run "$WORKFLOW" --interactive --input "$INPUT" --impl > /tmp/python_output.txt

# Compare outputs (ignoring timestamps)
diff <(grep -v "timestamp" /tmp/rust_output.txt) <(grep -v "timestamp" /tmp/python_output.txt)

echo "Parity test passed!"
```

```yaml
# examples/parity-test-interactive.yaml
name: parity-test-interactive
description: Test workflow for CLI parity testing

state_schema:
  test_id: str
  question: str
  response: str
  complete: bool

nodes:
  - name: ask
    run: |
      return {question = "What is your test input?"}
    interrupt: after

  - name: process
    run: |
      return {
        complete = true,
        result = "Received: " .. (state.response or "nothing")
      }

edges:
  - from: __start__
    to: ask
  - from: ask
    to: process
  - from: process
    to: __end__
```

## Definition of Done

- [ ] All `--interactive` flags available in Python CLI
- [ ] Python interactive loop works identically to Rust
- [ ] Same display format and UX
- [ ] Parity tests pass
- [ ] `--help` output matches Rust structure
- [ ] Unit tests for Python interactive functions
- [ ] Integration tests for Python interactive mode

## Tasks / Subtasks

### Phase 1: Python CLI Flags

- [ ] **Task 1**: Add `--interactive` and related flags to Python `run` command (AC-1 through AC-8)
- [ ] **Task 2**: Add mutual exclusivity with `--stream`

### Phase 2: Core Implementation

- [ ] **Task 3**: Create `interactive.py` module with `InteractiveRunner` class
- [ ] **Task 4**: Implement `read_multiline_input()` with double-enter (AC-9)
- [ ] **Task 5**: Implement `extract_question()`, `is_complete()`, `inject_response()`
- [ ] **Task 6**: Implement main interactive loop (AC-14)
- [ ] **Task 7**: Integrate with `cli.py` run command

### Phase 3: UX Parity

- [ ] **Task 8**: Implement `display_welcome()` matching Rust format (AC-10)
- [ ] **Task 9**: Implement `display_question()` matching Rust format (AC-11)
- [ ] **Task 10**: Implement `quit`/`exit`/`skip` handling (AC-12)
- [ ] **Task 11**: Implement Ctrl+C signal handling (AC-13)
- [ ] **Task 12**: Implement `--display-key` and `--display-format`
- [ ] **Task 13**: Implement `--input-timeout`

### Phase 4: Parity Tests

- [ ] **Task 14**: Create `parity-test-interactive.yaml` workflow (AC-15)
- [ ] **Task 15**: Create parity test script (AC-16, AC-17)
- [ ] **Task 16**: Verify `--help` output parity (AC-18)
- [ ] **Task 17**: Add to CI pipeline

### Phase 5: Documentation

- [ ] **Task 18**: Update Python CLI `--help` strings
- [ ] **Task 19**: Update README with interactive mode examples
- [ ] **Task 20**: Document any Python-specific behaviors

## Dev Notes

### Testing Commands

```bash
# Python interactive mode
cd python && pip install -e .
tea run ../examples/interview.yaml --interactive

# Parity test
./tests/cli_parity/test_interactive_parity.sh
```

### Key Differences to Watch

| Aspect | Rust | Python | Resolution |
|--------|------|--------|------------|
| Signal handling | `ctrlc` crate | `signal` module | Both save checkpoint |
| Timeout | `mpsc::recv_timeout` | `select.select` | Both raise timeout error |
| Default checkpoint dir | `/tmp/tea_checkpoints` | Same | Match exactly |

## Related Stories

- **TEA-CLI-005a**: Rust core (defines behavior)
- **TEA-CLI-005b**: Rust UX (defines display format)
- **TEA-CLI-004**: CLI Parity Alignment (parent initiative)

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2025-12-21 | 1.0 | Split from TEA-CLI-005, Python port + parity | Sarah (PO Agent) |
