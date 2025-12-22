# TEA-CLI-005: Interactive Human-in-the-Loop Mode (Epic)

## Status

**Approved** - Split into sub-stories

## Child Stories

| ID | Title | Effort | Status |
|----|-------|--------|--------|
| **TEA-CLI-005a** | [Interactive Mode - Rust Core](TEA-CLI-005a-interactive-rust-core.md) | ~4h | Approved |
| **TEA-CLI-005b** | [Interactive Mode - Rust UX & Error Handling](TEA-CLI-005b-interactive-rust-ux.md) | ~2h | Approved |
| **TEA-CLI-005c** | [Interactive Mode - Python Port & Parity](TEA-CLI-005c-interactive-python-parity.md) | ~3h | Approved |

## Execution Order

```
TEA-CLI-005a (Rust Core)
       │
       ▼
TEA-CLI-005b (Rust UX)
       │
       ▼
TEA-CLI-005c (Python + Parity)
```

---

## Story

**As a** workflow author running agents with human-in-the-loop interrupts,
**I want** an `--interactive` mode that automatically manages checkpoints and prompts me for input,
**So that** I can have a seamless Q&A session without manually running `tea resume` after each interrupt.

## Story Context

### Existing System Integration

| Aspect | Details |
|--------|---------|
| **Integrates with** | `tea run` and `tea resume` subcommands, `Checkpointer` trait, interrupt handling |
| **Technology** | Rust (clap, serde_json), Python (typer) |
| **Follows pattern** | Existing `--stream` and `--auto-continue` flag patterns |
| **Touch points** | `run_workflow()`, `resume_workflow()`, `ExecutionOptions`, Python `cli.py` |

### Reference Implementation

The Python `run_standalone.py` (in `firebase/functions-agents/`) provides a working reference:

```python
class InteractiveAgentRunner:
    # Configurable state key mappings
    question_keys = ["next_question", "question", "prompt", "message", "ask"]
    response_key = "response"
    complete_keys = ["interview_complete", "complete", "done", "finished"]

    def run_step(self, state, resume_from=None):
        # Merges checkpoint state with new input and invokes graph

    def find_latest_checkpoint(self):
        # Finds most recent .pkl file in checkpoint dir

    def get_question_from_state(self, state):
        # Extracts question using configurable keys
```

### Current Behavior vs Desired Behavior

| Aspect | Current (without `--interactive`) | Desired (with `--interactive`) |
|--------|-----------------------------------|-------------------------------|
| On interrupt | Exits with code 130 | Displays question, waits for input |
| Checkpoint management | User must manually specify | Automatic (finds latest) |
| Resume | Requires `tea resume <file>` | Automatic loop |
| Multi-line input | N/A | Double-enter to send |
| Session end | Single execution | Loops until completion or user quits |

## Acceptance Criteria

### Core Interactive Mode

- [ ] **AC-1**: `--interactive` / `-I` flag available on `tea run` command (both Rust and Python)
- [ ] **AC-2**: When interrupt occurs, CLI displays extracted question and waits for user input
- [ ] **AC-3**: User input is injected into state and execution resumes automatically
- [ ] **AC-4**: Loop continues until workflow reaches `__end__` or user types `quit`/`exit`
- [ ] **AC-5**: Double-enter (empty line) sends the current input (enables multi-line responses)
- [ ] **AC-6**: Checkpoint files are managed automatically (created, loaded, cleaned on completion)

### Configurable Q&A Keys

- [ ] **AC-7**: `--question-key KEY` flag to specify state key(s) for question extraction (comma-separated, default: `question,prompt,message,ask,next_question`)
- [ ] **AC-8**: `--response-key KEY` flag to specify state key for user response injection (default: `response`)
- [ ] **AC-9**: `--complete-key KEY` flag to specify state key(s) that signal completion (comma-separated, default: `complete,done,finished`)

### Output Formatting

- [ ] **AC-10**: `--display-key KEY` flag to filter which state keys are shown to user (comma-separated)
- [ ] **AC-11**: `--display-format FORMAT` flag with options: `pretty` (default), `json`, `raw`
- [ ] **AC-12**: When no `--display-key` is specified, show only the extracted question
- [ ] **AC-13**: After each step, display relevant state changes (respecting `--display-key` filter)

### User Experience

- [ ] **AC-14**: Display welcome banner with workflow name and available commands (`quit`, `skip`, etc.)
- [ ] **AC-15**: Show iteration/round number for each Q&A cycle
- [ ] **AC-16**: `skip` command sends a default skip response (configurable via `--skip-response`)
- [ ] **AC-17**: Clear visual separation between question display and input prompt
- [ ] **AC-18**: On completion, display final state summary (respecting `--display-key` filter)

### Error Handling

- [ ] **AC-19**: Graceful handling of Ctrl+C (save checkpoint before exit)
- [ ] **AC-20**: Timeout option `--input-timeout SECONDS` for automated testing (default: no timeout)
- [ ] **AC-21**: If question key not found in state, display warning and show available keys

### Parity

- [ ] **AC-22**: Rust and Python CLIs have identical `--interactive` behavior
- [ ] **AC-23**: Same default key mappings in both implementations
- [ ] **AC-24**: Same output format for questions and state display

## Technical Notes

### Integration Approach

The `--interactive` mode wraps the existing run/resume flow in a loop:

```
┌─────────────────────────────────────────────────────┐
│  tea run workflow.yaml --interactive                │
└─────────────────────────────────────────────────────┘
                      │
                      ▼
         ┌────────────────────────┐
         │  Initial Execution     │
         │  (invoke with state)   │
         └────────────────────────┘
                      │
           ┌──────────┴──────────┐
           │                     │
           ▼                     ▼
    ┌─────────────┐       ┌─────────────┐
    │  Complete   │       │  Interrupt  │
    │  (__end__)  │       │  (pause)    │
    └─────────────┘       └─────────────┘
           │                     │
           ▼                     ▼
    ┌─────────────┐       ┌─────────────────────┐
    │  Display    │       │  Save Checkpoint    │
    │  Final      │       │  Extract Question   │
    │  Summary    │       │  Display to User    │
    └─────────────┘       └─────────────────────┘
           │                     │
           ▼                     ▼
         Exit              ┌─────────────────────┐
                           │  Wait for Input     │
                           │  (double-enter)     │
                           └─────────────────────┘
                                  │
                                  ▼
                           ┌─────────────────────┐
                           │  Inject Response    │
                           │  Resume Execution   │
                           └─────────────────────┘
                                  │
                                  └───────► (loop back to execution)
```

### Rust Implementation Sketch

```rust
// In Commands::Run
/// Enable interactive human-in-the-loop mode
#[arg(short = 'I', long)]
interactive: bool,

/// State key(s) to extract question from (comma-separated)
#[arg(long, default_value = "question,prompt,message,ask,next_question")]
question_key: String,

/// State key to inject user response into
#[arg(long, default_value = "response")]
response_key: String,

/// State key(s) that signal completion (comma-separated)
#[arg(long, default_value = "complete,done,finished")]
complete_key: String,

/// State key(s) to display to user (comma-separated, default: question only)
#[arg(long)]
display_key: Option<String>,

/// Display format: pretty, json, raw
#[arg(long, default_value = "pretty")]
display_format: String,

/// Response to use when user types 'skip'
#[arg(long, default_value = "I don't have information about this.")]
skip_response: String,

/// Input timeout in seconds (for automated testing)
#[arg(long)]
input_timeout: Option<u64>,
```

### Input Handling (Double-Enter)

```rust
fn read_multiline_input() -> Result<String> {
    let mut lines = Vec::new();
    let stdin = std::io::stdin();

    loop {
        let mut line = String::new();
        stdin.read_line(&mut line)?;

        if line.trim().is_empty() && lines.last().map(|l: &String| l.trim().is_empty()).unwrap_or(false) {
            // Double empty line - done
            lines.pop(); // Remove the first empty line
            break;
        }
        lines.push(line);
    }

    Ok(lines.join("").trim().to_string())
}
```

### Key Constraints

1. **Checkpoint Directory**: Must be specified or default to temp dir
2. **State Immutability**: Response injection creates new state, doesn't mutate
3. **Signal Handling**: SIGINT must save checkpoint before exit
4. **Streaming Compatibility**: `--interactive` and `--stream` are mutually exclusive

## Definition of Done

- [ ] Functional requirements met (AC-1 through AC-21)
- [ ] Integration requirements verified (works with existing checkpointer)
- [ ] Existing functionality regression tested (`tea run` without `--interactive` unchanged)
- [ ] Code follows existing patterns and standards (clap args, error handling)
- [ ] Tests pass (existing and new)
- [ ] Documentation updated (README, `--help` output)
- [ ] Rust and Python implementations have identical behavior (AC-22, AC-23, AC-24)

## Tasks / Subtasks

### Phase 1: Rust Core Implementation

- [ ] **Task 1**: Add `--interactive` flag and related arguments to `Commands::Run` (AC-1, AC-7-12)
- [ ] **Task 2**: Implement `read_multiline_input()` with double-enter detection (AC-5)
- [ ] **Task 3**: Implement `extract_question_from_state()` with configurable keys (AC-2, AC-7)
- [ ] **Task 4**: Implement `inject_response_to_state()` (AC-3, AC-8)
- [ ] **Task 5**: Implement interactive loop in `run_workflow()` (AC-4, AC-6)
- [ ] **Task 6**: Add completion detection using `--complete-key` (AC-9)

### Phase 2: Rust UX Polish

- [ ] **Task 7**: Implement welcome banner and command display (AC-14)
- [ ] **Task 8**: Implement iteration counter display (AC-15)
- [ ] **Task 9**: Implement `skip` command handling (AC-16)
- [ ] **Task 10**: Implement visual separators and formatting (AC-17)
- [ ] **Task 11**: Implement final state summary display (AC-18)
- [ ] **Task 12**: Implement `--display-key` filtering (AC-10, AC-12, AC-13)
- [ ] **Task 13**: Implement `--display-format` options (AC-11)

### Phase 3: Rust Error Handling

- [ ] **Task 14**: Implement Ctrl+C signal handling with checkpoint save (AC-19)
- [ ] **Task 15**: Implement `--input-timeout` for testing (AC-20)
- [ ] **Task 16**: Implement missing key warning with available keys display (AC-21)

### Phase 4: Python Implementation

- [ ] **Task 17**: Port `--interactive` flag and arguments to Python CLI (AC-1, AC-7-12)
- [ ] **Task 18**: Port multiline input handling (AC-5)
- [ ] **Task 19**: Port interactive loop logic (AC-2-6)
- [ ] **Task 20**: Port UX features (AC-14-18)
- [ ] **Task 21**: Port error handling (AC-19-21)

### Phase 5: Testing & Documentation

- [ ] **Task 22**: Add Rust integration tests for interactive mode
- [ ] **Task 23**: Add Python integration tests for interactive mode
- [ ] **Task 24**: Add parity tests (same workflow, same behavior) (AC-22-24)
- [ ] **Task 25**: Update README with interactive mode examples
- [ ] **Task 26**: Ensure `--help` output is consistent between Rust and Python

## Dev Notes

### Testing

| Test Type | Location | Description |
|-----------|----------|-------------|
| Unit (Rust) | `rust/tests/test_cli_interactive.rs` | Input parsing, key extraction |
| Unit (Python) | `python/tests/test_cli_interactive.py` | Input parsing, key extraction |
| Integration | `tests/cli_interactive/` | End-to-end interactive sessions |
| Parity | `tests/cli_parity/test_interactive.sh` | Same inputs, same outputs |

### Example Usage

```bash
# Basic interactive mode
tea run interview.yaml --interactive

# With custom keys
tea run agent.yaml --interactive \
  --question-key "next_question,prompt" \
  --response-key "user_response" \
  --complete-key "interview_complete"

# With output filtering (only show question and options)
tea run agent.yaml --interactive \
  --display-key "question,options,context"

# With JSON output format
tea run agent.yaml --interactive --display-format json

# Initial state + interactive
tea run agent.yaml --interactive --input '{"user_id": "123"}'

# With timeout for CI/testing
echo -e "test answer\n\n" | tea run agent.yaml --interactive --input-timeout 5
```

### Sample Session Output

```
============================================================
  TEA - Interview Agent (Interactive Mode)
============================================================

Commands during interaction:
  - Type your answer and press Enter twice to send
  - Type 'quit' or 'exit' to end early
  - Type 'skip' to skip a question
------------------------------------------------------------

 Starting with state:
   user_id: 123
   matter_id: RUN-20251221-143022

 Processing...

------------------------------------------------------------
 Question (1/?)
------------------------------------------------------------

What is the primary nature of this legal matter?
Please describe the key facts and parties involved.

------------------------------------------------------------
Your answer (Enter twice to send):
> This is a corporate M&A matter involving Acme Corp.
> The deal is worth approximately $50M.
>
>

 Processing...

------------------------------------------------------------
 Question (2/?)
------------------------------------------------------------

What jurisdictions are involved in this matter?

------------------------------------------------------------
Your answer (Enter twice to send):
> quit

 Session ended by user.
 Checkpoint saved: /tmp/tea_checkpoints/interview_1234.bin
```

## Risk and Compatibility Check

### Minimal Risk Assessment

| Risk | Mitigation | Rollback |
|------|------------|----------|
| **Interactive mode breaks existing CLI** | `--interactive` is opt-in, default behavior unchanged | Remove flag, revert to 130 exit |
| **Checkpoint corruption on Ctrl+C** | Signal handler saves before exit | Manual checkpoint file deletion |
| **Parity drift between Rust/Python** | Shared test suite validates identical behavior | Fix implementation to match spec |

### Compatibility Verification

- [x] No breaking changes to existing APIs (new flags only)
- [x] Database changes: N/A
- [x] UI changes: N/A (CLI only)
- [x] Performance impact: Negligible (interactive is user-bound)

## Related Stories

- **TEA-CLI-004**: CLI Parity Alignment (parent story, provides foundation)
- **TEA-RUST-032**: Executor checkpoint wiring (prerequisite)
- **TEA-PY-003**: While loop node (related interrupt patterns)

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2025-12-21 | 1.0 | Initial story creation | Sarah (PO Agent) |
