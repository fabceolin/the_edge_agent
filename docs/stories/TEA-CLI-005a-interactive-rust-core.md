# TEA-CLI-005a: Interactive Mode - Rust Core Implementation

## Status

**Approved**

## Story

**As a** workflow author running agents with human-in-the-loop interrupts,
**I want** an `--interactive` mode in the Rust CLI that loops on interrupts and prompts for input,
**So that** I can have a seamless Q&A session without manually running `tea resume` after each interrupt.

## Story Context

### Existing System Integration

| Aspect | Details |
|--------|---------|
| **Integrates with** | `tea run` subcommand, `Checkpointer` trait, interrupt handling |
| **Technology** | Rust (clap, serde_json, std::io) |
| **Follows pattern** | Existing `--stream` and `--auto-continue` flag patterns |
| **Touch points** | `run_workflow()` in `rust/src/bin/tea.rs` |

### Reference Implementation

See `firebase/functions-agents/run_standalone.py` for Python reference.

## Acceptance Criteria

### Core Interactive Loop

- [ ] **AC-1**: `--interactive` / `-I` flag available on `tea run` command
- [ ] **AC-2**: When interrupt occurs, CLI extracts question from state and displays it
- [ ] **AC-3**: CLI waits for user input with double-enter (empty line) to send
- [ ] **AC-4**: User input is injected into state under configurable key and execution resumes
- [ ] **AC-5**: Loop continues until workflow reaches `__end__` or completion key is true
- [ ] **AC-6**: Checkpoint files are managed automatically (default: `/tmp/tea_checkpoints/`)

### Configurable Keys

- [ ] **AC-7**: `--question-key KEY` flag (comma-separated, default: `question,prompt,message,ask,next_question`)
- [ ] **AC-8**: `--response-key KEY` flag (default: `response`)
- [ ] **AC-9**: `--complete-key KEY` flag (comma-separated, default: `complete,done,finished`)

### Basic Commands

- [ ] **AC-10**: User can type `quit` or `exit` to end session (saves checkpoint)
- [ ] **AC-11**: User can type `skip` to send default skip response
- [ ] **AC-12**: `--skip-response TEXT` flag to customize skip response

### Mutual Exclusivity

- [ ] **AC-13**: `--interactive` and `--stream` are mutually exclusive (error if both specified)
- [ ] **AC-14**: `--interactive` implies `--checkpoint-dir` (uses default if not specified)

### Edge Cases

- [ ] **AC-15**: If state is empty at interrupt, display warning and continue loop (don't crash)
- [ ] **AC-16**: If no checkpoint file exists after interrupt, display error: "Interrupt occurred but no checkpoint found. Ensure --checkpoint-dir is writable."
- [ ] **AC-17**: Workflow completing via `__end__` (Ok path) exits loop cleanly without needing completion key

## Technical Notes

### CLI Arguments Addition

```rust
// In Commands::Run struct
/// Enable interactive human-in-the-loop mode
#[arg(short = 'I', long, conflicts_with = "stream")]
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

/// Response to use when user types 'skip'
#[arg(long, default_value = "I don't have information about this.")]
skip_response: String,
```

### Core Functions to Implement

```rust
/// Read multiline input, double-enter to send
fn read_multiline_input() -> Result<Option<String>> {
    // Returns None if user types quit/exit
    // Returns Some(text) otherwise
}

/// Extract question from state using key list
fn extract_question(state: &JsonValue, keys: &[&str]) -> Option<String> {
    for key in keys {
        if let Some(v) = state.get(key) {
            if let Some(s) = v.as_str() {
                if !s.is_empty() {
                    return Some(s.to_string());
                }
            }
        }
    }
    None
}

/// Check if workflow is complete
fn is_complete(state: &JsonValue, keys: &[&str]) -> bool {
    for key in keys {
        if let Some(v) = state.get(key) {
            if v.as_bool().unwrap_or(false) {
                return true;
            }
        }
    }
    false
}

/// Inject response into state
fn inject_response(state: &mut JsonValue, key: &str, response: &str) {
    if let JsonValue::Object(ref mut map) = state {
        map.insert(key.to_string(), JsonValue::String(response.to_string()));
    }
}
```

### Interactive Loop Structure

```rust
fn run_interactive(/* args */) -> Result<()> {
    let checkpoint_dir = checkpoint_dir.unwrap_or_else(|| PathBuf::from("/tmp/tea_checkpoints"));
    fs::create_dir_all(&checkpoint_dir)?;

    let question_keys: Vec<&str> = question_key.split(',').map(|s| s.trim()).collect();
    let complete_keys: Vec<&str> = complete_key.split(',').map(|s| s.trim()).collect();

    let mut current_state = initial_state;
    let mut iteration = 0;

    loop {
        iteration += 1;

        // Execute until interrupt or completion
        match executor.execute(current_state.clone(), &options) {
            Ok(events) => {
                // Completed successfully
                if let Some(last) = events.last() {
                    println!("\n Workflow complete!");
                    println!("{}", serde_json::to_string_pretty(&last.state)?);
                }
                break;
            }
            Err(TeaError::Interrupt(node)) => {
                // Find latest checkpoint
                let checkpoint = find_latest_checkpoint(&checkpoint_dir)?;
                let checkpoint_state = load_checkpoint(&checkpoint)?;

                // Check for completion
                if is_complete(&checkpoint_state, &complete_keys) {
                    println!("\n Complete!");
                    break;
                }

                // Extract and display question
                let question = extract_question(&checkpoint_state, &question_keys);
                if let Some(q) = question {
                    println!("\n Question ({}):", iteration);
                    println!("{}", q);
                    println!("\nYour answer (Enter twice to send, 'quit' to exit):");

                    match read_multiline_input()? {
                        None => {
                            println!(" Session ended.");
                            break;
                        }
                        Some(response) => {
                            let response = if response.to_lowercase() == "skip" {
                                skip_response.clone()
                            } else {
                                response
                            };

                            // Inject response and prepare for resume
                            inject_response(&mut current_state, &response_key, &response);
                            current_state = merge_states(checkpoint_state, current_state);
                        }
                    }
                } else {
                    eprintln!(" No question found in state at interrupt");
                    eprintln!("   Available keys: {:?}", state_keys(&checkpoint_state));
                    break;
                }
            }
            Err(e) => return Err(e.into()),
        }
    }

    Ok(())
}
```

## Definition of Done

- [ ] `--interactive` flag works with basic Q&A loop
- [ ] Double-enter input works correctly
- [ ] Configurable keys extract question and inject response
- [ ] `quit`, `exit`, `skip` commands work
- [ ] Checkpoint management is automatic
- [ ] Existing `tea run` behavior unchanged when `--interactive` not specified
- [ ] Unit tests for input parsing and key extraction
- [ ] Integration test with simple interrupt workflow

## Tasks / Subtasks

- [ ] **Task 1**: Add `--interactive` and related flags to `Commands::Run` (AC-1, AC-7-9, AC-12-14)
- [ ] **Task 2**: Implement `read_multiline_input()` with double-enter detection (AC-3)
- [ ] **Task 3**: Implement `extract_question()` with configurable keys (AC-2, AC-7)
- [ ] **Task 4**: Implement `inject_response()` (AC-4, AC-8)
- [ ] **Task 5**: Implement `is_complete()` check (AC-5, AC-9)
- [ ] **Task 6**: Implement `find_latest_checkpoint()` helper (AC-6)
- [ ] **Task 7**: Implement main interactive loop in `run_workflow()` (AC-5, AC-6)
- [ ] **Task 8**: Handle `quit`/`exit` commands (AC-10)
- [ ] **Task 9**: Handle `skip` command with `--skip-response` (AC-11, AC-12)
- [ ] **Task 10**: Handle edge case: empty state at interrupt (AC-15)
- [ ] **Task 11**: Handle edge case: missing checkpoint after interrupt (AC-16)
- [ ] **Task 12**: Ensure clean exit on `__end__` completion (AC-17)
- [ ] **Task 13**: Add unit tests for helper functions
- [ ] **Task 14**: Add integration test with interrupt workflow

## Dev Notes

### Edge Case Behaviors

| Scenario | Behavior |
|----------|----------|
| **Empty state at interrupt** | Display warning: "State is empty at interrupt point", continue loop |
| **No checkpoint after interrupt** | Display error: "Interrupt occurred but no checkpoint found. Ensure --checkpoint-dir is writable.", exit with code 1 |
| **Workflow reaches `__end__`** | `executor.execute()` returns `Ok(events)` - display final state, exit cleanly |
| **Completion key is true** | Detected in interrupt handler - display "Complete!", exit cleanly |
| **User types `quit`/`exit`** | Save current checkpoint (if any), display "Session ended.", exit cleanly |

### Checkpoint Behavior Clarification

The interactive loop handles two distinct completion paths:

1. **Normal completion** (`Ok` path): Workflow reaches `__end__` node
   - `executor.execute()` returns `Ok(events)`
   - No checkpoint is created (workflow finished)
   - Display final state and exit

2. **Interrupt with completion key** (`Err(Interrupt)` path): Workflow paused but logically complete
   - Checkpoint exists with `complete: true` (or similar key)
   - Detect via `is_complete()` check
   - Display final state and exit

### Testing

```bash
# Test with a simple interrupt workflow
tea run examples/interview.yaml --interactive

# With custom keys
tea run agent.yaml --interactive \
  --question-key "next_question" \
  --response-key "user_input"
```

### Test Workflow Example

```yaml
name: simple-interview
nodes:
  - name: ask
    run: |
      return {question = "What is your name?"}
    interrupt: after

  - name: greet
    run: |
      return {greeting = "Hello, " .. state.response .. "!"}

edges:
  - from: __start__
    to: ask
  - from: ask
    to: greet
  - from: greet
    to: __end__
```

## Related Stories

- **TEA-CLI-005b**: Rust UX polish + error handling (next)
- **TEA-CLI-005c**: Python port + parity tests (after b)
- **TEA-CLI-004**: CLI Parity Alignment (parent)

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2025-12-21 | 1.1 | Added edge case ACs (15-17), tasks (10-12), Dev Notes clarifications | Bob (SM Agent) |
| 2025-12-21 | 1.0 | Split from TEA-CLI-005, Rust core only | Sarah (PO Agent) |
