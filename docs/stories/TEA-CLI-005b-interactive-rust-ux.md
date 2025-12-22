# TEA-CLI-005b: Interactive Mode - Rust UX & Error Handling

## Status

**Approved**

## Story

**As a** workflow author using interactive mode,
**I want** polished UX with clear formatting, output filtering, and robust error handling,
**So that** the interactive session is pleasant to use and resilient to edge cases.

## Story Context

### Existing System Integration

| Aspect | Details |
|--------|---------|
| **Integrates with** | TEA-CLI-005a interactive loop |
| **Technology** | Rust (std::io, ctrlc crate for signals) |
| **Follows pattern** | `--format` pattern from `tea inspect` |
| **Touch points** | `run_interactive()` in `rust/src/bin/tea.rs` |

### Depends On

- **TEA-CLI-005a**: Core interactive loop must be complete

## Acceptance Criteria

### Welcome Banner & Session Display

- [ ] **AC-1**: Display welcome banner with workflow name on session start
- [ ] **AC-2**: Show available commands (`quit`, `skip`, etc.) in banner
- [ ] **AC-3**: Display iteration/round number for each Q&A cycle (e.g., "Question 1/?" or "Round 3")
- [ ] **AC-4**: Clear visual separators between sections (using `---` or box drawing)

### Output Filtering & Formatting

- [ ] **AC-5**: `--display-key KEY` flag to filter which state keys are shown (comma-separated)
- [ ] **AC-6**: `--display-format FORMAT` flag: `pretty` (default), `json`, `raw`
- [ ] **AC-7**: When `--display-key` not specified, show only the extracted question
- [ ] **AC-8**: After each step, display relevant state changes (respecting filter)
- [ ] **AC-9**: On completion, display final state summary (respecting filter)

### Error Handling & Signals

- [ ] **AC-10**: Graceful Ctrl+C handling - save checkpoint before exit
- [ ] **AC-11**: `--input-timeout SECONDS` flag for automated testing (stdin timeout)
- [ ] **AC-12**: If question key not found, display warning with available state keys
- [ ] **AC-13**: If checkpoint load fails, display helpful error message

### Edge Cases

- [ ] **AC-14**: Handle empty state gracefully
- [ ] **AC-15**: Handle very long questions (wrap or truncate display)
- [ ] **AC-16**: Handle binary/non-UTF8 state values gracefully

## Technical Notes

### New CLI Arguments

```rust
/// State key(s) to display to user (comma-separated, default: question only)
#[arg(long)]
display_key: Option<String>,

/// Display format: pretty, json, raw
#[arg(long, default_value = "pretty")]
display_format: String,

/// Input timeout in seconds (for automated testing)
#[arg(long)]
input_timeout: Option<u64>,
```

### Welcome Banner Implementation

```rust
fn display_welcome(workflow_name: &str) {
    println!();
    println!("============================================================");
    println!("   TEA - {} (Interactive Mode)", workflow_name);
    println!("============================================================");
    println!();
    println!("Commands:");
    println!("  - Type your answer and press Enter twice to send");
    println!("  - 'quit' or 'exit' to end session");
    println!("  - 'skip' to skip current question");
    println!();
    println!("------------------------------------------------------------");
}
```

### Question Display with Formatting

```rust
fn display_question(
    question: &str,
    state: &JsonValue,
    iteration: usize,
    display_keys: Option<&[&str]>,
    format: &str,
) {
    println!();
    println!("------------------------------------------------------------");
    println!(" Question ({})", iteration);
    println!("------------------------------------------------------------");
    println!();
    println!("{}", question);

    // Display additional keys if specified
    if let Some(keys) = display_keys {
        println!();
        for key in keys {
            if let Some(value) = state.get(*key) {
                match format {
                    "json" => println!("{}: {}", key, serde_json::to_string(value).unwrap_or_default()),
                    "raw" => println!("{}: {:?}", key, value),
                    _ => println!("{}: {}", key, format_value_pretty(value)),
                }
            }
        }
    }

    println!();
    println!("------------------------------------------------------------");
    println!("Your answer (Enter twice to send):");
}

fn format_value_pretty(value: &JsonValue) -> String {
    match value {
        JsonValue::String(s) => s.clone(),
        JsonValue::Array(arr) => {
            arr.iter()
                .filter_map(|v| v.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        }
        JsonValue::Object(_) => serde_json::to_string_pretty(value).unwrap_or_default(),
        _ => value.to_string(),
    }
}
```

### Ctrl+C Signal Handling

```rust
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

fn setup_signal_handler() -> Arc<AtomicBool> {
    let interrupted = Arc::new(AtomicBool::new(false));
    let i = interrupted.clone();

    ctrlc::set_handler(move || {
        i.store(true, Ordering::SeqCst);
        eprintln!("\n Interrupted! Saving checkpoint...");
    }).expect("Error setting Ctrl-C handler");

    interrupted
}

// In interactive loop:
if interrupted.load(Ordering::SeqCst) {
    // Save current state to checkpoint
    save_emergency_checkpoint(&checkpoint_dir, &current_state)?;
    println!(" Checkpoint saved. Use 'tea resume' to continue.");
    break;
}
```

### Input Timeout

```rust
use std::time::Duration;
use std::io::{self, BufRead};
use std::sync::mpsc;
use std::thread;

fn read_multiline_input_with_timeout(timeout: Option<Duration>) -> Result<Option<String>> {
    let (tx, rx) = mpsc::channel();

    thread::spawn(move || {
        let stdin = io::stdin();
        let mut lines = Vec::new();

        for line in stdin.lock().lines() {
            match line {
                Ok(l) => {
                    if l.is_empty() && lines.last().map(|s: &String| s.is_empty()).unwrap_or(false) {
                        lines.pop();
                        break;
                    }
                    lines.push(l);
                }
                Err(_) => break,
            }
        }

        let _ = tx.send(lines.join("\n").trim().to_string());
    });

    match timeout {
        Some(t) => {
            rx.recv_timeout(t)
                .map(Some)
                .map_err(|_| anyhow::anyhow!("Input timeout"))
        }
        None => rx.recv().map(Some).map_err(|e| e.into()),
    }
}
```

## Definition of Done

- [ ] Welcome banner displays on session start
- [ ] Iteration counter shows for each question
- [ ] `--display-key` filters output correctly
- [ ] `--display-format` changes output style
- [ ] Ctrl+C saves checkpoint and exits gracefully
- [ ] `--input-timeout` works for CI/testing
- [ ] Missing key warning shows available keys
- [ ] All edge cases handled gracefully
- [ ] Unit tests for formatting functions
- [ ] Integration test with timeout

## Tasks / Subtasks

- [ ] **Task 1**: Add `--display-key`, `--display-format`, `--input-timeout` flags (AC-5, AC-6, AC-11)
- [ ] **Task 2**: Implement `display_welcome()` (AC-1, AC-2)
- [ ] **Task 3**: Implement `display_question()` with formatting (AC-3, AC-4, AC-7, AC-8)
- [ ] **Task 4**: Implement `display_final_state()` (AC-9)
- [ ] **Task 5**: Add `ctrlc` crate dependency and signal handler (AC-10)
- [ ] **Task 6**: Implement `read_multiline_input_with_timeout()` (AC-11)
- [ ] **Task 7**: Implement missing key warning with available keys (AC-12)
- [ ] **Task 8**: Add helpful checkpoint error messages (AC-13)
- [ ] **Task 9**: Handle edge cases (AC-14, AC-15, AC-16)
- [ ] **Task 10**: Add unit tests for display functions
- [ ] **Task 11**: Add integration test with `--input-timeout`

## Dev Notes

### Dependencies to Add

```toml
# In rust/Cargo.toml
[dependencies]
ctrlc = "3.4"
```

### Example Session

```
============================================================
   TEA - Interview Agent (Interactive Mode)
============================================================

Commands:
  - Type your answer and press Enter twice to send
  - 'quit' or 'exit' to end session
  - 'skip' to skip current question

------------------------------------------------------------

 Starting with: {"user_id": "123"}
 Processing...

------------------------------------------------------------
 Question (1)
------------------------------------------------------------

What is the primary nature of this legal matter?

options: litigation, transactional, advisory
context: Corporate law department intake

------------------------------------------------------------
Your answer (Enter twice to send):
> transactional
>

 Processing...

------------------------------------------------------------
 Question (2)
------------------------------------------------------------

What is the estimated deal value?

------------------------------------------------------------
Your answer (Enter twice to send):
> ^C
 Interrupted! Saving checkpoint...
 Checkpoint saved: /tmp/tea_checkpoints/interview_abc123.bin
```

## Related Stories

- **TEA-CLI-005a**: Core interactive loop (prerequisite)
- **TEA-CLI-005c**: Python port + parity tests (next)

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2025-12-21 | 1.0 | Split from TEA-CLI-005, UX & error handling | Sarah (PO Agent) |
