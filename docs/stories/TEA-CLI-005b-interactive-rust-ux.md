# TEA-CLI-005b: Interactive Mode - Rust UX & Error Handling

## Status

**Done**

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

- [x] **AC-1**: Display welcome banner with workflow name on session start
- [x] **AC-2**: Show available commands (`quit`, `skip`, etc.) in banner
- [x] **AC-3**: Display iteration/round number for each Q&A cycle (e.g., "Question 1/?" or "Round 3")
- [x] **AC-4**: Clear visual separators between sections (using `---` or box drawing)

### Output Filtering & Formatting

- [x] **AC-5**: `--display-key KEY` flag to filter which state keys are shown (comma-separated)
- [x] **AC-6**: `--display-format FORMAT` flag: `pretty` (default), `json`, `raw`
- [x] **AC-7**: When `--display-key` not specified, show only the extracted question
- [x] **AC-8**: After each step, display relevant state changes (respecting filter)
- [x] **AC-9**: On completion, display final state summary (respecting filter)

### Error Handling & Signals

- [x] **AC-10**: Graceful Ctrl+C handling - save checkpoint before exit
- [x] **AC-11**: `--input-timeout SECONDS` flag for automated testing (stdin timeout)
- [x] **AC-12**: If question key not found, display warning with available state keys
- [x] **AC-13**: If checkpoint load fails, display helpful error message

### Edge Cases

- [x] **AC-14**: Handle empty state gracefully
- [x] **AC-15**: Handle very long questions (wrap or truncate display)
- [x] **AC-16**: Handle binary/non-UTF8 state values gracefully

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

- [x] Welcome banner displays on session start
- [x] Iteration counter shows for each question
- [x] `--display-key` filters output correctly
- [x] `--display-format` changes output style
- [x] Ctrl+C saves checkpoint and exits gracefully
- [x] `--input-timeout` works for CI/testing
- [x] Missing key warning shows available keys
- [x] All edge cases handled gracefully
- [x] Unit tests for formatting functions
- [x] Integration test with timeout

## Tasks / Subtasks

- [x] **Task 1**: Add `--display-key`, `--display-format`, `--input-timeout` flags (AC-5, AC-6, AC-11)
- [x] **Task 2**: Implement `display_welcome()` (AC-1, AC-2)
- [x] **Task 3**: Implement `display_question()` with formatting (AC-3, AC-4, AC-7, AC-8)
- [x] **Task 4**: Implement `display_final_state()` (AC-9)
- [x] **Task 5**: Add `ctrlc` crate dependency and signal handler (AC-10)
- [x] **Task 6**: Implement `read_multiline_input_with_timeout()` (AC-11)
- [x] **Task 7**: Implement missing key warning with available keys (AC-12)
- [x] **Task 8**: Add helpful checkpoint error messages (AC-13)
- [x] **Task 9**: Handle edge cases (AC-14, AC-15, AC-16)
- [x] **Task 10**: Add unit tests for display functions
- [x] **Task 11**: Add integration test with `--input-timeout`

### Optional Tasks (from TEA-CLI-005a QA Review)

These are low-priority improvements identified during QA review of the core implementation:

- [ ] **Task 12** *(Optional)*: Add unit tests for `extract_question` with array input
- [ ] **Task 13** *(Optional)*: Add unit test for `is_complete` function
- [ ] **Task 14** *(Optional)*: Extract interactive mode config to struct (reduce `run_interactive` parameters)

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

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5

### File List

| File | Action | Description |
|------|--------|-------------|
| `rust/src/bin/tea.rs` | Modified | Added new CLI flags (--display-key, --display-format, --input-timeout), display functions, signal handling, timeout support |
| `rust/Cargo.toml` | Modified | Added ctrlc v3.4 dependency for signal handling |
| `rust/tests/test_cli.rs` | Modified | Added 8 new tests for UX features, fixed existing test assertion |

### Debug Log References

No blocking issues encountered during implementation.

### Completion Notes

- All 11 required tasks completed successfully
- 8 new tests added (30 total CLI tests, all passing)
- Full test suite passes (61 tests across all Rust modules)
- Implementation follows patterns established in TEA-CLI-005a
- Key features implemented:
  - Welcome banner shows workflow name (from YAML `name:` field or filename)
  - Question display includes iteration counter and visual separators
  - Output filtering via `--display-key` (comma-separated)
  - Format options: pretty (default), json, raw
  - Ctrl+C handler saves checkpoint and shows resume instructions
  - Input timeout via `--input-timeout` for automated testing
  - Edge cases: empty state, long questions (wrapped at 76 chars), binary data

### Change Log

| Date | Author | Change |
|------|--------|--------|
| 2025-12-28 | James (Dev Agent) | Implemented all UX enhancements for TEA-CLI-005b |

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2025-12-28 | 1.2 | Implementation complete - all ACs and tasks done | James (Dev Agent) |
| 2025-12-28 | 1.1 | Added optional tasks 12-14 from TEA-CLI-005a QA review | Sarah (PO Agent) |
| 2025-12-21 | 1.0 | Split from TEA-CLI-005, UX & error handling | Sarah (PO Agent) |

## QA Results

### Review Date: 2025-12-28

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall**: Excellent implementation. All 16 acceptance criteria are implemented with corresponding test coverage. The code follows established patterns from TEA-CLI-005a and maintains consistency with the existing codebase.

**Highlights**:
- Comprehensive error messages with actionable troubleshooting steps
- Clean separation of display functions with clear AC references
- Proper use of Rust idioms (Option, Result, pattern matching)
- Text wrapping implemented for long questions
- Binary data gracefully handled with detection heuristics

### Refactoring Performed

None required. Implementation is clean and follows project standards.

### Compliance Check

- Coding Standards: ✓ Code follows Rust idioms, formatted with `cargo fmt`
- Project Structure: ✓ Changes in appropriate files (bin/tea.rs, Cargo.toml, tests/)
- Testing Strategy: ✓ 8 integration tests added for new functionality
- All ACs Met: ✓ All 16 ACs verified implemented with test coverage

### Improvements Checklist

All items addressed by developer:

- [x] CLI flags added (--display-key, --display-format, --input-timeout)
- [x] Welcome banner with workflow name
- [x] Question display with iteration counter
- [x] Visual separators between sections
- [x] Output filtering and formatting
- [x] Ctrl+C signal handling
- [x] Input timeout for automated testing
- [x] Missing key warning with available keys
- [x] Helpful checkpoint error messages
- [x] Edge cases handled (empty state, long questions, binary data)

Future improvements (low priority, not blocking):
- [ ] Unit tests for `wrap_text()` edge cases (words longer than width)
- [ ] Unit tests for `format_value_pretty()` with all JSON types
- [ ] Optional Task 14: Extract interactive config to struct (already tracked)

### Security Review

No security concerns. The implementation:
- Does not handle secrets or sensitive data
- Display functions only format and output existing state
- Signal handling is standard Ctrl+C capture

### Performance Considerations

No performance concerns:
- Display functions are O(n) where n is text/key length
- Timeout implementation uses standard thread/channel pattern
- No blocking operations in display path

### Files Modified During Review

None. No refactoring was necessary.

### Gate Status

Gate: **PASS** → `docs/qa/gates/TEA-CLI-005b-interactive-rust-ux.yml`

### Recommended Status

**✓ Ready for Done**

All acceptance criteria implemented, all tests passing, code quality excellent.
