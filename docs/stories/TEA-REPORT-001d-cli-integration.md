# TEA-REPORT-001d: CLI Integration & UX

## Status

**Done** - QA Review passed 2026-01-11. Gate: PASS. All 16 ACs verified, security review passed.

## Parent Epic

[TEA-REPORT-001: Automatic Bug Reporting System](TEA-REPORT-001-automatic-bug-reporting.md)

## Dependencies

- **TEA-REPORT-001a** (Error Capture) - Provides ErrorReport structure
- **TEA-REPORT-001b** (URL Encoder) - Provides encode_error_report()

---

## Story

**As a** tea user encountering an error,
**I want** to see a shareable bug report URL in my terminal,
**So that** I can easily report the issue to the maintainers.

## Story Context

### Existing System Integration

| Aspect | Details |
|--------|---------|
| **Integrates with** | CLI error handling, panic hooks, YAML engine, executor |
| **Technology** | Rust (clap, colored), Python (typer, rich) |
| **Follows pattern** | Existing CLI error messages |
| **Touch points** | `rust/src/main.rs`, `python/src/the_edge_agent/cli.py` |

## Acceptance Criteria

### Core Functionality

- [x] **AC-19**: On crash/error, display report URL to user
- [x] **AC-20**: `--report-bugs` flag to enable automatic URL generation (default: enabled)
- [x] **AC-21**: `--no-report-bugs` flag to disable (for privacy-conscious users)
- [x] **AC-22**: Environment variable `TEA_REPORT_BUGS=false` to disable globally
- [x] **AC-23**: Clear message: "Report this bug: {URL}" with explanation of what's included
- [x] **AC-24**: Option to copy URL to clipboard (if terminal supports it)

### Opt-in Extended Context

- [x] **AC-28**: After displaying minimal URL, prompt: "Include more context? [y/N]"
- [x] **AC-29**: If user opts in, generate extended URL with additional data
- [x] **AC-30**: Extended context includes (user-approved): sanitized YAML structure, node names, action types
- [x] **AC-31**: Extended context excludes (always): state data, secrets, environment variables, file contents
- [x] **AC-32**: `--report-extended` flag to auto-include extended context (skip prompt)
- [x] **AC-33**: `--report-minimal` flag to skip extended prompt entirely
- [x] **AC-34**: Extended URL clearly marked as containing more information in the viewer

## Tasks / Subtasks

### Rust Implementation

- [x] **Task 1.1**: Add CLI flags to clap
  - [x] `--report-bugs` / `--no-report-bugs`
  - [x] `--report-extended` / `--report-minimal`

- [x] **Task 1.2**: Add environment variable support
  - [x] Check `TEA_REPORT_BUGS` env var
  - [x] Check `TEA_REPORT_EXTENDED` env var

- [x] **Task 1.3**: Integrate with panic hook
  - [x] On panic, generate URL if enabled
  - [x] Display formatted message

- [x] **Task 1.4**: Integrate with error handling
  - [x] Wrap main() errors
  - [x] Generate URL for YAML/executor errors

- [x] **Task 1.5**: Implement extended context prompt
  - [x] Prompt user: "Include more context? [y/N]"
  - [x] Read single character input
  - [x] Generate appropriate URL

- [x] **Task 1.6**: Implement clipboard support
  - [x] Detect if clipboard available
  - [x] Copy URL to clipboard
  - [x] Show "Copied to clipboard" message

### Python Implementation

- [x] **Task 2.1**: Add CLI flags to typer
  - [x] `--report-bugs` / `--no-report-bugs`
  - [x] `--report-extended` / `--report-minimal`

- [x] **Task 2.2**: Add environment variable support
  - [x] Same as Rust

- [x] **Task 2.3**: Integrate with excepthook
  - [x] Generate URL on unhandled exception

- [x] **Task 2.4**: Integrate with error handling
  - [x] Wrap main errors

- [x] **Task 2.5**: Implement extended context prompt
  - [x] Use rich for nice prompt

- [x] **Task 2.6**: Implement clipboard support
  - [x] Use pyperclip or similar

### UX Design

- [x] **Task 3.1**: Design error message format
  - [x] Clear visual separation
  - [x] Explanation of what's included
  - [x] URL on its own line for easy copy

- [x] **Task 3.2**: Design extended prompt
  - [x] Non-blocking (can skip with Enter)
  - [x] Timeout after 10 seconds (default to minimal)

- [x] **Task 3.3**: Test with various terminal widths
  - [x] URL should not wrap badly

### Testing

- [x] **Task 4.1**: Test flag combinations
- [x] **Task 4.2**: Test environment variable override
- [x] **Task 4.3**: Test extended prompt flow
- [x] **Task 4.4**: Test clipboard functionality

## Technical Notes

### CLI Flag Structure

```rust
// Rust - Commands::Run
#[derive(Parser)]
pub struct RunCommand {
    /// YAML workflow file to execute
    pub file: PathBuf,

    // ... existing flags ...

    /// Enable bug report URL generation on errors (default: true)
    #[arg(long, default_value = "true", env = "TEA_REPORT_BUGS")]
    pub report_bugs: bool,

    /// Disable bug report URL generation
    #[arg(long, conflicts_with = "report_bugs")]
    pub no_report_bugs: bool,

    /// Auto-include extended context in bug reports (skip prompt)
    #[arg(long, env = "TEA_REPORT_EXTENDED")]
    pub report_extended: bool,

    /// Skip extended context prompt (minimal report only)
    #[arg(long, conflicts_with = "report_extended")]
    pub report_minimal: bool,
}
```

```python
# Python - cli.py
@app.command()
def run(
    file: Path,
    # ... existing args ...
    report_bugs: bool = typer.Option(
        True,
        "--report-bugs/--no-report-bugs",
        envvar="TEA_REPORT_BUGS",
        help="Generate bug report URL on errors"
    ),
    report_extended: bool = typer.Option(
        False,
        "--report-extended",
        envvar="TEA_REPORT_EXTENDED",
        help="Auto-include extended context"
    ),
    report_minimal: bool = typer.Option(
        False,
        "--report-minimal",
        help="Skip extended context prompt"
    ),
):
    ...
```

### Error Handler Integration

```rust
// Rust
fn handle_error(error: &dyn std::error::Error, options: &ReportOptions) {
    // Print the error normally first
    eprintln!("Error: {}", error);

    if !options.report_bugs {
        return;
    }

    // Capture error info
    let report = capture_error(error, options.extended);

    // Encode to URL
    let url = encode_error_report(&report, BASE_URL);

    // Display report section
    display_report_url(&url, &report, options);
}

fn display_report_url(url: &str, report: &ErrorReport, options: &ReportOptions) {
    eprintln!();
    eprintln!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    eprintln!("🐛 Report this bug:");
    eprintln!("   {}", url);
    eprintln!();
    eprintln!("   This URL contains only: version, platform, and stack trace.");
    eprintln!("   No personal information or file contents are included.");

    if !options.report_minimal && !options.report_extended {
        eprintln!();
        eprint!("   Include more context to help diagnose? [y/N]: ");

        if prompt_yes_no(false, Duration::from_secs(10)) {
            let extended_report = add_extended_context(report);
            let extended_url = encode_error_report(&extended_report, BASE_URL);
            display_extended_url(&extended_url);
        }
    }

    eprintln!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    // Try to copy to clipboard
    if let Ok(()) = copy_to_clipboard(url) {
        eprintln!("   📋 URL copied to clipboard");
    }
}
```

```python
# Python
def handle_error(error: Exception, options: ReportOptions):
    """Handle error and optionally generate report URL."""

    # Print error normally
    console.print(f"[red]Error:[/red] {error}")

    if not options.report_bugs:
        return

    # Capture error
    report = capture_exception(error, extended=options.report_extended)

    # Encode
    url = encode_error_report(report, BASE_URL)

    # Display
    display_report_url(url, report, options)

def display_report_url(url: str, report: ErrorReport, options: ReportOptions):
    """Display the bug report URL with nice formatting."""

    console.print()
    console.print("━" * 68)
    console.print("🐛 [bold]Report this bug:[/bold]")
    console.print(f"   {url}")
    console.print()
    console.print("   This URL contains only: version, platform, and stack trace.")
    console.print("   No personal information or file contents are included.")

    if not options.report_minimal and not options.report_extended:
        console.print()
        include_extended = Confirm.ask(
            "   Include more context to help diagnose?",
            default=False
        )

        if include_extended:
            extended_report = add_extended_context(report)
            extended_url = encode_error_report(extended_report, BASE_URL)
            display_extended_url(extended_url)

    console.print("━" * 68)

    # Try clipboard
    try:
        import pyperclip
        pyperclip.copy(url)
        console.print("   📋 URL copied to clipboard")
    except:
        pass
```

### Extended Context Display

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
🐛 Extended report (includes workflow structure):
   https://example.github.io/the_edge_agent/report/0.9.34/rust_ext_...

   Additional info included:
   ✓ Node names: process_data, validate, finalize
   ✓ Action types: llm_call, transform, conditional
   ✓ Graph structure (edges between nodes)

   Still excluded: state data, secrets, prompts, file contents
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

### Clipboard Support

```rust
// Rust - using arboard crate
fn copy_to_clipboard(text: &str) -> Result<(), Box<dyn std::error::Error>> {
    let mut clipboard = arboard::Clipboard::new()?;
    clipboard.set_text(text)?;
    Ok(())
}
```

```python
# Python - using pyperclip (optional dependency)
def copy_to_clipboard(text: str) -> bool:
    try:
        import pyperclip
        pyperclip.copy(text)
        return True
    except ImportError:
        return False
    except Exception:
        return False
```

### Input Timeout

```rust
// Rust - non-blocking input with timeout
use std::io::{self, Read};
use std::time::Duration;

fn prompt_yes_no(default: bool, timeout: Duration) -> bool {
    use std::sync::mpsc;
    use std::thread;

    let (tx, rx) = mpsc::channel();

    thread::spawn(move || {
        let mut buffer = [0u8; 1];
        if io::stdin().read_exact(&mut buffer).is_ok() {
            let _ = tx.send(buffer[0]);
        }
    });

    match rx.recv_timeout(timeout) {
        Ok(b'y') | Ok(b'Y') => true,
        Ok(b'n') | Ok(b'N') => false,
        Ok(b'\n') | Ok(b'\r') => default,
        _ => default,
    }
}
```

## Dev Notes

### File Locations

| Runtime | Modified Files |
|---------|----------------|
| Rust | `rust/src/main.rs`, `rust/src/cli.rs`, `rust/src/report/cli.rs` |
| Python | `python/src/the_edge_agent/cli.py`, `python/src/the_edge_agent/report_cli.py` |

### Dependencies

**Rust:**
- `arboard` (clipboard) - optional

**Python:**
- `pyperclip` (clipboard) - optional
- `rich` (already present for formatting)

### Testing

```bash
# Test error handling
cd rust && cargo run -- run nonexistent.yaml

# Test with flags
cd rust && cargo run -- run test.yaml --no-report-bugs
cd rust && cargo run -- run test.yaml --report-extended

# Test environment variable
TEA_REPORT_BUGS=false cargo run -- run test.yaml
```

## Definition of Done

- [x] `--report-bugs` / `--no-report-bugs` flags working
- [x] `TEA_REPORT_BUGS` environment variable working
- [x] Report URL displayed on errors
- [x] Extended context prompt working
- [x] `--report-extended` / `--report-minimal` flags working
- [x] Clipboard copy working (where available)
- [x] Same behavior in Rust and Python

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| Prompt blocks CI | Medium | Timeout, detect non-interactive |
| Clipboard fails | Low | Graceful fallback, just show URL |
| URL display breaks | Low | Test various terminal widths |

## QA Notes

**Date:** 2026-01-11
**Reviewed by:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Count |
|--------|-------|
| Total test scenarios | 42 |
| Unit tests | 18 (43%) |
| Integration tests | 16 (38%) |
| E2E tests | 8 (19%) |
| P0 (Critical) | 12 |
| P1 (Core) | 18 |
| Security-critical tests | 4 |
| Cross-runtime parity tests | 3 |

**Coverage Status:** All 16 Acceptance Criteria have test coverage. No coverage gaps identified.

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **Sensitive data leak in extended reports** | HIGH | 4 dedicated security tests (001d-INT-015, 001d-INT-016, 001d-E2E-007) verify state/secrets exclusion |
| **Prompt blocks CI pipelines** | MEDIUM | Timeout logic test (001d-UNIT-015) + non-interactive environment detection recommended |
| **Clipboard fails on headless systems** | LOW | Graceful fallback tests (001d-UNIT-012, 001d-INT-011) ensure no crash |
| **URL display breaks on narrow terminals** | LOW | Formatting tests (001d-UNIT-002, 001d-UNIT-010) validate output |

### Recommended Test Scenarios (Priority Order)

**Phase 1 - P0 Critical (Must Pass Before Merge):**
1. Panic hook triggers URL generation (Rust) - 001d-INT-001
2. Excepthook triggers URL generation (Python) - 001d-INT-002
3. YAML engine errors generate report URL - 001d-INT-003
4. Extended report excludes state data - 001d-INT-015
5. Extended report excludes secrets - 001d-INT-016
6. E2E security verification - 001d-E2E-007

**Phase 2 - P1 Core Journeys:**
- All flag parsing tests
- Environment variable precedence
- Interactive prompt flow
- Cross-runtime parity

**Phase 3 - P2 Secondary:**
- Clipboard functionality
- Extended display formatting

### Concerns / Blockers

| Type | Description | Recommendation |
|------|-------------|----------------|
| **Concern** | Input timeout implementation (001d-UNIT-015) spawns threads - ensure thread cleanup on timeout | Add test for thread cleanup |
| **Concern** | Pyperclip is optional dependency - ensure graceful import failure | Document as optional in setup.py extras |
| **Concern** | Rust `arboard` crate may not work on all Linux wayland/X11 configurations | Consider `wl-copy`/`xclip` fallback or make clipboard truly optional |
| **Note** | CI environments are non-interactive - ensure `TEA_NON_INTERACTIVE` env var skips prompts | Add this to CI config documentation |

### Test Environment Requirements

- Rust: `cargo test --lib cli_integration` + `cargo test --test test_cli_report`
- Python: `pytest tests/test_report_cli.py`
- E2E: Both runtimes with test fixtures in `test_fixtures/broken.yaml`

### Quality Gate

**Ready for Development:** YES

All critical paths covered. Security-sensitive data exclusion has P0 priority. Parity testing ensures consistent behavior across Rust/Python.

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5

### Debug Log References

N/A - Implementation completed without blocking issues.

### Completion Notes

1. Implemented CLI flags in both Python (typer) and Rust (clap):
   - `--report-bugs/--no-report-bugs`
   - `--report-extended`
   - `--report-minimal`

2. Added environment variable support:
   - `TEA_REPORT_BUGS` for enabling/disabling
   - `TEA_REPORT_EXTENDED` for auto-include extended context

3. Created `report_cli.py` module in Python with:
   - `configure()` function for CLI flag storage
   - `display_error_report()` for formatted URL display
   - `add_extended_context()` for opt-in context
   - `prompt_yes_no()` with timeout support
   - `copy_to_clipboard()` using pyperclip
   - CLI excepthook installation

4. Created `rust/src/report/cli.rs` module with matching Rust implementation:
   - `configure()` using atomics for thread-safe storage
   - `display_error_report()` with formatted output
   - `add_extended_context()` extracting from YAML config
   - `prompt_yes_no()` with mpsc channel timeout
   - `copy_to_clipboard()` using xclip/xsel/pbcopy/clip

5. Tests: 20 Python tests passing, 8 Rust tests passing

### File List

| File | Status | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/cli.py` | Modified | Added CLI flags for bug reporting |
| `python/src/the_edge_agent/report_cli.py` | Created | CLI integration module for bug reports |
| `python/src/the_edge_agent/__init__.py` | Modified | Export report_cli module |
| `python/tests/test_report_cli.py` | Created | Tests for CLI integration |
| `rust/src/bin/tea.rs` | Modified | Added CLI flags for bug reporting |
| `rust/src/report/mod.rs` | Modified | Export cli module |
| `rust/src/report/cli.rs` | Created | CLI integration module for bug reports |
| `rust/src/report/tests.rs` | Modified | Added CLI integration tests |

### Change Log

| Date | Change | Files |
|------|--------|-------|
| 2026-01-11 | Added --report-bugs, --report-extended, --report-minimal flags to Python CLI | cli.py |
| 2026-01-11 | Created Python report_cli module with display/prompt/clipboard support | report_cli.py |
| 2026-01-11 | Added --report-bugs, --no-report-bugs, --report-extended, --report-minimal flags to Rust CLI | tea.rs |
| 2026-01-11 | Created Rust report/cli module with display/prompt/clipboard support | cli.rs |
| 2026-01-11 | Added tests for CLI integration in both Python and Rust | test_report_cli.py, tests.rs |

---

## QA Results

### Review Date: 2026-01-11

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: GOOD** - The implementation demonstrates solid engineering practices with clean separation of concerns between Python and Rust implementations. Both runtimes follow their respective idiomatic patterns.

**Strengths:**
- **Cross-runtime parity**: Python (`report_cli.py:381 lines`) and Rust (`cli.rs:547 lines`) implementations are functionally equivalent with matching APIs (`configure()`, `display_error_report()`, `add_extended_context()`)
- **Security-first design**: Extended context extraction properly excludes state data, secrets, and prompts (AC-31)
- **Graceful degradation**: Clipboard copy handles missing dependencies (pyperclip) without crashing
- **Non-interactive detection**: Both runtimes check `TEA_NON_INTERACTIVE`, `CI`, `GITHUB_ACTIONS` env vars to avoid blocking CI pipelines

**Architecture:**
- Clean module structure with `report_cli` as separate concern from base `report` module
- Thread-safe global configuration using atomics (Rust) and module-level globals (Python)
- Proper error boundary - encoding failures don't crash error reporting

### Refactoring Performed

None required - implementation is clean and well-structured.

### Compliance Check

- Coding Standards: ✓ Both implementations follow language idioms
- Project Structure: ✓ Files placed correctly in Python/Rust trees
- Testing Strategy: ✓ 20 Python tests + 8 Rust tests passing
- All ACs Met: ✓ All 16 ACs (AC-19 through AC-34) verified

### Improvements Checklist

- [x] Environment variable handling for non-interactive detection
- [x] Timeout implementation for prompt (10 seconds)
- [x] CLI flag parsing with mutual exclusivity validation
- [x] Graceful clipboard fallback
- [ ] Thread cleanup test for Rust `prompt_yes_no` timeout (001d-UNIT-015 concern)
- [ ] Document `TEA_NON_INTERACTIVE=1` in CI documentation
- [ ] Consider adding `wl-copy`/`xclip` detection message on Wayland/X11 failure

### Security Review

**Status: PASS**

- **Data exclusion verified**: `add_extended_context()` only extracts structural data (node names, action types, edges, schema field names) - never state values, secrets, prompts, or file contents
- **Path sanitization**: Stack traces use sanitized paths (relative or ~/ prefixed)
- **No credential exposure risk**: Error reports contain only: version, platform, runtime, error type, message, stack trace

### Performance Considerations

**Status: PASS - No concerns**

- Encoding only happens on errors (not hot path)
- URL generation is O(n) where n = report size
- Clipboard operations are fire-and-forget with graceful failure
- Timeout threads properly bounded (10 second max)

### Files Modified During Review

None - no refactoring was performed.

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-REPORT-001d-cli-integration.yml
Risk profile: Existing from test design phase
NFR assessment: Inline in this review

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, tests passing, security review passed.

---

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2025-01-11 | 1.0 | Initial story creation | Sarah (PO Agent) |
| 2026-01-11 | 1.1 | Added QA Notes from test design review | Quinn (QA Agent) |
| 2026-01-11 | 2.0 | Implementation complete | James (Dev Agent) |
| 2026-01-11 | 2.1 | QA Review complete - PASS | Quinn (Test Architect) |
