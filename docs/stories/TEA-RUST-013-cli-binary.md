# Story: TEA-RUST-013 - CLI Binary with Clap

## Status

**Done**

---

## Story

**As a** developer deploying workflows to edge devices,
**I want** a CLI binary that can run, resume, validate, and inspect workflows,
**So that** I can execute and manage workflows from the command line without writing code.

---

## Story Context

**Existing System Integration:**

- **Integrates with:** `YamlEngine`, `Executor`, `Checkpointer` in `src/engine/`
- **Technology:** Rust, clap crate, serde_json, tracing-subscriber
- **Follows pattern:** Standard Rust CLI patterns with subcommands
- **Touch points:** `src/bin/tea.rs` (main CLI binary)

---

## Acceptance Criteria

### From Epic AC-17 to AC-20

1. **AC-17**: GIVEN `tea` CLI binary, WHEN run with `tea run workflow.yaml --input '{"key": "value"}'`, THEN workflow executes and outputs final state as JSON
2. **AC-18**: GIVEN `tea` CLI binary, WHEN run with `tea run workflow.yaml --stream`, THEN workflow outputs each event as newline-delimited JSON
3. **AC-19**: GIVEN `tea` CLI binary, WHEN run with `tea resume checkpoint.bin --input '{"update": "value"}'`, THEN execution resumes from checkpoint with merged state
4. **AC-20**: GIVEN `tea` CLI binary, WHEN run with `tea validate workflow.yaml`, THEN YAML is parsed and validated without execution

### Additional Requirements

5. `tea inspect workflow.yaml` shows graph structure (text, JSON, or DOT format)
6. Verbosity flags (-v, -vv, -vvv) control log output
7. Quiet flag (-q) suppresses non-error output
8. Checkpoint directory configurable via `--checkpoint-dir`
9. Interrupt points configurable via `--interrupt-before` and `--interrupt-after`
10. Input can be provided as JSON string or @file.json reference

---

## Technical Notes

### Implementation Status: MOSTLY COMPLETE

The CLI is substantially implemented in `src/bin/tea.rs`:

**Fully Implemented:**
- `tea run` with `--input`, `--stream`, `--checkpoint-dir`, interrupts
- `tea validate` with `--detailed` option
- `tea inspect` with text, JSON, and DOT output formats
- Verbosity and quiet flags
- Input parsing from string or @file.json

**Partially Implemented:**
- `tea resume` - Loads checkpoint, merges state, but **does not actually resume execution**
  - Line 275: "Note: Resume requires the original workflow file."
  - Missing: Loading workflow from checkpoint metadata, continuing execution

### CLI Structure

```
tea
├── run <file>              # Execute workflow
│   ├── --input, -i         # Initial state
│   ├── --stream, -s        # NDJSON output
│   ├── --checkpoint-dir    # Checkpoint storage
│   ├── --interrupt-before  # Pre-node interrupts
│   └── --interrupt-after   # Post-node interrupts
├── resume <checkpoint>     # Resume from checkpoint
│   ├── --input, -i         # State updates
│   ├── --stream, -s        # NDJSON output
│   └── --checkpoint-dir    # Checkpoint storage
├── validate <file>         # Validate YAML
│   └── --detailed          # Show structure
└── inspect <file>          # Show structure
    └── --format            # text|json|dot
```

### Gap Analysis

| AC | Status | Gap |
|----|--------|-----|
| AC-17 | ✅ Done | None |
| AC-18 | ✅ Done | None |
| AC-19 | ⚠️ Partial | Checkpoint loaded but execution not resumed |
| AC-20 | ✅ Done | None |
| 5-10 | ✅ Done | None |

### Resume Fix Required

To complete AC-19, the `resume_workflow` function needs:

1. Store workflow path in checkpoint metadata (during save)
2. Load workflow from checkpoint metadata (during resume)
3. Create executor and call `execute()` with `resume_from` option
4. Alternatively: Accept workflow path as required argument

---

## Tasks / Subtasks

- [x] **Task 1: Implement CLI structure** (AC: 17-20)
  - [x] Define clap Parser with subcommands
  - [x] Add global flags (verbose, quiet)
  - [x] Implement input parsing (string or @file)

- [x] **Task 2: Implement run command** (AC: 17, 18)
  - [x] Load workflow from YAML
  - [x] Execute with options
  - [x] Output final state or stream events

- [x] **Task 3: Complete resume command** (AC: 19)
  - [x] Load checkpoint file
  - [x] Parse and merge state updates
  - [x] Load original workflow (from checkpoint or argument)
  - [x] Resume execution from checkpoint state
  - [x] Handle interrupt continuation

- [x] **Task 4: Implement validate command** (AC: 20)
  - [x] Parse YAML without execution
  - [x] Report validation success/failure
  - [x] Optional detailed structure output

- [x] **Task 5: Implement inspect command** (AC: 5)
  - [x] Text format output
  - [x] JSON format output
  - [x] DOT (Graphviz) format output

- [x] **Task 6: Testing** (All AC)
  - [x] E2E test for run command
  - [x] E2E test for resume command
  - [x] E2E test for validate command
  - [x] E2E test for inspect command

---

## Dev Notes

### Relevant Source Tree

```
rust/src/bin/tea.rs         # CLI implementation (454 lines)
  - Cli struct (lines 23-37)
  - Commands enum (lines 39-108)
  - run_workflow (lines 159-232)
  - resume_workflow (lines 234-285) - NEEDS COMPLETION
  - validate_workflow (lines 287-327)
  - inspect_workflow (lines 329-440)
  - parse_input (lines 442-453)
```

### Testing

- **Test file location:** E2E tests in `tests/cli/` (to be created)
- **Test frameworks:** Shell scripts or Rust integration tests
- **Pattern:** Run CLI binary, capture output, verify results

### Example Test Scenarios

```bash
# AC-17: Run with input
tea run examples/simple.yaml --input '{"name": "test"}'

# AC-18: Stream output
tea run examples/simple.yaml --stream | head -1

# AC-19: Resume (after implementing)
tea run examples/interruptible.yaml --interrupt-before process --checkpoint-dir ./checkpoints
tea resume ./checkpoints/checkpoint-xxx.bin --input '{"approved": true}'

# AC-20: Validate
tea validate examples/simple.yaml
```

---

## Risk and Compatibility Check

**Minimal Risk Assessment:**

- **Primary Risk:** Resume functionality incomplete
- **Mitigation:** Can be completed in isolation, existing run/validate work
- **Rollback:** Not needed - additive change

**Compatibility Verification:**

- [x] No breaking changes to existing CLI commands
- [x] Checkpoint format unchanged
- [x] YAML format unchanged

---

## Definition of Done

- [x] AC-17 (run) satisfied
- [x] AC-18 (stream) satisfied
- [x] AC-19 (resume) satisfied
- [x] AC-20 (validate) satisfied
- [x] Additional requirements (5-10) satisfied
- [x] E2E tests written and passing
- [x] Code follows existing patterns

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-20 | 0.1 | Story created from epic | Bob (SM) |
| 2025-12-20 | 0.2 | Gap analysis: resume incomplete, needs Task 3 completion | Bob (SM) |
| 2025-12-20 | 0.3 | Implemented AC-19 resume with --workflow arg, added 14 E2E tests | James (Dev) |
| 2025-12-20 | 0.4 | QA re-review passed, gate PASS, status set to Done | Quinn (QA) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

None

### Completion Notes List

1. **AC-19 Resume Implementation**: Added required `--workflow` argument to `tea resume` command. The resume function now:
   - Loads checkpoint from file
   - Merges state updates from `--input`
   - Loads the workflow from the specified `--workflow` path
   - Creates executor with action registry
   - Calls `execute()` with the checkpoint state
   - Handles both stream and non-stream output modes
   - Properly handles interrupts during resume

2. **E2E Tests**: Created comprehensive test suite in `tests/test_cli.rs` with 14 tests:
   - `test_run_with_input` - AC-17
   - `test_run_without_input` - AC-17
   - `test_run_stream_mode` - AC-18
   - `test_run_missing_file` - error handling
   - `test_validate_valid_workflow` - AC-20
   - `test_validate_detailed` - AC-20
   - `test_validate_invalid_workflow` - validation error
   - `test_inspect_text_format` - AC-5
   - `test_inspect_json_format` - AC-5
   - `test_inspect_dot_format` - AC-5
   - `test_resume_with_workflow` - AC-19
   - `test_quiet_flag` - AC-7
   - `test_help` - CLI usability
   - `test_version` - CLI usability

3. **Test Fixtures**: Created 3 YAML workflow fixtures for testing:
   - `simple_workflow.yaml` - basic 2-node workflow
   - `interruptible_workflow.yaml` - workflow with interrupt points
   - `invalid_workflow.yaml` - workflow with validation errors

### File List

| File | Status | Description |
|------|--------|-------------|
| `rust/src/bin/tea.rs` | Modified | Added --workflow arg to resume, implemented full resume execution |
| `rust/tests/test_cli.rs` | Created | 14 E2E tests for CLI commands |
| `rust/tests/fixtures/simple_workflow.yaml` | Created | Test fixture for basic workflow |
| `rust/tests/fixtures/interruptible_workflow.yaml` | Created | Test fixture with interrupt points |
| `rust/tests/fixtures/invalid_workflow.yaml` | Created | Test fixture for validation errors |

---

## QA Results

### Review Date: 2025-12-20 (Re-review after dev completion)

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

The CLI implementation in `rust/src/bin/tea.rs` (488 lines) is well-structured and complete. All previous concerns have been addressed.

**Implementation Quality:**
- Clean clap derive-based CLI structure with proper global flags
- Good error handling using `anyhow::Context` for user-friendly error messages
- Proper logging integration with tracing-subscriber and configurable verbosity
- Input parsing supports both inline JSON and @file.json notation
- Inspect command provides multiple output formats (text, JSON, DOT)
- **Resume command now fully functional with `--workflow` argument**

**AC-19 Implementation Review (lines 239-319):**
- ✅ Loads checkpoint from file path
- ✅ Merges state updates via `checkpoint.merge_state(&updates)`
- ✅ Loads workflow from `--workflow` argument
- ✅ Creates executor with action registry
- ✅ Executes with `ExecutionOptions` including `resume_from`
- ✅ Handles both stream and non-stream output modes
- ✅ Proper interrupt handling on continued execution

### Compliance Check

- Coding Standards: ✅ Code passes `cargo fmt` and `cargo clippy` with no warnings
- Project Structure: ✅ Binary correctly placed in `src/bin/tea.rs` per development guide
- Testing Strategy: ✅ **14 E2E tests in `tests/test_cli.rs` - All passing**
- All ACs Met: ✅ **All acceptance criteria satisfied**

### Requirements Traceability

| AC | Test Coverage | Status |
|----|--------------|--------|
| AC-17 (run) | `test_run_with_input`, `test_run_without_input` | ✅ PASS |
| AC-18 (stream) | `test_run_stream_mode` | ✅ PASS |
| AC-19 (resume) | `test_resume_with_workflow` | ✅ PASS |
| AC-20 (validate) | `test_validate_valid_workflow`, `test_validate_detailed`, `test_validate_invalid_workflow` | ✅ PASS |
| AC-5 (inspect) | `test_inspect_text_format`, `test_inspect_json_format`, `test_inspect_dot_format` | ✅ PASS |
| AC-6/7 (flags) | `test_quiet_flag`, `test_help`, `test_version` | ✅ PASS |

### Test Quality Assessment

The E2E test suite is comprehensive:
- **Happy path tests**: run, stream, validate, inspect, resume
- **Error handling tests**: `test_run_missing_file`, `test_validate_invalid_workflow`
- **Full integration test**: `test_resume_with_workflow` tests the complete interrupt-checkpoint-resume flow
- Uses `tempfile` for isolated checkpoint testing
- Validates both stdout and exit codes

### Improvements Checklist

**Previously required - NOW COMPLETE:**

- [x] **Complete resume execution (AC-19)**: Added `--workflow` argument, full execution implemented
- [x] **Add E2E tests for CLI** (Task 6): 14 comprehensive tests covering all commands

**Nice-to-have (deferred):**

- [ ] Store workflow file path in checkpoint metadata for workflow-less resume
- [ ] Consider adding `--help-examples` subcommand showing usage examples

### Security Review

No security concerns identified:
- File paths are properly handled with std::fs
- JSON parsing uses serde_json safely
- No shell command injection vectors
- User input is properly validated before use

### Performance Considerations

No performance issues identified:
- Workflow execution is handled by existing Executor
- File I/O is minimal and appropriate
- No unnecessary allocations or clones in hot paths

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-RUST-013-cli-binary.yml

| Issue ID | Severity | Status |
|----------|----------|--------|
| AC-001 | ~~high~~ | ✅ **RESOLVED** - AC-19 fully implemented |
| TEST-001 | ~~medium~~ | ✅ **RESOLVED** - 14 E2E tests passing |

### Recommended Status

**✅ Ready for Done**

All acceptance criteria are satisfied. All tasks complete. All tests passing.
