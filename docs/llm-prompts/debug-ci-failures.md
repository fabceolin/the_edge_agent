# Debugging CI Failures with GitHub CLI

This guide documents how to debug CI failures using the `gh` CLI tool.

## Quick Commands

### 1. Fetch Failed Logs

```bash
# Get failed job logs for a specific run
gh run view <RUN_ID> --repo <OWNER>/<REPO> --log-failed

# Example:
gh run view 20886757065 --repo fabceolin/the_edge_agent --log-failed
```

### 2. Get Job Metadata

```bash
# Get structured JSON with job details
gh run view <RUN_ID> --repo <OWNER>/<REPO> --json jobs,conclusion,status,name,headBranch
```

### 3. Get All Logs

```bash
# Get complete logs (all steps)
gh run view <RUN_ID> --repo <OWNER>/<REPO> --log

# Download logs as zip
gh run download <RUN_ID> --repo <OWNER>/<REPO>
```

## Common CI Failure Patterns

### Rust Documentation Errors (`cargo doc`)

**Error Pattern:**
```
error: this URL is not a hyperlink
   --> src/engine/observability.rs:159:49
    |
159 |     /// Override the Opik API URL (defaults to "https://www.comet.com/opik/api")
    |                                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |
    = note: bare URLs are not automatically turned into clickable links
    = note: `-D rustdoc::bare-urls` implied by `-D warnings`
```

**Fix:** Wrap bare URLs in angle brackets `<URL>` or backticks `` `URL` ``:
```rust
// Before (fails with -D warnings)
/// Override the API URL (defaults to "https://example.com/api")

// After (fixed)
/// Override the API URL (defaults to `https://example.com/api`)
// or
/// Override the API URL (defaults to <https://example.com/api>)
```

**Verify locally:**
```bash
cd rust && RUSTDOCFLAGS="-D warnings" cargo doc --no-deps
```

### Rust Clippy Warnings

```bash
# Run clippy locally with same settings as CI
cd rust && cargo clippy --all-targets --all-features -- -D warnings
```

### Rust Formatting Errors

```bash
# Check formatting
cd rust && cargo fmt --check

# Fix formatting
cd rust && cargo fmt
```

### Rust Test Failures

```bash
# Run tests with same configuration
cd rust && cargo test

# Run specific test
cd rust && cargo test test_name
```

## Workflow Analysis

When debugging CI failures:

1. **Identify the failing step** from the JSON metadata
2. **Extract the exact command** from the logs (look for `##[group]Run`)
3. **Reproduce locally** with the same environment variables
4. **Check for environment differences** (Rust version, feature flags, etc.)

## Environment Variables Used in CI

The Rust CI workflow uses:
```bash
CARGO_INCREMENTAL=0
CARGO_TERM_COLOR=always
RUSTDOCFLAGS="-D warnings"  # Treat doc warnings as errors
```

## Debugging Tips

1. **Scroll to the actual error** - CI logs can be verbose. Look for `error:` or `Error:` lines
2. **Check the step name** - The JSON output shows which step failed
3. **Compare with previous runs** - Use `gh run list` to find passing runs
4. **Check branch differences** - Failed runs often come from new commits

```bash
# List recent runs
gh run list --repo <OWNER>/<REPO> --limit 10

# Compare with last passing run
gh run view <PASSING_RUN_ID> --repo <OWNER>/<REPO> --json jobs
```
