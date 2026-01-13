# TEA-REPORT-001: Automatic Bug Reporting System (Epic)

## Status

**Ready for Development** - All QA validation criteria passed (2026-01-12)

- Test design complete: 78 scenarios covering all 39 ACs
- No coverage gaps identified
- Privacy and parity risks properly mitigated with P0 tests

## Child Stories

| ID | Title | Effort | Status |
|----|-------|--------|--------|
| **TEA-REPORT-001a** | [Error Capture Protocol - Core](TEA-REPORT-001a-error-capture-protocol.md) | ~4h | Draft |
| **TEA-REPORT-001b** | [URL Encoder/Decoder Library](TEA-REPORT-001b-url-encoder-decoder.md) | ~3h | Draft |
| **TEA-REPORT-001c** | [GitHub Pages Report Viewer](TEA-REPORT-001c-github-pages-viewer.md) | ~4h | Draft |
| **TEA-REPORT-001d** | [CLI Integration & UX](TEA-REPORT-001d-cli-integration.md) | ~3h | Draft |
| **TEA-REPORT-001e** | [Cross-Runtime Parity Tests](TEA-REPORT-001e-parity-tests.md) | ~2h | Draft |

## Execution Order

```
TEA-REPORT-001a (Error Capture Protocol)
       │
       ▼
TEA-REPORT-001b (URL Encoder/Decoder)
       │
       ├──────────────────┐
       ▼                  ▼
TEA-REPORT-001c      TEA-REPORT-001d
(GitHub Pages)       (CLI Integration)
       │                  │
       └────────┬─────────┘
                ▼
        TEA-REPORT-001e
        (Parity Tests)
```

---

## Story

**As a** tea user encountering crashes, errors, or exceptions,
**I want** an automatic bug reporting system similar to bun.report,
**So that** I can report issues to GitHub with one click, without exposing any personal or sensitive information.

## Story Context

### Existing System Integration

| Aspect | Details |
|--------|---------|
| **Integrates with** | `tea` CLI (Rust and Python), error handling, panic hooks |
| **Technology** | Rust (panic handler, serde), Python (sys.excepthook), GitHub Pages (static JS) |
| **Follows pattern** | bun.report architecture - compressed URLs, client-side decoding |
| **Touch points** | CLI error handling, executor exceptions, YAML engine errors |

### Reference Implementation: bun.report

The Bun project provides an excellent reference at [bun.report](https://bun.report/):

**How it works:**
1. **Crash occurs** → Bun's panic handler captures stack trace, version, platform
2. **Encoding** → VLQ-encodes addresses, deflate-compresses panic message
3. **URL generation** → Creates URL like `{org}.github.io/the_edge_agent/report/{version}/{encoded_data}`
4. **Client-side decode** → JavaScript in browser decodes and remaps addresses
5. **GitHub integration** → "File issue on GitHub" button with pre-populated content

**Key privacy features:**
- No source code in URLs
- No personally-identifiable information (PII)
- Stack addresses only (remapped client-side)
- Version and platform metadata only

### Current Behavior vs Desired Behavior

| Aspect | Current | Desired |
|--------|---------|---------|
| On crash | Stack trace to stderr, exit code | Generate report URL, display to user |
| On error | Exception message only | Capture context, generate report URL |
| Reporting | Manual issue creation | One-click GitHub issue with full context |
| Privacy | N/A | Zero PII, compressed encoding |

## Acceptance Criteria

### Core Error Capture (TEA-REPORT-001a)

- [ ] **AC-1**: Rust panic hook captures: version, platform, architecture, stack addresses, panic message
- [ ] **AC-2**: Python excepthook captures: version, platform, architecture, traceback, exception message
- [ ] **AC-3**: YAML engine errors captured with: node name, action type, error kind
- [ ] **AC-4**: Executor errors captured with: checkpoint context (ID only), interrupted state (without data)
- [ ] **AC-5**: All captures exclude PII: no file contents, no state data, no user paths

### URL Encoding (TEA-REPORT-001b)

- [ ] **AC-6**: VLQ encoding for numeric values (stack addresses, line numbers)
- [ ] **AC-7**: Deflate compression for text data (panic message, traceback)
- [ ] **AC-8**: Base64url encoding for URL-safe output
- [ ] **AC-9**: URL format: `https://{org}.github.io/the_edge_agent/report/{version}/{encoded_data}`
- [ ] **AC-10**: Encoded URL length under 2000 characters (browser limit)
- [ ] **AC-11**: Rust and Python encoders produce identical output for same input

### GitHub Pages Viewer (TEA-REPORT-001c)

- [ ] **AC-12**: Report viewer section added to existing GitHub Pages site at `{org}.github.io/the_edge_agent/report/`
- [ ] **AC-13**: JavaScript decodes URL parameters (VLQ decode, inflate, base64url decode)
- [ ] **AC-14**: Display decoded error info: version, platform, stack trace, error message
- [ ] **AC-15**: Source remapping using debug symbols or source maps (optional, if available)
- [ ] **AC-16**: "File issue on GitHub" button with pre-populated title and body
- [ ] **AC-17**: Check for existing issues with similar stack traces before filing
- [ ] **AC-18**: Mobile-friendly responsive design

### CLI Integration (TEA-REPORT-001d)

- [ ] **AC-19**: On crash/error, display report URL to user
- [ ] **AC-20**: `--report-bugs` flag to enable automatic URL generation (default: enabled)
- [ ] **AC-21**: `--no-report-bugs` flag to disable (for privacy-conscious users)
- [ ] **AC-22**: Environment variable `TEA_REPORT_BUGS=false` to disable globally
- [ ] **AC-23**: Clear message: "Report this bug: {URL}" with explanation of what's included
- [ ] **AC-24**: Option to copy URL to clipboard (if terminal supports it)

### Opt-in Extended Context (TEA-REPORT-001d - Extended)

- [ ] **AC-28**: After displaying minimal URL, prompt: "Include more context? [y/N]"
- [ ] **AC-29**: If user opts in, generate extended URL with additional data
- [ ] **AC-30**: Extended context includes (user-approved): sanitized YAML structure, node names, action types
- [ ] **AC-31**: Extended context excludes (always): state data, secrets, environment variables, file contents
- [ ] **AC-32**: `--report-extended` flag to auto-include extended context (skip prompt)
- [ ] **AC-33**: `--report-minimal` flag to skip extended prompt entirely
- [ ] **AC-34**: Extended URL clearly marked as containing more information in the viewer

### Spam Prevention (TEA-REPORT-001c - Simplified)

- [ ] **AC-35**: "File issue" button redirects to GitHub's native issue creation page
- [ ] **AC-36**: User must be logged into GitHub to submit (implicit authentication)
- [ ] **AC-37**: Issue template pre-fills with decoded report data
- [ ] **AC-38**: Search existing issues for similar stack traces before "File issue"
- [ ] **AC-39**: If similar issue exists, show link instead of "File new issue"

**Note:** Following bun.report's model - GitHub's native authentication handles spam prevention.
No custom HMAC, rate limiting, or CAPTCHA needed since users must authenticate with GitHub.

### Cross-Runtime Parity (TEA-REPORT-001e)

- [ ] **AC-25**: Rust and Python produce identical URLs for equivalent errors
- [ ] **AC-26**: Same error types have same capture structure
- [ ] **AC-27**: Integration tests verify cross-runtime parity

## Technical Notes

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                        User's Machine                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌────────────────┐    ┌────────────────┐                       │
│  │   Rust CLI     │    │   Python CLI   │                       │
│  │  (tea-rust)    │    │  (tea-python)  │                       │
│  └───────┬────────┘    └───────┬────────┘                       │
│          │                     │                                 │
│          ▼                     ▼                                 │
│  ┌────────────────────────────────────────┐                     │
│  │         Error Capture Layer            │                     │
│  │  - Panic handler (Rust)                │                     │
│  │  - Exception hook (Python)             │                     │
│  │  - YAML engine error wrapper           │                     │
│  └───────────────────┬────────────────────┘                     │
│                      │                                           │
│                      ▼                                           │
│  ┌────────────────────────────────────────┐                     │
│  │         URL Encoder                    │                     │
│  │  - ErrorReport struct/dataclass        │                     │
│  │  - VLQ encode numbers                  │                     │
│  │  - Deflate compress text               │                     │
│  │  - Base64url encode                    │                     │
│  └───────────────────┬────────────────────┘                     │
│                      │                                           │
│                      ▼                                           │
│  ┌────────────────────────────────────────┐                     │
│  │  "Report this bug: https://..."        │                     │
│  │  (displayed to user in terminal)       │                     │
│  └────────────────────────────────────────┘                     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘

                              │
                              │ User clicks URL
                              ▼

┌─────────────────────────────────────────────────────────────────┐
│                  GitHub Pages (Static Site)                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌────────────────────────────────────────┐                     │
│  │         URL Decoder (JavaScript)       │                     │
│  │  - Parse URL path                      │                     │
│  │  - Base64url decode                    │                     │
│  │  - Inflate decompress                  │                     │
│  │  - VLQ decode numbers                  │                     │
│  └───────────────────┬────────────────────┘                     │
│                      │                                           │
│                      ▼                                           │
│  ┌────────────────────────────────────────┐                     │
│  │         Error Display UI               │                     │
│  │  - Version & platform info             │                     │
│  │  - Stack trace (remapped if possible)  │                     │
│  │  - Error message                       │                     │
│  └───────────────────┬────────────────────┘                     │
│                      │                                           │
│                      ▼                                           │
│  ┌────────────────────────────────────────┐                     │
│  │  [Search existing issues] [File new]   │                     │
│  └────────────────────────────────────────┘                     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Error Report Structure

```rust
// Rust
#[derive(Serialize)]
struct ErrorReport {
    version: String,           // e.g., "0.9.34"
    platform: String,          // e.g., "linux-x86_64"
    runtime: String,           // "rust" or "python"
    error_type: ErrorType,     // Panic, Exception, YamlError, ExecutorError
    message: String,           // Error message (truncated if too long)
    stack: Vec<StackFrame>,    // Stack frames
    context: Option<ErrorContext>, // Additional context (node, action, etc.)
    extended: Option<ExtendedContext>, // Opt-in additional context
}

#[derive(Serialize)]
struct ExtendedContext {
    workflow_name: Option<String>,    // Name from YAML
    nodes: Vec<NodeInfo>,              // Node names and types
    edges: Vec<EdgeInfo>,              // Edge topology
    schema_fields: Vec<String>,        // Field names only (no values)
    active_node: Option<String>,       // Node where error occurred
    active_action: Option<String>,     // Action type where error occurred
}

#[derive(Serialize)]
struct NodeInfo {
    name: String,
    action_type: Option<String>,  // "llm_call", "transform", etc.
}

#[derive(Serialize)]
struct EdgeInfo {
    from: String,
    to: String,
    condition: Option<String>,    // "conditional", "parallel", etc. (type only)
}

#[derive(Serialize)]
struct StackFrame {
    addr: u64,                 // Memory address (for remapping)
    symbol: Option<String>,    // Function name (if available)
    file: Option<String>,      // Relative path only (no absolute paths)
    line: Option<u32>,         // Line number
}

enum ErrorType {
    Panic,          // Rust panic or Python unhandled exception
    YamlError,      // YAML parsing or validation error
    ExecutorError,  // Runtime execution error
    ActionError,    // Action execution failure
}
```

```python
# Python
@dataclass
class ErrorReport:
    version: str
    platform: str
    runtime: str = "python"
    error_type: str
    message: str
    stack: list[StackFrame]
    context: Optional[ErrorContext] = None

@dataclass
class StackFrame:
    addr: int
    symbol: Optional[str] = None
    file: Optional[str] = None  # Relative path only
    line: Optional[int] = None
```

### URL Encoding Algorithm

```
1. Serialize ErrorReport to JSON
2. Deflate compress (zlib level 9)
3. Base64url encode (no padding)
4. Build URL: https://{org}.github.io/tea-report/{version}/{runtime}_{encoded}
```

**VLQ (Variable-Length Quantity) for numbers:**
- Used for compact encoding of stack addresses and line numbers
- Same algorithm as source maps

**URL length budget (2000 chars max):**
- Path: ~50 chars (`/tea-report/0.9.34/python_`)
- Encoded data: ~1950 chars
- With compression, this allows ~4KB of raw data

### Privacy Checklist

**Minimal Report (default):**

| Data Type | Included | Excluded |
|-----------|----------|----------|
| TEA version | ✅ | |
| Platform/arch | ✅ | |
| Stack addresses | ✅ | |
| Function names | ✅ | |
| Relative file paths | ✅ | |
| Line numbers | ✅ | |
| Error message | ✅ (sanitized) | |
| Absolute paths | | ❌ |
| State data | | ❌ |
| YAML contents | | ❌ |
| Environment variables | | ❌ |
| User identity | | ❌ |
| Checkpoint data | | ❌ |

**Extended Report (opt-in only):**

| Data Type | Included | Excluded |
|-----------|----------|----------|
| All minimal data | ✅ | |
| YAML structure (sanitized) | ✅ | |
| Node names | ✅ | |
| Action types | ✅ | |
| Edge topology | ✅ | |
| Schema field names | ✅ | |
| Schema field values | | ❌ |
| State data | | ❌ |
| Secrets/credentials | | ❌ |
| Environment variables | | ❌ |
| LLM prompts/responses | | ❌ |

### Spam Prevention Strategy (bun.report model)

Following the same approach as bun.report:

```
┌─────────────────────────────────────────────────────────────────┐
│                    Spam Prevention Flow                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1. USER CLICKS "File Issue" BUTTON                             │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │  {org}.github.io/the_edge_agent/report page decodes and displays error info           │ │
│  │  User clicks "File issue on GitHub"                        │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                  │
│  2. REDIRECT TO GITHUB (implicit auth)                          │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │  URL: github.com/{org}/{repo}/issues/new?title=...&body=...│ │
│  │  User MUST be logged into GitHub to proceed                │ │
│  │  GitHub handles spam prevention via account requirements   │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                  │
│  3. DUPLICATE CHECK (before showing "File issue")               │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │  JavaScript searches GitHub API for similar issues         │ │
│  │  If match found → show "Similar issue: #123" link          │ │
│  │  If no match → show "File new issue" button                │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

**Why this works:**
1. **GitHub authentication required** - Spammers need valid GitHub accounts
2. **GitHub's own spam detection** - They have rate limits and abuse detection
3. **Duplicate check** - Reduces noise from repeated reports
4. **No custom auth** - Simpler implementation, less maintenance

### Key Constraints

1. **No external services for encoding** - All encoding happens locally
2. **No network requests on error** - URL is generated offline
3. **URL must be copy-pasteable** - No special characters that break terminals
4. **Graceful degradation** - If encoding fails, still show raw error
5. **No backend server required** - Static GitHub Pages site only

## Definition of Done

- [ ] All acceptance criteria met (AC-1 through AC-39)
- [ ] Zero PII in any generated URLs
- [ ] Rust and Python implementations produce identical URLs
- [ ] GitHub Pages site deployed and functional
- [ ] Integration tests for all error types
- [ ] Duplicate issue search working
- [ ] Documentation in README and `--help`
- [ ] Privacy policy/notice in report viewer

## Tasks / Subtasks

### Phase 1: Error Capture Protocol (TEA-REPORT-001a)

- [ ] **Task 1.1**: Define `ErrorReport` struct in Rust (AC-1)
- [ ] **Task 1.2**: Define `ErrorReport` dataclass in Python (AC-2)
- [ ] **Task 1.3**: Implement Rust panic hook to capture ErrorReport (AC-1)
- [ ] **Task 1.4**: Implement Python excepthook to capture ErrorReport (AC-2)
- [ ] **Task 1.5**: Wrap YAML engine errors (AC-3)
- [ ] **Task 1.6**: Wrap executor errors (AC-4)
- [ ] **Task 1.7**: Add path sanitization (relative paths only) (AC-5)

### Phase 2: URL Encoder/Decoder (TEA-REPORT-001b)

- [ ] **Task 2.1**: Implement VLQ encoder in Rust
- [ ] **Task 2.2**: Implement VLQ encoder in Python
- [ ] **Task 2.3**: Implement deflate compression wrapper
- [ ] **Task 2.4**: Implement Base64url encoding (AC-8)
- [ ] **Task 2.5**: Implement `encode_error_report()` function (AC-6, AC-7)
- [ ] **Task 2.6**: Add URL length validation and truncation (AC-10)
- [ ] **Task 2.7**: Cross-runtime encoder parity tests (AC-11)

### Phase 3: GitHub Pages Viewer (TEA-REPORT-001c)

- [ ] **Task 3.1**: Add `/report/` page to existing Sphinx docs (docs/extra/report/ or as custom Sphinx page)
- [ ] **Task 3.2**: Implement JavaScript VLQ decoder (AC-13)
- [ ] **Task 3.3**: Implement JavaScript inflate (pako library) (AC-13)
- [ ] **Task 3.4**: Build error display UI (AC-14)
- [ ] **Task 3.5**: Add source remapping (optional) (AC-15)
- [ ] **Task 3.6**: Implement GitHub issue search (AC-17)
- [ ] **Task 3.7**: Implement "File issue" redirect to GitHub's native issue page (AC-35, AC-36, AC-37)
- [ ] **Task 3.8**: Add responsive design (AC-18)
- [ ] **Task 3.9**: Implement duplicate issue search using GitHub API (AC-38, AC-39)

### Phase 4: CLI Integration (TEA-REPORT-001d)

- [ ] **Task 4.1**: Add `--report-bugs` / `--no-report-bugs` flags (AC-20, AC-21)
- [ ] **Task 4.2**: Add `TEA_REPORT_BUGS` environment variable support (AC-22)
- [ ] **Task 4.3**: Integrate error capture into CLI error handling (AC-19)
- [ ] **Task 4.4**: Format and display report URL to user (AC-23)
- [ ] **Task 4.5**: Add clipboard copy support (AC-24)
- [ ] **Task 4.6**: Implement extended context prompt "[y/N]" (AC-28)
- [ ] **Task 4.7**: Implement `ExtendedContext` capture from YAML engine (AC-29, AC-30)
- [ ] **Task 4.8**: Add `--report-extended` flag (AC-32)
- [ ] **Task 4.9**: Add `--report-minimal` flag (AC-33)
- [ ] **Task 4.10**: Ensure extended context never includes sensitive data (AC-31)

### Phase 5: Parity Tests (TEA-REPORT-001e)

- [ ] **Task 5.1**: Create test fixtures for each error type
- [ ] **Task 5.2**: Implement cross-runtime parity tests (AC-25, AC-26)
- [ ] **Task 5.3**: Add integration tests for full flow (AC-27)

## Dev Notes

### Testing

| Test Type | Location | Description |
|-----------|----------|-------------|
| Unit (Rust) | `rust/src/report/tests.rs` | Encoder, capture |
| Unit (Python) | `python/tests/test_report.py` | Encoder, capture |
| Integration | `tests/report/` | Full error → URL → decode |
| Parity | `tests/report/parity/` | Same error, same URL |
| E2E | GitHub Pages | Manual verification |

### Example Generated URL

```
https://{org}.github.io/the_edge_agent/report/0.9.34/rust_eJxLzs8tyM9LzSvOzEktTlSwUFDIL0otLlGwUijIL0ktAgBNawpQ
```

Decoded to:
```json
{
  "version": "0.9.34",
  "platform": "linux-x86_64",
  "runtime": "rust",
  "error_type": "Panic",
  "message": "called `Option::unwrap()` on a `None` value",
  "stack": [
    {"addr": 4195432, "symbol": "tea::executor::run", "file": "src/executor.rs", "line": 142},
    {"addr": 4195100, "symbol": "tea::main", "file": "src/main.rs", "line": 45}
  ]
}
```

### Example User Experience

**Minimal report (default):**
```
$ tea run workflow.yaml

thread 'main' panicked at src/executor.rs:142:
called `Option::unwrap()` on a `None` value

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
🐛 Report this bug:
   https://{org}.github.io/the_edge_agent/report/0.9.34/rust_eJxLzs8t...

   This URL contains only: version, platform, and stack trace.
   No personal information or file contents are included.

   Include more context to help diagnose? [y/N]:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

**If user types 'y' (opt-in for extended context):**
```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
🐛 Extended report (includes workflow structure):
   https://{org}.github.io/the_edge_agent/report/0.9.34/rust_ext_aGVsbG8...

   Additional info included:
   ✓ Node names: process_data, validate, finalize
   ✓ Action types: llm_call, transform, conditional
   ✓ Graph structure (edges between nodes)

   Still excluded: state data, secrets, prompts, file contents
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

**Skip prompt with flag:**
```
$ tea run workflow.yaml --report-extended
# Automatically includes extended context without prompting

$ tea run workflow.yaml --report-minimal
# Never prompts for extended context
```

## Risk and Compatibility Check

### Risk Assessment

| Risk | Impact | Mitigation | Rollback |
|------|--------|------------|----------|
| **URL too long** | Medium | Truncate stack, compress more | Show raw error instead |
| **Encoding mismatch Rust/Python** | High | Extensive parity tests | Fix encoder |
| **GitHub Pages unavailable** | Low | URL still shows locally | Manual issue filing |
| **Privacy leak** | Critical | Audit all captured data, no absolute paths | Remove feature |

### Compatibility Verification

- [x] No breaking changes to existing CLI
- [x] Feature is opt-out (enabled by default, can disable)
- [x] Works offline (URL generation is local)
- [x] No new dependencies for core functionality

## Related Stories

- **TEA-CLI-005**: Interactive HITL Mode (similar UX patterns)
- **TEA-OBS-001**: Observability (error capture synergies)

## References

- [bun.report source](https://github.com/oven-sh/bun/tree/main/src/crash_report) - Reference implementation
- [VLQ encoding](https://en.wikipedia.org/wiki/Variable-length_quantity) - Compact number encoding
- [Source maps spec](https://sourcemaps.info/spec.html) - VLQ usage in practice

## QA Notes

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 78 |
| **Unit tests** | 32 (41%) |
| **Integration tests** | 28 (36%) |
| **E2E tests** | 18 (23%) |
| **Priority distribution** | P0: 24, P1: 32, P2: 16, P3: 6 |
| **AC coverage** | 39/39 (100%) |

### Risk Areas Identified

| Risk | Severity | Mitigating Tests | Notes |
|------|----------|------------------|-------|
| **Privacy leak** | Critical | 7 tests (UNIT-011/012/013, INT-009/010/032, E2E-001) | Zero PII must leak into URLs - highest priority |
| **Encoding mismatch Rust/Python** | High | 8 tests (INT-013/014/015, INT-035-039, E2E-018) | Cross-runtime parity is core requirement |
| **URL too long (>2000 chars)** | Medium | 3 tests (UNIT-026/027, INT-012) | Browser limit enforcement |
| **GitHub Pages unavailable** | Low | E2E-008 | Graceful degradation tested |

### Recommended Test Scenarios

**P0 - Must Pass (24 scenarios)**
- Privacy validation: Path sanitization, PII exclusion, env var exclusion
- Parity testing: Identical URL output from Rust and Python for same input
- Core capture: ErrorReport struct population in both runtimes
- URL encoding: VLQ, deflate, base64url round-trip correctness

**P1 - Should Pass (32 scenarios)**
- CLI integration: Report URL display on crash/error
- Extended context: Opt-in workflow, flag behavior
- GitHub viewer: Decode pipeline, issue creation flow
- Duplicate detection: Similar issue search

**P2/P3 - Nice to Have (22 scenarios)**
- Mobile responsiveness, accessibility
- Clipboard copy, graceful degradation for optional features

### Key Quality Observations

1. **Strategy Rationale**: Epic implements cross-runtime (Rust + Python) bug reporting following bun.report architecture. Testing strategy correctly emphasizes parity testing, privacy validation, and encoding correctness.

2. **Coverage Completeness**: All 39 Acceptance Criteria have at least one test scenario. No coverage gaps identified.

3. **Test Level Appropriateness**:
   - Unit tests for pure logic (encoding, sanitization)
   - Integration tests for cross-component flows (parity, error capture)
   - E2E tests for full user flows (viewer, issue creation)

### Concerns or Blockers

- **None identified** - Test design is comprehensive and well-structured

### Recommended Execution Order

1. **Phase 1 (CI on every commit)**: P0/P1 Unit tests - fast feedback
2. **Phase 2 (CI on PR)**: P0/P1 Integration tests - parity and privacy validation
3. **Phase 3 (Pre-release)**: All E2E tests - full flow verification

### Test Design Reference

```
Assessment: docs/qa/assessments/TEA-REPORT-001-test-design-20260112.md
Designer: Quinn (Test Architect)
Date: 2026-01-12
```

---

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2025-01-11 | 1.0 | Initial epic creation | Sarah (PO Agent) |
| 2026-01-12 | 1.1 | Added QA Notes section | Quinn (Test Architect) |
