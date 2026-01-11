# TEA-REPORT-001b: URL Encoder/Decoder Library

## Status

**Done** - QA Gate PASS (2026-01-11)

## Parent Epic

[TEA-REPORT-001: Automatic Bug Reporting System](TEA-REPORT-001-automatic-bug-reporting.md)

## Dependencies

- **TEA-REPORT-001a** (Error Capture Protocol) - Must be completed first

---

## Story

**As a** tea developer,
**I want** a URL encoding library that compresses ErrorReport into a short URL,
**So that** users can share bug reports by copying a single URL from the terminal.

## Story Context

### Existing System Integration

| Aspect | Details |
|--------|---------|
| **Integrates with** | ErrorReport from TEA-REPORT-001a |
| **Technology** | Rust (flate2, base64), Python (zlib, base64) |
| **Follows pattern** | bun.report URL encoding (VLQ + deflate + base64url) |
| **Touch points** | New module in both runtimes |

## Acceptance Criteria

- [x] **AC-6**: VLQ encoding for numeric values (stack addresses, line numbers)
- [x] **AC-7**: Deflate compression for text data (panic message, traceback)
- [x] **AC-8**: Base64url encoding for URL-safe output
- [x] **AC-9**: URL format: `https://{org}.github.io/the_edge_agent/report/{version}/{encoded_data}`
- [x] **AC-10**: Encoded URL length under 2000 characters (browser limit)
- [x] **AC-11**: Rust and Python encoders produce identical output for same input

## Tasks / Subtasks

### Core Encoding (Rust)

- [x] **Task 1.1**: Implement VLQ encoder
  - [x] Encode u64 values to variable-length bytes
  - [x] Match source map VLQ specification

- [x] **Task 1.2**: Implement deflate compression wrapper
  - [x] Use flate2 crate with compression level 9
  - [x] Handle compression errors gracefully

- [x] **Task 1.3**: Implement Base64url encoder
  - [x] URL-safe alphabet (+ → -, / → _)
  - [x] No padding (strip =)

- [x] **Task 1.4**: Implement `encode_error_report()` function
  - [x] Serialize ErrorReport to JSON
  - [x] Deflate compress
  - [x] Base64url encode
  - [x] Build final URL

- [x] **Task 1.5**: Implement URL length validation
  - [x] If > 2000 chars, truncate stack frames
  - [x] Re-encode and retry
  - [x] Minimum: version + platform + error message

### Core Encoding (Python)

- [x] **Task 2.1**: Implement VLQ encoder
  - [x] Match Rust implementation exactly

- [x] **Task 2.2**: Implement deflate compression wrapper
  - [x] Use zlib with compression level 9

- [x] **Task 2.3**: Implement Base64url encoder
  - [x] Use `base64.urlsafe_b64encode`
  - [x] Strip padding

- [x] **Task 2.4**: Implement `encode_error_report()` function
  - [x] Match Rust implementation exactly

- [x] **Task 2.5**: Implement URL length validation
  - [x] Same truncation logic as Rust

### Decoding (for testing/viewer)

- [x] **Task 3.1**: Implement decoder in Rust (for testing)
- [x] **Task 3.2**: Implement decoder in Python (for testing)
- [x] **Task 3.3**: Verify round-trip: encode → decode → same data

### Testing

- [x] **Task 4.1**: Unit tests for VLQ encoding (known values)
- [x] **Task 4.2**: Unit tests for compression (round-trip)
- [x] **Task 4.3**: Unit tests for Base64url (round-trip)
- [x] **Task 4.4**: Integration test: full encode/decode cycle
- [x] **Task 4.5**: Cross-runtime parity test: same input → same URL

## Technical Notes

### URL Format

```
https://{org}.github.io/the_edge_agent/report/{version}/{runtime}_{encoded_data}

Examples:
- https://example.github.io/the_edge_agent/report/0.9.34/rust_eJxLzs8tyM9LzSvOzEkt...
- https://example.github.io/the_edge_agent/report/0.9.34/python_aGVsbG8gd29ybGQ...
```

**URL parts:**
- `{version}`: TEA version (e.g., "0.9.34")
- `{runtime}`: "rust" or "python"
- `{encoded_data}`: Base64url encoded, deflate compressed JSON

### VLQ Encoding

Variable-Length Quantity encoding for compact number representation:

```rust
// Rust
pub fn vlq_encode(mut value: u64) -> Vec<u8> {
    let mut result = Vec::new();
    loop {
        let mut byte = (value & 0x7F) as u8;
        value >>= 7;
        if value != 0 {
            byte |= 0x80;
        }
        result.push(byte);
        if value == 0 {
            break;
        }
    }
    result
}

pub fn vlq_decode(bytes: &[u8]) -> (u64, usize) {
    let mut value: u64 = 0;
    let mut shift = 0;
    let mut consumed = 0;
    for &byte in bytes {
        consumed += 1;
        value |= ((byte & 0x7F) as u64) << shift;
        if byte & 0x80 == 0 {
            break;
        }
        shift += 7;
    }
    (value, consumed)
}
```

```python
# Python
def vlq_encode(value: int) -> bytes:
    result = bytearray()
    while True:
        byte = value & 0x7F
        value >>= 7
        if value != 0:
            byte |= 0x80
        result.append(byte)
        if value == 0:
            break
    return bytes(result)

def vlq_decode(data: bytes) -> tuple[int, int]:
    value = 0
    shift = 0
    consumed = 0
    for byte in data:
        consumed += 1
        value |= (byte & 0x7F) << shift
        if byte & 0x80 == 0:
            break
        shift += 7
    return value, consumed
```

### Full Encoding Pipeline

```python
# Python implementation
import zlib
import base64
import json

def encode_error_report(report: ErrorReport, base_url: str) -> str:
    """Encode ErrorReport to shareable URL."""

    # 1. Serialize to JSON
    json_str = report.to_json()
    json_bytes = json_str.encode('utf-8')

    # 2. Deflate compress (level 9)
    compressed = zlib.compress(json_bytes, level=9)

    # 3. Base64url encode (no padding)
    encoded = base64.urlsafe_b64encode(compressed).rstrip(b'=').decode('ascii')

    # 4. Build URL
    url = f"{base_url}/{report.version}/{report.runtime}_{encoded}"

    # 5. Validate length
    if len(url) > 2000:
        # Truncate stack and retry
        report = truncate_stack(report)
        return encode_error_report(report, base_url)

    return url

def decode_error_report(url: str) -> ErrorReport:
    """Decode URL back to ErrorReport."""

    # Parse URL
    parts = url.split('/')
    version = parts[-2]
    runtime_encoded = parts[-1]
    runtime, encoded = runtime_encoded.split('_', 1)

    # Add padding back
    padding = 4 - (len(encoded) % 4)
    if padding != 4:
        encoded += '=' * padding

    # Base64url decode
    compressed = base64.urlsafe_b64decode(encoded)

    # Inflate decompress
    json_bytes = zlib.decompress(compressed)

    # Parse JSON
    data = json.loads(json_bytes.decode('utf-8'))

    return ErrorReport(**data)
```

### URL Length Budget

| Component | Typical Size |
|-----------|--------------|
| Base URL | ~60 chars |
| Version | ~10 chars |
| Runtime prefix | ~7 chars |
| **Available for data** | **~1920 chars** |

With Base64 expansion (~4/3) and compression (~50% reduction):
- 1920 Base64 chars → ~1440 compressed bytes → ~2880 uncompressed bytes

This allows for ~2-3KB of JSON data, sufficient for:
- 10-20 stack frames
- Error message up to 500 chars
- Full context and extended info

### Truncation Strategy

When URL exceeds 2000 chars:

1. Remove extended context first
2. Reduce stack to 10 frames
3. Reduce stack to 5 frames
4. Truncate error message to 200 chars
5. Reduce stack to 3 frames (minimum)

## Dev Notes

### File Locations

| Runtime | New Files |
|---------|-----------|
| Rust | `rust/src/report/encoder.rs` |
| Python | `python/src/the_edge_agent/report_encoder.py` |
| Tests (Rust) | `rust/src/report/encoder_tests.rs` |
| Tests (Python) | `python/tests/test_report_encoder.py` |

### Dependencies

**Rust:**
- `flate2` crate (deflate compression)
- `base64` crate

**Python:**
- `zlib` (stdlib)
- `base64` (stdlib)

### Parity Test Example

```python
# tests/report/parity/test_encoder_parity.py
import subprocess
import json

def test_rust_python_parity():
    """Verify Rust and Python produce identical URLs."""

    test_report = {
        "version": "0.9.34",
        "platform": "linux-x86_64",
        "runtime": "rust",
        "error_type": "Panic",
        "message": "test error",
        "stack": [
            {"addr": 12345, "symbol": "main", "line": 42}
        ]
    }

    # Get Rust output
    rust_result = subprocess.run(
        ["cargo", "run", "--", "encode-report", json.dumps(test_report)],
        capture_output=True, text=True, cwd="rust"
    )
    rust_url = rust_result.stdout.strip()

    # Get Python output
    python_result = subprocess.run(
        ["python", "-m", "the_edge_agent.report_encoder", json.dumps(test_report)],
        capture_output=True, text=True, cwd="python"
    )
    python_url = python_result.stdout.strip()

    assert rust_url == python_url, f"Parity failed:\nRust: {rust_url}\nPython: {python_url}"
```

## Definition of Done

- [x] VLQ encoder working in both runtimes
- [x] Deflate compression working in both runtimes
- [x] Base64url encoding working in both runtimes
- [x] Full encode/decode round-trip verified
- [x] URL length validation working
- [x] Truncation strategy implemented
- [x] Cross-runtime parity verified (same input → same output)
- [x] Unit tests passing

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| Parity mismatch | High | Extensive cross-runtime tests |
| URL too long | Medium | Aggressive truncation strategy |
| Compression failure | Low | Fallback to uncompressed (truncated) |

## QA Notes

**Reviewer:** Quinn (Test Architect)
**Date:** 2026-01-11
**Assessment:** docs/qa/assessments/TEA-REPORT-001b-test-design-20260111.md

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 32 |
| **Unit tests** | 20 (62%) |
| **Integration tests** | 8 (25%) |
| **E2E tests** | 4 (13%) |
| **P0 (critical)** | 14 |
| **P1 (high)** | 12 |
| **P2 (medium)** | 6 |
| **AC coverage** | 6/6 (100%) |

### Risk Areas Identified

| Risk | Impact | Test Coverage |
|------|--------|---------------|
| **Parity mismatch between Rust/Python** | High | 8 dedicated tests (VLQ, deflate, base64url parity + E2E parity) |
| **URL exceeds browser limit (>2000 chars)** | Medium | 6 tests covering truncation logic and progressive fallback |
| **Compression failure** | Low | 3 tests for empty input, round-trip, UTF-8 handling |

### Recommended Test Scenarios

**Critical (P0) - Must pass before merge:**
1. VLQ encoding boundary cases (0, 127, 128, u64::MAX)
2. Cross-runtime VLQ parity (single-byte, multi-byte)
3. Deflate round-trip and Rust/Python parity
4. Base64url alphabet verification (no `+` or `/` in output)
5. URL format spec compliance
6. Truncation triggers correctly for oversized reports
7. Simple, complex, and truncated report parity (Rust URL == Python URL)

**High Priority (P1) - Should pass:**
1. Round-trip fuzz testing for VLQ
2. UTF-8 handling in compression
3. Progressive truncation strategy validation
4. Cross-decode interoperability

### Concerns and Notes

1. **Cross-runtime parity is the primary risk** - The story's value proposition depends on Rust and Python producing byte-identical URLs. The test strategy heavily emphasizes parity tests at all levels (unit, integration, E2E).

2. **Deterministic JSON serialization required** - For parity to work, both runtimes must serialize JSON identically (key ordering, whitespace). This should be explicitly addressed in implementation.

3. **Compression level parity** - Both runtimes must use compression level 9. Verify `flate2` and `zlib` produce identical output for same input.

4. **Test fixtures should be shared** - The same JSON test fixtures should be used by both Rust and Python tests to ensure true parity testing.

5. **Dependency on TEA-REPORT-001a** - Tests assume `ErrorReport` struct is available. Ensure TEA-REPORT-001a is complete before starting encoder implementation.

### Blockers

None identified. Story is well-defined with clear acceptance criteria and risk mitigations.

---

## QA Results

### Review Date: 2026-01-11

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: EXCELLENT** - The implementation demonstrates high-quality code with proper error handling, comprehensive documentation, and robust testing. Both Rust and Python implementations follow idiomatic patterns for their respective languages.

**Rust Implementation (rust/src/report/encoder.rs):**
- Clean, idiomatic Rust with proper use of Result types for error handling
- Well-documented with doc comments and examples
- Appropriate use of `serde` for JSON serialization
- Proper error type hierarchy with `EncoderError` enum implementing std::error::Error

**Python Implementation (python/src/the_edge_agent/report_encoder.py):**
- Clean Pythonic code using dataclasses and type hints
- Proper use of `OrderedDict` to ensure JSON field order parity with Rust
- Comprehensive docstrings with examples

### Refactoring Performed

No refactoring was necessary. The code is clean and follows best practices.

### Compliance Check

- Coding Standards: ✓ - Both implementations follow their respective language idioms
- Project Structure: ✓ - Files placed correctly in report modules
- Testing Strategy: ✓ - 52 total tests (23 Rust + 29 Python) covering all scenarios
- All ACs Met: ✓ - All 6 acceptance criteria verified

### Improvements Checklist

All items pass - no improvements required:

- [x] VLQ encoding implemented correctly in both runtimes (AC-6)
- [x] Deflate compression using level 9 in both runtimes (AC-7)
- [x] Base64url encoding with URL-safe alphabet, no padding (AC-8)
- [x] URL format matches specification (AC-9)
- [x] URL length validation with truncation strategy (AC-10)
- [x] Cross-runtime parity verified via dedicated tests (AC-11)

### Security Review

**PASS** - No security concerns identified:
- No PII exposure (inherits from TEA-REPORT-001a sanitization)
- No sensitive data in encoded URLs
- Base64url encoding uses URL-safe alphabet (no injection risks)
- Compression uses standard zlib (no custom crypto)

### Performance Considerations

**PASS** - Performance is appropriate:
- Compression level 9 may be slower but produces shorter URLs (correct trade-off for this use case)
- Truncation strategy handles edge cases efficiently with progressive fallback
- No unnecessary allocations in hot paths

### Files Modified During Review

None - no modifications were needed.

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-REPORT-001b-url-encoder-decoder.yml
Risk profile: N/A (no high-risk items identified)
NFR assessment: All PASS

### Recommended Status

**✓ Ready for Done** - All acceptance criteria met, tests passing, cross-runtime parity verified.

---

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2025-01-11 | 1.0 | Initial story creation | Sarah (PO Agent) |
| 2026-01-11 | 1.1 | Added QA Notes section | Quinn (Test Architect) |
| 2026-01-11 | 1.2 | Implementation complete | James (Dev Agent) |
| 2026-01-11 | 1.3 | QA Review completed - PASS | Quinn (Test Architect) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

N/A - No blockers encountered.

### Completion Notes

1. **Rust Implementation**: Created `rust/src/report/encoder.rs` with:
   - VLQ encoding/decoding matching source map specification
   - Deflate compression using flate2 crate (level 9)
   - Base64url encoding using base64 crate (URL_SAFE_NO_PAD)
   - Full encode/decode round-trip for ErrorReport
   - Progressive truncation strategy (5 levels)
   - 23 unit tests covering all components

2. **Python Implementation**: Created `python/src/the_edge_agent/report_encoder.py` with:
   - VLQ encoding/decoding matching Rust byte-for-byte
   - Deflate compression using zlib (level 9)
   - Base64url encoding using base64 module
   - OrderedDict serialization matching Rust struct field order for parity
   - Same truncation strategy as Rust

3. **Cross-Runtime Parity**: Both implementations produce identical JSON field ordering and use the same compression/encoding algorithms. The `tests/test_encoder_parity.py` verifies parity at VLQ, base64url, and full encode/decode levels.

4. **Test Results**:
   - Rust: 23 encoder tests pass
   - Python: 24 encoder tests + 5 parity tests pass
   - All report module tests pass in both runtimes

### File List

| Action | File |
|--------|------|
| Modified | `rust/Cargo.toml` (added flate2, base64 deps) |
| Modified | `rust/src/report/mod.rs` (added encoder module, Deserialize derives) |
| Created | `rust/src/report/encoder.rs` |
| Created | `python/src/the_edge_agent/report_encoder.py` |
| Created | `python/tests/test_report_encoder.py` |
| Created | `python/tests/test_encoder_parity.py` |
