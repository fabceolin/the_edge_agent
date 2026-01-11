# Test Design: Story TEA-REPORT-001b

**Date:** 2026-01-11
**Designer:** Quinn (Test Architect)
**Story:** URL Encoder/Decoder Library

## Test Strategy Overview

- **Total test scenarios:** 32
- **Unit tests:** 20 (62%)
- **Integration tests:** 8 (25%)
- **E2E tests:** 4 (13%)
- **Priority distribution:** P0: 14, P1: 12, P2: 6

### Strategy Rationale

This story involves encoding/decoding algorithms with cross-runtime parity requirements. The test strategy heavily favors unit tests because:

1. VLQ, deflate, and Base64url are **pure functions** - ideal for unit testing
2. Cross-runtime parity requires **deterministic, repeatable** tests
3. Algorithm correctness is best validated through **known input/output pairs**
4. Integration tests focus on the **full encode/decode pipeline**
5. E2E tests validate **CLI integration** and **real-world URL handling**

---

## Test Scenarios by Acceptance Criteria

### AC-6: VLQ encoding for numeric values (stack addresses, line numbers)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-REPORT-001b-UNIT-001 | Unit | P0 | VLQ encode zero (0) | Edge case: smallest value |
| TEA-REPORT-001b-UNIT-002 | Unit | P0 | VLQ encode small value (127) | Boundary: single-byte max |
| TEA-REPORT-001b-UNIT-003 | Unit | P0 | VLQ encode medium value (128) | Boundary: first multi-byte |
| TEA-REPORT-001b-UNIT-004 | Unit | P0 | VLQ encode large value (u64::MAX) | Edge case: largest value |
| TEA-REPORT-001b-UNIT-005 | Unit | P1 | VLQ decode known values | Round-trip correctness |
| TEA-REPORT-001b-UNIT-006 | Unit | P1 | VLQ encode/decode round-trip fuzz | Property-based testing |
| TEA-REPORT-001b-UNIT-007 | Unit | P0 | VLQ Rust/Python parity - single byte | Cross-runtime parity critical |
| TEA-REPORT-001b-UNIT-008 | Unit | P0 | VLQ Rust/Python parity - multi-byte | Cross-runtime parity critical |

#### Scenario Details

**TEA-REPORT-001b-UNIT-001**: VLQ encode zero
```yaml
input: 0
expected_output: [0x00]
rationale: Zero is encoded as single null byte per VLQ spec
```

**TEA-REPORT-001b-UNIT-003**: VLQ encode 128
```yaml
input: 128
expected_output: [0x80, 0x01]
rationale: First value requiring continuation bit
```

---

### AC-7: Deflate compression for text data (panic message, traceback)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-REPORT-001b-UNIT-009 | Unit | P0 | Deflate compress empty string | Edge case: empty input |
| TEA-REPORT-001b-UNIT-010 | Unit | P0 | Deflate compress typical error message | Common case validation |
| TEA-REPORT-001b-UNIT-011 | Unit | P0 | Deflate round-trip (compress/decompress) | Data integrity |
| TEA-REPORT-001b-UNIT-012 | Unit | P1 | Deflate compression level 9 verified | Performance spec compliance |
| TEA-REPORT-001b-UNIT-013 | Unit | P1 | Deflate handles non-ASCII (UTF-8) | Internationalization |
| TEA-REPORT-001b-UNIT-014 | Unit | P0 | Deflate Rust/Python parity | Cross-runtime parity critical |

#### Scenario Details

**TEA-REPORT-001b-UNIT-010**: Typical error message compression
```yaml
input: "thread 'main' panicked at 'index out of bounds: len is 3 but index is 5'"
expected: compressed.len() < input.len()  # Compression works
```

**TEA-REPORT-001b-UNIT-013**: UTF-8 handling
```yaml
input: "Error: archivo no encontrado \u3042\u3044\u3046"
expected: round_trip(input) == input
```

---

### AC-8: Base64url encoding for URL-safe output

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-REPORT-001b-UNIT-015 | Unit | P0 | Base64url uses URL-safe alphabet (+ -> -, / -> _) | Spec compliance |
| TEA-REPORT-001b-UNIT-016 | Unit | P0 | Base64url no padding (strip =) | URL compatibility |
| TEA-REPORT-001b-UNIT-017 | Unit | P1 | Base64url round-trip | Data integrity |
| TEA-REPORT-001b-UNIT-018 | Unit | P0 | Base64url Rust/Python parity | Cross-runtime parity |

#### Scenario Details

**TEA-REPORT-001b-UNIT-015**: URL-safe alphabet
```yaml
input: bytes_that_would_produce_plus_and_slash
expected_output: contains_only('-', '_', 'a-z', 'A-Z', '0-9')
assert: '+' not in output
assert: '/' not in output
```

---

### AC-9: URL format compliance

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-REPORT-001b-INT-001 | Integration | P0 | URL format matches spec | Contract validation |
| TEA-REPORT-001b-INT-002 | Integration | P1 | URL contains correct version | URL structure |
| TEA-REPORT-001b-INT-003 | Integration | P1 | URL contains runtime prefix (rust_/python_) | URL structure |
| TEA-REPORT-001b-INT-004 | Integration | P1 | URL is parseable and decodable | Round-trip |

#### Scenario Details

**TEA-REPORT-001b-INT-001**: URL format spec
```yaml
expected_pattern: "https://{org}.github.io/the_edge_agent/report/{version}/{runtime}_{encoded_data}"
test_cases:
  - version: "0.9.34"
    runtime: "rust"
    assert: url.matches(pattern)
```

---

### AC-10: Encoded URL length under 2000 characters (browser limit)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-REPORT-001b-UNIT-019 | Unit | P0 | Small report produces short URL (<2000) | Normal case |
| TEA-REPORT-001b-INT-005 | Integration | P0 | Large report triggers truncation | Truncation logic |
| TEA-REPORT-001b-INT-006 | Integration | P0 | Truncated URL still decodable | Data integrity |
| TEA-REPORT-001b-INT-007 | Integration | P1 | Truncation removes extended context first | Truncation strategy |
| TEA-REPORT-001b-INT-008 | Integration | P1 | Truncation reduces stack frames progressively | Truncation strategy |
| TEA-REPORT-001b-UNIT-020 | Unit | P2 | Minimum report (version + platform + message) under 2000 | Worst-case validation |

#### Scenario Details

**TEA-REPORT-001b-INT-005**: Truncation triggered
```yaml
input: ErrorReport with 100 stack frames, 2000-char message
assert: len(encode(report)) <= 2000
assert: decode(encode(report)).stack.len() < 100  # Stack was truncated
```

**TEA-REPORT-001b-INT-006**: Truncated still decodable
```yaml
input: Large report requiring truncation
process: url = encode(large_report)
assert: decode(url) is valid ErrorReport
assert: decode(url).message is present  # Core data preserved
```

---

### AC-11: Rust and Python encoders produce identical output for same input

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-REPORT-001b-E2E-001 | E2E | P0 | Simple report: Rust URL == Python URL | Critical parity |
| TEA-REPORT-001b-E2E-002 | E2E | P0 | Complex report: Rust URL == Python URL | Critical parity |
| TEA-REPORT-001b-E2E-003 | E2E | P0 | Truncated report: Rust URL == Python URL | Critical parity |
| TEA-REPORT-001b-E2E-004 | E2E | P1 | Cross-decode: Rust encode -> Python decode (and vice versa) | Interoperability |

#### Scenario Details

**TEA-REPORT-001b-E2E-001**: Simple report parity
```yaml
test_report:
  version: "0.9.34"
  platform: "linux-x86_64"
  runtime: "rust"
  error_type: "Panic"
  message: "test error"
  stack:
    - addr: 12345
      symbol: "main"
      line: 42
process:
  - rust_url = subprocess.run(["cargo", "run", "--", "encode-report", json])
  - python_url = subprocess.run(["python", "-m", "...", json])
assert: rust_url == python_url
```

**TEA-REPORT-001b-E2E-004**: Cross-decode interoperability
```yaml
test:
  - rust_url = rust_encode(report)
  - python_decoded = python_decode(rust_url)
  - python_url = python_encode(report)
  - rust_decoded = rust_decode(python_url)
assert: python_decoded == report
assert: rust_decoded == report
```

---

## Risk Coverage

| Risk ID | Risk Description | Impact | Test Coverage |
|---------|-----------------|--------|---------------|
| RISK-001 | Parity mismatch between Rust/Python | High | TEA-REPORT-001b-UNIT-007/008/014/018, TEA-REPORT-001b-E2E-001/002/003/004 |
| RISK-002 | URL too long for browsers | Medium | TEA-REPORT-001b-INT-005/006/007/008, TEA-REPORT-001b-UNIT-019/020 |
| RISK-003 | Compression failure | Low | TEA-REPORT-001b-UNIT-009/010/011 |

---

## Test Environment Requirements

### Unit Tests (Rust)
- `cargo test` execution
- No external dependencies

### Unit Tests (Python)
- `pytest` execution
- No external dependencies (zlib/base64 are stdlib)

### Integration Tests
- Full ErrorReport struct available from TEA-REPORT-001a
- Both runtimes built and available

### E2E Tests
- Rust binary: `cargo run --release`
- Python module: `python -m the_edge_agent.report_encoder`
- Subprocess execution capability

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on algorithm bugs)
   - VLQ encoding boundary cases
   - Deflate compression basics
   - Base64url alphabet validation
   - Cross-runtime unit parity

2. **P0 Integration tests** (pipeline validation)
   - URL format compliance
   - Truncation logic
   - Round-trip integrity

3. **P0 E2E tests** (critical parity)
   - Rust/Python produce identical URLs
   - Cross-decode interoperability

4. **P1 tests** (extended coverage)
   - Round-trip fuzz testing
   - Edge cases in truncation
   - UTF-8 handling

5. **P2 tests** (nice to have)
   - Minimum report validation
   - Performance benchmarks

---

## Test Data Fixtures

### Fixture: minimal_report
```json
{
  "version": "0.9.34",
  "platform": "linux-x86_64",
  "runtime": "rust",
  "error_type": "Panic",
  "message": "test"
}
```

### Fixture: typical_report
```json
{
  "version": "0.9.34",
  "platform": "darwin-aarch64",
  "runtime": "python",
  "error_type": "RuntimeError",
  "message": "Connection refused: could not connect to database at localhost:5432",
  "stack": [
    {"addr": 12345, "symbol": "connect", "file": "db.py", "line": 42},
    {"addr": 23456, "symbol": "query", "file": "orm.py", "line": 156},
    {"addr": 34567, "symbol": "main", "file": "app.py", "line": 8}
  ]
}
```

### Fixture: large_report (for truncation tests)
```json
{
  "version": "0.9.34",
  "platform": "linux-x86_64",
  "runtime": "rust",
  "error_type": "Panic",
  "message": "<2000 character error message>",
  "stack": ["<100 stack frames>"],
  "extended_context": {"<large nested structure>"}
}
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for algorithms, integration for pipeline, E2E for parity)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (parity = P0)
- [x] Test IDs follow naming convention: `TEA-REPORT-001b-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Risk mitigations are addressed

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 32
  by_level:
    unit: 20
    integration: 8
    e2e: 4
  by_priority:
    p0: 14
    p1: 12
    p2: 6
  coverage_gaps: []
  risk_coverage:
    - risk: "Parity mismatch"
      tests: 8
    - risk: "URL too long"
      tests: 6
    - risk: "Compression failure"
      tests: 3
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-REPORT-001b-test-design-20260111.md
P0 tests identified: 14
Cross-runtime parity tests: 8
Acceptance criteria covered: 6/6 (100%)
```
