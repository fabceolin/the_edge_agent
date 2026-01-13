# Test Design: Story TEA-REPORT-001a

Date: 2026-01-11
Designer: Quinn (Test Architect)

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 32 |
| Unit tests | 20 (62.5%) |
| Integration tests | 9 (28.1%) |
| E2E tests | 3 (9.4%) |
| Priority distribution | P0: 12, P1: 14, P2: 6 |

### Strategic Rationale

This story focuses on error capture infrastructure - a foundational component for bug reporting. The test strategy emphasizes:

1. **Unit tests dominate** - ErrorReport structs, serialization, and path sanitization are pure functions ideal for unit testing
2. **Integration tests** - Verify hook installation and error wrapping across component boundaries
3. **E2E tests** - Minimal, focused on actual crash/exception capture in real runtime scenarios

### Risk-Based Prioritization

| Risk | P0 Tests | Mitigation |
|------|----------|------------|
| Privacy leak (absolute paths) | 4 | Path sanitization + negative tests |
| Schema mismatch Rust/Python | 2 | JSON output comparison |
| Hook installation failure | 2 | Integration tests for hook behavior |

---

## Test Scenarios by Acceptance Criteria

### AC-1: Rust panic hook captures: version, platform, architecture, stack addresses, panic message

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001a-UNIT-001 | Unit | P0 | ErrorReport struct serializes to valid JSON | Core data structure correctness |
| 001a-UNIT-002 | Unit | P0 | StackFrame with all fields serializes correctly | Stack capture accuracy |
| 001a-UNIT-003 | Unit | P0 | StackFrame with optional fields omits None in JSON | Clean serialization output |
| 001a-UNIT-004 | Unit | P1 | ErrorType::Panic variant serializes as "Panic" | Enum serialization format |
| 001a-UNIT-005 | Unit | P1 | Platform detection returns valid string (e.g., "linux-x86_64") | System info capture |
| 001a-INT-001 | Integration | P0 | Panic hook captures panic message from `panic!()` | Hook installation works |
| 001a-INT-002 | Integration | P1 | Panic hook extracts stack addresses from backtrace | Stack trace extraction |
| 001a-INT-003 | Integration | P1 | Panic hook populates version from Cargo.toml | Version extraction |
| 001a-E2E-001 | E2E | P0 | Triggering panic produces complete ErrorReport JSON file | Full panic capture workflow |

---

### AC-2: Python excepthook captures: version, platform, architecture, traceback, exception message

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001a-UNIT-006 | Unit | P0 | ErrorReport dataclass serializes to valid JSON | Core data structure correctness |
| 001a-UNIT-007 | Unit | P0 | StackFrame with all fields serializes correctly | Stack capture accuracy |
| 001a-UNIT-008 | Unit | P0 | to_json() excludes None values from output | Clean serialization output |
| 001a-UNIT-009 | Unit | P1 | ErrorType enum values match Rust variants | Cross-runtime parity |
| 001a-UNIT-010 | Unit | P1 | Platform detection uses platform module correctly | System info capture |
| 001a-INT-004 | Integration | P0 | sys.excepthook replacement captures unhandled exception | Hook installation works |
| 001a-INT-005 | Integration | P1 | Excepthook extracts traceback frames correctly | Traceback extraction |
| 001a-INT-006 | Integration | P1 | Excepthook populates version from __init__.py | Version extraction |
| 001a-E2E-002 | E2E | P0 | Unhandled exception produces complete ErrorReport JSON | Full exception capture workflow |

---

### AC-3: YAML engine errors captured with: node name, action type, error kind

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001a-UNIT-011 | Unit | P1 | ErrorContext with node_name serializes correctly | Context structure |
| 001a-UNIT-012 | Unit | P1 | ErrorContext with action_type serializes correctly | Context structure |
| 001a-UNIT-013 | Unit | P1 | ErrorType::YamlError variant serializes as "YamlError" | Error classification |
| 001a-INT-007 | Integration | P1 | YAML engine exception wraps with node context (Python) | Error wrapping works |
| 001a-INT-008 | Integration | P1 | YAML engine error wraps with node context (Rust) | Error wrapping works |

---

### AC-4: Executor errors captured with: checkpoint context (ID only), interrupted state (without data)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001a-UNIT-014 | Unit | P1 | ErrorContext with checkpoint_id serializes correctly | Context structure |
| 001a-UNIT-015 | Unit | P1 | ErrorType::ExecutorError variant serializes as "ExecutorError" | Error classification |
| 001a-INT-009 | Integration | P2 | Executor error wraps with checkpoint ID only | Error wrapping preserves privacy |
| 001a-E2E-003 | E2E | P2 | Interrupted workflow error excludes state data | Full privacy validation |

---

### AC-5: All captures exclude PII: no file contents, no state data, no user paths (relative paths only)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001a-UNIT-016 | Unit | P0 | sanitize_path converts absolute to relative (Python) | Privacy core function |
| 001a-UNIT-017 | Unit | P0 | sanitize_path strips home directory (Python) | Privacy core function |
| 001a-UNIT-018 | Unit | P0 | sanitize_path returns basename for unknown absolute (Python) | Privacy fallback |
| 001a-UNIT-019 | Unit | P0 | path_sanitize converts absolute to relative (Rust) | Privacy core function |
| 001a-UNIT-020 | Unit | P0 | path_sanitize strips home directory (Rust) | Privacy core function |
| 001a-UNIT-021 | Unit | P2 | StackFrame file field only contains relative paths | Serialization privacy |
| 001a-UNIT-022 | Unit | P2 | ErrorReport serialization excludes state data values | Privacy by design |

---

## Cross-Runtime Parity Tests

These tests ensure Rust and Python implementations produce compatible output.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001a-UNIT-023 | Unit | P0 | Rust and Python ErrorReport JSON schemas match | Cross-runtime compatibility |
| 001a-UNIT-024 | Unit | P1 | Rust and Python ErrorType enum values identical | Error classification parity |
| 001a-UNIT-025 | Unit | P2 | Both runtimes handle EdgeInfo from_node field naming | Python reserved word handling |

---

## Risk Coverage Matrix

| Risk ID | Description | Test IDs |
|---------|-------------|----------|
| RISK-001 | Privacy leak via absolute paths | 001a-UNIT-016 through 001a-UNIT-020, 001a-E2E-003 |
| RISK-002 | Schema mismatch between runtimes | 001a-UNIT-023, 001a-UNIT-024, 001a-UNIT-025 |
| RISK-003 | Hook installation failure | 001a-INT-001, 001a-INT-004, 001a-E2E-001, 001a-E2E-002 |
| RISK-004 | Stack trace incomplete | 001a-INT-002, 001a-INT-005 |

---

## Recommended Execution Order

### Phase 1: P0 Unit Tests (Fail Fast) - 8 tests
```
001a-UNIT-001, 001a-UNIT-002, 001a-UNIT-003, 001a-UNIT-006,
001a-UNIT-007, 001a-UNIT-008, 001a-UNIT-016, 001a-UNIT-017,
001a-UNIT-018, 001a-UNIT-019, 001a-UNIT-020, 001a-UNIT-023
```

### Phase 2: P0 Integration Tests - 2 tests
```
001a-INT-001, 001a-INT-004
```

### Phase 3: P0 E2E Tests - 2 tests
```
001a-E2E-001, 001a-E2E-002
```

### Phase 4: P1 Tests in Order - 14 tests
```
Unit: 001a-UNIT-004, 001a-UNIT-005, 001a-UNIT-009, 001a-UNIT-010,
      001a-UNIT-011, 001a-UNIT-012, 001a-UNIT-013, 001a-UNIT-014,
      001a-UNIT-015, 001a-UNIT-024
Integration: 001a-INT-002, 001a-INT-003, 001a-INT-005, 001a-INT-006,
             001a-INT-007, 001a-INT-008
```

### Phase 5: P2+ Tests (as time permits) - 6 tests
```
001a-INT-009, 001a-E2E-003, 001a-UNIT-021, 001a-UNIT-022, 001a-UNIT-025
```

---

## Test Implementation Notes

### Unit Test Structure (Python)

```python
# python/tests/test_report.py

import pytest
from the_edge_agent.report import (
    ErrorReport, StackFrame, ErrorType, ErrorContext, sanitize_path
)
import json

class TestErrorReportSerialization:
    def test_full_report_serializes_to_valid_json(self):
        """001a-UNIT-006"""
        report = ErrorReport(
            version="0.9.34",
            platform="linux-x86_64",
            runtime="python",
            error_type=ErrorType.PANIC,
            message="Test error"
        )
        result = report.to_json()
        parsed = json.loads(result)
        assert parsed["version"] == "0.9.34"
        assert parsed["platform"] == "linux-x86_64"

class TestPathSanitization:
    def test_absolute_to_relative(self):
        """001a-UNIT-016"""
        result = sanitize_path("/home/user/project/src/main.py", "/home/user/project")
        assert result == "src/main.py"
        assert not result.startswith("/")
```

### Unit Test Structure (Rust)

```rust
// rust/src/report/tests.rs

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json;

    #[test]
    fn test_error_report_serializes_to_valid_json() {
        // 001a-UNIT-001
        let report = ErrorReport {
            version: "0.9.34".to_string(),
            platform: "linux-x86_64".to_string(),
            runtime: "rust".to_string(),
            error_type: ErrorType::Panic,
            message: "Test error".to_string(),
            stack: vec![],
            context: None,
            extended: None,
        };
        let json = serde_json::to_string(&report).unwrap();
        assert!(json.contains("\"version\":\"0.9.34\""));
    }
}
```

### Integration Test Structure

```python
# python/tests/test_report_integration.py

import subprocess
import json
import pytest

class TestExcepthookIntegration:
    def test_unhandled_exception_captured(self):
        """001a-INT-004"""
        # Run a script that raises unhandled exception
        result = subprocess.run(
            ["python", "-c", "import the_edge_agent; raise ValueError('test')"],
            capture_output=True, text=True
        )
        # Verify error report was generated
        # (implementation depends on output mechanism)
```

---

## Coverage Gaps

| Gap | AC | Reason | Recommendation |
|-----|-----|--------|----------------|
| ExtendedContext serialization | N/A | Optional feature, not in AC | Add P3 tests if implemented |
| NodeInfo/EdgeInfo lists | N/A | Part of ExtendedContext | Defer to extended context story |

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for boundaries)
- [x] No duplicate coverage across levels
- [x] Priorities align with privacy/security risk
- [x] Test IDs follow naming convention `001a-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Cross-runtime parity tests included

---

## Summary Statistics

| Category | Count |
|----------|-------|
| Acceptance Criteria | 5 |
| Total Tests | 32 |
| P0 (Critical) | 12 |
| P1 (High) | 14 |
| P2 (Medium) | 6 |
| Risks Mitigated | 4 |
| Coverage Gaps | 2 (deferred, not blocking) |
