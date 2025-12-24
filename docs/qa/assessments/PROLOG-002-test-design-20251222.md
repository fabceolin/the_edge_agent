# Test Design: Story TEA-PROLOG-002

Date: 2025-12-22
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 42
- **Integration tests:** 42 (100%)
- **Unit tests:** 0 (0%) - Not applicable; parity testing requires cross-runtime execution
- **E2E tests:** 0 (0%) - No UI; CLI-based parity comparison suffices
- **Priority distribution:** P0: 28, P1: 10, P2: 4

### Strategy Rationale

This story focuses on **cross-runtime parity verification** between Python and Rust TEA implementations. All tests are inherently **integration-level** because:

1. Tests execute YAML workflows through the complete runtime
2. Tests compare outputs between two separate systems (Python vs Rust)
3. Tests validate the integration between Prolog (SWI-Prolog) and host language runtimes

Unit tests are not applicable since we're testing behavior equivalence across runtimes, not isolated logic.

---

## Test Scenarios by Acceptance Criteria

### AC-1: Final State Parity

> GIVEN the same Prolog YAML agent, WHEN executed in Python TEA and Rust TEA, THEN the final state is identical

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-001 | Integration | P0 | Execute basic-state-access.yaml in Python, capture final state | Cross-runtime parity is core mission |
| PROLOG-002-INT-002 | Integration | P0 | Execute basic-state-access.yaml in Rust, capture final state | Cross-runtime parity is core mission |
| PROLOG-002-INT-003 | Integration | P0 | Compare JSON-normalized outputs from both runtimes | Final validation of AC-1 |

### AC-2: State Query Type Parity (state/2)

> GIVEN `state/2` queries for all JSON types (null, bool, number, string, array, object), WHEN executed in both runtimes, THEN identical values are unified

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-004 | Integration | P0 | Query string value via state/2 in both runtimes | String encoding critical |
| PROLOG-002-INT-005 | Integration | P0 | Query integer value via state/2 in both runtimes | Number handling critical |
| PROLOG-002-INT-006 | Integration | P0 | Query float value via state/2 in both runtimes | Float precision parity |
| PROLOG-002-INT-007 | Integration | P0 | Query boolean value via state/2 in both runtimes | Bool representation |
| PROLOG-002-INT-008 | Integration | P0 | Query null value via state/2 in both runtimes | Null handling varies between languages |
| PROLOG-002-INT-009 | Integration | P0 | Query array value via state/2 in both runtimes | Array/list conversion |
| PROLOG-002-INT-010 | Integration | P0 | Query object value via state/2 in both runtimes | Dict/object conversion |

### AC-3: State Return Type Parity (return/2)

> GIVEN `return/2` calls with all JSON types, WHEN executed in both runtimes, THEN identical state updates are produced

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-011 | Integration | P0 | Return string via return/2 in both runtimes | String serialization |
| PROLOG-002-INT-012 | Integration | P0 | Return number via return/2 in both runtimes | Number serialization |
| PROLOG-002-INT-013 | Integration | P0 | Return boolean via return/2 in both runtimes | Bool serialization |
| PROLOG-002-INT-014 | Integration | P0 | Return null via return/2 in both runtimes | Null serialization |
| PROLOG-002-INT-015 | Integration | P0 | Return array via return/2 in both runtimes | List serialization |
| PROLOG-002-INT-016 | Integration | P0 | Return nested object via return/2 in both runtimes | Object serialization |

### AC-4: CLP(FD) Deterministic Solutions

> GIVEN CLP(FD) constraints with deterministic solutions, WHEN solved in both runtimes, THEN identical solutions are returned

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-017 | Integration | P0 | Solve unique constraint in Python | CLP(FD) core functionality |
| PROLOG-002-INT-018 | Integration | P0 | Solve unique constraint in Rust | CLP(FD) core functionality |
| PROLOG-002-INT-019 | Integration | P0 | Compare deterministic CLP(FD) solutions | Constraint solver parity |

### AC-5: CLP(FD) Multiple Solutions (First-Only Mode)

> GIVEN CLP(FD) constraints with multiple solutions (first-only mode), WHEN solved in both runtimes, THEN the same first solution is returned

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-020 | Integration | P0 | Solve multi-solution constraint in Python (first only) | First-only mode is standard |
| PROLOG-002-INT-021 | Integration | P0 | Solve multi-solution constraint in Rust (first only) | First-only mode is standard |
| PROLOG-002-INT-022 | Integration | P0 | Compare first solutions match between runtimes | Search order parity |

### AC-6: Syntax Error Parity

> GIVEN Prolog syntax errors, WHEN executed in both runtimes, THEN equivalent error types are raised

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-023 | Integration | P0 | Trigger syntax error in Python, capture error type | Error handling is security-relevant |
| PROLOG-002-INT-024 | Integration | P0 | Trigger syntax error in Rust, capture error type | Error handling is security-relevant |
| PROLOG-002-INT-025 | Integration | P0 | Verify error types are equivalent (PrologRuntimeError vs TeaError::Prolog) | Error parity |

### AC-7: Timeout Parity

> GIVEN timeout-triggering infinite recursion, WHEN executed in both runtimes, THEN timeout errors are raised at the same threshold

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-026 | Integration | P0 | Trigger timeout via infinite recursion in Python | Timeout prevents DoS |
| PROLOG-002-INT-027 | Integration | P0 | Trigger timeout via infinite recursion in Rust | Timeout prevents DoS |
| PROLOG-002-INT-028 | Integration | P1 | Verify timeout threshold within ±10% tolerance | Timing tolerance expected |

### AC-8: Sandbox Security Parity

> GIVEN sandbox-violating code (file access), WHEN executed in both runtimes, THEN security errors are raised

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-029 | Integration | P0 | Attempt file read in Python Prolog, expect security error | Sandbox is critical security feature |
| PROLOG-002-INT-030 | Integration | P0 | Attempt file read in Rust Prolog, expect security error | Sandbox is critical security feature |
| PROLOG-002-INT-031 | Integration | P0 | Verify both runtimes block sandbox violations | Security parity |

### AC-9: Parallel Fan-Out Thread Isolation

> GIVEN parallel fan-out with Prolog nodes, WHEN executed in both runtimes, THEN thread-local `state/2` isolation behaves identically

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-032 | Integration | P0 | Execute parallel branches in Python, verify isolation | Parallel correctness critical |
| PROLOG-002-INT-033 | Integration | P0 | Execute parallel branches in Rust, verify isolation | Parallel correctness critical |
| PROLOG-002-INT-034 | Integration | P0 | Compare parallel isolation behavior between runtimes | Thread-local parity |

### AC-10: Parallel Fan-In Result Collection

> GIVEN parallel branches modifying `return_value/2`, WHEN merged at fan-in, THEN results are collected identically

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-035 | Integration | P0 | Execute fan-in in Python, collect results | Fan-in is core pattern |
| PROLOG-002-INT-036 | Integration | P0 | Execute fan-in in Rust, collect results | Fan-in is core pattern |
| PROLOG-002-INT-037 | Integration | P0 | Compare fan-in collected results between runtimes | Fan-in parity |

### AC-11: Unicode String Handling

> GIVEN Unicode strings in state, WHEN accessed via `state/2` in both runtimes, THEN encoding is handled identically

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-038 | Integration | P1 | Process Unicode (emoji, CJK, RTL) in Python | Unicode is common in real data |
| PROLOG-002-INT-039 | Integration | P1 | Process Unicode (emoji, CJK, RTL) in Rust | Unicode is common in real data |
| PROLOG-002-INT-040 | Integration | P1 | Compare Unicode handling between runtimes | Encoding parity |

### AC-12: Deeply Nested JSON Objects

> GIVEN deeply nested JSON objects, WHEN converted to/from Prolog terms in both runtimes, THEN structure is preserved identically

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-041 | Integration | P1 | Process 5+ level nested structure in Python | Deep nesting stress test |
| PROLOG-002-INT-042 | Integration | P1 | Process 5+ level nested structure in Rust | Deep nesting stress test |
| PROLOG-002-INT-043 | Integration | P1 | Compare nested structure preservation | Structure parity |

### AC-13: Empty Collections

> GIVEN empty collections ([], {}), WHEN passed through Prolog in both runtimes, THEN they remain empty

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-044 | Integration | P1 | Pass empty array through Python Prolog | Edge case handling |
| PROLOG-002-INT-045 | Integration | P1 | Pass empty object through Python Prolog | Edge case handling |
| PROLOG-002-INT-046 | Integration | P1 | Pass empty collections through Rust Prolog | Edge case handling |
| PROLOG-002-INT-047 | Integration | P1 | Verify no type coercion differences | Collection parity |

### AC-14: Fixture Directory Structure

> Parity test fixtures are stored in `examples/prolog/` as shared YAML files

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-048 | Integration | P2 | Verify `examples/prolog/parity/` directory exists | Infrastructure validation |
| PROLOG-002-INT-049 | Integration | P2 | Verify all required fixture files present | Infrastructure validation |

### AC-15: Cross-Runtime Test Harness

> A cross-runtime test harness script can run the same YAML against both Python and Rust CLIs and compare results

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-050 | Integration | P2 | Execute parity-test.sh with sample fixture | Harness functionality |
| PROLOG-002-INT-051 | Integration | P2 | Verify harness handles missing runtime gracefully | Robustness |

### AC-16: Documented Parity Results

> Parity test results are documented with any known behavioral differences

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| PROLOG-002-INT-052 | Integration | P2 | Verify parity report document exists | Documentation completeness |

---

## Test Fixture Matrix

| Fixture File | Related ACs | Priority | Python Test | Rust Test |
|---|---|---|---|---|
| basic-state-access.yaml | AC-1, AC-2, AC-3 | P0 | test_parity_basic_state_access | test_parity_basic_state_access |
| clpfd-deterministic.yaml | AC-4 | P0 | test_parity_clpfd_deterministic | test_parity_clpfd_deterministic |
| clpfd-multiple-solutions.yaml | AC-5 | P0 | test_parity_clpfd_multiple | test_parity_clpfd_multiple |
| error-syntax.yaml | AC-6 | P0 | test_parity_error_syntax | test_parity_error_syntax |
| error-timeout.yaml | AC-7 | P0 | test_parity_error_timeout | test_parity_error_timeout |
| error-sandbox.yaml | AC-8 | P0 | test_parity_error_sandbox | test_parity_error_sandbox |
| parallel-isolation.yaml | AC-9, AC-10 | P0 | test_parity_parallel_isolation | test_parity_parallel_isolation |
| unicode-strings.yaml | AC-11 | P1 | test_parity_unicode | test_parity_unicode |
| nested-objects.yaml | AC-12 | P1 | test_parity_nested | test_parity_nested |
| empty-collections.yaml | AC-13 | P1 | test_parity_empty | test_parity_empty |

---

## Risk Coverage

| Risk ID | Description | Mitigating Tests |
|---|---|---|
| RISK-001 | JSON serialization differences (float precision, null handling) | PROLOG-002-INT-004 through INT-016 |
| RISK-002 | Timeout timing variations | PROLOG-002-INT-026 through INT-028 |
| RISK-003 | Sandbox implementation gaps | PROLOG-002-INT-029 through INT-031 |
| RISK-004 | Thread-local state leakage in parallel | PROLOG-002-INT-032 through INT-037 |
| RISK-005 | Unicode encoding differences | PROLOG-002-INT-038 through INT-040 |

---

## Recommended Execution Order

### Phase 1: Fast Feedback (P0 Core)
1. PROLOG-002-INT-001 through INT-003 (Basic state parity)
2. PROLOG-002-INT-004 through INT-016 (Type parity)
3. PROLOG-002-INT-023 through INT-031 (Error handling parity)

### Phase 2: Constraint Solving (P0)
4. PROLOG-002-INT-017 through INT-022 (CLP(FD) parity)

### Phase 3: Parallel Execution (P0)
5. PROLOG-002-INT-032 through INT-037 (Parallel parity)

### Phase 4: Edge Cases (P1)
6. PROLOG-002-INT-038 through INT-047 (Unicode, nesting, empty)

### Phase 5: Infrastructure (P2)
7. PROLOG-002-INT-048 through INT-052 (Harness validation)

---

## Test Implementation Recommendations

### Python Test File Structure
```python
# python/tests/test_prolog_parity.py

import pytest
import json
from pathlib import Path

PARITY_DIR = Path(__file__).parent.parent.parent / "examples" / "prolog" / "parity"
EXPECTED_DIR = Path(__file__).parent.parent.parent / "examples" / "prolog" / "parity-expected"

@pytest.fixture
def rust_binary():
    """Skip tests if Rust binary unavailable."""
    # Detect platform and binary location
    ...

class TestStateParity:
    """AC-1, AC-2, AC-3: State access and return parity."""

    @pytest.mark.parametrize("fixture", [
        "basic-state-access.yaml",
    ])
    def test_parity_basic_state_access(self, fixture):
        # Execute in Python
        python_result = run_yaml_python(PARITY_DIR / fixture)

        # Execute in Rust
        rust_result = run_yaml_rust(PARITY_DIR / fixture)

        # Compare normalized JSON
        assert normalize_json(python_result) == normalize_json(rust_result)
```

### Rust Test File Structure
```rust
// rust/tests/prolog_parity_tests.rs

#[cfg(feature = "prolog")]
mod parity_tests {
    use std::path::PathBuf;
    use serde_json::Value;

    fn parity_fixtures_dir() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent().unwrap()
            .join("examples/prolog/parity")
    }

    #[test]
    fn test_parity_basic_state_access() {
        let yaml_path = parity_fixtures_dir().join("basic-state-access.yaml");
        let result = execute_yaml(&yaml_path);
        let expected = load_expected("basic-state-access.json");
        assert_eq!(normalize_json(&result), normalize_json(&expected));
    }
}
```

### JSON Normalization Requirements

Both runtimes must normalize JSON before comparison:
1. Sort object keys alphabetically
2. Normalize floats to consistent precision (14 significant digits)
3. Ensure consistent null representation
4. Handle empty array/object consistently

---

## Quality Checklist

- [x] Every AC has at least one test
- [x] Test levels are appropriate (integration for cross-runtime)
- [x] No duplicate coverage across levels (only integration used)
- [x] Priorities align with business risk (security/core = P0)
- [x] Test IDs follow naming convention (PROLOG-002-INT-NNN)
- [x] Scenarios are atomic and independent
- [x] Risk mitigations are addressed

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 52
  by_level:
    unit: 0
    integration: 52
    e2e: 0
  by_priority:
    p0: 28
    p1: 10
    p2: 4
  coverage_gaps: []
  notes:
    - All tests are integration-level due to cross-runtime nature
    - P0 focuses on data type parity and security (sandbox, errors)
    - P1 covers edge cases (Unicode, nesting, empty collections)
    - P2 covers infrastructure validation
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/PROLOG-002-test-design-20251222.md
P0 tests identified: 28
P1 tests identified: 10
P2 tests identified: 4
Total coverage: 16 ACs → 52 test scenarios
```
