# Story: TEA-PROLOG-002 - Cross-Runtime Parity Tests

## Status

**Draft**

---

## Story

**As a** workflow developer using Prolog in TEA,
**I want** comprehensive parity tests between Python and Rust implementations,
**So that** I can confidently run the same Prolog YAML agents in either runtime knowing they will produce identical results.

---

## Story Context

**Parent Epic:** [TEA-PROLOG-001 - Prolog Integration for Neurosymbolic AI](TEA-PROLOG-001-prolog-integration-epic.md)

**Dependencies:**
- TEA-PY-004 - Prolog Scripting Support in Python TEA (Must be complete)
- TEA-RUST-035 - Prolog Scripting Support in Rust TEA (Must be complete)

**Existing System Integration:**

- **Integrates with:** Python `PrologRuntime` and Rust `PrologRuntime`
- **Technology:** pytest (Python), cargo test (Rust), shared YAML fixtures
- **Follows pattern:** Existing `while-loop-parity-test.yaml` cross-runtime test
- **Touch points:** `examples/prolog/`, Python tests, Rust tests

**Why Parity Tests Matter:**

Both Python and Rust TEA use the **same SWI-Prolog engine**, which should guarantee identical behavior. However, subtle differences can arise from:
- JSON ↔ Prolog term conversion implementations
- Thread-local predicate handling
- Timeout edge cases
- Error message formatting

This story creates the test harness and fixtures to verify true parity.

---

## Acceptance Criteria

### Core Parity Requirements

1. **AC-1**: GIVEN the same Prolog YAML agent, WHEN executed in Python TEA and Rust TEA, THEN the final state is identical

2. **AC-2**: GIVEN `state/2` queries for all JSON types (null, bool, number, string, array, object), WHEN executed in both runtimes, THEN identical values are unified

3. **AC-3**: GIVEN `return/2` calls with all JSON types, WHEN executed in both runtimes, THEN identical state updates are produced

4. **AC-4**: GIVEN CLP(FD) constraints with deterministic solutions, WHEN solved in both runtimes, THEN identical solutions are returned

5. **AC-5**: GIVEN CLP(FD) constraints with multiple solutions (first-only mode), WHEN solved in both runtimes, THEN the same first solution is returned

### Error Handling Parity

6. **AC-6**: GIVEN Prolog syntax errors, WHEN executed in both runtimes, THEN equivalent error types are raised (`PrologRuntimeError` in Python, `TeaError::Prolog` in Rust)

7. **AC-7**: GIVEN timeout-triggering infinite recursion, WHEN executed in both runtimes, THEN timeout errors are raised at the same threshold

8. **AC-8**: GIVEN sandbox-violating code (file access), WHEN executed in both runtimes, THEN security errors are raised

### Parallel Execution Parity

9. **AC-9**: GIVEN parallel fan-out with Prolog nodes, WHEN executed in both runtimes, THEN thread-local `state/2` isolation behaves identically

10. **AC-10**: GIVEN parallel branches modifying `return_value/2`, WHEN merged at fan-in, THEN results are collected identically

### Edge Case Parity

11. **AC-11**: GIVEN Unicode strings in state, WHEN accessed via `state/2` in both runtimes, THEN encoding is handled identically

12. **AC-12**: GIVEN deeply nested JSON objects, WHEN converted to/from Prolog terms in both runtimes, THEN structure is preserved identically

13. **AC-13**: GIVEN empty collections ([], {}), WHEN passed through Prolog in both runtimes, THEN they remain empty (no type coercion differences)

### Test Infrastructure

14. **AC-14**: Parity test fixtures are stored in `examples/prolog/` as shared YAML files

15. **AC-15**: A cross-runtime test harness script can run the same YAML against both Python and Rust CLIs and compare results

16. **AC-16**: Parity test results are documented with any known behavioral differences

---

## Technical Notes

### Implementation Approach

#### 1. Shared Test Fixtures Directory

```
examples/prolog/
├── parity/
│   ├── basic-state-access.yaml       # AC-1, AC-2, AC-3
│   ├── clpfd-deterministic.yaml      # AC-4
│   ├── clpfd-multiple-solutions.yaml # AC-5
│   ├── error-syntax.yaml             # AC-6
│   ├── error-timeout.yaml            # AC-7
│   ├── error-sandbox.yaml            # AC-8
│   ├── parallel-isolation.yaml       # AC-9, AC-10
│   ├── unicode-strings.yaml          # AC-11
│   ├── nested-objects.yaml           # AC-12
│   └── empty-collections.yaml        # AC-13
└── parity-expected/
    ├── basic-state-access.json       # Expected outputs
    ├── clpfd-deterministic.json
    └── ...
```

#### 2. Cross-Runtime Test Harness

```bash
#!/bin/bash
# scripts/parity-test.sh

YAML_FILE=$1
EXPECTED_OUTPUT=$2

# Run Python implementation
python -m the_edge_agent run "$YAML_FILE" --output /tmp/python_result.json

# Run Rust implementation
tea-rust-linux-x86_64 run "$YAML_FILE" --output /tmp/rust_result.json

# Compare results
diff /tmp/python_result.json /tmp/rust_result.json
```

#### 3. Parity Test Categories

| Category | Fixtures | Priority |
|----------|----------|----------|
| State Access | 3 | P0 |
| CLP(FD) | 2 | P0 |
| Error Handling | 3 | P0 |
| Parallel Execution | 2 | P0 |
| Edge Cases | 3 | P1 |

#### 4. Example Parity Test Fixture

```yaml
# examples/prolog/parity/basic-state-access.yaml
name: parity-state-access
description: Test state/2 and return/2 parity across runtimes

state_schema:
  str_value: str
  int_value: int
  float_value: float
  bool_value: bool
  null_value: any
  list_value: list
  dict_value: dict
  result: dict

nodes:
  - name: test_state_access
    language: prolog
    run: |
      % Read all state values
      state(str_value, Str),
      state(int_value, Int),
      state(float_value, Float),
      state(bool_value, Bool),
      state(null_value, Null),
      state(list_value, List),
      state(dict_value, Dict),

      % Return them all in a result object
      return(result, _{
        str: Str,
        int: Int,
        float: Float,
        bool: Bool,
        null: Null,
        list: List,
        dict: Dict
      }).

edges:
  - from: __start__
    to: test_state_access
  - from: test_state_access
    to: __end__

# Initial state for parity test
initial_state:
  str_value: "hello world"
  int_value: 42
  float_value: 3.14159
  bool_value: true
  null_value: null
  list_value: [1, "two", 3.0, true, null]
  dict_value:
    nested_key: "nested_value"
    nested_int: 123
```

#### 5. CLP(FD) Parity Test

```yaml
# examples/prolog/parity/clpfd-deterministic.yaml
name: parity-clpfd
description: Test CLP(FD) constraint solving parity

state_schema:
  min_val: int
  max_val: int
  target_sum: int
  solution: list

nodes:
  - name: solve_constraints
    language: prolog
    run: |
      :- use_module(library(clpfd)).

      state(min_val, Min),
      state(max_val, Max),
      state(target_sum, Target),

      % Three variables in range
      X in Min..Max,
      Y in Min..Max,
      Z in Min..Max,

      % Constraints
      X + Y + Z #= Target,
      X #< Y,
      Y #< Z,

      % Solve (first solution)
      label([X, Y, Z]),

      return(solution, [X, Y, Z]).

edges:
  - from: __start__
    to: solve_constraints
  - from: solve_constraints
    to: __end__

initial_state:
  min_val: 1
  max_val: 10
  target_sum: 15
```

### Key Constraints

- **Same Engine**: Both implementations use SWI-Prolog, ensuring logic parity
- **JSON Serialization**: Results must be compared as JSON to normalize representation
- **Timeout Tolerance**: Allow small timing variations (e.g., ±10% on timeout tests)
- **Thread-Local**: Parallel tests must verify thread-local predicate isolation

### Known Potential Differences

Document any discovered differences between runtimes:

| Area | Python Behavior | Rust Behavior | Resolution |
|------|-----------------|---------------|------------|
| TBD | TBD | TBD | TBD |

---

## Tasks / Subtasks

- [ ] **Task 1: Create parity test fixture directory structure** (AC: 14)
  - [ ] Create `examples/prolog/parity/` directory
  - [ ] Create `examples/prolog/parity-expected/` for expected outputs
  - [ ] Add README explaining parity test purpose and usage

- [ ] **Task 2: Create state access parity fixtures** (AC: 1, 2, 3)
  - [ ] Create `basic-state-access.yaml` with all JSON types
  - [ ] Create expected output JSON
  - [ ] Verify identical results in Python and Rust

- [ ] **Task 3: Create CLP(FD) parity fixtures** (AC: 4, 5)
  - [ ] Create `clpfd-deterministic.yaml` with single-solution constraints
  - [ ] Create `clpfd-multiple-solutions.yaml` to test first-only mode
  - [ ] Create expected output JSON files
  - [ ] Verify identical constraint solutions

- [ ] **Task 4: Create error handling parity fixtures** (AC: 6, 7, 8)
  - [ ] Create `error-syntax.yaml` with intentional Prolog syntax error
  - [ ] Create `error-timeout.yaml` with infinite recursion
  - [ ] Create `error-sandbox.yaml` with file access attempt
  - [ ] Document expected error types for each runtime

- [ ] **Task 5: Create parallel execution parity fixtures** (AC: 9, 10)
  - [ ] Create `parallel-isolation.yaml` testing thread-local state
  - [ ] Create fan-in test with multiple Prolog branches
  - [ ] Verify parallel results are collected identically

- [ ] **Task 6: Create edge case parity fixtures** (AC: 11, 12, 13)
  - [ ] Create `unicode-strings.yaml` with non-ASCII characters
  - [ ] Create `nested-objects.yaml` with deep nesting (5+ levels)
  - [ ] Create `empty-collections.yaml` with [], {}
  - [ ] Verify edge cases handled identically

- [ ] **Task 7: Create cross-runtime test harness** (AC: 15)
  - [ ] Create `scripts/parity-test.sh` bash script
  - [ ] Add Python-side test runner (`pytest -k parity`)
  - [ ] Add Rust-side test runner (`cargo test --features prolog parity`)
  - [ ] Add JSON diff comparison logic
  - [ ] Handle error case comparison

- [ ] **Task 8: Python parity test module** (AC: 15, 16)
  - [ ] Create `python/tests/test_prolog_parity.py`
  - [ ] Add parametrized tests for all parity fixtures
  - [ ] Add expected output loading
  - [ ] Skip tests if Rust binary not available

- [ ] **Task 9: Rust parity test module** (AC: 15, 16)
  - [ ] Create `rust/tests/prolog_parity_tests.rs`
  - [ ] Add tests for all parity fixtures
  - [ ] Use `#[cfg(feature = "prolog")]` guard
  - [ ] Compare against expected outputs

- [ ] **Task 10: Document parity results** (AC: 16)
  - [ ] Create `docs/shared/prolog-parity-report.md`
  - [ ] Document any discovered behavioral differences
  - [ ] Add resolution notes for each difference
  - [ ] Update epic with parity status

---

## Dev Notes

### Relevant Source Tree

```
the_edge_agent/
├── examples/
│   └── prolog/
│       ├── parity/                    # NEW: Parity test fixtures
│       │   ├── README.md
│       │   ├── basic-state-access.yaml
│       │   ├── clpfd-deterministic.yaml
│       │   ├── clpfd-multiple-solutions.yaml
│       │   ├── error-syntax.yaml
│       │   ├── error-timeout.yaml
│       │   ├── error-sandbox.yaml
│       │   ├── parallel-isolation.yaml
│       │   ├── unicode-strings.yaml
│       │   ├── nested-objects.yaml
│       │   └── empty-collections.yaml
│       └── parity-expected/           # NEW: Expected outputs
│           └── *.json
│
├── python/tests/
│   └── test_prolog_parity.py          # NEW: Python parity tests
│
├── rust/tests/
│   └── prolog_parity_tests.rs         # NEW: Rust parity tests
│
├── scripts/
│   └── parity-test.sh                 # NEW: Cross-runtime harness
│
└── docs/shared/
    └── prolog-parity-report.md        # NEW: Parity documentation
```

### Testing

**Python parity tests:**
```bash
cd python && pytest tests/test_prolog_parity.py -v
```

**Rust parity tests:**
```bash
cd rust && cargo test --features prolog parity
```

**Full cross-runtime parity:**
```bash
./scripts/parity-test.sh examples/prolog/parity/basic-state-access.yaml
```

### Test Design Considerations

- Tests should be **deterministic** - same input always produces same output
- Use `first_only=True` mode for Prolog queries to ensure determinism
- CLP(FD) tests should have **unique first solutions**
- Timeout tests need consistent threshold values in both implementations
- Parallel tests should use **fixed random seeds** if any randomness involved

---

## Risk and Compatibility Check

**Minimal Risk Assessment:**

- **Primary Risk:** Subtle JSON serialization differences between Python and Rust
- **Mitigation:** Normalize JSON before comparison (sorted keys, consistent null handling)
- **Secondary Risk:** Timing-dependent timeout tests may flake
- **Mitigation:** Allow tolerance window for timeout comparisons

**Compatibility Verification:**

- [ ] Parity tests don't affect existing functionality
- [ ] Tests are skipped gracefully if Prolog not installed
- [ ] Test harness works on all supported platforms

---

## Definition of Done

- [ ] All 13 parity fixtures created and passing in both runtimes
- [ ] Cross-runtime test harness script operational
- [ ] Python `test_prolog_parity.py` passing
- [ ] Rust `prolog_parity_tests.rs` passing with `--features prolog`
- [ ] Parity report documenting any differences
- [ ] Epic TEA-PROLOG-001 updated with parity status

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-22 | 0.1 | Initial story draft created from epic requirements | Sarah (PO) |
