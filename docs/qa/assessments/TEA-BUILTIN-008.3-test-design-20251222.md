# Test Design: Story TEA-BUILTIN-008.3

**Date**: 2024-12-22
**Designer**: Quinn (Test Architect)
**Story**: Schema Deep Merge CLI & Algorithm

## Test Strategy Overview

- **Total test scenarios**: 48
- **Unit tests**: 30 (62%)
- **Integration tests**: 14 (29%)
- **E2E tests**: 4 (8%)
- **Priority distribution**: P0: 18, P1: 20, P2: 10
- **Property-based tests**: 6 (included in unit count)

## Test Scenarios by Acceptance Criteria

### AC1: Deep merge algorithm (kubectl-style semantics)

#### Object Merging - Python

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.3-UNIT-001 | Unit | P0 | Merge flat objects | Basic merge |
| 008.3-UNIT-002 | Unit | P0 | Merge nested objects (recursive) | Deep merge |
| 008.3-UNIT-003 | Unit | P0 | Add new keys from overlay | Key addition |
| 008.3-UNIT-004 | Unit | P0 | Override existing keys | Last wins |
| 008.3-UNIT-005 | Unit | P1 | Deeply nested (5+ levels) | Edge case |

#### Array Handling - Python

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.3-UNIT-006 | Unit | P0 | Array replacement (not concat) | kubectl semantics |
| 008.3-UNIT-007 | Unit | P1 | Empty array replaces non-empty | Edge case |
| 008.3-UNIT-008 | Unit | P1 | Array inside nested object | Nested array |

#### Scalar and Null Handling - Python

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.3-UNIT-009 | Unit | P0 | Scalar override (last wins) | Core semantics |
| 008.3-UNIT-010 | Unit | P0 | Null overrides non-null | Null handling |
| 008.3-UNIT-011 | Unit | P1 | Non-null overrides null | Null handling |
| 008.3-UNIT-012 | Unit | P1 | Type mismatch (object → scalar) | Type coercion |
| 008.3-UNIT-013 | Unit | P1 | Type mismatch (scalar → object) | Type coercion |

#### Object Merging - Rust (Parity)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.3-UNIT-014 | Unit | P0 | Merge flat objects (Rust) | Parity |
| 008.3-UNIT-015 | Unit | P0 | Merge nested objects (Rust) | Parity |
| 008.3-UNIT-016 | Unit | P0 | Array replacement (Rust) | Parity |
| 008.3-UNIT-017 | Unit | P0 | Null handling (Rust) | Parity |

#### Property-Based Tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.3-PROP-001 | Unit | P1 | merge(d, d) == d (identity) | Invariant |
| 008.3-PROP-002 | Unit | P1 | All overlay keys in result | Invariant |
| 008.3-PROP-003 | Unit | P1 | merge(a, merge(b, c)) == merge(merge(a, b), c) | Associativity |
| 008.3-PROP-004 | Unit | P2 | merge({}, x) == x | Identity element |
| 008.3-PROP-005 | Unit | P2 | Result is valid JSON | Output validity |
| 008.3-PROP-006 | Unit | P2 | No mutation of inputs | Immutability |

### AC2: Multiple input sources

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.3-INT-001 | Integration | P0 | Merge from local JSON files | File input |
| 008.3-INT-002 | Integration | P1 | Merge from local YAML files | YAML support |
| 008.3-INT-003 | Integration | P1 | Merge from Git refs (uses:) | Git integration |
| 008.3-INT-004 | Integration | P1 | Merge inline schema objects | Inline input |
| 008.3-INT-005 | Integration | P0 | Mix file + Git + inline | Mixed sources |

### AC3: CLI command (tea schema merge)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.3-INT-006 | Integration | P0 | tea schema merge a.json b.json -o out.json | Basic CLI |
| 008.3-INT-007 | Integration | P1 | tea schema merge *.json -o out.json | Glob input |
| 008.3-INT-008 | Integration | P1 | tea schema merge --uses ref1 --uses ref2 | Git refs |
| 008.3-INT-009 | Integration | P1 | tea schema merge local.json --uses ref | Mixed |
| 008.3-INT-010 | Integration | P2 | Exit code on success (0) | CLI contract |
| 008.3-INT-011 | Integration | P2 | Exit code on error (non-zero) | CLI contract |

### AC4: YAML action (schema.merge)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.3-UNIT-018 | Unit | P0 | schema.merge action registered | Action registry |
| 008.3-INT-012 | Integration | P0 | schema.merge with path: sources | YAML action |
| 008.3-INT-013 | Integration | P1 | schema.merge with uses: sources | YAML action |
| 008.3-INT-014 | Integration | P1 | schema.merge with inline: sources | YAML action |
| 008.3-INT-015 | Integration | P1 | output_key writes to state | State update |

### AC5: JSON Schema validation post-merge

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.3-UNIT-019 | Unit | P1 | Valid JSON Schema passes | Validation |
| 008.3-UNIT-020 | Unit | P1 | Invalid JSON Schema fails | Validation |
| 008.3-UNIT-021 | Unit | P2 | Validation uses Draft 2020-12 | Standard compliance |
| 008.3-INT-016 | Integration | P1 | --validate flag triggers check | CLI integration |

### AC6: Dry-run mode

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.3-UNIT-022 | Unit | P1 | --dry-run prints to stdout | Preview mode |
| 008.3-UNIT-023 | Unit | P1 | --dry-run does not write file | No side effects |

### AC7-8: Integration and programmatic use

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.3-UNIT-024 | Unit | P0 | deep_merge() is importable | API surface |
| 008.3-UNIT-025 | Unit | P0 | merge_all() is importable | API surface |

### AC9: Cross-runtime parity

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.3-PARITY-001 | Integration | P0 | Python/Rust identical flat merge | Parity |
| 008.3-PARITY-002 | Integration | P0 | Python/Rust identical nested merge | Parity |
| 008.3-PARITY-003 | Integration | P0 | Python/Rust identical array handling | Parity |
| 008.3-PARITY-004 | Integration | P1 | Python/Rust identical null handling | Parity |

### E2E Validation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.3-E2E-001 | E2E | P0 | CLI merge workflow end-to-end | Critical path |
| 008.3-E2E-002 | E2E | P0 | YAML agent using schema.merge | YAML workflow |
| 008.3-E2E-003 | E2E | P1 | llamaextract.extract with merged schema | Integration |
| 008.3-E2E-004 | E2E | P2 | Rust CLI merge workflow | Rust critical path |

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| Merge semantics mismatch | PARITY-001,002,003,004 | Cross-runtime tests |
| Data loss on merge | PROP-001,002 | Property-based tests |
| Invalid output schema | UNIT-019,020,021 | Validation tests |
| CLI usability | INT-006 through INT-011 | CLI integration tests |

## Recommended Execution Order

1. **P0 Unit tests** (core algorithm)
   - UNIT-001 through UNIT-006, UNIT-009,010,014-017,018,024,025
2. **P0 Integration tests** (sources and CLI)
   - INT-001,005,006,012
3. **P0 Parity tests** (cross-runtime)
   - PARITY-001,002,003
4. **P0 E2E tests** (critical paths)
   - E2E-001, E2E-002
5. **P1 tests in order**
6. **Property-based tests** (invariants)
   - PROP-001 through PROP-006
7. **P2+ as time permits**

## Test Data Requirements

| Data | Description | Source |
|------|-------------|--------|
| Base Schema | Flat JSON Schema | fixtures/base.json |
| Overlay Schema | Overlay with conflicts | fixtures/overlay.json |
| Nested Schema | Deep nesting (5 levels) | fixtures/nested.json |
| Invalid Schema | Schema with errors | fixtures/invalid.json |

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Property-based tests for algorithm invariants
- [x] Cross-runtime parity explicitly tested
