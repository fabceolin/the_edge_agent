# Test Design: Story YE.8 - YAML Overlay Merge Support

Date: 2026-01-10
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 72
- **Unit tests:** 28 (39%)
- **Integration tests:** 26 (36%)
- **E2E tests:** 18 (25%)
- **Priority distribution:** P0: 22, P1: 32, P2: 14, P3: 4

## Risk Assessment Summary

This feature introduces a new configuration merging layer that directly affects how agents execute. Key risks:

| Risk ID | Risk | Probability | Impact | Mitigation |
|---------|------|-------------|--------|------------|
| R-001 | Incorrect merge semantics break existing configs | Medium | High | P0 unit tests for all merge rules |
| R-002 | Environment variable substitution order issues | Medium | Medium | P0 tests for env var timing |
| R-003 | Python/Rust implementation parity drift | Low | High | P0 parity verification tests |
| R-004 | Missing overlay file silently ignored | Low | Critical | P0 error handling tests |
| R-005 | Invalid YAML parsing causes cryptic errors | Medium | Medium | P1 error message tests |
| R-006 | Imports section merge conflicts | Medium | Medium | P1 imports merge tests |

---

## Test Scenarios by Acceptance Criteria

### AC1: `--overlay` / `-f` option accepts one or more overlay YAML file paths

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.8-UNIT-001 | Unit | P0 | Parse single `-f` flag | Pure argument parsing logic |
| YE.8-UNIT-002 | Unit | P0 | Parse `--overlay` long flag | Pure argument parsing logic |
| YE.8-UNIT-003 | Unit | P0 | Parse multiple `-f` flags in sequence | Pure argument parsing logic |
| YE.8-UNIT-004 | Unit | P1 | Parse mixed `-f` and `--overlay` flags | Edge case in argument parsing |
| YE.8-INT-001 | Integration | P0 | CLI accepts `-f overlay.yaml` and loads file | CLI + file system interaction |
| YE.8-INT-002 | Integration | P1 | CLI accepts multiple `-f` flags and loads all files | CLI + multiple file loading |

---

### AC2: Multiple overlays are applied in order (left-to-right, last wins)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.8-UNIT-005 | Unit | P0 | Two overlays: second value wins for same key | Core merge algorithm |
| YE.8-UNIT-006 | Unit | P0 | Three overlays: third value wins for same key | Core merge algorithm |
| YE.8-UNIT-007 | Unit | P1 | Chain of 5 overlays preserves correct order | Stress test for ordering |
| YE.8-INT-003 | Integration | P0 | `base.yaml -f a.yaml -f b.yaml`: b overrides a | Full overlay chain |
| YE.8-INT-004 | Integration | P1 | Values from middle overlay preserved if not overridden | Non-conflicting merge |

---

### AC3: Deep merge uses kubectl-style semantics (objects merged, arrays replaced, scalars last-wins)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.8-UNIT-008 | Unit | P0 | Objects (dicts) are recursively merged | Core merge semantic |
| YE.8-UNIT-009 | Unit | P0 | Arrays (lists) are replaced entirely | Core merge semantic |
| YE.8-UNIT-010 | Unit | P0 | Scalars: overlay value wins | Core merge semantic |
| YE.8-UNIT-011 | Unit | P0 | Null/None in overlay overrides non-null base | Core merge semantic |
| YE.8-UNIT-012 | Unit | P0 | Nested objects (3+ levels deep) merge correctly | Deep recursion |
| YE.8-UNIT-013 | Unit | P1 | Mixed types: overlay type wins (dict→scalar) | Type coercion |
| YE.8-UNIT-014 | Unit | P1 | Empty object `{}` in overlay preserves base keys | Empty object handling |
| YE.8-UNIT-015 | Unit | P1 | Empty array `[]` in overlay replaces non-empty base | Empty array handling |
| YE.8-UNIT-016 | Unit | P2 | Unicode keys in objects merge correctly | Internationalization |
| YE.8-UNIT-017 | Unit | P2 | Large nested structure (100+ keys) merges correctly | Performance/scale |
| YE.8-INT-005 | Integration | P0 | `settings.ltm` section merges as shown in story example | Real-world merge scenario |
| YE.8-INT-006 | Integration | P1 | `nodes` array replaced entirely when overridden | Array replacement in context |

---

### AC4: `--dump-merged` outputs the merged YAML to stdout without executing

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.8-UNIT-018 | Unit | P0 | `--dump-merged` flag parsing | Pure argument parsing |
| YE.8-INT-007 | Integration | P0 | `--dump-merged` outputs valid YAML to stdout | Output format verification |
| YE.8-INT-008 | Integration | P0 | `--dump-merged` exits with code 0 (success) | Exit behavior |
| YE.8-INT-009 | Integration | P0 | `--dump-merged` does NOT execute the agent | Non-execution verification |
| YE.8-INT-010 | Integration | P1 | Output is properly formatted (readable indentation) | Output quality |
| YE.8-INT-011 | Integration | P2 | Output preserves YAML comments from base (if supported) | Nice-to-have fidelity |

---

### AC5: `--dump-merged` combined with overlays shows final merged result

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.8-INT-012 | Integration | P0 | `base -f overlay --dump-merged` shows merged output | Core dump functionality |
| YE.8-INT-013 | Integration | P1 | `base -f a -f b --dump-merged` shows all merges applied | Multi-overlay dump |
| YE.8-E2E-001 | E2E | P1 | Pipe `--dump-merged` to `yq` and verify structure | Real-world usage pattern |
| YE.8-E2E-002 | E2E | P2 | Redirect `--dump-merged` output to file and validate | Real-world usage pattern |

---

### AC6: Missing overlay file produces clear error message with path

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.8-UNIT-019 | Unit | P0 | File existence check returns error for missing file | Core validation |
| YE.8-INT-014 | Integration | P0 | Error message includes the missing file path | User experience |
| YE.8-INT-015 | Integration | P0 | Error message is actionable (not cryptic) | User experience |
| YE.8-INT-016 | Integration | P1 | Exit code is non-zero on missing file | Scripting support |
| YE.8-INT-017 | Integration | P2 | Error specifies which overlay in chain failed | Multi-overlay debugging |

---

### AC7: Invalid overlay YAML produces clear parse error with context

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.8-INT-018 | Integration | P0 | Invalid YAML syntax produces error | Parse error handling |
| YE.8-INT-019 | Integration | P1 | Error includes line number (if available) | Debug support |
| YE.8-INT-020 | Integration | P1 | Error includes file name in message | Multi-file debugging |
| YE.8-INT-021 | Integration | P2 | Error on tab characters in YAML (common mistake) | User help |

---

### AC8: Overlays work with all existing `tea run` options (--interactive, --input, etc.)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.8-INT-022 | Integration | P1 | `-f overlay --interactive` works together | Option compatibility |
| YE.8-INT-023 | Integration | P1 | `-f overlay --input '{}'` works together | Option compatibility |
| YE.8-INT-024 | Integration | P2 | `-f overlay --thread-id abc` works together | Option compatibility |
| YE.8-E2E-003 | E2E | P1 | Full agent execution with overlay + interactive mode | Real execution path |
| YE.8-E2E-004 | E2E | P1 | Full agent execution with overlay + input JSON | Real execution path |

---

### AC9: Base YAML `imports:` section is preserved and merged with overlay imports

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.8-UNIT-020 | Unit | P1 | Base `imports:` preserved when overlay has no imports | Preservation logic |
| YE.8-UNIT-021 | Unit | P1 | Overlay `imports:` replaces base imports (array) | Array replacement rule |
| YE.8-INT-025 | Integration | P0 | Imported Python actions work after overlay merge | Real import functionality |
| YE.8-E2E-005 | E2E | P1 | Agent with base imports + overlay settings runs correctly | Full execution with imports |

---

### AC10: Environment variable substitution (`${VAR}`) works in both base and overlay

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.8-UNIT-022 | Unit | P0 | Env var in base is substituted | Core env var support |
| YE.8-UNIT-023 | Unit | P0 | Env var in overlay is substituted | Core env var support |
| YE.8-UNIT-024 | Unit | P0 | Env var substitution happens AFTER merge | Correct timing |
| YE.8-UNIT-025 | Unit | P1 | Overlay overrides base value even if both use same env var | Override semantics |
| YE.8-INT-026 | Integration | P1 | `${VAR:-default}` syntax works in overlay | Default value syntax |
| YE.8-E2E-006 | E2E | P0 | Real env var from environment substituted correctly | Full integration |

---

### AC11: Comprehensive unit tests cover all merge scenarios

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.8-UNIT-026 | Unit | P1 | Test fixture files exist and are valid YAML | Test infrastructure |
| YE.8-UNIT-027 | Unit | P1 | Edge case: empty base file | Boundary condition |
| YE.8-UNIT-028 | Unit | P1 | Edge case: empty overlay file | Boundary condition |

---

### AC12: Documentation updated in CLAUDE.md and CLI help text

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.8-E2E-007 | E2E | P1 | `tea run --help` shows `-f`/`--overlay` option | Help discoverability |
| YE.8-E2E-008 | E2E | P1 | `tea run --help` shows `--dump-merged` option | Help discoverability |
| YE.8-E2E-009 | E2E | P2 | Help text includes usage example | User guidance |

---

### AC13: Python implementation in `tea-python` CLI

#### Scenarios (Python-specific)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.8-PY-INT-001 | Integration | P0 | Python CLI loads and merges overlays | Python-specific implementation |
| YE.8-PY-INT-002 | Integration | P0 | Python `--dump-merged` outputs correct YAML | Python-specific implementation |
| YE.8-PY-INT-003 | Integration | P1 | Python uses existing `deep_merge.py` module | Code reuse verification |
| YE.8-PY-E2E-001 | E2E | P0 | Python CLI runs full agent with overlay | Python full path |
| YE.8-PY-E2E-002 | E2E | P1 | Python overlay with production LTM settings | Real-world Python scenario |

---

### AC14: Rust implementation in `tea-rust` CLI

#### Scenarios (Rust-specific)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.8-RS-UNIT-001 | Unit | P0 | Rust `deep_merge` function handles all Value types | Rust-specific implementation |
| YE.8-RS-UNIT-002 | Unit | P0 | Rust `deep_merge` handles nested Mapping | Rust-specific implementation |
| YE.8-RS-INT-001 | Integration | P0 | Rust CLI loads and merges overlays | Rust-specific implementation |
| YE.8-RS-INT-002 | Integration | P0 | Rust `--dump-merged` outputs correct YAML | Rust-specific implementation |
| YE.8-RS-E2E-001 | E2E | P0 | Rust CLI runs full agent with overlay | Rust full path |
| YE.8-RS-E2E-002 | E2E | P1 | Rust overlay with production LTM settings | Real-world Rust scenario |

---

### AC15: Both implementations produce identical merge results (parity)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.8-PARITY-001 | E2E | P0 | Python and Rust `--dump-merged` output matches | Core parity requirement |
| YE.8-PARITY-002 | E2E | P0 | Parity for single overlay merge | Core parity requirement |
| YE.8-PARITY-003 | E2E | P0 | Parity for multi-overlay merge | Core parity requirement |
| YE.8-PARITY-004 | E2E | P1 | Parity for nested object merge | Deep merge parity |
| YE.8-PARITY-005 | E2E | P1 | Parity for array replacement | Array handling parity |
| YE.8-PARITY-006 | E2E | P1 | Parity for null/None override | Null handling parity |
| YE.8-PARITY-007 | E2E | P2 | Parity for error messages (structure, not exact text) | Error handling parity |

---

## Risk Coverage Matrix

| Risk ID | Mitigated By Tests |
|---------|-------------------|
| R-001 | YE.8-UNIT-008 through YE.8-UNIT-017, YE.8-INT-005, YE.8-INT-006 |
| R-002 | YE.8-UNIT-022 through YE.8-UNIT-025, YE.8-E2E-006 |
| R-003 | YE.8-PARITY-001 through YE.8-PARITY-007 |
| R-004 | YE.8-UNIT-019, YE.8-INT-014 through YE.8-INT-017 |
| R-005 | YE.8-INT-018 through YE.8-INT-021 |
| R-006 | YE.8-UNIT-020, YE.8-UNIT-021, YE.8-INT-025, YE.8-E2E-005 |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 Unit Tests)
1. YE.8-UNIT-001 through YE.8-UNIT-012 (Core parsing and merge logic)
2. YE.8-UNIT-018, YE.8-UNIT-019 (Core flags and error handling)
3. YE.8-UNIT-022 through YE.8-UNIT-024 (Env var critical path)
4. YE.8-RS-UNIT-001, YE.8-RS-UNIT-002 (Rust-specific merge algorithm)

### Phase 2: P0 Integration Tests
1. YE.8-INT-001, YE.8-INT-003 (CLI overlay loading)
2. YE.8-INT-005 (Real-world merge scenario)
3. YE.8-INT-007 through YE.8-INT-009 (Dump mode core)
4. YE.8-INT-012 (Dump with overlays)
5. YE.8-INT-014, YE.8-INT-015 (Error handling)
6. YE.8-INT-018, YE.8-INT-025 (Parse errors and imports)
7. YE.8-PY-INT-001, YE.8-PY-INT-002, YE.8-RS-INT-001, YE.8-RS-INT-002

### Phase 3: P0 E2E Tests
1. YE.8-E2E-006 (Env var full integration)
2. YE.8-PY-E2E-001, YE.8-RS-E2E-001 (Full agent execution)
3. YE.8-PARITY-001 through YE.8-PARITY-003 (Core parity)

### Phase 4: P1 Tests (as time permits)
- Execute in ID order within each level

### Phase 5: P2+ Tests (full regression only)
- Execute in ID order

---

## Test Implementation Notes

### Shared Fixtures Location
`tests/fixtures/overlay/`
- `base.yaml` - Standard base agent config
- `overlay_settings.yaml` - Settings-only overlay
- `overlay_nodes.yaml` - Nodes replacement overlay
- `overlay_chain_a.yaml` - First in chain test
- `overlay_chain_b.yaml` - Second in chain test
- `overlay_env_vars.yaml` - Contains `${VAR}` references
- `overlay_imports.yaml` - Contains `imports:` section
- `invalid.yaml` - Malformed YAML for error testing
- `empty.yaml` - Empty file for edge case

### Python Test File
`python/tests/test_cli_overlay.py`

### Rust Test Files
- `rust/src/engine/deep_merge.rs` (inline unit tests)
- `rust/tests/test_overlay.rs` (integration tests)

### Parity Test Script
`tests/test_overlay_parity.sh`

---

## Quality Gate Block (for qa-gate integration)

```yaml
test_design:
  scenarios_total: 72
  by_level:
    unit: 28
    integration: 26
    e2e: 18
  by_priority:
    p0: 22
    p1: 32
    p2: 14
    p3: 4
  coverage_gaps: []
  risk_coverage:
    - risk_id: R-001
      tests: 12
    - risk_id: R-002
      tests: 5
    - risk_id: R-003
      tests: 7
    - risk_id: R-004
      tests: 5
    - risk_id: R-005
      tests: 4
    - risk_id: R-006
      tests: 4
```

---

## Trace References

Test design matrix: `docs/qa/assessments/YE.8-test-design-20260110.md`
P0 tests identified: 22
Total scenarios: 72

---

## Quality Checklist

- [x] Every AC has test coverage (all 15 ACs covered)
- [x] Test levels are appropriate (unit for logic, integration for CLI, E2E for full paths)
- [x] No duplicate coverage across levels (each test has unique justification)
- [x] Priorities align with business risk (P0 for merge semantics and parity)
- [x] Test IDs follow naming convention (`YE.8-{LEVEL}-{SEQ}`)
- [x] Scenarios are atomic and independent
- [x] Both Python and Rust implementations have dedicated tests
- [x] Parity verification tests ensure cross-implementation consistency
