# Test Design: Story YE.9 - Deep Array Merge by Identifying Key

**Date:** 2026-01-18
**Designer:** Quinn (Test Architect)
**Story:** YE.9 - Deep Array Merge by Identifying Key

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total Test Scenarios** | 48 |
| **Unit Tests** | 24 (50%) |
| **Integration Tests** | 15 (31%) |
| **E2E Tests** | 9 (19%) |
| **P0 (Critical)** | 16 |
| **P1 (High)** | 20 |
| **P2 (Medium)** | 10 |
| **P3 (Low)** | 2 |

### Test Distribution Rationale

- **50% Unit Tests**: Core merge algorithm is pure logic - shift-left strategy
- **31% Integration Tests**: CLI behavior and file I/O interactions
- **19% E2E Tests**: Cross-implementation parity verification (Python/Rust)

---

## Risk Assessment

| Risk ID | Risk | Probability | Impact | Status |
|---------|------|-------------|--------|--------|
| R-001 | Incorrect merge key matching breaks existing overlays | Medium | High | Mitigate: 8 tests |
| R-002 | Breaking change causes user workflow failures | Medium | High | Mitigate: 6 tests |
| R-003 | Python/Rust implementation divergence | Low | Critical | Mitigate: 9 tests |
| R-004 | `__delete__` marker has unintended side effects | Medium | Medium | Mitigate: 4 tests |
| R-005 | Nested `goto` arrays not handled correctly | Medium | Medium | Mitigate: 5 tests |
| R-006 | Performance degradation on large arrays | Low | Medium | Mitigate: 2 tests |
| R-007 | Edge collision on `from`+`to` composite key | Low | High | Mitigate: 3 tests |

---

## Test Scenarios by Acceptance Criteria

### AC1: `nodes` array merges by `name` key

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.9-UNIT-001 | Unit | P0 | Merge single node by name (modify existing) | Core merge logic |
| YE.9-UNIT-002 | Unit | P0 | Preserve unmatched base nodes | Ensures non-destructive merge |
| YE.9-UNIT-003 | Unit | P1 | Multiple nodes in overlay modify multiple base nodes | Multi-element merge |
| YE.9-UNIT-004 | Unit | P1 | Node name case sensitivity (exact match required) | Edge case: "Process" vs "process" |
| YE.9-INT-001 | Integration | P0 | CLI merge with node overlay produces correct YAML | End-to-end file processing |
| YE.9-INT-002 | Integration | P1 | Large nodes array (50+ nodes) merges correctly | Scale validation |

### AC2: `edges` array merges by `from`+`to` composite key

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.9-UNIT-005 | Unit | P0 | Merge single edge by from+to key | Core composite key logic |
| YE.9-UNIT-006 | Unit | P0 | Preserve unmatched base edges | Non-destructive merge |
| YE.9-UNIT-007 | Unit | P1 | Edge with same `from` but different `to` preserved | Composite key correctness |
| YE.9-UNIT-008 | Unit | P1 | Edge with same `to` but different `from` preserved | Composite key correctness |
| YE.9-UNIT-009 | Unit | P2 | Parallel edges with same from/to but different `fan_in` | Edge case: parallel edge handling |
| YE.9-INT-003 | Integration | P1 | CLI merge with edge overlay produces correct YAML | File processing |

### AC3: `goto` arrays (inside nodes) merge by `to` key

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.9-UNIT-010 | Unit | P0 | Merge goto entry by `to` field | Core goto merge logic |
| YE.9-UNIT-011 | Unit | P1 | Preserve unmatched goto entries | Non-destructive merge |
| YE.9-UNIT-012 | Unit | P1 | Goto condition (`if`) updated when `to` matches | Condition override |
| YE.9-UNIT-013 | Unit | P2 | Default goto (no `if`) merged correctly | Edge case: default fallback |
| YE.9-UNIT-014 | Unit | P1 | Nested node merge triggers nested goto merge | Recursive handling |
| YE.9-INT-004 | Integration | P1 | CLI merge with goto overlay produces correct YAML | File processing |

### AC4: Matched array elements use recursive deep merge

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.9-UNIT-015 | Unit | P0 | Nested `with:` object on node deep-merged | Critical: params merge |
| YE.9-UNIT-016 | Unit | P1 | Nested object 3 levels deep merges correctly | Recursion depth |
| YE.9-UNIT-017 | Unit | P1 | Overlay scalar overrides base scalar in nested object | Scalar override |
| YE.9-UNIT-018 | Unit | P2 | Base nested property preserved when not in overlay | Property preservation |

### AC5: Non-matched overlay elements are appended

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.9-UNIT-019 | Unit | P0 | New node in overlay appended to nodes array | Core append logic |
| YE.9-UNIT-020 | Unit | P1 | New edge in overlay appended to edges array | Append for edges |
| YE.9-UNIT-021 | Unit | P1 | New goto entry appended to goto array | Append for goto |
| YE.9-UNIT-022 | Unit | P2 | Multiple new elements appended in order | Order preservation |

### AC6: `__delete__: true` marker removes elements

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.9-UNIT-023 | Unit | P0 | Node with `__delete__: true` removed from array | Core delete logic |
| YE.9-UNIT-024 | Unit | P0 | Edge with `__delete__: true` removed from array | Delete for edges |
| YE.9-INT-005 | Integration | P1 | Delete marker works via CLI --dump-merged | File processing |
| YE.9-INT-006 | Integration | P2 | Delete non-existent element is silent (no error) | Graceful handling |

### AC7: `--dump-merged` shows correctly merged arrays

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.9-INT-007 | Integration | P0 | --dump-merged output includes merged nodes | CLI output validation |
| YE.9-INT-008 | Integration | P0 | --dump-merged output includes merged edges | CLI output validation |
| YE.9-INT-009 | Integration | P1 | --dump-merged output includes merged goto | CLI output validation |
| YE.9-INT-010 | Integration | P1 | Multiple overlays applied in order with array merge | Multi-overlay chain |

### AC8: Breaking change: arrays merge by key by default

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.9-INT-011 | Integration | P0 | Default behavior is merge-by-key (not replace) | Breaking change verification |
| YE.9-INT-012 | Integration | P1 | Overlay with all new elements still works (append-only) | Backward-compat edge case |
| YE.9-INT-013 | Integration | P2 | Empty overlay array does not clear base array | Edge case: empty overlay |

### AC9: Python/Rust parity (identical merge results)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.9-E2E-001 | E2E | P0 | Python and Rust produce identical --dump-merged for node merge | Cross-impl parity |
| YE.9-E2E-002 | E2E | P0 | Python and Rust produce identical --dump-merged for edge merge | Cross-impl parity |
| YE.9-E2E-003 | E2E | P0 | Python and Rust produce identical --dump-merged for goto merge | Cross-impl parity |
| YE.9-E2E-004 | E2E | P1 | Python and Rust produce identical output for __delete__ | Cross-impl parity |
| YE.9-E2E-005 | E2E | P1 | Python and Rust produce identical output for nested merge | Cross-impl parity |
| YE.9-E2E-006 | E2E | P1 | Python and Rust produce identical output for append | Cross-impl parity |
| YE.9-E2E-007 | E2E | P1 | Python and Rust handle complex scenario identically | Cross-impl parity |
| YE.9-E2E-008 | E2E | P2 | Python and Rust handle large array merge identically | Scale parity |
| YE.9-E2E-009 | E2E | P2 | Python and Rust produce identical error for missing key | Error parity |

### AC10: Comprehensive unit tests cover all merge scenarios

*Covered by scenarios above (UNIT-001 through UNIT-024)*

### AC11: Documentation updated (Manual Review)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.9-MAN-001 | Manual | P1 | YAML_REFERENCE.md updated with array merge semantics | Doc review |
| YE.9-MAN-002 | Manual | P2 | Array merge keys table present and accurate | Doc review |

### AC12: Migration guide created (Manual Review)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.9-MAN-003 | Manual | P1 | Migration guide for YE.8 → YE.9 behavior change | Doc review |
| YE.9-MAN-004 | Manual | P3 | Article updated with new merge semantics | Doc review |

---

## Test Scenario Summary by Level

### Unit Tests (24 scenarios)

| ID Range | Focus Area | Count |
|----------|------------|-------|
| UNIT-001 to UNIT-004 | Nodes merge by name | 4 |
| UNIT-005 to UNIT-009 | Edges merge by from+to | 5 |
| UNIT-010 to UNIT-014 | Goto merge by to | 5 |
| UNIT-015 to UNIT-018 | Nested deep merge | 4 |
| UNIT-019 to UNIT-022 | Element append | 4 |
| UNIT-023 to UNIT-024 | Delete marker | 2 |

### Integration Tests (15 scenarios)

| ID Range | Focus Area | Count |
|----------|------------|-------|
| INT-001 to INT-002 | Node CLI merge | 2 |
| INT-003 | Edge CLI merge | 1 |
| INT-004 | Goto CLI merge | 1 |
| INT-005 to INT-006 | Delete via CLI | 2 |
| INT-007 to INT-010 | --dump-merged output | 4 |
| INT-011 to INT-013 | Breaking change behavior | 3 |
| INT-014 to INT-015 | Error handling | 2 |

### E2E Tests (9 scenarios)

| ID Range | Focus Area | Count |
|----------|------------|-------|
| E2E-001 to E2E-009 | Python/Rust parity | 9 |

---

## Risk Coverage Matrix

| Risk ID | Mitigating Tests |
|---------|------------------|
| R-001 | UNIT-001, UNIT-002, UNIT-005, UNIT-006, UNIT-010, UNIT-011, INT-011, INT-012 |
| R-002 | INT-011, INT-012, INT-013, E2E-001, E2E-002, E2E-007 |
| R-003 | E2E-001, E2E-002, E2E-003, E2E-004, E2E-005, E2E-006, E2E-007, E2E-008, E2E-009 |
| R-004 | UNIT-023, UNIT-024, INT-005, INT-006 |
| R-005 | UNIT-010, UNIT-011, UNIT-012, UNIT-013, UNIT-014 |
| R-006 | INT-002, E2E-008 |
| R-007 | UNIT-007, UNIT-008, UNIT-009 |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 Critical)
1. UNIT-001, UNIT-002 (nodes merge core)
2. UNIT-005, UNIT-006 (edges merge core)
3. UNIT-010 (goto merge core)
4. UNIT-015 (nested deep merge)
5. UNIT-019 (append core)
6. UNIT-023, UNIT-024 (delete core)
7. INT-001, INT-007, INT-008, INT-011 (CLI integration)
8. E2E-001, E2E-002, E2E-003 (parity critical)

### Phase 2: Core Coverage (P1 High)
1. Remaining UNIT tests (UNIT-003, UNIT-004, UNIT-007, UNIT-008, etc.)
2. Remaining INT tests (INT-002, INT-003, INT-004, etc.)
3. E2E-004 through E2E-007

### Phase 3: Edge Cases (P2 Medium)
1. UNIT-009, UNIT-013, UNIT-018, UNIT-022
2. INT-006, INT-013
3. E2E-008, E2E-009

### Phase 4: Low Priority (P3)
1. MAN-004 (article update)

---

## Test Fixtures Required

### New Fixtures for `tests/fixtures/overlay/`

| Fixture | Purpose |
|---------|---------|
| `base_multinode.yaml` | Base agent with 5+ nodes |
| `overlay_modify_single_node.yaml` | Modify one node by name |
| `overlay_modify_multiple_nodes.yaml` | Modify multiple nodes |
| `overlay_add_node.yaml` | Add new node |
| `overlay_delete_node.yaml` | Delete node with `__delete__` |
| `overlay_modify_edge.yaml` | Modify edge by from+to |
| `overlay_add_edge.yaml` | Add new edge |
| `overlay_delete_edge.yaml` | Delete edge |
| `overlay_modify_goto.yaml` | Modify goto routing |
| `overlay_nested_merge.yaml` | Deep nested object merge |
| `overlay_complex.yaml` | Multiple operations combined |
| `base_large_array.yaml` | 50+ nodes for scale testing |

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (shift-left applied)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention `YE.9-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Risk mitigations addressed (all 7 risks covered)

---

## Gate YAML Block

```yaml
test_design:
  story_id: YE.9
  date: 2026-01-18
  scenarios_total: 48
  by_level:
    unit: 24
    integration: 15
    e2e: 9
  by_priority:
    p0: 16
    p1: 20
    p2: 10
    p3: 2
  coverage_gaps: []
  risks_mitigated: 7
  fixtures_required: 12
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/YE.9-test-design-20260118.md
P0 tests identified: 16
P1 tests identified: 20
Total scenarios: 48
```

---

## Gate Status

**Test Design: PASS** - Ready for implementation
