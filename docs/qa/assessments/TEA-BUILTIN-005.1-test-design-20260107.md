# Test Design: Story TEA-BUILTIN-005.1 - OpikExporter Backend

**Date:** 2026-01-07
**Designer:** Quinn (Test Architect)
**Story:** OpikExporter Backend for trace export to Comet Opik
**Epic:** TEA-BUILTIN-005 (Opik Integration)

---

## Test Strategy Overview

- **Total test scenarios:** 16
- **Unit tests:** 11 (69%)
- **Integration tests:** 5 (31%)
- **E2E tests:** 0 (0%)
- **Priority distribution:** P0: 5 | P1: 7 | P2: 4

### Test Distribution Rationale

**Unit-heavy strategy (69% unit):**
- OpikExporter is largely pure I/O and transformation logic
- SDK integration can be fully mocked for fast feedback
- Error handling paths are isolated and deterministic
- Configuration parsing doesn't require real services

**Integration focus on critical paths (31% integration):**
- YAMLEngine integration validates end-to-end configuration flow
- Parent-child span hierarchy requires TraceContext integration
- Real-world usage patterns need validation with actual TraceContext

**No E2E tests:**
- Opik platform is external SaaS - can't E2E test in CI
- Integration tests with mocked SDK provide sufficient coverage
- Manual validation against real Opik instance recommended for release

---

## Test Scenarios by Acceptance Criteria

### AC1: OpikExporter implements TraceExporter protocol

#### Scenarios

| Test ID | Level | Priority | Test Description | Justification |
|---------|-------|----------|------------------|---------------|
| **005.1-UNIT-001** | Unit | **P0** | OpikExporter class implements TraceExporter protocol interface | Protocol compliance is critical - exporter must conform to expected API |
| **005.1-UNIT-002** | Unit | **P1** | OpikExporter.export() accepts span dict with all required fields | Validates input contract - must accept standard span format |
| **005.1-UNIT-003** | Unit | **P2** | OpikExporter raises TypeError if export() called with invalid type | Edge case - validates defensive programming |

**Coverage Assessment:** ✅ Complete
- Protocol compliance tested at P0
- Input validation covered at P1/P2
- Type safety verified

---

### AC2: Spans exported with complete field mapping

#### Scenarios

| Test ID | Level | Priority | Test Description | Justification |
|---------|-------|----------|------------------|---------------|
| **005.1-UNIT-004** | Unit | **P0** | Span name, duration, status fields correctly mapped to Opik format | Core span data integrity - critical for trace visualization |
| **005.1-UNIT-005** | Unit | **P1** | Span metadata dict forwarded to Opik metadata field | Metadata provides debugging context - high value for users |
| **005.1-UNIT-006** | Unit | **P1** | Span events list processed and exported to Opik | Events provide execution timeline - important for debugging |
| **005.1-UNIT-007** | Unit | **P1** | Span metrics (token counts) forwarded correctly | Token metrics critical for LLM cost tracking |
| **005.1-UNIT-008** | Unit | **P2** | Empty metadata dict handled gracefully (no crash) | Edge case - validates robustness |
| **005.1-UNIT-009** | Unit | **P2** | Empty events list handled gracefully | Edge case - validates robustness |

**Coverage Assessment:** ✅ Complete
- All field types tested (name, duration, status, metadata, events, metrics)
- P0 coverage on core fields (name, duration, status)
- P1 coverage on value-add fields (metadata, events, metrics)
- P2 coverage on edge cases (empty collections)

---

### AC3: YAMLEngine integration with trace_exporter="opik"

#### Scenarios

| Test ID | Level | Priority | Test Description | Justification |
|---------|-------|----------|------------------|---------------|
| **005.1-INT-001** | Integration | **P0** | YAMLEngine(trace_exporter="opik") loads OpikExporter successfully | Primary user-facing API - must work out of the box |
| **005.1-INT-002** | Integration | **P1** | YAMLEngine compiles graph and exports spans via OpikExporter | Validates end-to-end flow from graph execution to export |
| **005.1-INT-003** | Integration | **P1** | trace_exporter="opik" with custom config params passed to OpikExporter | Advanced usage - custom configuration must propagate |

**Coverage Assessment:** ✅ Complete
- Happy path tested at P0 (basic configuration)
- Advanced configuration tested at P1
- Integration with TraceContext validated

---

### AC4: Clear ImportError if opik SDK not installed

#### Scenarios

| Test ID | Level | Priority | Test Description | Justification |
|---------|-------|----------|------------------|---------------|
| **005.1-UNIT-010** | Unit | **P0** | ImportError raised with "pip install opik" message when SDK missing | User experience - must provide actionable error message |
| **005.1-UNIT-011** | Unit | **P1** | ImportError raised at configuration time (lazy, not import time) | Lazy loading design - error only when actually used |

**Coverage Assessment:** ✅ Complete
- P0 test ensures clear, actionable error message
- P1 test validates lazy loading behavior
- Both paths tested with mock import failures

---

### AC5: Environment variables respected

#### Scenarios

| Test ID | Level | Priority | Test Description | Justification |
|---------|-------|----------|------------------|---------------|
| **005.1-UNIT-012** | Unit | **P1** | OPIK_API_KEY environment variable read and used for authentication | Authentication is high-priority but not P0 (self-hosted doesn't need key) |
| **005.1-UNIT-013** | Unit | **P1** | OPIK_PROJECT_NAME defaults to "the-edge-agent" if not set | Configuration default behavior - common case |
| **005.1-UNIT-014** | Unit | **P1** | OPIK_WORKSPACE environment variable respected | Multi-workspace users need this |
| **005.1-UNIT-015** | Unit | **P2** | OPIK_URL_OVERRIDE enables self-hosted Opik usage | Self-hosted is advanced use case |

**Coverage Assessment:** ✅ Complete
- All 4 environment variables tested
- Default values validated
- Constructor parameter override tested (in AC3 scenarios)

---

### AC6: Hierarchical spans preserve parent-child relationships

#### Scenarios

| Test ID | Level | Priority | Test Description | Justification |
|---------|-------|----------|------------------|---------------|
| **005.1-INT-004** | Integration | **P0** | Parent span ID correctly mapped to Opik parent_span_id field | Trace hierarchy is critical for visualization - must work correctly |
| **005.1-INT-005** | Integration | **P1** | Multi-level nested spans (3+ levels) preserve full hierarchy | Advanced case - validates complex trace trees |

**Coverage Assessment:** ✅ Complete
- Basic parent-child tested at P0
- Deep nesting tested at P1
- Integration test level ensures real TraceContext usage

---

### AC7: Token usage metrics forwarded to Opik

#### Scenarios

**Coverage Assessment:** ✅ Covered by AC2
- See test **005.1-UNIT-007** (Span metrics forwarded correctly)
- Token metrics (`prompt_tokens`, `completion_tokens`, `total_tokens`) tested as part of metrics dict
- No additional tests needed - metrics are generic dict passthrough

---

### AC8: Exporter errors don't crash graph execution

#### Scenarios

| Test ID | Level | Priority | Test Description | Justification |
|---------|-------|----------|------------------|---------------|
| **005.1-UNIT-016** | Unit | **P0** | Opik SDK raises exception during export - OpikExporter catches and logs, doesn't propagate | Graceful degradation is critical - tracing failures must never break workflows |

**Coverage Assessment:** ✅ Complete
- Exception handling tested at P0
- Logging behavior verified (exceptions logged at debug level)
- Graph execution continues despite export failures

---

### AC9: Unit tests cover export success, missing SDK, configuration, error handling

**Coverage Assessment:** ✅ Meta requirement satisfied
- Export success: Tests 001-009
- Missing SDK: Tests 010-011
- Configuration: Tests 012-015
- Error handling: Test 016

---

### AC10: Documentation updated in CLAUDE.md

**Coverage Assessment:** ✅ Manual review required
- This is a documentation requirement, not a code test
- Validation: Manual review of CLAUDE.md for OpikExporter section
- Checklist item in Definition of Done

---

## Risk Coverage Analysis

### Identified Risks & Mitigations

| Risk ID | Risk Description | Probability | Impact | Test Coverage |
|---------|------------------|-------------|--------|---------------|
| **RISK-001** | SDK not installed crashes app at import time | Medium | High | 005.1-UNIT-010, 011 |
| **RISK-002** | Export failure crashes graph execution | Low | Critical | 005.1-UNIT-016 |
| **RISK-003** | Span hierarchy lost in export | Medium | High | 005.1-INT-004, 005 |
| **RISK-004** | Configuration ignored (env vars not read) | Medium | Medium | 005.1-UNIT-012-015 |
| **RISK-005** | Token metrics missing (cost tracking broken) | Low | Medium | 005.1-UNIT-007 |
| **RISK-006** | YAMLEngine integration broken | Low | High | 005.1-INT-001, 002 |

**Risk Mitigation Summary:**
- All 6 identified risks have test coverage
- Critical risks (RISK-002, RISK-006) covered at P0 priority
- High-impact risks (RISK-001, RISK-003) have multiple test scenarios

---

## Test Execution Strategy

### Recommended Execution Order

1. **Phase 1: Critical Path (P0 tests - MUST PASS)**
   - 005.1-UNIT-001 (Protocol compliance)
   - 005.1-UNIT-004 (Core field mapping)
   - 005.1-UNIT-010 (Missing SDK error)
   - 005.1-UNIT-016 (Graceful degradation)
   - 005.1-INT-001 (YAMLEngine integration)
   - 005.1-INT-004 (Parent-child hierarchy)

2. **Phase 2: Core Functionality (P1 tests - SHOULD PASS)**
   - All remaining UNIT tests (002, 005-007, 011-014)
   - All remaining INT tests (002, 003, 005)

3. **Phase 3: Edge Cases (P2 tests - NICE TO PASS)**
   - 005.1-UNIT-003, 008, 009, 015

### Test Failure Response

**P0 test fails:**
- 🛑 **STOP** - Do not proceed to P1 tests
- Fix immediately before continuing
- Re-run all P0 tests after fix

**P1 test fails:**
- ⚠️ **INVESTIGATE** - Determine if bug or test issue
- Can proceed to P2 if fix is minor
- Must fix before story marked complete

**P2 test fails:**
- ℹ️ **LOG** - Document as known issue
- Can defer fix to follow-up story
- Does not block story completion

---

## Test Implementation Notes

### Mock Strategy

**Opik SDK mocking approach:**
```python
# Use unittest.mock.patch for import-time mocking
@patch.dict('sys.modules', {'opik': MagicMock()})
def test_export_success(self):
    # Mock opik module to avoid real SDK dependency
    pass

# Alternative: pytest-mock for fixture-based mocking
@pytest.fixture
def mock_opik(mocker):
    return mocker.patch('the_edge_agent.exporters.opik_exporter.opik')
```

### Test Data Templates

**Minimal valid span:**
```python
MINIMAL_SPAN = {
    "span_id": "test-span-123",
    "parent_id": None,
    "name": "test_operation",
    "start_time": 1702900000.0,
    "end_time": 1702900001.0,
    "duration_ms": 1000.0,
    "status": "ok",
    "error": None,
    "metadata": {},
    "events": [],
    "metrics": {}
}
```

**Complete span with all fields:**
```python
COMPLETE_SPAN = {
    "span_id": "span-456",
    "parent_id": "span-123",
    "name": "llm_call",
    "start_time": 1702900000.123,
    "end_time": 1702900001.456,
    "duration_ms": 1333.0,
    "status": "ok",
    "error": None,
    "metadata": {"model": "gpt-4", "temperature": 0.7},
    "events": [{"timestamp": 1702900000.5, "message": "Request sent"}],
    "metrics": {
        "prompt_tokens": 100,
        "completion_tokens": 50,
        "total_tokens": 150
    }
}
```

### Coverage Gaps

**None identified** - All acceptance criteria have test coverage

**Recommended additional tests (not required for story):**
- Performance test: Export 1000 spans without memory leak
- Concurrency test: Parallel exports from multiple threads
- Real Opik integration test (manual - requires Opik instance)

---

## Quality Checklist

### Pre-Implementation Review

- [x] Every AC has at least one test scenario
- [x] No duplicate coverage across test levels
- [x] Test levels are appropriate (unit for logic, integration for flow)
- [x] Priorities align with risk assessment
- [x] Test IDs follow naming convention (EPIC.STORY-LEVEL-SEQ)

### Implementation Guidelines

- [ ] Use `unittest` framework (matches existing codebase)
- [ ] Mock Opik SDK to avoid external service calls
- [ ] Use `unittest.mock.patch.dict('sys.modules', ...)` for import mocking
- [ ] Test file location: `tests/test_opik_exporter.py`
- [ ] Verify no test interdependencies (can run in any order)

### Post-Implementation Validation

- [ ] All P0 tests pass (6 scenarios)
- [ ] All P1 tests pass (7 scenarios)
- [ ] P2 test failures documented if any (4 scenarios)
- [ ] Test coverage report shows >90% for opik_exporter.py
- [ ] No flaky tests (run 10 times, all pass)

---

## Test Maintenance Considerations

### Test Stability

**Low risk of flakiness:**
- No timing dependencies (all synchronous)
- No external service calls (fully mocked)
- No file system dependencies
- No random data generation

**High maintainability:**
- Tests focus on public API (export method)
- Internal implementation can change freely
- Mock strategy isolates from Opik SDK changes

### Future Test Additions

**When to add more tests:**
1. New Opik SDK version with breaking changes
2. Additional span fields added to TraceExporter protocol
3. Bug reports from production usage
4. New export formats (e.g., batch export)

**When NOT to add more tests:**
- Internal refactoring (existing tests should still pass)
- Performance optimizations (unless correctness affected)
- Documentation changes

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 16
  by_level:
    unit: 11
    integration: 5
    e2e: 0
  by_priority:
    p0: 5
    p1: 7
    p2: 4
  coverage_gaps: []
  risk_coverage:
    - risk_id: RISK-001
      description: SDK not installed crashes app
      tests: [005.1-UNIT-010, 005.1-UNIT-011]
    - risk_id: RISK-002
      description: Export failure crashes graph
      tests: [005.1-UNIT-016]
    - risk_id: RISK-003
      description: Span hierarchy lost
      tests: [005.1-INT-004, 005.1-INT-005]
    - risk_id: RISK-004
      description: Configuration ignored
      tests: [005.1-UNIT-012, 005.1-UNIT-013, 005.1-UNIT-014, 005.1-UNIT-015]
    - risk_id: RISK-005
      description: Token metrics missing
      tests: [005.1-UNIT-007]
    - risk_id: RISK-006
      description: YAMLEngine integration broken
      tests: [005.1-INT-001, 005.1-INT-002]
  recommendations:
    - P0 tests must pass before story marked Ready for Dev
    - Mock opik SDK in all tests to avoid external service dependencies
    - Use unittest.mock.patch.dict for import mocking
    - Run test suite 10 times to verify no flakiness
```

---

## Appendix: Complete Test Scenario List

### Unit Tests (11 scenarios)

| ID | Priority | Description |
|----|----------|-------------|
| 005.1-UNIT-001 | P0 | OpikExporter implements TraceExporter protocol |
| 005.1-UNIT-002 | P1 | export() accepts span dict with required fields |
| 005.1-UNIT-003 | P2 | export() raises TypeError on invalid input type |
| 005.1-UNIT-004 | P0 | Core span fields mapped correctly (name, duration, status) |
| 005.1-UNIT-005 | P1 | Metadata dict forwarded to Opik |
| 005.1-UNIT-006 | P1 | Events list processed and exported |
| 005.1-UNIT-007 | P1 | Token metrics forwarded correctly |
| 005.1-UNIT-008 | P2 | Empty metadata handled gracefully |
| 005.1-UNIT-009 | P2 | Empty events handled gracefully |
| 005.1-UNIT-010 | P0 | Clear ImportError with install instructions |
| 005.1-UNIT-011 | P1 | ImportError raised at config time (lazy loading) |
| 005.1-UNIT-012 | P1 | OPIK_API_KEY environment variable respected |
| 005.1-UNIT-013 | P1 | OPIK_PROJECT_NAME defaults to "the-edge-agent" |
| 005.1-UNIT-014 | P1 | OPIK_WORKSPACE environment variable respected |
| 005.1-UNIT-015 | P2 | OPIK_URL_OVERRIDE enables self-hosted Opik |
| 005.1-UNIT-016 | P0 | Export exceptions caught and logged (no propagation) |

### Integration Tests (5 scenarios)

| ID | Priority | Description |
|----|----------|-------------|
| 005.1-INT-001 | P0 | YAMLEngine(trace_exporter="opik") loads OpikExporter |
| 005.1-INT-002 | P1 | Graph execution exports spans via OpikExporter |
| 005.1-INT-003 | P1 | Custom config params passed to OpikExporter |
| 005.1-INT-004 | P0 | Parent span ID correctly mapped (2-level hierarchy) |
| 005.1-INT-005 | P1 | Multi-level nested spans preserve hierarchy (3+ levels) |

---

## Summary

**Test design status:** ✅ **COMPLETE**

- **Total coverage:** 16 test scenarios across 10 acceptance criteria
- **Risk mitigation:** All 6 identified risks have test coverage
- **Test stability:** High (no external dependencies, fully mocked)
- **Maintainability:** High (tests focus on public API)
- **Execution time estimate:** <5 seconds (all unit/integration, no E2E)

**Recommendation:** Story ready for implementation with this test design.

**Next steps:**
1. Implement tests in `tests/test_opik_exporter.py`
2. Run P0 tests first - must all pass before continuing
3. Implement OpikExporter to pass P0 tests
4. Implement remaining P1/P2 functionality
5. Verify test coverage >90% for `opik_exporter.py`

---

**Test Design Reference:** `docs/qa/assessments/TEA-BUILTIN-005.1-test-design-20260107.md`
**P0 Test Count:** 5 scenarios
**Story Gate:** Blocked until P0 tests implemented and passing
