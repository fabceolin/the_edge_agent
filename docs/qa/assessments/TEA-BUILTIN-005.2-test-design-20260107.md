# Test Design: Story TEA-BUILTIN-005.2

Date: 2026-01-07
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios**: 22
- **Unit tests**: 16 (73%)
- **Integration tests**: 6 (27%)
- **E2E tests**: 0 (0%)
- **Priority distribution**: P0: 6, P1: 11, P2: 5

## Test Philosophy

This story implements native Opik LLM instrumentation as an opt-in feature. The test strategy focuses on:
1. **Backward compatibility** - Default behavior must be unchanged
2. **Graceful degradation** - Missing SDK should not crash
3. **Multi-client support** - Both OpenAI and Azure OpenAI must work
4. **Streaming correctness** - Token aggregation must be accurate
5. **Coexistence** - Native tracing and OpikExporter must work together

## Test Scenarios by Acceptance Criteria

### AC1: `llm.call` supports optional `opik_trace=True` parameter for native Opik instrumentation

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-UNIT-001 | Unit | P0 | Default `opik_trace=False` behavior unchanged | Critical backward compatibility - existing code must work |
| 005.2-UNIT-002 | Unit | P0 | Explicit `opik_trace=False` behavior unchanged | Ensures parameter can be explicitly disabled |
| 005.2-UNIT-003 | Unit | P1 | `opik_trace=True` parameter accepted | Validates new parameter is recognized |

**Coverage Summary**: 3 scenarios (2 P0, 1 P1) - ensures backward compatibility while enabling new feature.

---

### AC2: When enabled, OpenAI client is wrapped with `track_openai()` from Opik SDK

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-UNIT-004 | Unit | P0 | `track_openai()` called when `opik_trace=True` | Core functionality - must verify wrapper applied |
| 005.2-UNIT-005 | Unit | P1 | `track_openai()` NOT called when `opik_trace=False` | Ensures no overhead when disabled |
| 005.2-UNIT-006 | Unit | P2 | Double-wrap prevention for same client ID | Prevents wrapper stacking issues |

**Coverage Summary**: 3 scenarios (1 P0, 1 P1, 1 P2) - validates wrapper logic and edge cases.

---

### AC3: Token usage (prompt_tokens, completion_tokens, total_tokens) automatically captured

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-UNIT-007 | Unit | P1 | Token usage fields present in result dict | Verifies Opik SDK captures usage data |
| 005.2-UNIT-008 | Unit | P1 | Token counts match expected values | Validates token counting accuracy |
| 005.2-INT-001 | Integration | P1 | Token usage recorded in Opik dashboard | End-to-end verification of telemetry |

**Coverage Summary**: 3 scenarios (all P1) - ensures token capture works from unit to integration level.

---

### AC4: Estimated cost calculated based on model pricing tables

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-UNIT-009 | Unit | P1 | Cost calculation for known models (GPT-4, GPT-3.5) | Validates pricing table lookup |
| 005.2-UNIT-010 | Unit | P1 | Fuzzy model name matching (e.g., "gpt-4-turbo-2024-04-09") | Ensures robust model detection |
| 005.2-UNIT-011 | Unit | P2 | Cost calculation for unknown models returns 0 | Graceful handling of new/unlisted models |

**Coverage Summary**: 3 scenarios (2 P1, 1 P2) - ensures cost calculation works for all cases.

---

### AC5: `llm.stream` streaming responses aggregated correctly for complete trace

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-UNIT-012 | Unit | P1 | `llm_stream()` accepts `opik_trace` parameter | Validates streaming API consistency |
| 005.2-UNIT-013 | Unit | P1 | Streaming chunks aggregated for token count | Ensures `stream_options={"include_usage": True}` works |
| 005.2-INT-002 | Integration | P1 | Streaming trace complete in Opik dashboard | End-to-end streaming verification |

**Coverage Summary**: 3 scenarios (all P1) - streaming is critical for real-world LLM usage.

---

### AC6: Azure OpenAI endpoint detected and traced identically to standard OpenAI

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-UNIT-014 | Unit | P1 | Azure client detected when env vars present | Validates auto-detection logic |
| 005.2-UNIT-015 | Unit | P2 | Azure client wrapped with `track_openai()` | Ensures Azure compatibility |
| 005.2-INT-003 | Integration | P1 | Azure endpoint traced to Opik dashboard | End-to-end Azure verification |

**Coverage Summary**: 3 scenarios (2 P1, 1 P2) - Azure support is critical for enterprise users.

---

### AC7: Feature is opt-in and disabled by default (no impact on existing behavior)

#### Scenarios

*Covered by AC1 scenarios (005.2-UNIT-001, 002)*

**Coverage Summary**: Implicit coverage through backward compatibility tests.

---

### AC8: Works alongside OpikExporter (Story 1) - can use both or either

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-INT-004 | Integration | P0 | Both OpikExporter and native tracing enabled | Critical coexistence test |
| 005.2-INT-005 | Integration | P1 | Only OpikExporter enabled (native tracing off) | Validates independent operation |
| 005.2-INT-006 | Integration | P2 | Only native tracing enabled (OpikExporter off) | Validates independent operation |

**Coverage Summary**: 3 scenarios (1 P0, 1 P1, 1 P2) - ensures flexible configuration.

---

### AC9: Integration tests verify end-to-end tracing to Opik dashboard

#### Scenarios

*Satisfied by integration tests: 005.2-INT-001 through 005.2-INT-006*

**Coverage Summary**: 6 integration tests provide comprehensive E2E verification.

---

### AC10: Documentation shows configuration options and expected dashboard output

#### Scenarios

*This is a documentation requirement - verified through manual review of CLAUDE.md and story dev notes.*

**Coverage Summary**: Manual verification required (not automated test).

---

## Graceful Degradation Tests

### Missing Opik SDK Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-UNIT-016 | Unit | P0 | Missing SDK logs RuntimeWarning | Critical error handling - must not crash |
| 005.2-UNIT-017 | Unit | P0 | LLM call proceeds without tracing when SDK missing | Ensures core functionality works |

**Coverage Summary**: 2 scenarios (both P0) - graceful degradation is critical for production reliability.

---

## Test Scenarios Summary Table

| Test ID | Level | Priority | Acceptance Criteria | Test Description |
|---------|-------|----------|---------------------|------------------|
| 005.2-UNIT-001 | Unit | P0 | AC1, AC7 | Default `opik_trace=False` unchanged |
| 005.2-UNIT-002 | Unit | P0 | AC1, AC7 | Explicit `opik_trace=False` unchanged |
| 005.2-UNIT-003 | Unit | P1 | AC1 | `opik_trace=True` parameter accepted |
| 005.2-UNIT-004 | Unit | P0 | AC2 | `track_openai()` called when enabled |
| 005.2-UNIT-005 | Unit | P1 | AC2 | `track_openai()` NOT called when disabled |
| 005.2-UNIT-006 | Unit | P2 | AC2 | Double-wrap prevention |
| 005.2-UNIT-007 | Unit | P1 | AC3 | Token usage fields in result |
| 005.2-UNIT-008 | Unit | P1 | AC3 | Token counts accuracy |
| 005.2-UNIT-009 | Unit | P1 | AC4 | Cost calculation for known models |
| 005.2-UNIT-010 | Unit | P1 | AC4 | Fuzzy model name matching |
| 005.2-UNIT-011 | Unit | P2 | AC4 | Unknown model returns 0 cost |
| 005.2-UNIT-012 | Unit | P1 | AC5 | `llm_stream()` accepts parameter |
| 005.2-UNIT-013 | Unit | P1 | AC5 | Streaming chunk aggregation |
| 005.2-UNIT-014 | Unit | P1 | AC6 | Azure client detection |
| 005.2-UNIT-015 | Unit | P2 | AC6 | Azure client wrapped |
| 005.2-UNIT-016 | Unit | P0 | Degradation | Missing SDK logs warning |
| 005.2-UNIT-017 | Unit | P0 | Degradation | LLM call proceeds without SDK |
| 005.2-INT-001 | Integration | P1 | AC3, AC9 | Token usage in Opik dashboard |
| 005.2-INT-002 | Integration | P1 | AC5, AC9 | Streaming trace in dashboard |
| 005.2-INT-003 | Integration | P1 | AC6, AC9 | Azure traced to dashboard |
| 005.2-INT-004 | Integration | P0 | AC8, AC9 | Both exporters enabled |
| 005.2-INT-005 | Integration | P1 | AC8 | Only OpikExporter enabled |
| 005.2-INT-006 | Integration | P2 | AC8 | Only native tracing enabled |

---

## Risk Coverage

### Risk: Backward compatibility broken (default behavior changed)

**Mitigations**:
- 005.2-UNIT-001 (P0): Validates default `opik_trace=False`
- 005.2-UNIT-002 (P0): Validates explicit `opik_trace=False`
- 005.2-UNIT-005 (P1): Confirms no wrapper when disabled

**Risk Level**: HIGH → Mitigated by 2 P0 tests

---

### Risk: Missing opik SDK crashes LLM calls in production

**Mitigations**:
- 005.2-UNIT-016 (P0): Validates RuntimeWarning logged
- 005.2-UNIT-017 (P0): Validates LLM call proceeds

**Risk Level**: HIGH → Mitigated by 2 P0 tests

---

### Risk: Streaming broken with tracing enabled

**Mitigations**:
- 005.2-UNIT-013 (P1): Unit test for chunk aggregation
- 005.2-INT-002 (P1): Integration test for dashboard

**Risk Level**: MEDIUM → Mitigated by 2 P1 tests

---

### Risk: Azure OpenAI not supported

**Mitigations**:
- 005.2-UNIT-014 (P1): Azure client detection
- 005.2-UNIT-015 (P2): Azure wrapper application
- 005.2-INT-003 (P1): Azure E2E verification

**Risk Level**: MEDIUM → Mitigated by 3 tests

---

### Risk: Conflict with OpikExporter creates duplicate traces

**Mitigations**:
- 005.2-INT-004 (P0): Both enabled simultaneously
- 005.2-INT-005 (P1): OpikExporter only
- 005.2-INT-006 (P2): Native tracing only

**Risk Level**: MEDIUM → Mitigated by 3 tests (1 P0)

---

## Recommended Execution Order

### Phase 1: Fast Fail (P0 Unit Tests)
1. 005.2-UNIT-001 - Default behavior unchanged
2. 005.2-UNIT-002 - Explicit disable unchanged
3. 005.2-UNIT-004 - Wrapper called when enabled
4. 005.2-UNIT-016 - Missing SDK warning
5. 005.2-UNIT-017 - LLM proceeds without SDK

**Rationale**: Critical backward compatibility and graceful degradation tests. If these fail, stop immediately.

---

### Phase 2: Core Functionality (P1 Unit Tests)
6. 005.2-UNIT-003 - Parameter accepted
7. 005.2-UNIT-005 - Wrapper not called when disabled
8. 005.2-UNIT-007 - Token usage fields
9. 005.2-UNIT-008 - Token counts accuracy
10. 005.2-UNIT-009 - Cost calculation known models
11. 005.2-UNIT-010 - Fuzzy model matching
12. 005.2-UNIT-012 - Stream accepts parameter
13. 005.2-UNIT-013 - Stream aggregation
14. 005.2-UNIT-014 - Azure detection

**Rationale**: Core feature validation. High confidence these will pass if P0 passed.

---

### Phase 3: Integration Validation (P0 + P1 Integration)
15. 005.2-INT-004 - Both exporters (P0)
16. 005.2-INT-001 - Token dashboard
17. 005.2-INT-002 - Streaming dashboard
18. 005.2-INT-003 - Azure dashboard
19. 005.2-INT-005 - OpikExporter only

**Rationale**: End-to-end verification with real Opik SDK interaction.

---

### Phase 4: Edge Cases (P2 Tests - as time permits)
20. 005.2-UNIT-006 - Double-wrap prevention
21. 005.2-UNIT-011 - Unknown model cost
22. 005.2-UNIT-015 - Azure wrapper
23. 005.2-INT-006 - Native tracing only

**Rationale**: Edge cases and secondary scenarios. Can defer if time-constrained.

---

## Test Implementation Guidelines

### Mocking Strategy

**Unit Tests**:
```python
import unittest
from unittest.mock import Mock, patch, MagicMock

class TestOpikLLMTracing(unittest.TestCase):
    @patch('the_edge_agent.actions.llm_actions.OpenAI')
    @patch('the_edge_agent.actions.llm_actions.track_openai')
    def test_opik_trace_wraps_client(self, mock_track_openai, mock_openai):
        # Mock OpenAI client
        mock_client = Mock()
        mock_openai.return_value = mock_client

        # Mock track_openai wrapper
        mock_wrapped_client = Mock()
        mock_track_openai.return_value = mock_wrapped_client

        # Call llm_call with opik_trace=True
        result = llm_call(
            state={},
            model="gpt-4",
            messages=[{"role": "user", "content": "test"}],
            opik_trace=True
        )

        # Verify track_openai was called
        mock_track_openai.assert_called_once_with(mock_client)
```

**Integration Tests**:
```python
class TestOpikIntegration(unittest.TestCase):
    @unittest.skipUnless(OPIK_SDK_AVAILABLE, "Requires opik SDK")
    def test_token_usage_in_dashboard(self):
        # Use real opik SDK but with test project
        # Verify traces appear in Opik dashboard
        pass
```

### Environment Setup

**Required Environment Variables** (for integration tests):
```bash
export OPIK_API_KEY="test_key_for_integration"
export OPIK_PROJECT_NAME="tea-test-005.2"
export OPENAI_API_KEY="test_openai_key"  # or use mock
```

**Azure Tests**:
```bash
export AZURE_OPENAI_API_KEY="test_azure_key"
export AZURE_OPENAI_ENDPOINT="https://test.openai.azure.com"
```

### Test Data

**Sample Token Usage**:
```python
MOCK_TOKEN_USAGE = {
    "prompt_tokens": 50,
    "completion_tokens": 100,
    "total_tokens": 150
}
```

**Sample Cost Calculation**:
```python
# GPT-4: $0.03/1K input, $0.06/1K output
# 50 prompt + 100 completion = $0.0015 + $0.006 = $0.0075
EXPECTED_COST_GPT4 = 0.0075
```

---

## Coverage Gaps

**None identified.** All 10 acceptance criteria have test coverage.

**Manual verification required**:
- AC10: Documentation review (CLAUDE.md and story dev notes)

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for E2E)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (`{epic}.{story}-{LEVEL}-{SEQ}`)
- [x] Scenarios are atomic and independent
- [x] Backward compatibility is P0 priority
- [x] Graceful degradation is tested
- [x] Streaming scenarios are comprehensive

---

## Key Test Principles Applied

1. **Shift left**: 73% unit tests (16/22) for fast feedback
2. **Risk-based**: 6 P0 tests focus on backward compatibility and graceful degradation
3. **Efficient coverage**: Integration tests only for E2E verification (no redundant unit→integration duplication)
4. **Maintainability**: Unit tests use mocking for fast, reliable execution
5. **Fast feedback**: P0 tests run first to fail fast on critical issues

---

## Test Maintenance Notes

### When to Update Tests

- **New OpenAI models added**: Update `MODEL_PRICING` and add cost calculation test
- **Opik SDK changes**: Review wrapper behavior and token capture
- **Azure API version changes**: Update Azure integration test
- **New LLM providers added**: Extend test coverage for new providers

### Test Dependencies

- **opik SDK**: Integration tests require `pip install opik`
- **openai SDK**: All tests require `pip install openai`
- **Mock libraries**: `unittest.mock` (standard library)

---

## Estimated Test Execution Time

| Phase | Test Count | Estimated Time | Notes |
|-------|------------|----------------|-------|
| Phase 1 (P0 Unit) | 5 | 5 seconds | Fast, no external deps |
| Phase 2 (P1 Unit) | 9 | 9 seconds | Fast, mocked |
| Phase 3 (P0+P1 Int) | 5 | 30 seconds | Real Opik SDK interaction |
| Phase 4 (P2 All) | 4 | 10 seconds | Mixed unit/integration |
| **Total** | **22** | **~1 minute** | Parallel execution possible |

---

## Success Criteria

**Test suite passes when**:
1. All 6 P0 tests pass (critical gate)
2. All 11 P1 tests pass (core functionality)
3. At least 3 of 5 P2 tests pass (edge cases - best effort)
4. No regression in existing `llm.call`/`llm.stream` behavior
5. Integration tests verify traces in Opik dashboard

**Ready for production when**:
- Test suite passes
- Code review complete
- Documentation verified (AC10)
- No open P0 or P1 issues

---

## Notes for Test Implementation

1. **Use `@unittest.skipUnless` for optional dependencies**:
   ```python
   OPIK_SDK_AVAILABLE = importlib.util.find_spec("opik") is not None

   @unittest.skipUnless(OPIK_SDK_AVAILABLE, "Requires opik SDK")
   def test_opik_integration(self):
       pass
   ```

2. **Mock environment variables for Azure tests**:
   ```python
   @patch.dict(os.environ, {
       "AZURE_OPENAI_API_KEY": "test_key",
       "AZURE_OPENAI_ENDPOINT": "https://test.openai.azure.com"
   })
   def test_azure_detection(self):
       pass
   ```

3. **Use `warnings.catch_warnings()` for graceful degradation tests**:
   ```python
   with warnings.catch_warnings(record=True) as w:
       warnings.simplefilter("always")
       result = llm_call(state, model, messages, opik_trace=True)
       assert len(w) == 1
       assert "opik SDK not installed" in str(w[0].message)
   ```

4. **Test double-wrap prevention**:
   ```python
   # Call twice with same client ID
   result1 = llm_call(..., opik_trace=True)
   result2 = llm_call(..., opik_trace=True)
   # Verify client ID only in _opik_wrapped_clients once
   ```

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 22
  by_level:
    unit: 16
    integration: 6
    e2e: 0
  by_priority:
    p0: 6
    p1: 11
    p2: 5
  coverage_gaps: []
  manual_verification:
    - AC10: Documentation review (CLAUDE.md)
  risk_mitigations:
    backward_compatibility: 3 tests (2 P0, 1 P1)
    missing_sdk: 2 tests (2 P0)
    streaming: 2 tests (2 P1)
    azure_support: 3 tests (2 P1, 1 P2)
    opik_exporter_conflict: 3 tests (1 P0, 1 P1, 1 P2)
```

---

## Trace References for Requirements Traceability

**For use by `trace-requirements` task**:

- Test design matrix: `docs/qa/assessments/TEA-BUILTIN-005.2-test-design-20260107.md`
- P0 tests identified: 6
- P1 tests identified: 11
- P2 tests identified: 5
- Total acceptance criteria covered: 10/10
- Integration tests for E2E verification: 6
- Backward compatibility tests: 3

---

**END OF TEST DESIGN**
