# Test Design: Story TEA-AGENT-001.9

**Story:** TextGrad Learning (P2)
**Date:** 2026-01-07
**Designer:** Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 28
- **Unit tests:** 18 (64%)
- **Integration tests:** 8 (29%)
- **E2E tests:** 2 (7%)
- **Priority distribution:** P0: 8, P1: 12, P2: 8

### Risk Profile

TextGrad introduces complex ML-based prompt optimization with high computational cost and potential for non-deterministic behavior. Key risks:

1. **Computational Cost Risk** - Multiple LLM calls per iteration could lead to unexpected bills
2. **Convergence Risk** - Poorly defined loss functions may not converge
3. **Dependency Risk** - Optional TextGrad library may have version conflicts
4. **Integration Risk** - Complex interaction with reflection loop

### Coverage Strategy

- **Shift left:** Focus on unit testing for core logic (variable tracking, feedback computation)
- **Integration focus:** Validate TextGrad client integration and optimizer behavior
- **E2E selective:** Only test critical user journeys (basic optimization, reflection integration)
- **Avoid duplication:** Test convergence logic at unit level, not E2E

---

## Test Scenarios by Acceptance Criteria

### AC1: `learn.textgrad.optimize_prompt` Action

**Priority:** P0 (Core optimization logic - critical functionality)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.9-UNIT-001 | Unit | P0 | Optimize prompt with valid loss function | Pure optimization logic with mocked TextGrad |
| 001.9-UNIT-002 | Unit | P0 | Handle optimization with max iterations | Iteration control is pure logic |
| 001.9-UNIT-003 | Unit | P1 | Return improvement trace with versions | State tracking logic |
| 001.9-UNIT-004 | Unit | P1 | Apply prompt structure constraints | Constraint validation logic |
| 001.9-UNIT-005 | Unit | P1 | Handle optimization convergence early | Convergence detection logic |
| 001.9-INT-001 | Integration | P0 | Optimize prompt via TextGrad library | Critical integration with external library |
| 001.9-INT-002 | Integration | P1 | Handle TextGrad API errors gracefully | Error boundary testing |
| 001.9-E2E-001 | E2E | P1 | Complete prompt optimization workflow in YAML | Critical user journey validation |

**Coverage Notes:**
- Unit tests focus on iteration control, constraint validation, trace generation
- Integration tests validate actual TextGrad library interaction
- E2E confirms end-to-end YAML workflow

---

### AC2: `learn.textgrad.feedback` Action

**Priority:** P0 (Gradient computation - critical for optimization)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.9-UNIT-006 | Unit | P0 | Compute textual gradients from output | Core gradient computation logic |
| 001.9-UNIT-007 | Unit | P0 | Evaluate against multiple criteria | Multi-aspect evaluation logic |
| 001.9-UNIT-008 | Unit | P1 | Return structured feedback format | Output format validation |
| 001.9-UNIT-009 | Unit | P1 | Handle invalid evaluation criteria | Input validation logic |
| 001.9-INT-003 | Integration | P0 | Generate gradients via LLM evaluator | Critical LLM integration |
| 001.9-INT-004 | Integration | P2 | Handle LLM evaluation failures | Error handling for external dependency |

**Coverage Notes:**
- Unit tests validate feedback structure and criteria handling
- Integration tests confirm LLM-based evaluation works correctly
- Gradient computation accuracy is tested via mocked LLM responses

---

### AC3: `learn.textgrad.variable` Action

**Priority:** P1 (State management - important but less critical than optimization)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.9-UNIT-010 | Unit | P1 | Define prompt as optimizable variable | Variable definition logic |
| 001.9-UNIT-011 | Unit | P1 | Track prompt versions and changes | Version tracking logic |
| 001.9-UNIT-012 | Unit | P1 | Apply variable constraints | Constraint validation |
| 001.9-UNIT-013 | Unit | P2 | Persist variable to state | State integration logic |
| 001.9-INT-005 | Integration | P1 | Load variable from persisted state | State round-trip validation |

**Coverage Notes:**
- Focus on version tracking correctness (pure logic)
- State persistence tested at integration level (requires StateGraph context)

---

### AC4: Integration with Reflection Loop

**Priority:** P0 (Critical integration - combines two complex systems)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.9-UNIT-014 | Unit | P0 | Trigger optimization on repeated failures | Trigger logic is pure function |
| 001.9-UNIT-015 | Unit | P1 | Preserve reflection history for gradients | History management logic |
| 001.9-INT-006 | Integration | P0 | TextGrad within reflection.loop workflow | Critical cross-feature integration |
| 001.9-INT-007 | Integration | P1 | Automatic optimization after N failures | Trigger behavior in full context |
| 001.9-E2E-002 | E2E | P0 | Self-improving agent via reflection + TextGrad | Mission-critical user journey |

**Coverage Notes:**
- Unit tests validate trigger and history logic in isolation
- Integration tests confirm proper interaction between reflection and TextGrad
- E2E validates complete self-improvement workflow

---

### AC5: Settings Configuration

**Priority:** P0 (Configuration errors could cause silent failures or cost overruns)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.9-UNIT-016 | Unit | P0 | Enable TextGrad via settings.textgrad.enabled | Configuration parsing logic |
| 001.9-UNIT-017 | Unit | P0 | Configure optimizer model | Model selection logic |
| 001.9-UNIT-018 | Unit | P1 | Configure learning rate (iterations) | Parameter validation |
| 001.9-UNIT-019 | Unit | P0 | Require explicit opt-in (default disabled) | Security/cost protection |
| 001.9-INT-008 | Integration | P0 | Gracefully fallback when TextGrad unavailable | Dependency resilience |

**Coverage Notes:**
- Explicit opt-in is P0 (protects against accidental cost)
- Fallback behavior validated at integration level (requires missing dependency simulation)

---

### AC6: Python Implementation

**Priority:** P1 (Code quality - important but covered by other tests)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.9-UNIT-020 | Unit | P1 | All actions registered in build_actions_registry() | Registration logic |
| 001.9-UNIT-021 | Unit | P2 | TextGrad actions raise ActionError correctly | Error handling consistency |
| 001.9-UNIT-022 | Unit | P2 | Optional dependency import handling | Import guards work |

**Coverage Notes:**
- Registration and error handling tested at unit level
- Coverage >90% enforced via pytest-cov

---

## Risk Coverage Matrix

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| **RISK-001: Cost overrun** | 001.9-UNIT-019, 001.9-INT-001 | Explicit opt-in, cost warnings |
| **RISK-002: Non-convergence** | 001.9-UNIT-005, 001.9-INT-001 | Max iterations, early stopping |
| **RISK-003: Dependency failure** | 001.9-INT-008, 001.9-UNIT-022 | Graceful fallback, import guards |
| **RISK-004: Reflection integration** | 001.9-INT-006, 001.9-E2E-002 | Integration + E2E validation |

---

## Test Implementation Guidelines

### Unit Test Setup

```python
# tests/test_textgrad_actions.py
import pytest
from unittest.mock import Mock, patch
from the_edge_agent.actions.textgrad_actions import (
    optimize_prompt,
    compute_feedback,
    define_variable,
)

@pytest.fixture
def mock_textgrad():
    with patch("the_edge_agent.learning.textgrad_client.TextGradClient") as mock:
        yield mock

def test_optimize_prompt_with_valid_loss_function(mock_textgrad):
    """001.9-UNIT-001: Optimize prompt with valid loss function"""
    # Setup
    state = {"prompt_var": "Initial prompt"}
    params = {
        "variable": "prompt_var",
        "loss_fn": "Quality score for: {{ state.output }}",
        "iterations": 3
    }

    # Mock TextGrad to return improved prompt
    mock_textgrad.optimize.return_value = "Improved prompt"

    # Execute
    result = optimize_prompt(state, params)

    # Verify
    assert result["optimized_prompt"] == "Improved prompt"
    assert len(result["improvement_trace"]) == 3
```

### Integration Test Setup

```python
# tests/integration/test_textgrad_integration.py
import pytest
from the_edge_agent import StateGraph
from the_edge_agent.learning.textgrad_client import TextGradClient

@pytest.mark.integration
@pytest.mark.skipif(not has_textgrad(), reason="textgrad not installed")
def test_optimize_prompt_via_textgrad_library():
    """001.9-INT-001: Optimize prompt via TextGrad library"""
    # Setup real TextGrad client
    client = TextGradClient(model="gpt-3.5-turbo")

    # Execute optimization
    result = client.optimize(
        prompt="You are a helpful assistant",
        loss_fn="Rate helpfulness 1-10",
        iterations=2
    )

    # Verify improvement occurred
    assert result.final_prompt != "You are a helpful assistant"
    assert result.loss_decreased
```

### E2E Test Setup

```python
# tests/e2e/test_textgrad_workflow.py
import pytest
from the_edge_agent import YAMLEngine

@pytest.mark.e2e
@pytest.mark.slow
def test_complete_prompt_optimization_workflow():
    """001.9-E2E-001: Complete prompt optimization workflow in YAML"""
    yaml_config = """
    settings:
      textgrad:
        enabled: true
        optimizer_model: gpt-3.5-turbo

    nodes:
      - name: optimize
        action: learn.textgrad.optimize_prompt
        with:
          variable: system_prompt
          loss_fn: "Rate quality 1-10: {{ state.output }}"
          iterations: 2
    """

    engine = YAMLEngine(yaml_config)
    result = engine.run({"system_prompt": "Initial", "output": "Test output"})

    assert "optimized_prompt" in result
    assert result["optimization_completed"]
```

---

## Recommended Execution Order

### Phase 1: Fast Unit Tests (P0)
1. `001.9-UNIT-001` through `001.9-UNIT-019` (Configuration + core logic)
2. Target: <5 seconds total execution

### Phase 2: Integration Tests (P0)
1. `001.9-INT-001` (TextGrad library integration)
2. `001.9-INT-003` (LLM-based gradient generation)
3. `001.9-INT-006` (Reflection loop integration)
4. `001.9-INT-008` (Dependency fallback)
5. Target: <30 seconds total execution

### Phase 3: E2E Tests (P0-P1)
1. `001.9-E2E-002` (Self-improving agent - critical)
2. `001.9-E2E-001` (Basic optimization workflow)
3. Target: <2 minutes total execution

### Phase 4: Secondary Tests (P1-P2)
1. Remaining integration tests
2. P2 unit tests
3. Execute only in full CI runs

---

## Quality Checklist

✅ **Coverage Verification:**
- [x] Every AC has test coverage
- [x] Test levels are appropriate (64% unit, 29% integration, 7% E2E)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk

✅ **Test Design Quality:**
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Clear justification for each test level
- [x] Risk mitigation mapped to tests

✅ **Implementation Readiness:**
- [x] Test setup examples provided
- [x] Mocking strategy defined
- [x] Execution order specified
- [x] Coverage target: >90% (per AC6)

---

## Special Considerations

### Cost Protection Testing

TextGrad optimization is computationally expensive. Tests must verify:

1. ✅ Explicit opt-in required (`001.9-UNIT-019`)
2. ✅ Warning displayed before first optimization
3. ✅ Iteration limits enforced (`001.9-UNIT-002`)
4. ✅ Early stopping on convergence (`001.9-UNIT-005`)

### Non-Determinism Handling

LLM-based optimization is non-deterministic. Test strategies:

- **Unit tests:** Mock LLM responses for deterministic behavior
- **Integration tests:** Assert on improvement patterns, not exact prompts
- **E2E tests:** Validate workflow completion, not specific outputs

### Optional Dependency Testing

Must verify behavior when `textgrad` is not installed:

- ✅ Import guards prevent crashes (`001.9-UNIT-022`)
- ✅ Graceful error messages (`001.9-INT-008`)
- ✅ No silent failures

---

## Test Maintenance Notes

### When to Update Tests

- **Add new tests:** When bugs are discovered in production
- **Update tests:** When TextGrad library API changes
- **Remove tests:** Never remove tests without replacement
- **Refactor tests:** When test execution time exceeds 5 minutes

### Known Limitations

1. **LLM cost:** Integration/E2E tests may incur API costs (use test API keys)
2. **Test duration:** TextGrad optimization is slow (consider mocking for CI)
3. **Flakiness risk:** LLM responses vary (use retry logic for E2E tests)

---

## Appendix: Test Scenario Summary

### By Level
- **Unit:** 18 scenarios (fast, isolated, deterministic)
- **Integration:** 8 scenarios (medium speed, external dependencies)
- **E2E:** 2 scenarios (slow, full workflow validation)

### By Priority
- **P0:** 8 scenarios (must pass before merge)
- **P1:** 12 scenarios (should pass in CI)
- **P2:** 8 scenarios (nice-to-have, may defer)

### Coverage Gaps
- None identified - all ACs have test coverage

---

**Next Steps:**
1. Implement unit tests in `tests/test_textgrad_actions.py`
2. Implement integration tests in `tests/integration/test_textgrad_integration.py`
3. Implement E2E tests in `tests/e2e/test_textgrad_workflow.py`
4. Configure pytest-cov to enforce >90% coverage
5. Update CI pipeline to run tests in recommended execution order
