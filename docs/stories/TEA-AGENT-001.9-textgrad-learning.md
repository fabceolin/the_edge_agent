# Story TEA-AGENT-001.9: TextGrad Learning (P2)

## Status

**Ready for Review**

_Note: This story is P2 priority and may be deferred to a future epic based on team capacity._

**Implementation Date:** 2026-01-07
**Implemented By:** James (Dev)
**Validation Date:** 2026-01-07
**Validated By:** Bob (Scrum Master)
**Validation Result:** READY - All checklist criteria passed with comprehensive QA test design

### Implementation Summary
- All 6 tasks completed
- 37 tests (31 passed, 6 skipped for optional TextGrad dependency)
- Documentation created at `docs/shared/yaml-reference/actions/learning.md`
- Examples: `examples/learning/textgrad-prompt-optimization.yaml`, `examples/learning/textgrad-reflection-learning.yaml`

## Story

**As a** YAML agent developer,
**I want** TextGrad integration for gradient-based prompt optimization,
**so that** I can build agents that automatically improve their prompts based on feedback, implementing true learning and adaptation.

## Background

The Edge Agent currently has no mechanism for automatic prompt improvement. TextGrad applies the concept of backpropagation to text, treating:

1. Prompts as "variables" that can be optimized
2. LLM critiques as "textual gradients"
3. Prompt updates as "gradient descent steps"

This enables agents to learn from failures and systematically improve their instructions over time, implementing the Learning & Adaptation pattern from Agentic Design Patterns.

## Acceptance Criteria

### AC1: `learn.textgrad.optimize_prompt` Action
1. Optimizes a prompt variable based on feedback
2. Configurable loss function (LLM-evaluated quality)
3. Iterative improvement with configurable iterations
4. Returns optimized prompt and improvement trace
5. Supports constraints on prompt structure

### AC2: `learn.textgrad.feedback` Action
1. Computes textual gradients from output evaluation
2. Configurable evaluation criteria
3. Returns structured feedback with improvement suggestions
4. Supports multi-aspect evaluation (accuracy, clarity, safety)

### AC3: `learn.textgrad.variable` Action
1. Defines a prompt as an optimizable variable
2. Tracks prompt versions and changes
3. Supports variable constraints
4. Integrates with state for persistence

### AC4: Integration with Reflection Loop
1. TextGrad can be used within `reflection.loop`
2. Automatic prompt optimization on repeated failures
3. Configurable optimization triggers
4. Preserves reflection history for gradient computation

### AC5: Settings Configuration
1. Configure via `settings.textgrad.enabled: true`
2. Optimizer model configuration
3. Learning rate (iteration count) configuration
4. Explicit opt-in required (computational cost warning)
5. Graceful fallback when TextGrad unavailable

### AC6: Python Implementation
1. New module: `python/src/the_edge_agent/actions/textgrad_actions.py`
2. All actions registered in `build_actions_registry()`
3. Test coverage >90%
4. Requires `textgrad` optional dependency

## Tasks / Subtasks

- [x] **Task 1: TextGrad Client Wrapper** (AC: 5)
  - [x] Create `TextGradClient` wrapper class
  - [x] Configuration from settings
  - [x] Optimizer model setup
  - [x] Cost warning implementation
  - [x] Unit tests

- [x] **Task 2: `learn.textgrad.variable` Action** (AC: 3)
  - [x] Implement variable definition
  - [x] Version tracking
  - [x] Constraint support
  - [x] State persistence
  - [x] Unit tests

- [x] **Task 3: `learn.textgrad.feedback` Action** (AC: 2)
  - [x] Implement gradient computation
  - [x] Multi-aspect evaluation
  - [x] Structured feedback format
  - [x] Unit tests

- [x] **Task 4: `learn.textgrad.optimize_prompt` Action** (AC: 1)
  - [x] Implement optimization loop
  - [x] Loss function integration
  - [x] Iteration control
  - [x] Improvement trace
  - [x] Unit tests

- [x] **Task 5: Reflection Loop Integration** (AC: 4)
  - [x] Integrate with reflection.loop
  - [x] Automatic optimization triggers
  - [x] History-based gradient computation
  - [x] Integration tests

- [x] **Task 6: Documentation & Examples**
  - [x] Update YAML_REFERENCE.md (created learning.md in docs/shared/yaml-reference/actions/)
  - [x] Create example: textgrad-prompt-optimization.yaml
  - [x] Create example: textgrad-reflection-learning.yaml

## Dev Notes

### Source Tree Context

**Python:**
```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py              # Add textgrad_actions
│   ├── reflection_actions.py    # Reference: reflection loop
│   ├── textgrad_actions.py      # NEW: TextGrad actions
│   └── ...
└── learning/
    └── textgrad_client.py       # NEW: TextGrad client wrapper
```

### YAML Syntax Reference

#### Basic Prompt Optimization
```yaml
settings:
  textgrad:
    enabled: true
    optimizer_model: gpt-4

nodes:
  - name: define_prompt
    action: learn.textgrad.variable
    with:
      name: system_prompt
      initial_value: "You are a helpful assistant..."

  - name: optimize_prompt
    action: learn.textgrad.optimize_prompt
    with:
      variable: system_prompt
      loss_fn: "Evaluate if the response is accurate and helpful: {{ state.response }}"
      iterations: 3
```

#### Integration with Reflection Loop
```yaml
nodes:
  - name: generate_with_learning
    action: reflection.loop
    with:
      generator:
        action: llm.call
        prompt: "{{ state.optimized_prompt }}"
      evaluator:
        type: llm
        prompt: "Evaluate quality..."
      corrector:
        action: learn.textgrad.optimize_prompt
        with:
          variable: optimized_prompt
          loss_fn: "{{ state.reflection_errors }}"
      max_iterations: 3
```

### Dependencies

```
pip install the_edge_agent[textgrad]
# or
pip install textgrad>=0.1.0
```

### Computational Cost Warning

TextGrad optimization requires multiple LLM calls per iteration:
- Forward pass (generate output)
- Loss evaluation (critique output)
- Gradient computation (generate improvement suggestions)
- Update step (apply suggestions)

For a 3-iteration optimization, this can result in 12+ LLM calls. Users must explicitly enable TextGrad and should be aware of cost implications.

## Constraints

- TextGrad is an optional dependency
- Requires explicit opt-in due to computational cost
- Optimization quality depends on optimizer model capability
- May not converge for poorly defined loss functions
- P2 priority - may be deferred based on capacity

## References

- [TextGrad GitHub](https://github.com/zou-group/textgrad)
- [TextGrad Paper](https://arxiv.org/abs/2406.07496) (Nature 2024)
- [TextGrad Website](http://textgrad.com/)
- [Agentic Design Patterns - Chapter 9: Learning & Adaptation](https://github.com/sarwarbeing-ai/Agentic_Design_Patterns)

## QA Notes

**Test Design Completed:** 2026-01-07
**Designer:** Quinn (Test Architect)

### Test Coverage Summary

- **Total test scenarios:** 28 (18 unit, 8 integration, 2 E2E)
- **Priority distribution:** 8 P0, 12 P1, 8 P2
- **Coverage strategy:** Shift-left focus with 64% unit tests for core logic
- **Target coverage:** >90% (per AC6 requirement)

### Risk Areas Identified

| Risk ID | Risk Description | Probability | Impact | Mitigation |
|---------|-----------------|-------------|--------|------------|
| RISK-001 | Cost overrun from multiple LLM calls per iteration | Medium | High | Explicit opt-in required, cost warnings, iteration limits |
| RISK-002 | Non-convergence with poorly defined loss functions | Medium | Medium | Max iterations enforced, early stopping logic |
| RISK-003 | Optional dependency failures (TextGrad not installed) | Low | Medium | Graceful fallback, import guards, clear error messages |
| RISK-004 | Complex reflection loop integration failures | Medium | High | Dedicated integration + E2E validation |

### Recommended Test Scenarios

**Phase 1 - Fast Unit Tests (P0):** Run first in CI pipeline
- Configuration validation (opt-in, model selection, iteration limits)
- Core optimization logic (convergence, constraints, trace generation)
- Gradient computation and feedback structure
- Variable tracking and version management
- **Target:** <5 seconds execution time

**Phase 2 - Integration Tests (P0):** Validate external dependencies
- TextGrad library integration (001.9-INT-001)
- LLM-based gradient generation (001.9-INT-003)
- Reflection loop integration (001.9-INT-006)
- Dependency fallback behavior (001.9-INT-008)
- **Target:** <30 seconds execution time

**Phase 3 - E2E Tests (P0-P1):** Critical user journeys
- Self-improving agent via reflection + TextGrad (001.9-E2E-002) - **CRITICAL**
- Complete YAML optimization workflow (001.9-E2E-001)
- **Target:** <2 minutes execution time

### Quality Concerns & Blockers

**Cost Protection (CRITICAL):**
- TextGrad optimization requires 12+ LLM calls per 3-iteration optimization
- Must verify explicit opt-in (`settings.textgrad.enabled: true`)
- Must display cost warning before first optimization
- Tests: 001.9-UNIT-019 (opt-in), 001.9-UNIT-002 (iteration limits), 001.9-UNIT-005 (early stopping)

**Non-Determinism Handling:**
- LLM-based optimization produces variable results
- Unit tests must mock LLM responses for deterministic behavior
- Integration/E2E tests should assert on improvement patterns, not exact prompts
- Consider retry logic for E2E tests to handle flakiness

**Optional Dependency:**
- Must gracefully handle missing `textgrad` library
- Import guards required (001.9-UNIT-022)
- Clear error messages when TextGrad unavailable (001.9-INT-008)
- No silent failures allowed

**Reflection Loop Integration:**
- Complex interaction between two advanced features
- Requires dedicated integration test (001.9-INT-006)
- Must validate history preservation for gradient computation
- E2E test (001.9-E2E-002) is mission-critical before merge

### Implementation Readiness

✅ **Ready to implement:**
- Test design complete with 28 scenarios across 3 levels
- Clear test setup examples provided for unit/integration/E2E
- Mocking strategy defined (mock TextGrad for unit, real for integration)
- Execution order specified (fast unit → integration → slow E2E)
- Risk mitigation mapped to specific test IDs

⚠️ **Considerations:**
- Integration/E2E tests will incur LLM API costs (use test API keys)
- TextGrad optimization is slow (consider mocking for CI speed)
- P2 priority story - may defer based on team capacity
- Coverage >90% enforced via pytest-cov

### Test Artifacts

- **Test Design Document:** `docs/qa/assessments/TEA-AGENT-001.9-test-design-20260107.md`
- **Test Files (to be created):**
  - `python/tests/test_textgrad_actions.py` (unit tests)
  - `python/tests/integration/test_textgrad_integration.py` (integration tests)
  - `python/tests/e2e/test_textgrad_workflow.py` (E2E tests)

### Next QA Steps

1. Implement P0 unit tests (8 scenarios) during development
2. Implement P0 integration tests (4 scenarios) after TextGrad client wrapper complete
3. Implement E2E-002 (self-improving agent) before merge - **BLOCKING**
4. Configure pytest-cov to enforce >90% coverage
5. Update CI pipeline to run tests in recommended execution order
6. Review test results and update risk assessment if needed

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-07 | 0.2 | Added QA Notes with test coverage and risk analysis | Quinn (QA) |
| 2026-01-05 | 0.1 | Initial story from Sprint Change Proposal (P2) | Sarah (PO) |
