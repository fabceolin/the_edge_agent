# Test Design: Story TEA-AGENT-001.4

**Date:** 2026-01-05
**Designer:** Quinn (Test Architect)
**Story:** Reasoning Techniques Primitives

## Test Strategy Overview

| Metric | Count |
|--------|-------|
| **Total test scenarios** | 72 |
| **Unit tests** | 42 (58%) |
| **Integration tests** | 22 (31%) |
| **E2E tests** | 8 (11%) |

**Priority Distribution:**

| Priority | Count | Description |
|----------|-------|-------------|
| P0 | 30 | Critical reasoning actions, infinite loop/recursion prevention, tool integration |
| P1 | 24 | Self-correction, decomposition, trace format, multi-model support |
| P2 | 12 | DSPy integration, documentation examples, edge cases |
| P3 | 6 | Rare configurations, DSPy compilation, legacy compatibility |

---

## Test Scenarios by Acceptance Criteria

---

### AC1: `reason.cot` Action (Chain-of-Thought)

**Requirements:**
1. Wraps LLM call with CoT prompting
2. Outputs structured `{thinking: str, answer: any}`
3. Supports few-shot examples
4. Configurable thinking format (step-by-step, pros-cons, etc.)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-001 | Unit | P0 | CoT prompt construction with step-by-step format | Core prompt engineering logic |
| 001.4-UNIT-002 | Unit | P0 | CoT prompt construction with pros-cons format | Core prompt engineering logic |
| 001.4-UNIT-003 | Unit | P0 | CoT prompt construction with tree format | Core prompt engineering logic |
| 001.4-UNIT-004 | Unit | P0 | Structured output parsing - extracts thinking field | Output parsing is critical for downstream use |
| 001.4-UNIT-005 | Unit | P0 | Structured output parsing - extracts answer field | Output parsing is critical for downstream use |
| 001.4-UNIT-006 | Unit | P0 | Structured output parsing - handles malformed response | Error handling for LLM unpredictability |
| 001.4-UNIT-007 | Unit | P1 | Few-shot example formatting - single example | Template construction logic |
| 001.4-UNIT-008 | Unit | P1 | Few-shot example formatting - multiple examples | Template construction logic |
| 001.4-INT-001 | Integration | P0 | CoT action with mock LLM returns structured output | End-to-end action flow with LLM integration |
| 001.4-INT-002 | Integration | P1 | CoT action sets state variables correctly | State management integration |
| 001.4-E2E-001 | E2E | P1 | YAML agent uses reason.cot to solve math problem | Validates full YAML-to-execution path |

**Mitigates Risks:** RISK-001 (malformed response)

---

### AC2: `reason.react` Action

**Requirements:**
1. Implements Thought→Action→Observation loop
2. Integrates with `llm.tools` for action execution
3. Maximum steps configurable
4. Returns full trace and final answer
5. Early termination when goal achieved

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-009 | Unit | P0 | ReAct loop parses thought from LLM response | Core parsing logic |
| 001.4-UNIT-010 | Unit | P0 | ReAct loop parses action name from LLM response | Core parsing logic |
| 001.4-UNIT-011 | Unit | P0 | ReAct loop parses action_input from LLM response | Core parsing logic |
| 001.4-UNIT-012 | Unit | P0 | ReAct loop terminates after max_steps | RISK-002: Prevent infinite loops (critical) |
| 001.4-UNIT-013 | Unit | P0 | ReAct loop detects final answer and terminates early | Core control flow logic |
| 001.4-UNIT-014 | Unit | P1 | ReAct accumulates steps in trace | Observability requirement |
| 001.4-UNIT-015 | Unit | P1 | ReAct handles LLM returning invalid action format | Robustness |
| 001.4-INT-003 | Integration | P0 | ReAct integrates with llm.tools for action execution | Critical integration point |
| 001.4-INT-004 | Integration | P0 | ReAct calls web.search tool with correct arguments | Tool integration validation |
| 001.4-INT-005 | Integration | P0 | ReAct receives observation from tool and continues loop | Core loop behavior |
| 001.4-INT-006 | Integration | P0 | ReAct completes full thought-action-observation cycle | Full loop validation |
| 001.4-INT-007 | Integration | P1 | ReAct with multiple tools selects correct tool | Multi-tool scenarios |
| 001.4-INT-008 | Integration | P1 | ReAct handles tool execution failure gracefully | RISK-004: Error handling at boundary |
| 001.4-E2E-002 | E2E | P0 | YAML agent uses reason.react to research topic | Critical user journey |
| 001.4-E2E-003 | E2E | P1 | ReAct agent terminates early on goal achieved | User expects efficiency |

**Mitigates Risks:** RISK-002 (infinite loop), RISK-004 (tool execution failure)

---

### AC3: `reason.self_correct` Action

**Requirements:**
1. Generates output, then critiques and improves
2. Configurable number of improvement rounds
3. Returns final improved output with improvement trace
4. Can use different models for generation vs. critique

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-016 | Unit | P1 | Self-correct generates initial output | Core generation logic |
| 001.4-UNIT-017 | Unit | P1 | Self-correct generates critique of output | Core critique logic |
| 001.4-UNIT-018 | Unit | P1 | Self-correct applies improvement based on critique | Core improvement logic |
| 001.4-UNIT-019 | Unit | P1 | Self-correct respects improvement_rounds config | Configuration handling |
| 001.4-UNIT-020 | Unit | P2 | Self-correct with 0 rounds returns initial output | Edge case |
| 001.4-UNIT-021 | Unit | P1 | Self-correct accumulates improvement_history | Observability |
| 001.4-INT-009 | Integration | P1 | Self-correct with same model for generate/critique | Common use case |
| 001.4-INT-010 | Integration | P1 | Self-correct with different models for generate/critique | Multi-model capability |
| 001.4-INT-011 | Integration | P1 | Self-correct sets state variables correctly | State management |
| 001.4-E2E-004 | E2E | P1 | YAML agent uses self_correct for code generation | Validates real-world usage |

---

### AC4: `reason.decompose` Action

**Requirements:**
1. Breaks complex problem into sub-problems
2. Solves sub-problems individually
3. Synthesizes final answer from sub-answers
4. Supports recursive decomposition

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-022 | Unit | P1 | Decompose breaks problem into sub-problems | Core decomposition logic |
| 001.4-UNIT-023 | Unit | P1 | Decompose solves each sub-problem | Core solving logic |
| 001.4-UNIT-024 | Unit | P1 | Decompose synthesizes final answer from sub-answers | Core synthesis logic |
| 001.4-UNIT-025 | Unit | P0 | Decompose respects max_depth for recursion | RISK-003: Prevent infinite recursion (critical) |
| 001.4-UNIT-026 | Unit | P2 | Decompose with max_depth=1 prevents recursion | Edge case boundary |
| 001.4-UNIT-027 | Unit | P2 | Decompose handles single sub-problem correctly | Edge case |
| 001.4-INT-012 | Integration | P1 | Decompose with recursive depth=2 | Multi-level decomposition |
| 001.4-INT-013 | Integration | P1 | Decompose applies synthesis_prompt correctly | Template integration |
| 001.4-E2E-005 | E2E | P2 | YAML agent uses decompose for complex problem | Validates user workflow |

**Mitigates Risks:** RISK-003 (infinite recursion)

---

### AC5: Structured Output

**Requirements:**
1. All reasoning actions return structured output
2. Full reasoning trace preserved in state
3. Trace can be logged for debugging/observability
4. Trace format compatible with Opik integration

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-028 | Unit | P0 | CoT returns dict with thinking and answer keys | Output contract |
| 001.4-UNIT-029 | Unit | P0 | ReAct returns dict with steps and final_answer keys | Output contract |
| 001.4-UNIT-030 | Unit | P1 | Self-correct returns dict with improvement_history | Output contract |
| 001.4-UNIT-031 | Unit | P1 | Decompose returns dict with sub_answers and final | Output contract |
| 001.4-UNIT-032 | Unit | P1 | Trace format matches documented JSON schema | API contract validation |
| 001.4-INT-014 | Integration | P0 | Trace is preserved in state.reasoning_trace | RISK-005: State persistence critical |
| 001.4-INT-015 | Integration | P1 | Trace format is compatible with Opik | Observability integration |
| 001.4-INT-016 | Integration | P2 | Trace can be serialized to JSON | Logging requirement |

**Mitigates Risks:** RISK-005 (state corruption)

---

### AC6: Tool Integration

**Requirements:**
1. `reason.react` uses existing `llm.tools` infrastructure
2. Tools from MCP/CrewAI/LangChain bridges available
3. Custom tools can be defined inline

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-INT-017 | Integration | P0 | ReAct uses llm.tools infrastructure | Core integration |
| 001.4-INT-018 | Integration | P0 | ReAct can call MCP-provided tools | Bridge integration |
| 001.4-INT-019 | Integration | P1 | ReAct can call CrewAI tools via bridge | Bridge integration |
| 001.4-INT-020 | Integration | P1 | ReAct can call LangChain tools via bridge | Bridge integration |
| 001.4-UNIT-033 | Unit | P1 | Custom inline tool definition parsing | YAML feature |
| 001.4-E2E-006 | E2E | P0 | YAML agent with multiple tool sources | Real-world multi-tool scenario |

---

### AC7: Python Implementation

**Requirements:**
1. New module: `python/src/the_edge_agent/actions/reasoning_actions.py`
2. All actions registered in `build_actions_registry()`
3. Test coverage >90%

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-034 | Unit | P0 | reason.cot registered in action registry | Action discovery |
| 001.4-UNIT-035 | Unit | P0 | reason.react registered in action registry | Action discovery |
| 001.4-UNIT-036 | Unit | P0 | reason.self_correct registered in action registry | Action discovery |
| 001.4-UNIT-037 | Unit | P0 | reason.decompose registered in action registry | Action discovery |
| 001.4-E2E-007 | E2E | P1 | Python CLI executes reasoning YAML agent | Full Python path |

---

### AC8: Rust Implementation

**Requirements:**
1. New module: `rust/src/engine/actions/reasoning_actions.rs`
2. Feature parity with Python

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-038 | Unit | P0 | Rust reason.cot matches Python behavior | RISK-006: Parity requirement |
| 001.4-UNIT-039 | Unit | P0 | Rust reason.react matches Python behavior | RISK-006: Parity requirement |
| 001.4-UNIT-040 | Unit | P0 | Rust reason.self_correct matches Python behavior | RISK-006: Parity requirement |
| 001.4-UNIT-041 | Unit | P0 | Rust reason.decompose matches Python behavior | RISK-006: Parity requirement |
| 001.4-E2E-008 | E2E | P0 | Rust CLI executes same YAML agent as Python | Cross-runtime parity validation |

**Mitigates Risks:** RISK-006 (Python/Rust parity drift)

---

### AC9: DSPy Backend Option (Optional)

**Requirements:**
1. `reason.dspy.cot` wraps DSPy ChainOfThought module
2. `reason.dspy.react` wraps DSPy ReAct module with tool bridge
3. Model-agnostic compiled prompts via DSPy
4. Graceful fallback to native `reason.*` when DSPy unavailable
5. Requires `dspy` optional dependency

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-042 | Unit | P2 | reason.dspy.cot wraps DSPy ChainOfThought module | DSPy integration |
| 001.4-UNIT-043 | Unit | P2 | reason.dspy.react wraps DSPy ReAct module | DSPy integration |
| 001.4-UNIT-044 | Unit | P2 | reason.dspy.cot tool bridge integration | Tool compatibility |
| 001.4-UNIT-045 | Unit | P2 | Graceful fallback when DSPy import fails | Resilience for optional dep |
| 001.4-UNIT-046 | Unit | P2 | Model-agnostic prompt construction | DSPy core feature |
| 001.4-INT-021 | Integration | P2 | reason.dspy.* with mocked DSPy module | External dependency mock |

---

### AC10: DSPy Compilation Support

**Requirements:**
1. `reason.dspy.compile` compiles DSPy module with teleprompter
2. Compiled prompts persist in checkpoint/state for reuse
3. Supports custom teleprompters (BootstrapFewShot, etc.)
4. Validation set support for optimization

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-047 | Unit | P3 | reason.dspy.compile with BootstrapFewShot teleprompter | DSPy compilation |
| 001.4-UNIT-048 | Unit | P3 | reason.dspy.compile with custom teleprompter | Extensibility |
| 001.4-UNIT-049 | Unit | P3 | Compiled prompt persistence to checkpoint | State management |
| 001.4-UNIT-050 | Unit | P3 | Compiled prompt reload from checkpoint | State restoration |
| 001.4-INT-022 | Integration | P3 | reason.dspy.compile with validation set | Optimization flow |

---

## Risk Coverage

| Risk ID | Risk Description | Severity | Mitigating Tests |
|---------|------------------|----------|------------------|
| RISK-001 | LLM returns malformed response | High | 001.4-UNIT-006, 001.4-UNIT-015 |
| RISK-002 | Infinite loop in ReAct | Critical | 001.4-UNIT-012 |
| RISK-003 | Infinite recursion in decompose | Critical | 001.4-UNIT-025 |
| RISK-004 | Tool execution failure | Medium | 001.4-INT-008 |
| RISK-005 | State corruption from parallel execution | Medium | 001.4-INT-014 |
| RISK-006 | Python/Rust parity drift | High | 001.4-UNIT-038 to 041, 001.4-E2E-008 |

---

## Recommended Execution Order

1. **Phase 1 - P0 Unit Tests (Fail Fast)**
   - Registry tests (001.4-UNIT-034 to 041) - Verify all actions discoverable
   - Infinite loop/recursion prevention (001.4-UNIT-012, 001.4-UNIT-025) - Critical safety
   - Output parsing (001.4-UNIT-004 to 006) - Core functionality
   - ReAct core logic (001.4-UNIT-009 to 013) - Main action
   - Prompt construction (001.4-UNIT-001 to 003) - CoT variations

2. **Phase 2 - P0 Integration Tests**
   - Tool integration (001.4-INT-017, 001.4-INT-018)
   - ReAct loop (001.4-INT-003 to 006)
   - State preservation (001.4-INT-014)
   - LLM integration (001.4-INT-001)

3. **Phase 3 - P0 E2E Tests**
   - ReAct research agent (001.4-E2E-002)
   - Multi-tool scenario (001.4-E2E-006)
   - Cross-runtime parity (001.4-E2E-008)

4. **Phase 4 - P1 Tests (Core Functionality)**
   - CoT variations (001.4-UNIT-007, 008)
   - Self-correct cycle (001.4-UNIT-016 to 021, 001.4-INT-009 to 011)
   - Decompose logic (001.4-UNIT-022 to 024, 001.4-INT-012, 013)
   - Trace format (001.4-UNIT-032, 001.4-INT-015)
   - Structured output contracts (001.4-UNIT-028 to 031)

5. **Phase 5 - P2 Tests (As Time Permits)**
   - Edge cases (001.4-UNIT-020, 026, 027)
   - DSPy integration (001.4-UNIT-042 to 046, 001.4-INT-021)
   - E2E decompose (001.4-E2E-005)
   - JSON serialization (001.4-INT-016)

6. **Phase 6 - P3 Tests (Full Regression Only)**
   - DSPy compilation (001.4-UNIT-047 to 050, 001.4-INT-022)
   - Legacy compatibility tests
   - Rare configuration combinations

---

## Test Implementation Guidance

### Mocking Strategy

```yaml
llm_mocking:
  approach: Mock at llm.call level
  rationale: Isolate reasoning logic from actual LLM calls
  tools:
    python: unittest.mock.patch or pytest-mock
    rust: mockall crate
  fixtures:
    - valid_cot_response.json
    - malformed_cot_response.json
    - react_thought_action.json
    - react_final_answer.json

tool_mocking:
  approach: Mock tool execution responses
  rationale: Test ReAct loop without external dependencies
  fixtures:
    - web_search_results.json
    - tool_failure_response.json

dspy_mocking:
  approach: Mock DSPy module entirely when unavailable
  rationale: Test graceful fallback behavior
  techniques:
    - sys.modules manipulation for import mocking
    - Mock DSPy ChainOfThought/ReAct classes
```

### Test Data Fixtures

```yaml
fixtures_needed:
  cot_responses:
    - description: Valid thinking/answer structure
    - description: Malformed (missing answer field)
    - description: Malformed (invalid JSON)
  react_traces:
    - description: Complete 3-step trace with final answer
    - description: Max steps reached without final answer
    - description: Early termination on goal achieved
  tool_responses:
    - description: Successful web.search result
    - description: Tool execution failure
    - description: Tool timeout
  self_correct_traces:
    - description: 2-round improvement history
    - description: Initial output unchanged after critique
  decompose_traces:
    - description: 3 sub-problems with synthesis
    - description: Recursive depth=2 decomposition
```

### Performance Considerations

```yaml
performance_tests:
  - id: 001.4-PERF-001
    description: ReAct loop completes 10 steps in <5s (mocked LLM)
    priority: P2
  - id: 001.4-PERF-002
    description: Decompose with depth=3 completes in <10s (mocked LLM)
    priority: P2
  - id: 001.4-PERF-003
    description: DSPy compilation caches correctly (no re-compile on same input)
    priority: P3
```

---

## Quality Checklist

- [x] Every AC has test coverage (10 ACs, 72 scenarios)
- [x] Test levels are appropriate (58% unit, shift-left applied)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (P0 on critical/infinite loop paths)
- [x] Test IDs follow naming convention `{epic}.{story}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] All 6 identified risks have direct test coverage
- [x] Python/Rust parity explicitly tested (5 tests)
- [x] DSPy optional feature has graceful fallback tests

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 72
  by_level:
    unit: 42
    integration: 22
    e2e: 8
  by_priority:
    p0: 30
    p1: 24
    p2: 12
    p3: 6
  coverage_gaps: []
  coverage_notes:
    - All 10 ACs have comprehensive test scenarios
    - Critical ReAct/CoT paths have defense-in-depth (unit + integration + e2e)
    - Python/Rust parity explicitly validated via 5 cross-runtime tests
    - DSPy optional feature (AC9, AC10) covered with P2/P3 tests and fallback validation
    - Infinite loop (RISK-002) and infinite recursion (RISK-003) prevention are P0 unit tests
  risk_coverage:
    - risk_id: RISK-001
      test_ids: ["001.4-UNIT-006", "001.4-UNIT-015"]
      status: covered
    - risk_id: RISK-002
      test_ids: ["001.4-UNIT-012"]
      status: covered
    - risk_id: RISK-003
      test_ids: ["001.4-UNIT-025"]
      status: covered
    - risk_id: RISK-004
      test_ids: ["001.4-INT-008"]
      status: covered
    - risk_id: RISK-005
      test_ids: ["001.4-INT-014"]
      status: covered
    - risk_id: RISK-006
      test_ids: ["001.4-UNIT-038", "001.4-UNIT-039", "001.4-UNIT-040", "001.4-UNIT-041", "001.4-E2E-008"]
      status: covered
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-AGENT-001.4-test-design-20260105.md
P0 tests identified: 30
P1 tests identified: 24
P2 tests identified: 12
P3 tests identified: 6
Total scenarios: 72
Story file: docs/stories/TEA-AGENT-001.4-reasoning-techniques.md
```
