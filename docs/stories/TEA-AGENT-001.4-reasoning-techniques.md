# Story TEA-AGENT-001.4: Reasoning Techniques Primitives

## Status

**Ready for Development**

## Story

**As a** YAML agent developer,
**I want** built-in reasoning technique primitives,
**so that** I can implement Chain-of-Thought, ReAct, and self-correction patterns without manual prompt engineering.

## Background

Reasoning techniques (Chapter 17 of Agentic Design Patterns) improve LLM output quality through structured thinking. Currently, users must:

1. Craft complex prompts with reasoning instructions
2. Parse structured outputs (thought, action, observation)
3. Implement ReAct loops with tool integration
4. Handle self-correction without framework support

This story introduces `reason.*` actions that provide battle-tested implementations of these patterns.

## Acceptance Criteria

### AC1: `reason.cot` Action (Chain-of-Thought)
1. Wraps LLM call with CoT prompting
2. Outputs structured `{thinking: str, answer: any}`
3. Supports few-shot examples
4. Configurable thinking format (step-by-step, pros-cons, etc.)

### AC2: `reason.react` Action
1. Implements Thought→Action→Observation loop
2. Integrates with `llm.tools` for action execution
3. Maximum steps configurable
4. Returns full trace and final answer
5. Early termination when goal achieved

### AC3: `reason.self_correct` Action
1. Generates output, then critiques and improves
2. Configurable number of improvement rounds
3. Returns final improved output with improvement trace
4. Can use different models for generation vs. critique

### AC4: `reason.decompose` Action
1. Breaks complex problem into sub-problems
2. Solves sub-problems individually
3. Synthesizes final answer from sub-answers
4. Supports recursive decomposition

### AC5: Structured Output
1. All reasoning actions return structured output
2. Full reasoning trace preserved in state
3. Trace can be logged for debugging/observability
4. Trace format compatible with Opik integration

### AC6: Tool Integration
1. `reason.react` uses existing `llm.tools` infrastructure
2. Tools from MCP/CrewAI/LangChain bridges available
3. Custom tools can be defined inline

### AC7: Python Implementation
1. New module: `python/src/the_edge_agent/actions/reasoning_actions.py`
2. All actions registered in `build_actions_registry()`
3. Test coverage >90%

### AC8: Rust Implementation
1. New module: `rust/src/engine/actions/reasoning_actions.rs`
2. Feature parity with Python

## Tasks / Subtasks

- [ ] **Task 1: `reason.cot` Action** (AC: 1, 5)
  - [ ] Implement CoT prompt wrapper
  - [ ] Structured output parsing
  - [ ] Few-shot example support
  - [ ] Thinking format options
  - [ ] Unit tests

- [ ] **Task 2: `reason.react` Action** (AC: 2, 5, 6)
  - [ ] Implement ReAct loop
  - [ ] Integrate with `llm.tools`
  - [ ] Maximum steps handling
  - [ ] Early termination logic
  - [ ] Trace accumulation
  - [ ] Unit and integration tests

- [ ] **Task 3: `reason.self_correct` Action** (AC: 3, 5)
  - [ ] Implement generate-critique-improve cycle
  - [ ] Configurable rounds
  - [ ] Multi-model support
  - [ ] Improvement trace
  - [ ] Unit tests

- [ ] **Task 4: `reason.decompose` Action** (AC: 4, 5)
  - [ ] Implement problem decomposition
  - [ ] Sub-problem solving
  - [ ] Answer synthesis
  - [ ] Recursive support
  - [ ] Unit tests

- [ ] **Task 5: Observability Integration** (AC: 5)
  - [ ] Trace format definition
  - [ ] Opik compatibility
  - [ ] Debug logging
  - [ ] Unit tests

- [ ] **Task 6: Rust Implementation** (AC: 7, 8)
  - [ ] Create `reasoning_actions.rs` module
  - [ ] Implement `reason.cot`
  - [ ] Implement `reason.react`
  - [ ] Implement `reason.self_correct`
  - [ ] Implement `reason.decompose`
  - [ ] Unit and integration tests

- [ ] **Task 7: Documentation & Examples**
  - [ ] Update YAML_REFERENCE.md
  - [ ] Create example: cot-problem-solving.yaml
  - [ ] Create example: react-research-agent.yaml
  - [ ] Create example: self-correcting-code-gen.yaml

## Dev Notes

### Source Tree Context

**Python:**
```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py           # Add reasoning_actions
│   ├── llm_actions.py        # Reference: llm.call, llm.tools
│   ├── reasoning_actions.py  # NEW: Reasoning actions
│   └── ...
└── tracing.py                # Reference: observability
```

**Rust:**
```
rust/src/engine/
├── actions/
│   ├── mod.rs
│   ├── llm_actions.rs        # Reference: llm actions
│   └── reasoning_actions.rs  # NEW: Reasoning actions
```

### YAML Syntax Reference

#### Chain-of-Thought
```yaml
nodes:
  - name: solve_math
    action: reason.cot
    with:
      problem: "{{ state.math_problem }}"
      model: gpt-4
      thinking_format: step_by_step  # step_by_step | pros_cons | tree
      few_shot_examples:
        - problem: "What is 15% of 80?"
          thinking: |
            Step 1: Convert 15% to decimal: 15/100 = 0.15
            Step 2: Multiply: 0.15 * 80 = 12
          answer: "12"
```

#### ReAct
```yaml
nodes:
  - name: research_topic
    action: reason.react
    with:
      goal: "{{ state.research_question }}"
      model: gpt-4
      tools:
        - web.search
        - web.scrape
        - memory.store
      max_steps: 10
      output_format:
        thought: str
        action: str
        action_input: any
        observation: str
        final_answer: str
```

#### Self-Correction
```yaml
nodes:
  - name: generate_code
    action: reason.self_correct
    with:
      task: "{{ state.coding_task }}"
      generator_model: gpt-4
      critic_model: gpt-4  # Can be same or different
      improvement_rounds: 2
      critic_prompt: |
        Review this code for:
        1. Correctness
        2. Edge cases
        3. Performance

        Code: {{ output }}
```

#### Decomposition
```yaml
nodes:
  - name: solve_complex
    action: reason.decompose
    with:
      problem: "{{ state.complex_problem }}"
      model: gpt-4
      max_depth: 2  # Recursive decomposition depth
      synthesis_prompt: |
        Combine these sub-answers into a final answer:
        {{ sub_answers | tojson }}
```

### State Variables Set by Reasoning Actions

| Variable | Type | Description |
|----------|------|-------------|
| `reasoning_trace` | list | Full reasoning trace (all steps) |
| `reasoning_thinking` | str | Chain-of-thought (for CoT) |
| `reasoning_answer` | any | Final answer |
| `react_steps` | list | ReAct thought-action-observation steps |
| `improvement_history` | list | Self-correction iterations |

### ReAct Trace Format

```json
{
  "steps": [
    {
      "step": 1,
      "thought": "I need to search for information about...",
      "action": "web.search",
      "action_input": {"query": "..."},
      "observation": "Found 5 results..."
    },
    {
      "step": 2,
      "thought": "The first result looks relevant...",
      "action": "web.scrape",
      "action_input": {"url": "..."},
      "observation": "Page content: ..."
    }
  ],
  "final_answer": "Based on my research..."
}
```

### Related Stories
- TEA-BUILTIN-001.2: LLM Enhanced Actions (llm.tools foundation)
- TEA-BUILTIN-002.3: Tools Bridge Actions (tool integration)
- TEA-BUILTIN-005: Opik Integration (observability)

## Testing

### Test File Locations
- Python: `python/tests/test_reasoning_actions.py`
- Rust: `rust/tests/test_reasoning_actions.rs`

### Test Categories

| Category | Count | Priority |
|----------|-------|----------|
| reason.cot | 8 | P0 |
| reason.react | 10 | P0 |
| reason.self_correct | 6 | P1 |
| reason.decompose | 6 | P1 |
| Tool Integration | 6 | P0 |
| Trace Format | 4 | P1 |

### Key Test Scenarios

1. **CoT output parsing** - Structured output correctly extracted
2. **ReAct tool execution** - Tools called with correct arguments
3. **ReAct max steps** - Terminates after max_steps
4. **ReAct early termination** - Stops when goal achieved
5. **Self-correct improvement** - Output improves with each round
6. **Decompose synthesis** - Sub-answers combined correctly
7. **Trace compatibility** - Trace format works with Opik

## QA Notes

**Reviewed:** 2026-01-05
**Reviewer:** Quinn (Test Architect)
**Test Design:** [TEA-AGENT-001.4-test-design-20260105.md](../qa/assessments/TEA-AGENT-001.4-test-design-20260105.md)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 62 |
| Unit tests | 34 (55%) |
| Integration tests | 20 (32%) |
| E2E tests | 8 (13%) |

**Priority Distribution:**
- **P0 (Critical):** 28 tests - Reasoning actions core, tool integration, registry
- **P1 (High):** 22 tests - Self-correction, decomposition, trace format
- **P2 (Medium):** 8 tests - Edge cases, documentation examples
- **P3 (Low):** 4 tests - Rare configurations

### Risk Areas Identified

| Risk ID | Risk | Severity | Mitigation |
|---------|------|----------|------------|
| RISK-001 | LLM returns malformed response | High | Unit tests for parsing robustness |
| RISK-002 | Infinite loop in ReAct | Critical | max_steps enforcement tested |
| RISK-003 | Infinite recursion in decompose | Critical | max_depth limit tested |
| RISK-004 | Tool execution failure | Medium | Graceful error handling tests |
| RISK-005 | State corruption from parallel execution | Medium | State preservation integration tests |
| RISK-006 | Python/Rust parity drift | High | Cross-runtime E2E parity test |

### Recommended Test Scenarios

**Must-Have (P0):**
1. Registry tests - All 4 reason.* actions discoverable
2. Output parsing - Structured output extraction from LLM responses
3. ReAct loop - max_steps termination, early exit on goal achieved
4. Tool integration - llm.tools infrastructure, MCP bridge
5. Cross-runtime parity - Same YAML executes identically in Python/Rust

**High Priority (P1):**
1. Self-correct cycle - Generate → Critique → Improve flow
2. Decompose recursion - max_depth enforcement
3. Trace format - Opik compatibility validation
4. Multi-model support - Different models for generator vs critic

### Concerns / Blockers

| Concern | Impact | Recommendation |
|---------|--------|----------------|
| LLM unpredictability | Test reliability | Use mocked LLM responses for deterministic testing |
| Tool bridge dependencies | Integration complexity | Mock at bridge boundary for unit tests |
| Trace schema evolution | Observability coupling | Document trace schema contract, version it |

### Gate Status

**READY FOR DEVELOPMENT** - Test design complete with 62 scenarios covering all 8 ACs. No blocking issues identified. Recommend implementing P0 tests first (28 scenarios) to establish core functionality validation.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-04 | 0.1 | Initial story draft | Sarah (PO) |
| 2026-01-05 | 0.2 | Added QA Notes section | Quinn (QA) |
