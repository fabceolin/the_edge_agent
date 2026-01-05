# Story TEA-AGENT-001.9: TextGrad Learning (P2)

## Status

**Draft (P2 - Future)**

_Note: This story is P2 priority and may be deferred to a future epic based on team capacity._

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

- [ ] **Task 1: TextGrad Client Wrapper** (AC: 5)
  - [ ] Create `TextGradClient` wrapper class
  - [ ] Configuration from settings
  - [ ] Optimizer model setup
  - [ ] Cost warning implementation
  - [ ] Unit tests

- [ ] **Task 2: `learn.textgrad.variable` Action** (AC: 3)
  - [ ] Implement variable definition
  - [ ] Version tracking
  - [ ] Constraint support
  - [ ] State persistence
  - [ ] Unit tests

- [ ] **Task 3: `learn.textgrad.feedback` Action** (AC: 2)
  - [ ] Implement gradient computation
  - [ ] Multi-aspect evaluation
  - [ ] Structured feedback format
  - [ ] Unit tests

- [ ] **Task 4: `learn.textgrad.optimize_prompt` Action** (AC: 1)
  - [ ] Implement optimization loop
  - [ ] Loss function integration
  - [ ] Iteration control
  - [ ] Improvement trace
  - [ ] Unit tests

- [ ] **Task 5: Reflection Loop Integration** (AC: 4)
  - [ ] Integrate with reflection.loop
  - [ ] Automatic optimization triggers
  - [ ] History-based gradient computation
  - [ ] Integration tests

- [ ] **Task 6: Documentation & Examples**
  - [ ] Update YAML_REFERENCE.md
  - [ ] Create example: textgrad-prompt-optimization.yaml
  - [ ] Create example: textgrad-reflection-learning.yaml

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

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-05 | 0.1 | Initial story from Sprint Change Proposal (P2) | Sarah (PO) |
