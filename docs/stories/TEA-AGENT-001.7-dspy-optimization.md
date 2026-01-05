# Story TEA-AGENT-001.7: DSPy Prompt Optimization

## Status

**Draft**

## Story

**As a** YAML agent developer,
**I want** DSPy integration for declarative prompt optimization,
**so that** I can build agents with compiled, model-agnostic prompts that automatically improve through optimization.

## Background

The Edge Agent currently uses static Jinja2 templates for prompts, which requires manual engineering. DSPy (Declarative Self-improving Python) offers:

1. Declarative prompt programming via Signatures
2. Automatic prompt compilation and optimization
3. Model-agnostic prompt portability
4. Teleprompter-based few-shot example generation
5. Composable modules (ChainOfThought, ReAct, etc.)

This story integrates DSPy as an optional reasoning backend, complementing the native `reason.*` actions from TEA-AGENT-001.4.

## Acceptance Criteria

### AC1: `reason.dspy.cot` Action
1. Wraps DSPy ChainOfThought module
2. Configurable signature (input -> output)
3. Returns structured output with reasoning trace
4. Supports few-shot examples from state
5. Graceful fallback to native `reason.cot` when DSPy unavailable

### AC2: `reason.dspy.react` Action
1. Wraps DSPy ReAct module
2. Integrates with TEA tool bridges (MCP, CrewAI, LangChain)
3. Configurable max_steps
4. Returns full action-observation trace
5. Graceful fallback to native `reason.react`

### AC3: `reason.dspy.compile` Action
1. Compiles DSPy module with teleprompter
2. Supports teleprompter types: BootstrapFewShot, BootstrapFewShotWithRandomSearch
3. Requires trainset (examples) from state
4. Returns compiled module configuration
5. Persists compiled prompts for reuse

### AC4: `reason.dspy.optimize` Action
1. Runs optimization against validation set
2. Configurable metric function
3. Returns optimization results and best configuration
4. Updates compiled prompts in state/checkpoint

### AC5: Compiled Prompt Persistence
1. Compiled prompts persist across checkpoint saves
2. Load compiled prompts on graph resume
3. Version tracking for compiled prompts
4. Export/import compiled configurations

### AC6: Settings Configuration
1. Configure via `settings.dspy.enabled: true`
2. Support DSPy LM configuration (model, API key)
3. Default teleprompter configuration
4. Graceful fallback when DSPy unavailable

### AC7: Python Implementation
1. New module: `python/src/the_edge_agent/actions/dspy_actions.py`
2. All actions registered in `build_actions_registry()`
3. Test coverage >90%
4. Requires `dspy` optional dependency

## Tasks / Subtasks

- [ ] **Task 1: DSPy Client Wrapper** (AC: 6)
  - [ ] Create `DSPyClient` wrapper class
  - [ ] LM configuration from settings
  - [ ] API key management
  - [ ] Fallback detection
  - [ ] Unit tests

- [ ] **Task 2: `reason.dspy.cot` Action** (AC: 1)
  - [ ] Implement ChainOfThought wrapper
  - [ ] Signature parsing and validation
  - [ ] Few-shot example injection
  - [ ] Structured output formatting
  - [ ] Unit tests

- [ ] **Task 3: `reason.dspy.react` Action** (AC: 2)
  - [ ] Implement ReAct wrapper
  - [ ] Tool bridge integration
  - [ ] Action-observation trace
  - [ ] Max steps handling
  - [ ] Unit and integration tests

- [ ] **Task 4: `reason.dspy.compile` Action** (AC: 3, 5)
  - [ ] Implement compile action
  - [ ] Teleprompter configuration
  - [ ] Trainset handling
  - [ ] Compiled prompt persistence
  - [ ] Unit tests

- [ ] **Task 5: `reason.dspy.optimize` Action** (AC: 4, 5)
  - [ ] Implement optimize action
  - [ ] Metric function support
  - [ ] Validation set handling
  - [ ] Results tracking
  - [ ] Unit tests

- [ ] **Task 6: Checkpoint Integration** (AC: 5)
  - [ ] Compiled prompt serialization
  - [ ] Load on resume
  - [ ] Version tracking
  - [ ] Export/import support
  - [ ] Integration tests

- [ ] **Task 7: Documentation & Examples**
  - [ ] Update YAML_REFERENCE.md
  - [ ] Create example: dspy-cot-reasoning.yaml
  - [ ] Create example: dspy-react-research.yaml
  - [ ] Create example: dspy-compile-optimize.yaml

## Dev Notes

### Source Tree Context

**Python:**
```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py           # Add dspy_actions
│   ├── reasoning_actions.py  # Reference: native reason.*
│   ├── dspy_actions.py       # NEW: DSPy actions
│   └── ...
└── reasoning/
    └── dspy_client.py        # NEW: DSPy client wrapper
```

### YAML Syntax Reference

#### Chain of Thought with DSPy
```yaml
settings:
  dspy:
    enabled: true
    model: gpt-4

nodes:
  - name: solve_problem
    action: reason.dspy.cot
    with:
      signature: "question -> thinking, answer"
      question: "{{ state.problem }}"
```

#### ReAct with DSPy
```yaml
nodes:
  - name: research_task
    action: reason.dspy.react
    with:
      signature: "goal -> result"
      goal: "{{ state.research_goal }}"
      tools:
        - web.search
        - web.scrape
      max_steps: 5
```

#### Compile and Optimize
```yaml
nodes:
  - name: compile_reasoning
    action: reason.dspy.compile
    with:
      module: cot
      signature: "question -> answer"
      teleprompter: BootstrapFewShot
      trainset: "{{ state.examples }}"

  - name: optimize_module
    action: reason.dspy.optimize
    with:
      module: "{{ state.compiled_module }}"
      valset: "{{ state.validation_examples }}"
      metric: exact_match
```

### Dependencies

```
pip install the_edge_agent[dspy]
# or
pip install dspy-ai>=2.0.0
```

### Integration with Native Reasoning

When DSPy is unavailable, `reason.dspy.*` actions fallback to native `reason.*` actions with a warning log. This ensures agents work across environments with different dependencies.

## Constraints

- DSPy is an optional dependency
- Compilation requires training examples
- Optimization can be computationally expensive
- Model API key required for DSPy LM

## References

- [DSPy GitHub](https://github.com/stanfordnlp/dspy)
- [DSPy Documentation](https://dspy-docs.vercel.app/)
- [Agentic Design Patterns - Chapter 9: Learning & Adaptation](https://github.com/sarwarbeing-ai/Agentic_Design_Patterns)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-05 | 0.1 | Initial story from Sprint Change Proposal | Sarah (PO) |
