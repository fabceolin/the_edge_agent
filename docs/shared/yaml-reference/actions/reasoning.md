# Reasoning Actions Reference

> **Parent document:** [Actions Index](./README.md)
> **Related:** [LLM Actions](./llm.md) | [Node Specification](../nodes.md)
> **Story:** [TEA-AGENT-001.4](../../../stories/TEA-AGENT-001.4-reasoning-techniques.md)

## Overview

The reasoning actions provide battle-tested implementations of common AI reasoning patterns:

- **Chain-of-Thought (CoT)**: Structured step-by-step reasoning
- **ReAct**: Thought-Action-Observation loop with tool integration
- **Self-Correction**: Generate-critique-improve cycles
- **Decomposition**: Break complex problems into sub-problems

All actions return structured output with full reasoning traces suitable for debugging and observability (Opik-compatible).

---

## Quick Reference

| Action | Description | Use Case |
|--------|-------------|----------|
| `reason.cot` | Chain-of-Thought prompting | Math, logic, analysis problems |
| `reason.react` | ReAct reasoning loop | Research, multi-step tool use |
| `reason.self_correct` | Generate-critique-improve | Code generation, writing tasks |
| `reason.decompose` | Problem decomposition | Complex multi-part problems |
| `reason.dspy.cot` | DSPy ChainOfThought wrapper | Model-agnostic compiled prompts |
| `reason.dspy.react` | DSPy ReAct wrapper | Optimized tool use |
| `reason.dspy.compile` | Compile DSPy module | Prompt optimization |

---

## `reason.cot`

Chain-of-Thought reasoning that produces structured thinking and answers.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `problem` | string | Yes | - | The problem or question to reason about |
| `model` | string | No | gpt-4 | LLM model to use |
| `thinking_format` | string | No | step_by_step | Format: `step_by_step`, `pros_cons`, `tree`, `first_principles` |
| `few_shot_examples` | list | No | - | Examples with `problem`, `thinking`, `answer` fields |
| `temperature` | float | No | 0.7 | LLM temperature |

### Returns

```yaml
thinking: "Step 1: ... Step 2: ... Step 3: ..."
answer: "The final answer"
reasoning_trace: [...]  # Full trace for debugging
reasoning_thinking: "..."  # Alias for thinking
reasoning_answer: "..."  # Alias for answer
model: "gpt-4"
thinking_format: "step_by_step"
```

### Example

```yaml
nodes:
  - name: solve_math
    uses: reason.cot
    with:
      problem: "{{ state.math_problem }}"
      model: gpt-4
      thinking_format: step_by_step
      few_shot_examples:
        - problem: "What is 15% of 80?"
          thinking: |
            Step 1: Convert 15% to decimal: 15/100 = 0.15
            Step 2: Multiply: 0.15 * 80 = 12
          answer: "12"
    output: solution
```

### Thinking Formats

| Format | Description |
|--------|-------------|
| `step_by_step` | Linear sequential reasoning (default) |
| `pros_cons` | Weighing positive and negative aspects |
| `tree` | Branch-and-combine reasoning |
| `first_principles` | Reason from fundamental axioms |

---

## `reason.react`

ReAct (Reason-Act) loop implementing Thought → Action → Observation cycles.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `goal` | string | Yes | - | The goal or question to achieve/answer |
| `model` | string | No | gpt-4 | LLM model to use |
| `tools` | list | No | [] | List of tool/action names to make available |
| `max_steps` | int | No | 10 | Maximum reasoning steps (safety limit) |
| `temperature` | float | No | 0.7 | LLM temperature |

### Returns

```yaml
steps:
  - step: 1
    thought: "I need to search for..."
    action: "web.search"
    action_input: {query: "..."}
    observation: "Found 5 results..."
  - step: 2
    thought: "The first result looks relevant..."
    action: "final_answer"
    action_input: {answer: "..."}
final_answer: "Based on my research..."
reasoning_trace: [...]
react_steps: [...]  # Alias for steps
total_steps: 2
model: "gpt-4"
```

### Example

```yaml
nodes:
  - name: research_topic
    uses: reason.react
    with:
      goal: "{{ state.research_question }}"
      model: gpt-4
      tools:
        - web.search
        - web.scrape
        - memory.store
      max_steps: 10
    output: research_result
```

### Tool Integration

ReAct integrates with the existing `llm.tools` infrastructure:

```yaml
tools:
  - web.search      # Built-in actions
  - tools.mcp       # MCP server tools
  - tools.langchain # LangChain tools
  - custom.mytool   # Custom registered actions
```

### Safety

The `max_steps` parameter prevents infinite loops. When reached, the action returns the best answer available with the steps taken.

---

## `reason.self_correct`

Generate-critique-improve cycle for iterative output refinement.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `task` | string | Yes | - | The task description |
| `model` | string | No | gpt-4 | Default LLM model |
| `generator_model` | string | No | (model) | Model for generation |
| `critic_model` | string | No | (model) | Model for critique |
| `improvement_rounds` | int | No | 2 | Number of improvement iterations |
| `critic_prompt` | string | No | - | Custom prompt for the critic |
| `temperature` | float | No | 0.7 | LLM temperature |

### Returns

```yaml
output: "The final improved output"
improvement_history:
  - round: 1
    output: "Initial output..."
    critique: {issues: [...], suggestions: [...]}
    improved_output: "Better output..."
  - round: 2
    output: "Better output..."
    critique: {issues: [...], suggestions: [...]}
    improved_output: "Best output..."
reasoning_trace: [...]
rounds_completed: 2
generator_model: "gpt-4"
critic_model: "gpt-4"
```

### Example

```yaml
nodes:
  - name: generate_code
    uses: reason.self_correct
    with:
      task: "{{ state.coding_task }}"
      generator_model: gpt-4
      critic_model: gpt-4
      improvement_rounds: 2
      critic_prompt: |
        Review this code for:
        1. Correctness
        2. Edge cases
        3. Performance

        Code: {{ output }}
    output: generated_code
```

### Multi-Model Support

Use different models for generation and critique:

```yaml
generator_model: gpt-4        # Strong generation
critic_model: gpt-4-turbo     # Fast critique
```

---

## `reason.decompose`

Break complex problems into sub-problems, solve each, and synthesize.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `problem` | string | Yes | - | The complex problem to decompose |
| `model` | string | No | gpt-4 | LLM model to use |
| `max_depth` | int | No | 2 | Maximum recursion depth |
| `synthesis_prompt` | string | No | - | Custom prompt for synthesis |
| `temperature` | float | No | 0.7 | LLM temperature |

### Returns

```yaml
sub_problems:
  - id: 1
    description: "Sub-problem 1"
    dependencies: []
  - id: 2
    description: "Sub-problem 2"
    dependencies: [1]
sub_answers:
  - id: 1
    answer: "Solution to sub-problem 1"
  - id: 2
    answer: "Solution to sub-problem 2"
final_answer: "Synthesized final answer"
synthesis_reasoning: "How the sub-answers were combined"
reasoning_trace: [...]
depth_used: 1
model: "gpt-4"
```

### Example

```yaml
nodes:
  - name: solve_complex
    uses: reason.decompose
    with:
      problem: "{{ state.complex_problem }}"
      model: gpt-4
      max_depth: 2
      synthesis_prompt: |
        Combine these sub-answers into a final answer:
        {{ sub_answers | tojson }}
    output: solution
```

### Recursive Decomposition

With `max_depth > 1`, sub-problems can themselves be decomposed:

```
Problem
├── Sub-problem 1
│   ├── Sub-sub-problem 1.1
│   └── Sub-sub-problem 1.2
└── Sub-problem 2
```

---

## DSPy Integration (TEA-AGENT-001.7)

The DSPy actions provide model-agnostic compiled prompts with automatic optimization. Requires the `dspy` optional dependency.

**Installation:**
```bash
pip install the_edge_agent[dspy]
# or
pip install dspy-ai>=2.0.0
```

### Settings Configuration

Configure DSPy globally in the settings section:

```yaml
settings:
  dspy:
    enabled: true
    model: gpt-4
    temperature: 0.3
    teleprompter: BootstrapFewShot
```

### `reason.dspy.cot`

Wraps DSPy's ChainOfThought module for structured reasoning with compiled prompts.

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `problem` | string | Yes | - | The problem to solve |
| `signature` | string | No | "question -> thinking, answer" | DSPy signature defining inputs/outputs |
| `model` | string | No | from settings | LLM model to use |
| `few_shot_examples` | list | No | - | Examples for few-shot prompting |
| `temperature` | float | No | 0.7 | LLM temperature |
| `compiled_key` | string | No | - | Key for pre-compiled module |

**Returns:**

```yaml
thinking: "Step-by-step reasoning..."
answer: "The final answer"
reasoning_trace: [...]
dspy_module: "ChainOfThought"  # or "native_fallback"
dspy_available: true
model: "gpt-4"
signature: "question -> thinking, answer"
```

**Example:**

```yaml
nodes:
  - name: solve_with_dspy
    action: reason.dspy.cot
    with:
      problem: "{{ state.question }}"
      signature: "question -> thinking, answer"
      few_shot_examples:
        - question: "What is 20% of 50?"
          thinking: "20% = 0.20, 0.20 * 50 = 10"
          answer: "10"
```

### `reason.dspy.react`

Wraps DSPy's ReAct module for tool-using agents with compiled prompts.

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `goal` | string | Yes | - | The goal to achieve |
| `signature` | string | No | "goal -> result" | DSPy signature |
| `model` | string | No | from settings | LLM model to use |
| `tools` | list | No | [] | List of tool/action names |
| `max_steps` | int | No | 10 | Maximum reasoning steps |
| `temperature` | float | No | 0.7 | LLM temperature |

**Returns:**

```yaml
final_answer: "The result..."
steps: []  # DSPy doesn't expose steps directly
reasoning_trace: [...]
dspy_module: "ReAct"  # or "native_fallback"
model: "gpt-4"
total_steps: 0
```

**Example:**

```yaml
nodes:
  - name: research_with_react
    action: reason.dspy.react
    with:
      goal: "{{ state.research_goal }}"
      signature: "goal -> result"
      tools:
        - web.search
        - web.scrape
      max_steps: 5
```

### `reason.dspy.compile`

Compile a DSPy module with a teleprompter for prompt optimization.

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `module_type` | string | No | "cot" | Module type: cot, react, predict |
| `signature` | string | No | "question -> answer" | DSPy signature |
| `training_data` | list | Yes | - | Training examples |
| `teleprompter` | string | No | from settings | BootstrapFewShot, BootstrapFewShotWithRandomSearch, MIPRO |
| `model` | string | No | from settings | LLM model |
| `metric` | string | No | - | Metric: "exact_match" or default presence |
| `output_key` | string | No | auto-generated | Key for storing compiled module |

**Returns:**

```yaml
compiled: true
module_type: "cot"
signature: "question -> answer"
teleprompter: "BootstrapFewShot"
training_examples: 5
module_key: "dspy_cot_question_answer_1704067200"
compiled_prompt_version: "abc123def456"
success: true
```

**Example:**

```yaml
nodes:
  - name: compile_prompts
    action: reason.dspy.compile
    with:
      module_type: cot
      signature: "question -> answer"
      training_data: "{{ state.examples }}"
      teleprompter: BootstrapFewShot
```

### `reason.dspy.optimize`

Run optimization against a validation set to find the best prompt configuration.

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `module_key` | string | No | - | Key of pre-compiled module |
| `module_type` | string | No | "cot" | Module type if creating new |
| `signature` | string | No | "question -> answer" | DSPy signature |
| `training_data` | list | Yes | - | Training examples |
| `validation_data` | list | Yes | - | Validation examples |
| `metric` | string | No | - | Metric: "exact_match" or default |
| `model` | string | No | from settings | LLM model |

**Returns:**

```yaml
success: true
train_score: 0.95
val_score: 0.87
training_examples: 10
validation_examples: 5
module_key: "dspy_optimized_cot_1704067200"
best_config:
  module_type: "cot"
  signature: "question -> answer"
  model: "gpt-4"
```

**Example:**

```yaml
nodes:
  - name: optimize_module
    action: reason.dspy.optimize
    with:
      module_type: cot
      signature: "question -> answer"
      training_data: "{{ state.train_examples }}"
      validation_data: "{{ state.val_examples }}"
      metric: exact_match
```

### `reason.dspy.export` and `reason.dspy.import`

Export and import compiled prompts for checkpoint persistence.

**Export:**
```yaml
nodes:
  - name: save_compiled
    action: reason.dspy.export
```

Returns: `{prompts: {...}, count: 2}`

**Import:**
```yaml
nodes:
  - name: load_compiled
    action: reason.dspy.import
    with:
      prompts: "{{ state.saved_prompts }}"
```

Returns: `{imported: 2, keys: [...]}`

### Teleprompter Options

| Teleprompter | Description | Use Case |
|--------------|-------------|----------|
| `BootstrapFewShot` | Basic few-shot bootstrapping | Quick optimization, small datasets |
| `BootstrapFewShotWithRandomSearch` | Random search over configurations | Better optimization, more compute |
| `MIPRO` / `MIPROv2` | Advanced optimization | Best results, highest compute cost |

### Graceful Fallback

If DSPy is not installed, the DSPy actions automatically fall back to native `reason.*` implementations:

```python
# When DSPy is unavailable:
reason.dspy.cot → reason.cot
reason.dspy.react → reason.react
reason.dspy.compile → returns error with installation instructions
reason.dspy.optimize → returns error with installation instructions
```

The fallback is transparent - your workflows continue to work, just without DSPy optimization.

---

## State Variables

All reasoning actions set these state variables:

| Variable | Type | Description |
|----------|------|-------------|
| `reasoning_trace` | list | Full reasoning trace (all steps) |
| `reasoning_thinking` | str | Chain-of-thought (for CoT) |
| `reasoning_answer` | any | Final answer |
| `react_steps` | list | ReAct thought-action-observation steps |
| `improvement_history` | list | Self-correction iterations |

---

## Observability

Reasoning traces are compatible with Opik integration:

```yaml
config:
  trace_exporter: opik

nodes:
  - name: traced_reasoning
    uses: reason.cot
    with:
      problem: "{{ state.problem }}"
    output: result
```

The `reasoning_trace` field contains timestamped entries for each reasoning step:

```json
{
  "step": "cot_request",
  "timestamp": 1704067200.123,
  "problem": "...",
  "thinking_format": "step_by_step",
  "model": "gpt-4"
}
```

---

## Best Practices

1. **Use CoT for analytical problems**: Math, logic, analysis benefit from explicit reasoning steps
2. **Use ReAct when tools are needed**: Research, data gathering, multi-step operations
3. **Use self-correction for creative tasks**: Writing, code generation, content creation
4. **Use decomposition for complex problems**: Multi-faceted questions, planning tasks
5. **Set appropriate max_steps/max_depth**: Prevent runaway execution while allowing thorough reasoning
6. **Monitor reasoning_trace**: Debug unexpected results by examining the trace
7. **Use DSPy for optimization**: When prompt quality matters and you have training examples

---

## See Also

- [LLM Actions](./llm.md) - Basic LLM calls and tool integration
- [Integrations](./integrations.md) - Web scraping and external APIs
- [Node Specification](../nodes.md) - Complete node structure
- [TEA-AGENT-001.4](../../../stories/TEA-AGENT-001.4-reasoning-techniques.md) - Story specification
