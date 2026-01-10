# Learning Actions

Learning actions provide gradient-based prompt optimization using TextGrad, enabling agents to automatically improve their prompts based on feedback.

> **Cost Warning**: TextGrad optimization requires multiple LLM calls per iteration (12+ calls for a 3-iteration optimization). Explicit opt-in is required.

## Settings Configuration

Enable TextGrad in your workflow settings:

```yaml
settings:
  textgrad:
    enabled: true                    # Required: explicit opt-in
    optimizer_model: gpt-4           # Model for gradient computation
    max_iterations: 3                # Default iteration limit
    learning_rate: 0.1               # Optimization step size
    early_stopping_threshold: 0.01   # Convergence threshold
```

## Actions

### learn.textgrad.variable

Defines a prompt as an optimizable variable with version tracking.

**Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `name` | string | Yes | Unique identifier for the variable |
| `initial_value` | string | Yes | Starting prompt text |
| `role_description` | string | No | Description of the variable's role |
| `constraints` | list | No | Constraints on prompt structure |

**Returns:**
| Field | Type | Description |
|-------|------|-------------|
| `variable_name` | string | The variable identifier |
| `variable` | object | Variable state including current_value, version |
| `version` | int | Current version number |
| `textgrad_enabled` | bool | Whether TextGrad is available |

**Example:**

```yaml
nodes:
  - name: define_prompt
    action: learn.textgrad.variable
    with:
      name: system_prompt
      initial_value: "You are a helpful assistant that provides accurate information."
      role_description: "System instruction for the LLM"
      constraints:
        - "Must be polite"
        - "Under 200 words"
```

### learn.textgrad.feedback

Computes textual gradients from output evaluation, providing structured feedback for prompt improvement.

**Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `output` | string | Yes | The output to evaluate |
| `evaluation_criteria` | list | Yes | Criteria for evaluation |
| `aspects` | list | No | Multi-aspect evaluation (accuracy, clarity, safety) |

**Returns:**
| Field | Type | Description |
|-------|------|-------------|
| `valid` | bool | Whether output meets criteria |
| `feedback` | string | Textual feedback/gradient |
| `gradient_text` | string | Improvement suggestions |
| `scores` | object | Scores per aspect |
| `aspects_evaluated` | list | Which aspects were evaluated |

**Example:**

```yaml
nodes:
  - name: evaluate_output
    action: learn.textgrad.feedback
    with:
      output: "{{ state.llm_response }}"
      evaluation_criteria:
        - "Is the response accurate?"
        - "Is the response helpful?"
      aspects:
        - accuracy
        - clarity
        - safety
```

### learn.textgrad.optimize_prompt

Optimizes a prompt variable based on a loss function over multiple iterations.

**Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `variable` | string | Yes | Name of variable to optimize |
| `loss_fn` | string | Yes | Loss function template (Jinja2) |
| `iterations` | int | No | Number of optimization iterations (default: 3) |
| `constraints` | list | No | Additional constraints for optimization |

**Returns:**
| Field | Type | Description |
|-------|------|-------------|
| `optimized_value` | string | The improved prompt |
| `initial_value` | string | Original prompt value |
| `iterations_completed` | int | Number of iterations run |
| `improvement_trace` | list | History of improvements |
| `converged` | bool | Whether optimization converged |
| `textgrad_enabled` | bool | Whether TextGrad was used |

**Example:**

```yaml
nodes:
  - name: optimize_prompt
    action: learn.textgrad.optimize_prompt
    with:
      variable: system_prompt
      loss_fn: |
        Evaluate if this prompt produces accurate and helpful responses.
        Current response: {{ state.response }}
        Was it accurate? Was it complete? What could be improved?
      iterations: 3
```

### learn.textgrad.reflection_corrector

Integrates TextGrad with reflection loops, triggering optimization after repeated failures.

**Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `variable` | string | Yes | Name of variable to optimize |
| `trigger_threshold` | int | No | Failures before optimization (default: 2) |
| `optimization_iterations` | int | No | Iterations per optimization (default: 3) |

**Returns:**
| Field | Type | Description |
|-------|------|-------------|
| `optimization_triggered` | bool | Whether threshold was reached |
| `reflection_iteration` | int | Current reflection iteration |
| `reflection_context_used` | bool | Whether history was used |
| `optimized_value` | string | New prompt value (if optimized) |

**Example:**

```yaml
nodes:
  - name: generate_with_learning
    action: reflection.loop
    with:
      generator:
        action: llm.call
        with:
          prompt: "{{ state.optimized_prompt }}"
      evaluator:
        type: llm
        prompt: "Evaluate the quality of this response..."
      corrector:
        action: learn.textgrad.reflection_corrector
        with:
          variable: optimized_prompt
          trigger_threshold: 2
          optimization_iterations: 3
      max_iterations: 5
```

## Alternative Namespaces

All actions are available under multiple namespaces:

| Primary | Alternative 1 | Alternative 2 |
|---------|--------------|---------------|
| `learn.textgrad.variable` | `textgrad.variable` | `actions.textgrad_variable` |
| `learn.textgrad.feedback` | `textgrad.feedback` | `actions.textgrad_feedback` |
| `learn.textgrad.optimize_prompt` | `textgrad.optimize_prompt` | `actions.textgrad_optimize_prompt` |
| `learn.textgrad.reflection_corrector` | `textgrad.reflection_corrector` | `actions.textgrad_reflection_corrector` |

## Complete Workflow Example

```yaml
name: self-improving-agent
description: Agent that learns from failures and improves its prompts

settings:
  textgrad:
    enabled: true
    optimizer_model: gpt-4
    max_iterations: 5

state_schema:
  task: str
  system_prompt: str
  response: str
  quality_score: float

nodes:
  - name: init_prompt
    action: learn.textgrad.variable
    with:
      name: system_prompt
      initial_value: |
        You are a helpful assistant. Answer questions accurately and concisely.
      constraints:
        - "Be accurate"
        - "Be helpful"

  - name: generate_response
    action: llm.call
    with:
      model: gpt-4
      system: "{{ state.system_prompt }}"
      prompt: "{{ state.task }}"
    output: response

  - name: evaluate_response
    action: learn.textgrad.feedback
    with:
      output: "{{ state.response }}"
      evaluation_criteria:
        - "Is the answer factually correct?"
        - "Does it fully address the question?"
      aspects:
        - accuracy
        - completeness
        - clarity

  - name: check_quality
    action: core.condition
    with:
      condition: "{{ state.quality_score >= 0.8 }}"
    edges:
      true: finish
      false: optimize

  - name: optimize
    action: learn.textgrad.optimize_prompt
    with:
      variable: system_prompt
      loss_fn: |
        The response was: {{ state.response }}
        Quality score: {{ state.quality_score }}
        Improve the system prompt to produce better responses.
      iterations: 2

  - name: finish
    action: core.return
    with:
      result: "{{ state.response }}"

edges:
  - from: __start__
    to: init_prompt
  - from: init_prompt
    to: generate_response
  - from: generate_response
    to: evaluate_response
  - from: evaluate_response
    to: check_quality
  - from: optimize
    to: generate_response
```

## Dependencies

TextGrad is an optional dependency:

```bash
pip install the_edge_agent[textgrad]
# or
pip install textgrad>=0.1.0
```

When TextGrad is not installed, actions will:
- Return graceful fallbacks (not crash)
- Use heuristic-based feedback instead of LLM gradients
- Skip optimization (return original prompt unchanged)

## Best Practices

1. **Start with fewer iterations**: Begin with 2-3 iterations and increase if needed
2. **Monitor costs**: Each iteration requires multiple LLM calls
3. **Define clear loss functions**: Vague criteria lead to poor optimization
4. **Use constraints**: Prevent prompt drift with structural constraints
5. **Combine with reflection**: Use `reflection_corrector` for automatic trigger

## References

- [TextGrad GitHub](https://github.com/zou-group/textgrad)
- [TextGrad Paper](https://arxiv.org/abs/2406.07496) (Nature 2024)
- Story: TEA-AGENT-001.9 (TextGrad Learning)
