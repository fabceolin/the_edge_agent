# Reflection Actions

> **Parent document:** [Actions Index](./README.md)
> **Story:** [TEA-AGENT-001.2 Reflection Loop](../../../stories/TEA-AGENT-001.2-reflection-loop.md)

## Overview

Reflection actions provide self-correcting agent capabilities through generate-evaluate-correct loops. These patterns enable agents to iteratively improve their outputs based on structured feedback.

**Key Features:**
- Multiple evaluator types (schema, LLM, custom code)
- Configurable failure strategies
- Full iteration history tracking
- Type coercion for schema validation
- Lua and Prolog evaluator support

---

## Actions

| Action | Description |
|--------|-------------|
| [`reflection.loop`](#reflectionloop) | Execute generate→evaluate→correct cycle |
| [`reflection.evaluate`](#reflectionevaluate) | Standalone evaluation action |
| [`reflection.correct`](#reflectioncorrect) | Standalone correction action |

---

## reflection.loop

Execute an iterative generate→evaluate→correct loop until output passes validation or max iterations is reached.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `generator` | object | Yes | - | Generator configuration (action or run) |
| `evaluator` | object | Yes | - | Evaluator configuration (type + params) |
| `corrector` | object | No | - | Corrector configuration (action or run) |
| `max_iterations` | int | No | `3` | Maximum correction attempts |
| `on_failure` | string | No | `"return_best"` | Strategy when max iterations reached |

### Generator Configuration

The generator produces output to be evaluated:

```yaml
generator:
  # Option 1: Use an action
  action: llm.call
  model: gpt-4
  prompt: "Generate a JSON user profile..."

  # Option 2: Inline Python code
  run: |
    import json
    return json.dumps({"name": "test", "email": "test@example.com"})
```

### Evaluator Configuration

Three evaluator types are supported:

#### Schema Evaluator

Uses JSON Schema validation:

```yaml
evaluator:
  type: schema
  schema:
    type: object
    required: [name, email]
    properties:
      name:
        type: string
        minLength: 1
      email:
        type: string
        pattern: "^[^@]+@[^@]+\\.[^@]+$"
```

**Features:**
- Automatic type coercion (string→int, string→boolean)
- External schema references via `$ref`
- Detailed error paths and suggestions

#### LLM Evaluator

Uses LLM-as-judge:

```yaml
evaluator:
  type: llm
  model: gpt-4
  prompt: |
    Evaluate if this JSON is a valid user profile:
    {{ state.reflection_output | tojson }}

    Requirements:
    - Has name and email fields
    - Email looks valid
    - No placeholder data
  examples:
    - input: '{"name": "John", "email": "john@test.com"}'
      output: '{"valid": true, "score": 1.0, "reason": "Valid profile"}'
    - input: '{"name": "", "email": "invalid"}'
      output: '{"valid": false, "score": 0.2, "reason": "Empty name, invalid email"}'
```

#### Custom Evaluator

Uses inline Python, Lua, or Prolog code:

```yaml
evaluator:
  type: custom
  language: python  # or: lua, prolog
  run: |
    import json
    try:
        data = json.loads(output) if isinstance(output, str) else output
        has_name = bool(data.get("name"))
        has_email = "@" in data.get("email", "")
        score = (has_name + has_email) / 2
        result = {
            "valid": score >= 1.0,
            "score": score,
            "errors": [] if score >= 1.0 else [{"message": "Missing required fields"}],
            "suggestions": ["Add name field"] if not has_name else []
        }
    except:
        result = {"valid": False, "score": 0.0, "errors": [{"message": "Invalid JSON"}]}
```

### Corrector Configuration

The corrector attempts to fix invalid output:

```yaml
corrector:
  action: llm.call
  model: gpt-4
  prompt: |
    Fix this JSON based on the validation errors:

    Original: {{ state.reflection_output | tojson }}

    Errors:
    {% for error in state.reflection_errors %}
    - {{ error.message }}
    {% endfor %}

    Return only the corrected JSON, no explanation.
```

### On-Failure Strategies

| Strategy | Description |
|----------|-------------|
| `return_best` | Return the attempt with highest score |
| `return_last` | Return the final attempt |
| `raise` | Raise `ReflectionFailedError` with history |

### State Variables

The loop sets these state variables during execution:

| Variable | Type | Description |
|----------|------|-------------|
| `reflection_iteration` | int | Current iteration (1-based) |
| `reflection_output` | any | Current generator output |
| `reflection_errors` | list | Errors from current evaluation |
| `reflection_history` | list | All attempts with outputs and scores |
| `reflection_best` | any | Best output seen so far |
| `reflection_best_score` | float | Score of best output (0.0-1.0) |

### Return Value

```yaml
# On success (valid output produced)
{
  "success": true,
  "valid": true,
  "reflection_iteration": 2,
  "reflection_output": {"name": "John", "email": "john@example.com"},
  "reflection_errors": [],
  "reflection_history": [...],
  "reflection_best": {"name": "John", "email": "john@example.com"},
  "reflection_best_score": 1.0,
  # Plus all keys from output if it's a dict
  "name": "John",
  "email": "john@example.com"
}

# On failure (max iterations exhausted)
{
  "success": false,
  "valid": false,
  "exhausted": true,
  "reflection_iteration": 3,
  "reflection_output": {...},
  "reflection_errors": [...],
  "reflection_history": [...],
  "reflection_best": {...},
  "reflection_best_score": 0.7
}
```

### Complete Example

```yaml
name: json-generator-with-reflection
description: Generate valid JSON with self-correction

nodes:
  - name: generate_profile
    uses: reflection.loop
    with:
      generator:
        action: llm.call
        model: gpt-4
        prompt: |
          Generate a JSON user profile with name, email, and age.
          User request: {{ state.request }}
          Return only valid JSON.

      evaluator:
        type: schema
        schema:
          type: object
          required: [name, email, age]
          properties:
            name:
              type: string
              minLength: 1
            email:
              type: string
              pattern: "^[^@]+@[^@]+\\.[^@]+$"
            age:
              type: integer
              minimum: 0
              maximum: 150

      corrector:
        action: llm.call
        model: gpt-4
        prompt: |
          Fix this JSON based on validation errors:

          Original: {{ state.reflection_output | tojson }}

          Errors:
          {% for error in state.reflection_errors %}
          - Path: {{ error.path }}, Message: {{ error.message }}
          {% endfor %}

          Return only corrected JSON.

      max_iterations: 3
      on_failure: return_best
    output: profile

  - name: use_profile
    run: |
      if state.get("success"):
          return {"message": f"Created profile for {state['name']}"}
      else:
          return {"message": "Failed to generate valid profile", "best_attempt": state.get("reflection_best")}
```

---

## reflection.evaluate

Standalone evaluation action for use outside the loop.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `data` | any | No | `state.reflection_output` | Data to evaluate |
| `evaluator_type` | string | No | `"schema"` | Type: schema, llm, custom |
| `schema` | object | Conditional | - | JSON Schema (for schema type) |
| `prompt` | string | Conditional | - | Evaluation prompt (for llm type) |
| `model` | string | No | From state | LLM model (for llm type) |
| `examples` | list | No | `[]` | Few-shot examples (for llm type) |
| `run` | string | Conditional | - | Code (for custom type) |
| `language` | string | No | `"python"` | Code language (for custom type) |

### Example

```yaml
nodes:
  - name: validate_output
    uses: reflection.evaluate
    with:
      data: "{{ state.generated_json }}"
      evaluator_type: schema
      schema:
        type: object
        required: [status, data]
    output: validation

  - name: check_result
    run: |
      if state["validation"]["valid"]:
          return {"status": "valid"}
      else:
          return {"status": "invalid", "errors": state["validation"]["errors"]}
```

---

## reflection.correct

Standalone correction action.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `data` | any | No | `state.reflection_output` | Data to correct |
| `errors` | list | No | `state.reflection_errors` | Validation errors |
| `corrector_action` | string | Conditional | - | Action to use |
| `run` | string | Conditional | - | Inline Python code |
| `prompt` | string | Conditional | - | LLM prompt (uses llm.call) |

### Example

```yaml
nodes:
  - name: fix_json
    uses: reflection.correct
    with:
      data: "{{ state.invalid_json }}"
      errors: "{{ state.validation_errors }}"
      prompt: |
        Fix this JSON:
        {{ state.invalid_json | tojson }}

        Errors: {{ state.validation_errors | tojson }}
    output: corrected
```

---

## Error Handling

### ReflectionFailedError

When `on_failure: raise` and max iterations are exhausted:

```python
from the_edge_agent.actions import ReflectionFailedError

try:
    result = graph.invoke({"request": "create user"})
except ReflectionFailedError as e:
    print(f"Reflection failed: {e}")
    print(f"History: {e.history}")
    # Each history entry: {"iteration": int, "output": any, "score": float, "valid": bool, "errors": list}
```

### Graceful Degradation

For non-critical workflows, use `on_failure: return_best`:

```yaml
- name: generate_with_fallback
  uses: reflection.loop
  with:
    generator:
      action: llm.call
      prompt: "Generate content..."
    evaluator:
      type: schema
      schema: {type: object, required: [content]}
    max_iterations: 3
    on_failure: return_best  # Return best attempt instead of failing
  output: result

- name: handle_result
  run: |
    if state.get("success"):
        return {"content": state["result"]["content"]}
    else:
        # Use best attempt or fallback
        best = state.get("reflection_best", {})
        return {"content": best.get("content", "Default content")}
```

---

## Best Practices

1. **Set appropriate max_iterations**: 3-5 is usually sufficient; more can waste tokens
2. **Use schema validation first**: It's faster and deterministic
3. **Provide good corrector prompts**: Include specific errors and examples
4. **Track iteration history**: Use `reflection_history` for debugging
5. **Consider fallbacks**: Use `on_failure: return_best` for non-critical paths

---

## See Also

- [Reasoning Actions](./reasoning.md) - Chain-of-Thought, ReAct patterns
- [Planning Actions](./planning.md) - Task decomposition with correction
- [Agent Actions](./agent.md) - Multi-agent with reflection
