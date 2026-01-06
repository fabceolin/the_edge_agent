# Specialized Actions

> **Parent document:** [Actions Overview](./README.md)
> **Related:** [Node Specification](../nodes.md)
> **Epic:** [DOC-002](../../../stories/DOC-002-yaml-reference-modularization.md)

## Overview

Specialized actions provide checkpoint persistence, schema manipulation, document extraction (LlamaExtract), extraction validation, retry loops, and custom action registration.

---

## Table of Contents

- [Checkpoint Actions](#checkpoint-actions)
  - [checkpoint.save](#checkpointsave)
  - [checkpoint.load](#checkpointload)
  - [Checkpoint Configuration](#checkpoint-configuration)
- [Schema Actions](#schema-actions)
  - [schema.merge](#schemamerge)
- [LlamaExtract Actions](#llamaextract-actions)
  - [llamaextract.extract](#llamaextractextract)
  - [llamaextract.upload_agent](#llamaextractupload_agent)
  - [llamaextract.list_agents](#llamaextractlist_agents)
  - [llamaextract.get_agent](#llamaextractget_agent)
  - [llamaextract.delete_agent](#llamaextractdelete_agent)
- [Validation Actions](#validation-actions)
  - [validate.input](#validateinput)
  - [validate.extraction](#validateextraction)
  - [validate.generate_prompt](#validategenerate_prompt)
  - [Extraction Schema](#extraction-schema)
  - [Validation Constraints](#validation-constraints)
  - [Semantic Probes](#semantic-probes)
  - [Validation Logging](#validation-logging)
- [Retry Actions](#retry-actions)
  - [retry.loop](#retryloop)
- [Reflection Actions](#reflection-actions)
  - [reflection.loop](#reflectionloop)
  - [reflection.evaluate](#reflectionevaluate)
  - [reflection.correct](#reflectioncorrect)
  - [Evaluator Types](#evaluator-types)
  - [On-Failure Strategies](#on-failure-strategies)
  - [State Variables](#state-variables)
- [Rate Limiting Actions](#rate-limiting-actions)
  - [ratelimit.wrap](#ratelimitwrap)
  - [Parallel Rate Limiting](#parallel-rate-limiting)
  - [Cache + Rate Limit Composition](#cache--rate-limit-composition)
  - [Settings-Based Configuration](#settings-based-configuration)
- [Secrets Actions](#secrets-actions)
  - [secrets.get](#secretsget)
  - [secrets.has](#secretshas)
  - [Secrets Configuration](#secrets-configuration)
  - [Cloud Provider Backends](#cloud-provider-backends)
- [HTTP Response Actions](#http-response-actions)
  - [http.respond](#httprespond)
- [Response Transformation](#response-transformation)
  - [output_schema](#output_schema)
  - [Field Mapping](#field-mapping)
  - [Conditional Fields](#conditional-fields)
  - [Default Values](#default-values)
- [Custom Actions](#custom-actions)
- [Server Endpoints](#server-endpoints)
  - [Health Endpoint](#health-endpoint)
  - [Readiness Endpoint](#readiness-endpoint)
  - [Agents List Endpoint](#agents-list-endpoint)
  - [Metrics Endpoint](#metrics-endpoint)
  - [OpenAPI Endpoint](#openapi-endpoint)
  - [Custom Health Checks](#custom-health-checks)
  - [Kubernetes Probe Configuration](#kubernetes-probe-configuration)

---

## Checkpoint Actions

### `checkpoint.save`

Save workflow checkpoint:

```yaml
- name: save_progress
  uses: checkpoint.save
  with:
    path: ./checkpoints/{{ state.step_name }}.pkl  # Required
  output: save_result
```

**Returns:**
- Success: `{"checkpoint_path": str, "saved": true}`
- Failure: `{"checkpoint_path": str, "saved": false, "error": str}`

### `checkpoint.load`

Load checkpoint from file:

```yaml
- name: load_previous
  uses: checkpoint.load
  with:
    path: ./checkpoints/previous.pkl               # Required
  output: loaded_checkpoint
```

**Returns:**
```python
{
  "checkpoint_state": dict,
  "checkpoint_node": str,
  "checkpoint_config": dict,
  "checkpoint_timestamp": float,
  "checkpoint_version": str
}
```

### Checkpoint Configuration

YAML agents support checkpoint persistence for saving and resuming workflow execution.

```yaml
config:
  # Directory for auto-save checkpoints at interrupt points
  checkpoint_dir: ./checkpoints

  # Resume from a specific checkpoint on load
  checkpoint: ./checkpoints/resume_point.pkl

  # Interrupt at specific nodes (triggers auto-save)
  interrupt_before: [critical_node]
  interrupt_after: [validation_node]
```

**Auto-Save at Interrupts:**

When `checkpoint_dir` is configured, checkpoints are automatically saved before yielding interrupt events. Files are saved as `{checkpoint_dir}/{node}_{timestamp_ms}.pkl`.

**Resume from Checkpoint:**

```yaml
config:
  checkpoint: ./checkpoints/review_node_1733500000.pkl
```

Or in Python:

```python
graph = engine.load_from_file("agent.yaml", checkpoint="./checkpoints/state.pkl")
```

**Template Variables for Checkpoints:**

- `{{ checkpoint.dir }}` - The configured `checkpoint_dir` value
- `{{ checkpoint.last }}` - Path to the most recent checkpoint saved

---

## Schema Actions

Schema manipulation actions for merging and loading JSON Schemas.

### `schema.merge`

Deep merge multiple JSON Schemas using kubectl-style semantics:

```yaml
- name: merge_schemas
  uses: schema.merge
  with:
    schemas:
      - path: ./base-schema.json
      - uses: company/schemas@v1.0.0#overlay.json
      - inline:
          properties:
            custom_field:
              type: string
    validate: true  # Optional: validate output schema
    output_key: merged  # Optional: default "merged_schema"
  output: schema_result
```

**Merge Semantics:**
- Objects: Recursively merged (overlay adds/overrides properties)
- Arrays: Last-wins (overlay replaces base array)
- Scalars: Last-wins
- `null`: Explicit null removes the key

**Schema Sources:**
- `path`: Local file path
- `uses`: Git reference (`owner/repo@ref#path`) or fsspec URI (`s3://bucket/path`)
- `inline`: Inline JSON Schema object

**Returns:**
```python
{
  "merged_schema": dict,  # The merged schema
  "success": true
}
```

---

## LlamaExtract Actions

Document extraction using LlamaCloud's LlamaExtract service.

**Requirements:**
- `requests` package (for REST API - default)
- `llama-cloud-services` package (optional - only for `agent_name` lookup or `use_sdk=true`)
- `LLAMAEXTRACT_API_KEY` or `LLAMAPARSE_API_KEY` environment variable

### `llamaextract.extract`

Extract structured data from documents using the LlamaExtract REST API.

```yaml
- name: extract_invoice
  uses: llamaextract.extract
  with:
    file: https://example.com/invoice.pdf  # URL, local path, or base64
    schema:
      type: object
      properties:
        total: { type: number }
        vendor: { type: string }
    mode: BALANCED  # BALANCED, MULTIMODAL, PREMIUM, FAST
    timeout: 300    # Optional: HTTP timeout in seconds (default: 300)
    max_retries: 3  # Optional: default 3
  output: extracted_data
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `file` | string | Yes | URL, local file path, or base64-encoded content |
| `schema` | dict | One of these | JSON Schema for extraction |
| `agent_id` | string | One of these | Use existing LlamaExtract agent by ID |
| `agent_name` | string | One of these | Use existing LlamaExtract agent by name (uses SDK) |
| `mode` | string | No | Extraction mode (default: BALANCED) |
| `timeout` | int | No | HTTP request timeout in seconds (default: 300) |
| `max_retries` | int | No | Max retry attempts for 429/5xx errors (default: 3) |
| `use_rest` | bool | No | Use direct REST API instead of SDK (default: false) |

**Extraction Modes:**
- `BALANCED`: Good balance of speed and accuracy (default)
- `MULTIMODAL`: Uses vision models for complex layouts
- `PREMIUM`: Highest accuracy, slower processing
- `FAST`: Fastest processing, may sacrifice accuracy

**Error Handling:**
- HTTP 429 (rate limit): Retries with exponential backoff
- HTTP 5xx (server error): Retries with exponential backoff
- HTTP 4xx (client error): Returns immediately without retry
- Timeout: Returns `error_type: "timeout"`

**Returns:**
```python
{
  "success": true,
  "data": {...},  # Extracted data matching schema
  "status": "completed"
}
```

**Error Response:**
```python
{
  "success": false,
  "error": "Error message",
  "error_type": "rate_limit" | "timeout" | "api_error" | "validation" | "configuration"
}
```

### `llamaextract.upload_agent`

Create or update a LlamaExtract agent:

```yaml
- name: create_agent
  uses: llamaextract.upload_agent
  with:
    name: invoice-extractor
    schema:
      type: object
      properties:
        total: { type: number }
    mode: BALANCED
    force: false  # Optional: update if exists
  output: agent_result
```

**Returns:**
```python
{
  "success": true,
  "agent_id": str,
  "agent_name": str
}
```

### `llamaextract.list_agents`

List available extraction agents:

```yaml
- name: list_all
  uses: llamaextract.list_agents
  with:
    name_filter: invoice  # Optional: filter by name
  output: agents_list
```

**Returns:**
```python
{
  "success": true,
  "agents": [{"id": str, "name": str}, ...]
}
```

### `llamaextract.get_agent`

Get details of a specific agent:

```yaml
- name: get_agent
  uses: llamaextract.get_agent
  with:
    agent_id: abc123  # or agent_name
  output: agent_info
```

### `llamaextract.delete_agent`

Delete an extraction agent:

```yaml
- name: remove_agent
  uses: llamaextract.delete_agent
  with:
    agent_id: abc123  # or agent_name
  output: delete_result
```

---

## Validation Actions

Actions for validating input data and LLM-extracted data.

### `validate.input`

Validates input data against a schema for explicit mid-flow validation:

```yaml
- name: validate_user_input
  uses: validate.input
  with:
    data: "{{ state.user_provided_data }}"
    schema:
      name:
        type: str
        required: true
        min_length: 1
      email:
        type: str
        pattern: "^[\\w.-]+@[\\w.-]+\\.\\w+$"
      age:
        type: int
        min: 0
        max: 150
  output: validation_result
```

**Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `data` | dict | No | Data to validate (defaults to entire state) |
| `schema` | dict | Yes | Validation schema definition |
| `raise_on_error` | bool | No | If true, raises exception on validation failure |

**Returns:**
```python
# Success
{"valid": True, "data": {...}}  # Coerced/defaulted data

# Failure
{
    "valid": False,
    "errors": [
        {"field": "name", "error": "required", "message": "Field 'name' is required"},
        {"field": "age", "error": "min", "message": "Field 'age' must be at least 0", "value": -5, "constraint": 0}
    ]
}
```

**Schema Field Options:**
| Option | Types | Description |
|--------|-------|-------------|
| `type` | all | Field type: `str`, `int`, `float`, `bool`, `list`, `dict` |
| `required` | all | Error if field is missing |
| `default` | all | Value when field is missing |
| `min_length` | str | Minimum string length |
| `max_length` | str | Maximum string length |
| `pattern` | str | Regex pattern |
| `min` | int, float | Minimum numeric value |
| `max` | int, float | Maximum numeric value |
| `choices` | all | Allowed values list |
| `properties` | dict | Nested field schemas |
| `items` | list | Schema for list elements |

**Example: Conditional Flow Based on Validation**

```yaml
nodes:
  - name: validate
    uses: validate.input
    with:
      data: "{{ state }}"
      schema:
        query:
          type: str
          required: true
    output: result
    goto:
      - when: "{{ state.result.valid }}"
        then: process
      - then: handle_error

  - name: process
    run: |
      return {"output": state["query"].upper()}
    goto: __end__

  - name: handle_error
    run: |
      return {"error": "Invalid input", "details": state["result"]["errors"]}
    goto: __end__
```

### `validate.extraction`

Validates extracted entities and relationships through 3 layers:

```yaml
- name: validate_data
  action: validate.extraction
  inputs:
    entities: "{{ state.entities }}"
    relationships: "{{ state.relationships }}"
    source_text: "{{ state.input_text }}"  # For semantic probes
  output: validation_result
```

The validation result includes:
- `valid`: Boolean indicating overall validation success
- `errors`: List of validation errors with details
- `validated_at`: ISO timestamp

### `validate.generate_prompt`

Generates a schema-guided extraction prompt from the `extraction_schema`:

```yaml
- name: get_prompt
  action: validate.generate_prompt
  output: extraction_prompt
```

This is automatically done when `guide_extraction: true` is set in the schema.

### Extraction Schema

Defines the structure of expected entities and relationships:

```yaml
extraction_schema:
  entities:
    required_fields: [name, type]       # Fields that must be present
    optional_fields: [birth_date]       # Fields that may be present
    type_field: type                    # Field containing entity type

  relationships:
    types: [mother, father, spouse]     # Allowed relationship types
    required_fields: [type, subject, object]
    optional_fields: [confidence]
    type_requirements:                  # Type-specific requirements
      affair:
        - start_date
        - end_date

  guide_extraction: true                # Generate extraction prompt
  confidence_tracking: true             # Include confidence in Prolog facts
```

**Field Descriptions:**

| Field | Type | Description |
|-------|------|-------------|
| `entities.required_fields` | list | Fields every entity must have |
| `entities.optional_fields` | list | Fields entities may have |
| `entities.type_field` | string | Field name containing entity type (default: `type`) |
| `relationships.types` | list | Allowed relationship type values (empty = any) |
| `relationships.required_fields` | list | Fields every relationship must have |
| `relationships.type_requirements` | dict | Extra required fields per relationship type |
| `guide_extraction` | bool | If true, generates `extraction_prompt` variable |
| `confidence_tracking` | bool | If true, passes confidence to Prolog facts |

**Error Types:**
- `missing_required_field`: Required field is missing or null
- `invalid_relationship_type`: Relationship type not in allowed list

### Validation Constraints

Prolog rules for semantic validation:

```yaml
validation_constraints:
  language: prolog                      # Only "prolog" supported
  rules: |
    % A person cannot be their own parent
    validation_error(self_parent, Person) :-
        relationship(Type, Person, Person),
        member(Type, ['mother', 'father']).

    % Each child can have at most one mother
    validation_error(multiple_mothers, Child) :-
        relationship('mother', M1, Child),
        relationship('mother', M2, Child),
        M1 \= M2.

    % Warn on low confidence (using entity/3 with confidence)
    validation_error(low_confidence, Entity) :-
        entity(Entity, _, Conf),
        Conf < 0.5.
```

**Available Prolog Predicates:**

| Predicate | Description |
|-----------|-------------|
| `entity(Name, Type)` | Entity without confidence |
| `entity(Name, Type, Confidence)` | Entity with confidence (when `confidence_tracking: true`) |
| `relationship(Type, Subject, Object)` | Basic relationship |
| `relationship(Type, Subject, Object, Confidence)` | With confidence |
| `relationship(Type, Subject, Object, Start, End)` | With date range |
| `validation_error(ErrorType, Context)` | Define a constraint violation |

When any `validation_error/2` predicate succeeds, validation fails.

### Semantic Probes

LLM-verified grounding checks:

```yaml
semantic_probes:
  - for_each: relationship              # "entity" or "relationship"
    where: "type == 'mother'"           # Optional filter condition
    probe: |
      Based on the text, is {{ subject }} explicitly stated
      to be the mother of {{ object }}?

      Text: "{{ state.input_text }}"
    on_fail: reject                     # "reject" (fail-fast) or "warn"

  - for_each: entity
    probe: "Is {{ name }} a real person mentioned in the text?"
    on_fail: warn
```

**Field Descriptions:**

| Field | Type | Description |
|-------|------|-------------|
| `for_each` | string | Item type to iterate: `entity` or `relationship` |
| `where` | string | Filter expression (e.g., `type == 'mother'`) |
| `probe` | string | Jinja2 template for yes/no question |
| `on_fail` | string | `reject` (default) for fail-fast, `warn` to continue |

**Template Variables:**
- All fields from the current entity/relationship
- `{{ state.field }}` - Access full state
- `{{ text }}` - Source text passed to validation

**Error Types:**
- `semantic_probe_failed`: LLM answered "no" to probe
- `probe_execution_error`: LLM call failed
- `configuration_error`: No LLM configured for probes

### Validation Logging

Log failures for analysis and model improvement:

```yaml
validation_logging:
  enabled: true
  log_path: "${VALIDATION_LOG_PATH:-./validation_failures.jsonl}"
  include_source: true                  # Include source text
  include_timestamp: true               # Include ISO timestamp
```

**JSONL Output Format:**

```json
{
  "agent_name": "family-extractor",
  "source_hash": "a1b2c3...",
  "source_text": "Mary is the mother of John.",
  "entities": [...],
  "relationships": [...],
  "validation_result": {"valid": false, "errors": [...]},
  "timestamp": "2024-01-15T10:30:00Z"
}
```

### Complete Validation Example

```yaml
name: family-validator

extraction_schema:
  entities:
    required_fields: [name, type]
  relationships:
    types: [mother, father]
    required_fields: [type, subject, object]
  guide_extraction: true
  confidence_tracking: true

validation_constraints:
  language: prolog
  rules: |
    validation_error(self_parent, P) :-
        relationship(T, P, P), member(T, ['mother', 'father']).

semantic_probes:
  - for_each: relationship
    probe: "Is {{ subject }} the {{ type }} of {{ object }}?"
    on_fail: reject

validation_logging:
  enabled: true
  log_path: ./failures.jsonl

nodes:
  - name: validate
    action: validate.extraction
    inputs:
      entities: "{{ state.entities }}"
      relationships: "{{ state.relationships }}"
      source_text: "{{ state.text }}"
    output: result

edges:
  - from: __start__
    to: validate
  - from: validate
    to: __end__
```

---

## Retry Actions

### `retry.loop`

Execute validation with automatic retry and correction loops.

```yaml
- name: validate_with_retry
  uses: retry.loop
  with:
    validate: validate.extraction       # Required: validation action to call
    validate_args:                      # Arguments passed to validation action
      entities: "{{ state.entities }}"
      relationships: "{{ state.relationships }}"
    correct: correct_extraction         # Required: correction node name
    max_retries: 2                      # Optional: max correction attempts (default: 1)
    retry_delay: 0.5                    # Optional: delay between retries (default: 0)
  output: retry_result
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `validate` | string | Yes | Name of validation action to call |
| `validate_args` | dict | No | Arguments for validation action |
| `correct` | string | Yes | Name of correction node to execute on failure |
| `max_retries` | int | No | Maximum correction attempts (default: 1) |
| `retry_delay` | float | No | Delay in seconds between retries (default: 0) |

**State Variables Set:**

| Variable | Type | Description |
|----------|------|-------------|
| `_retry_count` | int | Current retry attempt (0-indexed) |
| `_retry_errors` | list | Errors from last validation attempt |
| `_retry_result` | dict | Final validation result |
| `_retry_exhausted` | bool | True if max retries reached without success |

**Workflow:**
1. Call validation action
2. If valid, return success with `_retry_exhausted: false`
3. If invalid and retries remaining:
   - Set `_retry_errors` for correction context
   - Execute correction node
   - Increment `_retry_count`
   - Go to step 1
4. If invalid and no retries left, return failure with `_retry_exhausted: true`

**Returns:**
- Success: Validation result plus retry tracking state
- Failure: Last validation errors plus `_retry_exhausted: true`

**Example with LLM Correction:**

```yaml
nodes:
  - name: extract_with_retry
    uses: retry.loop
    with:
      validate: validate.extraction
      validate_args:
        entities: "{{ state.entities }}"
        relationships: "{{ state.relationships }}"
      correct: fix_with_llm
      max_retries: 2

  - name: fix_with_llm
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: system
          content: |
            Fix the extraction errors. Current retry: {{ state._retry_count }}
            Errors: {{ state._retry_errors | tojson }}
        - role: user
          content: "Re-extract from: {{ state.input_text }}"
    output: corrected_extraction

edges:
  - from: __start__
    to: extract_with_retry
  - from: extract_with_retry
    to: __end__
```

---

## Reflection Actions

Self-correcting agents using the reflection loop pattern (TEA-AGENT-001.2). Implements automatic generate→evaluate→correct cycles with configurable evaluators and failure strategies.

**Key Features:**
- Automatic iteration loop with circuit breaker
- Multiple evaluator types: schema, LLM, custom code
- On-failure strategies: return_best, return_last, raise
- Full iteration history tracking
- Standalone evaluate and correct actions

### `reflection.loop`

Execute a self-correcting generation loop:

```yaml
- name: generate_with_reflection
  uses: reflection.loop
  with:
    generator:
      action: llm.call                    # Any action or inline run:
      model: "ollama/gemma3:4b"
      messages:
        - role: system
          content: "Generate valid JSON for a user profile."
        - role: user
          content: "{{ state.request }}"

    evaluator:
      type: schema                        # schema | llm | custom
      schema:
        type: object
        required: [name, email]
        properties:
          name: { type: string }
          email: { type: string, format: email }

    corrector:
      action: llm.call
      messages:
        - role: system
          content: "Fix the JSON based on validation errors."
        - role: user
          content: |
            Original: {{ state.reflection_output | tojson }}
            Errors: {{ state.reflection_errors | tojson }}
            Please fix and return valid JSON.

    max_iterations: 3                     # Default: 3
    on_failure: return_best               # return_best | return_last | raise

  output: reflection_result
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `generator` | object | Yes | Generator configuration (action or run) |
| `evaluator` | object | Yes | Evaluator configuration |
| `corrector` | object | No | Corrector configuration (action or run) |
| `max_iterations` | int | No | Maximum attempts (default: 3) |
| `on_failure` | string | No | Strategy when exhausted (default: return_best) |

**Returns:**
```yaml
reflection_iteration: 2           # Final iteration count
reflection_output: {...}          # Final output
reflection_errors: []             # Empty if valid
reflection_history:               # All attempts
  - iteration: 1
    output: {...}
    score: 0.0
    valid: false
    errors: [...]
  - iteration: 2
    output: {...}
    score: 1.0
    valid: true
    errors: []
reflection_best: {...}            # Best scoring output
reflection_best_score: 1.0        # Best score
success: true                     # True if valid output produced
valid: true                       # True if final output is valid
```

### `reflection.evaluate`

Standalone evaluation action:

```yaml
- name: evaluate_output
  uses: reflection.evaluate
  with:
    data: "{{ state.generated_json }}"
    evaluator_type: schema
    schema:
      type: object
      required: [name, email]
  output: eval_result
```

**Returns:** `{valid: bool, score: float, errors: [...], suggestions: [...]}`

### `reflection.correct`

Standalone correction action:

```yaml
- name: correct_output
  uses: reflection.correct
  with:
    data: "{{ state.reflection_output }}"
    errors: "{{ state.reflection_errors }}"
    run: |
      result = {**state['reflection_output'], 'email': 'fixed@example.com'}
  output: corrected
```

**Returns:** `{corrected_output: any, success: bool}`

### Evaluator Types

#### Schema Evaluator

Uses JSON Schema validation with automatic type coercion:

```yaml
evaluator:
  type: schema
  schema:
    type: object
    required: [name, email]
    properties:
      name: { type: string, minLength: 1 }
      email: { type: string, format: email }
```

Supports `$ref` for external schema files:

```yaml
evaluator:
  type: schema
  schema:
    $ref: "./schemas/user.json"
```

#### LLM Evaluator

Uses an LLM as a judge:

```yaml
evaluator:
  type: llm
  prompt: |
    Evaluate if this user profile is complete and valid:
    {{ state.reflection_output | tojson }}

    Respond with JSON: {"valid": true/false, "score": 0.0-1.0, "reason": "...", "suggestions": [...]}
  model: "gpt-4"
  examples:                            # Optional few-shot examples
    - input: '{"name": "Alice"}'
      output: '{"valid": false, "score": 0.3, "reason": "Missing email"}'
    - input: '{"name": "Alice", "email": "a@b.com"}'
      output: '{"valid": true, "score": 1.0, "reason": "Complete profile"}'
```

#### Custom Evaluator

Inline Python/Lua code:

```yaml
evaluator:
  type: custom
  language: python                     # python | lua | prolog
  run: |
    # 'output' contains the generator output
    # 'state' contains workflow state
    if len(output.get('name', '')) > 0 and '@' in output.get('email', ''):
        result = {'valid': True, 'score': 1.0, 'errors': []}
    else:
        result = {'valid': False, 'score': 0.0, 'errors': [{'message': 'Invalid profile'}]}
```

### On-Failure Strategies

| Strategy | Description |
|----------|-------------|
| `return_best` | Return the highest-scoring attempt (default) |
| `return_last` | Return the final attempt regardless of score |
| `raise` | Raise `ReflectionFailedError` with full history |

### State Variables

Variables set during the reflection loop:

| Variable | Type | Description |
|----------|------|-------------|
| `reflection_iteration` | int | Current iteration (1-based) |
| `reflection_output` | any | Current generator output |
| `reflection_errors` | list | Errors from current evaluation |
| `reflection_history` | list | All attempts with outputs/scores |
| `reflection_best` | any | Best output so far |
| `reflection_best_score` | float | Score of best output |

### Complete Example

JSON generation agent with schema validation:

```yaml
name: json-generation-agent
description: Generate valid JSON with automatic correction

state_schema:
  request: str
  result: object

nodes:
  - name: generate_json
    uses: reflection.loop
    with:
      generator:
        action: llm.call
        model: "ollama/gemma3:4b"
        messages:
          - role: system
            content: "Generate a JSON user object with name and email fields."
          - role: user
            content: "{{ state.request }}"
      evaluator:
        type: schema
        schema:
          type: object
          required: [name, email]
          properties:
            name: { type: string }
            email: { type: string }
      corrector:
        action: llm.call
        model: "ollama/gemma3:4b"
        messages:
          - role: system
            content: "Fix JSON validation errors."
          - role: user
            content: |
              Original: {{ state.reflection_output | tojson }}
              Errors: {{ state.reflection_errors | tojson }}
      max_iterations: 3
      on_failure: return_best
    output: result

edges:
  - from: __start__
    to: generate_json
  - from: generate_json
    to: __end__
```

---

## Rate Limiting Actions

Enforce rate limits across parallel nodes using named limiters. Prevents API throttling when making concurrent calls to rate-limited services (LLM providers, web APIs).

**Key Features:**
- Named limiters shared across parallel branches
- RPM (requests per minute) or RPS (requests per second) configuration
- Thread-safe implementation with proper timing
- Response metadata (wait time, limiter name)
- Timeout support for bounded waiting
- Composable with `cache.wrap` for cache-before-ratelimit optimization

### `ratelimit.wrap`

Wrap any action with rate limiting:

```yaml
# Basic rate limiting
- name: call_api
  uses: ratelimit.wrap
  with:
    action: llm.call
    limiter: openai                          # Named limiter (shared across nodes)
    rpm: 60                                  # 60 requests per minute = 1 req/sec
    args:
      model: gpt-4
      messages:
        - role: user
          content: "{{ state.prompt }}"
  output: api_result

# Multiple parallel nodes share the same limiter
# Thread 1: wait(0s) → execute
# Thread 2: wait(1s) → execute
# Thread 3: wait(2s) → execute
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `action` | string | Yes | - | Action to wrap (e.g., `llm.call`, `http.get`) |
| `args` | dict | Yes | - | Arguments to pass to wrapped action |
| `limiter` | string | Yes | - | Name of the rate limiter (shared across nodes) |
| `rpm` | float | No | - | Requests per minute limit |
| `rps` | float | No | - | Requests per second limit (takes precedence over rpm) |
| `timeout` | float | No | - | Maximum wait time in seconds. Returns error if exceeded. |

**Returns:**
```json
{
  "success": true,
  "result": {...},              // Wrapped action result
  "_ratelimit_waited_ms": 1000, // Time spent waiting in milliseconds
  "_ratelimit_limiter": "openai" // Name of the limiter used
}
```

**Timeout Error:**
```json
{
  "success": false,
  "error": "Rate limit wait would exceed timeout (5s). Wait required: 10.00s",
  "error_type": "ratelimit_timeout",
  "_ratelimit_waited_ms": 10000,
  "_ratelimit_limiter": "openai"
}
```

### Parallel Rate Limiting

When using parallel fan-out, all nodes sharing the same limiter name will coordinate:

```yaml
name: parallel_rate_limited

edges:
  - from: start
    to: [query_1, query_2, query_3]
  - from: [query_1, query_2, query_3]
    to: aggregate

nodes:
  - name: query_1
    uses: ratelimit.wrap
    with:
      action: llm.call
      limiter: openai              # All 3 share this limiter
      rpm: 60
      args:
        model: gpt-4
        messages: [{ role: user, content: "{{ state.q1 }}" }]

  - name: query_2
    uses: ratelimit.wrap
    with:
      action: llm.call
      limiter: openai              # Same limiter = sequential at 1 req/sec
      rpm: 60
      args:
        model: gpt-4
        messages: [{ role: user, content: "{{ state.q2 }}" }]

  - name: query_3
    uses: ratelimit.wrap
    with:
      action: llm.call
      limiter: openai
      rpm: 60
      args:
        model: gpt-4
        messages: [{ role: user, content: "{{ state.q3 }}" }]
```

### Cache + Rate Limit Composition

Wrap rate limiting inside cache for optimal performance (cache hit skips rate limit):

```yaml
# OPTIMIZED: Cache checks BEFORE rate limit
# Cache hit = no rate limit token consumed
- name: smart_call
  uses: cache.wrap
  with:
    action: ratelimit.wrap
    key_strategy: args
    ttl_days: 7
    args:
      action: llm.call
      limiter: openai
      rpm: 60
      args:
        model: gpt-4
        messages: "{{ state.messages }}"
  output: result

# Flow:
# 1. cache.wrap checks cache
# 2. Cache HIT? → Returns result (no rate limit consumed!)
# 3. Cache MISS? → ratelimit.wrap.wait() → llm.call → cache.store
```

### Settings-Based Configuration

Pre-configure rate limiters in settings for multiple providers:

```yaml
name: multi_provider_agent

settings:
  rate_limiters:
    openai:
      rpm: 60                      # 60 requests per minute
    anthropic:
      rpm: 40                      # 40 requests per minute
    local_llm:
      rps: 10                      # 10 requests per second

nodes:
  - name: call_openai
    uses: ratelimit.wrap
    with:
      action: llm.call
      limiter: openai              # Uses settings: 60 rpm
      args:
        model: gpt-4
        messages: "{{ state.messages }}"

  - name: call_claude
    uses: ratelimit.wrap
    with:
      action: llm.call
      limiter: anthropic           # Uses settings: 40 rpm
      args:
        model: claude-3
        messages: "{{ state.messages }}"
```

**Notes:**
- Limiter configuration is "first-config-wins": if a limiter is already configured (either from settings or a previous node), subsequent configurations with different values log a warning but reuse the existing limiter
- Rate limiters are stored at the engine level and survive across node executions
- Thread-safe: uses `threading.Lock` internally for correct timing across concurrent calls

---

## Secrets Actions

Access secrets from cloud providers (AWS Secrets Manager, Azure Key Vault, GCP Secret Manager) or environment variables through a unified interface.

> **Epic:** [TEA-BUILTIN-012](../../../stories/TEA-BUILTIN-012-secrets-backend-epic.md)
> **Status:** Implemented (Python-only)

### `secrets.get`

Retrieve a secret value by key:

```yaml
- name: get_api_key
  uses: secrets.get
  with:
    key: API_KEY                    # Required: secret key name
    default: null                   # Optional: fallback value if not found
  output: api_key
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `key` | string | Yes | Secret key to retrieve |
| `default` | any | No | Default value if secret not found |

**Returns:**
```python
{
  "value": str | any  # The secret value, or default if not found
}
```

Access via `output`: `{{ state.api_key.value }}` or use Jinja2 filter `{{ state.api_key | first }}`

**Example with template syntax:**

```yaml
# Alternative: use template syntax directly
- name: call_api
  uses: http.get
  with:
    url: "{{ variables.api_url }}"
    headers:
      Authorization: "Bearer {{ secrets.API_KEY }}"
```

### `secrets.has`

Check if a secret exists:

```yaml
- name: check_secret
  uses: secrets.has
  with:
    key: OPTIONAL_FEATURE_KEY       # Required: secret key to check
  output: has_feature_key
```

**Returns:**
```python
{
  "exists": bool  # True if secret exists, False otherwise
}
```

**Example with conditional logic:**

```yaml
- name: check_feature
  uses: secrets.has
  with:
    key: PREMIUM_API_KEY
  output: has_premium

- name: use_premium_api
  when: "{{ state.has_premium.exists }}"
  uses: http.get
  with:
    url: "{{ variables.premium_api_url }}"
    headers:
      Authorization: "Bearer {{ secrets.PREMIUM_API_KEY }}"
```

### Secrets Configuration

Configure secrets backends in the `settings.secrets` section:

```yaml
settings:
  secrets:
    backend: aws                    # aws | azure | gcp | vault | env (default)

    # Environment variables (default backend)
    env:
      prefix: MYAPP_                # Optional: filter env vars by prefix

    # AWS Secrets Manager
    aws:
      region: us-east-1
      secret_name: myapp/production # Single JSON secret with multiple keys
      # OR
      secret_prefix: myapp/         # Multiple secrets by prefix

    # Azure Key Vault
    azure:
      vault_url: https://myvault.vault.azure.net/

    # GCP Secret Manager
    gcp:
      project_id: my-project
      secret_prefix: myapp-         # Optional: filter by prefix

    # HashiCorp Vault (via Dynaconf)
    vault:
      url: https://vault.example.com
      token: ${VAULT_TOKEN}
      mount: secret
```

**Backend Selection:**

| Backend | Provider | Install Extra |
|---------|----------|---------------|
| `env` | Environment variables | Built-in (default) |
| `aws` | AWS Secrets Manager | `pip install the-edge-agent[aws]` |
| `azure` | Azure Key Vault | `pip install the-edge-agent[azure]` |
| `gcp` | Google Secret Manager | `pip install the-edge-agent[gcp]` |
| `vault` | HashiCorp Vault | `pip install the-edge-agent[secrets]` |

### Cloud Provider Backends

#### AWS Secrets Manager

```yaml
settings:
  secrets:
    backend: aws
    aws:
      region: us-east-1
      # Option 1: Single JSON secret containing multiple keys
      secret_name: myapp/production
      # Option 2: Multiple secrets with common prefix
      # secret_prefix: myapp/
```

**Authentication:** Uses AWS default credential chain (environment variables, IAM role, instance profile).

#### Azure Key Vault

```yaml
settings:
  secrets:
    backend: azure
    azure:
      vault_url: https://myvault.vault.azure.net/
```

**Authentication:** Uses `DefaultAzureCredential` (environment variables, managed identity, Azure CLI).

#### GCP Secret Manager

```yaml
settings:
  secrets:
    backend: gcp
    gcp:
      project_id: my-gcp-project
      secret_prefix: myapp-         # Optional filter
```

**Authentication:** Uses Application Default Credentials (ADC).

**Security Notes:**
- Secrets are loaded at engine initialization and cached in memory
- Secrets are **never** serialized to checkpoints
- Use provider's default credential chain for production (avoid hardcoded tokens)
- Template syntax `{{ secrets.KEY }}` is evaluated at runtime

---

## HTTP Response Actions

> **TEA-BUILTIN-015.5**: HTTP response actions for early termination with custom status/body/headers.

### `http.respond`

Send a custom HTTP response and terminate graph execution immediately:

```yaml
- name: unauthorized_response
  uses: http.respond
  with:
    status: 401
    body:
      error: "unauthorized"
      message: "Invalid token"
    headers:
      WWW-Authenticate: "Bearer"
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `status` | int | No | 200 | HTTP status code |
| `body` | any | No | null | Response body (dict/list/string) |
| `headers` | dict | No | {} | HTTP headers |
| `content_type` | string | No | "application/json" | Content-Type header |

**Behavior:**

- **Terminates execution immediately** - no further nodes are executed
- Yields an event with `type: "http_response"` containing status, body, and headers
- Headers are automatically merged with Content-Type

**Example: Conditional Early Return**

```yaml
nodes:
  - name: check_auth
    run: |
      return {"authorized": check_token(state.get("token"))}
    goto:
      - to: error_response
        if: "not state.authorized"
      - to: process

  - name: error_response
    uses: http.respond
    with:
      status: 401
      body:
        error: "unauthorized"
        message: "Please provide a valid token"

  - name: process
    run: |
      return {"result": "processed"}
    goto: __end__
```

---

## Response Transformation

> **TEA-BUILTIN-015.5**: Transform internal state to structured API responses using `output_schema`.

### `output_schema`

Define the structure of your API response by mapping state fields using Jinja2 templates:

```yaml
name: my_agent

state_schema:
  input: str
  result: str
  error_msg: str
  has_error: bool

output_schema:
  success: "{{ not state.has_error }}"
  data:
    answer: "{{ state.result }}"
    query: "{{ state.input }}"
  error:
    value: "{{ state.error_msg }}"
    include_if: "state.has_error"
```

When the graph completes, the final event includes:
- `state`: The full internal state
- `output`: The transformed response (if `output_schema` is defined)

### Field Mapping

Fields in `output_schema` can be:

**Static values:**
```yaml
output_schema:
  version: "1.0"
  success: true
  count: 42
```

**Template strings:**
```yaml
output_schema:
  answer: "{{ state.result }}"
  greeting: "Hello, {{ state.user.name }}!"
  upper_name: "{{ state.name | upper }}"
```

**Nested structures:**
```yaml
output_schema:
  data:
    user:
      name: "{{ state.user.name }}"
      email: "{{ state.user.email }}"
    metadata:
      timestamp: "{{ now() }}"
```

### Conditional Fields

Use `include_if` to conditionally include fields based on state:

```yaml
output_schema:
  # Always included
  success: "{{ not state.has_error }}"
  result: "{{ state.data }}"

  # Only included when condition is true
  error:
    value: "{{ state.error_msg }}"
    include_if: "state.has_error"

  # Premium content for authorized users
  premium_data:
    value: "{{ state.secret_content }}"
    include_if: "state.user.is_premium"
```

### Default Values

Provide fallback values when templates evaluate to None or empty:

```yaml
output_schema:
  result:
    value: "{{ state.maybe_missing }}"
    default: "No data available"

  count:
    value: "{{ state.item_count }}"
    default: 0

  metadata:
    value: "{{ state.meta }}"
    default:
      version: "unknown"
      timestamp: null
```

### Complete Example

```yaml
name: api-handler
state_schema:
  query: str
  response: str
  tokens_used: int
  cached: bool

output_schema:
  success: true
  data:
    answer: "{{ state.response }}"
    cached: "{{ state.cached }}"
  usage:
    value:
      tokens: "{{ state.tokens_used }}"
    include_if: "state.tokens_used"
  debug:
    value: "{{ state.debug_info }}"
    include_if: "state.get('debug_mode', false)"
    default: null

nodes:
  - name: process
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: user
          content: "{{ state.query }}"
    output: response

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
```

**Result:**
```json
{
  "success": true,
  "data": {
    "answer": "The answer to your question is...",
    "cached": false
  },
  "usage": {
    "tokens": 150
  }
}
```

---

## Custom Actions

Register custom actions in Python:

```python
def my_custom_action(state, param1, param2, **kwargs):
    result = do_something(param1, param2)
    return {"result": result}

engine = YAMLEngine(actions_registry={
    "custom.my_action": my_custom_action
})
```

Use in YAML:

```yaml
- name: custom_step
  uses: custom.my_action
  with:
    param1: value1
    param2: "{{ state.dynamic_value }}"
  output: custom_result
```

**Action Signature:**

```python
def action_name(
    state: Dict[str, Any],      # Current workflow state
    **kwargs                     # Parameters from `with:` block
) -> Dict[str, Any]:            # Return values merged into state
    ...
```

**Best Practices:**
- Always return a dictionary
- Use descriptive action names with namespace prefix (`custom.`, `myapp.`)
- Access state values, don't modify state directly
- Raise `ValueError` for validation errors
- Return `{"success": false, "error": str}` for recoverable errors

---

## Server Endpoints

> **Story:** TEA-BUILTIN-015.8 (Health & Metadata Endpoints)

Auto-generated server endpoints for health checks, metrics, and agent metadata.
Configure via `settings.server` in your YAML.

### Settings Schema

```yaml
settings:
  server:
    # Enable/disable endpoints
    health_endpoint: true         # /health - Kubernetes liveness probe
    readiness_endpoint: true      # /ready - Kubernetes readiness probe
    list_agents: true             # /agents - List available agents
    metrics: false                # /metrics - Prometheus metrics (disabled by default)
    openapi: true                 # /openapi.json - OpenAPI specification

    # Custom paths (optional)
    paths:
      health: "/healthz"          # Override default path
      ready: "/readyz"
      agents: "/api/agents"
      metrics: "/api/metrics"
      openapi: "/api/openapi.json"

    # Service info
    service_name: "tea-agents"
    version: "1.0.0"

    # Custom health checks
    health_checks:
      - name: "database"
        type: firestore           # Built-in check type
      - name: "llm"
        type: http
        url: "${LLM_HEALTH_URL}"
        timeout: 5
```

### Health Endpoint

Simple liveness check. Returns 200 if service is running.

**Response:**
```json
{
  "status": "healthy",
  "service": "tea-agents",
  "version": "1.0.0",
  "timestamp": "2025-01-05T12:00:00Z"
}
```

### Readiness Endpoint

Checks configured dependencies. Returns 200 if all ready, 503 if any fail.

**Success Response (200):**
```json
{
  "status": "ready",
  "checks": {
    "firestore": {"status": "ok", "latency_ms": 12},
    "llm": {"status": "ok", "latency_ms": 45}
  },
  "timestamp": "2025-01-05T12:00:00Z"
}
```

**Failure Response (503):**
```json
{
  "status": "not_ready",
  "checks": {
    "firestore": {"status": "ok"},
    "llm": {"status": "error", "error": "Connection timeout"}
  }
}
```

### Agents List Endpoint

Lists all available agents with their endpoints.

**Response:**
```json
{
  "agents": [
    {
      "name": "research_agent",
      "description": "Research agent with web search",
      "endpoint": "/api/v1/research",
      "method": "POST"
    }
  ],
  "count": 1
}
```

### Metrics Endpoint

Prometheus-format metrics for agent executions.

**Response (text/plain):**
```
# HELP tea_agent_executions_total Total agent executions
# TYPE tea_agent_executions_total counter
tea_agent_executions_total{agent="research",status="success"} 1542
tea_agent_executions_total{agent="research",status="error"} 23

# HELP tea_agent_duration_seconds Agent execution duration
# TYPE tea_agent_duration_seconds summary
tea_agent_duration_seconds_sum{agent="research"} 2341.5
tea_agent_duration_seconds_count{agent="research"} 1542

# HELP tea_agent_errors_total Total agent errors by type
# TYPE tea_agent_errors_total counter
tea_agent_errors_total{agent="research",error_type="timeout"} 15
```

### OpenAPI Endpoint

Returns OpenAPI 3.0 specification aggregating all agent endpoints.

**Response:**
```json
{
  "openapi": "3.0.3",
  "info": {"title": "tea-agents", "version": "1.0.0"},
  "paths": {
    "/api/v1/research": {
      "post": {
        "summary": "Research agent",
        "requestBody": {...},
        "responses": {...}
      }
    }
  }
}
```

### Custom Health Checks

Register custom health check functions:

```python
from the_edge_agent import YAMLEngine
from the_edge_agent.http import register_health_check

engine = YAMLEngine()

# Sync health check
@register_health_check("custom_db")
def check_database():
    return db.ping()

# Async health check
@register_health_check("external_api")
async def check_external_api():
    response = await http_client.get("http://api/health")
    return response.status_code == 200
```

### Kubernetes Probe Configuration

Example Kubernetes deployment using TEA health endpoints:

```yaml
# kubernetes/deployment.yaml
spec:
  containers:
    - name: tea-agents
      livenessProbe:
        httpGet:
          path: /health
          port: 8080
        initialDelaySeconds: 10
        periodSeconds: 15

      readinessProbe:
        httpGet:
          path: /ready
          port: 8080
        initialDelaySeconds: 5
        periodSeconds: 10
```

---

## Dual Namespace

All specialized actions are available via dual namespaces:
- `checkpoint.*` and `actions.checkpoint_*`
- `schema.*` and `actions.schema_*`
- `llamaextract.*` and `actions.llamaextract_*`
- `validate.*` and `actions.validate_*`
- `retry.*` and `actions.retry_*`
- `ratelimit.*` and `actions.ratelimit_*`

---

## See Also

- [Actions Overview](./README.md)
- [LLM Actions](./llm.md) - LLM calls for semantic probes
- [Memory Actions](./memory.md) - Persistent state storage
- [Advanced Runtimes](../advanced-runtimes.md) - Prolog for validation constraints
