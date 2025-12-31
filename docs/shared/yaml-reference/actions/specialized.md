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
  - [validate.extraction](#validateextraction)
  - [validate.generate_prompt](#validategenerate_prompt)
  - [Extraction Schema](#extraction-schema)
  - [Validation Constraints](#validation-constraints)
  - [Semantic Probes](#semantic-probes)
  - [Validation Logging](#validation-logging)
- [Retry Actions](#retry-actions)
  - [retry.loop](#retryloop)
- [Custom Actions](#custom-actions)

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

Actions for validating LLM-extracted data using structural schemas, Prolog constraints, and semantic probes.

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

## Dual Namespace

All specialized actions are available via dual namespaces:
- `checkpoint.*` and `actions.checkpoint_*`
- `schema.*` and `actions.schema_*`
- `llamaextract.*` and `actions.llamaextract_*`
- `validate.*` and `actions.validate_*`
- `retry.*` and `actions.retry_*`

---

## See Also

- [Actions Overview](./README.md)
- [LLM Actions](./llm.md) - LLM calls for semantic probes
- [Memory Actions](./memory.md) - Persistent state storage
- [Advanced Runtimes](../advanced-runtimes.md) - Prolog for validation constraints
