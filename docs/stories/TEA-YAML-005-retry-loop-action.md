# Story TEA-YAML-005: General-Purpose Retry Loop Action

## Status

Ready

## Story

**As a** YAML agent author,
**I want** a general-purpose `retry.loop` action that can wrap any validation and retry with corrections,
**so that** I can build self-correcting agents without writing custom retry logic.

## Context

### Problem Statement

Currently, when extraction validation fails, the agent must fail or the author must manually implement retry logic using complex edge conditions. This leads to:

1. **Repeated boilerplate**: Every agent needing retry implements similar patterns
2. **Complex edge conditions**: Retry count tracking in state, conditional edges
3. **Error-prone**: Easy to create infinite loops or miss edge cases
4. **No standardization**: Different agents implement retry differently

### Proposed Solution

A new `retry.loop` action that:
- Wraps any validation action
- Automatically routes to a correction node on failure
- Tracks retry count
- Exits loop on success or max retries
- Provides error context to correction nodes

### Use Cases

1. **Extraction Validation Retry**: LLM extracts → validate → on failure, ask LLM to correct
2. **Schema Compliance Retry**: Generate JSON → validate schema → on failure, fix format
3. **Code Generation Retry**: Generate code → run tests → on failure, fix errors

### Example YAML Syntax

```yaml
nodes:
  - name: extract
    uses: llm.call
    with:
      messages:
        - role: user
          content: "Extract entities from: {{ state.text }}"
    output: extraction

  - name: validate_with_retry
    uses: retry.loop
    with:
      validate: validate.extraction
      validate_args:
        entities: "{{ state.entities }}"
        relationships: "{{ state.relationships }}"
      correct: correct_extraction
      max_retries: 2
      retry_delay: 0  # Optional delay between retries

  - name: correct_extraction
    uses: llm.call
    with:
      messages:
        - role: user
          content: |
            The extraction had errors:
            {% for error in state._retry_errors %}
            - {{ error.message }}
            {% endfor %}

            Original extraction:
            {{ state.entities | tojson }}

            Please fix these issues and re-extract.
    output: extraction

edges:
  - from: extract
    to: validate_with_retry
  - from: validate_with_retry
    to: process_result
    condition: "{{ state._retry_result.valid }}"
  - from: validate_with_retry
    to: handle_failure
    condition: "{{ not state._retry_result.valid }}"
```

### State Variables Set by `retry.loop`

| Variable | Type | Description |
|----------|------|-------------|
| `_retry_count` | int | Current retry attempt (0-indexed) |
| `_retry_errors` | list | Errors from last validation attempt |
| `_retry_result` | dict | Final validation result |
| `_retry_exhausted` | bool | True if max retries reached without success |

## Acceptance Criteria

### Core Functionality

1. New `retry.loop` action registered in action registry
2. `validate` parameter specifies which validation action to call
3. `validate_args` parameter passes arguments to validation action
4. `correct` parameter specifies node to route to on failure
5. `max_retries` parameter controls maximum correction attempts (default: 1)

### State Management

6. `_retry_count` incremented on each retry attempt
7. `_retry_errors` populated with validation errors on failure
8. `_retry_result` contains final validation result (success or last failure)
9. `_retry_exhausted` set to true when max retries exceeded

### Loop Control

10. Loop exits immediately when validation succeeds
11. Loop exits when `max_retries` exceeded
12. Loop routes to `correct` node between retries
13. After correction node, re-runs validation automatically

### Error Handling

14. If `correct` node fails, loop exits with error
15. If validation action not found, fails with clear error
16. Invalid `max_retries` (negative, non-integer) fails at load time

### Integration

17. Works with `validate.extraction` action from TEA-YAML-004
18. Works with any action that returns `{ "valid": bool, "errors": list }`
19. Correction node has full access to `_retry_errors` for context
20. Compatible with existing edge conditions

### Backward Compatibility

21. Existing agents without `retry.loop` work unchanged
22. Manual retry patterns using edges still work
23. No changes to existing validation actions required

## Tasks / Subtasks

### Phase 1: Core Action Implementation

- [ ] Task 1: Create `retry.loop` action skeleton (AC: 1)
  - [ ] Add `retry_actions.py` to `actions/` directory
  - [ ] Register `retry.loop` action in registry
  - [ ] Define action signature with all parameters

- [ ] Task 2: Implement validation wrapper (AC: 2-4)
  - [ ] Look up validation action from registry by name
  - [ ] Call validation action with provided args
  - [ ] Capture validation result

- [ ] Task 3: Implement loop control (AC: 10-13)
  - [ ] Check validation result, exit on success
  - [ ] Track retry count, exit on max exceeded
  - [ ] Route to correction node on failure
  - [ ] Re-run validation after correction

### Phase 2: State Management

- [ ] Task 4: Implement state variables (AC: 6-9)
  - [ ] Set `_retry_count` before each attempt
  - [ ] Set `_retry_errors` from validation result
  - [ ] Set `_retry_result` with final outcome
  - [ ] Set `_retry_exhausted` when limit reached

### Phase 3: Error Handling

- [ ] Task 5: Add error handling (AC: 14-16)
  - [ ] Handle correction node failures gracefully
  - [ ] Validate action name at runtime
  - [ ] Validate `max_retries` at load time

### Phase 4: Integration & Testing

- [ ] Task 6: Integration with validate.extraction (AC: 17-20)
  - [ ] Test with `validate.extraction` action
  - [ ] Verify error context available in correction node
  - [ ] Test edge conditions work correctly

- [ ] Task 7: Backward compatibility verification (AC: 21-23)
  - [ ] Verify existing agents work unchanged
  - [ ] Test manual retry patterns still work
  - [ ] No regressions in validation actions

- [ ] Task 8: Create example agent
  - [ ] Create `examples/yaml/extraction_with_retry.yaml`
  - [ ] Demonstrate all features
  - [ ] Include documentation comments

## Dev Notes

### Relevant Source Tree

```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py             # Register retry.loop action
│   ├── retry_actions.py        # NEW: retry.loop implementation
│   ├── validation_actions.py   # validate.extraction to wrap
│   └── llm_actions.py          # LLM call for correction nodes
├── yaml_engine.py              # Node execution, edge routing
└── extraction_validation.py    # Validation result format reference
```

### Key Design Decisions

1. **Action-based, not node-based**: `retry.loop` is an action that internally manages a sub-workflow, not a new node type. This keeps the graph model simple.

2. **Correction node is a regular node**: The `correct` parameter references an existing node by name. This node runs in the normal graph context with full access to state.

3. **State prefix convention**: All retry state variables start with `_retry_` to avoid collisions with user state.

4. **Validation result contract**: Any action returning `{ "valid": bool, "errors": list }` can be used with `retry.loop`. This makes it general-purpose.

### Implementation Approach

The `retry.loop` action should:

```python
def retry_loop_action(
    state: Dict[str, Any],
    validate: str,           # Action name, e.g., "validate.extraction"
    validate_args: Dict,     # Arguments to pass to validation
    correct: str,            # Node name to run on failure
    max_retries: int = 1,
    retry_delay: float = 0,
    **kwargs
) -> Dict[str, Any]:
    """
    Execute validation with retry loop.

    1. Call validation action with validate_args
    2. If valid, return success
    3. If invalid and retries remaining:
       a. Set _retry_errors in state
       b. Execute correction node
       c. Increment _retry_count
       d. Go to step 1
    4. If invalid and no retries, return failure
    """
```

### Correction Node Execution

The tricky part is executing a node from within an action. Options:

**Option A: Return routing instruction**
```python
# Action returns special routing instruction
return {
    "_route_to": "correct_extraction",
    "_then_retry": True,
    "_retry_count": 1
}
# YAML engine handles the routing
```

**Option B: Direct node execution**
```python
# Action calls engine to execute node directly
correction_result = engine.execute_node("correct_extraction", state)
# Then re-validate
```

**Recommendation**: Option A is cleaner - the action sets up state and returns a routing hint, letting the YAML engine handle actual node execution. This maintains separation of concerns.

### Alternative: Edge-Based Pattern (Simpler)

If implementing within the action is too complex, document an edge-based pattern:

```yaml
nodes:
  - name: validate
    uses: validate.extraction
    # ...

  - name: check_retry
    run: |
      count = state.get("_retry_count", 0)
      valid = state.get("validation_result", {}).get("valid", False)
      exhausted = count >= 2
      return {
        "_should_retry": not valid and not exhausted,
        "_retry_exhausted": not valid and exhausted,
        "_retry_count": count
      }

  - name: correct
    uses: llm.call
    # ... correction logic
    run: |
      return {"_retry_count": state.get("_retry_count", 0) + 1}

edges:
  - from: validate
    to: check_retry
  - from: check_retry
    to: process
    condition: "{{ state.validation_result.valid }}"
  - from: check_retry
    to: correct
    condition: "{{ state._should_retry }}"
  - from: check_retry
    to: fail
    condition: "{{ state._retry_exhausted }}"
  - from: correct
    to: validate  # Loop back
```

This pattern requires no new action code but is more verbose. The `retry.loop` action is syntactic sugar over this pattern.

## Testing

### Test File Location

`python/tests/test_retry_actions.py`

### Testing Standards

- Use `pytest` with existing fixtures
- Mock validation actions for controlled pass/fail
- Test loop termination conditions
- Test state variable lifecycle

### Key Test Scenarios

1. **Immediate success**: Validation passes first try → no retry
2. **Success after retry**: Fail once, correct, succeed → count = 1
3. **Max retries exceeded**: Fail all attempts → exhausted = true
4. **Correction node failure**: Correction raises → loop exits with error
5. **Invalid action name**: Unknown validation action → clear error
6. **State preservation**: User state not clobbered by _retry_* vars
7. **Integration**: Full flow with `validate.extraction` and mock LLM

## Dependencies

- **Requires TEA-YAML-004**: Generic Extraction Validation (already done)
- **Enables TEA-YAML-004a**: LLM Call Integration Fix will use this as context

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-25 | 0.1 | Initial draft - general-purpose retry loop action | Sarah (PO) |

## Dev Agent Record

### Agent Model Used

_To be filled during implementation_

### Debug Log References

_To be filled during implementation_

### Completion Notes List

_To be filled during implementation_

### File List

_To be filled during implementation_

## QA Results

_To be filled after implementation_
