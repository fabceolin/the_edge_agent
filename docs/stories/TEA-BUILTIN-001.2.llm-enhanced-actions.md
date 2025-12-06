# Story TEA-BUILTIN-001.2: LLM Enhanced Actions

## Status

Draft

## Story

**As a** YAML agent developer,
**I want** enhanced LLM actions (streaming, retry with backoff, tool calling),
**so that** I can build production-grade agents with resilience and advanced capabilities without writing Python code.

## Acceptance Criteria

1. `llm.stream` action streams LLM responses token-by-token via generator
2. `llm.retry` action wraps LLM calls with exponential backoff and configurable retry logic
3. `llm.tools` action supports OpenAI function/tool calling with automatic dispatch
4. Streaming integrates with existing `stream()` method yielding partial responses
5. Retry logic handles rate limits (429), timeouts, and transient errors
6. Tool definitions can be specified in YAML with automatic schema generation
7. All actions follow existing `_setup_builtin_actions()` pattern
8. Actions are accessible via both `llm.*` and `actions.llm_*` namespaces
9. Comprehensive unit tests cover all LLM enhanced operations
10. Documentation updated in CLAUDE.md and docs/YAML_AGENTS.md

## Dependencies

**Blocked By**: None (can start immediately, extends existing `llm.call`)

**Blocks**:
- TEA-BUILTIN-001.1 (Memory Actions) - `memory.summarize` can use `llm.retry` for resilience
- TEA-BUILTIN-002.3 (Tools Bridge) - `llm.tools` establishes action dispatch pattern

**Internal Dependencies**:
- Extends existing `llm.call` action (must remain backward compatible)

## User Prerequisites

- [ ] **Required**: Obtain `OPENAI_API_KEY` from https://platform.openai.com
- [ ] **Required**: OpenAI library >=1.0.0 installed (`pip install openai>=1.0.0`)

## Tasks / Subtasks

- [ ] Task 1: Implement `llm.stream` action (AC: 1, 4, 7, 8)
  - [ ] Define function signature: `llm_stream(state, model, messages, temperature=0.7, **kwargs)`
  - [ ] Use OpenAI streaming API (`stream=True`)
  - [ ] Yield partial content chunks as they arrive
  - [ ] Integrate with StateGraph `stream()` to emit intermediate events
  - [ ] Return final `{"content": str, "usage": dict, "streamed": True}`
  - [ ] Register in actions dict with both namespaces

- [ ] Task 2: Implement `llm.retry` action (AC: 2, 5, 7, 8)
  - [ ] Define function signature: `llm_retry(state, model, messages, max_retries=3, base_delay=1.0, max_delay=60.0, **kwargs)`
  - [ ] Implement exponential backoff: `delay = min(base_delay * 2^attempt, max_delay)`
  - [ ] Handle HTTP 429 (rate limit) with `Retry-After` header parsing
  - [ ] Handle timeout errors with retry
  - [ ] Handle transient 5xx errors with retry
  - [ ] Fail fast on 4xx errors (except 429)
  - [ ] Return `{"content": str, "usage": dict, "attempts": int, "total_delay": float}`
  - [ ] Register in actions dict with both namespaces

- [ ] Task 3: Implement `llm.tools` action (AC: 3, 6, 7, 8)
  - [ ] Define function signature: `llm_tools(state, model, messages, tools, tool_choice="auto", **kwargs)`
  - [ ] Accept tools as list of YAML-defined tool specs
  - [ ] Convert YAML tool specs to OpenAI function schema
  - [ ] Execute tool calls by dispatching to registered actions
  - [ ] Support multi-turn tool use (call → result → continue)
  - [ ] Return `{"content": str, "tool_calls": list, "tool_results": list}`
  - [ ] Register in actions dict with both namespaces

- [ ] Task 4: YAML tool definition schema (AC: 6)
  - [ ] Design YAML schema for tool definitions:
    ```yaml
    tools:
      - name: search_web
        description: Search the web for information
        parameters:
          query:
            type: string
            description: Search query
            required: true
        action: web.search  # Maps to registered action
    ```
  - [ ] Implement schema converter to OpenAI format
  - [ ] Validate tool definitions at load time

- [ ] Task 5: Streaming integration with StateGraph (AC: 4)
  - [ ] Modify `stream()` to handle generator-returning actions
  - [ ] Emit `{"type": "chunk", "content": str, "node": str}` events
  - [ ] Ensure final state update happens after stream completes
  - [ ] Handle streaming errors gracefully

- [ ] Task 6: Write tests (AC: 9)
  - [ ] Test llm.stream with mock streaming response
  - [ ] Test llm.retry with simulated failures and recovery
  - [ ] Test llm.retry respects Retry-After header
  - [ ] Test llm.tools with mock tool calls
  - [ ] Test tool dispatch to registered actions
  - [ ] Test error handling for each action

- [ ] Task 7: Update documentation (AC: 10)
  - [ ] Add LLM enhanced actions to CLAUDE.md
  - [ ] Add tool definition schema to docs/YAML_AGENTS.md
  - [ ] Create example YAML showing agent with tools

## Dev Notes

### Integration Points
- **File**: `src/the_edge_agent/yaml_engine.py`
- **Method**: `_setup_builtin_actions()` (lines 623-786)
- **Existing**: `llm.call` at line 628-648 (enhance, don't replace)

### Existing LLM Pattern
```python
def llm_call(state, model, messages, temperature=0.7, **kwargs):
    from openai import OpenAI
    client = OpenAI()
    response = client.chat.completions.create(
        model=model, messages=messages, temperature=temperature
    )
    return {
        'content': response.choices[0].message.content,
        'usage': response.usage.model_dump() if hasattr(response.usage, 'model_dump') else {}
    }
```

### Key Constraints
- Streaming requires generator support in action execution path
- Retry delays should not block other graph execution (consider async future)
- Tool dispatch must handle missing/invalid action references gracefully
- OpenAI client instantiation should be shared (avoid repeated auth)

### Technical Dependencies
- Extends existing `llm.call` action
- `llm.tools` may dispatch to any registered action (including custom ones)
- Requires OpenAI library >=1.0 for streaming and tools APIs

### Version Requirements
- Python: >=3.9
- openai: >=1.0.0 (required for streaming and tools APIs)

### Optional Dependency Pattern
```python
def llm_stream(state, model, messages, temperature=0.7, **kwargs):
    """Stream LLM responses token-by-token."""
    try:
        from openai import OpenAI
        client = OpenAI()
        # ... implementation
    except ImportError:
        return {
            "error": "OpenAI library not installed. Install with: pip install openai>=1.0.0",
            "success": False
        }
```

### OpenAI Streaming Pattern
```python
stream = client.chat.completions.create(
    model=model, messages=messages, stream=True
)
for chunk in stream:
    if chunk.choices[0].delta.content:
        yield chunk.choices[0].delta.content
```

### OpenAI Tools Pattern
```python
response = client.chat.completions.create(
    model=model, messages=messages,
    tools=[{"type": "function", "function": {...}}],
    tool_choice="auto"
)
if response.choices[0].message.tool_calls:
    for tool_call in response.choices[0].message.tool_calls:
        # Dispatch to action
```

## Testing

**Test File Location**: `tests/test_yaml_engine.py` (add to existing or new class)

**Testing Standards**:
- Mock OpenAI client for all tests
- Simulate streaming with generator mock
- Simulate retry scenarios with controlled failures
- Test tool dispatch with mock actions

**Unit Test Cases**:
```python
class TestLLMEnhancedActions(unittest.TestCase):
    def test_llm_stream_yields_chunks(self): ...
    def test_llm_stream_final_result(self): ...
    def test_llm_retry_succeeds_first_try(self): ...
    def test_llm_retry_succeeds_after_failures(self): ...
    def test_llm_retry_respects_max_retries(self): ...
    def test_llm_retry_uses_retry_after_header(self): ...
    def test_llm_tools_dispatches_to_action(self): ...
    def test_llm_tools_multi_turn(self): ...
    def test_llm_tools_invalid_action_error(self): ...
```

**Integration Test Cases**:
```python
class TestLLMEnhancedActionsIntegration(unittest.TestCase):
    def test_llm_stream_in_yaml_workflow(self): ...
    def test_llm_retry_with_checkpoint(self): ...
    def test_llm_tools_with_registered_actions(self): ...
    def test_existing_llm_call_unchanged(self): ...
```

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] All tasks completed
- [ ] Tests pass (existing and new)
- [ ] Existing `llm.call` continues to work unchanged
- [ ] Documentation updated
- [ ] Code follows existing patterns in yaml_engine.py

## Rollback Procedure

If LLM enhanced actions cause issues in production:

1. **Immediate Rollback**:
   ```python
   # In _setup_builtin_actions(), comment out:
   # actions['llm.stream'] = llm_stream
   # actions['llm.retry'] = llm_retry
   # actions['llm.tools'] = llm_tools
   # Keep existing llm.call intact!
   ```

2. **Backward Compatibility**:
   - Existing `llm.call` must remain unchanged
   - YAML agents using only `llm.call` continue working
   - Agents using new actions will fail gracefully with "action not found"

3. **Verification**:
   - Run: `pytest tests/test_yaml_engine.py -k "llm_call"`
   - Verify existing examples still work

4. **Gradual Rollout** (Recommended):
   - Feature flag: `YAMLEngine(enable_llm_enhanced=False)`
   - Test streaming separately from tools
   - Enable retry first (lowest risk)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-06 | 0.2 | Added Dependencies, User Prerequisites, Rollback, Integration Tests | Sarah (PO Agent) |
