# Story TEA-BUILTIN-001.1: Memory Actions

## Status

Draft

## Story

**As a** YAML agent developer,
**I want** built-in memory actions (store, retrieve, summarize),
**so that** I can build conversational agents with persistent context without writing Python code.

## Acceptance Criteria

1. `memory.store` action stores key-value pairs in a memory backend with optional TTL
2. `memory.retrieve` action retrieves values by key with optional default fallback
3. `memory.summarize` action condenses conversation history using LLM to fit token windows
4. Memory actions support pluggable backends (in-memory default, Redis optional)
5. Memory state persists across graph invocations within same session
6. Memory can be serialized/restored with checkpoints
7. All actions follow existing `_setup_builtin_actions()` pattern
8. Actions are accessible via both `memory.*` and `actions.memory_*` namespaces
9. Comprehensive unit tests cover all memory operations
10. Documentation updated in CLAUDE.md and docs/YAML_AGENTS.md

## Dependencies

**Blocked By**: None (can start immediately)

**Blocks**:
- TEA-BUILTIN-002.2 (RAG Actions) - may use memory for caching embeddings

**Internal Dependencies**:
- `memory.summarize` uses existing `llm.call` action internally

## User Prerequisites

- [ ] **Optional**: Obtain `OPENAI_API_KEY` from https://platform.openai.com (required for `memory.summarize`)
- [ ] **Optional**: Redis server running (only if using Redis backend)

## Tasks / Subtasks

- [ ] Task 1: Implement memory backend abstraction (AC: 4, 6)
  - [ ] Create `MemoryBackend` protocol/interface
  - [ ] Implement `InMemoryBackend` as default
  - [ ] Add serialization hooks for checkpoint compatibility
  - [ ] Add configuration for backend selection in YAML

- [ ] Task 2: Implement `memory.store` action (AC: 1, 7, 8)
  - [ ] Define function signature: `memory_store(state, key, value, ttl=None, namespace=None, **kwargs)`
  - [ ] Implement storage with optional TTL expiration
  - [ ] Support namespace isolation for multi-agent scenarios
  - [ ] Register in actions dict with both namespaces
  - [ ] Return `{"stored": True, "key": str, "namespace": str}`

- [ ] Task 3: Implement `memory.retrieve` action (AC: 2, 7, 8)
  - [ ] Define function signature: `memory_retrieve(state, key, default=None, namespace=None, **kwargs)`
  - [ ] Implement retrieval with default fallback
  - [ ] Handle TTL expiration (return default if expired)
  - [ ] Register in actions dict with both namespaces
  - [ ] Return `{"value": any, "found": bool, "key": str}`

- [ ] Task 4: Implement `memory.summarize` action (AC: 3, 7, 8)
  - [ ] Define function signature: `memory_summarize(state, messages_key, max_tokens=1000, model=None, **kwargs)`
  - [ ] Use existing `llm.call` internally for summarization
  - [ ] Implement sliding window + summarization strategy
  - [ ] Register in actions dict with both namespaces
  - [ ] Return `{"summary": str, "original_count": int, "token_estimate": int}`

- [ ] Task 5: Add session persistence (AC: 5, 6)
  - [ ] Inject memory backend into graph context
  - [ ] Ensure memory survives multiple `invoke()` calls
  - [ ] Add memory state to checkpoint serialization
  - [ ] Add memory restoration from checkpoint

- [ ] Task 6: Write tests (AC: 9)
  - [ ] Test memory.store with various value types
  - [ ] Test memory.retrieve with found/not-found/expired cases
  - [ ] Test memory.summarize with mock LLM
  - [ ] Test namespace isolation
  - [ ] Test checkpoint serialization/restoration
  - [ ] Test TTL expiration behavior

- [ ] Task 7: Update documentation (AC: 10)
  - [ ] Add memory actions to CLAUDE.md built-in actions section
  - [ ] Add examples in docs/YAML_AGENTS.md
  - [ ] Create example YAML showing conversational agent with memory

## Dev Notes

### Integration Points
- **File**: `src/the_edge_agent/yaml_engine.py`
- **Method**: `_setup_builtin_actions()` (lines 623-786)
- **Pattern**: Actions receive `(state, **params, **kwargs)` and return dict

### Existing Pattern Reference
```python
def action_name(state, required_param, optional_param=default, **kwargs):
    """Docstring."""
    # Implementation
    return {'result_key': value}

actions['namespace.action'] = action_name
actions['actions.namespace_action'] = action_name  # Alias
```

### Key Constraints
- Memory backend must be injectable via `YAMLEngine(memory_backend=...)`
- Default in-memory backend requires no external dependencies
- `memory.summarize` depends on `llm.call` - must handle missing OpenAI gracefully
- TTL implementation should use monotonic time, not wall clock

### Version Requirements
- Python: >=3.9
- openai: >=1.0.0 (optional, for memory.summarize)
- redis: >=4.0.0 (optional, for Redis backend)

### Optional Dependency Pattern
```python
def memory_summarize(state, messages_key, max_tokens=1000, model=None, **kwargs):
    """Summarize conversation history using LLM."""
    try:
        # Use internal llm.call action
        llm_call = kwargs.get('_actions', {}).get('llm.call')
        if not llm_call:
            return {
                "error": "llm.call action not available. Ensure OpenAI is configured.",
                "success": False
            }
        # ... implementation
    except Exception as e:
        return {"error": str(e), "success": False}
```

### Technical Dependencies
- Requires existing `llm.call` action for `memory.summarize`
- No external dependencies for default in-memory backend

## Testing

**Test File Location**: `tests/test_yaml_engine.py` (add new test class)

**Priority Levels**:
- **P0**: Critical - Must pass for basic functionality
- **P1**: Core - Required for production readiness
- **P2**: Edge cases - Important but not blocking

**Testing Standards**:
- Use `unittest` framework consistent with existing tests
- Mock OpenAI client for `memory.summarize` tests
- Test both success and error paths
- Verify checkpoint serialization round-trip

**Unit Test Cases**:
```python
class TestMemoryActions(unittest.TestCase):
    # P0 - Critical
    def test_memory_store_basic(self): ...  # (P0)
    def test_memory_retrieve_found(self): ...  # (P0)
    def test_memory_checkpoint_persistence(self): ...  # (P0)

    # P1 - Core functionality
    def test_memory_store_with_ttl(self): ...  # (P1)
    def test_memory_retrieve_not_found_default(self): ...  # (P1)
    def test_memory_retrieve_expired(self): ...  # (P1)
    def test_memory_summarize(self): ...  # (P1)
    def test_memory_dual_namespace_access(self): ...  # (P1) - AC8: Verify both memory.* and actions.memory_* work
    def test_memory_store_non_serializable(self): ...  # (P1) - Error handling for non-serializable data
    def test_memory_summarize_llm_unavailable(self): ...  # (P1) - Graceful handling when OpenAI unavailable

    # P2 - Edge cases
    def test_memory_namespace_isolation(self): ...  # (P2)
    def test_memory_backend_switching(self): ...  # (P2) - AC4: Verify custom backends can be injected
    def test_memory_backend_failure(self): ...  # (P2) - Error handling when backend fails
```

**Integration Test Cases**:
```python
class TestMemoryActionsIntegration(unittest.TestCase):
    def test_memory_in_yaml_workflow(self): ...  # (P0)
    def test_memory_with_checkpoint_save_resume(self): ...  # (P0)
    def test_memory_across_multiple_invokes(self): ...  # (P1)
    def test_memory_in_parallel_execution(self): ...  # (P2)
```

**Test Summary**: 17 tests (13 unit + 4 integration) | P0: 5 | P1: 8 | P2: 4

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] All tasks completed
- [ ] Tests pass (existing and new)
- [ ] No regressions in existing YAML engine functionality
- [ ] Documentation updated
- [ ] Code follows existing patterns in yaml_engine.py

## Rollback Procedure

If memory actions cause issues in production:

1. **Immediate Rollback**:
   ```python
   # In _setup_builtin_actions(), comment out:
   # actions['memory.store'] = memory_store
   # actions['memory.retrieve'] = memory_retrieve
   # actions['memory.summarize'] = memory_summarize
   ```

2. **State Cleanup**:
   - Memory state is isolated; removing actions doesn't affect other state
   - Existing checkpoints remain valid (memory data simply ignored)

3. **Verification**:
   - Run existing test suite: `pytest tests/test_yaml_engine.py`
   - Verify existing actions still work: `llm.call`, `http.*`, `file.*`

4. **Gradual Rollout** (Recommended):
   - Use feature flag in YAMLEngine: `YAMLEngine(enable_memory=False)`
   - Enable per-environment before full release

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-06 | 0.2 | Added Dependencies, User Prerequisites, Rollback, Integration Tests | Sarah (PO Agent) |
