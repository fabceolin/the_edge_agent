# Story TEA-BUILTIN-001.1: Memory Actions

## Status

Ready for Done

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

- [x] Task 1: Implement memory backend abstraction (AC: 4, 6)
  - [x] Create `MemoryBackend` protocol/interface
  - [x] Implement `InMemoryBackend` as default
  - [x] Add serialization hooks for checkpoint compatibility
  - [x] Add configuration for backend selection in YAML

- [x] Task 2: Implement `memory.store` action (AC: 1, 7, 8)
  - [x] Define function signature: `memory_store(state, key, value, ttl=None, namespace=None, **kwargs)`
  - [x] Implement storage with optional TTL expiration
  - [x] Support namespace isolation for multi-agent scenarios
  - [x] Register in actions dict with both namespaces
  - [x] Return `{"stored": True, "key": str, "namespace": str}`

- [x] Task 3: Implement `memory.retrieve` action (AC: 2, 7, 8)
  - [x] Define function signature: `memory_retrieve(state, key, default=None, namespace=None, **kwargs)`
  - [x] Implement retrieval with default fallback
  - [x] Handle TTL expiration (return default if expired)
  - [x] Register in actions dict with both namespaces
  - [x] Return `{"value": any, "found": bool, "key": str}`

- [x] Task 4: Implement `memory.summarize` action (AC: 3, 7, 8)
  - [x] Define function signature: `memory_summarize(state, messages_key, max_tokens=1000, model=None, **kwargs)`
  - [x] Use existing `llm.call` internally for summarization
  - [x] Implement sliding window + summarization strategy
  - [x] Register in actions dict with both namespaces
  - [x] Return `{"summary": str, "original_count": int, "token_estimate": int}`

- [x] Task 5: Add session persistence (AC: 5, 6)
  - [x] Inject memory backend into graph context
  - [x] Ensure memory survives multiple `invoke()` calls
  - [x] Add memory state to checkpoint serialization
  - [x] Add memory restoration from checkpoint

- [x] Task 6: Write tests (AC: 9)
  - [x] Test memory.store with various value types
  - [x] Test memory.retrieve with found/not-found/expired cases
  - [x] Test memory.summarize with mock LLM
  - [x] Test namespace isolation
  - [x] Test checkpoint serialization/restoration
  - [x] Test TTL expiration behavior

- [x] Task 7: Update documentation (AC: 10)
  - [x] Add memory actions to CLAUDE.md built-in actions section
  - [x] Add examples in docs/YAML_AGENTS.md
  - [x] Create example YAML showing conversational agent with memory

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

- [x] All acceptance criteria verified
- [x] All tasks completed
- [x] Tests pass (existing and new)
- [x] No regressions in existing YAML engine functionality
- [x] Documentation updated
- [x] Code follows existing patterns in yaml_engine.py

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

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
N/A - No blocking issues encountered

### Completion Notes
- Implemented `MemoryBackend` protocol and `InMemoryBackend` class with TTL support using monotonic time
- Added `memory.store`, `memory.retrieve`, `memory.summarize` actions to `_setup_builtin_actions()`
- Both `memory.*` and `actions.memory_*` namespaces registered for all actions
- Memory backend injectable via `YAMLEngine(memory_backend=...)`
- Added `get_memory_state()`, `restore_memory_state()`, `clear_memory()` methods to YAMLEngine
- Exported `MemoryBackend` and `InMemoryBackend` from package `__init__.py`
- Created comprehensive test file with 37 tests covering all requirements
- All 295 tests pass with no regressions

### File List
| File | Action | Description |
|------|--------|-------------|
| `src/the_edge_agent/yaml_engine.py` | Modified | Added MemoryBackend protocol, InMemoryBackend class, memory actions |
| `src/the_edge_agent/__init__.py` | Modified | Added MemoryBackend and InMemoryBackend exports |
| `tests/test_yaml_engine_memory.py` | Created | 37 tests for memory actions |
| `CLAUDE.md` | Modified | Added Memory Actions documentation |
| `docs/YAML_AGENTS.md` | Modified | Added Memory Actions section with examples |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-06 | 0.2 | Added Dependencies, User Prerequisites, Rollback, Integration Tests | Sarah (PO Agent) |
| 2025-12-06 | 1.0 | Implementation complete - all tasks done, 37 tests passing | James (Dev Agent) |

## QA Results

### Review Date: 2025-12-06

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: Excellent**

The implementation demonstrates high quality across all dimensions:

1. **Architecture**: Clean Protocol-based abstraction (`MemoryBackend`) enables pluggable backends. The `InMemoryBackend` implementation is straightforward and follows the existing codebase patterns.

2. **Thread Safety**: Proper use of `threading.Lock()` ensures thread-safe operations for parallel execution scenarios.

3. **Time Handling**: Uses `time.monotonic()` for TTL - this is the correct choice as it's immune to system clock changes. The state serialization correctly converts monotonic timestamps to relative remaining TTL for checkpoint compatibility.

4. **Error Handling**: All three actions (`memory.store`, `memory.retrieve`, `memory.summarize`) have comprehensive error handling with consistent return structures.

5. **Documentation**: Well-documented with docstrings, examples in CLAUDE.md, and comprehensive examples in YAML_AGENTS.md.

### Refactoring Performed

None required - implementation is clean and follows existing patterns.

### Compliance Check

- Coding Standards: ✓ Follows existing patterns in yaml_engine.py exactly
- Project Structure: ✓ Files in correct locations, exports properly updated in __init__.py
- Testing Strategy: ✓ 37 tests covering P0/P1/P2 priorities, unit + integration coverage
- All ACs Met: ✓ All 10 acceptance criteria verified

### Improvements Checklist

- [x] Memory backend protocol defined with all required methods
- [x] InMemoryBackend with TTL using monotonic time
- [x] Thread-safe implementation with locks
- [x] Checkpoint serialization/restoration
- [x] All actions registered with dual namespaces (memory.* and actions.memory_*)
- [x] Comprehensive test coverage (37 tests)
- [x] Documentation updated

### Security Review

**Status: PASS**

- Keys are converted to strings, preventing potential injection issues
- No hardcoded secrets or sensitive data handling issues
- Thread-safe implementation prevents race conditions
- `memory.summarize` gracefully handles missing OpenAI dependency

### Performance Considerations

**Status: PASS**

- O(1) operations for store/retrieve
- Uses monotonic time for TTL (immune to clock drift)
- Lock contention is minimal for typical single-threaded use
- Memory cleanup happens lazily on retrieve (expired entries cleaned when accessed)

### Files Modified During Review

None - no refactoring required.

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-BUILTIN-001.1-memory-actions.yml

### Recommended Status

**✓ Ready for Done**

All acceptance criteria met, comprehensive test coverage (37 tests, 100% of required scenarios), documentation complete, and no blocking issues identified.

---

**Future Considerations (Non-blocking):**
- Consider LRU eviction policy for InMemoryBackend when memory limits are needed
- Redis backend implementation for distributed scenarios (mentioned in story as optional)
