# Story YE.3: LangGraph-Compatible Interrupt Behavior

## Status
Complete

## Story
**As a** developer using The Edge Agent,
**I want** interrupt points to STOP execution and wait for explicit resume,
**so that** I can implement human-in-the-loop workflows, approval gates, and debugging breakpoints that match LangGraph's behavior.

## Context

This story aligns The Edge Agent's interrupt behavior with LangGraph's documented behavior. Currently, our implementation yields interrupt events but **continues execution**. This story changes interrupts to **STOP execution** until explicitly resumed.

**Current Behavior:**
```python
# Interrupts yield events but continue execution
for event in graph.invoke(state):
    if event["type"] == "interrupt":
        # Execution continues after this - WRONG
        pass
    if event["type"] == "final":
        # Reaches final in same invocation
        pass
```

**Target Behavior (LangGraph-compatible):**
```python
# First invocation runs until interrupt, then STOPS
events = list(graph.invoke(state))
# Last event is interrupt, execution is paused
assert events[-1]["type"] == "interrupt"
checkpoint_path = events[-1]["checkpoint_path"]

# Resume from checkpoint
events = list(graph.invoke(None, checkpoint=checkpoint_path))
# Continues from interrupt point to completion
assert events[-1]["type"] == "final"
```

**Key Changes:**
| Aspect | Before | After |
|--------|--------|-------|
| Interrupt behavior | Yield & continue | Stop & wait (default) |
| Resume mechanism | N/A | `invoke(None, checkpoint=path)` |
| Checkpointer required | Optional | Required when interrupts defined |
| Checkpointer options | File-based only | File-based OR in-memory |

**Depends on:**
- YE.1: YAML Checkpoint Persistence (completed)
- TD.10: Checkpoint Persistence (completed)

**References:**
- [LangGraph Interrupts Documentation](https://docs.langchain.com/oss/python/langgraph/interrupts)

## Acceptance Criteria

### Core Interrupt Behavior
1. When `interrupt_before` triggers, execution STOPS **before** executing the node
2. When `interrupt_after` triggers, execution STOPS **after** executing the node
3. Interrupt event contains `{"type": "interrupt", "node": str, "state": dict, "checkpoint_path": str}`
4. A checkpointer MUST be configured when interrupts are defined
5. Raise clear `ValueError` at compile time if interrupts defined without checkpointer

### Checkpointer Options
6. **File-based:** `checkpoint_dir="/path/to/dir"` - saves checkpoints as pickle files
7. **In-memory:** `checkpointer=MemoryCheckpointer()` - stores checkpoints in memory (for testing/simple use)
8. Either `checkpoint_dir` OR `checkpointer` satisfies the requirement (not both needed)
9. `MemoryCheckpointer` class provides dict-like storage keyed by checkpoint ID

### Resume Mechanism
10. `invoke(None, checkpoint=path)` resumes execution from the checkpoint
11. `stream(None, checkpoint=path)` resumes with streaming events
12. Resume without checkpoint path raises clear error
13. State is automatically restored from checkpoint on resume
14. Resumed execution continues until next interrupt or completion

### YAML Configuration
15. YAML `config.checkpoint_dir` or `config.checkpointer` required when interrupts defined
16. Clear error message if YAML has interrupts but no checkpointer
17. Resume via `config.checkpoint` parameter works as before

### Error Handling
18. Clear error: interrupts defined without checkpointer
19. Clear error: `invoke(None)` without checkpoint path
20. Clear error: checkpoint file/key not found
21. Clear error: checkpoint corrupted or invalid

### Test & Example Updates (CRITICAL)
22. **ALL** existing interrupt tests MUST be updated to use resume pattern
23. **ALL** interrupt examples in documentation MUST be updated
24. **ALL** YAML examples MUST show correct interrupt + checkpointer pattern
25. **ZERO** tests may rely on old "yield and continue" behavior
26. **ALL** tests MUST pass after changes - no skips, no failures

### Quality Requirements
27. Unit tests for stop behavior
28. Unit tests for resume behavior
29. Integration test for full interrupt → resume → complete cycle
30. Documentation fully updated

## Tasks / Subtasks

- [x] **Task 1: Create MemoryCheckpointer class** (AC: 7, 9)
  - [x] Create `src/the_edge_agent/checkpointers.py`
  - [x] Implement `MemoryCheckpointer` with save/load/list methods
  - [x] Interface matches file-based checkpoint operations
  - [x] Store checkpoints in dict keyed by unique ID

- [x] **Task 2: Require checkpointer for interrupts** (AC: 4, 5, 8, 18)
  - [x] In `compile()`, validate: if interrupts defined, checkpointer must be provided
  - [x] Accept either `checkpoint_dir` (file-based) OR `checkpointer` (memory/custom)
  - [x] Raise `ValueError` with clear message if neither provided
  - [x] Update `YAMLEngine.load_from_dict()` with same validation

- [x] **Task 3: Implement stop behavior in invoke()** (AC: 1, 2, 3)
  - [x] After yielding interrupt event, **return immediately** (stop execution)
  - [x] Checkpoint is auto-saved before yield (existing behavior)
  - [x] Include `checkpoint_path` in interrupt event

- [x] **Task 4: Implement resume via invoke(None, checkpoint=...)** (AC: 10, 12, 13, 14)
  - [x] Detect `initial_state is None` as resume signal
  - [x] Require `checkpoint` parameter for resume
  - [x] Load checkpoint and continue execution via `_invoke_from_node()`
  - [x] Continue until next interrupt or completion

- [x] **Task 5: Implement resume for stream()** (AC: 11)
  - [x] Same pattern: `stream(None, checkpoint=path)`
  - [x] Use `_stream_from_node()` for resumed streaming

- [x] **Task 6: Add error handling** (AC: 18, 19, 20, 21)
  - [x] `ValueError` if interrupts without checkpointer
  - [x] `ValueError` if `invoke(None)` without checkpoint
  - [x] `FileNotFoundError` / `KeyError` if checkpoint not found
  - [x] `ValueError` if checkpoint corrupted

- [x] **Task 7: Update YAML Engine** (AC: 15, 16, 17)
  - [x] Validate checkpointer when interrupts defined
  - [x] Support `checkpointer` parameter in load_from_dict/load_from_file
  - [x] Resume continues to work with `config.checkpoint`

- [x] **Task 8: Update ALL existing tests to pass** (AC: 22-26) **CRITICAL**
  - [x] Identify all tests using `interrupt_before`/`interrupt_after`
  - [x] `tests/test_stategraph_core.py` - update all interrupt tests
  - [x] `tests/test_stategraph_checkpoint.py` - update to use resume pattern
  - [x] `tests/test_yaml_engine.py` - update checkpoint tests
  - [x] Every test with interrupts MUST:
    - [x] Include checkpointer (file or memory)
    - [x] Expect execution to STOP at interrupt
    - [x] Use `invoke(None, checkpoint=...)` to resume
  - [x] Run `pytest tests/` - ALL tests MUST pass (172 tests passing)
  - [x] No `@pytest.skip` or `@pytest.xfail` for interrupt tests

- [x] **Task 9: Add new stop/resume tests** (AC: 27, 28, 29)
  - [x] Test: `invoke()` stops at `interrupt_before`
  - [x] Test: `invoke()` stops at `interrupt_after`
  - [x] Test: `invoke(None, checkpoint=...)` resumes and completes
  - [x] Test: `stream()` stops at interrupt
  - [x] Test: `stream(None, checkpoint=...)` resumes
  - [x] Test: Multiple interrupts require multiple resumes
  - [x] Test: MemoryCheckpointer works for interrupts
  - [x] Test: Error without checkpointer
  - [x] Test: Error resume without checkpoint
  - [x] Integration: Full workflow with 2+ interrupts

- [x] **Task 10: Update ALL documentation** (AC: 23, 24, 30)
  - [x] Update `CLAUDE.md` interrupt documentation
  - [x] Update examples to show stop/resume pattern
  - [x] Document MemoryCheckpointer usage

## Dev Notes

### File Locations
- `src/the_edge_agent/checkpointers.py` - NEW: MemoryCheckpointer class
- `src/the_edge_agent/stategraph.py` - core interrupt behavior (invoke, stream)
- `src/the_edge_agent/checkpoint.py` - may need updates for abstraction
- `src/the_edge_agent/yaml_engine.py` - validation, checkpointer config
- `tests/test_stategraph_*.py` - update all interrupt tests
- `tests/test_yaml_engine.py` - update checkpoint tests

### MemoryCheckpointer Class

```python
# src/the_edge_agent/checkpointers.py

class MemoryCheckpointer:
    """In-memory checkpoint storage for testing and simple use cases."""

    def __init__(self):
        self._storage: Dict[str, Dict[str, Any]] = {}

    def save(self, checkpoint_id: str, state: dict, node: str, config: dict) -> str:
        """Save checkpoint to memory. Returns checkpoint_id."""
        self._storage[checkpoint_id] = {
            "state": state.copy(),
            "node": node,
            "config": config.copy() if config else {},
            "timestamp": time.time(),
            "version": "1.0",
        }
        return checkpoint_id

    def load(self, checkpoint_id: str) -> Dict[str, Any]:
        """Load checkpoint from memory."""
        if checkpoint_id not in self._storage:
            raise KeyError(f"Checkpoint not found: {checkpoint_id}")
        return self._storage[checkpoint_id].copy()

    def list(self) -> List[str]:
        """List all checkpoint IDs."""
        return list(self._storage.keys())

    def delete(self, checkpoint_id: str) -> None:
        """Delete a checkpoint."""
        del self._storage[checkpoint_id]
```

### Compile Validation

```python
def compile(self, interrupt_before=None, interrupt_after=None,
            checkpoint_dir=None, checkpointer=None):
    has_interrupts = bool(interrupt_before or interrupt_after)
    has_checkpointer = bool(checkpoint_dir or checkpointer)

    if has_interrupts and not has_checkpointer:
        raise ValueError(
            "A checkpointer is required when using interrupt_before or interrupt_after. "
            "Provide either checkpoint_dir='/path' for file-based storage "
            "or checkpointer=MemoryCheckpointer() for in-memory storage."
        )
    # ... rest of compile
```

### Stop at Interrupt

```python
# In invoke() main loop:
if current_node in self.interrupt_before:
    checkpoint_path = self._auto_save_checkpoint(state, current_node, config)
    yield {
        "type": "interrupt",
        "node": current_node,
        "state": state.copy(),
        "checkpoint_path": checkpoint_path
    }
    return  # STOP - key change
```

### Resume Detection

```python
def invoke(self, initial_state=None, config=None, checkpoint=None):
    # Resume case
    if initial_state is None:
        if not checkpoint:
            raise ValueError("checkpoint path required to resume from interrupt")
        yield from self.resume_from_checkpoint(checkpoint, config)
        return

    # Normal execution continues...
```

### Test Update Pattern

**Before (old behavior):**
```python
def test_interrupt_before(self):
    graph.compile(interrupt_before=["node_b"])
    events = list(graph.invoke({"x": 1}))
    assert any(e["type"] == "interrupt" for e in events)
    assert events[-1]["type"] == "final"  # Continues to end
```

**After (new behavior):**
```python
def test_interrupt_before(self):
    checkpointer = MemoryCheckpointer()
    graph.compile(interrupt_before=["node_b"], checkpointer=checkpointer)

    # First invoke: runs until interrupt, then STOPS
    events1 = list(graph.invoke({"x": 1}))
    assert events1[-1]["type"] == "interrupt"
    assert events1[-1]["node"] == "node_b"
    checkpoint_path = events1[-1]["checkpoint_path"]

    # Resume: continues from interrupt to completion
    events2 = list(graph.invoke(None, checkpoint=checkpoint_path))
    assert events2[-1]["type"] == "final"
```

### YAML Example

```yaml
config:
  checkpoint_dir: ./checkpoints  # File-based
  # OR
  checkpointer: memory           # In-memory (for testing)
  interrupt_before: [approval_node]

nodes:
  - name: prepare
    run: return {"data": "prepared"}

  - name: approval_node
    run: return {"approved": state.get("approved", False)}

  - name: finalize
    run: return {"result": "done"}

edges:
  - from: __start__
    to: prepare
  - from: prepare
    to: approval_node
  - from: approval_node
    to: finalize
  - from: finalize
    to: __end__
```

## Definition of Done
- [x] All 30 acceptance criteria met
- [x] MemoryCheckpointer implemented and working
- [x] Interrupts STOP execution (no more yield-and-continue)
- [x] Resume via `invoke(None, checkpoint=...)` works correctly
- [x] **`pytest tests/` passes with 0 failures, 0 skips on interrupt tests** (172 tests passing)
- [x] All documentation and examples updated
- [x] **ZERO** references to old "yield and continue" behavior remain

## Risk Assessment
- **Primary Risk:** Breaking change - all existing interrupt usage changes
- **Mitigation:** Update all tests and examples in same PR
- **Impact:** Users with interrupt code must update to use resume pattern
- **Benefit:** Matches LangGraph behavior, enables proper human-in-the-loop workflows

## QA Results

### Test Design Review
**Date:** 2025-12-06
**Reviewer:** Quinn (Test Architect)
**Assessment:** `docs/qa/assessments/YE.3-test-design-20251206.md`

#### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 43 |
| Unit Tests | 18 (42%) |
| Integration Tests | 19 (44%) |
| E2E Tests | 6 (14%) |
| P0 (Critical) | 16 |
| P1 (High) | 18 |
| P2 (Medium) | 9 |

#### Risk Coverage

| Risk ID | Description | Tests Covering |
|---------|-------------|----------------|
| RISK-001 | Breaking change breaks existing code | 6 tests |
| RISK-002 | Checkpointer validation fails silently | 4 tests |
| RISK-003 | Resume doesn't restore state correctly | 5 tests |
| RISK-004 | MemoryCheckpointer thread safety | 2 tests |
| RISK-005 | YAML engine inconsistent with core | 3 tests |

#### Critical Test Scenarios (P0)

1. **Stop Behavior:** YE.3-UNIT-001 to YE.3-UNIT-003 - Verify interrupts STOP execution
2. **Event Structure:** YE.3-UNIT-007 to YE.3-UNIT-010 - Validate interrupt event fields
3. **Validation:** YE.3-UNIT-012 to YE.3-UNIT-014 - Compile-time checkpointer requirement
4. **Resume:** YE.3-INT-004 to YE.3-INT-008 - Resume mechanism correctness
5. **YAML:** YE.3-INT-011 to YE.3-INT-013 - YAML engine parity
6. **Full Cycle:** YE.3-E2E-003 - Complete interrupt/resume/final workflow

#### QA Recommendations

1. **High Priority:** Task 8 (Update existing tests) is critical path - must be done carefully to avoid regressions
2. **Test Pattern:** Use `MemoryCheckpointer` for all unit/integration tests to avoid filesystem dependencies
3. **Execution Order:** Run P0 unit tests first to fail fast on core behavior changes
4. **Coverage Gap:** None identified - all 30 ACs have test coverage

#### Testability Assessment

- **Controllability:** Good - MemoryCheckpointer enables deterministic testing
- **Observability:** Good - Interrupt events contain all needed state
- **Isolation:** Good - Each test can use fresh MemoryCheckpointer instance

## Implementation Completion

### Completion Date
2025-12-06

### Test Results
- Total tests: 172
- Passed: 172
- Failed: 0
- Skipped: 0

### Files Modified/Created

**New Files:**
- `src/the_edge_agent/checkpointers.py` - MemoryCheckpointer class

**Modified Source Files:**
- `src/the_edge_agent/__init__.py` - Added MemoryCheckpointer export
- `src/the_edge_agent/stategraph.py` - Updated compile() with checkpointer validation, updated invoke() with stop behavior and resume detection
- `src/the_edge_agent/checkpoint.py` - Updated _auto_save_checkpoint for MemoryCheckpointer support, updated _invoke_from_node and _stream_from_node with skip_first_interrupt logic
- `src/the_edge_agent/yaml_engine.py` - Added checkpointer parameter support

**Modified Test Files:**
- `tests/test_stategraph_core.py` - Updated interrupt tests for stop/resume behavior
- `tests/test_stategraph_checkpoint.py` - Added new TestStopResumeBehavior class with 8 new tests
- `tests/test_stategraph_stream.py` - Updated parallel interrupt test to use checkpointer
- `tests/test_yaml_engine.py` - Updated interrupt tests to use checkpointer

**Modified Documentation:**
- `CLAUDE.md` - Updated interrupt and checkpoint documentation

## Change Log
| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-06 | 0.2 | Removed backward compatibility, simplified to single behavior | Sarah (PO Agent) |
| 2025-12-06 | 0.3 | Removed thread_id, added MemoryCheckpointer, simplified resume | Sarah (PO Agent) |
| 2025-12-06 | 0.4 | QA Test Design Review completed - 43 test scenarios defined | Quinn (QA Agent) |
| 2025-12-06 | 1.0 | **Implementation Complete** - All tasks done, 172 tests passing | Claude Code |
