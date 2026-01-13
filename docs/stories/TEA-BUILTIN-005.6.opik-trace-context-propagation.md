# Story TEA-BUILTIN-005.6: Opik Trace Context Propagation

## Status

Done

- ✅ Parallel execution context loss: Fixed (ThreadExecutor.submit with context propagation)
- ✅ Sequential execution context loss: Fixed (wrapper span in CLI keeps span stack non-empty)

## Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

## Story

**As a** TEA developer running YAML agents with Opik tracing,
**I want** all LLM calls within a single agent execution to be nested as child spans under the parent trace,
**so that** I can see the complete execution flow and all LLM interactions in Opik's dashboard.

## Problem Statement

When running TEA YAML agents with Opik tracing enabled, only the **first LLM call** is correctly nested under the parent trace. Subsequent LLM calls either:
1. Appear as separate traces (different trace IDs)
2. Go to a different Opik project
3. Don't appear at all

### Evidence

Debug output from `llm_actions.py` showing trace context loss:
```
[OPIK DEBUG] Trace context: 019bb52f-7cb4-7ebf-9970-07c1140c7cb2  # First LLM call
[OPIK DEBUG] Span context: None
[OPIK DEBUG] Trace context: None  # Second LLM call - context LOST
[OPIK DEBUG] Span context: None
```

### Visual Evidence

- Parent trace shows only 1 LLM span instead of expected 2+
- Second LLM call creates a new trace with different ID (e.g., ending in `...0844` vs parent `...1f4`)
- The second trace appears outside the parent trace hierarchy

## Acceptance Criteria

1. **Multiple LLM Nesting**: All LLM calls within a single agent execution appear as child spans under the parent trace
2. **Correct Project**: All spans go to the project specified in YAML settings (not env var)
3. **Context Preservation**: Trace context remains valid throughout agent execution
4. **No Orphan Traces**: No LLM calls create separate/orphan traces
5. **Backward Compatible**: Fix doesn't break existing single-LLM-call agents

## Prior Work Completed (TEA-BUILTIN-005.5)

These fixes were already applied in the parent story:

### Fix 1: CLI Trace Context (APPLIED)
Changed `cli.py` from `client.trace()` to `opik.start_as_current_trace()` context manager:
```python
# Before:
trace = client.trace(name=agent_name, ...)

# After:
opik_context_manager = opik.start_as_current_trace(name=agent_name, ...)
opik_context_manager.__enter__()
# ... agent execution ...
opik_context_manager.__exit__(*exc_info)
```
**Result**: Parent trace created correctly, agent graph appears in Opik UI.

### Fix 2: Project Name Source (APPLIED)
Changed `yaml_nodes.py` to use raw YAML settings instead of resolved `_opik_config`:
```python
# Before (env var had priority):
opik_config = getattr(engine, "_opik_config", {})
project_name = opik_config.get("project_name")

# After (YAML settings have priority):
engine_config = getattr(engine, "_config", {})
project_name = engine_config.get("settings", {}).get("opik", {}).get("project_name")
```
**Result**: LLM calls use same project as parent trace.

### Fix 3: Opik Flush Before Exit (APPLIED)
Added `opik.flush()` in `cli.py` before exiting trace context:
```python
finally:
    if opik_enabled and opik_context_manager is not None:
        try:
            import opik
            opik.flush()
        except Exception as flush_err:
            ...
```
**Result**: Ensures spans are uploaded before trace ends.

## Failed Debugging Attempts

### Attempt 1: Client Caching by Project Name
**Hypothesis**: Creating new `track_openai` wrappers resets context.
**Implementation**:
```python
_wrapped_clients: Dict[Tuple[str, str, str], Any] = {}

def _get_wrapped_client(client, provider, api_base, opik_project_name):
    cache_key = (provider, api_base, opik_project_name or "")
    if cache_key not in _wrapped_clients:
        _wrapped_clients[cache_key] = track_openai(client, project_name=opik_project_name)
    return _wrapped_clients[cache_key]
```
**Result**: ❌ Did not fix the issue. Second LLM call still loses trace context.

### Attempt 2: Remove Client Caching
**Hypothesis**: Cached clients might have stale context references.
**Implementation**: Removed caching, create fresh `track_openai` wrapper for each call.
**Result**: ❌ Did not fix the issue.

### Attempt 3: Use `@track` Decorator
**Hypothesis**: `track_openai` wrapper doesn't propagate context correctly; explicit `@track` might work.
**Implementation**:
```python
from opik import track as opik_track

def _make_api_call_impl():
    return client.chat.completions.create(...)

if opik_track_decorator:
    make_api_call = opik_track_decorator(
        name="llm_call",
        type="llm"
    )(_make_api_call_impl)
else:
    make_api_call = _make_api_call_impl
```
**Result**: ❌ Did not fix the issue. Second call still has `None` trace context.

### Attempt 4: Debug Logging with `opik_context`
**Implementation**:
```python
try:
    from opik import opik_context
    trace_data = opik_context.get_current_trace_data()
    span_data = opik_context.get_current_span_data()
    logger.debug(f"[OPIK DEBUG] Trace context: {trace_data.trace_id if trace_data else None}")
    logger.debug(f"[OPIK DEBUG] Span context: {span_data.span_id if span_data else None}")
except Exception as e:
    logger.debug(f"[OPIK DEBUG] Could not get context: {e}")
```
**Result**: ✅ Confirmed the issue - first call has trace context, second call has `None`.

### Attempt 5: Restore track_openai wrapper approach
**Hypothesis**: The `@track` decorator approach doesn't properly integrate with Opik's span hierarchy.
**Finding**: `llm.call` was using `@track` decorator while `llm.stream` and `llm.tools` use `track_openai` wrapper.
**Implementation**: Restored `track_openai(client, project_name=opik_project_name)` approach in `llm_call` function with debug logging.

**Files Modified**:
- `llm_actions.py`: Changed from `@track` decorator to `track_openai(client, project_name=opik_project_name)`
- Added debug logging to check trace context before/after wrapping

**Debug Logging Added**:
```python
_logger.debug(
    f"[OPIK DEBUG llm_call] Before track_openai: "
    f"trace={trace_data.id if trace_data else None}, "
    f"span={span_data.id if span_data else None}, "
    f"project={opik_project_name}"
)
```

**Result**: Debug output shows trace context IS present during first LLM call. Unable to test second LLM call due to Azure deployment 404 error. Further testing needed with working API endpoint.

### Attempt 6: Unit Tests for YAMLEngine Trace Context
**Implementation**: Created `test_opik_trace_propagation.py` with tests that verify:
1. Basic Opik trace context propagation
2. Trace context with `track_openai` wrapper
3. Trace context in nested function calls
4. Trace context with ThreadPoolExecutor context manager
5. Trace context through YAMLEngine execution loop

**Result**: ✅ All 9 tests pass. Trace context is preserved in all tested scenarios including:
- Pure function calls
- `track_openai` wrapper
- `ThreadPoolExecutor` context manager
- YAMLEngine execution loop

**Conclusion**: The issue is NOT with Python contextvars, Opik SDK, or YAMLEngine execution loop. The issue likely occurs during actual API calls or specific LLM action execution paths that we couldn't test due to API unavailability.

## Potential Root Causes to Investigate

### 1. TEA Node Execution Loop
The YAML engine executes nodes sequentially. Between node executions, something might be clearing the contextvars.

**Files to investigate**:
- `python/src/the_edge_agent/yaml_engine.py` - Main execution loop
- `python/src/the_edge_agent/stategraph.py` - Graph traversal

**Hypothesis**: The graph traversal or node execution might be using threads or async that don't inherit contextvars.

### 2. Opik SDK Contextvar Behavior
The Opik SDK stores trace context in Python's `contextvars`. These are thread-local-like and should propagate to child tasks, but:
- Threads started with `threading.Thread` don't inherit context by default
- `ThreadPoolExecutor` doesn't inherit context by default
- `asyncio` tasks inherit context, but sync-to-async bridges might not

**Investigation needed**:
- Check if TEA uses threads between node executions
- Check if Opik uses `copy_context()` for child tasks

### 3. `track_openai` Wrapper Lifecycle
The `track_openai` wrapper might be designed for single-use scenarios.

**Investigation needed**:
- Review Opik's `track_openai` source code
- Check if wrapper maintains internal state that gets exhausted

### 4. Context Manager Exit Timing
The `start_as_current_trace()` context might be getting exited prematurely.

**Investigation needed**:
- Add debug logging around context manager lifecycle
- Verify context is still active during second LLM call

### 5. Multiple LLM Action Function Calls
Each LLM call might be creating its own isolated scope.

**Investigation needed**:
- Check if the function call boundaries reset contextvars
- Try wrapping the entire agent execution in a single `@track` span

## Tasks / Subtasks

- [x] **Task 1: Investigate TEA Execution Loop**
  - [x] Add debug logging to `yaml_engine.py` execution loop
  - [x] Check for thread usage between nodes
  - [x] Verify contextvars persistence across node boundaries
  - **Finding**: `stategraph.py` uses `ThreadPoolExecutor` via `parallel_executors.py` for parallel flows

- [x] **Task 2: Review Opik SDK Internals**
  - [x] Read `track_openai` source code
  - [x] Understand contextvar storage mechanism
  - [x] Check for known issues in Opik GitHub
  - **Finding**: Opik uses `contextvars.ContextVar` for trace context storage

- [x] **Task 3: Create Minimal Reproduction**
  - [x] Create simple Python script that:
    - Starts Opik trace context
    - Makes two sequential LLM calls
    - Logs trace context between calls
  - [x] Verify if issue is TEA-specific or Opik-general
  - **Finding**: Issue is TEA-specific - `ThreadPoolExecutor` workers start with blank context

- [x] **Task 4: Test ThreadPoolExecutor Hypothesis**
  - [x] Check if TEA uses executors between nodes
  - [x] If yes, try using `contextvars.copy_context().run()`
  - **Finding**: `parallel_executors.py:ThreadExecutor.submit()` directly calls `ThreadPoolExecutor.submit()` without context propagation

- [x] **Task 5: Implement Fix**
  - [x] Modified `ThreadExecutor.submit()` to propagate contextvars using `copy_context().run()`
  - [x] Added 3 unit tests for context propagation
  - [x] All 33 parallel executor tests pass
  - [x] Updated documentation in `parallel_executors.py`

## Dev Notes

### Key Files

| File | Purpose |
|------|---------|
| `python/src/the_edge_agent/cli.py` | Creates parent Opik trace context |
| `python/src/the_edge_agent/yaml_nodes.py` | Injects `opik_trace` and `opik_project_name` to LLM actions |
| `python/src/the_edge_agent/actions/llm_actions.py` | Wraps client with `track_openai`, makes API calls |
| `python/src/the_edge_agent/yaml_engine.py` | Executes nodes sequentially |
| `python/src/the_edge_agent/stategraph.py` | Graph traversal logic |

### Current Debug Code Location

Debug logging is in `llm_actions.py`:
```python
# Near line ~200 in llm_call function
try:
    from opik import opik_context
    trace_data = opik_context.get_current_trace_data()
    span_data = opik_context.get_current_span_data()
    logger.debug(f"[OPIK DEBUG] Trace context: {trace_data.trace_id if trace_data else None}")
    logger.debug(f"[OPIK DEBUG] Span context: {span_data.span_id if span_data else None}")
except Exception as e:
    logger.debug(f"[OPIK DEBUG] Could not get context: {e}")
```

### Test Command

```bash
cd firebase/functions-agents
tea run agents/rankellix_alignment_check.yaml \
  --state-json /tmp/alignment_3files_test.json \
  --log-level DEBUG 2>&1 | grep -E "(OPIK|Started|Trace)"
```

### Expected vs Actual Behavior

**Expected**:
```
Trace: parent-trace-id
  └── llm_call (span 1)
  └── llm_call (span 2)
```

**Actual**:
```
Trace: parent-trace-id
  └── llm_call (span 1)

Trace: orphan-trace-id (SEPARATE!)
  └── llm_call (span 2)
```

## References

- [Opik SDK GitHub](https://github.com/comet-ml/opik)
- [Python contextvars documentation](https://docs.python.org/3/library/contextvars.html)
- [TEA-BUILTIN-005.5 Story](./TEA-BUILTIN-005.5.opik-agent-graph-visualization.md)
- [TEA-BUILTIN-005 Epic](./TEA-BUILTIN-005.opik-integration-epic.md)

## Solution Implemented (Partial)

### Root Cause 1: Parallel Execution Context Loss (FIXED ✅)

The `ThreadExecutor.submit()` method in `parallel_executors.py` was directly calling `ThreadPoolExecutor.submit()` without propagating Python's `contextvars` context. When parallel flows were executed:

1. CLI created Opik trace context using `start_as_current_trace()`
2. TEA's execution loop submitted parallel flows via `ThreadExecutor.submit()`
3. Worker threads started with **blank context** (Python default behavior)
4. LLM calls in worker threads had no trace context → orphaned traces

### Root Cause 2: Sequential Execution Context Loss (UNDER INVESTIGATION ⚠️)

Manual testing with the Alignment Check agent revealed that context is also lost for **sequential** LLM calls within the same node (no threading involved).

**Debug output from manual test (2026-01-12):**
```
# First LLM call - context present ✅
DEBUG: [OPIK DEBUG llm_call] Before track_openai: trace=019bb548-55f6-76b1-9dc7-e0c1f8873188, span=None
DEBUG: [OPIK DEBUG llm_call] After track_openai: trace=019bb548-55f6-76b1-9dc7-e0c1f8873188

# Second LLM call - context LOST ❌
DEBUG: [OPIK DEBUG llm_call] Before track_openai: trace=None, span=None
DEBUG: [OPIK DEBUG llm_call] After track_openai: trace=None
```

**Observations:**
- The "Mother Pattern" alignment check makes O(N) sequential LLM calls within a single node
- First call has trace context, second call shows `trace=None` BEFORE `track_openai` is called
- No threading is involved in sequential execution path
- LiteLLM debug shows it uses `AiohttpTransport` (async HTTP client internally)

**ROOT CAUSE FOUND (2026-01-12):**

The bug is in Opik SDK's interaction between `start_as_current_trace()` and `track_openai`:

1. `start_as_current_trace()` creates trace and adds its ID to `TRACES_CREATED_BY_DECORATOR` set
2. `track_openai` wraps LLM calls with spans
3. When first LLM call's span ends, Opik checks if span stack is empty
4. If empty AND trace is in `TRACES_CREATED_BY_DECORATOR`, it **pops the trace** (`pop_trace_data()` → sets to None)
5. Second LLM call sees `trace=None`

**Evidence from Opik SDK source code:**

File: `opik/decorator/base_track_decorator.py` (lines 604-610):
```python
if (
    context_storage.span_data_stack_empty()  # TRUE after first span ends
    and possible_trace_data_to_end is not None
    and possible_trace_data_to_end.id in TRACES_CREATED_BY_DECORATOR  # TRUE!
):
    trace_data_to_end = context_storage.pop_trace_data()  # BUG: Pops trace!
```

File: `opik/decorator/context_manager/trace_context_manager.py` (line 57-61):
```python
# start_as_current_trace() adds to this set:
base_track_decorator.add_start_trace_candidate(...)
# Which calls: TRACES_CREATED_BY_DECORATOR.add(trace_data.id)
```

**This is an Opik SDK bug** - traces created by `start_as_current_trace()` should NOT be popped when decorator spans end.

**Workaround Implemented (2026-01-12):**

Created a wrapper span in `cli.py` that stays active throughout agent execution. This keeps the span stack non-empty so the trace is never popped.

### Fix Applied (Both Parallel and Sequential)

#### Fix 1: Parallel Execution (ThreadExecutor)

Modified `ThreadExecutor.submit()` to capture and propagate context:

```python
def submit(self, fn: Callable[..., Any], *args: Any, **kwargs: Any) -> Future:
    # TEA-BUILTIN-005.6: Capture current context for propagation
    context = contextvars.copy_context()

    def context_wrapper() -> Any:
        """Run the function within the captured context."""
        return context.run(fn, *args, **kwargs)

    return self._executor.submit(context_wrapper)
```

This ensures:
1. Context is captured at submission time (not execution time)
2. Each worker runs within the captured context
3. Opik trace context propagates to all parallel flows
4. No orphaned traces

#### Fix 2: Sequential Execution (CLI Wrapper Span)

Added wrapper span creation in `cli.py` to prevent trace from being popped:

```python
# TEA-BUILTIN-005.6: Create wrapper span to prevent trace context loss
wrapper_span_data = opik_span.SpanData(
    id=opik_helpers.generate_id(),
    trace_id=opik_trace.id,
    parent_span_id=None,
    start_time=datetime_helpers.local_timestamp(),
    name=f"{trace_name}_execution",
    type="general",
    metadata={"_tea_wrapper_span": True},
    project_name=project_name,
)
context_storage.add_span_data(wrapper_span_data)
```

This ensures:
1. Span stack is never empty during agent execution
2. `track_openai` spans end without triggering trace pop
3. All LLM calls remain children of the parent trace
4. Wrapper span is properly ended and logged when agent completes

### Verification

- **33 existing tests** for `parallel_executors.py` pass
- **3 new tests** specifically verify context propagation:
  - `test_contextvars_propagation_through_thread_executor`
  - `test_opik_trace_context_through_thread_executor`
  - `test_context_propagation_does_not_leak_between_submits`
- **Manual test** with alignment check agent shows both LLM calls with same trace ID

**Before fix:**
```
DEBUG: [OPIK DEBUG llm_call] Before track_openai: trace=019bb548-55f6... ✅
DEBUG: [OPIK DEBUG llm_call] Before track_openai: trace=None ❌
```

**After fix:**
```
DEBUG: [OPIK DEBUG llm_call] Before track_openai: trace=019bb54f-a19a... ✅
DEBUG: [OPIK DEBUG llm_call] Before track_openai: trace=019bb54f-a19a... ✅
```

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-12 | 0.1 | Initial story with debugging attempts documented | Claude Opus 4.5 |
| 2026-01-12 | 0.2 | Added debug logging to llm_actions.py, restored track_openai, created unit tests | Claude Opus 4.5 |
| 2026-01-12 | 1.0 | **FIXED**: Modified ThreadExecutor.submit() to propagate contextvars (parallel flows) | Claude Opus 4.5 |
| 2026-01-12 | 1.1 | **FIXED**: Added wrapper span in cli.py to preserve trace context (sequential flows) | Claude Opus 4.5 |

## Files Created/Modified

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/parallel_executors.py` | Modified | Added `contextvars` import, updated `ThreadExecutor.submit()` to propagate context |
| `python/src/the_edge_agent/cli.py` | Modified | Added wrapper span creation to prevent trace from being popped during sequential execution |
| `firebase/functions-agents/routers/run_agent.py` | Modified | Added wrapper span creation for FastAPI endpoint (same fix as cli.py) |
| `python/src/the_edge_agent/actions/llm_actions.py` | Modified | Restored `track_openai` wrapper, added debug logging |
| `python/tests/test_opik_trace_propagation.py` | Modified | Added 3 new tests for `TestThreadExecutorContextPropagation` |
| `firebase/functions-agents/scripts/test_opik_trace_context.py` | Created | Manual test script for trace context with YAMLEngine |
| `docs/stories/TEA-BUILTIN-005.6.opik-trace-context-propagation.md` | Modified | Updated with solution details |
| `docs/stories/TEA-BUILTIN-005.opik-integration-epic.md` | Modified | Added Story 6 reference |
