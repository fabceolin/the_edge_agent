# Story TD.7: Add Parallel Execution Support to stream()

## Status
Ready for Dev

## Story
**As a** developer debugging parallel workflows,
**I want** the `stream()` method to support parallel execution,
**so that** I can observe intermediate states from all parallel branches.

## Dependencies
- **TD.4** (Thread Safety for Fan-in) - Should be completed first to ensure thread-safe state handling
- **TD.5** (Executor Max Workers) - Should be completed first for proper executor configuration

## Acceptance Criteria
1. `stream()` handles parallel edges like `invoke()` does
2. Intermediate states from parallel flows are yielded with branch identification
3. Fan-in synchronization works correctly in stream mode
4. Parallel results are available to fan-in nodes
5. Parallel thread exceptions are handled gracefully (yield error, don't crash)
6. All existing tests pass
7. New tests cover parallel streaming scenarios

## Tasks / Subtasks
- [ ] Analyze current `invoke()` parallel implementation (lines 186-288)
- [ ] Port parallel edge detection to `stream()` (after line 526)
- [ ] Implement queue-based parallel flow streaming
- [ ] Handle fan-in node in stream context
- [ ] Add `parallel_results` to stream's available params
- [ ] Implement error handling for parallel thread failures
- [ ] Add comprehensive tests for parallel streaming
- [ ] Update docstrings

## Dev Notes

### File Location
- `src/the_edge_agent/stategraph.py`

### Issue Details
Currently `stream()` (lines 500-560) only handles normal successors via `_get_next_node()`. It ignores parallel edges entirely, meaning:
- Parallel workflows silently fail in stream mode
- No intermediate visibility into parallel branches

### Yield Format Specification

**Standard node yield (existing):**
```python
{"type": "state", "node": "node_name", "state": {...}}
```

**Parallel branch yield (new):**
```python
{"type": "parallel_state", "branch": "branch_start_node", "node": "current_node", "state": {...}}
```

**Parallel error yield (new):**
```python
{"type": "parallel_error", "branch": "branch_start_node", "node": "failed_node", "error": "message", "state": {...}}
```

**Fan-in yield (existing pattern):**
```python
{"type": "state", "node": "fan_in_node", "state": {...}}  # includes parallel_results
```

### Implementation Approach

**Recommended: Queue-based collection**

```python
from queue import Queue
from threading import Thread

def _stream_parallel_flow(self, start_node, state, config, fan_in_node, result_queue):
    """Execute a parallel flow and put yields into queue."""
    current = start_node
    flow_state = copy.deepcopy(state)

    while current != fan_in_node and current != END:
        # Execute node
        node_data = self.node(current)
        run_func = node_data.get("run")
        if run_func:
            try:
                result = self._execute_node_function(run_func, flow_state, config, current)
                flow_state.update(result)
                # Put yield into queue with branch identification
                result_queue.put({
                    "type": "parallel_state",
                    "branch": start_node,
                    "node": current,
                    "state": flow_state.copy()
                })
            except Exception as e:
                result_queue.put({
                    "type": "parallel_error",
                    "branch": start_node,
                    "node": current,
                    "error": str(e),
                    "state": flow_state.copy()
                })
                return  # Stop this branch on error

        current = self._get_next_node(current, flow_state, config)

    # Signal branch completion
    result_queue.put({"type": "branch_complete", "branch": start_node, "state": flow_state})
```

### Edge Cases & Error Handling

| Scenario | Behavior |
|----------|----------|
| Parallel thread throws exception | Yield `parallel_error`, continue other branches, fan-in receives partial results |
| All parallel threads fail | Fan-in still executes with empty/partial `parallel_results` |
| Queue polling timeout | Use reasonable timeout (e.g., 0.1s), continue polling until all branches complete |
| Yield ordering | **Non-deterministic** - yields from different branches may interleave. Document this behavior. |

### Complexity Warning
This is the most complex story in the TD epic. The threading + generator combination requires careful queue management. Consider if this complexity is justified for your use case - `invoke()` may be sufficient for most parallel workflows.

### Testing

**Test file:** `tests/test_stategraph.py`
**Run:** `pytest tests/test_stategraph.py -v -k parallel`

**Required Test Scenarios:**

1. **Basic parallel streaming** - 2 branches, verify all intermediate states yielded
2. **3-branch parallel streaming** - verify fan-in receives all 3 results
3. **Parallel streaming with one branch failure** - verify error yielded, other branches continue, fan-in executes
4. **Parallel streaming with all branches failing** - verify graceful handling
5. **Verify parallel_results available to fan-in** - same as invoke() behavior
6. **Verify yield types** - check `parallel_state`, `parallel_error`, `branch_complete` types present
7. **Interrupt support in parallel streaming** - if time permits, verify interrupts work within parallel branches

**Test Template:**
```python
def test_stream_parallel_basic(self):
    """Test that stream() handles basic parallel execution."""
    graph = tea.StateGraph({"value": int, "results": list})

    def branch_a(state):
        return {"results": state.get("results", []) + ["a"]}

    def branch_b(state):
        return {"results": state.get("results", []) + ["b"]}

    def fan_in(state, parallel_results):
        combined = []
        for r in parallel_results:
            combined.extend(r.get("results", []))
        return {"results": combined}

    graph.add_node("start", run=lambda state: state)
    graph.add_node("branch_a", run=branch_a)
    graph.add_node("branch_b", run=branch_b)
    graph.add_fanin_node("fan_in", run=fan_in)
    graph.add_node("end", run=lambda state: state)

    graph.set_entry_point("start")
    graph.add_parallel_edge("start", "branch_a", "fan_in")
    graph.add_parallel_edge("start", "branch_b", "fan_in")
    graph.add_edge("fan_in", "end")
    graph.set_finish_point("end")

    compiled = graph.compile()
    events = list(compiled.stream({"value": 1}))

    # Verify we got parallel_state events from both branches
    parallel_events = [e for e in events if e.get("type") == "parallel_state"]
    branches_seen = set(e.get("branch") for e in parallel_events)
    self.assertEqual(branches_seen, {"branch_a", "branch_b"})

    # Verify final state has combined results
    final = [e for e in events if e.get("type") == "final"][0]
    self.assertIn("a", final["state"]["results"])
    self.assertIn("b", final["state"]["results"])
```

## Change Log
| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-06 | 0.1 | Initial draft | Sarah (PO) |
| 2024-12-06 | 0.2 | Revised per checklist - added dependencies, edge cases, yield format, test scenarios | Sarah (PO) |

## QA Results

### Review Date: 2025-12-06

### Reviewed By: Quinn (Test Architect)

### Review Type: Pre-Implementation Story Review

This is a pre-implementation review. All tasks are unchecked (not started). Dependencies TD.4 and TD.5 are both **Done** ✓

### Risk Assessment

**Risk Level: MEDIUM-HIGH**

| Risk Factor | Assessment |
|-------------|------------|
| Threading + Generators | HIGH RISK pattern - inherently complex |
| Expected diff size | ~150-200 lines (moderate) |
| Acceptance criteria | 7 ACs (medium complexity) |
| Dependencies | TD.4 ✓ TD.5 ✓ (both complete) |

### Story Quality Assessment

**Strengths:**
1. Clear dependencies - TD.4/TD.5 prerequisite explicit
2. Yield format specification - 4 event types well-defined
3. Implementation approach - Queue-based pattern with working code
4. Edge cases table - 4 scenarios documented
5. Test template - Complete working test example
6. Complexity warning - Honest about difficulty

**Gaps Found:**
1. Queue exhaustion not addressed - What if queue fills up?
2. Cleanup on early termination - What if user stops iterating mid-stream?
3. `branch_complete` event not in AC - Mentioned in code but not acceptance criteria
4. Line numbers may be stale - Story references lines 500-560, actual stream() starts at line 530

### Requirements Traceability

All 7 ACs can be mapped to Given-When-Then test scenarios. Test coverage plan is adequate with 7 specified scenarios. Recommended 4 additional scenarios:
- Empty parallel results (fan-in with 0 branches)
- Mixed parallel + sequential edges
- Queue timeout behavior
- Thread safety under load (builds on TD.4)

### NFR Assessment

| NFR | Status | Notes |
|-----|--------|-------|
| Security | PASS | Internal threading only, no external exposure |
| Performance | CONCERNS | Queue polling timeout could add latency |
| Reliability | CONCERNS | No guidance on queue.put() blocking |
| Maintainability | PASS | Good documentation, clear patterns |

### Testability Evaluation

| Dimension | Score | Notes |
|-----------|-------|-------|
| Controllability | 8/10 | Can construct test graphs, control timing |
| Observability | 9/10 | Events make state observable |
| Debuggability | 6/10 | Thread interleaving complicates debugging |

### Recommendations Before Implementation

1. **Add to Edge Cases table:** Queue exhaustion scenario (use `queue.put(item, timeout=X)`)
2. **Add to Edge Cases table:** Early termination cleanup (generator not fully consumed)
3. **Update AC or remove:** `branch_complete` event - either add to AC2 or remove from implementation
4. **Verify line numbers:** Confirm stream() location before implementation begins

### Gate Status

Gate: CONCERNS → docs/qa/gates/TD.7-stream-parallel-support.yml

### Recommended Status

Story is ready for implementation with noted concerns. Developer should address gaps during implementation or flag for story revision.

**[Ready for Implementation with CONCERNS]** - Gaps are addressable during implementation
