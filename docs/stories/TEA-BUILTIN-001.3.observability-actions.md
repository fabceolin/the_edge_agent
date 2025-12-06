# Story TEA-BUILTIN-001.3: Observability Actions

## Status

Draft

## Story

**As a** YAML agent developer,
**I want** built-in observability actions (trace start, log, end),
**so that** I can monitor, debug, and analyze agent execution without external tooling or Python code.

## Acceptance Criteria

1. `trace.start` action begins a trace span with name, metadata, and optional parent
2. `trace.log` action records events, metrics, and state snapshots within a span
3. `trace.end` action closes a span with status and optional error details
4. Traces support hierarchical spans (parent-child relationships)
5. Trace data is accessible via configurable exporters (console, file, callback)
6. Traces capture token usage, latency, and custom metrics automatically
7. All actions follow existing `_setup_builtin_actions()` pattern
8. Actions are accessible via both `trace.*` and `actions.trace_*` namespaces
9. Comprehensive unit tests cover all observability operations
10. Documentation updated in CLAUDE.md and docs/YAML_AGENTS.md

## Dependencies

**Blocked By**: None (can start immediately)

**Blocks**: None (foundational capability used by all other stories)

**Internal Dependencies**:
- Can integrate with `llm.*` actions for auto-capture of token usage
- Can integrate with `http.*` actions for latency metrics

## User Prerequisites

- [ ] **None required** - No external services or API keys needed
- [ ] **Optional**: File write permissions for file exporter

## Tasks / Subtasks

- [ ] Task 1: Implement trace context manager (AC: 1, 4, 5)
  - [ ] Create `TraceContext` class to manage active spans
  - [ ] Implement span stack for hierarchical tracing
  - [ ] Design span data structure:
    ```python
    {
        "span_id": str,
        "parent_id": Optional[str],
        "name": str,
        "start_time": float,
        "end_time": Optional[float],
        "status": str,  # "ok", "error"
        "metadata": dict,
        "events": list,
        "metrics": dict
    }
    ```
  - [ ] Add configurable exporters (console, file, callback)
  - [ ] Inject TraceContext into graph execution

- [ ] Task 2: Implement `trace.start` action (AC: 1, 4, 7, 8)
  - [ ] Define function signature: `trace_start(state, name, metadata=None, parent_id=None, **kwargs)`
  - [ ] Create new span and push to span stack
  - [ ] Auto-parent to current span if no parent_id specified
  - [ ] Return `{"span_id": str, "name": str, "parent_id": Optional[str]}`
  - [ ] Register in actions dict with both namespaces

- [ ] Task 3: Implement `trace.log` action (AC: 2, 6, 7, 8)
  - [ ] Define function signature: `trace_log(state, message=None, event=None, metrics=None, snapshot_state=False, **kwargs)`
  - [ ] Add event to current span's events list
  - [ ] Merge metrics into span's metrics dict
  - [ ] Optionally snapshot current state (for debugging)
  - [ ] Auto-capture token usage if present in state
  - [ ] Return `{"logged": True, "span_id": str, "event_count": int}`
  - [ ] Register in actions dict with both namespaces

- [ ] Task 4: Implement `trace.end` action (AC: 3, 7, 8)
  - [ ] Define function signature: `trace_end(state, status="ok", error=None, **kwargs)`
  - [ ] Pop span from stack and set end_time
  - [ ] Set status and optional error details
  - [ ] Calculate duration
  - [ ] Export span via configured exporter
  - [ ] Return `{"span_id": str, "duration_ms": float, "status": str}`
  - [ ] Register in actions dict with both namespaces

- [ ] Task 5: Implement exporters (AC: 5)
  - [ ] Console exporter: Pretty-print spans to stdout
  - [ ] File exporter: Append JSON lines to file
  - [ ] Callback exporter: Call user-provided function
  - [ ] Make exporters configurable via `YAMLEngine(trace_exporter=...)`
  - [ ] Support multiple exporters simultaneously

- [ ] Task 6: Auto-instrumentation hooks (AC: 6)
  - [ ] Create optional auto-trace decorator for node execution
  - [ ] Auto-capture LLM token usage from `llm.call` results
  - [ ] Auto-capture HTTP latency from `http.*` results
  - [ ] Add `auto_trace: true` option to YAML config

- [ ] Task 7: Write tests (AC: 9)
  - [ ] Test trace.start creates span correctly
  - [ ] Test trace.log adds events and metrics
  - [ ] Test trace.end closes span with duration
  - [ ] Test hierarchical spans (parent-child)
  - [ ] Test each exporter type
  - [ ] Test auto-instrumentation hooks

- [ ] Task 8: Update documentation (AC: 10)
  - [ ] Add observability actions to CLAUDE.md
  - [ ] Add tracing examples to docs/YAML_AGENTS.md
  - [ ] Document exporter configuration
  - [ ] Create example YAML with full tracing

## Dev Notes

### Integration Points
- **File**: `src/the_edge_agent/yaml_engine.py`
- **Method**: `_setup_builtin_actions()` (lines 623-786)
- **Graph Integration**: May need hooks in `stategraph.py` for auto-instrumentation

### Trace Context Design
```python
class TraceContext:
    def __init__(self, exporters: List[TraceExporter]):
        self.exporters = exporters
        self.span_stack: List[Span] = []
        self.completed_spans: List[Span] = []

    def start_span(self, name: str, metadata: dict = None, parent_id: str = None) -> Span: ...
    def log_event(self, message: str = None, metrics: dict = None) -> None: ...
    def end_span(self, status: str = "ok", error: str = None) -> Span: ...
    def current_span(self) -> Optional[Span]: ...
```

### Exporter Protocol
```python
class TraceExporter(Protocol):
    def export(self, span: Span) -> None: ...

class ConsoleExporter:
    def export(self, span: Span) -> None:
        print(f"[TRACE] {span['name']} ({span['duration_ms']:.2f}ms) - {span['status']}")
```

### Key Constraints
- TraceContext must be thread-safe for parallel execution
- Exporters should not block graph execution
- Span data should be serializable for checkpoint compatibility
- Console exporter should respect verbosity settings

### Technical Dependencies
- No external dependencies required
- Optional: OpenTelemetry compatibility layer (future enhancement)

### Version Requirements
- Python: >=3.9
- No external dependencies (pure Python implementation)

### YAML Auto-Trace Example
```yaml
settings:
  auto_trace: true
  trace_exporter: file
  trace_file: ./traces.jsonl

nodes:
  - name: process
    run: |
      # Automatically traced
      return {"result": "done"}
```

## Testing

**Test File Location**: `tests/test_yaml_engine.py` (add new test class)

**Priority Levels**:
- **P0**: Critical - Core tracing, resilience
- **P1**: Core - Logging, exporters, thread safety
- **P2**: Edge cases - Parallel execution, sanitization

**Testing Standards**:
- Use `unittest` framework
- Capture console output for console exporter tests
- Use temp files for file exporter tests
- Test thread safety with parallel execution

**Unit Test Cases**:
```python
class TestObservabilityActions(unittest.TestCase):
    # P0 - Critical
    def test_trace_start_creates_span(self): ...  # (P0)
    def test_trace_start_with_parent(self): ...  # (P0)
    def test_trace_end_closes_span(self): ...  # (P0)
    def test_trace_end_calculates_duration(self): ...  # (P0)
    def test_hierarchical_spans(self): ...  # (P0)
    def test_exporter_failure_handling(self): ...  # (P0) - Exporters don't crash graph execution

    # P1 - Core functionality
    def test_trace_log_adds_event(self): ...  # (P1)
    def test_trace_log_adds_metrics(self): ...  # (P1)
    def test_trace_log_snapshots_state(self): ...  # (P1)
    def test_console_exporter(self): ...  # (P1)
    def test_file_exporter(self): ...  # (P1)
    def test_callback_exporter(self): ...  # (P1)
    def test_auto_instrumentation(self): ...  # (P1)
    def test_dual_namespace_access(self): ...  # (P1) - AC8: Verify trace.* and actions.trace_* work
    def test_trace_end_without_start(self): ...  # (P1) - Graceful error handling
    def test_span_serialization(self): ...  # (P1) - Spans can be pickled for checkpoints
    def test_concurrent_span_creation(self): ...  # (P1) - Thread safety: multiple threads

    # P2 - Edge cases
    def test_trace_log_outside_span(self): ...  # (P2) - Behavior when no active span
    def test_state_snapshot_sanitization(self): ...  # (P2) - Sensitive keys can be excluded
```

**Integration Test Cases**:
```python
class TestObservabilityActionsIntegration(unittest.TestCase):
    def test_trace_in_yaml_workflow(self): ...  # (P0)
    def test_trace_with_checkpoint(self): ...  # (P1)
    def test_trace_captures_llm_usage(self): ...  # (P1)
    def test_trace_with_parallel_execution(self): ...  # (P2)
    def test_parallel_execution_isolated_spans(self): ...  # (P1) - Parallel flows don't corrupt each other
```

**Test Summary**: 24 tests (19 unit + 5 integration) | P0: 7 | P1: 14 | P2: 3

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] All tasks completed
- [ ] Tests pass (existing and new)
- [ ] No regressions in existing YAML engine functionality
- [ ] Documentation updated
- [ ] Code follows existing patterns in yaml_engine.py

## Rollback Procedure

If observability actions cause issues in production:

1. **Immediate Rollback**:
   ```python
   # In _setup_builtin_actions(), comment out:
   # actions['trace.start'] = trace_start
   # actions['trace.log'] = trace_log
   # actions['trace.end'] = trace_end
   ```

2. **State Cleanup**:
   - TraceContext is isolated; removing actions has no side effects
   - Trace files can be safely deleted
   - No impact on workflow state or checkpoints

3. **Verification**:
   - Run existing test suite: `pytest tests/test_yaml_engine.py`
   - Verify YAML agents work without tracing

4. **Gradual Rollout** (Recommended):
   - Feature flag: `YAMLEngine(enable_tracing=False)`
   - Start with console exporter only
   - Add file exporter after validation

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-06 | 0.2 | Added Dependencies, User Prerequisites, Rollback, Integration Tests | Sarah (PO Agent) |
