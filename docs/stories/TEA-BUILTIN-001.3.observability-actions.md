# Story TEA-BUILTIN-001.3: Observability Actions

## Status

Ready for Review

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

- [x] Task 1: Implement trace context manager (AC: 1, 4, 5)
  - [x] Create `TraceContext` class to manage active spans
  - [x] Implement span stack for hierarchical tracing
  - [x] Design span data structure:
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
  - [x] Add configurable exporters (console, file, callback)
  - [x] Inject TraceContext into graph execution

- [x] Task 2: Implement `trace.start` action (AC: 1, 4, 7, 8)
  - [x] Define function signature: `trace_start(state, name, metadata=None, parent_id=None, **kwargs)`
  - [x] Create new span and push to span stack
  - [x] Auto-parent to current span if no parent_id specified
  - [x] Return `{"span_id": str, "name": str, "parent_id": Optional[str]}`
  - [x] Register in actions dict with both namespaces

- [x] Task 3: Implement `trace.log` action (AC: 2, 6, 7, 8)
  - [x] Define function signature: `trace_log(state, message=None, event=None, metrics=None, snapshot_state=False, **kwargs)`
  - [x] Add event to current span's events list
  - [x] Merge metrics into span's metrics dict
  - [x] Optionally snapshot current state (for debugging)
  - [x] Auto-capture token usage if present in state
  - [x] Return `{"logged": True, "span_id": str, "event_count": int}`
  - [x] Register in actions dict with both namespaces

- [x] Task 4: Implement `trace.end` action (AC: 3, 7, 8)
  - [x] Define function signature: `trace_end(state, status="ok", error=None, **kwargs)`
  - [x] Pop span from stack and set end_time
  - [x] Set status and optional error details
  - [x] Calculate duration
  - [x] Export span via configured exporter
  - [x] Return `{"span_id": str, "duration_ms": float, "status": str}`
  - [x] Register in actions dict with both namespaces

- [x] Task 5: Implement exporters (AC: 5)
  - [x] Console exporter: Pretty-print spans to stdout
  - [x] File exporter: Append JSON lines to file
  - [x] Callback exporter: Call user-provided function
  - [x] Make exporters configurable via `YAMLEngine(trace_exporter=...)`
  - [x] Support multiple exporters simultaneously

- [x] Task 6: Auto-instrumentation hooks (AC: 6)
  - [x] Create optional auto-trace decorator for node execution
  - [x] Auto-capture LLM token usage from `llm.call` results
  - [x] Auto-capture HTTP latency from `http.*` results
  - [x] Add `auto_trace: true` option to YAML config

- [x] Task 7: Write tests (AC: 9)
  - [x] Test trace.start creates span correctly
  - [x] Test trace.log adds events and metrics
  - [x] Test trace.end closes span with duration
  - [x] Test hierarchical spans (parent-child)
  - [x] Test each exporter type
  - [x] Test auto-instrumentation hooks

- [x] Task 8: Update documentation (AC: 10)
  - [x] Add observability actions to CLAUDE.md
  - [x] Add tracing examples to docs/YAML_AGENTS.md
  - [x] Document exporter configuration
  - [x] Create example YAML with full tracing

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

- [x] All acceptance criteria verified
- [x] All tasks completed
- [x] Tests pass (existing and new)
- [x] No regressions in existing YAML engine functionality
- [x] Documentation updated
- [x] Code follows existing patterns in yaml_engine.py

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

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5

### File List
- `src/the_edge_agent/yaml_engine.py` - Added TraceContext, exporters, trace actions
- `src/the_edge_agent/__init__.py` - Exported tracing classes
- `tests/test_yaml_engine_observability.py` - New test file (29 tests)
- `CLAUDE.md` - Added Observability Actions section
- `docs/YAML_AGENTS.md` - Added tracing documentation

### Completion Notes
- Implemented all observability actions (trace.start, trace.log, trace.end)
- Created TraceContext with thread-safe span management
- Added ConsoleExporter, FileExporter, CallbackExporter
- Implemented auto-instrumentation via `auto_trace: true` setting
- 29 new tests, all passing (258 total tests pass)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-06 | 0.2 | Added Dependencies, User Prerequisites, Rollback, Integration Tests | Sarah (PO Agent) |
| 2025-12-06 | 1.0 | Implementation complete - all tasks done, tests pass | James (Dev Agent) |

## QA Results

### Review Date: 2025-12-06

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: Excellent** - The implementation demonstrates solid engineering practices:

1. **Thread Safety**: Proper use of `threading.local()` for span stacks ensures isolation in parallel execution
2. **Protocol Design**: `TraceExporter` as a Protocol enables easy extension without coupling
3. **Graceful Degradation**: Exporter failures are silently caught, preventing graph execution crashes
4. **Documentation**: Comprehensive docstrings on all public methods

### Requirements Traceability

| AC | Requirement | Test Coverage | Status |
|----|-------------|---------------|--------|
| AC1 | trace.start with name, metadata, parent | `test_trace_start_creates_span`, `test_trace_start_with_parent` | ✓ |
| AC2 | trace.log records events/metrics/snapshots | `test_trace_log_adds_event`, `test_trace_log_adds_metrics`, `test_trace_log_snapshots_state` | ✓ |
| AC3 | trace.end closes span with status | `test_trace_end_closes_span`, `test_trace_end_calculates_duration` | ✓ |
| AC4 | Hierarchical spans | `test_hierarchical_spans` | ✓ |
| AC5 | Configurable exporters | `test_console_exporter`, `test_file_exporter`, `test_callback_exporter` | ✓ |
| AC6 | Auto-capture token usage/latency | `test_trace_captures_llm_usage`, `test_auto_instrumentation` | ✓ |
| AC7 | Follows _setup_builtin_actions pattern | Code review verified | ✓ |
| AC8 | Dual namespace access | `test_dual_namespace_access` | ✓ |
| AC9 | Comprehensive tests | 29 tests (7 P0, 17 P1, 5 P2) | ✓ |
| AC10 | Documentation updated | CLAUDE.md and YAML_AGENTS.md updated | ✓ |

### Refactoring Performed

None required - code quality is excellent.

### Compliance Check

- Coding Standards: ✓ Follows existing patterns in yaml_engine.py
- Project Structure: ✓ All files in correct locations
- Testing Strategy: ✓ Uses unittest framework with proper priority levels
- All ACs Met: ✓ All 10 acceptance criteria verified

### Improvements Checklist

- [x] Thread-safe span management implemented
- [x] Error handling for exporter failures
- [x] State sanitization for sensitive keys
- [x] Serialization support for checkpoints
- [ ] Consider adding trace sampling for high-volume scenarios (future)
- [ ] Consider OpenTelemetry compatibility layer (future)

### Security Review

✓ **No security issues found**
- Sensitive state keys can be redacted via `sanitize_keys` parameter
- Exporter errors are caught silently to prevent information leakage
- No hardcoded secrets or credentials

### Performance Considerations

✓ **No performance issues found**
- Thread-local storage (`threading.local()`) prevents lock contention
- Exporter errors are swallowed to prevent blocking graph execution
- FileExporter uses thread-safe locking only during file write

### Files Modified During Review

None - no refactoring required.

### Gate Status

**Gate: PASS** → docs/qa/gates/TEA-BUILTIN-001.3-observability-actions.yml

### Recommended Status

✓ **Ready for Done**

The implementation is complete, well-tested (29 tests, 100% AC coverage), and follows all project standards. No blocking issues identified.
