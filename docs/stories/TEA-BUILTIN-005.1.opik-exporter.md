# Story TEA-BUILTIN-005.1: OpikExporter Backend

## Status

Ready for Review

## Story

**As a** YAML agent developer,
**I want** to export traces to Comet Opik using the existing TraceContext infrastructure,
**so that** I can visualize, analyze, and debug agent execution in the Opik dashboard without writing custom instrumentation code.

## Acceptance Criteria

1. `OpikExporter` class implements the `TraceExporter` protocol from `tracing.py`
2. Spans are exported to Opik with: name, duration, status, metadata, events, metrics
3. `YAMLEngine(trace_exporter="opik")` enables Opik export automatically
4. Clear `ImportError` message if `opik` package not installed, with install instructions
5. Environment variables respected: `OPIK_API_KEY`, `OPIK_PROJECT_NAME`, `OPIK_WORKSPACE`, `OPIK_URL_OVERRIDE`
6. Hierarchical spans preserve parent-child relationships via `parent_span_id`
7. Token usage metrics (`prompt_tokens`, `completion_tokens`, `total_tokens`) forwarded to Opik
8. Exporter errors don't crash graph execution (graceful degradation)
9. Unit tests cover: export success, missing SDK, configuration, error handling
10. Documentation updated in CLAUDE.md with OpikExporter usage examples

## Dependencies

**Blocked By:** None (can start immediately)

**Blocks:** TEA-BUILTIN-005.2 (Native LLM instrumentation builds on this)

**Internal Dependencies:**
- Uses existing `TraceExporter` protocol from `tracing.py`
- Integrates with `YAMLEngine` trace_exporter configuration

## User Prerequisites

- [ ] **Optional**: `pip install opik` - Required to enable Opik export
- [ ] **Optional**: `OPIK_API_KEY` environment variable for Opik Cloud
- [ ] **None required for local Opik**: Self-hosted Opik doesn't require API key

## Tasks / Subtasks

- [x] Task 1: Create OpikExporter class (AC: 1, 2, 8)
  - [x] Create new file `src/the_edge_agent/exporters/__init__.py`
  - [x] Create `src/the_edge_agent/exporters/opik_exporter.py`
  - [x] Implement `TraceExporter` protocol with `export(span: dict)` method
  - [x] Convert TEA span format to Opik span format
  - [x] Handle exporter errors gracefully (try/except, don't propagate)
  - [x] Add logging for export failures (debug level)

- [x] Task 2: Implement Opik SDK integration (AC: 2, 6, 7)
  - [x] Use `opik.track()` decorator or context manager pattern
  - [x] Map span fields: name, duration, status, metadata
  - [x] Map parent_id to parent_span_id for hierarchy
  - [x] Forward metrics dict (especially token counts) to Opik
  - [x] Handle events list (log as Opik events or metadata)

- [x] Task 3: Configuration and environment variables (AC: 5)
  - [x] Read `OPIK_API_KEY` from environment
  - [x] Read `OPIK_PROJECT_NAME` (default: "the-edge-agent")
  - [x] Read `OPIK_WORKSPACE` (default: user's default)
  - [x] Read `OPIK_URL_OVERRIDE` for self-hosted Opik
  - [x] Initialize Opik client with configuration on first export

- [x] Task 4: Integrate with YAMLEngine (AC: 3)
  - [x] Add "opik" to recognized trace_exporter values in YAMLEngine
  - [x] Lazy-load OpikExporter on first use
  - [x] Pass configuration to OpikExporter constructor
  - [x] Update `_create_trace_exporter()` method or equivalent

- [x] Task 5: Graceful degradation for missing SDK (AC: 4, 8)
  - [x] Wrap `import opik` in try/except
  - [x] Raise clear `ImportError` with install instructions
  - [x] Log warning and skip export if SDK unavailable at runtime
  - [x] Test behavior when opik not installed

- [x] Task 6: Write tests (AC: 9)
  - [x] Test OpikExporter initialization
  - [x] Test span export (mock Opik SDK)
  - [x] Test hierarchical span export
  - [x] Test metrics forwarding
  - [x] Test error handling (exporter failure doesn't crash)
  - [x] Test missing SDK graceful degradation
  - [x] Test environment variable configuration

- [x] Task 7: Update documentation (AC: 10)
  - [x] Add OpikExporter section to CLAUDE.md
  - [x] Document environment variables
  - [x] Add YAML configuration example
  - [x] Add Python configuration example

## Dev Notes

### Integration Points

- **Primary File**: `src/the_edge_agent/exporters/opik_exporter.py` (new)
- **Integration File**: `src/the_edge_agent/yaml_engine.py` (modify trace_exporter handling)
- **Protocol Reference**: `src/the_edge_agent/tracing.py` (TraceExporter protocol)

### Span Format Mapping

```python
# TEA TraceContext span format
tea_span = {
    "span_id": "uuid-string",
    "parent_id": "uuid-string-or-none",
    "name": "operation_name",
    "start_time": 1702900000.123,  # Unix timestamp
    "end_time": 1702900001.456,
    "duration_ms": 1333.0,
    "status": "ok",  # or "error"
    "error": None,  # or error message
    "metadata": {"key": "value"},
    "events": [{"timestamp": ..., "message": "..."}],
    "metrics": {"prompt_tokens": 100, "completion_tokens": 50}
}

# Opik span format (via SDK)
# Uses @track decorator or opik.start_as_current_span()
# Key mappings:
#   name -> span name
#   metadata -> metadata dict
#   metrics -> logged as attributes or metrics
#   parent_id -> parent context propagation
#   duration -> calculated from start/end time
```

### OpikExporter Implementation Pattern

```python
class OpikExporter:
    """Export spans to Comet Opik platform."""

    def __init__(
        self,
        api_key: Optional[str] = None,
        project_name: Optional[str] = None,
        workspace: Optional[str] = None,
        url_override: Optional[str] = None
    ):
        self._configured = False
        self._api_key = api_key or os.getenv("OPIK_API_KEY")
        self._project_name = project_name or os.getenv("OPIK_PROJECT_NAME", "the-edge-agent")
        self._workspace = workspace or os.getenv("OPIK_WORKSPACE")
        self._url_override = url_override or os.getenv("OPIK_URL_OVERRIDE")

    def _ensure_configured(self) -> None:
        """Lazy configuration on first export."""
        if self._configured:
            return
        try:
            import opik
            opik.configure(
                api_key=self._api_key,
                workspace=self._workspace,
                project_name=self._project_name,
                url=self._url_override
            )
            self._configured = True
        except ImportError:
            raise ImportError(
                "Opik SDK not installed. Install with: pip install opik"
            )

    def export(self, span: Dict[str, Any]) -> None:
        """Export a completed span to Opik."""
        try:
            self._ensure_configured()
            # Convert and send span to Opik
            # ...
        except Exception:
            # Graceful degradation - don't crash graph execution
            pass
```

### Key Constraints

- OpikExporter must NOT block graph execution on export failures
- Configuration should be lazy (only connect to Opik when first span exported)
- Must handle case where user has local Opik vs Opik Cloud
- Parent-child span relationships must be preserved for trace visualization

### Optional Dependency Handling

```python
# In setup.py or pyproject.toml
extras_require = {
    "opik": ["opik>=1.0.0"],
}

# Usage: pip install the-edge-agent[opik]
```

## Testing

**Test File Location**: `tests/test_opik_exporter.py` (new file)

**Testing Standards**:
- Use `unittest` framework
- Mock Opik SDK to avoid external service calls
- Use `unittest.mock.patch` for import mocking

**Unit Test Cases**:

```python
class TestOpikExporter(unittest.TestCase):
    # P0 - Critical
    def test_export_span_success(self): ...  # (P0) Basic export works
    def test_export_preserves_hierarchy(self): ...  # (P0) Parent-child spans
    def test_export_failure_graceful(self): ...  # (P0) Errors don't propagate

    # P1 - Core functionality
    def test_configuration_from_env(self): ...  # (P1) Env vars read correctly
    def test_configuration_override(self): ...  # (P1) Constructor params override env
    def test_metrics_forwarded(self): ...  # (P1) Token counts exported
    def test_metadata_forwarded(self): ...  # (P1) Custom metadata exported
    def test_lazy_configuration(self): ...  # (P1) Only configure on first export

    # P2 - Edge cases
    def test_missing_sdk_error_message(self): ...  # (P2) Clear install instructions
    def test_empty_span_export(self): ...  # (P2) Minimal span works
    def test_events_handling(self): ...  # (P2) Events list processed

class TestOpikExporterIntegration(unittest.TestCase):
    def test_yaml_engine_opik_exporter(self): ...  # (P0) trace_exporter="opik" works
    def test_trace_context_with_opik(self): ...  # (P1) Full trace flow
```

**Test Summary**: 12 tests (10 unit + 2 integration) | P0: 4 | P1: 6 | P2: 3

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] All tasks completed
- [ ] Tests pass (existing and new)
- [ ] No regressions in existing tracing functionality
- [ ] Documentation updated
- [ ] Code follows existing patterns in tracing.py

## Rollback Procedure

If OpikExporter causes issues:

1. **Immediate Disable**:
   ```python
   # Change trace_exporter config
   engine = YAMLEngine(trace_exporter="console")  # Instead of "opik"
   ```

2. **Remove Integration**:
   - OpikExporter is isolated in separate module
   - Remove import from yaml_engine.py
   - Existing exporters unaffected

3. **Verification**:
   - Run existing test suite: `pytest tests/test_yaml_engine_observability.py`
   - Verify Console/File exporters still work

## QA Results

### Test Design Review

**Reviewer:** Quinn (Test Architect)
**Date:** 2025-12-18
**Status:** ✅ Test Design Complete

#### Test Design Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 16 |
| Unit Tests | 11 (69%) |
| Integration Tests | 5 (31%) |
| E2E Tests | 0 (0%) |
| P0 (Critical) | 5 |
| P1 (High) | 7 |
| P2 (Medium) | 4 |

#### Test Design Document

**Location:** `docs/qa/assessments/TEA-BUILTIN-005.1-test-design-20251218.md`

#### Coverage Analysis

| Acceptance Criteria | Test Coverage | Priority Tests |
|---------------------|---------------|----------------|
| AC1: TraceExporter protocol | 005.1-UNIT-001, 002, 003 | P0, P1, P2 |
| AC2: Span field export | 005.1-UNIT-004 to 009 | P0, P1, P2 |
| AC3: YAMLEngine integration | 005.1-INT-001, 002, 003 | P0, P1 |
| AC4: ImportError message | 005.1-UNIT-010, 011 | P0, P1 |
| AC5: Environment variables | 005.1-UNIT-012 to 015 | P1, P2 |
| AC6: Parent-child hierarchy | 005.1-INT-004, 005 | P0, P1 |
| AC7: Token metrics | (Covered in AC2) | P1 |
| AC8: Graceful degradation | 005.1-UNIT-016 | P0 |
| AC9: Unit test coverage | (Meta requirement) | - |
| AC10: Documentation | (Manual review) | - |

#### Key Risk Mitigations

- **SDK not installed crashes app**: Tests 005.1-UNIT-010, 011 verify clear ImportError with install instructions
- **Export failure crashes graph**: Test 005.1-UNIT-016 verifies graceful degradation
- **Span hierarchy lost**: Tests 005.1-INT-004, 005 verify parent_id mapping
- **Configuration ignored**: Tests 005.1-UNIT-012 to 015 verify environment variable handling

#### Recommendations

1. **P0 tests must pass** before story can be marked Ready for Dev
2. Mock `opik` SDK in all tests to avoid external service dependencies
3. Use `unittest.mock.patch.dict('sys.modules', ...)` for import mocking

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-18 | 0.3 | Implementation complete | James (Dev Agent) |
| 2025-12-18 | 0.2 | Added QA Results - Test Design | Quinn (QA Agent) |
| 2025-12-18 | 0.1 | Initial story draft | Sarah (PO Agent) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

**New Files:**
- `src/the_edge_agent/exporters/__init__.py` - Exporters package init with OpikExporter export
- `src/the_edge_agent/exporters/opik_exporter.py` - OpikExporter implementation
- `tests/test_opik_exporter.py` - Unit and integration tests (20 tests)

**Modified Files:**
- `src/the_edge_agent/__init__.py` - Added OpikExporter to package exports
- `src/the_edge_agent/yaml_engine.py` - Added "opik" trace_exporter support
- `CLAUDE.md` - Added OpikExporter documentation section

### Debug Log References

None - implementation completed without issues.

### Completion Notes

1. All 7 tasks completed successfully
2. 20 new tests added, all passing
3. Observability tests (49 total) all pass
4. Pre-existing LLM test failures in test_llm_call_consolidation.py and test_yaml_engine_llm.py are unrelated to this story
5. OpikExporter uses lazy configuration - only connects to Opik on first span export
6. Graceful degradation: export failures are logged at debug level and don't crash graph execution
7. Clear ImportError with "pip install opik" instructions when SDK not installed
