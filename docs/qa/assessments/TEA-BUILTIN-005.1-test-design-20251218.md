# Test Design: Story TEA-BUILTIN-005.1 (OpikExporter Backend)

Date: 2025-12-18
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 16
- Unit tests: 11 (69%)
- Integration tests: 5 (31%)
- E2E tests: 0 (0%)
- Priority distribution: P0: 5, P1: 7, P2: 4

### Rationale

This story implements a new exporter backend that integrates with the existing `TraceExporter` protocol. The testing strategy emphasizes:

1. **Unit tests** for OpikExporter class logic, span conversion, and error handling
2. **Integration tests** for YAMLEngine integration and TraceContext interoperability
3. **No E2E tests** required - external Opik service calls are mocked; real E2E validation is manual/staging

The critical paths are: graceful degradation (no SDK crashes), span hierarchy preservation, and YAMLEngine configuration.

---

## Test Scenarios by Acceptance Criteria

### AC1: OpikExporter implements TraceExporter protocol

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.1-UNIT-001 | Unit | P0 | Verify OpikExporter has `export(span: dict)` method | Protocol compliance is foundational |
| 005.1-UNIT-002 | Unit | P1 | Verify OpikExporter accepts span dict parameter | Type signature must match protocol |
| 005.1-UNIT-003 | Unit | P2 | Verify OpikExporter can be assigned to TraceExporter type hint | Python typing validation |

#### Scenarios

```python
def test_opik_exporter_has_export_method(self):
    """(P0) OpikExporter must implement export(span) method."""
    exporter = OpikExporter()
    assert hasattr(exporter, 'export')
    assert callable(exporter.export)

def test_opik_exporter_export_accepts_span_dict(self):
    """(P1) Export method accepts span dictionary."""
    exporter = OpikExporter()
    span = {"name": "test", "span_id": "123"}
    # Should not raise TypeError
    exporter.export(span)

def test_opik_exporter_satisfies_protocol(self):
    """(P2) Type system recognizes OpikExporter as TraceExporter."""
    from the_edge_agent.tracing import TraceExporter
    exporter = OpikExporter()
    # Protocol structural typing check
    assert isinstance(exporter, TraceExporter) or hasattr(exporter, 'export')
```

---

### AC2: Spans exported with name, duration, status, metadata, events, metrics

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.1-UNIT-004 | Unit | P0 | Verify span name is passed to Opik | Core field mapping |
| 005.1-UNIT-005 | Unit | P0 | Verify duration_ms is calculated/passed | Critical for latency analysis |
| 005.1-UNIT-006 | Unit | P1 | Verify status field maps correctly (ok->completed, error->error) | Status must be interpretable in Opik |
| 005.1-UNIT-007 | Unit | P1 | Verify metadata dict is forwarded | Custom context must be preserved |
| 005.1-UNIT-008 | Unit | P2 | Verify events list is processed | Events are secondary telemetry |
| 005.1-UNIT-009 | Unit | P1 | Verify metrics (token counts) are forwarded | Token usage is key for LLM observability |

#### Scenarios

```python
def test_export_span_name(self):
    """(P0) Span name is passed to Opik SDK."""
    with patch('opik.track') as mock_track:
        exporter = OpikExporter()
        exporter.export({"name": "llm.call", "span_id": "abc"})
        # Verify name was used in Opik call
        mock_track.assert_called()
        call_kwargs = mock_track.call_args
        assert "llm.call" in str(call_kwargs)

def test_export_duration_ms(self):
    """(P0) Duration is exported to Opik."""
    span = {
        "name": "test",
        "span_id": "123",
        "start_time": 1000.0,
        "end_time": 1001.5,
        "duration_ms": 1500.0
    }
    # Verify duration is captured

def test_export_status_mapping(self):
    """(P1) Status 'ok' maps to Opik 'completed', 'error' maps to 'error'."""
    exporter = OpikExporter()
    ok_span = {"name": "test", "status": "ok"}
    error_span = {"name": "test", "status": "error", "error": "Failed"}
    # Verify correct status mapping

def test_export_metadata_forwarded(self):
    """(P1) Metadata dict is passed to Opik span."""
    span = {
        "name": "test",
        "metadata": {"model": "gpt-4", "temperature": 0.7}
    }
    # Verify metadata appears in Opik call

def test_export_events_processed(self):
    """(P2) Events list is logged or attached to span."""
    span = {
        "name": "test",
        "events": [
            {"timestamp": 1000.0, "message": "Started"},
            {"timestamp": 1001.0, "message": "Completed"}
        ]
    }
    # Verify events are handled

def test_export_metrics_token_counts(self):
    """(P1) Token usage metrics are forwarded."""
    span = {
        "name": "llm.call",
        "metrics": {
            "prompt_tokens": 150,
            "completion_tokens": 50,
            "total_tokens": 200
        }
    }
    # Verify metrics are exported
```

---

### AC3: YAMLEngine(trace_exporter="opik") enables Opik export

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.1-INT-001 | Integration | P0 | Verify YAMLEngine accepts trace_exporter="opik" | Core integration point |
| 005.1-INT-002 | Integration | P1 | Verify OpikExporter is instantiated when trace_exporter="opik" | Lazy instantiation must work |
| 005.1-INT-003 | Integration | P1 | Verify spans from TraceContext are sent to OpikExporter | End-to-end span flow |

#### Scenarios

```python
def test_yaml_engine_accepts_opik_exporter(self):
    """(P0) YAMLEngine constructor accepts trace_exporter='opik'."""
    with patch.dict('sys.modules', {'opik': MagicMock()}):
        engine = YAMLEngine(trace_exporter="opik")
        assert engine is not None

def test_yaml_engine_creates_opik_exporter(self):
    """(P1) OpikExporter is instantiated when trace_exporter='opik'."""
    with patch('the_edge_agent.exporters.opik_exporter.OpikExporter') as mock:
        engine = YAMLEngine(trace_exporter="opik")
        # Trigger exporter creation
        mock.assert_called()

def test_trace_context_sends_to_opik_exporter(self):
    """(P1) Spans from TraceContext are exported via OpikExporter."""
    with patch.dict('sys.modules', {'opik': MagicMock()}):
        engine = YAMLEngine(trace_exporter="opik")
        # Create span and verify it reaches OpikExporter
```

---

### AC4: Clear ImportError message if opik not installed

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.1-UNIT-010 | Unit | P0 | Verify ImportError raised with install instructions | Critical for user experience |
| 005.1-UNIT-011 | Unit | P1 | Verify error message includes "pip install opik" | Actionable guidance |

#### Scenarios

```python
def test_missing_sdk_raises_import_error(self):
    """(P0) ImportError raised when opik SDK not installed."""
    with patch.dict('sys.modules', {'opik': None}):
        with pytest.raises(ImportError) as exc_info:
            OpikExporter()
        assert "opik" in str(exc_info.value).lower()

def test_import_error_includes_install_instructions(self):
    """(P1) Error message includes pip install command."""
    with patch.dict('sys.modules', {'opik': None}):
        try:
            OpikExporter()
        except ImportError as e:
            assert "pip install opik" in str(e)
```

---

### AC5: Environment variables respected

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.1-UNIT-012 | Unit | P1 | Verify OPIK_API_KEY is read from environment | Authentication config |
| 005.1-UNIT-013 | Unit | P1 | Verify OPIK_PROJECT_NAME defaults to "the-edge-agent" | Default project naming |
| 005.1-UNIT-014 | Unit | P2 | Verify OPIK_WORKSPACE is read from environment | Workspace isolation |
| 005.1-UNIT-015 | Unit | P2 | Verify OPIK_URL_OVERRIDE configures self-hosted URL | Self-hosted support |

#### Scenarios

```python
@patch.dict(os.environ, {"OPIK_API_KEY": "test-key-123"})
def test_api_key_from_environment(self):
    """(P1) OPIK_API_KEY environment variable is used."""
    exporter = OpikExporter()
    assert exporter._api_key == "test-key-123"

def test_default_project_name(self):
    """(P1) Default project name is 'the-edge-agent'."""
    exporter = OpikExporter()
    assert exporter._project_name == "the-edge-agent"

@patch.dict(os.environ, {"OPIK_WORKSPACE": "my-team"})
def test_workspace_from_environment(self):
    """(P2) OPIK_WORKSPACE environment variable is used."""
    exporter = OpikExporter()
    assert exporter._workspace == "my-team"

@patch.dict(os.environ, {"OPIK_URL_OVERRIDE": "https://opik.mycompany.com"})
def test_url_override_from_environment(self):
    """(P2) OPIK_URL_OVERRIDE configures custom URL."""
    exporter = OpikExporter()
    assert exporter._url_override == "https://opik.mycompany.com"
```

---

### AC6: Hierarchical spans preserve parent-child relationships

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.1-INT-004 | Integration | P0 | Verify parent_id is mapped to parent_span_id | Critical for trace visualization |
| 005.1-INT-005 | Integration | P1 | Verify multi-level hierarchy (grandparent->parent->child) | Deep traces must work |

#### Scenarios

```python
def test_parent_child_span_relationship(self):
    """(P0) parent_id is correctly mapped to Opik's parent_span_id."""
    exporter = OpikExporter()
    parent_span = {"span_id": "parent-123", "name": "parent", "parent_id": None}
    child_span = {"span_id": "child-456", "name": "child", "parent_id": "parent-123"}

    exporter.export(parent_span)
    exporter.export(child_span)
    # Verify child references parent in Opik

def test_multi_level_hierarchy(self):
    """(P1) Three-level hierarchy is preserved."""
    exporter = OpikExporter()
    root = {"span_id": "root", "name": "root", "parent_id": None}
    middle = {"span_id": "middle", "name": "middle", "parent_id": "root"}
    leaf = {"span_id": "leaf", "name": "leaf", "parent_id": "middle"}

    for span in [root, middle, leaf]:
        exporter.export(span)
    # Verify full hierarchy in Opik
```

---

### AC7: Token usage metrics forwarded to Opik

*(Covered in AC2 tests - 005.1-UNIT-009)*

---

### AC8: Exporter errors don't crash graph execution (graceful degradation)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.1-UNIT-016 | Unit | P0 | Verify export errors are caught and don't propagate | Critical reliability requirement |

#### Scenarios

```python
def test_export_error_graceful_degradation(self):
    """(P0) Errors during export don't crash graph execution."""
    exporter = OpikExporter()
    with patch.object(exporter, '_send_to_opik', side_effect=Exception("Network error")):
        # Should not raise
        exporter.export({"name": "test", "span_id": "123"})
        # Test passes if no exception propagates
```

---

### AC9: Unit tests cover export success, missing SDK, configuration, error handling

*(This AC defines test coverage requirements - satisfied by tests above)*

---

### AC10: Documentation updated in CLAUDE.md

*(Documentation task - no automated test, verified in code review)*

---

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| SDK not installed crashes app | 005.1-UNIT-010, 005.1-UNIT-011 | Clear ImportError with install instructions |
| Export failure crashes graph | 005.1-UNIT-016 | try/except wrapper, graceful degradation |
| Span hierarchy lost | 005.1-INT-004, 005.1-INT-005 | parent_id mapping preserved |
| Configuration not respected | 005.1-UNIT-012 to 015 | Environment variable tests |
| YAMLEngine integration broken | 005.1-INT-001 to 003 | Integration test suite |

---

## Recommended Execution Order

1. P0 Unit tests (005.1-UNIT-001, 004, 005, 010, 016) - fail fast on critical issues
2. P0 Integration tests (005.1-INT-001, 004) - verify core integration
3. P1 Unit tests (005.1-UNIT-002, 006, 007, 009, 011, 012, 013)
4. P1 Integration tests (005.1-INT-002, 003, 005)
5. P2 Unit tests (005.1-UNIT-003, 008, 014, 015) - as time permits

---

## Test Environment Requirements

- **Mocking**: `opik` SDK must be mocked for all tests (no real Opik calls)
- **Environment variables**: Use `unittest.mock.patch.dict(os.environ, ...)`
- **Import mocking**: Use `unittest.mock.patch.dict('sys.modules', ...)` for missing SDK tests
- **Test framework**: `unittest` (project standard)

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for wiring)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (005.1-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
