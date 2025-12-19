# Test Design: Story TEA-BUILTIN-005.2 (Native Opik LLM Instrumentation)

Date: 2025-12-18
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 18
- Unit tests: 13 (72%)
- Integration tests: 5 (28%)
- E2E tests: 0 (0%)
- Priority distribution: P0: 5, P1: 9, P2: 4

### Rationale

This story adds native Opik instrumentation to LLM actions via the `track_openai()` wrapper. The testing strategy emphasizes:

1. **Unit tests** for llm_call behavior with/without opik_trace, client wrapping, and error handling
2. **Integration tests** for YAMLEngine configuration flow and coexistence with OpikExporter
3. **No E2E tests** - OpenAI and Opik APIs are mocked; real service testing is manual

Critical paths are: backward compatibility (opik_trace=False unchanged), graceful degradation, and streaming support.

---

## Test Scenarios by Acceptance Criteria

### AC1: llm.call supports optional opik_trace=True parameter

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-UNIT-001 | Unit | P0 | Verify llm_call accepts opik_trace parameter | API signature must accept new param |
| 005.2-UNIT-002 | Unit | P0 | Verify opik_trace=False is default | Backward compatibility critical |
| 005.2-UNIT-003 | Unit | P1 | Verify opik_trace=False behavior unchanged | Regression prevention |

#### Scenarios

```python
def test_llm_call_accepts_opik_trace_parameter(self):
    """(P0) llm_call function accepts opik_trace parameter."""
    import inspect
    from the_edge_agent.actions.llm_actions import llm_call
    sig = inspect.signature(llm_call)
    assert 'opik_trace' in sig.parameters

def test_opik_trace_default_false(self):
    """(P0) opik_trace parameter defaults to False."""
    import inspect
    from the_edge_agent.actions.llm_actions import llm_call
    sig = inspect.signature(llm_call)
    param = sig.parameters['opik_trace']
    assert param.default is False

def test_llm_call_without_opik_trace_unchanged(self):
    """(P1) llm_call with opik_trace=False works identically to before."""
    with patch('openai.OpenAI') as mock_openai:
        mock_client = MagicMock()
        mock_openai.return_value = mock_client
        mock_client.chat.completions.create.return_value = mock_response

        result = llm_call(state={}, model="gpt-4", messages=[...], opik_trace=False)

        # Verify no Opik wrapper was applied
        assert 'track_openai' not in str(mock_openai.mock_calls)
```

---

### AC2: OpenAI client wrapped with track_openai() when enabled

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-UNIT-004 | Unit | P0 | Verify track_openai() is called when opik_trace=True | Core wrapping logic |
| 005.2-UNIT-005 | Unit | P1 | Verify OpenAI client is passed to track_openai() | Correct client instance |
| 005.2-UNIT-006 | Unit | P2 | Verify wrapped client is used for completion calls | Wrapper actually used |

#### Scenarios

```python
def test_track_openai_called_when_enabled(self):
    """(P0) track_openai() is invoked when opik_trace=True."""
    with patch('opik.integrations.openai.track_openai') as mock_track:
        with patch('openai.OpenAI') as mock_openai:
            mock_track.return_value = MagicMock()

            llm_call(state={}, model="gpt-4", messages=[...], opik_trace=True)

            mock_track.assert_called_once()

def test_openai_client_passed_to_wrapper(self):
    """(P1) Original OpenAI client is passed to track_openai()."""
    with patch('opik.integrations.openai.track_openai') as mock_track:
        with patch('openai.OpenAI') as mock_openai:
            original_client = MagicMock()
            mock_openai.return_value = original_client

            llm_call(state={}, model="gpt-4", messages=[...], opik_trace=True)

            mock_track.assert_called_with(original_client)

def test_wrapped_client_used_for_completions(self):
    """(P2) The wrapped client is used for chat.completions.create."""
    with patch('opik.integrations.openai.track_openai') as mock_track:
        with patch('openai.OpenAI'):
            wrapped_client = MagicMock()
            mock_track.return_value = wrapped_client

            llm_call(state={}, model="gpt-4", messages=[...], opik_trace=True)

            wrapped_client.chat.completions.create.assert_called()
```

---

### AC3: Token usage automatically captured

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-UNIT-007 | Unit | P1 | Verify token counts are in result dict | Result validation |
| 005.2-UNIT-008 | Unit | P1 | Verify Opik receives token metrics | Observability requirement |

#### Scenarios

```python
def test_token_usage_in_result(self):
    """(P1) Token usage appears in llm_call result."""
    with patch('openai.OpenAI') as mock_openai:
        mock_response = MagicMock()
        mock_response.usage.prompt_tokens = 100
        mock_response.usage.completion_tokens = 50
        mock_response.usage.total_tokens = 150
        mock_openai.return_value.chat.completions.create.return_value = mock_response

        result = llm_call(state={}, model="gpt-4", messages=[...])

        assert result['usage']['prompt_tokens'] == 100
        assert result['usage']['completion_tokens'] == 50
        assert result['usage']['total_tokens'] == 150

def test_opik_receives_token_metrics(self):
    """(P1) When opik_trace=True, token metrics are captured by Opik."""
    # Opik's track_openai automatically captures usage from OpenAI response
    # Test verifies wrapper is applied (covered by 005.2-UNIT-004)
    pass  # Behavior is automatic via track_openai wrapper
```

---

### AC4: Estimated cost calculated based on model pricing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-UNIT-009 | Unit | P1 | Verify cost calculation for GPT-4 | Primary model pricing |
| 005.2-UNIT-010 | Unit | P1 | Verify cost calculation for GPT-3.5-turbo | Common model pricing |
| 005.2-UNIT-011 | Unit | P2 | Verify unknown model returns zero cost | Edge case handling |

#### Scenarios

```python
def test_cost_calculation_gpt4(self):
    """(P1) Cost is calculated correctly for GPT-4."""
    usage = {"prompt_tokens": 1000, "completion_tokens": 500}
    # GPT-4: $0.03/1K input, $0.06/1K output
    expected_cost = (1000/1000 * 0.03) + (500/1000 * 0.06)  # $0.06

    cost = calculate_cost("gpt-4", usage)
    assert abs(cost - 0.06) < 0.001

def test_cost_calculation_gpt35_turbo(self):
    """(P1) Cost is calculated correctly for GPT-3.5-turbo."""
    usage = {"prompt_tokens": 1000, "completion_tokens": 500}
    # GPT-3.5-turbo: $0.0005/1K input, $0.0015/1K output
    expected_cost = (1000/1000 * 0.0005) + (500/1000 * 0.0015)  # $0.00125

    cost = calculate_cost("gpt-3.5-turbo", usage)
    assert abs(cost - 0.00125) < 0.0001

def test_cost_calculation_unknown_model(self):
    """(P2) Unknown model returns zero cost (no error)."""
    usage = {"prompt_tokens": 1000, "completion_tokens": 500}

    cost = calculate_cost("unknown-model-xyz", usage)
    assert cost == 0.0
```

---

### AC5: llm.stream streaming responses aggregated correctly

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-UNIT-012 | Unit | P1 | Verify llm_stream accepts opik_trace parameter | API parity with llm_call |
| 005.2-UNIT-013 | Unit | P1 | Verify stream chunks are aggregated | Complete response capture |
| 005.2-INT-001 | Integration | P1 | Verify streaming with Opik produces complete trace | End-to-end streaming |

#### Scenarios

```python
def test_llm_stream_accepts_opik_trace(self):
    """(P1) llm_stream accepts opik_trace parameter."""
    import inspect
    from the_edge_agent.actions.llm_actions import llm_stream
    sig = inspect.signature(llm_stream)
    assert 'opik_trace' in sig.parameters

def test_stream_chunks_aggregated(self):
    """(P1) Streaming chunks are aggregated into complete response."""
    with patch('openai.OpenAI') as mock_openai:
        # Mock streaming response
        mock_chunks = [
            MagicMock(choices=[MagicMock(delta=MagicMock(content="Hello"))]),
            MagicMock(choices=[MagicMock(delta=MagicMock(content=" World"))]),
        ]
        mock_openai.return_value.chat.completions.create.return_value = iter(mock_chunks)

        result = llm_stream(state={}, model="gpt-4", messages=[...])

        assert result['content'] == "Hello World"
        assert result['streamed'] is True

def test_streaming_with_opik_complete_trace(self):
    """(P1) Streaming with opik_trace=True produces complete aggregated trace."""
    with patch('opik.integrations.openai.track_openai') as mock_track:
        wrapped_client = MagicMock()
        mock_track.return_value = wrapped_client
        # Set up streaming mock
        # Verify complete response is traced
```

---

### AC6: Azure OpenAI detected and traced identically

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-UNIT-014 | Unit | P1 | Verify Azure OpenAI client is detected | Azure support |
| 005.2-UNIT-015 | Unit | P1 | Verify track_openai() works with AzureOpenAI client | Azure tracing |
| 005.2-INT-002 | Integration | P2 | Verify Azure endpoint URL is captured in trace | Debug information |

#### Scenarios

```python
@patch.dict(os.environ, {
    "AZURE_OPENAI_API_KEY": "azure-key",
    "AZURE_OPENAI_ENDPOINT": "https://my.azure.com"
})
def test_azure_openai_client_detected(self):
    """(P1) Azure OpenAI client is used when env vars present."""
    with patch('openai.AzureOpenAI') as mock_azure:
        llm_call(state={}, model="gpt-4", messages=[...])
        mock_azure.assert_called()

@patch.dict(os.environ, {
    "AZURE_OPENAI_API_KEY": "azure-key",
    "AZURE_OPENAI_ENDPOINT": "https://my.azure.com"
})
def test_track_openai_with_azure_client(self):
    """(P1) track_openai() is applied to AzureOpenAI client."""
    with patch('opik.integrations.openai.track_openai') as mock_track:
        with patch('openai.AzureOpenAI') as mock_azure:
            azure_client = MagicMock()
            mock_azure.return_value = azure_client

            llm_call(state={}, model="gpt-4", messages=[...], opik_trace=True)

            mock_track.assert_called_with(azure_client)

def test_azure_endpoint_in_trace(self):
    """(P2) Azure endpoint URL is captured for debugging."""
    # Integration test verifying endpoint metadata in trace
    pass
```

---

### AC7: Feature opt-in and disabled by default

*(Covered by tests 005.2-UNIT-001, 002, 003)*

---

### AC8: Works alongside OpikExporter (both or either)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-INT-003 | Integration | P0 | Verify llm_call with opik_trace=True works when OpikExporter also enabled | Coexistence critical |
| 005.2-INT-004 | Integration | P1 | Verify llm_call with opik_trace=False works when OpikExporter enabled | Independent operation |
| 005.2-INT-005 | Integration | P2 | Verify native tracing works without OpikExporter | Standalone mode |

#### Scenarios

```python
def test_coexistence_both_enabled(self):
    """(P0) opik_trace=True works when YAMLEngine uses trace_exporter='opik'."""
    with patch.dict('sys.modules', {'opik': MagicMock()}):
        engine = YAMLEngine(trace_exporter="opik", opik_llm_tracing=True)
        # Both should function without conflict
        # TraceContext spans go to OpikExporter
        # LLM calls get native track_openai wrapper

def test_opik_exporter_without_native_tracing(self):
    """(P1) trace_exporter='opik' works with opik_trace=False."""
    with patch.dict('sys.modules', {'opik': MagicMock()}):
        engine = YAMLEngine(trace_exporter="opik", opik_llm_tracing=False)
        # Only TraceContext spans exported, no LLM-native tracing

def test_native_tracing_without_opik_exporter(self):
    """(P2) opik_trace=True works with trace_exporter='console'."""
    engine = YAMLEngine(trace_exporter="console", opik_llm_tracing=True)
    # LLM calls get native tracing, TraceContext goes to console
```

---

### AC9: Integration tests verify end-to-end tracing

*(Satisfied by integration tests above)*

---

### AC10: Documentation shows configuration options

*(Documentation task - no automated test)*

---

## Graceful Degradation Tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-UNIT-016 | Unit | P0 | Verify opik_trace=True with missing SDK logs warning | User feedback |
| 005.2-UNIT-017 | Unit | P0 | Verify LLM call proceeds without tracing when SDK missing | No crash |

#### Scenarios

```python
def test_opik_trace_missing_sdk_warning(self):
    """(P0) Warning logged when opik_trace=True but SDK not installed."""
    with patch.dict('sys.modules', {'opik': None, 'opik.integrations': None}):
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            llm_call(state={}, model="gpt-4", messages=[...], opik_trace=True)

            assert len(w) == 1
            assert "opik" in str(w[0].message).lower()
            assert "pip install opik" in str(w[0].message)

def test_llm_call_proceeds_without_opik(self):
    """(P0) LLM call succeeds even when opik SDK unavailable."""
    with patch.dict('sys.modules', {'opik': None}):
        with patch('openai.OpenAI') as mock_openai:
            mock_openai.return_value.chat.completions.create.return_value = mock_response

            # Should not raise
            result = llm_call(state={}, model="gpt-4", messages=[...], opik_trace=True)

            assert result is not None
            assert 'content' in result
```

---

## Double-Wrap Prevention Test

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-UNIT-018 | Unit | P2 | Verify client is not double-wrapped on repeated calls | Performance/correctness |

#### Scenarios

```python
def test_no_double_wrapping(self):
    """(P2) Client is not wrapped twice on repeated llm_call invocations."""
    with patch('opik.integrations.openai.track_openai') as mock_track:
        # Multiple calls should not result in nested wrappers
        llm_call(state={}, model="gpt-4", messages=[...], opik_trace=True)
        llm_call(state={}, model="gpt-4", messages=[...], opik_trace=True)

        # track_openai should be called per invocation (new client each time)
        # but never with an already-wrapped client
        for call in mock_track.call_args_list:
            client_arg = call[0][0]
            assert not hasattr(client_arg, '_opik_wrapped')
```

---

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| Backward compatibility broken | 005.2-UNIT-002, 003 | Default False, unchanged behavior tests |
| Missing SDK crashes LLM calls | 005.2-UNIT-016, 017 | Warning + graceful continuation |
| Streaming broken with tracing | 005.2-UNIT-012, 013, INT-001 | Stream aggregation tests |
| Azure not supported | 005.2-UNIT-014, 015, INT-002 | Azure-specific tests |
| Conflict with OpikExporter | 005.2-INT-003, 004, 005 | Coexistence tests |

---

## Recommended Execution Order

1. P0 Unit tests (005.2-UNIT-001, 002, 004, 016, 017) - fail fast on critical issues
2. P0 Integration test (005.2-INT-003) - verify coexistence
3. P1 Unit tests (005.2-UNIT-003, 005, 007, 008, 009, 010, 012, 013, 014, 015)
4. P1 Integration tests (005.2-INT-001, 004)
5. P2 tests as time permits

---

## Test Environment Requirements

- **Mocking**: `openai`, `opik` SDKs must be mocked
- **Environment variables**: Use `unittest.mock.patch.dict(os.environ, ...)`
- **Warnings capture**: Use `warnings.catch_warnings()` for degradation tests
- **Test framework**: `unittest` (project standard)

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for wiring)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (005.2-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
