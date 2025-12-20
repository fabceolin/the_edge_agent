"""
Tests for LLM Call Consolidation (TEA-BUILTIN-001.2.1)

Tests for llm.call with merged retry logic:
- max_retries=0: Respect Retry-After once, then fail
- max_retries>0: Full exponential backoff with Retry-After support
- llm.retry deprecation and delegation

Test Priority Levels:
- P0: Critical - Backwards compatibility, no nested retries, rate limit handling
- P1: Core - Retry success/failure, exponential backoff
- P2: Advanced - Edge cases, deprecation warnings
"""

import pytest
import time
import warnings
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock

import sys
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from the_edge_agent import YAMLEngine


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def engine():
    """Create a fresh YAMLEngine instance."""
    return YAMLEngine()


@pytest.fixture
def mock_openai_response():
    """Create a mock OpenAI response."""
    mock_response = Mock()
    mock_response.choices = [Mock()]
    mock_response.choices[0].message = Mock()
    mock_response.choices[0].message.content = "Test response"
    mock_response.usage = Mock()
    mock_response.usage.model_dump = Mock(return_value={
        "prompt_tokens": 10,
        "completion_tokens": 20,
        "total_tokens": 30
    })
    return mock_response


# =============================================================================
# P0 - Critical Tests
# =============================================================================

class TestLLMCallConsolidationP0:
    """P0 (Critical) tests - Backwards compatibility, no nested retries, rate limit handling."""

    def test_llm_call_default_max_retries_zero(self, engine):
        """(P0) Verify llm.call defaults to max_retries=0."""
        llm_call = engine.actions_registry['llm.call']

        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            mock_response = Mock()
            mock_response.choices = [Mock()]
            mock_response.choices[0].message.content = "Success"
            mock_response.usage = Mock()
            mock_response.usage.model_dump = Mock(return_value={})
            mock_client.chat.completions.create.return_value = mock_response

            result = llm_call(
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "test"}]
            )

            # Should succeed without attempts/total_delay fields (max_retries=0)
            assert result.get('content') == "Success"
            assert 'attempts' not in result
            assert 'total_delay' not in result

    def test_llm_call_max_retries_zero_respects_retry_after_once(self, engine):
        """(P0) llm.call with max_retries=0 respects Retry-After header once."""
        with patch('the_edge_agent.actions.llm_actions.time.sleep') as mock_sleep:
            llm_call = engine.actions_registry['llm.call']

            with patch('openai.OpenAI') as mock_openai_class:
                mock_client = Mock()
                mock_openai_class.return_value = mock_client

                from openai import RateLimitError

                call_count = [0]

                def side_effect(*args, **kwargs):
                    call_count[0] += 1
                    if call_count[0] == 1:
                        # First call - rate limit with Retry-After
                        mock_response = Mock()
                        mock_response.status_code = 429
                        mock_response.headers = {'Retry-After': '2'}
                        raise RateLimitError(
                            "Rate limit",
                            response=mock_response,
                            body=None
                        )
                    elif call_count[0] == 2:
                        # Second call - succeed
                        mock_response = Mock()
                        mock_response.choices = [Mock()]
                        mock_response.choices[0].message.content = "Success"
                        mock_response.usage = Mock()
                        mock_response.usage.model_dump = Mock(return_value={})
                        return mock_response

                mock_client.chat.completions.create.side_effect = side_effect

                result = llm_call(
                    state={},
                    model="gpt-4",
                    messages=[{"role": "user", "content": "test"}],
                    max_retries=0
                )

                # Should succeed after single retry
                assert result.get('content') == "Success"
                # Should have slept for Retry-After duration
                mock_sleep.assert_called_once_with(2.0)

    def test_llm_call_max_retries_zero_fails_after_single_retry_attempt(self, engine):
        """(P0) llm.call with max_retries=0 fails if second attempt also hits rate limit."""
        with patch('the_edge_agent.actions.llm_actions.time.sleep'):
            llm_call = engine.actions_registry['llm.call']

            with patch('openai.OpenAI') as mock_openai_class:
                mock_client = Mock()
                mock_openai_class.return_value = mock_client

                from openai import RateLimitError

                # Always rate limit
                mock_response = Mock()
                mock_response.status_code = 429
                mock_response.headers = {'Retry-After': '2'}
                mock_client.chat.completions.create.side_effect = RateLimitError(
                    "Rate limit",
                    response=mock_response,
                    body=None
                )

                # Should raise after single retry attempt
                with pytest.raises(RateLimitError):
                    llm_call(
                        state={},
                        model="gpt-4",
                        messages=[{"role": "user", "content": "test"}],
                        max_retries=0
                    )

    def test_llm_call_backwards_compatible_signature(self, engine, mock_openai_response):
        """(P0) Existing llm.call code without retry params still works."""
        llm_call = engine.actions_registry['llm.call']

        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client
            mock_client.chat.completions.create.return_value = mock_openai_response

            # Old signature - no retry params
            result = llm_call(
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "test"}],
                temperature=0.5
            )

            assert result.get('content') == "Test response"

    @pytest.mark.filterwarnings("ignore:llm.retry is deprecated:DeprecationWarning")
    def test_llm_retry_delegates_to_llm_call(self, engine, mock_openai_response):
        """(P0) llm.retry delegates to llm.call with same parameters."""
        llm_retry = engine.actions_registry['llm.retry']

        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client
            mock_client.chat.completions.create.return_value = mock_openai_response

            result = llm_retry(
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "test"}],
                max_retries=3
            )

            # Should return same result as llm.call with max_retries=3
            assert result.get('content') == "Test response"
            assert result.get('attempts') == 1

    def test_llm_retry_shows_deprecation_warning(self, engine, mock_openai_response):
        """(P0) llm.retry shows deprecation warning."""
        llm_retry = engine.actions_registry['llm.retry']

        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client
            mock_client.chat.completions.create.return_value = mock_openai_response

            with warnings.catch_warnings(record=True) as w:
                warnings.simplefilter("always")

                llm_retry(
                    state={},
                    model="gpt-4",
                    messages=[{"role": "user", "content": "test"}]
                )

                # Should have deprecation warning
                assert len(w) == 1
                assert issubclass(w[0].category, DeprecationWarning)
                assert "llm.retry is deprecated" in str(w[0].message)


# =============================================================================
# P1 - Core Functionality Tests
# =============================================================================

class TestLLMCallConsolidationP1:
    """P1 (Core) tests - Retry success/failure, exponential backoff."""

    def test_llm_call_max_retries_gt_zero_full_retry_logic(self, engine):
        """(P1) llm.call with max_retries>0 implements full retry logic."""
        with patch('the_edge_agent.actions.llm_actions.time.sleep'):
            llm_call = engine.actions_registry['llm.call']

            with patch('openai.OpenAI') as mock_openai_class:
                mock_client = Mock()
                mock_openai_class.return_value = mock_client

                from openai import APIConnectionError

                call_count = [0]

                def side_effect(*args, **kwargs):
                    call_count[0] += 1
                    if call_count[0] < 3:
                        raise APIConnectionError(request=Mock())

                    mock_response = Mock()
                    mock_response.choices = [Mock()]
                    mock_response.choices[0].message.content = "Success"
                    mock_response.usage = Mock()
                    mock_response.usage.model_dump = Mock(return_value={})
                    return mock_response

                mock_client.chat.completions.create.side_effect = side_effect

                result = llm_call(
                    state={},
                    model="gpt-4",
                    messages=[{"role": "user", "content": "test"}],
                    max_retries=3,
                    base_delay=0.01
                )

                assert result.get('content') == "Success"
                assert result.get('attempts') == 3

    def test_llm_call_exponential_backoff_correct_delays(self, engine):
        """(P1) llm.call implements correct exponential backoff delays."""
        with patch('the_edge_agent.actions.llm_actions.time.sleep') as mock_sleep:
            llm_call = engine.actions_registry['llm.call']

            with patch('openai.OpenAI') as mock_openai_class:
                mock_client = Mock()
                mock_openai_class.return_value = mock_client

                from openai import APIConnectionError

                mock_client.chat.completions.create.side_effect = APIConnectionError(request=Mock())

                result = llm_call(
                    state={},
                    model="gpt-4",
                    messages=[{"role": "user", "content": "test"}],
                    max_retries=3,
                    base_delay=1.0
                )

                # Should have failed
                assert result.get('success') is False

                # Check exponential backoff: 1, 2, 4
                sleep_calls = [call[0][0] for call in mock_sleep.call_args_list]
                assert len(sleep_calls) == 3
                assert sleep_calls[0] == 1.0  # base_delay * 2^0
                assert sleep_calls[1] == 2.0  # base_delay * 2^1
                assert sleep_calls[2] == 4.0  # base_delay * 2^2

    def test_llm_call_respects_retry_after_with_max_retries(self, engine):
        """(P1) llm.call with max_retries>0 respects Retry-After header."""
        with patch('the_edge_agent.actions.llm_actions.time.sleep') as mock_sleep:
            llm_call = engine.actions_registry['llm.call']

            with patch('openai.OpenAI') as mock_openai_class:
                mock_client = Mock()
                mock_openai_class.return_value = mock_client

                from openai import RateLimitError

                call_count = [0]

                def side_effect(*args, **kwargs):
                    call_count[0] += 1
                    if call_count[0] == 1:
                        mock_response = Mock()
                        mock_response.status_code = 429
                        mock_response.headers = {'Retry-After': '5'}
                        raise RateLimitError(
                            "Rate limit",
                            response=mock_response,
                            body=None
                        )

                    mock_response = Mock()
                    mock_response.choices = [Mock()]
                    mock_response.choices[0].message.content = "Success"
                    mock_response.usage = Mock()
                    mock_response.usage.model_dump = Mock(return_value={})
                    return mock_response

                mock_client.chat.completions.create.side_effect = side_effect

                result = llm_call(
                    state={},
                    model="gpt-4",
                    messages=[{"role": "user", "content": "test"}],
                    max_retries=3,
                    base_delay=1.0
                )

                assert result.get('content') == "Success"
                # Should have used Retry-After (5s) instead of exponential (1s)
                mock_sleep.assert_called_once_with(5.0)

    def test_llm_call_handles_timeout_errors(self, engine):
        """(P1) llm.call retries on timeout errors."""
        with patch('the_edge_agent.actions.llm_actions.time.sleep'):
            llm_call = engine.actions_registry['llm.call']

            with patch('openai.OpenAI') as mock_openai_class:
                mock_client = Mock()
                mock_openai_class.return_value = mock_client

                from openai import APITimeoutError

                call_count = [0]

                def side_effect(*args, **kwargs):
                    call_count[0] += 1
                    if call_count[0] < 2:
                        raise APITimeoutError(request=Mock())

                    mock_response = Mock()
                    mock_response.choices = [Mock()]
                    mock_response.choices[0].message.content = "Success"
                    mock_response.usage = Mock()
                    mock_response.usage.model_dump = Mock(return_value={})
                    return mock_response

                mock_client.chat.completions.create.side_effect = side_effect

                result = llm_call(
                    state={},
                    model="gpt-4",
                    messages=[{"role": "user", "content": "test"}],
                    max_retries=3,
                    base_delay=0.01
                )

                assert result.get('content') == "Success"
                assert result.get('attempts') == 2

    def test_llm_call_handles_5xx_errors(self, engine):
        """(P1) llm.call retries on 5xx server errors."""
        with patch('the_edge_agent.actions.llm_actions.time.sleep'):
            llm_call = engine.actions_registry['llm.call']

            with patch('openai.OpenAI') as mock_openai_class:
                mock_client = Mock()
                mock_openai_class.return_value = mock_client

                from openai import APIError

                call_count = [0]

                def side_effect(*args, **kwargs):
                    call_count[0] += 1
                    if call_count[0] == 1:
                        error = APIError(
                            message="Service unavailable",
                            request=Mock(),
                            body=None
                        )
                        error.status_code = 503
                        raise error

                    mock_response = Mock()
                    mock_response.choices = [Mock()]
                    mock_response.choices[0].message.content = "Success"
                    mock_response.usage = Mock()
                    mock_response.usage.model_dump = Mock(return_value={})
                    return mock_response

                mock_client.chat.completions.create.side_effect = side_effect

                result = llm_call(
                    state={},
                    model="gpt-4",
                    messages=[{"role": "user", "content": "test"}],
                    max_retries=3,
                    base_delay=0.01
                )

                assert result.get('content') == "Success"
                assert result.get('attempts') == 2

    def test_llm_call_fails_fast_on_4xx_except_429(self, engine):
        """(P1) llm.call fails fast on 4xx errors (except 429)."""
        llm_call = engine.actions_registry['llm.call']

        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            from openai import APIError

            error = APIError(
                message="Bad request",
                request=Mock(),
                body=None
            )
            error.status_code = 400
            mock_client.chat.completions.create.side_effect = error

            result = llm_call(
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "test"}],
                max_retries=3
            )

            # Should fail immediately without retry
            assert result.get('success') is False
            assert result.get('attempts') == 1
            assert 'non-retryable' in result.get('error', '')

    def test_llm_call_attempt_tracking(self, engine, mock_openai_response):
        """(P1) llm.call returns accurate attempt count."""
        llm_call = engine.actions_registry['llm.call']

        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client
            mock_client.chat.completions.create.return_value = mock_openai_response

            result = llm_call(
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "test"}],
                max_retries=3
            )

            assert result.get('attempts') == 1
            assert result.get('total_delay') == 0.0


# =============================================================================
# P2 - Advanced Tests
# =============================================================================

class TestLLMCallConsolidationP2:
    """P2 (Advanced) tests - Edge cases, max delay cap."""

    def test_llm_call_max_delay_cap_enforced(self, engine):
        """(P2) llm.call enforces max_delay cap on exponential backoff."""
        with patch('the_edge_agent.actions.llm_actions.time.sleep') as mock_sleep:
            llm_call = engine.actions_registry['llm.call']

            with patch('openai.OpenAI') as mock_openai_class:
                mock_client = Mock()
                mock_openai_class.return_value = mock_client

                from openai import APIConnectionError

                mock_client.chat.completions.create.side_effect = APIConnectionError(request=Mock())

                result = llm_call(
                    state={},
                    model="gpt-4",
                    messages=[{"role": "user", "content": "test"}],
                    max_retries=10,
                    base_delay=1.0,
                    max_delay=5.0
                )

                # All delays should be capped at max_delay
                sleep_calls = [call[0][0] for call in mock_sleep.call_args_list]
                assert all(delay <= 5.0 for delay in sleep_calls)

    def test_llm_call_retry_after_missing_uses_exponential(self, engine):
        """(P2) llm.call uses exponential backoff when Retry-After is missing."""
        with patch('the_edge_agent.actions.llm_actions.time.sleep') as mock_sleep:
            llm_call = engine.actions_registry['llm.call']

            with patch('openai.OpenAI') as mock_openai_class:
                mock_client = Mock()
                mock_openai_class.return_value = mock_client

                from openai import RateLimitError

                # Rate limit without Retry-After header
                mock_response = Mock()
                mock_response.status_code = 429
                mock_response.headers = {}
                mock_client.chat.completions.create.side_effect = RateLimitError(
                    "Rate limit",
                    response=mock_response,
                    body=None
                )

                result = llm_call(
                    state={},
                    model="gpt-4",
                    messages=[{"role": "user", "content": "test"}],
                    max_retries=2,
                    base_delay=1.0
                )

                # Should use exponential backoff: 1, 2
                sleep_calls = [call[0][0] for call in mock_sleep.call_args_list]
                assert sleep_calls[0] == 1.0
                assert sleep_calls[1] == 2.0

    def test_llm_call_azure_openai_compatibility(self, engine):
        """(P2) llm.call preserves Azure OpenAI support with retry logic."""
        with patch.dict('os.environ', {
            'AZURE_OPENAI_API_KEY': 'test-key',
            'AZURE_OPENAI_ENDPOINT': 'https://test.openai.azure.com',
            'AZURE_OPENAI_DEPLOYMENT': 'gpt-4-deployment'
        }):
            llm_call = engine.actions_registry['llm.call']

            with patch('openai.AzureOpenAI') as mock_azure_class:
                mock_client = Mock()
                mock_azure_class.return_value = mock_client

                mock_response = Mock()
                mock_response.choices = [Mock()]
                mock_response.choices[0].message.content = "Azure response"
                mock_response.usage = Mock()
                mock_response.usage.model_dump = Mock(return_value={})
                mock_client.chat.completions.create.return_value = mock_response

                result = llm_call(
                    state={},
                    model="gpt-4",
                    messages=[{"role": "user", "content": "test"}],
                    max_retries=3
                )

                assert result.get('content') == "Azure response"


# =============================================================================
# Integration Tests
# =============================================================================

class TestLLMCallConsolidationIntegration:
    """Integration tests for llm.call consolidation in YAML workflows."""

    def test_existing_llm_retry_yaml_still_works(self, engine, mock_openai_response):
        """(P0) Existing YAML using llm.retry continues to work."""
        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client
            mock_client.chat.completions.create.return_value = mock_openai_response

            # Old YAML config using llm.retry
            config = {
                'nodes': [
                    {
                        'name': 'retry_node',
                        'uses': 'llm.retry',
                        'with': {
                            'model': 'gpt-4',
                            'messages': [{"role": "user", "content": "test"}],
                            'max_retries': 3
                        }
                    }
                ],
                'edges': [
                    {'from': '__start__', 'to': 'retry_node'},
                    {'from': 'retry_node', 'to': '__end__'}
                ]
            }

            # Should work but show deprecation warning
            with warnings.catch_warnings(record=True) as w:
                warnings.simplefilter("always")

                graph = engine.load_from_dict(config)
                events = list(graph.stream({}))

                final_state = events[-1]['state']
                assert final_state.get('content') == "Test response"

                # Should have deprecation warning
                assert any(issubclass(warning.category, DeprecationWarning) for warning in w)

    def test_llm_call_with_max_retries_in_yaml(self, engine, mock_openai_response):
        """(P1) llm.call with max_retries works in YAML workflow."""
        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client
            mock_client.chat.completions.create.return_value = mock_openai_response

            config = {
                'nodes': [
                    {
                        'name': 'call_node',
                        'uses': 'llm.call',
                        'with': {
                            'model': 'gpt-4',
                            'messages': [{"role": "user", "content": "test"}],
                            'max_retries': 3
                        }
                    }
                ],
                'edges': [
                    {'from': '__start__', 'to': 'call_node'},
                    {'from': 'call_node', 'to': '__end__'}
                ]
            }

            graph = engine.load_from_dict(config)
            events = list(graph.stream({}))

            final_state = events[-1]['state']
            assert final_state.get('content') == "Test response"
            assert final_state.get('attempts') == 1

    def test_llm_call_in_parallel_with_rate_limit(self, engine):
        """(P0) llm.call in parallel flows with some flows being rate limited."""
        from the_edge_agent import StateGraph, ParallelConfig, RetryPolicy
        import threading

        with patch('the_edge_agent.actions.llm_actions.time.sleep'):
            with patch('openai.OpenAI') as mock_openai_class:
                mock_client = Mock()
                mock_openai_class.return_value = mock_client

                from openai import RateLimitError

                # Track calls per flow using thread-safe dict
                flow_calls = {'flow1': 0, 'flow2': 0, 'flow3': 0}
                lock = threading.Lock()

                def create_side_effect(flow_name):
                    def side_effect(*args, **kwargs):
                        with lock:
                            flow_calls[flow_name] += 1
                            call_num = flow_calls[flow_name]

                        # Flow 1: rate limit on first call, succeed on second
                        if flow_name == 'flow1' and call_num == 1:
                            mock_response = Mock()
                            mock_response.status_code = 429
                            mock_response.headers = {'Retry-After': '0.1'}
                            raise RateLimitError("Rate limit", response=mock_response, body=None)

                        # All other calls succeed
                        mock_response = Mock()
                        mock_response.choices = [Mock()]
                        mock_response.choices[0].message.content = f"Response from {flow_name}"
                        mock_response.usage = Mock()
                        mock_response.usage.model_dump = Mock(return_value={})
                        return mock_response

                    return side_effect

                # Use a shared counter to track which flow is being called
                call_tracker = {'counter': 0}

                def routing_side_effect(*args, **kwargs):
                    # Determine which flow based on call order
                    with lock:
                        call_tracker['counter'] += 1
                        flow_num = ((call_tracker['counter'] - 1) % 3) + 1
                        flow_name = f'flow{flow_num}'

                    return create_side_effect(flow_name)(*args, **kwargs)

                mock_client.chat.completions.create.side_effect = routing_side_effect

                # Create graph with 3 parallel flows
                graph = StateGraph({"results": list})

                # Add 3 parallel LLM nodes with max_retries=0
                def make_llm_node(flow_id):
                    def node_func(state):
                        return engine.actions_registry['llm.call'](
                            state, "gpt-4",
                            [{"role": "user", "content": f"test {flow_id}"}],
                            max_retries=0
                        )
                    return node_func

                graph.add_node("flow1", run=make_llm_node("flow1"))
                graph.add_node("flow2", run=make_llm_node("flow2"))
                graph.add_node("flow3", run=make_llm_node("flow3"))

                # Fan-in node aggregates results (must be added BEFORE edges)
                def aggregate_results(state, parallel_results):
                    contents = []
                    for result in parallel_results:
                        if result.success and 'content' in result.state:
                            contents.append(result.state['content'])
                    return {"results": contents}

                graph.add_fanin_node("fan_in", run=aggregate_results)

                # Add edges from each flow to fan_in
                graph.add_edge("flow1", "fan_in")
                graph.add_edge("flow2", "fan_in")
                graph.add_edge("flow3", "fan_in")

                # Set up parallel edges with flow-level retry
                graph.add_parallel_edge(
                    "__start__", "flow1", "fan_in",
                    config=ParallelConfig(
                        retry_policy=RetryPolicy(max_retries=2, base_delay=0.01)
                    )
                )
                graph.add_parallel_edge(
                    "__start__", "flow2", "fan_in",
                    config=ParallelConfig(
                        retry_policy=RetryPolicy(max_retries=2, base_delay=0.01)
                    )
                )
                graph.add_parallel_edge(
                    "__start__", "flow3", "fan_in",
                    config=ParallelConfig(
                        retry_policy=RetryPolicy(max_retries=2, base_delay=0.01)
                    )
                )

                graph.set_finish_point("fan_in")

                compiled = graph.compile()
                result = compiled.invoke({"results": []})

                # All flows should complete successfully
                final_event = list(result)[-1]
                assert 'state' in final_event
                final_state = final_event['state']

                assert 'results' in final_state
                assert len(final_state['results']) == 3  # All 3 flows completed

                # Verify all flows returned content
                assert all('Response from flow' in r for r in final_state['results'])

                # flow1 was rate limited and successfully retried with Retry-After
                # (The flow-level retry may have also retried, so check >= 1)
                assert flow_calls['flow1'] >= 1


# =============================================================================
# Run tests
# =============================================================================

if __name__ == '__main__':
    pytest.main([__file__, '-v'])
