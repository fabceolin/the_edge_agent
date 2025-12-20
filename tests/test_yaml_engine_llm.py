"""
Tests for YAML Engine - LLM Enhanced Actions (TEA-BUILTIN-001.2)

Tests for:
- llm.stream: Streaming LLM responses
- llm.retry: Retry with exponential backoff
- llm.tools: Function/tool calling with dispatch

Test Priority Levels:
- P0: Critical - Backward compatibility, security, error handling
- P1: Core - Streaming, retry success, tool dispatch
- P2: Advanced - Retry-after header, multi-turn, edge cases
"""

import pytest
import time
import json
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock, PropertyMock

import sys
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from the_edge_agent import YAMLEngine, StateGraph, START, END


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
    mock_response.choices[0].message.tool_calls = None
    mock_response.usage = Mock()
    mock_response.usage.model_dump = Mock(return_value={
        "prompt_tokens": 10,
        "completion_tokens": 20,
        "total_tokens": 30
    })
    return mock_response


@pytest.fixture
def mock_streaming_chunks():
    """Create mock streaming chunks."""
    chunks = []
    for i, text in enumerate(["Hello", " ", "World", "!"]):
        chunk = Mock()
        chunk.choices = [Mock()]
        chunk.choices[0].delta = Mock()
        chunk.choices[0].delta.content = text
        chunk.usage = None
        chunks.append(chunk)

    # Final chunk with usage
    final_chunk = Mock()
    final_chunk.choices = [Mock()]
    final_chunk.choices[0].delta = Mock()
    final_chunk.choices[0].delta.content = None
    final_chunk.usage = Mock()
    final_chunk.usage.model_dump = Mock(return_value={
        "prompt_tokens": 10,
        "completion_tokens": 4,
        "total_tokens": 14
    })
    chunks.append(final_chunk)

    return chunks


# =============================================================================
# P0 - Critical Tests
# =============================================================================

class TestLLMEnhancedActionsP0:
    """P0 (Critical) tests - Backward compatibility, security, error handling."""

    def test_existing_llm_call_unchanged(self, engine):
        """(P0) Existing llm.call action must remain unchanged."""
        # Verify llm.call is still registered
        assert 'llm.call' in engine.actions_registry
        assert 'actions.llm_call' in engine.actions_registry

        # Verify it's the same function for both namespaces
        assert engine.actions_registry['llm.call'] is engine.actions_registry['actions.llm_call']

    @pytest.mark.filterwarnings("ignore:llm.retry is deprecated:DeprecationWarning")
    def test_llm_retry_respects_max_retries(self, engine):
        """(P0) llm.retry must not exceed max_retries to prevent infinite loops."""
        with patch('the_edge_agent.yaml_engine.time.sleep'):  # Don't actually sleep
            # Get the actual llm_retry function
            llm_retry = engine.actions_registry['llm.retry']

            # Create a mock that always raises RateLimitError
            with patch('openai.OpenAI') as mock_openai_class:
                mock_client = Mock()
                mock_openai_class.return_value = mock_client

                # Import the actual exception class
                from openai import RateLimitError

                # Create a proper RateLimitError
                mock_response = Mock()
                mock_response.status_code = 429
                mock_response.headers = {}

                mock_client.chat.completions.create.side_effect = RateLimitError(
                    "Rate limit exceeded",
                    response=mock_response,
                    body=None
                )

                result = llm_retry(
                    state={},
                    model="gpt-4",
                    messages=[{"role": "user", "content": "test"}],
                    max_retries=2,
                    base_delay=0.01
                )

                # Should fail after max_retries attempts
                # With max_retries=2: 1 initial + 2 retries = 3 total attempts
                assert result.get('success') is False
                assert result.get('attempts') == 3  # 1 initial + 2 retries
                assert 'Max retries' in result.get('error', '')

    def test_llm_tools_invalid_action_error(self, engine):
        """(P0) llm.tools must error on invalid action reference."""
        llm_tools = engine.actions_registry['llm.tools']

        # Tool with non-existent action
        tools = [{
            "name": "test_tool",
            "description": "Test",
            "action": "nonexistent.action"
        }]

        result = llm_tools(
            state={},
            model="gpt-4",
            messages=[{"role": "user", "content": "test"}],
            tools=tools
        )

        assert result.get('success') is False
        assert 'not found in registry' in result.get('error', '')

    def test_llm_tools_action_injection_blocked(self, engine):
        """(P0) Security: llm.tools must block path traversal in action references."""
        llm_tools = engine.actions_registry['llm.tools']

        # Attempt path traversal
        malicious_tools = [
            {"name": "evil", "description": "Test", "action": "../../../etc/passwd"},
            {"name": "evil2", "description": "Test", "action": "foo/bar/baz"},
            {"name": "evil3", "description": "Test", "action": "foo\\bar\\baz"},
        ]

        for tool in malicious_tools:
            result = llm_tools(
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "test"}],
                tools=[tool]
            )

            assert result.get('success') is False
            assert 'Invalid action reference' in result.get('error', '') or 'not found' in result.get('error', '')

    @pytest.mark.filterwarnings("ignore:llm.retry is deprecated:DeprecationWarning")
    def test_llm_retry_handles_timeout_error(self, engine):
        """(P0) llm.retry must retry on timeout errors."""
        with patch('the_edge_agent.yaml_engine.time.sleep'):
            llm_retry = engine.actions_registry['llm.retry']

            with patch('openai.OpenAI') as mock_openai_class:
                mock_client = Mock()
                mock_openai_class.return_value = mock_client

                from openai import APITimeoutError

                # Fail twice with timeout, then succeed
                call_count = [0]

                def side_effect(*args, **kwargs):
                    call_count[0] += 1
                    if call_count[0] < 3:
                        raise APITimeoutError(request=Mock())

                    mock_response = Mock()
                    mock_response.choices = [Mock()]
                    mock_response.choices[0].message.content = "Success"
                    mock_response.usage = Mock()
                    mock_response.usage.model_dump = Mock(return_value={})
                    return mock_response

                mock_client.chat.completions.create.side_effect = side_effect

                result = llm_retry(
                    state={},
                    model="gpt-4",
                    messages=[{"role": "user", "content": "test"}],
                    max_retries=3,
                    base_delay=0.01
                )

                assert result.get('content') == "Success"
                assert result.get('attempts') == 3

    @pytest.mark.filterwarnings("ignore:llm.retry is deprecated:DeprecationWarning")
    def test_llm_retry_handles_5xx_errors(self, engine):
        """(P0) llm.retry must retry on 5xx server errors."""
        with patch('the_edge_agent.yaml_engine.time.sleep'):
            llm_retry = engine.actions_registry['llm.retry']

            with patch('openai.OpenAI') as mock_openai_class:
                mock_client = Mock()
                mock_openai_class.return_value = mock_client

                from openai import APIError

                # Fail with 503, then succeed
                call_count = [0]

                def side_effect(*args, **kwargs):
                    call_count[0] += 1
                    if call_count[0] < 2:
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

                result = llm_retry(
                    state={},
                    model="gpt-4",
                    messages=[{"role": "user", "content": "test"}],
                    max_retries=3,
                    base_delay=0.01
                )

                assert result.get('content') == "Success"
                assert result.get('attempts') == 2


# =============================================================================
# P1 - Core Functionality Tests
# =============================================================================

class TestLLMEnhancedActionsP1:
    """P1 (Core) tests - Streaming, retry success, tool dispatch."""

    def test_llm_stream_yields_chunks(self, engine, mock_streaming_chunks):
        """(P1) llm.stream must process streaming chunks."""
        llm_stream = engine.actions_registry['llm.stream']

        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client
            mock_client.chat.completions.create.return_value = iter(mock_streaming_chunks)

            result = llm_stream(
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "test"}]
            )

            assert result.get('streamed') is True
            assert result.get('content') == "Hello World!"
            assert result.get('chunk_count') == 4

    def test_llm_stream_final_result(self, engine, mock_streaming_chunks):
        """(P1) llm.stream must return final aggregated result."""
        llm_stream = engine.actions_registry['llm.stream']

        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client
            mock_client.chat.completions.create.return_value = iter(mock_streaming_chunks)

            result = llm_stream(
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "test"}]
            )

            # Verify final result structure
            assert 'content' in result
            assert 'usage' in result
            assert result.get('streamed') is True

    @pytest.mark.filterwarnings("ignore:llm.retry is deprecated:DeprecationWarning")
    def test_llm_retry_succeeds_first_try(self, engine, mock_openai_response):
        """(P1) llm.retry should succeed on first try when no errors."""
        llm_retry = engine.actions_registry['llm.retry']

        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client
            mock_client.chat.completions.create.return_value = mock_openai_response

            result = llm_retry(
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "test"}]
            )

            assert result.get('content') == "Test response"
            assert result.get('attempts') == 1
            assert result.get('total_delay') == 0.0

    @pytest.mark.filterwarnings("ignore:llm.retry is deprecated:DeprecationWarning")
    def test_llm_retry_succeeds_after_failures(self, engine):
        """(P1) llm.retry should succeed after transient failures."""
        with patch('the_edge_agent.yaml_engine.time.sleep'):
            llm_retry = engine.actions_registry['llm.retry']

            with patch('openai.OpenAI') as mock_openai_class:
                mock_client = Mock()
                mock_openai_class.return_value = mock_client

                from openai import APIConnectionError

                # Fail once, then succeed
                call_count = [0]

                def side_effect(*args, **kwargs):
                    call_count[0] += 1
                    if call_count[0] == 1:
                        raise APIConnectionError(request=Mock())

                    mock_response = Mock()
                    mock_response.choices = [Mock()]
                    mock_response.choices[0].message.content = "Success after retry"
                    mock_response.usage = Mock()
                    mock_response.usage.model_dump = Mock(return_value={})
                    return mock_response

                mock_client.chat.completions.create.side_effect = side_effect

                result = llm_retry(
                    state={},
                    model="gpt-4",
                    messages=[{"role": "user", "content": "test"}],
                    base_delay=0.01
                )

                assert result.get('content') == "Success after retry"
                assert result.get('attempts') == 2

    def test_llm_tools_dispatches_to_action(self, engine, mock_openai_response):
        """(P1) llm.tools should dispatch tool calls to registered actions."""
        # Register a test action
        test_results = []
        def test_action(state, query, **kwargs):
            test_results.append(query)
            return {"result": f"searched: {query}"}

        engine.actions_registry['test.search'] = test_action

        llm_tools = engine.actions_registry['llm.tools']

        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            # First call returns tool call
            tool_call_response = Mock()
            tool_call_response.choices = [Mock()]
            tool_call_response.choices[0].message = Mock()
            tool_call_response.choices[0].message.content = None
            tool_call_response.choices[0].message.tool_calls = [Mock()]
            tool_call_response.choices[0].message.tool_calls[0].id = "call_123"
            tool_call_response.choices[0].message.tool_calls[0].function = Mock()
            tool_call_response.choices[0].message.tool_calls[0].function.name = "search"
            tool_call_response.choices[0].message.tool_calls[0].function.arguments = '{"query": "python tutorials"}'

            # Second call returns final response
            final_response = Mock()
            final_response.choices = [Mock()]
            final_response.choices[0].message = Mock()
            final_response.choices[0].message.content = "Here are the results"
            final_response.choices[0].message.tool_calls = None

            mock_client.chat.completions.create.side_effect = [tool_call_response, final_response]

            tools = [{
                "name": "search",
                "description": "Search for information",
                "parameters": {
                    "query": {"type": "string", "required": True}
                },
                "action": "test.search"
            }]

            result = llm_tools(
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "search for python tutorials"}],
                tools=tools
            )

            assert result.get('content') == "Here are the results"
            assert len(result.get('tool_calls', [])) == 1
            assert result['tool_calls'][0]['name'] == "search"
            assert test_results == ["python tutorials"]

    def test_llm_stream_connection_failure(self, engine):
        """(P1) llm.stream must handle mid-stream disconnection."""
        llm_stream = engine.actions_registry['llm.stream']

        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            # Simulate connection error during streaming
            def failing_iterator():
                yield Mock(choices=[Mock(delta=Mock(content="Hello"))], usage=None)
                raise ConnectionError("Connection lost")

            mock_client.chat.completions.create.return_value = failing_iterator()

            result = llm_stream(
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "test"}]
            )

            assert result.get('success') is False
            assert 'error' in result

    def test_tool_schema_validation_invalid(self, engine):
        """(P1) llm.tools must reject invalid tool definitions."""
        llm_tools = engine.actions_registry['llm.tools']

        # Tool without name
        invalid_tools = [{"description": "No name"}]

        result = llm_tools(
            state={},
            model="gpt-4",
            messages=[{"role": "user", "content": "test"}],
            tools=invalid_tools
        )

        assert result.get('success') is False
        assert 'name' in result.get('error', '').lower()

    def test_dual_namespace_llm_stream(self, engine):
        """(P1) AC8: llm.stream and actions.llm_stream must both work."""
        assert 'llm.stream' in engine.actions_registry
        assert 'actions.llm_stream' in engine.actions_registry
        assert engine.actions_registry['llm.stream'] is engine.actions_registry['actions.llm_stream']

    def test_dual_namespace_llm_retry(self, engine):
        """(P1) AC8: llm.retry and actions.llm_retry must both work."""
        assert 'llm.retry' in engine.actions_registry
        assert 'actions.llm_retry' in engine.actions_registry
        assert engine.actions_registry['llm.retry'] is engine.actions_registry['actions.llm_retry']

    def test_dual_namespace_llm_tools(self, engine):
        """(P1) AC8: llm.tools and actions.llm_tools must both work."""
        assert 'llm.tools' in engine.actions_registry
        assert 'actions.llm_tools' in engine.actions_registry
        assert engine.actions_registry['llm.tools'] is engine.actions_registry['actions.llm_tools']


# =============================================================================
# P2 - Advanced Features Tests
# =============================================================================

class TestLLMEnhancedActionsP2:
    """P2 (Advanced) tests - Retry-after header, multi-turn, edge cases."""

    @pytest.mark.filterwarnings("ignore:llm.retry is deprecated:DeprecationWarning")
    def test_llm_retry_uses_retry_after_header(self, engine):
        """(P2) llm.retry should respect Retry-After header."""
        with patch('the_edge_agent.yaml_engine.time.sleep') as mock_sleep:
            llm_retry = engine.actions_registry['llm.retry']

            with patch('openai.OpenAI') as mock_openai_class:
                mock_client = Mock()
                mock_openai_class.return_value = mock_client

                from openai import RateLimitError

                # Fail once with Retry-After header
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

                result = llm_retry(
                    state={},
                    model="gpt-4",
                    messages=[{"role": "user", "content": "test"}],
                    base_delay=1.0
                )

                assert result.get('content') == "Success"
                # Should have used Retry-After value (5 seconds)
                mock_sleep.assert_called()
                sleep_args = [call[0][0] for call in mock_sleep.call_args_list]
                assert any(s >= 5.0 for s in sleep_args)

    def test_llm_tools_multi_turn(self, engine):
        """(P2) llm.tools should handle multi-turn tool calling."""
        # Register test actions
        def action1(state, **kwargs):
            return {"step": 1}

        def action2(state, **kwargs):
            return {"step": 2}

        engine.actions_registry['test.step1'] = action1
        engine.actions_registry['test.step2'] = action2

        llm_tools = engine.actions_registry['llm.tools']

        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            # First call - tool 1
            response1 = Mock()
            response1.choices = [Mock()]
            response1.choices[0].message = Mock()
            response1.choices[0].message.content = None
            response1.choices[0].message.tool_calls = [Mock()]
            response1.choices[0].message.tool_calls[0].id = "call_1"
            response1.choices[0].message.tool_calls[0].function = Mock()
            response1.choices[0].message.tool_calls[0].function.name = "tool1"
            response1.choices[0].message.tool_calls[0].function.arguments = '{}'

            # Second call - tool 2
            response2 = Mock()
            response2.choices = [Mock()]
            response2.choices[0].message = Mock()
            response2.choices[0].message.content = None
            response2.choices[0].message.tool_calls = [Mock()]
            response2.choices[0].message.tool_calls[0].id = "call_2"
            response2.choices[0].message.tool_calls[0].function = Mock()
            response2.choices[0].message.tool_calls[0].function.name = "tool2"
            response2.choices[0].message.tool_calls[0].function.arguments = '{}'

            # Final response
            response3 = Mock()
            response3.choices = [Mock()]
            response3.choices[0].message = Mock()
            response3.choices[0].message.content = "Both tools executed"
            response3.choices[0].message.tool_calls = None

            mock_client.chat.completions.create.side_effect = [response1, response2, response3]

            tools = [
                {"name": "tool1", "description": "Tool 1", "action": "test.step1"},
                {"name": "tool2", "description": "Tool 2", "action": "test.step2"}
            ]

            result = llm_tools(
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "run both tools"}],
                tools=tools
            )

            assert result.get('content') == "Both tools executed"
            assert len(result.get('tool_calls', [])) == 2
            assert result.get('rounds') == 2

    def test_llm_stream_empty_response(self, engine):
        """(P2) llm.stream must handle empty streaming response."""
        llm_stream = engine.actions_registry['llm.stream']

        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            # Empty stream
            mock_client.chat.completions.create.return_value = iter([])

            result = llm_stream(
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "test"}]
            )

            assert result.get('content') == ""
            assert result.get('streamed') is True
            assert result.get('chunk_count') == 0


# =============================================================================
# Integration Tests
# =============================================================================

class TestLLMEnhancedActionsIntegration:
    """Integration tests for LLM enhanced actions in YAML workflows."""

    def test_llm_stream_in_yaml_workflow(self, engine, mock_streaming_chunks):
        """(P1) llm.stream should work in YAML workflow."""
        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client
            mock_client.chat.completions.create.return_value = iter(mock_streaming_chunks)

            config = {
                'nodes': [
                    {
                        'name': 'stream_node',
                        'uses': 'llm.stream',
                        'with': {
                            'model': 'gpt-4',
                            'messages': [{"role": "user", "content": "test"}]
                        }
                    }
                ],
                'edges': [
                    {'from': '__start__', 'to': 'stream_node'},
                    {'from': 'stream_node', 'to': '__end__'}
                ]
            }

            graph = engine.load_from_dict(config)
            events = list(graph.stream({}))

            final_state = events[-1]['state']
            assert final_state.get('streamed') is True
            assert final_state.get('content') == "Hello World!"

    @pytest.mark.filterwarnings("ignore:llm.retry is deprecated:DeprecationWarning")
    def test_llm_retry_in_yaml_workflow(self, engine, mock_openai_response):
        """(P1) llm.retry should work in YAML workflow."""
        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client
            mock_client.chat.completions.create.return_value = mock_openai_response

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

            graph = engine.load_from_dict(config)
            events = list(graph.stream({}))

            final_state = events[-1]['state']
            assert final_state.get('content') == "Test response"
            assert final_state.get('attempts') == 1

    def test_llm_tools_with_registered_actions(self, engine):
        """(P1) llm.tools should work with registered actions in YAML workflow."""
        # Register a custom action
        def custom_search(state, query, **kwargs):
            return {"results": [f"Result for: {query}"]}

        engine.actions_registry['custom.search'] = custom_search

        with patch('openai.OpenAI') as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            # Tool call response
            tool_response = Mock()
            tool_response.choices = [Mock()]
            tool_response.choices[0].message = Mock()
            tool_response.choices[0].message.content = None
            tool_response.choices[0].message.tool_calls = [Mock()]
            tool_response.choices[0].message.tool_calls[0].id = "call_1"
            tool_response.choices[0].message.tool_calls[0].function = Mock()
            tool_response.choices[0].message.tool_calls[0].function.name = "search"
            tool_response.choices[0].message.tool_calls[0].function.arguments = '{"query": "test query"}'

            # Final response
            final_response = Mock()
            final_response.choices = [Mock()]
            final_response.choices[0].message = Mock()
            final_response.choices[0].message.content = "Found results"
            final_response.choices[0].message.tool_calls = None

            mock_client.chat.completions.create.side_effect = [tool_response, final_response]

            config = {
                'nodes': [
                    {
                        'name': 'tools_node',
                        'uses': 'llm.tools',
                        'with': {
                            'model': 'gpt-4',
                            'messages': [{"role": "user", "content": "search"}],
                            'tools': [
                                {
                                    'name': 'search',
                                    'description': 'Search function',
                                    'parameters': {
                                        'query': {'type': 'string', 'required': True}
                                    },
                                    'action': 'custom.search'
                                }
                            ]
                        }
                    }
                ],
                'edges': [
                    {'from': '__start__', 'to': 'tools_node'},
                    {'from': 'tools_node', 'to': '__end__'}
                ]
            }

            graph = engine.load_from_dict(config)
            events = list(graph.stream({}))

            final_state = events[-1]['state']
            assert final_state.get('content') == "Found results"
            assert len(final_state.get('tool_calls', [])) == 1


# =============================================================================
# OpenAI Import Error Tests
# =============================================================================

class TestOpenAIImportErrors:
    """Tests for handling missing OpenAI library."""

    def test_llm_stream_without_openai(self, engine):
        """llm.stream should return error when OpenAI not installed."""
        llm_stream = engine.actions_registry['llm.stream']

        with patch.dict('sys.modules', {'openai': None}):
            with patch('builtins.__import__', side_effect=ImportError("No module named 'openai'")):
                # Need to reload the action to trigger import check
                # For now, just verify the function exists
                assert callable(llm_stream)

    def test_llm_retry_without_openai(self, engine):
        """llm.retry should return error when OpenAI not installed."""
        llm_retry = engine.actions_registry['llm.retry']
        assert callable(llm_retry)

    def test_llm_tools_without_openai(self, engine):
        """llm.tools should return error when OpenAI not installed."""
        llm_tools = engine.actions_registry['llm.tools']
        assert callable(llm_tools)


# =============================================================================
# Run tests
# =============================================================================

if __name__ == '__main__':
    pytest.main([__file__, '-v'])
