"""
Tests for error handling module (TEA-BUILTIN-015.6).

Covers:
- AC1: settings.error_handling block with mode, max_retries, retry_delay, backoff_multiplier
- AC2: Default mode=raise for backward compatibility
- AC3: Retry with exponential backoff for transient errors
- AC4: Node-level on_error override
- AC5: Fallback routing on error
- AC6: Error state capture in __error__
- AC7: Error response templates for HTTP status codes
- AC8: error.is_retryable, error.clear, error.retry actions
- AC9: clear_on_success behavior
"""

import asyncio
import pytest
import time
from typing import Any, Dict
from unittest.mock import patch, MagicMock, AsyncMock

from the_edge_agent.error_handling import (
    ErrorInfo,
    ErrorMode,
    ErrorHandler,
    ErrorHandlingSettings,
    NodeErrorSettings,
    RetryPolicy,
    with_retry,
    with_retry_sync,
    classify_error,
    is_retryable_error,
    create_error_info,
    render_error_response,
    ErrorResponseRenderer,
    ErrorResponseConfig,
    parse_node_error_settings,
    create_error_handler,
    RETRYABLE_ERRORS,
    NON_RETRYABLE_ERRORS,
)


class TestErrorClassification:
    """Test error type classification (AC3, AC6)."""

    def test_classify_retryable_timeout(self):
        """TimeoutError is classified as retryable."""
        error_type, is_retryable = classify_error(TimeoutError("timed out"))
        assert error_type == "TimeoutError"
        assert is_retryable is True

    def test_classify_retryable_connection(self):
        """ConnectionError is classified as retryable."""
        error_type, is_retryable = classify_error(ConnectionError("connection failed"))
        assert error_type == "ConnectionError"
        assert is_retryable is True

    def test_classify_non_retryable_value_error(self):
        """ValueError is classified as non-retryable."""
        error_type, is_retryable = classify_error(ValueError("invalid value"))
        assert error_type == "ValueError"
        assert is_retryable is False

    def test_classify_non_retryable_key_error(self):
        """KeyError is classified as non-retryable."""
        error_type, is_retryable = classify_error(KeyError("missing key"))
        assert error_type == "KeyError"
        assert is_retryable is False

    def test_classify_unknown_error_not_retryable(self):
        """Unknown errors default to non-retryable for safety."""

        class CustomError(Exception):
            pass

        error_type, is_retryable = classify_error(CustomError("custom error"))
        assert error_type == "CustomError"
        assert is_retryable is False

    def test_classify_with_rate_limit_message(self):
        """Error with 'rate limit' in message is retryable."""
        error = Exception("rate limit exceeded")
        error_type, is_retryable = classify_error(error)
        # Message heuristic makes it retryable
        assert is_retryable is True

    def test_is_retryable_with_custom_set(self):
        """Custom retryable error set works."""
        error = ValueError("custom retryable")
        # By default ValueError is not retryable
        assert is_retryable_error(error) is False
        # With custom set, it is
        assert is_retryable_error(error, {"ValueError"}) is True


class TestErrorInfo:
    """Test ErrorInfo model (AC6)."""

    def test_error_info_creation(self):
        """ErrorInfo is created with required fields."""
        info = ErrorInfo(
            type="TimeoutError",
            message="Request timed out",
            node="call_api",
        )
        assert info.type == "TimeoutError"
        assert info.message == "Request timed out"
        assert info.node == "call_api"
        assert info.timestamp is not None
        assert info.retry_count == 0
        assert info.is_retryable is False

    def test_error_info_with_action(self):
        """ErrorInfo captures action name."""
        info = ErrorInfo(
            type="ConnectionError",
            message="Connection refused",
            node="fetch_data",
            action="http.get",
            is_retryable=True,
        )
        assert info.action == "http.get"
        assert info.is_retryable is True

    def test_create_error_info_helper(self):
        """create_error_info helper correctly classifies errors."""
        try:
            raise TimeoutError("connection timed out")
        except Exception as e:
            info = create_error_info(e, "my_node", action="http.request")

        assert info.type == "TimeoutError"
        assert info.is_retryable is True
        assert info.node == "my_node"
        assert info.action == "http.request"

    def test_create_error_info_with_traceback(self):
        """create_error_info captures traceback when enabled."""
        try:
            raise ValueError("test error")
        except Exception as e:
            info = create_error_info(e, "node1", capture_traceback=True)

        assert info.traceback is not None
        assert "ValueError" in info.traceback


class TestErrorHandlingSettings:
    """Test settings models (AC1, AC2)."""

    def test_default_settings_raise_mode(self):
        """Default settings use raise mode for backward compatibility (AC2)."""
        settings = ErrorHandlingSettings.default()
        assert settings.mode == ErrorMode.RAISE
        assert settings.max_retries == 3
        assert settings.retry_delay == 1.0
        assert settings.backoff_multiplier == 2.0

    def test_settings_from_yaml_graceful(self):
        """Parse graceful mode settings from YAML config."""
        config = {
            "mode": "graceful",
            "max_retries": 5,
            "retry_delay": 2.0,
            "capture_traceback": True,
        }
        settings = ErrorHandlingSettings.from_yaml(config)
        assert settings.mode == ErrorMode.GRACEFUL
        assert settings.max_retries == 5
        assert settings.retry_delay == 2.0
        assert settings.capture_traceback is True

    def test_settings_from_yaml_retry(self):
        """Parse retry mode settings from YAML config (AC1)."""
        config = {
            "mode": "retry",
            "max_retries": 10,
            "retry_delay": 0.5,
            "backoff_multiplier": 3.0,
            "retryable_errors": ["timeout", "connection_error"],
        }
        settings = ErrorHandlingSettings.from_yaml(config)
        assert settings.mode == ErrorMode.RETRY
        assert settings.max_retries == 10
        assert settings.backoff_multiplier == 3.0
        # Error aliases are expanded
        assert "TimeoutError" in settings.retryable_errors
        assert "ConnectionError" in settings.retryable_errors

    def test_settings_with_error_responses(self):
        """Parse error_responses templates."""
        config = {
            "mode": "graceful",
            "error_responses": {
                "validation_error": {
                    "status": 422,
                    "body": {"error": "validation_failed"},
                }
            },
        }
        settings = ErrorHandlingSettings.from_yaml(config)
        assert "validation_error" in settings.error_responses
        assert settings.error_responses["validation_error"].status == 422


class TestNodeErrorSettings:
    """Test node-level error settings (AC4)."""

    def test_parse_node_error_settings(self):
        """Parse on_error block from node config."""
        on_error = {
            "mode": "retry",
            "max_retries": 5,
            "fallback": "use_cache",
        }
        settings = parse_node_error_settings(on_error)
        assert settings.mode == ErrorMode.RETRY
        assert settings.max_retries == 5
        assert settings.fallback == "use_cache"

    def test_node_settings_merge_with_global(self):
        """Node settings override global settings (AC4)."""
        global_settings = ErrorHandlingSettings(
            mode=ErrorMode.GRACEFUL,
            max_retries=3,
            retry_delay=1.0,
        )
        node_settings = NodeErrorSettings(
            max_retries=10,
            fallback="fallback_node",
        )

        merged = node_settings.merge_with_global(global_settings)
        # Node override
        assert merged.max_retries == 10
        assert merged.fallback == "fallback_node"
        # Inherited from global
        assert merged.mode == ErrorMode.GRACEFUL
        assert merged.retry_delay == 1.0


class TestRetryPolicy:
    """Test retry logic (AC3)."""

    def test_retry_policy_defaults(self):
        """RetryPolicy has sensible defaults."""
        policy = RetryPolicy()
        assert policy.max_retries == 3
        assert policy.retry_delay == 1.0
        assert policy.backoff_multiplier == 2.0
        assert policy.retryable_errors == RETRYABLE_ERRORS

    def test_retry_policy_delay_calculation(self):
        """Exponential backoff delays are calculated correctly."""
        policy = RetryPolicy(
            retry_delay=1.0,
            backoff_multiplier=2.0,
            max_delay=60.0,
            jitter=0,  # Disable jitter for deterministic test
        )
        # Delays: 1s, 2s, 4s, 8s...
        assert policy.get_delay(0) == 1.0
        assert policy.get_delay(1) == 2.0
        assert policy.get_delay(2) == 4.0
        assert policy.get_delay(3) == 8.0

    def test_retry_policy_max_delay_cap(self):
        """Delay is capped at max_delay."""
        policy = RetryPolicy(
            retry_delay=10.0,
            backoff_multiplier=10.0,
            max_delay=30.0,
            jitter=0,
        )
        # 10 * 10 = 100, but capped at 30
        assert policy.get_delay(1) == 30.0

    def test_retry_policy_is_retryable(self):
        """Policy correctly identifies retryable errors."""
        policy = RetryPolicy()
        assert policy.is_retryable(TimeoutError("test")) is True
        assert policy.is_retryable(ValueError("test")) is False


class TestWithRetrySync:
    """Test synchronous retry wrapper (AC3)."""

    def test_with_retry_success_first_try(self):
        """Function succeeds on first try."""
        calls = []

        def func():
            calls.append(1)
            return "success"

        policy = RetryPolicy(max_retries=3)
        result = with_retry_sync(func, policy)
        assert result == "success"
        assert len(calls) == 1

    def test_with_retry_success_after_retries(self):
        """Function succeeds after retries."""
        attempts = [0]

        def func():
            attempts[0] += 1
            if attempts[0] < 3:
                raise TimeoutError("timeout")
            return "success"

        policy = RetryPolicy(max_retries=3, retry_delay=0.01, jitter=0)
        result = with_retry_sync(func, policy)
        assert result == "success"
        assert attempts[0] == 3

    def test_with_retry_exhausted(self):
        """Raises after max retries exhausted."""

        def func():
            raise TimeoutError("always fails")

        policy = RetryPolicy(max_retries=2, retry_delay=0.01, jitter=0)
        with pytest.raises(TimeoutError):
            with_retry_sync(func, policy)

    def test_with_retry_non_retryable_error(self):
        """Non-retryable errors are raised immediately."""
        attempts = [0]

        def func():
            attempts[0] += 1
            raise ValueError("not retryable")

        policy = RetryPolicy(max_retries=3, retry_delay=0.01)
        with pytest.raises(ValueError):
            with_retry_sync(func, policy)
        # Only one attempt, no retries
        assert attempts[0] == 1

    def test_with_retry_callback(self):
        """on_retry callback is called on each retry."""
        retry_info = []

        def on_retry(error, attempt, delay):
            retry_info.append({"attempt": attempt, "delay": delay})

        attempts = [0]

        def func():
            attempts[0] += 1
            if attempts[0] < 3:
                raise TimeoutError("timeout")
            return "success"

        policy = RetryPolicy(max_retries=3, retry_delay=0.01, jitter=0)
        with_retry_sync(func, policy, on_retry=on_retry)

        assert len(retry_info) == 2
        assert retry_info[0]["attempt"] == 0
        assert retry_info[1]["attempt"] == 1


class TestWithRetryAsync:
    """Test async retry wrapper (AC3)."""

    def test_with_retry_async_success(self):
        """Async function succeeds after retries."""
        attempts = [0]

        async def func():
            attempts[0] += 1
            if attempts[0] < 2:
                raise ConnectionError("connection failed")
            return "success"

        async def run_test():
            policy = RetryPolicy(max_retries=3, retry_delay=0.01, jitter=0)
            result = await with_retry(func, policy)
            return result

        result = asyncio.run(run_test())
        assert result == "success"
        assert attempts[0] == 2

    def test_with_retry_async_exhausted(self):
        """Raises after max retries exhausted."""

        async def func():
            raise TimeoutError("always fails")

        async def run_test():
            policy = RetryPolicy(max_retries=2, retry_delay=0.01, jitter=0)
            await with_retry(func, policy)

        with pytest.raises(TimeoutError):
            asyncio.run(run_test())


class TestErrorHandler:
    """Test ErrorHandler class (AC2, AC5, AC6)."""

    def test_handler_raise_mode(self):
        """Raise mode re-raises exceptions (AC2 backward compat)."""
        handler = ErrorHandler(mode=ErrorMode.RAISE)
        with pytest.raises(ValueError):
            handler.handle_sync(ValueError("test"), "node1")

    def test_handler_graceful_mode(self):
        """Graceful mode captures error in state (AC6)."""
        handler = ErrorHandler(mode=ErrorMode.GRACEFUL)
        result = handler.handle_sync(TimeoutError("timeout"), "node1", "http.get")

        assert "__error__" in result
        assert result["__error__"]["type"] == "TimeoutError"
        assert result["__error__"]["node"] == "node1"
        assert result["__error__"]["action"] == "http.get"

    def test_handler_with_fallback(self):
        """Fallback node is stored for routing (AC5)."""
        handler = ErrorHandler(
            mode=ErrorMode.GRACEFUL,
            fallback="fallback_node",
        )
        result = handler.handle_sync(ValueError("error"), "node1")

        assert "__error__" in result
        # Note: fallback is set but not directly in result
        # It's accessed via handler.get_fallback_node()
        assert handler.get_fallback_node() == "fallback_node"

    def test_handler_from_settings(self):
        """Create handler from settings."""
        settings = ErrorHandlingSettings(
            mode=ErrorMode.RETRY,
            max_retries=5,
            retry_delay=0.5,
        )
        handler = ErrorHandler.from_settings(settings)

        assert handler.mode == ErrorMode.RETRY
        assert handler.retry_policy.max_retries == 5
        assert handler.retry_policy.retry_delay == 0.5


class TestErrorResponseRenderer:
    """Test HTTP error response rendering (AC7)."""

    def test_render_validation_error(self):
        """ValidationError maps to 422."""
        error_info = ErrorInfo(
            type="ValidationError",
            message="Invalid email format",
            node="validate_input",
        )
        response = render_error_response(error_info)
        assert response["status"] == 422
        assert response["body"]["error"] == "validation_error"

    def test_render_timeout_error(self):
        """TimeoutError maps to 504."""
        error_info = ErrorInfo(
            type="TimeoutError",
            message="Request timed out",
            node="call_api",
        )
        response = render_error_response(error_info)
        assert response["status"] == 504

    def test_render_with_custom_template(self):
        """Custom templates override defaults."""
        templates = {
            "CustomError": ErrorResponseConfig(
                status=418,
                body={"error": "teapot", "msg": "{{ error.message }}"},
            )
        }
        renderer = ErrorResponseRenderer(templates)

        error_info = ErrorInfo(
            type="CustomError",
            message="I'm a teapot",
            node="teapot_node",
        )
        response = renderer.render(error_info)

        assert response["status"] == 418
        assert response["body"]["error"] == "teapot"
        assert "teapot" in response["body"]["msg"]

    def test_render_fallback_to_internal_error(self):
        """Unknown errors fall back to 500."""
        error_info = ErrorInfo(
            type="UnknownError",
            message="Something went wrong",
            node="mystery_node",
        )
        response = render_error_response(error_info)
        assert response["status"] == 500


class TestCreateErrorHandler:
    """Test factory function."""

    def test_create_from_dict(self):
        """Create handler from configuration dict."""
        config = {
            "mode": "graceful",
            "max_retries": 5,
            "retry_delay": 2.0,
        }
        handler = create_error_handler(config)
        assert handler.mode == ErrorMode.GRACEFUL
        assert handler.retry_policy.max_retries == 5

    def test_create_with_fallback(self):
        """Handler with fallback from config."""
        config = {
            "mode": "graceful",
            "fallback": "cache_node",
        }
        handler = create_error_handler(config)
        assert handler.get_fallback_node() == "cache_node"

    def test_create_with_unknown_mode(self):
        """Unknown mode defaults to raise."""
        config = {"mode": "invalid_mode"}
        handler = create_error_handler(config)
        assert handler.mode == ErrorMode.RAISE


class TestClearOnSuccess:
    """Test clear_on_success behavior (AC9)."""

    def test_clear_on_success_default(self):
        """clear_on_success is True by default."""
        settings = ErrorHandlingSettings.default()
        assert settings.clear_on_success is True

    def test_handler_should_clear_error(self):
        """Handler reports clear_on_success setting."""
        handler = ErrorHandler(clear_on_success=True)
        assert handler.should_clear_error() is True

        handler = ErrorHandler(clear_on_success=False)
        assert handler.should_clear_error() is False


class TestErrorActions:
    """Test error.* actions (AC8)."""

    def test_error_is_retryable_action(self):
        """error.is_retryable checks __error__ field."""
        from the_edge_agent.actions.error_actions import register_actions

        registry = {}
        engine = MagicMock()
        register_actions(registry, engine)

        async def run_test():
            # With retryable error
            state = {
                "__error__": {
                    "type": "TimeoutError",
                    "is_retryable": True,
                }
            }
            result1 = await registry["error.is_retryable"](state)

            # With non-retryable error
            state = {
                "__error__": {
                    "type": "ValueError",
                    "is_retryable": False,
                }
            }
            result2 = await registry["error.is_retryable"](state)

            # With no error
            state = {}
            result3 = await registry["error.is_retryable"](state)

            return result1, result2, result3

        result1, result2, result3 = asyncio.run(run_test())
        assert result1 is True
        assert result2 is False
        assert result3 is False

    def test_error_clear_action(self):
        """error.clear removes __error__ from state."""
        from the_edge_agent.actions.error_actions import register_actions

        registry = {}
        engine = MagicMock()
        register_actions(registry, engine)

        async def run_test():
            state = {"__error__": {"type": "SomeError"}}
            return await registry["error.clear"](state)

        result = asyncio.run(run_test())
        assert result["__error__"] is None

    def test_error_has_action(self):
        """error.has checks for error presence."""
        from the_edge_agent.actions.error_actions import register_actions

        registry = {}
        engine = MagicMock()
        register_actions(registry, engine)

        async def run_test():
            # Has error
            state = {"__error__": {"type": "SomeError"}}
            result1 = await registry["error.has"](state)

            # No error
            state = {}
            result2 = await registry["error.has"](state)
            return result1, result2

        result1, result2 = asyncio.run(run_test())
        assert result1 is True
        assert result2 is False

    def test_error_type_action(self):
        """error.type returns error type string."""
        from the_edge_agent.actions.error_actions import register_actions

        registry = {}
        engine = MagicMock()
        register_actions(registry, engine)

        async def run_test():
            state = {"__error__": {"type": "TimeoutError"}}
            result1 = await registry["error.type"](state)

            # No error
            state = {}
            result2 = await registry["error.type"](state)
            return result1, result2

        result1, result2 = asyncio.run(run_test())
        assert result1 == "TimeoutError"
        assert result2 is None


class TestIntegration:
    """Integration tests with YAMLEngine."""

    def test_yaml_engine_parses_error_settings(self):
        """YAMLEngine parses settings.error_handling block."""
        from the_edge_agent import YAMLEngine

        yaml_content = """
name: test-agent
state_schema:
  input: str

settings:
  error_handling:
    mode: graceful
    max_retries: 5
    retry_delay: 2.0
    capture_traceback: false

nodes:
  - name: start
    run: |
      return {"output": "done"}

edges:
  - from: __start__
    to: start
  - from: start
    to: __end__
"""
        import tempfile

        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(yaml_content)
            f.flush()

            engine = YAMLEngine()
            graph = engine.load_from_file(f.name)

            # Check settings were parsed
            assert engine._error_handling_settings is not None
            assert engine._error_handling_settings.mode == ErrorMode.GRACEFUL
            assert engine._error_handling_settings.max_retries == 5

    def test_node_on_error_graceful(self):
        """Node with on_error captures error gracefully."""
        from the_edge_agent import YAMLEngine

        yaml_content = """
name: test-agent
state_schema:
  input: str

nodes:
  - name: failing_node
    run: |
      raise ValueError("intentional failure")
    on_error:
      mode: graceful
      capture_traceback: true

edges:
  - from: __start__
    to: failing_node
  - from: failing_node
    to: __end__
"""
        import tempfile

        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(yaml_content)
            f.flush()

            engine = YAMLEngine()
            graph = engine.load_from_file(f.name)

            events = list(graph.invoke({"input": "test"}))
            # Should complete with final event, not error
            final = events[-1]
            assert final["type"] == "final"
            # Error should be captured in state
            assert "__error__" in final["state"]
            assert final["state"]["__error__"]["type"] == "ValueError"
