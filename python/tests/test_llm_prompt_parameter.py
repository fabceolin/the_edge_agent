"""
Tests for TEA-LLM-005: Add `prompt` Parameter Support to Python LLM Actions.

This module tests the Rust parity feature that allows llm.call, llm.stream,
and llm.tools to accept a `prompt` parameter as an alternative to `messages`.
"""

import unittest
from unittest.mock import MagicMock, patch
import sys


class TestConvertPromptToMessages(unittest.TestCase):
    """Test the _convert_prompt_to_messages helper function directly."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.mock_engine = MagicMock()
        self.mock_engine.actions_registry = {}

        from the_edge_agent.actions.llm_actions import register_actions

        register_actions(self.registry, self.mock_engine)

    def _create_mock_openai(self):
        """Create mock OpenAI module and client."""
        mock_openai_module = MagicMock()
        mock_client = MagicMock()
        mock_openai_module.OpenAI.return_value = mock_client
        mock_openai_module.AzureOpenAI.return_value = mock_client
        mock_openai_module.APIError = Exception
        mock_openai_module.APIConnectionError = Exception
        mock_openai_module.RateLimitError = Exception
        mock_openai_module.APITimeoutError = Exception
        return mock_openai_module, mock_client

    def test_messages_takes_precedence(self):
        """When both messages and prompt provided, messages takes precedence."""
        messages = [{"role": "user", "content": "Hello from messages"}]

        mock_openai_module, mock_client = self._create_mock_openai()

        mock_response = MagicMock()
        mock_response.choices = [MagicMock()]
        mock_response.choices[0].message.content = "Response"
        mock_response.usage = None
        mock_client.chat.completions.create.return_value = mock_response

        with patch.dict(sys.modules, {"openai": mock_openai_module}):
            # Re-register to pick up the patched module
            self.registry = {}
            from the_edge_agent.actions.llm_actions import register_actions

            register_actions(self.registry, self.mock_engine)

            result = self.registry["llm.call"](
                state={},
                model="gpt-4",
                messages=messages,
                prompt="This should be ignored",
            )

            # Verify messages was used, not prompt
            call_args = mock_client.chat.completions.create.call_args
            self.assertEqual(call_args.kwargs["messages"], messages)

    def test_prompt_only_converts_to_user_message(self):
        """When only prompt provided, converts to single user message."""
        mock_openai_module, mock_client = self._create_mock_openai()

        mock_response = MagicMock()
        mock_response.choices = [MagicMock()]
        mock_response.choices[0].message.content = "Response"
        mock_response.usage = None
        mock_client.chat.completions.create.return_value = mock_response

        with patch.dict(sys.modules, {"openai": mock_openai_module}):
            self.registry = {}
            from the_edge_agent.actions.llm_actions import register_actions

            register_actions(self.registry, self.mock_engine)

            result = self.registry["llm.call"](
                state={},
                model="gpt-4",
                prompt="Hello from prompt",
            )

            # Verify prompt was converted to messages
            call_args = mock_client.chat.completions.create.call_args
            expected_messages = [{"role": "user", "content": "Hello from prompt"}]
            self.assertEqual(call_args.kwargs["messages"], expected_messages)

    def test_prompt_with_system_prepends_system_message(self):
        """When prompt and system provided, system message is prepended."""
        mock_openai_module, mock_client = self._create_mock_openai()

        mock_response = MagicMock()
        mock_response.choices = [MagicMock()]
        mock_response.choices[0].message.content = "Response"
        mock_response.usage = None
        mock_client.chat.completions.create.return_value = mock_response

        with patch.dict(sys.modules, {"openai": mock_openai_module}):
            self.registry = {}
            from the_edge_agent.actions.llm_actions import register_actions

            register_actions(self.registry, self.mock_engine)

            result = self.registry["llm.call"](
                state={},
                model="gpt-4",
                prompt="Hello from prompt",
                system="You are a helpful assistant",
            )

            # Verify both system and user messages
            call_args = mock_client.chat.completions.create.call_args
            expected_messages = [
                {"role": "system", "content": "You are a helpful assistant"},
                {"role": "user", "content": "Hello from prompt"},
            ]
            self.assertEqual(call_args.kwargs["messages"], expected_messages)

    def test_neither_prompt_nor_messages_returns_error(self):
        """When neither prompt nor messages provided, returns error."""
        result = self.registry["llm.call"](
            state={},
            model="gpt-4",
        )

        self.assertIn("error", result)
        self.assertEqual(result["success"], False)
        self.assertIn("prompt or messages", result["error"])


class TestLlmCallPromptParameter(unittest.TestCase):
    """Test llm.call with prompt parameter."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.mock_engine = MagicMock()
        self.mock_engine.actions_registry = {}

        from the_edge_agent.actions.llm_actions import register_actions

        register_actions(self.registry, self.mock_engine)

    def _create_mock_openai(self):
        """Create mock OpenAI module and client."""
        mock_openai_module = MagicMock()
        mock_client = MagicMock()
        mock_openai_module.OpenAI.return_value = mock_client
        mock_openai_module.AzureOpenAI.return_value = mock_client
        mock_openai_module.APIError = Exception
        mock_openai_module.APIConnectionError = Exception
        mock_openai_module.RateLimitError = Exception
        mock_openai_module.APITimeoutError = Exception
        return mock_openai_module, mock_client

    def test_llm_call_with_prompt_succeeds(self):
        """llm.call works with prompt parameter."""
        mock_openai_module, mock_client = self._create_mock_openai()

        mock_response = MagicMock()
        mock_response.choices = [MagicMock()]
        mock_response.choices[0].message.content = "Hello!"
        mock_response.usage = {"prompt_tokens": 5, "completion_tokens": 2}
        mock_client.chat.completions.create.return_value = mock_response

        with patch.dict(sys.modules, {"openai": mock_openai_module}):
            self.registry = {}
            from the_edge_agent.actions.llm_actions import register_actions

            register_actions(self.registry, self.mock_engine)

            result = self.registry["llm.call"](
                state={},
                model="gpt-4",
                prompt="Say hello",
            )

            self.assertEqual(result["content"], "Hello!")
            self.assertNotIn("error", result)

    def test_llm_call_existing_messages_unchanged(self):
        """Existing messages-based calls work unchanged (regression test)."""
        mock_openai_module, mock_client = self._create_mock_openai()

        mock_response = MagicMock()
        mock_response.choices = [MagicMock()]
        mock_response.choices[0].message.content = "Response"
        mock_response.usage = None
        mock_client.chat.completions.create.return_value = mock_response

        with patch.dict(sys.modules, {"openai": mock_openai_module}):
            self.registry = {}
            from the_edge_agent.actions.llm_actions import register_actions

            register_actions(self.registry, self.mock_engine)

            messages = [
                {"role": "system", "content": "System prompt"},
                {"role": "user", "content": "User message"},
            ]
            result = self.registry["llm.call"](
                state={},
                model="gpt-4",
                messages=messages,
            )

            call_args = mock_client.chat.completions.create.call_args
            self.assertEqual(call_args.kwargs["messages"], messages)


class TestLlmStreamPromptParameter(unittest.TestCase):
    """Test llm.stream with prompt parameter."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.mock_engine = MagicMock()
        self.mock_engine.actions_registry = {}

        from the_edge_agent.actions.llm_actions import register_actions

        register_actions(self.registry, self.mock_engine)

    def _create_mock_openai(self):
        """Create mock OpenAI module and client."""
        mock_openai_module = MagicMock()
        mock_client = MagicMock()
        mock_openai_module.OpenAI.return_value = mock_client
        mock_openai_module.AzureOpenAI.return_value = mock_client
        mock_openai_module.APIError = Exception
        mock_openai_module.APIConnectionError = Exception
        mock_openai_module.RateLimitError = Exception
        mock_openai_module.APITimeoutError = Exception
        return mock_openai_module, mock_client

    def test_llm_stream_with_prompt_succeeds(self):
        """llm.stream works with prompt parameter."""
        mock_openai_module, mock_client = self._create_mock_openai()

        # Mock streaming response
        mock_chunk = MagicMock()
        mock_chunk.choices = [MagicMock()]
        mock_chunk.choices[0].delta.content = "Hello!"
        mock_chunk.usage = None

        mock_client.chat.completions.create.return_value = iter([mock_chunk])

        with patch.dict(sys.modules, {"openai": mock_openai_module}):
            self.registry = {}
            from the_edge_agent.actions.llm_actions import register_actions

            register_actions(self.registry, self.mock_engine)

            result = self.registry["llm.stream"](
                state={},
                model="gpt-4",
                prompt="Say hello",
            )

            self.assertEqual(result["content"], "Hello!")
            self.assertEqual(result["streamed"], True)

    def test_llm_stream_with_prompt_and_system(self):
        """llm.stream works with prompt and system parameters."""
        mock_openai_module, mock_client = self._create_mock_openai()

        mock_chunk = MagicMock()
        mock_chunk.choices = [MagicMock()]
        mock_chunk.choices[0].delta.content = "Response"
        mock_chunk.usage = None

        mock_client.chat.completions.create.return_value = iter([mock_chunk])

        with patch.dict(sys.modules, {"openai": mock_openai_module}):
            self.registry = {}
            from the_edge_agent.actions.llm_actions import register_actions

            register_actions(self.registry, self.mock_engine)

            result = self.registry["llm.stream"](
                state={},
                model="gpt-4",
                prompt="Hello",
                system="Be concise",
            )

            call_args = mock_client.chat.completions.create.call_args
            expected_messages = [
                {"role": "system", "content": "Be concise"},
                {"role": "user", "content": "Hello"},
            ]
            self.assertEqual(call_args.kwargs["messages"], expected_messages)

    def test_llm_stream_neither_returns_error(self):
        """llm.stream returns error when neither prompt nor messages provided."""
        result = self.registry["llm.stream"](
            state={},
            model="gpt-4",
        )

        self.assertIn("error", result)
        self.assertEqual(result["success"], False)
        self.assertIn("prompt or messages", result["error"])


class TestLlmToolsPromptParameter(unittest.TestCase):
    """Test llm.tools with prompt parameter."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.mock_engine = MagicMock()
        self.mock_engine.actions_registry = {}

        from the_edge_agent.actions.llm_actions import register_actions

        register_actions(self.registry, self.mock_engine)

    def _create_mock_openai(self):
        """Create mock OpenAI module and client."""
        mock_openai_module = MagicMock()
        mock_client = MagicMock()
        mock_openai_module.OpenAI.return_value = mock_client
        mock_openai_module.AzureOpenAI.return_value = mock_client
        mock_openai_module.APIError = Exception
        mock_openai_module.APIConnectionError = Exception
        mock_openai_module.RateLimitError = Exception
        mock_openai_module.APITimeoutError = Exception
        return mock_openai_module, mock_client

    def test_llm_tools_with_prompt_succeeds(self):
        """llm.tools works with prompt parameter."""
        mock_openai_module, mock_client = self._create_mock_openai()

        # Mock response without tool calls
        mock_response = MagicMock()
        mock_response.choices = [MagicMock()]
        mock_response.choices[0].message.content = "I don't need tools for this."
        mock_response.choices[0].message.tool_calls = None
        mock_client.chat.completions.create.return_value = mock_response

        tools = [
            {
                "name": "search",
                "description": "Search the web",
                "parameters": {"query": {"type": "string"}},
            }
        ]

        with patch.dict(sys.modules, {"openai": mock_openai_module}):
            self.registry = {}
            from the_edge_agent.actions.llm_actions import register_actions

            register_actions(self.registry, self.mock_engine)

            result = self.registry["llm.tools"](
                state={},
                model="gpt-4",
                prompt="What is 2+2?",
                tools=tools,
            )

            self.assertEqual(result["content"], "I don't need tools for this.")
            call_args = mock_client.chat.completions.create.call_args
            expected_messages = [{"role": "user", "content": "What is 2+2?"}]
            self.assertEqual(call_args.kwargs["messages"], expected_messages)

    def test_llm_tools_with_prompt_and_system(self):
        """llm.tools works with prompt and system parameters."""
        mock_openai_module, mock_client = self._create_mock_openai()

        mock_response = MagicMock()
        mock_response.choices = [MagicMock()]
        mock_response.choices[0].message.content = "Response"
        mock_response.choices[0].message.tool_calls = None
        mock_client.chat.completions.create.return_value = mock_response

        tools = [
            {
                "name": "search",
                "description": "Search the web",
                "parameters": {},
            }
        ]

        with patch.dict(sys.modules, {"openai": mock_openai_module}):
            self.registry = {}
            from the_edge_agent.actions.llm_actions import register_actions

            register_actions(self.registry, self.mock_engine)

            result = self.registry["llm.tools"](
                state={},
                model="gpt-4",
                prompt="Hello",
                system="Use tools wisely",
                tools=tools,
            )

            call_args = mock_client.chat.completions.create.call_args
            expected_messages = [
                {"role": "system", "content": "Use tools wisely"},
                {"role": "user", "content": "Hello"},
            ]
            self.assertEqual(call_args.kwargs["messages"], expected_messages)

    def test_llm_tools_neither_returns_error(self):
        """llm.tools returns error when neither prompt nor messages provided."""
        tools = [
            {
                "name": "search",
                "description": "Search the web",
                "parameters": {},
            }
        ]

        result = self.registry["llm.tools"](
            state={},
            model="gpt-4",
            tools=tools,
        )

        self.assertIn("error", result)
        self.assertEqual(result["success"], False)
        self.assertIn("prompt or messages", result["error"])


if __name__ == "__main__":
    unittest.main()
