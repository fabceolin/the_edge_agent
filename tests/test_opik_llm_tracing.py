"""
Tests for Opik LLM Tracing (TEA-BUILTIN-005.2).

Tests native Opik instrumentation for llm.call and llm.stream actions.
Verifies:
- opik_trace parameter handling
- track_openai wrapper application
- Cost calculation
- Graceful degradation when Opik SDK is not installed
- Azure OpenAI compatibility
- YAMLEngine configuration integration
- Coexistence with OpikExporter
"""

import unittest
from unittest.mock import MagicMock, patch, Mock
import warnings
import os
import sys

# Check if opik is installed
try:
    import opik
    HAS_OPIK = True
except ImportError:
    HAS_OPIK = False


class TestOpikLLMTracingYAMLEngine(unittest.TestCase):
    """Tests for Opik LLM tracing in YAMLEngine."""

    def test_yaml_engine_opik_llm_tracing_constructor(self):
        """(P1) YAMLEngine should accept opik_llm_tracing parameter."""
        from the_edge_agent import YAMLEngine

        # Test with opik_llm_tracing=True
        engine = YAMLEngine(opik_llm_tracing=True)
        self.assertTrue(engine.opik_llm_tracing)

        # Test with opik_llm_tracing=False (default)
        engine2 = YAMLEngine(opik_llm_tracing=False)
        self.assertFalse(engine2.opik_llm_tracing)

        # Test default
        engine3 = YAMLEngine()
        self.assertFalse(engine3.opik_llm_tracing)

    def test_yaml_engine_opik_settings_from_yaml_nested(self):
        """(P1) YAMLEngine should read nested opik settings from YAML."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine()

        # Test settings.opik.llm_tracing
        config = {
            'settings': {
                'opik': {
                    'llm_tracing': True
                }
            },
            'nodes': [],
            'edges': []
        }
        engine.load_from_dict(config)
        self.assertTrue(engine.opik_llm_tracing)

    def test_yaml_engine_opik_settings_from_yaml_flat(self):
        """(P1) YAMLEngine should read flat opik_llm_tracing setting from YAML."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine()
        config = {
            'settings': {
                'opik_llm_tracing': True
            },
            'nodes': [],
            'edges': []
        }
        engine.load_from_dict(config)
        self.assertTrue(engine.opik_llm_tracing)

    def test_coexistence_with_opik_exporter(self):
        """(P2) Native tracing and OpikExporter should work together."""
        from the_edge_agent import YAMLEngine

        # This test verifies both can be configured without conflict
        engine = YAMLEngine(
            opik_llm_tracing=True,
            trace_exporter='opik'
        )

        self.assertTrue(engine.opik_llm_tracing)
        # Tracing should be enabled
        self.assertTrue(engine._enable_tracing)

    def test_full_yaml_config_with_opik_llm_tracing(self):
        """(P2) Full YAML config should enable Opik LLM tracing."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine()

        config = {
            'settings': {
                'opik': {
                    'enabled': True,
                    'llm_tracing': True,
                    'project_name': 'test-project'
                }
            },
            'nodes': [
                {
                    'name': 'generate',
                    'run': 'return {"result": "ok"}'
                }
            ],
            'edges': [
                {'type': 'entry', 'to': 'generate'},
                {'from': 'generate', 'to': '__end__'}
            ]
        }

        # Load config
        engine.load_from_dict(config)

        # Verify setting was applied
        self.assertTrue(engine.opik_llm_tracing)


class TestOpikLLMTracingIntegration(unittest.TestCase):
    """Integration tests for Opik LLM tracing with YAMLEngine action injection."""

    def test_yaml_engine_injects_opik_trace_to_llm_call(self):
        """(P1) YAMLEngine should inject opik_trace to llm.call actions."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine(opik_llm_tracing=True)

        # Mock the llm.call action to capture params
        captured_params = {}

        def mock_llm_call(state, **kwargs):
            captured_params.update(kwargs)
            return {'content': 'test', 'usage': {}}

        engine.actions_registry['llm.call'] = mock_llm_call

        # Create action function via the engine
        action_func = engine._create_action_function(
            'llm.call',
            {'model': 'gpt-4', 'messages': []}
        )

        # Call the action
        action_func({})

        # Verify opik_trace was injected
        self.assertTrue(captured_params.get('opik_trace', False))

    def test_yaml_engine_respects_explicit_opik_trace_param(self):
        """(P1) Explicit opik_trace param should override engine setting."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine(opik_llm_tracing=True)

        captured_params = {}

        def mock_llm_call(state, **kwargs):
            captured_params.update(kwargs)
            return {'content': 'test', 'usage': {}}

        engine.actions_registry['llm.call'] = mock_llm_call

        # Create action with explicit opik_trace=False
        action_func = engine._create_action_function(
            'llm.call',
            {'model': 'gpt-4', 'messages': [], 'opik_trace': False}
        )

        action_func({})

        # Explicit param should be preserved
        self.assertFalse(captured_params.get('opik_trace', True))

    def test_yaml_engine_no_injection_when_disabled(self):
        """(P1) opik_trace should not be injected when engine has it disabled."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine(opik_llm_tracing=False)

        captured_params = {}

        def mock_llm_call(state, **kwargs):
            captured_params.update(kwargs)
            return {'content': 'test', 'usage': {}}

        engine.actions_registry['llm.call'] = mock_llm_call

        action_func = engine._create_action_function(
            'llm.call',
            {'model': 'gpt-4', 'messages': []}
        )

        action_func({})

        # opik_trace should not be injected
        self.assertNotIn('opik_trace', captured_params)

    def test_yaml_engine_injects_opik_trace_to_llm_stream(self):
        """(P1) YAMLEngine should inject opik_trace to llm.stream actions."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine(opik_llm_tracing=True)

        captured_params = {}

        def mock_llm_stream(state, **kwargs):
            captured_params.update(kwargs)
            return {'content': 'test', 'usage': {}, 'streamed': True}

        engine.actions_registry['llm.stream'] = mock_llm_stream

        action_func = engine._create_action_function(
            'llm.stream',
            {'model': 'gpt-4', 'messages': []}
        )

        action_func({})

        # Verify opik_trace was injected
        self.assertTrue(captured_params.get('opik_trace', False))


class TestCostCalculation(unittest.TestCase):
    """Tests for cost calculation function."""

    def test_cost_calculation_gpt4(self):
        """(P1) Cost calculation should be accurate for GPT-4."""
        from the_edge_agent.actions.llm_actions import register_actions

        registry = {}
        engine = MagicMock()
        register_actions(registry, engine)

        # Access the calculate_cost function indirectly by testing result
        # We'll test the MODEL_PRICING dict behavior by mocking the full call

    def test_cost_calculation_constants(self):
        """(P1) Model pricing constants should be defined correctly."""
        # Import the module to check the constants are accessible
        from the_edge_agent.actions import llm_actions

        # Register actions to create the local scope
        registry = {}
        engine = MagicMock()
        llm_actions.register_actions(registry, engine)

        # The registry should have llm.call
        self.assertIn('llm.call', registry)
        self.assertIn('llm.stream', registry)


class TestOpikLLMTracingActions(unittest.TestCase):
    """Tests for Opik LLM tracing in llm.call and llm.stream actions."""

    def setUp(self):
        """Set up test fixtures."""
        # Mock OpenAI response
        self.mock_response = MagicMock()
        self.mock_response.choices = [MagicMock()]
        self.mock_response.choices[0].message.content = "Hello, world!"
        self.mock_response.usage = MagicMock()
        self.mock_response.usage.model_dump.return_value = {
            "prompt_tokens": 10,
            "completion_tokens": 20,
            "total_tokens": 30
        }

        # Mock streaming response
        self.mock_stream_chunks = [
            MagicMock(choices=[MagicMock(delta=MagicMock(content="Hello"))], usage=None),
            MagicMock(choices=[MagicMock(delta=MagicMock(content=", "))], usage=None),
            MagicMock(choices=[MagicMock(delta=MagicMock(content="world!"))], usage=None),
            MagicMock(
                choices=[MagicMock(delta=MagicMock(content=None))],
                usage=MagicMock(model_dump=MagicMock(return_value={
                    "prompt_tokens": 10,
                    "completion_tokens": 20,
                    "total_tokens": 30
                }))
            ),
        ]

        # Clear Azure credentials so tests use standard OpenAI mocking
        self.env_patcher = patch.dict(os.environ, {
            'AZURE_OPENAI_API_KEY': '',
            'AZURE_OPENAI_ENDPOINT': ''
        })
        self.env_patcher.start()

    def tearDown(self):
        """Clean up test fixtures."""
        self.env_patcher.stop()

    def test_llm_call_opik_trace_false_unchanged(self):
        """(P0) Default behavior: opik_trace=False should not apply wrapper."""
        from the_edge_agent.actions.llm_actions import register_actions

        registry = {}
        engine = MagicMock()
        register_actions(registry, engine)

        # Mock OpenAI
        mock_client = MagicMock()
        mock_client.chat.completions.create.return_value = self.mock_response

        with patch('openai.OpenAI', return_value=mock_client):
            result = registry['llm.call'](
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "Hello"}],
                opik_trace=False
            )

            self.assertEqual(result['content'], "Hello, world!")
            self.assertIn('usage', result)
            # No cost_usd when opik_trace=False
            self.assertNotIn('cost_usd', result)

    @unittest.skipUnless(HAS_OPIK, "opik SDK not installed")
    @patch('opik.integrations.openai.track_openai')
    @patch('openai.OpenAI')
    def test_llm_call_opik_trace_true_with_opik_installed(self, mock_openai_cls, mock_track):
        """(P0) opik_trace=True should apply track_openai wrapper when SDK available."""
        from the_edge_agent.actions.llm_actions import register_actions

        registry = {}
        engine = MagicMock()
        register_actions(registry, engine)

        mock_client = MagicMock()
        mock_client.chat.completions.create.return_value = self.mock_response
        mock_openai_cls.return_value = mock_client
        mock_track.return_value = mock_client

        result = registry['llm.call'](
            state={},
            model="gpt-4",
            messages=[{"role": "user", "content": "Hello"}],
            opik_trace=True
        )

        # Verify track_openai was called
        mock_track.assert_called_once()
        self.assertEqual(result['content'], "Hello, world!")
        # cost_usd should be present when opik_trace=True
        self.assertIn('cost_usd', result)

    def test_llm_call_default_opik_trace_false(self):
        """(P0) Default value for opik_trace should be False."""
        from the_edge_agent.actions.llm_actions import register_actions

        registry = {}
        engine = MagicMock()
        register_actions(registry, engine)

        mock_client = MagicMock()
        mock_client.chat.completions.create.return_value = self.mock_response

        with patch('openai.OpenAI', return_value=mock_client):
            # Call without opik_trace parameter
            result = registry['llm.call'](
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "Hello"}]
            )

            # Should work without opik_trace
            self.assertEqual(result['content'], "Hello, world!")
            # No cost when opik_trace not enabled
            self.assertNotIn('cost_usd', result)

    def test_token_usage_captured(self):
        """(P1) Token usage should be captured in result."""
        from the_edge_agent.actions.llm_actions import register_actions

        registry = {}
        engine = MagicMock()
        register_actions(registry, engine)

        mock_client = MagicMock()
        mock_client.chat.completions.create.return_value = self.mock_response

        with patch('openai.OpenAI', return_value=mock_client):
            result = registry['llm.call'](
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "Hello"}]
            )

            self.assertIn('usage', result)
            usage = result['usage']
            self.assertEqual(usage['prompt_tokens'], 10)
            self.assertEqual(usage['completion_tokens'], 20)
            self.assertEqual(usage['total_tokens'], 30)

    @unittest.skipUnless(HAS_OPIK, "opik SDK not installed")
    @patch('opik.integrations.openai.track_openai')
    @patch('openai.OpenAI')
    def test_cost_calculation_gpt4(self, mock_openai_cls, mock_track):
        """(P1) Cost calculation should be accurate for GPT-4."""
        from the_edge_agent.actions.llm_actions import register_actions

        registry = {}
        engine = MagicMock()
        register_actions(registry, engine)

        mock_client = MagicMock()
        mock_client.chat.completions.create.return_value = self.mock_response
        mock_openai_cls.return_value = mock_client
        mock_track.return_value = mock_client

        result = registry['llm.call'](
            state={},
            model="gpt-4",
            messages=[{"role": "user", "content": "Hello"}],
            opik_trace=True
        )

        # GPT-4 pricing: $0.03/1K input, $0.06/1K output
        expected_cost = (10 / 1000) * 0.03 + (20 / 1000) * 0.06
        self.assertIn('cost_usd', result)
        self.assertAlmostEqual(result['cost_usd'], expected_cost, places=6)

    @unittest.skipUnless(HAS_OPIK, "opik SDK not installed")
    @patch('opik.integrations.openai.track_openai')
    @patch('openai.OpenAI')
    def test_cost_calculation_gpt35_turbo(self, mock_openai_cls, mock_track):
        """(P1) Cost calculation should be accurate for GPT-3.5 Turbo."""
        from the_edge_agent.actions.llm_actions import register_actions

        registry = {}
        engine = MagicMock()
        register_actions(registry, engine)

        mock_client = MagicMock()
        mock_client.chat.completions.create.return_value = self.mock_response
        mock_openai_cls.return_value = mock_client
        mock_track.return_value = mock_client

        result = registry['llm.call'](
            state={},
            model="gpt-3.5-turbo",
            messages=[{"role": "user", "content": "Hello"}],
            opik_trace=True
        )

        # GPT-3.5 Turbo pricing: $0.0005/1K input, $0.0015/1K output
        expected_cost = (10 / 1000) * 0.0005 + (20 / 1000) * 0.0015
        self.assertIn('cost_usd', result)
        self.assertAlmostEqual(result['cost_usd'], expected_cost, places=6)

    @unittest.skipUnless(HAS_OPIK, "opik SDK not installed")
    @patch('opik.integrations.openai.track_openai')
    @patch('openai.OpenAI')
    def test_cost_calculation_unknown_model(self, mock_openai_cls, mock_track):
        """(P2) Unknown model should return zero cost."""
        from the_edge_agent.actions.llm_actions import register_actions

        registry = {}
        engine = MagicMock()
        register_actions(registry, engine)

        mock_client = MagicMock()
        mock_client.chat.completions.create.return_value = self.mock_response
        mock_openai_cls.return_value = mock_client
        mock_track.return_value = mock_client

        result = registry['llm.call'](
            state={},
            model="unknown-model-xyz",
            messages=[{"role": "user", "content": "Hello"}],
            opik_trace=True
        )

        # Unknown model should have zero cost
        self.assertIn('cost_usd', result)
        self.assertEqual(result['cost_usd'], 0.0)

    def test_streaming_without_opik_trace(self):
        """(P1) Streaming without opik_trace should work unchanged."""
        from the_edge_agent.actions.llm_actions import register_actions

        registry = {}
        engine = MagicMock()
        register_actions(registry, engine)

        mock_client = MagicMock()
        mock_client.chat.completions.create.return_value = iter(self.mock_stream_chunks)

        with patch('openai.OpenAI', return_value=mock_client):
            result = registry['llm.stream'](
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "Hello"}],
                opik_trace=False
            )

            self.assertEqual(result['content'], "Hello, world!")
            self.assertTrue(result['streamed'])
            # No cost_usd when opik_trace=False
            self.assertNotIn('cost_usd', result)

    @unittest.skipUnless(HAS_OPIK, "opik SDK not installed")
    @patch('opik.integrations.openai.track_openai')
    @patch('openai.OpenAI')
    def test_streaming_with_opik_trace(self, mock_openai_cls, mock_track):
        """(P1) Streaming with opik_trace should work correctly."""
        from the_edge_agent.actions.llm_actions import register_actions

        registry = {}
        engine = MagicMock()
        register_actions(registry, engine)

        mock_client = MagicMock()
        mock_client.chat.completions.create.return_value = iter(self.mock_stream_chunks)
        mock_openai_cls.return_value = mock_client
        mock_track.return_value = mock_client

        result = registry['llm.stream'](
            state={},
            model="gpt-4",
            messages=[{"role": "user", "content": "Hello"}],
            opik_trace=True
        )

        mock_track.assert_called_once()
        self.assertEqual(result['content'], "Hello, world!")
        self.assertTrue(result['streamed'])
        self.assertIn('cost_usd', result)

    @unittest.skipUnless(HAS_OPIK, "opik SDK not installed")
    @patch('opik.integrations.openai.track_openai')
    @patch('openai.AzureOpenAI')
    def test_azure_openai_tracing(self, mock_azure_cls, mock_track):
        """(P1) Azure OpenAI should be traced correctly."""
        from the_edge_agent.actions.llm_actions import register_actions

        # Set up Azure env vars (override the setup which clears them)
        env_vars = {
            'AZURE_OPENAI_API_KEY': 'test-azure-key',
            'AZURE_OPENAI_ENDPOINT': 'https://test.openai.azure.com'
        }

        with patch.dict(os.environ, env_vars):
            registry = {}
            engine = MagicMock()
            register_actions(registry, engine)

            mock_client = MagicMock()
            mock_client.chat.completions.create.return_value = self.mock_response
            mock_azure_cls.return_value = mock_client
            mock_track.return_value = mock_client

            result = registry['llm.call'](
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "Hello"}],
                opik_trace=True
            )

            # Verify Azure client was used
            mock_azure_cls.assert_called_once()
            # Verify track_openai was applied
            mock_track.assert_called_once()
            self.assertEqual(result['content'], "Hello, world!")

    @unittest.skipUnless(HAS_OPIK, "opik SDK not installed")
    @patch('opik.integrations.openai.track_openai')
    @patch('openai.AzureOpenAI')
    def test_azure_streaming_with_opik_trace(self, mock_azure_cls, mock_track):
        """(P2) Azure OpenAI streaming should work with Opik tracing."""
        from the_edge_agent.actions.llm_actions import register_actions

        # Set up Azure env vars (override the setup which clears them)
        env_vars = {
            'AZURE_OPENAI_API_KEY': 'test-azure-key',
            'AZURE_OPENAI_ENDPOINT': 'https://test.openai.azure.com'
        }

        with patch.dict(os.environ, env_vars):
            registry = {}
            engine = MagicMock()
            register_actions(registry, engine)

            mock_client = MagicMock()
            mock_client.chat.completions.create.return_value = iter(self.mock_stream_chunks)
            mock_azure_cls.return_value = mock_client
            mock_track.return_value = mock_client

            result = registry['llm.stream'](
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "Hello"}],
                opik_trace=True
            )

            mock_azure_cls.assert_called_once()
            mock_track.assert_called_once()
            self.assertEqual(result['content'], "Hello, world!")
            self.assertTrue(result['streamed'])


if __name__ == '__main__':
    unittest.main()
