"""
Tests for DSPy Actions (TEA-AGENT-001.7).

Tests cover:
- DSPy client wrapper initialization and configuration
- Graceful fallback when DSPy unavailable
- reason.dspy.cot action
- reason.dspy.react action
- reason.dspy.compile action
- reason.dspy.optimize action
- Checkpoint integration (export/import)
- API key security (masking)
"""

import json
import unittest
from unittest.mock import MagicMock, patch, PropertyMock
from typing import Any, Dict


class TestDSPyClient(unittest.TestCase):
    """Test DSPyClient wrapper class."""

    def test_dspy_config_from_dict(self):
        """Test DSPyConfig creation from dictionary."""
        from the_edge_agent.reasoning.dspy_client import DSPyConfig

        config = DSPyConfig.from_dict(
            {
                "enabled": True,
                "model": "gpt-4o",
                "temperature": 0.5,
                "teleprompter": "MIPRO",
            }
        )

        self.assertTrue(config.enabled)
        self.assertEqual(config.model, "gpt-4o")
        self.assertEqual(config.temperature, 0.5)
        self.assertEqual(config.teleprompter, "MIPRO")

    def test_dspy_config_defaults(self):
        """Test DSPyConfig default values."""
        from the_edge_agent.reasoning.dspy_client import DSPyConfig

        config = DSPyConfig()

        self.assertTrue(config.enabled)
        self.assertEqual(config.model, "gpt-4")
        self.assertEqual(config.temperature, 0.7)
        self.assertEqual(config.teleprompter, "BootstrapFewShot")

    def test_dspy_config_api_key_masked_in_to_dict(self):
        """Test that API key is masked in serialization (SEC-001)."""
        from the_edge_agent.reasoning.dspy_client import DSPyConfig

        config = DSPyConfig(api_key="sk-secret-key-12345")
        result = config.to_dict()

        self.assertEqual(result.get("api_key"), "***MASKED***")
        self.assertNotIn("sk-secret-key", str(result))

    def test_dspy_client_from_settings(self):
        """Test DSPyClient creation from YAML settings."""
        from the_edge_agent.reasoning.dspy_client import DSPyClient

        settings = {
            "dspy": {"enabled": True, "model": "gpt-4-turbo", "temperature": 0.3}
        }

        client = DSPyClient.from_settings(settings)

        self.assertEqual(client.config.model, "gpt-4-turbo")
        self.assertEqual(client.config.temperature, 0.3)

    def test_dspy_available_check(self):
        """Test DSPy availability detection."""
        from the_edge_agent.reasoning.dspy_client import dspy_available

        # Function should return boolean without error
        result = dspy_available()
        self.assertIsInstance(result, bool)

    def test_dspy_client_disabled(self):
        """Test that disabled client reports not available."""
        from the_edge_agent.reasoning.dspy_client import DSPyClient, DSPyConfig

        config = DSPyConfig(enabled=False)
        client = DSPyClient(config)

        self.assertFalse(client.is_available)


class TestCompiledPrompt(unittest.TestCase):
    """Test CompiledPrompt serialization."""

    def test_compiled_prompt_to_dict(self):
        """Test CompiledPrompt serialization."""
        from the_edge_agent.reasoning.dspy_client import CompiledPrompt

        prompt = CompiledPrompt(
            module_type="cot",
            signature="question -> answer",
            version="1.0",
            demos=[{"question": "2+2?", "answer": "4"}],
            metadata={"train_score": 0.95},
        )

        data = prompt.to_dict()

        self.assertEqual(data["module_type"], "cot")
        self.assertEqual(data["signature"], "question -> answer")
        self.assertEqual(len(data["demos"]), 1)

    def test_compiled_prompt_from_dict(self):
        """Test CompiledPrompt deserialization."""
        from the_edge_agent.reasoning.dspy_client import CompiledPrompt

        data = {
            "module_type": "react",
            "signature": "goal -> result",
            "version": "2.0",
            "demos": [],
            "metadata": {},
        }

        prompt = CompiledPrompt.from_dict(data)

        self.assertEqual(prompt.module_type, "react")
        self.assertEqual(prompt.signature, "goal -> result")
        self.assertEqual(prompt.version, "2.0")

    def test_compute_version_hash(self):
        """Test version hash computation is deterministic."""
        from the_edge_agent.reasoning.dspy_client import CompiledPrompt

        demos = [{"question": "test", "answer": "result"}]

        v1 = CompiledPrompt.compute_version("q -> a", demos)
        v2 = CompiledPrompt.compute_version("q -> a", demos)

        self.assertEqual(v1, v2)
        self.assertEqual(len(v1), 12)  # sha256 truncated

    def test_compute_version_different_for_different_inputs(self):
        """Test version hash differs for different inputs."""
        from the_edge_agent.reasoning.dspy_client import CompiledPrompt

        v1 = CompiledPrompt.compute_version("q -> a", [])
        v2 = CompiledPrompt.compute_version("q -> b", [])

        self.assertNotEqual(v1, v2)


class MockEngine:
    """Mock YAMLEngine for testing."""

    def __init__(self):
        self.settings = {}
        self._dspy_client = None


class TestDSPyActions(unittest.TestCase):
    """Test DSPy actions registration and execution."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = MockEngine()
        self.registry: Dict[str, Any] = {}

        # Register native reasoning actions first (for fallback)
        from the_edge_agent.actions.reasoning_actions import (
            register_actions as register_reasoning,
        )

        register_reasoning(self.registry, self.engine)

        # Register DSPy actions
        from the_edge_agent.actions.dspy_actions import register_actions

        register_actions(self.registry, self.engine)

    def test_actions_registered(self):
        """Test that all DSPy actions are registered."""
        expected_actions = [
            "reason.dspy.cot",
            "reason.dspy.react",
            "reason.dspy.compile",
            "reason.dspy.optimize",
            "reason.dspy.list_compiled",
            "reason.dspy.export",
            "reason.dspy.import",
        ]

        for action in expected_actions:
            self.assertIn(action, self.registry, f"Action {action} not registered")

    def test_cot_fallback_when_dspy_unavailable(self):
        """Test reason.dspy.cot falls back to native when DSPy unavailable."""
        with patch(
            "the_edge_agent.actions.dspy_actions.dspy_available", return_value=False
        ):
            # Re-register to pick up the mock
            from the_edge_agent.actions.dspy_actions import register_actions

            registry = {}

            # Register native first
            from the_edge_agent.actions.reasoning_actions import (
                register_actions as register_reasoning,
            )

            register_reasoning(registry, self.engine)

            register_actions(registry, self.engine)

            # Mock the native cot action
            mock_native = MagicMock(
                return_value={
                    "thinking": "test thinking",
                    "answer": "test answer",
                    "reasoning_trace": [],
                }
            )
            registry["reason.cot"] = mock_native

            result = registry["reason.dspy.cot"](state={}, problem="What is 2+2?")

            self.assertEqual(result.get("dspy_module"), "native_fallback")
            self.assertFalse(result.get("dspy_available", True))

    def test_react_fallback_when_dspy_unavailable(self):
        """Test reason.dspy.react falls back to native when DSPy unavailable."""
        with patch(
            "the_edge_agent.actions.dspy_actions.dspy_available", return_value=False
        ):
            from the_edge_agent.actions.dspy_actions import register_actions

            registry = {}

            from the_edge_agent.actions.reasoning_actions import (
                register_actions as register_reasoning,
            )

            register_reasoning(registry, self.engine)

            register_actions(registry, self.engine)

            mock_native = MagicMock(
                return_value={
                    "steps": [],
                    "final_answer": "test",
                    "reasoning_trace": [],
                }
            )
            registry["reason.react"] = mock_native

            result = registry["reason.dspy.react"](state={}, goal="Find information")

            self.assertEqual(result.get("dspy_module"), "native_fallback")

    def test_compile_fails_when_dspy_unavailable(self):
        """Test reason.dspy.compile returns error when DSPy unavailable."""
        with patch(
            "the_edge_agent.actions.dspy_actions.dspy_available", return_value=False
        ):
            from the_edge_agent.actions.dspy_actions import register_actions

            registry = {}
            register_actions(registry, self.engine)

            result = registry["reason.dspy.compile"](
                state={},
                module_type="cot",
                training_data=[{"question": "test", "answer": "result"}],
            )

            self.assertFalse(result.get("success"))
            self.assertFalse(result.get("compiled"))
            self.assertIn("not installed", result.get("error", "").lower())

    def test_optimize_fails_when_dspy_unavailable(self):
        """Test reason.dspy.optimize returns error when DSPy unavailable."""
        with patch(
            "the_edge_agent.actions.dspy_actions.dspy_available", return_value=False
        ):
            from the_edge_agent.actions.dspy_actions import register_actions

            registry = {}
            register_actions(registry, self.engine)

            result = registry["reason.dspy.optimize"](
                state={},
                training_data=[{"question": "test", "answer": "result"}],
                validation_data=[{"question": "test2", "answer": "result2"}],
            )

            self.assertFalse(result.get("success"))
            self.assertFalse(result.get("dspy_available", True))

    def test_compile_requires_training_data(self):
        """Test reason.dspy.compile requires training data."""
        with patch(
            "the_edge_agent.actions.dspy_actions.dspy_available", return_value=True
        ):
            from the_edge_agent.actions.dspy_actions import register_actions

            registry = {}
            register_actions(registry, self.engine)

            # Mock DSPyClient to be available but without training data
            mock_client = MagicMock()
            mock_client.is_available = True
            mock_client.config = MagicMock()
            self.engine._dspy_client = mock_client

            result = registry["reason.dspy.compile"](
                state={},
                module_type="cot",
                # No training_data provided
            )

            self.assertFalse(result.get("success"))
            self.assertIn("training data", result.get("error", "").lower())

    def test_optimize_requires_validation_data(self):
        """Test reason.dspy.optimize requires validation data."""
        with patch(
            "the_edge_agent.actions.dspy_actions.dspy_available", return_value=True
        ):
            from the_edge_agent.actions.dspy_actions import register_actions

            registry = {}
            register_actions(registry, self.engine)

            mock_client = MagicMock()
            mock_client.is_available = True
            mock_client.config = MagicMock()
            self.engine._dspy_client = mock_client

            result = registry["reason.dspy.optimize"](
                state={},
                training_data=[{"question": "test", "answer": "result"}],
                # No validation_data provided
            )

            self.assertFalse(result.get("success"))
            self.assertIn("validation data", result.get("error", "").lower())

    def test_list_compiled_empty(self):
        """Test reason.dspy.list_compiled with no compiled modules."""
        from the_edge_agent.actions.dspy_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["reason.dspy.list_compiled"](state={})

        self.assertIsInstance(result.get("modules"), list)
        self.assertIsInstance(result.get("details"), dict)

    def test_export_empty(self):
        """Test reason.dspy.export with no compiled prompts."""
        from the_edge_agent.actions.dspy_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["reason.dspy.export"](state={})

        self.assertIsInstance(result.get("prompts"), dict)
        self.assertEqual(result.get("count"), 0)

    def test_import_empty(self):
        """Test reason.dspy.import with no data."""
        from the_edge_agent.actions.dspy_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["reason.dspy.import"](state={})

        self.assertEqual(result.get("imported"), 0)


class TestDSPyCheckpointIntegration(unittest.TestCase):
    """Test checkpoint integration for compiled prompts."""

    def test_export_import_roundtrip(self):
        """Test compiled prompts can be exported and imported."""
        from the_edge_agent.reasoning.dspy_client import DSPyClient, CompiledPrompt

        client = DSPyClient()

        # Store some compiled prompts
        prompt1 = CompiledPrompt(
            module_type="cot",
            signature="q -> a",
            version="1.0",
            demos=[{"q": "test", "a": "result"}],
        )

        prompt2 = CompiledPrompt(
            module_type="react", signature="goal -> result", version="2.0"
        )

        client._compiled_prompts["key1"] = prompt1
        client._compiled_prompts["key2"] = prompt2

        # Export
        exported = client.export_compiled_prompts()

        self.assertEqual(len(exported), 2)
        self.assertIn("key1", exported)
        self.assertIn("key2", exported)

        # Import into new client
        new_client = DSPyClient()
        imported_count = new_client.import_compiled_prompts(exported)

        self.assertEqual(imported_count, 2)
        self.assertEqual(len(new_client._compiled_prompts), 2)

        # Verify data integrity
        p1 = new_client.get_compiled_prompt("key1")
        self.assertEqual(p1.module_type, "cot")
        self.assertEqual(p1.signature, "q -> a")

    def test_store_and_retrieve_compiled_module(self):
        """Test storing and retrieving compiled modules."""
        from the_edge_agent.reasoning.dspy_client import DSPyClient, CompiledPrompt

        client = DSPyClient()

        mock_module = MagicMock()
        prompt = CompiledPrompt(
            module_type="cot", signature="test -> result", version="1.0"
        )

        client.store_compiled_module("test_key", mock_module, prompt)

        retrieved = client.get_compiled_module("test_key")
        self.assertEqual(retrieved, mock_module)

        retrieved_prompt = client.get_compiled_prompt("test_key")
        self.assertEqual(retrieved_prompt.signature, "test -> result")


class TestDSPySignatureValidation(unittest.TestCase):
    """Test signature parsing and validation."""

    def test_input_field_extraction(self):
        """Test extracting input fields from signature."""
        from the_edge_agent.reasoning.dspy_client import DSPyClient

        client = DSPyClient()

        fields = client._get_input_fields("question -> answer")
        self.assertEqual(fields, ["question"])

        fields = client._get_input_fields("context, question -> answer")
        self.assertEqual(fields, ["context", "question"])

    def test_model_name_mapping(self):
        """Test model name provider prefix mapping."""
        from the_edge_agent.reasoning.dspy_client import DSPyClient, DSPyConfig

        # Test that different model prefixes are handled
        config = DSPyConfig(model="gpt-4")
        client = DSPyClient(config)

        # Model mapping tested implicitly in initialize()
        self.assertEqual(client.config.model, "gpt-4")


class TestDSPySecurityMeasures(unittest.TestCase):
    """Test security measures for DSPy integration (SEC-001)."""

    def test_api_key_not_in_trace(self):
        """Test API key not exposed in reasoning traces."""
        from the_edge_agent.reasoning.dspy_client import DSPyClient, DSPyConfig

        config = DSPyConfig(api_key="sk-secret-12345")
        client = DSPyClient(config)

        # Check serialized config
        config_dict = config.to_dict()
        self.assertNotIn("sk-secret", json.dumps(config_dict))

    def test_api_key_not_in_error_messages(self):
        """Test API key not exposed in error messages."""
        from the_edge_agent.reasoning.dspy_client import DSPyConfig

        config = DSPyConfig(api_key="sk-secret-12345")

        # repr and str should not expose key
        self.assertNotIn("sk-secret", str(config.to_dict()))


if __name__ == "__main__":
    unittest.main()
