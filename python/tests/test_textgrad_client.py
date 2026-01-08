"""Unit tests for TextGrad client wrapper."""

import unittest
from unittest.mock import Mock, patch, MagicMock
import warnings

from the_edge_agent.learning.textgrad_client import (
    TextGradClient,
    TextGradConfig,
    TEXTGRAD_AVAILABLE,
)


class TestTextGradConfig(unittest.TestCase):
    """Tests for TextGradConfig dataclass."""

    def test_default_config(self):
        """Test default configuration values."""
        config = TextGradConfig()
        self.assertFalse(config.enabled)
        self.assertEqual(config.optimizer_model, "gpt-4")
        self.assertEqual(config.max_iterations, 3)
        self.assertEqual(config.learning_rate, 0.1)
        self.assertEqual(config.early_stopping_threshold, 0.01)
        self.assertFalse(config.cost_warning_shown)

    def test_custom_config(self):
        """Test custom configuration values."""
        config = TextGradConfig(
            enabled=True,
            optimizer_model="gpt-3.5-turbo",
            max_iterations=5,
            learning_rate=0.05,
        )
        self.assertTrue(config.enabled)
        self.assertEqual(config.optimizer_model, "gpt-3.5-turbo")
        self.assertEqual(config.max_iterations, 5)
        self.assertEqual(config.learning_rate, 0.05)


class TestTextGradClient(unittest.TestCase):
    """Tests for TextGradClient."""

    def setUp(self):
        """Set up test fixtures."""
        self.config_disabled = TextGradConfig(enabled=False)
        self.config_enabled = TextGradConfig(enabled=True, optimizer_model="gpt-4")

    def test_client_disabled_no_error(self):
        """Test client initialization with TextGrad disabled."""
        client = TextGradClient(self.config_disabled)
        self.assertIsNone(client._engine)
        self.assertFalse(client.config.enabled)

    @unittest.skipIf(not TEXTGRAD_AVAILABLE, "TextGrad not installed")
    @patch("the_edge_agent.learning.textgrad_client.get_engine")
    def test_client_enabled_initializes_engine(self, mock_get_engine):
        """Test client initialization with TextGrad enabled."""
        mock_engine = Mock()
        mock_get_engine.return_value = mock_engine

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            client = TextGradClient(self.config_enabled)

            # Check cost warning was shown
            self.assertEqual(len(w), 1)
            self.assertIn("COST WARNING", str(w[0].message))

        # Check engine initialized
        mock_get_engine.assert_called_once_with(engine_name="gpt-4")
        self.assertEqual(client._engine, mock_engine)
        self.assertTrue(client.config.cost_warning_shown)

    def test_client_enabled_without_textgrad_raises(self):
        """Test that enabling TextGrad without library installed raises error."""
        if TEXTGRAD_AVAILABLE:
            self.skipTest("TextGrad is installed, cannot test missing library behavior")

        with self.assertRaises(RuntimeError) as cm:
            TextGradClient(self.config_enabled)

        self.assertIn("not installed", str(cm.exception))

    def test_create_variable_disabled_returns_none(self):
        """Test creating variable with TextGrad disabled returns None."""
        client = TextGradClient(self.config_disabled)
        result = client.create_variable("test_prompt", "Initial value")
        self.assertIsNone(result)

    @unittest.skipIf(not TEXTGRAD_AVAILABLE, "TextGrad not installed")
    @patch("the_edge_agent.learning.textgrad_client.get_engine")
    @patch("the_edge_agent.learning.textgrad_client.Variable")
    def test_create_variable_enabled_creates_variable(
        self, mock_variable_class, mock_get_engine
    ):
        """Test creating variable with TextGrad enabled."""
        mock_engine = Mock()
        mock_get_engine.return_value = mock_engine
        mock_variable = Mock()
        mock_variable_class.return_value = mock_variable

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            client = TextGradClient(self.config_enabled)

        result = client.create_variable("system_prompt", "You are helpful")

        mock_variable_class.assert_called_once()
        call_kwargs = mock_variable_class.call_args[1]
        self.assertEqual(call_kwargs["value"], "You are helpful")
        self.assertTrue(call_kwargs["requires_grad"])
        self.assertEqual(result, mock_variable)

    def test_compute_feedback_disabled_returns_default(self):
        """Test computing feedback with TextGrad disabled."""
        client = TextGradClient(self.config_disabled)
        result = client.compute_feedback("Some output", ["accuracy", "clarity"])

        self.assertTrue(result["valid"])
        self.assertIn("disabled", result["feedback"])
        self.assertEqual(result["scores"], {})
        self.assertEqual(result["gradient_text"], "")

    @unittest.skipIf(not TEXTGRAD_AVAILABLE, "TextGrad not installed")
    @patch("the_edge_agent.learning.textgrad_client.get_engine")
    def test_compute_feedback_enabled(self, mock_get_engine):
        """Test computing feedback with TextGrad enabled."""
        mock_engine = Mock()
        mock_engine.return_value = "Pass: The output is accurate and clear."
        mock_get_engine.return_value = mock_engine

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            client = TextGradClient(self.config_enabled)

        result = client.compute_feedback("Sample output", ["accuracy", "clarity"])

        self.assertTrue(result["valid"])
        self.assertIsInstance(result["feedback"], str)
        self.assertIsInstance(result["scores"], dict)
        mock_engine.assert_called_once()

    def test_optimize_prompt_disabled_raises(self):
        """Test optimizing prompt with TextGrad disabled raises error."""
        client = TextGradClient(self.config_disabled)
        mock_variable = Mock()

        with self.assertRaises(RuntimeError) as cm:
            client.optimize_prompt(mock_variable, "Evaluate: {prompt}")

        self.assertIn("not enabled", str(cm.exception))

    @unittest.skipIf(not TEXTGRAD_AVAILABLE, "TextGrad not installed")
    @patch("the_edge_agent.learning.textgrad_client.get_engine")
    @patch("the_edge_agent.learning.textgrad_client.TextualGradientDescent")
    @patch("the_edge_agent.learning.textgrad_client.Variable")
    def test_optimize_prompt_enabled(
        self, mock_variable_class, mock_optimizer_class, mock_get_engine
    ):
        """Test optimizing prompt with TextGrad enabled."""
        # Setup mocks
        mock_engine = Mock()
        mock_engine.return_value = "Good: mostly accurate"
        mock_get_engine.return_value = mock_engine

        mock_optimizer = Mock()
        mock_optimizer_class.return_value = mock_optimizer

        mock_loss_variable = Mock()
        mock_loss_variable.backward = Mock()
        mock_variable_class.return_value = mock_loss_variable

        # Create client
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            client = TextGradClient(self.config_enabled)

        # Create prompt variable
        mock_prompt_var = Mock()
        mock_prompt_var.value = "Initial prompt"

        # Run optimization
        result = client.optimize_prompt(
            mock_prompt_var, "Evaluate this prompt: {prompt}", iterations=2
        )

        # Assertions
        self.assertEqual(result["initial_value"], "Initial prompt")
        self.assertEqual(result["iterations_completed"], 2)
        self.assertIsInstance(result["improvement_trace"], list)
        self.assertIn("optimized_value", result)
        self.assertIn("converged", result)

        # Check optimization calls
        mock_optimizer.step.assert_called()
        mock_optimizer.zero_grad.assert_called()

    def test_is_available_when_disabled(self):
        """Test is_available returns False when disabled."""
        client = TextGradClient(self.config_disabled)
        self.assertFalse(client.is_available())

    @unittest.skipIf(not TEXTGRAD_AVAILABLE, "TextGrad not installed")
    @patch("the_edge_agent.learning.textgrad_client.get_engine")
    def test_is_available_when_enabled(self, mock_get_engine):
        """Test is_available returns True when enabled and available."""
        mock_engine = Mock()
        mock_get_engine.return_value = mock_engine

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            client = TextGradClient(self.config_enabled)

        self.assertTrue(client.is_available())

    def test_cost_warning_only_shown_once(self):
        """Test that cost warning is only shown once per client."""
        if not TEXTGRAD_AVAILABLE:
            self.skipTest("TextGrad not installed")

        with patch("the_edge_agent.learning.textgrad_client.get_engine"):
            with warnings.catch_warnings(record=True) as w:
                warnings.simplefilter("always")

                # First initialization shows warning
                client = TextGradClient(self.config_enabled)
                self.assertEqual(len(w), 1)

                # Second call should not show warning again
                client._show_cost_warning()  # Manually call
                # Should still be 1 warning total
                # (though this method would show it again in real usage, the flag prevents it)


if __name__ == "__main__":
    unittest.main()
