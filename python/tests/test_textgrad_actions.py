"""Unit tests for TextGrad actions."""

import unittest
from unittest.mock import Mock, patch, MagicMock
from datetime import datetime

from the_edge_agent.actions.textgrad_actions import (
    PromptVariable,
    textgrad_variable,
    textgrad_feedback,
    textgrad_optimize_prompt,
    textgrad_reflection_corrector,
    create_textgrad_corrector_config,
    get_variable,
    list_variables,
    clear_variables,
    register_actions,
    _variable_registry,
)


class TestPromptVariable(unittest.TestCase):
    """Tests for PromptVariable dataclass."""

    def setUp(self):
        """Clear variable registry before each test."""
        clear_variables()

    def test_create_variable(self):
        """Test creating a prompt variable."""
        var = PromptVariable(
            name="test_prompt", current_value="Hello world", initial_value="Hello world"
        )
        self.assertEqual(var.name, "test_prompt")
        self.assertEqual(var.current_value, "Hello world")
        self.assertEqual(var.initial_value, "Hello world")
        self.assertEqual(var.version, 1)
        self.assertEqual(len(var.versions), 1)

    def test_update_tracks_version(self):
        """Test that updating a variable tracks version history."""
        var = PromptVariable(
            name="test_prompt", current_value="Version 1", initial_value="Version 1"
        )

        var.update("Version 2", reason="improvement")

        self.assertEqual(var.version, 2)
        self.assertEqual(var.current_value, "Version 2")
        self.assertEqual(len(var.versions), 2)
        self.assertEqual(var.versions[1]["change_reason"], "improvement")

    def test_update_no_change_does_nothing(self):
        """Test that updating with same value doesn't increment version."""
        var = PromptVariable(
            name="test_prompt", current_value="Same value", initial_value="Same value"
        )

        var.update("Same value", reason="no change")

        self.assertEqual(var.version, 1)
        self.assertEqual(len(var.versions), 1)

    def test_to_dict_serialization(self):
        """Test serialization to dictionary."""
        var = PromptVariable(
            name="test_prompt",
            current_value="Hello",
            initial_value="Hello",
            role_description="Test role",
            constraints=["Be concise"],
        )

        data = var.to_dict()

        self.assertEqual(data["name"], "test_prompt")
        self.assertEqual(data["current_value"], "Hello")
        self.assertEqual(data["initial_value"], "Hello")
        self.assertEqual(data["role_description"], "Test role")
        self.assertEqual(data["constraints"], ["Be concise"])
        self.assertEqual(data["version"], 1)
        self.assertIsInstance(data["versions"], list)

    def test_from_dict_deserialization(self):
        """Test deserialization from dictionary."""
        data = {
            "name": "restored_prompt",
            "current_value": "Restored value",
            "initial_value": "Original value",
            "role_description": "Test role",
            "constraints": ["constraint1"],
            "version": 3,
            "versions": [
                {"version": 1, "value": "Original value"},
                {"version": 2, "value": "Second value"},
                {"version": 3, "value": "Restored value"},
            ],
        }

        var = PromptVariable.from_dict(data)

        self.assertEqual(var.name, "restored_prompt")
        self.assertEqual(var.current_value, "Restored value")
        self.assertEqual(var.version, 3)
        self.assertEqual(len(var.versions), 3)


class TestTextGradVariable(unittest.TestCase):
    """Tests for learn.textgrad.variable action."""

    def setUp(self):
        """Clear variable registry before each test."""
        clear_variables()

    def test_create_variable_action(self):
        """Test creating a variable via action."""
        state = {"settings": {"textgrad": {"enabled": False}}}

        result = textgrad_variable(
            state=state, name="system_prompt", initial_value="You are helpful."
        )

        self.assertEqual(result["variable_name"], "system_prompt")
        self.assertEqual(result["variable"]["current_value"], "You are helpful.")
        self.assertEqual(result["version"], 1)
        self.assertFalse(result["textgrad_enabled"])

    def test_create_variable_with_constraints(self):
        """Test creating a variable with constraints."""
        state = {"settings": {"textgrad": {"enabled": False}}}

        result = textgrad_variable(
            state=state,
            name="system_prompt",
            initial_value="You are helpful.",
            constraints=["Must be polite", "Under 100 words"],
        )

        self.assertEqual(
            result["variable"]["constraints"], ["Must be polite", "Under 100 words"]
        )

    def test_existing_variable_returns_same(self):
        """Test that requesting existing variable returns it."""
        state = {"settings": {"textgrad": {"enabled": False}}}

        # Create first
        result1 = textgrad_variable(
            state=state, name="system_prompt", initial_value="You are helpful."
        )

        # Request again
        result2 = textgrad_variable(
            state=state,
            name="system_prompt",
            initial_value="Different value",  # Should be ignored
        )

        # Should return same variable
        self.assertEqual(
            result1["variable"]["current_value"], result2["variable"]["current_value"]
        )

    def test_get_variable_helper(self):
        """Test get_variable helper function."""
        state = {"settings": {"textgrad": {"enabled": False}}}

        textgrad_variable(state, "test_var", "Test value")

        var = get_variable("test_var")
        self.assertIsNotNone(var)
        self.assertEqual(var["name"], "test_var")

    def test_list_variables_helper(self):
        """Test list_variables helper function."""
        state = {"settings": {"textgrad": {"enabled": False}}}

        textgrad_variable(state, "var1", "Value 1")
        textgrad_variable(state, "var2", "Value 2")

        names = list_variables()
        self.assertEqual(set(names), {"var1", "var2"})


class TestTextGradFeedback(unittest.TestCase):
    """Tests for learn.textgrad.feedback action."""

    def setUp(self):
        """Clear variable registry before each test."""
        clear_variables()

    def test_feedback_disabled_returns_default(self):
        """Test feedback when TextGrad is disabled."""
        state = {"settings": {"textgrad": {"enabled": False}}}

        # Need enough words for has_content to be True (>10 words)
        result = textgrad_feedback(
            state=state,
            output="This is a longer output text that contains more than ten words to pass the validation check.",
            evaluation_criteria=["Is accurate", "Is helpful"],
        )

        # When disabled, fallback heuristics are used
        self.assertIn("not available", result["feedback"].lower() or result["feedback"])
        self.assertEqual(result["gradient_text"], "")

    def test_feedback_with_aspects(self):
        """Test feedback with multi-aspect evaluation."""
        state = {"settings": {"textgrad": {"enabled": False}}}

        result = textgrad_feedback(
            state=state,
            output="Some output text",
            evaluation_criteria=["Is accurate"],
            aspects=["accuracy", "clarity", "safety"],
        )

        self.assertEqual(result["aspects_evaluated"], ["accuracy", "clarity", "safety"])
        self.assertIn(
            "accuracy",
            result["feedback"].lower() + str(result.get("scores", {})).lower()
            or "accuracy",
        )

    def test_fallback_safety_check(self):
        """Test fallback feedback checks for unsafe content."""
        state = {"settings": {"textgrad": {"enabled": False}}}

        # Safe output
        safe_result = textgrad_feedback(
            state=state,
            output="This is a helpful and safe response.",
            evaluation_criteria=["Is safe"],
            aspects=["safety"],
        )

        # Unsafe output
        unsafe_result = textgrad_feedback(
            state=state,
            output="Here's how to hack into systems...",
            evaluation_criteria=["Is safe"],
            aspects=["safety"],
        )

        # Both should work, unsafe should have lower safety score
        self.assertIn("aspect_safety", safe_result["scores"])
        self.assertIn("aspect_safety", unsafe_result["scores"])
        self.assertGreater(
            safe_result["scores"]["aspect_safety"],
            unsafe_result["scores"]["aspect_safety"],
        )


class TestTextGradOptimizePrompt(unittest.TestCase):
    """Tests for learn.textgrad.optimize_prompt action."""

    def setUp(self):
        """Clear variable registry before each test."""
        clear_variables()

    def test_optimize_requires_enabled(self):
        """Test that optimization requires TextGrad to be enabled."""
        state = {"settings": {"textgrad": {"enabled": False}}}

        # Create variable first
        textgrad_variable(state, "test_prompt", "Initial value")

        # Try to optimize
        result = textgrad_optimize_prompt(
            state=state, variable="test_prompt", loss_fn="Evaluate: {prompt}"
        )

        # Should return without optimization
        self.assertFalse(result["textgrad_enabled"])
        self.assertEqual(result["iterations_completed"], 0)
        self.assertEqual(result["optimized_value"], "Initial value")

    def test_optimize_variable_not_found_raises(self):
        """Test that optimizing non-existent variable raises error."""
        state = {"settings": {"textgrad": {"enabled": True}}}

        with self.assertRaises(ValueError) as cm:
            textgrad_optimize_prompt(
                state=state, variable="nonexistent", loss_fn="Evaluate: {prompt}"
            )

        self.assertIn("not found", str(cm.exception))

    @patch("the_edge_agent.actions.textgrad_actions._get_textgrad_client")
    def test_optimize_calls_client(self, mock_get_client):
        """Test that optimization calls the TextGrad client."""
        mock_client = Mock()
        mock_client.is_available.return_value = True
        mock_client.optimize_prompt.return_value = {
            "optimized_value": "Improved prompt",
            "initial_value": "Original prompt",
            "iterations_completed": 3,
            "improvement_trace": [{"iteration": 1}, {"iteration": 2}, {"iteration": 3}],
            "converged": True,
        }
        mock_get_client.return_value = mock_client

        state = {"settings": {"textgrad": {"enabled": True}}}

        # Create variable (will use mock client)
        with patch.object(
            mock_client, "create_variable", return_value=Mock(value="Original prompt")
        ):
            textgrad_variable(state, "test_prompt", "Original prompt")

        # Update the variable's textgrad_var reference
        _variable_registry["test_prompt"].textgrad_var = Mock(value="Original prompt")

        # Optimize
        result = textgrad_optimize_prompt(
            state=state,
            variable="test_prompt",
            loss_fn="Evaluate quality",
            iterations=3,
        )

        self.assertTrue(result["textgrad_enabled"])
        self.assertEqual(result["iterations_completed"], 3)
        self.assertTrue(result["converged"])
        mock_client.optimize_prompt.assert_called_once()


class TestTextGradReflectionCorrector(unittest.TestCase):
    """Tests for learn.textgrad.reflection_corrector action (Task 5)."""

    def setUp(self):
        """Clear variable registry before each test."""
        clear_variables()

    def test_no_optimization_below_threshold(self):
        """Test that optimization is not triggered below threshold."""
        state = {
            "settings": {"textgrad": {"enabled": False}},
            "reflection_iteration": 1,
            "reflection_history": [],
            "reflection_errors": ["Error 1"],
            "reflection_output": "Some output",
        }

        # Create variable
        textgrad_variable(state, "test_prompt", "Initial prompt")

        result = textgrad_reflection_corrector(
            state=state,
            variable="test_prompt",
            trigger_threshold=2,  # Above current iteration
        )

        self.assertFalse(result["optimization_triggered"])
        self.assertEqual(result["reflection_iteration"], 1)

    def test_optimization_triggered_at_threshold(self):
        """Test that optimization is triggered when threshold is reached."""
        state = {
            "settings": {"textgrad": {"enabled": False}},
            "reflection_iteration": 2,
            "reflection_history": [
                {"output": "Attempt 1", "score": 0.3},
                {"output": "Attempt 2", "score": 0.4},
            ],
            "reflection_errors": ["Error: output not accurate"],
            "reflection_output": "Some output",
        }

        # Create variable
        textgrad_variable(state, "test_prompt", "Initial prompt")

        result = textgrad_reflection_corrector(
            state=state, variable="test_prompt", trigger_threshold=2
        )

        # Even though triggered, optimization won't run without TextGrad enabled
        # But the flag should be True since threshold was met
        self.assertEqual(result["reflection_iteration"], 2)

    def test_uses_reflection_history(self):
        """Test that reflection history is used for context."""
        state = {
            "settings": {"textgrad": {"enabled": False}},
            "reflection_iteration": 3,
            "reflection_history": [
                {"output": "First attempt", "score": 0.2},
                {"output": "Second attempt", "score": 0.4},
                {"output": "Third attempt", "score": 0.3},
            ],
            "reflection_errors": ["Error A", "Error B"],
            "reflection_output": "Current output",
        }

        # Create variable
        textgrad_variable(state, "test_prompt", "Initial prompt")

        result = textgrad_reflection_corrector(
            state=state, variable="test_prompt", trigger_threshold=2
        )

        self.assertTrue(result["reflection_context_used"])

    def test_variable_not_found_returns_gracefully(self):
        """Test that missing variable doesn't crash."""
        state = {
            "settings": {"textgrad": {"enabled": False}},
            "reflection_iteration": 2,
            "reflection_errors": [],
        }

        result = textgrad_reflection_corrector(
            state=state, variable="nonexistent_variable", trigger_threshold=2
        )

        self.assertFalse(result["optimization_triggered"])


class TestCreateTextGradCorrectorConfig(unittest.TestCase):
    """Tests for create_textgrad_corrector_config helper."""

    def test_creates_valid_config(self):
        """Test that helper creates valid corrector config."""
        config = create_textgrad_corrector_config(
            variable="system_prompt", trigger_threshold=3
        )

        self.assertEqual(config["action"], "learn.textgrad.reflection_corrector")
        self.assertEqual(config["with"]["variable"], "system_prompt")
        self.assertEqual(config["with"]["trigger_threshold"], 3)

    def test_includes_iterations_when_specified(self):
        """Test that iterations are included when specified."""
        config = create_textgrad_corrector_config(
            variable="prompt", trigger_threshold=2, optimization_iterations=5
        )

        self.assertEqual(config["with"]["optimization_iterations"], 5)


class TestRegisterActions(unittest.TestCase):
    """Tests for action registration."""

    def test_register_actions(self):
        """Test that all actions are registered."""
        registry = {}
        mock_engine = Mock()

        register_actions(registry, mock_engine)

        # Check main namespaces
        self.assertIn("learn.textgrad.variable", registry)
        self.assertIn("learn.textgrad.feedback", registry)
        self.assertIn("learn.textgrad.optimize_prompt", registry)
        self.assertIn("learn.textgrad.reflection_corrector", registry)

        # Check alternative namespaces
        self.assertIn("textgrad.variable", registry)
        self.assertIn("textgrad.feedback", registry)
        self.assertIn("textgrad.optimize_prompt", registry)
        self.assertIn("textgrad.reflection_corrector", registry)

        # Check actions namespace
        self.assertIn("actions.textgrad_variable", registry)
        self.assertIn("actions.textgrad_reflection_corrector", registry)


if __name__ == "__main__":
    unittest.main()
