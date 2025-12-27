"""
Tests for retry.loop action (TEA-YAML-005).

Tests cover:
- Immediate success (validation passes first try)
- Success after retry (fail once, correct, succeed)
- Max retries exceeded (fail all attempts)
- Correction node failure
- Invalid action name error
- State preservation
- Integration with validate.extraction
"""

import unittest
from unittest.mock import MagicMock, patch
from typing import Any, Dict

from the_edge_agent import YAMLEngine


class MockGraph:
    """Mock StateGraph for testing retry actions."""

    def __init__(self):
        self.nodes = {}

    def add_node(self, name: str, run=None):
        """Add a mock node."""
        self.nodes[name] = {"run": run}


class TestRetryLoopAction(unittest.TestCase):
    """Test suite for retry.loop action."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = YAMLEngine()
        # Get the actions registry
        from the_edge_agent.actions import build_actions_registry

        self.registry = build_actions_registry(self.engine)

        # Create mock graph
        self.mock_graph = MockGraph()
        self.engine._current_graph = MagicMock()
        self.engine._current_graph.graph = self.mock_graph

    def test_immediate_success(self):
        """Test: Validation passes first try - no retry needed."""

        # Mock validation action that always succeeds
        def mock_validate(state, **kwargs):
            return {"valid": True, "errors": []}

        self.registry["test.validate"] = mock_validate

        # Add a correction node (should not be called)
        correction_called = {"count": 0}

        def mock_correct(state, **kwargs):
            correction_called["count"] += 1
            return {}

        self.mock_graph.add_node("correct_node", run=mock_correct)

        # Get retry.loop action
        retry_action = self.registry["retry.loop"]

        # Execute
        state = {"input": "test"}
        result = retry_action(
            state=state,
            validate="test.validate",
            validate_args={},
            correct="correct_node",
            max_retries=3,
        )

        # Verify
        self.assertTrue(result["valid"])
        self.assertEqual(result["_retry_count"], 0)
        self.assertEqual(result["_retry_errors"], [])
        self.assertFalse(result["_retry_exhausted"])
        self.assertEqual(correction_called["count"], 0)

    def test_success_after_one_retry(self):
        """Test: Fail once, correct, then succeed."""
        # Mock validation that fails first, succeeds second
        validation_calls = {"count": 0}

        def mock_validate(state, **kwargs):
            validation_calls["count"] += 1
            if validation_calls["count"] == 1:
                return {"valid": False, "errors": [{"message": "First failure"}]}
            return {"valid": True, "errors": []}

        self.registry["test.validate"] = mock_validate

        # Correction node that "fixes" the issue
        def mock_correct(state, **kwargs):
            return {"fixed": True}

        self.mock_graph.add_node("correct_node", run=mock_correct)

        # Get retry.loop action
        retry_action = self.registry["retry.loop"]

        # Execute
        state = {"input": "test"}
        result = retry_action(
            state=state,
            validate="test.validate",
            validate_args={},
            correct="correct_node",
            max_retries=3,
        )

        # Verify
        self.assertTrue(result["valid"])
        self.assertEqual(result["_retry_count"], 1)
        self.assertEqual(result["_retry_errors"], [])
        self.assertFalse(result["_retry_exhausted"])
        self.assertEqual(validation_calls["count"], 2)

    def test_max_retries_exceeded(self):
        """Test: Fail all attempts - exhausted = true."""
        # Mock validation that always fails
        validation_calls = {"count": 0}

        def mock_validate(state, **kwargs):
            validation_calls["count"] += 1
            return {
                "valid": False,
                "errors": [{"message": f"Failure {validation_calls['count']}"}],
            }

        self.registry["test.validate"] = mock_validate

        # Correction node that doesn't fix the issue
        correction_calls = {"count": 0}

        def mock_correct(state, **kwargs):
            correction_calls["count"] += 1
            return {"attempt": correction_calls["count"]}

        self.mock_graph.add_node("correct_node", run=mock_correct)

        # Get retry.loop action
        retry_action = self.registry["retry.loop"]

        # Execute with max_retries=2
        state = {"input": "test"}
        result = retry_action(
            state=state,
            validate="test.validate",
            validate_args={},
            correct="correct_node",
            max_retries=2,
        )

        # Verify
        self.assertFalse(result["valid"])
        self.assertEqual(result["_retry_count"], 2)
        self.assertTrue(result["_retry_exhausted"])
        self.assertEqual(len(result["_retry_errors"]), 1)
        # Validation called: initial + 2 retries = 3 times
        self.assertEqual(validation_calls["count"], 3)
        # Correction called 2 times (for max_retries=2)
        self.assertEqual(correction_calls["count"], 2)

    def test_correction_node_failure(self):
        """Test: Correction node raises exception - loop exits with error."""

        # Mock validation that fails
        def mock_validate(state, **kwargs):
            return {"valid": False, "errors": [{"message": "Validation failed"}]}

        self.registry["test.validate"] = mock_validate

        # Correction node that raises an exception
        def mock_correct(state, **kwargs):
            raise RuntimeError("Correction failed!")

        self.mock_graph.add_node("correct_node", run=mock_correct)

        # Get retry.loop action
        retry_action = self.registry["retry.loop"]

        # Execute
        state = {"input": "test"}
        result = retry_action(
            state=state,
            validate="test.validate",
            validate_args={},
            correct="correct_node",
            max_retries=3,
        )

        # Verify - should exit with error, not exhausted
        self.assertFalse(result["valid"])
        self.assertEqual(result["_retry_count"], 0)
        self.assertFalse(result["_retry_exhausted"])
        # Should have both validation error and correction error
        self.assertTrue(len(result["_retry_errors"]) >= 1)
        self.assertTrue(
            any("Correction node" in str(e) for e in result["_retry_errors"])
        )

    def test_invalid_action_name(self):
        """Test: Unknown validation action - fails with clear error."""
        # Add correction node
        self.mock_graph.add_node("correct_node", run=lambda s, **k: {})

        # Get retry.loop action
        retry_action = self.registry["retry.loop"]

        # Execute with unknown action
        state = {"input": "test"}
        with self.assertRaises(ValueError) as ctx:
            retry_action(
                state=state,
                validate="unknown.action",
                validate_args={},
                correct="correct_node",
                max_retries=1,
            )

        self.assertIn("Unknown validation action", str(ctx.exception))

    def test_invalid_correction_node(self):
        """Test: Unknown correction node - fails with clear error."""

        # Add validation action
        def mock_validate(state, **kwargs):
            return {"valid": False, "errors": []}

        self.registry["test.validate"] = mock_validate

        # Get retry.loop action
        retry_action = self.registry["retry.loop"]

        # Execute with unknown correction node
        state = {"input": "test"}
        with self.assertRaises(ValueError) as ctx:
            retry_action(
                state=state,
                validate="test.validate",
                validate_args={},
                correct="nonexistent_node",
                max_retries=1,
            )

        self.assertIn("Correction node not found", str(ctx.exception))

    def test_invalid_max_retries_negative(self):
        """Test: Negative max_retries - fails at runtime."""
        # Add nodes
        self.registry["test.validate"] = lambda s, **k: {"valid": True, "errors": []}
        self.mock_graph.add_node("correct_node", run=lambda s, **k: {})

        # Get retry.loop action
        retry_action = self.registry["retry.loop"]

        # Execute with negative max_retries
        state = {"input": "test"}
        with self.assertRaises(ValueError) as ctx:
            retry_action(
                state=state,
                validate="test.validate",
                validate_args={},
                correct="correct_node",
                max_retries=-1,
            )

        self.assertIn("max_retries must be a non-negative integer", str(ctx.exception))

    def test_invalid_max_retries_non_integer(self):
        """Test: Non-integer max_retries - fails at runtime."""
        # Add nodes
        self.registry["test.validate"] = lambda s, **k: {"valid": True, "errors": []}
        self.mock_graph.add_node("correct_node", run=lambda s, **k: {})

        # Get retry.loop action
        retry_action = self.registry["retry.loop"]

        # Execute with string max_retries
        state = {"input": "test"}
        with self.assertRaises(ValueError) as ctx:
            retry_action(
                state=state,
                validate="test.validate",
                validate_args={},
                correct="correct_node",
                max_retries="not_an_int",
            )

        self.assertIn("max_retries must be a non-negative integer", str(ctx.exception))

    def test_state_preservation(self):
        """Test: User state not clobbered by _retry_* vars."""

        # Mock validation
        def mock_validate(state, **kwargs):
            # Check user state is preserved
            if state.get("user_data") != "preserved":
                return {"valid": False, "errors": [{"message": "State lost!"}]}
            return {"valid": True, "errors": []}

        self.registry["test.validate"] = mock_validate

        # Add correction node
        self.mock_graph.add_node("correct_node", run=lambda s, **k: {})

        # Get retry.loop action
        retry_action = self.registry["retry.loop"]

        # Execute with user data
        state = {"user_data": "preserved", "other_key": 123}
        result = retry_action(
            state=state,
            validate="test.validate",
            validate_args={},
            correct="correct_node",
            max_retries=1,
        )

        # Verify success (state was preserved)
        self.assertTrue(result["valid"])

    def test_retry_errors_available_to_correction(self):
        """Test: Correction node has access to _retry_errors."""
        validation_calls = {"count": 0}

        def mock_validate(state, **kwargs):
            validation_calls["count"] += 1
            if validation_calls["count"] == 1:
                return {
                    "valid": False,
                    "errors": [{"message": "Error A"}, {"message": "Error B"}],
                }
            return {"valid": True, "errors": []}

        self.registry["test.validate"] = mock_validate

        # Correction node checks for errors
        received_errors = {"value": None}

        def mock_correct(state, **kwargs):
            received_errors["value"] = state.get("_retry_errors")
            return {}

        self.mock_graph.add_node("correct_node", run=mock_correct)

        # Get retry.loop action
        retry_action = self.registry["retry.loop"]

        # Execute
        state = {}
        result = retry_action(
            state=state,
            validate="test.validate",
            validate_args={},
            correct="correct_node",
            max_retries=1,
        )

        # Verify correction node received errors
        self.assertIsNotNone(received_errors["value"])
        self.assertEqual(len(received_errors["value"]), 2)
        self.assertEqual(received_errors["value"][0]["message"], "Error A")

    def test_zero_max_retries(self):
        """Test: max_retries=0 means no retries, just validation."""
        validation_calls = {"count": 0}

        def mock_validate(state, **kwargs):
            validation_calls["count"] += 1
            return {"valid": False, "errors": [{"message": "Always fails"}]}

        self.registry["test.validate"] = mock_validate

        correction_calls = {"count": 0}

        def mock_correct(state, **kwargs):
            correction_calls["count"] += 1
            return {}

        self.mock_graph.add_node("correct_node", run=mock_correct)

        # Get retry.loop action
        retry_action = self.registry["retry.loop"]

        # Execute with max_retries=0
        state = {}
        result = retry_action(
            state=state,
            validate="test.validate",
            validate_args={},
            correct="correct_node",
            max_retries=0,
        )

        # Verify - validation called once, correction never called
        self.assertFalse(result["valid"])
        self.assertTrue(result["_retry_exhausted"])
        self.assertEqual(validation_calls["count"], 1)
        self.assertEqual(correction_calls["count"], 0)

    def test_validate_args_passed_correctly(self):
        """Test: validate_args are passed to validation action."""
        received_args = {}

        def mock_validate(state, **kwargs):
            received_args.update(kwargs)
            return {"valid": True, "errors": []}

        self.registry["test.validate"] = mock_validate
        self.mock_graph.add_node("correct_node", run=lambda s, **k: {})

        # Get retry.loop action
        retry_action = self.registry["retry.loop"]

        # Execute with custom args
        state = {}
        result = retry_action(
            state=state,
            validate="test.validate",
            validate_args={"custom_arg": "custom_value", "number": 42},
            correct="correct_node",
            max_retries=1,
        )

        # Verify args were passed
        self.assertEqual(received_args.get("custom_arg"), "custom_value")
        self.assertEqual(received_args.get("number"), 42)

    def test_correction_result_updates_state(self):
        """Test: Correction node result updates state for next validation."""
        validation_calls = {"count": 0}

        def mock_validate(state, **kwargs):
            validation_calls["count"] += 1
            # Succeed only if correction added the fix
            if state.get("fix_applied"):
                return {"valid": True, "errors": []}
            return {"valid": False, "errors": [{"message": "Need fix"}]}

        self.registry["test.validate"] = mock_validate

        def mock_correct(state, **kwargs):
            # Apply the fix
            return {"fix_applied": True}

        self.mock_graph.add_node("correct_node", run=mock_correct)

        # Get retry.loop action
        retry_action = self.registry["retry.loop"]

        # Execute
        state = {}
        result = retry_action(
            state=state,
            validate="test.validate",
            validate_args={},
            correct="correct_node",
            max_retries=1,
        )

        # Verify - should succeed after correction
        self.assertTrue(result["valid"])
        self.assertEqual(result["_retry_count"], 1)


class TestRetryLoopYAMLIntegration(unittest.TestCase):
    """Integration tests for retry.loop with YAML engine."""

    def test_yaml_basic_retry(self):
        """Test retry.loop works in a full YAML workflow."""
        yaml_config = """
name: retry-test-agent
state_schema:
  value: int
  corrected: bool

nodes:
  - name: start
    run: |
      return {"value": 0}

  - name: validate_with_retry
    uses: retry.loop
    with:
      validate: test.custom_validate
      validate_args: {}
      correct: apply_correction
      max_retries: 2

  - name: apply_correction
    run: |
      # This node is called by retry.loop on validation failure
      return {"value": state.get("value", 0) + 1, "corrected": True}

  - name: finish
    run: |
      return {"result": "done", "final_value": state.get("value")}

edges:
  - from: __start__
    to: start
  - from: start
    to: validate_with_retry
  - from: validate_with_retry
    to: finish
  - from: finish
    to: __end__
"""

        # Register custom validation action that needs value >= 2
        def custom_validate(state, **kwargs):
            value = state.get("value", 0)
            if value >= 2:
                return {"valid": True, "errors": []}
            return {
                "valid": False,
                "errors": [{"message": f"Value {value} must be >= 2"}],
            }

        # Create engine and register custom action after init (registry is built at init)
        engine = YAMLEngine()
        engine.actions_registry["test.custom_validate"] = custom_validate

        # Load and run
        import yaml as yaml_lib

        config = yaml_lib.safe_load(yaml_config)
        graph = engine.load_from_dict(config)

        # Execute
        results = list(graph.invoke({}))
        final_state = results[-1]["state"]

        # Verify - should succeed after 2 corrections (0 -> 1 -> 2)
        self.assertEqual(final_state.get("value"), 2)
        self.assertTrue(final_state.get("corrected"))
        self.assertEqual(final_state.get("result"), "done")


class TestRetryLoopWithValidateExtraction(unittest.TestCase):
    """Integration tests for retry.loop with validate.extraction (AC 17)."""

    def test_retry_loop_with_validate_extraction_action(self):
        """Test: retry.loop works with the actual validate.extraction action."""
        # This test verifies AC 17: Works with validate.extraction action from TEA-YAML-004
        yaml_config = """
name: retry-with-validate-extraction
state_schema:
  entities: list
  relationships: list
  valid: bool

# Correct extraction_schema format (nested under entities/relationships)
extraction_schema:
  entities:
    required_fields:
      - name
      - type
  relationships:
    types:
      - knows
    required_fields:
      - type
      - subject
      - object

nodes:
  - name: start
    run: |
      # Start with invalid entities (missing 'type' field)
      return {
          "entities": [{"name": "John"}],
          "relationships": []
      }

  - name: validate_with_retry
    uses: retry.loop
    with:
      validate: validate.extraction
      validate_args: {}
      # Note: Don't pass entities/relationships in validate_args when correction
      # nodes update them - validate.extraction pulls from state automatically
      correct: fix_entities
      max_retries: 1

  - name: fix_entities
    run: |
      # Fix the entities by adding missing 'type' field
      entities = state.get("entities", [])
      fixed = [{"name": e.get("name", ""), "type": "Person"} for e in entities]
      return {"entities": fixed}

  - name: finish
    run: |
      return {"done": True}

edges:
  - from: __start__
    to: start
  - from: start
    to: validate_with_retry
  - from: validate_with_retry
    to: finish
  - from: finish
    to: __end__
"""
        from the_edge_agent import YAMLEngine
        import yaml as yaml_lib

        engine = YAMLEngine()
        config = yaml_lib.safe_load(yaml_config)
        graph = engine.load_from_dict(config)

        # Execute
        results = list(graph.invoke({}))
        final_state = results[-1]["state"]

        # Verify - should succeed after correction fixes the missing 'type' field
        self.assertTrue(final_state.get("valid", False))
        self.assertEqual(final_state.get("_retry_count"), 1)
        self.assertFalse(final_state.get("_retry_exhausted", True))
        self.assertTrue(final_state.get("done"))
        # Entities should now have 'type' field
        entities = final_state.get("entities", [])
        self.assertEqual(len(entities), 1)
        self.assertEqual(entities[0].get("type"), "Person")


def build_actions_registry(engine):
    """Import helper for tests."""
    from the_edge_agent.actions import build_actions_registry

    return build_actions_registry(engine)


if __name__ == "__main__":
    unittest.main()
