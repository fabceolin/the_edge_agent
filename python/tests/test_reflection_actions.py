"""
Tests for reflection actions (TEA-AGENT-001.2).

Tests cover:
- AC1: Core reflection loop (generate->evaluate->correct cycle)
- AC2: Schema-based evaluator with JSON Schema validation
- AC3: LLM-based evaluator (mocked)
- AC4: Custom evaluator with inline code
- AC5: Iteration tracking (reflection_iteration, reflection_history, etc.)
- AC6: On-failure strategies (return_best, return_last, raise)
- AC7: reflection.evaluate standalone action
- AC8: reflection.correct standalone action
- AC9: Python implementation and registry integration
"""

import unittest
from unittest.mock import MagicMock, patch
from typing import Any, Dict, List

from the_edge_agent import YAMLEngine
from the_edge_agent.actions import build_actions_registry, ReflectionFailedError


class TestReflectionLoopCore(unittest.TestCase):
    """Test suite for reflection.loop core functionality (AC1, AC5)."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = YAMLEngine()
        self.registry = build_actions_registry(self.engine)

    def test_first_pass_success(self):
        """Test: Generator output passes evaluation immediately (AC1)."""

        # Mock generator that produces valid output
        # Note: Generator returns the data directly, evaluator receives raw output
        def mock_llm_call(state, **kwargs):
            return {"name": "Alice", "email": "alice@test.com"}

        self.registry["llm.call"] = mock_llm_call

        # Get reflection.loop action
        loop_action = self.registry["reflection.loop"]

        # Execute with schema evaluator
        state = {}
        result = loop_action(
            state=state,
            generator={"action": "llm.call", "prompt": "Generate user"},
            evaluator={
                "type": "schema",
                "schema": {
                    "type": "object",
                    "required": ["name", "email"],
                    "properties": {
                        "name": {"type": "string"},
                        "email": {"type": "string"},
                    },
                },
            },
            max_iterations=3,
            on_failure="return_best",
        )

        # Verify
        self.assertTrue(result.get("valid", result.get("success")))
        self.assertEqual(result["reflection_iteration"], 1)
        self.assertEqual(result["reflection_errors"], [])
        self.assertEqual(len(result["reflection_history"]), 1)

    def test_correction_success(self):
        """Test: Corrector fixes issues within max_iterations (AC1)."""
        call_count = {"generator": 0}

        # Generator produces invalid output first, then valid after correction
        def mock_generator(state, **kwargs):
            call_count["generator"] += 1
            if call_count["generator"] == 1:
                # First call: missing email
                return {"name": "Bob"}
            else:
                # After correction: valid output
                return {"name": "Bob", "email": "bob@test.com"}

        self.registry["llm.call"] = mock_generator

        # Corrector provides feedback
        def mock_corrector(state, **kwargs):
            return {"correction_applied": True}

        self.registry["correction.action"] = mock_corrector

        # Get reflection.loop action
        loop_action = self.registry["reflection.loop"]

        # Execute
        state = {}
        result = loop_action(
            state=state,
            generator={"action": "llm.call", "prompt": "Generate user"},
            evaluator={
                "type": "schema",
                "schema": {
                    "type": "object",
                    "required": ["name", "email"],
                },
            },
            corrector={"action": "correction.action"},
            max_iterations=3,
            on_failure="return_best",
        )

        # Verify
        self.assertTrue(result.get("valid", result.get("success")))
        self.assertEqual(result["reflection_iteration"], 2)
        self.assertEqual(len(result["reflection_history"]), 2)
        self.assertEqual(call_count["generator"], 2)

    def test_circuit_breaker_triggers(self):
        """Test: Circuit breaker triggers after max_iterations (AC5)."""

        # Generator always produces invalid output
        def mock_generator(state, **kwargs):
            return {"invalid": True}

        self.registry["llm.call"] = mock_generator

        def mock_corrector(state, **kwargs):
            return {}

        self.registry["correction.action"] = mock_corrector

        # Get reflection.loop action
        loop_action = self.registry["reflection.loop"]

        # Execute with max_iterations=2
        state = {}
        result = loop_action(
            state=state,
            generator={"action": "llm.call", "prompt": "Generate"},
            evaluator={
                "type": "schema",
                "schema": {"type": "object", "required": ["name"]},
            },
            corrector={"action": "correction.action"},
            max_iterations=2,
            on_failure="return_last",
        )

        # Verify
        self.assertFalse(result.get("valid", result.get("success")))
        self.assertEqual(result["reflection_iteration"], 2)
        self.assertTrue(result.get("exhausted"))
        self.assertEqual(len(result["reflection_history"]), 2)

    def test_iteration_tracking_in_state(self):
        """Test: State includes iteration tracking variables (AC5)."""
        iterations_seen = []

        def mock_generator(state, **kwargs):
            iterations_seen.append(state.get("reflection_iteration", 0))
            return {"name": "Test"}

        self.registry["llm.call"] = mock_generator

        loop_action = self.registry["reflection.loop"]

        state = {}
        result = loop_action(
            state=state,
            generator={"action": "llm.call", "prompt": "Generate"},
            evaluator={
                "type": "schema",
                "schema": {"type": "object", "required": ["name"]},
            },
            max_iterations=3,
        )

        # Verify iteration tracking was available
        self.assertIn("reflection_iteration", result)
        self.assertIn("reflection_history", result)
        self.assertIn("reflection_best", result)
        self.assertIn("reflection_best_score", result)

    def test_reflection_history_accumulation(self):
        """Test: reflection_history accumulates all attempts (AC5)."""
        call_count = {"n": 0}

        def mock_generator(state, **kwargs):
            call_count["n"] += 1
            if call_count["n"] < 3:
                return {"partial": call_count["n"]}
            return {"name": "Valid", "complete": True}

        self.registry["llm.call"] = mock_generator

        def mock_corrector(state, **kwargs):
            return {}

        self.registry["correction.action"] = mock_corrector

        loop_action = self.registry["reflection.loop"]

        state = {}
        result = loop_action(
            state=state,
            generator={"action": "llm.call", "prompt": "Generate"},
            evaluator={
                "type": "schema",
                "schema": {"type": "object", "required": ["name"]},
            },
            corrector={"action": "correction.action"},
            max_iterations=5,
        )

        # Verify history contains all attempts
        self.assertEqual(len(result["reflection_history"]), 3)
        for i, entry in enumerate(result["reflection_history"]):
            self.assertIn("iteration", entry)
            self.assertIn("output", entry)
            self.assertIn("score", entry)
            self.assertIn("valid", entry)


class TestSchemaEvaluator(unittest.TestCase):
    """Test suite for schema-based evaluator (AC2)."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = YAMLEngine()
        self.registry = build_actions_registry(self.engine)

    def test_schema_validation_valid(self):
        """Test: Schema validation passes for valid data."""
        evaluate_action = self.registry["reflection.evaluate"]

        result = evaluate_action(
            state={},
            data={"name": "Alice", "age": 30},
            evaluator_type="schema",
            schema={
                "type": "object",
                "required": ["name", "age"],
                "properties": {
                    "name": {"type": "string"},
                    "age": {"type": "integer"},
                },
            },
        )

        self.assertTrue(result["valid"])
        self.assertEqual(result["score"], 1.0)
        self.assertEqual(result["errors"], [])

    def test_schema_validation_invalid_missing_field(self):
        """Test: Schema validation fails for missing required field."""
        evaluate_action = self.registry["reflection.evaluate"]

        result = evaluate_action(
            state={},
            data={"name": "Bob"},
            evaluator_type="schema",
            schema={
                "type": "object",
                "required": ["name", "email"],
            },
        )

        self.assertFalse(result["valid"])
        self.assertEqual(result["score"], 0.0)
        self.assertTrue(len(result["errors"]) > 0)

    def test_schema_validation_invalid_type(self):
        """Test: Schema validation fails for wrong type."""
        evaluate_action = self.registry["reflection.evaluate"]

        result = evaluate_action(
            state={},
            data={"age": "not a number"},
            evaluator_type="schema",
            schema={
                "type": "object",
                "properties": {"age": {"type": "integer"}},
            },
        )

        self.assertFalse(result["valid"])


class TestLLMEvaluator(unittest.TestCase):
    """Test suite for LLM-based evaluator (AC3)."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = YAMLEngine()
        self.registry = build_actions_registry(self.engine)

    def test_llm_evaluator_pass(self):
        """Test: LLM evaluator returns pass verdict."""

        def mock_llm_call(state, model, messages, **kwargs):
            return {"content": '{"valid": true, "score": 0.9, "reason": "Good output"}'}

        self.registry["llm.call"] = mock_llm_call

        evaluate_action = self.registry["reflection.evaluate"]

        result = evaluate_action(
            state={"llm_model": "test-model"},
            data="Some output to evaluate",
            evaluator_type="llm",
            prompt="Is this a good output?",
        )

        self.assertTrue(result["valid"])
        self.assertGreaterEqual(result["score"], 0.9)

    def test_llm_evaluator_fail(self):
        """Test: LLM evaluator returns fail verdict with suggestions."""

        def mock_llm_call(state, model, messages, **kwargs):
            return {
                "content": '{"valid": false, "score": 0.3, "reason": "Missing key info", "suggestions": ["Add more detail"]}'
            }

        self.registry["llm.call"] = mock_llm_call

        evaluate_action = self.registry["reflection.evaluate"]

        result = evaluate_action(
            state={"llm_model": "test-model"},
            data="Incomplete output",
            evaluator_type="llm",
            prompt="Evaluate completeness",
        )

        self.assertFalse(result["valid"])
        self.assertLess(result["score"], 0.5)


class TestCustomEvaluator(unittest.TestCase):
    """Test suite for custom evaluator (AC4)."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = YAMLEngine()
        self.registry = build_actions_registry(self.engine)

    def test_custom_python_evaluator_valid(self):
        """Test: Custom Python evaluator returns valid."""
        evaluate_action = self.registry["reflection.evaluate"]

        result = evaluate_action(
            state={},
            data={"name": "Alice", "age": 25},
            evaluator_type="custom",
            run='result = {"valid": len(output.get("name", "")) > 0, "score": 1.0}',
            language="python",
        )

        self.assertTrue(result["valid"])

    def test_custom_python_evaluator_invalid(self):
        """Test: Custom Python evaluator returns invalid with errors."""
        evaluate_action = self.registry["reflection.evaluate"]

        result = evaluate_action(
            state={},
            data={"name": ""},
            evaluator_type="custom",
            run="""
name = output.get("name", "")
if len(name) == 0:
    result = {"valid": False, "score": 0.0, "errors": [{"message": "Name cannot be empty"}]}
else:
    result = {"valid": True, "score": 1.0}
""",
            language="python",
        )

        self.assertFalse(result["valid"])
        self.assertEqual(result["score"], 0.0)

    def test_custom_evaluator_returns_bool(self):
        """Test: Custom evaluator can return simple bool."""
        evaluate_action = self.registry["reflection.evaluate"]

        result = evaluate_action(
            state={},
            data={"value": 10},
            evaluator_type="custom",
            run="result = output.get('value', 0) > 5",
            language="python",
        )

        self.assertTrue(result["valid"])
        self.assertEqual(result["score"], 1.0)


class TestOnFailureStrategies(unittest.TestCase):
    """Test suite for on-failure strategies (AC6)."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = YAMLEngine()
        self.registry = build_actions_registry(self.engine)

    def _setup_failing_generator(self, scores):
        """Setup generator that produces outputs with specific scores."""
        call_count = {"n": 0}

        def mock_generator(state, **kwargs):
            call_count["n"] += 1
            idx = min(call_count["n"] - 1, len(scores) - 1)
            return {"score": scores[idx], "iteration": call_count["n"]}

        self.registry["llm.call"] = mock_generator

        def mock_corrector(state, **kwargs):
            return {}

        self.registry["correction.action"] = mock_corrector

    def test_return_best_strategy(self):
        """Test: return_best returns highest-scoring attempt (AC6)."""
        self._setup_failing_generator([0.3, 0.8, 0.5])

        loop_action = self.registry["reflection.loop"]

        state = {}
        result = loop_action(
            state=state,
            generator={"action": "llm.call", "prompt": "Generate"},
            evaluator={
                "type": "custom",
                "run": """
score = output.get("score", 0) if isinstance(output, dict) else 0
result = {"valid": False, "score": score, "errors": []}
""",
            },
            corrector={"action": "correction.action"},
            max_iterations=3,
            on_failure="return_best",
        )

        # Best score was 0.8 (iteration 2)
        self.assertFalse(result.get("valid", result.get("success")))
        self.assertEqual(result["reflection_best_score"], 0.8)
        best = result["reflection_best"]
        self.assertEqual(best.get("iteration"), 2)

    def test_return_last_strategy(self):
        """Test: return_last returns final attempt (AC6)."""
        self._setup_failing_generator([0.8, 0.3, 0.5])

        loop_action = self.registry["reflection.loop"]

        state = {}
        result = loop_action(
            state=state,
            generator={"action": "llm.call", "prompt": "Generate"},
            evaluator={
                "type": "custom",
                "run": """
score = output.get("score", 0) if isinstance(output, dict) else 0
result = {"valid": False, "score": score, "errors": []}
""",
            },
            corrector={"action": "correction.action"},
            max_iterations=3,
            on_failure="return_last",
        )

        # Last was iteration 3
        self.assertFalse(result.get("valid", result.get("success")))
        self.assertEqual(result["reflection_iteration"], 3)
        output = result["reflection_output"]
        self.assertEqual(output.get("iteration"), 3)

    def test_raise_strategy(self):
        """Test: raise strategy throws ReflectionFailedError (AC6)."""
        self._setup_failing_generator([0.3, 0.3, 0.3])

        loop_action = self.registry["reflection.loop"]

        state = {}
        with self.assertRaises(ReflectionFailedError) as ctx:
            loop_action(
                state=state,
                generator={"action": "llm.call", "prompt": "Generate"},
                evaluator={
                    "type": "custom",
                    "run": "result = {'valid': False, 'score': 0.3}",
                },
                corrector={"action": "correction.action"},
                max_iterations=3,
                on_failure="raise",
            )

        self.assertIn("history", dir(ctx.exception))
        self.assertEqual(len(ctx.exception.history), 3)


class TestStandaloneActions(unittest.TestCase):
    """Test suite for standalone actions (AC7, AC8)."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = YAMLEngine()
        self.registry = build_actions_registry(self.engine)

    def test_evaluate_standalone(self):
        """Test: reflection.evaluate works independently (AC7)."""
        evaluate_action = self.registry["reflection.evaluate"]

        result = evaluate_action(
            state={},
            data={"key": "value"},
            evaluator_type="schema",
            schema={"type": "object", "required": ["key"]},
        )

        self.assertTrue(result["valid"])
        self.assertTrue(result["success"])

    def test_correct_standalone(self):
        """Test: reflection.correct works independently (AC8)."""

        def mock_corrector(state, **kwargs):
            output = state.get("reflection_output", {})
            errors = state.get("reflection_errors", [])
            return {"corrected": True, "fixed_errors": len(errors)}

        self.registry["my.corrector"] = mock_corrector

        correct_action = self.registry["reflection.correct"]

        result = correct_action(
            state={},
            data={"original": "data"},
            errors=[{"message": "Error 1"}, {"message": "Error 2"}],
            corrector_action="my.corrector",
        )

        self.assertTrue(result["success"])
        corrected = result["corrected_output"]
        self.assertTrue(corrected.get("corrected"))
        self.assertEqual(corrected.get("fixed_errors"), 2)


class TestRegistryIntegration(unittest.TestCase):
    """Test action registry integration (AC9)."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = YAMLEngine()
        self.registry = build_actions_registry(self.engine)

    def test_actions_registered(self):
        """Test: All reflection actions are registered."""
        self.assertIn("reflection.loop", self.registry)
        self.assertIn("reflection.evaluate", self.registry)
        self.assertIn("reflection.correct", self.registry)
        self.assertIn("actions.reflection_loop", self.registry)
        self.assertIn("actions.reflection_evaluate", self.registry)
        self.assertIn("actions.reflection_correct", self.registry)

    def test_reflection_failed_error_exported(self):
        """Test: ReflectionFailedError is exported from actions module."""
        from the_edge_agent.actions import ReflectionFailedError

        self.assertTrue(issubclass(ReflectionFailedError, Exception))


class TestEdgeCases(unittest.TestCase):
    """Test edge cases and error handling."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = YAMLEngine()
        self.registry = build_actions_registry(self.engine)

    def test_max_iterations_validation(self):
        """Test: Invalid max_iterations raises ValueError."""
        loop_action = self.registry["reflection.loop"]

        with self.assertRaises(ValueError):
            loop_action(
                state={},
                generator={"run": "result = {}"},
                evaluator={"type": "schema", "schema": {}},
                max_iterations=0,
            )

    def test_invalid_on_failure_strategy(self):
        """Test: Invalid on_failure raises ValueError."""
        loop_action = self.registry["reflection.loop"]

        with self.assertRaises(ValueError):
            loop_action(
                state={},
                generator={"run": "result = {}"},
                evaluator={"type": "schema", "schema": {}},
                max_iterations=1,
                on_failure="invalid_strategy",
            )

    def test_invalid_evaluator_type(self):
        """Test: Invalid evaluator type raises ValueError."""
        loop_action = self.registry["reflection.loop"]

        with self.assertRaises(ValueError):
            loop_action(
                state={},
                generator={"run": "result = {}"},
                evaluator={"type": "invalid_type"},
                max_iterations=1,
            )

    def test_generator_exception_handling(self):
        """Test: Generator exception is caught and recorded in history."""

        def failing_generator(state, **kwargs):
            raise RuntimeError("Generator crashed!")

        self.registry["failing.action"] = failing_generator

        loop_action = self.registry["reflection.loop"]

        result = loop_action(
            state={},
            generator={"action": "failing.action"},
            evaluator={"type": "schema", "schema": {}},
            max_iterations=2,
            on_failure="return_last",
        )

        self.assertEqual(len(result["reflection_history"]), 2)
        self.assertFalse(result["reflection_history"][0]["valid"])


if __name__ == "__main__":
    unittest.main()
