"""
Tests for TEA-BUILTIN-005.4: Experiment Framework - Comparison Module

Test coverage for:
- compare_strategies function
- A/B comparison result structure
- Error handling
"""

import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch


class TestCompareStrategies(unittest.TestCase):
    """Unit tests for compare_strategies function."""

    def setUp(self):
        """Set up mock Opik and temp files for each test."""
        self.mock_opik = MagicMock()
        self.mock_client = MagicMock()
        self.mock_dataset = MagicMock()
        self.mock_opik.Opik.return_value = self.mock_client
        self.mock_client.get_dataset.return_value = self.mock_dataset

        # Create temp YAML files for strategies
        self.temp_dir = tempfile.mkdtemp()

        self.agent_a = Path(self.temp_dir) / "agent_a.yaml"
        self.agent_a.write_text(
            """
name: agent-a
state_schema:
  input: str
  output: str

nodes:
  - name: process
    run: |
      return {"output": "A: " + state["input"]}

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"""
        )

        self.agent_b = Path(self.temp_dir) / "agent_b.yaml"
        self.agent_b.write_text(
            """
name: agent-b
state_schema:
  input: str
  output: str

nodes:
  - name: process
    run: |
      return {"output": "B: " + state["input"]}

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"""
        )

    def tearDown(self):
        """Clean up temp files."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_runs_both_strategies(self):
        """P0: Both strategies are executed."""
        with patch.dict("sys.modules", {"opik": self.mock_opik}):
            with patch("the_edge_agent.experiments.runner.evaluate") as mock_evaluate:
                from the_edge_agent.experiments.comparison import compare_strategies

                result = compare_strategies(
                    strategy_a={"name": "strategy_a", "agent_yaml": str(self.agent_a)},
                    strategy_b={"name": "strategy_b", "agent_yaml": str(self.agent_b)},
                    dataset_name="test",
                    metrics=[],
                )

                # evaluate should be called twice (once per strategy)
                self.assertEqual(mock_evaluate.call_count, 2)

    def test_returns_both_results(self):
        """P0: Returns results for both strategies."""
        with patch.dict("sys.modules", {"opik": self.mock_opik}):
            with patch("the_edge_agent.experiments.runner.evaluate") as mock_evaluate:
                from the_edge_agent.experiments.comparison import compare_strategies

                result = compare_strategies(
                    strategy_a={"name": "strategy_a", "agent_yaml": str(self.agent_a)},
                    strategy_b={"name": "strategy_b", "agent_yaml": str(self.agent_b)},
                    dataset_name="test",
                    metrics=[],
                )

                self.assertIn("strategy_a_result", result)
                self.assertIn("strategy_b_result", result)

    def test_returns_comparison_summary(self):
        """P0: Returns comparison summary."""
        with patch.dict("sys.modules", {"opik": self.mock_opik}):
            with patch("the_edge_agent.experiments.runner.evaluate") as mock_evaluate:
                from the_edge_agent.experiments.comparison import compare_strategies

                result = compare_strategies(
                    strategy_a={"name": "optimized", "agent_yaml": str(self.agent_a)},
                    strategy_b={"name": "baseline", "agent_yaml": str(self.agent_b)},
                    dataset_name="test",
                    metrics=[],
                )

                self.assertIn("comparison", result)
                self.assertIn("both_completed", result)
                self.assertEqual(result["comparison"]["strategy_a_name"], "optimized")
                self.assertEqual(result["comparison"]["strategy_b_name"], "baseline")

    def test_missing_name_raises_error(self):
        """P0: ValueError if strategy missing 'name' field."""
        with patch.dict("sys.modules", {"opik": self.mock_opik}):
            from the_edge_agent.experiments.comparison import compare_strategies

            with self.assertRaises(ValueError) as ctx:
                compare_strategies(
                    strategy_a={"agent_yaml": str(self.agent_a)},  # Missing name
                    strategy_b={"name": "b", "agent_yaml": str(self.agent_b)},
                    dataset_name="test",
                    metrics=[],
                )

            self.assertIn("name", str(ctx.exception).lower())

    def test_missing_agent_yaml_raises_error(self):
        """P0: ValueError if strategy missing 'agent_yaml' field."""
        with patch.dict("sys.modules", {"opik": self.mock_opik}):
            from the_edge_agent.experiments.comparison import compare_strategies

            with self.assertRaises(ValueError) as ctx:
                compare_strategies(
                    strategy_a={"name": "a"},  # Missing agent_yaml
                    strategy_b={"name": "b", "agent_yaml": str(self.agent_b)},
                    dataset_name="test",
                    metrics=[],
                )

            self.assertIn("agent_yaml", str(ctx.exception).lower())

    def test_custom_experiment_prefix(self):
        """P1: Custom experiment prefix used in names."""
        with patch.dict("sys.modules", {"opik": self.mock_opik}):
            with patch("the_edge_agent.experiments.runner.evaluate") as mock_evaluate:
                from the_edge_agent.experiments.comparison import compare_strategies

                result = compare_strategies(
                    strategy_a={"name": "a", "agent_yaml": str(self.agent_a)},
                    strategy_b={"name": "b", "agent_yaml": str(self.agent_b)},
                    dataset_name="test",
                    metrics=[],
                    experiment_prefix="my_comparison_2024",
                )

                # Check experiment names include prefix
                self.assertIn(
                    "my_comparison_2024", result["comparison"]["experiment_a_name"]
                )
                self.assertIn(
                    "my_comparison_2024", result["comparison"]["experiment_b_name"]
                )

    def test_both_completed_true_on_success(self):
        """P1: both_completed is True when both strategies succeed."""
        with patch.dict("sys.modules", {"opik": self.mock_opik}):
            with patch("the_edge_agent.experiments.runner.evaluate") as mock_evaluate:
                from the_edge_agent.experiments.comparison import compare_strategies

                result = compare_strategies(
                    strategy_a={"name": "a", "agent_yaml": str(self.agent_a)},
                    strategy_b={"name": "b", "agent_yaml": str(self.agent_b)},
                    dataset_name="test",
                    metrics=[],
                )

                self.assertTrue(result["both_completed"])

    def test_both_completed_false_on_failure(self):
        """P1: both_completed is False when a strategy fails."""
        with patch.dict("sys.modules", {"opik": self.mock_opik}):
            with patch("the_edge_agent.experiments.runner.evaluate") as mock_evaluate:
                # Make first call succeed, second fail
                mock_evaluate.side_effect = [None, Exception("Failed!")]

                from the_edge_agent.experiments.comparison import compare_strategies

                result = compare_strategies(
                    strategy_a={"name": "a", "agent_yaml": str(self.agent_a)},
                    strategy_b={"name": "b", "agent_yaml": str(self.agent_b)},
                    dataset_name="test",
                    metrics=[],
                )

                self.assertFalse(result["both_completed"])

    def test_passes_project_name(self):
        """P1: Project name is passed to both experiments."""
        with patch.dict("sys.modules", {"opik": self.mock_opik}):
            with patch("the_edge_agent.experiments.runner.evaluate") as mock_evaluate:
                from the_edge_agent.experiments.comparison import compare_strategies

                result = compare_strategies(
                    strategy_a={"name": "a", "agent_yaml": str(self.agent_a)},
                    strategy_b={"name": "b", "agent_yaml": str(self.agent_b)},
                    dataset_name="test",
                    metrics=[],
                    project_name="my-comparison-project",
                )

                # Verify Opik was called with project name
                self.mock_opik.Opik.assert_called_with(
                    project_name="my-comparison-project"
                )


if __name__ == "__main__":
    unittest.main()
