"""
Tests for TEA-BUILTIN-005.4: Experiment Framework - Runner Module

Test coverage for:
- run_tea_experiment function
- Task factory and YAMLEngine integration
- Configuration resolution (explicit > env > yaml)
- Graceful degradation when Opik not installed
"""

import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch


class TestRunTeaExperiment(unittest.TestCase):
    """Unit tests for run_tea_experiment function."""

    def setUp(self):
        """Set up mock Opik for each test."""
        # Create patches for runner module
        self.opik_available_patch = patch(
            "the_edge_agent.experiments.runner.OPIK_AVAILABLE", True
        )
        self.opik_patch = patch("the_edge_agent.experiments.runner.opik")
        self.evaluate_patch = patch("the_edge_agent.experiments.runner.evaluate")

        self.opik_available_patch.start()
        self.mock_opik = self.opik_patch.start()
        self.mock_evaluate = self.evaluate_patch.start()

        self.mock_client = MagicMock()
        self.mock_dataset = MagicMock()
        self.mock_opik.Opik.return_value = self.mock_client
        self.mock_client.get_dataset.return_value = self.mock_dataset

        # Create temp YAML file
        self.temp_dir = tempfile.mkdtemp()
        self.agent_yaml = Path(self.temp_dir) / "test_agent.yaml"
        self.agent_yaml.write_text(
            """
name: test-agent
state_schema:
  input: str
  output: str

nodes:
  - name: process
    run: |
      return {"output": state["input"].upper()}

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"""
        )

    def tearDown(self):
        """Clean up temp files and stop patches."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

        self.evaluate_patch.stop()
        self.opik_patch.stop()
        self.opik_available_patch.stop()

    def test_agent_not_found_raises_error(self):
        """P0: FileNotFoundError for nonexistent agent."""
        from the_edge_agent.experiments.runner import run_tea_experiment

        with self.assertRaises(FileNotFoundError):
            run_tea_experiment(
                agent_yaml="/nonexistent/agent.yaml",
                dataset_name="test",
                metrics=[],
                experiment_name="test_exp",
            )

    def test_uses_default_project_name(self):
        """P1: Default project name is 'the-edge-agent'."""
        from the_edge_agent.experiments.runner import run_tea_experiment

        run_tea_experiment(
            agent_yaml=str(self.agent_yaml),
            dataset_name="test",
            metrics=[],
            experiment_name="test_exp",
        )

        # Verify Opik was called with default project name
        self.mock_opik.Opik.assert_called_with(project_name="the-edge-agent")

    def test_explicit_project_name_overrides_default(self):
        """P1: Explicit project_name overrides default."""
        from the_edge_agent.experiments.runner import run_tea_experiment

        run_tea_experiment(
            agent_yaml=str(self.agent_yaml),
            dataset_name="test",
            metrics=[],
            experiment_name="test_exp",
            project_name="my-custom-project",
        )

        self.mock_opik.Opik.assert_called_with(project_name="my-custom-project")

    def test_env_var_project_name(self):
        """P1: OPIK_PROJECT_NAME env var is used."""
        with patch.dict(
            os.environ, {"OPIK_PROJECT_NAME": "env-project"}, clear=False
        ):
            from the_edge_agent.experiments.runner import run_tea_experiment

            run_tea_experiment(
                agent_yaml=str(self.agent_yaml),
                dataset_name="test",
                metrics=[],
                experiment_name="test_exp",
            )

            self.mock_opik.Opik.assert_called_with(project_name="env-project")

    def test_returns_success_result(self):
        """P0: Returns success result on completion."""
        from the_edge_agent.experiments.runner import run_tea_experiment

        result = run_tea_experiment(
            agent_yaml=str(self.agent_yaml),
            dataset_name="test",
            metrics=[],
            experiment_name="test_exp",
        )

        self.assertEqual(result["status"], "complete")
        self.assertEqual(result["experiment_name"], "test_exp")
        self.assertEqual(result["dataset_name"], "test")

    def test_returns_error_result_on_failure(self):
        """P0: Returns error result on experiment failure."""
        self.mock_evaluate.side_effect = Exception("Experiment failed!")

        from the_edge_agent.experiments.runner import run_tea_experiment

        result = run_tea_experiment(
            agent_yaml=str(self.agent_yaml),
            dataset_name="test",
            metrics=[],
            experiment_name="test_exp",
        )

        self.assertEqual(result["status"], "error")
        self.assertIn("error", result)
        self.assertIn("Experiment failed!", result["error"])

    def test_dataset_not_found_raises_error(self):
        """P0: ValueError for nonexistent dataset."""
        self.mock_client.get_dataset.side_effect = Exception("Dataset not found")

        from the_edge_agent.experiments.runner import run_tea_experiment

        with self.assertRaises(ValueError) as ctx:
            run_tea_experiment(
                agent_yaml=str(self.agent_yaml),
                dataset_name="nonexistent",
                metrics=[],
                experiment_name="test_exp",
            )

        self.assertIn("not found", str(ctx.exception).lower())

    def test_experiment_config_passed(self):
        """P1: Experiment config is passed to evaluate."""
        from the_edge_agent.experiments.runner import run_tea_experiment

        run_tea_experiment(
            agent_yaml=str(self.agent_yaml),
            dataset_name="test",
            metrics=[],
            experiment_name="test_exp",
            experiment_config={"model": "gpt-4", "temperature": 0.7},
        )

        call_kwargs = self.mock_evaluate.call_args[1]
        self.assertIn("experiment_config", call_kwargs)
        self.assertEqual(call_kwargs["experiment_config"]["model"], "gpt-4")


class TestConfigResolution(unittest.TestCase):
    """Tests for configuration resolution (explicit > env > yaml)."""

    def test_explicit_overrides_env(self):
        """P0: Explicit params override env vars."""
        from the_edge_agent.experiments.runner import _resolve_config

        with patch.dict(os.environ, {"OPIK_PROJECT_NAME": "env-project"}):
            result = _resolve_config(
                yaml_config={},
                explicit={"project_name": "explicit-project"},
            )

            self.assertEqual(result["project_name"], "explicit-project")

    def test_env_overrides_yaml(self):
        """P0: Env vars override YAML config."""
        from the_edge_agent.experiments.runner import _resolve_config

        with patch.dict(os.environ, {"OPIK_PROJECT_NAME": "env-project"}):
            result = _resolve_config(
                yaml_config={"project_name": "yaml-project"},
                explicit={},
            )

            self.assertEqual(result["project_name"], "env-project")

    def test_yaml_used_as_fallback(self):
        """P0: YAML config used when no explicit/env."""
        from the_edge_agent.experiments.runner import _resolve_config

        # Ensure env var is not set
        with patch.dict(os.environ, {}, clear=True):
            result = _resolve_config(
                yaml_config={"project_name": "yaml-project"},
                explicit={},
            )

            self.assertEqual(result["project_name"], "yaml-project")


class TestRunnerMissingOpik(unittest.TestCase):
    """Tests for behavior when Opik is not installed."""

    def test_import_error_without_opik(self):
        """P0: ImportError raised when Opik not installed."""
        # Clear opik from modules
        saved_modules = {}
        modules_to_hide = [k for k in list(sys.modules.keys()) if "opik" in k]
        for mod in modules_to_hide:
            saved_modules[mod] = sys.modules.pop(mod)

        try:
            with patch.dict("sys.modules", {"opik": None}):
                # Force reimport
                import importlib
                from the_edge_agent.experiments import runner

                importlib.reload(runner)

                with self.assertRaises(ImportError) as ctx:
                    runner.run_tea_experiment(
                        agent_yaml="test.yaml",
                        dataset_name="test",
                        metrics=[],
                        experiment_name="test",
                    )

                self.assertIn("opik", str(ctx.exception).lower())

        finally:
            sys.modules.update(saved_modules)


if __name__ == "__main__":
    unittest.main()
