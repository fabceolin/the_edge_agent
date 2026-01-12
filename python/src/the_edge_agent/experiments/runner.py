"""
Experiment runner for TEA agents using Opik evaluate().

This module provides the main experiment runner function that wraps
Opik's evaluate() API for use with TEA YAML agents.

Example:
    >>> from the_edge_agent.experiments import run_tea_experiment
    >>> from the_edge_agent.experiments.metrics import BaseTeaMetric
    >>>
    >>> # Define custom metric
    >>> class MyMetric(BaseTeaMetric):
    ...     name = "my_metric"
    ...     def score(self, output, expected_output=None, **kwargs):
    ...         return self.make_result(0.8, "Good")
    >>>
    >>> # Run experiment
    >>> result = run_tea_experiment(
    ...     agent_yaml="my_agent.yaml",
    ...     dataset_name="test_cases",
    ...     metrics=[MyMetric()],
    ...     experiment_name="agent_v1"
    ... )
"""

import logging
import os
import time
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

logger = logging.getLogger(__name__)


# Graceful degradation when Opik is not installed
try:
    import opik
    from opik import evaluate
    from opik.evaluation.metrics import BaseMetric

    OPIK_AVAILABLE = True
except ImportError:
    OPIK_AVAILABLE = False
    BaseMetric = object  # Stub for type hints


def _check_opik_available() -> None:
    """
    Check if the opik package is installed.

    Raises:
        ImportError: If opik is not installed.
    """
    if not OPIK_AVAILABLE:
        raise ImportError(
            "Opik SDK not installed. Install with: pip install opik\n"
            "Or install The Edge Agent with Opik support: "
            "pip install the-edge-agent[opik]"
        )


def _get_config_from_yaml_settings(
    yaml_path: Union[str, Path],
) -> Dict[str, Any]:
    """
    Extract Opik configuration from YAML agent settings.

    Looks for settings.opik.* in the YAML file.

    Args:
        yaml_path: Path to YAML agent file.

    Returns:
        Dictionary with Opik configuration from YAML.
    """
    import yaml

    yaml_path = Path(yaml_path)
    if not yaml_path.exists():
        return {}

    try:
        with open(yaml_path, "r", encoding="utf-8") as f:
            config = yaml.safe_load(f)

        if not config:
            return {}

        settings = config.get("settings", {})
        opik_settings = settings.get("opik", {})

        return {
            "project_name": opik_settings.get("project_name"),
            "workspace": opik_settings.get("workspace"),
            "api_key": opik_settings.get("api_key"),
        }
    except Exception as e:
        logger.debug(f"Could not read Opik settings from YAML: {e}")
        return {}


def _resolve_config(
    yaml_config: Dict[str, Any],
    env_prefix: str = "OPIK_",
    explicit: Dict[str, Any] = None,
) -> Dict[str, Any]:
    """
    Resolve configuration with precedence: explicit > env > yaml.

    Args:
        yaml_config: Configuration from YAML settings.
        env_prefix: Prefix for environment variables.
        explicit: Explicitly provided configuration.

    Returns:
        Merged configuration dictionary.
    """
    explicit = explicit or {}

    # Environment variable mapping
    env_mapping = {
        "project_name": "PROJECT_NAME",
        "workspace": "WORKSPACE",
        "api_key": "API_KEY",
    }

    result = {}
    for key, env_suffix in env_mapping.items():
        # Priority: explicit > env > yaml
        if explicit.get(key):
            result[key] = explicit[key]
        elif os.getenv(f"{env_prefix}{env_suffix}"):
            result[key] = os.getenv(f"{env_prefix}{env_suffix}")
        elif yaml_config.get(key):
            result[key] = yaml_config[key]

    return result


def run_tea_experiment(
    agent_yaml: Union[str, Path],
    dataset_name: str,
    metrics: List[BaseMetric],
    experiment_name: str,
    experiment_config: Optional[Dict[str, Any]] = None,
    settings: Optional[Dict[str, Any]] = None,
    project_name: Optional[str] = None,
    workspace: Optional[str] = None,
    variables: Optional[Dict[str, Any]] = None,
    capture_output: bool = True,
    capture_duration: bool = True,
) -> Dict[str, Any]:
    """
    Run an Opik experiment for a TEA YAML agent.

    This function wraps Opik's evaluate() API to run experiments on TEA agents.
    It creates a task function that instantiates and runs the YAML agent for
    each dataset item, then scores the results using provided metrics.

    Configuration is resolved with precedence:
    1. Explicit arguments (project_name, workspace)
    2. Environment variables (OPIK_PROJECT_NAME, OPIK_WORKSPACE)
    3. YAML agent settings (settings.opik.*)

    Args:
        agent_yaml: Path to YAML agent file.
        dataset_name: Name of existing Opik dataset.
        metrics: List of scoring metrics (BaseTeaMetric instances).
        experiment_name: Name for the experiment run.
        experiment_config: Optional metadata dict to attach to experiment.
        settings: Optional YAMLEngine settings override.
        project_name: Optional Opik project name.
        workspace: Optional Opik workspace.
        variables: Optional variables to pass to YAMLEngine.
        capture_output: Whether to capture output in results (default: True).
        capture_duration: Whether to capture duration metrics (default: True).

    Returns:
        Dictionary with experiment results:
        - experiment_name: Name of the experiment
        - status: "complete" or "error"
        - dataset_name: Name of dataset used
        - metrics_count: Number of metrics used
        - error: Error message if status is "error"

    Raises:
        ImportError: If opik SDK not installed.
        FileNotFoundError: If agent_yaml does not exist.
        ValueError: If dataset not found.

    Example:
        >>> from the_edge_agent.experiments import run_tea_experiment
        >>> from the_edge_agent.experiments.metrics import BaseTeaMetric
        >>>
        >>> class MyMetric(BaseTeaMetric):
        ...     name = "accuracy"
        ...     def score(self, output, expected_output=None, **kwargs):
        ...         match = output.get("answer") == expected_output.get("answer")
        ...         return self.make_result(1.0 if match else 0.0)
        >>>
        >>> result = run_tea_experiment(
        ...     agent_yaml="qa_agent.yaml",
        ...     dataset_name="qa_test_cases",
        ...     metrics=[MyMetric()],
        ...     experiment_name="qa_agent_v2.0",
        ...     experiment_config={"model": "gpt-4", "temperature": 0.7}
        ... )
    """
    _check_opik_available()

    # Validate agent path
    agent_path = Path(agent_yaml)
    if not agent_path.exists():
        raise FileNotFoundError(f"Agent YAML not found: {agent_yaml}")

    # Get configuration from YAML
    yaml_config = _get_config_from_yaml_settings(agent_path)

    # Resolve final configuration
    resolved_config = _resolve_config(
        yaml_config,
        explicit={
            "project_name": project_name,
            "workspace": workspace,
        },
    )

    # Use resolved project name or default
    final_project_name = resolved_config.get("project_name") or "the-edge-agent"

    # Configure Opik if we have custom settings
    if resolved_config.get("workspace"):
        opik.configure(workspace=resolved_config["workspace"])

    # Import YAMLEngine here to avoid circular imports
    from the_edge_agent import YAMLEngine

    # Create task function factory
    def create_task() -> callable:
        """Create the task function that runs the TEA agent."""

        def tea_task(dataset_item: Dict[str, Any]) -> Dict[str, Any]:
            """
            Task function that runs TEA agent on a single dataset item.

            Args:
                dataset_item: Dict with 'input' and optionally 'expected_output'.

            Returns:
                Dict with 'output' key containing agent results.
            """
            # Extract input from dataset item
            input_data = dataset_item.get("input", dataset_item)

            # Create engine for this task
            engine = YAMLEngine(
                str(agent_path),
                settings=settings,
                variables=variables,
            )

            # Track timing
            start_time = time.time()

            # Run agent and collect results
            result = {}
            try:
                for event in engine.run(input_data):
                    if isinstance(event, dict):
                        result.update(event)
            except Exception as e:
                logger.warning(f"Agent execution error: {e}")
                result["error"] = str(e)

            # Calculate duration
            duration_ms = (time.time() - start_time) * 1000

            # Build output
            output = {"output": result}

            if capture_duration:
                output["duration_ms"] = duration_ms

            return output

        return tea_task

    # Get client and dataset
    client = opik.Opik(project_name=final_project_name)

    try:
        dataset = client.get_dataset(name=dataset_name)
    except Exception as e:
        raise ValueError(f"Dataset '{dataset_name}' not found: {e}")

    # Build experiment config
    final_experiment_config = experiment_config or {}
    final_experiment_config["agent_yaml"] = str(agent_path)

    # Run experiment
    try:
        evaluate(
            dataset=dataset,
            task=create_task(),
            scoring_metrics=metrics,
            experiment_name=experiment_name,
            experiment_config=final_experiment_config,
        )

        logger.info(f"Experiment complete: {experiment_name}")

        return {
            "experiment_name": experiment_name,
            "status": "complete",
            "dataset_name": dataset_name,
            "metrics_count": len(metrics),
        }

    except Exception as e:
        logger.error(f"Experiment failed: {e}")
        return {
            "experiment_name": experiment_name,
            "status": "error",
            "dataset_name": dataset_name,
            "metrics_count": len(metrics),
            "error": str(e),
        }
