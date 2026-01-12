"""
A/B comparison utilities for TEA experiments.

This module provides utilities for comparing different agent strategies
or configurations side-by-side using Opik experiments.

Example:
    >>> from the_edge_agent.experiments import compare_strategies
    >>> from the_edge_agent.experiments.metrics import BaseTeaMetric
    >>>
    >>> class AccuracyMetric(BaseTeaMetric):
    ...     name = "accuracy"
    ...     def score(self, output, expected_output=None, **kwargs):
    ...         return self.make_result(0.8)
    >>>
    >>> result = compare_strategies(
    ...     strategy_a={"name": "gpt4", "agent_yaml": "agent_gpt4.yaml"},
    ...     strategy_b={"name": "gpt35", "agent_yaml": "agent_gpt35.yaml"},
    ...     dataset_name="test_cases",
    ...     metrics=[AccuracyMetric()],
    ... )
    >>> print(result["comparison"])
"""

import logging
from typing import Any, Dict, List, Optional

from .runner import run_tea_experiment, _check_opik_available, BaseMetric

logger = logging.getLogger(__name__)


def compare_strategies(
    strategy_a: Dict[str, Any],
    strategy_b: Dict[str, Any],
    dataset_name: str,
    metrics: List[BaseMetric],
    experiment_prefix: Optional[str] = None,
    project_name: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Run A/B comparison between two agent strategies.

    Runs both strategies on the same dataset and returns comparison results
    including metric deltas. Both experiments are created in the same Opik
    project for easy side-by-side comparison in the UI.

    Args:
        strategy_a: First strategy configuration dict with:
            - name: Strategy name (used in experiment name)
            - agent_yaml: Path to YAML agent file
            - settings: Optional YAMLEngine settings override
            - variables: Optional variables for YAMLEngine
        strategy_b: Second strategy configuration dict (same structure).
        dataset_name: Name of Opik dataset to use.
        metrics: List of scoring metrics.
        experiment_prefix: Optional prefix for experiment names.
            Default: "comparison_{timestamp}".
        project_name: Optional Opik project name.

    Returns:
        Dictionary with comparison results:
        - strategy_a_result: Result dict from strategy A experiment
        - strategy_b_result: Result dict from strategy B experiment
        - comparison: Summary of comparison with:
            - winner: Name of better performing strategy (or "tie")
            - metric_deltas: Dict of metric name -> (A score - B score)
        - both_completed: Whether both experiments completed successfully

    Raises:
        ImportError: If opik SDK not installed.
        ValueError: If strategy config missing required fields.

    Example:
        >>> result = compare_strategies(
        ...     strategy_a={
        ...         "name": "optimized",
        ...         "agent_yaml": "agent_v2.yaml",
        ...         "settings": {"temperature": 0.3}
        ...     },
        ...     strategy_b={
        ...         "name": "baseline",
        ...         "agent_yaml": "agent_v1.yaml",
        ...         "settings": {"temperature": 0.7}
        ...     },
        ...     dataset_name="eval_set",
        ...     metrics=[AccuracyMetric(), LatencyMetric()],
        ...     experiment_prefix="model_comparison_2024"
        ... )
        >>> print(result["comparison"]["winner"])
        "optimized"
    """
    _check_opik_available()

    # Validate strategy configs
    for strategy, name in [(strategy_a, "strategy_a"), (strategy_b, "strategy_b")]:
        if "name" not in strategy:
            raise ValueError(f"{name} must have 'name' field")
        if "agent_yaml" not in strategy:
            raise ValueError(f"{name} must have 'agent_yaml' field")

    # Generate experiment names
    import time

    timestamp = int(time.time())
    prefix = experiment_prefix or f"comparison_{timestamp}"

    experiment_a_name = f"{prefix}_{strategy_a['name']}"
    experiment_b_name = f"{prefix}_{strategy_b['name']}"

    # Run strategy A
    logger.info(f"Running strategy A: {strategy_a['name']}")
    result_a = run_tea_experiment(
        agent_yaml=strategy_a["agent_yaml"],
        dataset_name=dataset_name,
        metrics=metrics,
        experiment_name=experiment_a_name,
        experiment_config={
            "strategy": strategy_a["name"],
            "comparison_id": prefix,
            "role": "strategy_a",
        },
        settings=strategy_a.get("settings"),
        variables=strategy_a.get("variables"),
        project_name=project_name,
    )

    # Run strategy B
    logger.info(f"Running strategy B: {strategy_b['name']}")
    result_b = run_tea_experiment(
        agent_yaml=strategy_b["agent_yaml"],
        dataset_name=dataset_name,
        metrics=metrics,
        experiment_name=experiment_b_name,
        experiment_config={
            "strategy": strategy_b["name"],
            "comparison_id": prefix,
            "role": "strategy_b",
        },
        settings=strategy_b.get("settings"),
        variables=strategy_b.get("variables"),
        project_name=project_name,
    )

    # Build comparison result
    both_completed = (
        result_a.get("status") == "complete" and result_b.get("status") == "complete"
    )

    comparison = {
        "strategy_a_name": strategy_a["name"],
        "strategy_b_name": strategy_b["name"],
        "experiment_a_name": experiment_a_name,
        "experiment_b_name": experiment_b_name,
        "dataset_name": dataset_name,
        "comparison_id": prefix,
        "winner": "unknown",
        "metric_deltas": {},
        "note": (
            "View both experiments in Opik UI for detailed comparison. "
            "Filter by comparison_id in experiment config."
        ),
    }

    if not both_completed:
        comparison["winner"] = "incomplete"
        comparison["note"] = (
            f"Strategy A status: {result_a.get('status')}, "
            f"Strategy B status: {result_b.get('status')}"
        )

    logger.info(f"Comparison complete: {strategy_a['name']} vs {strategy_b['name']}")

    return {
        "strategy_a_result": result_a,
        "strategy_b_result": result_b,
        "comparison": comparison,
        "both_completed": both_completed,
    }
