"""
TEA Experiment Framework for Opik Integration.

This module provides a generic experiment infrastructure for systematically
evaluating TEA agents using Comet Opik's evaluate() API. It enables:

- Running experiments on TEA YAML agents
- Creating datasets for evaluation
- Defining custom scoring metrics
- Comparing different agent strategies (A/B testing)

Usage:
    from the_edge_agent.experiments import (
        run_tea_experiment,
        create_dataset_from_fixtures,
        create_dataset_from_list,
        compare_strategies,
        BaseTeaMetric,
        jaccard_similarity,
        f1_score,
        completeness_score,
    )

Example:
    >>> from the_edge_agent.experiments import run_tea_experiment, BaseTeaMetric
    >>>
    >>> # Create a custom metric
    >>> class MyMetric(BaseTeaMetric):
    ...     name = "my_metric"
    ...     def score(self, output, expected_output=None, **kwargs):
    ...         # Custom scoring logic
    ...         return self.make_result(0.8, "Good match")
    >>>
    >>> # Run an experiment
    >>> result = run_tea_experiment(
    ...     agent_yaml="my_agent.yaml",
    ...     dataset_name="test_cases",
    ...     metrics=[MyMetric()],
    ...     experiment_name="my_agent_v1"
    ... )

Note:
    This module requires the `opik` package. Install with:
    - pip install opik
    - pip install the-edge-agent[opik]
    - pip install the-edge-agent[experiments]

    When Opik is not installed, stub classes are provided for type hints
    but actual experiment functionality will raise ImportError.
"""

from .runner import run_tea_experiment
from .datasets import create_dataset_from_fixtures, create_dataset_from_list
from .metrics import (
    BaseTeaMetric,
    jaccard_similarity,
    f1_score,
    completeness_score,
)
from .comparison import compare_strategies

__all__ = [
    "run_tea_experiment",
    "create_dataset_from_fixtures",
    "create_dataset_from_list",
    "compare_strategies",
    "BaseTeaMetric",
    "jaccard_similarity",
    "f1_score",
    "completeness_score",
]
