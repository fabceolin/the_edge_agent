"""
Base metric framework and scoring helpers for TEA experiments.

This module provides:
- BaseTeaMetric: Base class for creating custom experiment metrics
- Scoring helper functions: jaccard_similarity, f1_score, completeness_score

The module gracefully degrades when Opik is not installed, providing stub
classes that allow code to be written and tested without Opik available.

Example:
    >>> from the_edge_agent.experiments.metrics import (
    ...     BaseTeaMetric,
    ...     jaccard_similarity,
    ...     f1_score,
    ...     completeness_score,
    ... )
    >>>
    >>> # Create a custom metric
    >>> class AccuracyMetric(BaseTeaMetric):
    ...     name = "accuracy"
    ...
    ...     def score(self, output, expected_output=None, **kwargs):
    ...         actual = set(output.get("items", []))
    ...         expected = set(expected_output.get("items", []))
    ...         score = f1_score(actual, expected)
    ...         return self.make_result(score, f"F1: {score:.2f}")
    >>>
    >>> # Use scoring helpers
    >>> jaccard_similarity({1, 2, 3}, {2, 3, 4})  # 0.5
    >>> f1_score({1, 2}, {1, 2, 3})  # 0.8
    >>> completeness_score(8, 10)  # 0.8
"""

from typing import Any, Set


# Graceful degradation when Opik is not installed
try:
    from opik.evaluation.metrics import BaseMetric
    from opik.evaluation.metrics import score_result

    OPIK_AVAILABLE = True
except ImportError:
    OPIK_AVAILABLE = False

    # Stub classes for when Opik is not installed
    class ScoreResult:
        """Stub ScoreResult for when Opik is not installed."""

        def __init__(self, name: str, value: float, reason: str = ""):
            self.name = name
            self.value = value
            self.reason = reason

        def __repr__(self) -> str:
            return f"ScoreResult(name={self.name!r}, value={self.value}, reason={self.reason!r})"

    class score_result:  # noqa: N801 - Match Opik's naming convention
        """Stub score_result module for when Opik is not installed."""

        ScoreResult = ScoreResult

    class BaseMetric:
        """
        Stub BaseMetric for when Opik is not installed.

        Allows code to be written and type-checked without Opik available.
        Actual experiments will raise ImportError at runtime.
        """

        name: str = "stub_metric"

        def score(self, **kwargs) -> ScoreResult:
            """
            Stub score method.

            Returns a zero score with a message indicating Opik is not installed.
            """
            return ScoreResult(self.name, 0.0, "Opik not installed")


class BaseTeaMetric(BaseMetric):
    """
    Base class for TEA experiment metrics.

    Extend this class to create domain-specific metrics for your experiments.
    The base class provides helper methods for creating score results.

    Attributes:
        name: The metric name displayed in Opik UI.

    Example:
        >>> class MyMetric(BaseTeaMetric):
        ...     name = "my_metric"
        ...
        ...     def score(self, output, expected_output=None, **kwargs):
        ...         # Custom scoring logic
        ...         value = self._calculate_score(output, expected_output)
        ...         return self.make_result(value, "Custom reason")
        ...
        ...     def _calculate_score(self, output, expected) -> float:
        ...         # Implementation details
        ...         return 0.8

    Note:
        When implementing score(), you receive:
        - output: The task output from running the agent
        - expected_output: The expected output from the dataset item (if provided)
        - **kwargs: Additional context (input, metadata, etc.)
    """

    name: str = "base_tea_metric"

    def make_result(self, value: float, reason: str = "") -> score_result.ScoreResult:
        """
        Create a ScoreResult with this metric's name.

        Args:
            value: The score value (typically 0.0 to 1.0).
            reason: Optional explanation for the score.

        Returns:
            A ScoreResult object with the metric name, value, and reason.

        Example:
            >>> def score(self, output, **kwargs):
            ...     return self.make_result(0.85, "85% match")
        """
        return score_result.ScoreResult(name=self.name, value=value, reason=reason)


def jaccard_similarity(set_a: Set[Any], set_b: Set[Any]) -> float:
    """
    Calculate Jaccard similarity coefficient between two sets.

    The Jaccard similarity is defined as the size of the intersection
    divided by the size of the union of the two sets.

    Args:
        set_a: First set of elements.
        set_b: Second set of elements.

    Returns:
        Float between 0.0 (no overlap) and 1.0 (identical sets).
        Returns 1.0 if both sets are empty.

    Example:
        >>> jaccard_similarity({1, 2, 3}, {2, 3, 4})
        0.5
        >>> jaccard_similarity({1, 2}, {1, 2})
        1.0
        >>> jaccard_similarity({1, 2}, {3, 4})
        0.0
        >>> jaccard_similarity(set(), set())
        1.0
    """
    if not set_a and not set_b:
        return 1.0
    intersection = len(set_a & set_b)
    union = len(set_a | set_b)
    return intersection / union if union > 0 else 0.0


def f1_score(actual: Set[Any], expected: Set[Any]) -> float:
    """
    Calculate F1 score (harmonic mean of precision and recall).

    F1 = 2 * (precision * recall) / (precision + recall)

    Where:
    - Precision = true_positives / len(actual)
    - Recall = true_positives / len(expected)

    Args:
        actual: Set of actual (predicted) elements.
        expected: Set of expected (ground truth) elements.

    Returns:
        Float between 0.0 and 1.0.
        Returns 1.0 if both sets are empty (perfect match of nothing).
        Returns 0.0 if actual is empty but expected is not.

    Example:
        >>> f1_score({1, 2}, {1, 2, 3})  # precision=1.0, recall=0.67
        0.8
        >>> f1_score({1, 2, 3}, {1, 2})  # precision=0.67, recall=1.0
        0.8
        >>> f1_score({1, 2}, {1, 2})
        1.0
        >>> f1_score({1, 2}, {3, 4})
        0.0
        >>> f1_score(set(), set())
        1.0
    """
    if not expected:
        return 1.0 if not actual else 0.0
    if not actual:
        return 0.0

    true_positives = len(actual & expected)
    precision = true_positives / len(actual) if actual else 0.0
    recall = true_positives / len(expected) if expected else 0.0

    if precision + recall == 0:
        return 0.0
    return 2 * precision * recall / (precision + recall)


def completeness_score(filled: int, total: int) -> float:
    """
    Calculate completeness as ratio of filled to total.

    Useful for measuring how much of an expected output was populated.

    Args:
        filled: Number of filled/present items.
        total: Total expected items.

    Returns:
        Float between 0.0 and 1.0.
        Returns 0.0 if total is 0 or negative.
        Caps at 1.0 if filled > total.

    Example:
        >>> completeness_score(8, 10)
        0.8
        >>> completeness_score(10, 10)
        1.0
        >>> completeness_score(0, 10)
        0.0
        >>> completeness_score(15, 10)  # Capped at 1.0
        1.0
        >>> completeness_score(5, 0)  # Invalid total
        0.0
    """
    if total <= 0:
        return 0.0
    return min(filled / total, 1.0)
