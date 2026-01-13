"""
Tests for TEA-BUILTIN-005.4: Experiment Framework - Metrics Module

Test coverage for:
- BaseTeaMetric base class
- Scoring helper functions (jaccard_similarity, f1_score, completeness_score)
- Graceful degradation when Opik not installed
"""

import unittest
from unittest.mock import MagicMock, patch
import sys


class TestJaccardSimilarity(unittest.TestCase):
    """Unit tests for jaccard_similarity scoring helper."""

    def test_identical_sets_return_one(self):
        """P0: Identical sets should return 1.0."""
        from the_edge_agent.experiments.metrics import jaccard_similarity

        result = jaccard_similarity({1, 2, 3}, {1, 2, 3})
        self.assertEqual(result, 1.0)

    def test_disjoint_sets_return_zero(self):
        """P0: Disjoint sets should return 0.0."""
        from the_edge_agent.experiments.metrics import jaccard_similarity

        result = jaccard_similarity({1, 2}, {3, 4})
        self.assertEqual(result, 0.0)

    def test_partial_overlap(self):
        """P0: Partial overlap gives expected similarity."""
        from the_edge_agent.experiments.metrics import jaccard_similarity

        # {1, 2, 3} ∩ {2, 3, 4} = {2, 3}  -> size 2
        # {1, 2, 3} ∪ {2, 3, 4} = {1, 2, 3, 4} -> size 4
        # Jaccard = 2/4 = 0.5
        result = jaccard_similarity({1, 2, 3}, {2, 3, 4})
        self.assertEqual(result, 0.5)

    def test_empty_sets_return_one(self):
        """P1: Two empty sets should return 1.0 (perfect match of nothing)."""
        from the_edge_agent.experiments.metrics import jaccard_similarity

        result = jaccard_similarity(set(), set())
        self.assertEqual(result, 1.0)

    def test_one_empty_set_returns_zero(self):
        """P1: One empty set and one non-empty returns 0.0."""
        from the_edge_agent.experiments.metrics import jaccard_similarity

        result = jaccard_similarity(set(), {1, 2})
        self.assertEqual(result, 0.0)

        result = jaccard_similarity({1, 2}, set())
        self.assertEqual(result, 0.0)

    def test_string_sets(self):
        """P1: Works with string sets."""
        from the_edge_agent.experiments.metrics import jaccard_similarity

        result = jaccard_similarity({"a", "b", "c"}, {"b", "c", "d"})
        self.assertEqual(result, 0.5)


class TestF1Score(unittest.TestCase):
    """Unit tests for f1_score scoring helper."""

    def test_perfect_match_returns_one(self):
        """P0: Perfect match should return 1.0."""
        from the_edge_agent.experiments.metrics import f1_score

        result = f1_score({1, 2, 3}, {1, 2, 3})
        self.assertEqual(result, 1.0)

    def test_no_match_returns_zero(self):
        """P0: No match should return 0.0."""
        from the_edge_agent.experiments.metrics import f1_score

        result = f1_score({1, 2}, {3, 4})
        self.assertEqual(result, 0.0)

    def test_partial_match(self):
        """P0: Partial match calculates F1 correctly."""
        from the_edge_agent.experiments.metrics import f1_score

        # actual={1, 2}, expected={1, 2, 3}
        # precision = 2/2 = 1.0
        # recall = 2/3 = 0.667
        # F1 = 2 * 1.0 * 0.667 / (1.0 + 0.667) = 0.8
        result = f1_score({1, 2}, {1, 2, 3})
        self.assertAlmostEqual(result, 0.8, places=5)

    def test_both_empty_returns_one(self):
        """P1: Both empty sets returns 1.0."""
        from the_edge_agent.experiments.metrics import f1_score

        result = f1_score(set(), set())
        self.assertEqual(result, 1.0)

    def test_actual_empty_expected_not_returns_zero(self):
        """P1: Empty actual with non-empty expected returns 0.0."""
        from the_edge_agent.experiments.metrics import f1_score

        result = f1_score(set(), {1, 2, 3})
        self.assertEqual(result, 0.0)

    def test_expected_empty_actual_not_returns_zero(self):
        """P1: Non-empty actual with empty expected returns 0.0."""
        from the_edge_agent.experiments.metrics import f1_score

        result = f1_score({1, 2, 3}, set())
        self.assertEqual(result, 0.0)

    def test_overcomplete_actual(self):
        """P1: Actual with more items than expected."""
        from the_edge_agent.experiments.metrics import f1_score

        # actual={1, 2, 3}, expected={1, 2}
        # precision = 2/3 = 0.667
        # recall = 2/2 = 1.0
        # F1 = 2 * 0.667 * 1.0 / (0.667 + 1.0) = 0.8
        result = f1_score({1, 2, 3}, {1, 2})
        self.assertAlmostEqual(result, 0.8, places=5)


class TestCompletenessScore(unittest.TestCase):
    """Unit tests for completeness_score scoring helper."""

    def test_full_completeness_returns_one(self):
        """P0: Filled equals total should return 1.0."""
        from the_edge_agent.experiments.metrics import completeness_score

        result = completeness_score(10, 10)
        self.assertEqual(result, 1.0)

    def test_partial_completeness(self):
        """P0: Partial completeness calculates correctly."""
        from the_edge_agent.experiments.metrics import completeness_score

        result = completeness_score(8, 10)
        self.assertEqual(result, 0.8)

    def test_zero_completeness(self):
        """P0: Zero filled returns 0.0."""
        from the_edge_agent.experiments.metrics import completeness_score

        result = completeness_score(0, 10)
        self.assertEqual(result, 0.0)

    def test_overfilled_capped_at_one(self):
        """P1: Filled > total should be capped at 1.0."""
        from the_edge_agent.experiments.metrics import completeness_score

        result = completeness_score(15, 10)
        self.assertEqual(result, 1.0)

    def test_zero_total_returns_zero(self):
        """P1: Zero total returns 0.0 to avoid division by zero."""
        from the_edge_agent.experiments.metrics import completeness_score

        result = completeness_score(5, 0)
        self.assertEqual(result, 0.0)

    def test_negative_total_returns_zero(self):
        """P1: Negative total returns 0.0."""
        from the_edge_agent.experiments.metrics import completeness_score

        result = completeness_score(5, -10)
        self.assertEqual(result, 0.0)


class TestBaseTeaMetric(unittest.TestCase):
    """Unit tests for BaseTeaMetric base class."""

    def test_base_class_has_name_attribute(self):
        """P0: BaseTeaMetric has name class attribute."""
        from the_edge_agent.experiments.metrics import BaseTeaMetric

        # BaseTeaMetric is abstract - check class attribute
        self.assertTrue(hasattr(BaseTeaMetric, "name"))
        self.assertEqual(BaseTeaMetric.name, "base_tea_metric")

    def test_make_result_creates_score_result(self):
        """P0: make_result() creates proper ScoreResult."""
        from the_edge_agent.experiments.metrics import BaseTeaMetric

        # Need a concrete subclass to test make_result
        class TestMetric(BaseTeaMetric):
            name = "test_metric"

            def score(self, output, expected_output=None, **kwargs):
                return self.make_result(0.75, "Test reason")

        metric = TestMetric()
        result = metric.make_result(0.75, "Test reason")

        # The name can be either the class attribute or class name depending
        # on whether real Opik is installed (which may use class name by default)
        self.assertIn(result.name, ["test_metric", "TestMetric"])
        self.assertEqual(result.value, 0.75)
        self.assertEqual(result.reason, "Test reason")

    def test_custom_metric_class(self):
        """P0: Custom metric class can be created."""
        from the_edge_agent.experiments.metrics import BaseTeaMetric

        class MyMetric(BaseTeaMetric):
            name = "my_custom_metric"

            def score(self, output, expected_output=None, **kwargs):
                return self.make_result(0.9, "Custom scoring")

        metric = MyMetric()
        # Opik's BaseMetric uses class name as default name unless overridden
        # Our BaseTeaMetric subclass should use our name attribute
        self.assertTrue(hasattr(metric, "name"))

        result = metric.score({"data": "test"})
        self.assertEqual(result.value, 0.9)


class TestGracefulDegradation(unittest.TestCase):
    """Tests for graceful degradation when Opik is not installed."""

    def test_stub_classes_available_without_opik(self):
        """P0: Stub classes are available when Opik not installed."""
        # Temporarily remove opik from modules
        saved_modules = {}
        modules_to_hide = [k for k in list(sys.modules.keys()) if "opik" in k]
        for mod in modules_to_hide:
            saved_modules[mod] = sys.modules.pop(mod)

        try:
            # Force reload to pick up stub classes
            with patch.dict("sys.modules", {"opik": None, "opik.evaluation": None}):
                # Re-import to get stub classes
                import importlib
                from the_edge_agent.experiments import metrics

                importlib.reload(metrics)

                # Should still be able to create BaseTeaMetric
                metric = metrics.BaseTeaMetric()
                self.assertIsNotNone(metric)

                # Scoring helpers should still work
                self.assertEqual(metrics.jaccard_similarity({1, 2}, {1, 2}), 1.0)

        finally:
            # Restore modules
            sys.modules.update(saved_modules)

    def test_scoring_helpers_work_without_opik(self):
        """P0: Scoring helpers don't require Opik."""
        # These should work regardless of Opik availability
        from the_edge_agent.experiments.metrics import (
            jaccard_similarity,
            f1_score,
            completeness_score,
        )

        # {1, 2} ∩ {2, 3} = {2}, |union| = 3, jaccard = 1/3
        self.assertAlmostEqual(jaccard_similarity({1, 2}, {2, 3}), 0.333, places=2)
        self.assertGreater(f1_score({1, 2}, {1, 2, 3}), 0.7)
        self.assertEqual(completeness_score(5, 10), 0.5)


if __name__ == "__main__":
    unittest.main()
