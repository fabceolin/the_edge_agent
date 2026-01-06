"""
Tests for Planning Actions (TEA-AGENT-001.3).

Tests cover:
- Plan data structure (Plan, Subtask classes)
- DAG validation and cycle detection
- Topological sorting
- JSON serialization/deserialization
- plan.decompose action with all strategies
- plan.execute action with sequential/parallel execution
- Failure handling strategies (replan, retry, skip, abort)
- plan.replan action
- plan.status action
"""

import copy
import json
import time
import unittest
from unittest.mock import MagicMock, patch
from typing import Any, Dict, List

from the_edge_agent.actions.planning_actions import (
    Plan,
    Subtask,
    SubtaskStatus,
    PlanningStrategy,
    FailureStrategy,
    register_actions,
)


class TestSubtask(unittest.TestCase):
    """Tests for Subtask class."""

    def test_subtask_creation(self):
        """Test basic subtask creation."""
        subtask = Subtask(id="test_1", description="Test subtask")
        self.assertEqual(subtask.id, "test_1")
        self.assertEqual(subtask.description, "Test subtask")
        self.assertEqual(subtask.dependencies, [])
        self.assertEqual(subtask.status, SubtaskStatus.PENDING)
        self.assertIsNone(subtask.result)

    def test_subtask_with_dependencies(self):
        """Test subtask with dependencies."""
        subtask = Subtask(
            id="test_2", description="Dependent subtask", dependencies=["test_1"]
        )
        self.assertEqual(subtask.dependencies, ["test_1"])

    def test_subtask_to_dict(self):
        """Test subtask serialization to dict."""
        subtask = Subtask(
            id="test_1",
            description="Test",
            dependencies=["dep_1"],
            status=SubtaskStatus.COMPLETED,
            result="Result value",
        )
        d = subtask.to_dict()
        self.assertEqual(d["id"], "test_1")
        self.assertEqual(d["status"], "completed")
        self.assertEqual(d["result"], "Result value")

    def test_subtask_from_dict(self):
        """Test subtask deserialization from dict."""
        data = {
            "id": "test_1",
            "description": "Test",
            "dependencies": ["dep_1"],
            "status": "in_progress",
            "result": None,
        }
        subtask = Subtask.from_dict(data)
        self.assertEqual(subtask.id, "test_1")
        self.assertEqual(subtask.status, SubtaskStatus.IN_PROGRESS)
        self.assertEqual(subtask.dependencies, ["dep_1"])


class TestPlan(unittest.TestCase):
    """Tests for Plan class."""

    def test_plan_creation(self):
        """Test basic plan creation."""
        plan = Plan(id="plan_1", goal="Test goal", strategy=PlanningStrategy.FLAT)
        self.assertEqual(plan.id, "plan_1")
        self.assertEqual(plan.goal, "Test goal")
        self.assertEqual(plan.strategy, PlanningStrategy.FLAT)
        self.assertEqual(plan.subtasks, [])
        self.assertIn("created_at", plan.metadata)

    def test_plan_with_subtasks(self):
        """Test plan with subtasks."""
        subtasks = [
            Subtask(id="st_1", description="First"),
            Subtask(id="st_2", description="Second", dependencies=["st_1"]),
        ]
        plan = Plan(
            id="plan_1",
            goal="Test",
            strategy=PlanningStrategy.HIERARCHICAL,
            subtasks=subtasks,
        )
        self.assertEqual(len(plan.subtasks), 2)

    def test_plan_to_dict(self):
        """Test plan serialization."""
        subtasks = [
            Subtask(id="st_1", description="First"),
            Subtask(id="st_2", description="Second", dependencies=["st_1"]),
        ]
        plan = Plan(
            id="plan_1",
            goal="Test goal",
            strategy=PlanningStrategy.FLAT,
            subtasks=subtasks,
        )
        d = plan.to_dict()
        self.assertEqual(d["id"], "plan_1")
        self.assertEqual(d["strategy"], "flat")
        self.assertEqual(len(d["subtasks"]), 2)

    def test_plan_from_dict(self):
        """Test plan deserialization."""
        data = {
            "id": "plan_1",
            "goal": "Test goal",
            "strategy": "hierarchical",
            "subtasks": [
                {"id": "st_1", "description": "First", "dependencies": []},
                {"id": "st_2", "description": "Second", "dependencies": ["st_1"]},
            ],
            "metadata": {"replan_count": 0},
        }
        plan = Plan.from_dict(data)
        self.assertEqual(plan.id, "plan_1")
        self.assertEqual(plan.strategy, PlanningStrategy.HIERARCHICAL)
        self.assertEqual(len(plan.subtasks), 2)


class TestDagValidation(unittest.TestCase):
    """Tests for DAG validation and cycle detection."""

    def test_valid_dag(self):
        """Test validation of valid DAG."""
        subtasks = [
            Subtask(id="a", description="A"),
            Subtask(id="b", description="B", dependencies=["a"]),
            Subtask(id="c", description="C", dependencies=["a"]),
            Subtask(id="d", description="D", dependencies=["b", "c"]),
        ]
        plan = Plan(id="p", goal="G", strategy=PlanningStrategy.FLAT, subtasks=subtasks)
        is_valid, error = plan.validate_dag()
        self.assertTrue(is_valid)
        self.assertIsNone(error)

    def test_invalid_dag_cycle(self):
        """Test detection of cycle in DAG."""
        subtasks = [
            Subtask(id="a", description="A", dependencies=["c"]),
            Subtask(id="b", description="B", dependencies=["a"]),
            Subtask(id="c", description="C", dependencies=["b"]),
        ]
        plan = Plan(id="p", goal="G", strategy=PlanningStrategy.FLAT, subtasks=subtasks)
        is_valid, error = plan.validate_dag()
        self.assertFalse(is_valid)
        self.assertIn("Cycle detected", error)

    def test_invalid_dag_self_reference(self):
        """Test detection of self-referencing subtask."""
        subtasks = [Subtask(id="a", description="A", dependencies=["a"])]
        plan = Plan(id="p", goal="G", strategy=PlanningStrategy.FLAT, subtasks=subtasks)
        is_valid, error = plan.validate_dag()
        self.assertFalse(is_valid)
        self.assertIn("Cycle detected", error)

    def test_invalid_dag_missing_dependency(self):
        """Test detection of missing dependency."""
        subtasks = [Subtask(id="a", description="A", dependencies=["nonexistent"])]
        plan = Plan(id="p", goal="G", strategy=PlanningStrategy.FLAT, subtasks=subtasks)
        is_valid, error = plan.validate_dag()
        self.assertFalse(is_valid)
        self.assertIn("unknown subtask", error)

    def test_empty_plan_valid(self):
        """Test that empty plan is valid."""
        plan = Plan(id="p", goal="G", strategy=PlanningStrategy.FLAT, subtasks=[])
        is_valid, error = plan.validate_dag()
        self.assertTrue(is_valid)


class TestTopologicalSort(unittest.TestCase):
    """Tests for topological sorting."""

    def test_topological_order_simple(self):
        """Test simple topological ordering."""
        subtasks = [
            Subtask(id="a", description="A"),
            Subtask(id="b", description="B", dependencies=["a"]),
            Subtask(id="c", description="C", dependencies=["b"]),
        ]
        plan = Plan(id="p", goal="G", strategy=PlanningStrategy.FLAT, subtasks=subtasks)
        order = plan.get_topological_order()
        # a must come before b, b must come before c
        self.assertEqual(order.index("a") < order.index("b"), True)
        self.assertEqual(order.index("b") < order.index("c"), True)

    def test_topological_order_parallel(self):
        """Test topological order with parallel subtasks."""
        subtasks = [
            Subtask(id="start", description="Start"),
            Subtask(id="a", description="A", dependencies=["start"]),
            Subtask(id="b", description="B", dependencies=["start"]),
            Subtask(id="end", description="End", dependencies=["a", "b"]),
        ]
        plan = Plan(id="p", goal="G", strategy=PlanningStrategy.FLAT, subtasks=subtasks)
        order = plan.get_topological_order()
        # start must be first
        self.assertEqual(order[0], "start")
        # end must be last
        self.assertEqual(order[-1], "end")
        # a and b must be in the middle
        self.assertIn("a", order[1:3])
        self.assertIn("b", order[1:3])

    def test_topological_order_invalid_dag(self):
        """Test that topological sort raises on invalid DAG."""
        subtasks = [
            Subtask(id="a", description="A", dependencies=["b"]),
            Subtask(id="b", description="B", dependencies=["a"]),
        ]
        plan = Plan(id="p", goal="G", strategy=PlanningStrategy.FLAT, subtasks=subtasks)
        with self.assertRaises(ValueError):
            plan.get_topological_order()


class TestGetReadySubtasks(unittest.TestCase):
    """Tests for get_ready_subtasks method."""

    def test_initial_ready(self):
        """Test getting initial ready subtasks."""
        subtasks = [
            Subtask(id="a", description="A"),
            Subtask(id="b", description="B", dependencies=["a"]),
        ]
        plan = Plan(id="p", goal="G", strategy=PlanningStrategy.FLAT, subtasks=subtasks)
        ready = plan.get_ready_subtasks()
        self.assertEqual(len(ready), 1)
        self.assertEqual(ready[0].id, "a")

    def test_after_completion(self):
        """Test ready subtasks after completing dependencies."""
        subtasks = [
            Subtask(id="a", description="A", status=SubtaskStatus.COMPLETED),
            Subtask(id="b", description="B", dependencies=["a"]),
        ]
        plan = Plan(id="p", goal="G", strategy=PlanningStrategy.FLAT, subtasks=subtasks)
        ready = plan.get_ready_subtasks()
        self.assertEqual(len(ready), 1)
        self.assertEqual(ready[0].id, "b")

    def test_multiple_ready(self):
        """Test multiple ready subtasks (parallel execution)."""
        subtasks = [
            Subtask(id="start", description="Start", status=SubtaskStatus.COMPLETED),
            Subtask(id="a", description="A", dependencies=["start"]),
            Subtask(id="b", description="B", dependencies=["start"]),
            Subtask(id="c", description="C", dependencies=["start"]),
        ]
        plan = Plan(id="p", goal="G", strategy=PlanningStrategy.FLAT, subtasks=subtasks)
        ready = plan.get_ready_subtasks()
        self.assertEqual(len(ready), 3)


class TestStatusCounts(unittest.TestCase):
    """Tests for get_status_counts method."""

    def test_all_pending(self):
        """Test status counts with all pending subtasks."""
        subtasks = [
            Subtask(id="a", description="A"),
            Subtask(id="b", description="B"),
            Subtask(id="c", description="C"),
        ]
        plan = Plan(id="p", goal="G", strategy=PlanningStrategy.FLAT, subtasks=subtasks)
        counts = plan.get_status_counts()
        self.assertEqual(counts["pending"], 3)
        self.assertEqual(counts["completed"], 0)
        self.assertEqual(counts["total"], 3)

    def test_mixed_status(self):
        """Test status counts with mixed statuses."""
        subtasks = [
            Subtask(id="a", description="A", status=SubtaskStatus.COMPLETED),
            Subtask(id="b", description="B", status=SubtaskStatus.IN_PROGRESS),
            Subtask(id="c", description="C", status=SubtaskStatus.FAILED),
            Subtask(id="d", description="D", status=SubtaskStatus.SKIPPED),
            Subtask(id="e", description="E"),
        ]
        plan = Plan(id="p", goal="G", strategy=PlanningStrategy.FLAT, subtasks=subtasks)
        counts = plan.get_status_counts()
        self.assertEqual(counts["completed"], 1)
        self.assertEqual(counts["in_progress"], 1)
        self.assertEqual(counts["failed"], 1)
        self.assertEqual(counts["skipped"], 1)
        self.assertEqual(counts["pending"], 1)
        self.assertEqual(counts["total"], 5)


class TestPlanDecomposeAction(unittest.TestCase):
    """Tests for plan.decompose action."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.engine = MagicMock()

        # Create mock llm.call
        def mock_llm_call(state, model, messages, temperature=0.7, **kwargs):
            # Return a simple flat plan response
            return {
                "content": json.dumps(
                    {
                        "subtasks": [
                            {
                                "id": "research",
                                "description": "Research the topic",
                                "dependencies": [],
                            },
                            {
                                "id": "outline",
                                "description": "Create outline",
                                "dependencies": ["research"],
                            },
                            {
                                "id": "write",
                                "description": "Write content",
                                "dependencies": ["outline"],
                            },
                        ]
                    }
                )
            }

        self.registry["llm.call"] = mock_llm_call
        register_actions(self.registry, self.engine)

    def test_decompose_flat_strategy(self):
        """Test plan.decompose with flat strategy."""
        result = self.registry["plan.decompose"](
            state={}, goal="Write a blog post", strategy="flat"
        )
        self.assertTrue(result["success"])
        self.assertIn("plan", result)
        self.assertEqual(len(result["plan"]["subtasks"]), 3)

    def test_decompose_invalid_strategy(self):
        """Test plan.decompose with invalid strategy."""
        result = self.registry["plan.decompose"](
            state={}, goal="Test", strategy="invalid"
        )
        self.assertFalse(result["success"])
        self.assertIn("Unknown strategy", result["error"])

    def test_decompose_validates_dag(self):
        """Test that decompose validates DAG structure."""

        # Mock response with cycle
        def mock_llm_cycle(state, model, messages, temperature=0.7, **kwargs):
            return {
                "content": json.dumps(
                    {
                        "subtasks": [
                            {"id": "a", "description": "A", "dependencies": ["c"]},
                            {"id": "b", "description": "B", "dependencies": ["a"]},
                            {"id": "c", "description": "C", "dependencies": ["b"]},
                        ]
                    }
                )
            }

        self.registry["llm.call"] = mock_llm_cycle
        register_actions(self.registry, self.engine)

        result = self.registry["plan.decompose"](state={}, goal="Test", strategy="flat")
        self.assertFalse(result["success"])
        self.assertIn("Cycle detected", result["error"])

    def test_decompose_enforces_max_subtasks(self):
        """Test that decompose enforces max_subtasks limit."""

        def mock_llm_many(state, model, messages, temperature=0.7, **kwargs):
            subtasks = [
                {"id": f"st_{i}", "description": f"Task {i}", "dependencies": []}
                for i in range(20)
            ]
            return {"content": json.dumps({"subtasks": subtasks})}

        self.registry["llm.call"] = mock_llm_many
        register_actions(self.registry, self.engine)

        result = self.registry["plan.decompose"](
            state={}, goal="Test", strategy="flat", max_subtasks=5
        )
        self.assertTrue(result["success"])
        self.assertEqual(len(result["plan"]["subtasks"]), 5)


class TestPlanExecuteAction(unittest.TestCase):
    """Tests for plan.execute action."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.engine = MagicMock()

        self.call_count = 0

        def mock_llm_call(state, model=None, messages=None, **kwargs):
            self.call_count += 1
            return {"content": f"Result {self.call_count}"}

        self.registry["llm.call"] = mock_llm_call
        register_actions(self.registry, self.engine)

    def test_execute_sequential(self):
        """Test sequential plan execution."""
        plan = {
            "id": "plan_1",
            "goal": "Test",
            "strategy": "flat",
            "subtasks": [
                {
                    "id": "st_1",
                    "description": "First",
                    "dependencies": [],
                    "status": "pending",
                },
                {
                    "id": "st_2",
                    "description": "Second",
                    "dependencies": ["st_1"],
                    "status": "pending",
                },
            ],
            "metadata": {},
        }

        result = self.registry["plan.execute"](state={"plan": plan}, parallel=False)

        self.assertTrue(result["success"])
        self.assertEqual(result["plan_status"]["completed"], 2)
        self.assertIn("st_1", result["subtask_results"])
        self.assertIn("st_2", result["subtask_results"])

    def test_execute_skips_completed(self):
        """Test that execute skips already completed subtasks."""
        plan = {
            "id": "plan_1",
            "goal": "Test",
            "strategy": "flat",
            "subtasks": [
                {
                    "id": "st_1",
                    "description": "First",
                    "dependencies": [],
                    "status": "completed",
                    "result": "Done",
                },
                {
                    "id": "st_2",
                    "description": "Second",
                    "dependencies": ["st_1"],
                    "status": "pending",
                },
            ],
            "metadata": {},
        }

        self.call_count = 0
        result = self.registry["plan.execute"](state={"plan": plan}, parallel=False)

        self.assertTrue(result["success"])
        # Only st_2 should have been executed
        self.assertEqual(self.call_count, 1)

    def test_execute_no_plan_error(self):
        """Test execute fails without plan."""
        result = self.registry["plan.execute"](state={})
        self.assertFalse(result["success"])
        self.assertIn("No plan provided", result["error"])

    def test_execute_abort_on_failure(self):
        """Test abort failure strategy."""

        def mock_llm_fail(state, **kwargs):
            return {"error": "LLM error"}

        self.registry["llm.call"] = mock_llm_fail
        register_actions(self.registry, self.engine)

        plan = {
            "id": "plan_1",
            "goal": "Test",
            "strategy": "flat",
            "subtasks": [
                {
                    "id": "st_1",
                    "description": "First",
                    "dependencies": [],
                    "status": "pending",
                }
            ],
            "metadata": {},
        }

        result = self.registry["plan.execute"](
            state={"plan": plan}, on_subtask_failure="abort"
        )

        self.assertFalse(result["success"])
        self.assertIn("error", result)


class TestPlanReplanAction(unittest.TestCase):
    """Tests for plan.replan action."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.engine = MagicMock()

        def mock_llm_call(state, model, messages, temperature=0.7, **kwargs):
            return {
                "content": json.dumps(
                    {
                        "subtasks": [
                            {
                                "id": "retry_1",
                                "description": "Retry with different approach",
                                "dependencies": [],
                            },
                            {
                                "id": "continue_1",
                                "description": "Continue work",
                                "dependencies": ["retry_1"],
                            },
                        ],
                        "adjustment_rationale": "Changed approach due to failure",
                    }
                )
            }

        self.registry["llm.call"] = mock_llm_call
        register_actions(self.registry, self.engine)

    def test_replan_preserves_completed(self):
        """Test that replan preserves completed subtasks."""
        plan = {
            "id": "plan_1",
            "goal": "Test goal",
            "strategy": "flat",
            "subtasks": [
                {
                    "id": "done_1",
                    "description": "Done",
                    "dependencies": [],
                    "status": "completed",
                    "result": "OK",
                },
                {
                    "id": "failed_1",
                    "description": "Failed",
                    "dependencies": ["done_1"],
                    "status": "failed",
                },
            ],
            "metadata": {"replan_count": 0},
        }

        result = self.registry["plan.replan"](
            state={
                "plan": plan,
                "subtask_results": {"done_1": "OK"},
                "failed_subtask": "failed_1",
                "failure_reason": "API error",
            }
        )

        self.assertTrue(result["success"])
        self.assertEqual(result["preserved_subtasks"], 1)
        self.assertEqual(result["new_subtasks"], 2)
        # Check completed subtask is still in the plan
        subtask_ids = [st["id"] for st in result["plan"]["subtasks"]]
        self.assertIn("done_1", subtask_ids)

    def test_replan_no_plan_error(self):
        """Test replan fails without plan."""
        result = self.registry["plan.replan"](state={})
        self.assertFalse(result["success"])
        self.assertIn("No plan to replan", result["error"])


class TestPlanStatusAction(unittest.TestCase):
    """Tests for plan.status action."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.engine = MagicMock()
        register_actions(self.registry, self.engine)

    def test_status_basic(self):
        """Test basic status retrieval."""
        plan = {
            "id": "plan_1",
            "goal": "Test",
            "strategy": "flat",
            "subtasks": [
                {
                    "id": "st_1",
                    "description": "A",
                    "dependencies": [],
                    "status": "completed",
                },
                {
                    "id": "st_2",
                    "description": "B",
                    "dependencies": [],
                    "status": "pending",
                },
                {
                    "id": "st_3",
                    "description": "C",
                    "dependencies": [],
                    "status": "pending",
                },
            ],
            "metadata": {},
        }

        result = self.registry["plan.status"](state={"plan": plan})

        self.assertTrue(result["success"])
        self.assertEqual(result["status"]["completed"], 1)
        self.assertEqual(result["status"]["pending"], 2)
        self.assertEqual(result["status"]["total"], 3)
        self.assertAlmostEqual(result["progress"], 1 / 3, places=2)

    def test_status_include_details(self):
        """Test status with subtask details."""
        plan = {
            "id": "plan_1",
            "goal": "Test",
            "strategy": "flat",
            "subtasks": [
                {
                    "id": "st_1",
                    "description": "A",
                    "dependencies": [],
                    "status": "completed",
                },
                {
                    "id": "st_2",
                    "description": "B",
                    "dependencies": [],
                    "status": "pending",
                },
            ],
            "metadata": {},
        }

        result = self.registry["plan.status"](
            state={"plan": plan}, include_details=True
        )

        self.assertTrue(result["success"])
        self.assertIn("subtasks", result)
        self.assertEqual(len(result["subtasks"]), 2)

    def test_status_exclude_completed(self):
        """Test status excluding completed subtasks."""
        plan = {
            "id": "plan_1",
            "goal": "Test",
            "strategy": "flat",
            "subtasks": [
                {
                    "id": "st_1",
                    "description": "A",
                    "dependencies": [],
                    "status": "completed",
                },
                {
                    "id": "st_2",
                    "description": "B",
                    "dependencies": [],
                    "status": "pending",
                },
            ],
            "metadata": {},
        }

        result = self.registry["plan.status"](
            state={"plan": plan}, include_details=True, include_completed=False
        )

        self.assertTrue(result["success"])
        self.assertEqual(len(result["subtasks"]), 1)
        self.assertEqual(result["subtasks"][0]["id"], "st_2")

    def test_status_no_plan_error(self):
        """Test status fails without plan."""
        result = self.registry["plan.status"](state={})
        self.assertFalse(result["success"])


class TestFailureHandlingStrategies(unittest.TestCase):
    """Tests for failure handling strategies."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.engine = MagicMock()

        self.fail_count = 0
        self.max_fails = 2

        def mock_llm_fail_then_succeed(state, **kwargs):
            self.fail_count += 1
            if self.fail_count <= self.max_fails:
                return {"error": f"Fail {self.fail_count}"}
            return {"content": "Success after retry"}

        self.registry["llm.call"] = mock_llm_fail_then_succeed
        register_actions(self.registry, self.engine)

    def test_skip_strategy(self):
        """Test skip failure strategy."""

        def mock_fail(state, **kwargs):
            return {"error": "Always fail"}

        self.registry["llm.call"] = mock_fail
        register_actions(self.registry, self.engine)

        plan = {
            "id": "plan_1",
            "goal": "Test",
            "strategy": "flat",
            "subtasks": [
                {
                    "id": "st_1",
                    "description": "Will fail",
                    "dependencies": [],
                    "status": "pending",
                },
                {
                    "id": "st_2",
                    "description": "Should run",
                    "dependencies": [],
                    "status": "pending",
                },
            ],
            "metadata": {},
        }

        result = self.registry["plan.execute"](
            state={"plan": plan}, on_subtask_failure="skip"
        )

        # With skip, both subtasks should be processed
        self.assertEqual(result["plan_status"]["skipped"], 2)

    def test_retry_strategy_success(self):
        """Test retry strategy succeeds after retries."""
        self.fail_count = 0
        self.max_fails = 1

        plan = {
            "id": "plan_1",
            "goal": "Test",
            "strategy": "flat",
            "subtasks": [
                {
                    "id": "st_1",
                    "description": "Flaky",
                    "dependencies": [],
                    "status": "pending",
                }
            ],
            "metadata": {},
        }

        result = self.registry["plan.execute"](
            state={"plan": plan},
            on_subtask_failure="retry",
            max_retries=3,
            retry_delay=0.01,  # Fast for testing
        )

        self.assertTrue(result["success"])
        self.assertEqual(result["plan_status"]["completed"], 1)

    def test_retry_strategy_exceeds_max(self):
        """Test retry strategy fails after max retries."""

        def always_fail(state, **kwargs):
            return {"error": "Always fail"}

        self.registry["llm.call"] = always_fail
        register_actions(self.registry, self.engine)

        plan = {
            "id": "plan_1",
            "goal": "Test",
            "strategy": "flat",
            "subtasks": [
                {
                    "id": "st_1",
                    "description": "Always fails",
                    "dependencies": [],
                    "status": "pending",
                }
            ],
            "metadata": {},
        }

        result = self.registry["plan.execute"](
            state={"plan": plan},
            on_subtask_failure="retry",
            max_retries=2,
            retry_delay=0.01,
        )

        self.assertFalse(result["success"])
        self.assertIn("Max retries", result["error"])


class TestJSONSerialization(unittest.TestCase):
    """Tests for JSON serialization round-trips."""

    def test_plan_serialization_roundtrip(self):
        """Test plan can be serialized and deserialized."""
        original = Plan(
            id="plan_test",
            goal="Test serialization",
            strategy=PlanningStrategy.HIERARCHICAL,
            subtasks=[
                Subtask(
                    id="a",
                    description="A",
                    status=SubtaskStatus.COMPLETED,
                    result="Done",
                ),
                Subtask(
                    id="b",
                    description="B",
                    dependencies=["a"],
                    status=SubtaskStatus.PENDING,
                ),
            ],
            metadata={"replan_count": 1, "custom": "value"},
        )

        # Serialize to JSON string
        json_str = json.dumps(original.to_dict())

        # Deserialize back
        data = json.loads(json_str)
        restored = Plan.from_dict(data)

        self.assertEqual(restored.id, original.id)
        self.assertEqual(restored.goal, original.goal)
        self.assertEqual(restored.strategy, original.strategy)
        self.assertEqual(len(restored.subtasks), len(original.subtasks))
        self.assertEqual(restored.subtasks[0].status, SubtaskStatus.COMPLETED)
        self.assertEqual(restored.metadata["custom"], "value")


class TestCheckpointIntegration(unittest.TestCase):
    """Tests for checkpoint/resume functionality."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.engine = MagicMock()

        self.execution_count = 0

        def mock_llm_call(state, **kwargs):
            self.execution_count += 1
            return {"content": f"Executed {self.execution_count}"}

        self.registry["llm.call"] = mock_llm_call
        register_actions(self.registry, self.engine)

    def test_resume_from_partial_execution(self):
        """Test resuming execution from partially completed plan."""
        # Simulate a plan that was interrupted
        plan = {
            "id": "plan_1",
            "goal": "Test resume",
            "strategy": "flat",
            "subtasks": [
                {
                    "id": "st_1",
                    "description": "Done",
                    "dependencies": [],
                    "status": "completed",
                    "result": "R1",
                },
                {
                    "id": "st_2",
                    "description": "Done",
                    "dependencies": ["st_1"],
                    "status": "completed",
                    "result": "R2",
                },
                {
                    "id": "st_3",
                    "description": "Not started",
                    "dependencies": ["st_2"],
                    "status": "pending",
                },
                {
                    "id": "st_4",
                    "description": "Not started",
                    "dependencies": ["st_3"],
                    "status": "pending",
                },
            ],
            "metadata": {},
        }

        self.execution_count = 0
        result = self.registry["plan.execute"](
            state={"plan": plan, "subtask_results": {"st_1": "R1", "st_2": "R2"}},
            parallel=False,
        )

        self.assertTrue(result["success"])
        # Only st_3 and st_4 should be executed
        self.assertEqual(self.execution_count, 2)
        self.assertIn("st_3", result["subtask_results"])
        self.assertIn("st_4", result["subtask_results"])


if __name__ == "__main__":
    unittest.main()
