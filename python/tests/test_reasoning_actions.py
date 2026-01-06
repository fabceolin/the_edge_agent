"""
Unit and Integration Tests for Reasoning Actions (TEA-AGENT-001.4).

These tests cover:
- reason.cot: Chain-of-Thought reasoning
- reason.react: ReAct loop with tool integration
- reason.self_correct: Generate-critique-improve cycle
- reason.decompose: Problem decomposition and synthesis

Tests use mocked LLM responses for deterministic testing.
"""

import json
import time
import unittest
from unittest.mock import MagicMock, patch, call
from typing import Any, Dict, List

import sys
import os

# Add parent directory to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

from the_edge_agent.actions.reasoning_actions import register_actions


class MockEngine:
    """Mock YAMLEngine for testing."""

    def __init__(self):
        self.actions_registry = {}


def create_mock_llm_response(content: str, usage: Dict = None) -> Dict[str, Any]:
    """Create a mock LLM response."""
    return {
        "content": content,
        "usage": usage or {"prompt_tokens": 100, "completion_tokens": 50},
    }


def create_mock_llm_error(error: str) -> Dict[str, Any]:
    """Create a mock LLM error response."""
    return {"error": error, "success": False}


def setup_registry_with_mock_llm(mock_llm_call: MagicMock) -> Dict[str, Any]:
    """Set up registry with mock LLM already in place."""
    engine = MockEngine()
    registry = {}
    # Add mock llm.call BEFORE registering reasoning actions
    registry["llm.call"] = mock_llm_call
    register_actions(registry, engine)
    return registry


class TestReasonCot(unittest.TestCase):
    """Tests for reason.cot action."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_llm_call = MagicMock()
        self.registry = setup_registry_with_mock_llm(self.mock_llm_call)

    def test_cot_basic_success(self):
        """Test basic CoT reasoning with valid JSON response."""
        # Setup mock response
        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps(
                {
                    "thinking": "Step 1: Identify components. Step 2: Calculate result.",
                    "answer": "42",
                }
            )
        )

        # Execute
        result = self.registry["reason.cot"](
            state={}, problem="What is the meaning of life?", model="gpt-4"
        )

        # Verify
        self.assertIn("thinking", result)
        self.assertIn("answer", result)
        self.assertEqual(
            result["thinking"], "Step 1: Identify components. Step 2: Calculate result."
        )
        self.assertEqual(result["answer"], "42")
        self.assertIn("reasoning_trace", result)
        self.assertEqual(result["model"], "gpt-4")
        self.assertEqual(result["thinking_format"], "step_by_step")

    def test_cot_different_thinking_formats(self):
        """Test CoT with different thinking formats."""
        formats = ["step_by_step", "pros_cons", "tree", "first_principles"]

        for fmt in formats:
            self.mock_llm_call.return_value = create_mock_llm_response(
                json.dumps({"thinking": f"Using {fmt}", "answer": "result"})
            )

            result = self.registry["reason.cot"](
                state={}, problem="Test problem", model="gpt-4", thinking_format=fmt
            )

            self.assertEqual(result["thinking_format"], fmt)
            self.assertNotIn("error", result)

    def test_cot_with_few_shot_examples(self):
        """Test CoT with few-shot examples."""
        examples = [
            {
                "problem": "What is 2+2?",
                "thinking": "Adding two numbers together",
                "answer": "4",
            },
            {
                "problem": "What is 3*3?",
                "thinking": "Multiplying three by three",
                "answer": "9",
            },
        ]

        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps({"thinking": "Following the examples", "answer": "16"})
        )

        result = self.registry["reason.cot"](
            state={}, problem="What is 4*4?", model="gpt-4", few_shot_examples=examples
        )

        self.assertNotIn("error", result)
        self.assertEqual(result["answer"], "16")

        # Verify examples were included in the prompt
        call_args = self.mock_llm_call.call_args
        messages = call_args.kwargs.get("messages") or call_args[1].get("messages")
        system_content = messages[0]["content"]
        self.assertIn("Example 1:", system_content)
        self.assertIn("What is 2+2?", system_content)

    def test_cot_llm_error(self):
        """Test CoT handles LLM errors gracefully."""
        self.mock_llm_call.return_value = create_mock_llm_error(
            "API rate limit exceeded"
        )

        result = self.registry["reason.cot"](
            state={}, problem="Test problem", model="gpt-4"
        )

        self.assertIn("error", result)
        self.assertEqual(result["success"], False)
        self.assertIn("reasoning_trace", result)

    def test_cot_malformed_json_response(self):
        """Test CoT handles malformed JSON from LLM."""
        # Return invalid JSON
        self.mock_llm_call.return_value = create_mock_llm_response(
            "This is not valid JSON"
        )

        result = self.registry["reason.cot"](
            state={}, problem="Test problem", model="gpt-4"
        )

        self.assertIn("error", result)
        self.assertEqual(result["success"], False)

    def test_cot_json_in_markdown_block(self):
        """Test CoT extracts JSON from markdown code blocks."""
        self.mock_llm_call.return_value = create_mock_llm_response(
            '```json\n{"thinking": "Step by step", "answer": "result"}\n```'
        )

        result = self.registry["reason.cot"](
            state={}, problem="Test problem", model="gpt-4"
        )

        self.assertNotIn("error", result)
        self.assertEqual(result["thinking"], "Step by step")
        self.assertEqual(result["answer"], "result")

    def test_cot_trace_format(self):
        """Test CoT reasoning trace format is Opik-compatible."""
        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps({"thinking": "reasoning", "answer": "answer"})
        )

        result = self.registry["reason.cot"](state={}, problem="Test", model="gpt-4")

        trace = result["reasoning_trace"]
        self.assertIsInstance(trace, list)
        self.assertGreater(len(trace), 0)

        # Check trace entries have required fields
        for entry in trace:
            self.assertIn("step", entry)
            self.assertIn("timestamp", entry)

    def test_cot_state_variables(self):
        """Test CoT sets expected state variables."""
        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps({"thinking": "my thinking", "answer": "my answer"})
        )

        result = self.registry["reason.cot"](state={}, problem="Test", model="gpt-4")

        # Check state variable aliases
        self.assertEqual(result["reasoning_thinking"], "my thinking")
        self.assertEqual(result["reasoning_answer"], "my answer")


class TestReasonReact(unittest.TestCase):
    """Tests for reason.react action."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_llm_call = MagicMock()
        self.registry = setup_registry_with_mock_llm(self.mock_llm_call)

        # Add mock tool
        self.mock_tool = MagicMock(return_value={"result": "tool output"})
        self.mock_tool.__doc__ = "A test tool for searching"
        self.registry["test.search"] = self.mock_tool

    def test_react_basic_final_answer(self):
        """Test ReAct returns final answer immediately."""
        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps(
                {
                    "thought": "I already know the answer",
                    "action": "final_answer",
                    "action_input": {"answer": "Paris is the capital of France"},
                }
            )
        )

        result = self.registry["reason.react"](
            state={}, goal="What is the capital of France?", model="gpt-4"
        )

        self.assertNotIn("error", result)
        self.assertEqual(result["final_answer"], "Paris is the capital of France")
        self.assertEqual(result["total_steps"], 1)
        self.assertIn("steps", result)
        self.assertIn("reasoning_trace", result)

    def test_react_tool_execution(self):
        """Test ReAct executes tools correctly."""
        # First call: use tool
        # Second call: final answer
        self.mock_llm_call.side_effect = [
            create_mock_llm_response(
                json.dumps(
                    {
                        "thought": "I need to search for this",
                        "action": "test.search",
                        "action_input": {"query": "weather paris"},
                    }
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {
                        "thought": "Now I have the information",
                        "action": "final_answer",
                        "action_input": {"answer": "It's sunny in Paris"},
                    }
                )
            ),
        ]

        result = self.registry["reason.react"](
            state={},
            goal="What is the weather in Paris?",
            model="gpt-4",
            tools=["test.search"],
        )

        self.assertNotIn("error", result)
        self.assertEqual(result["total_steps"], 2)
        self.mock_tool.assert_called_once()
        self.assertEqual(result["final_answer"], "It's sunny in Paris")

    def test_react_max_steps_limit(self):
        """Test ReAct respects max_steps limit (RISK-002: Infinite loop prevention)."""
        # Always return tool call, never final answer
        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps(
                {
                    "thought": "Keep searching",
                    "action": "test.search",
                    "action_input": {"query": "more info"},
                }
            )
        )

        result = self.registry["reason.react"](
            state={},
            goal="Find everything",
            model="gpt-4",
            tools=["test.search"],
            max_steps=3,
        )

        self.assertEqual(result["total_steps"], 3)
        self.assertIn("warning", result)
        self.assertIsNone(result["final_answer"])
        self.assertEqual(len(result["steps"]), 3)

    def test_react_unknown_tool(self):
        """Test ReAct handles unknown tool gracefully."""
        self.mock_llm_call.side_effect = [
            create_mock_llm_response(
                json.dumps(
                    {
                        "thought": "Using unknown tool",
                        "action": "unknown.tool",
                        "action_input": {"query": "test"},
                    }
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {
                        "thought": "That didn't work, giving up",
                        "action": "final_answer",
                        "action_input": {"answer": "Unable to find"},
                    }
                )
            ),
        ]

        result = self.registry["reason.react"](
            state={}, goal="Test goal", model="gpt-4", tools=["test.search"]
        )

        self.assertNotIn("error", result)
        # First step should have observation about unknown action
        first_step = result["steps"][0]
        self.assertIn("Unknown action", first_step["observation"])

    def test_react_tool_error(self):
        """Test ReAct handles tool execution errors."""
        self.mock_tool.side_effect = Exception("Tool crashed")

        self.mock_llm_call.side_effect = [
            create_mock_llm_response(
                json.dumps(
                    {
                        "thought": "Using tool",
                        "action": "test.search",
                        "action_input": {"query": "test"},
                    }
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {
                        "thought": "Tool failed, answering anyway",
                        "action": "final_answer",
                        "action_input": {"answer": "Partial answer"},
                    }
                )
            ),
        ]

        result = self.registry["reason.react"](
            state={}, goal="Test goal", model="gpt-4", tools=["test.search"]
        )

        self.assertNotIn("error", result)
        # First step should have error in observation
        first_step = result["steps"][0]
        self.assertIn("Error executing", first_step["observation"])

    def test_react_llm_error(self):
        """Test ReAct handles LLM errors gracefully."""
        self.mock_llm_call.return_value = create_mock_llm_error("API error")

        result = self.registry["reason.react"](
            state={}, goal="Test goal", model="gpt-4"
        )

        self.assertIn("error", result)
        self.assertEqual(result["success"], False)

    def test_react_steps_trace(self):
        """Test ReAct steps format matches expected trace structure."""
        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps(
                {
                    "thought": "Final thought",
                    "action": "final_answer",
                    "action_input": {"answer": "Done"},
                }
            )
        )

        result = self.registry["reason.react"](state={}, goal="Test", model="gpt-4")

        steps = result["steps"]
        self.assertEqual(len(steps), 1)
        step = steps[0]
        self.assertIn("step", step)
        self.assertIn("thought", step)
        self.assertIn("action", step)
        self.assertIn("action_input", step)
        self.assertIn("observation", step)

    def test_react_state_variables(self):
        """Test ReAct sets expected state variables."""
        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps(
                {
                    "thought": "Done",
                    "action": "final_answer",
                    "action_input": {"answer": "The answer"},
                }
            )
        )

        result = self.registry["reason.react"](state={}, goal="Test", model="gpt-4")

        self.assertEqual(result["react_steps"], result["steps"])
        self.assertEqual(result["reasoning_answer"], "The answer")


class TestReasonSelfCorrect(unittest.TestCase):
    """Tests for reason.self_correct action."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_llm_call = MagicMock()
        self.registry = setup_registry_with_mock_llm(self.mock_llm_call)

    def test_self_correct_basic(self):
        """Test basic self-correction cycle."""
        self.mock_llm_call.side_effect = [
            # Initial generation
            create_mock_llm_response(
                json.dumps(
                    {
                        "output": "Initial draft",
                        "confidence": 0.6,
                        "potential_issues": ["May need refinement"],
                    }
                )
            ),
            # First critique
            create_mock_llm_response(
                json.dumps(
                    {
                        "issues": ["Not detailed enough"],
                        "suggestions": ["Add more details"],
                        "improved_output": "Improved draft with details",
                        "improvement_score": 0.8,
                    }
                )
            ),
            # Second critique
            create_mock_llm_response(
                json.dumps(
                    {
                        "issues": [],
                        "suggestions": [],
                        "improved_output": "Final polished draft",
                        "improvement_score": 0.95,
                    }
                )
            ),
        ]

        result = self.registry["reason.self_correct"](
            state={}, task="Write a summary", model="gpt-4", improvement_rounds=2
        )

        self.assertNotIn("error", result)
        self.assertEqual(result["output"], "Final polished draft")
        self.assertEqual(result["rounds_completed"], 2)
        self.assertIn("improvement_history", result)
        self.assertEqual(
            len(result["improvement_history"]), 3
        )  # Initial + 2 improvements

    def test_self_correct_different_models(self):
        """Test self-correction with different generator and critic models."""
        self.mock_llm_call.side_effect = [
            create_mock_llm_response(
                json.dumps(
                    {
                        "output": "Generated text",
                        "confidence": 0.7,
                        "potential_issues": [],
                    }
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {
                        "issues": ["Minor issue"],
                        "suggestions": ["Small fix"],
                        "improved_output": "Improved text",
                        "improvement_score": 0.9,
                    }
                )
            ),
        ]

        result = self.registry["reason.self_correct"](
            state={},
            task="Generate content",
            generator_model="gpt-4",
            critic_model="gpt-3.5-turbo",
            improvement_rounds=1,
        )

        self.assertNotIn("error", result)
        self.assertEqual(result["generator_model"], "gpt-4")
        self.assertEqual(result["critic_model"], "gpt-3.5-turbo")

    def test_self_correct_custom_critic_prompt(self):
        """Test self-correction with custom critic prompt."""
        self.mock_llm_call.side_effect = [
            create_mock_llm_response(
                json.dumps(
                    {
                        "output": "Code snippet",
                        "confidence": 0.7,
                        "potential_issues": [],
                    }
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {
                        "issues": ["Security vulnerability"],
                        "suggestions": ["Add input validation"],
                        "improved_output": "Secure code",
                        "improvement_score": 0.95,
                    }
                )
            ),
        ]

        result = self.registry["reason.self_correct"](
            state={},
            task="Write a function",
            model="gpt-4",
            improvement_rounds=1,
            critic_prompt="Focus on security issues and best practices",
        )

        self.assertNotIn("error", result)
        # Verify custom critic prompt was used
        calls = self.mock_llm_call.call_args_list
        critic_call = calls[1]
        messages = critic_call.kwargs.get("messages") or critic_call[1].get("messages")
        system_content = messages[0]["content"]
        self.assertIn("Focus on security", system_content)

    def test_self_correct_improvement_history(self):
        """Test improvement history tracking."""
        self.mock_llm_call.side_effect = [
            create_mock_llm_response(
                json.dumps(
                    {"output": "V1", "confidence": 0.5, "potential_issues": ["Issue A"]}
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {
                        "issues": ["Issue B"],
                        "suggestions": ["Fix B"],
                        "improved_output": "V2",
                        "improvement_score": 0.7,
                    }
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {
                        "issues": [],
                        "suggestions": [],
                        "improved_output": "V3",
                        "improvement_score": 0.9,
                    }
                )
            ),
        ]

        result = self.registry["reason.self_correct"](
            state={}, task="Improve this", model="gpt-4", improvement_rounds=2
        )

        history = result["improvement_history"]
        self.assertEqual(len(history), 3)
        self.assertEqual(history[0]["type"], "initial")
        self.assertEqual(history[0]["output"], "V1")
        self.assertEqual(history[1]["type"], "improvement")
        self.assertEqual(history[1]["output"], "V2")
        self.assertEqual(history[2]["output"], "V3")

    def test_self_correct_llm_error(self):
        """Test self-correction handles LLM errors."""
        self.mock_llm_call.return_value = create_mock_llm_error("API error")

        result = self.registry["reason.self_correct"](
            state={}, task="Test task", model="gpt-4"
        )

        self.assertIn("error", result)
        self.assertEqual(result["success"], False)


class TestReasonDecompose(unittest.TestCase):
    """Tests for reason.decompose action."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_llm_call = MagicMock()
        self.registry = setup_registry_with_mock_llm(self.mock_llm_call)

    def test_decompose_basic(self):
        """Test basic problem decomposition."""
        self.mock_llm_call.side_effect = [
            # Decomposition
            create_mock_llm_response(
                json.dumps(
                    {
                        "analysis": "This is a multi-part problem",
                        "sub_problems": [
                            {"id": 1, "description": "First part", "dependencies": []},
                            {
                                "id": 2,
                                "description": "Second part",
                                "dependencies": [1],
                            },
                        ],
                        "synthesis_strategy": "Combine results",
                    }
                )
            ),
            # Solve sub-problem 1
            create_mock_llm_response(
                json.dumps(
                    {
                        "reasoning": "Solving first part",
                        "answer": "Answer 1",
                        "confidence": 0.9,
                    }
                )
            ),
            # Solve sub-problem 2
            create_mock_llm_response(
                json.dumps(
                    {
                        "reasoning": "Solving second part using first",
                        "answer": "Answer 2",
                        "confidence": 0.85,
                    }
                )
            ),
            # Synthesis
            create_mock_llm_response(
                json.dumps(
                    {
                        "reasoning": "Combining both answers",
                        "final_answer": "Combined answer",
                        "confidence": 0.88,
                    }
                )
            ),
        ]

        result = self.registry["reason.decompose"](
            state={}, problem="Solve this complex problem", model="gpt-4"
        )

        self.assertNotIn("error", result)
        self.assertEqual(len(result["sub_problems"]), 2)
        self.assertEqual(len(result["sub_answers"]), 2)
        self.assertEqual(result["final_answer"], "Combined answer")
        self.assertIn("reasoning_trace", result)

    def test_decompose_max_depth_limit(self):
        """Test decomposition respects max_depth (RISK-003: Infinite recursion prevention)."""
        # This tests the max_depth parameter directly
        self.mock_llm_call.side_effect = [
            create_mock_llm_response(
                json.dumps(
                    {
                        "analysis": "Complex problem",
                        "sub_problems": [
                            {"id": 1, "description": "Sub 1", "dependencies": []}
                        ],
                        "synthesis_strategy": "Combine",
                    }
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {"reasoning": "Solving", "answer": "Sub answer", "confidence": 0.8}
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {
                        "reasoning": "Final",
                        "final_answer": "Final answer",
                        "confidence": 0.9,
                    }
                )
            ),
        ]

        result = self.registry["reason.decompose"](
            state={}, problem="Test problem", model="gpt-4", max_depth=1
        )

        self.assertNotIn("error", result)
        self.assertEqual(result["depth_used"], 0)

    def test_decompose_with_dependencies(self):
        """Test sub-problems with dependencies get context from prior answers."""
        self.mock_llm_call.side_effect = [
            create_mock_llm_response(
                json.dumps(
                    {
                        "analysis": "Sequential problem",
                        "sub_problems": [
                            {"id": 1, "description": "Calculate A", "dependencies": []},
                            {
                                "id": 2,
                                "description": "Use A to calculate B",
                                "dependencies": [1],
                            },
                        ],
                        "synthesis_strategy": "Combine A and B",
                    }
                )
            ),
            create_mock_llm_response(
                json.dumps({"reasoning": "A = 10", "answer": "10", "confidence": 0.95})
            ),
            create_mock_llm_response(
                json.dumps(
                    {
                        "reasoning": "Using A=10, B = A*2 = 20",
                        "answer": "20",
                        "confidence": 0.9,
                    }
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {
                        "reasoning": "A=10, B=20",
                        "final_answer": "A=10, B=20",
                        "confidence": 0.92,
                    }
                )
            ),
        ]

        result = self.registry["reason.decompose"](
            state={}, problem="Calculate A and B", model="gpt-4"
        )

        self.assertNotIn("error", result)
        # Verify sub-problem 2 received context from sub-problem 1
        calls = self.mock_llm_call.call_args_list
        sub2_call = calls[2]
        messages = sub2_call.kwargs.get("messages") or sub2_call[1].get("messages")
        user_content = messages[1]["content"]
        self.assertIn("Sub-answer 1", user_content)

    def test_decompose_synthesis_prompt(self):
        """Test custom synthesis prompt is used."""
        self.mock_llm_call.side_effect = [
            create_mock_llm_response(
                json.dumps(
                    {
                        "analysis": "Simple problem",
                        "sub_problems": [
                            {"id": 1, "description": "Part 1", "dependencies": []}
                        ],
                        "synthesis_strategy": "Default strategy",
                    }
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {"reasoning": "Solved", "answer": "Part answer", "confidence": 0.9}
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {
                        "reasoning": "Custom synthesis",
                        "final_answer": "Final",
                        "confidence": 0.95,
                    }
                )
            ),
        ]

        result = self.registry["reason.decompose"](
            state={},
            problem="Test",
            model="gpt-4",
            synthesis_prompt="Use this custom synthesis approach",
        )

        self.assertNotIn("error", result)
        # Verify custom synthesis prompt was used
        calls = self.mock_llm_call.call_args_list
        synth_call = calls[2]
        messages = synth_call.kwargs.get("messages") or synth_call[1].get("messages")
        system_content = messages[0]["content"]
        self.assertIn("Use this custom synthesis approach", system_content)

    def test_decompose_sub_problem_error(self):
        """Test decomposition handles sub-problem errors gracefully."""
        self.mock_llm_call.side_effect = [
            create_mock_llm_response(
                json.dumps(
                    {
                        "analysis": "Has two parts",
                        "sub_problems": [
                            {"id": 1, "description": "Part 1", "dependencies": []},
                            {"id": 2, "description": "Part 2", "dependencies": []},
                        ],
                        "synthesis_strategy": "Combine",
                    }
                )
            ),
            create_mock_llm_error("LLM failed for sub-problem 1"),
            create_mock_llm_response(
                json.dumps(
                    {
                        "reasoning": "Solved",
                        "answer": "Part 2 answer",
                        "confidence": 0.8,
                    }
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {
                        "reasoning": "Partial synthesis",
                        "final_answer": "Partial answer",
                        "confidence": 0.6,
                    }
                )
            ),
        ]

        result = self.registry["reason.decompose"](
            state={}, problem="Test", model="gpt-4"
        )

        # Should still complete with partial answers
        self.assertNotIn("error", result)
        self.assertEqual(len(result["sub_answers"]), 2)
        # First sub-answer should have error
        self.assertIn("error", result["sub_answers"][0])

    def test_decompose_llm_error(self):
        """Test decomposition handles initial LLM errors."""
        self.mock_llm_call.return_value = create_mock_llm_error("API error")

        result = self.registry["reason.decompose"](
            state={}, problem="Test", model="gpt-4"
        )

        self.assertIn("error", result)
        self.assertEqual(result["success"], False)


class TestActionRegistration(unittest.TestCase):
    """Tests for action registration."""

    def test_all_actions_registered(self):
        """Test all reasoning actions are registered in the registry."""
        engine = MockEngine()
        registry = {}
        register_actions(registry, engine)

        # Check main action names
        expected_actions = [
            "reason.cot",
            "reason.react",
            "reason.self_correct",
            "reason.decompose",
        ]

        for action in expected_actions:
            self.assertIn(action, registry, f"Action {action} not found in registry")
            self.assertTrue(callable(registry[action]))

    def test_alias_registration(self):
        """Test actions are also registered with actions.* prefix."""
        engine = MockEngine()
        registry = {}
        register_actions(registry, engine)

        aliases = [
            "actions.reason_cot",
            "actions.reason_react",
            "actions.reason_self_correct",
            "actions.reason_decompose",
        ]

        for alias in aliases:
            self.assertIn(alias, registry, f"Alias {alias} not found in registry")


class TestTraceFormat(unittest.TestCase):
    """Tests for reasoning trace format compatibility."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_llm_call = MagicMock()
        self.registry = setup_registry_with_mock_llm(self.mock_llm_call)

    def test_trace_has_timestamps(self):
        """Test all trace entries have timestamps."""
        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps({"thinking": "test", "answer": "result"})
        )

        result = self.registry["reason.cot"](state={}, problem="Test", model="gpt-4")

        trace = result["reasoning_trace"]
        for entry in trace:
            self.assertIn("timestamp", entry)
            self.assertIsInstance(entry["timestamp"], float)

    def test_trace_has_step_identifiers(self):
        """Test all trace entries have step identifiers."""
        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps({"thinking": "test", "answer": "result"})
        )

        result = self.registry["reason.cot"](state={}, problem="Test", model="gpt-4")

        trace = result["reasoning_trace"]
        for entry in trace:
            self.assertIn("step", entry)

    def test_trace_serializable(self):
        """Test trace can be serialized to JSON (Opik compatibility)."""
        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps({"thinking": "test", "answer": "result"})
        )

        result = self.registry["reason.cot"](state={}, problem="Test", model="gpt-4")

        trace = result["reasoning_trace"]
        # Should not raise
        json_str = json.dumps(trace, default=str)
        self.assertIsInstance(json_str, str)


class TestEdgeCases(unittest.TestCase):
    """Tests for edge cases and error handling."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_llm_call = MagicMock()
        self.registry = setup_registry_with_mock_llm(self.mock_llm_call)

    def test_empty_problem(self):
        """Test handling of empty problem string."""
        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps({"thinking": "No problem given", "answer": "N/A"})
        )

        result = self.registry["reason.cot"](state={}, problem="", model="gpt-4")

        # Should still work, LLM handles empty input
        self.assertNotIn("error", result)

    def test_unicode_in_problem(self):
        """Test handling of unicode characters."""
        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps({"thinking": "Unicode handled", "answer": "Caf\u00e9"})
        )

        result = self.registry["reason.cot"](
            state={},
            problem="What is the correct spelling of caf\u00e9?",
            model="gpt-4",
        )

        self.assertNotIn("error", result)
        self.assertEqual(result["answer"], "Caf\u00e9")

    def test_very_long_response(self):
        """Test handling of very long LLM responses."""
        long_thinking = "x" * 10000
        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps({"thinking": long_thinking, "answer": "result"})
        )

        result = self.registry["reason.cot"](state={}, problem="Test", model="gpt-4")

        self.assertNotIn("error", result)
        self.assertEqual(len(result["thinking"]), 10000)

    def test_missing_llm_call_action(self):
        """Test error when llm.call is not in registry."""
        # Create a registry without llm.call
        engine = MockEngine()
        empty_registry = {}
        register_actions(empty_registry, engine)
        # Note: llm.call was never added, so reason.cot should return an error dict

        result = empty_registry["reason.cot"](state={}, problem="Test", model="gpt-4")

        # Error should be caught and returned in result dict
        self.assertIn("error", result)
        self.assertEqual(result["success"], False)
        self.assertIn("llm.call action not found", result["error"])


class TestDSPyBackend(unittest.TestCase):
    """Tests for DSPy backend actions (AC9, AC10)."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_llm_call = MagicMock()
        self.registry = setup_registry_with_mock_llm(self.mock_llm_call)

    def test_dspy_cot_fallback_when_dspy_unavailable(self):
        """Test reason.dspy.cot falls back to native when DSPy not installed."""
        # Mock DSPy import to fail
        with patch.dict("sys.modules", {"dspy": None}):
            self.mock_llm_call.return_value = create_mock_llm_response(
                json.dumps({"thinking": "Native fallback", "answer": "42"})
            )

            result = self.registry["reason.dspy.cot"](
                state={}, problem="What is 6*7?", model="gpt-4"
            )

            self.assertIn("dspy_module", result)
            self.assertEqual(result["dspy_module"], "native_fallback")
            self.assertEqual(result["answer"], "42")

    def test_dspy_react_fallback_when_dspy_unavailable(self):
        """Test reason.dspy.react falls back to native when DSPy not installed."""
        with patch.dict("sys.modules", {"dspy": None}):
            self.mock_llm_call.return_value = create_mock_llm_response(
                json.dumps(
                    {
                        "thought": "Done",
                        "action": "final_answer",
                        "action_input": {"answer": "Result"},
                    }
                )
            )

            result = self.registry["reason.dspy.react"](
                state={}, goal="Test goal", model="gpt-4"
            )

            self.assertIn("dspy_module", result)
            self.assertEqual(result["dspy_module"], "native_fallback")

    def test_dspy_compile_error_when_dspy_unavailable(self):
        """Test reason.dspy.compile returns error when DSPy not installed."""
        with patch.dict("sys.modules", {"dspy": None}):
            result = self.registry["reason.dspy.compile"](
                state={},
                module_type="cot",
                training_data=[{"question": "test", "answer": "result"}],
            )

            self.assertFalse(result["success"])
            self.assertFalse(result["compiled"])
            self.assertIn("DSPy not installed", result["error"])

    def test_dspy_cot_action_registered(self):
        """Test reason.dspy.cot action is registered."""
        self.assertIn("reason.dspy.cot", self.registry)
        self.assertTrue(callable(self.registry["reason.dspy.cot"]))

    def test_dspy_react_action_registered(self):
        """Test reason.dspy.react action is registered."""
        self.assertIn("reason.dspy.react", self.registry)
        self.assertTrue(callable(self.registry["reason.dspy.react"]))

    def test_dspy_compile_action_registered(self):
        """Test reason.dspy.compile action is registered."""
        self.assertIn("reason.dspy.compile", self.registry)
        self.assertTrue(callable(self.registry["reason.dspy.compile"]))

    def test_dspy_cot_returns_trace(self):
        """Test DSPy CoT returns reasoning trace even in fallback."""
        with patch.dict("sys.modules", {"dspy": None}):
            self.mock_llm_call.return_value = create_mock_llm_response(
                json.dumps({"thinking": "Steps", "answer": "Done"})
            )

            result = self.registry["reason.dspy.cot"](
                state={}, problem="Test", model="gpt-4"
            )

            self.assertIn("reasoning_trace", result)
            self.assertIsInstance(result["reasoning_trace"], list)


class TestPythonRustParity(unittest.TestCase):
    """Tests to ensure Python/Rust implementation parity (RISK-006)."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_llm_call = MagicMock()
        self.registry = setup_registry_with_mock_llm(self.mock_llm_call)

    def test_cot_output_schema(self):
        """Test CoT output schema matches expected format."""
        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps({"thinking": "Steps", "answer": "Result"})
        )

        result = self.registry["reason.cot"](state={}, problem="Test", model="gpt-4")

        # Verify required output fields
        required_fields = ["thinking", "answer", "reasoning_trace", "model"]
        for field in required_fields:
            self.assertIn(field, result, f"Missing required field: {field}")

        # Verify state variable aliases
        self.assertIn("reasoning_thinking", result)
        self.assertIn("reasoning_answer", result)

    def test_react_output_schema(self):
        """Test ReAct output schema matches expected format."""
        self.mock_llm_call.return_value = create_mock_llm_response(
            json.dumps(
                {
                    "thought": "Done",
                    "action": "final_answer",
                    "action_input": {"answer": "Result"},
                }
            )
        )

        result = self.registry["reason.react"](state={}, goal="Test", model="gpt-4")

        required_fields = [
            "steps",
            "final_answer",
            "reasoning_trace",
            "react_steps",
            "total_steps",
        ]
        for field in required_fields:
            self.assertIn(field, result, f"Missing required field: {field}")

    def test_self_correct_output_schema(self):
        """Test self-correct output schema matches expected format."""
        self.mock_llm_call.side_effect = [
            create_mock_llm_response(
                json.dumps(
                    {"output": "Initial", "confidence": 0.7, "potential_issues": []}
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {
                        "issues": [],
                        "suggestions": [],
                        "improved_output": "Final",
                        "improvement_score": 0.9,
                    }
                )
            ),
        ]

        result = self.registry["reason.self_correct"](
            state={}, task="Test", model="gpt-4", improvement_rounds=1
        )

        required_fields = [
            "output",
            "improvement_history",
            "reasoning_trace",
            "rounds_completed",
        ]
        for field in required_fields:
            self.assertIn(field, result, f"Missing required field: {field}")

    def test_decompose_output_schema(self):
        """Test decompose output schema matches expected format."""
        self.mock_llm_call.side_effect = [
            create_mock_llm_response(
                json.dumps(
                    {
                        "analysis": "Analysis",
                        "sub_problems": [
                            {"id": 1, "description": "Sub", "dependencies": []}
                        ],
                        "synthesis_strategy": "Combine",
                    }
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {"reasoning": "Solving", "answer": "Sub answer", "confidence": 0.8}
                )
            ),
            create_mock_llm_response(
                json.dumps(
                    {"reasoning": "Final", "final_answer": "Result", "confidence": 0.9}
                )
            ),
        ]

        result = self.registry["reason.decompose"](
            state={}, problem="Test", model="gpt-4"
        )

        required_fields = [
            "sub_problems",
            "sub_answers",
            "final_answer",
            "reasoning_trace",
        ]
        for field in required_fields:
            self.assertIn(field, result, f"Missing required field: {field}")


if __name__ == "__main__":
    unittest.main()
