"""
Reasoning Actions for YAMLEngine (TEA-AGENT-001.4).

This module provides reasoning technique primitives for YAMLEngine workflows.
These actions implement common agentic design patterns from Chapter 17 of
Agentic Design Patterns:

- reason.cot: Chain-of-Thought prompting with structured output
- reason.react: ReAct (Reason-Act) loop with tool integration
- reason.self_correct: Generate-critique-improve cycle
- reason.decompose: Problem decomposition with sub-problem solving

All actions return structured output with reasoning traces suitable for
debugging and observability (Opik-compatible).

Example:
    >>> # Chain-of-Thought
    >>> result = registry['reason.cot'](
    ...     state={},
    ...     problem="What is 15% of 80?",
    ...     model="gpt-4"
    ... )
    >>> print(result['thinking'])
    >>> print(result['answer'])

    >>> # ReAct with tools
    >>> result = registry['reason.react'](
    ...     state={},
    ...     goal="Find the current weather in Paris",
    ...     model="gpt-4",
    ...     tools=["web.search"]
    ... )
    >>> print(result['steps'])
    >>> print(result['final_answer'])
"""

import json
import logging
import time
import copy
from typing import Any, Callable, Dict, List, Optional

logger = logging.getLogger(__name__)


# Constants for thinking formats
THINKING_FORMATS = {
    "step_by_step": """Think through this step by step:
1. First, identify the key components of the problem
2. Work through each component systematically
3. Show your reasoning at each step
4. Arrive at a final answer""",
    "pros_cons": """Analyze this by considering pros and cons:
1. List the positive aspects/arguments
2. List the negative aspects/arguments
3. Weigh the evidence
4. Reach a balanced conclusion""",
    "tree": """Think through this using tree-structured reasoning:
1. Identify the main branches of analysis
2. Explore each branch systematically
3. Combine insights from all branches
4. Synthesize a final answer""",
    "first_principles": """Reason from first principles:
1. Identify the fundamental truths or axioms
2. Build up your reasoning from these foundations
3. Question assumptions at each step
4. Derive the answer logically""",
}

# Default prompts for reasoning actions
COT_SYSTEM_PROMPT = """You are a careful reasoning assistant. When given a problem, you will think through it systematically before providing your answer.

Your response MUST be valid JSON in this exact format:
{{
  "thinking": "Your detailed step-by-step reasoning here...",
  "answer": "Your final answer here (can be a string, number, or structured data)"
}}

{thinking_format}"""

REACT_SYSTEM_PROMPT = """You are a ReAct agent. For each step, you will:
1. THINK about what you need to do
2. Decide on an ACTION to take (or conclude if you have the answer)
3. Wait for the OBSERVATION from the action

You have access to these tools:
{tools_description}

Your response MUST be valid JSON in one of these formats:

For taking an action:
{{
  "thought": "Your reasoning about what to do next...",
  "action": "tool_name",
  "action_input": {{"param1": "value1", "param2": "value2"}}
}}

For providing the final answer (when you have enough information):
{{
  "thought": "Your reasoning about why you can now answer...",
  "action": "final_answer",
  "action_input": {{"answer": "Your final answer here"}}
}}

Think carefully and only use the final_answer action when you truly have the answer."""

SELF_CORRECT_SYSTEM_PROMPT = """You are a self-improving assistant. You will generate an initial response, then critique and improve it.

Your response MUST be valid JSON in this format:
{{
  "output": "Your response to the task",
  "confidence": 0.0 to 1.0,
  "potential_issues": ["Issue 1", "Issue 2", ...]
}}"""

CRITIC_SYSTEM_PROMPT = """You are a critical reviewer. Analyze the given output and identify improvements.

{critic_prompt}

Your response MUST be valid JSON in this format:
{{
  "issues": ["Issue 1", "Issue 2", ...],
  "suggestions": ["Suggestion 1", "Suggestion 2", ...],
  "improved_output": "The improved version of the output",
  "improvement_score": 0.0 to 1.0
}}"""

DECOMPOSE_SYSTEM_PROMPT = """You are a problem decomposition expert. Break down complex problems into smaller, manageable sub-problems.

Your response MUST be valid JSON in this format:
{{
  "analysis": "Brief analysis of the problem",
  "sub_problems": [
    {{"id": 1, "description": "Sub-problem 1", "dependencies": []}},
    {{"id": 2, "description": "Sub-problem 2", "dependencies": [1]}}
  ],
  "synthesis_strategy": "How to combine the sub-answers"
}}"""

SUB_PROBLEM_SYSTEM_PROMPT = """You are solving a specific sub-problem as part of a larger problem.

Main problem context: {main_problem}

Your response MUST be valid JSON in this format:
{{
  "reasoning": "Your reasoning process",
  "answer": "Your answer to this sub-problem",
  "confidence": 0.0 to 1.0
}}"""

SYNTHESIS_SYSTEM_PROMPT = """You are synthesizing sub-answers into a final answer.

{synthesis_prompt}

Your response MUST be valid JSON in this format:
{{
  "reasoning": "How you combined the sub-answers",
  "final_answer": "The synthesized final answer",
  "confidence": 0.0 to 1.0
}}"""


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register reasoning actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    def _make_llm_call(
        model: str, messages: List[Dict], temperature: float = 0.7, **kwargs
    ) -> Dict[str, Any]:
        """
        Helper to make LLM call using the engine's llm.call action.

        Args:
            model: Model name
            messages: List of message dicts
            temperature: Sampling temperature
            **kwargs: Additional LLM parameters

        Returns:
            LLM response dict with 'content' key
        """
        llm_call = registry.get("llm.call")
        if not llm_call:
            raise RuntimeError("llm.call action not found in registry")

        return llm_call(
            state={}, model=model, messages=messages, temperature=temperature, **kwargs
        )

    def _parse_json_response(content: str) -> Dict[str, Any]:
        """
        Parse JSON from LLM response, handling common issues.

        Args:
            content: LLM response content

        Returns:
            Parsed JSON dict

        Raises:
            ValueError: If JSON cannot be parsed
        """
        if not content:
            raise ValueError("Empty response from LLM")

        # Try direct JSON parse first
        try:
            return json.loads(content)
        except json.JSONDecodeError:
            pass

        # Try to extract JSON from markdown code blocks
        import re

        json_match = re.search(r"```(?:json)?\s*([\s\S]*?)```", content)
        if json_match:
            try:
                return json.loads(json_match.group(1).strip())
            except json.JSONDecodeError:
                pass

        # Try to find JSON object in the response
        brace_match = re.search(r"\{[\s\S]*\}", content)
        if brace_match:
            try:
                return json.loads(brace_match.group())
            except json.JSONDecodeError:
                pass

        raise ValueError(f"Could not parse JSON from response: {content[:200]}...")

    def _build_few_shot_prompt(examples: List[Dict]) -> str:
        """
        Build few-shot examples section for prompts.

        Args:
            examples: List of example dicts with problem/thinking/answer

        Returns:
            Formatted examples string
        """
        if not examples:
            return ""

        parts = ["\n\nHere are some examples:\n"]
        for i, ex in enumerate(examples, 1):
            parts.append(f"\nExample {i}:")
            parts.append(f"Problem: {ex.get('problem', '')}")
            parts.append(f"Thinking: {ex.get('thinking', '')}")
            parts.append(f"Answer: {ex.get('answer', '')}")

        return "\n".join(parts)

    def reason_cot(
        state: Dict[str, Any],
        problem: str,
        model: str = "gpt-4",
        thinking_format: str = "step_by_step",
        few_shot_examples: Optional[List[Dict]] = None,
        temperature: float = 0.7,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Chain-of-Thought reasoning action.

        Wraps an LLM call with CoT prompting to produce structured output
        with explicit thinking steps and a final answer.

        Args:
            state: Current state dictionary
            problem: The problem or question to reason about
            model: LLM model to use (default: gpt-4)
            thinking_format: Reasoning format - step_by_step, pros_cons, tree, first_principles
            few_shot_examples: Optional list of example dicts with problem/thinking/answer
            temperature: LLM temperature (default: 0.7)
            **kwargs: Additional LLM parameters

        Returns:
            {
                "thinking": str,           # Chain-of-thought reasoning
                "answer": any,             # Final answer
                "reasoning_trace": list,   # Full trace for observability
                "model": str,
                "thinking_format": str
            }
            Or {"error": str, "success": False} on failure
        """
        start_time = time.time()
        trace = []

        try:
            # Get thinking format instruction
            format_instruction = THINKING_FORMATS.get(
                thinking_format, THINKING_FORMATS["step_by_step"]
            )

            # Build system prompt
            system_prompt = COT_SYSTEM_PROMPT.format(thinking_format=format_instruction)

            # Add few-shot examples if provided
            if few_shot_examples:
                system_prompt += _build_few_shot_prompt(few_shot_examples)

            # Build messages
            messages = [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": f"Problem: {problem}"},
            ]

            # Record trace entry
            trace.append(
                {
                    "step": "cot_request",
                    "timestamp": time.time(),
                    "problem": problem,
                    "thinking_format": thinking_format,
                    "model": model,
                }
            )

            # Make LLM call
            response = _make_llm_call(
                model=model, messages=messages, temperature=temperature, **kwargs
            )

            if response.get("error"):
                return {
                    "error": response.get("error"),
                    "success": False,
                    "reasoning_trace": trace,
                }

            # Parse response
            content = response.get("content", "")
            parsed = _parse_json_response(content)

            thinking = parsed.get("thinking", "")
            answer = parsed.get("answer", "")

            # Record completion trace
            elapsed = time.time() - start_time
            trace.append(
                {
                    "step": "cot_response",
                    "timestamp": time.time(),
                    "thinking": thinking,
                    "answer": answer,
                    "elapsed_seconds": elapsed,
                    "usage": response.get("usage", {}),
                }
            )

            return {
                "thinking": thinking,
                "answer": answer,
                "reasoning_trace": trace,
                "model": model,
                "thinking_format": thinking_format,
                "reasoning_thinking": thinking,  # For state variable compatibility
                "reasoning_answer": answer,
            }

        except Exception as e:
            logger.error(f"reason.cot failed: {e}")
            trace.append({"step": "error", "timestamp": time.time(), "error": str(e)})
            return {
                "error": f"Chain-of-thought reasoning failed: {str(e)}",
                "success": False,
                "reasoning_trace": trace,
            }

    registry["reason.cot"] = reason_cot
    registry["actions.reason_cot"] = reason_cot

    def reason_react(
        state: Dict[str, Any],
        goal: str,
        model: str = "gpt-4",
        tools: Optional[List[str]] = None,
        max_steps: int = 10,
        temperature: float = 0.7,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        ReAct (Reason-Act) reasoning action.

        Implements the Thought -> Action -> Observation loop for
        goal-directed reasoning with tool use.

        Args:
            state: Current state dictionary
            goal: The goal or question to achieve/answer
            model: LLM model to use (default: gpt-4)
            tools: List of tool/action names to make available
            max_steps: Maximum reasoning steps (default: 10)
            temperature: LLM temperature (default: 0.7)
            **kwargs: Additional LLM parameters

        Returns:
            {
                "steps": list,             # Thought-action-observation steps
                "final_answer": any,       # Final answer when goal achieved
                "reasoning_trace": list,   # Full trace for observability
                "react_steps": list,       # Alias for steps (state variable)
                "model": str,
                "total_steps": int
            }
            Or {"error": str, "success": False} on failure
        """
        start_time = time.time()
        trace = []
        steps = []

        try:
            # Build tools description
            available_tools = tools or []
            tools_desc_parts = []

            for tool_name in available_tools:
                # Try to get tool from registry for description
                tool_func = registry.get(tool_name)
                if tool_func and hasattr(tool_func, "__doc__") and tool_func.__doc__:
                    doc = tool_func.__doc__.split("\n")[0].strip()
                    tools_desc_parts.append(f"- {tool_name}: {doc}")
                else:
                    tools_desc_parts.append(
                        f"- {tool_name}: Execute the {tool_name} action"
                    )

            # Add final_answer as special tool
            tools_desc_parts.append(
                "- final_answer: Use this when you have the final answer to the goal"
            )

            tools_description = (
                "\n".join(tools_desc_parts)
                if tools_desc_parts
                else "No tools available."
            )

            # Build system prompt
            system_prompt = REACT_SYSTEM_PROMPT.format(
                tools_description=tools_description
            )

            # Initialize conversation
            messages = [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": f"Goal: {goal}"},
            ]

            # Record initial trace
            trace.append(
                {
                    "step": "react_start",
                    "timestamp": time.time(),
                    "goal": goal,
                    "tools": available_tools,
                    "max_steps": max_steps,
                    "model": model,
                }
            )

            # ReAct loop
            step_num = 0
            while step_num < max_steps:
                step_num += 1

                # Get LLM response
                response = _make_llm_call(
                    model=model, messages=messages, temperature=temperature, **kwargs
                )

                if response.get("error"):
                    return {
                        "error": response.get("error"),
                        "success": False,
                        "steps": steps,
                        "reasoning_trace": trace,
                        "total_steps": step_num,
                    }

                content = response.get("content", "")

                try:
                    parsed = _parse_json_response(content)
                except ValueError as e:
                    # LLM returned invalid JSON - try to recover
                    trace.append(
                        {
                            "step": f"step_{step_num}_parse_error",
                            "timestamp": time.time(),
                            "error": str(e),
                            "raw_content": content[:500],
                        }
                    )
                    # Add error to messages and continue
                    messages.append({"role": "assistant", "content": content})
                    messages.append(
                        {
                            "role": "user",
                            "content": "Your response was not valid JSON. Please respond with valid JSON.",
                        }
                    )
                    continue

                thought = parsed.get("thought", "")
                action = parsed.get("action", "")
                action_input = parsed.get("action_input", {})

                # Record step
                step_record = {
                    "step": step_num,
                    "thought": thought,
                    "action": action,
                    "action_input": action_input,
                    "timestamp": time.time(),
                }

                # Check for final answer
                if action == "final_answer":
                    answer = action_input.get("answer", action_input)
                    step_record["observation"] = "Goal achieved."
                    steps.append(step_record)

                    trace.append(
                        {
                            "step": f"step_{step_num}",
                            "timestamp": time.time(),
                            **step_record,
                            "type": "final_answer",
                        }
                    )

                    elapsed = time.time() - start_time
                    trace.append(
                        {
                            "step": "react_complete",
                            "timestamp": time.time(),
                            "elapsed_seconds": elapsed,
                            "total_steps": step_num,
                            "final_answer": answer,
                        }
                    )

                    return {
                        "steps": steps,
                        "final_answer": answer,
                        "reasoning_trace": trace,
                        "react_steps": steps,
                        "reasoning_answer": answer,
                        "model": model,
                        "total_steps": step_num,
                    }

                # Execute tool/action
                observation = ""
                if action in available_tools:
                    action_func = registry.get(action)
                    if action_func:
                        try:
                            # Execute action with state and parsed input
                            if isinstance(action_input, dict):
                                result = action_func(state=state, **action_input)
                            else:
                                result = action_func(state=state, input=action_input)
                            observation = json.dumps(result, default=str)
                        except Exception as e:
                            observation = f"Error executing {action}: {str(e)}"
                    else:
                        observation = f"Action {action} not found in registry"
                else:
                    observation = (
                        f"Unknown action: {action}. Available: {available_tools}"
                    )

                step_record["observation"] = observation
                steps.append(step_record)

                # Record trace
                trace.append(
                    {
                        "step": f"step_{step_num}",
                        "timestamp": time.time(),
                        **step_record,
                        "type": "action",
                    }
                )

                # Add to conversation
                messages.append({"role": "assistant", "content": content})
                messages.append(
                    {"role": "user", "content": f"Observation: {observation}"}
                )

            # Max steps reached without final answer
            elapsed = time.time() - start_time
            trace.append(
                {
                    "step": "react_max_steps",
                    "timestamp": time.time(),
                    "elapsed_seconds": elapsed,
                    "total_steps": max_steps,
                    "warning": "Max steps reached without final answer",
                }
            )

            return {
                "steps": steps,
                "final_answer": None,
                "reasoning_trace": trace,
                "react_steps": steps,
                "model": model,
                "total_steps": max_steps,
                "warning": f"Max steps ({max_steps}) reached without achieving goal",
            }

        except Exception as e:
            logger.error(f"reason.react failed: {e}")
            trace.append({"step": "error", "timestamp": time.time(), "error": str(e)})
            return {
                "error": f"ReAct reasoning failed: {str(e)}",
                "success": False,
                "steps": steps,
                "reasoning_trace": trace,
                "total_steps": len(steps),
            }

    registry["reason.react"] = reason_react
    registry["actions.reason_react"] = reason_react

    def reason_self_correct(
        state: Dict[str, Any],
        task: str,
        model: str = "gpt-4",
        generator_model: Optional[str] = None,
        critic_model: Optional[str] = None,
        improvement_rounds: int = 2,
        critic_prompt: Optional[str] = None,
        temperature: float = 0.7,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Self-correction reasoning action.

        Implements generate -> critique -> improve cycle for iterative
        refinement of outputs.

        Args:
            state: Current state dictionary
            task: The task description
            model: Default LLM model to use
            generator_model: Model for generation (default: same as model)
            critic_model: Model for critique (default: same as model)
            improvement_rounds: Number of improvement iterations (default: 2)
            critic_prompt: Custom prompt for the critic
            temperature: LLM temperature (default: 0.7)
            **kwargs: Additional LLM parameters

        Returns:
            {
                "output": any,                 # Final improved output
                "improvement_history": list,   # History of improvements
                "reasoning_trace": list,       # Full trace for observability
                "model": str,
                "rounds_completed": int
            }
            Or {"error": str, "success": False} on failure
        """
        start_time = time.time()
        trace = []
        improvement_history = []

        # Resolve models
        gen_model = generator_model or model
        crit_model = critic_model or model

        try:
            # Record start trace
            trace.append(
                {
                    "step": "self_correct_start",
                    "timestamp": time.time(),
                    "task": task,
                    "generator_model": gen_model,
                    "critic_model": crit_model,
                    "improvement_rounds": improvement_rounds,
                }
            )

            # Initial generation
            gen_messages = [
                {"role": "system", "content": SELF_CORRECT_SYSTEM_PROMPT},
                {"role": "user", "content": f"Task: {task}"},
            ]

            response = _make_llm_call(
                model=gen_model,
                messages=gen_messages,
                temperature=temperature,
                **kwargs,
            )

            if response.get("error"):
                return {
                    "error": response.get("error"),
                    "success": False,
                    "reasoning_trace": trace,
                }

            try:
                parsed = _parse_json_response(response.get("content", ""))
                current_output = parsed.get("output", response.get("content", ""))
                confidence = parsed.get("confidence", 0.5)
                issues = parsed.get("potential_issues", [])
            except ValueError:
                # Fall back to raw content
                current_output = response.get("content", "")
                confidence = 0.5
                issues = []

            improvement_history.append(
                {
                    "round": 0,
                    "type": "initial",
                    "output": current_output,
                    "confidence": confidence,
                    "potential_issues": issues,
                    "timestamp": time.time(),
                }
            )

            trace.append(
                {
                    "step": "initial_generation",
                    "timestamp": time.time(),
                    "output": current_output[:500],  # Truncate for trace
                    "confidence": confidence,
                }
            )

            # Improvement rounds
            for round_num in range(1, improvement_rounds + 1):
                # Build critic prompt
                custom_critic = (
                    critic_prompt
                    or "Identify any issues with correctness, clarity, completeness, and style."
                )
                crit_system = CRITIC_SYSTEM_PROMPT.format(critic_prompt=custom_critic)

                crit_messages = [
                    {"role": "system", "content": crit_system},
                    {
                        "role": "user",
                        "content": f"Task: {task}\n\nCurrent output:\n{current_output}",
                    },
                ]

                # Get critique
                crit_response = _make_llm_call(
                    model=crit_model,
                    messages=crit_messages,
                    temperature=temperature,
                    **kwargs,
                )

                if crit_response.get("error"):
                    trace.append(
                        {
                            "step": f"critique_round_{round_num}_error",
                            "timestamp": time.time(),
                            "error": crit_response.get("error"),
                        }
                    )
                    continue

                try:
                    crit_parsed = _parse_json_response(crit_response.get("content", ""))
                    critique_issues = crit_parsed.get("issues", [])
                    suggestions = crit_parsed.get("suggestions", [])
                    improved_output = crit_parsed.get("improved_output", current_output)
                    improvement_score = crit_parsed.get("improvement_score", 0.5)
                except ValueError:
                    # Skip this round if parsing fails
                    trace.append(
                        {
                            "step": f"critique_round_{round_num}_parse_error",
                            "timestamp": time.time(),
                            "raw_content": crit_response.get("content", "")[:200],
                        }
                    )
                    continue

                # Record improvement
                improvement_history.append(
                    {
                        "round": round_num,
                        "type": "improvement",
                        "issues_found": critique_issues,
                        "suggestions": suggestions,
                        "output": improved_output,
                        "improvement_score": improvement_score,
                        "timestamp": time.time(),
                    }
                )

                trace.append(
                    {
                        "step": f"improvement_round_{round_num}",
                        "timestamp": time.time(),
                        "issues_count": len(critique_issues),
                        "improvement_score": improvement_score,
                    }
                )

                current_output = improved_output

            elapsed = time.time() - start_time
            trace.append(
                {
                    "step": "self_correct_complete",
                    "timestamp": time.time(),
                    "elapsed_seconds": elapsed,
                    "rounds_completed": improvement_rounds,
                }
            )

            return {
                "output": current_output,
                "improvement_history": improvement_history,
                "reasoning_trace": trace,
                "reasoning_answer": current_output,
                "model": model,
                "generator_model": gen_model,
                "critic_model": crit_model,
                "rounds_completed": improvement_rounds,
            }

        except Exception as e:
            logger.error(f"reason.self_correct failed: {e}")
            trace.append({"step": "error", "timestamp": time.time(), "error": str(e)})
            return {
                "error": f"Self-correction failed: {str(e)}",
                "success": False,
                "improvement_history": improvement_history,
                "reasoning_trace": trace,
            }

    registry["reason.self_correct"] = reason_self_correct
    registry["actions.reason_self_correct"] = reason_self_correct

    def reason_decompose(
        state: Dict[str, Any],
        problem: str,
        model: str = "gpt-4",
        max_depth: int = 2,
        synthesis_prompt: Optional[str] = None,
        temperature: float = 0.7,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Problem decomposition reasoning action.

        Breaks complex problems into sub-problems, solves each independently,
        and synthesizes a final answer.

        Args:
            state: Current state dictionary
            problem: The complex problem to decompose
            model: LLM model to use (default: gpt-4)
            max_depth: Maximum recursion depth for sub-decomposition (default: 2)
            synthesis_prompt: Custom prompt for answer synthesis
            temperature: LLM temperature (default: 0.7)
            **kwargs: Additional LLM parameters

        Returns:
            {
                "sub_problems": list,      # Decomposed sub-problems
                "sub_answers": list,       # Answers to sub-problems
                "final_answer": any,       # Synthesized final answer
                "reasoning_trace": list,   # Full trace for observability
                "model": str,
                "depth_used": int
            }
            Or {"error": str, "success": False} on failure
        """
        start_time = time.time()
        trace = []

        # Track current depth for recursion limit
        current_depth = kwargs.pop("_current_depth", 0)

        if current_depth > max_depth:
            return {
                "error": f"Max decomposition depth ({max_depth}) exceeded",
                "success": False,
                "reasoning_trace": trace,
            }

        try:
            # Record start trace
            trace.append(
                {
                    "step": "decompose_start",
                    "timestamp": time.time(),
                    "problem": problem,
                    "max_depth": max_depth,
                    "current_depth": current_depth,
                    "model": model,
                }
            )

            # Decompose problem
            decomp_messages = [
                {"role": "system", "content": DECOMPOSE_SYSTEM_PROMPT},
                {"role": "user", "content": f"Problem: {problem}"},
            ]

            response = _make_llm_call(
                model=model, messages=decomp_messages, temperature=temperature, **kwargs
            )

            if response.get("error"):
                return {
                    "error": response.get("error"),
                    "success": False,
                    "reasoning_trace": trace,
                }

            try:
                parsed = _parse_json_response(response.get("content", ""))
                analysis = parsed.get("analysis", "")
                sub_problems = parsed.get("sub_problems", [])
                synthesis_strategy = parsed.get(
                    "synthesis_strategy", "Combine sub-answers"
                )
            except ValueError as e:
                return {
                    "error": f"Failed to parse decomposition: {e}",
                    "success": False,
                    "reasoning_trace": trace,
                }

            trace.append(
                {
                    "step": "decomposition",
                    "timestamp": time.time(),
                    "analysis": analysis,
                    "sub_problem_count": len(sub_problems),
                }
            )

            # Solve each sub-problem
            sub_answers = []
            for sub_prob in sub_problems:
                sub_id = sub_prob.get("id", len(sub_answers) + 1)
                sub_desc = sub_prob.get("description", str(sub_prob))
                dependencies = sub_prob.get("dependencies", [])

                # Build context from dependent answers
                dep_context = ""
                for dep_id in dependencies:
                    for ans in sub_answers:
                        if ans.get("id") == dep_id:
                            dep_context += (
                                f"\nSub-answer {dep_id}: {ans.get('answer', '')}"
                            )

                # Solve sub-problem
                sub_system = SUB_PROBLEM_SYSTEM_PROMPT.format(main_problem=problem)
                sub_messages = [
                    {"role": "system", "content": sub_system},
                    {
                        "role": "user",
                        "content": f"Sub-problem: {sub_desc}{dep_context}",
                    },
                ]

                sub_response = _make_llm_call(
                    model=model,
                    messages=sub_messages,
                    temperature=temperature,
                    **kwargs,
                )

                if sub_response.get("error"):
                    sub_answers.append(
                        {
                            "id": sub_id,
                            "description": sub_desc,
                            "error": sub_response.get("error"),
                            "answer": None,
                        }
                    )
                    continue

                try:
                    sub_parsed = _parse_json_response(sub_response.get("content", ""))
                    sub_answer = sub_parsed.get(
                        "answer", sub_response.get("content", "")
                    )
                    sub_reasoning = sub_parsed.get("reasoning", "")
                    sub_confidence = sub_parsed.get("confidence", 0.5)
                except ValueError:
                    sub_answer = sub_response.get("content", "")
                    sub_reasoning = ""
                    sub_confidence = 0.5

                sub_answers.append(
                    {
                        "id": sub_id,
                        "description": sub_desc,
                        "reasoning": sub_reasoning,
                        "answer": sub_answer,
                        "confidence": sub_confidence,
                    }
                )

                trace.append(
                    {
                        "step": f"sub_problem_{sub_id}",
                        "timestamp": time.time(),
                        "description": sub_desc,
                        "answer": str(sub_answer)[:200],
                        "confidence": sub_confidence,
                    }
                )

            # Synthesize final answer
            custom_synthesis = synthesis_prompt or synthesis_strategy
            synth_system = SYNTHESIS_SYSTEM_PROMPT.format(
                synthesis_prompt=custom_synthesis
            )

            sub_answers_text = json.dumps(sub_answers, indent=2, default=str)
            synth_messages = [
                {"role": "system", "content": synth_system},
                {
                    "role": "user",
                    "content": f"Problem: {problem}\n\nSub-answers:\n{sub_answers_text}",
                },
            ]

            synth_response = _make_llm_call(
                model=model, messages=synth_messages, temperature=temperature, **kwargs
            )

            if synth_response.get("error"):
                return {
                    "error": synth_response.get("error"),
                    "success": False,
                    "sub_problems": sub_problems,
                    "sub_answers": sub_answers,
                    "reasoning_trace": trace,
                }

            try:
                synth_parsed = _parse_json_response(synth_response.get("content", ""))
                final_answer = synth_parsed.get(
                    "final_answer", synth_response.get("content", "")
                )
                synth_reasoning = synth_parsed.get("reasoning", "")
                synth_confidence = synth_parsed.get("confidence", 0.5)
            except ValueError:
                final_answer = synth_response.get("content", "")
                synth_reasoning = ""
                synth_confidence = 0.5

            elapsed = time.time() - start_time
            trace.append(
                {
                    "step": "synthesis",
                    "timestamp": time.time(),
                    "reasoning": synth_reasoning,
                    "final_answer": str(final_answer)[:200],
                    "confidence": synth_confidence,
                }
            )

            trace.append(
                {
                    "step": "decompose_complete",
                    "timestamp": time.time(),
                    "elapsed_seconds": elapsed,
                    "sub_problems_solved": len(sub_answers),
                    "depth_used": current_depth,
                }
            )

            return {
                "sub_problems": sub_problems,
                "sub_answers": sub_answers,
                "final_answer": final_answer,
                "synthesis_reasoning": synth_reasoning,
                "reasoning_trace": trace,
                "reasoning_answer": final_answer,
                "model": model,
                "depth_used": current_depth,
            }

        except Exception as e:
            logger.error(f"reason.decompose failed: {e}")
            trace.append({"step": "error", "timestamp": time.time(), "error": str(e)})
            return {
                "error": f"Problem decomposition failed: {str(e)}",
                "success": False,
                "reasoning_trace": trace,
            }

    registry["reason.decompose"] = reason_decompose
    registry["actions.reason_decompose"] = reason_decompose

    # DSPy backend actions (AC9, AC10)
    # These wrap DSPy modules when available, with graceful fallback

    def _dspy_available() -> bool:
        """Check if DSPy is available."""
        try:
            import dspy

            return True
        except ImportError:
            return False

    def reason_dspy_cot(
        state: Dict[str, Any],
        problem: str,
        model: str = "gpt-4",
        signature: Optional[str] = None,
        temperature: float = 0.7,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Chain-of-Thought using DSPy ChainOfThought module.

        Wraps DSPy's ChainOfThought for model-agnostic CoT prompting with
        compiled/optimized prompts. Falls back to native reason.cot when
        DSPy is unavailable.

        Args:
            state: Current state dictionary
            problem: The problem to solve
            model: LLM model to use (default: gpt-4)
            signature: DSPy signature string (default: "question -> answer")
            temperature: LLM temperature (default: 0.7)
            **kwargs: Additional parameters

        Returns:
            {
                "thinking": str,           # Chain-of-thought reasoning
                "answer": any,             # Final answer
                "reasoning_trace": list,   # Trace for observability
                "dspy_module": str,        # "ChainOfThought" or "native"
            }
            Or falls back to native reason.cot if DSPy unavailable
        """
        if not _dspy_available():
            # Graceful fallback to native implementation
            result = reason_cot(
                state=state,
                problem=problem,
                model=model,
                temperature=temperature,
                **kwargs,
            )
            result["dspy_module"] = "native_fallback"
            return result

        try:
            import dspy

            trace = []
            start_time = time.time()

            trace.append(
                {
                    "step": "dspy_cot_start",
                    "timestamp": time.time(),
                    "problem": problem,
                    "model": model,
                    "signature": signature,
                }
            )

            # Configure DSPy with the specified model
            if model.startswith("gpt-") or model.startswith("o1"):
                lm = dspy.LM(f"openai/{model}", temperature=temperature)
            else:
                lm = dspy.LM(model, temperature=temperature)
            dspy.configure(lm=lm)

            # Create signature
            sig = signature or "question -> answer"

            # Create and run ChainOfThought
            cot = dspy.ChainOfThought(sig)
            result = cot(question=problem)

            # Extract rationale and answer
            thinking = (
                getattr(result, "rationale", "")
                or getattr(result, "reasoning", "")
                or ""
            )
            answer = getattr(result, "answer", "") or str(result)

            elapsed = time.time() - start_time
            trace.append(
                {
                    "step": "dspy_cot_complete",
                    "timestamp": time.time(),
                    "elapsed_seconds": elapsed,
                    "thinking_length": len(thinking),
                    "answer_length": len(str(answer)),
                }
            )

            return {
                "thinking": thinking,
                "answer": answer,
                "reasoning_thinking": thinking,
                "reasoning_answer": answer,
                "reasoning_trace": trace,
                "dspy_module": "ChainOfThought",
                "model": model,
            }

        except Exception as e:
            logger.warning(f"DSPy ChainOfThought failed, falling back to native: {e}")
            result = reason_cot(
                state=state,
                problem=problem,
                model=model,
                temperature=temperature,
                **kwargs,
            )
            result["dspy_module"] = "native_fallback"
            result["dspy_error"] = str(e)
            return result

    registry["reason.dspy.cot"] = reason_dspy_cot
    registry["actions.reason_dspy_cot"] = reason_dspy_cot

    def reason_dspy_react(
        state: Dict[str, Any],
        goal: str,
        model: str = "gpt-4",
        tools: Optional[List[str]] = None,
        max_steps: int = 10,
        signature: Optional[str] = None,
        temperature: float = 0.7,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        ReAct using DSPy ReAct module with tool bridge.

        Wraps DSPy's ReAct for tool-using agents with compiled prompts.
        Falls back to native reason.react when DSPy is unavailable.

        Args:
            state: Current state dictionary
            goal: The goal to achieve
            model: LLM model to use (default: gpt-4)
            tools: List of tool/action names to make available
            max_steps: Maximum reasoning steps (default: 10)
            signature: DSPy signature string
            temperature: LLM temperature (default: 0.7)
            **kwargs: Additional parameters

        Returns:
            ReAct result with steps and final_answer, or falls back to native
        """
        if not _dspy_available():
            result = reason_react(
                state=state,
                goal=goal,
                model=model,
                tools=tools,
                max_steps=max_steps,
                temperature=temperature,
                **kwargs,
            )
            result["dspy_module"] = "native_fallback"
            return result

        try:
            import dspy
            import json

            trace = []
            start_time = time.time()

            trace.append(
                {
                    "step": "dspy_react_start",
                    "timestamp": time.time(),
                    "goal": goal,
                    "model": model,
                    "tools": tools,
                    "max_steps": max_steps,
                }
            )

            # Configure DSPy
            if model.startswith("gpt-") or model.startswith("o1"):
                lm = dspy.LM(f"openai/{model}", temperature=temperature)
            else:
                lm = dspy.LM(model, temperature=temperature)
            dspy.configure(lm=lm)

            # Convert tools to DSPy tools
            available_tools = tools or []
            dspy_tools = []

            for tool_name in available_tools:
                action_func = registry.get(tool_name)
                if action_func:
                    # Create a DSPy-compatible tool wrapper
                    def make_tool_wrapper(fn, name):
                        def wrapper(**tool_kwargs):
                            try:
                                result = fn(state=state, **tool_kwargs)
                                return json.dumps(result, default=str)
                            except Exception as e:
                                return json.dumps({"error": str(e)})

                        wrapper.__name__ = name
                        wrapper.__doc__ = (
                            getattr(fn, "__doc__", f"Execute {name}")
                            or f"Execute {name}"
                        )
                        return wrapper

                    dspy_tools.append(make_tool_wrapper(action_func, tool_name))

            if not dspy_tools:
                # No valid tools, fall back to native
                result = reason_react(
                    state=state,
                    goal=goal,
                    model=model,
                    tools=tools,
                    max_steps=max_steps,
                    temperature=temperature,
                    **kwargs,
                )
                result["dspy_module"] = "native_fallback"
                result["dspy_reason"] = "No valid tools for DSPy"
                return result

            # Create and run DSPy ReAct
            sig = signature or "goal -> final_answer"
            react = dspy.ReAct(sig, tools=dspy_tools, max_iters=max_steps)
            result = react(goal=goal)

            final_answer = getattr(result, "final_answer", str(result))

            elapsed = time.time() - start_time
            trace.append(
                {
                    "step": "dspy_react_complete",
                    "timestamp": time.time(),
                    "elapsed_seconds": elapsed,
                    "final_answer_length": len(str(final_answer)),
                }
            )

            return {
                "final_answer": final_answer,
                "steps": [],  # DSPy doesn't expose step trace directly
                "react_steps": [],
                "reasoning_answer": final_answer,
                "reasoning_trace": trace,
                "dspy_module": "ReAct",
                "model": model,
                "total_steps": 0,  # Unknown from DSPy
            }

        except Exception as e:
            logger.warning(f"DSPy ReAct failed, falling back to native: {e}")
            result = reason_react(
                state=state,
                goal=goal,
                model=model,
                tools=tools,
                max_steps=max_steps,
                temperature=temperature,
                **kwargs,
            )
            result["dspy_module"] = "native_fallback"
            result["dspy_error"] = str(e)
            return result

    registry["reason.dspy.react"] = reason_dspy_react
    registry["actions.reason_dspy_react"] = reason_dspy_react

    def reason_dspy_compile(
        state: Dict[str, Any],
        module_type: str = "cot",
        signature: Optional[str] = None,
        training_data: Optional[List[Dict[str, Any]]] = None,
        teleprompter: str = "BootstrapFewShot",
        model: str = "gpt-4",
        metric: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Compile DSPy module with teleprompter for optimized prompts.

        Uses DSPy's compilation capabilities to optimize prompts based on
        training examples. Compiled prompts can be persisted and reused.

        Args:
            state: Current state dictionary
            module_type: Type of module to compile ("cot", "react")
            signature: DSPy signature string
            training_data: List of example dicts for optimization
            teleprompter: Teleprompter to use (BootstrapFewShot, BootstrapFewShotWithRandomSearch)
            model: Model for compilation (default: gpt-4)
            metric: Metric function name or "exact_match"
            **kwargs: Additional parameters

        Returns:
            {
                "compiled": True,
                "module_type": str,
                "teleprompter": str,
                "training_examples": int,
                "success": True
            }
            Or {"error": str, "success": False} if DSPy unavailable
        """
        if not _dspy_available():
            return {
                "error": "DSPy not installed. Install with: pip install dspy-ai",
                "success": False,
                "compiled": False,
            }

        try:
            import dspy

            trace = []
            start_time = time.time()

            trace.append(
                {
                    "step": "dspy_compile_start",
                    "timestamp": time.time(),
                    "module_type": module_type,
                    "teleprompter": teleprompter,
                    "training_examples": len(training_data) if training_data else 0,
                }
            )

            # Configure model
            if model.startswith("gpt-") or model.startswith("o1"):
                lm = dspy.LM(f"openai/{model}")
            else:
                lm = dspy.LM(model)
            dspy.configure(lm=lm)

            # Create signature
            sig = signature or "question -> answer"

            # Create module based on type
            if module_type == "cot":
                module = dspy.ChainOfThought(sig)
            elif module_type == "react":
                module = dspy.ReAct(sig)
            elif module_type == "predict":
                module = dspy.Predict(sig)
            else:
                return {
                    "error": f"Unknown module type: {module_type}. Use 'cot', 'react', or 'predict'",
                    "success": False,
                    "compiled": False,
                }

            # Prepare training data
            trainset = []
            if training_data:
                for ex in training_data:
                    # Convert dict to DSPy Example
                    example = dspy.Example(**ex)
                    # Mark inputs based on signature
                    if "question" in ex:
                        example = example.with_inputs("question")
                    elif "goal" in ex:
                        example = example.with_inputs("goal")
                    elif "input" in ex:
                        example = example.with_inputs("input")
                    trainset.append(example)

            if not trainset:
                return {
                    "error": "No training data provided for compilation",
                    "success": False,
                    "compiled": False,
                }

            # Define metric
            def default_metric(example, prediction, trace=None):
                """Default metric - checks if answer is present and non-empty."""
                pred_answer = getattr(prediction, "answer", None)
                if pred_answer is None:
                    return 0.0
                return 1.0 if len(str(pred_answer)) > 0 else 0.0

            def exact_match_metric(example, prediction, trace=None):
                """Exact match metric for compilation."""
                expected = getattr(example, "answer", None)
                predicted = getattr(prediction, "answer", None)
                if expected is None or predicted is None:
                    return 0.0
                return 1.0 if str(expected).strip() == str(predicted).strip() else 0.0

            if metric == "exact_match":
                metric_fn = exact_match_metric
            else:
                metric_fn = default_metric

            # Get teleprompter
            if teleprompter == "BootstrapFewShot":
                tp = dspy.BootstrapFewShot(metric=metric_fn)
            elif teleprompter == "BootstrapFewShotWithRandomSearch":
                tp = dspy.BootstrapFewShotWithRandomSearch(
                    metric=metric_fn, num_candidate_programs=5
                )
            elif teleprompter == "MIPRO":
                tp = dspy.MIPROv2(metric=metric_fn, num_candidates=5)
            else:
                return {
                    "error": f"Unknown teleprompter: {teleprompter}",
                    "success": False,
                    "compiled": False,
                }

            # Compile
            compiled_module = tp.compile(module, trainset=trainset)

            # Store compiled module in state for later use
            module_key = f"dspy_{module_type}_{signature or 'default'}"
            if "dspy_modules" not in state:
                state["dspy_modules"] = {}
            state["dspy_modules"][module_key] = compiled_module

            elapsed = time.time() - start_time
            trace.append(
                {
                    "step": "dspy_compile_complete",
                    "timestamp": time.time(),
                    "elapsed_seconds": elapsed,
                    "module_key": module_key,
                }
            )

            return {
                "compiled": True,
                "module_type": module_type,
                "signature": sig,
                "teleprompter": teleprompter,
                "training_examples": len(trainset),
                "module_key": module_key,
                "reasoning_trace": trace,
                "success": True,
            }

        except Exception as e:
            logger.error(f"DSPy compilation failed: {e}")
            return {
                "error": f"DSPy compilation failed: {str(e)}",
                "success": False,
                "compiled": False,
            }

    registry["reason.dspy.compile"] = reason_dspy_compile
    registry["actions.reason_dspy_compile"] = reason_dspy_compile
