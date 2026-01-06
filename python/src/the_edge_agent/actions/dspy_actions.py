"""
DSPy Actions for YAMLEngine (TEA-AGENT-001.7).

This module provides DSPy integration actions for declarative prompt optimization:

- reason.dspy.cot: Chain-of-Thought with DSPy ChainOfThought module
- reason.dspy.react: ReAct with DSPy ReAct module and tool integration
- reason.dspy.compile: Compile modules with teleprompters
- reason.dspy.optimize: Run optimization against validation set

All actions provide graceful fallback to native `reason.*` actions when DSPy
is unavailable, ensuring agents work across environments.

Example:
    >>> # Chain-of-Thought with DSPy
    >>> result = registry['reason.dspy.cot'](
    ...     state={},
    ...     problem="What is 15% of 80?",
    ...     signature="question -> thinking, answer",
    ...     model="gpt-4"
    ... )
    >>> print(result['thinking'])
    >>> print(result['answer'])

    >>> # Compile with DSPy
    >>> result = registry['reason.dspy.compile'](
    ...     state={},
    ...     module_type="cot",
    ...     signature="question -> answer",
    ...     training_data=[{"question": "2+2?", "answer": "4"}],
    ...     teleprompter="BootstrapFewShot"
    ... )
"""

import json
import logging
import time
from typing import Any, Callable, Dict, List, Optional

from ..reasoning.dspy_client import (
    DSPyClient,
    DSPyConfig,
    CompiledPrompt,
    dspy_available,
)

logger = logging.getLogger(__name__)


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register DSPy actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    # Get or create DSPy client from engine settings
    def _get_dspy_client() -> DSPyClient:
        """Get DSPy client from engine or create new one."""
        if hasattr(engine, "_dspy_client") and engine._dspy_client is not None:
            return engine._dspy_client

        # Get settings from engine
        settings = {}
        if hasattr(engine, "settings") and engine.settings:
            settings = engine.settings

        client = DSPyClient.from_settings(settings)
        engine._dspy_client = client
        return client

    def _get_native_cot() -> Optional[Callable]:
        """Get native reason.cot action for fallback."""
        return registry.get("reason.cot")

    def _get_native_react() -> Optional[Callable]:
        """Get native reason.react action for fallback."""
        return registry.get("reason.react")

    def reason_dspy_cot(
        state: Dict[str, Any],
        problem: str,
        signature: str = "question -> thinking, answer",
        model: Optional[str] = None,
        few_shot_examples: Optional[List[Dict]] = None,
        temperature: float = 0.7,
        compiled_key: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Chain-of-Thought reasoning using DSPy ChainOfThought module.

        Wraps DSPy's ChainOfThought for model-agnostic CoT prompting with
        compiled/optimized prompts. Falls back to native reason.cot when
        DSPy is unavailable.

        Args:
            state: Current state dictionary
            problem: The problem or question to reason about
            signature: DSPy signature string (default: "question -> thinking, answer")
            model: LLM model to use (default: from settings or gpt-4)
            few_shot_examples: Optional list of example dicts
            temperature: LLM temperature (default: 0.7)
            compiled_key: Key for pre-compiled module to use
            **kwargs: Additional parameters

        Returns:
            {
                "thinking": str,           # Chain-of-thought reasoning
                "answer": any,             # Final answer
                "reasoning_trace": list,   # Trace for observability
                "dspy_module": str,        # "ChainOfThought" or "native_fallback"
                "model": str
            }
            Or fallback to native reason.cot result if DSPy unavailable
        """
        client = _get_dspy_client()
        trace = []
        start_time = time.time()

        # Check for DSPy availability
        if not client.is_available:
            native_cot = _get_native_cot()
            if native_cot:
                logger.debug("DSPy unavailable, using native reason.cot")
                result = native_cot(
                    state=state,
                    problem=problem,
                    few_shot_examples=few_shot_examples,
                    temperature=temperature,
                    **kwargs,
                )
                result["dspy_module"] = "native_fallback"
                result["dspy_available"] = False
                return result
            else:
                return {
                    "error": "DSPy unavailable and no native fallback found",
                    "success": False,
                    "dspy_available": False,
                }

        try:
            trace.append(
                {
                    "step": "dspy_cot_start",
                    "timestamp": time.time(),
                    "problem": problem[:200] + "..." if len(problem) > 200 else problem,
                    "signature": signature,
                    "model": model,
                }
            )

            # Override model in config if specified
            if model:
                client.config.model = model
            if temperature:
                client.config.temperature = temperature

            # Get compiled module or create new one
            compiled_prompt = None
            if compiled_key:
                compiled_prompt = client.get_compiled_prompt(compiled_key)

            # Create few-shot examples as CompiledPrompt demos
            if few_shot_examples and not compiled_prompt:
                compiled_prompt = CompiledPrompt(
                    module_type="cot",
                    signature=signature,
                    version="1.0",
                    demos=few_shot_examples,
                )

            # Get ChainOfThought module
            cot = client.get_chain_of_thought(signature, compiled_prompt)
            if cot is None:
                # Fallback to native
                native_cot = _get_native_cot()
                if native_cot:
                    result = native_cot(
                        state=state,
                        problem=problem,
                        few_shot_examples=few_shot_examples,
                        temperature=temperature,
                        **kwargs,
                    )
                    result["dspy_module"] = "native_fallback"
                    result["dspy_error"] = "Failed to create ChainOfThought module"
                    return result
                else:
                    return {
                        "error": "Failed to create ChainOfThought and no fallback",
                        "success": False,
                    }

            # Execute
            result = cot(question=problem)

            # Extract rationale and answer
            thinking = (
                getattr(result, "rationale", "")
                or getattr(result, "thinking", "")
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
                "dspy_available": True,
                "model": client.config.model,
                "signature": signature,
            }

        except Exception as e:
            logger.warning(f"DSPy ChainOfThought failed, falling back to native: {e}")
            trace.append(
                {"step": "dspy_cot_error", "timestamp": time.time(), "error": str(e)}
            )

            native_cot = _get_native_cot()
            if native_cot:
                result = native_cot(
                    state=state,
                    problem=problem,
                    few_shot_examples=few_shot_examples,
                    temperature=temperature,
                    **kwargs,
                )
                result["dspy_module"] = "native_fallback"
                result["dspy_error"] = str(e)
                return result
            else:
                return {
                    "error": f"DSPy failed and no fallback: {e}",
                    "success": False,
                    "reasoning_trace": trace,
                }

    registry["reason.dspy.cot"] = reason_dspy_cot
    registry["actions.reason_dspy_cot"] = reason_dspy_cot

    def reason_dspy_react(
        state: Dict[str, Any],
        goal: str,
        signature: str = "goal -> result",
        model: Optional[str] = None,
        tools: Optional[List[str]] = None,
        max_steps: int = 10,
        temperature: float = 0.7,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        ReAct reasoning using DSPy ReAct module.

        Wraps DSPy's ReAct for tool-using agents with compiled prompts.
        Integrates with TEA tool bridges (MCP, CrewAI, LangChain).
        Falls back to native reason.react when DSPy is unavailable.

        Args:
            state: Current state dictionary
            goal: The goal to achieve
            signature: DSPy signature string (default: "goal -> result")
            model: LLM model to use
            tools: List of tool/action names to make available
            max_steps: Maximum reasoning steps (default: 10)
            temperature: LLM temperature (default: 0.7)
            **kwargs: Additional parameters

        Returns:
            {
                "steps": list,             # Action-observation trace
                "final_answer": any,       # Final result
                "reasoning_trace": list,   # Trace for observability
                "dspy_module": str,        # "ReAct" or "native_fallback"
                "model": str,
                "total_steps": int
            }
        """
        client = _get_dspy_client()
        trace = []
        start_time = time.time()

        # Check for DSPy availability
        if not client.is_available:
            native_react = _get_native_react()
            if native_react:
                logger.debug("DSPy unavailable, using native reason.react")
                result = native_react(
                    state=state,
                    goal=goal,
                    tools=tools,
                    max_steps=max_steps,
                    temperature=temperature,
                    **kwargs,
                )
                result["dspy_module"] = "native_fallback"
                result["dspy_available"] = False
                return result
            else:
                return {
                    "error": "DSPy unavailable and no native fallback found",
                    "success": False,
                    "dspy_available": False,
                }

        try:
            trace.append(
                {
                    "step": "dspy_react_start",
                    "timestamp": time.time(),
                    "goal": goal[:200] + "..." if len(goal) > 200 else goal,
                    "signature": signature,
                    "tools": tools,
                    "max_steps": max_steps,
                }
            )

            # Override model in config if specified
            if model:
                client.config.model = model
            if temperature:
                client.config.temperature = temperature

            # Convert tools to DSPy-compatible functions
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

            if not dspy_tools and available_tools:
                # Tools specified but none found, fall back to native
                logger.warning(f"No valid tools found for DSPy: {available_tools}")
                native_react = _get_native_react()
                if native_react:
                    result = native_react(
                        state=state,
                        goal=goal,
                        tools=tools,
                        max_steps=max_steps,
                        temperature=temperature,
                        **kwargs,
                    )
                    result["dspy_module"] = "native_fallback"
                    result["dspy_reason"] = "No valid tools for DSPy"
                    return result

            # Get ReAct module
            react = client.get_react(signature, dspy_tools, max_steps)
            if react is None:
                native_react = _get_native_react()
                if native_react:
                    result = native_react(
                        state=state,
                        goal=goal,
                        tools=tools,
                        max_steps=max_steps,
                        temperature=temperature,
                        **kwargs,
                    )
                    result["dspy_module"] = "native_fallback"
                    result["dspy_error"] = "Failed to create ReAct module"
                    return result
                else:
                    return {
                        "error": "Failed to create ReAct and no fallback",
                        "success": False,
                    }

            # Execute
            result = react(goal=goal)

            final_answer = (
                getattr(result, "result", None)
                or getattr(result, "final_answer", None)
                or str(result)
            )

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
                "dspy_available": True,
                "model": client.config.model,
                "total_steps": 0,  # Unknown from DSPy
                "signature": signature,
            }

        except Exception as e:
            logger.warning(f"DSPy ReAct failed, falling back to native: {e}")
            trace.append(
                {"step": "dspy_react_error", "timestamp": time.time(), "error": str(e)}
            )

            native_react = _get_native_react()
            if native_react:
                result = native_react(
                    state=state,
                    goal=goal,
                    tools=tools,
                    max_steps=max_steps,
                    temperature=temperature,
                    **kwargs,
                )
                result["dspy_module"] = "native_fallback"
                result["dspy_error"] = str(e)
                return result
            else:
                return {
                    "error": f"DSPy failed and no fallback: {e}",
                    "success": False,
                    "reasoning_trace": trace,
                }

    registry["reason.dspy.react"] = reason_dspy_react
    registry["actions.reason_dspy_react"] = reason_dspy_react

    def reason_dspy_compile(
        state: Dict[str, Any],
        module_type: str = "cot",
        signature: str = "question -> answer",
        training_data: Optional[List[Dict[str, Any]]] = None,
        teleprompter: str = "BootstrapFewShot",
        model: Optional[str] = None,
        metric: Optional[str] = None,
        output_key: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Compile a DSPy module with teleprompter for optimized prompts.

        Uses DSPy's compilation capabilities to optimize prompts based on
        training examples. Compiled prompts can be persisted and reused.

        Args:
            state: Current state dictionary
            module_type: Type of module to compile ("cot", "react", "predict")
            signature: DSPy signature string
            training_data: List of example dicts for optimization (or from state)
            teleprompter: Teleprompter to use (BootstrapFewShot, BootstrapFewShotWithRandomSearch, MIPRO)
            model: Model for compilation
            metric: Metric function name ("exact_match" or custom)
            output_key: Key to store compiled module in state
            **kwargs: Additional teleprompter parameters

        Returns:
            {
                "compiled": True,
                "module_type": str,
                "signature": str,
                "teleprompter": str,
                "training_examples": int,
                "module_key": str,       # Key for retrieving compiled module
                "success": True
            }
            Or {"error": str, "success": False} if compilation fails
        """
        client = _get_dspy_client()
        trace = []
        start_time = time.time()

        if not client.is_available:
            return {
                "error": "DSPy not installed. Install with: pip install the_edge_agent[dspy]",
                "success": False,
                "compiled": False,
                "dspy_available": False,
            }

        try:
            trace.append(
                {
                    "step": "dspy_compile_start",
                    "timestamp": time.time(),
                    "module_type": module_type,
                    "signature": signature,
                    "teleprompter": teleprompter,
                    "training_examples": len(training_data) if training_data else 0,
                }
            )

            # Override model if specified
            if model:
                client.config.model = model

            # Get training data from state if not provided
            trainset = training_data
            if trainset is None:
                trainset = (
                    state.get("training_data")
                    or state.get("trainset")
                    or state.get("examples")
                )

            if not trainset:
                return {
                    "error": "No training data provided for compilation. Provide 'training_data' parameter or set in state.",
                    "success": False,
                    "compiled": False,
                }

            # Create base module
            import dspy

            if module_type == "cot":
                module = dspy.ChainOfThought(signature)
            elif module_type == "react":
                module = dspy.ReAct(signature)
            elif module_type == "predict":
                module = dspy.Predict(signature)
            else:
                return {
                    "error": f"Unknown module type: {module_type}. Use 'cot', 'react', or 'predict'.",
                    "success": False,
                    "compiled": False,
                }

            # Define metric
            def exact_match_metric(example, prediction, trace=None):
                expected = getattr(example, "answer", None) or getattr(
                    example, "result", None
                )
                predicted = getattr(prediction, "answer", None) or getattr(
                    prediction, "result", None
                )
                if expected is None or predicted is None:
                    return 0.0
                return 1.0 if str(expected).strip() == str(predicted).strip() else 0.0

            def presence_metric(example, prediction, trace=None):
                predicted = getattr(prediction, "answer", None) or getattr(
                    prediction, "result", None
                )
                return 1.0 if predicted and len(str(predicted)) > 0 else 0.0

            if metric == "exact_match":
                metric_fn = exact_match_metric
            else:
                metric_fn = presence_metric

            # Compile
            compiled_module = client.compile(
                module, trainset, teleprompter=teleprompter, metric=metric_fn, **kwargs
            )

            if compiled_module is None:
                return {
                    "error": "Compilation failed",
                    "success": False,
                    "compiled": False,
                }

            # Generate module key
            module_key = (
                output_key
                or f"dspy_{module_type}_{signature.replace(' ', '_').replace('->', '_')}_{int(time.time())}"
            )

            # Create compiled prompt for persistence
            compiled_prompt = CompiledPrompt(
                module_type=module_type,
                signature=signature,
                version=CompiledPrompt.compute_version(signature, trainset),
                demos=trainset[:10],  # Store up to 10 demos
                metadata={
                    "teleprompter": teleprompter,
                    "training_examples": len(trainset),
                    "compiled_at": time.time(),
                    "model": client.config.model,
                },
            )

            # Store in client
            client.store_compiled_module(module_key, compiled_module, compiled_prompt)

            # Also store key in state for later retrieval
            if "dspy_compiled_keys" not in state:
                state["dspy_compiled_keys"] = []
            state["dspy_compiled_keys"].append(module_key)

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
                "signature": signature,
                "teleprompter": teleprompter,
                "training_examples": len(trainset),
                "module_key": module_key,
                "compiled_prompt_version": compiled_prompt.version,
                "reasoning_trace": trace,
                "success": True,
                "dspy_available": True,
            }

        except Exception as e:
            logger.error(f"DSPy compilation failed: {e}")
            trace.append(
                {
                    "step": "dspy_compile_error",
                    "timestamp": time.time(),
                    "error": str(e),
                }
            )
            return {
                "error": f"DSPy compilation failed: {str(e)}",
                "success": False,
                "compiled": False,
                "reasoning_trace": trace,
            }

    registry["reason.dspy.compile"] = reason_dspy_compile
    registry["actions.reason_dspy_compile"] = reason_dspy_compile

    def reason_dspy_optimize(
        state: Dict[str, Any],
        module_key: Optional[str] = None,
        module_type: str = "cot",
        signature: str = "question -> answer",
        training_data: Optional[List[Dict[str, Any]]] = None,
        validation_data: Optional[List[Dict[str, Any]]] = None,
        metric: Optional[str] = None,
        model: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Run optimization against a validation set.

        Compiles and optimizes a DSPy module, evaluating against validation
        examples to find the best configuration.

        Args:
            state: Current state dictionary
            module_key: Key of pre-compiled module to optimize (or create new)
            module_type: Type of module to optimize ("cot", "react", "predict")
            signature: DSPy signature string
            training_data: Training examples (or from state)
            validation_data: Validation examples (or from state)
            metric: Metric function ("exact_match" or custom)
            model: Model for optimization
            **kwargs: Additional optimization parameters

        Returns:
            {
                "success": True,
                "train_score": float,
                "val_score": float,
                "training_examples": int,
                "validation_examples": int,
                "module_key": str,
                "best_config": dict
            }
            Or {"error": str, "success": False} if optimization fails
        """
        client = _get_dspy_client()
        trace = []
        start_time = time.time()

        if not client.is_available:
            return {
                "error": "DSPy not installed. Install with: pip install the_edge_agent[dspy]",
                "success": False,
                "dspy_available": False,
            }

        try:
            # Get training and validation data
            trainset = (
                training_data or state.get("training_data") or state.get("trainset")
            )
            valset = (
                validation_data or state.get("validation_data") or state.get("valset")
            )

            if not trainset:
                return {
                    "error": "No training data provided. Provide 'training_data' parameter or set in state.",
                    "success": False,
                }

            if not valset:
                return {
                    "error": "No validation data provided. Provide 'validation_data' parameter or set in state.",
                    "success": False,
                }

            trace.append(
                {
                    "step": "dspy_optimize_start",
                    "timestamp": time.time(),
                    "module_type": module_type,
                    "signature": signature,
                    "training_examples": len(trainset),
                    "validation_examples": len(valset),
                }
            )

            # Override model if specified
            if model:
                client.config.model = model

            # Get or create base module
            import dspy

            base_module = None
            if module_key:
                base_module = client.get_compiled_module(module_key)

            if base_module is None:
                if module_type == "cot":
                    base_module = dspy.ChainOfThought(signature)
                elif module_type == "react":
                    base_module = dspy.ReAct(signature)
                elif module_type == "predict":
                    base_module = dspy.Predict(signature)
                else:
                    return {
                        "error": f"Unknown module type: {module_type}",
                        "success": False,
                    }

            # Define metric
            def exact_match_metric(example, prediction, trace=None):
                expected = getattr(example, "answer", None) or getattr(
                    example, "result", None
                )
                predicted = getattr(prediction, "answer", None) or getattr(
                    prediction, "result", None
                )
                if expected is None or predicted is None:
                    return 0.0
                return 1.0 if str(expected).strip() == str(predicted).strip() else 0.0

            metric_fn = exact_match_metric if metric == "exact_match" else None

            # Run optimization
            result = client.optimize(
                base_module, trainset, valset, metric=metric_fn, **kwargs
            )

            if not result.get("success"):
                return {
                    "error": result.get("error", "Optimization failed"),
                    "success": False,
                    "reasoning_trace": trace,
                }

            # Store optimized module
            new_key = f"dspy_optimized_{module_type}_{int(time.time())}"
            compiled_prompt = CompiledPrompt(
                module_type=module_type,
                signature=signature,
                version=CompiledPrompt.compute_version(signature, trainset),
                demos=trainset[:10],
                metadata={
                    "train_score": result.get("train_score"),
                    "val_score": result.get("val_score"),
                    "optimized_at": time.time(),
                    "model": client.config.model,
                },
            )

            client.store_compiled_module(
                new_key, result.get("compiled_module"), compiled_prompt
            )

            # Update state
            state["dspy_optimized_module_key"] = new_key
            if "dspy_compiled_keys" not in state:
                state["dspy_compiled_keys"] = []
            state["dspy_compiled_keys"].append(new_key)

            elapsed = time.time() - start_time
            trace.append(
                {
                    "step": "dspy_optimize_complete",
                    "timestamp": time.time(),
                    "elapsed_seconds": elapsed,
                    "train_score": result.get("train_score"),
                    "val_score": result.get("val_score"),
                }
            )

            return {
                "success": True,
                "train_score": result.get("train_score"),
                "val_score": result.get("val_score"),
                "training_examples": result.get("training_examples"),
                "validation_examples": result.get("validation_examples"),
                "module_key": new_key,
                "best_config": {
                    "module_type": module_type,
                    "signature": signature,
                    "model": client.config.model,
                },
                "reasoning_trace": trace,
                "dspy_available": True,
            }

        except Exception as e:
            logger.error(f"DSPy optimization failed: {e}")
            trace.append(
                {
                    "step": "dspy_optimize_error",
                    "timestamp": time.time(),
                    "error": str(e),
                }
            )
            return {
                "error": f"DSPy optimization failed: {str(e)}",
                "success": False,
                "reasoning_trace": trace,
            }

    registry["reason.dspy.optimize"] = reason_dspy_optimize
    registry["actions.reason_dspy_optimize"] = reason_dspy_optimize

    # Helper action to list compiled modules
    def reason_dspy_list_compiled(state: Dict[str, Any], **kwargs) -> Dict[str, Any]:
        """
        List all compiled DSPy modules.

        Returns:
            {
                "modules": list of module keys,
                "details": dict of key -> metadata
            }
        """
        client = _get_dspy_client()

        if not client.is_available:
            return {"modules": [], "details": {}, "dspy_available": False}

        keys = client.list_compiled_modules()
        details = {}

        for key in keys:
            prompt = client.get_compiled_prompt(key)
            if prompt:
                details[key] = {
                    "module_type": prompt.module_type,
                    "signature": prompt.signature,
                    "version": prompt.version,
                    "metadata": prompt.metadata,
                }

        return {"modules": keys, "details": details, "dspy_available": True}

    registry["reason.dspy.list_compiled"] = reason_dspy_list_compiled
    registry["actions.reason_dspy_list_compiled"] = reason_dspy_list_compiled

    # Export/import actions for checkpoint integration
    def reason_dspy_export(state: Dict[str, Any], **kwargs) -> Dict[str, Any]:
        """
        Export all compiled DSPy prompts for checkpoint persistence.

        Returns:
            {
                "prompts": dict of key -> prompt config,
                "count": int
            }
        """
        client = _get_dspy_client()

        if not client.is_available:
            return {"prompts": {}, "count": 0, "dspy_available": False}

        prompts = client.export_compiled_prompts()

        return {"prompts": prompts, "count": len(prompts), "dspy_available": True}

    registry["reason.dspy.export"] = reason_dspy_export
    registry["actions.reason_dspy_export"] = reason_dspy_export

    def reason_dspy_import(
        state: Dict[str, Any], prompts: Optional[Dict[str, Dict]] = None, **kwargs
    ) -> Dict[str, Any]:
        """
        Import compiled DSPy prompts from checkpoint persistence.

        Args:
            state: Current state dictionary
            prompts: Dictionary of key -> prompt config (or from state)

        Returns:
            {
                "imported": int,
                "keys": list of imported keys
            }
        """
        client = _get_dspy_client()

        if not client.is_available:
            return {"imported": 0, "keys": [], "dspy_available": False}

        # Get prompts from state if not provided
        data = prompts or state.get("dspy_prompts") or state.get("compiled_prompts")

        if not data:
            return {"imported": 0, "keys": [], "error": "No prompts data provided"}

        count = client.import_compiled_prompts(data)

        return {"imported": count, "keys": list(data.keys()), "dspy_available": True}

    registry["reason.dspy.import"] = reason_dspy_import
    registry["actions.reason_dspy_import"] = reason_dspy_import
