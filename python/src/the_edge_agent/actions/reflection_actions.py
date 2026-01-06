"""
Reflection Actions for YAML Agents (TEA-AGENT-001.2).

Provides the reflection loop primitive for self-correcting agents:
- reflection.loop: Automatic generate→evaluate→correct cycle
- reflection.evaluate: Standalone evaluation action
- reflection.correct: Standalone correction action

Evaluator types:
- schema: JSON Schema validation
- llm: LLM-as-judge evaluation
- custom: Inline Python/Lua/Prolog code

On-failure strategies:
- return_best: Return highest-scoring attempt
- return_last: Return final attempt
- raise: Raise ReflectionFailedError with history

Usage in YAML:
    nodes:
      - name: generate_with_reflection
        action: reflection.loop
        with:
          generator:
            action: llm.call
            prompt: "Generate JSON..."
          evaluator:
            type: schema
            schema:
              type: object
              required: [name, email]
          corrector:
            action: llm.call
            prompt: |
              Fix this JSON based on errors:
              Original: {{ state.reflection_output }}
              Errors: {{ state.reflection_errors | tojson }}
          max_iterations: 3
          on_failure: return_best

State Variables Set:
    reflection_iteration: int    - Current iteration (1-based)
    reflection_output: any       - Current generator output
    reflection_errors: list      - Evaluation errors from current iteration
    reflection_history: list     - All attempts with outputs and scores
    reflection_best: any         - Best output so far (for return_best)
    reflection_best_score: float - Score of best output
"""

import copy
from typing import Any, Callable, Dict, List, Optional, Union


class ReflectionFailedError(Exception):
    """Raised when reflection loop exhausts max_iterations with on_failure='raise'."""

    def __init__(self, message: str, history: List[Dict[str, Any]]):
        super().__init__(message)
        self.history = history


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register reflection actions in the YAMLEngine registry.

    Actions registered:
        - reflection.loop: Execute generate→evaluate→correct cycle
        - reflection.evaluate: Standalone evaluation
        - reflection.correct: Standalone correction

    Args:
        registry: Action registry to populate
        engine: YAMLEngine instance for accessing runtime and settings
    """

    def _execute_generator(
        state: Dict[str, Any],
        generator_config: Dict[str, Any],
        registry: Dict[str, Callable],
        engine: Any,
    ) -> Any:
        """
        Execute the generator to produce output.

        Supports:
        - action: Reference to another action (e.g., llm.call)
        - run: Inline Python code

        Returns the generator output.
        """
        if "action" in generator_config:
            action_name = generator_config["action"]
            if action_name not in registry:
                raise ValueError(
                    f"reflection.loop: Unknown generator action: {action_name}"
                )

            action_func = registry[action_name]
            # Extract action params (excluding 'action' key)
            action_params = {k: v for k, v in generator_config.items() if k != "action"}

            # Process templates in params
            processed_params = _process_template_params(action_params, state, engine)

            result = action_func(state=state, **processed_params)
            return result

        elif "run" in generator_config:
            # Inline Python code
            code = generator_config["run"]
            return _execute_inline_code(code, state, engine, "generator")

        else:
            raise ValueError(
                "reflection.loop: Generator must have 'action' or 'run' key"
            )

    def _execute_corrector(
        state: Dict[str, Any],
        corrector_config: Dict[str, Any],
        registry: Dict[str, Callable],
        engine: Any,
    ) -> Any:
        """
        Execute the corrector to fix output based on evaluation errors.

        Supports:
        - action: Reference to another action (e.g., llm.call)
        - run: Inline Python code

        Returns the corrected output.
        """
        if "action" in corrector_config:
            action_name = corrector_config["action"]
            if action_name not in registry:
                raise ValueError(
                    f"reflection.loop: Unknown corrector action: {action_name}"
                )

            action_func = registry[action_name]
            # Extract action params (excluding 'action' key)
            action_params = {k: v for k, v in corrector_config.items() if k != "action"}

            # Process templates in params
            processed_params = _process_template_params(action_params, state, engine)

            result = action_func(state=state, **processed_params)
            return result

        elif "run" in corrector_config:
            # Inline Python code
            code = corrector_config["run"]
            return _execute_inline_code(code, state, engine, "corrector")

        else:
            raise ValueError(
                "reflection.loop: Corrector must have 'action' or 'run' key"
            )

    def _evaluate_with_schema(
        state: Dict[str, Any],
        output: Any,
        schema_config: Dict[str, Any],
        engine: Any,
    ) -> Dict[str, Any]:
        """
        Evaluate output against JSON Schema (AC: 2).

        Returns:
            {
                "valid": bool,
                "score": float,  # 1.0 if valid, 0.0 if invalid
                "errors": List[Dict],
                "suggestions": List[str]
            }
        """
        try:
            import jsonschema
            from jsonschema import Draft7Validator
        except ImportError:
            return {
                "valid": False,
                "score": 0.0,
                "errors": [{"message": "jsonschema library not installed"}],
                "suggestions": ["Install with: pip install jsonschema"],
            }

        # Handle $ref for external schemas
        schema = _resolve_schema_refs(schema_config, engine)

        # Attempt type coercion before validation
        coerced_output = _attempt_type_coercion(output, schema)

        try:
            validator = Draft7Validator(schema)
            errors = []
            suggestions = []

            for error in validator.iter_errors(coerced_output):
                path = (
                    ".".join(str(p) for p in error.absolute_path)
                    if error.absolute_path
                    else ""
                )
                errors.append(
                    {
                        "path": path,
                        "message": error.message,
                        "schema_path": list(error.schema_path),
                    }
                )
                # Generate suggestions based on error type
                if "required" in error.message.lower():
                    suggestions.append(
                        f"Add missing required field at path: {path or 'root'}"
                    )
                elif "type" in error.message.lower():
                    suggestions.append(f"Fix type at path: {path or 'root'}")

            is_valid = len(errors) == 0
            return {
                "valid": is_valid,
                "score": 1.0 if is_valid else 0.0,
                "errors": errors,
                "suggestions": suggestions,
            }

        except jsonschema.SchemaError as e:
            return {
                "valid": False,
                "score": 0.0,
                "errors": [{"message": f"Invalid schema: {e.message}"}],
                "suggestions": ["Fix the JSON Schema definition"],
            }

    def _evaluate_with_llm(
        state: Dict[str, Any],
        output: Any,
        evaluator_config: Dict[str, Any],
        registry: Dict[str, Callable],
        engine: Any,
    ) -> Dict[str, Any]:
        """
        Evaluate output using LLM as judge (AC: 3).

        Returns:
            {
                "valid": bool,
                "score": float,  # 0.0-1.0 from LLM
                "errors": List[Dict],
                "suggestions": List[str]
            }
        """
        llm_call = registry.get("llm.call")
        if llm_call is None:
            return {
                "valid": False,
                "score": 0.0,
                "errors": [{"message": "llm.call action not available"}],
                "suggestions": ["Register llm.call action"],
            }

        # Build evaluation prompt
        prompt = evaluator_config.get(
            "prompt", "Evaluate if this output is valid and correct."
        )

        # Process template in prompt
        eval_state = copy.copy(state)
        eval_state["output_to_evaluate"] = output
        processed_prompt = _process_template(prompt, eval_state, engine)

        # Build messages for LLM
        system_message = """You are an evaluation assistant. Evaluate the given output and respond with JSON:
{
  "valid": true/false,
  "score": 0.0-1.0,
  "reason": "explanation",
  "suggestions": ["suggestion1", "suggestion2"]
}
Only respond with valid JSON, no other text."""

        # Include few-shot examples if provided
        messages = [{"role": "system", "content": system_message}]

        examples = evaluator_config.get("examples", [])
        for example in examples:
            if "input" in example and "output" in example:
                messages.append({"role": "user", "content": str(example["input"])})
                messages.append(
                    {"role": "assistant", "content": str(example["output"])}
                )

        messages.append({"role": "user", "content": processed_prompt})

        # Get model from config or state
        model = evaluator_config.get("model")
        if not model:
            provider = state.get("llm_provider", "ollama")
            model_name = state.get("llm_model", "gemma3:4b")
            if provider == "ollama":
                model = f"ollama/{model_name}"
            else:
                model = model_name

        try:
            result = llm_call(state=state, model=model, messages=messages)
            response_text = result.get("content", result.get("response", ""))

            # Parse LLM response
            import json

            try:
                parsed = json.loads(response_text)
                return {
                    "valid": parsed.get("valid", False),
                    "score": float(parsed.get("score", 0.0)),
                    "errors": (
                        [{"message": parsed.get("reason", "")}]
                        if not parsed.get("valid")
                        else []
                    ),
                    "suggestions": parsed.get("suggestions", []),
                }
            except json.JSONDecodeError:
                # Try to extract boolean from response
                lower_response = response_text.lower()
                is_valid = "valid" in lower_response and "invalid" not in lower_response
                return {
                    "valid": is_valid,
                    "score": 1.0 if is_valid else 0.0,
                    "errors": [] if is_valid else [{"message": response_text}],
                    "suggestions": [],
                }

        except Exception as e:
            return {
                "valid": False,
                "score": 0.0,
                "errors": [{"message": f"LLM evaluation failed: {str(e)}"}],
                "suggestions": ["Check LLM configuration and connectivity"],
            }

    def _evaluate_with_custom(
        state: Dict[str, Any],
        output: Any,
        evaluator_config: Dict[str, Any],
        engine: Any,
    ) -> Dict[str, Any]:
        """
        Evaluate output using custom inline code (AC: 4).

        Supports Python, Lua, or Prolog inline code.

        Returns:
            {
                "valid": bool,
                "score": float,
                "errors": List[Dict],
                "suggestions": List[str]
            }
        """
        code = evaluator_config.get("run", "")
        if not code:
            return {
                "valid": False,
                "score": 0.0,
                "errors": [{"message": "Custom evaluator requires 'run' code"}],
                "suggestions": ["Add Python/Lua/Prolog code to 'run' field"],
            }

        language = evaluator_config.get("language", "python")

        # Create evaluation context
        eval_state = copy.copy(state)
        eval_state["output"] = output

        try:
            if language == "python":
                result = _execute_inline_code(code, eval_state, engine, "evaluator")
            elif language == "lua":
                result = _execute_lua_code(code, eval_state, engine)
            elif language == "prolog":
                result = _execute_prolog_code(code, eval_state, engine)
            else:
                return {
                    "valid": False,
                    "score": 0.0,
                    "errors": [{"message": f"Unknown language: {language}"}],
                    "suggestions": ["Use 'python', 'lua', or 'prolog'"],
                }

            # Normalize result
            if isinstance(result, bool):
                return {
                    "valid": result,
                    "score": 1.0 if result else 0.0,
                    "errors": [] if result else [{"message": "Validation failed"}],
                    "suggestions": [],
                }
            elif isinstance(result, dict):
                return {
                    "valid": result.get("valid", False),
                    "score": float(
                        result.get("score", 1.0 if result.get("valid") else 0.0)
                    ),
                    "errors": result.get("errors", []),
                    "suggestions": result.get("suggestions", []),
                }
            elif isinstance(result, (int, float)):
                # Treat as score (0.0-1.0)
                score = float(result)
                return {
                    "valid": score >= 0.5,
                    "score": score,
                    "errors": (
                        []
                        if score >= 0.5
                        else [{"message": f"Score {score} below threshold"}]
                    ),
                    "suggestions": [],
                }
            else:
                return {
                    "valid": bool(result),
                    "score": 1.0 if result else 0.0,
                    "errors": [] if result else [{"message": "Validation failed"}],
                    "suggestions": [],
                }

        except Exception as e:
            return {
                "valid": False,
                "score": 0.0,
                "errors": [{"message": f"Custom evaluator error: {str(e)}"}],
                "suggestions": ["Check your custom evaluation code"],
            }

    def _execute_inline_code(
        code: str,
        state: Dict[str, Any],
        engine: Any,
        context_name: str,
    ) -> Any:
        """Execute inline Python code with state access."""
        # Build execution namespace
        namespace = {
            "state": state,
            "output": state.get("output"),
            "reflection_output": state.get("reflection_output"),
            "reflection_errors": state.get("reflection_errors", []),
            "reflection_iteration": state.get("reflection_iteration", 0),
        }

        # Add common imports
        import json as json_module
        import re as re_module

        namespace["json"] = json_module
        namespace["re"] = re_module

        # Execute the code
        exec(code, namespace)

        # Return result if defined
        if "result" in namespace:
            return namespace["result"]
        elif "output" in namespace and namespace["output"] != state.get("output"):
            return namespace["output"]
        else:
            return namespace.get("__builtins__", {}).get("__result__")

    def _execute_lua_code(
        code: str,
        state: Dict[str, Any],
        engine: Any,
    ) -> Any:
        """Execute Lua code for evaluation."""
        lua_runtime = getattr(engine, "_lua_runtime", None)
        if lua_runtime is None:
            # Try to create a Lua runtime
            try:
                from lupa import LuaRuntime

                lua_runtime = LuaRuntime(unpack_returned_tuples=True)
            except ImportError:
                raise ValueError(
                    "Lua runtime not available. Install with: pip install lupa"
                )

        # Convert state to Lua table
        lua_state = lua_runtime.table_from(state)

        # Execute code
        lua_func = lua_runtime.eval(f"function(state, output) {code} end")
        result = lua_func(lua_state, state.get("output"))

        return result

    def _execute_prolog_code(
        code: str,
        state: Dict[str, Any],
        engine: Any,
    ) -> Any:
        """Execute Prolog code for evaluation."""
        prolog_runtime = getattr(engine, "_prolog_runtime", None)
        if prolog_runtime is None:
            raise ValueError("Prolog runtime not available")

        # Execute Prolog query
        result = prolog_runtime.query(code, state=state)
        return result

    def _process_template(template: str, state: Dict[str, Any], engine: Any) -> str:
        """Process Jinja2 template with state variables."""
        jinja_env = getattr(engine, "_jinja_env", None)
        if jinja_env is None:
            # Simple fallback template processing
            import re
            import json

            def replace_var(match):
                expr = match.group(1).strip()
                # Handle state.key or just key
                if expr.startswith("state."):
                    key = expr[6:]
                else:
                    key = expr

                # Handle filters
                if "|" in key:
                    key, filter_part = key.split("|", 1)
                    key = key.strip()
                    filter_name = filter_part.strip()
                    value = state.get(key, "")
                    if filter_name == "tojson":
                        return json.dumps(value)
                    return str(value)

                return str(state.get(key, ""))

            return re.sub(r"\{\{\s*(.+?)\s*\}\}", replace_var, template)

        # Use Jinja2 environment
        try:
            from jinja2 import Template

            tmpl = jinja_env.from_string(template)
            return tmpl.render(state=state, **state)
        except Exception:
            return template

    def _process_template_params(
        params: Dict[str, Any],
        state: Dict[str, Any],
        engine: Any,
    ) -> Dict[str, Any]:
        """Process templates in all string parameters."""
        processed = {}
        for key, value in params.items():
            if isinstance(value, str):
                processed[key] = _process_template(value, state, engine)
            elif isinstance(value, dict):
                processed[key] = _process_template_params(value, state, engine)
            elif isinstance(value, list):
                processed[key] = [
                    (
                        _process_template(v, state, engine)
                        if isinstance(v, str)
                        else (
                            _process_template_params(v, state, engine)
                            if isinstance(v, dict)
                            else v
                        )
                    )
                    for v in value
                ]
            else:
                processed[key] = value
        return processed

    def _resolve_schema_refs(schema: Dict[str, Any], engine: Any) -> Dict[str, Any]:
        """Resolve $ref references in JSON Schema."""
        if "$ref" not in schema:
            return schema

        ref = schema["$ref"]
        if ref.startswith("#"):
            # Internal reference - not supported yet
            return schema

        # External file reference
        import os
        import json as json_module
        import yaml as yaml_module

        base_path = getattr(engine, "_base_path", ".")
        ref_path = os.path.join(base_path, ref)

        try:
            with open(ref_path, "r") as f:
                if ref_path.endswith(".json"):
                    return json_module.load(f)
                else:
                    return yaml_module.safe_load(f)
        except Exception:
            return schema

    def _attempt_type_coercion(data: Any, schema: Dict[str, Any]) -> Any:
        """Attempt to coerce data types before validation."""
        if not isinstance(schema, dict):
            return data

        schema_type = schema.get("type")
        if not schema_type:
            return data

        try:
            if schema_type == "integer" and isinstance(data, str):
                return int(data)
            elif schema_type == "number" and isinstance(data, str):
                return float(data)
            elif schema_type == "boolean" and isinstance(data, str):
                return data.lower() in ("true", "1", "yes")
            elif schema_type == "string" and not isinstance(data, str):
                return str(data)
            elif schema_type == "object" and isinstance(data, dict):
                # Recursively coerce properties
                properties = schema.get("properties", {})
                result = {}
                for key, value in data.items():
                    if key in properties:
                        result[key] = _attempt_type_coercion(value, properties[key])
                    else:
                        result[key] = value
                return result
            elif schema_type == "array" and isinstance(data, list):
                items_schema = schema.get("items", {})
                return [_attempt_type_coercion(item, items_schema) for item in data]
        except (ValueError, TypeError):
            pass

        return data

    def reflection_loop_action(
        state: Dict[str, Any],
        generator: Dict[str, Any],
        evaluator: Dict[str, Any],
        corrector: Optional[Dict[str, Any]] = None,
        max_iterations: int = 3,
        on_failure: str = "return_best",
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Execute a generate→evaluate→correct loop (AC: 1, 5, 6).

        This action implements the reflection loop pattern:
        1. Generate output using generator config
        2. Evaluate output using evaluator config
        3. If valid, return success
        4. If invalid and iterations remaining, correct and repeat
        5. On exhaustion, apply on_failure strategy

        Args:
            state: Current workflow state
            generator: Generator configuration
                - action: Action name to call (e.g., "llm.call")
                - run: Inline Python code (alternative to action)
                - Additional params passed to the action
            evaluator: Evaluator configuration
                - type: "schema" | "llm" | "custom"
                - For schema: schema dict or $ref path
                - For llm: prompt, model, examples
                - For custom: run code, language
            corrector: Corrector configuration (optional, same format as generator)
            max_iterations: Maximum iterations before giving up (default: 3)
            on_failure: Strategy when max_iterations reached
                - "return_best": Return highest-scoring attempt
                - "return_last": Return final attempt
                - "raise": Raise ReflectionFailedError

        Returns:
            Dict with:
                - reflection_iteration: Final iteration count
                - reflection_output: Final output
                - reflection_errors: Errors from final evaluation (empty if valid)
                - reflection_history: All attempts
                - reflection_best: Best output seen
                - reflection_best_score: Score of best output
                - success: True if valid output produced

        Raises:
            ReflectionFailedError: If on_failure="raise" and max_iterations exhausted
            ValueError: If configuration is invalid
        """
        # Validate parameters
        if max_iterations < 1:
            raise ValueError("reflection.loop: max_iterations must be at least 1")

        if on_failure not in ("return_best", "return_last", "raise"):
            raise ValueError(
                f"reflection.loop: Invalid on_failure strategy: {on_failure}"
            )

        # Initialize tracking state
        loop_state = copy.copy(state)
        history: List[Dict[str, Any]] = []
        best_output: Any = None
        best_score: float = -1.0

        iteration = 0
        while iteration < max_iterations:
            iteration += 1

            # Update state with iteration tracking (AC: 5)
            loop_state["reflection_iteration"] = iteration
            loop_state["reflection_history"] = history

            # Step 1: Generate output
            try:
                output = _execute_generator(loop_state, generator, registry, engine)
                loop_state["reflection_output"] = output
            except ValueError:
                # Configuration errors should be raised immediately
                raise
            except Exception as e:
                # Runtime errors are recorded and loop continues
                error_entry = {
                    "iteration": iteration,
                    "output": None,
                    "score": 0.0,
                    "valid": False,
                    "errors": [{"message": f"Generator failed: {str(e)}"}],
                }
                history.append(error_entry)
                loop_state["reflection_errors"] = error_entry["errors"]
                continue

            # Step 2: Evaluate output
            evaluator_type = evaluator.get("type", "schema")

            if evaluator_type == "schema":
                schema = evaluator.get("schema", {})
                eval_result = _evaluate_with_schema(loop_state, output, schema, engine)
            elif evaluator_type == "llm":
                eval_result = _evaluate_with_llm(
                    loop_state, output, evaluator, registry, engine
                )
            elif evaluator_type == "custom":
                eval_result = _evaluate_with_custom(
                    loop_state, output, evaluator, engine
                )
            else:
                raise ValueError(
                    f"reflection.loop: Unknown evaluator type: {evaluator_type}"
                )

            # Record attempt in history
            history_entry = {
                "iteration": iteration,
                "output": output,
                "score": eval_result.get("score", 0.0),
                "valid": eval_result.get("valid", False),
                "errors": eval_result.get("errors", []),
            }
            history.append(history_entry)

            # Update best if this is highest scoring
            if eval_result.get("score", 0.0) > best_score:
                best_score = eval_result["score"]
                best_output = output

            loop_state["reflection_best"] = best_output
            loop_state["reflection_best_score"] = best_score
            loop_state["reflection_errors"] = eval_result.get("errors", [])

            # Check if valid
            if eval_result.get("valid", False):
                # Success! Return valid output
                return {
                    "reflection_iteration": iteration,
                    "reflection_output": output,
                    "reflection_errors": [],
                    "reflection_history": history,
                    "reflection_best": output,
                    "reflection_best_score": eval_result.get("score", 1.0),
                    "success": True,
                    "valid": True,
                    **(
                        {k: v for k, v in output.items()}
                        if isinstance(output, dict)
                        else {"result": output}
                    ),
                }

            # Step 3: Correct if we have more iterations and a corrector
            if iteration < max_iterations and corrector:
                try:
                    corrected = _execute_corrector(
                        loop_state, corrector, registry, engine
                    )
                    # Update state for next iteration
                    if isinstance(corrected, dict):
                        loop_state.update(corrected)
                except Exception as e:
                    # Corrector failed, continue to next iteration anyway
                    loop_state["reflection_errors"].append(
                        {
                            "message": f"Corrector failed: {str(e)}",
                            "type": "corrector_error",
                        }
                    )

        # Max iterations exhausted - apply on_failure strategy (AC: 6)
        if on_failure == "return_best":
            final_output = best_output
            final_score = best_score
        elif on_failure == "return_last":
            final_output = history[-1]["output"] if history else None
            final_score = history[-1].get("score", 0.0) if history else 0.0
        else:  # raise
            raise ReflectionFailedError(
                f"Reflection loop failed after {max_iterations} iterations",
                history=history,
            )

        return {
            "reflection_iteration": iteration,
            "reflection_output": final_output,
            "reflection_errors": history[-1].get("errors", []) if history else [],
            "reflection_history": history,
            "reflection_best": best_output,
            "reflection_best_score": best_score,
            "success": False,
            "valid": False,
            "exhausted": True,
            **(
                {k: v for k, v in final_output.items()}
                if isinstance(final_output, dict)
                else {"result": final_output}
            ),
        }

    def reflection_evaluate_action(
        state: Dict[str, Any],
        data: Any = None,
        evaluator_type: str = "schema",
        schema: Optional[Dict[str, Any]] = None,
        prompt: Optional[str] = None,
        model: Optional[str] = None,
        examples: Optional[List[Dict[str, Any]]] = None,
        run: Optional[str] = None,
        language: str = "python",
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Standalone evaluation action (AC: 7).

        Evaluates data using the specified evaluator type without running a full loop.

        Args:
            state: Current workflow state
            data: Data to evaluate (or from state['reflection_output'])
            evaluator_type: "schema" | "llm" | "custom"
            schema: JSON Schema for schema evaluator
            prompt: Evaluation prompt for LLM evaluator
            model: LLM model for LLM evaluator
            examples: Few-shot examples for LLM evaluator
            run: Code for custom evaluator
            language: Language for custom evaluator ("python", "lua", "prolog")

        Returns:
            {
                "valid": bool,
                "score": float,
                "errors": List[Dict],
                "suggestions": List[str],
                "success": True
            }
        """
        # Get data from parameter or state
        if data is None:
            data = state.get("reflection_output", state.get("output"))

        if evaluator_type == "schema":
            if schema is None:
                return {
                    "valid": False,
                    "score": 0.0,
                    "errors": [{"message": "Schema required for schema evaluator"}],
                    "suggestions": ["Provide a JSON Schema"],
                    "success": False,
                }
            result = _evaluate_with_schema(state, data, schema, engine)

        elif evaluator_type == "llm":
            evaluator_config = {
                "prompt": prompt or "Evaluate if this output is valid.",
                "model": model,
                "examples": examples or [],
            }
            result = _evaluate_with_llm(state, data, evaluator_config, registry, engine)

        elif evaluator_type == "custom":
            if run is None:
                return {
                    "valid": False,
                    "score": 0.0,
                    "errors": [{"message": "Run code required for custom evaluator"}],
                    "suggestions": ["Provide inline code in 'run' parameter"],
                    "success": False,
                }
            evaluator_config = {"run": run, "language": language}
            result = _evaluate_with_custom(state, data, evaluator_config, engine)

        else:
            return {
                "valid": False,
                "score": 0.0,
                "errors": [{"message": f"Unknown evaluator type: {evaluator_type}"}],
                "suggestions": ["Use 'schema', 'llm', or 'custom'"],
                "success": False,
            }

        result["success"] = True
        return result

    def reflection_correct_action(
        state: Dict[str, Any],
        data: Any = None,
        errors: Optional[List[Dict[str, Any]]] = None,
        corrector_action: Optional[str] = None,
        run: Optional[str] = None,
        prompt: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Standalone correction action (AC: 8).

        Corrects data based on evaluation errors.

        Args:
            state: Current workflow state
            data: Data to correct (or from state['reflection_output'])
            errors: Evaluation errors (or from state['reflection_errors'])
            corrector_action: Action to use for correction
            run: Inline Python code for correction
            prompt: LLM prompt for correction (uses llm.call)

        Returns:
            {
                "corrected_output": any,
                "success": True
            }
        """
        # Get data and errors from parameters or state
        if data is None:
            data = state.get("reflection_output", state.get("output"))
        if errors is None:
            errors = state.get("reflection_errors", [])

        # Prepare correction state
        correction_state = copy.copy(state)
        correction_state["reflection_output"] = data
        correction_state["reflection_errors"] = errors

        corrector_config: Dict[str, Any] = {}

        if corrector_action:
            corrector_config["action"] = corrector_action
            corrector_config.update(kwargs)
        elif run:
            corrector_config["run"] = run
        elif prompt:
            corrector_config["action"] = "llm.call"
            corrector_config["prompt"] = prompt
        else:
            return {
                "corrected_output": data,
                "success": False,
                "error": "No corrector specified",
            }

        try:
            corrected = _execute_corrector(
                correction_state, corrector_config, registry, engine
            )
            return {
                "corrected_output": corrected,
                "success": True,
            }
        except Exception as e:
            return {
                "corrected_output": data,
                "success": False,
                "error": str(e),
            }

    # Register actions (AC: 9)
    registry["reflection.loop"] = reflection_loop_action
    registry["actions.reflection_loop"] = reflection_loop_action
    registry["reflection.evaluate"] = reflection_evaluate_action
    registry["actions.reflection_evaluate"] = reflection_evaluate_action
    registry["reflection.correct"] = reflection_correct_action
    registry["actions.reflection_correct"] = reflection_correct_action
