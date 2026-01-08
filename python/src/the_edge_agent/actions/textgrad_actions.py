"""TextGrad actions for gradient-based prompt optimization.

This module provides YAML actions for prompt optimization using TextGrad's
textual gradient approach, enabling agents to learn and improve their prompts.

Actions:
    - learn.textgrad.variable: Define an optimizable prompt variable
    - learn.textgrad.feedback: Compute textual gradients from output evaluation
    - learn.textgrad.optimize_prompt: Optimize a prompt using TextGrad
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from ..learning.textgrad_client import (
    TextGradClient,
    TextGradConfig,
    TEXTGRAD_AVAILABLE,
)

logger = logging.getLogger(__name__)


@dataclass
class PromptVariable:
    """Represents an optimizable prompt variable with version tracking.

    Attributes:
        name: Unique variable name
        current_value: Current prompt text
        initial_value: Original prompt text
        role_description: Description of the variable's role
        constraints: List of constraints for optimization
        version: Current version number
        versions: History of all versions with timestamps
        textgrad_var: Reference to TextGrad Variable object (if enabled)
    """

    name: str
    current_value: str
    initial_value: str
    role_description: Optional[str] = None
    constraints: List[str] = field(default_factory=list)
    version: int = 1
    versions: List[Dict[str, Any]] = field(default_factory=list)
    textgrad_var: Any = None

    def __post_init__(self):
        """Initialize version history."""
        if not self.versions:
            self.versions.append(
                {
                    "version": 1,
                    "value": self.initial_value,
                    "timestamp": datetime.now(timezone.utc).isoformat(),
                    "change_reason": "initial",
                }
            )

    def update(self, new_value: str, reason: str = "optimization") -> None:
        """Update the variable value and track the change.

        Args:
            new_value: New prompt text
            reason: Reason for the update
        """
        if new_value != self.current_value:
            self.version += 1
            self.current_value = new_value
            self.versions.append(
                {
                    "version": self.version,
                    "value": new_value,
                    "timestamp": datetime.now(timezone.utc).isoformat(),
                    "change_reason": reason,
                }
            )

    def to_dict(self) -> Dict[str, Any]:
        """Serialize to dictionary for state persistence."""
        return {
            "name": self.name,
            "current_value": self.current_value,
            "initial_value": self.initial_value,
            "role_description": self.role_description,
            "constraints": self.constraints,
            "version": self.version,
            "versions": self.versions,
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "PromptVariable":
        """Deserialize from dictionary."""
        return cls(
            name=data["name"],
            current_value=data["current_value"],
            initial_value=data["initial_value"],
            role_description=data.get("role_description"),
            constraints=data.get("constraints", []),
            version=data.get("version", 1),
            versions=data.get("versions", []),
        )


# Global variable registry for the session
_variable_registry: Dict[str, PromptVariable] = {}


def _get_textgrad_client(state: Dict[str, Any]) -> TextGradClient:
    """Get or create TextGrad client from state/settings.

    Args:
        state: Current workflow state

    Returns:
        TextGradClient instance
    """
    # Check for existing client in state
    if "_textgrad_client" in state:
        return state["_textgrad_client"]

    # Get settings from state
    settings = state.get("settings", {})
    textgrad_settings = settings.get("textgrad", {})

    config = TextGradConfig(
        enabled=textgrad_settings.get("enabled", False),
        optimizer_model=textgrad_settings.get("optimizer_model", "gpt-4"),
        max_iterations=textgrad_settings.get("max_iterations", 3),
        learning_rate=textgrad_settings.get("learning_rate", 0.1),
        early_stopping_threshold=textgrad_settings.get(
            "early_stopping_threshold", 0.01
        ),
    )

    client = TextGradClient(config)
    return client


def textgrad_variable(
    state: Dict[str, Any],
    name: str,
    initial_value: str,
    role_description: Optional[str] = None,
    constraints: Optional[List[str]] = None,
    requires_grad: bool = True,
) -> Dict[str, Any]:
    """Define an optimizable prompt variable (learn.textgrad.variable action).

    This action creates a prompt variable that can be optimized using TextGrad.
    Variables are tracked with version history for state persistence.

    Args:
        state: Current workflow state
        name: Unique variable name
        initial_value: Initial prompt text
        role_description: Description of the variable's role
        constraints: List of constraints for optimization
        requires_grad: Whether variable should be optimized (default: True)

    Returns:
        Dictionary with:
            - variable: PromptVariable object
            - variable_name: Name of the variable
            - textgrad_enabled: Whether TextGrad is active

    Example YAML:
        - name: define_prompt
          action: learn.textgrad.variable
          with:
            name: system_prompt
            initial_value: "You are a helpful assistant."
            constraints:
              - "Must be polite"
              - "Must be concise"
    """
    global _variable_registry

    # Check if variable already exists
    if name in _variable_registry:
        logger.debug(f"Variable '{name}' already exists, returning existing")
        existing = _variable_registry[name]
        return {
            "variable": existing.to_dict(),
            "variable_name": name,
            "textgrad_enabled": TEXTGRAD_AVAILABLE
            and _get_textgrad_client(state).is_available(),
            "version": existing.version,
        }

    # Create new variable
    variable = PromptVariable(
        name=name,
        current_value=initial_value,
        initial_value=initial_value,
        role_description=role_description,
        constraints=constraints or [],
    )

    # Register in global registry
    _variable_registry[name] = variable

    # Create TextGrad variable if enabled
    try:
        client = _get_textgrad_client(state)
        if client.is_available():
            variable.textgrad_var = client.create_variable(
                name=name,
                initial_value=initial_value,
                role_description=role_description,
                requires_grad=requires_grad,
            )
    except Exception as e:
        logger.warning(f"Could not create TextGrad variable: {e}")

    logger.info(f"Created prompt variable: {name}")

    return {
        "variable": variable.to_dict(),
        "variable_name": name,
        "textgrad_enabled": TEXTGRAD_AVAILABLE
        and _get_textgrad_client(state).is_available(),
        "version": variable.version,
    }


def textgrad_feedback(
    state: Dict[str, Any],
    output: str,
    evaluation_criteria: List[str],
    aspects: Optional[List[str]] = None,
    context: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    """Compute textual gradients from output evaluation (learn.textgrad.feedback action).

    This action evaluates an output against criteria and generates structured
    feedback that can be used as textual gradients for optimization.

    Args:
        state: Current workflow state
        output: The output to evaluate
        evaluation_criteria: List of criteria to evaluate against
        aspects: Multi-aspect evaluation (accuracy, clarity, safety, etc.)
        context: Optional context for evaluation

    Returns:
        Dictionary with:
            - valid: Whether output passes evaluation
            - feedback: Structured feedback text
            - scores: Dictionary mapping criteria/aspects to scores (0-1)
            - gradient_text: Raw textual gradient for optimization
            - suggestions: List of specific improvement suggestions

    Example YAML:
        - name: evaluate_output
          action: learn.textgrad.feedback
          with:
            output: "{{ state.response }}"
            evaluation_criteria:
              - "Response is accurate"
              - "Response is helpful"
            aspects:
              - accuracy
              - clarity
              - safety
    """
    aspects = aspects or ["quality"]
    all_criteria = evaluation_criteria + [f"Evaluate {a}" for a in aspects]

    try:
        client = _get_textgrad_client(state)

        if client.is_available():
            result = client.compute_feedback(
                output=output, evaluation_criteria=all_criteria, context=context
            )

            # Extract suggestions from feedback
            suggestions = _extract_suggestions(result.get("feedback", ""))

            return {
                "valid": result.get("valid", False),
                "feedback": result.get("feedback", ""),
                "scores": result.get("scores", {}),
                "gradient_text": result.get("gradient_text", ""),
                "suggestions": suggestions,
                "aspects_evaluated": aspects,
            }
        else:
            # Fallback: Simple heuristic evaluation
            return _fallback_feedback(output, evaluation_criteria, aspects)

    except Exception as e:
        logger.error(f"Error computing feedback: {e}")
        return {
            "valid": False,
            "feedback": f"Error: {e}",
            "scores": {},
            "gradient_text": "",
            "suggestions": [],
            "aspects_evaluated": aspects,
        }


def _extract_suggestions(feedback: str) -> List[str]:
    """Extract improvement suggestions from feedback text."""
    suggestions = []
    lines = feedback.split("\n")

    for line in lines:
        line = line.strip()
        # Look for numbered suggestions or bullet points
        if line and (
            line.startswith(("-", "*", "•"))
            or (len(line) > 2 and line[0].isdigit() and line[1] in ".)")
        ):
            # Clean up the line
            if line[0].isdigit():
                suggestion = line[2:].strip()
            else:
                suggestion = line[1:].strip()
            if suggestion and len(suggestion) > 5:
                suggestions.append(suggestion)

    return suggestions[:5]  # Return top 5 suggestions


def _fallback_feedback(
    output: str, criteria: List[str], aspects: List[str]
) -> Dict[str, Any]:
    """Provide fallback feedback when TextGrad is not available."""
    # Simple heuristic checks
    word_count = len(output.split())
    has_content = word_count > 10

    scores = {}
    for criterion in criteria:
        scores[criterion] = 0.5  # Neutral score

    for aspect in aspects:
        if aspect == "clarity":
            scores[f"aspect_{aspect}"] = min(1.0, word_count / 100)
        elif aspect == "safety":
            # Check for potentially unsafe patterns
            unsafe_words = ["hack", "attack", "exploit", "harm"]
            is_safe = not any(w in output.lower() for w in unsafe_words)
            scores[f"aspect_{aspect}"] = 1.0 if is_safe else 0.3
        else:
            scores[f"aspect_{aspect}"] = 0.5

    valid = has_content and all(s >= 0.3 for s in scores.values())

    return {
        "valid": valid,
        "feedback": "TextGrad not available. Basic heuristic evaluation applied.",
        "scores": scores,
        "gradient_text": "",
        "suggestions": ["Enable TextGrad for detailed feedback and optimization"],
        "aspects_evaluated": aspects,
    }


def textgrad_optimize_prompt(
    state: Dict[str, Any],
    variable: str,
    loss_fn: str,
    iterations: Optional[int] = None,
    constraints: Optional[List[str]] = None,
    context: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    """Optimize a prompt variable using TextGrad (learn.textgrad.optimize_prompt action).

    This action optimizes a previously defined prompt variable using TextGrad's
    gradient-based approach. It iteratively improves the prompt based on the
    loss function evaluation.

    Args:
        state: Current workflow state
        variable: Name of the variable to optimize (from learn.textgrad.variable)
        loss_fn: Loss function prompt template
        iterations: Number of optimization iterations (default: from config)
        constraints: Additional constraints for this optimization
        context: Optional context for loss function evaluation

    Returns:
        Dictionary with:
            - optimized_value: Final optimized prompt
            - initial_value: Original prompt before optimization
            - variable_name: Name of the optimized variable
            - iterations_completed: Number of iterations run
            - improvement_trace: List of improvements per iteration
            - converged: Whether optimization converged early
            - version: New version number of the variable

    Example YAML:
        - name: optimize_prompt
          action: learn.textgrad.optimize_prompt
          with:
            variable: system_prompt
            loss_fn: "Evaluate if the response is accurate and helpful: {{ state.response }}"
            iterations: 3
    """
    global _variable_registry

    # Get the variable
    if variable not in _variable_registry:
        raise ValueError(
            f"Variable '{variable}' not found. Define it first with learn.textgrad.variable"
        )

    var = _variable_registry[variable]
    initial_value = var.current_value

    try:
        client = _get_textgrad_client(state)

        if not client.is_available():
            # Fallback: Return without optimization
            logger.warning("TextGrad not available, returning variable unchanged")
            return {
                "optimized_value": var.current_value,
                "initial_value": initial_value,
                "variable_name": variable,
                "iterations_completed": 0,
                "improvement_trace": [],
                "converged": False,
                "version": var.version,
                "textgrad_enabled": False,
            }

        # Apply constraints to loss function if provided
        full_loss_fn = loss_fn
        if constraints or var.constraints:
            all_constraints = (constraints or []) + var.constraints
            constraint_text = "\n".join(f"- {c}" for c in all_constraints)
            full_loss_fn = f"{loss_fn}\n\nConstraints to respect:\n{constraint_text}"

        # Run optimization
        result = client.optimize_prompt(
            variable=var.textgrad_var,
            loss_fn=full_loss_fn,
            iterations=iterations,
            context=context or {},
        )

        # Update the variable
        optimized_value = result.get("optimized_value", var.current_value)
        if optimized_value != var.current_value:
            var.update(optimized_value, reason="textgrad_optimization")

        logger.info(
            f"Optimized variable '{variable}' from v{var.version - 1} to v{var.version}"
        )

        return {
            "optimized_value": optimized_value,
            "initial_value": initial_value,
            "variable_name": variable,
            "iterations_completed": result.get("iterations_completed", 0),
            "improvement_trace": result.get("improvement_trace", []),
            "converged": result.get("converged", False),
            "version": var.version,
            "textgrad_enabled": True,
        }

    except Exception as e:
        logger.error(f"Optimization failed for variable '{variable}': {e}")
        raise RuntimeError(f"Prompt optimization failed: {e}") from e


def get_variable(name: str) -> Optional[Dict[str, Any]]:
    """Get a variable by name.

    Args:
        name: Variable name

    Returns:
        Variable dictionary or None if not found
    """
    if name in _variable_registry:
        return _variable_registry[name].to_dict()
    return None


def list_variables() -> List[str]:
    """List all registered variable names.

    Returns:
        List of variable names
    """
    return list(_variable_registry.keys())


def clear_variables() -> None:
    """Clear all registered variables (useful for testing)."""
    global _variable_registry
    _variable_registry.clear()


def textgrad_reflection_corrector(
    state: Dict[str, Any],
    variable: str,
    trigger_threshold: int = 2,
    optimization_iterations: Optional[int] = None,
    context: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    """Corrector for reflection.loop that uses TextGrad for prompt optimization (AC: 4).

    This action integrates with reflection.loop to automatically trigger
    TextGrad optimization when the reflection loop has failed multiple times.
    It uses the reflection history as context for gradient computation.

    Args:
        state: Current workflow state (includes reflection_history)
        variable: Name of the prompt variable to optimize
        trigger_threshold: Number of failures before triggering optimization
        optimization_iterations: Iterations for optimization (default: from config)
        context: Additional context for optimization

    Returns:
        Dictionary with:
            - corrected_output: The corrected output (from optimized prompt)
            - optimization_triggered: Whether optimization was triggered
            - new_prompt: The optimized prompt (if optimization triggered)
            - reflection_context_used: Whether reflection history was used

    Example YAML (as corrector in reflection.loop):
        - name: generate_with_learning
          action: reflection.loop
          with:
            generator:
              action: llm.call
              prompt: "{{ state.optimized_prompt }}"
            evaluator:
              type: llm
              prompt: "Evaluate quality..."
            corrector:
              action: learn.textgrad.reflection_corrector
              with:
                variable: optimized_prompt
                trigger_threshold: 2
            max_iterations: 3
    """
    global _variable_registry

    # Get reflection context from state
    reflection_iteration = state.get("reflection_iteration", 0)
    reflection_history = state.get("reflection_history", [])
    reflection_errors = state.get("reflection_errors", [])
    current_output = state.get("reflection_output", "")

    # Check if we should trigger optimization
    optimization_triggered = reflection_iteration >= trigger_threshold

    result = {
        "corrected_output": current_output,
        "optimization_triggered": False,
        "new_prompt": None,
        "reflection_context_used": len(reflection_history) > 0,
        "reflection_iteration": reflection_iteration,
    }

    if not optimization_triggered:
        logger.debug(
            f"Not triggering optimization (iteration {reflection_iteration} < threshold {trigger_threshold})"
        )
        return result

    # Check if variable exists
    if variable not in _variable_registry:
        logger.warning(f"Variable '{variable}' not found for optimization")
        return result

    var = _variable_registry[variable]

    # Build loss function from reflection history
    loss_fn = _build_reflection_loss_fn(reflection_history, reflection_errors)

    # Add context from reflection history
    full_context = {
        "reflection_iteration": reflection_iteration,
        "reflection_errors": reflection_errors,
        "reflection_history": reflection_history,
        **(context or {}),
    }

    try:
        # Run optimization
        opt_result = textgrad_optimize_prompt(
            state=state,
            variable=variable,
            loss_fn=loss_fn,
            iterations=optimization_iterations,
            context=full_context,
        )

        result["optimization_triggered"] = True
        result["new_prompt"] = opt_result.get("optimized_value")

        # Update the state with the new prompt
        logger.info(
            f"TextGrad optimization triggered at iteration {reflection_iteration}"
        )

        return result

    except Exception as e:
        logger.error(f"TextGrad optimization failed in reflection corrector: {e}")
        return result


def _build_reflection_loss_fn(history: List[Dict[str, Any]], errors: List[Any]) -> str:
    """Build a loss function from reflection history for TextGrad optimization.

    Args:
        history: List of reflection attempts with outputs and scores
        errors: List of evaluation errors from current iteration

    Returns:
        Loss function prompt string for TextGrad
    """
    error_text = "\n".join(f"- {e}" for e in errors) if errors else "No specific errors"

    history_text = ""
    if history:
        recent_history = history[-3:]  # Last 3 attempts
        for i, attempt in enumerate(recent_history, 1):
            score = attempt.get("score", "N/A")
            output_preview = str(attempt.get("output", ""))[:100]
            history_text += (
                f"\nAttempt {i}: Score={score}, Output preview: {output_preview}..."
            )

    loss_fn = f"""
Evaluate this prompt for the following criteria:

1. Does the prompt lead to outputs that avoid these errors?
   {error_text}

2. Consider the history of failed attempts:
   {history_text if history_text else "No previous attempts"}

3. The prompt should be improved to:
   - Generate more accurate outputs
   - Avoid repeated mistakes
   - Be clearer and more specific

Rate the prompt from 0 to 1 based on how well it addresses these issues.
    """.strip()

    return loss_fn


def create_textgrad_corrector_config(
    variable: str,
    trigger_threshold: int = 2,
    optimization_iterations: Optional[int] = None,
) -> Dict[str, Any]:
    """Create a corrector configuration for use in reflection.loop YAML.

    This helper function creates the configuration dictionary that can be
    used to specify a TextGrad corrector in YAML workflows.

    Args:
        variable: Name of the variable to optimize
        trigger_threshold: Failures before optimization
        optimization_iterations: Number of optimization iterations

    Returns:
        Dictionary configuration for use as corrector in reflection.loop

    Example:
        >>> config = create_textgrad_corrector_config("system_prompt", trigger_threshold=2)
        >>> # Use in Python: reflection_loop(..., corrector=config)
    """
    config = {
        "action": "learn.textgrad.reflection_corrector",
        "with": {"variable": variable, "trigger_threshold": trigger_threshold},
    }

    if optimization_iterations is not None:
        config["with"]["optimization_iterations"] = optimization_iterations

    return config


# Action registry entries
def register_actions(registry: Dict[str, Any], engine: Any) -> None:
    """Register TextGrad actions in the action registry.

    Args:
        registry: Action registry dictionary
        engine: YAMLEngine instance (for shared resources)
    """
    registry["learn.textgrad.variable"] = textgrad_variable
    registry["learn.textgrad.feedback"] = textgrad_feedback
    registry["learn.textgrad.optimize_prompt"] = textgrad_optimize_prompt
    registry["learn.textgrad.reflection_corrector"] = textgrad_reflection_corrector

    # Also register with alternative namespaces
    registry["textgrad.variable"] = textgrad_variable
    registry["textgrad.feedback"] = textgrad_feedback
    registry["textgrad.optimize_prompt"] = textgrad_optimize_prompt
    registry["textgrad.reflection_corrector"] = textgrad_reflection_corrector

    # Actions namespace (for backward compatibility)
    registry["actions.textgrad_variable"] = textgrad_variable
    registry["actions.textgrad_feedback"] = textgrad_feedback
    registry["actions.textgrad_optimize_prompt"] = textgrad_optimize_prompt
    registry["actions.textgrad_reflection_corrector"] = textgrad_reflection_corrector

    logger.debug("TextGrad actions registered")
