"""TextGrad client wrapper for gradient-based prompt optimization.

This module provides a wrapper around the TextGrad library for prompt optimization
using textual gradients and backpropagation through text.
"""

import logging
import warnings
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)

# Try to import textgrad, but provide graceful fallback
try:
    import textgrad as tg
    from textgrad import Variable
    from textgrad.optimizer import TextualGradientDescent
    from textgrad.engine import get_engine

    TEXTGRAD_AVAILABLE = True
except ImportError:
    TEXTGRAD_AVAILABLE = False
    logger.debug(
        "TextGrad library not available. Install with: pip install the_edge_agent[textgrad]"
    )


@dataclass
class TextGradConfig:
    """Configuration for TextGrad optimization.

    Attributes:
        enabled: Whether TextGrad optimization is enabled (requires explicit opt-in)
        optimizer_model: Model to use for generating textual gradients (default: "gpt-4")
        max_iterations: Maximum number of optimization iterations (default: 3)
        learning_rate: Learning rate for gradient descent (default: 0.1)
        early_stopping_threshold: Stop if improvement below this threshold (default: 0.01)
        cost_warning_shown: Track if cost warning has been displayed (internal)
    """

    enabled: bool = False
    optimizer_model: str = "gpt-4"
    max_iterations: int = 3
    learning_rate: float = 0.1
    early_stopping_threshold: float = 0.01
    cost_warning_shown: bool = field(default=False, repr=False)


class TextGradClient:
    """Client wrapper for TextGrad prompt optimization.

    This class provides a high-level interface to TextGrad's gradient-based
    prompt optimization, with built-in cost warnings and configuration management.

    Example:
        >>> config = TextGradConfig(enabled=True, optimizer_model="gpt-4")
        >>> client = TextGradClient(config)
        >>> prompt_var = client.create_variable("system_prompt", "You are a helpful assistant.")
        >>> optimized = client.optimize_prompt(
        ...     prompt_var,
        ...     loss_fn="Evaluate if response is helpful: {response}",
        ...     iterations=3
        ... )
    """

    def __init__(self, config: TextGradConfig):
        """Initialize TextGrad client with configuration.

        Args:
            config: TextGrad configuration object

        Raises:
            RuntimeError: If TextGrad is not available and enabled=True
        """
        self.config = config
        self._engine = None
        self._optimizer = None

        if self.config.enabled:
            if not TEXTGRAD_AVAILABLE:
                raise RuntimeError(
                    "TextGrad is enabled but the library is not installed. "
                    "Install with: pip install the_edge_agent[textgrad] or pip install textgrad"
                )

            # Show cost warning on first use
            if not self.config.cost_warning_shown:
                self._show_cost_warning()
                self.config.cost_warning_shown = True

            # Initialize TextGrad engine
            try:
                self._engine = get_engine(engine_name=self.config.optimizer_model)
                logger.info(
                    f"TextGrad initialized with model: {self.config.optimizer_model}"
                )
            except Exception as e:
                logger.error(f"Failed to initialize TextGrad engine: {e}")
                raise RuntimeError(f"Failed to initialize TextGrad: {e}") from e

    def _show_cost_warning(self) -> None:
        """Display cost warning for TextGrad optimization."""
        warning_message = (
            "\n" + "=" * 70 + "\n"
            "⚠️  TEXTGRAD OPTIMIZATION COST WARNING\n"
            "=" * 70 + "\n"
            "TextGrad optimization requires multiple LLM calls per iteration:\n"
            "  - Forward pass (generate output)\n"
            "  - Loss evaluation (critique output)\n"
            "  - Gradient computation (generate improvement suggestions)\n"
            "  - Update step (apply suggestions)\n"
            "\n"
            f"For {self.config.max_iterations} iterations, this can result in "
            f"{self.config.max_iterations * 4}+ LLM API calls.\n"
            "\n"
            "Estimated cost per optimization:\n"
            f"  - Model: {self.config.optimizer_model}\n"
            f"  - Iterations: {self.config.max_iterations}\n"
            "  - Approx cost: $0.10 - $1.00+ (depends on model and prompt length)\n"
            "\n"
            "Please ensure you have:\n"
            "  1. Set appropriate API rate limits\n"
            "  2. Monitored your usage dashboard\n"
            "  3. Tested with small iteration counts first\n"
            "=" * 70 + "\n"
        )
        warnings.warn(warning_message, UserWarning, stacklevel=3)
        logger.warning("TextGrad cost warning displayed to user")

    def create_variable(
        self,
        name: str,
        initial_value: str,
        role_description: Optional[str] = None,
        requires_grad: bool = True,
    ) -> Optional[Any]:
        """Create a TextGrad variable (optimizable prompt).

        Args:
            name: Variable name for tracking
            initial_value: Initial prompt text
            role_description: Optional description of variable's role
            requires_grad: Whether variable should be optimized (default: True)

        Returns:
            TextGrad Variable object if enabled, None otherwise

        Raises:
            RuntimeError: If TextGrad is not available but required
        """
        if not self.config.enabled:
            logger.debug(f"TextGrad disabled, returning None for variable: {name}")
            return None

        if not TEXTGRAD_AVAILABLE:
            raise RuntimeError("TextGrad library not available")

        try:
            variable = Variable(
                value=initial_value,
                role_description=role_description or f"Optimizable prompt: {name}",
                requires_grad=requires_grad,
            )
            logger.debug(
                f"Created TextGrad variable: {name} (requires_grad={requires_grad})"
            )
            return variable
        except Exception as e:
            logger.error(f"Failed to create TextGrad variable {name}: {e}")
            raise

    def compute_feedback(
        self,
        output: str,
        evaluation_criteria: List[str],
        context: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """Compute textual gradients (feedback) for an output.

        Args:
            output: The output to evaluate
            evaluation_criteria: List of criteria to evaluate against
            context: Optional context for evaluation

        Returns:
            Dictionary with feedback structure:
                - valid: bool indicating if output is acceptable
                - feedback: str with improvement suggestions
                - scores: dict mapping criteria to scores (0-1)
                - gradient_text: raw textual gradient
        """
        if not self.config.enabled:
            return {
                "valid": True,
                "feedback": "TextGrad disabled, no feedback generated",
                "scores": {},
                "gradient_text": "",
            }

        if not TEXTGRAD_AVAILABLE:
            raise RuntimeError("TextGrad library not available")

        # Construct evaluation prompt
        criteria_text = "\n".join(
            f"  - {criterion}" for criterion in evaluation_criteria
        )
        eval_prompt = f"""
Evaluate the following output against these criteria:
{criteria_text}

Output to evaluate:
{output}

Provide:
1. Overall assessment (pass/fail)
2. Specific feedback for improvement
3. Score for each criterion (0.0 to 1.0)
        """.strip()

        try:
            # Use TextGrad engine to generate feedback
            feedback_response = self._engine(eval_prompt)

            # Parse response (simplified - real implementation would be more robust)
            # For now, return structured feedback
            return {
                "valid": "pass" in feedback_response.lower(),
                "feedback": feedback_response,
                "scores": {
                    criterion: 0.5 for criterion in evaluation_criteria
                },  # Placeholder
                "gradient_text": feedback_response,
            }
        except Exception as e:
            logger.error(f"Failed to compute feedback: {e}")
            return {
                "valid": False,
                "feedback": f"Error computing feedback: {e}",
                "scores": {},
                "gradient_text": "",
            }

    def optimize_prompt(
        self,
        variable: Any,
        loss_fn: str,
        iterations: Optional[int] = None,
        context: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """Optimize a prompt variable using TextGrad.

        Args:
            variable: TextGrad Variable to optimize
            loss_fn: Loss function prompt template
            iterations: Number of optimization iterations (default: from config)
            context: Optional context for optimization

        Returns:
            Dictionary with optimization results:
                - optimized_value: Final optimized prompt
                - initial_value: Original prompt
                - iterations_completed: Number of iterations run
                - improvement_trace: List of improvements per iteration
                - converged: Whether optimization converged early
        """
        if not self.config.enabled:
            raise RuntimeError(
                "TextGrad is not enabled. Set settings.textgrad.enabled=true"
            )

        if not TEXTGRAD_AVAILABLE:
            raise RuntimeError("TextGrad library not available")

        if variable is None:
            raise ValueError("Variable cannot be None")

        max_iter = iterations or self.config.max_iterations
        initial_value = variable.value if hasattr(variable, "value") else str(variable)

        improvement_trace = []
        converged = False

        try:
            # Initialize optimizer if not already done
            if self._optimizer is None:
                self._optimizer = TextualGradientDescent(
                    parameters=[variable], engine=self._engine
                )

            logger.info(f"Starting TextGrad optimization for {max_iter} iterations")

            for i in range(max_iter):
                # Evaluate loss
                loss_prompt = loss_fn.format(prompt=variable.value, **(context or {}))
                loss_response = self._engine(loss_prompt)

                # Create loss variable for backprop
                loss = Variable(value=loss_response, role_description="Loss evaluation")

                # Backward pass (compute gradients)
                loss.backward()

                # Optimization step
                self._optimizer.step()

                # Track improvement
                improvement_trace.append(
                    {
                        "iteration": i + 1,
                        "value": variable.value,
                        "loss_eval": loss_response,
                    }
                )

                # Check for early stopping (simplified)
                if i > 0 and "excellent" in loss_response.lower():
                    logger.info(f"Early stopping at iteration {i+1}")
                    converged = True
                    break

                # Zero gradients
                self._optimizer.zero_grad()

            logger.info(f"TextGrad optimization completed. Converged: {converged}")

            return {
                "optimized_value": variable.value,
                "initial_value": initial_value,
                "iterations_completed": len(improvement_trace),
                "improvement_trace": improvement_trace,
                "converged": converged,
            }

        except Exception as e:
            logger.error(f"TextGrad optimization failed: {e}")
            raise RuntimeError(f"Optimization failed: {e}") from e

    def is_available(self) -> bool:
        """Check if TextGrad is available and enabled.

        Returns:
            True if TextGrad is available and enabled, False otherwise
        """
        return self.config.enabled and TEXTGRAD_AVAILABLE
