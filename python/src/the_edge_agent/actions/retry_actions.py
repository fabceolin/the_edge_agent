"""
Retry Actions for YAML Agents (TEA-YAML-005).

Provides the retry.loop action for general-purpose retry with correction loops:
- Wraps any validation action
- Automatically routes to a correction node on failure
- Tracks retry count and error context
- Exits loop on success or max retries

Usage in YAML:
    nodes:
      - name: validate_with_retry
        uses: retry.loop
        with:
          validate: validate.extraction
          validate_args:
            entities: "{{ state.entities }}"
            relationships: "{{ state.relationships }}"
          correct: correct_extraction
          max_retries: 2

State Variables Set:
    _retry_count: int     - Current retry attempt (0-indexed)
    _retry_errors: list   - Errors from last validation attempt
    _retry_result: dict   - Final validation result
    _retry_exhausted: bool - True if max retries reached without success
"""

import time
from typing import Any, Callable, Dict, List, Optional


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register retry actions in the YAMLEngine registry.

    Actions registered:
        - retry.loop: Execute validation with automatic retry and correction

    Args:
        registry: Action registry to populate
        engine: YAMLEngine instance for accessing graph and other resources
    """

    def retry_loop_action(
        state: Dict[str, Any],
        validate: str,
        validate_args: Optional[Dict[str, Any]] = None,
        correct: str = "",
        max_retries: int = 1,
        retry_delay: float = 0.0,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Execute validation with retry loop (TEA-YAML-005).

        This action wraps a validation action and automatically retries
        with a correction node on failure. It implements the pattern:
        1. Call validation action
        2. If valid, return success
        3. If invalid and retries remaining:
           a. Set _retry_errors in state for correction context
           b. Execute correction node
           c. Increment _retry_count
           d. Go to step 1
        4. If invalid and no retries left, return failure

        Args:
            state: Current workflow state
            validate: Name of validation action to call (e.g., "validate.extraction")
            validate_args: Arguments to pass to validation action (template-processed)
            correct: Name of correction node to execute on validation failure
            max_retries: Maximum number of correction attempts (default: 1)
            retry_delay: Delay in seconds between retry attempts (default: 0)

        Returns:
            Dict with:
                - _retry_count: int - Final retry attempt number (0-indexed)
                - _retry_errors: list - Errors from last validation attempt
                - _retry_result: dict - Final validation result
                - _retry_exhausted: bool - True if max retries exceeded
                - Plus all fields from the final validation result

        Raises:
            ValueError: If validation action not found, correction node not found,
                       or max_retries is invalid

        Example:
            >>> # In YAML:
            >>> # uses: retry.loop
            >>> # with:
            >>> #   validate: validate.extraction
            >>> #   validate_args:
            >>> #     entities: "{{ state.entities }}"
            >>> #   correct: fix_extraction
            >>> #   max_retries: 2
        """
        # Validate max_retries parameter (AC: 16)
        if not isinstance(max_retries, int) or max_retries < 0:
            raise ValueError(
                f"retry.loop: max_retries must be a non-negative integer, got {max_retries}"
            )

        # Validate action name exists (AC: 15)
        if validate not in registry:
            raise ValueError(f"retry.loop: Unknown validation action: {validate}")

        # Validate correction node exists (AC: 15)
        if not correct:
            raise ValueError("retry.loop: 'correct' parameter is required")

        # Get correction node function from the graph
        graph = getattr(engine, "_current_graph", None)
        if graph is None:
            raise ValueError("retry.loop: Engine has no current graph")

        # Check if correction node exists in the graph
        if correct not in graph.graph.nodes:
            raise ValueError(f"retry.loop: Correction node not found: {correct}")

        correction_node_data = graph.graph.nodes[correct]
        correction_func = correction_node_data.get("run")
        if correction_func is None:
            raise ValueError(
                f"retry.loop: Correction node '{correct}' has no run function"
            )

        # Get validation action
        validate_func = registry[validate]

        # Initialize retry tracking
        retry_count = 0
        validation_result: Dict[str, Any] = {}
        retry_errors: List[Dict[str, Any]] = []

        # Create a mutable state for the retry loop
        # We need to track changes from correction nodes
        loop_state = state.copy()

        while retry_count <= max_retries:
            # Set retry count in state (AC: 6)
            loop_state["_retry_count"] = retry_count

            # Prepare validation arguments
            processed_args = validate_args.copy() if validate_args else {}

            # Call validation action (AC: 2-4)
            try:
                validation_result = validate_func(state=loop_state, **processed_args)
            except Exception as e:
                # Validation action failed unexpectedly
                validation_result = {
                    "valid": False,
                    "errors": [{"message": str(e), "type": "validation_error"}],
                }

            # Check if valid (AC: 10)
            is_valid = validation_result.get("valid", False)
            retry_errors = validation_result.get("errors", [])

            if is_valid:
                # Success! Exit loop (AC: 10)
                # Include loop_state updates (from correction nodes) in return
                return {
                    **loop_state,  # Include all state updates from corrections
                    "_retry_count": retry_count,
                    "_retry_errors": [],
                    "_retry_result": validation_result,
                    "_retry_exhausted": False,
                    **validation_result,
                }

            # Validation failed - set error context (AC: 7)
            loop_state["_retry_errors"] = retry_errors

            # Check if we've exhausted retries (AC: 11)
            if retry_count >= max_retries:
                # Max retries exceeded (AC: 9)
                # Include loop_state updates (from correction nodes) in return
                return {
                    **loop_state,  # Include all state updates from corrections
                    "_retry_count": retry_count,
                    "_retry_errors": retry_errors,
                    "_retry_result": validation_result,
                    "_retry_exhausted": True,
                    **validation_result,
                }

            # Execute correction node (AC: 12, 13)
            try:
                correction_result = correction_func(loop_state, **kwargs)
                if isinstance(correction_result, dict):
                    loop_state.update(correction_result)
            except Exception as e:
                # Correction node failed (AC: 14)
                # Include loop_state updates (from previous corrections) in return
                return {
                    **loop_state,  # Include all state updates from corrections so far
                    "_retry_count": retry_count,
                    "_retry_errors": [
                        *retry_errors,
                        {
                            "message": f"Correction node '{correct}' failed: {e}",
                            "type": "correction_error",
                        },
                    ],
                    "_retry_result": {
                        "valid": False,
                        "errors": retry_errors,
                        "correction_error": str(e),
                    },
                    "_retry_exhausted": False,
                    "valid": False,
                }

            # Optional delay between retries
            if retry_delay > 0:
                time.sleep(retry_delay)

            # Increment retry count and continue loop
            retry_count += 1

        # Should not reach here, but safety fallback
        # Include loop_state updates (from correction nodes) in return
        return {
            **loop_state,  # Include all state updates from corrections
            "_retry_count": retry_count,
            "_retry_errors": retry_errors,
            "_retry_result": validation_result,
            "_retry_exhausted": True,
            **validation_result,
        }

    # Register actions (AC: 1)
    registry["retry.loop"] = retry_loop_action
    registry["actions.retry_loop"] = retry_loop_action
