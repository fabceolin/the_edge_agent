"""
Error handling actions for YAML workflows (TEA-BUILTIN-015.6).

Provides actions for manual error control:
- error.is_retryable: Check if current error is retryable
- error.clear: Clear error from state
- error.retry: Retry last failed action

These actions allow workflow authors to implement custom error
handling logic within YAML workflows.
"""

import logging
from typing import Any, Callable, Dict, Optional

logger = logging.getLogger(__name__)


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register error handling actions in the actions registry.

    Args:
        registry: The actions registry to populate
        engine: YAMLEngine instance for accessing error handler
    """

    async def error_is_retryable(
        state: Dict[str, Any],
        **kwargs,
    ) -> bool:
        """
        Check if the current error is retryable.

        Examines the __error__ field in state to determine if
        the error type is in the retryable errors set.

        Args:
            state: Current workflow state

        Returns:
            True if error is retryable, False otherwise

        Example YAML:
            ```yaml
            - name: check_retry
              uses: error.is_retryable
              output: can_retry

            - name: handle_error
              if: "{{ state.can_retry }}"
              goto: retry_node
            ```
        """
        error = state.get("__error__", {})
        if not error:
            return False

        return error.get("is_retryable", False)

    async def error_clear(
        state: Dict[str, Any],
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Clear error from state.

        Removes the __error__ field from state, allowing
        the workflow to continue cleanly.

        Args:
            state: Current workflow state

        Returns:
            State update with __error__ set to None

        Example YAML:
            ```yaml
            - name: clear_error
              uses: error.clear

            - name: continue_normally
              # __error__ is now cleared
            ```
        """
        logger.debug("Clearing error from state")
        return {"__error__": None}

    async def error_get(
        state: Dict[str, Any],
        **kwargs,
    ) -> Optional[Dict[str, Any]]:
        """
        Get the current error info from state.

        Returns the full __error__ dict for inspection.

        Args:
            state: Current workflow state

        Returns:
            Error info dict or None if no error

        Example YAML:
            ```yaml
            - name: get_error_info
              uses: error.get
              output: current_error

            - name: log_error
              run: |
                if state.get("current_error"):
                    print(f"Error type: {state['current_error']['type']}")
            ```
        """
        return state.get("__error__")

    async def error_has(
        state: Dict[str, Any],
        **kwargs,
    ) -> bool:
        """
        Check if there is an error in state.

        Args:
            state: Current workflow state

        Returns:
            True if __error__ is present and not None

        Example YAML:
            ```yaml
            - name: check_error
              uses: error.has
              output: has_error

            - name: handle_error
              if: "{{ state.has_error }}"
              goto: error_handler
            ```
        """
        error = state.get("__error__")
        return error is not None and error != {}

    async def error_type(
        state: Dict[str, Any],
        **kwargs,
    ) -> Optional[str]:
        """
        Get the error type from state.

        Args:
            state: Current workflow state

        Returns:
            Error type string or None if no error

        Example YAML:
            ```yaml
            - name: get_type
              uses: error.type
              output: error_type

            - name: route_by_type
              goto:
                TimeoutError: handle_timeout
                RateLimitError: handle_rate_limit
                default: handle_generic
              condition: "{{ state.error_type }}"
            ```
        """
        error = state.get("__error__", {})
        return error.get("type") if error else None

    async def error_retry(
        max_attempts: int = 1,
        state: Dict[str, Any] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Retry the last failed action.

        This action requires execution context tracking to know
        what action to retry. It should be used after an error
        occurs to attempt recovery.

        Args:
            max_attempts: Maximum number of retry attempts
            state: Current workflow state

        Returns:
            Result from retried action or error state

        Note:
            This action requires the error handler to track
            execution context. If no context is available,
            returns an error.

        Example YAML:
            ```yaml
            - name: check_and_retry
              if: "{{ state.__error__ and state.__error__.is_retryable }}"
              uses: error.retry
              with:
                max_attempts: 2
              output: retry_result
            ```
        """
        if state is None:
            state = {}

        error = state.get("__error__")
        if not error:
            return {"__retry_error__": "No error to retry"}

        # Check if error is retryable
        if not error.get("is_retryable", False):
            return {
                "__retry_error__": f"Error type {error.get('type')} is not retryable"
            }

        # Get retry context from engine if available
        retry_executor = getattr(engine, "_retry_executor", None)
        if retry_executor is None:
            return {
                "__retry_error__": "No retry context available. "
                "error.retry requires the action to be executed "
                "within an error handler with retry tracking."
            }

        try:
            result = await retry_executor.retry_last(max_attempts=max_attempts)
            return {
                "__retry_success__": True,
                "__retry_result__": result,
                "__error__": None,  # Clear error on success
            }
        except Exception as e:
            from ..error_handling import classify_error, ErrorInfo

            error_type, is_retryable = classify_error(e)
            return {
                "__retry_success__": False,
                "__error__": ErrorInfo(
                    type=error_type,
                    message=str(e),
                    node=error.get("node", "unknown"),
                    action=error.get("action"),
                    retry_count=error.get("retry_count", 0) + max_attempts,
                    is_retryable=is_retryable,
                ).model_dump(),
            }

    async def error_respond(
        state: Dict[str, Any],
        template: Optional[str] = None,
        status: Optional[int] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Generate HTTP error response from current error.

        Uses error response templates to convert the current
        __error__ into an HTTP response format.

        Args:
            state: Current workflow state
            template: Optional template name to use (overrides auto-mapping)
            status: Optional status code override

        Returns:
            Dict with __http_response__ containing status, body, headers

        Example YAML:
            ```yaml
            - name: convert_to_response
              uses: error.respond
              output: http_error

            - name: return_error
              uses: http.respond
              with:
                status: "{{ state.http_error.status }}"
                body: "{{ state.http_error.body | tojson }}"
            ```
        """
        error = state.get("__error__")
        if not error:
            return {
                "__http_response__": {
                    "status": 200,
                    "body": {},
                    "headers": {},
                }
            }

        from ..error_handling import ErrorInfo, render_error_response

        error_info = ErrorInfo(**error)
        response = render_error_response(error_info)

        # Apply overrides
        if status is not None:
            response["status"] = status

        return {"__http_response__": response}

    # Register actions with both naming conventions
    registry["error.is_retryable"] = error_is_retryable
    registry["error.clear"] = error_clear
    registry["error.get"] = error_get
    registry["error.has"] = error_has
    registry["error.type"] = error_type
    registry["error.retry"] = error_retry
    registry["error.respond"] = error_respond

    # Legacy naming convention
    registry["actions.error_is_retryable"] = error_is_retryable
    registry["actions.error_clear"] = error_clear
    registry["actions.error_get"] = error_get
    registry["actions.error_has"] = error_has
    registry["actions.error_type"] = error_type
    registry["actions.error_retry"] = error_retry
    registry["actions.error_respond"] = error_respond
