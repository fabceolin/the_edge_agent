"""
Error handling settings models (TEA-BUILTIN-015.6).

Pydantic models for validating error handling configuration
from YAML settings.
"""

from enum import Enum
from typing import Any, Dict, List, Optional, Set, Union
from pydantic import BaseModel, Field, field_validator


class ErrorMode(str, Enum):
    """Error handling mode for YAML workflows."""

    RAISE = "raise"  # Propagate exceptions (default, backward compatible)
    GRACEFUL = "graceful"  # Continue with error in state
    RETRY = "retry"  # Retry before failing


class ErrorResponseConfig(BaseModel):
    """
    HTTP error response configuration.

    Defines how errors map to HTTP responses with status codes
    and Jinja2 template bodies.

    Attributes:
        status: HTTP status code (e.g., 422, 401, 500)
        body: Response body template (supports Jinja2)
        headers: Optional response headers

    Example YAML:
        ```yaml
        validation_error:
          status: 422
          body:
            error: "validation_error"
            message: "{{ error.message }}"
            details: "{{ error.details }}"
        ```
    """

    status: int = Field(..., ge=100, le=599, description="HTTP status code")
    body: Union[Dict[str, Any], str] = Field(..., description="Response body template")
    headers: Optional[Dict[str, str]] = Field(
        None, description="Optional response headers"
    )


class NodeErrorSettings(BaseModel):
    """
    Node-level error handling override.

    Allows individual nodes to override global error handling
    settings via the `on_error` block.

    Attributes:
        mode: Error mode override (raise, graceful, retry)
        max_retries: Maximum retry attempts
        retry_delay: Initial delay between retries (seconds)
        backoff_multiplier: Exponential backoff multiplier
        fallback: Node name to route to on error
        capture_traceback: Whether to capture full traceback
        retryable_errors: Custom set of retryable error types

    Example YAML:
        ```yaml
        nodes:
          - name: call_api
            uses: http.request
            on_error:
              mode: retry
              max_retries: 5
              fallback: use_cache
        ```
    """

    mode: Optional[ErrorMode] = Field(None, description="Error mode override")
    max_retries: Optional[int] = Field(
        None, ge=0, le=100, description="Maximum retry attempts"
    )
    retry_delay: Optional[float] = Field(
        None, ge=0, le=300, description="Initial retry delay in seconds"
    )
    backoff_multiplier: Optional[float] = Field(
        None, ge=1.0, le=10.0, description="Exponential backoff multiplier"
    )
    fallback: Optional[str] = Field(None, description="Fallback node name on error")
    capture_traceback: Optional[bool] = Field(
        None, description="Capture full traceback in error info"
    )
    retryable_errors: Optional[List[str]] = Field(
        None, description="Custom retryable error types"
    )

    def merge_with_global(
        self, global_settings: "ErrorHandlingSettings"
    ) -> "ErrorHandlingSettings":
        """
        Merge node settings with global settings.

        Node-level settings take precedence over global settings.

        Args:
            global_settings: Global error handling settings

        Returns:
            Merged ErrorHandlingSettings with node overrides applied
        """
        return ErrorHandlingSettings(
            mode=self.mode if self.mode is not None else global_settings.mode,
            max_retries=(
                self.max_retries
                if self.max_retries is not None
                else global_settings.max_retries
            ),
            retry_delay=(
                self.retry_delay
                if self.retry_delay is not None
                else global_settings.retry_delay
            ),
            backoff_multiplier=(
                self.backoff_multiplier
                if self.backoff_multiplier is not None
                else global_settings.backoff_multiplier
            ),
            retryable_errors=(
                self.retryable_errors
                if self.retryable_errors is not None
                else global_settings.retryable_errors
            ),
            capture_traceback=(
                self.capture_traceback
                if self.capture_traceback is not None
                else global_settings.capture_traceback
            ),
            clear_on_success=global_settings.clear_on_success,
            error_responses=global_settings.error_responses,
            # Node-specific settings
            fallback=self.fallback,
        )


class ErrorHandlingSettings(BaseModel):
    """
    Global error handling configuration.

    Parsed from settings.error_handling in YAML configuration.

    Attributes:
        mode: Default error handling mode
        max_retries: Maximum retry attempts for retry mode
        retry_delay: Initial delay between retries (seconds)
        backoff_multiplier: Exponential backoff multiplier
        retryable_errors: List of error types to retry
        capture_traceback: Include full traceback in error info
        clear_on_success: Clear __error__ after successful execution
        error_responses: HTTP error response templates by error type
        fallback: Default fallback node (node-level only)

    Example YAML:
        ```yaml
        settings:
          error_handling:
            mode: retry
            max_retries: 3
            retry_delay: 1.0
            backoff_multiplier: 2.0
            retryable_errors:
              - TimeoutError
              - ConnectionError
            capture_traceback: false
            clear_on_success: true
            error_responses:
              validation_error:
                status: 422
                body:
                  error: validation_error
                  message: "{{ error.message }}"
        ```
    """

    mode: ErrorMode = Field(ErrorMode.RAISE, description="Error handling mode")
    max_retries: int = Field(3, ge=0, le=100, description="Maximum retry attempts")
    retry_delay: float = Field(
        1.0, ge=0, le=300, description="Initial retry delay in seconds"
    )
    backoff_multiplier: float = Field(
        2.0, ge=1.0, le=10.0, description="Exponential backoff multiplier"
    )
    retryable_errors: Optional[List[str]] = Field(
        None, description="Custom retryable error types"
    )
    capture_traceback: bool = Field(
        False, description="Include full traceback in error info"
    )
    clear_on_success: bool = Field(
        True, description="Clear __error__ after successful node execution"
    )
    error_responses: Optional[Dict[str, ErrorResponseConfig]] = Field(
        None, description="HTTP error response templates"
    )
    fallback: Optional[str] = Field(
        None, description="Fallback node name (node-level only)"
    )

    @field_validator("retryable_errors", mode="before")
    @classmethod
    def convert_error_aliases(cls, v: Optional[List[str]]) -> Optional[List[str]]:
        """Convert shorthand error aliases to full class names."""
        if v is None:
            return None

        aliases = {
            "timeout": "TimeoutError",
            "connection_error": "ConnectionError",
            "rate_limit": "RateLimitError",
            "service_unavailable": "ServiceUnavailableError",
            "http_error": "HTTPError",
        }

        return [aliases.get(e.lower(), e) for e in v]

    def get_retryable_set(self) -> Optional[Set[str]]:
        """Get retryable errors as a set for fast lookup."""
        if self.retryable_errors is None:
            return None
        return set(self.retryable_errors)

    @classmethod
    def default(cls) -> "ErrorHandlingSettings":
        """Create default settings (backward compatible - raise mode)."""
        return cls(mode=ErrorMode.RAISE)

    @classmethod
    def from_yaml(cls, config: Dict[str, Any]) -> "ErrorHandlingSettings":
        """
        Parse settings from YAML configuration dictionary.

        Args:
            config: Dictionary from settings.error_handling YAML section

        Returns:
            Validated ErrorHandlingSettings instance

        Raises:
            ValidationError: If configuration is invalid
        """
        # Convert error_responses to ErrorResponseConfig instances
        if "error_responses" in config and isinstance(config["error_responses"], dict):
            error_responses = {}
            for name, response_config in config["error_responses"].items():
                if isinstance(response_config, dict):
                    error_responses[name] = ErrorResponseConfig(**response_config)
                else:
                    error_responses[name] = response_config
            config["error_responses"] = error_responses

        return cls(**config)


def parse_node_error_settings(
    on_error_config: Optional[Dict[str, Any]]
) -> Optional[NodeErrorSettings]:
    """
    Parse on_error block from node configuration.

    Args:
        on_error_config: Dictionary from node's on_error YAML block

    Returns:
        Parsed NodeErrorSettings or None if not configured
    """
    if on_error_config is None:
        return None

    return NodeErrorSettings(**on_error_config)
