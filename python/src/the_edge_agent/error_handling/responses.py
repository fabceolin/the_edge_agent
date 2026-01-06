"""
Error response template rendering (TEA-BUILTIN-015.6).

Provides HTTP error response rendering with Jinja2 templates
for consistent error responses in API workflows.
"""

import json
import logging
from typing import Any, Dict, Optional

from jinja2 import Environment, BaseLoader, StrictUndefined, TemplateError

from .errors import ErrorInfo
from .settings import ErrorResponseConfig

logger = logging.getLogger(__name__)


class ErrorResponseRenderer:
    """
    Renders HTTP error responses using Jinja2 templates.

    Maps error types to HTTP status codes and response bodies
    using configurable templates.

    Attributes:
        templates: Mapping of error type to ErrorResponseConfig
        jinja_env: Jinja2 environment for template rendering
        default_responses: Built-in error response mappings

    Example:
        >>> renderer = ErrorResponseRenderer({
        ...     "validation_error": ErrorResponseConfig(
        ...         status=422,
        ...         body={"error": "validation", "message": "{{ error.message }}"}
        ...     )
        ... })
        >>> response = renderer.render(error_info)
        >>> # {"status": 422, "body": {...}, "headers": {...}}
    """

    # Default error response mappings
    DEFAULT_RESPONSES = {
        "validation_error": {
            "status": 422,
            "body": {
                "error": "validation_error",
                "message": "{{ error.message }}",
                "details": "{{ error.details }}",
            },
        },
        "auth_error": {
            "status": 401,
            "body": {
                "error": "unauthorized",
                "message": "Authentication required",
            },
        },
        "not_found": {
            "status": 404,
            "body": {
                "error": "not_found",
                "message": "{{ error.message }}",
            },
        },
        "rate_limit": {
            "status": 429,
            "body": {
                "error": "rate_limited",
                "message": "Too many requests",
                "retry_after": "{{ error.details.retry_after | default(60) }}",
            },
        },
        "internal_error": {
            "status": 500,
            "body": {
                "error": "internal_error",
                "message": "An unexpected error occurred",
            },
        },
        "timeout": {
            "status": 504,
            "body": {
                "error": "timeout",
                "message": "{{ error.message }}",
            },
        },
        "service_unavailable": {
            "status": 503,
            "body": {
                "error": "service_unavailable",
                "message": "Service temporarily unavailable",
            },
        },
    }

    # Error type to template name mapping
    ERROR_TYPE_MAPPING = {
        "ValidationError": "validation_error",
        "AuthenticationError": "auth_error",
        "AuthorizationError": "auth_error",
        "NotFoundError": "not_found",
        "RateLimitError": "rate_limit",
        "TooManyRequests": "rate_limit",
        "TimeoutError": "timeout",
        "Timeout": "timeout",
        "ServiceUnavailableError": "service_unavailable",
        "ServiceUnavailable": "service_unavailable",
    }

    def __init__(
        self,
        templates: Optional[Dict[str, ErrorResponseConfig]] = None,
        jinja_env: Optional[Environment] = None,
    ):
        """
        Initialize the error response renderer.

        Args:
            templates: Custom error response templates
            jinja_env: Optional Jinja2 environment to use
        """
        self.templates = templates or {}

        # Create Jinja2 environment if not provided
        if jinja_env is None:
            self.jinja_env = Environment(
                loader=BaseLoader(),
                undefined=StrictUndefined,
            )
            # Add custom filters
            self.jinja_env.filters["tojson"] = json.dumps
        else:
            self.jinja_env = jinja_env

    def get_response_config(
        self,
        error_info: ErrorInfo,
    ) -> Optional[ErrorResponseConfig]:
        """
        Get response config for an error, checking custom then default templates.

        Args:
            error_info: Error information

        Returns:
            ErrorResponseConfig if found, None otherwise
        """
        # Try direct error type match in custom templates
        if error_info.type in self.templates:
            return self.templates[error_info.type]

        # Try mapped error type
        mapped_type = self.ERROR_TYPE_MAPPING.get(error_info.type)
        if mapped_type and mapped_type in self.templates:
            return self.templates[mapped_type]

        # Try default responses
        if mapped_type and mapped_type in self.DEFAULT_RESPONSES:
            config = self.DEFAULT_RESPONSES[mapped_type]
            return ErrorResponseConfig(**config)

        # Fallback to internal_error
        if "internal_error" in self.templates:
            return self.templates["internal_error"]

        config = self.DEFAULT_RESPONSES["internal_error"]
        return ErrorResponseConfig(**config)

    def render(
        self,
        error_info: ErrorInfo,
        extra_context: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Render error response for an error.

        Args:
            error_info: Error information
            extra_context: Additional template context

        Returns:
            Response dict with status, body, and headers
        """
        config = self.get_response_config(error_info)
        if config is None:
            # Fallback response
            return {
                "status": 500,
                "body": {
                    "error": "internal_error",
                    "message": str(error_info.message),
                },
                "headers": {},
            }

        # Build template context
        context = {
            "error": error_info.model_dump(),
            **(extra_context or {}),
        }

        # Render body template
        body = self._render_template_value(config.body, context)

        # Render headers if present
        headers = {}
        if config.headers:
            for key, value in config.headers.items():
                headers[key] = self._render_template_value(value, context)

        return {
            "status": config.status,
            "body": body,
            "headers": headers,
        }

    def _render_template_value(
        self,
        value: Any,
        context: Dict[str, Any],
    ) -> Any:
        """
        Recursively render template values.

        Args:
            value: Value to render (str, dict, list, or primitive)
            context: Template context

        Returns:
            Rendered value
        """
        if isinstance(value, str):
            # Check if it's a template
            if "{{" in value or "{%" in value:
                try:
                    template = self.jinja_env.from_string(value)
                    return template.render(context)
                except TemplateError as e:
                    logger.warning(f"Template error: {e}")
                    return value
            return value

        elif isinstance(value, dict):
            return {
                k: self._render_template_value(v, context) for k, v in value.items()
            }

        elif isinstance(value, list):
            return [self._render_template_value(item, context) for item in value]

        else:
            return value


def render_error_response(
    error_info: ErrorInfo,
    templates: Optional[Dict[str, ErrorResponseConfig]] = None,
    extra_context: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    """
    Convenience function to render error response.

    Args:
        error_info: Error information
        templates: Optional custom templates
        extra_context: Additional template context

    Returns:
        Response dict with status, body, and headers

    Example:
        >>> error_info = ErrorInfo(
        ...     type="ValidationError",
        ...     message="Invalid email format",
        ...     node="validate",
        ... )
        >>> response = render_error_response(error_info)
        >>> # {"status": 422, "body": {...}, "headers": {}}
    """
    renderer = ErrorResponseRenderer(templates)
    return renderer.render(error_info, extra_context)


def get_http_status_for_error(error_info: ErrorInfo) -> int:
    """
    Get HTTP status code for an error type.

    Args:
        error_info: Error information

    Returns:
        HTTP status code
    """
    renderer = ErrorResponseRenderer()
    config = renderer.get_response_config(error_info)
    return config.status if config else 500
