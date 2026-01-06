"""
Endpoint Configuration Schema for per-agent HTTP endpoint definitions.

Story: TEA-BUILTIN-015.7 (HTTP Endpoint Configuration)

Provides Pydantic models for defining HTTP endpoints in YAML agent configurations.
Each agent can define its own API contract including path, method, parameters,
authentication requirements, and schema references.

Example YAML:
    name: research_agent

    endpoint:
      path: "/api/v1/research"
      method: POST
      summary: "Execute research query"
      description: "Searches the web and synthesizes an answer."
      tags:
        - Research
        - AI Agents
      auth:
        required: true
      path_params: {}
      query_params: {}
      request:
        content_type: "application/json"
        schema_ref: input_schema
      response:
        content_type: "application/json"
        schema_ref: output_schema
"""

import re
from enum import Enum
from typing import Any, Dict, List, Optional, Pattern

from pydantic import BaseModel, ConfigDict, Field, field_validator, model_validator


class HTTPMethod(str, Enum):
    """Supported HTTP methods for agent endpoints."""

    GET = "GET"
    POST = "POST"
    PUT = "PUT"
    DELETE = "DELETE"
    PATCH = "PATCH"


class PathParam(BaseModel):
    """
    Configuration for a path parameter.

    Attributes:
        type: Python type string ("str", "int", "float")
        description: Human-readable description
        pattern: Regex pattern for validation
    """

    type: str = Field(default="str", description="Parameter type: str, int, float")
    description: Optional[str] = Field(
        default=None, description="Parameter description"
    )
    pattern: Optional[str] = Field(
        default=None, description="Regex pattern for validation"
    )
    _compiled_pattern: Optional[Pattern] = None

    @field_validator("type", mode="before")
    @classmethod
    def validate_type(cls, v: Any) -> str:
        """Validate type is a supported Python type."""
        valid_types = ("str", "int", "float", "string", "integer", "number")
        if isinstance(v, str) and v.lower() in valid_types:
            # Normalize types
            type_map = {
                "string": "str",
                "integer": "int",
                "number": "float",
            }
            return type_map.get(v.lower(), v.lower())
        return v

    @field_validator("pattern", mode="before")
    @classmethod
    def validate_pattern(cls, v: Any) -> Optional[str]:
        """Validate regex pattern is valid."""
        if v is not None:
            try:
                re.compile(v)
            except re.error as e:
                raise ValueError(f"Invalid regex pattern: {e}")
        return v

    def validate_value(self, value: str) -> Any:
        """
        Validate and convert a path parameter value.

        Args:
            value: Raw string value from URL path

        Returns:
            Converted value (str, int, or float)

        Raises:
            ValueError: If validation fails
        """
        # Pattern validation
        if self.pattern:
            if not re.match(self.pattern, value):
                raise ValueError(
                    f"Value '{value}' does not match pattern '{self.pattern}'"
                )

        # Type conversion
        if self.type == "int":
            try:
                return int(value)
            except ValueError:
                raise ValueError(f"Cannot convert '{value}' to int")
        elif self.type == "float":
            try:
                return float(value)
            except ValueError:
                raise ValueError(f"Cannot convert '{value}' to float")
        return value


class QueryParam(BaseModel):
    """
    Configuration for a query parameter.

    Attributes:
        type: Python type string ("str", "int", "float", "bool", "list")
        required: Whether the parameter is required
        default: Default value if not provided
        description: Human-readable description
        map_to: Input schema field name to map this param to
        min: Minimum value (for int/float)
        max: Maximum value (for int/float)
        min_length: Minimum length (for str/list)
        max_length: Maximum length (for str/list)
    """

    type: str = Field(default="str", description="Parameter type")
    required: bool = Field(default=False, description="Whether parameter is required")
    default: Optional[Any] = Field(default=None, description="Default value")
    description: Optional[str] = Field(
        default=None, description="Parameter description"
    )
    map_to: Optional[str] = Field(
        default=None, description="Input schema field to map to"
    )
    min: Optional[float] = Field(default=None, description="Minimum value (int/float)")
    max: Optional[float] = Field(default=None, description="Maximum value (int/float)")
    min_length: Optional[int] = Field(
        default=None, description="Minimum length (str/list)"
    )
    max_length: Optional[int] = Field(
        default=None, description="Maximum length (str/list)"
    )

    @field_validator("type", mode="before")
    @classmethod
    def validate_type(cls, v: Any) -> str:
        """Validate type is a supported Python type."""
        valid_types = (
            "str",
            "int",
            "float",
            "bool",
            "list",
            "string",
            "integer",
            "number",
            "boolean",
            "array",
        )
        if isinstance(v, str) and v.lower() in valid_types:
            type_map = {
                "string": "str",
                "integer": "int",
                "number": "float",
                "boolean": "bool",
                "array": "list",
            }
            return type_map.get(v.lower(), v.lower())
        return v

    def validate_value(self, value: Any) -> Any:
        """
        Validate and convert a query parameter value.

        Args:
            value: Raw value from query string

        Returns:
            Converted and validated value

        Raises:
            ValueError: If validation fails
        """
        # Handle None for optional params
        if value is None:
            if self.required:
                raise ValueError("Required parameter is missing")
            return self.default

        # Type conversion
        converted: Any
        if self.type == "int":
            try:
                converted = int(value)
            except (ValueError, TypeError):
                raise ValueError(f"Cannot convert '{value}' to int")
        elif self.type == "float":
            try:
                converted = float(value)
            except (ValueError, TypeError):
                raise ValueError(f"Cannot convert '{value}' to float")
        elif self.type == "bool":
            if isinstance(value, bool):
                converted = value
            elif isinstance(value, str):
                converted = value.lower() in ("true", "1", "yes", "on")
            else:
                converted = bool(value)
        elif self.type == "list":
            if isinstance(value, list):
                converted = value
            elif isinstance(value, str):
                converted = [v.strip() for v in value.split(",")]
            else:
                converted = [value]
        else:
            converted = str(value)

        # Range validation for numeric types
        if self.type in ("int", "float"):
            if self.min is not None and converted < self.min:
                raise ValueError(f"Value {converted} is less than minimum {self.min}")
            if self.max is not None and converted > self.max:
                raise ValueError(
                    f"Value {converted} is greater than maximum {self.max}"
                )

        # Length validation for str/list
        if self.type in ("str", "list"):
            length = len(converted)
            if self.min_length is not None and length < self.min_length:
                raise ValueError(
                    f"Length {length} is less than minimum {self.min_length}"
                )
            if self.max_length is not None and length > self.max_length:
                raise ValueError(
                    f"Length {length} is greater than maximum {self.max_length}"
                )

        return converted


class AuthConfig(BaseModel):
    """
    Authentication configuration for an endpoint.

    Attributes:
        required: Whether authentication is required
        roles: Required roles for access (empty = any authenticated user)
    """

    required: bool = Field(default=True, description="Whether auth is required")
    roles: List[str] = Field(
        default_factory=list, description="Required roles for access"
    )

    @field_validator("roles", mode="before")
    @classmethod
    def validate_roles(cls, v: Any) -> List[str]:
        """Accept single role string or list."""
        if isinstance(v, str):
            return [v]
        return v or []


class RequestConfig(BaseModel):
    """
    Request body configuration.

    Attributes:
        content_type: MIME type for request body
        schema_ref: Reference to schema section (e.g., "input_schema")
    """

    content_type: str = Field(
        default="application/json", description="Request content type"
    )
    schema_ref: str = Field(
        default="input_schema", description="Reference to schema section"
    )


class ResponseExample(BaseModel):
    """
    Example response for OpenAPI documentation.

    Attributes:
        summary: Short description of this example
        value: Example response value
    """

    summary: str = Field(description="Short description of this example")
    value: Dict[str, Any] = Field(description="Example response value")


class ResponseConfig(BaseModel):
    """
    Response configuration.

    Attributes:
        content_type: MIME type for response
        schema_ref: Reference to schema section (e.g., "output_schema")
        examples: Named examples for OpenAPI documentation
    """

    content_type: str = Field(
        default="application/json", description="Response content type"
    )
    schema_ref: str = Field(
        default="output_schema", description="Reference to schema section"
    )
    examples: Dict[str, ResponseExample] = Field(
        default_factory=dict, description="Named response examples"
    )

    @field_validator("examples", mode="before")
    @classmethod
    def validate_examples(cls, v: Any) -> Dict[str, ResponseExample]:
        """Convert dict examples to ResponseExample objects."""
        if not v:
            return {}
        result: Dict[str, ResponseExample] = {}
        for name, example in v.items():
            if isinstance(example, dict):
                result[name] = ResponseExample(**example)
            elif isinstance(example, ResponseExample):
                result[name] = example
        return result


class EndpointConfig(BaseModel):
    """
    Complete endpoint configuration for an agent.

    This model represents the full HTTP endpoint specification that can be
    defined in agent YAML files. It supports custom paths, HTTP methods,
    path/query parameters, authentication overrides, and schema linkage.

    Attributes:
        path: URL path for the endpoint (e.g., "/api/v1/research")
        method: HTTP method (GET, POST, PUT, DELETE, PATCH)
        summary: Short description for OpenAPI
        description: Full description for OpenAPI
        tags: Tags for OpenAPI grouping
        auth: Authentication configuration
        path_params: Path parameter definitions
        query_params: Query parameter definitions
        request: Request body configuration
        response: Response configuration

    Example:
        >>> config = EndpointConfig(
        ...     path="/api/v1/users/{user_id}",
        ...     method=HTTPMethod.GET,
        ...     summary="Get user by ID",
        ...     path_params={"user_id": PathParam(type="int")}
        ... )
        >>> config.path
        '/api/v1/users/{user_id}'
    """

    path: str = Field(..., description="URL path for the endpoint")
    method: HTTPMethod = Field(default=HTTPMethod.POST, description="HTTP method")
    summary: Optional[str] = Field(default=None, description="Short OpenAPI summary")
    description: Optional[str] = Field(
        default=None, description="Full OpenAPI description"
    )
    tags: List[str] = Field(default_factory=list, description="OpenAPI tags")
    auth: Optional[AuthConfig] = Field(default=None, description="Auth configuration")
    path_params: Dict[str, PathParam] = Field(
        default_factory=dict, description="Path parameter definitions"
    )
    query_params: Dict[str, QueryParam] = Field(
        default_factory=dict, description="Query parameter definitions"
    )
    request: Optional[RequestConfig] = Field(
        default=None, description="Request body configuration"
    )
    response: Optional[ResponseConfig] = Field(
        default=None, description="Response configuration"
    )

    model_config = ConfigDict(use_enum_values=True)

    @field_validator("path", mode="before")
    @classmethod
    def validate_path(cls, v: Any) -> str:
        """Ensure path starts with / and is valid."""
        if not isinstance(v, str):
            raise ValueError("Path must be a string")
        if not v.startswith("/"):
            v = "/" + v
        # Basic path validation - no double slashes, valid characters
        if "//" in v:
            raise ValueError("Path cannot contain double slashes")
        return v

    @field_validator("method", mode="before")
    @classmethod
    def validate_method(cls, v: Any) -> HTTPMethod:
        """Accept string method names and convert to enum."""
        if isinstance(v, str):
            try:
                return HTTPMethod(v.upper())
            except ValueError:
                valid = [m.value for m in HTTPMethod]
                raise ValueError(f"Invalid HTTP method '{v}'. Valid: {valid}")
        return v

    @field_validator("auth", mode="before")
    @classmethod
    def validate_auth(cls, v: Any) -> Optional[AuthConfig]:
        """Accept dict or AuthConfig."""
        if v is None:
            return None
        if isinstance(v, dict):
            # Handle shorthand: auth: {required: true}
            return AuthConfig(**v)
        return v

    @field_validator("path_params", mode="before")
    @classmethod
    def validate_path_params(cls, v: Any) -> Dict[str, PathParam]:
        """Convert dict to PathParam objects."""
        if not v:
            return {}
        result: Dict[str, PathParam] = {}
        for name, config in v.items():
            if isinstance(config, dict):
                result[name] = PathParam(**config)
            elif isinstance(config, PathParam):
                result[name] = config
            else:
                # Simple type string
                result[name] = PathParam(type=str(config))
        return result

    @field_validator("query_params", mode="before")
    @classmethod
    def validate_query_params(cls, v: Any) -> Dict[str, QueryParam]:
        """Convert dict to QueryParam objects."""
        if not v:
            return {}
        result: Dict[str, QueryParam] = {}
        for name, config in v.items():
            if isinstance(config, dict):
                result[name] = QueryParam(**config)
            elif isinstance(config, QueryParam):
                result[name] = config
            else:
                result[name] = QueryParam(type=str(config))
        return result

    @field_validator("request", mode="before")
    @classmethod
    def validate_request(cls, v: Any) -> Optional[RequestConfig]:
        """Convert dict to RequestConfig."""
        if v is None:
            return None
        if isinstance(v, dict):
            return RequestConfig(**v)
        return v

    @field_validator("response", mode="before")
    @classmethod
    def validate_response(cls, v: Any) -> Optional[ResponseConfig]:
        """Convert dict to ResponseConfig."""
        if v is None:
            return None
        if isinstance(v, dict):
            return ResponseConfig(**v)
        return v

    @model_validator(mode="after")
    def validate_path_params_match(self) -> "EndpointConfig":
        """Validate that path_params match placeholders in path."""
        # Extract {param} placeholders from path
        placeholders = set(re.findall(r"\{(\w+)\}", self.path))
        defined_params = set(self.path_params.keys())

        # Check for undefined placeholders
        undefined = placeholders - defined_params
        if undefined:
            # Auto-create string params for undefined placeholders
            for param in undefined:
                self.path_params[param] = PathParam(type="str")

        return self

    @classmethod
    def from_yaml(cls, yaml_dict: Dict[str, Any]) -> "EndpointConfig":
        """
        Parse endpoint config from YAML dictionary.

        Args:
            yaml_dict: Dictionary from YAML endpoint section

        Returns:
            EndpointConfig instance

        Example:
            >>> config = EndpointConfig.from_yaml({
            ...     "path": "/api/v1/search",
            ...     "method": "GET",
            ...     "query_params": {
            ...         "q": {"type": "str", "required": True}
            ...     }
            ... })
        """
        return cls(**yaml_dict)

    def extract_path_params(self, actual_path: str) -> Dict[str, Any]:
        """
        Extract path parameters from an actual URL path.

        Args:
            actual_path: The actual URL path to extract from

        Returns:
            Dictionary mapping parameter names to validated values

        Raises:
            ValueError: If path doesn't match or validation fails
        """
        # Build regex from path template
        pattern = self.path
        for param_name in self.path_params.keys():
            pattern = pattern.replace(f"{{{param_name}}}", f"(?P<{param_name}>[^/]+)")
        pattern = f"^{pattern}$"

        match = re.match(pattern, actual_path)
        if not match:
            raise ValueError(
                f"Path '{actual_path}' does not match pattern '{self.path}'"
            )

        result: Dict[str, Any] = {}
        for param_name, raw_value in match.groupdict().items():
            param_config = self.path_params.get(param_name)
            if param_config:
                result[param_name] = param_config.validate_value(raw_value)
            else:
                result[param_name] = raw_value

        return result

    def extract_query_params(self, query_dict: Dict[str, Any]) -> Dict[str, Any]:
        """
        Extract and validate query parameters.

        Args:
            query_dict: Raw query parameters from request

        Returns:
            Dictionary mapping parameter names (or map_to targets) to validated values

        Raises:
            ValueError: If validation fails for any parameter
        """
        result: Dict[str, Any] = {}
        errors: List[str] = []

        for param_name, param_config in self.query_params.items():
            raw_value = query_dict.get(param_name)

            try:
                validated = param_config.validate_value(raw_value)

                # Use map_to if specified, otherwise use param name
                target_name = param_config.map_to or param_name
                if validated is not None:
                    result[target_name] = validated
            except ValueError as e:
                errors.append(f"{param_name}: {e}")

        if errors:
            raise ValueError("; ".join(errors))

        return result

    def is_public(self) -> bool:
        """
        Check if this endpoint is public (no auth required).

        Returns:
            True if endpoint is public, False if auth is required.
        """
        if self.auth is None:
            return False  # Default to requiring auth
        return not self.auth.required

    def requires_role(self, role: str) -> bool:
        """
        Check if a specific role is required for this endpoint.

        Args:
            role: Role name to check

        Returns:
            True if the role is required.
        """
        if self.auth is None or not self.auth.roles:
            return False
        return role in self.auth.roles


def parse_endpoint_config(agent_config: Dict[str, Any]) -> Optional[EndpointConfig]:
    """
    Parse endpoint configuration from an agent's YAML config.

    Args:
        agent_config: Full agent configuration dictionary

    Returns:
        EndpointConfig if endpoint section exists, None otherwise.

    Example:
        >>> config = parse_endpoint_config({
        ...     "name": "research_agent",
        ...     "endpoint": {"path": "/research", "method": "POST"}
        ... })
        >>> config.path
        '/research'
    """
    endpoint_section = agent_config.get("endpoint")
    if not endpoint_section:
        return None

    if not isinstance(endpoint_section, dict):
        return None

    try:
        return EndpointConfig.from_yaml(endpoint_section)
    except Exception:
        return None
