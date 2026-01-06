"""
OpenAPI Specification Generation for TEA Server.

Story: TEA-BUILTIN-015.7 (HTTP Endpoint Configuration)
Story: TEA-BUILTIN-015.8 (Health & Metadata Endpoints)

Provides dynamic OpenAPI 3.0 specification generation from:
- Agent registry (TEA-BUILTIN-015.8)
- Route registry with EndpointConfig (TEA-BUILTIN-015.7)

Example:
    >>> from the_edge_agent.http import generate_openapi_spec, RouteRegistry
    >>>
    >>> # From agent registry (legacy)
    >>> agent_registry = {
    ...     "research": {
    ...         "description": "Research agent",
    ...         "endpoint": {"path": "/api/v1/research", "method": "POST"},
    ...         "input_schema": {"query": {"type": "str", "required": True}},
    ...     }
    ... }
    >>> spec = generate_openapi_spec(agent_registry, "my-agents", "1.0.0")
    >>>
    >>> # From route registry (015.7)
    >>> registry = RouteRegistry()
    >>> spec = generate_openapi_from_routes(registry, "my-agents", "1.0.0")
"""

import logging
from typing import Any, Dict, List, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from .router import RouteRegistry
    from .endpoint import EndpointConfig

logger = logging.getLogger(__name__)

# Try to import FastAPI, but provide fallback if not available
try:
    from fastapi import APIRouter
    from fastapi.responses import JSONResponse

    FASTAPI_AVAILABLE = True
except ImportError:
    FASTAPI_AVAILABLE = False
    APIRouter = None  # type: ignore
    JSONResponse = None  # type: ignore

# Global references (set by configure_openapi)
_agent_registry: Optional[Dict[str, Any]] = None
_settings: Dict[str, Any] = {}
_auth_schemes: Dict[str, Any] = {}


def configure_openapi(
    agent_registry: Optional[Dict[str, Any]] = None,
    settings: Optional[Dict[str, Any]] = None,
    auth_schemes: Optional[Dict[str, Any]] = None,
) -> None:
    """
    Configure OpenAPI generation with agent registry and settings.

    Args:
        agent_registry: Dictionary mapping agent names to their info dicts.
        settings: Server settings dict with service_name and version.
        auth_schemes: Authentication schemes configuration.
    """
    global _agent_registry, _settings, _auth_schemes
    _agent_registry = agent_registry
    _settings = settings or {}
    _auth_schemes = auth_schemes or {}


def _convert_type_to_openapi(type_info: Any) -> Dict[str, Any]:
    """
    Convert Python type info to OpenAPI schema format.

    Args:
        type_info: Type information (string like "str", "int", or dict)

    Returns:
        OpenAPI schema object.
    """
    if isinstance(type_info, dict):
        # Already a dict with type info
        result: Dict[str, Any] = {}
        python_type = type_info.get("type", "string")

        # Map Python types to OpenAPI types
        type_mapping = {
            "str": "string",
            "string": "string",
            "int": "integer",
            "integer": "integer",
            "float": "number",
            "number": "number",
            "bool": "boolean",
            "boolean": "boolean",
            "list": "array",
            "array": "array",
            "dict": "object",
            "object": "object",
        }

        openapi_type = type_mapping.get(str(python_type).lower(), "string")
        result["type"] = openapi_type

        if "default" in type_info:
            result["default"] = type_info["default"]
        if "description" in type_info:
            result["description"] = type_info["description"]
        if openapi_type == "array" and "items" in type_info:
            result["items"] = _convert_type_to_openapi(type_info["items"])

        return result
    elif isinstance(type_info, str):
        # Simple type string
        type_mapping = {
            "str": "string",
            "string": "string",
            "int": "integer",
            "integer": "integer",
            "float": "number",
            "number": "number",
            "bool": "boolean",
            "boolean": "boolean",
            "list": "array",
            "array": "array",
            "dict": "object",
            "object": "object",
        }
        return {"type": type_mapping.get(type_info.lower(), "string")}
    else:
        return {"type": "string"}


def _build_schema_from_dict(
    schema_dict: Optional[Dict[str, Any]],
) -> Dict[str, Any]:
    """
    Build OpenAPI schema from agent input/output schema dict.

    Args:
        schema_dict: Dictionary mapping field names to type info.

    Returns:
        OpenAPI schema object with type, properties, and required fields.
    """
    if not schema_dict:
        return {"type": "object"}

    properties: Dict[str, Any] = {}
    required: List[str] = []

    for field_name, type_info in schema_dict.items():
        properties[field_name] = _convert_type_to_openapi(type_info)

        # Check if field is required
        if isinstance(type_info, dict) and type_info.get("required", False):
            required.append(field_name)

    result: Dict[str, Any] = {
        "type": "object",
        "properties": properties,
    }
    if required:
        result["required"] = required

    return result


def generate_openapi_spec(
    agent_registry: Optional[Dict[str, Any]] = None,
    service_name: str = "tea-agents",
    version: str = "1.0.0",
    auth_schemes: Optional[Dict[str, Any]] = None,
    servers: Optional[List[Dict[str, str]]] = None,
) -> Dict[str, Any]:
    """
    Generate OpenAPI 3.0 specification from agent registry.

    Args:
        agent_registry: Dictionary mapping agent names to their info.
                       If None, uses the globally configured registry.
        service_name: Service name for OpenAPI info.
        version: API version for OpenAPI info.
        auth_schemes: Authentication scheme definitions.
        servers: List of server objects with url and description.

    Returns:
        OpenAPI 3.0 specification dictionary.

    Example:
        >>> spec = generate_openapi_spec(
        ...     {"research": {"description": "Research agent", ...}},
        ...     "my-api", "1.0.0"
        ... )
        >>> spec["openapi"]
        '3.0.3'
    """
    registry = agent_registry if agent_registry is not None else _agent_registry

    # Build OpenAPI spec
    spec: Dict[str, Any] = {
        "openapi": "3.0.3",
        "info": {
            "title": service_name,
            "version": version,
            "description": f"TEA Agents API for {service_name}",
        },
        "paths": {},
    }

    # Add servers if provided
    if servers:
        spec["servers"] = servers

    # Add paths for each agent
    if registry:
        for agent_name, agent_info in registry.items():
            endpoint = agent_info.get("endpoint", {})
            if isinstance(endpoint, dict):
                path = endpoint.get("path", f"/run-agent/{agent_name}")
                method = endpoint.get("method", "POST").lower()
            else:
                path = f"/run-agent/{agent_name}"
                method = "post"

            # Build request body schema
            input_schema = agent_info.get("input_schema")
            request_body = None
            if input_schema:
                request_body = {
                    "required": True,
                    "content": {
                        "application/json": {
                            "schema": _build_schema_from_dict(input_schema),
                        },
                    },
                }

            # Build response schema
            output_schema = agent_info.get("output_schema")
            responses = {
                "200": {
                    "description": "Successful response",
                    "content": {
                        "application/json": {
                            "schema": _build_schema_from_dict(output_schema),
                        },
                    },
                },
                "400": {
                    "description": "Bad request",
                },
                "500": {
                    "description": "Internal server error",
                },
            }

            # Build operation object
            operation: Dict[str, Any] = {
                "summary": agent_info.get("description", f"Execute {agent_name}"),
                "operationId": f"run_{agent_name.replace('-', '_')}",
                "tags": ["agents"],
                "responses": responses,
            }
            if request_body:
                operation["requestBody"] = request_body

            # Add security if auth is required
            agent_settings = agent_info.get("settings", {})
            if isinstance(agent_settings, dict) and agent_settings.get("auth", {}).get(
                "required", False
            ):
                operation["security"] = [{"bearerAuth": []}]

            # Add path to spec
            if path not in spec["paths"]:
                spec["paths"][path] = {}
            spec["paths"][path][method] = operation

    # Add security schemes if auth is configured
    security_schemes = auth_schemes or _auth_schemes
    if security_schemes:
        spec["components"] = {"securitySchemes": security_schemes}
    else:
        # Add default bearer auth scheme
        spec["components"] = {
            "securitySchemes": {
                "bearerAuth": {
                    "type": "http",
                    "scheme": "bearer",
                    "bearerFormat": "JWT",
                },
            },
        }

    return spec


async def _openapi_handler() -> Dict[str, Any]:
    """
    OpenAPI specification endpoint handler.

    Returns:
        OpenAPI 3.0 specification dictionary.
    """
    return generate_openapi_spec(
        agent_registry=_agent_registry,
        service_name=_settings.get("service_name", "tea-agents"),
        version=_settings.get("version", "1.0.0"),
        auth_schemes=_auth_schemes,
    )


def create_openapi_router(path: str = "/openapi.json") -> Optional["APIRouter"]:
    """
    Create a router with the OpenAPI endpoint at a custom path.

    Args:
        path: Custom path for the OpenAPI endpoint.

    Returns:
        FastAPI APIRouter with OpenAPI endpoint, or None if FastAPI not available.
    """
    if not FASTAPI_AVAILABLE:
        return None

    openapi_router = APIRouter(tags=["openapi"])

    @openapi_router.get(path)
    async def openapi():
        return await _openapi_handler()

    return openapi_router


# TEA-BUILTIN-015.7: OpenAPI generation from RouteRegistry


def _build_path_params_openapi(config: "EndpointConfig") -> List[Dict[str, Any]]:
    """
    Build OpenAPI path parameters from EndpointConfig.

    Args:
        config: Endpoint configuration with path_params

    Returns:
        List of OpenAPI parameter objects.
    """
    params: List[Dict[str, Any]] = []

    for param_name, param_config in config.path_params.items():
        # Map Python types to OpenAPI types
        type_mapping = {
            "str": "string",
            "int": "integer",
            "float": "number",
        }
        openapi_type = type_mapping.get(param_config.type, "string")

        param: Dict[str, Any] = {
            "name": param_name,
            "in": "path",
            "required": True,
            "schema": {"type": openapi_type},
        }

        if param_config.description:
            param["description"] = param_config.description
        if param_config.pattern:
            param["schema"]["pattern"] = param_config.pattern

        params.append(param)

    return params


def _build_query_params_openapi(config: "EndpointConfig") -> List[Dict[str, Any]]:
    """
    Build OpenAPI query parameters from EndpointConfig.

    Args:
        config: Endpoint configuration with query_params

    Returns:
        List of OpenAPI parameter objects.
    """
    params: List[Dict[str, Any]] = []

    for param_name, param_config in config.query_params.items():
        # Map Python types to OpenAPI types
        type_mapping = {
            "str": "string",
            "int": "integer",
            "float": "number",
            "bool": "boolean",
            "list": "array",
        }
        openapi_type = type_mapping.get(param_config.type, "string")

        schema: Dict[str, Any] = {"type": openapi_type}
        if param_config.default is not None:
            schema["default"] = param_config.default
        if param_config.min is not None:
            schema["minimum"] = param_config.min
        if param_config.max is not None:
            schema["maximum"] = param_config.max
        if openapi_type == "array":
            schema["items"] = {"type": "string"}

        param: Dict[str, Any] = {
            "name": param_name,
            "in": "query",
            "required": param_config.required,
            "schema": schema,
        }

        if param_config.description:
            param["description"] = param_config.description

        params.append(param)

    return params


def _build_response_examples_openapi(
    config: "EndpointConfig",
) -> Dict[str, Any]:
    """
    Build OpenAPI response examples from EndpointConfig.

    Args:
        config: Endpoint configuration with response.examples

    Returns:
        OpenAPI examples object.
    """
    if not config.response or not config.response.examples:
        return {}

    examples: Dict[str, Any] = {}
    for name, example in config.response.examples.items():
        examples[name] = {
            "summary": example.summary,
            "value": example.value,
        }

    return examples


def generate_openapi_from_routes(
    registry: "RouteRegistry",
    service_name: str = "tea-agents",
    version: str = "1.0.0",
    description: str = "",
    auth_schemes: Optional[Dict[str, Any]] = None,
    servers: Optional[List[Dict[str, str]]] = None,
) -> Dict[str, Any]:
    """
    Generate OpenAPI 3.0 specification from RouteRegistry.

    TEA-BUILTIN-015.7: Uses EndpointConfig for rich OpenAPI generation including
    path/query parameters, authentication, and response examples.

    Args:
        registry: RouteRegistry with registered agent routes
        service_name: Service name for OpenAPI info
        version: API version for OpenAPI info
        description: API description
        auth_schemes: Authentication scheme definitions
        servers: List of server objects with url and description

    Returns:
        OpenAPI 3.0 specification dictionary.

    Example:
        >>> registry = RouteRegistry()
        >>> registry.register("research", endpoint_config, handler)
        >>> spec = generate_openapi_from_routes(registry, "my-api", "1.0.0")
        >>> print(spec["openapi"])
        '3.0.3'
    """
    # Build base OpenAPI spec
    spec: Dict[str, Any] = {
        "openapi": "3.0.3",
        "info": {
            "title": service_name,
            "version": version,
            "description": description or f"TEA Agents API for {service_name}",
        },
        "paths": {},
    }

    # Add servers if provided
    if servers:
        spec["servers"] = servers

    # Add paths for each registered route
    for route_key, route in registry.routes.items():
        config = route.config
        method = (
            config.method if isinstance(config.method, str) else config.method.value
        )
        method_lower = method.lower()

        # Build parameters (path + query)
        parameters: List[Dict[str, Any]] = []
        parameters.extend(_build_path_params_openapi(config))
        parameters.extend(_build_query_params_openapi(config))

        # Build request body schema from input_schema
        request_body = None
        if method_lower != "get" and route.input_schema:
            request_body = {
                "required": True,
                "content": {
                    (
                        config.request.content_type
                        if config.request
                        else "application/json"
                    ): {
                        "schema": _build_schema_from_dict(route.input_schema),
                    },
                },
            }

        # Build response schema from output_schema
        response_content: Dict[str, Any] = {}
        if route.output_schema:
            content_type = (
                config.response.content_type if config.response else "application/json"
            )
            response_content = {
                content_type: {
                    "schema": _build_schema_from_dict(route.output_schema),
                },
            }
            # Add examples if defined
            examples = _build_response_examples_openapi(config)
            if examples:
                response_content[content_type]["examples"] = examples

        responses: Dict[str, Any] = {
            "200": {
                "description": "Successful response",
            },
            "400": {"description": "Bad request - validation error"},
            "500": {"description": "Internal server error"},
        }
        if response_content:
            responses["200"]["content"] = response_content

        # Add auth error responses if auth required
        if not config.is_public():
            responses["401"] = {"description": "Unauthorized - authentication required"}
            if config.auth and config.auth.roles:
                responses["403"] = {
                    "description": "Forbidden - insufficient permissions"
                }

        # Build operation object
        operation: Dict[str, Any] = {
            "summary": config.summary or f"Execute {route.agent_name}",
            "operationId": f"run_{route.agent_name.replace('-', '_').replace('.', '_')}",
            "tags": config.tags or ["agents"],
            "responses": responses,
        }

        if config.description:
            operation["description"] = config.description
        if parameters:
            operation["parameters"] = parameters
        if request_body:
            operation["requestBody"] = request_body

        # Add security if auth is required
        if not config.is_public():
            operation["security"] = [{"bearerAuth": []}]

        # Add path to spec
        if config.path not in spec["paths"]:
            spec["paths"][config.path] = {}
        spec["paths"][config.path][method_lower] = operation

    # Add security schemes
    security_schemes = auth_schemes or {}
    if not security_schemes:
        # Add default bearer auth scheme
        security_schemes = {
            "bearerAuth": {
                "type": "http",
                "scheme": "bearer",
                "bearerFormat": "JWT",
            },
        }

    spec["components"] = {"securitySchemes": security_schemes}

    return spec
