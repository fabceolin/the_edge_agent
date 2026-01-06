"""
Route Registry and FastAPI Router Factory for TEA Agents.

Story: TEA-BUILTIN-015.7 (HTTP Endpoint Configuration)

Provides a RouteRegistry that collects agent routes from YAML endpoint
configurations and generates FastAPI routers for use in server applications.

Example:
    >>> from the_edge_agent.http import RouteRegistry
    >>>
    >>> registry = RouteRegistry()
    >>> registry.register(
    ...     agent_name="research",
    ...     config=EndpointConfig(path="/api/v1/research", method="POST"),
    ...     handler=lambda input_state: agent.run(input_state)
    ... )
    >>> router = registry.get_fastapi_router()
    >>> app.include_router(router)
"""

import logging
from dataclasses import dataclass, field
from typing import Any, Callable, Dict, List, Optional, Union

from .endpoint import EndpointConfig, HTTPMethod

logger = logging.getLogger(__name__)

# Try to import FastAPI, but provide fallback if not available
try:
    from fastapi import APIRouter, Request, Depends, HTTPException
    from fastapi.responses import JSONResponse

    FASTAPI_AVAILABLE = True
except ImportError:
    FASTAPI_AVAILABLE = False
    APIRouter = None  # type: ignore
    Request = None  # type: ignore
    Depends = None  # type: ignore
    HTTPException = None  # type: ignore
    JSONResponse = None  # type: ignore


@dataclass
class RegisteredRoute:
    """
    A registered agent route with its configuration and handler.

    Attributes:
        path: URL path for the route
        method: HTTP method
        agent_name: Name of the agent this route serves
        handler: Callable that executes the agent
        config: Full endpoint configuration
        input_schema: Agent's input schema (if any)
        output_schema: Agent's output schema (if any)
    """

    path: str
    method: str
    agent_name: str
    handler: Callable
    config: EndpointConfig
    input_schema: Optional[Dict[str, Any]] = None
    output_schema: Optional[Dict[str, Any]] = None


class RouteRegistry:
    """
    Registry of agent routes for HTTP endpoint management.

    The RouteRegistry collects agent endpoint configurations and provides
    methods to generate FastAPI routers and OpenAPI specifications.

    Attributes:
        _routes: Internal dictionary of registered routes keyed by "{method}:{path}"
        _auth_handler: Optional authentication handler function

    Example:
        >>> registry = RouteRegistry()
        >>> registry.register("research", endpoint_config, handler_fn)
        >>> router = registry.get_fastapi_router()
    """

    def __init__(self, auth_handler: Optional[Callable] = None):
        """
        Initialize the route registry.

        Args:
            auth_handler: Optional async function for authentication.
                         Signature: async (request: Request, config: EndpointConfig) -> Optional[dict]
                         Should return user info dict or raise HTTPException.
        """
        self._routes: Dict[str, RegisteredRoute] = {}
        self._auth_handler = auth_handler

    def register(
        self,
        agent_name: str,
        config: EndpointConfig,
        handler: Callable,
        input_schema: Optional[Dict[str, Any]] = None,
        output_schema: Optional[Dict[str, Any]] = None,
    ) -> None:
        """
        Register an agent route.

        Args:
            agent_name: Unique name for the agent
            config: Endpoint configuration from YAML
            handler: Callable that executes the agent.
                    Signature: (input_state: dict) -> dict or async generator
            input_schema: Agent's input schema for request validation
            output_schema: Agent's output schema for response formatting

        Raises:
            ValueError: If a route with the same path/method already exists

        Example:
            >>> registry.register(
            ...     "research",
            ...     EndpointConfig(path="/research", method="POST"),
            ...     lambda state: {"result": "done"}
            ... )
        """
        method = (
            config.method if isinstance(config.method, str) else config.method.value
        )
        route_key = f"{method}:{config.path}"

        if route_key in self._routes:
            existing = self._routes[route_key]
            raise ValueError(
                f"Route conflict: {route_key} already registered by agent "
                f"'{existing.agent_name}'. Cannot register for '{agent_name}'."
            )

        self._routes[route_key] = RegisteredRoute(
            path=config.path,
            method=method,
            agent_name=agent_name,
            handler=handler,
            config=config,
            input_schema=input_schema,
            output_schema=output_schema,
        )

        logger.debug(f"Registered route: {method} {config.path} -> {agent_name}")

    def unregister(self, agent_name: str) -> int:
        """
        Unregister all routes for an agent.

        Args:
            agent_name: Name of the agent to unregister

        Returns:
            Number of routes removed
        """
        to_remove = [
            key for key, route in self._routes.items() if route.agent_name == agent_name
        ]
        for key in to_remove:
            del self._routes[key]
            logger.debug(f"Unregistered route: {key}")
        return len(to_remove)

    def get_route(self, method: str, path: str) -> Optional[RegisteredRoute]:
        """
        Get a registered route by method and path.

        Args:
            method: HTTP method (e.g., "GET", "POST")
            path: URL path

        Returns:
            RegisteredRoute if found, None otherwise
        """
        route_key = f"{method.upper()}:{path}"
        return self._routes.get(route_key)

    @property
    def routes(self) -> Dict[str, RegisteredRoute]:
        """Get all registered routes."""
        return self._routes.copy()

    def set_auth_handler(self, handler: Callable) -> None:
        """
        Set the authentication handler.

        Args:
            handler: Async function for authentication.
                    Signature: async (request: Request, config: EndpointConfig) -> Optional[dict]
        """
        self._auth_handler = handler

    def get_fastapi_router(
        self,
        prefix: str = "",
        include_in_schema: bool = True,
    ) -> Optional["APIRouter"]:
        """
        Generate a FastAPI router from registered routes.

        Args:
            prefix: URL prefix to add to all routes
            include_in_schema: Whether to include routes in OpenAPI schema

        Returns:
            FastAPI APIRouter with all registered routes, or None if FastAPI not available.

        Example:
            >>> router = registry.get_fastapi_router(prefix="/api/v1")
            >>> app.include_router(router)
        """
        if not FASTAPI_AVAILABLE:
            logger.warning("FastAPI not available, cannot create router")
            return None

        router = APIRouter(prefix=prefix)
        auth_handler = self._auth_handler

        for route_key, route in self._routes.items():
            # Create handler closure with proper scoping
            self._add_route_to_router(router, route, auth_handler, include_in_schema)

        return router

    def _add_route_to_router(
        self,
        router: "APIRouter",
        route: RegisteredRoute,
        auth_handler: Optional[Callable],
        include_in_schema: bool,
    ) -> None:
        """
        Add a single route to a FastAPI router.

        Args:
            router: FastAPI router to add route to
            route: Route configuration
            auth_handler: Optional auth handler
            include_in_schema: Whether to include in OpenAPI schema
        """
        config = route.config
        handler = route.handler

        async def route_handler(
            request: Request,
            _route: RegisteredRoute = route,
            _config: EndpointConfig = config,
            _handler: Callable = handler,
            _auth_handler: Optional[Callable] = auth_handler,
        ):
            """Dynamic route handler for agent execution."""
            # Authentication check
            user_info = None
            if not _config.is_public():
                if _auth_handler is None:
                    raise HTTPException(
                        status_code=401,
                        detail="Authentication required but no auth handler configured",
                    )
                user_info = await _auth_handler(request, _config)

            # Extract path parameters
            path_params = {}
            if _config.path_params and request.path_params:
                try:
                    path_params = _config.extract_path_params(request.url.path)
                except ValueError as e:
                    raise HTTPException(status_code=400, detail=str(e))

            # Extract query parameters
            query_params = {}
            if _config.query_params:
                try:
                    query_dict = dict(request.query_params)
                    query_params = _config.extract_query_params(query_dict)
                except ValueError as e:
                    raise HTTPException(status_code=400, detail=str(e))

            # Build input state
            input_state: Dict[str, Any] = {}
            input_state.update(path_params)
            input_state.update(query_params)

            # Parse request body for non-GET requests
            method = (
                _config.method
                if isinstance(_config.method, str)
                else _config.method.value
            )
            if method != "GET":
                try:
                    body = await request.json()
                    if isinstance(body, dict):
                        input_state.update(body)
                except Exception:
                    pass  # No body or not JSON

            # Add auth info if available
            if user_info:
                input_state["_auth"] = user_info

            # Execute handler
            try:
                import asyncio

                if asyncio.iscoroutinefunction(_handler):
                    result = await _handler(input_state)
                else:
                    result = _handler(input_state)

                # Handle generator results (streaming)
                if hasattr(result, "__iter__") and not isinstance(
                    result, (dict, str, list)
                ):
                    # Collect final state from generator
                    final_state = None
                    for event in result:
                        if isinstance(event, dict) and event.get("type") == "final":
                            final_state = event.get("state", {})
                    result = final_state or {}

                return result

            except HTTPException:
                raise
            except Exception as e:
                logger.exception(f"Error executing agent {_route.agent_name}: {e}")
                raise HTTPException(status_code=500, detail=str(e))

        # Get method as string
        method = (
            config.method if isinstance(config.method, str) else config.method.value
        )

        # Add route to router
        router.add_api_route(
            config.path,
            route_handler,
            methods=[method],
            summary=config.summary,
            description=config.description,
            tags=config.tags or ["agents"],
            include_in_schema=include_in_schema,
            response_model=None,  # Allow dynamic responses
        )


# Global route registry instance
_global_registry: Optional[RouteRegistry] = None


def get_global_registry() -> RouteRegistry:
    """
    Get the global route registry instance.

    Returns:
        The global RouteRegistry instance.
    """
    global _global_registry
    if _global_registry is None:
        _global_registry = RouteRegistry()
    return _global_registry


def reset_global_registry() -> None:
    """Reset the global route registry (for testing)."""
    global _global_registry
    _global_registry = None


def get_fastapi_router(
    registry: Optional[RouteRegistry] = None,
    prefix: str = "",
) -> Optional["APIRouter"]:
    """
    Get a FastAPI router from the global or provided registry.

    Args:
        registry: Optional registry (uses global if not provided)
        prefix: URL prefix for all routes

    Returns:
        FastAPI APIRouter or None if FastAPI not available.
    """
    reg = registry or get_global_registry()
    return reg.get_fastapi_router(prefix=prefix)
