"""
Health, Readiness, and Metadata Endpoints for TEA Server.

Story: TEA-BUILTIN-015.8 (Health & Metadata Endpoints)

Provides FastAPI router with health, readiness, agents list, and agent info endpoints.
These endpoints enable Kubernetes probes and operational monitoring.

Example:
    >>> from fastapi import FastAPI
    >>> from the_edge_agent.http import configure_endpoints, endpoints_router
    >>>
    >>> app = FastAPI()
    >>> configure_endpoints(
    ...     agent_registry={"research": {...}},
    ...     settings={"service_name": "my-agents", "version": "1.0.0"}
    ... )
    >>> app.include_router(endpoints_router)
"""

import asyncio
import json
import logging
from datetime import datetime, timezone
from typing import Any, Callable, Dict, List, Optional, Union

logger = logging.getLogger(__name__)

# Try to import FastAPI, but provide fallback if not available
try:
    from fastapi import APIRouter, Response
    from fastapi.responses import JSONResponse

    FASTAPI_AVAILABLE = True
except ImportError:
    FASTAPI_AVAILABLE = False
    APIRouter = None  # type: ignore
    Response = None  # type: ignore
    JSONResponse = None  # type: ignore

# Global references (set by configure_endpoints or YAMLEngine)
_health_checks: Dict[str, Callable] = {}
_agent_registry: Optional[Dict[str, Any]] = None
_settings: Dict[str, Any] = {}

# Create router if FastAPI is available
if FASTAPI_AVAILABLE:
    router = APIRouter(tags=["health"])
else:
    router = None  # type: ignore


def configure_endpoints(
    agent_registry: Optional[Dict[str, Any]] = None,
    settings: Optional[Dict[str, Any]] = None,
    health_checks: Optional[Dict[str, Callable]] = None,
) -> None:
    """
    Configure endpoints with references to agent registry and settings.

    This function must be called before the endpoints are used to provide
    the necessary context for health checks and agent info.

    Args:
        agent_registry: Dictionary mapping agent names to their info dicts.
                       Each agent info should have: description, endpoint,
                       input_schema, output_schema, settings.
        settings: Server settings dict with keys:
                 - service_name: Name of the service
                 - version: Version string
        health_checks: Dictionary mapping check names to async/sync callables
                      that return True if healthy, False otherwise.

    Example:
        >>> configure_endpoints(
        ...     agent_registry={
        ...         "research_agent": {
        ...             "description": "Research agent",
        ...             "endpoint": {"path": "/api/v1/research", "method": "POST"},
        ...         }
        ...     },
        ...     settings={"service_name": "tea-agents", "version": "1.0.0"},
        ...     health_checks={"database": lambda: True}
        ... )
    """
    global _agent_registry, _settings, _health_checks
    _agent_registry = agent_registry
    _settings = settings or {}
    _health_checks = health_checks or {}


def register_health_check(name: str, check_func: Callable) -> None:
    """
    Register a custom health check function.

    The check function should be a sync or async callable that returns
    True if the dependency is healthy, False otherwise. It may also
    raise an exception which will be caught and reported.

    Args:
        name: Unique name for this health check
        check_func: Callable that returns True if healthy

    Example:
        >>> async def check_database():
        ...     # Check database connection
        ...     return await db.ping()
        >>> register_health_check("database", check_database)
    """
    global _health_checks
    _health_checks[name] = check_func
    logger.debug(f"Registered health check: {name}")


def get_health_checks() -> Dict[str, Callable]:
    """
    Get all registered health checks.

    Returns:
        Dictionary mapping check names to their callables.
    """
    return _health_checks.copy()


def _utcnow_iso() -> str:
    """Get current UTC time in ISO format."""
    return datetime.now(timezone.utc).isoformat().replace("+00:00", "Z")


async def _health_handler() -> Dict[str, Any]:
    """
    Health check endpoint handler.

    Returns 200 if service is running with status info.
    This is a simple liveness check - if this responds, the service is alive.

    Returns:
        Dictionary with status, service, version, and timestamp.
    """
    return {
        "status": "healthy",
        "service": _settings.get("service_name", "tea-agents"),
        "version": _settings.get("version", "1.0.0"),
        "timestamp": _utcnow_iso(),
    }


async def _ready_handler() -> tuple[Dict[str, Any], int]:
    """
    Readiness check endpoint handler.

    Checks all configured dependencies and returns their status.
    Returns 200 if all ready, 503 if any check fails.

    Returns:
        Tuple of (response_dict, status_code).
    """
    checks: Dict[str, Dict[str, Any]] = {}
    all_ready = True

    for name, check_func in _health_checks.items():
        try:
            start = datetime.now(timezone.utc)

            # Handle both sync and async check functions
            if asyncio.iscoroutinefunction(check_func):
                result = await check_func()
            else:
                result = check_func()

            latency = (datetime.now(timezone.utc) - start).total_seconds() * 1000

            checks[name] = {
                "status": "ok" if result else "error",
                "latency_ms": round(latency, 2),
            }
            if not result:
                all_ready = False
        except Exception as e:
            checks[name] = {
                "status": "error",
                "error": str(e),
            }
            all_ready = False

    status_code = 200 if all_ready else 503
    response = {
        "status": "ready" if all_ready else "not_ready",
        "checks": checks,
        "timestamp": _utcnow_iso(),
    }

    return response, status_code


async def _agents_list_handler() -> Dict[str, Any]:
    """
    List all available agents handler.

    Returns:
        Dictionary with agents list and count.
    """
    if _agent_registry is None:
        return {"agents": [], "count": 0}

    agents = []
    for name, info in _agent_registry.items():
        endpoint_info = info.get("endpoint", {})
        if isinstance(endpoint_info, dict):
            path = endpoint_info.get("path", "/run-agent")
            method = endpoint_info.get("method", "POST")
        else:
            path = "/run-agent"
            method = "POST"

        agents.append(
            {
                "name": name,
                "description": info.get("description"),
                "endpoint": path,
                "method": method,
            }
        )

    return {"agents": agents, "count": len(agents)}


async def _agent_info_handler(agent_name: str) -> tuple[Dict[str, Any], int]:
    """
    Get detailed info about a specific agent handler.

    Args:
        agent_name: Name of the agent to get info for.

    Returns:
        Tuple of (response_dict, status_code).
    """
    if _agent_registry is None or agent_name not in _agent_registry:
        return {
            "error": "not_found",
            "message": f"Agent '{agent_name}' not found",
        }, 404

    info = _agent_registry[agent_name]

    # Filter out sensitive settings (llm config, auth tokens, etc.)
    filtered_settings = {}
    raw_settings = info.get("settings", {})
    if isinstance(raw_settings, dict):
        for key, value in raw_settings.items():
            # Exclude potentially sensitive keys
            if key.lower() not in (
                "llm",
                "auth",
                "api_key",
                "secret",
                "token",
                "password",
            ):
                filtered_settings[key] = value

    return {
        "name": agent_name,
        "description": info.get("description"),
        "endpoint": info.get("endpoint"),
        "input_schema": info.get("input_schema"),
        "output_schema": info.get("output_schema"),
        "settings": filtered_settings,
    }, 200


# FastAPI route decorators (only if FastAPI is available)
if FASTAPI_AVAILABLE and router is not None:

    @router.get("/health")
    async def health():
        """
        Health check endpoint.

        Returns 200 if service is running.
        Used for Kubernetes liveness probes.
        """
        return await _health_handler()

    @router.get("/ready")
    async def ready():
        """
        Readiness check endpoint.

        Checks all dependencies and returns their status.
        Returns 200 if all ready, 503 if not.
        Used for Kubernetes readiness probes.
        """
        response, status_code = await _ready_handler()
        return JSONResponse(content=response, status_code=status_code)

    @router.get("/agents")
    async def list_agents():
        """
        List all available agents.

        Returns a list of agents with their names, descriptions, and endpoints.
        """
        return await _agents_list_handler()

    @router.get("/agents/{agent_name}")
    async def get_agent_info(agent_name: str):
        """
        Get detailed info about a specific agent.

        Returns agent details including input/output schemas and settings.
        Sensitive settings (llm, auth) are filtered out.
        """
        response, status_code = await _agent_info_handler(agent_name)
        return JSONResponse(content=response, status_code=status_code)


def create_health_router(path: str = "/health") -> Optional["APIRouter"]:
    """
    Create a router with just the health endpoint at a custom path.

    Args:
        path: Custom path for the health endpoint.

    Returns:
        FastAPI APIRouter with health endpoint, or None if FastAPI not available.
    """
    if not FASTAPI_AVAILABLE:
        return None

    health_router = APIRouter(tags=["health"])

    @health_router.get(path)
    async def health():
        return await _health_handler()

    return health_router


def create_ready_router(path: str = "/ready") -> Optional["APIRouter"]:
    """
    Create a router with just the readiness endpoint at a custom path.

    Args:
        path: Custom path for the readiness endpoint.

    Returns:
        FastAPI APIRouter with ready endpoint, or None if FastAPI not available.
    """
    if not FASTAPI_AVAILABLE:
        return None

    ready_router = APIRouter(tags=["health"])

    @ready_router.get(path)
    async def ready():
        response, status_code = await _ready_handler()
        return JSONResponse(content=response, status_code=status_code)

    return ready_router


def create_agents_router(
    list_path: str = "/agents",
    info_path: str = "/agents/{agent_name}",
) -> Optional["APIRouter"]:
    """
    Create a router with agents list and info endpoints at custom paths.

    Args:
        list_path: Custom path for the agents list endpoint.
        info_path: Custom path for the agent info endpoint (must include {agent_name}).

    Returns:
        FastAPI APIRouter with agents endpoints, or None if FastAPI not available.
    """
    if not FASTAPI_AVAILABLE:
        return None

    agents_router = APIRouter(tags=["agents"])

    @agents_router.get(list_path)
    async def list_agents():
        return await _agents_list_handler()

    @agents_router.get(info_path)
    async def get_agent_info(agent_name: str):
        response, status_code = await _agent_info_handler(agent_name)
        return JSONResponse(content=response, status_code=status_code)

    return agents_router
