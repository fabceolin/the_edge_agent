"""
HTTP module for TEA server endpoints.

Story: TEA-BUILTIN-015.7 (HTTP Endpoint Configuration)
Story: TEA-BUILTIN-015.8 (Health & Metadata Endpoints)

Provides:
- Per-agent HTTP endpoint configuration via YAML (TEA-BUILTIN-015.7)
- Auto-generated health, metrics, and metadata endpoints (TEA-BUILTIN-015.8)

Example YAML (015.7 - Agent Endpoint):
    name: research_agent

    endpoint:
      path: "/api/v1/research"
      method: POST
      summary: "Execute research query"
      tags: [Research, AI]
      auth:
        required: true
      query_params:
        limit:
          type: int
          default: 10

Example YAML (015.8 - Server Settings):
    settings:
      server:
        health_endpoint: true
        readiness_endpoint: true
        list_agents: true
        metrics: true
        openapi: true
        paths:
          health: "/healthz"
          ready: "/readyz"
        service_name: "my-tea-agents"
        version: "1.0.0"
"""

from .settings import (
    ServerSettings,
    HealthCheckConfig,
    HealthCheckType,
    PathOverrides,
    parse_server_settings,
)
from .endpoints import (
    router as endpoints_router,
    configure_endpoints,
    register_health_check,
    get_health_checks,
    create_health_router,
    create_ready_router,
    create_agents_router,
)
from .metrics import (
    MetricsCollector,
    metrics,
    get_metrics_endpoint,
)
from .openapi import (
    generate_openapi_spec,
    generate_openapi_from_routes,
    create_openapi_router,
    configure_openapi,
)

# TEA-BUILTIN-015.7: Endpoint Configuration
from .endpoint import (
    EndpointConfig,
    HTTPMethod,
    PathParam,
    QueryParam,
    AuthConfig,
    RequestConfig,
    ResponseConfig,
    ResponseExample,
    parse_endpoint_config,
)
from .router import (
    RouteRegistry,
    RegisteredRoute,
    get_global_registry,
    reset_global_registry,
    get_fastapi_router,
)

__all__ = [
    # Settings (015.8)
    "ServerSettings",
    "HealthCheckConfig",
    "HealthCheckType",
    "PathOverrides",
    "parse_server_settings",
    # Endpoints (015.8)
    "endpoints_router",
    "configure_endpoints",
    "register_health_check",
    "get_health_checks",
    "create_health_router",
    "create_ready_router",
    "create_agents_router",
    # Metrics (015.8)
    "MetricsCollector",
    "metrics",
    "get_metrics_endpoint",
    # OpenAPI (015.7, 015.8)
    "generate_openapi_spec",
    "generate_openapi_from_routes",
    "create_openapi_router",
    "configure_openapi",
    # Endpoint Configuration (015.7)
    "EndpointConfig",
    "HTTPMethod",
    "PathParam",
    "QueryParam",
    "AuthConfig",
    "RequestConfig",
    "ResponseConfig",
    "ResponseExample",
    "parse_endpoint_config",
    # Route Registry (015.7)
    "RouteRegistry",
    "RegisteredRoute",
    "get_global_registry",
    "reset_global_registry",
    "get_fastapi_router",
]
