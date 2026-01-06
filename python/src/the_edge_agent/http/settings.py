"""
Server Settings Schema for YAML-based Server Configuration.

Story: TEA-BUILTIN-015.8 (Health & Metadata Endpoints)

Provides Pydantic models for server endpoint configuration via YAML settings.
Supports enabling/disabling endpoints, custom paths, and health check configuration.

Example YAML:
    settings:
      server:
        health_endpoint: true         # /health
        readiness_endpoint: true      # /ready
        list_agents: true             # /agents
        metrics: true                 # /metrics
        openapi: true                 # /openapi.json
        paths:
          health: "/healthz"          # Override default path
          ready: "/readyz"
        service_name: "tea-agents"
        version: "1.0.0"
        health_checks:
          - name: "database"
            type: firestore
          - name: "llm"
            type: http
            url: "${LLM_HEALTH_URL}"
            timeout: 5
"""

from enum import Enum
from typing import Dict, List, Optional

from pydantic import BaseModel, ConfigDict, Field, field_validator


class HealthCheckType(str, Enum):
    """Supported health check types."""

    FIRESTORE = "firestore"
    HTTP = "http"
    CUSTOM = "custom"


class HealthCheckConfig(BaseModel):
    """
    Configuration for a health check.

    Attributes:
        name: Unique name for this health check
        type: Type of check (firestore, http, custom)
        url: URL for HTTP health checks
        timeout: Timeout in seconds for the check
    """

    name: str = Field(..., description="Unique name for this health check")
    type: HealthCheckType = Field(
        default=HealthCheckType.CUSTOM,
        description="Type of health check",
    )
    url: Optional[str] = Field(
        default=None,
        description="URL for HTTP health checks",
    )
    timeout: int = Field(
        default=5,
        ge=1,
        le=60,
        description="Timeout in seconds for the check",
    )

    @field_validator("type", mode="before")
    @classmethod
    def validate_type(cls, v):
        """Accept string values and convert to enum."""
        if isinstance(v, str):
            try:
                return HealthCheckType(v.lower())
            except ValueError:
                valid = [e.value for e in HealthCheckType]
                raise ValueError(
                    f"Invalid health check type '{v}'. Valid options: {valid}"
                )
        return v


class PathOverrides(BaseModel):
    """
    Custom path overrides for endpoints.

    Attributes:
        health: Custom path for health endpoint (default: /health)
        ready: Custom path for readiness endpoint (default: /ready)
        agents: Custom path for agents list endpoint (default: /agents)
        metrics: Custom path for metrics endpoint (default: /metrics)
        openapi: Custom path for OpenAPI endpoint (default: /openapi.json)
    """

    health: str = Field(default="/health", description="Path for health endpoint")
    ready: str = Field(default="/ready", description="Path for readiness endpoint")
    agents: str = Field(default="/agents", description="Path for agents list endpoint")
    metrics: str = Field(default="/metrics", description="Path for metrics endpoint")
    openapi: str = Field(
        default="/openapi.json", description="Path for OpenAPI endpoint"
    )

    @field_validator("health", "ready", "agents", "metrics", "openapi", mode="before")
    @classmethod
    def validate_path(cls, v):
        """Ensure paths start with /."""
        if isinstance(v, str) and not v.startswith("/"):
            return "/" + v
        return v


class ServerSettings(BaseModel):
    """
    Pydantic model for server endpoint configuration.

    This model validates the server configuration from YAML files and
    provides typed access to endpoint settings.

    Attributes:
        health_endpoint: Enable /health endpoint (default: True)
        readiness_endpoint: Enable /ready endpoint (default: True)
        list_agents: Enable /agents endpoint (default: True)
        metrics: Enable /metrics Prometheus endpoint (default: False)
        openapi: Enable /openapi.json endpoint (default: True)
        paths: Custom path overrides for endpoints
        service_name: Service name reported in health responses
        version: Service version reported in health responses
        health_checks: List of health check configurations

    Example:
        >>> config = {
        ...     "health_endpoint": True,
        ...     "service_name": "my-agents",
        ...     "version": "2.0.0"
        ... }
        >>> settings = ServerSettings(**config)
        >>> settings.service_name
        'my-agents'
    """

    # Enable/disable endpoints
    health_endpoint: bool = Field(
        default=True,
        description="Enable /health endpoint",
    )
    readiness_endpoint: bool = Field(
        default=True,
        description="Enable /ready endpoint",
    )
    list_agents: bool = Field(
        default=True,
        description="Enable /agents list endpoint",
    )
    metrics: bool = Field(
        default=False,
        description="Enable /metrics Prometheus endpoint",
    )
    openapi: bool = Field(
        default=True,
        description="Enable /openapi.json endpoint",
    )

    # Custom paths
    paths: PathOverrides = Field(
        default_factory=PathOverrides,
        description="Custom path overrides for endpoints",
    )

    # Service info
    service_name: str = Field(
        default="tea-agents",
        description="Service name reported in health responses",
    )
    version: str = Field(
        default="1.0.0",
        description="Service version reported in health responses",
    )

    # Health checks
    health_checks: List[HealthCheckConfig] = Field(
        default_factory=list,
        description="List of health check configurations",
    )

    @field_validator("paths", mode="before")
    @classmethod
    def validate_paths(cls, v):
        """Accept dict and convert to PathOverrides."""
        if isinstance(v, dict):
            return PathOverrides(**v)
        return v

    @field_validator("health_checks", mode="before")
    @classmethod
    def validate_health_checks(cls, v):
        """Accept list of dicts and convert to HealthCheckConfig."""
        if v is None:
            return []
        if isinstance(v, list):
            result = []
            for item in v:
                if isinstance(item, dict):
                    result.append(HealthCheckConfig(**item))
                elif isinstance(item, HealthCheckConfig):
                    result.append(item)
                else:
                    raise ValueError(f"Invalid health check config: {item}")
            return result
        return v

    model_config = ConfigDict(use_enum_values=True)


def parse_server_settings(config: dict) -> Optional[ServerSettings]:
    """
    Parse server settings from a configuration dictionary.

    Args:
        config: Configuration dictionary, typically from YAML settings.
                Expected to have a 'server' key with server configuration.

    Returns:
        ServerSettings instance if server configuration is present,
        None otherwise.

    Example:
        >>> config = {"settings": {"server": {"health_endpoint": True}}}
        >>> settings = parse_server_settings(config.get("settings", {}))
        >>> settings.health_endpoint
        True
    """
    server_config = config.get("server")
    if server_config is None:
        return None

    if not isinstance(server_config, dict):
        return None

    try:
        return ServerSettings(**server_config)
    except Exception:
        # Return None for invalid configurations to maintain backward compatibility
        return None
