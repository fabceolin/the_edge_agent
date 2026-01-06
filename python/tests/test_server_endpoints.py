"""
Tests for TEA-BUILTIN-015.8: Health & Metadata Endpoints.

Tests cover:
- AC1: ServerSettings schema validation
- AC2: Health endpoint (/health)
- AC3: Readiness endpoint (/ready)
- AC4: Agents list endpoint (/agents)
- AC5: Agent info endpoint (/agents/{name})
- AC6: Metrics endpoint (/metrics) with Prometheus format
- AC7: OpenAPI endpoint (/openapi.json)
- AC8: Custom health checks
- AC9: Configurable paths
"""

import asyncio
import threading
import time
import unittest
from datetime import datetime, timezone
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from the_edge_agent.http import (
    ServerSettings,
    HealthCheckConfig,
    HealthCheckType,
    PathOverrides,
    parse_server_settings,
    configure_endpoints,
    register_health_check,
    get_health_checks,
    MetricsCollector,
    metrics,
    generate_openapi_spec,
    configure_openapi,
)
from the_edge_agent.http.endpoints import (
    _health_handler,
    _ready_handler,
    _agents_list_handler,
    _agent_info_handler,
    _utcnow_iso,
)
from the_edge_agent.http.settings import PathOverrides


class TestServerSettings(unittest.TestCase):
    """Tests for ServerSettings Pydantic model (AC1, AC9)."""

    def test_default_settings(self):
        """Test default ServerSettings values."""
        settings = ServerSettings()

        self.assertTrue(settings.health_endpoint)
        self.assertTrue(settings.readiness_endpoint)
        self.assertTrue(settings.list_agents)
        self.assertFalse(settings.metrics)  # Disabled by default
        self.assertTrue(settings.openapi)
        self.assertEqual(settings.service_name, "tea-agents")
        self.assertEqual(settings.version, "1.0.0")
        self.assertEqual(settings.health_checks, [])

    def test_custom_settings(self):
        """Test ServerSettings with custom values."""
        settings = ServerSettings(
            health_endpoint=False,
            metrics=True,
            service_name="my-agents",
            version="2.0.0",
        )

        self.assertFalse(settings.health_endpoint)
        self.assertTrue(settings.metrics)
        self.assertEqual(settings.service_name, "my-agents")
        self.assertEqual(settings.version, "2.0.0")

    def test_path_overrides(self):
        """Test custom path configuration (AC9)."""
        settings = ServerSettings(
            paths={
                "health": "/healthz",
                "ready": "/readyz",
                "agents": "/api/agents",
            }
        )

        self.assertEqual(settings.paths.health, "/healthz")
        self.assertEqual(settings.paths.ready, "/readyz")
        self.assertEqual(settings.paths.agents, "/api/agents")
        # Defaults for unspecified paths
        self.assertEqual(settings.paths.metrics, "/metrics")
        self.assertEqual(settings.paths.openapi, "/openapi.json")

    def test_path_auto_prefix_slash(self):
        """Test paths without leading slash get prefixed."""
        paths = PathOverrides(health="healthz", ready="readyz")

        self.assertEqual(paths.health, "/healthz")
        self.assertEqual(paths.ready, "/readyz")

    def test_health_check_config(self):
        """Test health check configuration."""
        settings = ServerSettings(
            health_checks=[
                {"name": "database", "type": "firestore"},
                {
                    "name": "llm",
                    "type": "http",
                    "url": "http://llm/health",
                    "timeout": 10,
                },
            ]
        )

        self.assertEqual(len(settings.health_checks), 2)
        self.assertEqual(settings.health_checks[0].name, "database")
        self.assertEqual(settings.health_checks[0].type, HealthCheckType.FIRESTORE)
        self.assertEqual(settings.health_checks[1].name, "llm")
        self.assertEqual(settings.health_checks[1].url, "http://llm/health")
        self.assertEqual(settings.health_checks[1].timeout, 10)

    def test_invalid_health_check_type(self):
        """Test invalid health check type raises error."""
        with self.assertRaises(ValueError):
            HealthCheckConfig(name="test", type="invalid_type")

    def test_parse_server_settings_from_dict(self):
        """Test parsing server settings from config dict."""
        config = {
            "server": {
                "health_endpoint": True,
                "metrics": True,
                "service_name": "test-service",
            }
        }

        settings = parse_server_settings(config)

        self.assertIsNotNone(settings)
        self.assertTrue(settings.health_endpoint)
        self.assertTrue(settings.metrics)
        self.assertEqual(settings.service_name, "test-service")

    def test_parse_server_settings_missing(self):
        """Test parsing returns None when server config missing."""
        config = {"other": "value"}
        settings = parse_server_settings(config)
        self.assertIsNone(settings)

    def test_parse_server_settings_invalid(self):
        """Test parsing returns None for invalid config."""
        config = {"server": "not_a_dict"}
        settings = parse_server_settings(config)
        self.assertIsNone(settings)


class TestHealthEndpoint(unittest.TestCase):
    """Tests for health endpoint (AC2)."""

    def setUp(self):
        """Configure endpoints with test settings."""
        configure_endpoints(
            agent_registry=None,
            settings={"service_name": "test-service", "version": "1.2.3"},
            health_checks={},
        )

    def test_health_returns_healthy(self):
        """Test health endpoint returns healthy status."""
        result = asyncio.run(_health_handler())

        self.assertEqual(result["status"], "healthy")
        self.assertEqual(result["service"], "test-service")
        self.assertEqual(result["version"], "1.2.3")
        self.assertIn("timestamp", result)

    def test_health_timestamp_format(self):
        """Test health timestamp is ISO format with Z suffix."""
        result = asyncio.run(_health_handler())

        timestamp = result["timestamp"]
        self.assertTrue(timestamp.endswith("Z"))
        # Should be parseable as ISO datetime
        datetime.fromisoformat(timestamp.replace("Z", "+00:00"))


class TestReadinessEndpoint(unittest.TestCase):
    """Tests for readiness endpoint (AC3)."""

    def setUp(self):
        """Reset health checks before each test."""
        configure_endpoints(
            agent_registry=None,
            settings={"service_name": "test-service"},
            health_checks={},
        )

    def test_ready_no_checks(self):
        """Test readiness with no health checks returns ready."""
        result, status = asyncio.run(_ready_handler())

        self.assertEqual(status, 200)
        self.assertEqual(result["status"], "ready")
        self.assertEqual(result["checks"], {})

    def test_ready_all_checks_pass(self):
        """Test readiness when all checks pass."""
        register_health_check("check1", lambda: True)
        register_health_check("check2", lambda: True)

        result, status = asyncio.run(_ready_handler())

        self.assertEqual(status, 200)
        self.assertEqual(result["status"], "ready")
        self.assertEqual(result["checks"]["check1"]["status"], "ok")
        self.assertEqual(result["checks"]["check2"]["status"], "ok")
        self.assertIn("latency_ms", result["checks"]["check1"])

    def test_ready_check_fails(self):
        """Test readiness returns 503 when check fails."""
        register_health_check("passing", lambda: True)
        register_health_check("failing", lambda: False)

        result, status = asyncio.run(_ready_handler())

        self.assertEqual(status, 503)
        self.assertEqual(result["status"], "not_ready")
        self.assertEqual(result["checks"]["passing"]["status"], "ok")
        self.assertEqual(result["checks"]["failing"]["status"], "error")

    def test_ready_check_exception(self):
        """Test readiness handles check exception."""

        def failing_check():
            raise ConnectionError("Database unavailable")

        register_health_check("database", failing_check)

        result, status = asyncio.run(_ready_handler())

        self.assertEqual(status, 503)
        self.assertEqual(result["checks"]["database"]["status"], "error")
        self.assertIn("Database unavailable", result["checks"]["database"]["error"])

    def test_ready_async_check(self):
        """Test readiness with async health check (AC8)."""

        async def async_check():
            await asyncio.sleep(0.01)
            return True

        register_health_check("async_db", async_check)

        result, status = asyncio.run(_ready_handler())

        self.assertEqual(status, 200)
        self.assertEqual(result["checks"]["async_db"]["status"], "ok")


class TestAgentsListEndpoint(unittest.TestCase):
    """Tests for agents list endpoint (AC4)."""

    def test_agents_list_empty(self):
        """Test agents list when no agents registered."""
        configure_endpoints(agent_registry=None, settings={})

        result = asyncio.run(_agents_list_handler())

        self.assertEqual(result["agents"], [])
        self.assertEqual(result["count"], 0)

    def test_agents_list_with_agents(self):
        """Test agents list returns all agents."""
        registry = {
            "research_agent": {
                "description": "Research agent",
                "endpoint": {"path": "/api/v1/research", "method": "POST"},
            },
            "interview_agent": {
                "description": "Interview agent",
                "endpoint": {"path": "/api/v1/interview", "method": "POST"},
            },
        }
        configure_endpoints(agent_registry=registry, settings={})

        result = asyncio.run(_agents_list_handler())

        self.assertEqual(result["count"], 2)
        names = [a["name"] for a in result["agents"]]
        self.assertIn("research_agent", names)
        self.assertIn("interview_agent", names)

        research = next(a for a in result["agents"] if a["name"] == "research_agent")
        self.assertEqual(research["description"], "Research agent")
        self.assertEqual(research["endpoint"], "/api/v1/research")
        self.assertEqual(research["method"], "POST")


class TestAgentInfoEndpoint(unittest.TestCase):
    """Tests for agent info endpoint (AC5)."""

    def setUp(self):
        """Configure endpoints with test agents."""
        registry = {
            "research_agent": {
                "description": "Research agent with web search",
                "endpoint": {"path": "/api/v1/research", "method": "POST"},
                "input_schema": {
                    "query": {"type": "str", "required": True},
                    "max_results": {"type": "int", "default": 5},
                },
                "output_schema": {"answer": "str", "sources": "list"},
                "settings": {
                    "llm": {"provider": "azure_openai"},  # Should be filtered
                    "auth": {"required": True},  # Should be filtered
                    "timeout": 30,  # Should be included
                },
            },
        }
        configure_endpoints(agent_registry=registry, settings={})

    def test_agent_info_found(self):
        """Test agent info returns correct details."""
        result, status = asyncio.run(_agent_info_handler("research_agent"))

        self.assertEqual(status, 200)
        self.assertEqual(result["name"], "research_agent")
        self.assertEqual(result["description"], "Research agent with web search")
        self.assertEqual(result["endpoint"]["path"], "/api/v1/research")
        self.assertIn("query", result["input_schema"])
        self.assertIn("answer", result["output_schema"])

    def test_agent_info_filters_sensitive_settings(self):
        """Test agent info filters llm and auth settings."""
        result, status = asyncio.run(_agent_info_handler("research_agent"))

        self.assertEqual(status, 200)
        # Sensitive settings should be filtered
        self.assertNotIn("llm", result["settings"])
        self.assertNotIn("auth", result["settings"])
        # Non-sensitive settings should remain
        self.assertEqual(result["settings"]["timeout"], 30)

    def test_agent_info_not_found(self):
        """Test agent info returns 404 for unknown agent."""
        result, status = asyncio.run(_agent_info_handler("unknown_agent"))

        self.assertEqual(status, 404)
        self.assertEqual(result["error"], "not_found")
        self.assertIn("unknown_agent", result["message"])


class TestMetricsEndpoint(unittest.TestCase):
    """Tests for metrics endpoint (AC6)."""

    def setUp(self):
        """Reset metrics before each test."""
        metrics.reset()

    def test_metrics_collector_record_execution(self):
        """Test recording agent executions."""
        metrics.record_execution("agent1", "success", 0.5)
        metrics.record_execution("agent1", "success", 0.3)
        metrics.record_execution("agent1", "error", 1.0)

        self.assertEqual(metrics.get_execution_count("agent1", "success"), 2)
        self.assertEqual(metrics.get_execution_count("agent1", "error"), 1)
        self.assertEqual(metrics.get_execution_count("agent1"), 3)

    def test_metrics_collector_record_error(self):
        """Test recording errors by type."""
        metrics.record_error("agent1", "timeout")
        metrics.record_error("agent1", "timeout")
        metrics.record_error("agent1", "llm_error")

        self.assertEqual(metrics.get_error_count("agent1", "timeout"), 2)
        self.assertEqual(metrics.get_error_count("agent1", "llm_error"), 1)
        self.assertEqual(metrics.get_error_count("agent1"), 3)

    def test_metrics_collector_duration_stats(self):
        """Test duration statistics calculation."""
        metrics.record_execution("agent1", "success", 1.0)
        metrics.record_execution("agent1", "success", 2.0)
        metrics.record_execution("agent1", "success", 3.0)

        stats = metrics.get_duration_stats("agent1")

        self.assertEqual(stats["count"], 3)
        self.assertEqual(stats["sum"], 6.0)
        self.assertEqual(stats["min"], 1.0)
        self.assertEqual(stats["max"], 3.0)
        self.assertEqual(stats["avg"], 2.0)

    def test_metrics_prometheus_format(self):
        """Test Prometheus text format output."""
        metrics.record_execution("research", "success", 1.5)
        metrics.record_execution("research", "error", 0.5)
        metrics.record_error("research", "timeout")

        output = metrics.to_prometheus()

        # Check execution counters
        self.assertIn("# HELP tea_agent_executions_total", output)
        self.assertIn("# TYPE tea_agent_executions_total counter", output)
        self.assertIn(
            'tea_agent_executions_total{agent="research",status="success"} 1', output
        )
        self.assertIn(
            'tea_agent_executions_total{agent="research",status="error"} 1', output
        )

        # Check duration summary
        self.assertIn("# HELP tea_agent_duration_seconds", output)
        self.assertIn("# TYPE tea_agent_duration_seconds summary", output)
        self.assertIn('tea_agent_duration_seconds_sum{agent="research"}', output)
        self.assertIn('tea_agent_duration_seconds_count{agent="research"} 2', output)

        # Check error counters
        self.assertIn("# HELP tea_agent_errors_total", output)
        self.assertIn(
            'tea_agent_errors_total{agent="research",error_type="timeout"} 1', output
        )

    def test_metrics_thread_safety(self):
        """Test metrics collector is thread-safe (015.8-UNIT-010)."""
        metrics.reset()

        def record_metrics():
            for _ in range(100):
                metrics.record_execution("concurrent", "success", 0.1)
                metrics.record_error("concurrent", "test")

        threads = [threading.Thread(target=record_metrics) for _ in range(10)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()

        # Each thread records 100 executions and 100 errors
        self.assertEqual(metrics.get_execution_count("concurrent"), 1000)
        self.assertEqual(metrics.get_error_count("concurrent"), 1000)

    def test_metrics_reset(self):
        """Test metrics reset clears all data."""
        metrics.record_execution("agent1", "success", 1.0)
        metrics.record_error("agent1", "timeout")

        metrics.reset()

        self.assertEqual(metrics.get_execution_count(), 0)
        self.assertEqual(metrics.get_error_count(), 0)
        self.assertEqual(
            metrics.to_prometheus().strip(),
            "# HELP tea_agent_executions_total Total agent executions\n# TYPE tea_agent_executions_total counter",
        )


class TestOpenAPIEndpoint(unittest.TestCase):
    """Tests for OpenAPI endpoint (AC7)."""

    def test_generate_openapi_basic(self):
        """Test basic OpenAPI spec generation."""
        spec = generate_openapi_spec(
            agent_registry=None,
            service_name="test-api",
            version="1.0.0",
        )

        self.assertEqual(spec["openapi"], "3.0.3")
        self.assertEqual(spec["info"]["title"], "test-api")
        self.assertEqual(spec["info"]["version"], "1.0.0")

    def test_generate_openapi_with_agents(self):
        """Test OpenAPI spec includes agent endpoints."""
        registry = {
            "research": {
                "description": "Research agent",
                "endpoint": {"path": "/api/v1/research", "method": "POST"},
                "input_schema": {
                    "query": {"type": "str", "required": True},
                },
                "output_schema": {"answer": "str"},
            },
        }

        spec = generate_openapi_spec(registry, "test-api", "1.0.0")

        self.assertIn("/api/v1/research", spec["paths"])
        post_op = spec["paths"]["/api/v1/research"]["post"]
        self.assertEqual(post_op["summary"], "Research agent")
        self.assertIn("requestBody", post_op)
        self.assertIn("200", post_op["responses"])

    def test_generate_openapi_auth_schemes(self):
        """Test OpenAPI spec includes auth schemes."""
        registry = {
            "secure_agent": {
                "description": "Secure agent",
                "endpoint": {"path": "/api/secure", "method": "POST"},
                "settings": {"auth": {"required": True}},
            },
        }

        spec = generate_openapi_spec(registry, "test-api", "1.0.0")

        # Check security scheme is defined
        self.assertIn("components", spec)
        self.assertIn("securitySchemes", spec["components"])
        self.assertIn("bearerAuth", spec["components"]["securitySchemes"])

        # Check endpoint has security requirement
        post_op = spec["paths"]["/api/secure"]["post"]
        self.assertIn("security", post_op)

    def test_generate_openapi_type_conversion(self):
        """Test Python types convert to OpenAPI types."""
        registry = {
            "typed_agent": {
                "description": "Typed agent",
                "endpoint": {"path": "/typed", "method": "POST"},
                "input_schema": {
                    "text": {"type": "str"},
                    "count": {"type": "int"},
                    "rate": {"type": "float"},
                    "enabled": {"type": "bool"},
                    "items": {"type": "list"},
                },
            },
        }

        spec = generate_openapi_spec(registry, "test", "1.0.0")
        schema = spec["paths"]["/typed"]["post"]["requestBody"]["content"][
            "application/json"
        ]["schema"]

        self.assertEqual(schema["properties"]["text"]["type"], "string")
        self.assertEqual(schema["properties"]["count"]["type"], "integer")
        self.assertEqual(schema["properties"]["rate"]["type"], "number")
        self.assertEqual(schema["properties"]["enabled"]["type"], "boolean")
        self.assertEqual(schema["properties"]["items"]["type"], "array")


class TestCustomHealthChecks(unittest.TestCase):
    """Tests for custom health check registration (AC8)."""

    def setUp(self):
        """Reset health checks before each test."""
        configure_endpoints(
            agent_registry=None,
            settings={},
            health_checks={},
        )

    def test_register_sync_check(self):
        """Test registering synchronous health check."""
        register_health_check("sync_db", lambda: True)

        checks = get_health_checks()
        self.assertIn("sync_db", checks)

    def test_register_async_check(self):
        """Test registering async health check."""

        async def async_check():
            return True

        register_health_check("async_service", async_check)

        checks = get_health_checks()
        self.assertIn("async_service", checks)

    def test_health_check_decorator_pattern(self):
        """Test health check can be used with decorator pattern."""
        # This tests the API usability for custom health checks

        def custom_check():
            """Check custom service health."""
            return True

        register_health_check("custom_service", custom_check)

        result, status = asyncio.run(_ready_handler())
        self.assertEqual(status, 200)
        self.assertEqual(result["checks"]["custom_service"]["status"], "ok")


class TestConfigurablePaths(unittest.TestCase):
    """Tests for configurable endpoint paths (AC9)."""

    def test_custom_paths_in_settings(self):
        """Test ServerSettings accepts custom paths."""
        settings = ServerSettings(
            paths={
                "health": "/healthz",
                "ready": "/readyz",
                "agents": "/api/agents",
                "metrics": "/api/metrics",
                "openapi": "/api/openapi.json",
            }
        )

        self.assertEqual(settings.paths.health, "/healthz")
        self.assertEqual(settings.paths.ready, "/readyz")
        self.assertEqual(settings.paths.agents, "/api/agents")
        self.assertEqual(settings.paths.metrics, "/api/metrics")
        self.assertEqual(settings.paths.openapi, "/api/openapi.json")

    def test_multiple_custom_paths(self):
        """Test multiple custom paths configured simultaneously."""
        paths = PathOverrides(
            health="/v2/health",
            ready="/v2/ready",
            agents="/v2/agents",
        )

        self.assertEqual(paths.health, "/v2/health")
        self.assertEqual(paths.ready, "/v2/ready")
        self.assertEqual(paths.agents, "/v2/agents")


class TestEndpointEnableDisable(unittest.TestCase):
    """Tests for enabling/disabling endpoints."""

    def test_endpoints_enabled_by_default(self):
        """Test health, ready, agents, openapi enabled by default."""
        settings = ServerSettings()

        self.assertTrue(settings.health_endpoint)
        self.assertTrue(settings.readiness_endpoint)
        self.assertTrue(settings.list_agents)
        self.assertTrue(settings.openapi)

    def test_metrics_disabled_by_default(self):
        """Test metrics disabled by default."""
        settings = ServerSettings()
        self.assertFalse(settings.metrics)

    def test_disable_all_endpoints(self):
        """Test all endpoints can be disabled."""
        settings = ServerSettings(
            health_endpoint=False,
            readiness_endpoint=False,
            list_agents=False,
            metrics=False,
            openapi=False,
        )

        self.assertFalse(settings.health_endpoint)
        self.assertFalse(settings.readiness_endpoint)
        self.assertFalse(settings.list_agents)
        self.assertFalse(settings.metrics)
        self.assertFalse(settings.openapi)


class TestIntegration(unittest.TestCase):
    """Integration tests for full endpoint flow."""

    def test_full_workflow(self):
        """Test complete workflow: configure, register checks, verify."""
        # Configure endpoints
        configure_endpoints(
            agent_registry={
                "agent1": {
                    "description": "Test agent",
                    "endpoint": {"path": "/api/agent1", "method": "POST"},
                }
            },
            settings={"service_name": "integration-test", "version": "1.0.0"},
        )

        # Register health check
        register_health_check("test_check", lambda: True)

        # Verify health
        health_result = asyncio.run(_health_handler())
        self.assertEqual(health_result["status"], "healthy")
        self.assertEqual(health_result["service"], "integration-test")

        # Verify ready
        ready_result, ready_status = asyncio.run(_ready_handler())
        self.assertEqual(ready_status, 200)
        self.assertEqual(ready_result["checks"]["test_check"]["status"], "ok")

        # Verify agents list
        agents_result = asyncio.run(_agents_list_handler())
        self.assertEqual(agents_result["count"], 1)
        self.assertEqual(agents_result["agents"][0]["name"], "agent1")

        # Verify agent info
        info_result, info_status = asyncio.run(_agent_info_handler("agent1"))
        self.assertEqual(info_status, 200)
        self.assertEqual(info_result["description"], "Test agent")


if __name__ == "__main__":
    unittest.main()
