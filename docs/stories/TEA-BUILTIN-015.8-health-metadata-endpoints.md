# Story TEA-BUILTIN-015.8: Health & Metadata Endpoints

## Status: Done

## Story

**As a** TEA agent developer,
**I want** auto-generated health, metrics, and metadata endpoints,
**so that** I get standard operational endpoints without writing any Python code.

## Acceptance Criteria

1. **AC1: Settings Schema** - `settings.server` configures which endpoints to enable
2. **AC2: Health Endpoint** - `/health` returns service health status
3. **AC3: Readiness Endpoint** - `/ready` checks dependencies are available
4. **AC4: Agents List Endpoint** - `/agents` lists available agents with metadata
5. **AC5: Agent Info Endpoint** - `/agents/{name}` returns specific agent details
6. **AC6: Metrics Endpoint** - `/metrics` returns Prometheus-format metrics
7. **AC7: OpenAPI Endpoint** - `/openapi.json` returns OpenAPI specification
8. **AC8: Custom Health Checks** - Support custom health check functions
9. **AC9: Configurable Paths** - Allow customization of endpoint paths

## Tasks / Subtasks

- [x] **Task 1: Define Server Settings Schema** (AC1, AC9)
  - [x] Create `ServerSettings` Pydantic model
  - [x] Add `server` field to Settings
  - [x] Define enable flags and path overrides

- [x] **Task 2: Implement Health Endpoint** (AC2)
  - [x] Return `{"status": "healthy", "timestamp": "..."}`
  - [x] Include version and service name
  - [x] Always returns 200 if service is running

- [x] **Task 3: Implement Readiness Endpoint** (AC3)
  - [x] Check configured backends (Firestore, LTM, etc.)
  - [x] Return 200 if all ready, 503 if not
  - [x] Include individual check results

- [x] **Task 4: Implement Agents List Endpoint** (AC4)
  - [x] Scan agents directory
  - [x] Return list with name, description, endpoint
  - [x] Cache results for performance

- [x] **Task 5: Implement Agent Info Endpoint** (AC5)
  - [x] Return detailed agent info
  - [x] Include input/output schemas
  - [x] Include endpoint configuration

- [x] **Task 6: Implement Metrics Endpoint** (AC6)
  - [x] Track agent execution count
  - [x] Track execution duration histogram
  - [x] Track error count by type
  - [x] Format as Prometheus text

- [x] **Task 7: Implement OpenAPI Endpoint** (AC7)
  - [x] Aggregate all agent endpoint specs
  - [x] Include auth schemes
  - [x] Include server info

- [x] **Task 8: Implement Custom Health Checks** (AC8)
  - [x] Allow registering custom check functions
  - [x] Support async checks
  - [x] Include in readiness response

- [x] **Task 9: Write Tests** (AC1-AC9)
  - [x] Test each endpoint
  - [x] Test enable/disable flags
  - [x] Test custom paths
  - [x] Test metrics collection
  - [x] Test custom health checks

- [x] **Task 10: Documentation**
  - [x] Update `docs/shared/YAML_REFERENCE.md` with server settings
  - [x] Add operational endpoints guide
  - [x] Include Kubernetes probe examples

## Dev Notes

### Settings Schema

```yaml
settings:
  server:
    # Enable/disable endpoints
    health_endpoint: true         # /health
    readiness_endpoint: true      # /ready
    list_agents: true             # /agents
    metrics: true                 # /metrics
    openapi: true                 # /openapi.json

    # Custom paths (optional)
    paths:
      health: "/healthz"          # Override default path
      ready: "/readyz"
      agents: "/api/agents"
      metrics: "/api/metrics"
      openapi: "/api/openapi.json"

    # Service info
    service_name: "tea-agents"
    version: "1.0.0"

    # Custom health checks
    health_checks:
      - name: "database"
        type: firestore           # Built-in check type
      - name: "llm"
        type: http
        url: "${LLM_HEALTH_URL}"
        timeout: 5
```

### Health Endpoint Response

```json
// GET /health
{
  "status": "healthy",
  "service": "tea-agents",
  "version": "1.0.0",
  "timestamp": "2025-01-05T12:00:00Z"
}
```

### Readiness Endpoint Response

```json
// GET /ready
{
  "status": "ready",
  "checks": {
    "firestore": {
      "status": "ok",
      "latency_ms": 12
    },
    "llm": {
      "status": "ok",
      "latency_ms": 45
    }
  },
  "timestamp": "2025-01-05T12:00:00Z"
}

// When not ready (returns 503)
{
  "status": "not_ready",
  "checks": {
    "firestore": {
      "status": "ok"
    },
    "llm": {
      "status": "error",
      "error": "Connection timeout"
    }
  }
}
```

### Agents List Endpoint Response

```json
// GET /agents
{
  "agents": [
    {
      "name": "research_agent",
      "description": "Research agent with web search",
      "endpoint": "/api/v1/research",
      "method": "POST"
    },
    {
      "name": "interview_agent",
      "description": "Interactive interview agent",
      "endpoint": "/api/v1/interview",
      "method": "POST"
    }
  ],
  "count": 2
}
```

### Agent Info Endpoint Response

```json
// GET /agents/research_agent
{
  "name": "research_agent",
  "description": "Research agent with web search and LLM synthesis",
  "endpoint": {
    "path": "/api/v1/research",
    "method": "POST"
  },
  "input_schema": {
    "query": {"type": "str", "required": true},
    "max_results": {"type": "int", "default": 5}
  },
  "output_schema": {
    "answer": "str",
    "sources": "list"
  },
  "settings": {
    "llm": {"provider": "azure_openai"},
    "auth": {"required": true}
  }
}
```

### Metrics Endpoint Response

```text
# GET /metrics (Prometheus format)

# HELP tea_agent_executions_total Total agent executions
# TYPE tea_agent_executions_total counter
tea_agent_executions_total{agent="research_agent",status="success"} 1542
tea_agent_executions_total{agent="research_agent",status="error"} 23
tea_agent_executions_total{agent="interview_agent",status="success"} 891

# HELP tea_agent_duration_seconds Agent execution duration
# TYPE tea_agent_duration_seconds histogram
tea_agent_duration_seconds_bucket{agent="research_agent",le="0.5"} 120
tea_agent_duration_seconds_bucket{agent="research_agent",le="1.0"} 890
tea_agent_duration_seconds_bucket{agent="research_agent",le="5.0"} 1520
tea_agent_duration_seconds_bucket{agent="research_agent",le="+Inf"} 1542
tea_agent_duration_seconds_sum{agent="research_agent"} 2341.5
tea_agent_duration_seconds_count{agent="research_agent"} 1542

# HELP tea_agent_errors_total Total agent errors by type
# TYPE tea_agent_errors_total counter
tea_agent_errors_total{agent="research_agent",error_type="timeout"} 15
tea_agent_errors_total{agent="research_agent",error_type="llm_error"} 8
```

### Kubernetes Probe Configuration

```yaml
# Example Kubernetes deployment
spec:
  containers:
    - name: tea-agents
      livenessProbe:
        httpGet:
          path: /health
          port: 8080
        initialDelaySeconds: 10
        periodSeconds: 15

      readinessProbe:
        httpGet:
          path: /ready
          port: 8080
        initialDelaySeconds: 5
        periodSeconds: 10
```

### Custom Health Check Registration

```python
# For advanced use cases
from the_edge_agent import YAMLEngine

engine = YAMLEngine()

# Register custom health check
@engine.health_check("custom_service")
async def check_custom_service():
    # Return True if healthy, False otherwise
    response = await http_client.get("http://service/health")
    return response.status_code == 200
```

### Module Structure (Files to Create)

```
python/src/the_edge_agent/
├── http/                           # EXTEND MODULE (from 015.7)
│   ├── __init__.py                 # Add: health_router, metrics exports
│   ├── endpoints.py                # NEW: Health, ready, agents list endpoints
│   └── metrics.py                  # NEW: Prometheus metrics collection
│
├── settings.py                     # MODIFY: Add ServerSettings model
└── yaml_engine.py                  # MODIFY: Add server endpoints registration
```

### File Contents Overview

**http/endpoints.py:**
```python
from typing import Dict, List, Any, Optional, Callable
from fastapi import APIRouter, Response
from datetime import datetime
import asyncio

router = APIRouter()

# Global references (set by YAMLEngine)
_health_checks: Dict[str, Callable] = {}
_agent_registry = None
_settings: Dict[str, Any] = {}

def configure_endpoints(
    agent_registry,
    settings: dict,
    health_checks: Dict[str, Callable] = None
):
    """Configure endpoints with references."""
    global _agent_registry, _settings, _health_checks
    _agent_registry = agent_registry
    _settings = settings
    _health_checks = health_checks or {}


@router.get("/health")
async def health():
    """
    Health check endpoint.
    Returns 200 if service is running.
    """
    return {
        "status": "healthy",
        "service": _settings.get("service_name", "tea-agents"),
        "version": _settings.get("version", "1.0.0"),
        "timestamp": datetime.utcnow().isoformat()
    }


@router.get("/ready")
async def ready():
    """
    Readiness check endpoint.
    Checks all dependencies and returns their status.
    """
    checks = {}
    all_ready = True

    for name, check_func in _health_checks.items():
        try:
            start = datetime.utcnow()
            result = await check_func() if asyncio.iscoroutinefunction(check_func) else check_func()
            latency = (datetime.utcnow() - start).total_seconds() * 1000

            checks[name] = {
                "status": "ok" if result else "error",
                "latency_ms": round(latency, 2)
            }
            if not result:
                all_ready = False
        except Exception as e:
            checks[name] = {
                "status": "error",
                "error": str(e)
            }
            all_ready = False

    status_code = 200 if all_ready else 503
    return Response(
        content={
            "status": "ready" if all_ready else "not_ready",
            "checks": checks,
            "timestamp": datetime.utcnow().isoformat()
        },
        status_code=status_code,
        media_type="application/json"
    )


@router.get("/agents")
async def list_agents():
    """
    List all available agents.
    """
    if _agent_registry is None:
        return {"agents": [], "count": 0}

    agents = []
    for name, info in _agent_registry.items():
        agents.append({
            "name": name,
            "description": info.get("description"),
            "endpoint": info.get("endpoint", {}).get("path", f"/run-agent"),
            "method": info.get("endpoint", {}).get("method", "POST")
        })

    return {"agents": agents, "count": len(agents)}


@router.get("/agents/{agent_name}")
async def get_agent_info(agent_name: str):
    """
    Get detailed info about a specific agent.
    """
    if _agent_registry is None or agent_name not in _agent_registry:
        return Response(
            content={"error": "not_found", "message": f"Agent '{agent_name}' not found"},
            status_code=404,
            media_type="application/json"
        )

    info = _agent_registry[agent_name]
    return {
        "name": agent_name,
        "description": info.get("description"),
        "endpoint": info.get("endpoint"),
        "input_schema": info.get("input_schema"),
        "output_schema": info.get("output_schema"),
        "settings": {
            k: v for k, v in info.get("settings", {}).items()
            if k not in ("llm", "auth")  # Hide sensitive settings
        }
    }
```

**http/metrics.py:**
```python
from typing import Dict
from dataclasses import dataclass, field
from datetime import datetime
import threading

@dataclass
class MetricsCollector:
    """Collects agent execution metrics."""

    executions: Dict[str, Dict[str, int]] = field(default_factory=dict)
    durations: Dict[str, list] = field(default_factory=dict)
    errors: Dict[str, Dict[str, int]] = field(default_factory=dict)
    _lock: threading.Lock = field(default_factory=threading.Lock)

    def record_execution(self, agent: str, status: str, duration_seconds: float):
        """Record an agent execution."""
        with self._lock:
            if agent not in self.executions:
                self.executions[agent] = {"success": 0, "error": 0}
                self.durations[agent] = []

            self.executions[agent][status] = self.executions[agent].get(status, 0) + 1
            self.durations[agent].append(duration_seconds)

    def record_error(self, agent: str, error_type: str):
        """Record an error by type."""
        with self._lock:
            if agent not in self.errors:
                self.errors[agent] = {}
            self.errors[agent][error_type] = self.errors[agent].get(error_type, 0) + 1

    def to_prometheus(self) -> str:
        """Export metrics in Prometheus text format."""
        lines = []

        # Executions counter
        lines.append("# HELP tea_agent_executions_total Total agent executions")
        lines.append("# TYPE tea_agent_executions_total counter")
        for agent, counts in self.executions.items():
            for status, count in counts.items():
                lines.append(f'tea_agent_executions_total{{agent="{agent}",status="{status}"}} {count}')

        # Duration histogram (simplified - just sum and count)
        lines.append("")
        lines.append("# HELP tea_agent_duration_seconds Agent execution duration")
        lines.append("# TYPE tea_agent_duration_seconds summary")
        for agent, durations in self.durations.items():
            if durations:
                lines.append(f'tea_agent_duration_seconds_sum{{agent="{agent}"}} {sum(durations):.3f}')
                lines.append(f'tea_agent_duration_seconds_count{{agent="{agent}"}} {len(durations)}')

        # Errors counter
        lines.append("")
        lines.append("# HELP tea_agent_errors_total Total agent errors by type")
        lines.append("# TYPE tea_agent_errors_total counter")
        for agent, error_counts in self.errors.items():
            for error_type, count in error_counts.items():
                lines.append(f'tea_agent_errors_total{{agent="{agent}",error_type="{error_type}"}} {count}')

        return "\n".join(lines)


# Global metrics instance
metrics = MetricsCollector()


def get_metrics_endpoint():
    """FastAPI endpoint for Prometheus metrics."""
    from fastapi import Response

    async def metrics_handler():
        return Response(
            content=metrics.to_prometheus(),
            media_type="text/plain"
        )

    return metrics_handler
```

**settings.py additions:**
```python
from typing import Optional, List, Dict
from pydantic import BaseModel

class HealthCheckConfig(BaseModel):
    name: str
    type: str  # firestore, http, custom
    url: Optional[str] = None  # For http type
    timeout: int = 5

class ServerSettings(BaseModel):
    # Enable/disable endpoints
    health_endpoint: bool = True
    readiness_endpoint: bool = True
    list_agents: bool = True
    metrics: bool = False
    openapi: bool = True

    # Custom paths
    paths: Dict[str, str] = {
        "health": "/health",
        "ready": "/ready",
        "agents": "/agents",
        "metrics": "/metrics",
        "openapi": "/openapi.json"
    }

    # Service info
    service_name: str = "tea-agents"
    version: str = "1.0.0"

    # Health checks
    health_checks: List[HealthCheckConfig] = []
```

### Relevant Existing Files (Minimal Changes)

- `python/src/the_edge_agent/settings.py` - Add `ServerSettings` model
- `python/src/the_edge_agent/yaml_engine.py` - Add server endpoints registration (~25 lines)
- `python/src/the_edge_agent/http/__init__.py` - Export new endpoints

### Integration with YAMLEngine

```python
# yaml_engine.py additions

class YAMLEngine:
    def __init__(self):
        # ... existing code ...
        self._metrics = None
        self._server_settings = None

    def configure_server(self, settings: dict):
        """Configure server endpoints from settings."""
        from .http.endpoints import configure_endpoints, router as endpoints_router
        from .http.metrics import metrics, get_metrics_endpoint
        from .settings import ServerSettings

        self._server_settings = ServerSettings(**settings.get("server", {}))
        self._metrics = metrics

        # Configure endpoints with agent registry
        configure_endpoints(
            agent_registry=self._loaded_agents,
            settings=self._server_settings.model_dump()
        )

    def get_server_router(self):
        """Get router with health, agents, metrics endpoints."""
        from fastapi import APIRouter
        from .http.endpoints import router as endpoints_router
        from .http.metrics import get_metrics_endpoint

        router = APIRouter()

        if self._server_settings.health_endpoint:
            router.include_router(endpoints_router)

        if self._server_settings.metrics:
            router.add_api_route(
                self._server_settings.paths.get("metrics", "/metrics"),
                get_metrics_endpoint(),
                methods=["GET"]
            )

        return router

    # Hook into execution for metrics
    def _record_execution(self, agent: str, status: str, duration: float):
        if self._metrics:
            self._metrics.record_execution(agent, status, duration)
```

### Dependencies

- No external dependencies for basic endpoints
- `prometheus_client` optional for advanced metrics
- Builds on TEA-BUILTIN-015.7 (Endpoint Configuration) for OpenAPI

### Testing

**Test file location:** `python/tests/test_server_endpoints.py`

**Testing standards:**
- Test each endpoint independently
- Test enable/disable flags
- Test custom paths
- Mock dependencies for health checks
- Minimum 90% coverage

**Test cases:**
1. Health endpoint returns 200
2. Readiness checks dependencies
3. Readiness returns 503 when check fails
4. Agents list returns all agents
5. Agent info returns correct schema
6. Metrics formatted correctly
7. OpenAPI includes all agents
8. Custom paths work
9. Disabled endpoints return 404

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-05 | 1.0 | Initial story creation | Sarah (PO) |

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
- No critical issues encountered during implementation
- Pydantic Config deprecation warning fixed (migrated to ConfigDict)

### Completion Notes List
1. Created new `http` module at `python/src/the_edge_agent/http/`
2. Implemented ServerSettings Pydantic model with all endpoint configuration options
3. Health endpoint returns JSON with service info and timestamp
4. Readiness endpoint checks all registered health checks, returns 503 on failure
5. Agents list and info endpoints filter sensitive settings (llm, auth)
6. Metrics endpoint uses thread-safe MetricsCollector, outputs Prometheus text format
7. OpenAPI endpoint generates OpenAPI 3.0 spec from agent registry with type conversion
8. Custom health checks support both sync and async callables
9. All 40 tests pass covering AC1-AC9
10. Documentation added to specialized.md with Kubernetes probe examples

### File List
| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/http/__init__.py` | Created | Module exports for http endpoints |
| `python/src/the_edge_agent/http/settings.py` | Created | ServerSettings, HealthCheckConfig, PathOverrides models |
| `python/src/the_edge_agent/http/endpoints.py` | Created | Health, ready, agents list/info endpoint handlers |
| `python/src/the_edge_agent/http/metrics.py` | Created | MetricsCollector class and Prometheus export |
| `python/src/the_edge_agent/http/openapi.py` | Created | OpenAPI 3.0 spec generation from agent registry |
| `python/tests/test_server_endpoints.py` | Created | 40 unit/integration tests covering all ACs |
| `docs/shared/yaml-reference/actions/specialized.md` | Modified | Added Server Endpoints section with full documentation |

## QA Results

### QA Notes

**Review Date:** 2026-01-05
**Reviewer:** Quinn (Test Architect)

---

#### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 42 |
| Unit Tests | 14 (33%) |
| Integration Tests | 19 (45%) |
| E2E Tests | 9 (22%) |
| **Priority Distribution** | P0: 12, P1: 18, P2: 10, P3: 2 |

**All 9 Acceptance Criteria have complete test coverage.** Integration-heavy strategy is appropriate given this story's focus on HTTP routing and component configuration.

---

#### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **Kubernetes deployment failures** | HIGH | P0 priority on health/readiness probe tests (015.8-INT-003, 015.8-INT-008, 015.8-E2E-003/004) |
| **Metrics thread race conditions** | MEDIUM | Thread lock validation test (015.8-UNIT-010) |
| **Dependency check timeouts** | MEDIUM | Exception isolation testing (015.8-INT-012) |
| **Sensitive settings exposure** | MEDIUM | Security filter validation for llm/auth (015.8-INT-021) |
| **Invalid OpenAPI spec generation** | LOW | Schema validation (015.8-INT-024) |

---

#### Recommended Test Scenarios

**P0 Critical (CI Gate - Must Pass):**
1. Health endpoint returns 200 with correct service info (AC2)
2. Readiness returns 200 when all checks pass, 503 when any fails (AC3)
3. MetricsCollector core logic: record_execution, record_error, to_prometheus (AC6)
4. Server starts with all endpoints enabled by default (Full Integration)
5. Kubernetes liveness/readiness probes succeed (E2E Deployment Validation)

**P1 Core Functionality:**
- ServerSettings Pydantic model validation (AC1)
- Agents list and individual agent info endpoints (AC4, AC5)
- Async/sync health check handling (AC8)
- Custom path configuration works correctly (AC9)
- Agent execution updates observable metrics (Full Observability)

**P2 Edge Cases:**
- Empty registries/health_checks return graceful responses
- Toggle behaviors (enable/disable flags for all endpoints)
- Multiple custom paths configured simultaneously

---

#### Concerns & Recommendations

1. **Thread Safety**: The `MetricsCollector` uses a `threading.Lock` - ensure test 015.8-UNIT-010 verifies concurrent access patterns
2. **Prometheus Format**: Test 015.8-UNIT-008 should validate against actual Prometheus text format spec, not just string matching
3. **HTTP Check Timeout**: HealthCheckConfig has `timeout: 5` default - verify this is honored in 015.8-INT-028
4. **Response Content-Type**: Verify 015.8-INT-022 confirms `text/plain` media type for `/metrics` (Prometheus requirement)

---

#### Test Execution Order

**Phase 1 (Fail Fast):** P0 unit + integration + E2E tests (12 tests)
**Phase 2 (Core):** P1 tests (18 tests)
**Phase 3 (Coverage):** P2 tests (10 tests)
**Phase 4 (Extended):** P3 tests if time permits (2 tests)

---

#### Blockers

None identified. Story is ready for development with clear test criteria.

---

**Test Design Reference:** `docs/qa/assessments/TEA-BUILTIN-015.8-test-design-20260105.md`

---

### Review Date: 2026-01-05

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall Grade: EXCELLENT**

The implementation demonstrates high-quality engineering with well-structured, modular code following Python best practices. Key strengths:

1. **Clean Architecture**: The `http` module is well-organized with separate files for settings, endpoints, metrics, and OpenAPI generation
2. **Pydantic Models**: Excellent use of Pydantic v2 with `ConfigDict`, field validators, and proper type annotations
3. **Thread Safety**: `MetricsCollector` correctly uses `threading.Lock` for concurrent access protection
4. **Graceful Fallbacks**: FastAPI import is optional with proper `FASTAPI_AVAILABLE` guards
5. **Async/Sync Support**: Health checks correctly handle both sync and async callables via `asyncio.iscoroutinefunction()`
6. **Security Filtering**: Agent info endpoint properly filters sensitive settings (llm, auth, api_key, secret, token, password)
7. **ISO Timestamps**: Consistent UTC timestamp formatting with Z suffix

### Refactoring Performed

No refactoring required. The implementation is clean and follows all established patterns.

### Compliance Check

- Coding Standards: ✓ Follows Python conventions, type hints, docstrings
- Project Structure: ✓ Module organization matches story specification
- Testing Strategy: ✓ 40 tests covering all 9 ACs with proper unit/integration split
- All ACs Met: ✓ All 9 acceptance criteria fully implemented and tested

### Improvements Checklist

All items addressed by developer - no outstanding improvements required:

- [x] ServerSettings Pydantic model with all endpoint configuration options (AC1)
- [x] Health endpoint returns JSON with service info and timestamp (AC2)
- [x] Readiness endpoint checks all registered health checks, returns 503 on failure (AC3)
- [x] Agents list and info endpoints with proper filtering (AC4, AC5)
- [x] MetricsCollector with thread-safe Prometheus text format export (AC6)
- [x] OpenAPI 3.0 spec generation from agent registry (AC7)
- [x] Custom health checks support both sync and async callables (AC8)
- [x] Custom path configuration via PathOverrides model (AC9)
- [x] Comprehensive test coverage (40 tests passing)
- [x] Documentation added to specialized.md with Kubernetes probe examples

### Security Review

**Status: PASS**

1. **Sensitive Settings Filtering**: Implemented in `_agent_info_handler()` - filters `llm`, `auth`, `api_key`, `secret`, `token`, `password` keys (endpoints.py:248-251)
2. **No Secrets Exposure**: Health/ready/agents endpoints don't expose credentials
3. **Input Validation**: Pydantic models validate all configuration inputs

### Performance Considerations

**Status: PASS**

1. **Thread-Safe Metrics**: `threading.Lock` ensures safe concurrent metric recording
2. **Efficient Handler Design**: Handlers are async-first with sync fallback
3. **No N+1 Issues**: Agent registry accessed directly without nested queries
4. **Prometheus Export**: Simple string concatenation, efficient for scraping

### Files Modified During Review

No files modified during review - implementation quality is production-ready.

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-BUILTIN-015.8-health-metadata-endpoints.yml

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, 40/40 tests passing, documentation complete, no blocking issues identified.
