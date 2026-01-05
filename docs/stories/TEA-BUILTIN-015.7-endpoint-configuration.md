# Story TEA-BUILTIN-015.7: HTTP Endpoint Configuration

## Status: Ready for Development

## Story

**As a** TEA agent developer,
**I want** to define HTTP endpoint configuration per agent in YAML,
**so that** each agent can have its own API contract without modifying Python router code.

## Acceptance Criteria

1. **AC1: Endpoint Definition** - `endpoint` section defines path, method, and description
2. **AC2: Custom Paths** - Agents can define custom URL paths (e.g., `/research`, `/chat`)
3. **AC3: HTTP Methods** - Support GET, POST, PUT, DELETE methods
4. **AC4: Request Schema Integration** - Link to `input_schema` for request body/params
5. **AC5: Response Schema Integration** - Link to `output_schema` for response format
6. **AC6: Path Parameters** - Support path parameters (e.g., `/users/{user_id}`)
7. **AC7: Query Parameters** - Map query params to input schema fields
8. **AC8: OpenAPI Generation** - Auto-generate OpenAPI spec from endpoint definitions
9. **AC9: Auth Override** - Per-endpoint auth requirements (public vs authenticated)
10. **AC10: Route Registration** - Routes auto-registered when agent is loaded

## Tasks / Subtasks

- [ ] **Task 1: Define Endpoint Schema** (AC1, AC2, AC3)
  - [ ] Create `EndpointConfig` Pydantic model
  - [ ] Add `endpoint` field to agent config parser
  - [ ] Support path, method, description, tags

- [ ] **Task 2: Implement Path Parameter Parsing** (AC6)
  - [ ] Parse `{param}` syntax in path
  - [ ] Extract parameters from URL
  - [ ] Map to input schema fields

- [ ] **Task 3: Implement Query Parameter Mapping** (AC7)
  - [ ] Define query params in endpoint config
  - [ ] Extract from request URL
  - [ ] Map to input schema fields

- [ ] **Task 4: Integrate with Input/Output Schemas** (AC4, AC5)
  - [ ] Link endpoint request to input_schema
  - [ ] Link endpoint response to output_schema
  - [ ] Validate schemas match endpoint definition

- [ ] **Task 5: Implement OpenAPI Generation** (AC8)
  - [ ] Generate OpenAPI path spec from endpoint
  - [ ] Include request/response schemas
  - [ ] Add authentication requirements
  - [ ] Export as `/openapi.json`

- [ ] **Task 6: Implement Auth Override** (AC9)
  - [ ] Add `auth` field to endpoint config
  - [ ] Support: `required`, `optional`, `public`
  - [ ] Override global auth settings per endpoint

- [ ] **Task 7: Implement Auto-Registration** (AC10)
  - [ ] Create route registry in YAMLEngine
  - [ ] Auto-register routes when agents loaded
  - [ ] Support dynamic route addition/removal
  - [ ] Provide FastAPI router export

- [ ] **Task 8: Write Tests** (AC1-AC10)
  - [ ] Test endpoint definition parsing
  - [ ] Test path parameter extraction
  - [ ] Test query parameter mapping
  - [ ] Test OpenAPI generation
  - [ ] Test auth override
  - [ ] Integration test with FastAPI

- [ ] **Task 9: Documentation**
  - [ ] Update `docs/shared/YAML_REFERENCE.md` with endpoint config
  - [ ] Add API design patterns guide
  - [ ] Include OpenAPI integration example

## Dev Notes

### Endpoint Schema

```yaml
name: research_agent

endpoint:
  path: "/api/v1/research"      # Custom path
  method: POST                   # HTTP method
  summary: "Execute research query"
  description: |
    Searches the web and synthesizes an answer from sources.
  tags:
    - Research
    - AI Agents

  # Auth override (optional)
  auth:
    required: true               # Override global auth

  # Request body (links to input_schema)
  request:
    content_type: "application/json"
    schema_ref: input_schema     # Use defined input_schema

  # Response (links to output_schema)
  response:
    content_type: "application/json"
    schema_ref: output_schema    # Use defined output_schema
    examples:
      success:
        summary: "Successful response"
        value:
          success: true
          answer: "The answer is 42"
          sources: ["http://example.com"]

# Input/Output schemas as usual
input_schema:
  query:
    type: str
    required: true

output_schema:
  success: true
  answer: "{{ state.answer }}"
```

### Path Parameters

```yaml
endpoint:
  path: "/api/v1/users/{user_id}/sessions/{session_id}"
  method: GET

  # Path params auto-mapped to input
  path_params:
    user_id:
      type: str
      description: "User identifier"
    session_id:
      type: str
      pattern: "^[a-f0-9-]{36}$"
```

### Query Parameters

```yaml
endpoint:
  path: "/api/v1/search"
  method: GET

  # Query params
  query_params:
    q:
      type: str
      required: true
      description: "Search query"
      map_to: query             # Maps to input_schema.query

    limit:
      type: int
      default: 10
      min: 1
      max: 100
```

### Auth Override Options

```yaml
endpoint:
  path: "/api/v1/public/health"
  method: GET
  auth:
    required: false             # Public endpoint

---

endpoint:
  path: "/api/v1/admin/config"
  method: PUT
  auth:
    required: true
    roles: ["admin"]            # Require admin role
```

### Generated OpenAPI Spec

```json
{
  "openapi": "3.0.0",
  "paths": {
    "/api/v1/research": {
      "post": {
        "summary": "Execute research query",
        "tags": ["Research", "AI Agents"],
        "security": [{"firebase_auth": []}],
        "requestBody": {
          "content": {
            "application/json": {
              "schema": {
                "type": "object",
                "required": ["query"],
                "properties": {
                  "query": {"type": "string"}
                }
              }
            }
          }
        },
        "responses": {
          "200": {
            "description": "Successful response",
            "content": {
              "application/json": {
                "schema": {...}
              }
            }
          }
        }
      }
    }
  }
}
```

### Route Registry API

```python
from the_edge_agent import YAMLEngine

engine = YAMLEngine()

# Load agents (auto-registers routes)
engine.load_agents_from_directory("./agents")

# Get FastAPI router
router = engine.get_router()

# Use in FastAPI app
app.include_router(router, prefix="/api/v1")

# Or get OpenAPI spec
openapi_spec = engine.get_openapi_spec()
```

### Module Structure (Files to Create)

```
python/src/the_edge_agent/
├── http/                           # NEW MODULE
│   ├── __init__.py                 # Exports: RouteRegistry, EndpointConfig
│   ├── endpoint.py                 # EndpointConfig model parsing
│   ├── router.py                   # RouteRegistry + FastAPI router factory
│   ├── params.py                   # Path/query parameter extraction
│   └── openapi.py                  # OpenAPI spec generation
│
├── yaml_engine.py                  # MODIFY: Add route registration on agent load
└── settings.py                     # No changes (endpoint is agent-level config)
```

### File Contents Overview

**http/__init__.py:**
```python
from .endpoint import EndpointConfig
from .router import RouteRegistry, get_fastapi_router
from .openapi import generate_openapi_spec

__all__ = [
    "EndpointConfig",
    "RouteRegistry",
    "get_fastapi_router",
    "generate_openapi_spec"
]
```

**http/endpoint.py:**
```python
from typing import Optional, List, Dict, Any
from pydantic import BaseModel
from enum import Enum

class HTTPMethod(str, Enum):
    GET = "GET"
    POST = "POST"
    PUT = "PUT"
    DELETE = "DELETE"
    PATCH = "PATCH"

class PathParam(BaseModel):
    type: str = "str"
    description: Optional[str] = None
    pattern: Optional[str] = None

class QueryParam(BaseModel):
    type: str = "str"
    required: bool = False
    default: Any = None
    description: Optional[str] = None
    map_to: Optional[str] = None  # Maps to input_schema field

class EndpointConfig(BaseModel):
    path: str
    method: HTTPMethod = HTTPMethod.POST
    summary: Optional[str] = None
    description: Optional[str] = None
    tags: List[str] = []
    auth: Optional[Dict[str, Any]] = None  # Override global auth
    path_params: Dict[str, PathParam] = {}
    query_params: Dict[str, QueryParam] = {}
    request_schema_ref: str = "input_schema"
    response_schema_ref: str = "output_schema"

    @classmethod
    def from_yaml(cls, yaml_dict: dict) -> "EndpointConfig":
        """Parse endpoint config from YAML."""
        return cls(**yaml_dict)
```

**http/router.py:**
```python
from typing import Dict, Callable, Optional
from dataclasses import dataclass, field

@dataclass
class RegisteredRoute:
    path: str
    method: str
    agent_name: str
    handler: Callable
    config: "EndpointConfig"

class RouteRegistry:
    """Registry of agent routes."""

    def __init__(self):
        self._routes: Dict[str, RegisteredRoute] = {}

    def register(self, agent_name: str, config: "EndpointConfig", handler: Callable):
        """Register an agent route."""
        route_key = f"{config.method}:{config.path}"
        self._routes[route_key] = RegisteredRoute(
            path=config.path,
            method=config.method,
            agent_name=agent_name,
            handler=handler,
            config=config
        )

    def get_fastapi_router(self):
        """Generate FastAPI router from registered routes."""
        from fastapi import APIRouter, Request
        router = APIRouter()

        for route in self._routes.values():
            # Create route handler
            async def handler(request: Request, route=route):
                # Extract params, validate, execute agent
                pass

            router.add_api_route(
                route.path,
                handler,
                methods=[route.method],
                summary=route.config.summary,
                description=route.config.description,
                tags=route.config.tags
            )

        return router

    @property
    def routes(self) -> Dict[str, RegisteredRoute]:
        return self._routes
```

**http/openapi.py:**
```python
from typing import Dict, Any
from .router import RouteRegistry

def generate_openapi_spec(
    registry: RouteRegistry,
    title: str = "TEA Agents API",
    version: str = "1.0.0",
    description: str = ""
) -> Dict[str, Any]:
    """Generate OpenAPI 3.0 specification from route registry."""
    spec = {
        "openapi": "3.0.0",
        "info": {
            "title": title,
            "version": version,
            "description": description
        },
        "paths": {},
        "components": {
            "schemas": {},
            "securitySchemes": {}
        }
    }

    for route_key, route in registry.routes.items():
        path_spec = _generate_path_spec(route)
        spec["paths"][route.path] = spec["paths"].get(route.path, {})
        spec["paths"][route.path][route.method.lower()] = path_spec

    return spec

def _generate_path_spec(route) -> Dict[str, Any]:
    """Generate OpenAPI path specification for a route."""
    return {
        "summary": route.config.summary,
        "description": route.config.description,
        "tags": route.config.tags,
        "operationId": f"{route.agent_name}_{route.method.lower()}",
        "responses": {
            "200": {"description": "Successful response"}
        }
    }
```

### Relevant Existing Files (Minimal Changes)

- `python/src/the_edge_agent/yaml_engine.py` - Add route registration on load (~20 lines)

### Integration with YAMLEngine

```python
# yaml_engine.py additions

class YAMLEngine:
    def __init__(self):
        # ... existing code ...
        self._route_registry = RouteRegistry()

    def load_from_file(self, path: str, ...):
        # ... existing loading code ...

        # Register route if endpoint defined
        if "endpoint" in config:
            from .http import EndpointConfig
            endpoint = EndpointConfig.from_yaml(config["endpoint"])
            self._route_registry.register(
                agent_name=config.get("name", path),
                config=endpoint,
                handler=lambda state: self._execute_agent(state)
            )

    def get_router(self):
        """Get FastAPI router with all registered agent routes."""
        return self._route_registry.get_fastapi_router()

    def get_openapi_spec(self):
        """Get OpenAPI spec for all registered agents."""
        from .http import generate_openapi_spec
        return generate_openapi_spec(self._route_registry)
```

### Dependencies

- Builds on TEA-BUILTIN-015.4 (Input Validation)
- Builds on TEA-BUILTIN-015.5 (Response Transformation)
- Builds on TEA-BUILTIN-015.3 (Auth Middleware)

### Testing

**Test file location:** `python/tests/test_endpoint_config.py`

**Testing standards:**
- Test endpoint parsing
- Test path/query param extraction
- Test OpenAPI generation
- Mock FastAPI for route tests
- Minimum 90% coverage

**Test cases:**
1. Simple POST endpoint registered
2. Path params extracted correctly
3. Query params mapped to input
4. Auth override respected
5. OpenAPI spec generated correctly
6. Multiple agents registered
7. Invalid endpoint config rejected

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-05 | 1.0 | Initial story creation | Sarah (PO) |

## Dev Agent Record

### Agent Model Used
_To be filled by dev agent_

### Debug Log References
_To be filled by dev agent_

### Completion Notes List
_To be filled by dev agent_

### File List
_To be filled by dev agent_

## QA Results

### Test Design Assessment

**Date:** 2026-01-05
**Reviewer:** Quinn (Test Architect)

#### Test Strategy Summary

| Metric | Value |
|--------|-------|
| Total Scenarios | 42 |
| Unit Tests | 22 (52%) |
| Integration Tests | 14 (33%) |
| E2E Tests | 6 (15%) |

#### Priority Distribution

| Priority | Count | Focus Areas |
|----------|-------|-------------|
| P0 | 12 | Auth security, core parsing, schema linkage |
| P1 | 18 | HTTP methods, param mapping, OpenAPI |
| P2 | 10 | Edge cases, defaults, multi-agent |
| P3 | 2 | Full regression only |

#### High-Risk Areas Identified

1. **Auth Override Security** - Per-endpoint auth bypass could create vulnerabilities
2. **Path Parameter Extraction** - Regex patterns and injection risks
3. **OpenAPI Spec Validity** - Generated specs must be valid OpenAPI 3.0

#### Coverage Assessment

- **AC Coverage:** All 10 ACs have test scenarios ✓
- **Security Tests:** Auth override tested at Unit, Integration, and E2E levels ✓
- **Error Handling:** Invalid inputs return proper 400/401/403/422 errors ✓

#### Recommendations

1. Use `openapi-spec-validator` library to validate generated OpenAPI specs
2. Include security test patterns (SQL injection, path traversal) in path param tests
3. Mock FastAPI with `TestClient` for integration tests
4. Create auth fixtures for authenticated/unauthenticated/role-based scenarios

#### Test Design Document

📄 `docs/qa/assessments/TEA-BUILTIN-015.7-test-design-20260105.md`

---

## QA Notes

**Review Date:** 2026-01-05
**QA Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Level | Count | Coverage Focus |
|-------|-------|----------------|
| Unit | 22 | Core parsing, model validation, HTTPMethod enum, schema linking |
| Integration | 14 | YAMLEngine integration, route registration, param mapping, auth flow |
| E2E | 6 | Security paths, OpenAPI endpoint, dynamic agent loading |

**Acceptance Criteria Coverage:** 100% - All 10 ACs have dedicated test scenarios with appropriate level distribution.

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **Auth Override Security** | HIGH | Defense-in-depth testing at Unit/Int/E2E levels (5 tests). Public endpoint bypass requires careful validation. |
| **Path Parameter Injection** | HIGH | P0 tests for param extraction + security pattern tests (SQL injection, path traversal) recommended in 015.7-INT-008. |
| **OpenAPI Spec Validity** | MEDIUM | Use `openapi-spec-validator` library. 3 dedicated tests for spec structure and compliance. |
| **Route Conflicts** | MEDIUM | 015.7-UNIT-030 tests duplicate path/method prevention. Multi-agent scenarios in E2E. |
| **Schema Mismatch** | MEDIUM | 015.7-INT-004, 015.7-INT-006 validate request/response schema linkage. |

### Recommended Test Scenarios

**P0 Critical (Must Pass Before Merge):**
- `015.7-UNIT-001`: EndpointConfig parses valid endpoint with path, method, description
- `015.7-UNIT-002`: EndpointConfig validates required `path` field
- `015.7-INT-004`: Request body validated against linked input_schema
- `015.7-INT-013`: Public endpoint bypasses auth middleware correctly
- `015.7-INT-014`: Authenticated endpoint enforces auth when global auth disabled
- `015.7-E2E-002`: Public health endpoint accessible without token
- `015.7-E2E-003`: Protected endpoint returns 401 without valid token

**P1 High Priority:**
- All HTTP method variations (GET/POST/PUT/DELETE/PATCH)
- Path parameter extraction with multiple segments
- Query parameter mapping with type coercion
- OpenAPI generation with request/response schemas
- FastAPI router export via `engine.get_router()`

### Concerns & Blockers

| Type | Description | Recommendation |
|------|-------------|----------------|
| **CONCERN** | Auth override allows public endpoints - requires careful security review during implementation | Add integration test that verifies public endpoints cannot access authenticated resources |
| **CONCERN** | Path param regex patterns could be exploited | Include OWASP-style security inputs in 015.7-INT-008 test data |
| **CONCERN** | No explicit test for malformed YAML endpoint section | Add 015.7-UNIT-031 for YAML parse error handling |

### Quality Gate Readiness

| Criteria | Status |
|----------|--------|
| All ACs have test coverage | ✅ PASS |
| Security-critical paths identified | ✅ PASS |
| Test priorities align with risk | ✅ PASS |
| Test file locations defined | ✅ PASS |
| Implementation dependencies clear | ✅ PASS |

**Gate Status:** READY FOR DEVELOPMENT

### Test File Locations

```
python/tests/test_endpoint_config.py          # Unit tests (22)
python/tests/test_openapi_generation.py       # OpenAPI unit tests (subset)
python/tests/test_endpoint_integration.py     # Integration tests (14)
python/tests/e2e/test_endpoint_e2e.py         # E2E tests (6)
```

### Implementation Notes for Dev

1. **FastAPI Dependency:** Tests require `fastapi` and `httpx` (for TestClient)
2. **Auth Fixtures:** Create reusable fixtures for token generation/validation mocking
3. **OpenAPI Validation:** Add `openapi-spec-validator` to dev dependencies
4. **Security Testing:** Include path traversal (`../../../etc/passwd`) and injection patterns in param tests

---

*QA Notes generated by Quinn, Test Architect - BMAD QA Agent*
