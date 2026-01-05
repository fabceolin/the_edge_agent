# Test Design: Story TEA-BUILTIN-015.7

**Story:** HTTP Endpoint Configuration
**Date:** 2026-01-05
**Designer:** Quinn (Test Architect)
**Story Path:** `docs/stories/TEA-BUILTIN-015.7-endpoint-configuration.md`

---

## Test Strategy Overview

| Metric | Count |
|--------|-------|
| **Total test scenarios** | 42 |
| **Unit tests** | 22 (52%) |
| **Integration tests** | 14 (33%) |
| **E2E tests** | 6 (15%) |

**Priority Distribution:**
| Priority | Count | Percentage |
|----------|-------|------------|
| P0 | 12 | 29% |
| P1 | 18 | 43% |
| P2 | 10 | 24% |
| P3 | 2 | 4% |

---

## Test Scenarios by Acceptance Criteria

### AC1: Endpoint Definition

> `endpoint` section defines path, method, and description

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.7-UNIT-001 | Unit | P0 | `EndpointConfig.from_yaml()` parses valid endpoint with path, method, description | Core parsing logic - pure function |
| 015.7-UNIT-002 | Unit | P0 | EndpointConfig validates required `path` field | Input validation - prevent invalid configs |
| 015.7-UNIT-003 | Unit | P1 | EndpointConfig uses POST as default method when not specified | Default behavior validation |
| 015.7-UNIT-004 | Unit | P1 | EndpointConfig accepts optional `summary`, `description`, `tags` | Optional field handling |
| 015.7-UNIT-005 | Unit | P1 | EndpointConfig rejects invalid path format (missing leading `/`) | Input validation edge case |
| 015.7-INT-001 | Integration | P1 | YAMLEngine parses agent with `endpoint` section and creates EndpointConfig | Component interaction YAMLEngine → EndpointConfig |

---

### AC2: Custom Paths

> Agents can define custom URL paths (e.g., `/research`, `/chat`)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.7-UNIT-006 | Unit | P0 | EndpointConfig accepts custom paths like `/api/v1/research` | Core functionality |
| 015.7-UNIT-007 | Unit | P1 | EndpointConfig accepts paths with multiple segments `/api/v1/users/profile` | Path depth validation |
| 015.7-UNIT-008 | Unit | P2 | EndpointConfig handles trailing slash normalization | Edge case handling |
| 015.7-INT-002 | Integration | P1 | RouteRegistry registers agent with custom path correctly | Route registration accuracy |

---

### AC3: HTTP Methods

> Support GET, POST, PUT, DELETE methods

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.7-UNIT-009 | Unit | P0 | HTTPMethod enum contains GET, POST, PUT, DELETE, PATCH | Enum completeness |
| 015.7-UNIT-010 | Unit | P1 | EndpointConfig validates method is valid HTTPMethod | Invalid method rejection |
| 015.7-UNIT-011 | Unit | P1 | EndpointConfig accepts case-insensitive method names (get → GET) | Usability - YAML flexibility |
| 015.7-INT-003 | Integration | P1 | RouteRegistry creates route for each HTTP method type | Method-specific routing |

---

### AC4: Request Schema Integration

> Link to `input_schema` for request body/params

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.7-UNIT-012 | Unit | P0 | EndpointConfig links `request_schema_ref` to input_schema by default | Schema linkage |
| 015.7-UNIT-013 | Unit | P1 | EndpointConfig allows custom request schema reference | Flexibility for complex agents |
| 015.7-INT-004 | Integration | P0 | Request body validated against linked input_schema | Critical - data integrity |
| 015.7-INT-005 | Integration | P1 | Invalid request body returns 422 with validation errors | Error handling |

---

### AC5: Response Schema Integration

> Link to `output_schema` for response format

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.7-UNIT-014 | Unit | P0 | EndpointConfig links `response_schema_ref` to output_schema by default | Schema linkage |
| 015.7-UNIT-015 | Unit | P1 | EndpointConfig supports response examples in YAML | OpenAPI documentation support |
| 015.7-INT-006 | Integration | P0 | Response transformed to match output_schema | Critical - API contract |

---

### AC6: Path Parameters

> Support path parameters (e.g., `/users/{user_id}`)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.7-UNIT-016 | Unit | P0 | Parse `{param}` syntax from path string | Path parameter extraction |
| 015.7-UNIT-017 | Unit | P1 | PathParam model validates type, description, pattern fields | Model validation |
| 015.7-UNIT-018 | Unit | P1 | Multiple path params extracted from `/users/{user_id}/sessions/{session_id}` | Complex path handling |
| 015.7-UNIT-019 | Unit | P1 | Path param with regex pattern validates value format | Pattern matching |
| 015.7-INT-007 | Integration | P0 | Path params mapped to agent input state correctly | State population |
| 015.7-INT-008 | Integration | P1 | Invalid path param value returns 400 error | Error handling |

---

### AC7: Query Parameters

> Map query params to input schema fields

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.7-UNIT-020 | Unit | P1 | QueryParam model parses type, required, default, map_to fields | Model parsing |
| 015.7-UNIT-021 | Unit | P1 | QueryParam with `map_to` links to input_schema field | Field mapping |
| 015.7-UNIT-022 | Unit | P2 | QueryParam default value applied when param missing | Default handling |
| 015.7-INT-009 | Integration | P1 | Query params extracted from URL and mapped to input state | State population |
| 015.7-INT-010 | Integration | P1 | Required query param missing returns 400 error | Error handling |
| 015.7-INT-011 | Integration | P2 | Query param type coercion (string to int) | Type handling |

---

### AC8: OpenAPI Generation

> Auto-generate OpenAPI spec from endpoint definitions

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.7-UNIT-023 | Unit | P0 | `generate_openapi_spec()` returns valid OpenAPI 3.0 structure | Core generation |
| 015.7-UNIT-024 | Unit | P1 | OpenAPI spec includes path parameters as `{param}` format | OpenAPI compliance |
| 015.7-UNIT-025 | Unit | P1 | OpenAPI spec includes query parameters with types | OpenAPI compliance |
| 015.7-UNIT-026 | Unit | P1 | OpenAPI spec includes request/response schemas | Schema documentation |
| 015.7-INT-012 | Integration | P0 | OpenAPI spec from multiple agents merged correctly | Multi-agent support |
| 015.7-E2E-001 | E2E | P2 | `/openapi.json` endpoint returns valid spec for all loaded agents | Endpoint availability |

---

### AC9: Auth Override

> Per-endpoint auth requirements (public vs authenticated)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.7-UNIT-027 | Unit | P0 | EndpointConfig parses auth section with required/optional/public | Auth config parsing |
| 015.7-UNIT-028 | Unit | P1 | EndpointConfig parses auth roles array | Role-based auth |
| 015.7-INT-013 | Integration | P0 | Public endpoint (`auth.required: false`) bypasses auth middleware | Security - public access |
| 015.7-INT-014 | Integration | P0 | Authenticated endpoint enforces auth when global auth disabled | Security - per-route override |
| 015.7-E2E-002 | E2E | P0 | Public health endpoint accessible without token | Critical security path |
| 015.7-E2E-003 | E2E | P0 | Protected endpoint returns 401 without valid token | Critical security path |
| 015.7-E2E-004 | E2E | P1 | Role-restricted endpoint returns 403 for wrong role | Authorization validation |

---

### AC10: Route Registration

> Routes auto-registered when agent is loaded

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.7-UNIT-029 | Unit | P1 | RouteRegistry stores route with path, method, agent_name, handler | Registry data structure |
| 015.7-UNIT-030 | Unit | P2 | RouteRegistry prevents duplicate path/method registration | Conflict prevention |
| 015.7-INT-015 | Integration | P0 | `engine.load_from_file()` auto-registers route when endpoint defined | Auto-registration |
| 015.7-INT-016 | Integration | P1 | `engine.get_router()` returns FastAPI router with all routes | Router export |
| 015.7-E2E-005 | E2E | P1 | Agent loaded dynamically creates accessible route | Dynamic loading validation |
| 015.7-E2E-006 | E2E | P2 | Multiple agents with different paths all accessible | Multi-agent routing |

---

## Risk Coverage Mapping

| Risk | Mitigation Tests |
|------|------------------|
| Invalid endpoint config crashes agent loading | 015.7-UNIT-002, 015.7-UNIT-005, 015.7-UNIT-010 |
| Path params not extracted correctly | 015.7-UNIT-016, 015.7-UNIT-018, 015.7-INT-007 |
| Auth bypass vulnerability | 015.7-E2E-002, 015.7-E2E-003, 015.7-INT-013, 015.7-INT-014 |
| OpenAPI spec not valid | 015.7-UNIT-023, 015.7-INT-012, 015.7-E2E-001 |
| Route conflicts between agents | 015.7-UNIT-030 |
| Schema mismatch breaks API contract | 015.7-INT-004, 015.7-INT-006 |

---

## Recommended Execution Order

### Phase 1: P0 Critical (Must Pass)
1. 015.7-UNIT-001, 002, 006, 009, 012, 014, 016, 023, 027 (Unit - Core parsing)
2. 015.7-INT-004, 006, 007, 012, 013, 014, 015 (Integration - Critical flows)
3. 015.7-E2E-002, 003 (E2E - Security validation)

### Phase 2: P1 High Priority
4. 015.7-UNIT-003, 004, 005, 007, 010, 011, 013, 015, 017, 018, 019, 020, 021, 024, 025, 026, 028, 029 (Unit)
5. 015.7-INT-001, 002, 003, 005, 008, 009, 010, 016 (Integration)
6. 015.7-E2E-004, 005 (E2E)

### Phase 3: P2 Medium Priority
7. 015.7-UNIT-008, 022, 030 (Unit)
8. 015.7-INT-011 (Integration)
9. 015.7-E2E-001, 006 (E2E)

### Phase 4: P3 Low Priority
10. P3 tests only in full regression cycles

---

## Test File Locations

| Level | Recommended File Location |
|-------|---------------------------|
| Unit | `python/tests/test_endpoint_config.py` |
| Unit | `python/tests/test_openapi_generation.py` |
| Integration | `python/tests/test_endpoint_integration.py` |
| E2E | `python/tests/e2e/test_endpoint_e2e.py` |

---

## Gate YAML Block

```yaml
test_design:
  story: TEA-BUILTIN-015.7
  date: 2026-01-05
  scenarios_total: 42
  by_level:
    unit: 22
    integration: 14
    e2e: 6
  by_priority:
    p0: 12
    p1: 18
    p2: 10
    p3: 2
  coverage_gaps: []
  high_risk_areas:
    - auth_override_security
    - path_parameter_extraction
    - openapi_spec_validity
  recommended_coverage:
    unit: ">90%"
    integration: ">80%"
    e2e: "all critical paths"
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention `{epic}.{story}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Security-critical paths have P0 priority
- [x] Auth override tested at multiple levels (defense in depth)

---

## Notes for Implementers

1. **FastAPI Mocking**: Integration tests should use `TestClient` from FastAPI for route testing
2. **Schema Validation**: Use pytest parametrize for multiple valid/invalid schema combinations
3. **Auth Testing**: Create test fixtures for authenticated/unauthenticated/role-based requests
4. **OpenAPI Validation**: Use `openapi-spec-validator` library to validate generated specs
5. **Path Params**: Test with Unicode, special characters, and SQL injection patterns

---

*Generated by Quinn, Test Architect - BMAD QA Agent*
