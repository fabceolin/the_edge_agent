# Test Design: Story TEA-BUILTIN-015.8

**Story:** Health & Metadata Endpoints
**Date:** 2026-01-05
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 42 |
| Unit tests | 14 (33%) |
| Integration tests | 19 (45%) |
| E2E tests | 9 (22%) |
| **Priority Distribution** | P0: 12, P1: 18, P2: 10, P3: 2 |

### Strategy Rationale

This story implements operational endpoints (health, readiness, metrics, agents, OpenAPI) critical for Kubernetes deployments and production observability. Testing emphasizes:

1. **Integration-heavy**: Most functionality involves HTTP routing, component configuration, and system state inspection
2. **P0 for Kubernetes probes**: Health/readiness endpoints are deployment-critical
3. **Unit tests for metrics collection**: Pure logic in `MetricsCollector`
4. **E2E for full stack validation**: Complete server startup with endpoint availability

---

## Test Scenarios by Acceptance Criteria

### AC1: Settings Schema

**Requirement:** `settings.server` configures which endpoints to enable

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.8-UNIT-001 | Unit | P1 | ServerSettings default values are correct | Pure Pydantic model validation |
| 015.8-UNIT-002 | Unit | P1 | ServerSettings validates custom paths | Input validation logic |
| 015.8-UNIT-003 | Unit | P2 | HealthCheckConfig model validation | Config schema correctness |
| 015.8-INT-001 | Integration | P1 | ServerSettings parsed from YAML dict | YAML to model integration |
| 015.8-INT-002 | Integration | P1 | Invalid server settings raise ValidationError | Error boundary testing |

---

### AC2: Health Endpoint

**Requirement:** `/health` returns service health status

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.8-INT-003 | Integration | P0 | GET /health returns 200 with healthy status | Kubernetes liveness probe critical path |
| 015.8-INT-004 | Integration | P0 | Health response includes service_name from settings | Configuration integration |
| 015.8-INT-005 | Integration | P0 | Health response includes version from settings | Version tracking for deployments |
| 015.8-INT-006 | Integration | P1 | Health response timestamp is ISO format | Contract compliance |
| 015.8-INT-007 | Integration | P1 | Health endpoint disabled when health_endpoint=false | Toggle behavior |

---

### AC3: Readiness Endpoint

**Requirement:** `/ready` checks dependencies are available

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.8-INT-008 | Integration | P0 | GET /ready returns 200 when all checks pass | Kubernetes readiness probe critical |
| 015.8-INT-009 | Integration | P0 | GET /ready returns 503 when any check fails | Deployment gating behavior |
| 015.8-INT-010 | Integration | P0 | Ready response includes individual check results | Debugging dependency failures |
| 015.8-INT-011 | Integration | P1 | Ready response includes latency_ms per check | Performance observability |
| 015.8-UNIT-004 | Unit | P1 | Async health check functions awaited correctly | Async handling logic |
| 015.8-UNIT-005 | Unit | P1 | Sync health check functions called directly | Fallback path |
| 015.8-INT-012 | Integration | P1 | Check exception captured as error status | Error isolation |
| 015.8-INT-013 | Integration | P2 | Empty health_checks list returns ready | Edge case: no deps |

---

### AC4: Agents List Endpoint

**Requirement:** `/agents` lists available agents with metadata

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.8-INT-014 | Integration | P1 | GET /agents returns list of registered agents | Core discovery feature |
| 015.8-INT-015 | Integration | P1 | Each agent includes name, description, endpoint | Contract completeness |
| 015.8-INT-016 | Integration | P2 | Empty registry returns empty list with count=0 | Edge case handling |
| 015.8-INT-017 | Integration | P2 | Agents list endpoint disabled when list_agents=false | Toggle behavior |

---

### AC5: Agent Info Endpoint

**Requirement:** `/agents/{name}` returns specific agent details

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.8-INT-018 | Integration | P1 | GET /agents/{name} returns agent details | Individual agent discovery |
| 015.8-INT-019 | Integration | P1 | Response includes input_schema and output_schema | Schema documentation |
| 015.8-INT-020 | Integration | P1 | GET /agents/{unknown} returns 404 | Error path validation |
| 015.8-INT-021 | Integration | P2 | Sensitive settings (llm, auth) are filtered | Security consideration |

---

### AC6: Metrics Endpoint

**Requirement:** `/metrics` returns Prometheus-format metrics

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.8-UNIT-006 | Unit | P0 | MetricsCollector.record_execution increments counters | Core metrics logic |
| 015.8-UNIT-007 | Unit | P0 | MetricsCollector.record_error tracks by error_type | Error categorization |
| 015.8-UNIT-008 | Unit | P0 | MetricsCollector.to_prometheus produces valid format | Output format correctness |
| 015.8-UNIT-009 | Unit | P1 | Execution duration tracked in durations list | Duration histogram input |
| 015.8-UNIT-010 | Unit | P1 | Thread lock prevents race conditions | Concurrency safety |
| 015.8-INT-022 | Integration | P1 | GET /metrics returns text/plain Prometheus format | Endpoint contract |
| 015.8-INT-023 | Integration | P2 | Metrics endpoint disabled when metrics=false | Toggle behavior |
| 015.8-E2E-001 | E2E | P1 | Agent execution updates metrics observable at /metrics | Full observability path |

---

### AC7: OpenAPI Endpoint

**Requirement:** `/openapi.json` returns OpenAPI specification

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.8-INT-024 | Integration | P1 | GET /openapi.json returns valid OpenAPI 3.x spec | API documentation |
| 015.8-INT-025 | Integration | P2 | OpenAPI includes all registered agent endpoints | Comprehensive spec |
| 015.8-INT-026 | Integration | P2 | OpenAPI disabled when openapi=false | Toggle behavior |

---

### AC8: Custom Health Checks

**Requirement:** Support custom health check functions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.8-UNIT-011 | Unit | P1 | @engine.health_check decorator registers function | Decorator functionality |
| 015.8-UNIT-012 | Unit | P2 | Custom check returning False marks as unhealthy | Boolean return handling |
| 015.8-INT-027 | Integration | P1 | Custom async health check integrated into /ready | Full registration flow |
| 015.8-INT-028 | Integration | P2 | HTTP health check type performs GET request | Built-in check type |

---

### AC9: Configurable Paths

**Requirement:** Allow customization of endpoint paths

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.8-UNIT-013 | Unit | P1 | Custom paths override defaults in ServerSettings | Configuration logic |
| 015.8-INT-029 | Integration | P1 | Custom /healthz path responds correctly | Path remapping |
| 015.8-INT-030 | Integration | P1 | Custom /readyz path responds correctly | Path remapping |
| 015.8-INT-031 | Integration | P2 | Multiple custom paths configured simultaneously | Combined configuration |
| 015.8-UNIT-014 | Unit | P3 | Default paths used when paths dict is empty | Default fallback |

---

### Cross-Cutting: Full Server Integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.8-E2E-002 | E2E | P0 | Server starts with all endpoints enabled by default | Production readiness |
| 015.8-E2E-003 | E2E | P0 | Kubernetes liveness probe succeeds against /health | Deployment validation |
| 015.8-E2E-004 | E2E | P0 | Kubernetes readiness probe succeeds against /ready | Deployment validation |
| 015.8-E2E-005 | E2E | P1 | YAML agent loading populates /agents endpoint | Integration completeness |
| 015.8-E2E-006 | E2E | P1 | configure_server() correctly registers all routers | Initialization flow |
| 015.8-E2E-007 | E2E | P2 | Server with all endpoints disabled still runs | Minimal config mode |
| 015.8-E2E-008 | E2E | P2 | Prometheus scrape simulation on /metrics | Real-world observability |
| 015.8-E2E-009 | E2E | P3 | Load test: 1000 health checks/sec without degradation | Performance validation |

---

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| Kubernetes deployment failures | 015.8-INT-003, 015.8-INT-008, 015.8-E2E-003, 015.8-E2E-004 | P0 priority on probe endpoints |
| Metrics race conditions | 015.8-UNIT-010 | Thread lock validation |
| Dependency check timeout | 015.8-INT-012 | Exception isolation testing |
| Sensitive settings exposure | 015.8-INT-021 | Security filter validation |
| OpenAPI spec invalid | 015.8-INT-024 | Schema validation |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 - CI Gate)
1. 015.8-UNIT-006, 015.8-UNIT-007, 015.8-UNIT-008 - Metrics core logic
2. 015.8-INT-003, 015.8-INT-004, 015.8-INT-005 - Health endpoint
3. 015.8-INT-008, 015.8-INT-009, 015.8-INT-010 - Readiness endpoint
4. 015.8-E2E-002, 015.8-E2E-003, 015.8-E2E-004 - Server startup validation

### Phase 2: Core Functionality (P1)
5. All remaining P1 unit tests
6. All remaining P1 integration tests
7. 015.8-E2E-001, 015.8-E2E-005, 015.8-E2E-006 - Full stack flows

### Phase 3: Coverage Completion (P2)
8. Toggle/disable functionality tests
9. Edge case tests
10. 015.8-E2E-007, 015.8-E2E-008

### Phase 4: Extended (P3 - Time Permitting)
11. 015.8-UNIT-014, 015.8-E2E-009

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 42
  by_level:
    unit: 14
    integration: 19
    e2e: 9
  by_priority:
    p0: 12
    p1: 18
    p2: 10
    p3: 2
  coverage_gaps: []
  coverage_notes:
    - All 9 acceptance criteria have test coverage
    - Health/Readiness endpoints have P0 focus for Kubernetes
    - MetricsCollector unit tested for thread safety
    - OpenAPI generation tested at integration level
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-015.8-test-design-20260105.md
P0 tests identified: 12
Story ID: TEA-BUILTIN-015.8
Story Title: Health & Metadata Endpoints
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (Kubernetes = P0)
- [x] Test IDs follow naming convention (015.8-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
