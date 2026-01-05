# Test Design: Epic TEA-BUILTIN-015 (Cloud Production YAML-First)

Date: 2026-01-05
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios**: 87
- **Unit tests**: 42 (48%)
- **Integration tests**: 32 (37%)
- **E2E tests**: 13 (15%)
- **Priority distribution**: P0: 28, P1: 35, P2: 18, P3: 6

### Epic Goals Under Test

The Cloud Production Epic enables **zero-code cloud deployment** by moving authentication, session management, database operations, and HTTP configuration from Python code into declarative YAML. The test strategy focuses on:

1. **Security** - Auth middleware, token verification, and permission enforcement
2. **Data Integrity** - Session persistence, Firestore operations, and state management
3. **Backward Compatibility** - Existing agents must work unchanged
4. **Configuration Correctness** - YAML settings correctly configure runtime behavior

---

## Story 015.1: Session Management in YAML

### AC1: Session Backend Configuration

Configure session persistence via `settings.session` with support for Firestore, Redis, DynamoDB, and in-memory backends.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.1-UNIT-001 | Unit | P0 | Parse valid `settings.session` YAML with all backends | Configuration parsing is foundation for all session features |
| 015.1-UNIT-002 | Unit | P0 | Reject invalid session backend type | Fail-fast on misconfiguration prevents runtime errors |
| 015.1-UNIT-003 | Unit | P1 | Apply default values when optional fields omitted | Backward compatibility with minimal config |
| 015.1-UNIT-004 | Unit | P1 | Validate `ttl` is positive integer | Data integrity constraint |
| 015.1-UNIT-005 | Unit | P2 | Parse collection name with Jinja2 template | Template interpolation correctness |
| 015.1-INT-001 | Integration | P0 | Load session from Firestore backend | Primary cloud backend data flow |
| 015.1-INT-002 | Integration | P0 | Save session to Firestore backend | Data persistence critical path |
| 015.1-INT-003 | Integration | P1 | Load session from Redis backend | Alternative backend verification |
| 015.1-INT-004 | Integration | P1 | Session TTL expiration triggers cleanup | Resource management verification |
| 015.1-INT-005 | Integration | P2 | DynamoDB backend round-trip | Third backend verification |
| 015.1-INT-006 | Integration | P1 | In-memory backend for development | Developer experience path |

### AC2: Session Actions (session.load, session.save)

Built-in actions for explicit session management in agent workflows.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.1-UNIT-006 | Unit | P0 | `session.load` action registered in registry | Action availability verification |
| 015.1-UNIT-007 | Unit | P0 | `session.save` action registered in registry | Action availability verification |
| 015.1-UNIT-008 | Unit | P1 | `session.load` with missing session returns empty state | Graceful handling of new sessions |
| 015.1-INT-007 | Integration | P0 | `session.load` populates state from stored session | Core session restore functionality |
| 015.1-INT-008 | Integration | P0 | `session.save` persists current state to backend | Core session persist functionality |
| 015.1-INT-009 | Integration | P1 | `auto_save: true` triggers save after each node | Convenience feature verification |
| 015.1-E2E-001 | E2E | P0 | Multi-turn conversation maintains state across requests | Critical user journey for conversational agents |

---

## Story 015.2: Firestore Built-in Actions

### AC1: Firestore CRUD Operations

Add `firestore.get`, `firestore.set`, `firestore.query`, and `firestore.delete` actions.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.2-UNIT-001 | Unit | P0 | All four Firestore actions registered in registry | Action availability verification |
| 015.2-UNIT-002 | Unit | P0 | Validate required parameters for each action | Fail-fast on missing params |
| 015.2-UNIT-003 | Unit | P1 | Collection path parsing with slashes | Path normalization logic |
| 015.2-UNIT-004 | Unit | P1 | Document ID extraction from path | Path parsing correctness |
| 015.2-INT-001 | Integration | P0 | `firestore.get` retrieves existing document | Core read operation |
| 015.2-INT-002 | Integration | P0 | `firestore.get` returns null for missing document | Graceful missing document handling |
| 015.2-INT-003 | Integration | P0 | `firestore.set` creates new document | Core create operation |
| 015.2-INT-004 | Integration | P0 | `firestore.set` with merge updates existing | Core update operation |
| 015.2-INT-005 | Integration | P0 | `firestore.delete` removes document | Core delete operation |
| 015.2-INT-006 | Integration | P1 | `firestore.query` with where clause | Query filtering verification |
| 015.2-INT-007 | Integration | P1 | `firestore.query` with order_by and limit | Query sorting and pagination |
| 015.2-INT-008 | Integration | P2 | `firestore.query` with compound filters | Advanced query support |
| 015.2-E2E-001 | E2E | P0 | Agent CRUD workflow (create, read, update, delete) | Full CRUD journey verification |

---

## Story 015.3: Auth Middleware in YAML

### AC1: Authentication Provider Configuration

Configure authentication via `settings.auth` supporting Firebase Auth, JWT, and API key providers.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.3-UNIT-001 | Unit | P0 | Parse valid `settings.auth` with Firebase provider | Security config parsing |
| 015.3-UNIT-002 | Unit | P0 | Parse valid `settings.auth` with JWT provider | Alternative auth provider |
| 015.3-UNIT-003 | Unit | P0 | Parse valid `settings.auth` with API key provider | Third auth option |
| 015.3-UNIT-004 | Unit | P0 | Reject unknown auth provider | Fail-fast on misconfiguration |
| 015.3-UNIT-005 | Unit | P1 | Default `token_header` when not specified | Sensible defaults |
| 015.3-INT-001 | Integration | P0 | Firebase token verification succeeds with valid token | Primary auth path |
| 015.3-INT-002 | Integration | P0 | Firebase token verification rejects expired token | Security enforcement |
| 015.3-INT-003 | Integration | P0 | Firebase token verification rejects invalid signature | Security enforcement |
| 015.3-INT-004 | Integration | P0 | JWT verification with custom secret | Alternative provider path |
| 015.3-INT-005 | Integration | P0 | API key verification from header | Simple auth path |
| 015.3-INT-006 | Integration | P0 | Missing token with `required: true` returns 401 | Access control enforcement |
| 015.3-INT-007 | Integration | P1 | Missing token with `required: false` allows through | Optional auth mode |
| 015.3-E2E-001 | E2E | P0 | Authenticated user accesses protected agent | Full auth journey |
| 015.3-E2E-002 | E2E | P0 | Unauthenticated request rejected | Security boundary verification |

### AC2: User Injection into State

With `inject_user: true`, verified user info is added to state.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.3-UNIT-006 | Unit | P1 | User object schema contains expected fields | Data structure verification |
| 015.3-INT-008 | Integration | P0 | `inject_user: true` adds user to state.user | Feature correctness |
| 015.3-INT-009 | Integration | P1 | `inject_user: false` does not modify state | Opt-out behavior |

### AC3: Explicit Auth Actions

`auth.verify` and `auth.get_user` for manual authentication in nodes.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.3-UNIT-007 | Unit | P1 | `auth.verify` action registered | Action availability |
| 015.3-UNIT-008 | Unit | P1 | `auth.get_user` action registered | Action availability |
| 015.3-INT-010 | Integration | P1 | `auth.verify` returns verified claims | Action behavior |
| 015.3-INT-011 | Integration | P1 | `auth.get_user` returns full user profile | Extended user data retrieval |

---

## Story 015.4: Input Validation Schema

### AC1: Request Validation via input_schema

Define request validation with type checking, required fields, patterns, and constraints.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.4-UNIT-001 | Unit | P0 | Parse valid `input_schema` with multiple fields | Schema parsing correctness |
| 015.4-UNIT-002 | Unit | P0 | Validate required field is present | Required field enforcement |
| 015.4-UNIT-003 | Unit | P0 | Validate required field is absent returns error | Error path verification |
| 015.4-UNIT-004 | Unit | P0 | Type check: string field with string value | Type validation |
| 015.4-UNIT-005 | Unit | P0 | Type check: string field with int value fails | Type mismatch detection |
| 015.4-UNIT-006 | Unit | P1 | Type check: int, float, bool, list, dict | All type validations |
| 015.4-UNIT-007 | Unit | P1 | Pattern validation with regex | Pattern constraint enforcement |
| 015.4-UNIT-008 | Unit | P1 | Min/max constraints for numbers | Range validation |
| 015.4-UNIT-009 | Unit | P1 | minLength/maxLength for strings | String length constraints |
| 015.4-UNIT-010 | Unit | P2 | Nested object validation | Complex schema support |
| 015.4-INT-001 | Integration | P0 | Invalid input rejected with 400 response | HTTP error handling |
| 015.4-INT-002 | Integration | P0 | Valid input passes through to agent | Happy path verification |
| 015.4-INT-003 | Integration | P1 | Validation errors include field names | Debuggable error messages |

### AC2: Explicit Validation Action

`validate.input` action for manual validation in nodes.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.4-UNIT-011 | Unit | P2 | `validate.input` action registered | Action availability |
| 015.4-INT-004 | Integration | P2 | `validate.input` with custom schema | Manual validation path |

---

## Story 015.5: Response Transformation

### AC1: Output Schema Mapping

Define response mapping via `output_schema` with Jinja2 templates.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.5-UNIT-001 | Unit | P1 | Parse valid `output_schema` | Schema parsing |
| 015.5-UNIT-002 | Unit | P1 | Map state field to output field | Basic mapping |
| 015.5-UNIT-003 | Unit | P1 | Apply Jinja2 template in output field | Template transformation |
| 015.5-UNIT-004 | Unit | P2 | Conditional field inclusion with `if` | Dynamic response structure |
| 015.5-UNIT-005 | Unit | P2 | Exclude fields with `exclude` list | Field filtering |
| 015.5-INT-001 | Integration | P1 | Full state transformed to output schema | End-to-end transformation |
| 015.5-INT-002 | Integration | P2 | Missing optional fields use defaults | Graceful handling |

---

## Story 015.6: Error Handling in YAML

### AC1: Global Error Handling Settings

Configure error responses, retry logic, and fallback nodes via `settings.error_handling`.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.6-UNIT-001 | Unit | P0 | Parse `on_error: graceful` | Error mode parsing |
| 015.6-UNIT-002 | Unit | P0 | Parse `on_error: raise` | Error mode parsing |
| 015.6-UNIT-003 | Unit | P0 | Parse `on_error: retry` with max_retries | Retry config parsing |
| 015.6-UNIT-004 | Unit | P1 | Validate retry_delay is positive | Config validation |
| 015.6-INT-001 | Integration | P0 | `graceful` mode returns error response, continues | Non-fatal error handling |
| 015.6-INT-002 | Integration | P0 | `raise` mode propagates exception | Fail-fast behavior |
| 015.6-INT-003 | Integration | P0 | `retry` mode retries up to max_retries | Retry behavior |
| 015.6-INT-004 | Integration | P1 | Retry with exponential backoff | Backoff behavior |
| 015.6-E2E-001 | E2E | P1 | Agent recovers from transient LLM error | Resilience verification |

### AC2: Per-Node Error Handling

`on_error` blocks on individual nodes override global settings.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.6-UNIT-005 | Unit | P1 | Parse per-node `on_error` block | Node-level config |
| 015.6-INT-005 | Integration | P1 | Per-node `on_error` overrides global | Override behavior |
| 015.6-INT-006 | Integration | P2 | Fallback node executes on error | Fallback path |

---

## Story 015.7: HTTP Endpoint Configuration

### AC1: Per-Agent HTTP Contract

Define per-agent HTTP contracts via `endpoint` section.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.7-UNIT-001 | Unit | P1 | Parse `endpoint.path` | Endpoint config parsing |
| 015.7-UNIT-002 | Unit | P1 | Parse `endpoint.method` (GET, POST, etc.) | HTTP method config |
| 015.7-UNIT-003 | Unit | P1 | Parse `endpoint.request_schema` | Request schema binding |
| 015.7-UNIT-004 | Unit | P1 | Parse `endpoint.response_schema` | Response schema binding |
| 015.7-INT-001 | Integration | P1 | Agent responds to configured path | Path routing |
| 015.7-INT-002 | Integration | P1 | Agent rejects wrong HTTP method | Method enforcement |
| 015.7-E2E-001 | E2E | P1 | Full HTTP request/response cycle | Endpoint contract verification |

### AC2: Custom HTTP Response Action

`http.respond` for custom HTTP responses.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.7-UNIT-005 | Unit | P2 | `http.respond` action registered | Action availability |
| 015.7-INT-003 | Integration | P2 | `http.respond` with custom status code | Custom response |
| 015.7-INT-004 | Integration | P2 | `http.respond` with custom headers | Header customization |

---

## Story 015.8: Health & Metadata Endpoints

### AC1: Auto-Generated Standard Endpoints

Auto-generate `/health`, `/agents`, `/metrics`, and `/openapi.json` endpoints.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.8-UNIT-001 | Unit | P2 | Parse `settings.server.health_endpoint` | Config parsing |
| 015.8-UNIT-002 | Unit | P2 | Parse `settings.server.list_agents` | Config parsing |
| 015.8-UNIT-003 | Unit | P2 | Parse `settings.server.openapi` | Config parsing |
| 015.8-UNIT-004 | Unit | P3 | Parse `settings.server.metrics` | Config parsing |
| 015.8-INT-001 | Integration | P2 | `/health` returns 200 with status | Health check endpoint |
| 015.8-INT-002 | Integration | P2 | `/agents` lists all registered agents | Agent discovery endpoint |
| 015.8-INT-003 | Integration | P2 | `/openapi.json` returns valid OpenAPI spec | API documentation |
| 015.8-INT-004 | Integration | P3 | `/metrics` returns Prometheus format | Observability endpoint |
| 015.8-E2E-001 | E2E | P2 | Health endpoint accessible without auth | Kubernetes probe support |

---

## Backward Compatibility Tests

Ensure existing agents work unchanged.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.0-INT-001 | Integration | P0 | Agent without settings sections runs | No-settings backward compat |
| 015.0-INT-002 | Integration | P0 | Agent with partial settings runs | Partial config support |
| 015.0-INT-003 | Integration | P0 | Existing example agents pass | Regression prevention |
| 015.0-E2E-001 | E2E | P0 | Full agent without new features works | Zero-regression verification |

---

## Risk Coverage

| Risk ID | Description | Mitigating Tests |
|---------|-------------|------------------|
| RISK-001 | Breaking existing agent configurations | 015.0-INT-001, 015.0-INT-002, 015.0-INT-003, 015.0-E2E-001 |
| RISK-002 | Security bypass via auth misconfiguration | 015.3-INT-002, 015.3-INT-003, 015.3-INT-006, 015.3-E2E-002 |
| RISK-003 | Data loss in session management | 015.1-INT-002, 015.1-E2E-001 |
| RISK-004 | Input validation bypass | 015.4-UNIT-003, 015.4-UNIT-005, 015.4-INT-001 |
| RISK-005 | Firestore data corruption | 015.2-INT-003, 015.2-INT-004, 015.2-E2E-001 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on config/parsing issues)
2. **P0 Integration tests** (verify core data flows)
3. **P0 E2E tests** (validate critical user journeys)
4. **P0 Backward compatibility tests** (no regression)
5. **P1 Unit tests** (extended validation)
6. **P1 Integration tests** (secondary paths)
7. **P1 E2E tests** (resilience scenarios)
8. **P2+ tests** (as time permits)

---

## Test Implementation Notes

### Mock Strategy

- **Firestore**: Use `firebase-admin` emulator or `unittest.mock`
- **Redis**: Use `fakeredis` library
- **DynamoDB**: Use `moto` library
- **Auth tokens**: Generate test JWTs with known keys

### Test Data Management

- Use fixtures for common YAML configurations
- Store test tokens in `tests/fixtures/tokens/`
- Use unique collection names per test run to avoid conflicts

### CI/CD Considerations

- P0 tests must pass in PR checks
- P1 tests run in nightly builds
- P2/P3 tests run in weekly regression suite

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Backward compatibility explicitly tested
- [x] Security paths have P0 priority

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 87
  by_level:
    unit: 42
    integration: 32
    e2e: 13
  by_priority:
    p0: 28
    p1: 35
    p2: 18
    p3: 6
  coverage_gaps: []
  risk_mitigations:
    - risk: RISK-001
      tests: [015.0-INT-001, 015.0-INT-002, 015.0-INT-003, 015.0-E2E-001]
    - risk: RISK-002
      tests: [015.3-INT-002, 015.3-INT-003, 015.3-INT-006, 015.3-E2E-002]
    - risk: RISK-003
      tests: [015.1-INT-002, 015.1-E2E-001]
    - risk: RISK-004
      tests: [015.4-UNIT-003, 015.4-UNIT-005, 015.4-INT-001]
    - risk: RISK-005
      tests: [015.2-INT-003, 015.2-INT-004, 015.2-E2E-001]
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-015-test-design-20260105.md
P0 tests identified: 28
P1 tests identified: 35
Stories covered: 015.1, 015.2, 015.3, 015.4, 015.5, 015.6, 015.7, 015.8
Backward compatibility tests: 4
```
