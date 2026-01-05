# TEA-BUILTIN-015: Cloud Production YAML-First Epic

## Status: Ready for Development

## Epic Goal

Enable **zero-code cloud deployment** of TEA agents by moving authentication, session management, database operations, and HTTP configuration from Python code into declarative YAML settings and built-in actions. This reduces the Python wrapper from ~80 lines to ~10 lines while increasing YAML expressiveness.

## Epic Description

### Existing System Context

- **Current functionality**: TEA YAMLEngine supports YAML-based agent definitions with built-in actions for LLM calls, web search, and cloud memory
- **Technology stack**: Python 3.10+, YAMLEngine, Jinja2 templates, built-in actions registry
- **Integration points**: Firebase Cloud Functions, Firestore, Firebase Auth, Cloud Storage (GCS/S3)

### Enhancement Details

**What's being added:**

A comprehensive set of YAML settings and built-in actions that eliminate boilerplate Python code for cloud deployments:

1. **Session Management** - Declarative session persistence with multiple backends
2. **Firestore Actions** - Built-in CRUD operations for Firestore
3. **Auth Middleware** - Token verification configurable in YAML
4. **Input Validation** - Schema-based request validation in YAML
5. **Response Transformation** - Output schema mapping in YAML
6. **Error Handling** - Declarative error responses and retry logic
7. **Endpoint Configuration** - Per-agent HTTP contract in YAML
8. **Health/Metadata Endpoints** - Auto-generated standard endpoints

**How it integrates:**

- New `settings.*` sections in YAML for configuration
- New built-in actions registered in the actions registry
- Backward compatible - existing agents work unchanged

**Success criteria:**

- Python wrapper reduced to <15 lines (bootstrap only)
- All 8 capabilities configurable via YAML
- Full test coverage for new actions
- Documentation with examples

## Stories

| # | Story ID | Title | Priority | Complexity |
|---|----------|-------|----------|------------|
| 1 | TEA-BUILTIN-015.1 | Session Management in YAML | P1 | Medium |
| 2 | TEA-BUILTIN-015.2 | Firestore Built-in Actions | P1 | Medium |
| 3 | TEA-BUILTIN-015.3 | Auth Middleware in YAML | P2 | Low |
| 4 | TEA-BUILTIN-015.4 | Input Validation Schema | P2 | Low |
| 5 | TEA-BUILTIN-015.5 | Response Transformation | P2 | Low |
| 6 | TEA-BUILTIN-015.6 | Error Handling in YAML | P3 | Medium |
| 7 | TEA-BUILTIN-015.7 | HTTP Endpoint Configuration | P3 | Medium |
| 8 | TEA-BUILTIN-015.8 | Health & Metadata Endpoints | P3 | Low |

### Story Descriptions

**1. TEA-BUILTIN-015.1: Session Management in YAML**
Configure session persistence via `settings.session` with support for Firestore, Redis, DynamoDB, and in-memory backends. Includes `session.load` and `session.save` built-in actions.

**2. TEA-BUILTIN-015.2: Firestore Built-in Actions**
Add `firestore.get`, `firestore.set`, `firestore.query`, and `firestore.delete` actions for direct Firestore operations from YAML agents.

**3. TEA-BUILTIN-015.3: Auth Middleware in YAML**
Configure authentication via `settings.auth` supporting Firebase Auth, JWT, and API key providers with automatic user injection into state.

**4. TEA-BUILTIN-015.4: Input Validation Schema**
Define request validation via `input_schema` in YAML with type checking, required fields, patterns, and constraints.

**5. TEA-BUILTIN-015.5: Response Transformation**
Define response mapping via `output_schema` with Jinja2 templates and conditional field inclusion.

**6. TEA-BUILTIN-015.6: Error Handling in YAML**
Configure error responses, retry logic, and fallback nodes via `settings.error_handling` and per-node `on_error` blocks.

**7. TEA-BUILTIN-015.7: HTTP Endpoint Configuration**
Define per-agent HTTP contracts via `endpoint` section including path, method, and request/response schemas.

**8. TEA-BUILTIN-015.8: Health & Metadata Endpoints**
Auto-generate `/health`, `/agents`, `/metrics`, and `/openapi.json` endpoints via `settings.server` configuration.

## Compatibility Requirements

- [x] Existing YAML agents work unchanged (no breaking changes)
- [x] New settings sections are optional with sensible defaults
- [x] Actions follow existing registry pattern
- [x] Works with Python 3.10+ (no new dependencies required)

## Dependencies

### Internal Dependencies
- TEA-BUILTIN-006: Cloud Memory Actions (foundation for session storage)
- TEA-BUILTIN-012: Secrets Backend (for secure credential access)

### External Dependencies
- `firebase-admin` (optional, for Firestore/Auth backends)
- `redis` (optional, for Redis session backend)

### Story Dependencies
```
TEA-BUILTIN-015.1 (Session) ─┐
                             ├──► TEA-BUILTIN-015.6 (Error Handling)
TEA-BUILTIN-015.2 (Firestore)┘

TEA-BUILTIN-015.3 (Auth) ────► TEA-BUILTIN-015.7 (Endpoint Config)

TEA-BUILTIN-015.4 (Input) ───┬──► TEA-BUILTIN-015.7 (Endpoint Config)
                             │
TEA-BUILTIN-015.5 (Output) ──┘

TEA-BUILTIN-015.8 (Health) ──► Independent (can be done anytime)
```

## Risk Mitigation

- **Primary Risk:** Breaking existing agent configurations
- **Mitigation:** All new settings are optional; defaults preserve current behavior
- **Rollback Plan:** Feature flags for each capability; can disable individually

## Definition of Done

- [ ] All 8 stories completed with acceptance criteria met
- [ ] Existing agent tests pass (no regression)
- [ ] New actions have >90% test coverage
- [ ] YAML Reference documentation updated
- [ ] Article "Cloud Production with Firebase" updated with examples
- [ ] No new required dependencies (optional only)

## Technical Notes

### Settings Schema Overview

```yaml
settings:
  # Story 1: Session Management
  session:
    backend: firestore|redis|dynamodb|memory
    collection: "agent_sessions"
    auto_save: true
    ttl: 3600

  # Story 3: Auth Middleware
  auth:
    provider: firebase|jwt|api_key
    token_header: "X-Firebase-Token"
    inject_user: true
    required: true

  # Story 6: Error Handling
  error_handling:
    on_error: graceful|raise|retry
    max_retries: 3
    retry_delay: 1.0

  # Story 8: Server/Health
  server:
    health_endpoint: true
    list_agents: true
    openapi: true
    metrics: true
```

### Actions Registry Additions

| Action | Story | Description |
|--------|-------|-------------|
| `session.load` | 015.1 | Load session from configured backend |
| `session.save` | 015.1 | Save current state to session |
| `firestore.get` | 015.2 | Get Firestore document |
| `firestore.set` | 015.2 | Set/merge Firestore document |
| `firestore.query` | 015.2 | Query Firestore collection |
| `firestore.delete` | 015.2 | Delete Firestore document |
| `auth.verify` | 015.3 | Verify token explicitly |
| `auth.get_user` | 015.3 | Get full user profile |
| `validate.input` | 015.4 | Explicit input validation |
| `http.respond` | 015.7 | Custom HTTP response |

## QA Notes

**Review Date:** 2026-01-05
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Count |
|--------|-------|
| Total test scenarios | 87 |
| Unit tests | 42 (48%) |
| Integration tests | 32 (37%) |
| E2E tests | 13 (15%) |

**Priority Distribution:**
- P0 (Critical): 28 tests
- P1 (High): 35 tests
- P2 (Medium): 18 tests
- P3 (Low): 6 tests

### Risk Areas Identified

| Risk ID | Description | Severity | Mitigating Tests |
|---------|-------------|----------|------------------|
| RISK-001 | Breaking existing agent configurations | High | 015.0-INT-001, 015.0-INT-002, 015.0-INT-003, 015.0-E2E-001 |
| RISK-002 | Security bypass via auth misconfiguration | Critical | 015.3-INT-002, 015.3-INT-003, 015.3-INT-006, 015.3-E2E-002 |
| RISK-003 | Data loss in session management | High | 015.1-INT-002, 015.1-E2E-001 |
| RISK-004 | Input validation bypass | High | 015.4-UNIT-003, 015.4-UNIT-005, 015.4-INT-001 |
| RISK-005 | Firestore data corruption | Medium | 015.2-INT-003, 015.2-INT-004, 015.2-E2E-001 |

### Recommended Test Scenarios

**Critical Path (P0):**
1. **Session Management**: Load/save round-trip with Firestore backend, multi-turn conversation state persistence
2. **Firestore Actions**: Full CRUD workflow (create, read, update, delete), missing document handling
3. **Auth Middleware**: Firebase token verification (valid/expired/invalid), 401 on missing required token
4. **Input Validation**: Required field enforcement, type mismatch detection, 400 on invalid input
5. **Backward Compatibility**: Existing agents without new settings run unchanged, partial config support

**Secondary Path (P1):**
1. Session TTL expiration, auto_save behavior, Redis/in-memory backends
2. Firestore query with filters, sorting, pagination
3. JWT/API key auth providers, optional auth mode, user injection
4. All type validations, pattern/range constraints, debuggable error messages
5. Per-node error handling override, retry with backoff
6. HTTP endpoint path/method routing

### Concerns and Blockers

**No blocking concerns identified.**

**Advisory notes:**
- All 8 stories have comprehensive test coverage with no gaps identified
- Security paths (auth, validation) are appropriately prioritized at P0
- Backward compatibility is explicitly tested with 4 dedicated scenarios
- Mock strategy documented for external dependencies (Firestore, Redis, DynamoDB)

### Test Implementation Recommendations

1. **Mock Strategy**: Use Firebase emulator, `fakeredis`, and `moto` for external dependencies
2. **CI/CD**: P0 tests in PR checks, P1 in nightly builds, P2/P3 in weekly regression
3. **Test Data**: Use fixtures for YAML configs, unique collection names per test run
4. **Execution Order**: Unit tests first (fail fast), then integration, then E2E

### Quality Gate Status

**PASS** - Epic is ready for development with comprehensive test coverage.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-05 | 1.1 | Added QA Notes section | Quinn (QA) |
| 2025-01-05 | 1.0 | Initial epic creation | Sarah (PO) |
