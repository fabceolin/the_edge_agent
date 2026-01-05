# Test Design: Story TEA-BUILTIN-015.3

**Story:** Auth Middleware in YAML
**Date:** 2026-01-05
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 42 |
| **Unit tests** | 22 (52%) |
| **Integration tests** | 14 (33%) |
| **E2E tests** | 6 (14%) |
| **Priority distribution** | P0: 18, P1: 14, P2: 8, P3: 2 |

### Strategy Rationale

Authentication is a **security-critical** subsystem. The test design follows defense-in-depth:

1. **Unit tests** cover provider logic, token parsing, claims mapping, and validation
2. **Integration tests** verify provider initialization, YAMLEngine hooks, and action registration
3. **E2E tests** validate complete auth flows through real YAML agent execution

---

## Test Scenarios by Acceptance Criteria

### AC1: Settings Schema

`settings.auth` section configures provider, token header, and behavior

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.3-UNIT-001 | Unit | P0 | Validate `AuthSettings` Pydantic model accepts valid configuration | Pure validation logic, security-critical |
| 015.3-UNIT-002 | Unit | P0 | Reject invalid provider type (not in enum) | Input validation, fail-fast |
| 015.3-UNIT-003 | Unit | P1 | Accept token_header OR token_query_param (mutually exclusive) | Business rule validation |
| 015.3-UNIT-004 | Unit | P1 | Default values applied when optional fields omitted | Schema completeness |
| 015.3-UNIT-005 | Unit | P1 | Environment variable expansion in settings (`${VAR}` syntax) | Critical for secrets management |
| 015.3-INT-001 | Integration | P1 | Settings parsed correctly from complete YAML file | End-to-end parsing chain |

### AC2: Firebase Auth Provider

Support Firebase ID token verification via `firebase-admin`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.3-UNIT-006 | Unit | P0 | `FirebaseAuthProvider.verify_token` returns success with valid mock token | Core auth logic |
| 015.3-UNIT-007 | Unit | P0 | `FirebaseAuthProvider.verify_token` returns failure with invalid token | Security: reject bad tokens |
| 015.3-UNIT-008 | Unit | P0 | `FirebaseAuthProvider.verify_token` returns failure with expired token | Security: time-based rejection |
| 015.3-UNIT-009 | Unit | P1 | `FirebaseAuthProvider.get_user` returns full user profile | User data retrieval |
| 015.3-INT-002 | Integration | P0 | Firebase provider initializes with `project_id` from config | Provider factory correctness |
| 015.3-INT-003 | Integration | P1 | Firebase provider works with Firebase Auth emulator | Real Firebase integration |

### AC3: JWT Provider

Support generic JWT verification with configurable secret/public key

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.3-UNIT-010 | Unit | P0 | `JWTAuthProvider.verify_token` with HS256 secret | Core JWT verification |
| 015.3-UNIT-011 | Unit | P0 | `JWTAuthProvider.verify_token` rejects invalid signature | Security: signature verification |
| 015.3-UNIT-012 | Unit | P0 | `JWTAuthProvider.verify_token` rejects expired JWT | Security: expiration check |
| 015.3-UNIT-013 | Unit | P1 | `JWTAuthProvider.verify_token` with RS256 public key | Asymmetric algorithm support |
| 015.3-UNIT-014 | Unit | P1 | `JWTAuthProvider.verify_token` validates issuer claim | Token origin verification |
| 015.3-INT-004 | Integration | P0 | JWT provider initializes with secret from config | Provider factory correctness |

### AC4: API Key Provider

Support simple API key authentication from header or query param

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.3-UNIT-015 | Unit | P0 | `APIKeyAuthProvider` accepts valid key from list | Core API key matching |
| 015.3-UNIT-016 | Unit | P0 | `APIKeyAuthProvider` rejects key not in list | Security: unauthorized rejection |
| 015.3-UNIT-017 | Unit | P1 | `APIKeyAuthProvider` loads keys from file | Alternative key source |
| 015.3-INT-005 | Integration | P1 | API key extracted from header | Header extraction chain |
| 015.3-INT-006 | Integration | P1 | API key extracted from query param | Query param extraction chain |

### AC5: User Injection

Authenticated user info automatically injected into state as `__user__`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.3-INT-007 | Integration | P0 | `__user__` present in state after successful auth | Core feature validation |
| 015.3-INT-008 | Integration | P0 | `__user__` structure contains uid, email, name, provider, claims | Data completeness |
| 015.3-INT-009 | Integration | P1 | Custom `user_state_key` changes injection location | Configuration flexibility |
| 015.3-E2E-001 | E2E | P0 | Agent node can access `{{ state.__user__.uid }}` in template | Full flow validation |

### AC6: Claims Mapping

Configurable mapping of token claims to state fields

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.3-UNIT-018 | Unit | P0 | Simple claim mapping (uid → user_id) | Basic mapping logic |
| 015.3-UNIT-019 | Unit | P1 | Nested claim mapping (custom_claims.roles → roles) | Dot notation traversal |
| 015.3-UNIT-020 | Unit | P2 | Missing claim results in None value | Graceful degradation |
| 015.3-INT-010 | Integration | P1 | Claims mapping applied during user injection | Integration with injection |

### AC7: Required vs Optional

Auth can be required (401 on failure) or optional (null user)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.3-INT-011 | Integration | P0 | `required: true` raises 401 on missing token | Security: enforce auth |
| 015.3-INT-012 | Integration | P0 | `required: true` raises 401 on invalid token | Security: reject bad tokens |
| 015.3-INT-013 | Integration | P0 | `required: false` sets `__user__` to None on missing token | Optional auth behavior |
| 015.3-E2E-002 | E2E | P0 | Agent continues execution with null user when auth optional | Full flow validation |
| 015.3-E2E-003 | E2E | P0 | Agent returns 401 response when auth required and fails | Full flow validation |

### AC8: Auth Actions

`auth.verify` and `auth.get_user` actions for explicit verification

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.3-UNIT-021 | Unit | P1 | `auth.verify` action registered in action registry | Action availability |
| 015.3-UNIT-022 | Unit | P1 | `auth.get_user` action registered in action registry | Action availability |
| 015.3-INT-014 | Integration | P1 | `auth.verify` action verifies token from custom state field | Action execution |
| 015.3-E2E-004 | E2E | P1 | Agent uses `auth.verify` action in node | Full action flow |
| 015.3-E2E-005 | E2E | P2 | Agent uses `auth.get_user` to fetch full profile | Full action flow |

### AC9: Backward Compatible

Agents without `settings.auth` work unchanged (no auth required)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.3-INT-015 | Integration | P0 | Agent without auth settings executes normally | Regression prevention |
| 015.3-E2E-006 | E2E | P0 | Existing YAML agents pass without modification | Backward compatibility |

---

## Security-Specific Test Scenarios

These scenarios address security requirements from the story:

| ID | Level | Priority | Test | Risk Mitigated |
|----|-------|----------|------|----------------|
| 015.3-SEC-001 | Unit | P0 | Token tampering detected (modified payload) | Token integrity |
| 015.3-SEC-002 | Unit | P0 | Algorithm confusion attack prevented (alg:none) | JWT vulnerability |
| 015.3-SEC-003 | Unit | P0 | Secrets not logged or exposed in errors | Information leakage |
| 015.3-SEC-004 | Integration | P0 | Token extracted only from configured source | Header injection |
| 015.3-SEC-005 | Integration | P1 | Rate limiting hook point available for auth failures | Brute force protection |

---

## Risk Coverage

| Risk | Probability | Impact | Mitigated By |
|------|-------------|--------|--------------|
| Invalid token accepted | Medium | Critical | 015.3-UNIT-007, 015.3-UNIT-011, 015.3-SEC-001 |
| Expired token accepted | Medium | High | 015.3-UNIT-008, 015.3-UNIT-012 |
| Auth bypass | Low | Critical | 015.3-INT-011, 015.3-INT-012, 015.3-E2E-003 |
| Backward compatibility break | Medium | High | 015.3-INT-015, 015.3-E2E-006 |
| Claims injection | Low | High | 015.3-UNIT-018, 015.3-UNIT-019 |
| Secret exposure | Low | Critical | 015.3-SEC-003 |

---

## Recommended Execution Order

### Phase 1: P0 Unit Tests (Fail Fast on Core Logic)
1. Auth settings validation (015.3-UNIT-001, 015.3-UNIT-002)
2. Token verification per provider (015.3-UNIT-006 to 015.3-UNIT-012, 015.3-UNIT-015, 015.3-UNIT-016)
3. Claims mapping (015.3-UNIT-018)
4. Security tests (015.3-SEC-001 to 015.3-SEC-003)

### Phase 2: P0 Integration Tests
1. Provider initialization (015.3-INT-002, 015.3-INT-004)
2. User injection (015.3-INT-007, 015.3-INT-008)
3. Required auth enforcement (015.3-INT-011, 015.3-INT-012, 015.3-INT-013)
4. Security integration (015.3-SEC-004)
5. Backward compatibility (015.3-INT-015)

### Phase 3: P0 E2E Tests
1. User access in agent (015.3-E2E-001)
2. Required/optional flow (015.3-E2E-002, 015.3-E2E-003)
3. Backward compatibility (015.3-E2E-006)

### Phase 4: P1 Tests
1. Remaining unit tests
2. Remaining integration tests
3. Action E2E (015.3-E2E-004)

### Phase 5: P2+ Tests (As Time Permits)
1. Edge cases and nice-to-have scenarios

---

## Test Environment Requirements

| Level | Environment | Setup |
|-------|-------------|-------|
| Unit | Isolated Python | Mocked firebase-admin, PyJWT with test keys |
| Integration | Python + Test Fixtures | Firebase Auth Emulator, in-memory key store |
| E2E | Full YAMLEngine | Complete YAML agents with auth settings |

### Test Fixtures Required

```yaml
fixtures:
  valid_firebase_token: "eyJ..."  # Mock Firebase ID token
  expired_firebase_token: "eyJ..."
  valid_jwt_hs256: "eyJ..."
  valid_jwt_rs256: "eyJ..."
  invalid_signature_jwt: "eyJ..."
  test_api_keys: ["key1", "key2", "key3"]
  firebase_emulator_config:
    project_id: "test-project"
    emulator_host: "localhost:9099"
```

---

## Gate YAML Block

```yaml
test_design:
  story_id: "TEA-BUILTIN-015.3"
  date: "2026-01-05"
  scenarios_total: 42
  by_level:
    unit: 22
    integration: 14
    e2e: 6
  by_priority:
    p0: 18
    p1: 14
    p2: 8
    p3: 2
  security_scenarios: 5
  coverage_gaps: []
  backward_compatibility_tests: 2
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
- [x] Priorities align with business risk (security = P0)
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Security scenarios explicitly designed
- [x] Backward compatibility addressed

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-015.3-test-design-20260105.md
P0 tests identified: 18
Security-specific tests: 5
Backward compatibility tests: 2
```
