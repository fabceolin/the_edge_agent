# Story TEA-BUILTIN-015.3: Auth Middleware in YAML

## Status: Ready for Development

## Story

**As a** TEA agent developer,
**I want** to configure authentication via YAML settings,
**so that** I can secure agent endpoints without writing Python authentication code.

## Acceptance Criteria

1. **AC1: Settings Schema** - `settings.auth` section configures provider, token header, and behavior
2. **AC2: Firebase Auth Provider** - Support Firebase ID token verification via `firebase-admin`
3. **AC3: JWT Provider** - Support generic JWT verification with configurable secret/public key
4. **AC4: API Key Provider** - Support simple API key authentication from header or query param
5. **AC5: User Injection** - Authenticated user info automatically injected into state as `__user__`
6. **AC6: Claims Mapping** - Configurable mapping of token claims to state fields
7. **AC7: Required vs Optional** - Auth can be required (401 on failure) or optional (null user)
8. **AC8: Auth Actions** - `auth.verify` and `auth.get_user` actions for explicit verification
9. **AC9: Backward Compatible** - Agents without `settings.auth` work unchanged (no auth required)

## Tasks / Subtasks

- [ ] **Task 1: Define Auth Settings Schema** (AC1)
  - [ ] Create `AuthSettings` Pydantic model
  - [ ] Add `auth` field to main Settings model
  - [ ] Define provider enum: `firebase`, `jwt`, `api_key`, `none`
  - [ ] Define token source options: header name, query param

- [ ] **Task 2: Implement Auth Provider Protocol** (AC2, AC3, AC4)
  - [ ] Create `AuthProvider` abstract base class
  - [ ] Implement `FirebaseAuthProvider` using `firebase-admin`
  - [ ] Implement `JWTAuthProvider` using PyJWT
  - [ ] Implement `APIKeyAuthProvider` for simple key matching
  - [ ] Add provider factory function

- [ ] **Task 3: Implement User Injection** (AC5, AC6)
  - [ ] Add auth verification hook in YAMLEngine initialization
  - [ ] Extract user info from verified token
  - [ ] Apply claims mapping configuration
  - [ ] Inject as `__user__` in initial state

- [ ] **Task 4: Implement Required vs Optional Auth** (AC7)
  - [ ] When `required: true`, raise 401 if token invalid/missing
  - [ ] When `required: false`, set `__user__` to None and continue
  - [ ] Support per-endpoint override in future (Endpoint Config story)

- [ ] **Task 5: Implement Auth Actions** (AC8)
  - [ ] `auth.verify` - Explicit token verification within agent flow
  - [ ] `auth.get_user` - Fetch full user profile from provider
  - [ ] Register actions in built-in registry

- [ ] **Task 6: Write Tests** (AC1-AC9)
  - [ ] Unit tests for each auth provider
  - [ ] Integration tests with Firebase Auth emulator
  - [ ] Test user injection and claims mapping
  - [ ] Test required vs optional behavior
  - [ ] Regression test: agent without auth settings

- [ ] **Task 7: Documentation**
  - [ ] Update `docs/shared/YAML_REFERENCE.md` with auth settings
  - [ ] Update `docs/python/actions-reference.md` with auth actions
  - [ ] Add security best practices section

## Dev Notes

### Settings Schema

```yaml
settings:
  auth:
    provider: firebase         # Required: firebase | jwt | api_key | none
    token_header: "X-Firebase-Token"  # Header containing token
    # Alternative: token from query param
    # token_query_param: "api_key"
    required: true             # 401 if auth fails (default: true)
    inject_user: true          # Inject user into state (default: true)
    user_state_key: "__user__" # State key for user info (default: __user__)

    # Claims mapping (optional)
    claims_mapping:
      user_id: uid             # state.__user__.user_id = token.uid
      user_email: email
      user_name: name
      roles: custom_claims.roles

    # Provider-specific config
    firebase:
      project_id: "${FIREBASE_PROJECT_ID}"

    jwt:
      secret: "${JWT_SECRET}"
      # Or use public key for RS256
      # public_key_path: "/path/to/public.pem"
      algorithms: ["HS256"]
      issuer: "https://your-issuer.com"

    api_key:
      keys:
        - "${API_KEY_1}"
        - "${API_KEY_2}"
      # Or load from file
      # keys_file: "/path/to/keys.txt"
```

### User Info Structure

```python
# Injected into state as __user__
{
    "uid": "abc123",           # Always present
    "email": "user@example.com",
    "name": "John Doe",        # If available
    "provider": "firebase",    # Auth provider used
    "claims": {                # Raw token claims
        "custom_claims": {...}
    },
    # Mapped fields from claims_mapping
    "user_id": "abc123",
    "roles": ["admin", "user"]
}
```

### Action Signatures

```yaml
# Explicit verification (when inject_user is false)
- name: verify_token
  uses: auth.verify
  with:
    token: "{{ state.custom_token_field }}"
  output: auth_result

# Get full user profile
- name: get_profile
  uses: auth.get_user
  with:
    uid: "{{ state.__user__.uid }}"
  output: user_profile
```

### Module Structure (Files to Create)

```
python/src/the_edge_agent/
├── auth/                           # NEW MODULE
│   ├── __init__.py                 # Exports: create_auth_provider, AuthProvider
│   ├── base.py                     # AuthProvider ABC + UserInfo model
│   ├── firebase_provider.py        # FirebaseAuthProvider
│   ├── jwt_provider.py             # JWTAuthProvider
│   └── apikey_provider.py          # APIKeyAuthProvider
│
├── actions/
│   └── auth_actions.py             # NEW: auth.verify, auth.get_user actions
│
├── settings.py                     # MODIFY: Add AuthSettings model
└── yaml_engine.py                  # MODIFY: Add _init_auth() hook
```

### File Contents Overview

**auth/__init__.py:**
```python
from .base import AuthProvider, UserInfo, AuthResult
from .firebase_provider import FirebaseAuthProvider
from .jwt_provider import JWTAuthProvider
from .apikey_provider import APIKeyAuthProvider

def create_auth_provider(config: dict) -> AuthProvider:
    """Factory function for auth providers."""
    provider_type = config.get("provider", "none")
    if provider_type == "firebase":
        return FirebaseAuthProvider(
            project_id=config.get("firebase", {}).get("project_id")
        )
    elif provider_type == "jwt":
        return JWTAuthProvider(
            secret=config.get("jwt", {}).get("secret"),
            algorithms=config.get("jwt", {}).get("algorithms", ["HS256"])
        )
    elif provider_type == "api_key":
        return APIKeyAuthProvider(
            keys=config.get("api_key", {}).get("keys", [])
        )
    elif provider_type == "none":
        return None
    raise ValueError(f"Unknown auth provider: {provider_type}")
```

**auth/base.py:**
```python
from abc import ABC, abstractmethod
from typing import Optional, Dict, Any
from pydantic import BaseModel

class UserInfo(BaseModel):
    uid: str
    email: Optional[str] = None
    name: Optional[str] = None
    provider: str
    claims: Dict[str, Any] = {}

class AuthResult(BaseModel):
    success: bool
    user: Optional[UserInfo] = None
    error: Optional[str] = None

class AuthProvider(ABC):
    @abstractmethod
    async def verify_token(self, token: str) -> AuthResult:
        """Verify token and return user info."""
        pass

    @abstractmethod
    async def get_user(self, uid: str) -> Optional[UserInfo]:
        """Get full user profile by UID."""
        pass
```

**auth/firebase_provider.py:**
```python
from typing import Optional
from .base import AuthProvider, AuthResult, UserInfo

class FirebaseAuthProvider(AuthProvider):
    def __init__(self, project_id: Optional[str] = None):
        self._project_id = project_id

    async def verify_token(self, token: str) -> AuthResult:
        try:
            from firebase_admin import auth
            decoded = auth.verify_id_token(token)
            return AuthResult(
                success=True,
                user=UserInfo(
                    uid=decoded["uid"],
                    email=decoded.get("email"),
                    name=decoded.get("name"),
                    provider="firebase",
                    claims=decoded
                )
            )
        except Exception as e:
            return AuthResult(success=False, error=str(e))

    async def get_user(self, uid: str) -> Optional[UserInfo]:
        from firebase_admin import auth
        user = auth.get_user(uid)
        return UserInfo(
            uid=user.uid,
            email=user.email,
            name=user.display_name,
            provider="firebase",
            claims=user.custom_claims or {}
        )
```

### Relevant Existing Files (Minimal Changes)

- `python/src/the_edge_agent/settings.py` - Add `AuthSettings` Pydantic model
- `python/src/the_edge_agent/yaml_engine.py` - Add `_init_auth()` hook (~15 lines)
- `python/src/the_edge_agent/actions/__init__.py` - Register auth actions

### Dependencies

- `firebase-admin>=6.2.0` (optional, for Firebase provider)
- `PyJWT>=2.8.0` (optional, for JWT provider)

### Testing

**Test file location:** `python/tests/test_auth_providers.py`

**Testing standards:**
- Mock token verification for unit tests
- Use Firebase Auth emulator for integration tests
- Test each provider independently
- Test claims mapping variations
- Test required vs optional auth
- Minimum 90% coverage

**Security testing:**
- Invalid token rejection
- Expired token handling
- Token tampering detection
- Missing token handling

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

### Test Design Review - 2026-01-05

**Reviewer:** Quinn (Test Architect)

#### Test Coverage Summary

| Metric | Value |
|--------|-------|
| **Total Scenarios** | 42 |
| **Unit Tests** | 22 (52%) |
| **Integration Tests** | 14 (33%) |
| **E2E Tests** | 6 (14%) |
| **Security Tests** | 5 |
| **Backward Compatibility Tests** | 2 |

**Coverage by Priority:**
- P0 (Critical): 18 scenarios
- P1 (High): 14 scenarios
- P2 (Medium): 8 scenarios
- P3 (Low): 2 scenarios

**All 9 Acceptance Criteria have test coverage.**

#### Risk Areas Identified

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Invalid token accepted | Medium | **Critical** | Unit tests for each provider's rejection logic |
| Expired token accepted | Medium | High | Explicit expiration tests per provider |
| Auth bypass | Low | **Critical** | Integration tests for required auth enforcement |
| Backward compatibility break | Medium | High | Regression tests for agents without auth |
| Secret exposure in logs | Low | **Critical** | Security test for error message sanitization |
| Algorithm confusion (JWT alg:none) | Low | **Critical** | Explicit security test 015.3-SEC-002 |

#### Recommended Test Scenarios (Key P0 Tests)

1. **Token Verification** - Each provider must reject invalid, expired, and tampered tokens
2. **Required Auth Enforcement** - 401 on missing/invalid token when `required: true`
3. **Optional Auth Behavior** - `__user__` set to None when `required: false` and no token
4. **User Injection** - `__user__` contains uid, email, name, provider, claims
5. **Backward Compatibility** - Existing agents without auth settings work unchanged
6. **Security** - Token tampering detection, alg:none prevention, secrets not logged

#### Concerns and Blockers

**Concerns (Non-blocking):**
1. **Firebase Emulator Dependency** - Integration tests require Firebase Auth emulator setup; CI pipeline must provision this
2. **Optional Dependencies** - `firebase-admin` and `PyJWT` are optional; tests must handle import failures gracefully
3. **Rate Limiting** - Story mentions hook point for rate limiting on auth failures but no implementation detail; recommend follow-up story

**Blockers:** None identified

#### Test Environment Notes

- Unit tests: Mocked providers, no external dependencies
- Integration tests: Firebase Auth emulator at `localhost:9099`
- E2E tests: Full YAML agent execution with auth configuration
- Test fixtures needed: Valid/expired/invalid tokens for each provider type

#### Gate Recommendation

**READY FOR DEVELOPMENT** - Test design is comprehensive with appropriate security focus. All acceptance criteria have corresponding test scenarios. Recommended 90%+ unit test coverage for auth providers.

---

*Reference: [Test Design Document](../qa/assessments/TEA-BUILTIN-015.3-test-design-20260105.md)*
