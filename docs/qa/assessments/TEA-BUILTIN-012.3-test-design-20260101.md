# Test Design: Story TEA-BUILTIN-012.3

**Story Title:** YAML Engine Integration & Actions
**Date:** 2026-01-01
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 27 |
| Unit tests | 11 (41%) |
| Integration tests | 11 (41%) |
| E2E tests | 5 (18%) |
| Priority distribution | P0: 8, P1: 12, P2: 5, P3: 2 |

### Strategy Rationale

This story involves secrets management integration across multiple layers:
1. **YAML configuration parsing** - Unit testable pure logic
2. **Template rendering with secrets** - Integration between Jinja2 and secrets backend
3. **Action execution** - Integration with action registry and state management
4. **CLI flag handling** - Integration of configuration override logic
5. **Checkpoint security** - Critical P0 security validation

Security-critical nature of secrets management elevates base priorities.

---

## Test Scenarios by Acceptance Criteria

### AC-1: YAML settings.secrets creates backend

**Requirement:** GIVEN `settings.secrets.backend: aws` in YAML, WHEN engine initializes, THEN `AWSSecretsBackend` is created with config from `settings.secrets.aws`

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 012.3-UNIT-001 | Unit | P0 | Parse `settings.secrets.backend` field correctly | Pure parsing logic, security-critical |
| 012.3-UNIT-002 | Unit | P1 | Parse provider-specific config (`aws.region`, `aws.secret_name`) | Pure config extraction |
| 012.3-UNIT-003 | Unit | P1 | Return `None` for missing optional fields with defaults | Defensive parsing behavior |
| 012.3-INT-001 | Integration | P0 | `_configure_secrets_backend()` calls factory with parsed config | Component interaction verification |
| 012.3-INT-002 | Integration | P0 | Engine initialization creates correct backend type (AWS, Azure, GCP) | Critical path for all secret access |

**Test Data Variations:**
- AWS backend with `secret_name` (single JSON secret)
- AWS backend with `secret_prefix` (multiple secrets)
- Azure backend with `vault_url`
- GCP backend with `project_id` and `secret_prefix`
- Vault backend with `url`, `token`, `mount`
- Invalid backend type (should raise clear error)

| 012.3-UNIT-011 | Unit | P2 | Parse Vault backend config (`vault.url`, `vault.token`, `vault.mount`) | Vault integration from documentation |

---

### AC-2: Template substitution with secrets

**Requirement:** GIVEN secrets backend configured, WHEN template contains `{{ secrets.API_KEY }}`, THEN the secret value is substituted

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 012.3-UNIT-004 | Unit | P0 | `prepare_context()` includes secrets dict in context | Pure function, security-critical |
| 012.3-INT-003 | Integration | P0 | Jinja2 renders `{{ secrets.KEY }}` with actual value | Template engine + secrets integration |
| 012.3-INT-004 | Integration | P1 | Missing secret key renders as empty string or raises error | Error handling behavior |
| 012.3-INT-005 | Integration | P1 | Nested secret access `{{ secrets.db.password }}` if backend supports | Complex template scenarios |
| 012.3-INT-011 | Integration | P1 | Template substitution in HTTP headers (`Authorization: "Bearer {{ secrets.API_KEY }}"`) | Documented use case for API calls |

**Security Considerations:**
- Verify secrets are not logged during template expansion
- Verify secrets do not appear in error messages

**Documented Pattern Test:**

```yaml
# 012.3-INT-011 - Test from specialized.md documentation
- name: call_api
  uses: http.get
  with:
    url: "{{ variables.api_url }}"
    headers:
      Authorization: "Bearer {{ secrets.API_KEY }}"
```

---

### AC-3: secrets.get action

**Requirement:** GIVEN `secrets.get` action, WHEN called with `key: "api_key"`, THEN the secret value is stored in state

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 012.3-UNIT-005 | Unit | P1 | `secrets_get()` function returns value from backend | Pure action logic |
| 012.3-UNIT-006 | Unit | P1 | `secrets_get()` returns default when key not found | Default value handling |
| 012.3-INT-006 | Integration | P0 | Action execution stores secret in state via `output:` | State management integration |
| 012.3-E2E-001 | E2E | P1 | Full YAML workflow uses `secrets.get` action successfully | End-to-end action execution |

**YAML Example for Testing:**
```yaml
steps:
  - uses: secrets.get
    with:
      key: API_KEY
      default: "fallback_value"
    output: api_key
```

---

### AC-4: secrets.has action

**Requirement:** GIVEN `secrets.has` action, WHEN called with `key: "api_key"`, THEN boolean existence check is stored in state

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 012.3-UNIT-007 | Unit | P1 | `secrets_has()` returns True for existing key | Pure action logic |
| 012.3-UNIT-008 | Unit | P1 | `secrets_has()` returns False for missing key | Edge case handling |
| 012.3-INT-007 | Integration | P1 | Action execution stores boolean in state via `output:` | State management integration |
| 012.3-E2E-005 | E2E | P1 | Conditional logic pattern with `secrets.has` and `condition:` | Documented use case |

**Documented Conditional Pattern:**

```yaml
# 012.3-E2E-005 - Test from specialized.md documentation
- name: check_feature
  uses: secrets.has
  with:
    key: PREMIUM_API_KEY
  output: has_premium

- name: use_premium_api
  condition: "{{ state.has_premium }}"
  uses: http.get
  with:
    url: "{{ variables.premium_api_url }}"
    headers:
      Authorization: "Bearer {{ secrets.PREMIUM_API_KEY }}"
```

---

### AC-5: Secrets excluded from checkpoints

**Requirement:** GIVEN secrets backend, WHEN checkpoint is saved, THEN secrets are NOT included in the serialized state

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 012.3-INT-008 | Integration | P0 | `_secrets_backend` not serialized in checkpoint JSON | **SECURITY CRITICAL** - data leak prevention |
| 012.3-INT-009 | Integration | P0 | State keys from `secrets.get` not persisted if marked transient | Security - secrets in state |
| 012.3-E2E-002 | E2E | P0 | Full workflow with checkpoint: verify file contains no secrets | Defense in depth |

**Critical Test Implementation:**
```python
def test_secrets_not_in_checkpoint(tmp_path):
    """GIVEN secrets backend, WHEN checkpoint saved, THEN secrets not serialized."""
    # 1. Run workflow that accesses secrets
    # 2. Save checkpoint to tmp_path
    # 3. Read checkpoint file as JSON
    # 4. Assert no secret values present
    # 5. Assert _secrets_backend field not serialized
    checkpoint_content = json.loads(checkpoint_file.read_text())
    assert "secret123" not in json.dumps(checkpoint_content)
    assert "_secrets_backend" not in checkpoint_content
```

---

### AC-6: CLI --secrets-backend override

**Requirement:** GIVEN CLI with `--secrets-backend aws`, WHEN running a YAML file, THEN AWS backend is used regardless of YAML settings

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 012.3-UNIT-009 | Unit | P2 | CLI argument parsing extracts secrets flags correctly | Pure CLI parsing |
| 012.3-UNIT-010 | Unit | P2 | Build secrets config dict from CLI flags | Pure config building |
| 012.3-INT-010 | Integration | P1 | CLI flags override YAML `settings.secrets` | Configuration precedence |
| 012.3-E2E-003 | E2E | P2 | Run CLI with `--secrets-backend env` overriding YAML aws config | Full CLI integration |

**CLI Flags to Test:**
- `--secrets-backend {env|aws|azure|gcp|vault}`
- `--aws-region`
- `--aws-secret-name`
- `--azure-vault-url`
- `--gcp-project-id`

---

### AC-7: Fallback to EnvSecretsBackend

**Requirement:** GIVEN no secrets backend configured, WHEN `{{ secrets.KEY }}` is used, THEN `EnvSecretsBackend` is used as fallback

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 012.3-E2E-004 | E2E | P1 | YAML without settings.secrets still accesses env vars via `{{ secrets.X }}` | Backward compatibility |

**Test Implementation:**
```python
def test_fallback_to_env_when_no_backend(monkeypatch):
    """GIVEN no settings.secrets, WHEN template uses secrets, THEN EnvSecretsBackend used."""
    monkeypatch.setenv("MY_SECRET", "env_value")
    yaml_content = """
    nodes:
      - name: check
        run: |
          result = "{{ secrets.MY_SECRET }}"
    """
    engine = YAMLEngine()
    engine.load_yaml(yaml_content)
    assert isinstance(engine._secrets_backend, EnvSecretsBackend)
```

---

## Error Scenarios (Cross-Cutting)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 012.3-ERR-001 | Unit | P1 | Invalid backend type raises `ConfigurationError` | Clear error messaging |
| 012.3-ERR-002 | Integration | P1 | AWS backend with invalid credentials raises clear error | Operational debugging |
| 012.3-ERR-003 | Integration | P2 | Missing required provider config raises validation error | Config validation |
| 012.3-ERR-004 | Unit | P3 | Malformed YAML settings.secrets structure | Defensive parsing |

---

## Risk Coverage Matrix

| Risk | Test IDs | Coverage |
|------|----------|----------|
| Secrets leaked in logs | Template tests + manual review | Partial |
| Secrets in checkpoint | 012.3-INT-008, 012.3-INT-009, 012.3-E2E-002 | Full |
| Config precedence confusion | 012.3-INT-010, 012.3-E2E-003 | Full |
| Backend initialization failures | 012.3-INT-001, 012.3-ERR-002 | Full |
| Missing fallback behavior | 012.3-E2E-004 | Full |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 Critical)
1. 012.3-INT-008 - Checkpoint exclusion (security)
2. 012.3-INT-009 - State secrets exclusion (security)
3. 012.3-E2E-002 - Full checkpoint security validation
4. 012.3-UNIT-001 - Settings parsing
5. 012.3-INT-001 - Backend factory call
6. 012.3-INT-002 - Backend type creation
7. 012.3-UNIT-004 - Context preparation
8. 012.3-INT-003 - Template rendering

### Phase 2: Core Functionality (P1)
1. 012.3-UNIT-002, 012.3-UNIT-003 - Config parsing details
2. 012.3-UNIT-005 through 012.3-UNIT-008 - Action logic
3. 012.3-INT-004 through 012.3-INT-007 - Integration tests
4. 012.3-INT-010 - CLI override
5. 012.3-E2E-001, 012.3-E2E-004 - Workflow tests
6. 012.3-ERR-001, 012.3-ERR-002 - Error handling

### Phase 3: Secondary (P2+)
1. 012.3-UNIT-009, 012.3-UNIT-010 - CLI parsing
2. 012.3-E2E-003 - CLI integration
3. 012.3-ERR-003, 012.3-ERR-004 - Edge case errors

---

## Test Infrastructure Requirements

### Mock Backends
```python
class MockSecretsBackend(SecretsBackend):
    """Test double for secrets backend."""
    def __init__(self, secrets: Dict[str, str]):
        self._secrets = secrets

    def get(self, key: str, default: Any = None) -> Any:
        return self._secrets.get(key, default)

    def has(self, key: str) -> bool:
        return key in self._secrets

    def get_all(self) -> Dict[str, Any]:
        return self._secrets.copy()
```

### Fixtures
```python
@pytest.fixture
def mock_secrets():
    return {"API_KEY": "secret123", "DB_PASSWORD": "db_pass"}

@pytest.fixture
def mock_secrets_backend(mock_secrets):
    return MockSecretsBackend(mock_secrets)

@pytest.fixture
def yaml_with_secrets():
    return """
    settings:
      secrets:
        backend: env
    nodes:
      - name: use_secret
        run: |
          key = "{{ secrets.API_KEY }}"
    edges:
      - from: __start__
        to: use_secret
      - from: use_secret
        to: __end__
    """
```

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-BUILTIN-012.3
  story_title: YAML Engine Integration & Actions
  date: 2026-01-01
  scenarios_total: 27
  by_level:
    unit: 11
    integration: 11
    e2e: 5
  by_priority:
    p0: 8
    p1: 12
    p2: 5
    p3: 2
  coverage_gaps: []
  security_tests:
    - 012.3-INT-008 # Checkpoint exclusion
    - 012.3-INT-009 # State secrets exclusion
    - 012.3-E2E-002 # Full checkpoint validation
  risk_mitigations:
    secrets_in_checkpoint: ["012.3-INT-008", "012.3-INT-009", "012.3-E2E-002"]
    config_precedence: ["012.3-INT-010", "012.3-E2E-003"]
    backend_failures: ["012.3-INT-001", "012.3-ERR-002"]
    template_patterns: ["012.3-INT-011"]  # HTTP header template
    conditional_logic: ["012.3-E2E-005"]  # secrets.has with condition
  updated: 2026-01-01
  update_notes: |
    - Added 012.3-INT-011 for HTTP header template substitution pattern
    - Added 012.3-E2E-005 for secrets.has conditional logic pattern
    - Added 012.3-UNIT-011 for Vault backend config parsing
    - Aligned with documentation in specialized.md
```

---

## Quality Checklist

- [x] Every AC has test coverage (AC-1: 6, AC-2: 5, AC-3: 4, AC-4: 4, AC-5: 3, AC-6: 4, AC-7: 1)
- [x] Test levels are appropriate (shift-left applied)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (security = P0)
- [x] Test IDs follow naming convention (012.3-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Security-critical paths have defense in depth (3 tests for checkpoint exclusion)
- [x] Error scenarios documented
- [x] Test infrastructure requirements specified
- [x] Documented patterns from specialized.md included (HTTP header templates, conditional logic)

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-012.3-test-design-20260101.md
P0 tests identified: 8
P1 tests identified: 12
P2 tests identified: 5
P3 tests identified: 2
Total test scenarios: 27
Security-critical tests: 3
Documented pattern tests: 3 (HTTP headers, conditional logic, Vault config)
```
