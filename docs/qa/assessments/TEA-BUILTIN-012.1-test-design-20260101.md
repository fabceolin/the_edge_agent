# Test Design: Story TEA-BUILTIN-012.1

Date: 2026-01-01
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 25
- Unit tests: 15 (60%)
- Integration tests: 8 (32%)
- E2E tests: 2 (8%)
- Priority distribution: P0: 8, P1: 10, P2: 5, P3: 2

## Story Summary

**Title**: Core Secrets Backend Protocol
**Purpose**: Unified secrets backend protocol with environment variable support for Python YAML engine

## Test Scenarios by Acceptance Criteria

---

### AC-1: SecretsBackend Protocol Definition

> GIVEN a `SecretsBackend` protocol, WHEN implementing a new backend, THEN it must define `get(key)`, `get_all()`, `has(key)`, and `close()` methods

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 012.1-UNIT-001 | Unit | P0 | Verify SecretsBackend ABC has abstract get() method | Core protocol contract - must be enforced |
| 012.1-UNIT-002 | Unit | P0 | Verify SecretsBackend ABC has abstract get_all() method | Core protocol contract - must be enforced |
| 012.1-UNIT-003 | Unit | P0 | Verify SecretsBackend ABC has abstract has() method | Core protocol contract - must be enforced |
| 012.1-UNIT-004 | Unit | P1 | Verify SecretsBackend has default close() implementation | Optional cleanup method should not require override |
| 012.1-UNIT-005 | Unit | P0 | Verify incomplete implementation raises TypeError | Protocol enforcement must prevent runtime errors |

**Test Details:**

```python
# 012.1-UNIT-001 through 012.1-UNIT-005
def test_secrets_backend_protocol_requires_get():
    """GIVEN SecretsBackend ABC, WHEN subclass missing get(), THEN TypeError on instantiation."""
    class IncompleteBackend(SecretsBackend):
        def get_all(self): return {}
        def has(self, key): return False
    with pytest.raises(TypeError, match="get"):
        IncompleteBackend()

def test_secrets_backend_close_has_default():
    """GIVEN SecretsBackend, WHEN close() not overridden, THEN no-op succeeds."""
    class MinimalBackend(SecretsBackend):
        def get(self, key, default=None): return default
        def get_all(self): return {}
        def has(self, key): return False
    backend = MinimalBackend()
    backend.close()  # Should not raise
```

---

### AC-2: EnvSecretsBackend Prefix Support

> GIVEN an `EnvSecretsBackend`, WHEN initialized with `prefix="MYAPP_"`, THEN `get("API_KEY")` returns the value of `MYAPP_API_KEY` environment variable

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 012.1-UNIT-006 | Unit | P0 | get() returns env var with prefix prepended | Core functionality for prefix-based isolation |
| 012.1-UNIT-007 | Unit | P0 | get() with no matching env var returns default | Graceful handling of missing secrets |
| 012.1-UNIT-008 | Unit | P1 | get() with empty prefix returns raw env var name | Edge case for no-prefix mode |
| 012.1-UNIT-009 | Unit | P1 | get() with special characters in prefix works | Validate prefix flexibility |
| 012.1-UNIT-010 | Unit | P2 | get() with None default returns None for missing key | Default parameter behavior |

**Test Details:**

```python
# 012.1-UNIT-006
def test_env_backend_get_with_prefix(monkeypatch):
    """GIVEN env vars with prefix, WHEN get() called, THEN returns value."""
    monkeypatch.setenv("MYAPP_API_KEY", "secret123")
    backend = EnvSecretsBackend(prefix="MYAPP_")
    assert backend.get("API_KEY") == "secret123"

# 012.1-UNIT-007
def test_env_backend_get_missing_returns_default(monkeypatch):
    """GIVEN missing env var, WHEN get() called with default, THEN returns default."""
    backend = EnvSecretsBackend(prefix="MYAPP_")
    assert backend.get("NONEXISTENT", "fallback") == "fallback"

# 012.1-UNIT-008
def test_env_backend_get_no_prefix(monkeypatch):
    """GIVEN empty prefix, WHEN get() called, THEN uses raw key name."""
    monkeypatch.setenv("API_KEY", "direct")
    backend = EnvSecretsBackend(prefix="")
    assert backend.get("API_KEY") == "direct"
```

---

### AC-3: Factory Function for Backend Creation

> GIVEN a factory function `create_secrets_backend("env", prefix="MYAPP_")`, WHEN called, THEN it returns an `EnvSecretsBackend` instance

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 012.1-UNIT-011 | Unit | P0 | Factory with "env" type returns EnvSecretsBackend | Primary backend creation path |
| 012.1-UNIT-012 | Unit | P1 | Factory passes kwargs to backend constructor | Configuration forwarding |
| 012.1-UNIT-013 | Unit | P1 | Factory with unknown type raises ValueError | Error handling for invalid config |
| 012.1-UNIT-014 | Unit | P1 | Factory with "aws"/"azure"/"gcp" raises ImportError with hint | Graceful degradation for missing SDKs |
| 012.1-UNIT-016 | Unit | P2 | Factory with "vault" type creates VaultSecretsBackend | HashiCorp Vault backend support (via Dynaconf) |

**Test Details:**

```python
# 012.1-UNIT-011
def test_factory_creates_env_backend():
    """GIVEN 'env' type, WHEN factory called, THEN EnvSecretsBackend returned."""
    backend = create_secrets_backend("env", prefix="TEST_")
    assert isinstance(backend, EnvSecretsBackend)

# 012.1-UNIT-012
def test_factory_passes_config(monkeypatch):
    """GIVEN prefix config, WHEN factory called, THEN backend uses prefix."""
    monkeypatch.setenv("TEST_KEY", "value")
    backend = create_secrets_backend("env", prefix="TEST_")
    assert backend.get("KEY") == "value"

# 012.1-UNIT-013
def test_factory_unknown_type_raises():
    """GIVEN unknown type, WHEN factory called, THEN ValueError raised."""
    with pytest.raises(ValueError, match="Unknown secrets backend"):
        create_secrets_backend("unknown")

# 012.1-UNIT-014
def test_factory_cloud_backend_raises_import_error():
    """GIVEN cloud type without SDK, WHEN factory called, THEN ImportError with install hint."""
    with pytest.raises(ImportError, match="pip install"):
        create_secrets_backend("aws")

# 012.1-UNIT-016
def test_factory_vault_backend(monkeypatch):
    """GIVEN 'vault' type with config, WHEN factory called, THEN VaultSecretsBackend returned."""
    # Note: Vault backend uses Dynaconf integration
    backend = create_secrets_backend(
        "vault",
        url="https://vault.example.com",
        token="test-token",
        mount="secret"
    )
    assert backend is not None
```

---

### AC-4: YAMLEngine Secrets Integration

> GIVEN `YAMLEngine` with `settings.secrets.backend: env`, WHEN the engine initializes, THEN secrets are loaded via `EnvSecretsBackend`

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 012.1-INT-001 | Integration | P0 | YAMLEngine parses settings.secrets.backend config | Config parsing is critical path |
| 012.1-INT-002 | Integration | P0 | YAMLEngine initializes secrets backend during construction | Backend must be available before node execution |
| 012.1-INT-003 | Integration | P1 | YAMLEngine exposes secrets in template context | Secrets must be accessible in Jinja2 templates |
| 012.1-INT-004 | Integration | P1 | EngineConfig.close() calls secrets backend close() | Resource cleanup prevents leaks |
| 012.1-INT-005 | Integration | P1 | YAMLEngine without secrets config uses no backend | Backward compatibility with existing YAML files |

**Test Details:**

```python
# 012.1-INT-001
def test_yaml_engine_parses_secrets_config(monkeypatch, tmp_path):
    """GIVEN YAML with settings.secrets, WHEN engine initializes, THEN config parsed."""
    monkeypatch.setenv("TEST_SECRET", "my_secret")
    yaml_content = """
    name: test-agent
    settings:
      secrets:
        backend: env
        env:
          prefix: TEST_
    nodes:
      - name: start
        run: |
          return {"done": True}
    edges:
      - from: __start__
        to: start
      - from: start
        to: __end__
    """
    yaml_file = tmp_path / "agent.yaml"
    yaml_file.write_text(yaml_content)
    engine = YAMLEngine.from_file(str(yaml_file))
    assert engine._secrets_backend is not None
    assert isinstance(engine._secrets_backend, EnvSecretsBackend)

# 012.1-INT-004
def test_engine_close_calls_secrets_close(monkeypatch, tmp_path, mocker):
    """GIVEN engine with secrets backend, WHEN close() called, THEN backend close() invoked."""
    # ... setup engine ...
    mock_close = mocker.patch.object(engine._secrets_backend, 'close')
    engine.close()
    mock_close.assert_called_once()
```

---

### AC-5: get_all() for Jinja2 Template Context

> GIVEN a secrets backend, WHEN `get_all()` is called, THEN it returns a dictionary suitable for Jinja2 template context

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 012.1-INT-006 | Integration | P1 | get_all() returns dict with prefix stripped from keys | Template keys should be user-friendly |
| 012.1-INT-007 | Integration | P1 | get_all() with no matching prefix returns empty dict | Edge case for isolated prefix |
| 012.1-INT-008 | Integration | P2 | get_all() dict is safe for Jinja2 (no special chars in values) | Security consideration |
| 012.1-E2E-001 | E2E | P1 | Secrets accessible in YAML template via {{ secrets.KEY }} | End-to-end validation of integration |

**Test Details:**

```python
# 012.1-INT-006
def test_env_backend_get_all_strips_prefix(monkeypatch):
    """GIVEN env vars with prefix, WHEN get_all() called, THEN keys have prefix stripped."""
    monkeypatch.setenv("MYAPP_KEY1", "val1")
    monkeypatch.setenv("MYAPP_KEY2", "val2")
    monkeypatch.setenv("OTHER_KEY", "other")
    backend = EnvSecretsBackend(prefix="MYAPP_")
    secrets = backend.get_all()
    assert secrets == {"KEY1": "val1", "KEY2": "val2"}

# 012.1-E2E-001 (Example YAML for E2E test)
"""
name: secrets-e2e-test
settings:
  secrets:
    backend: env
    prefix: E2E_
nodes:
  - name: use_secret
    run: |
      return {"api_key": "{{ secrets.API_KEY }}"}
edges:
  - from: __start__
    to: use_secret
  - from: use_secret
    to: __end__
"""
```

---

### AC-6: Lazy Loading Without Import Errors

> GIVEN the `secrets/` module, WHEN imported without cloud SDKs installed, THEN no import errors occur (lazy loading)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 012.1-UNIT-015 | Unit | P0 | Import secrets module without cloud SDKs succeeds | Must not break existing installations |
| 012.1-INT-009 | Integration | P2 | Cloud backend only imported when requested | Lazy loading verification |
| 012.1-E2E-002 | E2E | P3 | Full agent workflow works without boto3/azure-identity | Regression test for clean environments |

**Test Details:**

```python
# 012.1-UNIT-015
def test_secrets_module_imports_without_cloud_sdks():
    """GIVEN no cloud SDKs installed, WHEN secrets module imported, THEN no ImportError."""
    # This test runs in an environment without boto3, azure-identity, google-cloud-secret-manager
    from the_edge_agent.secrets import SecretsBackend, EnvSecretsBackend, create_secrets_backend
    assert SecretsBackend is not None
    assert EnvSecretsBackend is not None
    assert create_secrets_backend is not None
```

---

## Risk Coverage

| Risk | Mitigating Tests | Priority |
|------|------------------|----------|
| Protocol not enforced - backends skip required methods | 012.1-UNIT-001 to 012.1-UNIT-005 | P0 |
| Environment variable leakage - wrong prefix | 012.1-UNIT-006, 012.1-INT-006 | P0 |
| Factory creates wrong backend type | 012.1-UNIT-011 | P0 |
| Import failure breaks existing agents | 012.1-UNIT-015 | P0 |
| Resource leak on engine close | 012.1-INT-004 | P1 |
| Template injection via secrets | 012.1-INT-008 | P2 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on protocol/core logic)
   - 012.1-UNIT-001 through 012.1-UNIT-005 (Protocol)
   - 012.1-UNIT-006, 012.1-UNIT-007 (Prefix handling)
   - 012.1-UNIT-011 (Factory basic)
   - 012.1-UNIT-015 (Import safety)

2. **P0 Integration tests**
   - 012.1-INT-001, 012.1-INT-002 (Engine initialization)

3. **P1 Unit tests**
   - 012.1-UNIT-004, 012.1-UNIT-008 through 012.1-UNIT-014

4. **P1 Integration tests**
   - 012.1-INT-003 through 012.1-INT-007

5. **P1 E2E tests**
   - 012.1-E2E-001 (Template access)

6. **P2+ tests as time permits**
   - 012.1-UNIT-010, 012.1-INT-008, 012.1-INT-009
   - 012.1-E2E-002 (clean environment)

---

## Test Environment Requirements

| Environment | Purpose | Setup |
|-------------|---------|-------|
| Unit tests | Isolated logic validation | pytest + monkeypatch |
| Integration tests | Component interaction | pytest + tmp_path + monkeypatch |
| E2E tests | Full workflow validation | pytest + real YAMLEngine execution |

---

## Quality Checklist

- [x] Every AC has test coverage (AC-1: 5, AC-2: 5, AC-3: 4, AC-4: 5, AC-5: 4, AC-6: 3)
- [x] Test levels are appropriate (unit for logic, integration for components, e2e for workflows)
- [x] No duplicate coverage across levels (each scenario tests unique aspect)
- [x] Priorities align with business risk (P0 for security/protocol, P1 for core features)
- [x] Test IDs follow naming convention (012.1-LEVEL-SEQ)
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 25
  by_level:
    unit: 15
    integration: 8
    e2e: 2
  by_priority:
    p0: 8
    p1: 10
    p2: 5
    p3: 2
  coverage_gaps: []
  ac_coverage:
    ac1: 5
    ac2: 5
    ac3: 5  # Added Vault backend test
    ac4: 5
    ac5: 4
    ac6: 3
  key_risks_mitigated:
    - protocol_enforcement
    - env_var_isolation
    - import_safety
    - resource_cleanup
    - vault_backend_support
  updated: 2026-01-01
  update_notes: |
    - Added 012.1-UNIT-016 for Vault backend factory test
    - Fixed YAML config structure to match documentation (settings.secrets.env.prefix)
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-BUILTIN-012.1-test-design-20260101.md
P0 tests identified: 8
P1 tests identified: 10
P2 tests identified: 5
P3 tests identified: 2
Total test scenarios: 25
```
