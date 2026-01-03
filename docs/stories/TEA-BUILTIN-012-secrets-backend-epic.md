# TEA-BUILTIN-012: Cloud Secrets Backend Integration - Epic

## Epic Overview

| Field | Value |
|-------|-------|
| **ID** | TEA-BUILTIN-012 |
| **Type** | Epic |
| **Priority** | High |
| **Status** | Ready for Development |
| **Estimated Stories** | 3 |

## Epic Goal

Provide a unified secrets management abstraction layer for the Python YAML engine, enabling workflows to fetch secrets from AWS Secrets Manager, Azure Key Vault, and GCP Secret Manager using a consistent API similar to how `fsspec` abstracts filesystems.

## Epic Description

### Existing System Context

- **Current Secrets Support**: Environment variables and `.env` files via `python-dotenv`
- **Technology Stack**: Python 3.9+, Jinja2 templates, YAMLEngine
- **Integration Points**:
  - `yaml_engine.py` - Template rendering with `{{ secrets.key }}` syntax
  - `yaml_config.py` - Engine configuration management
  - `actions/` - Action modules that consume secrets
- **Related Work**:
  - TEA-RUST-016 implemented secrets context for Rust (env vars only)
  - Dynaconf already supports HashiCorp Vault + Redis natively

### Enhancement Details

**What's Being Added:**

1. **Abstract `SecretsBackend` Protocol** - Unified interface for all secrets providers
2. **Dynaconf-Based Loader Pattern** - Custom loaders for cloud providers
3. **Cloud Provider Implementations**:
   - AWS Secrets Manager (via `boto3`)
   - Azure Key Vault (via `azure-keyvault-secrets`)
   - GCP Secret Manager (via `google-cloud-secret-manager`)
4. **YAML Configuration** - Declarative secrets backend configuration
5. **`secrets.get` Action** - Runtime secret retrieval in workflows

**How It Integrates:**

```yaml
settings:
  secrets:
    backend: aws  # aws | azure | gcp | vault | env
    # Provider-specific configuration
    aws:
      region: us-east-1
      secret_name: myapp/production
    azure:
      vault_url: https://myvault.vault.azure.net/
    gcp:
      project_id: my-gcp-project

nodes:
  - name: call_api
    run: |
      api_key = secrets.get("API_KEY")
      # or via template: {{ secrets.API_KEY }}
```

**Success Criteria:**

- [ ] Secrets can be loaded from AWS, Azure, and GCP at engine initialization
- [ ] `{{ secrets.KEY }}` template syntax works with all backends
- [ ] `secrets.get` action available for runtime secret retrieval
- [ ] Backend selection via YAML `settings.secrets.backend`
- [ ] Secrets are NOT serialized to checkpoints (security)
- [ ] Graceful fallback to environment variables when backend unavailable

---

## Stories

### Story 1: TEA-BUILTIN-012.1 - Core Secrets Backend Protocol

**Goal:** Define the abstract `SecretsBackend` protocol and integrate with Dynaconf's custom loader pattern.

**Scope:**
- Create `python/src/the_edge_agent/secrets/` module
- Define `SecretsBackend` abstract base class
- Implement `EnvSecretsBackend` (baseline using environment variables)
- Create factory function `create_secrets_backend(backend_type, **config)`
- Integrate with `EngineConfig` for lifecycle management

**Acceptance Criteria:**
1. `SecretsBackend` protocol defines `get(key)`, `get_all()`, `has(key)` methods
2. `EnvSecretsBackend` loads secrets from environment variables with optional prefix
3. Factory function creates backends based on type string
4. Secrets backend is initialized during `YAMLEngine` construction
5. Unit tests cover protocol compliance and env backend

---

### Story 2: TEA-BUILTIN-012.2 - Cloud Provider Backend Implementations

**Goal:** Implement AWS, Azure, and GCP secrets backends following the protocol.

**Scope:**
- Implement `AWSSecretsBackend` using `boto3`
- Implement `AzureSecretsBackend` using `azure-keyvault-secrets`
- Implement `GCPSecretsBackend` using `google-cloud-secret-manager`
- Add optional dependencies to `pyproject.toml` as extras
- Handle authentication via default credential chains

**Acceptance Criteria:**
1. AWS backend fetches secrets from AWS Secrets Manager
2. Azure backend fetches secrets from Azure Key Vault
3. GCP backend fetches secrets from Google Secret Manager
4. Each backend uses provider's default credential chain
5. Backends handle missing secrets gracefully (return None or raise)
6. Install extras: `pip install the-edge-agent[aws]`, `[azure]`, `[gcp]`
7. Integration tests with mocked cloud APIs

---

### Story 3: TEA-BUILTIN-012.3 - YAML Engine Integration & Actions

**Goal:** Integrate secrets backends with YAMLEngine and provide runtime actions.

**Scope:**
- Add `settings.secrets` configuration parsing in `yaml_config.py`
- Inject secrets into Jinja2 template context
- Create `secrets.get` action in `actions/secrets_actions.py`
- Update documentation with usage examples

**Acceptance Criteria:**
1. `settings.secrets.backend` selects the secrets provider
2. `{{ secrets.KEY }}` template syntax resolves secrets
3. `secrets.get` action retrieves secrets at runtime
4. Secrets are excluded from checkpoint serialization
5. CLI supports `--secrets-backend` override flag
6. YAML_REFERENCE.md updated with secrets configuration
7. End-to-end test with mock backend

---

## Compatibility Requirements

- [x] Existing `.env` support continues to work (backward compatible)
- [x] Template syntax `{{ secrets.KEY }}` unchanged from documented API
- [x] No changes to checkpoint serialization format
- [x] Rust implementation unaffected (Python-only feature)
- [x] Optional dependencies - core package works without cloud SDKs

## Dependencies

**Python Dependencies:**

| Dependency | Backend | Install Extra |
|------------|---------|---------------|
| `boto3` | AWS | `[aws]` |
| `azure-identity` | Azure | `[azure]` |
| `azure-keyvault-secrets` | Azure | `[azure]` |
| `google-cloud-secret-manager` | GCP | `[gcp]` |
| `dynaconf` | Core (Vault/Redis) | `[secrets]` |

**Related Epics:**
- **TEA-PARALLEL-001** (Multi-Strategy Parallel Execution) - Secrets backend integration with `remote` strategy documented in that epic's "Feature Interaction Limitations" section

## Risk Mitigation

- **Primary Risk:** Cloud SDK authentication failures in production
- **Mitigation:**
  - Use provider default credential chains (IAM roles, managed identities)
  - Graceful fallback to environment variables
  - Clear error messages with authentication hints
- **Rollback Plan:** Remove `settings.secrets` section; system falls back to env vars

## Technical Design Notes

### Module Structure

```
python/src/the_edge_agent/
├── secrets/
│   ├── __init__.py          # Exports, factory function
│   ├── base.py               # SecretsBackend protocol
│   ├── env.py                # EnvSecretsBackend
│   ├── aws.py                # AWSSecretsBackend
│   ├── azure.py              # AzureSecretsBackend
│   └── gcp.py                # GCPSecretsBackend
├── actions/
│   └── secrets_actions.py    # secrets.get, secrets.has actions
└── yaml_config.py            # Updated for secrets configuration
```

### Protocol Definition

```python
from abc import ABC, abstractmethod
from typing import Any, Dict, Optional

class SecretsBackend(ABC):
    """Abstract base class for secrets backends."""

    @abstractmethod
    def get(self, key: str, default: Any = None) -> Any:
        """Get a secret by key."""
        pass

    @abstractmethod
    def get_all(self) -> Dict[str, Any]:
        """Get all secrets as a dictionary."""
        pass

    @abstractmethod
    def has(self, key: str) -> bool:
        """Check if a secret exists."""
        pass

    def close(self) -> None:
        """Optional cleanup."""
        pass
```

### YAML Configuration Schema

```yaml
settings:
  secrets:
    backend: aws | azure | gcp | vault | env
    cache_ttl: 300  # Optional: cache secrets for N seconds
    prefix: MYAPP_  # Optional: env var prefix for 'env' backend

    # AWS-specific
    aws:
      region: us-east-1
      secret_name: myapp/secrets  # Single secret with JSON
      # OR
      secret_prefix: myapp/       # Multiple secrets by prefix

    # Azure-specific
    azure:
      vault_url: https://myvault.vault.azure.net/
      # Uses DefaultAzureCredential

    # GCP-specific
    gcp:
      project_id: my-project
      secret_prefix: myapp-      # Optional prefix filter

    # Vault-specific (via Dynaconf)
    vault:
      url: https://vault.example.com
      token: ${VAULT_TOKEN}      # Or use AppRole/AWS IAM auth
      mount: secret
```

---

## Definition of Done

- [ ] All 3 stories completed with acceptance criteria met
- [ ] Existing `.env` and environment variable support unchanged
- [ ] `{{ secrets.KEY }}` template syntax works with all backends
- [ ] Optional dependencies properly configured in `pyproject.toml`
- [ ] Unit tests for each backend with mocked cloud APIs
- [ ] Integration test with at least one real cloud backend (CI optional)
- [ ] `docs/shared/YAML_REFERENCE.md` updated with secrets configuration
- [ ] `docs/python/actions-reference.md` updated with `secrets.*` actions
- [ ] No regression in existing features

---

## Story Manager Handoff

"Please develop detailed user stories for this brownfield epic. Key considerations:

- This is an enhancement to the Python YAML engine
- Integration points: `yaml_config.py`, `yaml_engine.py`, template context, actions
- Existing patterns to follow:
  - `memory/` module structure (backends with factory functions)
  - `actions/` module pattern (register with `@action` decorator)
  - Optional dependencies as extras in `pyproject.toml`
- Critical compatibility requirements:
  - `{{ secrets.KEY }}` must work identically to current env var behavior
  - Secrets must NOT appear in checkpoints
- Each story must include verification that existing functionality remains intact

The epic should maintain system integrity while delivering unified cloud secrets management."

---

## QA Notes

**Date:** 2026-01-01
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Count | Distribution |
|--------|-------|--------------|
| **Total test scenarios** | 76+ | Across 3 child stories |
| **Unit tests** | 43 (52%) | Protocol, backends, actions, parsing |
| **Integration tests** | 30 (36%) | Cloud SDK mocks, YAMLEngine, templates |
| **E2E tests** | 10 (12%) | Full workflows, checkpoints, CLI |

**Priority Distribution:**
- **P0 (Critical):** 26 tests - Security, protocol compliance, core retrieval
- **P1 (High):** 36 tests - Backend operations, template rendering, actions
- **P2 (Medium):** 17 tests - Edge cases, CLI override, cache behavior
- **P3 (Low):** 4 tests - Advanced configurations, stress testing

### Risk Areas Identified

| Risk | Severity | Mitigating Tests | Notes |
|------|----------|------------------|-------|
| **Secrets serialized to checkpoints** | CRITICAL | 4 tests (P0) | Defense in depth with 012.3-INT-008, 012.3-INT-009, 012.3-INT-012, 012.3-E2E-002 |
| **Cloud SDK authentication failures** | HIGH | 6 tests | AWS, Azure, GCP auth error handling |
| **Credential leakage in error messages** | HIGH | 3 tests (P0) | 012.2-UNIT-017 to 019 sanitize errors |
| **Protocol non-compliance** | HIGH | 5 tests (P0) | ABC enforcement for SecretsBackend |
| **Environment variable prefix leakage** | HIGH | 2 tests | Prefix isolation in EnvSecretsBackend |
| **Import breaks without cloud SDKs** | MEDIUM | 3 tests | Lazy loading verification |
| **CLI/YAML config precedence conflict** | MEDIUM | 4 tests | Override behavior validated |
| **Cache inconsistency across backends** | LOW | 3 tests | Immutability and instance isolation |

### Recommended Test Scenarios

**Security-Critical (Execute First):**
1. Checkpoint exclusion - secrets NOT serialized to disk
2. Error message sanitization - no credential/URL leakage
3. Prefix isolation - env backend only exposes prefixed vars

**Core Functionality:**
1. Factory creates correct backend types (env, aws, azure, gcp, vault)
2. Template substitution `{{ secrets.KEY }}` with all backends
3. `secrets.get` and `secrets.has` actions store values in state
4. Backend lifecycle (init, get, get_all, has, close)

**Integration Patterns:**
1. HTTP header template: `Authorization: "Bearer {{ secrets.API_KEY }}"`
2. Conditional logic: `secrets.has` with `condition:` for optional features
3. CLI override: `--secrets-backend aws` takes precedence over YAML

**Edge Cases:**
1. Missing secrets return default values gracefully
2. Empty/missing prefix treated correctly
3. Unicode values preserved through backends

### Concerns and Blockers

| Concern | Impact | Recommendation |
|---------|--------|----------------|
| **No live cloud integration tests in CI** | MEDIUM | Document manual testing procedure for cloud backends; consider optional CI job with cloud credentials |
| **Vault backend requires hvac** | LOW | Add to `[secrets]` extra; mock tests sufficient for protocol compliance |
| **Cache TTL not extensively tested** | LOW | Add P3 stress tests if cache_ttl feature is implemented |
| **State transience for secrets.get results** | MEDIUM | Verify `output:` values from secrets.get are marked transient if they should not checkpoint |

### Test Infrastructure Notes

**Mocking Strategy:**
- `boto3` mocked at module level (`patch("the_edge_agent.secrets.aws.boto3")`)
- Azure SDK mocked via `SecretClient` and `DefaultAzureCredential`
- GCP SDK mocked via `SecretManagerServiceClient`
- Environment variables via `pytest.monkeypatch.setenv()`

**Required Fixtures:**
- `mock_secrets_backend` - Generic mock for unit tests
- `env_with_secrets` - Env vars with test prefix
- `checkpoint_dir` - Temp directory for checkpoint file tests

### Gate Status

**READY FOR DEVELOPMENT**

All epic and child story acceptance criteria have comprehensive test coverage. Security-critical checkpoint exclusion tests are properly prioritized as P0. Mocking strategy documented for all cloud SDKs. No blocking issues identified.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-01 | 0.2 | Added QA Notes section | Quinn (QA) |
| 2026-01-01 | 0.1 | Initial epic draft | Sarah (PO) |
