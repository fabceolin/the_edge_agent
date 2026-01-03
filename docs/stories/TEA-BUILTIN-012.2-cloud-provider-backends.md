# TEA-BUILTIN-012.2: Cloud Provider Backend Implementations

## Status

**Done**

> Story validated by Scrum Master on 2026-01-01. All acceptance criteria have complete test coverage (28 scenarios). Test design approved by QA.
> Implementation completed on 2026-01-01 by James (Dev Agent).
> QA Gate PASS on 2026-01-01 by Quinn (Test Architect). All 7 ACs met, 79 tests passing, security verified.

## Story

**As a** workflow developer deploying to cloud environments,
**I want** secrets backends for AWS, Azure, and GCP,
**so that** I can use cloud-native secrets management without code changes.

## Acceptance Criteria

1. **AC-1**: GIVEN `AWSSecretsBackend` with valid credentials, WHEN `get("api_key")` is called, THEN the secret value from AWS Secrets Manager is returned

2. **AC-2**: GIVEN `AzureSecretsBackend` with `vault_url`, WHEN `get("api-key")` is called, THEN the secret value from Azure Key Vault is returned

3. **AC-3**: GIVEN `GCPSecretsBackend` with `project_id`, WHEN `get("api_key")` is called, THEN the secret value from Google Secret Manager is returned

4. **AC-4**: GIVEN AWS/Azure/GCP backend, WHEN credentials are not configured, THEN a clear error message with authentication hints is raised

5. **AC-5**: GIVEN `pip install the-edge-agent[aws]`, WHEN importing `AWSSecretsBackend`, THEN no import errors occur

6. **AC-6**: GIVEN `pip install the-edge-agent` (no extras), WHEN importing `AWSSecretsBackend`, THEN `ImportError` is raised with install hint

7. **AC-7**: GIVEN any cloud backend, WHEN `get_all()` is called, THEN all accessible secrets are returned as a dictionary

---

## Tasks / Subtasks

- [x] **Task 1: Implement AWSSecretsBackend** (AC: 1, 4, 7)
  - [x] Create `python/src/the_edge_agent/secrets/aws.py`
  - [x] Use `boto3` with default credential chain
  - [x] Support single secret (JSON) or prefix-based multi-secret
  - [x] Handle `ResourceNotFoundException` gracefully
  - [x] Add clear error message for credential issues

- [x] **Task 2: Implement AzureSecretsBackend** (AC: 2, 4, 7)
  - [x] Create `python/src/the_edge_agent/secrets/azure.py`
  - [x] Use `azure-identity.DefaultAzureCredential`
  - [x] Use `azure-keyvault-secrets.SecretClient`
  - [x] Handle `HttpResponseError` for missing secrets
  - [x] Support listing all secrets via `list_properties_of_secrets()`

- [x] **Task 3: Implement GCPSecretsBackend** (AC: 3, 4, 7)
  - [x] Create `python/src/the_edge_agent/secrets/gcp.py`
  - [x] Use `google-cloud-secret-manager` with ADC
  - [x] Access latest version of each secret
  - [x] Handle `NotFound` exception gracefully
  - [x] Support prefix filtering for `get_all()`

- [x] **Task 4: Configure optional dependencies** (AC: 5, 6)
  - [x] Add `[aws]` extra: `boto3`
  - [x] Add `[azure]` extra: `azure-identity`, `azure-keyvault-secrets`
  - [x] Add `[gcp]` extra: `google-cloud-secret-manager`
  - [x] Add `[secrets]` extra: all of the above
  - [x] Update factory function to handle ImportError

- [x] **Task 5: Unit tests with mocked APIs** (AC: 1-7)
  - [x] Mock `boto3.client` for AWS tests
  - [x] Mock `SecretClient` for Azure tests
  - [x] Mock `SecretManagerServiceClient` for GCP tests
  - [x] Test credential error handling

---

## Dev Notes

### Relevant Source Tree

```
python/src/the_edge_agent/secrets/
├── __init__.py         # Factory with lazy imports
├── base.py             # SecretsBackend ABC (from 012.1)
├── env.py              # EnvSecretsBackend (from 012.1)
├── aws.py              # NEW: AWSSecretsBackend
├── azure.py            # NEW: AzureSecretsBackend
└── gcp.py              # NEW: GCPSecretsBackend
```

### AWS Implementation Notes

```python
# AWS Secrets Manager supports two patterns:
# 1. Single JSON secret: {"api_key": "...", "db_password": "..."}
# 2. Multiple secrets with prefix: myapp/api_key, myapp/db_password

import json
from typing import Any, Dict, Optional
from .base import SecretsBackend

class AWSSecretsBackend(SecretsBackend):
    def __init__(
        self,
        region: Optional[str] = None,
        secret_name: Optional[str] = None,  # Single JSON secret
        secret_prefix: Optional[str] = None,  # Multiple secrets
    ):
        try:
            import boto3
        except ImportError:
            raise ImportError(
                "boto3 is required for AWS secrets. "
                "Install with: pip install the-edge-agent[aws]"
            )
        self._client = boto3.client("secretsmanager", region_name=region)
        self._secret_name = secret_name
        self._secret_prefix = secret_prefix
        self._cache: Dict[str, Any] = {}
        self._load_secrets()

    def _load_secrets(self) -> None:
        if self._secret_name:
            # Single JSON secret
            response = self._client.get_secret_value(SecretId=self._secret_name)
            self._cache = json.loads(response["SecretString"])
        elif self._secret_prefix:
            # Multiple secrets with prefix
            paginator = self._client.get_paginator("list_secrets")
            for page in paginator.paginate():
                for secret in page["SecretList"]:
                    if secret["Name"].startswith(self._secret_prefix):
                        key = secret["Name"][len(self._secret_prefix):]
                        value = self._client.get_secret_value(SecretId=secret["Name"])
                        self._cache[key] = value["SecretString"]

    def get(self, key: str, default: Any = None) -> Any:
        return self._cache.get(key, default)

    def get_all(self) -> Dict[str, Any]:
        return self._cache.copy()

    def has(self, key: str) -> bool:
        return key in self._cache
```

### Azure Implementation Notes

```python
from typing import Any, Dict, Optional
from .base import SecretsBackend

class AzureSecretsBackend(SecretsBackend):
    def __init__(self, vault_url: str):
        try:
            from azure.identity import DefaultAzureCredential
            from azure.keyvault.secrets import SecretClient
        except ImportError:
            raise ImportError(
                "azure-identity and azure-keyvault-secrets required. "
                "Install with: pip install the-edge-agent[azure]"
            )
        credential = DefaultAzureCredential()
        self._client = SecretClient(vault_url=vault_url, credential=credential)
        self._cache: Dict[str, Any] = {}

    def get(self, key: str, default: Any = None) -> Any:
        if key not in self._cache:
            try:
                secret = self._client.get_secret(key)
                self._cache[key] = secret.value
            except Exception:
                return default
        return self._cache.get(key, default)

    def get_all(self) -> Dict[str, Any]:
        # Load all secrets on first call
        if not self._cache:
            for prop in self._client.list_properties_of_secrets():
                secret = self._client.get_secret(prop.name)
                self._cache[prop.name] = secret.value
        return self._cache.copy()

    def has(self, key: str) -> bool:
        try:
            self._client.get_secret(key)
            return True
        except Exception:
            return False
```

### GCP Implementation Notes

```python
from typing import Any, Dict, Optional
from .base import SecretsBackend

class GCPSecretsBackend(SecretsBackend):
    def __init__(self, project_id: str, secret_prefix: str = ""):
        try:
            from google.cloud import secretmanager
        except ImportError:
            raise ImportError(
                "google-cloud-secret-manager required. "
                "Install with: pip install the-edge-agent[gcp]"
            )
        self._client = secretmanager.SecretManagerServiceClient()
        self._project_id = project_id
        self._secret_prefix = secret_prefix
        self._cache: Dict[str, Any] = {}

    def get(self, key: str, default: Any = None) -> Any:
        if key not in self._cache:
            name = f"projects/{self._project_id}/secrets/{self._secret_prefix}{key}/versions/latest"
            try:
                response = self._client.access_secret_version(request={"name": name})
                self._cache[key] = response.payload.data.decode("UTF-8")
            except Exception:
                return default
        return self._cache.get(key, default)

    def get_all(self) -> Dict[str, Any]:
        parent = f"projects/{self._project_id}"
        for secret in self._client.list_secrets(request={"parent": parent}):
            name = secret.name.split("/")[-1]
            if name.startswith(self._secret_prefix):
                key = name[len(self._secret_prefix):] if self._secret_prefix else name
                if key not in self._cache:
                    self.get(key)
        return self._cache.copy()

    def has(self, key: str) -> bool:
        return self.get(key) is not None
```

### pyproject.toml Extras

```toml
[project.optional-dependencies]
aws = ["boto3>=1.26"]
azure = ["azure-identity>=1.12", "azure-keyvault-secrets>=4.7"]
gcp = ["google-cloud-secret-manager>=2.16"]
secrets = [
    "boto3>=1.26",
    "azure-identity>=1.12",
    "azure-keyvault-secrets>=4.7",
    "google-cloud-secret-manager>=2.16",
]
```

---

## Testing

### Test File Location

`python/tests/test_secrets_cloud.py`

### Test Standards

- Use `pytest` with `unittest.mock` for cloud API mocking
- No real cloud API calls in unit tests
- Integration tests (optional) in `tests/integration/`

### Test Cases

```python
from unittest.mock import Mock, patch

def test_aws_backend_get_from_json_secret():
    """GIVEN JSON secret in AWS, WHEN get() called, THEN value returned."""
    with patch("boto3.client") as mock_client:
        mock_client.return_value.get_secret_value.return_value = {
            "SecretString": '{"api_key": "secret123"}'
        }
        backend = AWSSecretsBackend(secret_name="myapp/secrets")
        assert backend.get("api_key") == "secret123"

def test_azure_backend_get_secret():
    """GIVEN secret in Azure Key Vault, WHEN get() called, THEN value returned."""
    with patch("azure.keyvault.secrets.SecretClient") as MockClient:
        mock_secret = Mock()
        mock_secret.value = "secret123"
        MockClient.return_value.get_secret.return_value = mock_secret

        backend = AzureSecretsBackend(vault_url="https://test.vault.azure.net")
        assert backend.get("api-key") == "secret123"

def test_gcp_backend_get_secret():
    """GIVEN secret in GCP, WHEN get() called, THEN value returned."""
    with patch("google.cloud.secretmanager.SecretManagerServiceClient") as MockClient:
        mock_response = Mock()
        mock_response.payload.data = b"secret123"
        MockClient.return_value.access_secret_version.return_value = mock_response

        backend = GCPSecretsBackend(project_id="my-project")
        assert backend.get("api_key") == "secret123"

def test_aws_import_error_without_boto3():
    """GIVEN boto3 not installed, WHEN AWSSecretsBackend imported, THEN ImportError."""
    with patch.dict("sys.modules", {"boto3": None}):
        with pytest.raises(ImportError, match="pip install the-edge-agent\\[aws\\]"):
            AWSSecretsBackend(secret_name="test")
```

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-01 | 0.1 | Initial story draft | Sarah (PO) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

No blocking issues encountered during implementation.

### Completion Notes List

1. **AWSSecretsBackend** - Implemented with support for both single JSON secrets and prefix-based multi-secret patterns. Uses boto3 default credential chain. Includes lazy loading to avoid API calls until secrets are accessed.

2. **AzureSecretsBackend** - Implemented with DefaultAzureCredential for flexible authentication. Validates vault_url format. Supports get_all() via list_properties_of_secrets().

3. **GCPSecretsBackend** - Implemented with Application Default Credentials (ADC). Supports prefix filtering for namespacing secrets. Accesses latest version of each secret.

4. **Optional Dependencies** - Added [aws], [azure], [gcp], and [secrets] extras to setup.py. Factory function updated with lazy loading to avoid ImportError when cloud SDKs not installed.

5. **Security** - Error messages sanitized to avoid leaking credentials, vault URLs, or project IDs. All backends use caching to minimize API calls.

6. **Tests** - Created comprehensive test suite with 44 test cases covering all acceptance criteria, including security tests for credential leakage.

### File List

**New Files:**
- `python/src/the_edge_agent/secrets/aws.py` - AWSSecretsBackend implementation
- `python/src/the_edge_agent/secrets/azure.py` - AzureSecretsBackend implementation
- `python/src/the_edge_agent/secrets/gcp.py` - GCPSecretsBackend implementation
- `python/tests/test_secrets_cloud.py` - Cloud secrets backend unit tests (44 tests)

**Modified Files:**
- `python/src/the_edge_agent/secrets/__init__.py` - Updated factory with lazy imports for cloud backends
- `python/setup.py` - Added [aws], [azure], [gcp], [secrets] optional dependencies
- `python/tests/test_secrets_backend.py` - Updated tests for new backend behavior

---

## QA Results

### QA Notes

**Assessment Date:** 2026-01-01
**Reviewer:** Quinn (Test Architect)

#### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 28 |
| Unit Tests | 15 (54%) |
| Integration Tests | 10 (36%) |
| E2E Tests | 3 (10%) |
| P0 (Critical) | 10 |
| P1 (High) | 12 |
| P2 (Medium) | 6 |
| Coverage Gaps | None identified |

**All 7 Acceptance Criteria have test coverage mapped.**

#### Risk Areas Identified

| Risk | Severity | Mitigation | Test IDs |
|------|----------|------------|----------|
| **Security: Credential leakage in error messages** | HIGH | Error messages must be sanitized to avoid exposing secrets or credentials | 012.2-UNIT-009, 010, 011 |
| **Reliability: SDK version incompatibility** | MEDIUM | Fresh virtualenv installation tests validate dependency compatibility | 012.2-E2E-001, 002, 003 |
| **Data: Cache inconsistency** | MEDIUM | get_all() must return dict copy, not reference; cache immutability enforced | 012.2-UNIT-004, 006, 015 |
| **Usability: Unclear dependency errors** | LOW | ImportError messages must include exact pip install command | 012.2-UNIT-012, 013, 014 |

#### Recommended Test Scenarios

1. **P0 Critical Path Tests (Execute First):**
   - JSON secret parsing from AWS response
   - Secret value extraction from Azure/GCP responses
   - Credential error handling for all three providers
   - Import error messages with install hints

2. **P0 Integration Tests:**
   - Mock boto3 client for AWS get_secret_value
   - Mock Azure SecretClient.get_secret()
   - Mock GCP SecretManagerServiceClient.access_secret_version

3. **P1 Secondary Logic:**
   - Prefix-based multi-secret retrieval (AWS)
   - Lazy loading cache population (Azure/GCP)
   - Pagination behavior for list operations

4. **P2 Full Regression Only:**
   - Fresh virtualenv with [aws], [azure], [gcp] extras

#### Concerns and Blockers

| Item | Type | Description |
|------|------|-------------|
| **Mocking Strategy** | Concern | Must mock at correct module path (e.g., `the_edge_agent.secrets.aws.boto3`) to avoid real API calls |
| **Import Error Testing** | Concern | Requires `importlib.reload()` to properly test missing dependency behavior |
| **Credential Chain Testing** | Advisory | Real cloud credential testing deferred to integration environment; unit tests use mocks only |

#### Recommendations

1. **Test File:** Create `python/tests/test_secrets_cloud.py` with all 28 scenarios
2. **Mocking Pattern:** Follow documented mocking strategy in test design to prevent real cloud API calls
3. **CI Configuration:** Run P2 E2E tests only in full regression, not on every commit
4. **Security Review:** Verify error messages do not leak credential paths or vault URLs

**Test Design Reference:** `docs/qa/assessments/TEA-BUILTIN-012.2-test-design-20260101.md`

---

### Review Date: 2026-01-01

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: Excellent** - The implementation demonstrates high-quality software engineering practices. All three cloud backends (AWS, Azure, GCP) follow a consistent pattern, implement proper error handling, and include comprehensive test coverage.

**Key Strengths:**
- **Lazy loading pattern** prevents unnecessary API calls until secrets are actually needed
- **Caching strategy** minimizes cloud API calls with proper cache isolation (returns copies)
- **Error message sanitization** prevents credential leakage - verified by dedicated security tests
- **Configuration validation** catches invalid parameters early (vault_url format, mutually exclusive options)
- **Optional dependency handling** with clear installation hints in ImportError messages

### Refactoring Performed

None required. The implementation is clean and well-structured.

### Compliance Check

- Coding Standards: [✓] Clean code, consistent naming, proper docstrings
- Project Structure: [✓] Follows established patterns in secrets/ module
- Testing Strategy: [✓] 79 total tests (44 cloud + 35 base), all passing
- All ACs Met: [✓] All 7 acceptance criteria fully implemented and tested

### Improvements Checklist

All items below are suggestions for future enhancements, not blockers:

- [x] AWSSecretsBackend implemented with JSON and prefix modes
- [x] AzureSecretsBackend implemented with vault URL validation
- [x] GCPSecretsBackend implemented with prefix filtering
- [x] Optional dependencies configured in setup.py
- [x] Factory function supports lazy loading
- [x] Security tests verify no credential leakage
- [x] Cache returns copies (immutability enforced)
- [x] Context manager support (inherited from base class)
- [ ] Consider adding Dynaconf/HashiCorp Vault backend for on-premises secrets
- [ ] Consider adding secret rotation support with TTL-based cache invalidation
- [ ] Consider adding async versions of the backends for async workflows

### Security Review

**Status: PASS**

Security testing verified:
- `test_aws_error_does_not_leak_credentials` - AWS ARNs not exposed in error messages
- `test_azure_error_does_not_leak_vault_url` - Vault URL not exposed in generic errors
- `test_gcp_error_does_not_leak_project_id` - Project ID not exposed in error messages

All backends transform cloud-specific exceptions to generic `RuntimeError` with actionable hints that don't expose sensitive configuration.

### Performance Considerations

**Status: PASS**

- Lazy loading: Secrets loaded on first access, not at backend initialization
- Caching: All backends cache retrieved secrets to minimize API calls
- Verified by tests: `test_azure_backend_caches_secrets`, `test_gcp_backend_caches_secrets`
- AWS loads all secrets upfront (required by JSON secret pattern), but uses lazy evaluation

### Files Modified During Review

None - no modifications required.

### Gate Status

Gate: **PASS** -> `docs/qa/gates/TEA-BUILTIN-012.2-cloud-provider-backends.yml`
Risk profile: No new risks identified (all documented risks mitigated by tests)
NFR assessment: All NFRs PASS (security, performance, reliability, maintainability)

### Recommended Status

[✓ Ready for Done] - All acceptance criteria met, comprehensive test coverage, no blocking issues.

(Story owner decides final status)
