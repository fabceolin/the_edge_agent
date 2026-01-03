# Test Design: Story TEA-BUILTIN-012.2

Date: 2026-01-01
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 31
- Unit tests: 17 (55%)
- Integration tests: 11 (35%)
- E2E tests: 3 (10%)
- Priority distribution: P0: 10, P1: 14, P2: 7

## Test Scenarios by Acceptance Criteria

### AC-1: AWS Secrets Manager Get Operation

GIVEN `AWSSecretsBackend` with valid credentials, WHEN `get("api_key")` is called, THEN the secret value from AWS Secrets Manager is returned

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 012.2-UNIT-001 | Unit | P0 | Validate JSON secret parsing from AWS response | Pure logic extracting secret value from JSON |
| 012.2-UNIT-002 | Unit | P0 | Validate single JSON secret retrieval pattern | Core business logic for single-secret mode |
| 012.2-UNIT-003 | Unit | P1 | Validate prefix-based multi-secret retrieval | Logic for iterating paginated secret list |
| 012.2-UNIT-004 | Unit | P1 | Validate cache population on initialization | State management logic for loaded secrets |
| 012.2-INT-001 | Integration | P0 | Mock boto3 client for get_secret_value | Component interaction with AWS SDK |
| 012.2-INT-002 | Integration | P1 | Mock paginator for list_secrets with prefix | Pagination behavior with mocked AWS API |

---

### AC-2: Azure Key Vault Get Operation

GIVEN `AzureSecretsBackend` with `vault_url`, WHEN `get("api-key")` is called, THEN the secret value from Azure Key Vault is returned

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 012.2-UNIT-005 | Unit | P0 | Validate secret value extraction from Azure response | Pure logic extracting value from SecretClient response |
| 012.2-UNIT-006 | Unit | P1 | Validate lazy loading on get() call | Cache population logic on first access |
| 012.2-INT-003 | Integration | P0 | Mock SecretClient.get_secret() | Component interaction with Azure SDK |
| 012.2-INT-004 | Integration | P1 | Mock DefaultAzureCredential initialization | Credential chain integration |

---

### AC-3: GCP Secret Manager Get Operation

GIVEN `GCPSecretsBackend` with `project_id`, WHEN `get("api_key")` is called, THEN the secret value from Google Secret Manager is returned

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 012.2-UNIT-007 | Unit | P0 | Validate secret version path construction | String formatting logic for GCP resource paths |
| 012.2-UNIT-008 | Unit | P1 | Validate UTF-8 decoding of payload data | Data transformation logic |
| 012.2-INT-005 | Integration | P0 | Mock SecretManagerServiceClient.access_secret_version | Component interaction with GCP SDK |
| 012.2-INT-006 | Integration | P1 | Mock list_secrets for get_all() | Pagination and filtering behavior |

---

### AC-4: Credential Error Handling

GIVEN AWS/Azure/GCP backend, WHEN credentials are not configured, THEN a clear error message with authentication hints is raised

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 012.2-UNIT-009 | Unit | P0 | AWS: ResourceNotFoundException raises clear error | Error transformation logic |
| 012.2-UNIT-010 | Unit | P0 | Azure: HttpResponseError raises clear error | Error transformation logic |
| 012.2-UNIT-011 | Unit | P0 | GCP: NotFound exception raises clear error | Error transformation logic |
| 012.2-INT-007 | Integration | P1 | AWS: Invalid credentials produce helpful message | SDK exception handling |
| 012.2-INT-008 | Integration | P1 | Azure: Credential failure produces helpful message | SDK exception handling |
| 012.2-INT-009 | Integration | P1 | GCP: ADC failure produces helpful message | SDK exception handling |

---

### AC-5: Optional Dependency Installation Success

GIVEN `pip install the-edge-agent[aws]`, WHEN importing `AWSSecretsBackend`, THEN no import errors occur

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 012.2-INT-010 | Integration | P1 | Verify AWS backend imports with boto3 available | Dependency wiring validation |
| 012.2-E2E-001 | E2E | P2 | Fresh virtualenv with [aws] extra imports successfully | Full installation validation |
| 012.2-E2E-002 | E2E | P2 | Fresh virtualenv with [azure] extra imports successfully | Full installation validation |
| 012.2-E2E-003 | E2E | P2 | Fresh virtualenv with [gcp] extra imports successfully | Full installation validation |

---

### AC-6: Optional Dependency Missing Error

GIVEN `pip install the-edge-agent` (no extras), WHEN importing `AWSSecretsBackend`, THEN `ImportError` is raised with install hint

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 012.2-UNIT-012 | Unit | P0 | AWS: ImportError message contains "pip install the-edge-agent[aws]" | Error message validation |
| 012.2-UNIT-013 | Unit | P0 | Azure: ImportError message contains "pip install the-edge-agent[azure]" | Error message validation |
| 012.2-UNIT-014 | Unit | P0 | GCP: ImportError message contains "pip install the-edge-agent[gcp]" | Error message validation |

---

### AC-7: Get All Secrets Operation

GIVEN any cloud backend, WHEN `get_all()` is called, THEN all accessible secrets are returned as a dictionary

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 012.2-UNIT-015 | Unit | P1 | Validate get_all() returns dict copy (not reference) | Data isolation logic |
| 012.2-INT-011 | Integration | P1 | AWS: get_all() with prefix returns filtered secrets | Multi-component pagination flow |
| 012.2-INT-012 | Integration | P1 | Azure: list_properties_of_secrets populates cache | SDK list operation behavior |
| 012.2-INT-013 | Integration | P1 | GCP: list_secrets with prefix filtering | SDK list operation behavior |

---

### AC-8: HashiCorp Vault Backend (via Dynaconf)

GIVEN `VaultSecretsBackend` with `url`, `token`, and `mount`, WHEN `get("key")` is called, THEN the secret value from HashiCorp Vault is returned

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 012.2-UNIT-016 | Unit | P1 | Validate Vault secret path construction | Path formatting logic |
| 012.2-UNIT-017 | Unit | P1 | Validate Vault authentication token handling | Token configuration logic |
| 012.2-INT-014 | Integration | P1 | Mock hvac client for get_secret | Component interaction with Vault SDK |
| 012.2-E2E-004 | E2E | P2 | Fresh virtualenv with [secrets] extra imports Vault backend | Full installation validation |

**Test Details:**

```python
# 012.2-UNIT-016
def test_vault_backend_path_construction():
    """GIVEN Vault config, WHEN get() called, THEN correct path used."""
    with patch("the_edge_agent.secrets.vault.hvac") as mock_hvac:
        mock_client = Mock()
        mock_hvac.Client.return_value = mock_client
        mock_client.secrets.kv.v2.read_secret_version.return_value = {
            "data": {"data": {"api_key": "secret123"}}
        }
        backend = VaultSecretsBackend(
            url="https://vault.example.com",
            token="test-token",
            mount="secret"
        )
        assert backend.get("api_key") == "secret123"
```

---

## Risk Coverage

| Risk | Test IDs | Coverage |
|------|----------|----------|
| Security: Credential leakage in errors | 012.2-UNIT-009,010,011 | Error messages sanitized |
| Reliability: SDK version incompatibility | 012.2-E2E-001,002,003 | Fresh install validation |
| Data: Cache inconsistency | 012.2-UNIT-004,006,015 | Immutability enforced |
| Usability: Unclear dependency errors | 012.2-UNIT-012,013,014 | Actionable error messages |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - 012.2-UNIT-001, 002, 005, 007, 009, 010, 011, 012, 013, 014
2. **P0 Integration tests** (SDK interaction)
   - 012.2-INT-001, 003, 005
3. **P1 Unit tests** (secondary logic)
   - 012.2-UNIT-003, 004, 006, 008, 015
4. **P1 Integration tests** (extended coverage)
   - 012.2-INT-002, 004, 006, 007, 008, 009, 010, 011, 012, 013
5. **P2 E2E tests** (installation validation - only in full regression)
   - 012.2-E2E-001, 002, 003

---

## Test Implementation Notes

### Mocking Strategy

```python
# AWS: Mock boto3.client at module level
with patch("the_edge_agent.secrets.aws.boto3") as mock_boto:
    mock_client = Mock()
    mock_boto.client.return_value = mock_client
    mock_client.get_secret_value.return_value = {"SecretString": '{"key": "value"}'}

# Azure: Mock both identity and secrets modules
with patch("the_edge_agent.secrets.azure.DefaultAzureCredential"), \
     patch("the_edge_agent.secrets.azure.SecretClient") as MockClient:
    mock_secret = Mock(value="secret123")
    MockClient.return_value.get_secret.return_value = mock_secret

# GCP: Mock SecretManagerServiceClient
with patch("the_edge_agent.secrets.gcp.secretmanager") as mock_sm:
    mock_response = Mock()
    mock_response.payload.data = b"secret123"
    mock_sm.SecretManagerServiceClient.return_value.access_secret_version.return_value = mock_response
```

### Import Error Testing

```python
# Simulate missing dependency
def test_aws_import_error():
    with patch.dict("sys.modules", {"boto3": None}):
        # Force reimport
        import importlib
        import the_edge_agent.secrets.aws as aws_module
        importlib.reload(aws_module)
        with pytest.raises(ImportError, match=r"pip install the-edge-agent\[aws\]"):
            aws_module.AWSSecretsBackend(secret_name="test")
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (security = P0)
- [x] Test IDs follow naming convention (012.2-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Mocking strategy prevents real cloud API calls

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 31
  by_level:
    unit: 17
    integration: 11
    e2e: 4
  by_priority:
    p0: 10
    p1: 14
    p2: 7
  coverage_gaps: []
  key_risks_mitigated:
    - credential_error_clarity
    - import_error_guidance
    - cache_data_isolation
    - vault_backend_support
  updated: 2026-01-01
  update_notes: |
    - Added AC-8 for HashiCorp Vault backend (4 scenarios: 012.2-UNIT-016, 017, 012.2-INT-014, 012.2-E2E-004)
    - Aligned with documentation in specialized.md which includes Vault backend
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-012.2-test-design-20260101.md
P0 tests identified: 10
P1 tests identified: 14
P2 tests identified: 7
Total test scenarios: 31
Critical paths covered: AWS/Azure/GCP/Vault get operations, credential errors, import errors
```
