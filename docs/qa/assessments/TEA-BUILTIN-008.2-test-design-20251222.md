# Test Design: Story TEA-BUILTIN-008.2

**Date**: 2024-12-22 (Updated)
**Designer**: Quinn (Test Architect)
**Story**: Schema Loading with Git Refs & Remote Storage

## Test Strategy Overview

- **Total test scenarios**: 60
- **Unit tests**: 32 (53%)
- **Integration tests**: 22 (37%)
- **E2E tests**: 6 (10%)
- **Priority distribution**: P0: 22, P1: 25, P2: 13

## Test Scenarios by Acceptance Criteria

### AC1: Short form Git reference (owner/repo@ref#path)

#### Scenarios - Python

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.2-UNIT-001 | Unit | P0 | Parse valid short form reference | Core parsing logic |
| 008.2-UNIT-002 | Unit | P0 | Parse with semver tag (v1.0.0) | Version tag handling |
| 008.2-UNIT-003 | Unit | P0 | Parse with branch name (main) | Branch reference |
| 008.2-UNIT-004 | Unit | P1 | Parse with commit SHA | Commit pinning |
| 008.2-UNIT-005 | Unit | P1 | Parse with nested path (a/b/c.json) | Deep paths |
| 008.2-UNIT-006 | Unit | P0 | Error on missing @ delimiter | Syntax validation |
| 008.2-UNIT-007 | Unit | P0 | Error on missing # delimiter | Syntax validation |
| 008.2-UNIT-008 | Unit | P1 | Error on empty owner | Field validation |

#### Scenarios - Rust

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.2-UNIT-009 | Unit | P0 | Parse valid short form (Rust parity) | Cross-runtime |
| 008.2-UNIT-010 | Unit | P0 | Parse with semver tag (Rust parity) | Cross-runtime |
| 008.2-UNIT-011 | Unit | P0 | Parse error handling (Rust parity) | Cross-runtime |

### AC2: Full URL Git reference (git+protocol://host/repo.git@ref#path)

#### Scenarios - Python

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.2-UNIT-012 | Unit | P0 | Parse git+https:// URL | HTTPS protocol |
| 008.2-UNIT-013 | Unit | P0 | Parse git+ssh:// URL | SSH protocol |
| 008.2-UNIT-014 | Unit | P1 | Parse GitLab URL | Non-GitHub host |
| 008.2-UNIT-015 | Unit | P1 | Parse Bitbucket URL | Non-GitHub host |
| 008.2-UNIT-016 | Unit | P1 | Parse with .git suffix | URL normalization |
| 008.2-UNIT-017 | Unit | P1 | Parse without .git suffix | URL normalization |
| 008.2-UNIT-018 | Unit | P0 | Error on invalid protocol | Protocol validation |

#### Scenarios - Rust

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.2-UNIT-019 | Unit | P0 | Parse git+https:// (Rust parity) | Cross-runtime |
| 008.2-UNIT-020 | Unit | P0 | Parse git+ssh:// (Rust parity) | Cross-runtime |

### AC3: fsspec URI support (S3, GCS, Azure, HTTP)

#### Scenarios - URI Detection

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.2-UNIT-026 | Unit | P0 | Detect s3:// as fsspec URI | URI routing |
| 008.2-UNIT-027 | Unit | P0 | Detect gs:// as fsspec URI | URI routing |
| 008.2-UNIT-028 | Unit | P0 | Detect az:// as fsspec URI | URI routing |
| 008.2-UNIT-029 | Unit | P0 | Detect https:// as fsspec URI | URI routing |
| 008.2-UNIT-030 | Unit | P1 | Detect file:// as fsspec URI | URI routing |
| 008.2-UNIT-031 | Unit | P0 | Detect Git short form (not fsspec) | URI routing |
| 008.2-UNIT-032 | Unit | P0 | Detect git+https:// as Git (not fsspec) | URI routing |

#### Scenarios - fsspec Fetching

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.2-INT-020 | Integration | P0 | Fetch schema from S3 (mocked) | S3 integration |
| 008.2-INT-021 | Integration | P0 | Fetch schema from GCS (mocked) | GCS integration |
| 008.2-INT-022 | Integration | P1 | Fetch schema from Azure (mocked) | Azure integration |
| 008.2-INT-023 | Integration | P0 | Fetch schema from HTTPS | HTTP integration |
| 008.2-INT-024 | Integration | P1 | Fetch schema from file:// | Local file |
| 008.2-INT-025 | Integration | P0 | fsspec bucket not found error | Error handling |
| 008.2-INT-026 | Integration | P0 | fsspec path not found error | Error handling |

### AC4: Multiple schemas with deep merge (mixed sources)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.2-INT-001 | Integration | P0 | Resolve single uses: reference | Basic resolution |
| 008.2-INT-002 | Integration | P0 | Resolve multiple uses: (list) | Multiple resolution |
| 008.2-INT-003 | Integration | P0 | Merge order: first is base, last wins | Merge semantics |
| 008.2-INT-004 | Integration | P0 | Mixed Git and fsspec sources | Source mixing |
| 008.2-INT-005 | Integration | P1 | Mixed local file and S3 | Source mixing |
| 008.2-INT-019 | Integration | P1 | Empty uses: list | Edge case |

### AC5: Private repository access (SSH key auth)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.2-INT-006 | Integration | P0 | Clone with GIT_SSH_KEY env var | Primary auth |
| 008.2-INT-007 | Integration | P0 | Clone with GIT_SSH_KEY_PATH env var | File-based auth |
| 008.2-INT-008 | Integration | P1 | SSH key with passphrase (error) | Unsupported case |
| 008.2-INT-009 | Integration | P0 | No SSH key for private repo (error) | Auth failure |
| 008.2-UNIT-021 | Unit | P0 | GIT_SSH_COMMAND construction | SSH config |
| 008.2-UNIT-022 | Unit | P1 | SSH key file permissions (0600) | Security |

### AC6: Schema caching

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.2-UNIT-023 | Unit | P1 | Cache hit returns cached schema | Performance |
| 008.2-UNIT-024 | Unit | P1 | Cache key format correctness | Cache key |
| 008.2-INT-010 | Integration | P1 | Cache miss triggers Git fetch | Fetch behavior |
| 008.2-INT-011 | Integration | P2 | TTL-based cache invalidation | Cache expiry |
| 008.2-INT-012 | Integration | P2 | Manual cache clear | Cache control |

### AC7: Error handling

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.2-UNIT-025 | Unit | P0 | Invalid reference syntax error | User feedback |
| 008.2-INT-013 | Integration | P0 | Repository not found error | 404 handling |
| 008.2-INT-014 | Integration | P0 | Path not found in repo error | File 404 |
| 008.2-INT-015 | Integration | P1 | Authentication failure error | Auth errors |
| 008.2-INT-016 | Integration | P2 | Network timeout error | Network issues |

### AC8-9: Integration with llamaextract.extract and schema.merge

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.2-INT-017 | Integration | P0 | schema.uses in llamaextract.extract | Action wiring |
| 008.2-INT-018 | Integration | P1 | schema.uses in schema.merge action | Action wiring |

### AC10: Cross-runtime parity

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.2-PARITY-001 | Integration | P0 | Python/Rust parse same reference | Parity test |
| 008.2-PARITY-002 | Integration | P0 | Python/Rust produce identical schema | Output parity |

### E2E Validation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.2-E2E-001 | E2E | P0 | YAML agent with uses: from GitHub | Critical path |
| 008.2-E2E-002 | E2E | P1 | YAML agent with private SSH repo | Private repo |
| 008.2-E2E-003 | E2E | P1 | YAML agent with uses: from S3 | Cloud storage path |
| 008.2-E2E-004 | E2E | P2 | YAML agent with mixed Git + S3 sources | Mixed sources |

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| SSH key exposure | UNIT-021,022, INT-006,007 | Secure handling |
| Git command injection | UNIT-025 | Input validation |
| Cache poisoning | UNIT-023,024 | Cache key integrity |
| Network reliability | INT-016 | Timeout handling |
| Cloud credential exposure | INT-020,021,022 | Env var isolation |
| fsspec URI injection | UNIT-026-032 | URI validation |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on parsing + URI detection)
   - UNIT-001 through UNIT-013, UNIT-018-021,025-032
2. **P0 Integration tests** (Git + fsspec operations)
   - INT-001,002,003,004,006,007,009,013,014,017,020,021,023,025,026
3. **P0 Parity tests** (cross-runtime)
   - PARITY-001,002
4. **P0 E2E tests** (critical path)
   - E2E-001
5. **P1 tests in order**
6. **P2+ as time permits**

## Test Data Requirements

| Data | Description | Source |
|------|-------------|--------|
| Local Git Repo | Test repository | pytest fixture (tmp_path) |
| Test Schema | Sample JSON Schema | fixtures/test-schema.json |
| Mock SSH Key | Test authentication | fixtures/test_rsa (generated) |
| Public Repo | GitHub test repo | github.com/test-org/schemas |
| Mock S3 Bucket | Mocked S3 storage | moto/localstack fixture |
| Mock GCS Bucket | Mocked GCS storage | gcsfs mock fixture |
| Mock Azure Container | Mocked Azure storage | adlfs mock fixture |
| HTTP Schema Server | Local HTTP server | pytest httpserver fixture |

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Cross-runtime parity explicitly tested
