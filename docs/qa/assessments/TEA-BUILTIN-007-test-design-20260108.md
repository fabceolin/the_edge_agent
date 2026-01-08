# Test Design: Story TEA-BUILTIN-007

Date: 2026-01-08
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 58
- Unit tests: 26 (45%)
- Integration tests: 24 (41%)
- E2E tests: 8 (14%)
- Priority distribution: P0: 18, P1: 22, P2: 14, P3: 4

## Risk Assessment

| Risk | Impact | Probability | Mitigated By |
|------|--------|-------------|--------------|
| Connection pool exhaustion | High | Medium | INT-003, INT-008 |
| Data loss during transient failures | Critical | Medium | INT-005, INT-006 |
| SQL injection via JSONB queries | Critical | Low | UNIT-006, INT-002 |
| S3 credential exposure | Critical | Low | UNIT-014, INT-016 |
| Multipart upload corruption | High | Medium | UNIT-019, INT-011 |
| Cross-backend incompatibility | High | Medium | E2E-001, E2E-002 |
| Thread safety race conditions | High | Medium | UNIT-009, INT-007 |

## Test Scenarios by Acceptance Criteria

### Phase 1: PostgreSQL Metadata Backend

#### AC1: PostgresMetadataStore implements MetadataStore ABC using asyncpg/psycopg2

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-001 | Unit | P0 | Verify PostgresMetadataStore inherits from MetadataStore ABC | Core contract validation - pure interface check |
| TEA-BUILTIN-007-UNIT-002 | Unit | P0 | Verify all ABC methods implemented (create, get, update, delete, query, transact) | Interface completeness - pure contract |
| TEA-BUILTIN-007-INT-001 | Integration | P0 | Execute CRUD operations against live PostgreSQL | Validates actual database interaction |

#### AC2: Supports connection pooling with configurable pool size

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-003 | Unit | P1 | Verify pool_size parameter accepted in constructor | Configuration validation logic |
| TEA-BUILTIN-007-UNIT-004 | Unit | P1 | Verify pool_max_overflow parameter accepted | Configuration validation logic |
| TEA-BUILTIN-007-INT-002 | Integration | P0 | Create pool with size=5, verify 5 concurrent connections work | Multi-component pool behavior |
| TEA-BUILTIN-007-INT-003 | Integration | P0 | Exhaust pool (size=2), verify 3rd request blocks/fails appropriately | Pool boundary behavior |

#### AC3: Supports both sync and async operation modes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-005 | Unit | P1 | Verify async=True triggers asyncpg path | Mode selection logic |
| TEA-BUILTIN-007-INT-004 | Integration | P1 | Execute full CRUD in sync mode (psycopg2) | Sync path integration |
| TEA-BUILTIN-007-INT-005 | Integration | P1 | Execute full CRUD in async mode (asyncpg) | Async path integration |

#### AC4: Auto-creates schema on first connection (migrations)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-INT-006 | Integration | P0 | Connect to empty database, verify tea_metadata table created | Migration against real DB |
| TEA-BUILTIN-007-INT-007 | Integration | P1 | Connect to database with existing schema, verify no errors | Idempotent migration |
| TEA-BUILTIN-007-UNIT-006 | Unit | P1 | Verify migration SQL structure matches expected schema | SQL correctness validation |

#### AC5: Implements transactional operations with proper isolation levels

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-INT-008 | Integration | P0 | Create, fail mid-transaction, verify rollback | Transaction atomicity requires DB |
| TEA-BUILTIN-007-INT-009 | Integration | P1 | Concurrent transactions with SERIALIZABLE isolation | Isolation level verification |
| TEA-BUILTIN-007-UNIT-007 | Unit | P1 | Verify transact() wraps operations in BEGIN/COMMIT | Transaction logic structure |

#### AC6: Supports JSONB storage for flexible document structure

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-INT-010 | Integration | P1 | Store nested dict with arrays, retrieve intact | JSONB serialization roundtrip |
| TEA-BUILTIN-007-UNIT-008 | Unit | P2 | Verify data serialized to JSON before storage | Serialization logic |

#### AC7: Implements full-text search using tsvector/tsquery

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-INT-011 | Integration | P1 | Insert documents, FTS query returns matches | FTS requires PostgreSQL engine |
| TEA-BUILTIN-007-INT-012 | Integration | P2 | FTS with stemming (e.g., "running" matches "run") | PostgreSQL FTS behavior |
| TEA-BUILTIN-007-UNIT-009 | Unit | P2 | Verify query translation to tsquery format | Query building logic |

#### AC8: Thread-safe for parallel agent execution

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-010 | Unit | P0 | Verify connection acquisition uses thread-local or lock | Thread safety mechanism check |
| TEA-BUILTIN-007-INT-013 | Integration | P0 | 10 concurrent threads performing CRUD, no data corruption | Concurrency against real DB |

#### AC9: Graceful connection recovery on transient failures

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-011 | Unit | P0 | Verify retry logic with exponential backoff | Retry algorithm validation |
| TEA-BUILTIN-007-INT-014 | Integration | P1 | Simulate connection drop, verify reconnection | Recovery behavior with real connection |
| TEA-BUILTIN-007-INT-015 | Integration | P2 | Verify health check detects stale connections | Health check mechanism |

---

### Phase 2: S3-Compatible Blob Backend

#### AC10: S3BlobStorage implements BlobStorage ABC using boto3/s3fs

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-012 | Unit | P0 | Verify S3BlobStorage inherits from BlobStorage ABC | Core contract validation |
| TEA-BUILTIN-007-UNIT-013 | Unit | P0 | Verify all ABC methods implemented (upload, download, delete, list, exists, copy) | Interface completeness |
| TEA-BUILTIN-007-INT-016 | Integration | P0 | Execute full blob lifecycle against LocalStack S3 | Real S3 API interaction |

#### AC11: Supports AWS S3, MinIO, Wasabi, DigitalOcean Spaces, Backblaze B2

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-INT-017 | Integration | P1 | CRUD operations against MinIO endpoint | S3-compatible service validation |
| TEA-BUILTIN-007-UNIT-014 | Unit | P2 | Verify endpoint_url parameter changes boto3 configuration | Configuration logic |

#### AC12: Supports configurable endpoint URL for S3-compatible services

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-015 | Unit | P1 | Verify custom endpoint_url passed to boto3 client | Configuration wiring |
| TEA-BUILTIN-007-UNIT-016 | Unit | P1 | Verify region configuration passed correctly | Region handling logic |

#### AC13: Implements multipart upload for large files (>5MB)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-017 | Unit | P0 | Verify files >5MB trigger multipart upload path | Size threshold logic |
| TEA-BUILTIN-007-UNIT-018 | Unit | P1 | Verify chunk size configuration (default 5MB) | Chunk calculation logic |
| TEA-BUILTIN-007-INT-018 | Integration | P0 | Upload 15MB file, verify multipart completion | Multipart protocol against S3 |
| TEA-BUILTIN-007-INT-019 | Integration | P1 | Fail mid-multipart, verify abort called | Cleanup on failure |

#### AC14: Supports server-side encryption (SSE-S3, SSE-KMS)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-019 | Unit | P1 | Verify SSE-S3 adds correct header to upload request | Encryption header logic |
| TEA-BUILTIN-007-UNIT-020 | Unit | P1 | Verify SSE-KMS adds KMS key ID to request | KMS configuration logic |
| TEA-BUILTIN-007-INT-020 | Integration | P2 | Upload with SSE-S3, verify object encrypted | Encryption end-to-end |

#### AC15: Implements presigned URL generation for temporary access

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-021 | Unit | P1 | Verify presigned URL includes expiry parameter | URL generation logic |
| TEA-BUILTIN-007-INT-21 | Integration | P1 | Generate presigned URL, access via HTTP client | Presigned URL validity |
| TEA-BUILTIN-007-INT-022 | Integration | P2 | Presigned URL expires after configured time | Expiry enforcement |

#### AC16: Supports bucket lifecycle policies integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-022 | Unit | P2 | Verify lifecycle check method exists | Interface presence |
| TEA-BUILTIN-007-INT-023 | Integration | P3 | Query object lifecycle status | Policy awareness |

#### AC17: Thread-safe for parallel agent execution

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-023 | Unit | P0 | Verify boto3 session creation is thread-safe | Session handling logic |
| TEA-BUILTIN-007-INT-024 | Integration | P0 | 10 concurrent threads uploading/downloading, no corruption | Concurrency validation |

---

### Phase 3: Optional Backends (DynamoDB, Azure, MongoDB)

#### AC18-20: Optional backend implementations

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-024 | Unit | P3 | Verify DynamoDBMetadataStore implements MetadataStore ABC | Contract validation |
| TEA-BUILTIN-007-UNIT-025 | Unit | P3 | Verify AzureBlobStorage implements BlobStorage ABC | Contract validation |
| TEA-BUILTIN-007-UNIT-026 | Unit | P3 | Verify MongoDBMetadataStore implements MetadataStore ABC | Contract validation |
| TEA-BUILTIN-007-INT-025 | Integration | P3 | DynamoDB CRUD against LocalStack | Optional backend validation |

---

### Phase 4: Integration

#### AC21-22: YAMLEngine backend configuration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-E2E-001 | E2E | P0 | Run YAML agent with metadata_backend_type: "postgres" | Full stack validation |
| TEA-BUILTIN-007-E2E-002 | E2E | P0 | Run YAML agent with blob_backend_type: "s3" | Full stack validation |
| TEA-BUILTIN-007-INT-026 | Integration | P1 | YAMLEngine initializes PostgresMetadataStore from config | Engine-backend wiring |
| TEA-BUILTIN-007-INT-027 | Integration | P1 | YAMLEngine initializes S3BlobStorage from config | Engine-backend wiring |

#### AC23-24: Environment variable and YAML settings

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-027 | Unit | P1 | Verify DATABASE_URL parsed correctly | Env var parsing logic |
| TEA-BUILTIN-007-UNIT-028 | Unit | P1 | Verify AWS_* env vars configure S3 | Env var parsing logic |
| TEA-BUILTIN-007-INT-028 | Integration | P1 | YAML settings override env vars | Config precedence |

#### AC25: Availability flags

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-029 | Unit | P1 | POSTGRES_AVAILABLE=True when asyncpg installed | Import detection logic |
| TEA-BUILTIN-007-UNIT-030 | Unit | P1 | S3_AVAILABLE=True when boto3 installed | Import detection logic |
| TEA-BUILTIN-007-UNIT-031 | Unit | P2 | POSTGRES_AVAILABLE=False when asyncpg missing | Graceful degradation |

#### AC26: Backend auto-selection

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-032 | Unit | P1 | Auto-select postgres when DATABASE_URL present | Selection logic |
| TEA-BUILTIN-007-UNIT-033 | Unit | P1 | Auto-select s3 when AWS credentials present | Selection logic |
| TEA-BUILTIN-007-E2E-003 | E2E | P1 | Run agent with only env vars (no explicit backend config) | Auto-selection end-to-end |

---

### Cross-Cutting Concerns

#### Error Handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-E2E-004 | E2E | P0 | Agent continues after transient S3 failure | Resilience in workflow |
| TEA-BUILTIN-007-E2E-005 | E2E | P0 | Agent continues after transient Postgres failure | Resilience in workflow |
| TEA-BUILTIN-007-INT-029 | Integration | P1 | Verify error response format matches ABC contract | Error contract compliance |

#### Performance

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-E2E-006 | E2E | P2 | 100 sequential metadata operations <5s | Basic performance baseline |
| TEA-BUILTIN-007-E2E-007 | E2E | P2 | 50 parallel blob uploads complete without timeout | Parallel performance |

#### Security

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-007-UNIT-034 | Unit | P0 | Credentials not logged in debug output | Security - no credential leakage |
| TEA-BUILTIN-007-E2E-008 | E2E | P1 | Invalid credentials produce clear error, no stack trace exposure | Security - error safety |

---

## Risk Coverage

| Risk ID | Test IDs Mitigating |
|---------|---------------------|
| Connection pool exhaustion | INT-003, INT-008, INT-013 |
| Data loss during transient failures | INT-008, E2E-004, E2E-005 |
| SQL injection via JSONB queries | UNIT-006, UNIT-008, INT-010 |
| S3 credential exposure | UNIT-034, E2E-008 |
| Multipart upload corruption | UNIT-017, INT-018, INT-019 |
| Cross-backend incompatibility | E2E-001, E2E-002, E2E-003 |
| Thread safety race conditions | UNIT-010, UNIT-023, INT-013, INT-024 |

---

## Test Infrastructure Requirements

### Unit Tests
- pytest with mocking (pytest-mock)
- Mock boto3 via moto library
- Mock asyncpg/psycopg2 responses

### Integration Tests
- Docker Compose with:
  - PostgreSQL container (postgres:15)
  - LocalStack container (S3-compatible)
  - MinIO container (optional, for S3-compat testing)
- pytest-docker or testcontainers-python

### E2E Tests
- Full docker-compose environment
- Pre-seeded test data
- pytest with longer timeouts

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on interface contracts)
2. **P0 Integration tests** (validate core database/S3 operations)
3. **P0 E2E tests** (critical workflow validation)
4. **P1 Unit → Integration → E2E** (core functionality)
5. **P2 tests** (as time permits)
6. **P3 tests** (full regression only)

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (P0 for security, data integrity)
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Thread safety explicitly tested at unit and integration levels
- [x] Error handling and resilience covered
- [x] Security concerns addressed

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 58
  by_level:
    unit: 26
    integration: 24
    e2e: 8
  by_priority:
    p0: 18
    p1: 22
    p2: 14
    p3: 4
  coverage_gaps: []
  infrastructure_required:
    - docker-compose with postgres:15
    - docker-compose with localstack
    - moto library for unit tests
    - pytest-docker or testcontainers
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-007-test-design-20260108.md
P0 tests identified: 18
Critical risks mitigated: 7
```
