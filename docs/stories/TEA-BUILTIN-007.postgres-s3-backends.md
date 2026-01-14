# Story TEA-BUILTIN-007: PostgreSQL and S3 Backend Implementations

## Status

**✅ Implemented**

_Status updated: 2026-01-13 - Implementation verified in codebase. TEA-BUILTIN-006 dependency resolved._
- `python/src/the_edge_agent/memory/postgres.py` - PostgreSQL LTM backend
- `python/src/the_edge_agent/memory/catalog_postgres.py` - PostgreSQL catalog backend
- `python/src/the_edge_agent/memory/blob/` - S3/GCS blob storage module
- `python/src/the_edge_agent/memory/blob/gcs.py` - GCS blob storage

> **Validation Notes (2026-01-08)**:
> - ✅ Story format complete with clear user story
> - ✅ 26 acceptance criteria defined across 4 phases
> - ✅ Tasks/subtasks with file paths specified
> - ✅ Dev notes with schema and configuration examples
> - ✅ QA test design completed (58 test scenarios)
> - ✅ Dependencies resolved - TEA-BUILTIN-006 implemented

## Story

**As a** YAML agent developer deploying outside Firebase infrastructure,
**I want** PostgreSQL metadata storage and S3-compatible blob storage backends,
**so that** I can run the same agents on AWS, self-hosted infrastructure, or hybrid cloud environments without Firebase dependencies.

## Acceptance Criteria

### Phase 1: PostgreSQL Metadata Backend

1. `PostgresMetadataStore` implements MetadataStore ABC using asyncpg/psycopg2
2. Supports connection pooling with configurable pool size
3. Supports both sync and async operation modes
4. Auto-creates schema on first connection (migrations)
5. Implements transactional operations with proper isolation levels
6. Supports JSONB storage for flexible document structure
7. Implements full-text search using tsvector/tsquery
8. Thread-safe for parallel agent execution
9. Graceful connection recovery on transient failures

### Phase 2: S3-Compatible Blob Backend

10. `S3BlobStorage` implements BlobStorage ABC using boto3/s3fs
11. Supports AWS S3, MinIO, Wasabi, DigitalOcean Spaces, Backblaze B2
12. Supports configurable endpoint URL for S3-compatible services
13. Implements multipart upload for large files (>5MB)
14. Supports server-side encryption (SSE-S3, SSE-KMS)
15. Implements presigned URL generation for temporary access
16. Supports bucket lifecycle policies integration
17. Thread-safe for parallel agent execution

### Phase 3: Additional Backends (Optional)

18. `DynamoDBMetadataStore` implements MetadataStore for AWS-native deployments
19. `AzureBlobStorage` implements BlobStorage for Azure environments
20. `MongoDBMetadataStore` implements MetadataStore for document-oriented storage

### Phase 4: Integration

21. YAMLEngine accepts `metadata_backend_type: "postgres"` configuration
22. YAMLEngine accepts `blob_backend_type: "s3"` configuration
23. Environment variable configuration for credentials (standard AWS/PG patterns)
24. YAML settings support for all backend options
25. Availability flags: `POSTGRES_AVAILABLE`, `S3_AVAILABLE`, `DYNAMODB_AVAILABLE`
26. Backend auto-selection based on available credentials

## Dependencies

**Blocked By**:
- TEA-BUILTIN-006 (Firebase Agent Memory Layer) - establishes ABC interfaces

**Blocks**:
- Multi-cloud agent deployments
- Self-hosted agent infrastructure

## Tasks / Subtasks

### Week 1: PostgreSQL Backend (AC 1-9)

- [ ] Task 1: Implement PostgresMetadataStore core (AC 1-4)
  - [ ] Create `src/the_edge_agent/memory/metadata/postgres.py`
  - [ ] Implement connection pooling with asyncpg
  - [ ] Implement sync wrapper using psycopg2
  - [ ] Create auto-migration for schema creation
  - [ ] Schema: `tea_metadata` table with id, collection, doc_id, data (JSONB), created_at, updated_at

- [ ] Task 2: Implement PostgresMetadataStore operations (AC 5-7)
  - [ ] Implement create/get/update/delete with proper transactions
  - [ ] Implement query with filter translation to SQL
  - [ ] Implement transact with SERIALIZABLE isolation
  - [ ] Implement FTS using tsvector index on data column

- [ ] Task 3: Implement PostgresMetadataStore resilience (AC 8-9)
  - [ ] Add connection retry with exponential backoff
  - [ ] Add connection health checks
  - [ ] Add thread-safe connection acquisition
  - [ ] Add graceful shutdown with connection draining

### Week 2: S3 Backend (AC 10-17)

- [ ] Task 4: Implement S3BlobStorage core (AC 10-12)
  - [ ] Create `src/the_edge_agent/memory/blob/s3.py`
  - [ ] Implement boto3-based operations (upload, download, delete, list, exists, copy)
  - [ ] Add configurable endpoint_url for S3-compatible services
  - [ ] Add region and signature version configuration

- [ ] Task 5: Implement S3BlobStorage advanced features (AC 13-16)
  - [ ] Implement multipart upload for files > 5MB
  - [ ] Add SSE-S3 and SSE-KMS encryption support
  - [ ] Implement presigned URL generation with expiry
  - [ ] Add lifecycle policy awareness (check object status)

- [ ] Task 6: Implement S3BlobStorage resilience (AC 17)
  - [ ] Add retry with exponential backoff for transient errors
  - [ ] Add thread-safe session management
  - [ ] Add connection pooling via botocore

### Week 3: Optional Backends (AC 18-20)

- [ ] Task 7: Implement DynamoDBMetadataStore (AC 18) - Optional
  - [ ] Create `src/the_edge_agent/memory/metadata/dynamodb.py`
  - [ ] Implement single-table design pattern
  - [ ] Implement GSI for collection queries

- [ ] Task 8: Implement AzureBlobStorage (AC 19) - Optional
  - [ ] Create `src/the_edge_agent/memory/blob/azure.py`
  - [ ] Implement using azure-storage-blob SDK

- [ ] Task 9: Implement MongoDBMetadataStore (AC 20) - Optional
  - [ ] Create `src/the_edge_agent/memory/metadata/mongodb.py`
  - [ ] Implement using pymongo with connection pooling

### Week 4: Integration (AC 21-26)

- [ ] Task 10: Update YAMLEngine (AC 21-24)
  - [ ] Add postgres/s3 backend type handling
  - [ ] Add environment variable parsing for credentials
  - [ ] Add YAML settings parsing for backend config

- [ ] Task 11: Add availability flags (AC 25)
  - [ ] Add `_check_postgres_available()` function
  - [ ] Add `_check_s3_available()` function
  - [ ] Add `_check_dynamodb_available()` function
  - [ ] Export flags from memory/__init__.py

- [ ] Task 12: Implement auto-selection (AC 26)
  - [ ] Detect available credentials in environment
  - [ ] Select appropriate backend based on what's configured
  - [ ] Log backend selection for debugging

- [ ] Task 13: Write tests
  - [ ] Unit tests with mocked boto3/asyncpg
  - [ ] Integration tests with LocalStack (S3)
  - [ ] Integration tests with PostgreSQL container

- [ ] Task 14: Update documentation
  - [ ] Update CLAUDE.md with new backends
  - [ ] Add deployment examples for AWS, self-hosted

## Dev Notes

### PostgreSQL Schema

```sql
-- Auto-created on first connection
CREATE TABLE IF NOT EXISTS tea_metadata (
    id SERIAL PRIMARY KEY,
    collection VARCHAR(255) NOT NULL,
    doc_id VARCHAR(255) NOT NULL,
    data JSONB NOT NULL DEFAULT '{}',
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    UNIQUE(collection, doc_id)
);

-- Full-text search index
CREATE INDEX IF NOT EXISTS tea_metadata_fts_idx
ON tea_metadata USING GIN (to_tsvector('english', data::text));

-- Collection index for queries
CREATE INDEX IF NOT EXISTS tea_metadata_collection_idx
ON tea_metadata (collection);

-- JSONB index for property queries
CREATE INDEX IF NOT EXISTS tea_metadata_data_idx
ON tea_metadata USING GIN (data);
```

### S3 Configuration Examples

```python
# AWS S3
engine = YAMLEngine(
    blob_backend_type="s3",
    s3_bucket="my-agent-storage",
    s3_region="us-east-1",
    # Uses AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY from env
)

# MinIO (self-hosted S3)
engine = YAMLEngine(
    blob_backend_type="s3",
    s3_bucket="agent-storage",
    s3_endpoint_url="http://minio:9000",
    s3_access_key="minioadmin",
    s3_secret_key="minioadmin",
)

# DigitalOcean Spaces
engine = YAMLEngine(
    blob_backend_type="s3",
    s3_bucket="my-space",
    s3_endpoint_url="https://nyc3.digitaloceanspaces.com",
    s3_region="nyc3",
)
```

### PostgreSQL Configuration Examples

```python
# Standard PostgreSQL
engine = YAMLEngine(
    metadata_backend_type="postgres",
    postgres_url="postgresql://user:pass@localhost:5432/agents",
)

# With connection pool settings
engine = YAMLEngine(
    metadata_backend_type="postgres",
    postgres_url="postgresql://user:pass@localhost:5432/agents",
    postgres_pool_size=10,
    postgres_pool_max_overflow=20,
)

# Async mode (for async agents)
engine = YAMLEngine(
    metadata_backend_type="postgres",
    postgres_url="postgresql://user:pass@localhost:5432/agents",
    postgres_async=True,
)
```

### YAML Settings

```yaml
settings:
  backends:
    metadata:
      type: postgres
      url: "${DATABASE_URL}"
      pool_size: 10
      async: false

    blob:
      type: s3
      bucket: "${S3_BUCKET}"
      region: us-east-1
      endpoint_url: null  # Use AWS default
      encryption: SSE-S3
```

### Environment Variables

| Variable | Backend | Description |
|----------|---------|-------------|
| `DATABASE_URL` | postgres | PostgreSQL connection string |
| `POSTGRES_HOST` | postgres | Alternative: host |
| `POSTGRES_PORT` | postgres | Alternative: port (default 5432) |
| `POSTGRES_USER` | postgres | Alternative: username |
| `POSTGRES_PASSWORD` | postgres | Alternative: password |
| `POSTGRES_DB` | postgres | Alternative: database name |
| `AWS_ACCESS_KEY_ID` | s3 | AWS access key |
| `AWS_SECRET_ACCESS_KEY` | s3 | AWS secret key |
| `AWS_DEFAULT_REGION` | s3 | AWS region |
| `S3_ENDPOINT_URL` | s3 | S3-compatible endpoint |

### Existing TEA Patterns to Follow

1. **Backend ABC Pattern** (from TEA-BUILTIN-006):
   - Implement MetadataStore ABC from `memory/metadata/base.py`
   - Implement BlobStorage ABC from `memory/blob/base.py`
   - Return `{"success": bool, "error": str, "error_type": str}` format

2. **Availability Check Pattern** (from `memory/graph.py`):
   ```python
   def _check_postgres_available() -> bool:
       try:
           import asyncpg  # noqa: F401
           return True
       except ImportError:
           return False

   POSTGRES_AVAILABLE = _check_postgres_available()
   ```

3. **Graceful Degradation**:
   - Return informative error when backend not installed
   - Include installation instructions in error message

### Testing Strategy

- **Unit Tests**: Mock boto3/asyncpg, test logic in isolation
- **Integration Tests**:
  - LocalStack for S3 (docker-compose)
  - PostgreSQL container (docker-compose)
- **Test Fixtures**: Reuse fixtures from TEA-BUILTIN-006

## Files to Create

```
src/the_edge_agent/
├── memory/
│   ├── metadata/
│   │   ├── postgres.py       # PostgresMetadataStore
│   │   ├── dynamodb.py       # DynamoDBMetadataStore (optional)
│   │   └── mongodb.py        # MongoDBMetadataStore (optional)
│   └── blob/
│       ├── s3.py             # S3BlobStorage
│       └── azure.py          # AzureBlobStorage (optional)
```

## Files to Modify

- `src/the_edge_agent/memory/__init__.py` - Add new backend exports and availability flags
- `src/the_edge_agent/memory/metadata/__init__.py` - Add postgres backend
- `src/the_edge_agent/memory/blob/__init__.py` - Add s3 backend
- `src/the_edge_agent/yaml_engine.py` - Add backend type handling
- `the_edge_agent/CLAUDE.md` - Document new backends

## Optional Dependencies

```toml
# pyproject.toml extras
[project.optional-dependencies]
postgres = ["asyncpg>=0.29.0", "psycopg2-binary>=2.9.0"]
s3 = ["boto3>=1.34.0", "s3fs>=2024.2.0"]
dynamodb = ["boto3>=1.34.0"]
azure = ["azure-storage-blob>=12.19.0"]
mongodb = ["pymongo>=4.6.0"]

# Combined
cloud = ["asyncpg", "psycopg2-binary", "boto3", "s3fs"]
all-backends = ["asyncpg", "psycopg2-binary", "boto3", "s3fs", "azure-storage-blob", "pymongo"]
```

## Estimated Effort

| Task | Description | Hours |
|------|-------------|-------|
| 1-3 | PostgresMetadataStore | 12 |
| 4-6 | S3BlobStorage | 10 |
| 7 | DynamoDBMetadataStore (optional) | 6 |
| 8 | AzureBlobStorage (optional) | 4 |
| 9 | MongoDBMetadataStore (optional) | 6 |
| 10-12 | Integration | 6 |
| 13 | Tests | 8 |
| 14 | Documentation | 4 |
| **Total (required)** | | **40 hours** |
| **Total (with optional)** | | **56 hours** |

## QA Notes

### Test Coverage Summary

- **Total test scenarios**: 58
- **Unit tests**: 26 (45%) - Interface contracts, configuration logic, security checks
- **Integration tests**: 24 (41%) - Database/S3 operations, connection pooling, concurrency
- **E2E tests**: 8 (14%) - Full workflow validation with YAMLEngine

**Priority Distribution**: P0: 18, P1: 22, P2: 14, P3: 4

All 26 acceptance criteria have explicit test coverage with appropriate test levels.

### Risk Areas Identified

| Risk | Severity | Probability | Mitigation Tests |
|------|----------|-------------|------------------|
| Connection pool exhaustion | High | Medium | INT-003, INT-008, INT-013 |
| Data loss during transient failures | Critical | Medium | INT-008, E2E-004, E2E-005 |
| SQL injection via JSONB queries | Critical | Low | UNIT-006, UNIT-008, INT-010 |
| S3 credential exposure | Critical | Low | UNIT-034, E2E-008 |
| Multipart upload corruption | High | Medium | UNIT-017, INT-018, INT-019 |
| Cross-backend incompatibility | High | Medium | E2E-001, E2E-002, E2E-003 |
| Thread safety race conditions | High | Medium | UNIT-010, UNIT-023, INT-013, INT-024 |

### Recommended Test Scenarios

**P0 (Must Pass Before Merge)**:
1. PostgresMetadataStore ABC interface compliance (UNIT-001, UNIT-002)
2. S3BlobStorage ABC interface compliance (UNIT-012, UNIT-013)
3. CRUD operations against live PostgreSQL (INT-001)
4. Connection pool boundary behavior (INT-002, INT-003)
5. Transaction atomicity with rollback (INT-008)
6. Auto-schema migration on first connection (INT-006)
7. Thread safety under concurrent load (INT-013, INT-024)
8. Multipart upload for large files (UNIT-017, INT-018)
9. Credential non-exposure in logs (UNIT-034)
10. Full YAMLEngine integration (E2E-001, E2E-002)

**P1 (Core Functionality)**:
- Sync/async operation modes
- Connection recovery with exponential backoff
- S3-compatible service support (MinIO, etc.)
- Presigned URL generation
- Environment variable configuration
- Backend auto-selection logic

### Concerns and Blockers

**Blockers**:
- Story blocked by TEA-BUILTIN-006 (Firebase Agent Memory Layer) which establishes ABC interfaces

**Testing Infrastructure Requirements**:
- Docker Compose with PostgreSQL 15 container
- LocalStack container for S3-compatible testing
- MinIO container (optional, for additional S3-compat validation)
- moto library for unit test mocking
- pytest-docker or testcontainers-python

**Concerns**:
1. Optional backends (DynamoDB, Azure, MongoDB) have P3 priority - may not receive full test coverage
2. Performance baselines (E2E-006, E2E-007) are P2 - establish early to catch regressions
3. Security tests must pass before any merge - credential exposure is critical risk

### Test Design Reference

Full test design matrix: `docs/qa/assessments/TEA-BUILTIN-007-test-design-20260108.md`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-19 | 0.1 | Initial story draft | Claude (PO Agent) |
| 2026-01-08 | 0.2 | Added QA Notes section with test coverage analysis | Quinn (QA Agent) |
