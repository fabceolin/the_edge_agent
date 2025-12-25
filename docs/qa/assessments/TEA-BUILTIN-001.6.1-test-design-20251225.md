# Test Design: Story TEA-BUILTIN-001.6.1

**Date:** 2025-12-25
**Designer:** Quinn (Test Architect)
**Story:** Catalog Backend Protocol & Implementations

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 47 | 100% |
| Unit tests | 14 | 30% |
| Integration tests | 33 | 70% |
| E2E tests | 0 | 0% |

| Priority | Count |
|----------|-------|
| P0 | 16 |
| P1 | 24 |
| P2 | 7 |

### Test Level Rationale

- **No E2E tests**: This story defines internal infrastructure (catalog backends). User-facing E2E validation happens in parent story TEA-BUILTIN-001.6 when the full LTM system is integrated.
- **Heavy integration focus**: All catalog implementations interact with external storage (SQLite, Firestore, PostgreSQL, Supabase). Testing the protocol in isolation provides less value than testing actual storage interaction.
- **Unit tests for pure functions**: Content hash computation and entry ID generation are pure functions suitable for unit testing.

---

## Test Scenarios by Acceptance Criteria

### Catalog Protocol (AC-1 to AC-7)

#### AC-1: CatalogBackend Protocol

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-UNIT-001 | Unit | P1 | Verify `CatalogBackend` protocol defines all required methods | Protocol structure validation - no runtime behavior |
| 1.6.1-UNIT-002 | Unit | P1 | Verify protocol is `typing.Protocol` subclass for structural typing | Ensures duck typing works correctly |

#### AC-2: track_entry Method

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-001 | Integration | P0 | Create new entry with all fields populated | Core write path - data integrity critical |
| 1.6.1-INT-002 | Integration | P0 | Update existing entry (upsert behavior) | Data consistency on updates |
| 1.6.1-INT-003 | Integration | P1 | Create entry with optional fields as None | Handles minimal entry correctly |
| 1.6.1-INT-004 | Integration | P1 | Create entry with `expires_at` in future | TTL functionality validation |

#### AC-3: get_entry Method

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-005 | Integration | P0 | Get existing entry by key returns correct data | Core read path - must work |
| 1.6.1-INT-006 | Integration | P0 | Get non-existent key returns None (not error) | Error handling contract |
| 1.6.1-INT-007 | Integration | P1 | Get entry returns all fields with correct types | Data serialization roundtrip |

#### AC-4: list_entries Method

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-008 | Integration | P0 | List entries with prefix filter returns matches | Core listing functionality |
| 1.6.1-INT-009 | Integration | P1 | List entries with metadata filter | Complex query capability |
| 1.6.1-INT-010 | Integration | P1 | List entries respects limit parameter | Pagination support |
| 1.6.1-INT-011 | Integration | P2 | List entries with empty result returns empty list | Empty state handling |

#### AC-5: delete_entry Method

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-012 | Integration | P0 | Delete existing entry returns True | Core delete path |
| 1.6.1-INT-013 | Integration | P1 | Delete non-existent entry returns False | Idempotent delete behavior |
| 1.6.1-INT-014 | Integration | P1 | Deleted entry not returned by get_entry | Verify actual removal |

#### AC-6: get_changed_entries Method

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-015 | Integration | P1 | Get entries changed since snapshot | Incremental sync support |
| 1.6.1-INT-016 | Integration | P1 | Get changed entries with None snapshot returns all | Initial sync scenario |

#### AC-7: create_snapshot Method

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-017 | Integration | P1 | Create snapshot returns valid snapshot_id | Point-in-time capture |
| 1.6.1-INT-018 | Integration | P2 | Snapshot captures correct entry count and total bytes | Metadata accuracy |

---

### Content Hash (AC-8 to AC-9)

#### AC-8: Hash Computation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-UNIT-003 | Unit | P0 | `compute_content_hash()` returns `sha256:{hex}` format | Hash format contract |
| 1.6.1-UNIT-004 | Unit | P0 | Hash of string value is correct SHA-256 | Cryptographic correctness |
| 1.6.1-UNIT-005 | Unit | P1 | Hash of dict/list values serializes correctly | Complex type handling |

#### AC-9: Hash Determinism

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-UNIT-006 | Unit | P0 | Same value always produces same hash | Determinism is critical |
| 1.6.1-UNIT-007 | Unit | P0 | Dict with keys in different order produces same hash | sort_keys=True validation |
| 1.6.1-UNIT-008 | Unit | P1 | `generate_entry_id()` produces consistent key hash | Entry ID stability |

---

### SQLite Catalog (AC-10 to AC-13)

#### AC-10: SQLiteCatalog Class

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-019 | Integration | P0 | SQLiteCatalog implements all CatalogBackend methods | Protocol compliance |
| 1.6.1-INT-020 | Integration | P0 | Full CRUD cycle with SQLiteCatalog | End-to-end backend validation |

#### AC-11: SQLite Schema

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-021 | Integration | P1 | Schema creates `ltm_entries` table with correct columns | Schema correctness |
| 1.6.1-INT-022 | Integration | P1 | Schema creates `ltm_snapshots` table | Snapshot table exists |
| 1.6.1-INT-023 | Integration | P2 | Indexes created for key, expires_at, updated_at | Query performance |

#### AC-12: SQLite In-Memory

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-024 | Integration | P0 | SQLiteCatalog(":memory:") works for testing | Development/test mode |

#### AC-13: SQLite File

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-025 | Integration | P1 | SQLiteCatalog with file path persists data | Persistent storage mode |

---

### Firestore Catalog (AC-14 to AC-18)

#### AC-14: FirestoreCatalog Class

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-026 | Integration | P0 | FirestoreCatalog implements all CatalogBackend methods | Protocol compliance |
| 1.6.1-INT-027 | Integration | P0 | Full CRUD cycle with mocked Firestore | Backend functionality |

#### AC-15: Firestore Collections

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-028 | Integration | P1 | Operations use correct collection names | Collection structure |

#### AC-16: Firestore Transactions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-029 | Integration | P0 | Write operations wrapped in transactions | Data consistency |

#### AC-17: Collection Prefix

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-030 | Integration | P2 | Configurable prefix applied to collection names | Multi-tenant support |

#### AC-18: Firestore Project

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-031 | Integration | P1 | Configurable Firebase project ID | Deployment flexibility |

---

### PostgreSQL Catalog (AC-19 to AC-22)

#### AC-19: PostgresCatalog Class

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-032 | Integration | P0 | PostgresCatalog implements all CatalogBackend methods | Protocol compliance |
| 1.6.1-INT-033 | Integration | P0 | Full CRUD cycle with mocked connection | Backend functionality |

#### AC-20: Postgres Schema

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-034 | Integration | P1 | Schema migration creates tables with indexes | Schema correctness |

#### AC-21: Postgres Connection Pool

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-035 | Integration | P1 | Connection pooling configured correctly | Performance/scalability |

#### AC-22: Postgres Connection String

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-036 | Integration | P1 | Supports postgresql:// connection string | Standard connectivity |

---

### Supabase Catalog (AC-23 to AC-26)

#### AC-23: SupabaseCatalog Class

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-037 | Integration | P0 | SupabaseCatalog implements all CatalogBackend methods | Protocol compliance |
| 1.6.1-INT-038 | Integration | P0 | Full CRUD cycle with mocked HTTP | Backend functionality |

#### AC-24: Supabase REST API

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-039 | Integration | P1 | Uses REST API not direct Postgres | Architecture compliance |

#### AC-25: Supabase Auth

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-040 | Integration | P0 | Supports anon_key authentication | Core auth path |
| 1.6.1-INT-041 | Integration | P1 | Supports service_role authentication | Admin operations |

#### AC-26: Supabase RLS

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-042 | Integration | P2 | Compatible with Row Level Security policies | Security compliance |

---

### Factory (AC-27 to AC-29)

#### AC-27: Catalog Factory

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-UNIT-009 | Unit | P0 | `create_catalog_backend("sqlite", path=":memory:")` returns SQLiteCatalog | Factory pattern |
| 1.6.1-UNIT-010 | Unit | P0 | `create_catalog_backend("firestore", ...)` returns FirestoreCatalog | Factory pattern |
| 1.6.1-UNIT-011 | Unit | P1 | `create_catalog_backend("postgres", ...)` returns PostgresCatalog | Factory pattern |
| 1.6.1-UNIT-012 | Unit | P1 | `create_catalog_backend("supabase", ...)` returns SupabaseCatalog | Factory pattern |

#### AC-28: Backend Registration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-UNIT-013 | Unit | P1 | All four backends registered in factory | Factory completeness |
| 1.6.1-UNIT-014 | Unit | P2 | Unknown backend type raises ValueError | Error handling |

#### AC-29: YAML Config Parsing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.1-INT-043 | Integration | P1 | Factory parses YAML configuration block | YAML engine integration |

---

## Risk Coverage

| Risk | Mitigated By |
|------|--------------|
| Data corruption in catalog | 1.6.1-INT-001, 1.6.1-INT-002, 1.6.1-INT-005 |
| Hash collisions or instability | 1.6.1-UNIT-006, 1.6.1-UNIT-007 |
| Backend implementation drift | Protocol compliance tests for each backend |
| Firestore transaction failures | 1.6.1-INT-029 |
| Connection pool exhaustion | 1.6.1-INT-035 |
| Auth token handling errors | 1.6.1-INT-040, 1.6.1-INT-041 |

---

## Recommended Execution Order

1. **P0 Unit tests** (hash functions, factory basics) - fail fast on core logic
2. **P0 SQLite Integration tests** - validate reference implementation
3. **P0 Firestore/Postgres/Supabase Integration tests** - backend parity
4. **P1 tests** - comprehensive coverage
5. **P2 tests** - edge cases and polish

---

## Test Environment Requirements

| Backend | Unit Test Env | Integration Test Env |
|---------|--------------|---------------------|
| SQLite | N/A | In-memory SQLite (`:memory:`) |
| Firestore | Mock `google.cloud.firestore` | Firebase Emulator (optional) |
| PostgreSQL | Mock `asyncpg`/`psycopg` | Docker PostgreSQL 15+ |
| Supabase | Mock `httpx` | Local Supabase (optional) |

---

## Quality Checklist

- [x] Every AC has at least one test scenario
- [x] Test levels are appropriate (unit for pure functions, integration for storage)
- [x] No duplicate coverage across levels
- [x] Priorities align with data integrity and business risk
- [x] Test IDs follow `{epic}.{story}-{LEVEL}-{SEQ}` convention
- [x] Scenarios are atomic and independent
