# Test Design: Story TEA-WASM-003.3

Date: 2026-01-12
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 47
- Unit tests: 19 (40%)
- Integration tests: 18 (38%)
- E2E tests: 10 (21%)
- Priority distribution: P0: 15, P1: 19, P2: 10, P3: 3

## Story Context

This story implements an LTM (Long-Term Memory) backend for WASM environments using:
- **IndexedDB** as the catalog backend (offline-first)
- **OpenDAL** for blob storage (cloud sync)
- **DuckDB WASM** for analytics/search

Key risk factors:
- Browser API complexity (IndexedDB async nature)
- Offline/online state management
- Cross-component integration (3 subsystems)
- Data integrity across storage tiers

---

## Test Scenarios by Acceptance Criteria

### AC-1: Store Operation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-UNIT-001 | Unit | P0 | Validate `ltm_store_async` serializes JSON correctly | Pure serialization logic |
| WASM-003.3-UNIT-002 | Unit | P0 | Compute SHA-256 content hash correctly | Cryptographic hash function |
| WASM-003.3-UNIT-003 | Unit | P1 | Handle empty value storage | Edge case - empty string |
| WASM-003.3-INT-001 | Integration | P0 | Store small value (<1KB) inlines in catalog | Inlining decision + IndexedDB |
| WASM-003.3-INT-002 | Integration | P0 | Store large value (>=1KB) writes to blob storage | Blob storage integration |
| WASM-003.3-INT-003 | Integration | P1 | Store with metadata includes all metadata fields | Metadata persistence flow |

### AC-2: Retrieve Operation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-UNIT-004 | Unit | P0 | Retrieve returns default when key not found | Pure logic branch |
| WASM-003.3-INT-004 | Integration | P0 | Retrieve inlined value from catalog | IndexedDB read path |
| WASM-003.3-INT-005 | Integration | P0 | Retrieve large value from blob storage | OpenDAL read integration |
| WASM-003.3-INT-006 | Integration | P1 | Retrieve returns content_hash and metadata | Complete response structure |

### AC-3: Delete Operation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-UNIT-005 | Unit | P1 | Delete returns false for non-existent key | Pure logic branch |
| WASM-003.3-INT-007 | Integration | P0 | Delete removes inlined value from catalog | IndexedDB delete |
| WASM-003.3-INT-008 | Integration | P0 | Delete removes large value from blob storage | Cascading delete across systems |
| WASM-003.3-INT-009 | Integration | P1 | Delete clears IndexedDB fallback blob | Offline blob cleanup |

### AC-4: Search Operation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-UNIT-006 | Unit | P1 | Search with empty query returns prefix match | Fallback logic |
| WASM-003.3-INT-010 | Integration | P1 | Search uses DuckDB FTS when available | DuckDB integration |
| WASM-003.3-INT-011 | Integration | P2 | Search respects metadata filter | Complex query path |
| WASM-003.3-INT-012 | Integration | P1 | Search respects limit parameter | Query constraint |

### AC-5: List Operation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-UNIT-007 | Unit | P1 | List with prefix filters correctly | String matching logic |
| WASM-003.3-INT-013 | Integration | P1 | List returns entries sorted by key | IndexedDB cursor behavior |

### AC-6: IndexedDB Catalog

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-INT-014 | Integration | P0 | IndexedDB schema creates all stores | Database initialization |
| WASM-003.3-INT-015 | Integration | P0 | IndexedDB indexes are created correctly | Query performance |
| WASM-003.3-E2E-001 | E2E | P0 | Catalog persists across page reloads | Browser storage durability |

### AC-7: Content Hash Tracking

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-UNIT-008 | Unit | P0 | SHA-256 hash format is `sha256:{hex}` | Hash format contract |
| WASM-003.3-UNIT-009 | Unit | P1 | Same content produces same hash | Determinism |
| WASM-003.3-INT-016 | Integration | P0 | Content hash enables deduplication | Dedup flow |

### AC-8: Small Data Inlining

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-UNIT-010 | Unit | P0 | Value < 1024 bytes is inlined | Threshold logic |
| WASM-003.3-UNIT-011 | Unit | P0 | Value == 1024 bytes is NOT inlined | Boundary condition |
| WASM-003.3-UNIT-012 | Unit | P1 | Custom inline_threshold is respected | Configuration |

### AC-9: Large Data Files

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-INT-017 | Integration | P0 | Large value stored with correct URI | Blob path generation |
| WASM-003.3-INT-018 | Integration | P1 | Catalog entry has null inlined_value for large | Schema correctness |

### AC-10: TTL Support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-UNIT-013 | Unit | P1 | TTL metadata sets expires_at timestamp | Timestamp calculation |
| WASM-003.3-INT-019 | Integration | P2 | Expired entries filtered on retrieve | TTL enforcement |

### AC-11: OpenDAL Storage

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-INT-020 | Integration | P0 | Large values stored via OpenDAL | External storage integration |
| WASM-003.3-E2E-002 | E2E | P1 | Round-trip store/retrieve via OpenDAL | Full storage path |

### AC-12: Configurable Backend

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-UNIT-014 | Unit | P1 | Storage URI parsed correctly | URI handling |
| WASM-003.3-E2E-003 | E2E | P2 | S3 backend integration works | Cloud provider test |

### AC-13: Offline Fallback

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-UNIT-015 | Unit | P0 | Fallback URI uses `indexeddb://` scheme | Offline path decision |
| WASM-003.3-INT-021 | Integration | P0 | Large value falls back to IndexedDB blobs when offline | Offline resilience |

### AC-14: Offline-First

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-E2E-004 | E2E | P0 | All CRUD operations work while offline | Critical offline capability |
| WASM-003.3-E2E-005 | E2E | P0 | Data persisted offline survives browser restart | Durability |

### AC-15: Background Sync

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-INT-022 | Integration | P1 | Sync queue populated on store | Queue mechanics |
| WASM-003.3-E2E-006 | E2E | P1 | Background sync triggers when online | Network state handling |
| WASM-003.3-E2E-007 | E2E | P2 | Sync queue processed in order | FIFO guarantee |

### AC-16: Conflict Resolution

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-UNIT-016 | Unit | P0 | Duplicate content hash skips re-store | Dedup logic |
| WASM-003.3-INT-023 | Integration | P1 | Conflict resolved by content hash comparison | Multi-system dedup |

### AC-17: Query Integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-INT-024 | Integration | P1 | LTM entries queryable via DuckDB SQL | Analytics integration |

### AC-18: Vector Search

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-INT-025 | Integration | P2 | Embeddings searchable via VSS | Vector search path |
| WASM-003.3-E2E-008 | E2E | P2 | Vector similarity search returns relevant results | User-facing search |

### AC-19: Full-Text Search

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-INT-026 | Integration | P1 | FTS matches content in inlined values | Text search mechanics |
| WASM-003.3-E2E-009 | E2E | P2 | FTS search returns ranked results | User-facing search |

### AC-20: YAML Configuration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-UNIT-017 | Unit | P1 | YAML `ltm.backend: wasm` selects WASM backend | Config parsing |
| WASM-003.3-UNIT-018 | Unit | P2 | Invalid YAML config returns clear error | Error handling |

### AC-21: Credential Injection

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-INT-027 | Integration | P1 | JS credentials passed to OpenDAL | Security integration |
| WASM-003.3-UNIT-019 | Unit | P2 | Credentials not exposed in logs | Security validation |

### AC-22: Storage URI

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-003.3-E2E-010 | E2E | P3 | Different storage URIs route correctly | Multi-backend test |

---

## Risk Coverage

| Risk | Mitigated By |
|------|--------------|
| IndexedDB quota exceeded | WASM-003.3-INT-021 (offline fallback), WASM-003.3-E2E-004/005 (durability) |
| Data loss during sync | WASM-003.3-INT-016 (dedup), WASM-003.3-INT-022/23 (conflict resolution) |
| Offline functionality broken | WASM-003.3-E2E-004, WASM-003.3-E2E-005, WASM-003.3-INT-021 |
| Hash collision | WASM-003.3-UNIT-008/009 (SHA-256 correctness) |
| Inlining threshold bugs | WASM-003.3-UNIT-010/011/012 (boundary testing) |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - Hash computation, inlining threshold, dedup logic
2. **P0 Integration tests** (validate component interactions)
   - Store/retrieve/delete flows, IndexedDB schema, dedup
3. **P0 E2E tests** (critical offline paths)
   - Offline CRUD, browser persistence
4. **P1 tests in order** (core journeys)
5. **P2+ as time permits**

---

## Test Environment Requirements

### Unit Tests
- Rust test framework (`cargo test`)
- No external dependencies
- Mock IndexedDB/OpenDAL callbacks

### Integration Tests
- `wasm-pack test --headless --chrome`
- In-memory IndexedDB mock or headless browser
- OpenDAL memory backend

### E2E Tests
- Playwright + Vitest (as noted in story)
- Real browser (Chrome/Firefox)
- Mock cloud storage endpoint
- Network condition emulation (offline mode)

---

## Quality Checklist

- [x] Every AC has test coverage (22 ACs → 47 tests)
- [x] Test levels appropriate (no over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (offline-first = P0)
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 47
  by_level:
    unit: 19
    integration: 18
    e2e: 10
  by_priority:
    p0: 15
    p1: 19
    p2: 10
    p3: 3
  coverage_gaps: []
  key_risk_areas:
    - offline_first_operations
    - indexeddb_quota_management
    - sync_conflict_resolution
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-WASM-003.3-test-design-20260112.md
P0 tests identified: 15
Story file: docs/stories/TEA-WASM-003.3.ltm-backend-wasm.md
```
