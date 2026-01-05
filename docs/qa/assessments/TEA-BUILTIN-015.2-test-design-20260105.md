# Test Design: Story TEA-BUILTIN-015.2 - Firestore Built-in Actions

Date: 2026-01-05
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 47
- **Unit tests:** 22 (47%)
- **Integration tests:** 19 (40%)
- **E2E tests:** 6 (13%)
- **Priority distribution:** P0: 15, P1: 20, P2: 10, P3: 2

## Test Strategy Rationale

This story implements critical infrastructure for Firestore integration in TEA agents. The testing strategy emphasizes:

1. **Unit tests** for pure logic (parameter validation, template interpolation, error structure generation)
2. **Integration tests** for Firestore operations using Firebase emulator
3. **E2E tests** for complete YAML agent workflows with Firestore actions

Security and data integrity are paramount given this is a database integration layer.

---

## Test Scenarios by Acceptance Criteria

### AC1: Settings Schema

**Requirement:** `settings.firestore` section configures project ID and optional emulator host

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.2-UNIT-001 | Unit | P0 | Validate FirestoreSettings Pydantic model accepts valid config | Core schema validation, no external deps |
| 015.2-UNIT-002 | Unit | P0 | Reject missing required `project` field | Input validation boundary |
| 015.2-UNIT-003 | Unit | P1 | Accept optional `emulator_host` field | Schema completeness |
| 015.2-UNIT-004 | Unit | P1 | Accept optional `credentials_path` field | Schema completeness |
| 015.2-UNIT-005 | Unit | P1 | Expand environment variables in settings | Template functionality |
| 015.2-INT-001 | Integration | P1 | Settings schema integrates with main Settings model | Component boundary test |

---

### AC2: Get Action

**Requirement:** `firestore.get` retrieves a single document by collection and document ID

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.2-UNIT-006 | Unit | P0 | Parse get action parameters correctly | Input parsing logic |
| 015.2-UNIT-007 | Unit | P1 | Return default value when document not found | Business logic |
| 015.2-INT-002 | Integration | P0 | Get existing document returns data dict | Core CRUD, emulator |
| 015.2-INT-003 | Integration | P0 | Get non-existent document returns default | Core CRUD, emulator |
| 015.2-INT-004 | Integration | P1 | Get document from nested collection path | Path parsing with DB |
| 015.2-INT-005 | Integration | P1 | Get document with complex nested data | Data serialization |

---

### AC3: Set Action

**Requirement:** `firestore.set` creates or updates a document with optional merge mode

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.2-UNIT-008 | Unit | P0 | Parse set action parameters correctly | Input parsing logic |
| 015.2-UNIT-009 | Unit | P1 | Validate merge boolean parameter | Parameter validation |
| 015.2-INT-006 | Integration | P0 | Create new document with explicit ID | Core CRUD, emulator |
| 015.2-INT-007 | Integration | P0 | Update existing document (overwrite) | Core CRUD, emulator |
| 015.2-INT-008 | Integration | P0 | Merge update preserves existing fields | Critical data integrity |
| 015.2-INT-009 | Integration | P1 | Auto-generate document ID when omitted | Feature completeness |
| 015.2-INT-010 | Integration | P1 | Return document reference path | Response validation |

---

### AC4: Query Action

**Requirement:** `firestore.query` executes queries with where, order_by, and limit clauses

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.2-UNIT-010 | Unit | P0 | Parse where clause list format | Complex input parsing |
| 015.2-UNIT-011 | Unit | P1 | Validate supported operators | Input validation |
| 015.2-UNIT-012 | Unit | P1 | Reject unsupported operators | Error boundary |
| 015.2-INT-011 | Integration | P0 | Query with == operator | Core query, emulator |
| 015.2-INT-012 | Integration | P0 | Query with multiple where clauses | Compound query |
| 015.2-INT-013 | Integration | P1 | Query with < and > operators | Range queries |
| 015.2-INT-014 | Integration | P1 | Query with `in` operator | Array operator |
| 015.2-INT-015 | Integration | P1 | Query with `array_contains` operator | Array operator |
| 015.2-INT-016 | Integration | P1 | Query with order_by clause | Sorting |
| 015.2-INT-017 | Integration | P2 | Query with limit and offset | Pagination |
| 015.2-INT-018 | Integration | P2 | Query returns empty list for no matches | Edge case |

---

### AC5: Delete Action

**Requirement:** `firestore.delete` removes a document by collection and document ID

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.2-UNIT-013 | Unit | P1 | Parse delete action parameters | Input parsing |
| 015.2-INT-019 | Integration | P0 | Delete existing document returns success | Core CRUD, emulator |
| 015.2-INT-020 | Integration | P1 | Delete non-existent document returns success | Idempotency |
| 015.2-INT-021 | Integration | P1 | Verify document no longer exists after delete | Data integrity |

---

### AC6: Batch Operations

**Requirement:** `firestore.batch` executes multiple operations atomically

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.2-UNIT-014 | Unit | P0 | Parse batch operations list | Complex input parsing |
| 015.2-UNIT-015 | Unit | P1 | Validate operation types (set/delete) | Input validation |
| 015.2-INT-022 | Integration | P0 | Batch with multiple set operations | Atomicity test |
| 015.2-INT-023 | Integration | P0 | Batch with mixed set and delete | Atomicity test |
| 015.2-INT-024 | Integration | P0 | Batch failure rolls back all operations | Critical atomicity |
| 015.2-INT-025 | Integration | P1 | Return operation count on success | Response validation |

---

### AC7: Template Support

**Requirement:** All parameters support Jinja2 template interpolation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.2-UNIT-016 | Unit | P0 | Interpolate collection name from state | Template engine |
| 015.2-UNIT-017 | Unit | P0 | Interpolate document ID from state | Template engine |
| 015.2-UNIT-018 | Unit | P0 | Interpolate data fields from state | Template engine |
| 015.2-UNIT-019 | Unit | P1 | Interpolate query where values from state | Template engine |
| 015.2-UNIT-020 | Unit | P1 | Handle `now()` function in templates | Built-in function |
| 015.2-UNIT-021 | Unit | P2 | Nested template expressions | Complex templates |

---

### AC8: Error Handling

**Requirement:** Actions return structured errors on failure (not found, permission denied)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.2-UNIT-022 | Unit | P0 | Generate NOT_FOUND error structure | Error format |
| 015.2-UNIT-023 | Unit | P0 | Generate PERMISSION_DENIED error structure | Error format |
| 015.2-UNIT-024 | Unit | P0 | Generate INVALID_ARGUMENT error structure | Error format |
| 015.2-INT-026 | Integration | P0 | Return structured error on permission denied | Real error flow |
| 015.2-INT-027 | Integration | P1 | Error includes collection and document fields | Error detail |

---

### AC9: Emulator Support

**Requirement:** Actions work with Firebase emulator when `FIRESTORE_EMULATOR_HOST` is set

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.2-UNIT-025 | Unit | P1 | Detect emulator from environment variable | Config detection |
| 015.2-INT-028 | Integration | P0 | All CRUD operations work with emulator | Platform validation |
| 015.2-E2E-001 | E2E | P1 | Complete agent workflow with emulator | Full stack validation |

---

## E2E Test Scenarios (Complete Agent Workflows)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.2-E2E-001 | E2E | P1 | YAML agent with get/set workflow | Critical path |
| 015.2-E2E-002 | E2E | P1 | YAML agent with query returning list | Query integration |
| 015.2-E2E-003 | E2E | P1 | YAML agent with batch operations | Complex workflow |
| 015.2-E2E-004 | E2E | P2 | YAML agent handles Firestore errors gracefully | Error resilience |
| 015.2-E2E-005 | E2E | P2 | YAML agent with nested collection paths | Path complexity |
| 015.2-E2E-006 | E2E | P3 | Long-running agent with multiple Firestore calls | Performance |

---

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| Data corruption from merge mode | 015.2-INT-008 | Verify merge preserves existing fields |
| Atomicity failure in batch | 015.2-INT-024 | Verify rollback on partial failure |
| Template injection | 015.2-UNIT-016-021 | Validate sandboxed Jinja2 |
| Emulator parity with production | 015.2-INT-028, 015.2-E2E-001 | Full CRUD test suite on emulator |
| Permission errors not structured | 015.2-INT-026 | Verify error structure matches spec |

---

## Test Environment Requirements

### Unit Tests
- Python 3.10+
- pytest, pytest-asyncio
- Mocked `firebase_admin` module

### Integration Tests
- Firebase CLI with emulator
- `FIRESTORE_EMULATOR_HOST=localhost:8080`
- pytest-docker or manual emulator start

### E2E Tests
- Full TEA agent environment
- Firebase emulator running
- Example YAML agents in `examples/firestore/`

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on schema/parsing)
2. **P0 Integration tests** (core CRUD operations)
3. **P1 Unit tests** (extended validation)
4. **P1 Integration tests** (feature completeness)
5. **P1 E2E tests** (workflow validation)
6. **P2+ tests** as time permits

---

## Test File Locations

| Level | Location |
|-------|----------|
| Unit | `python/tests/test_firestore_actions_unit.py` |
| Integration | `python/tests/test_firestore_actions_integration.py` |
| E2E | `python/tests/e2e/test_firestore_agent.py` |

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 47
  by_level:
    unit: 22
    integration: 19
    e2e: 6
  by_priority:
    p0: 15
    p1: 20
    p2: 10
    p3: 2
  coverage_gaps: []
  minimum_coverage: 90%
  emulator_required: true
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Risk mitigations are addressed
- [x] P0 tests cover security-critical paths (error handling, data integrity)

---

## Notes for Implementation

1. **Firebase emulator CI:** Ensure GitHub Actions workflow starts emulator before integration tests
2. **Test fixtures:** Create shared fixtures for Firestore test data setup/teardown
3. **Async testing:** All Firestore actions are async; use `pytest-asyncio` fixtures
4. **Mock isolation:** Unit tests must mock `firebase_admin` entirely to avoid import errors without Firebase SDK
5. **Environment variables:** Integration tests should set `FIRESTORE_EMULATOR_HOST` programmatically
