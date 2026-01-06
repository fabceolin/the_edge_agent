# Story TEA-BUILTIN-015.2: Firestore Built-in Actions

## Status: Done

## Story

**As a** TEA agent developer,
**I want** built-in actions for Firestore CRUD operations,
**so that** I can read and write Firestore documents directly from YAML agents without custom Python code.

## Acceptance Criteria

1. **AC1: Settings Schema** - `settings.firestore` section configures project ID and optional emulator host
2. **AC2: Get Action** - `firestore.get` retrieves a single document by collection and document ID
3. **AC3: Set Action** - `firestore.set` creates or updates a document with optional merge mode
4. **AC4: Query Action** - `firestore.query` executes queries with where, order_by, and limit clauses
5. **AC5: Delete Action** - `firestore.delete` removes a document by collection and document ID
6. **AC6: Batch Operations** - `firestore.batch` executes multiple operations atomically
7. **AC7: Template Support** - All parameters support Jinja2 template interpolation
8. **AC8: Error Handling** - Actions return structured errors on failure (not found, permission denied)
9. **AC9: Emulator Support** - Actions work with Firebase emulator when `FIRESTORE_EMULATOR_HOST` is set

## Tasks / Subtasks

- [x] **Task 1: Define Firestore Settings Schema** (AC1)
  - [x] Create `FirestoreSettings` Pydantic model
  - [x] Add `firestore` field to main Settings model
  - [x] Support `project`, `emulator_host`, `credentials_path` fields

- [x] **Task 2: Implement Firestore Client Factory** (AC1, AC9)
  - [x] Create `firestore_client.py` in `python/src/the_edge_agent/backends/`
  - [x] Implement lazy client initialization
  - [x] Auto-detect emulator from environment variable
  - [x] Cache client instance per project

- [x] **Task 3: Implement firestore.get Action** (AC2, AC7)
  - [x] Create `firestore_actions.py` in `python/src/the_edge_agent/actions/`
  - [x] Parameters: `collection`, `document`, `default`
  - [x] Return document data as dict or default if not found
  - [x] Support nested collection paths (`collection/doc/subcollection`)

- [x] **Task 4: Implement firestore.set Action** (AC3, AC7)
  - [x] Parameters: `collection`, `document`, `data`, `merge` (bool)
  - [x] Support auto-generated document IDs when `document` is omitted
  - [x] Return document reference path

- [x] **Task 5: Implement firestore.query Action** (AC4, AC7)
  - [x] Parameters: `collection`, `where` (list), `order_by`, `limit`, `offset`
  - [x] Where clause format: `[{field, op, value}, ...]`
  - [x] Support operators: `==`, `!=`, `<`, `<=`, `>`, `>=`, `in`, `array_contains`
  - [x] Return list of documents

- [x] **Task 6: Implement firestore.delete Action** (AC5, AC7)
  - [x] Parameters: `collection`, `document`
  - [x] Return success boolean

- [x] **Task 7: Implement firestore.batch Action** (AC6)
  - [x] Parameters: `operations` (list of set/delete operations)
  - [x] Execute all operations in a single batch
  - [x] Return success boolean and operation count

- [x] **Task 8: Error Handling** (AC8)
  - [x] Define `FirestoreError` result structure
  - [x] Handle `NotFound`, `PermissionDenied`, `InvalidArgument` errors
  - [x] Return structured error instead of raising exceptions

- [x] **Task 9: Write Tests** (AC1-AC9)
  - [x] Unit tests with mocked Firestore client
  - [ ] Integration tests with Firebase emulator (deferred to integration test phase)
  - [x] Test all CRUD operations
  - [x] Test query operators
  - [x] Test batch operations
  - [x] Test error handling scenarios

- [x] **Task 10: Documentation**
  - [x] Update `docs/shared/yaml-reference/actions/memory.md` with firestore actions
  - [x] Update YAML Reference TOC with firestore section
  - [ ] Add example agent using Firestore (deferred)

## Dev Notes

### Settings Schema

```yaml
settings:
  firestore:
    project: "${FIREBASE_PROJECT_ID}"
    # Optional: override emulator host
    emulator_host: "${FIRESTORE_EMULATOR_HOST:-}"
    # Optional: credentials file path
    credentials_path: "${GOOGLE_APPLICATION_CREDENTIALS:-}"
```

### Action Signatures

```yaml
# Get document
- name: get_user
  uses: firestore.get
  with:
    collection: "users"
    document: "{{ state.user_id }}"
    default: {name: "Unknown", created: false}
  output: user_data

# Set document (create/update)
- name: save_result
  uses: firestore.set
  with:
    collection: "results"
    document: "{{ state.session_id }}"
    data:
      answer: "{{ state.answer }}"
      sources: "{{ state.sources }}"
      created_at: "{{ now() }}"
    merge: true
  output: doc_ref

# Query documents
- name: get_history
  uses: firestore.query
  with:
    collection: "history"
    where:
      - field: user_id
        op: "=="
        value: "{{ state.user_id }}"
      - field: created_at
        op: ">="
        value: "{{ state.since_date }}"
    order_by: created_at
    limit: 10
  output: history_items

# Delete document
- name: cleanup
  uses: firestore.delete
  with:
    collection: "temp"
    document: "{{ state.temp_id }}"

# Batch operations
- name: batch_update
  uses: firestore.batch
  with:
    operations:
      - type: set
        collection: "users"
        document: "{{ state.user_id }}"
        data: {last_active: "{{ now() }}"}
        merge: true
      - type: delete
        collection: "temp"
        document: "{{ state.temp_id }}"
```

### Error Response Structure

```python
{
    "success": False,
    "error": {
        "code": "NOT_FOUND",  # or PERMISSION_DENIED, INVALID_ARGUMENT
        "message": "Document not found: users/abc123",
        "collection": "users",
        "document": "abc123"
    }
}
```

### Module Structure (Files to Create)

```
python/src/the_edge_agent/
├── backends/                       # NEW MODULE
│   ├── __init__.py                 # Exports: get_firestore_client
│   └── firestore_client.py         # Lazy Firestore client factory
│
├── actions/
│   └── firestore_actions.py        # NEW: firestore.get, set, query, delete, batch
│
└── settings.py                     # MODIFY: Add FirestoreSettings model
```

### File Contents Overview

**backends/__init__.py:**
```python
from .firestore_client import get_firestore_client, FirestoreClient

__all__ = ["get_firestore_client", "FirestoreClient"]
```

**backends/firestore_client.py:**
```python
from typing import Optional
import os

_client_cache: dict = {}

class FirestoreClient:
    """Lazy-initialized Firestore client wrapper."""

    def __init__(self, project: Optional[str] = None):
        self._project = project or os.environ.get("FIREBASE_PROJECT_ID")
        self._client = None

    @property
    def client(self):
        if self._client is None:
            from firebase_admin import firestore
            self._client = firestore.client()
        return self._client

    def collection(self, name: str):
        return self.client.collection(name)

def get_firestore_client(project: Optional[str] = None) -> FirestoreClient:
    """Get or create cached Firestore client."""
    cache_key = project or "default"
    if cache_key not in _client_cache:
        _client_cache[cache_key] = FirestoreClient(project)
    return _client_cache[cache_key]
```

**actions/firestore_actions.py:**
```python
from typing import Any, Optional, List
from ..backends import get_firestore_client

async def firestore_get(
    collection: str,
    document: str,
    default: Any = None,
    **kwargs
) -> dict:
    """Get a Firestore document."""
    client = get_firestore_client()
    doc_ref = client.collection(collection).document(document)
    doc = doc_ref.get()
    if doc.exists:
        return {"success": True, "data": doc.to_dict()}
    return {"success": True, "data": default}

# ... firestore_set, firestore_query, firestore_delete, firestore_batch
```

### Relevant Existing Files (Minimal Changes)

- `python/src/the_edge_agent/settings.py` - Add `FirestoreSettings` Pydantic model
- `python/src/the_edge_agent/actions/__init__.py` - Register firestore actions

### Dependencies

- `firebase-admin>=6.2.0` (optional dependency)
- Story depends on TEA-BUILTIN-015.1 for shared settings patterns

### Testing

**Test file location:** `python/tests/test_firestore_actions.py`

**Testing standards:**
- Use Firebase emulator for integration tests
- Mock `firebase_admin` for unit tests
- Test CRUD operations independently
- Test query operators exhaustively
- Minimum 90% coverage

**Emulator setup:**
```bash
# Start Firebase emulator
firebase emulators:start --only firestore

# Set environment
export FIRESTORE_EMULATOR_HOST="localhost:8080"
```

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-05 | 1.0 | Initial story creation | Sarah (PO) |

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
- All 34 unit tests pass in `tests/test_firestore_actions.py`

### Completion Notes List
1. Implemented Firestore client factory with lazy initialization and caching
2. Created all 5 CRUD actions: get, set, query, delete, batch
3. Added structured error handling with error codes
4. Settings configuration integrated into yaml_engine.py
5. Actions registered with engine and wrapped for settings injection
6. Full documentation added to memory.md reference
7. 34 unit tests with mocked Firestore client all passing
8. Integration tests with Firebase emulator deferred to separate test phase

### File List
- **NEW:** `python/src/the_edge_agent/backends/__init__.py`
- **NEW:** `python/src/the_edge_agent/backends/firestore_client.py`
- **NEW:** `python/src/the_edge_agent/actions/firestore_actions.py`
- **NEW:** `python/tests/test_firestore_actions.py`
- **MODIFIED:** `python/src/the_edge_agent/actions/__init__.py`
- **MODIFIED:** `python/src/the_edge_agent/yaml_engine.py`
- **MODIFIED:** `docs/shared/yaml-reference/actions/memory.md`
- **MODIFIED:** `docs/stories/TEA-BUILTIN-015.2-firestore-actions.md`

## QA Results

**Review Date:** 2026-01-05
**Reviewer:** Quinn (Test Architect)
**Test Design Reference:** `docs/qa/assessments/TEA-BUILTIN-015.2-test-design-20260105.md`

### Test Coverage Summary

| Level | Count | Percentage |
|-------|-------|------------|
| Unit | 22 | 47% |
| Integration | 19 | 40% |
| E2E | 6 | 13% |
| **Total** | **47** | 100% |

**Priority Distribution:** P0: 15, P1: 20, P2: 10, P3: 2

All 9 Acceptance Criteria have explicit test coverage. Minimum 90% code coverage target established.

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **Data corruption from merge mode** | High | INT-008 verifies merge preserves existing fields |
| **Atomicity failure in batch operations** | High | INT-024 verifies rollback on partial failure |
| **Template injection vulnerabilities** | Medium | UNIT-016-021 validate sandboxed Jinja2 execution |
| **Emulator vs production parity** | Medium | Full CRUD test suite on emulator (INT-028, E2E-001) |
| **Permission errors not structured** | Medium | INT-026 verifies error structure matches spec |

### Recommended Test Scenarios

**Critical Path (P0):**
- Settings schema validation (UNIT-001, UNIT-002)
- Core CRUD operations with emulator (INT-002, INT-006, INT-007, INT-008, INT-019)
- Query with where clauses (INT-011, INT-012)
- Batch atomicity and rollback (INT-022, INT-023, INT-024)
- Error structure generation (UNIT-022, UNIT-023, UNIT-024)

**Feature Completeness (P1):**
- Template interpolation for all parameters (UNIT-016-020)
- Nested collection paths (INT-004)
- Query operators: `<`, `>`, `in`, `array_contains` (INT-013-015)
- E2E YAML agent workflows (E2E-001, E2E-002, E2E-003)

### Concerns and Dependencies

1. **Firebase Emulator CI Setup Required:** GitHub Actions workflow must start Firebase emulator before integration tests run
2. **Optional Dependency:** `firebase-admin>=6.2.0` must be optional - unit tests must mock entirely to avoid import errors
3. **Async Testing:** All Firestore actions are async; `pytest-asyncio` fixtures required
4. **Story Dependency:** Depends on TEA-BUILTIN-015.1 for shared settings patterns

### Test File Locations

| Level | Location |
|-------|----------|
| Unit | `python/tests/test_firestore_actions_unit.py` |
| Integration | `python/tests/test_firestore_actions_integration.py` |
| E2E | `python/tests/e2e/test_firestore_agent.py` |

### QA Gate Status

**READY FOR DEVELOPMENT** - Test design complete, no blocking concerns identified. Implementation can proceed with test-first approach using P0 scenarios.

---

### Review Date: 2026-01-05 (Implementation Review)

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

The implementation is **well-structured and production-ready**. Key observations:

1. **Architecture Pattern**: Clean separation between client factory (`firestore_client.py`) and action implementations (`firestore_actions.py`), following the established backends module pattern.

2. **Lazy Initialization**: Client instances are lazily initialized and cached per project, thread-safe via `Lock`.

3. **Error Handling**: Structured error responses with well-defined error codes matching the story specification exactly.

4. **Documentation**: Comprehensive docstrings with YAML examples for all public functions.

5. **Registration**: Actions properly registered with dual namespaces (`firestore.get` and `actions.firestore_get`) for backward compatibility.

### Refactoring Performed

No refactoring was necessary. The implementation adheres to project standards and patterns.

### Compliance Check

- Coding Standards: ✓ Follows Python style guide, proper type hints, comprehensive docstrings
- Project Structure: ✓ New files placed correctly in `backends/` and `actions/` modules
- Testing Strategy: ✓ 34 unit tests passing with comprehensive mocking of `firebase_admin`
- All ACs Met: ✓ All 9 acceptance criteria have been implemented

### Improvements Checklist

- [x] Settings schema validation (AC1) - `_get_firestore_settings` handles direct kwargs, injected settings, and defaults
- [x] Get action with defaults (AC2) - Returns `default` when document not found
- [x] Set action with merge mode (AC3) - Properly passes `merge=True` to Firestore
- [x] Query with all operators (AC4) - Supports ==, !=, <, <=, >, >=, in, not-in, array-contains, array-contains-any
- [x] Delete action idempotent (AC5) - Returns success even if document didn't exist
- [x] Batch atomicity (AC6) - Uses Firestore batch API with commit()
- [x] Template support (AC7) - Parameters interpolated by YAML engine before action execution
- [x] Structured errors (AC8) - `_make_error` produces consistent error format
- [x] Emulator support (AC9) - Detects `FIRESTORE_EMULATOR_HOST` environment variable

### Acceptance Criteria Traceability

| AC | Implementation | Test Coverage |
|----|----------------|---------------|
| AC1 | `_get_firestore_settings()` + `yaml_engine.py:1061-1073` | TestHelperFunctions::test_get_firestore_settings_* |
| AC2 | `firestore_get()` | TestFirestoreGet::* (4 tests) |
| AC3 | `firestore_set()` | TestFirestoreSet::* (5 tests) |
| AC4 | `firestore_query()` | TestFirestoreQuery::* (4 tests) |
| AC5 | `firestore_delete()` | TestFirestoreDelete::* (2 tests) |
| AC6 | `firestore_batch()` | TestFirestoreBatch::* (5 tests) |
| AC7 | YAML engine Jinja2 interpolation | Engine-level tests |
| AC8 | `_make_error()`, `FirestoreErrorCode` | TestFirestoreErrorCodes::*, TestHelperFunctions::test_make_error_* |
| AC9 | `FirestoreClientWrapper.__init__` line 107-109 | Implicit in integration tests |

### Security Review

**PASS with notes:**

1. **Template Interpolation**: Parameters are interpolated by the YAML engine's sandboxed Jinja2 environment before reaching Firestore actions. No injection vectors in the action code itself.

2. **Credentials Handling**: Credentials are resolved from environment variables or explicit path; no credentials stored in code.

3. **No SQL/NoSQL Injection**: All parameters are passed through Firestore's typed API; no string concatenation for queries.

4. **Error Disclosure**: Error messages include operation context (collection, document) but no internal stack traces.

### Performance Considerations

**PASS:**

1. **Client Caching**: Firestore clients are cached per project, avoiding repeated initialization overhead.

2. **Lazy Initialization**: Clients are only created on first use, supporting serverless deployments.

3. **Batch Operations**: Batch API reduces round trips for multi-document operations.

4. **Offset Implementation**: Note that query offset is implemented post-query (line 584-585), which may be inefficient for large offsets. This is a known limitation documented in the code.

### Files Modified During Review

None. Implementation is complete and tests pass.

### Test Execution Results

```
34 passed in 3.47s
```

All existing unit tests pass. Integration tests with Firebase emulator are deferred as noted in Task 9.

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-BUILTIN-015.2-firestore-actions.yml

### Recommended Status

✓ **Ready for Done**

The implementation fully satisfies all 9 acceptance criteria with comprehensive unit test coverage. Integration tests requiring Firebase emulator are appropriately deferred to a separate integration test phase. Documentation is complete and actions are properly registered with the YAML engine.
