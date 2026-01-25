# BUG.001: HierarchicalLTMBackend YAML Config Mismatch

**Status:** Done
**Priority:** High
**Reported:** 2026-01-18
**Reporter:** Dev Agent (RX.43 post-deployment testing)
**Affected Version:** TEA v0.9.70

---

## Story

**As a** TEA developer using YAML agent configuration,
**I want** the HierarchicalLTMBackend to accept the nested YAML config format documented in CLAUDE.md,
**so that** I can use A3 mode hierarchical LTM with PostgreSQL catalog and blob storage without errors.

---

## Acceptance Criteria

- [x] **AC-1:** YAML config with nested `catalog:`, `storage:`, `hierarchy:` blocks correctly instantiates `HierarchicalLTMBackend`
- [x] **AC-2:** Flat parameter API remains backward-compatible for programmatic use
- [x] **AC-3:** Error message is clear if required config is missing
- [x] **AC-4:** Unit tests cover both YAML-style and programmatic initialization
- [x] **AC-5:** CLAUDE.md documentation matches actual supported config format

---

## Tasks / Subtasks

### Task 1: Implement Config Transformation Function (AC-1, AC-3)

- [x] 1.1 Create `_parse_hierarchical_ltm_config()` function in `python/src/the_edge_agent/memory/base.py`
  - [x] Transform `catalog.url` → `catalog_url`
  - [x] Transform `storage.uri` → `storage_uri`
  - [x] Transform `hierarchy.levels` → `hierarchy_levels`
  - [x] Transform `hierarchy.defaults` → `hierarchy_defaults`
  - [x] Extract `catalog.pool_size` → `pool_size`
  - [x] Extract `catalog.lazy` → `lazy`
  - [x] Transform `index` block → `index_config` dict
  - [x] Transform `performance` block → `performance_config` dict
- [x] 1.2 Add validation for required fields with clear error messages
  - [x] Raise `ValueError` if `catalog.url` missing with message: "HierarchicalLTMBackend requires 'catalog.url' in YAML config"
  - [x] Raise `ValueError` if `storage.uri` missing with message: "HierarchicalLTMBackend requires 'storage.uri' in YAML config"
  - [x] Raise `ValueError` if `hierarchy.levels` missing or empty with message: "HierarchicalLTMBackend requires non-empty 'hierarchy.levels' list"

### Task 2: Integrate Transformation in create_ltm_backend Factory (AC-1, AC-2)

- [x] 2.1 Update `parse_ltm_config()` in `python/src/the_edge_agent/memory/base.py`
  - [x] Detect nested config format (presence of `catalog`, `storage`, `hierarchy` keys)
  - [x] Call `_parse_hierarchical_ltm_config()` for nested config
  - [x] Pass flat params directly for backward compatibility
- [x] 2.2 Ensure flat parameter API unchanged for existing callers

### Task 3: Update YAMLEngine LTM Config Handling (AC-1)

- [x] 3.1 Locate LTM config parsing in `python/src/the_edge_agent/yaml_engine.py`
- [x] 3.2 Ensure `settings.ltm` config is passed through `create_ltm_backend()` factory
- [x] 3.3 Add environment variable expansion for `catalog.url`, `storage.uri` (${VAR:-default} syntax)

### Task 4: Write Unit Tests for Config Transformation (AC-4)

- [x] 4.1 Create `python/tests/test_ltm_config_parsing.py`
- [x] 4.2 Implement P0 unit tests (BUG.001-UNIT-001 through 006):
  - [x] Test `catalog.url` → `catalog_url` transformation
  - [x] Test `storage.uri` → `storage_uri` transformation
  - [x] Test `hierarchy.levels` → `hierarchy_levels` transformation
  - [x] Test `hierarchy.defaults` → `hierarchy_defaults` transformation
  - [x] Test `catalog.pool_size` → `pool_size` extraction
  - [x] Test `catalog.lazy` → `lazy` extraction
- [x] 4.3 Implement P0 error tests (BUG.001-UNIT-014 through 016):
  - [x] Test missing `catalog.url` error message
  - [x] Test missing `storage.uri` error message
  - [x] Test missing `hierarchy.levels` error message
- [x] 4.4 Implement backward compatibility tests (BUG.001-UNIT-011, 012):
  - [x] Test flat params still work directly
  - [x] Test factory with flat kwargs

### Task 5: Write Integration Tests (AC-4)

- [x] 5.1 Added to `python/tests/test_yaml_engine_ltm.py`
- [x] 5.2 Implement integration tests:
  - [x] Test YAMLEngine loads agent with full A3 config (BUG.001-INT-001)
  - [x] Test HierarchicalLTMBackend instantiates from parsed config (BUG.001-INT-002)
  - [x] Test flat params and YAML config behave identically (BUG.001-INT-005)

### Task 6: Verify Documentation Accuracy (AC-5)

- [x] 6.1 Load test fixture `hierarchical_ltm_claude_md_example.yaml` (exact CLAUDE.md example)
- [x] 6.2 Verify it instantiates HierarchicalLTMBackend successfully
- [x] 6.3 Confirm CLAUDE.md documentation needs no changes (fix is in code, not docs)

### Task 7: Run Full Test Suite

- [x] 7.1 Run `pytest python/tests/test_ltm_config_parsing.py -v` - **25 PASSED**
- [x] 7.2 Run `pytest python/tests/test_yaml_engine_ltm.py -v` - **69 PASSED, 1 SKIPPED**
- [x] 7.3 Integration tests include regression coverage
- [x] 7.4 All tests pass

---

## Dev Notes

### Solution Approach

**Selected: Option A - Update YAMLEngine Config Parser**

Add a transformation layer that converts nested YAML config to flat parameters expected by `HierarchicalLTMBackend`. This preserves the existing flat API for programmatic use while enabling the documented YAML format.

### Relevant Source Tree

```
python/src/the_edge_agent/
├── memory/
│   ├── __init__.py           # Factory: create_ltm_backend() - MODIFY
│   ├── hierarchical_ltm.py   # HierarchicalLTMBackend class - READ ONLY (flat API preserved)
│   └── ...
├── yaml_engine.py            # YAMLEngine.load_from_file() - MAY MODIFY
└── ...

python/tests/
├── test_hierarchical_ltm.py      # Existing backend tests - REGRESSION
├── test_ltm_config_parsing.py    # NEW: Config transformation tests
├── test_yaml_engine_ltm.py       # NEW: YAMLEngine LTM integration tests
└── fixtures/
    ├── hierarchical_ltm_config.yaml           # Full A3 config
    ├── hierarchical_ltm_minimal.yaml          # Minimum required
    ├── hierarchical_ltm_missing_required.yaml # Error testing
    ├── hierarchical_ltm_env_vars.yaml         # Env var expansion
    └── hierarchical_ltm_claude_md_example.yaml # Doc accuracy
```

### Backend Constructor (Current - DO NOT MODIFY)

**File:** `python/src/the_edge_agent/memory/hierarchical_ltm.py`

```python
def __init__(
    self,
    catalog_url: str,               # Expects flat string
    storage_uri: str,               # Expects flat string
    hierarchy_levels: List[str],    # Expects flat list
    hierarchy_defaults: Optional[Dict[str, str]] = None,
    index_config: Optional[Dict[str, Any]] = None,
    performance_config: Optional[Dict[str, Any]] = None,
    pool_size: int = 10,
    inline_threshold: int = 1024,
    lazy: bool = False,
):
```

### Transformation Logic

```python
def _parse_hierarchical_ltm_config(config: dict) -> dict:
    """Transform nested YAML config to flat HierarchicalLTMBackend params."""
    catalog = config.get("catalog", {})
    storage = config.get("storage", {})
    hierarchy = config.get("hierarchy", {})
    index = config.get("index", {})
    performance = config.get("performance", {})

    # Validate required fields
    if not catalog.get("url"):
        raise ValueError("HierarchicalLTMBackend requires 'catalog.url' in YAML config")
    if not storage.get("uri"):
        raise ValueError("HierarchicalLTMBackend requires 'storage.uri' in YAML config")
    if not hierarchy.get("levels"):
        raise ValueError("HierarchicalLTMBackend requires non-empty 'hierarchy.levels' list")

    return {
        "catalog_url": catalog.get("url"),
        "storage_uri": storage.get("uri"),
        "hierarchy_levels": hierarchy.get("levels", []),
        "hierarchy_defaults": hierarchy.get("defaults", {}),
        "pool_size": catalog.get("pool_size", 10),
        "inline_threshold": config.get("inline_threshold", 1024),
        "lazy": catalog.get("lazy", False),
        "index_config": {
            "format": index.get("format", "parquet"),
            "row_group_size": index.get("row_group_size", 122880),
            "compression": index.get("compression", "zstd"),
        } if index else None,
        "performance_config": {
            "metadata_cache_ttl": performance.get("metadata_cache", {}).get("ttl_seconds", 600),
            "parallel_threads": performance.get("parallel_reads", {}).get("threads", 8),
        } if performance else None,
    }
```

### Testing

**Test File Locations:**
- `python/tests/test_ltm_config_parsing.py` - Unit tests for transformation function
- `python/tests/test_yaml_engine_ltm.py` - Integration tests for YAMLEngine + LTM

**Testing Framework:** pytest

**Run Tests:**
```bash
cd python && pytest tests/test_ltm_config_parsing.py tests/test_yaml_engine_ltm.py -v
```

**Fixtures Already Created:**
- `python/tests/fixtures/hierarchical_ltm_config.yaml`
- `python/tests/fixtures/hierarchical_ltm_minimal.yaml`
- `python/tests/fixtures/hierarchical_ltm_missing_required.yaml`
- `python/tests/fixtures/hierarchical_ltm_env_vars.yaml`
- `python/tests/fixtures/hierarchical_ltm_claude_md_example.yaml`

---

## Original Bug Report

### Summary

The `HierarchicalLTMBackend` does not accept the nested YAML configuration format documented in CLAUDE.md. When YAMLEngine tries to instantiate the backend with the documented A3 mode config, it fails with:

```
Failed to configure LTM backend from YAML: HierarchicalLTMBackend.__init__() got an unexpected keyword argument 'catalog_config'
```

### Environment

- **TEA Version:** 0.9.70
- **Python Version:** 3.12
- **Deployment:** Firebase Cloud Functions (Gen2)
- **Context:** RX.43 A3 Mode LTM Migration

### Steps to Reproduce

1. Create a YAML agent with A3 mode LTM config (as documented in CLAUDE.md):

```yaml
settings:
  ltm:
    backend: hierarchical

    catalog:
      type: sqlalchemy
      url: "${SUPABASE_DATABASE_URL}"
      pool_size: 10
      lazy: true

    storage:
      uri: "${LTM_GCS_BUCKET_URI:-gs://rankellix-ltm-prod/ltm/}"

    hierarchy:
      enabled: true
      levels: [agency, firm, user, session]
      root_entity:
        type: agency
        id: "${AGENCY_ID:-rankellix}"
      defaults:
        agency: "none"
        firm: "_unassigned"
        user: "_unassigned"

    performance:
      metadata_cache:
        enabled: true
        ttl_seconds: 600
      parallel_reads:
        threads: 8

    index:
      format: parquet
      row_group_size: 122880
      compression: zstd
```

2. Load the agent using YAMLEngine:

```python
from engine import YAMLEngine

engine = YAMLEngine(enable_ltm=True)
graph = engine.load_from_file("agent.yaml")
```

3. Observe the error in logs:
```
Failed to configure LTM backend from YAML: HierarchicalLTMBackend.__init__() got an unexpected keyword argument 'catalog_config'
```

### Root Cause

The mismatch is between:
1. **Documented YAML format** (CLAUDE.md): Nested config with `catalog:`, `storage:`, `hierarchy:` blocks
2. **Backend constructor** (`hierarchical_ltm.py`): Flat parameters like `catalog_url`, `storage_uri`, `hierarchy_levels`

### Impact

- **Affected:** All YAML agents using A3 mode (`backend: hierarchical`)
- **Severity:** Medium (fallback to SQLite works, but defeats A3 purpose)
- **Users Affected:** RX.40, RX.43 stories in spa-base

---

## Related

- **TEA-LTM-015:** Hierarchical LTM Backend
- **TEA-LTM-013:** Entity Hierarchy
- **RX.40:** Hierarchical LTM Backend (spa-base)
- **RX.43:** Agent A3 LTM Migration (spa-base)

---

## QA Results

### Test Design

**Date:** 2026-01-18
**Designer:** Quinn (Test Architect)
**Document:** [`docs/qa/assessments/BUG.001-test-design-20260118.md`](../qa/assessments/BUG.001-test-design-20260118.md)

**Summary:**
- **48 total test scenarios** covering all 5 acceptance criteria
- **Distribution:** Unit: 28 (58%), Integration: 16 (33%), E2E: 4 (8%)
- **Priorities:** P0: 12, P1: 18, P2: 14, P3: 4
- **Risks Identified:** 5 (all mitigated by test coverage)

### Test Fixtures Created

| Fixture | Purpose |
|---------|---------|
| `python/tests/fixtures/hierarchical_ltm_config.yaml` | Full A3 config with all options |
| `python/tests/fixtures/hierarchical_ltm_minimal.yaml` | Minimum required config only |
| `python/tests/fixtures/hierarchical_ltm_missing_required.yaml` | Invalid config for error message testing |
| `python/tests/fixtures/hierarchical_ltm_env_vars.yaml` | Environment variable expansion testing |
| `python/tests/fixtures/hierarchical_ltm_claude_md_example.yaml` | Exact CLAUDE.md documented example |

### Coverage by Acceptance Criteria

| AC | Description | Scenarios |
|----|-------------|-----------|
| AC-1 | Nested YAML config transformation | 14 |
| AC-2 | Flat API backward compatibility | 5 |
| AC-3 | Clear error messages | 6 |
| AC-4 | Both init paths tested | 10 |
| AC-5 | Documentation accuracy | 4 |

---

### Review Date: 2026-01-18

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Excellent implementation.** The config transformation layer cleanly bridges the documented nested YAML format to the flat parameter API expected by `HierarchicalLTMBackend`. Key strengths:

1. **Clean separation of concerns**: `_parse_hierarchical_ltm_config()` handles transformation, `_is_hierarchical_nested_config()` handles detection
2. **Proper validation**: Clear error messages for all required fields (AC-3)
3. **Backward compatibility**: Flat params bypass transformation entirely (AC-2)
4. **Good documentation**: Docstrings with examples, references to story ID
5. **Comprehensive tests**: 32 tests covering all ACs with proper cleanup

### Refactoring Performed

None required - implementation is clean and follows project conventions.

### Compliance Check

- Coding Standards: ✓ Clean Python, proper docstrings, no style issues
- Project Structure: ✓ Changes in correct locations (`memory/base.py`, `yaml_engine.py`)
- Testing Strategy: ✓ Unit tests in dedicated file, integration tests appended to existing suite
- All ACs Met: ✓ All 5 acceptance criteria verified with passing tests

### Improvements Checklist

- [x] Config transformation function with validation
- [x] Nested config detection helper
- [x] Factory integration for parse_ltm_config
- [x] YAMLEngine integration for settings.ltm
- [x] Unit tests for transformation (25 tests)
- [x] Integration tests for YAMLEngine + backend (7 tests)
- [x] Documentation accuracy tests (CLAUDE.md example fixture)
- [ ] Consider adding validation for `catalog.type` field (currently ignored) - P3 enhancement

### Security Review

**No concerns.** Config parsing only - no credential handling, no code execution, no injection vectors. Environment variables are expanded using existing `expand_env_vars()` function.

### Performance Considerations

**No concerns.** Transformation is O(1) dictionary operations with negligible overhead. Detection uses set intersection which is also O(1).

### Files Modified During Review

None - no refactoring was needed.

### Gate Status

**Gate: PASS** → [`docs/qa/gates/BUG.001-hierarchical-ltm-yaml-config-mismatch.yml`](../qa/gates/BUG.001-hierarchical-ltm-yaml-config-mismatch.yml)

| Metric | Result |
|--------|--------|
| Quality Score | 100/100 |
| Tests Passed | 32/32 |
| Security | PASS |
| Performance | PASS |
| Reliability | PASS |
| Maintainability | PASS |

### Recommended Status

**✓ Ready for Done**

All acceptance criteria met, comprehensive test coverage, clean implementation. Story can be marked Done.

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

No debug logs required - all tests passed on first run.

### Completion Notes List

1. **Config Transformation Function** (`base.py:603-682`): Created `_parse_hierarchical_ltm_config()` that transforms nested YAML config format to flat parameters expected by `HierarchicalLTMBackend.__init__()`.

2. **Nested Config Detection** (`base.py:685-704`): Added `_is_hierarchical_nested_config()` helper to detect whether config uses nested format (catalog/storage/hierarchy keys) vs flat format (catalog_url/storage_uri/hierarchy_levels).

3. **Factory Integration** (`base.py:751-756`): Updated `parse_ltm_config()` to route hierarchical backend with nested config through the transformation function.

4. **YAMLEngine Integration** (`yaml_engine.py:1051-1056`): Updated LTM config parsing to use the hierarchical config transformation when `backend: hierarchical` is specified with nested config.

5. **Test Coverage**: 25 unit tests + 7 integration tests = 32 new tests for this fix. All 69 tests in test_yaml_engine_ltm.py pass (including existing tests).

6. **Backward Compatibility**: Flat parameter API preserved. Tests confirm both flat kwargs and nested YAML config produce identical backend instances.

### File List

**Modified:**
- `python/src/the_edge_agent/memory/base.py` - Added config transformation functions
- `python/src/the_edge_agent/memory/__init__.py` - Exported new functions
- `python/src/the_edge_agent/yaml_engine.py` - Added hierarchical config handling
- `python/tests/test_yaml_engine_ltm.py` - Added integration tests

**Created:**
- `python/tests/test_ltm_config_parsing.py` - Unit tests for config transformation

**Used (pre-existing fixtures):**
- `python/tests/fixtures/hierarchical_ltm_config.yaml`
- `python/tests/fixtures/hierarchical_ltm_minimal.yaml`
- `python/tests/fixtures/hierarchical_ltm_missing_required.yaml`
- `python/tests/fixtures/hierarchical_ltm_env_vars.yaml`
- `python/tests/fixtures/hierarchical_ltm_claude_md_example.yaml`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-18 | 0.1 | Bug report created | Dev Agent |
| 2026-01-18 | 0.2 | Test design completed (48 scenarios), 5 test fixtures created | Quinn (QA) |
| 2026-01-18 | 1.0 | Transformed to development story with tasks/subtasks | Sarah (PO) |
| 2026-01-18 | 1.1 | Story validated and approved for development | Sarah (PO) |
| 2026-01-18 | 2.0 | Implementation complete - config transformation layer added, 32 tests pass | James (Dev Agent) |
| 2026-01-18 | 2.1 | QA Review PASS - all ACs verified, 32/32 tests pass, quality score 100/100 | Quinn (QA) |
| 2026-01-18 | 3.0 | Story marked Done | Bob (SM) |
