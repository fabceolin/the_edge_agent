# Test Design: Story TD.14 - Split Graph Backends

**Date:** 2026-01-04
**Designer:** Quinn (Test Architect)
**Story:** TD.14 - Split Graph Backends into Separate Module Files

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 28 |
| **Unit tests** | 8 (29%) |
| **Integration tests** | 16 (57%) |
| **E2E tests** | 4 (14%) |
| **Priority distribution** | P0: 10, P1: 12, P2: 6 |

### Strategy Rationale

This is a **pure refactoring story** with no functional changes. The primary testing focus is:

1. **Import path preservation** - Verify all existing import paths continue to work
2. **API equivalence** - Ensure extracted classes behave identically
3. **No regression** - All 2,016 lines of existing tests must pass unchanged
4. **Module isolation** - Verify no circular imports or missing dependencies

**Key Principle:** Existing tests (`test_neo4j_backend.py`, `test_neo4j_gds.py`, `test_neo4j_triggers.py`) serve as the primary regression suite. New tests focus on the structural changes introduced by the refactoring.

---

## Test Scenarios by Acceptance Criteria

### AC 1-3: Protocol Module Creation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.14-UNIT-001 | Unit | P0 | `GraphBackend` Protocol importable from `graph/protocol.py` | Core module structure validation |
| TD.14-UNIT-002 | Unit | P0 | `COZO_AVAILABLE`, `KUZU_AVAILABLE`, `NEO4J_AVAILABLE` constants exported | Availability check constants critical for conditional imports |
| TD.14-UNIT-003 | Unit | P1 | `_check_*_available()` functions return boolean | Pure function validation |
| TD.14-INT-001 | Integration | P0 | `from .protocol import GraphBackend` works in package `__init__.py` | Internal import chain |

### AC 4-6: Cozo Backend Extraction

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.14-UNIT-004 | Unit | P1 | `CozoBackend` importable from `graph/cozo.py` | Module structure |
| TD.14-INT-002 | Integration | P1 | `CozoBackend` methods unchanged after extraction | API preservation |
| TD.14-INT-003 | Integration | P2 | Cozo-specific imports isolated (pycozo not imported until class instantiation) | Dependency isolation |

### AC 7-9: Kuzu Backend Extraction

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.14-UNIT-005 | Unit | P1 | `KuzuBackend` importable from `graph/kuzu.py` | Module structure |
| TD.14-UNIT-006 | Unit | P1 | `BighornBackend` alias equals `KuzuBackend` | Backward compatibility alias |
| TD.14-INT-004 | Integration | P1 | `KuzuBackend` methods unchanged after extraction | API preservation |

### AC 10-12: Neo4j Backend Extraction

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.14-INT-005 | Integration | P0 | `Neo4jBackend` importable from `graph/neo4j.py` | Most critical backend |
| TD.14-INT-006 | Integration | P0 | `Neo4jTransaction` importable alongside `Neo4jBackend` | Transaction support |
| TD.14-INT-007 | Integration | P0 | All Neo4j methods preserved (triggers, GDS, batching) | Feature completeness |
| TD.14-E2E-001 | E2E | P0 | All 985 lines of `test_neo4j_backend.py` pass unchanged | Critical regression |
| TD.14-E2E-002 | E2E | P1 | All 464 lines of `test_neo4j_gds.py` pass unchanged | GDS feature regression |
| TD.14-E2E-003 | E2E | P1 | All 567 lines of `test_neo4j_triggers.py` pass unchanged | Trigger feature regression |

### AC 13-15: Package Structure

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.14-INT-008 | Integration | P0 | `graph/__init__.py` exports all public symbols | Package re-export completeness |
| TD.14-INT-009 | Integration | P0 | `__all__` list matches original `graph.py` exports | API surface preservation |
| TD.14-INT-010 | Integration | P1 | `memory/graph.py` no longer exists (replaced by package) | Clean replacement |

### AC 16-19: Backward Compatibility

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.14-INT-011 | Integration | P0 | `from the_edge_agent.memory.graph import Neo4jBackend` works | Primary import path |
| TD.14-INT-012 | Integration | P0 | `from the_edge_agent.memory.graph import CozoBackend` works | Cozo import path |
| TD.14-INT-013 | Integration | P0 | `from the_edge_agent.memory.graph import KuzuBackend` works | Kuzu import path |
| TD.14-INT-014 | Integration | P1 | `from the_edge_agent.memory import Neo4jBackend` works | Top-level import path |
| TD.14-E2E-004 | E2E | P0 | Full `pytest python/tests/` suite passes | Complete regression |

### AC 20-23: Quality Requirements

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.14-UNIT-007 | Unit | P2 | Each new module has `__doc__` (module docstring) | Documentation quality |
| TD.14-INT-015 | Integration | P1 | No circular import when importing any backend | Import hygiene |
| TD.14-UNIT-008 | Unit | P2 | Type hints preserved on public classes | Type safety |
| TD.14-INT-016 | Integration | P2 | No duplicate code between modules (verified by inspection) | Code quality |

---

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| Import path breakage | TD.14-INT-011 through TD.14-INT-014 | Comprehensive import path testing |
| Missing exports | TD.14-INT-008, TD.14-INT-009 | `__all__` verification |
| Neo4j feature loss | TD.14-E2E-001 through TD.14-E2E-003 | Full existing test suite |
| Circular imports | TD.14-INT-015 | Direct import chain testing |
| Availability constant loss | TD.14-UNIT-002 | Protocol module constant check |

---

## Recommended Execution Order

### Phase 1: Pre-Refactoring Baseline (P0)
1. Run existing test suite to establish baseline
   - `pytest python/tests/test_neo4j_backend.py -v`
   - `pytest python/tests/test_neo4j_gds.py -v`
   - `pytest python/tests/test_neo4j_triggers.py -v`
2. Record test count and pass rate

### Phase 2: Unit Tests (Fail Fast)
1. P0 Unit tests: TD.14-UNIT-001, TD.14-UNIT-002
2. P1 Unit tests: TD.14-UNIT-003 through TD.14-UNIT-006

### Phase 3: Integration Tests
1. P0 Integration tests: TD.14-INT-001, TD.14-INT-005 through TD.14-INT-014
2. P1 Integration tests: TD.14-INT-002 through TD.14-INT-004, TD.14-INT-010, TD.14-INT-015

### Phase 4: E2E Regression
1. P0 E2E: TD.14-E2E-001 (Neo4j backend tests)
2. P0 E2E: TD.14-E2E-004 (Full test suite)
3. P1 E2E: TD.14-E2E-002, TD.14-E2E-003

### Phase 5: Quality Validation (P2)
1. TD.14-UNIT-007 (Docstrings)
2. TD.14-UNIT-008 (Type hints)
3. TD.14-INT-016 (No duplication)

---

## Test Implementation Recommendations

### New Test File: `tests/test_graph_package_structure.py`

```python
"""
Test scenarios for TD.14: Graph Backend Module Split

These tests validate the refactoring of graph.py into the graph/ package.
They focus on import path preservation and module structure, NOT functionality
(which is covered by existing test_neo4j_*.py tests).
"""
import unittest

class TestGraphPackageImports(unittest.TestCase):
    """TD.14-INT-011 through TD.14-INT-014: Import path backward compatibility."""

    def test_import_neo4j_backend_from_graph(self):
        """TD.14-INT-011: Primary import path preserved."""
        from the_edge_agent.memory.graph import Neo4jBackend
        self.assertIsNotNone(Neo4jBackend)

    def test_import_cozo_backend_from_graph(self):
        """TD.14-INT-012: Cozo import path preserved."""
        from the_edge_agent.memory.graph import CozoBackend
        # CozoBackend may be None if pycozo not installed
        # The import itself should not fail

    def test_import_kuzu_backend_from_graph(self):
        """TD.14-INT-013: Kuzu import path preserved."""
        from the_edge_agent.memory.graph import KuzuBackend, BighornBackend
        # Backends may be None if not installed

    def test_import_from_memory_toplevel(self):
        """TD.14-INT-014: Top-level memory import preserved."""
        from the_edge_agent.memory import Neo4jBackend
        self.assertIsNotNone(Neo4jBackend)


class TestProtocolModuleExports(unittest.TestCase):
    """TD.14-UNIT-001 through TD.14-UNIT-003: Protocol module validation."""

    def test_graph_backend_protocol_exists(self):
        """TD.14-UNIT-001: GraphBackend Protocol importable."""
        from the_edge_agent.memory.graph import GraphBackend
        self.assertIsNotNone(GraphBackend)

    def test_availability_constants_exported(self):
        """TD.14-UNIT-002: Availability constants exported."""
        from the_edge_agent.memory.graph import (
            COZO_AVAILABLE,
            KUZU_AVAILABLE,
            NEO4J_AVAILABLE,
        )
        self.assertIsInstance(COZO_AVAILABLE, bool)
        self.assertIsInstance(KUZU_AVAILABLE, bool)
        self.assertIsInstance(NEO4J_AVAILABLE, bool)

    def test_check_functions_exported(self):
        """TD.14-UNIT-003: Check functions exported."""
        from the_edge_agent.memory.graph import _check_neo4j_available
        result = _check_neo4j_available()
        self.assertIsInstance(result, bool)


class TestPackageAllExports(unittest.TestCase):
    """TD.14-INT-008, TD.14-INT-009: Package __all__ completeness."""

    def test_all_exports_defined(self):
        """TD.14-INT-008: __all__ defined in graph package."""
        from the_edge_agent.memory import graph
        self.assertTrue(hasattr(graph, '__all__'))

    def test_all_exports_complete(self):
        """TD.14-INT-009: __all__ contains expected exports."""
        from the_edge_agent.memory.graph import __all__
        expected = {
            "GraphBackend",
            "CozoBackend", "COZO_AVAILABLE",
            "KuzuBackend", "BighornBackend", "KUZU_AVAILABLE",
            "Neo4jBackend", "Neo4jTransaction", "NEO4J_AVAILABLE",
            "_check_neo4j_available",
        }
        self.assertTrue(expected.issubset(set(__all__)))


class TestNoCircularImports(unittest.TestCase):
    """TD.14-INT-015: No circular import issues."""

    def test_import_all_backends_no_error(self):
        """Importing all backends should not cause circular import."""
        try:
            from the_edge_agent.memory.graph import (
                GraphBackend,
                CozoBackend,
                KuzuBackend,
                Neo4jBackend,
            )
        except ImportError as e:
            if "circular" in str(e).lower():
                self.fail(f"Circular import detected: {e}")
            # Other import errors (missing deps) are acceptable


class TestModuleDocstrings(unittest.TestCase):
    """TD.14-UNIT-007: Module documentation."""

    def test_graph_package_has_docstring(self):
        """Package __init__.py has docstring."""
        from the_edge_agent.memory import graph
        self.assertIsNotNone(graph.__doc__)


class TestBighornAlias(unittest.TestCase):
    """TD.14-UNIT-006: BighornBackend alias preserved."""

    def test_bighorn_is_kuzu(self):
        """BighornBackend should equal KuzuBackend."""
        from the_edge_agent.memory.graph import KuzuBackend, BighornBackend
        if KuzuBackend is not None:
            self.assertIs(BighornBackend, KuzuBackend)


if __name__ == "__main__":
    unittest.main()
```

### Test Commands

```bash
# Phase 1: Baseline
cd python && pytest tests/test_neo4j_backend.py tests/test_neo4j_gds.py tests/test_neo4j_triggers.py -v

# Phase 2-4: After refactoring
cd python && pytest tests/test_graph_package_structure.py -v

# Phase 4: Full regression
cd python && pytest tests/ -v

# Verify test count unchanged
cd python && pytest tests/test_neo4j*.py --collect-only | grep "test session starts"
```

---

## Coverage Gaps

| Gap | Severity | Recommendation |
|-----|----------|----------------|
| Cozo/Kuzu tests (no existing test files) | Low | Not in scope - backends are optional |
| Performance regression testing | Low | Pure refactoring should have zero performance impact |
| IDE type hint verification | Low | Manual verification during code review |

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for structure, integration for imports, E2E for regression)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (import paths = P0)
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  story_id: TD.14
  date: 2026-01-04
  designer: Quinn (Test Architect)
  scenarios_total: 28
  by_level:
    unit: 8
    integration: 16
    e2e: 4
  by_priority:
    p0: 10
    p1: 12
    p2: 6
  coverage_gaps:
    - "Cozo/Kuzu functional tests (not in scope - optional backends)"
  existing_tests_as_regression:
    - test_neo4j_backend.py (985 lines, ~75 tests)
    - test_neo4j_gds.py (464 lines, ~25 tests)
    - test_neo4j_triggers.py (567 lines, ~35 tests)
  new_test_file: tests/test_graph_package_structure.py
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TD.14-test-design-20260104.md
P0 tests identified: 10
Existing test files serving as E2E regression: 3 files, 2016 LOC
New structural test file recommended: tests/test_graph_package_structure.py
```
