# Epic TEA-CONSOLIDATE-001: Rust-PyO3 Engine Unification

## Status
Draft

## Epic Goal

Eliminate dual-codebase maintenance burden by consolidating Python and Rust implementations into a single Rust core with PyO3 bindings, maintaining full Python API compatibility.

## Epic Description

### Existing System Context

**Current Architecture (Dual Implementation):**

```
┌─────────────────────────────────────────────────────────────────┐
│                    CURRENT: Dual Codebase                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Python Implementation          Rust Implementation             │
│  ~~~~~~~~~~~~~~~~~~~~~~         ~~~~~~~~~~~~~~~~~~~~            │
│  python/src/the_edge_agent/     rust/src/                       │
│  ├── stategraph.py (~724 LOC)   ├── engine/                     │
│  ├── yaml_engine.py (~553 LOC)  │   ├── state_graph.rs          │
│  ├── actions/                   │   ├── yaml_engine.rs          │
│  │   ├── llm.py                 │   ├── parallel.rs             │
│  │   ├── http.py                │   ├── retry.rs                │
│  │   ├── file.py                │   └── lua_runtime.rs          │
│  │   └── ...                    ├── actions/                    │
│  └── memory/ (Python-only)      │   ├── llm.rs                  │
│      ├── ltm_backends/          │   ├── http.rs                 │
│      ├── entity_hierarchy.py    │   ├── file.rs                 │
│      └── ...                    │   └── ...                     │
│                                 └── bin/tea.rs                  │
│                                                                 │
│  PROBLEM: Same logic maintained twice, divergent features       │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Technology Stack:**
- Python 3.9+ with networkx, pyyaml, jinja2
- Rust 1.82+ with petgraph, serde_yaml, mlua
- PyO3 0.23+ for Python bindings (proven with md-parser)

**Integration Points:**
- YAML configuration loading
- StateGraph compilation and execution
- Action dispatch (llm, http, file, data, memory)
- Template rendering (Jinja2 in Python, Tera in Rust)

### Enhancement Details

**Target Architecture (Unified):**

```
┌─────────────────────────────────────────────────────────────────┐
│                    TARGET: Unified Codebase                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Rust Core (Single Source of Truth)                             │
│  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                             │
│  rust/src/                                                      │
│  ├── engine/                                                    │
│  │   ├── state_graph.rs      ─┐                                 │
│  │   ├── yaml_engine.rs       │                                 │
│  │   ├── parallel.rs          ├── PyO3 Bindings ──► Python API  │
│  │   ├── retry.rs             │                                 │
│  │   └── lua_runtime.rs      ─┘                                 │
│  ├── actions/                                                   │
│  │   ├── llm.rs              ─┐                                 │
│  │   ├── http.rs              ├── PyO3 Bindings ──► Python API  │
│  │   ├── file.rs              │                                 │
│  │   └── ...                 ─┘                                 │
│  ├── python.rs               ◄── PyO3 module definition         │
│  └── bin/tea.rs                                                 │
│                                                                 │
│  Python Layer (Thin Wrapper + Python-Only Features)             │
│  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             │
│  python/src/the_edge_agent/                                     │
│  ├── __init__.py             ◄── Re-exports from Rust bindings  │
│  ├── _rust_bindings.py       ◄── Import tea_core (Rust module)  │
│  └── memory/                 ◄── Remains Python-only            │
│      ├── ltm_backends/                                          │
│      ├── entity_hierarchy.py                                    │
│      └── experiments/                                           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**What Changes:**
- `stategraph.py` → Thin wrapper around Rust `StateGraph` via PyO3
- `yaml_engine.py` → Thin wrapper around Rust `YamlEngine` via PyO3
- Python actions → Rust actions with PyO3 bindings
- Template engine → Rust Tera (already supports Jinja2 syntax subset)

**What Stays Python-Only:**
- `memory/` - LTM backends (SQLAlchemy, DuckDB, Hierarchical)
- `memory/entity_hierarchy.py` - Entity hierarchy
- `experiments/` - Experiment framework
- CLI Python wrapper (optional, can call Rust CLI)

**Success Criteria:**
1. All existing Python tests pass with Rust backend
2. Python API remains 100% backward compatible
3. Single Rust codebase for core engine logic
4. Performance equal or better than pure Python
5. `pip install the-edge-agent` installs pre-built wheels (no Rust toolchain required)

## Stories

### Story 1: PyO3 Core Engine Bindings

**Title:** TEA-CONSOLIDATE-001.1 - PyO3 Bindings for StateGraph and YAMLEngine

**Summary:** Create PyO3 bindings exposing Rust `StateGraph` and `YamlEngine` to Python, following the proven `md-parser` pattern.

**Scope:**
- `#[pyclass]` for `StateGraph`, `CompiledGraph`, `YamlEngine`
- `#[pymethods]` for `add_node`, `add_edge`, `compile`, `invoke`, `stream`
- Python-compatible state dict handling (serde_json ↔ Python dict)
- Error conversion (Rust errors → Python exceptions)
- Maturin build configuration

**Acceptance Criteria:**
1. `StateGraph` class importable from Python
2. `add_node()`, `add_edge()`, `add_conditional_edges()` work from Python
3. `compile()` returns `CompiledGraph` usable from Python
4. `invoke()` and `stream()` execute graphs with Python state dicts
5. `YamlEngine.load_from_file()` and `load_from_dict()` work from Python
6. All operations handle errors as Python exceptions
7. Maturin builds wheels for Linux, macOS, Windows

---

### Story 2: PyO3 Actions Framework Bindings

**Title:** TEA-CONSOLIDATE-001.2 - PyO3 Bindings for Actions Framework

**Summary:** Expose Rust actions (llm, http, file, data, memory) to Python via PyO3.

**Scope:**
- Action registry accessible from Python
- Built-in actions callable from Python
- Custom Python actions registerable in Rust engine
- Async action support (tokio ↔ asyncio bridge)

**Acceptance Criteria:**
1. `llm.call` action works from Python with OpenAI-compatible APIs
2. `http.get`, `http.post` actions work from Python
3. `file.read`, `file.write` actions work from Python
4. `data.transform` (JMESPath) works from Python
5. Custom Python functions registerable as actions
6. Async actions work with Python asyncio
7. Action errors propagate as Python exceptions

---

### Story 3: Python API Migration and Cleanup

**Title:** TEA-CONSOLIDATE-001.3 - Python API Wrapper and Codebase Cleanup

**Summary:** Create thin Python wrapper maintaining API compatibility, migrate tests, remove redundant Python code.

**Scope:**
- Python `StateGraph` class wrapping Rust implementation
- Python `YAMLEngine` class wrapping Rust implementation
- Backward-compatible imports (`from the_edge_agent import StateGraph`)
- Test suite migration to use Rust backend
- Remove redundant Python implementation files
- Update documentation

**Acceptance Criteria:**
1. `from the_edge_agent import StateGraph, YAMLEngine, START, END` works unchanged
2. All existing Python unit tests pass
3. All existing Python integration tests pass
4. `stategraph.py` reduced to thin wrapper (~50 LOC)
5. `yaml_engine.py` reduced to thin wrapper (~50 LOC)
6. Old implementation code removed
7. Documentation updated with new architecture
8. `pip install` works without Rust toolchain (pre-built wheels)

## Compatibility Requirements

- [x] Existing Python API remains unchanged (100% backward compatible)
- [x] YAML agent files work without modification
- [x] Existing tests pass without modification (beyond import paths)
- [x] Performance equal or better than pure Python
- [x] No new runtime dependencies for Python users

## Risk Mitigation

**Primary Risk:** API incompatibility discovered late in migration

**Mitigation:**
1. Comprehensive API surface mapping before implementation
2. Run full test suite against Rust bindings in Story 1
3. Document any intentional behavior differences

**Rollback Plan:**
- Git revert to pre-migration commit
- PyPI version pinning (`the-edge-agent<1.0` for pure Python)
- Feature branch until all tests pass

## Technical Notes

### PyO3 Pattern (from md-parser)

```rust
// rust/src/python.rs
use pyo3::prelude::*;

#[pyclass]
pub struct PyStateGraph {
    inner: StateGraph,
}

#[pymethods]
impl PyStateGraph {
    #[new]
    fn new(state_schema: &Bound<'_, PyDict>) -> PyResult<Self> {
        // Convert Python dict to Rust schema
        let schema = dict_to_schema(state_schema)?;
        Ok(Self { inner: StateGraph::new(schema) })
    }

    fn add_node(&mut self, name: &str, run: PyObject) -> PyResult<()> {
        // Wrap Python callable for Rust
        self.inner.add_node(name, PyCallableWrapper(run));
        Ok(())
    }

    fn invoke(&self, py: Python<'_>, state: &Bound<'_, PyDict>) -> PyResult<PyObject> {
        // Execute and convert result back to Python
        let result = self.inner.invoke(dict_to_state(state)?)?;
        state_to_dict(py, result)
    }
}

#[pymodule]
fn tea_core(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<PyStateGraph>()?;
    m.add_class::<PyYamlEngine>()?;
    m.add("START", "__start__")?;
    m.add("END", "__end__")?;
    Ok(())
}
```

### Python Wrapper Pattern

```python
# python/src/the_edge_agent/__init__.py
try:
    from tea_core import PyStateGraph as _RustStateGraph
    from tea_core import PyYamlEngine as _RustYamlEngine
    from tea_core import START, END
    _USE_RUST = True
except ImportError:
    # Fallback for development without compiled bindings
    from ._pure_python.stategraph import StateGraph as _RustStateGraph
    from ._pure_python.yaml_engine import YAMLEngine as _RustYamlEngine
    from ._pure_python.stategraph import START, END
    _USE_RUST = False

class StateGraph(_RustStateGraph):
    """State graph for workflow execution.

    This class wraps the Rust implementation via PyO3 bindings.
    API is 100% backward compatible with previous Python implementation.
    """
    pass

class YAMLEngine(_RustYamlEngine):
    """YAML-based agent configuration engine.

    This class wraps the Rust implementation via PyO3 bindings.
    """
    pass

__all__ = ["StateGraph", "YAMLEngine", "START", "END"]
```

### CI/CD Updates Required

1. **GitHub Actions:** Add wheel build job (maturin-action)
2. **PyPI:** Upload pre-built wheels for all platforms
3. **Tests:** Run against both pure Python (fallback) and Rust bindings

## Definition of Done

- [ ] All three stories completed with acceptance criteria met
- [ ] All existing Python tests pass (no modifications except imports)
- [ ] All existing Rust tests pass
- [ ] Pre-built wheels available for Linux, macOS, Windows (x86_64, aarch64)
- [ ] Documentation updated with new architecture
- [ ] No regression in existing features
- [ ] Performance benchmarks show equal or better performance

## Dependencies

**Prerequisites:**
- TEA-RALPHY-001.0 (md-parser PyO3 bindings) - DONE, provides pattern

**Blocked By:**
- None

**Blocks:**
- Future cross-language feature development (single implementation)

## Validation Checklist

### Scope Validation
- [x] Epic can be completed in 3 stories (aggressive but achievable)
- [ ] No new architectural documentation required (uses existing patterns)
- [x] Enhancement follows existing PyO3 patterns (md-parser)
- [x] Integration complexity is manageable (well-defined boundaries)

### Risk Assessment
- [x] Risk to existing system is mitigated (backward-compatible API)
- [x] Rollback plan is feasible (git revert, version pinning)
- [x] Testing approach covers existing functionality (all tests must pass)
- [x] Team has sufficient knowledge (md-parser experience)

### Completeness Check
- [x] Epic goal is clear and achievable
- [x] Stories are properly scoped
- [x] Success criteria are measurable
- [x] Dependencies are identified

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-19 | 0.1 | Initial epic creation | Sarah (PO) |

---

## Story Manager Handoff

**Handoff to Story Manager:**

"Please develop detailed user stories for this brownfield epic. Key considerations:

- This is an enhancement to an existing polyglot system (Python + Rust)
- Integration points: PyO3 bindings between Rust engine and Python API
- Existing patterns to follow: `md-parser` PyO3 implementation (TEA-RALPHY-001.0)
- Critical compatibility requirements: 100% Python API backward compatibility
- Each story must include verification that existing functionality remains intact
- Rollout strategy: Big bang replacement (no feature flag)

The epic should maintain system integrity while delivering single-codebase maintenance for the core engine."
