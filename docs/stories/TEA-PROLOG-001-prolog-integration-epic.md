# Epic TEA-PROLOG-001: Prolog Integration for Neurosymbolic AI

## Status

**In Progress** (Stories Approved, Test Design Complete)

## Epic Goal

Integrate SWI-Prolog scripting support into The Edge Agent (both Python and Rust implementations), enabling neurosymbolic AI workflows that combine neural network outputs with symbolic logic reasoning, constraint solving, and rule-based inference.

## Epic Description

### Business Context

**Neurosymbolic AI** combines the pattern recognition capabilities of neural networks with the logical reasoning capabilities of symbolic AI. This hybrid approach enables:

- **Explainable decisions**: Logic rules provide human-readable reasoning chains
- **Constraint satisfaction**: CLP(FD) solves discrete constraints from classifier outputs
- **Knowledge integration**: Domain rules combine with learned patterns
- **Reliable inference**: Symbolic logic provides deterministic conclusions

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                        SWI-Prolog Engine                        │
│                    (System Installation)                        │
│         CLP(FD) | CLP(R) | Tabling | JSON Support              │
└─────────────────────────────────────────────────────────────────┘
                    ▲                           ▲
                    │ FFI                       │ FFI
          ┌─────────┴─────────┐       ┌────────┴────────┐
          │      pyswip       │       │    swipl-rs     │
          │  (Python bindings)│       │  (Rust bindings)│
          └─────────┬─────────┘       └────────┬────────┘
                    │                          │
          ┌─────────┴─────────┐       ┌────────┴────────┐
          │   PrologRuntime   │       │  PrologRuntime  │
          │   (Python TEA)    │       │   (Rust TEA)    │
          └─────────┬─────────┘       └────────┬────────┘
                    │                          │
          ┌─────────┴─────────┐       ┌────────┴────────┐
          │    YAMLEngine     │       │   YamlEngine    │
          │  run: {type: prolog}      │  run: {type: prolog}
          └───────────────────┘       └─────────────────┘
```

### Existing System Context

**Current Relevant Functionality:**
- `LuaRuntime` class in `python/src/the_edge_agent/lua_runtime.py` - reference pattern
- `LuaRuntime` struct in `rust/src/engine/lua_runtime.rs` - reference pattern
- `YAMLEngine._create_run_function()` - multi-language execution dispatch
- `YAMLEngine._detect_lua_code()` - language detection heuristics
- Existing `language: lua` YAML configuration pattern

**Technology Stack:**
- Python 3.11+ with pyswip (SWI-Prolog bindings)
- Rust with swipl-rs crate (terminusdb-labs/swipl-rs)
- SWI-Prolog 9.x (system installation required)

**Integration Points:**
- `YAMLEngine.__init__()` - Add `prolog_enabled`, `prolog_timeout` options
- `_create_run_function()` - Add `type: prolog` dispatch
- `_evaluate_condition()` - Add Prolog condition evaluation (optional)
- New environment variables: `SWI_HOME_DIR` (optional, for custom installs)

### Why SWI-Prolog?

| Feature | Benefit for Neurosymbolic AI |
|---------|------------------------------|
| **CLP(FD)** | Discrete constraint solving from classifier outputs |
| **CLP(R)** | Real-valued constraint solving from regression outputs |
| **Tabling** | Memoization for repeated inference queries |
| **JSON Support** | Native `library(http/json)` for state conversion |
| **Sandboxing** | Built-in `sandbox` library for security |
| **Same Engine** | True cross-runtime parity (Python + Rust use same engine) |
| **Thread-Local Facts** | Native parallel isolation via `:- thread_local` |
| **Maturity** | 35+ years development, excellent documentation |

### Parallel Execution Strategy

**Architectural Decision: Thread-Local Predicates**

Unlike Lua (which creates fresh VM instances per parallel branch), Prolog uses SWI-Prolog's native **thread-local predicates** for isolation:

```prolog
:- thread_local state/2.
:- thread_local return_value/2.
```

| Approach | Lua | Prolog |
|----------|-----|--------|
| **Isolation Method** | Fresh LuaRuntime per branch | Thread-local facts |
| **Overhead** | ~1-5ms (create VM) | ~0ms (native threading) |
| **State Isolation** | Complete (separate VMs) | Per-thread facts only |
| **Rule Sharing** | N/A | Shared rules visible |
| **Cross-Runtime Parity** | LuaJIT vs Lua 5.4 differences | Same SWI-Prolog engine |

**Why not separate Prolog engines?**
- SWI-Prolog engine creation is heavy (~50-100ms)
- Thread-local predicates are the idiomatic solution
- Same behavior in pyswip (Python) and swipl-rs (Rust)
- Rules can be shared while state remains isolated

### Enhancement Details

**What's Being Added:**

1. **PrologRuntime Class (Python)**: New runtime using `pyswip` that provides:
   - State access via `state/2` predicate
   - Query execution with variable binding extraction
   - Timeout protection via `call_with_time_limit/2`
   - Sandboxed execution for security
   - CLP support (FD, R, B)

2. **PrologRuntime Struct (Rust)**: New runtime using `swipl-rs` crate that provides:
   - Same API surface as Python for cross-runtime parity
   - JSON ↔ Prolog term conversion
   - Timeout and sandbox support

3. **YAML Syntax Extensions**:
   - `language: prolog` global or per-node setting
   - `run: { type: prolog, code: "..." }` explicit syntax
   - `consult: "rules.pl"` for external rule files
   - Prolog-specific settings (timeout, sandbox mode)

4. **Neurosymbolic Examples**: Reference implementations showing:
   - Neural classifier → Prolog rules → Symbolic reasoning
   - CLP(FD) constraint solving from embeddings
   - Knowledge graph reasoning patterns

**Success Criteria:**
- Prolog code executes identically in Python and Rust TEA runtimes
- CLP(FD) constraints can be defined and solved
- State from neural nodes is accessible in Prolog nodes
- Timeout protection prevents infinite recursion
- Sandboxing prevents file/network access
- Clear error messages when SWI-Prolog not installed

## Stories

### Story 1: TEA-PY-004 - Prolog Scripting Support in Python TEA

**Status:** Approved (Test Design Complete - 42 scenarios)

**Description:** Implement `PrologRuntime` class for Python using `pyswip` library, mirroring the `LuaRuntime` pattern. Enable Prolog code execution in YAML agents with state access, timeout protection, and sandboxing.

**Scope:**
- New file: `python/src/the_edge_agent/prolog_runtime.py`
- Modify: `python/src/the_edge_agent/yaml_engine.py`
- New tests: `python/tests/test_prolog_runtime.py`
- Documentation updates

**Link:** [TEA-PY-004-prolog-scripting-support.md](TEA-PY-004-prolog-scripting-support.md)

---

### Story 2: TEA-RUST-035 - Prolog Scripting Support in Rust TEA

**Status:** Approved (Test Design Complete - 48 scenarios)

**Description:** Implement `PrologRuntime` struct for Rust using `swipl-rs` crate, providing the same API surface as Python for cross-runtime parity.

**Scope:**
- New file: `rust/src/engine/prolog_runtime.rs`
- Modify: `rust/src/engine/mod.rs`, `rust/src/engine/yaml.rs`
- New tests: `rust/tests/test_prolog_runtime.rs`
- Cargo.toml dependency updates

**Link:** [TEA-RUST-035-prolog-scripting-support.md](TEA-RUST-035-prolog-scripting-support.md)

---

### Story 3: TEA-PROLOG-002 - Cross-Runtime Parity Tests (Future)

**Status:** Blocked by TEA-PY-004, TEA-RUST-035

**Description:** Create comprehensive parity tests ensuring the same Prolog YAML agents produce identical results in both Python and Rust runtimes.

**Scope:**
- Parity test fixtures in `examples/prolog/`
- Cross-runtime test harness
- CLP(FD) parity verification
- Edge case documentation

---

### Story 4: TEA-PROLOG-003 - Neurosymbolic Examples & Documentation (Future)

**Status:** Blocked by TEA-PY-004, TEA-RUST-035

**Description:** Create reference implementations and documentation for neurosymbolic AI patterns using Prolog integration.

**Scope:**
- Example: Neural classifier + rule engine
- Example: CLP(FD) constraint solving
- Example: Knowledge graph reasoning
- Tutorial documentation

## YAML Syntax

### Basic Prolog Node

```yaml
name: reasoning-agent
state_schema:
  input: str
  classification: str
  confidence: float
  reasoning: str

nodes:
  - name: classify
    run: |
      # Python node - neural network inference
      result = classifier.predict(state["input"])
      return {"classification": result.label, "confidence": result.score}

  - name: reason
    language: prolog
    run: |
      % Access state via state/2 predicate
      state(classification, Class),
      state(confidence, Conf),
      Conf > 0.7,
      % Apply domain rules
      applies_rule(Class, Conclusion),
      % Return result via return/2
      return(reasoning, Conclusion).

edges:
  - from: __start__
    to: classify
  - from: classify
    to: reason
  - from: reason
    to: __end__
```

### CLP(FD) Constraint Solving

```yaml
nodes:
  - name: solve_constraints
    language: prolog
    run: |
      :- use_module(library(clpfd)).

      state(min_value, Min),
      state(max_value, Max),
      state(sum_target, Target),

      % Define variables
      X in Min..Max,
      Y in Min..Max,
      Z in Min..Max,

      % Constraints
      X + Y + Z #= Target,
      X #< Y,
      Y #< Z,

      % Solve
      label([X, Y, Z]),

      return(solution, [X, Y, Z]).
```

### External Rules File

```yaml
settings:
  prolog:
    consult:
      - rules/domain_rules.pl
      - rules/inference_rules.pl

nodes:
  - name: apply_rules
    language: prolog
    run: |
      state(facts, Facts),
      assert_facts(Facts),
      infer_conclusions(Conclusions),
      return(conclusions, Conclusions).
```

## Compatibility Requirements

- [x] Existing Lua scripting continues to work unchanged
- [x] Existing Python `run:` blocks continue to work unchanged
- [x] Existing Jinja2/Tera condition evaluation unchanged
- [x] **CRITICAL: SWI-Prolog is OPTIONAL** - graceful degradation when not installed
- [x] **CRITICAL: pyswip/swipl-rs are OPTIONAL** - clear install instructions on ImportError
- [x] No breaking changes to existing API signatures
- [x] Performance impact negligible when Prolog disabled

## Optional Dependency Model

**Python:**
```bash
# Install with Prolog support
pip install the-edge-agent[prolog]

# Requires SWI-Prolog system installation
# Ubuntu/Debian: apt install swi-prolog
# macOS: brew install swi-prolog
# Windows: Download from swi-prolog.org
```

**Rust:**
```toml
# Cargo.toml - optional feature
[dependencies]
swipl = { version = "0.3", optional = true }

[features]
prolog = ["swipl"]
```

**Behavior when not installed:**
- `language: prolog` raises clear error with install instructions
- `type: prolog` in run config provides same guidance
- All other functionality works unchanged

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| SWI-Prolog install complexity | Medium | Medium | Clear docs, Docker option |
| pyswip/swipl-rs API instability | Low | Medium | Pin versions, isolate in runtime class |
| Prolog query infinite loops | Medium | High | Timeout via `call_with_time_limit/2` |
| Sandbox bypass | Low | High | Use SWI-Prolog's built-in sandbox library |
| Cross-runtime parity issues | Medium | Medium | Same engine (SWI-Prolog) for both |

## Definition of Done

- [ ] TEA-PY-004 completed with all acceptance criteria
- [ ] TEA-RUST-035 completed with all acceptance criteria
- [ ] Same Prolog YAML agent produces same results in Python and Rust
- [ ] CLP(FD) works in both runtimes
- [ ] Timeout protection verified
- [ ] Sandbox prevents file/network access
- [ ] Documentation updated (YAML_REFERENCE.md, getting-started docs)
- [ ] No regressions in existing Lua or Python functionality
- [ ] Optional dependency handling tested

## Technical References

### SWI-Prolog Resources
- [SWI-Prolog Documentation](https://www.swi-prolog.org/pldoc/doc_for?object=manual)
- [CLP(FD) Guide](https://www.swi-prolog.org/pldoc/man?section=clpfd)
- [Sandbox Library](https://www.swi-prolog.org/pldoc/man?section=sandbox)
- [JSON Library](https://www.swi-prolog.org/pldoc/man?section=json)

### Library Documentation
- [pyswip GitHub](https://github.com/yuce/pyswip)
- [pyswip PyPI](https://pypi.org/project/pyswip/)
- [swipl-rs GitHub](https://github.com/terminusdb-labs/swipl-rs)
- [swipl-rs Crates.io](https://crates.io/crates/swipl)

### Neurosymbolic AI References
- [Neurosymbolic AI: The 3rd Wave](https://arxiv.org/abs/2012.05876)
- [Logic Tensor Networks](https://arxiv.org/abs/2012.13635)

## QA Summary

### Test Design Status

| Story | Status | Scenarios | P0 Tests | Design Document |
|-------|--------|-----------|----------|-----------------|
| TEA-PY-004 | Approved | 42 | 16 | `docs/qa/assessments/TEA-PY-004-test-design-20251221.md` |
| TEA-RUST-035 | Approved | 48 | 18 | `docs/qa/assessments/TEA-RUST-035-test-design-20251221.md` |
| **Total** | | **90** | **34** | |

### Key Quality Focus Areas

| Area | Python Tests | Rust Tests | Notes |
|------|--------------|------------|-------|
| Sandbox Security | 6 (P0) | 6 (P0) | Blocks file/network/shell |
| Timeout Protection | 5 | 5 | `call_with_time_limit/2` |
| JSON Conversion | 9 | 9 | Bidirectional type mapping |
| Parallel Isolation | 2 (P0) | 2 (P0) | Thread-local predicates |
| Cross-Runtime Parity | 2 | 6 | Same SWI-Prolog engine |
| Backward Compatibility | 4 (P0) | 4 (P0) | Python/Lua/Jinja2/Tera |

### Reviewer Notes

**Quinn (Test Architect) - 2025-12-21:**

Both stories are well-defined with comprehensive acceptance criteria. The test designs provide:
- Strong shift-left strategy (60% unit tests)
- Security-critical tests prioritized as P0
- Cross-runtime parity explicitly tested
- Parallel isolation via thread-local predicates validated
- Backward compatibility regression tests included

**Recommendation:** Both stories APPROVED for implementation.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-21 | 0.1 | Initial epic draft | Sarah (PO) |
| 2025-12-21 | 0.2 | Test design complete for both stories, epic status → In Progress | Quinn (QA) |
