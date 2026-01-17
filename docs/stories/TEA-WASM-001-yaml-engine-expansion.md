# TEA-WASM-001: WASM YAML Engine Expansion

## Epic Type
Brownfield Enhancement

## Epic Goal

Expand `tea-wasm-llm` from a limited action executor to a conformant YAML agent engine capable of parsing standard TEA YAML workflows, rendering Tera templates, and supporting conditional navigation - enabling browser-based agents with near feature parity to the Python/Rust engines.

## Status
Draft

## Priority
High

## Estimated Effort
~2 weeks (5-7 stories)

---

## Existing System Context

### Current `tea-wasm-llm` Capabilities
- **Location:** `rust/tea-wasm-llm/`
- **Target:** `wasm32-unknown-unknown`
- **Size:** ~2,700 lines
- **Actions:** 17 hardcoded actions (llm.call, lua.eval, storage.*, ltm.*, etc.)
- **Flow:** Simple sequential node execution
- **Templates:** Basic regex `{{ state.key }}` substitution
- **YAML:** No parsing - config passed as JSON

### Technology Stack
- Rust with wasm-bindgen
- serde_yaml (WASM-compatible)
- OpenDAL for storage abstraction
- js-sys/web-sys for browser APIs

### Integration Points
- `execute_yaml()` - Main entry point
- `LlmNodeConfig` / `LlmEdgeConfig` - Current config structs
- Template processing in `process_template()`
- Action dispatch in `execute_node()`

---

## Enhancement Details

### What's Being Added

#### Phase 1: YAML Parsing + Tera Templates + Conditional Edges (~1 week)

1. **Full YAML Parsing**
   - Parse standard TEA YAML format with serde_yaml
   - Support `state_schema`, `variables`, `nodes`, `edges`, `settings`
   - Validate config structure at parse time

2. **Tera Template Engine**
   - Replace regex-based templates with Tera (pure Rust, WASM-compatible)
   - Support filters: `| tojson`, `| upper`, `| lower`, `| default`
   - Support conditionals: `{% if %}`, `{% for %}`
   - Object passthrough for single expressions

3. **Conditional Edges**
   - Support `goto` with `when` conditions
   - Support array-based conditional routing
   - Evaluate conditions using Tera expressions

#### Phase 2: Async Sequential Execution (~1 week)

4. **Async Node Execution**
   - Convert synchronous flow to async/await
   - Enable "simulated parallel" via sequential async execution
   - Support `parallel_results` aggregation pattern

5. **Enhanced Action Set**
   - Add missing core actions from Python/Rust engines
   - Standardize action parameter handling
   - Improve error reporting with context

### Explicit Non-Goals (This Epic)
- ❌ True thread-based parallelism (WASM limitation)
- ❌ Lua runtime changes (deferred)
- ❌ Prolog runtime changes (deferred)
- ❌ Interrupt/checkpoint support (future epic)
- ❌ Full 100+ action parity (incremental)

### How It Integrates
- Backward compatible: existing `execute_yaml()` API unchanged
- New `execute_yaml_workflow()` for full YAML support
- Existing actions remain functional
- Storage backends (OpenDAL) unchanged

### Success Criteria
- [ ] Standard TEA YAML files parse without modification
- [ ] Tera templates render correctly with filters
- [ ] Conditional edges route based on state
- [ ] Async execution completes all nodes
- [ ] Existing demo workflows continue to work
- [ ] Bundle size increase < 100KB

---

## Stories

### Phase 1: Core Engine (~1 week)

#### Story 1: YAML Config Parsing (TEA-WASM-001.1)
**Goal:** Parse standard TEA YAML format into typed config structures

**Scope:**
- Define `WasmYamlConfig` matching Python/Rust schema
- Parse `state_schema`, `variables`, `nodes`, `edges`, `settings`
- Validate required fields and structure
- Error messages with line numbers

**Acceptance Criteria:**
- [ ] Parse `examples/` YAML files successfully
- [ ] Invalid YAML returns descriptive error
- [ ] Config accessible for engine execution

---

#### Story 2: Tera Template Integration (TEA-WASM-001.2)
**Goal:** Replace regex templates with Tera engine

**Scope:**
- Integrate Tera crate (pure Rust, WASM-compatible)
- Register custom filters: `tojson`, `fromjson`
- Support `{{ state.key }}`, `{{ variables.key }}`
- Object passthrough for single expressions
- Template caching for performance

**Acceptance Criteria:**
- [ ] `{{ state.user.name }}` renders correctly
- [ ] `{{ data | tojson }}` serializes objects
- [ ] `{% if state.active %}` conditionals work
- [ ] `{% for item in state.items %}` loops work
- [ ] Template errors include context

---

#### Story 3: Conditional Edge Routing (TEA-WASM-001.3)
**Goal:** Support conditional navigation between nodes

**Scope:**
- Parse `goto` with `when` conditions
- Parse array-based conditional edges
- Evaluate conditions via Tera expressions
- Support `__start__` and `__end__` special nodes
- Fallback to sequential when no match

**Acceptance Criteria:**
- [ ] `goto: nodeB, when: state.score > 0.8` routes correctly
- [ ] Array conditions evaluate in order
- [ ] Default/else fallback works
- [ ] Circular edge detection (optional warning)

---

### Phase 2: Async Execution (~1 week)

#### Story 4: Async Node Executor (TEA-WASM-001.4)
**Goal:** Convert execution to async/await pattern

**Scope:**
- Make `execute_node()` async
- Sequential async execution of node chain
- Proper error propagation with context
- State mutation between nodes

**Acceptance Criteria:**
- [ ] All existing actions work async
- [ ] Errors include node name and action
- [ ] State persists across node transitions
- [ ] No blocking in WASM main thread

---

#### Story 5: Simulated Parallel Execution (TEA-WASM-001.5)
**Goal:** Support fan-out/fan-in pattern via async

**Scope:**
- Detect parallel edge patterns
- Execute "parallel" nodes sequentially (async)
- Aggregate results into `parallel_results`
- Fan-in node receives combined state

**Acceptance Criteria:**
- [ ] Multiple outgoing edges trigger parallel pattern
- [ ] Fan-in node receives all branch results
- [ ] Order is deterministic (edge definition order)
- [ ] Works with conditional parallel branches

---

#### Story 6: Action Parameter Standardization (TEA-WASM-001.6)
**Goal:** Align action parameters with Python/Rust engines

**Scope:**
- Standardize `with:` parameter handling
- Add `output:` field support
- Template processing for all parameters
- Default value handling

**Acceptance Criteria:**
- [ ] `output: result.data` stores correctly
- [ ] Template params: `prompt: "{{ state.question }}"`
- [ ] Missing required params error clearly
- [ ] Optional params use defaults

---

#### Story 7: Integration Testing & Documentation (TEA-WASM-001.7)
**Goal:** Verify end-to-end functionality and document

**Scope:**
- Port key Python/Rust test YAML files
- Browser-based test harness
- Update wasm-demo with new capabilities
- API documentation for new features

**Acceptance Criteria:**
- [ ] 10+ example YAML files execute correctly
- [ ] wasm-demo showcases conditional routing
- [ ] README documents new capabilities
- [ ] Breaking changes documented (if any)

---

## Compatibility Requirements

- [x] Existing `execute_yaml()` API remains functional
- [x] Current 17 actions unchanged
- [x] Storage backends (OpenDAL) unaffected
- [x] Bundle size increase reasonable (< 100KB)
- [x] No new native dependencies

## Technical Considerations

### Dependencies to Add
```toml
[dependencies]
tera = "1.19"  # Pure Rust template engine, WASM-compatible
```

### Dependencies Unchanged
- serde_yaml (already included)
- wasm-bindgen, js-sys, web-sys
- OpenDAL with storage features

### Architecture Notes
- Tera is pure Rust - no C bindings, WASM-safe
- Async via wasm-bindgen-futures (already used)
- No threading required for "simulated parallel"

---

## Risk Mitigation

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Tera bundle size too large | Low | Medium | Tree-shake unused features |
| Template syntax differences | Medium | Low | Document differences, provide migration |
| Async execution ordering | Medium | Medium | Comprehensive test suite |
| Breaking existing demos | Low | High | Backward compatibility layer |

**Rollback Plan:**
- Feature flag for new engine (`use_yaml_engine: true`)
- Keep legacy `execute_yaml()` as fallback
- Version bump with clear changelog

---

## Definition of Done

- [ ] All 7 stories completed with acceptance criteria met
- [ ] Existing `tea-wasm-llm` functionality verified
- [ ] wasm-demo updated with new capabilities
- [ ] Documentation updated in `docs/rust/`
- [ ] No regression in existing features
- [ ] CI passing for WASM builds

---

## Future Considerations (Out of Scope)

These may become separate epics:

1. **TEA-WASM-002:** Lua runtime enhancement (wasmoon integration)
2. **TEA-WASM-003:** Prolog runtime (Trealla WASM)
3. **TEA-WASM-004:** Interrupt/checkpoint support
4. **TEA-WASM-005:** Additional action parity (http.*, file.*, etc.)

---

## References

- Current implementation: `rust/tea-wasm-llm/src/lib.rs`
- Python YAML engine: `python/src/the_edge_agent/yaml_engine.py`
- Rust YAML engine: `rust/src/engine/yaml.rs`
- Tera documentation: https://tera.netlify.app/
- Example YAML agents: `examples/`
