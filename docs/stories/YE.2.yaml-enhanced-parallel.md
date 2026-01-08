# Story YE.2: YAML Engine Enhanced Parallel Execution

## Status
Ready for Development

**QA Test Design:** ✅ Approved (2026-01-07)
- Test design completed and validated
- 47 test scenarios across 3 levels (28 unit, 15 integration, 4 E2E)
- All 28 acceptance criteria covered
- No blockers identified
- Backward compatibility explicitly validated

## Story
**As a** developer using YAML-based agent configurations,
**I want** advanced parallel execution patterns like matrix strategies and dynamic parallelism,
**so that** I can process multiple items concurrently, test across combinations, and optimize workflow performance without writing Python code.

## Context
This story enhances the YAML Engine (`src/the_edge_agent/yaml_engine.py`) parallel execution capabilities. The current implementation supports basic parallel edges with fan-in nodes. This story adds GitHub Actions-style matrix strategies for static combinations and dynamic parallelism based on runtime state values.

**Builds upon:**
- Existing parallel edge support (`type: parallel`, `fan_in:`)
- Core StateGraph `ThreadPoolExecutor` parallel execution (TD.5)
- Core StateGraph thread safety (TD.4)

**Inspired by:**
- GitHub Actions `strategy.matrix`
- GitLab CI `parallel:matrix`
- Airflow dynamic task mapping

**Follows patterns:**
- YAML configuration structure from `docs/YAML_AGENTS.md`
- Existing parallel edge configuration
- Template variable pattern (`{{ variable }}`)

## Acceptance Criteria

### Matrix Strategy (Static Combinations)
1. Nodes support `strategy.matrix` configuration for static parallel combinations
2. Matrix defines named parameters with lists of values
3. Each combination spawns a parallel execution branch
4. Matrix variables accessible in node via `{{ matrix.param_name }}`
5. Fan-in node receives results from all matrix combinations
6. Optional `strategy.fail_fast: true` stops all branches on first failure
7. Optional `strategy.max_parallel` limits concurrent executions

### Dynamic Parallelism (State-Based)
8. Nodes support `parallel_each: "{{ state.items }}"` for dynamic parallelism
9. `parallel_each` iterates over list from state at runtime
10. Current item accessible via `{{ item }}` or `{{ item.field }}`
11. Current index accessible via `{{ item_index }}`
12. Fan-in node receives results array matching input order
13. Empty list results in no parallel executions (skip to fan-in with empty results)

### Worker Pool Configuration
14. YAML `config.max_workers` sets ThreadPoolExecutor max workers
15. Default max_workers follows core StateGraph default (None = CPU count)
16. Per-node `max_workers` override supported for fine-grained control

### Result Collection
17. Matrix results include matrix parameter values: `{"matrix": {...}, "result": ...}`
18. Dynamic parallel results include item and index: `{"item": ..., "index": N, "result": ...}`
19. Results maintain deterministic ordering (by matrix combo or item index)
20. Fan-in node `parallel_results` contains structured result objects

### Integration Requirements
21. Existing parallel edge syntax continues to work unchanged
22. Matrix and parallel_each can be combined with conditional edges
23. Checkpoint persistence works with enhanced parallel (captures main thread)
24. All existing tests pass without modification

### Quality Requirements
25. Clear error messages for: invalid matrix config, non-iterable parallel_each, missing fan_in
26. New functionality covered by unit tests
27. Docstrings document new parameters
28. YAML_AGENTS.md updated with enhanced parallel documentation

## Tasks / Subtasks

- [ ] **Task 1: Implement matrix strategy parsing** (AC: 1, 2, 3, 4)
  - [ ] Parse `strategy.matrix` in node configuration
  - [ ] Generate all combinations from matrix parameters
  - [ ] Create parallel execution branches for each combination
  - [ ] Inject `matrix` dict into node execution context
  - [ ] Add `{{ matrix.* }}` to template variable resolution

- [ ] **Task 2: Implement matrix fan-in** (AC: 5, 17, 19)
  - [ ] Connect all matrix branches to specified fan-in node
  - [ ] Structure results with matrix params: `{"matrix": {...}, "result": ...}`
  - [ ] Maintain deterministic ordering of results
  - [ ] Pass structured results to fan-in node's `parallel_results`

- [ ] **Task 3: Implement matrix options** (AC: 6, 7)
  - [ ] Parse `strategy.fail_fast` option (default: false)
  - [ ] If fail_fast, cancel remaining branches on first failure
  - [ ] Parse `strategy.max_parallel` option
  - [ ] Limit concurrent matrix executions to max_parallel

- [ ] **Task 4: Implement parallel_each parsing** (AC: 8, 9, 10, 11)
  - [ ] Parse `parallel_each` node attribute
  - [ ] Evaluate expression at runtime to get iterable
  - [ ] Create parallel branch for each item
  - [ ] Inject `item` and `item_index` into execution context
  - [ ] Add `{{ item }}` and `{{ item_index }}` to template resolution

- [ ] **Task 5: Implement parallel_each fan-in** (AC: 12, 13, 18, 19)
  - [ ] Connect all item branches to specified fan_in node
  - [ ] Structure results: `{"item": ..., "index": N, "result": ...}`
  - [ ] Maintain index-based ordering
  - [ ] Handle empty list case (skip to fan-in with `parallel_results=[]`)

- [ ] **Task 6: Implement worker pool configuration** (AC: 14, 15, 16)
  - [ ] Parse `config.max_workers` from YAML
  - [ ] Pass to StateGraph or ThreadPoolExecutor
  - [ ] Support per-node `max_workers` override
  - [ ] Document interaction with matrix.max_parallel

- [ ] **Task 7: Verify integration scenarios** (AC: 21, 22, 23, 24)
  - [ ] Test existing parallel edges still work
  - [ ] Test matrix with conditional edges
  - [ ] Test parallel_each with conditional edges
  - [ ] Test checkpoint at matrix fan-in node
  - [ ] Verify all existing tests pass

- [ ] **Task 8: Add tests** (AC: 24, 26)
  - [ ] Test matrix strategy creates correct combinations
  - [ ] Test matrix variables accessible in nodes
  - [ ] Test matrix fan-in receives all results
  - [ ] Test matrix fail_fast behavior
  - [ ] Test matrix max_parallel limiting
  - [ ] Test parallel_each with state list
  - [ ] Test parallel_each item/item_index injection
  - [ ] Test parallel_each empty list handling
  - [ ] Test parallel_each result ordering
  - [ ] Test config.max_workers
  - [ ] Test error handling for invalid configs
  - [ ] Verify existing tests still pass

- [ ] **Task 9: Update documentation** (AC: 27, 28)
  - [ ] Add docstrings to new configuration handlers
  - [ ] Update docs/YAML_AGENTS.md with Matrix Strategy section
  - [ ] Update docs/YAML_AGENTS.md with Dynamic Parallelism section
  - [ ] Add comprehensive examples for both patterns
  - [ ] Update CLAUDE.md if needed

## Dev Notes

### File Locations
- `src/the_edge_agent/yaml_engine.py` - main implementation
- `src/the_edge_agent/stategraph.py` - may need minor extensions for max_workers config
- `tests/test_yaml_engine.py` - new tests
- `docs/YAML_AGENTS.md` - documentation updates

### Matrix Strategy YAML Example
```yaml
name: matrix-workflow
description: Test across multiple configurations

nodes:
  - name: test_version
    strategy:
      matrix:
        python_version: ["3.9", "3.10", "3.11"]
        os: [ubuntu, macos]
      fail_fast: false
      max_parallel: 4
    fan_in: collect_results
    run: |
      version = "{{ matrix.python_version }}"
      os_name = "{{ matrix.os }}"
      # Simulate test
      return {"passed": True, "version": version, "os": os_name}

  - name: collect_results
    fan_in: true
    run: |
      # parallel_results contains 6 items (3 versions × 2 os)
      # Each: {"matrix": {"python_version": "3.9", "os": "ubuntu"}, "result": {...}}
      all_passed = all(r["result"]["passed"] for r in parallel_results)
      return {"all_passed": all_passed, "total_tests": len(parallel_results)}

edges:
  - from: __start__
    to: test_version
  - from: collect_results
    to: __end__
```

### Dynamic Parallelism YAML Example
```yaml
name: dynamic-parallel-workflow
description: Process items from state in parallel

state_schema:
  items: list
  results: list

nodes:
  - name: process_items
    parallel_each: "{{ state.items }}"
    fan_in: aggregate
    run: |
      # item is the current item from state.items
      # item_index is 0, 1, 2, ...
      processed = {
          "id": "{{ item.id }}",
          "index": {{ item_index }},
          "value": "{{ item.value }}".upper()
      }
      return {"processed": processed}

  - name: aggregate
    fan_in: true
    run: |
      # parallel_results contains one entry per item
      # Each: {"item": {...}, "index": N, "result": {"processed": {...}}}
      results = [r["result"]["processed"] for r in parallel_results]
      # Results are ordered by index
      return {"results": results}

edges:
  - from: __start__
    to: process_items
  - from: aggregate
    to: __end__
```

### Combined Matrix + Parallel_each (Advanced)
```yaml
nodes:
  - name: process_batch
    strategy:
      matrix:
        model: [gpt-4, gpt-3.5-turbo]
    parallel_each: "{{ state.queries }}"
    fan_in: combine_all
    run: |
      # Runs for each (model × query) combination
      model = "{{ matrix.model }}"
      query = "{{ item }}"
      return {"model": model, "query": query, "response": "..."}
```

### Worker Pool Configuration
```yaml
config:
  max_workers: 8  # Global limit

nodes:
  - name: heavy_processing
    max_workers: 2  # Override for this node only
    parallel_each: "{{ state.large_items }}"
    fan_in: collect
    run: |
      # Only 2 concurrent executions for heavy work
      return {"processed": True}
```

### Implementation Considerations

1. **Matrix combination generation:**
   ```python
   from itertools import product

   def generate_matrix_combinations(matrix_config):
       keys = list(matrix_config.keys())
       values = [matrix_config[k] for k in keys]
       for combo in product(*values):
           yield dict(zip(keys, combo))
   ```

2. **Dynamic parallel_each evaluation:**
   - Evaluate `parallel_each` expression at node entry time
   - Must resolve to a list/iterable
   - Error if not iterable

3. **Result ordering:**
   - Matrix: sort by matrix param values (deterministic)
   - parallel_each: preserve original index order
   - Use `enumerate()` and store index with each result

4. **Fail-fast implementation:**
   - Use `concurrent.futures.wait()` with `FIRST_EXCEPTION`
   - Cancel pending futures on failure
   - Still collect completed results for fan-in

5. **Context injection for templates:**
   - Add `matrix` dict to template context for matrix nodes
   - Add `item` and `item_index` to template context for parallel_each

### Testing Strategy
- Create inline YAML configs for tests
- Test small matrices (2×2) for quick execution
- Test parallel_each with lists of 3-5 items
- Mock slow operations to test fail_fast quickly
- Verify result ordering with index assertions

## Definition of Done
- [ ] All acceptance criteria met
- [ ] All tasks completed
- [ ] Existing tests pass (`pytest tests/`)
- [ ] New tests pass
- [ ] No regressions in existing functionality
- [ ] Code follows existing patterns
- [ ] Documentation updated

## Risk Assessment
- **Primary Risk:** Complex interaction between matrix and parallel_each could be confusing
- **Mitigation:** Clear documentation with examples; validate configs and provide helpful errors
- **Secondary Risk:** fail_fast with many branches could leave orphan threads
- **Mitigation:** Proper cleanup in finally block; use executor context manager
- **Rollback:** Feature is additive; existing parallel syntax unchanged

## QA Notes

**Test Design Completed:** 2026-01-07
**Test Architect:** Quinn
**Assessment Document:** `docs/qa/assessments/YE.2-test-design-20260107.md`

### Test Coverage Summary

- **Total test scenarios:** 47
- **Unit tests:** 28 (59.6%)
- **Integration tests:** 15 (31.9%)
- **E2E tests:** 4 (8.5%)
- **Priority distribution:** P0: 22, P1: 19, P2: 6

### Risk Areas Identified

1. **Critical: Thread Safety in Parallel Execution**
   - Parallel branches must not share mutable state
   - Covered by: YE.2-INT-035 (P0)
   - Mitigation: Deep copies of state, proper executor cleanup

2. **Critical: Result Ordering Determinism**
   - Matrix and parallel_each results must maintain predictable order
   - Covered by: YE.2-INT-018, YE.2-INT-019, YE.2-UNIT-024, YE.2-UNIT-025 (P0)
   - Critical for reproducible workflows and debugging

3. **Critical: Backward Compatibility**
   - Existing parallel edges and YAML agents must work unchanged
   - Covered by: YE.2-INT-028, YE.2-E2E-003, YE.2-INT-033 (P0)
   - Rollback safe: feature is purely additive

4. **High: Fail-Fast Thread Cleanup**
   - fail_fast=true could leave orphan threads
   - Covered by: YE.2-INT-008 (P0)
   - Requires proper executor context manager and finally blocks

5. **Medium: Complex Feature Interaction**
   - Matrix + parallel_each + conditional edges combinations
   - Covered by: YE.2-E2E-004, YE.2-INT-029, YE.2-INT-030 (P1)
   - Needs clear documentation and error messages

### Recommended Test Scenarios

**Phase 1: Critical Unit Tests (Run First)**
- All parsing and configuration validation (YE.2-UNIT-001 to YE.2-UNIT-028)
- Fast feedback loop: ~45 seconds total
- Fail-fast approach: catch logic errors early

**Phase 2: Critical Integration Tests**
- Core parallel execution: YE.2-INT-001, YE.2-INT-002, YE.2-INT-006
- Dynamic parallelism: YE.2-INT-012, YE.2-INT-013, YE.2-INT-014
- Result ordering: YE.2-INT-018, YE.2-INT-020
- Thread safety: YE.2-INT-035
- Backward compatibility: YE.2-INT-028

**Phase 3: Regression Validation**
- Full test suite: YE.2-INT-033 (P0)
- Existing YAML agents: YE.2-E2E-003 (P0)
- Critical before merging

**Phase 4: E2E Validation**
- Complete workflows: YE.2-E2E-001, YE.2-E2E-002
- Advanced combinations: YE.2-E2E-004

### Quality Gate Decision

**RECOMMENDATION:** Test design approved, implementation ready to proceed.

**Strengths:**
- Comprehensive coverage of all 28 acceptance criteria
- Strong P0 focus on correctness (22 P0 tests)
- Explicit backward compatibility validation
- Thread safety explicitly tested
- Clear error handling requirements

**Requirements for Story Completion:**
1. All P0 tests must pass (22 tests)
2. All P1 tests must pass (19 tests)
3. Backward compatibility tests must pass (YE.2-INT-028, YE.2-E2E-003, YE.2-INT-033)
4. Documentation updated (YAML_AGENTS.md) with examples
5. No regressions in existing test suite

**Estimated Test Execution Time:**
- Unit: ~45 seconds
- Integration: ~3 minutes
- E2E: ~2 minutes
- **Total:** ~5.75 minutes per run

**CI/CD Recommendation:**
- Run P0 tests on every commit (~2 minutes)
- Run full suite on PR merge (~6 minutes)

### Concerns / Blockers

**NONE** - Story is well-defined and testable.

**Notes:**
- Matrix combination generation using `itertools.product` is straightforward
- Template variable injection patterns already established
- ThreadPoolExecutor management follows existing StateGraph patterns
- Clear error messages specified for common misconfiguration scenarios

### QA Signoff

**Status:** ✅ **APPROVED FOR IMPLEMENTATION**

Test design is comprehensive, risk-aware, and pragmatic. All acceptance criteria are testable. No blockers identified.

---

## Change Log
| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2026-01-07 | 0.2 | QA Notes added | Quinn (Test Architect) |
