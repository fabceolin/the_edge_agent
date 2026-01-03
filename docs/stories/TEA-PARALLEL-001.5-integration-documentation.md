# Story: TEA-PARALLEL-001.5 - Integration Testing + Documentation

## Status: Ready for Development

**SM Review:** 2026-01-01 - Story checklist PASSED (9/10 clarity)
**Epic**: [TEA-PARALLEL-001 - Multi-Strategy Parallel Execution](./TEA-PARALLEL-001-multi-strategy-execution-epic.md)
**Estimated Tests**: 12 scenarios (revised from 7 by QA)
**Dependencies**:
- [TEA-PARALLEL-001.1](./TEA-PARALLEL-001.1-executor-abstraction.md)
- [TEA-PARALLEL-001.2](./TEA-PARALLEL-001.2-cli-scoped-execution.md)
- [TEA-PARALLEL-001.3](./TEA-PARALLEL-001.3-remote-executor-core.md)
- [TEA-PARALLEL-001.4](./TEA-PARALLEL-001.4-remote-environment-security.md)

---

## User Story

**As a** workflow developer,
**I want** comprehensive documentation and examples for parallel strategies,
**So that** I can choose the right strategy for my use case.

---

## Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | Integration tests for each strategy (thread, process, remote) | E2E tests pass |
| AC2 | Compatibility tests: verify thread remains default | E2E test: default behavior |
| AC3 | Documentation in `YAML_REFERENCE.md` with trade-offs table | Doc lint: section exists |
| AC4 | Example YAML demonstrating each strategy | Example files parseable |
| AC5 | Error handling documented for each failure mode | Doc lint: error section exists |

---

## Technical Design

### Files to Create/Modify

| File | Action | Description |
|------|--------|-------------|
| `docs/shared/YAML_REFERENCE.md` | Modify | Add "Parallel Execution Strategies" section |
| `examples/parallel_strategies_demo.yaml` | Create | Example workflow |
| `examples/parallel_remote_distributed.yaml` | Create | Remote strategy example |
| `python/tests/test_parallel_integration.py` | Create | Cross-strategy integration tests |

### Documentation: YAML_REFERENCE.md Addition

```markdown
## Parallel Execution Strategies

TEA supports three parallel execution strategies for fan-out edges:

### Strategy Comparison

| Strategy | Use Case | Pros | Cons |
|----------|----------|------|------|
| `thread` | I/O-bound tasks, API calls | Low overhead, shared memory | GIL limits CPU parallelism |
| `process` | CPU-bound tasks | True parallelism, bypasses GIL | Serialization overhead |
| `remote` | Distributed execution | Horizontal scaling | Network latency, setup complexity |

### Configuration

```yaml
# Global default
settings:
  parallel:
    strategy: thread  # thread | process | remote
    max_workers: 4

# Per-edge override
edges:
  - from: prepare
    to: [branch_a, branch_b]
    parallel: true
    parallel_strategy: process  # Overrides global
    fan_in: merge
```

### Thread Strategy (Default)

Best for I/O-bound operations like API calls, file reads, or network requests.

```yaml
settings:
  parallel:
    strategy: thread
    max_workers: 10  # Concurrent threads

edges:
  - from: prepare
    to: [fetch_api_a, fetch_api_b, fetch_api_c]
    parallel: true
    fan_in: combine_results
```

**Characteristics:**
- Shared memory (state modifications visible across threads)
- GIL prevents true CPU parallelism
- Low overhead, fast context switching
- Rate limiters and caches are shared

### Process Strategy

Best for CPU-bound operations like data processing, calculations, or transformations.

```yaml
settings:
  parallel:
    strategy: process
    max_workers: 4  # Concurrent processes

edges:
  - from: load_data
    to: [process_chunk_1, process_chunk_2, process_chunk_3]
    parallel: true
    parallel_strategy: process
    fan_in: merge_results
```

**Characteristics:**
- True parallelism, bypasses GIL
- State must be picklable (no lambdas, connections, file handles)
- Higher memory usage (process isolation)
- Rate limiters and caches are per-process

**Serialization Requirements:**
- ✅ `dict`, `list`, `str`, `int`, `float`, `bool`
- ✅ `dataclass` (with picklable fields)
- ❌ `lambda` functions
- ❌ Open file handles
- ❌ Database connections

### Remote Strategy

Best for distributed execution across multiple machines.

```yaml
settings:
  parallel:
    strategy: remote
    remote:
      hosts:
        - user@server1
        - user@server2
      basefile: ./tea
      workdir: /tmp/tea-jobs
      cleanup: true
      env_vars:
        include:
          - OPENAI_API_KEY
          - LOG_LEVEL
        exclude_patterns:
          - "*_SECRET"
        mode: ssh_env

edges:
  - from: prepare
    to: [analyze_region_1, analyze_region_2, analyze_region_3]
    parallel: true
    parallel_strategy: remote
    fan_in: aggregate
```

**Characteristics:**
- Horizontal scaling across machines
- State must be JSON-serializable
- Requires SSH access to remote hosts
- Full TEA engine runs on each remote
- Rate limiters and caches are per-host

**Requirements:**
- SSH key authentication configured
- TEA binary compatible with remote OS/arch
- Sufficient disk space on remotes

### Error Handling

| Strategy | Error Type | Behavior |
|----------|------------|----------|
| All | Timeout | Configurable via `ParallelConfig.timeout_seconds` |
| `process` | Pickle error | Fail fast with clear message before execution |
| `remote` | SSH auth failure | Fail fast with setup instructions |
| `remote` | Network timeout | Retry with exponential backoff |
| All | Partial failure | Configurable `fail_fast` or collect all results |

### Feature Interactions

| Feature | `thread` | `process` | `remote` |
|---------|----------|-----------|----------|
| Rate limiting | Shared | Per-process | Per-host |
| Caching | Shared | Per-process | Per-host |
| LTM | Shared | Shared | Distributed backend required |
| Interrupts | ✅ | ✅ | ❌ Not in remote scope |
```

### Example YAML Files

**examples/parallel_strategies_demo.yaml:**
```yaml
name: parallel-strategies-demo
description: Demonstrates all three parallel execution strategies

state_schema:
  input: str
  thread_results: list
  process_results: list
  remote_results: list

settings:
  parallel:
    strategy: thread  # Default
    max_workers: 4

nodes:
  - name: start
    run: |
      return {"input": "demo data"}

  # Thread strategy - I/O bound
  - name: thread_task_1
    run: |
      import time
      time.sleep(0.1)  # Simulate I/O
      return {"thread_result": "task_1"}

  - name: thread_task_2
    run: |
      import time
      time.sleep(0.1)
      return {"thread_result": "task_2"}

  - name: thread_merge
    run: |
      return {"thread_results": parallel_results}

  # Process strategy - CPU bound
  - name: process_task_1
    run: |
      # CPU-intensive calculation
      result = sum(i * i for i in range(10000))
      return {"process_result": result}

  - name: process_task_2
    run: |
      result = sum(i * i for i in range(10000))
      return {"process_result": result}

  - name: process_merge
    run: |
      return {"process_results": parallel_results}

  - name: end
    run: |
      return state

edges:
  - from: __start__
    to: start

  # Thread parallel
  - from: start
    to: [thread_task_1, thread_task_2]
    parallel: true
    parallel_strategy: thread
    fan_in: thread_merge

  # Process parallel
  - from: thread_merge
    to: [process_task_1, process_task_2]
    parallel: true
    parallel_strategy: process
    fan_in: process_merge

  - from: process_merge
    to: end

  - from: end
    to: __end__
```

**examples/parallel_remote_distributed.yaml:**
```yaml
name: distributed-analysis
description: Distributed execution across remote hosts

state_schema:
  dataset: str
  region_results: dict
  final_report: str

settings:
  parallel:
    strategy: remote
    remote:
      hosts:
        - analyst@region-us.example.com
        - analyst@region-eu.example.com
        - analyst@region-asia.example.com
      basefile: ./tea
      workdir: /opt/tea-analysis
      cleanup: true
      env_vars:
        include:
          - OPENAI_API_KEY
          - DATABASE_URL
        mode: ssh_env

  ltm:
    backend: duckdb
    storage:
      uri: "s3://company-data/ltm/"

nodes:
  - name: prepare_datasets
    run: |
      return {
        "dataset": "global_sales_2024",
        "regions": ["us", "eu", "asia"]
      }

  - name: analyze_region_us
    run: |
      # This runs on region-us host
      from the_edge_agent import ltm
      data = ltm.read(f"sales/{state['dataset']}/us")
      return {"region": "us", "analysis": analyze(data)}

  - name: analyze_region_eu
    run: |
      from the_edge_agent import ltm
      data = ltm.read(f"sales/{state['dataset']}/eu")
      return {"region": "eu", "analysis": analyze(data)}

  - name: analyze_region_asia
    run: |
      from the_edge_agent import ltm
      data = ltm.read(f"sales/{state['dataset']}/asia")
      return {"region": "asia", "analysis": analyze(data)}

  - name: aggregate_results
    run: |
      results = {r["region"]: r["analysis"] for r in parallel_results}
      return {"region_results": results}

  - name: generate_report
    run: |
      report = create_global_report(state["region_results"])
      return {"final_report": report}

edges:
  - from: __start__
    to: prepare_datasets

  - from: prepare_datasets
    to: [analyze_region_us, analyze_region_eu, analyze_region_asia]
    parallel: true
    parallel_strategy: remote
    fan_in: aggregate_results

  - from: aggregate_results
    to: generate_report

  - from: generate_report
    to: __end__
```

---

## Testing

### Test Location

`python/tests/test_parallel_integration.py`

### Test Scenarios (7 total)

#### AC1: Strategy Integration Tests (4 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.5-E2E-001 | E2E | P0 | Thread strategy: simple fan-out/fan-in |
| 001.5-E2E-002 | E2E | P0 | Process strategy: simple fan-out/fan-in |
| 001.5-E2E-003 | E2E | P1 | Remote strategy: mocked SSH execution |
| 001.5-E2E-004 | E2E | P1 | Mixed strategies in same workflow |

#### AC2: Compatibility Tests (2 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.5-INT-001 | Integration | P0 | **Critical**: Legacy YAML uses ThreadExecutor |
| 001.5-INT-002 | Integration | P1 | Performance baseline: no overhead vs old impl |

#### AC3-5: Documentation Tests (1 test)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.5-UNIT-001 | Unit | P3 | Example YAML files are valid and parseable |

---

## Definition of Done

- [ ] Integration tests for all three strategies
- [ ] Backward compatibility tests pass
- [ ] `YAML_REFERENCE.md` updated with strategies section
- [ ] `parallel_strategies_demo.yaml` example created
- [ ] `parallel_remote_distributed.yaml` example created
- [ ] All 7 test scenarios pass
- [ ] Documentation reviewed for accuracy
- [ ] Code reviewed and merged

---

## Testing Matrix

| Test Case | Thread | Process | Remote |
|-----------|--------|---------|--------|
| Simple fan-out/fan-in | ✅ | ✅ | ✅ (mocked) |
| Complex nested state | ✅ | ✅ (pickle) | ✅ (JSON) |
| Timeout handling | ✅ | ✅ | ✅ |
| Retry on failure | ✅ | ✅ | ✅ |
| Circuit breaker | ✅ | ✅ | ✅ |
| Non-serializable state | N/A | Error | Error |
| Network failure | N/A | N/A | Retry/Error |
| Mixed strategies | ✅ | ✅ | ✅ |

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Documentation out of sync | Medium | Link docs to tests |
| Example YAML becomes stale | Low | Parse examples in tests |
| Performance regression | High | Baseline benchmark |

---

## Notes for Developer

1. **Performance baseline**: Capture timing of 100 parallel tasks with old impl before starting. Assert <5% overhead with new abstraction.

2. **Mock SSH in E2E**: For remote strategy E2E tests, mock subprocess calls rather than requiring actual SSH.

3. **Example validation**: Parse all example YAML files in tests to ensure they're valid.

4. **Doc sections**: Ensure YAML_REFERENCE.md has:
   - Strategy comparison table
   - Configuration examples
   - Error handling section
   - Feature interaction matrix

---

## QA Results

**Review Date:** 2026-01-01
**Reviewer:** Quinn (Test Architect)
**Status:** Test Design Complete

### Test Design Summary

| Metric | Original Estimate | Revised |
|--------|-------------------|---------|
| Total Scenarios | 7 | 12 |
| Unit Tests | 1 | 3 (25%) |
| Integration Tests | 2 | 4 (33%) |
| E2E Tests | 4 | 5 (42%) |

### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| P0 | 4 | Backward compatibility, core strategy validation |
| P1 | 5 | Performance baseline, mixed strategies, examples |
| P2 | 2 | Documentation section validation |
| P3 | 1 | Remote example schema validation |

### Coverage Assessment

| AC | Coverage | Notes |
|----|----------|-------|
| AC1 | Full | 4 E2E tests cover thread, process, remote, mixed |
| AC2 | Full | 2 integration tests for default behavior + performance |
| AC3 | Full | 2 unit tests for doc section + trade-offs table |
| AC4 | Full | 3 tests for example parsing + execution |
| AC5 | Full | 1 E2E test for actionable error messages |

### Risk Coverage

| Risk | Test ID | Mitigation |
|------|---------|------------|
| RISK-001: Process serialization | 001.5-E2E-005 | Actionable pickle error messages |
| RISK-006: Backward compatibility | 001.5-INT-001 | Legacy YAML defaults to thread |
| RISK-PERF: Performance regression | 001.5-INT-002 | <5% overhead benchmark |

### Recommendations

1. **Expanded test count**: Increased from 7 to 12 scenarios to ensure comprehensive coverage of documentation-as-code validation
2. **Fail-fast execution order**: P0 tests (compatibility, core strategies) run first
3. **Documentation tests**: Added explicit tests for YAML_REFERENCE.md sections to prevent doc rot

### Test Design Document

[TEA-PARALLEL-001.5-test-design-20260101.md](../qa/assessments/TEA-PARALLEL-001.5-test-design-20260101.md)

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-01 | 1.0 | Story extracted from epic | Sarah (PO) |
| 2026-01-01 | 1.1 | QA test design review complete | Quinn (QA) |
| 2026-01-01 | 1.2 | SM story checklist passed (9/10), test count updated | Bob (SM) |
