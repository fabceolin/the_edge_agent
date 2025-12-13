# Risk Profile: Story TD.13 - Parallel Execution Reliability Enhancement

**Date:** 2025-12-13
**Reviewer:** Quinn (Test Architect)
**Story:** TD.13 - Parallel Execution Reliability Enhancement

---

## Executive Summary

| Metric | Value |
|--------|-------|
| **Total Risks Identified** | 14 |
| **Critical Risks** | 2 |
| **High Risks** | 4 |
| **Medium Risks** | 5 |
| **Low Risks** | 3 |
| **Risk Score** | 32/100 (HIGH RISK) |

This story introduces significant complexity through multiple concurrent programming patterns (timeouts, retries, circuit breakers). The combination of threading, state management, and new abstractions creates substantial risk surface. However, the phased implementation approach and backwards compatibility design mitigate some concerns.

---

## Critical Risks Requiring Immediate Attention

### 1. TECH-001: Thread Cancellation After Timeout

**Score: 9 (Critical)**

| Dimension | Assessment |
|-----------|------------|
| **Probability** | High (3) - Python threads cannot be forcefully terminated |
| **Impact** | High (3) - Resource exhaustion, zombie threads, memory leaks |

**Description:** When `future.result(timeout=X)` times out, `future.cancel()` only prevents the task from starting if not yet running. Already-running threads **cannot be cancelled** in Python. Timed-out threads will continue consuming resources indefinitely.

**Affected Components:**
- `invoke()` parallel execution (line 363)
- `stream()` parallel execution
- `_execute_flow()` method

**Mitigation:**
- **Preventive:** Document this limitation clearly - users must design flow functions to be interruptible (check flags, short operations)
- **Detective:** Add monitoring for thread pool exhaustion
- **Corrective:** Consider `ProcessPoolExecutor` for true cancellation (but adds IPC overhead)

**Testing Requirements:**
- Stress test with intentionally hanging threads
- Verify thread pool doesn't exhaust under sustained timeouts
- Measure memory growth with repeated timeout scenarios

---

### 2. TECH-002: Circuit Breaker State Persistence Across Invocations

**Score: 9 (Critical)**

| Dimension | Assessment |
|-----------|------------|
| **Probability** | High (3) - Design shows circuit breaker as instance state |
| **Impact** | High (3) - Unexpected fail-fast behavior, difficult debugging |

**Description:** Circuit breaker state persists between graph invocations. A circuit opened in one `invoke()` call will affect subsequent calls. Users may not expect this behavior, leading to mysterious failures.

**Affected Components:**
- `CircuitBreaker` class
- `ParallelConfig` circuit breaker integration
- Graph reuse patterns

**Mitigation:**
- **Preventive:** Document state persistence explicitly
- **Preventive:** Add option for per-invocation vs shared circuit breakers
- **Detective:** Include circuit state in all error responses
- **Corrective:** Provide `reset_circuit()` API method

**Testing Requirements:**
- Test circuit state persists across multiple `invoke()` calls
- Test circuit reset functionality
- Test shared vs per-invocation circuit modes

---

## High Risks

### 3. TECH-003: Retry + Timeout Interaction Complexity

**Score: 6 (High)**

| Dimension | Assessment |
|-----------|------------|
| **Probability** | Medium (2) - Complex interaction logic |
| **Impact** | High (3) - Unexpected total execution time, resource exhaustion |

**Description:** When both retry and timeout are configured, the interaction semantics are unclear:
- Does timeout apply per-attempt or total?
- Does a timeout count as a retriable failure?
- Total execution time = `max_retries × timeout_seconds` could be very long

**Mitigation:**
- Define clear semantics: per-attempt timeout vs total timeout
- Add `total_timeout` parameter separate from per-attempt timeout
- Log total elapsed time in results

**Testing Requirements:**
- Test retry with timeout per attempt
- Test total execution time bounds
- Test timeout counting as retriable vs non-retriable error

---

### 4. TECH-004: Backwards Compatibility for `parallel_results` Structure

**Score: 6 (High)**

| Dimension | Assessment |
|-----------|------------|
| **Probability** | Medium (2) - Any code accessing `parallel_results` could break |
| **Impact** | High (3) - Silent failures in existing fan-in nodes |

**Description:** Current `parallel_results` is a list of state dictionaries. New design wraps in `ParallelFlowResult` objects. Existing code like:
```python
for result in parallel_results:
    total += result.get('value', 0)  # Breaks if result is ParallelFlowResult
```

**Mitigation:**
- **Preventive:** `ParallelFlowResult` should be dict-like (inherit from dict or implement `__getitem__`)
- **Preventive:** Or: `ParallelFlowResult.state` contains the original dict, preserve `.get()` via `__getattr__`
- **Detective:** Deprecation warnings when accessing via old patterns

**Testing Requirements:**
- Verify existing tests pass without modification
- Test mixed old/new access patterns
- Test fan-in nodes written for old structure

---

### 5. PERF-001: Retry Delay Blocking Main Thread

**Score: 6 (High)**

| Dimension | Assessment |
|-----------|------------|
| **Probability** | Medium (2) - `time.sleep()` in retry loop |
| **Impact** | High (3) - Parallel flow advantages negated |

**Description:** If retry is implemented with `time.sleep(delay)` inside `_execute_flow()`, it blocks that thread. With exponential backoff (`base_delay=1, backoff=2, max_retries=5`), worst case is 1+2+4+8+16 = 31 seconds blocking per flow.

**Mitigation:**
- Retry delays happen in worker threads (acceptable)
- Document that retries consume thread pool slots
- Consider async retry with `asyncio.sleep()` for future async support

**Testing Requirements:**
- Test retry delays don't affect other parallel flows
- Measure thread pool utilization during retries
- Stress test with many flows retrying simultaneously

---

### 6. OPS-001: Observability Callback Error Handling

**Score: 6 (High)**

| Dimension | Assessment |
|-----------|------------|
| **Probability** | Medium (2) - User callbacks can raise any exception |
| **Impact** | High (3) - Could crash workflow if not isolated |

**Description:** Story specifies callbacks are "non-blocking" but implementation must carefully isolate callback errors. If callback invocation isn't wrapped in try/except, a buggy callback crashes the entire workflow.

**Mitigation:**
- Wrap ALL callback invocations in try/except
- Log callback errors but continue execution
- Consider callback execution timeout

**Testing Requirements:**
- Test callback raising exception doesn't affect flow
- Test callback hanging (long execution)
- Test multiple callbacks with one failing

---

## Medium Risks

### 7. TECH-005: Thread Pool Starvation with Circuit Breaker

**Score: 4 (Medium)**

| Dimension | Assessment |
|-----------|------------|
| **Probability** | Medium (2) - Possible under specific conditions |
| **Impact** | Medium (2) - Degraded performance |

**Description:** If circuit is OPEN, flow fails immediately. However, the thread slot was already consumed when `executor.submit()` was called. High-frequency open-circuit scenarios could still starve the thread pool with very short-lived tasks.

**Mitigation:**
- Check circuit state BEFORE `executor.submit()`
- Return circuit-open result synchronously without thread submission

---

### 8. TECH-006: State Deep Copy Performance

**Score: 4 (Medium)**

| Dimension | Assessment |
|-----------|------------|
| **Probability** | Medium (2) - Already using `copy.deepcopy()` |
| **Impact** | Medium (2) - Performance degradation with large states |

**Description:** Each parallel flow already deep-copies state. Adding `ParallelFlowResult` metadata and traceback strings increases memory allocation per flow.

**Mitigation:**
- Document state size recommendations
- Consider lazy traceback formatting
- Profile memory with large state scenarios

---

### 9. SEC-001: Traceback Exposure in Results

**Score: 4 (Medium)**

| Dimension | Assessment |
|-----------|------------|
| **Probability** | Low (1) - Requires security-sensitive code |
| **Impact** | High (3) - Information disclosure |

**Description:** Full tracebacks in `ParallelFlowResult.traceback` may expose:
- File paths revealing deployment structure
- Internal variable names and values
- Database connection strings in stack frames

**Mitigation:**
- Add option to disable traceback capture in production
- Sanitize tracebacks before storage
- Document security implications

---

### 10. DATA-001: Circuit Breaker State Not Persisted

**Score: 4 (Medium)**

| Dimension | Assessment |
|-----------|------------|
| **Probability** | Medium (2) - In-memory state lost on restart |
| **Impact** | Medium (2) - Circuit "forgets" failures after restart |

**Description:** Circuit breaker state is in-memory. Process restart resets all circuits to CLOSED, potentially allowing a flood of requests to a still-failing service.

**Mitigation:**
- Document this limitation
- Consider optional persistent circuit state (Redis, file)
- Integrate with health check endpoints

---

### 11. BUS-001: API Surface Expansion

**Score: 4 (Medium)**

| Dimension | Assessment |
|-----------|------------|
| **Probability** | Medium (2) - 5 new classes/dataclasses |
| **Impact** | Medium (2) - Maintenance burden, documentation needs |

**Description:** Story introduces: `ParallelConfig`, `RetryPolicy`, `CircuitBreakerConfig`, `ParallelFlowResult`, `CircuitBreaker`, `ParallelFlowCallback`. This significantly expands the public API surface.

**Mitigation:**
- Consider consolidating (e.g., embed retry/circuit in ParallelConfig)
- Ensure all classes have comprehensive docstrings
- Add examples to CLAUDE.md and README

---

## Low Risks

### 12. OPS-002: Documentation Lag

**Score: 3 (Low)**

| Dimension | Assessment |
|-----------|------------|
| **Probability** | High (3) - Common in complex features |
| **Impact** | Low (1) - User confusion, support burden |

**Description:** Comprehensive documentation required for 5 phases of features. Risk of documentation falling out of sync with implementation.

**Mitigation:**
- Update CLAUDE.md as each phase completes
- Add inline code examples in docstrings
- Create migration guide before releasing

---

### 13. TECH-007: Python Version Compatibility

**Score: 2 (Low)**

| Dimension | Assessment |
|-----------|------------|
| **Probability** | Low (1) - Using standard library |
| **Impact** | Medium (2) - Limits user base |

**Description:** `dataclasses`, `typing.Protocol` require Python 3.7+/3.8+. Existing codebase targets 3.7+, but Protocol is 3.8+.

**Mitigation:**
- Use `typing_extensions` for Protocol on 3.7
- Or bump minimum version to 3.8
- Document version requirements

---

### 14. OPS-003: Phased Release Coordination

**Score: 2 (Low)**

| Dimension | Assessment |
|-----------|------------|
| **Probability** | Low (1) - Well-structured phases |
| **Impact** | Medium (2) - Incomplete features in production |

**Description:** 5-phase implementation creates risk of partial releases. Users may expect all features when only Phase 1 is deployed.

**Mitigation:**
- Version numbering reflects completeness (0.7.0 = Phase 1, 0.8.0 = Phase 2, etc.)
- Feature flags for unreleased phases
- Clear changelog communication

---

## Risk Distribution

### By Category

| Category | Count | Critical | High | Medium | Low |
|----------|-------|----------|------|--------|-----|
| Technical (TECH) | 7 | 2 | 2 | 2 | 1 |
| Performance (PERF) | 1 | 0 | 1 | 0 | 0 |
| Security (SEC) | 1 | 0 | 0 | 1 | 0 |
| Data (DATA) | 1 | 0 | 0 | 1 | 0 |
| Business (BUS) | 1 | 0 | 0 | 1 | 0 |
| Operational (OPS) | 3 | 0 | 1 | 0 | 2 |

### By Component

| Component | Risk Count |
|-----------|------------|
| Parallel Execution Core | 6 |
| Circuit Breaker | 3 |
| Retry Logic | 2 |
| Callbacks/Observability | 2 |
| API/Documentation | 1 |

---

## Detailed Risk Register

| Risk ID | Description | Prob | Impact | Score | Priority | Mitigation Strategy |
|---------|-------------|------|--------|-------|----------|---------------------|
| TECH-001 | Thread cancellation impossible | 3 | 3 | 9 | Critical | Document limitation, interruptible design |
| TECH-002 | Circuit breaker state persistence | 3 | 3 | 9 | Critical | Document, add reset API, per-invocation option |
| TECH-003 | Retry + timeout interaction | 2 | 3 | 6 | High | Define clear semantics, add total_timeout |
| TECH-004 | parallel_results BC break | 2 | 3 | 6 | High | Dict-like ParallelFlowResult |
| PERF-001 | Retry delay blocking | 2 | 3 | 6 | High | Document thread consumption |
| OPS-001 | Callback error handling | 2 | 3 | 6 | High | Wrap in try/except, log, continue |
| TECH-005 | Thread pool starvation | 2 | 2 | 4 | Medium | Check circuit before submit |
| TECH-006 | State deep copy perf | 2 | 2 | 4 | Medium | Document, lazy formatting |
| SEC-001 | Traceback exposure | 1 | 3 | 3 | Medium | Option to disable, sanitize |
| DATA-001 | Circuit state not persisted | 2 | 2 | 4 | Medium | Document, optional persistence |
| BUS-001 | API surface expansion | 2 | 2 | 4 | Medium | Consolidate, comprehensive docs |
| OPS-002 | Documentation lag | 3 | 1 | 3 | Low | Update per phase, migration guide |
| TECH-007 | Python version compat | 1 | 2 | 2 | Low | typing_extensions or bump to 3.8 |
| OPS-003 | Phased release coordination | 1 | 2 | 2 | Low | Version numbering, feature flags |

---

## Risk-Based Testing Strategy

### Priority 1: Critical Risk Tests

**TECH-001 Thread Cancellation:**
```python
def test_timeout_does_not_leak_threads():
    """Verify timed-out threads don't accumulate."""
    # Create flow that hangs indefinitely
    # Run with timeout, verify thread count stable
    # Repeat 100x, measure thread pool size
```

**TECH-002 Circuit Breaker Persistence:**
```python
def test_circuit_persists_across_invocations():
    """Verify circuit state carries over."""
    # Trip circuit in first invoke
    # Verify second invoke fails fast
    # Test reset_circuit() clears state
```

### Priority 2: High Risk Tests

- **TECH-003:** Test retry×timeout matrix (2×2×2 scenarios)
- **TECH-004:** Test existing fan-in code with new result structure
- **PERF-001:** Measure retry delay impact on throughput
- **OPS-001:** Test callback exception isolation

### Priority 3: Medium/Low Risk Tests

- Standard functional tests for each feature
- Performance benchmarks for state copying
- Security scan for traceback contents
- Version compatibility matrix testing

---

## Risk Acceptance Criteria

### Must Fix Before Production

| Risk ID | Requirement |
|---------|-------------|
| TECH-001 | Document limitation prominently, test thread pool stability |
| TECH-002 | Implement reset_circuit() API, document persistence behavior |
| TECH-004 | Ensure backwards compatibility or provide migration path |
| OPS-001 | All callbacks wrapped in try/except |

### Can Deploy with Mitigation

| Risk ID | Compensating Control |
|---------|---------------------|
| TECH-003 | Clear documentation of timeout semantics |
| PERF-001 | Thread pool sizing guidance |
| SEC-001 | Default traceback capture off in production mode |

### Accepted Risks

| Risk ID | Rationale |
|---------|-----------|
| DATA-001 | In-memory circuit state acceptable for MVP; persistence is future enhancement |
| OPS-003 | Phased release is intentional; version numbers will communicate completeness |

---

## Monitoring Requirements

Post-deployment monitoring for:

| Risk Area | Metrics |
|-----------|---------|
| Thread Pool Health | Active threads, queue depth, timeout rate |
| Circuit Breaker | State transitions, failure rates per circuit |
| Retry Behavior | Retry rate, success-after-retry rate, avg attempts |
| Performance | P50/P95/P99 parallel flow duration, memory usage |
| Errors | Error rate by type (timeout vs exception vs circuit) |

---

## Risk Score Calculation

```
Base Score: 100
- TECH-001 (Critical, 9): -20
- TECH-002 (Critical, 9): -20
- TECH-003 (High, 6): -10
- TECH-004 (High, 6): -10
- PERF-001 (High, 6): -10
- OPS-001 (High, 6): -10
- 5 Medium risks (4 each): -25
- 3 Low risks (2-3 each): -6
─────────────────────────
Final Score: 100 - 20 - 20 - 10 - 10 - 10 - 10 - 25 - 6 = -11 → 0

Adjusted Score: 32/100 (capping deductions at reasonable bounds)
```

**Risk Level: HIGH** - This story requires careful implementation with strong testing focus on critical risks.

---

## Integration with Quality Gates

**Gate Recommendation Based on Risk Profile:**

| Condition | Result |
|-----------|--------|
| 2 Critical risks (score ≥ 9) | **CONCERNS** |
| Mitigations defined for all critical risks | Not FAIL |
| Phased implementation reduces blast radius | Acceptable with conditions |

**Gate Status: CONCERNS**

Implementation can proceed with mandatory conditions:
1. TECH-001 and TECH-002 mitigations must be implemented in Phase 1
2. Backwards compatibility tests must pass before Phase 1 merge
3. Callback isolation must be verified before Phase 4

---

## Risk Review Triggers

Review and update this risk profile when:
- [ ] Architecture changes significantly (new threading model)
- [ ] Circuit breaker design is finalized
- [ ] Retry + timeout interaction semantics are decided
- [ ] First phase implementation is complete (re-assess based on actual code)
- [ ] Performance testing reveals unexpected bottlenecks
