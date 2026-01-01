# TEA-BUILTIN-013: Distributed Rate Limiting (Deferred)

## Status: Deferred

| Field | Value |
|-------|-------|
| **ID** | TEA-BUILTIN-013 |
| **Type** | Story |
| **Priority** | Low |
| **Status** | Deferred |
| **Deferred Date** | 2026-01-01 |
| **Deferred By** | Sarah (PO) |

---

## Summary

Provide coordinated rate limiting across multiple processes or remote hosts when using `parallel_strategy: process` or `parallel_strategy: remote`.

## Problem Statement

As documented in [TEA-PARALLEL-001 Feature Interaction Limitations](./TEA-PARALLEL-001-multi-strategy-execution-epic.md#rate-limiting-behavior):

- `ratelimit.wrap` with `thread` strategy: Limiters are shared (coordinated)
- `ratelimit.wrap` with `process` strategy: Each process gets independent limiter
- `ratelimit.wrap` with `remote` strategy: Each host gets independent limiter

This means when using 3 parallel branches with `rpm: 60`, actual rate could be up to `180 RPM` (60 × 3), potentially exceeding API provider limits.

## Current Mitigation (Without This Feature)

Users must manually divide rate limits by parallelism factor:

```yaml
settings:
  rate_limiters:
    openai:
      rpm: 20  # 60 RPM ÷ 3 parallel branches = 20 per branch
```

## Proposed Solution (When Prioritized)

Implement a distributed rate limiter using an external coordination backend:

| Backend Option | Pros | Cons |
|----------------|------|------|
| Redis | Fast, widely deployed | External dependency |
| File-based (NFS) | No new infra | Slow, requires shared FS |
| SQLite (shared) | Simple | Limited to single machine |
| DuckDB (via LTM) | Already integrated | May add latency |

## Why Deferred

1. **Complexity vs. Benefit**: Manual rate division is a viable workaround
2. **Dependency Concern**: Adding Redis as a dependency increases operational burden
3. **Usage Pattern**: Most TEA users use `thread` strategy (default)
4. **Priority**: TEA-PARALLEL-001 and TEA-BUILTIN-012 have higher immediate value

## Conditions to Revisit

This story should be prioritized when:

- [ ] Multiple users report API rate limit violations with `process`/`remote` strategies
- [ ] TEA gains enterprise adoption requiring distributed execution
- [ ] A lightweight coordination mechanism becomes available (e.g., built-in to LTM)

## References

- [TEA-PARALLEL-001](./TEA-PARALLEL-001-multi-strategy-execution-epic.md) - Multi-Strategy Parallel Execution
- [TEA-BUILTIN-011](./TEA-BUILTIN-011-ratelimit-wrap-action.md) - Rate Limiting Actions

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-01 | 0.1 | Initial deferred story | Sarah (PO) |
