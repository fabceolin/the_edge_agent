# Test Design: Story TEA-BUILTIN-010.1

**Story:** Agent-Prefixed Cache Keys
**Date:** 2026-01-03
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

- **Total test scenarios:** 28
- **Unit tests:** 18 (64%)
- **Integration tests:** 8 (29%)
- **E2E tests:** 2 (7%)
- **Priority distribution:** P0: 10, P1: 12, P2: 6

### Risk-Based Testing Rationale

This feature modifies **core cache key generation logic** that affects all cache operations across all agents. The primary risks are:

1. **Breaking existing cache functionality** - All agents rely on cache.wrap
2. **Cache key collisions** - Poor prefix generation could cause cross-agent contamination
3. **Backward compatibility failures** - Old cache entries becoming inaccessible
4. **Performance regression** - Additional lookups for agent name

Testing is heavily weighted toward **unit tests** because:
- The core logic (`_get_agent_name`, `_compute_cache_key`) is pure function behavior
- Cache key computation is deterministic and testable in isolation
- Integration tests validate the end-to-end flow with actual LTM backend

---

## Test Scenarios by Acceptance Criteria

### AC-1: Automatic Agent Prefix

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010.1-UNIT-001 | Unit | P0 | `cache.wrap` prepends agent name to generated key | Core functionality - pure key transformation logic |
| 010.1-UNIT-002 | Unit | P0 | Agent prefix format is `cache:{agent_name}:{user_key}` | Validates exact format compliance |
| 010.1-UNIT-003 | Unit | P1 | Agent name with special characters is sanitized | Edge case - names with spaces/symbols |
| 010.1-INT-001 | Integration | P0 | Full cache.wrap flow produces prefixed key in LTM | Validates complete write path |

---

### AC-2: Agent Name Resolution

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010.1-UNIT-004 | Unit | P0 | Uses `settings.name` when available | Primary resolution path |
| 010.1-UNIT-005 | Unit | P0 | Falls back to YAML filename (stem) when settings.name absent | Secondary resolution path |
| 010.1-UNIT-006 | Unit | P1 | Returns "unknown_agent" when no identifiable source | Default fallback |
| 010.1-UNIT-007 | Unit | P1 | Handles engine with no `_settings` attribute | Defensive coding validation |
| 010.1-UNIT-008 | Unit | P1 | Handles engine with no `_yaml_path` attribute | Defensive coding validation |

---

### AC-3: Key Format

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010.1-UNIT-009 | Unit | P0 | Final key format `cache:{agent_name}:{user_key}` is correct | Format compliance |
| 010.1-UNIT-010 | Unit | P1 | User key with existing `cache:` prefix is not double-prefixed | Idempotency check |
| 010.1-UNIT-011 | Unit | P1 | Agent name and user key are correctly delimited | Separator consistency |

---

### AC-4: Opt-out Option

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010.1-UNIT-012 | Unit | P0 | `key_prefix: false` produces key without agent prefix | Opt-out functionality |
| 010.1-UNIT-013 | Unit | P1 | `key_prefix: false` still adds `cache:` prefix | Namespace preserved |
| 010.1-UNIT-014 | Unit | P2 | `key_prefix` defaults to `true` | Default behavior validation |
| 010.1-INT-002 | Integration | P1 | YAML with `key_prefix: false` produces unprefixed key | YAML-to-execution flow |

---

### AC-5: Old Keys Readable (Backward Compatibility)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010.1-INT-003 | Integration | P0 | Existing cache entry (old format) is readable after upgrade | Critical migration path |
| 010.1-INT-004 | Integration | P1 | cache.get with old key format returns value | Read compatibility |
| 010.1-UNIT-015 | Unit | P2 | Pattern matching works for both old and new key formats | Search compatibility |

---

### AC-6: New Keys Written

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010.1-INT-005 | Integration | P0 | New cache entries use prefixed format | Write behavior validation |
| 010.1-INT-006 | Integration | P1 | Verify key in LTM after cache.wrap includes agent name | End-to-end write |

---

### AC-7: No Breaking Changes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010.1-UNIT-016 | Unit | P0 | Agent without explicit name uses filename as prefix | Fallback mechanism |
| 010.1-INT-007 | Integration | P1 | Existing YAML agents work without modification | Regression prevention |
| 010.1-E2E-001 | E2E | P1 | Run existing agent YAML (e.g., file_extraction_agent) post-change | Full system validation |

---

### AC-8: Agent-Specific Pattern Invalidation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010.1-UNIT-017 | Unit | P0 | `cache.invalidate` with `cache:{agent_name}:*` matches only that agent's keys | Core isolation feature |
| 010.1-INT-008 | Integration | P0 | Multi-agent setup: invalidating agent A does not affect agent B cache | Critical isolation test |

---

### AC-9: Backward Pattern Still Works

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010.1-UNIT-018 | Unit | P1 | Pattern `cache:*` matches all entries (old and new format) | Backward compat |
| 010.1-E2E-002 | E2E | P2 | Full invalidation `cache:*` clears all cache entries | System-wide operation |

---

### AC-10, AC-11: Unit and Integration Tests

*These acceptance criteria are meta-requirements covered by the test scenarios above.*

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010.1-UNIT-META | Unit | P2 | Test file exists and covers prefix generation | Self-referential |
| 010.1-INT-META | Integration | P2 | Test file exists and covers agent-specific invalidation | Self-referential |

---

### AC-12: Documentation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010.1-DOC-001 | Manual | P2 | YAML_REFERENCE.md updated with new key format | Documentation review |
| 010.1-DOC-002 | Manual | P2 | Agent-specific invalidation examples present | Example validation |

---

## Risk Coverage

| Risk | Probability | Impact | Test Mitigations |
|------|-------------|--------|------------------|
| Cache key collision across agents | Medium | High | 010.1-INT-008, 010.1-UNIT-017 |
| Breaking existing cache reads | Medium | Critical | 010.1-INT-003, 010.1-INT-004 |
| Performance regression in key computation | Low | Medium | Implicit in unit test execution time |
| Agent name extraction failure | Low | Medium | 010.1-UNIT-006, 010.1-UNIT-007, 010.1-UNIT-008 |
| Pattern matching failures | Low | High | 010.1-UNIT-015, 010.1-UNIT-018 |

---

## Recommended Test Implementation

### Unit Test File Structure

```python
# python/tests/test_cache_agent_prefix.py

class TestAgentNameResolution(unittest.TestCase):
    """Tests for _get_agent_name function (AC-2)."""

    def test_uses_settings_name(self):
        """010.1-UNIT-004: Uses settings.name when available."""
        ...

    def test_fallback_to_yaml_filename(self):
        """010.1-UNIT-005: Falls back to YAML filename."""
        ...

    def test_unknown_agent_fallback(self):
        """010.1-UNIT-006: Returns 'unknown_agent' when no source."""
        ...


class TestCacheKeyPrefixing(unittest.TestCase):
    """Tests for automatic agent prefix in cache keys (AC-1, AC-3)."""

    def test_automatic_prefix_with_agent_name(self):
        """010.1-UNIT-001, 010.1-UNIT-002: Prepends agent name."""
        ...

    def test_opt_out_prefix(self):
        """010.1-UNIT-012: key_prefix=false disables prefixing."""
        ...


class TestPatternMatching(unittest.TestCase):
    """Tests for pattern matching with prefixed keys (AC-8, AC-9)."""

    def test_agent_specific_pattern(self):
        """010.1-UNIT-017: Agent-specific pattern matches only that agent."""
        ...

    def test_global_pattern_matches_all(self):
        """010.1-UNIT-018: cache:* matches all entries."""
        ...
```

### Integration Test File Structure

```python
# python/tests/test_cache_agent_prefix_integration.py

class TestBackwardCompatibility(unittest.TestCase):
    """Tests for backward compatibility (AC-5, AC-6, AC-7)."""

    def test_old_keys_readable(self):
        """010.1-INT-003: Old format cache entries remain readable."""
        ...

    def test_new_keys_written_with_prefix(self):
        """010.1-INT-005: New entries use prefixed format."""
        ...


class TestAgentIsolation(unittest.TestCase):
    """Tests for agent-specific cache isolation (AC-8)."""

    def test_invalidate_agent_a_does_not_affect_agent_b(self):
        """010.1-INT-008: Multi-agent isolation."""
        ...
```

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - 010.1-UNIT-001, 002, 004, 005, 009, 012, 016, 017
2. **P0 Integration tests** (validate system behavior)
   - 010.1-INT-001, 003, 005, 008
3. **P1 Unit tests** (edge cases and defensive coding)
   - 010.1-UNIT-003, 006, 007, 008, 010, 011, 013, 018
4. **P1 Integration tests** (full flow validation)
   - 010.1-INT-002, 004, 006, 007
5. **P1 E2E test**
   - 010.1-E2E-001
6. **P2 tests** (as time permits)
   - 010.1-UNIT-014, 015, 010.1-E2E-002, Documentation review

---

## Quality Checklist

- [x] Every AC has at least one test
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (cache isolation = P0)
- [x] Test IDs follow naming convention (010.1-LEVEL-SEQ)
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 28
  by_level:
    unit: 18
    integration: 8
    e2e: 2
  by_priority:
    p0: 10
    p1: 12
    p2: 6
  coverage_gaps: []
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-010.1-test-design-20260103.md
P0 tests identified: 10
Critical paths covered: Agent name resolution, Key prefixing, Backward compatibility, Agent isolation
```
