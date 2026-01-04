# Story TEA-BUILTIN-010.1: Agent-Prefixed Cache Keys

## Status

**Done**

## Story

**As a** YAML agent developer,
**I want** cache keys to automatically include the agent name as a prefix,
**So that** I can invalidate cache entries for a specific agent without affecting other agents sharing the same LTM backend.

## Context

### Problem Statement

Currently, all agents using `cache.wrap` share the same LTM storage and use generic cache key patterns:

```yaml
# Current format (file_extraction_agent.yaml)
key: "classify:file:{{ sha256 }}"
key: "extract:{{ file_sha256 }}:{{ agent_name }}"
```

When running `cache.invalidate` with pattern `cache:*`, this purges cache from **ALL** agents. This creates:

1. Risk of accidentally invalidating cache for unrelated agents
2. No way to selectively clear cache for a single agent
3. Increased API costs when cache is unnecessarily cleared

### Proposed Solution

Automatically prefix cache keys with agent name from `settings.name` or YAML file:

```yaml
# Proposed automatic format
cache:{agent_name}:{user_key}

# Examples:
cache:file_extraction_agent:classify:file:abc123
cache:file_extraction_agent:extract:def456:rankellix-chambers-partners-fast
```

This enables targeted invalidation:

```yaml
# Invalidate only file_extraction_agent cache
- name: clear_agent_cache
  uses: cache.invalidate
  with:
    pattern: "cache:file_extraction_agent:*"
```

## Acceptance Criteria

### Core Functionality

1. **AC-1: Automatic Agent Prefix**: `cache.wrap` automatically prepends agent name to cache keys
2. **AC-2: Agent Name Resolution**: Uses `settings.name` from YAML, or filename without extension as fallback
3. **AC-3: Key Format**: Final key format is `cache:{agent_name}:{user_key}`
4. **AC-4: Opt-out Option**: Support `key_prefix: false` to disable automatic prefixing

### Backward Compatibility

5. **AC-5: Old Keys Readable**: Existing cache entries (without prefix) remain readable until TTL expiry
6. **AC-6: New Keys Written**: New entries immediately use prefixed format
7. **AC-7: No Breaking Changes**: Agents not specifying name continue to work (use filename as prefix)

### Cache Invalidation

8. **AC-8: Agent-Specific Pattern**: `cache.invalidate` with pattern `cache:{agent_name}:*` only affects that agent
9. **AC-9: Backward Pattern**: Pattern `cache:*` still matches all entries (old and new format)

### Quality

10. **AC-10: Unit Tests**: Tests for prefix generation, key format, opt-out
11. **AC-11: Integration Tests**: Tests for agent-specific invalidation
12. **AC-12: Documentation**: Updated YAML_REFERENCE.md with new key format

## Tasks / Subtasks

- [x] Task 1: Implement automatic agent prefix in cache.wrap (AC: 1, 2, 3)
  - [x] Extract agent name from engine settings or YAML filename
  - [x] Modify `_compute_cache_key()` to include agent prefix
  - [x] Update key format to `cache:{agent_name}:{user_key}`

- [x] Task 2: Add opt-out option (AC: 4)
  - [x] Add `key_prefix` parameter to cache.wrap (default: true)
  - [x] When `key_prefix: false`, use existing behavior

- [x] Task 3: Ensure backward compatibility (AC: 5, 6, 7)
  - [x] Test reading old keys still works
  - [x] Test new keys are written with prefix
  - [x] Test agents without explicit name use filename

- [x] Task 4: Update cache.invalidate pattern matching (AC: 8, 9)
  - [x] Verify agent-specific patterns work
  - [x] Verify `cache:*` still matches all

- [x] Task 5: Add tests (AC: 10, 11)
  - [x] Unit tests for prefix generation
  - [x] Integration tests for agent-specific invalidation

- [x] Task 6: Update documentation (AC: 12)
  - [x] Update YAML_REFERENCE.md Cache section
  - [x] Add examples of agent-specific invalidation

## Dev Notes

### Implementation in cache_actions.py

```python
def _get_agent_name(engine: Any) -> str:
    """Get agent name for cache key prefix."""
    # Try settings.name first
    if hasattr(engine, '_settings') and engine._settings:
        name = engine._settings.get('name')
        if name:
            return name

    # Fallback to YAML filename
    if hasattr(engine, '_yaml_path') and engine._yaml_path:
        from pathlib import Path
        return Path(engine._yaml_path).stem

    # Default fallback
    return "unknown_agent"


def _compute_cache_key(
    engine: Any,
    action: str,
    args: dict,
    key: Optional[str],
    key_strategy: str,
    key_source: Optional[str],
    key_prefix: bool = True,  # NEW parameter
) -> str:
    """Compute cache key with optional agent prefix."""

    # Compute base key (existing logic)
    if key:
        base_key = key
    elif key_strategy == "args":
        args_json = json.dumps(args, sort_keys=True)
        base_key = f"{action}:{hashlib.sha256(args_json.encode()).hexdigest()}"
    # ... etc

    # Apply agent prefix if enabled
    if key_prefix:
        agent_name = _get_agent_name(engine)
        return f"cache:{agent_name}:{base_key}"
    else:
        return f"cache:{base_key}"
```

### YAML Usage Examples

```yaml
# Agent name from settings (recommended)
name: file_extraction_agent
settings:
  ltm:
    backend: duckdb

nodes:
  - name: extract_cached
    uses: cache.wrap
    with:
      action: llamaextract.extract
      key: "extract:{{ state.file_hash }}"
      # Final key: cache:file_extraction_agent:extract:abc123
```

```yaml
# Opt-out of prefix
  - name: shared_cache
    uses: cache.wrap
    with:
      action: llm.call
      key: "shared:translate:{{ state.text | sha256 }}"
      key_prefix: false
      # Final key: cache:shared:translate:xyz789
```

### Migration Strategy

1. **No migration needed** - old keys continue to work
2. **Natural turnover** - as TTL expires, new prefixed keys replace old
3. **Full transition** - after 60 days (max TTL), all keys will be prefixed

## Risk Assessment

### Primary Risk
Cache miss storm during transition if old keys are invalidated

### Mitigation
- Old keys remain readable
- Natural TTL expiry handles transition
- No forced migration

### Rollback
Revert changes; set `key_prefix: false` in agents to restore old behavior

## Definition of Done

- [x] All acceptance criteria verified
- [x] Backward compatibility tested
- [x] No regression in cache hit rates
- [x] Documentation updated
- [x] Tests pass (75 tests, all passing)

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
N/A - No blocking issues encountered

### Completion Notes
- Implemented `_get_agent_name()` helper function with proper fallback chain
- Modified `_compute_cache_key()` to accept `engine` and `key_prefix` parameters
- Added `key_prefix` parameter to `cache_wrap()` (default: True)
- Added 22 new tests across 6 test classes covering all acceptance criteria
- Updated `docs/shared/yaml-reference/actions/memory.md` with comprehensive documentation

### File List

| File | Status | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/actions/cache_actions.py` | Modified | Added `_get_agent_name()`, updated `_compute_cache_key()` with agent prefix |
| `python/tests/test_cache_actions.py` | Modified | Added 22 new tests for TEA-BUILTIN-010.1 |
| `docs/shared/yaml-reference/actions/memory.md` | Modified | Updated cache documentation with agent prefix feature |
| `docs/stories/TEA-BUILTIN-010.1-agent-prefixed-cache-keys.md` | Modified | Updated status and completion notes |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-03 | 1.0 | Initial story creation | James (Dev Agent) |
| 2026-01-03 | 1.1 | Status: Draft → Ready for Dev (QA test design complete) | Bob (Scrum Master) |
| 2026-01-03 | 1.2 | Implementation complete, all tests passing | James (Dev Agent) |
| 2026-01-03 | 1.3 | QA Review: PASS (95/100) | Quinn (Test Architect) |
| 2026-01-03 | 1.4 | Status: Ready for Review → Done | Bob (Scrum Master) |

## QA Results

### Review Date: 2026-01-03

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: Excellent**

The implementation is clean, well-structured, and follows established patterns in the codebase. Key observations:

1. **`_get_agent_name()` function**: Well-designed with proper fallback chain (settings.name → config.name → workflow_name → "unknown_agent"). Handles edge cases like None values and empty strings correctly.

2. **`_compute_cache_key()` modification**: Minimal, focused changes that add the new functionality without disrupting existing behavior. The `engine` and `key_prefix` parameters are properly optional with sensible defaults.

3. **Test coverage**: 22 new tests across 6 test classes provide comprehensive coverage of:
   - Agent name resolution priority (7 tests)
   - Cache key format validation (6 tests)
   - Integration with cache.wrap (3 tests)
   - Agent-specific invalidation (3 tests)
   - Backward compatibility (3 tests)

### Refactoring Performed

None required - the implementation is clean and well-structured.

### Compliance Check

- Coding Standards: ✓ Follows project conventions for docstrings, type hints, and function organization
- Project Structure: ✓ All changes in appropriate files (`cache_actions.py`, `test_cache_actions.py`, `memory.md`)
- Testing Strategy: ✓ Unit tests for pure functions, integration tests for LTM interactions
- All ACs Met: ✓ All 12 acceptance criteria implemented and tested

### Improvements Checklist

All items completed by developer:

- [x] Implemented `_get_agent_name()` with proper fallback chain
- [x] Modified `_compute_cache_key()` with agent prefix support
- [x] Added `key_prefix` parameter with `True` default
- [x] Added comprehensive unit tests for agent name resolution
- [x] Added integration tests for cache.wrap with prefixes
- [x] Added integration tests for agent-specific invalidation
- [x] Updated documentation in `memory.md`
- [x] Verified backward compatibility with old key formats

### Security Review

**PASS** - No security concerns identified.

- Agent names are used as-is from configuration (no user input)
- No injection vectors in cache key generation
- Pattern matching uses existing safe implementation

### Performance Considerations

**PASS** - Negligible performance impact.

- Single attribute lookup and string concatenation per cache operation
- No additional I/O or computation
- Existing tests execute in ~5 seconds (no regression)

### Files Modified During Review

None - implementation meets all quality standards.

### Gate Status

**Gate: PASS** → `docs/qa/gates/TEA-BUILTIN-010.1-agent-prefixed-cache-keys.yml`

**Quality Score: 95/100**

### Recommended Status

✓ **Ready for Done** - All acceptance criteria verified, comprehensive test coverage, documentation complete.
