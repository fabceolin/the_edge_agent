# Story TEA-BUILTIN-010: Cache and Memoization Actions

## Status

**Ready for Development** ✅

**Checklist Validation:** PASS (2024-12-24)
- Story Clarity Score: 9/10
- ACs: 23 acceptance criteria with clear PASS/FAIL conditions
- Test Design: 48 scenarios (18 P0, 20 P1, 10 P2)
- Dependencies: None (foundational story)
- Risk Assessment: Complete with mitigations

## Story

**As a** YAML agent developer,
**I want** cache and memoization actions that automatically cache action results in Long-Term Memory with configurable TTL and cache key strategies,
**so that** I can avoid redundant expensive operations (API calls, file processing, LLM inference) and improve agent performance without writing custom caching logic.

## Acceptance Criteria

### Core Cache Actions

1. **AC-1: cache.wrap action**: Wraps any action with automatic caching - checks cache before execution, stores result after
2. **AC-2: Cache Key Generation**: Supports multiple key strategies: `sha256` (content hash), `args` (action arguments hash), `custom` (Jinja expression)
3. **AC-3: Cache Lookup**: Before wrapped action executes, checks LTM for existing cached result using computed key
4. **AC-4: Cache Hit Return**: If cache hit with valid TTL, returns cached result with `_cache_hit: true` metadata
5. **AC-5: Cache Miss Execute**: If cache miss or expired, executes wrapped action normally
6. **AC-6: Cache Store**: After successful execution, stores result in LTM with expiration metadata
7. **AC-7: Configurable TTL**: Default TTL is 60 days, configurable via `ttl_days` or `ttl_seconds` parameter

### Cache Control

8. **AC-8: Skip Cache**: Supports `skip_cache: true` to bypass cache lookup and force fresh execution
9. **AC-9: Cache Disabled**: Supports `cache_enabled: false` to disable caching entirely (no lookup or store)
10. **AC-10: cache.invalidate action**: Explicitly invalidate/delete a cached entry by key or pattern
11. **AC-11: cache.get action**: Retrieve cached value without executing any action (for debugging/inspection)

### File Content Hashing

12. **AC-12: storage.hash action**: Computes SHA256 hash of file content from any URI (local/S3/GCS/Azure)
13. **AC-13: sha256 Jinja filter**: `{{ value | sha256 }}` filter for inline hash computation in templates

### Automatic Cleanup

14. **AC-14: Probabilistic Cleanup**: After cache miss, runs cleanup with configurable probability (default: 5%)
15. **AC-15: Cleanup Logic**: Searches LTM for entries with `_cache_expires_at < now()` and deletes expired entries
16. **AC-16: Cleanup Limit**: Cleanup deletes at most N entries per run (configurable, default: 5)

### Error Handling

17. **AC-17: LTM Failure Graceful**: If LTM lookup/store fails, proceeds with normal action execution (no failure)
18. **AC-18: Hash Failure Graceful**: If content hashing fails, proceeds without caching

### Observability

19. **AC-19: Cache Metadata**: Cached entries include metadata: `_cache_type`, `_cache_key`, `_cache_created_at`, `_cache_expires_at`, `_cache_action`
20. **AC-20: Response Indicators**: Wrapped action response includes `_cache_hit`, `_cache_key`, `_cache_created_at` (if hit)

### Integration

21. **AC-21: Uses LTM Backend**: All cache operations use the existing `ltm.*` actions (TEA-BUILTIN-001.4)
22. **AC-22: Dual Namespace**: Actions accessible as `cache.*` and `actions.cache_*`
23. **AC-23: Documentation**: Updated CLAUDE.md and YAML_REFERENCE.md with cache action examples

## Dependencies

**Blocked By**:
- TEA-BUILTIN-001.4 (Long-Term Memory) - provides `ltm.store`, `ltm.retrieve`, `ltm.delete`, `ltm.search`
- TEA-BUILTIN-004.1 (Remote Storage) - provides `fsspec` integration for file access

**Blocks**:
- Agent-level result caching
- Expensive API call optimization
- Document processing pipelines

**Internal Dependencies**:
- `cache.wrap` uses `ltm.store`, `ltm.retrieve`, `ltm.delete`
- `storage.hash` uses `fsspec` for file access

## User Prerequisites

- [ ] **Required**: LTM backend configured (one of the following):
  - `sqlite` - Local SQLite (default, no setup needed)
  - `blob-sqlite` - SQLite on S3/GCS/Azure with fsspec (TEA-BUILTIN-001.5)
  - `duckdb` - DuckDB with native httpfs cloud I/O (TEA-BUILTIN-001.6)
  - `turso` - Turso/libSQL edge database (TEA-BUILTIN-001.5)
  - `firestore` - Firebase Firestore (TEA-BUILTIN-001.5)
- [ ] **Optional**: Cloud storage credentials for `storage.hash` with remote files

### Recommended Backend by Use Case

| Use Case | Backend | Why |
|----------|---------|-----|
| Local development | `sqlite` | Zero config, fast |
| Low-concurrency serverless | `blob-sqlite` | Simple, SQLite compatible |
| High-concurrency serverless | `duckdb` | Native httpfs, no locking |
| Firebase stack | `firestore` | Native integration |
| Edge computing | `turso` | Low latency, edge replicas |

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         CACHE ARCHITECTURE                                   │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  YAML Agent                           Cache Layer                            │
│  ──────────                           ───────────                            │
│                                                                              │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │  - name: extract_with_cache                                          │   │
│  │    uses: cache.wrap                                                  │   │
│  │    with:                                                             │   │
│  │      action: llamaextract.extract                                    │   │
│  │      key: "{{ state.file_path | storage_hash }}"                     │   │
│  │      ttl_days: 60                                                    │   │
│  │      args:                                                           │   │
│  │        file: "{{ state.file_path }}"                                 │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                            │                                                 │
│                            ▼                                                 │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                      cache.wrap                                      │   │
│  │                                                                      │   │
│  │   1. Compute cache key (sha256, args hash, or custom)               │   │
│  │   2. ltm.retrieve(key) → Check cache                                │   │
│  │   3. If hit & not expired → Return cached result                    │   │
│  │   4. If miss → Execute wrapped action                               │   │
│  │   5. ltm.store(key, result, metadata) → Cache result                │   │
│  │   6. Maybe cleanup (5% probability)                                 │   │
│  │   7. Return result with _cache_hit metadata                         │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                            │                                                 │
│                            ▼                                                 │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                   Long-Term Memory (LTM)                             │   │
│  │                                                                      │   │
│  │   Key: cache:{action}:{hash}                                        │   │
│  │   Value: { result: {...}, _cache_action: "llamaextract.extract" }   │   │
│  │   Metadata: { _cache_expires_at: "2025-02-22T...", ... }            │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Cache Key Strategies

| Strategy | Description | Use Case |
|----------|-------------|----------|
| `sha256` | SHA256 hash of specified content | File content deduplication |
| `args` | Hash of action arguments | Deterministic API calls |
| `custom` | Jinja expression evaluated at runtime | Complex key logic |
| `file_content` | Automatic file content hash from path arg | Document processing |

### Key Strategy Examples

```yaml
# Strategy 1: File content hash (for document extraction)
- name: extract_cached
  uses: cache.wrap
  with:
    action: llamaextract.extract
    key_strategy: file_content
    key_source: file                    # Argument name containing file path
    ttl_days: 60
    args:
      file: "{{ state.file_path }}"
      agent_name: "{{ state.agent_name }}"

# Strategy 2: Arguments hash (for deterministic API calls)
- name: translate_cached
  uses: cache.wrap
  with:
    action: llm.call
    key_strategy: args                  # Hash all args
    ttl_days: 30
    args:
      model: gpt-4o
      messages:
        - role: user
          content: "Translate to Spanish: {{ state.text }}"

# Strategy 3: Custom key expression
- name: search_cached
  uses: cache.wrap
  with:
    action: web.search
    key_strategy: custom
    key: "search:{{ state.query | lower | sha256 }}"
    ttl_hours: 24
    args:
      query: "{{ state.query }}"
```

## Action Specifications

### cache.wrap

```yaml
cache.wrap:
  description: |
    Wraps any action with automatic caching. Checks LTM before execution,
    stores result after successful execution.

  parameters:
    action:
      type: string
      required: true
      description: The action to wrap (e.g., "llamaextract.extract", "llm.call")

    args:
      type: dict
      required: true
      description: Arguments to pass to the wrapped action

    key:
      type: string
      required: false
      description: |
        Cache key or Jinja expression. If not provided, uses key_strategy.
        Example: "cache:extract:{{ state.file_hash }}"

    key_strategy:
      type: string
      required: false
      default: args
      enum: [sha256, args, custom, file_content]
      description: Strategy for generating cache key

    key_source:
      type: string
      required: false
      description: |
        For file_content strategy, the argument name containing file path.
        For sha256 strategy, the value to hash.

    ttl_days:
      type: integer
      required: false
      default: 60
      description: Cache TTL in days

    ttl_hours:
      type: integer
      required: false
      description: Cache TTL in hours (overrides ttl_days)

    ttl_seconds:
      type: integer
      required: false
      description: Cache TTL in seconds (overrides ttl_hours and ttl_days)

    skip_cache:
      type: boolean
      required: false
      default: false
      description: Bypass cache lookup, force fresh execution

    cache_enabled:
      type: boolean
      required: false
      default: true
      description: Enable/disable caching entirely

    cleanup_probability:
      type: float
      required: false
      default: 0.05
      description: Probability of running cleanup after cache miss (0.0 to 1.0)

    cleanup_limit:
      type: integer
      required: false
      default: 5
      description: Maximum expired entries to delete per cleanup run

  returns:
    success: boolean
    result: any              # Result from wrapped action
    _cache_hit: boolean      # Whether result came from cache
    _cache_key: string       # The cache key used
    _cache_created_at: string  # ISO timestamp (if cache hit)
    error: string            # Error message if failed
```

### cache.invalidate

```yaml
cache.invalidate:
  description: |
    Invalidate (delete) cached entries by exact key or pattern.

  parameters:
    key:
      type: string
      required: false
      description: Exact cache key to invalidate

    pattern:
      type: string
      required: false
      description: |
        Key pattern for bulk invalidation (uses LTM search).
        Example: "cache:extract:*"

    metadata_filter:
      type: dict
      required: false
      description: |
        Metadata filter for selective invalidation.
        Example: { "_cache_action": "llamaextract.extract" }

  returns:
    success: boolean
    deleted_count: integer
    deleted_keys: list[string]
```

### cache.get

```yaml
cache.get:
  description: |
    Retrieve cached value by key without executing any action.
    Useful for debugging and cache inspection.

  parameters:
    key:
      type: string
      required: true
      description: Cache key to retrieve

    include_metadata:
      type: boolean
      required: false
      default: false
      description: Include cache metadata in response

  returns:
    success: boolean
    found: boolean
    value: any
    metadata: dict           # If include_metadata=true
    expired: boolean         # Whether entry has expired
```

### storage.hash

```yaml
storage.hash:
  description: |
    Compute SHA256 hash of file content from any URI.
    Supports local files, S3, GCS, Azure via fsspec.

  parameters:
    path:
      type: string
      required: true
      description: File path or URI (file://, s3://, gs://, az://)

    algorithm:
      type: string
      required: false
      default: sha256
      enum: [sha256, md5, blake2b]
      description: Hash algorithm to use

  returns:
    success: boolean
    hash: string             # Hex-encoded hash
    algorithm: string
    size_bytes: integer      # File size
    path: string
    error: string            # If failed
```

## Jinja Filters

### sha256 filter

```yaml
# Usage in templates
key: "{{ state.content | sha256 }}"
key: "{{ state.file_path | sha256 }}"
key: "cache:{{ state.query | lower | sha256 }}"
```

### storage_hash filter (async-aware)

```yaml
# Note: This filter requires file I/O, so it's computed during
# template rendering with access to the action registry
key: "{{ state.file_path | storage_hash }}"
```

## Tasks / Subtasks

- [ ] **Task 1**: Implement cache.wrap action (AC: 1-7, 17, 20)
  - [ ] Create `cache_actions.py` in actions directory
  - [ ] Implement key generation (sha256, args, custom, file_content)
  - [ ] Implement cache lookup via `ltm.retrieve`
  - [ ] Implement wrapped action execution
  - [ ] Implement cache store via `ltm.store`
  - [ ] Handle LTM failures gracefully
  - [ ] Add `_cache_hit`, `_cache_key` to response

- [ ] **Task 2**: Implement cache control parameters (AC: 8, 9)
  - [ ] Add `skip_cache` parameter handling
  - [ ] Add `cache_enabled` parameter handling
  - [ ] Test bypass scenarios

- [ ] **Task 3**: Implement cache.invalidate action (AC: 10)
  - [ ] Implement exact key deletion
  - [ ] Implement pattern-based deletion via `ltm.search`
  - [ ] Implement metadata filter deletion

- [ ] **Task 4**: Implement cache.get action (AC: 11)
  - [ ] Implement key lookup
  - [ ] Check expiration status
  - [ ] Include metadata option

- [ ] **Task 5**: Implement storage.hash action (AC: 12, 18)
  - [ ] Use fsspec for file access
  - [ ] Implement SHA256/MD5/Blake2b algorithms
  - [ ] Handle file access errors gracefully
  - [ ] Support all URI schemes

- [ ] **Task 6**: Implement sha256 Jinja filter (AC: 13)
  - [ ] Add filter to Jinja environment
  - [ ] Handle bytes and string inputs
  - [ ] Document usage

- [ ] **Task 7**: Implement automatic cleanup (AC: 14, 15, 16)
  - [ ] Add probabilistic cleanup trigger
  - [ ] Search for expired entries
  - [ ] Delete up to N entries
  - [ ] Make probability and limit configurable

- [ ] **Task 8**: Add cache metadata (AC: 19)
  - [ ] Define standard metadata fields
  - [ ] Include action name in metadata
  - [ ] Store creation and expiration timestamps

- [ ] **Task 9**: Register actions with dual namespace (AC: 22)
  - [ ] Register `cache.*` namespace
  - [ ] Register `actions.cache_*` namespace
  - [ ] Register `storage.hash` action

- [ ] **Task 10**: Documentation (AC: 23)
  - [ ] Update CLAUDE.md with cache examples
  - [ ] Update YAML_REFERENCE.md with action specs
  - [ ] Add example YAML agents using cache

- [ ] **Task 11**: Testing
  - [ ] Unit tests for key generation strategies
  - [ ] Unit tests for cache hit/miss logic
  - [ ] Unit tests for expiration handling
  - [ ] Unit tests for cleanup logic
  - [ ] Integration tests with mocked LTM
  - [ ] Integration tests with real LTM backend
  - [ ] Test graceful degradation scenarios

## Dev Notes

### Implementation Approach

The `cache.wrap` action will:

1. **Compute cache key** based on strategy:
   ```python
   def compute_cache_key(action: str, args: dict, key: str, key_strategy: str, key_source: str) -> str:
       if key:
           # Custom key (already a Jinja expression result)
           return f"cache:{key}"

       if key_strategy == "args":
           # Hash all arguments
           args_json = json.dumps(args, sort_keys=True)
           return f"cache:{action}:{hashlib.sha256(args_json.encode()).hexdigest()}"

       if key_strategy == "file_content":
           # Load file and hash content
           file_path = args.get(key_source)
           content = fsspec.open(file_path, 'rb').read()
           return f"cache:{action}:{hashlib.sha256(content).hexdigest()}"

       # Default: args hash
       ...
   ```

2. **Check cache** via `ltm.retrieve`:
   ```python
   cache_result = ltm_retrieve(key=cache_key)
   if cache_result.get("found"):
       metadata = cache_result.get("metadata", {})
       expires_at = metadata.get("_cache_expires_at")
       if not is_expired(expires_at):
           return {
               "success": True,
               "result": cache_result["value"]["result"],
               "_cache_hit": True,
               "_cache_key": cache_key,
               "_cache_created_at": metadata.get("_cache_created_at")
           }
   ```

3. **Execute wrapped action** if cache miss:
   ```python
   result = registry[action](state=state, **args)
   ```

4. **Store in cache** if successful:
   ```python
   if result.get("success", True):
       ltm_store(
           key=cache_key,
           value={"result": result},
           metadata={
               "_cache_type": "action_result",
               "_cache_action": action,
               "_cache_key": cache_key,
               "_cache_created_at": now.isoformat() + "Z",
               "_cache_expires_at": (now + timedelta(days=ttl_days)).isoformat() + "Z"
           }
       )
   ```

5. **Maybe cleanup** with probability check:
   ```python
   if random.random() < cleanup_probability:
       expired = ltm_search(metadata_filter={"_cache_type": "action_result"}, limit=20)
       for entry in expired[:cleanup_limit]:
           if is_expired(entry["metadata"]["_cache_expires_at"]):
               ltm_delete(key=entry["key"])
   ```

### Thread Safety

- Cache operations are atomic via LTM backend
- Concurrent cache writes for same key are idempotent (last write wins)
- No explicit locking needed

### Performance Considerations

- **Key Computation**: SHA256 is fast (~10ms for 10MB file)
- **LTM Lookup**: SQLite query, typically <5ms
- **Cleanup Overhead**: 5% probability × ~50ms = negligible average impact
- **Net Savings**: Cache hit saves seconds to minutes on expensive operations

### Limitations

- **Not for streaming**: Cache stores complete results, not streaming responses
- **Memory bound**: Large results stored in SQLite (consider for multi-GB responses)
- **No distributed cache**: LTM is single-node; use Redis for distributed caching

### Cache Key Prefix Convention

All cache keys use `cache:` prefix for easy identification:
- `cache:llamaextract.extract:abc123...` (action + hash)
- `cache:custom:my-custom-key` (custom key)

### Testing Strategy

```python
class TestCacheWrap(unittest.TestCase):
    def test_cache_miss_executes_action(self):
        """First call should execute wrapped action."""
        ...

    def test_cache_hit_returns_cached(self):
        """Second call should return cached result."""
        ...

    def test_expired_cache_is_miss(self):
        """Expired entries should trigger fresh execution."""
        ...

    def test_skip_cache_bypasses_lookup(self):
        """skip_cache=true should always execute."""
        ...

    def test_ltm_failure_graceful(self):
        """LTM failures should not fail the action."""
        ...

    def test_cleanup_probability(self):
        """Cleanup should run ~5% of the time."""
        ...
```

## Example Usage

### Document Extraction with Cache

```yaml
name: document_extraction_agent
description: Extract data from documents with caching

nodes:
  - name: extract_with_cache
    uses: cache.wrap
    with:
      action: llamaextract.extract
      key_strategy: file_content
      key_source: file
      ttl_days: 60
      cleanup_probability: 0.05
      args:
        file: "{{ state.file_path }}"
        agent_name: "{{ state.agent_name }}"
    output: extraction_result

  - name: check_result
    run: |
      result = state.get("extraction_result", {})
      cache_hit = result.get("_cache_hit", False)

      if cache_hit:
        print(f"Cache hit! Created at: {result.get('_cache_created_at')}")
      else:
        print("Cache miss - fresh extraction performed")

      return {"data": result.get("result", {}).get("data")}
```

### LLM Call with Cache

```yaml
name: cached_llm_agent

nodes:
  - name: translate_cached
    uses: cache.wrap
    with:
      action: llm.call
      key_strategy: args
      ttl_days: 30
      args:
        model: gpt-4o
        messages:
          - role: user
            content: "Translate to {{ state.target_language }}: {{ state.text }}"
    output: translation_result
```

### Manual Cache Invalidation

```yaml
name: cache_management_agent

nodes:
  - name: invalidate_old_extractions
    uses: cache.invalidate
    with:
      metadata_filter:
        _cache_action: llamaextract.extract
    output: invalidation_result

  - name: report
    run: |
      result = state.get("invalidation_result", {})
      return {"deleted": result.get("deleted_count", 0)}
```

## Risk and Compatibility

### Minimal Risk Assessment

- **Primary Risk**: LTM backend not configured
- **Mitigation**: Graceful degradation - action executes without caching
- **Rollback**: Remove `cache.wrap` calls; underlying actions work unchanged

### Compatibility

- No breaking changes to existing actions
- Cache is opt-in via `cache.wrap`
- Works with all existing LTM backends (SQLite, Turso, blob-backed)

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] All tasks completed
- [ ] Unit tests pass (target: 40+ tests)
- [ ] Integration tests pass
- [ ] No regressions in existing LTM functionality
- [ ] Documentation updated
- [ ] Code follows existing TEA patterns

## QA Results

### Test Design Assessment (2024-12-24)

**Reviewer:** Quinn (Test Architect)

| Metric | Value |
|--------|-------|
| **Total Test Scenarios** | 48 |
| **Unit Tests** | 28 (58%) |
| **Integration Tests** | 15 (31%) |
| **E2E Tests** | 5 (11%) |
| **P0 (Critical)** | 18 |
| **P1 (High)** | 20 |
| **P2 (Medium)** | 10 |
| **Coverage Gaps** | None |

**Test Design Document:** `docs/qa/assessments/TEA-BUILTIN-010-test-design-20251224.md`

**Key Observations:**
- High unit test ratio (58%) appropriate for pure logic functions
- Strong focus on graceful degradation scenarios (AC-17, AC-18)
- LTM integration requires thorough mocking strategy
- Probabilistic cleanup needs statistical verification tests

**Risks Identified:**
1. Cache key collisions (mitigated by SHA256)
2. LTM backend unavailable (mitigated by graceful degradation tests)
3. Cleanup never runs (mitigated by probability verification tests)

**Recommendations:**
- Use pytest fixtures for LTM mocking
- Consider property-based testing for key determinism
- Add load tests for large cached results

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-24 | 0.1.0 | Initial story creation | Sarah (PO) |
| 2024-12-24 | 0.1.1 | Added QA test design (48 scenarios) | Quinn (QA) |
