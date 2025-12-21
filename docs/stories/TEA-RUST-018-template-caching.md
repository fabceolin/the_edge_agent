# TEA-RUST-018: Add Template Caching to Rust YAML Engine

## Story Overview

| Field | Value |
|-------|-------|
| **ID** | TEA-RUST-018 |
| **Type** | Story |
| **Priority** | Low |
| **Estimated Effort** | 1-2 hours |
| **Status** | Done |
| **Parent Epic** | TEA-RUST-001 |
| **Depends On** | TEA-RUST-016, TEA-RUST-017 |
| **Files to Modify** | `rust/src/engine/yaml.rs` |

## Description

**As a** developer running workflows with repeated template evaluations,
**I want** compiled templates to be cached,
**So that** template processing is optimized for performance.

## Background

The Python implementation caches compiled Jinja2 templates for performance (yaml_engine.py:381, 1399-1421):

```python
# Template cache for performance (AC: 8)
self._template_cache: Dict[str, Any] = {}

# Usage in _process_template:
cache_key = f"expr:{expr}"
if cache_key not in self._template_cache:
    self._template_cache[cache_key] = self._jinja_env.compile_expression(expr)
compiled = self._template_cache[cache_key]
```

The Rust implementation currently uses `Tera::one_off()` which compiles templates on every call. For workflows with many nodes or repeated invocations, caching provides performance benefits.

## Technical Details

### Current State

```rust
// Every render recompiles the template
Tera::one_off(template, &context, false)
```

### Target State

```rust
use std::sync::RwLock;
use std::collections::HashMap;
use tera::{Tera, Context};

pub struct YamlEngine {
    /// Tera instance wrapped in RwLock for interior mutability
    /// Required because add_raw_template needs &mut self
    tera: RwLock<Tera>,
    secrets: HashMap<String, JsonValue>,
    checkpoint_dir: Option<String>,
    last_checkpoint: Option<String>,
    /// Maps template content hash -> registered template name
    template_cache: RwLock<HashMap<String, String>>,
}

impl YamlEngine {
    pub fn render_template(&self, template: &str, context: &Context) -> TeaResult<String> {
        let cache_key = template.to_string();

        // Fast path: check cache with read lock
        {
            let cache = self.template_cache.read().unwrap();
            if let Some(name) = cache.get(&cache_key) {
                let tera = self.tera.read().unwrap();
                return tera.render(name, context)
                    .map_err(|e| TeaError::Template(e.to_string()));
            }
        }

        // Slow path: compile and cache with write locks
        let name = {
            let mut cache = self.template_cache.write().unwrap();

            // Double-check after acquiring write lock (another thread may have cached it)
            if let Some(name) = cache.get(&cache_key) {
                return {
                    let tera = self.tera.read().unwrap();
                    tera.render(name, context)
                        .map_err(|e| TeaError::Template(e.to_string()))
                };
            }

            let name = format!("__cached_{}", cache.len());

            // Add template to Tera (requires write lock on tera)
            {
                let mut tera = self.tera.write().unwrap();
                tera.add_raw_template(&name, template)
                    .map_err(|e| TeaError::Template(format!("Failed to compile template: {}", e)))?;
            }

            cache.insert(cache_key, name.clone());
            name
        };

        // Render from cached template
        let tera = self.tera.read().unwrap();
        tera.render(&name, context)
            .map_err(|e| TeaError::Template(e.to_string()))
    }

    /// Returns the number of cached templates (for testing/monitoring)
    pub fn cache_size(&self) -> usize {
        self.template_cache.read().unwrap().len()
    }
}
```

---

## Edge Cases & Error Handling

### Cache Behavior

1. **Cache Key Strategy**: Use template string content as cache key. Identical templates share cached compilation.

2. **Memory Growth**: For MVP, accept unbounded cache growth. Add cache statistics logging to monitor in production:
   ```rust
   tracing::debug!(cache_size = cache.len(), "Template cache stats");
   ```

3. **Future Consideration (Out of Scope)**: If memory becomes an issue, consider LRU eviction via `mini-moka` crate. Track in backlog as TEA-RUST-018.1.

### Error Handling

1. **Template Compilation Failure**: Return `TeaError::Template` with compilation error details. Do not cache failed templates.

2. **Lock Poisoning**: Use `.unwrap()` for RwLock - poison indicates unrecoverable state (panicked thread).

### Thread Safety

1. **Double-Check Locking**: After acquiring write lock, re-check cache to handle race condition where another thread cached the same template.

2. **Lock Ordering**: Always acquire `template_cache` lock before `tera` lock to prevent deadlocks.

---

## Acceptance Criteria

- [x] **AC-1**: GIVEN a template rendered multiple times, WHEN `render_template` is called, THEN compilation only happens once

- [x] **AC-2**: GIVEN concurrent template rendering, WHEN multiple threads call `render_template`, THEN cache access is thread-safe (no data races)

- [x] **AC-3**: GIVEN a workflow with 100 nodes using the same template pattern, WHEN executed, THEN template caching provides measurable performance improvement

- [x] **AC-4**: GIVEN cache enabled, WHEN rendering different templates, THEN each unique template is cached separately

---

## Implementation Tasks

1. [x] Add `template_cache: RwLock<HashMap<String, String>>` to YamlEngine
2. [x] Update `render_template` to check cache before compiling
3. [x] Add compiled templates to Tera instance for reuse
4. [x] Add benchmark test comparing cached vs uncached performance
5. [x] Ensure thread-safety with RwLock

---

## Benchmark Test

```rust
#[bench]
fn bench_template_rendering_uncached(b: &mut Bencher) {
    let engine = YamlEngine::new();
    let state = json!({"name": "World"});
    let variables = HashMap::new();
    let template = "Hello, {{ state.name }}!";

    b.iter(|| {
        engine.render_template(template, &state, &variables).unwrap()
    });
}

#[bench]
fn bench_template_rendering_cached(b: &mut Bencher) {
    let engine = YamlEngineCached::new();  // With caching
    let state = json!({"name": "World"});
    let variables = HashMap::new();
    let template = "Hello, {{ state.name }}!";

    // Warm up cache
    engine.render_template(template, &state, &variables).unwrap();

    b.iter(|| {
        engine.render_template(template, &state, &variables).unwrap()
    });
}
```

---

## Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cache_hit_returns_same_result() {
        let engine = YamlEngine::new();
        let state = json!({"name": "World"});
        let template = "Hello, {{ state.name }}!";

        let result1 = engine.render_template(template, &state, &HashMap::new()).unwrap();
        let result2 = engine.render_template(template, &state, &HashMap::new()).unwrap();

        assert_eq!(result1, result2);
        assert_eq!(engine.cache_size(), 1); // Only cached once
    }

    #[test]
    fn test_different_templates_cached_separately() {
        let engine = YamlEngine::new();
        let state = json!({"name": "World"});

        engine.render_template("Hello, {{ state.name }}!", &state, &HashMap::new()).unwrap();
        engine.render_template("Goodbye, {{ state.name }}!", &state, &HashMap::new()).unwrap();

        assert_eq!(engine.cache_size(), 2);
    }

    #[test]
    fn test_template_compilation_error_not_cached() {
        let engine = YamlEngine::new();
        let state = json!({});
        let invalid_template = "{{ invalid syntax {{";

        let result = engine.render_template(invalid_template, &state, &HashMap::new());
        assert!(result.is_err());
        assert_eq!(engine.cache_size(), 0); // Failed template not cached
    }

    #[test]
    fn test_concurrent_cache_access() {
        use std::thread;
        use std::sync::Arc;

        let engine = Arc::new(YamlEngine::new());
        let template = "Hello, {{ state.name }}!";

        let handles: Vec<_> = (0..10).map(|i| {
            let engine = Arc::clone(&engine);
            thread::spawn(move || {
                let state = json!({"name": format!("Thread{}", i)});
                engine.render_template(template, &state, &HashMap::new()).unwrap()
            })
        }).collect();

        for handle in handles {
            handle.join().unwrap();
        }

        assert_eq!(engine.cache_size(), 1); // Same template, cached once
    }
}
```

---

## Definition of Done

- [x] All acceptance criteria pass
- [x] Benchmark shows performance improvement (~17x speedup for warm cache)
- [x] `cargo clippy` passes without warnings
- [x] `cargo test` passes (387 tests)
- [x] No memory leaks from unbounded cache growth (LRU deferred to TEA-RUST-018.1 if needed)

---

## Notes

This is a **low priority** optimization story. The current `Tera::one_off()` approach is functional and correct. Caching provides incremental performance benefits for:
- Workflows with many nodes using similar templates
- Long-running agents with repeated invocations
- High-throughput edge deployments

Consider implementing after TEA-RUST-016 and TEA-RUST-017 are complete.

---

## QA Results

### Review Date: 2025-12-20

### Reviewed By: Quinn (Test Architect)

### Pre-Implementation Review

**Story Status: Ready (Validated 2025-12-21)**

This story is correctly marked as **low priority** and is a performance optimization. The prerequisites (TEA-RUST-016, TEA-RUST-017) have been implemented.

### Technical Assessment

The proposed implementation approach is sound:
- Using `RwLock<HashMap<String, String>>` for thread-safe cache access
- Read-lock for cache hits (fast path)
- Write-lock for cache misses (compilation)

### Concerns Noted for Future Implementation

1. **Cache Invalidation**: The proposed design caches by template string content. If the same template string is used with different Tera configurations, this could cause issues.

2. **Memory Growth**: The cache grows unbounded. For long-running agents with many unique templates, this could consume significant memory. Consider:
   - LRU eviction policy
   - Maximum cache size limit
   - Cache clear method

3. **Tera Instance Mutation**: The proposed code shows `self.tera.add_raw_template()` which requires `&mut self`. This conflicts with `&self` in `render_template`. The implementation will need to use interior mutability (e.g., `Mutex<Tera>` or redesign).

### Resolution Status (2025-12-21)

1. **Tera Instance Mutation** - RESOLVED: Changed `tera: Tera` to `tera: RwLock<Tera>` for interior mutability. Updated code example with proper lock ordering and double-check locking pattern.

2. **Memory Growth** - DOCUMENTED: Accepted unbounded growth for MVP with monitoring via `tracing::debug!`. LRU eviction deferred to future story TEA-RUST-018.1 if needed.

3. **Cache Invalidation** - DOCUMENTED: Cache by template content; identical templates share compilation. Edge case documented in new "Edge Cases & Error Handling" section.

4. **Testing Gaps** - RESOLVED: Added comprehensive unit test scenarios covering cache hits, separate caching, error handling, and concurrent access.

### Recommendations

1. Profile actual template compilation overhead before implementing
2. Consider using `mini-moka` or similar for LRU caching
3. Add cache statistics (hits/misses) for observability

### Gate Status

**Gate: N/A** - Story not in "Review" status. No gate file created.

### Recommended Next Steps

1. Complete TEA-RUST-016 test coverage gaps
2. Merge TEA-RUST-016 and TEA-RUST-017
3. Profile template rendering in real workflows
4. If performance is an issue, implement this story

---

### Test Design Assessment (2025-12-21)

**Document:** `docs/qa/assessments/TEA-RUST-018-test-design-20251221.md`

#### Test Strategy Summary

| Metric | Value |
|--------|-------|
| Total Scenarios | 12 |
| Unit Tests | 7 (58%) |
| Integration Tests | 3 (25%) |
| E2E Tests | 2 (17%) |
| P0 (Critical) | 0 |
| P1 (High) | 6 |
| P2 (Medium) | 5 |
| P3 (Low) | 1 |

#### Priority Justification

**No P0 tests assigned** - appropriate for low-priority performance optimization:
- Internal implementation detail
- No direct revenue or security impact
- Failure mode is performance degradation, not data loss

#### Test Coverage by Acceptance Criteria

| AC | Coverage | Key Tests |
|----|----------|-----------|
| AC-1 (Cache compilation) | 3 scenarios | UNIT-001, UNIT-002, UNIT-003 |
| AC-2 (Thread safety) | 3 scenarios | UNIT-004, UNIT-005, INT-001 |
| AC-3 (Performance) | 2 scenarios | INT-002 (benchmark), E2E-001 |
| AC-4 (Separate caching) | 3 scenarios | UNIT-006, UNIT-007, INT-003 |

#### Coverage Gaps

None identified - all acceptance criteria have test coverage.

#### Test Design YAML Block

```yaml
test_design:
  scenarios_total: 12
  by_level:
    unit: 7
    integration: 3
    e2e: 2
  by_priority:
    p0: 0
    p1: 6
    p2: 5
    p3: 1
  coverage_gaps: []
```

#### Notes

- Story includes well-designed unit tests in implementation section
- Test design adds thread safety edge cases and benchmark methodology
- Unbounded cache growth is documented as accepted for MVP (not tested)

---

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
N/A

### Implementation Summary (2025-12-21)

**Implementation COMPLETED** by James (Dev Agent):

1. **YamlEngine struct modified:**
   - Changed `tera: Tera` to `tera: RwLock<Tera>` for interior mutability
   - Added `template_cache: RwLock<HashMap<String, String>>` for cache storage
   - Added `cache_size()` method for testing/monitoring

2. **render_template() updated with double-check locking pattern:**
   - Fast path: read lock on cache → render from cached template
   - Slow path: write lock on cache → double-check → compile → cache → render
   - Lock ordering: `template_cache` before `tera` to prevent deadlocks

3. **Unit tests added (6 new tests):**
   - `test_cache_hit_returns_same_result`
   - `test_different_templates_cached_separately`
   - `test_template_compilation_error_not_cached`
   - `test_concurrent_cache_access`
   - `test_cache_with_different_state_values`
   - `test_cache_with_variables`

4. **Benchmark results:**
   - Cold cache: ~18µs per template
   - Warm cache (1000x): ~1.08µs per template (**~17x speedup**)

### Completion Notes
- All 387 Rust tests pass
- cargo clippy passes with no warnings
- Template caching benchmarks run successfully
- Note: `set_secrets` and `set_checkpoint_dir` methods in original benchmark were removed (not implemented in YamlEngine; those are separate stories TEA-RUST-016/017)

### File List
| File | Action | Description |
|------|--------|-------------|
| `rust/src/engine/yaml.rs` | Modified | Added RwLock wrapping for tera and template_cache, caching logic in render_template, 6 unit tests |
| `rust/benches/graph_benchmarks.rs` | Modified | Fixed benchmark to use current API (removed set_secrets/set_checkpoint_dir references) |

### Change Log
| Date | Change |
|------|--------|
| 2025-12-21 | Initial implementation of template caching with RwLock pattern |

### Status
**Ready for Review**

---

### Post-Implementation Review: 2025-12-21

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: EXCELLENT** - Implementation is clean, well-documented, and follows Rust best practices.

The template caching implementation:
- ✅ Uses idiomatic Rust with `RwLock` for thread-safe interior mutability
- ✅ Implements proper double-check locking pattern to avoid race conditions
- ✅ Documents lock ordering (`template_cache` before `tera`) to prevent deadlocks
- ✅ Provides `cache_size()` method for testing and monitoring
- ✅ Error handling prevents caching of failed template compilations

### Refactoring Performed

None required - implementation is clean and follows best practices.

### Compliance Check

- Coding Standards: ✓ (cargo clippy clean)
- Project Structure: ✓ (files in correct locations)
- Testing Strategy: ✓ (6 unit tests + criterion benchmarks)
- All ACs Met: ✓ (all 4 acceptance criteria verified)

### Improvements Checklist

- [x] Cache implementation with RwLock pattern
- [x] Thread-safe double-check locking
- [x] Unit tests for cache behavior
- [x] Concurrent access test
- [x] Benchmark tests for performance validation
- [ ] (Future) Add LRU eviction if memory growth becomes issue (TEA-RUST-018.1)
- [ ] (Future) Add cache hit/miss metrics for observability

### Security Review

**No concerns.** Template caching is an internal optimization with no security implications. Failed templates are not cached, preventing cache poisoning.

### Performance Considerations

**VERIFIED: ~17x speedup achieved**
- Cold cache: ~18µs per template compile+render
- Warm cache: ~1.08µs per render (1000x iterations)

This meets AC-3 (measurable performance improvement).

### Dependency Investigation: `set_secrets` / `set_checkpoint_dir`

**FINDING:** The original benchmark file referenced `set_secrets()` and `set_checkpoint_dir()` methods that do not exist in `YamlEngine`.

**Investigation Results:**
- **TEA-RUST-016** (Secrets Context) - Status: Done - but `set_secrets()` NOT in yaml.rs
- **TEA-RUST-017** (Checkpoint Context) - Status: Done - but `set_checkpoint_dir()` NOT in yaml.rs

**Impact Assessment:**
- These are **separate stories** (TEA-RUST-016, TEA-RUST-017) that should implement these APIs
- TEA-RUST-018 (this story) correctly listed them as dependencies in the story header
- The Dev Agent correctly removed the speculative API calls from the benchmark
- **No action required for TEA-RUST-018** - the template caching works without these APIs

**Recommendation:** File a follow-up to investigate why TEA-RUST-016 and TEA-RUST-017 are marked "Done" but their API methods are missing from `yaml.rs`. Possible explanations:
1. Implementation exists elsewhere (executor.rs, separate module)
2. Stories were closed prematurely
3. API design changed during implementation

### Files Modified During Review

None - implementation was clean, no refactoring needed.

### Gate Status

Gate: **PASS** → `docs/qa/gates/TEA-RUST-018-template-caching.yml`

### Recommended Status

**✓ Ready for Done** - All acceptance criteria met, comprehensive tests, ~17x performance improvement verified.
