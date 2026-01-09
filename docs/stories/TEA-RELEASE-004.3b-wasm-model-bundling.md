# Story TEA-RELEASE-004.3b: WASM Model Loading and Caching (Phi-4-mini)

## Status

Ready for Review

**Validation Date:** 2026-01-08
**Validated By:** Bob (Scrum Master Agent)
**Checklist:** All 5 categories passed - Goal & Context Clarity, Technical Implementation Guidance, Reference Effectiveness, Self-Containment Assessment, Testing Guidance

## Story

**As a** developer deploying TEA WASM with offline LLM,
**I want** the Phi-4-mini Q3_K_S model (~1.9GB) loaded and cached in IndexedDB,
**So that** users can run LLM workflows without re-downloading the model on each visit.

> **Note:** Phi-4-mini Q3_K_S (1.9GB) fits GitHub's 2GB limit as a single file - **no chunking required!** This significantly simplifies the implementation compared to the original Gemma 3n E4B (4.54GB) plan.

## Story Context

**Existing System Integration:**

- Integrates with: TEA-RELEASE-004.3a (WASM LLM Core Package)
- Technology: TypeScript, IndexedDB, GitHub Releases
- Follows pattern: Model chunking for GitHub 2GB limit
- Touch points: `rust/tea-wasm-llm/js/`, GitHub Release assets

**Dependency:** TEA-RELEASE-004.3a (WASM LLM Core Package) - must be completed first

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: ~~REMOVED~~ - Phi-4-mini Q3_K_S (1.9GB) fits GitHub 2GB limit, no chunking needed
2. **AC-2**: Model loads directly from single GGUF file (no reassembly)
3. **AC-3**: Model loads from bundled/local assets without external CDN requests
4. **AC-4**: IndexedDB caching stores model after first load
5. **AC-5**: Subsequent page loads use cached model (skip download)
6. **AC-6**: Cache invalidation works when model version changes

### Build Requirements

7. **AC-7**: `model-manifest.json` describes single file and SHA256 checksum
8. **AC-8**: Build script downloads model (no splitting needed)
9. **AC-9**: Total bundled size documented (~1.9GB model + ~50MB WASM = ~2GB total)

### Quality Requirements

10. **AC-10**: Cache hit/miss behavior tested
11. **AC-11**: Corrupted cache recovery works (re-download)
12. **AC-12**: Progress callback during model loading

## Tasks / Subtasks

- [x] Task 1: Create model download script (AC: 7, 8) [SIMPLIFIED]
  - [x] Create `scripts/download-model.sh` script
  - [x] Download `microsoft_Phi-4-mini-instruct-Q3_K_S.gguf` (~1.9GB) from HuggingFace
  - [x] Generate `model-manifest.json` with single file metadata and SHA256 checksum
  - [x] **No chunking required** - file is under 2GB GitHub limit

- [x] Task 2: Create model loader TypeScript module (AC: 2, 3, 12) [SIMPLIFIED]
  - [x] Create `js/model-loader.ts`
  - [x] Implement `loadModel(manifest)` function (single file, no reassembly)
  - [x] Add progress callback: `onProgress(loaded, total)`
  - [x] Verify SHA256 checksum after download

- [x] Task 3: Implement IndexedDB caching (AC: 4, 5, 6)
  - [x] Create `js/model-cache.ts`
  - [x] Implement `openModelCache()` - create/open IndexedDB
  - [x] Implement `getCachedModel(version)` - retrieve if exists
  - [x] Implement `cacheModel(version, data)` - store model
  - [x] Implement `clearCache()` - manual cache clear
  - [x] Add version check for cache invalidation

- [x] Task 4: Integrate caching with model loader (AC: 5, 11)
  - [x] Update `loadBundledModel()` to check cache first
  - [x] If cache hit: return cached model
  - [x] If cache miss: load chunks, reassemble, cache, return
  - [x] Handle corrupted cache: clear and re-download

- [x] Task 5: Add tests for caching behavior (AC: 10, 11)
  - [x] Test cache miss scenario (first load)
  - [x] Test cache hit scenario (second load)
  - [x] Test cache invalidation (version change)
  - [x] Test corrupted cache recovery

## Dev Notes

### Model Manifest Format (Simplified - Single File)

```json
{
  "model": "microsoft_Phi-4-mini-instruct-Q3_K_S",
  "version": "v1",
  "totalSize": 1900000000,
  "file": "microsoft_Phi-4-mini-instruct-Q3_K_S.gguf",
  "sha256": "..."
}
```

> **Note:** No chunking needed! Single file format is much simpler than the original multi-chunk design.

### Model Download Script (Simplified - No Chunking)

```bash
#!/bin/bash
# scripts/download-model.sh
set -e

MODEL_URL="https://huggingface.co/bartowski/microsoft_Phi-4-mini-instruct-GGUF/resolve/main/microsoft_Phi-4-mini-instruct-Q3_K_S.gguf"
MODEL_NAME="microsoft_Phi-4-mini-instruct-Q3_K_S"
OUTPUT_DIR="models"

mkdir -p "$OUTPUT_DIR"

echo "Downloading Phi-4-mini Q3_K_S (~1.9GB)..."
wget -q --show-progress -O "$OUTPUT_DIR/$MODEL_NAME.gguf" "$MODEL_URL"

echo "Generating manifest..."
SIZE=$(stat -c%s "$OUTPUT_DIR/$MODEL_NAME.gguf" 2>/dev/null || stat -f%z "$OUTPUT_DIR/$MODEL_NAME.gguf")
SHA=$(sha256sum "$OUTPUT_DIR/$MODEL_NAME.gguf" | cut -d' ' -f1)

cat > "$OUTPUT_DIR/manifest.json" << EOF
{
  "model": "$MODEL_NAME",
  "version": "v1",
  "totalSize": $SIZE,
  "file": "$MODEL_NAME.gguf",
  "sha256": "$SHA"
}
EOF

echo "Done! Model in $OUTPUT_DIR/"
ls -lh "$OUTPUT_DIR"
```

> **Note:** No splitting needed - Phi-4-mini Q3_K_S (1.9GB) fits GitHub's 2GB limit!

### Model Loader (Simplified - Single File)

```typescript
// js/model-loader.ts

export interface ModelManifest {
  model: string;
  version: string;
  totalSize: number;
  file: string;      // Single file, no chunks array
  sha256: string;
}

export type ProgressCallback = (loaded: number, total: number) => void;

export async function loadModel(
  basePath: string,
  manifest: ModelManifest,
  onProgress?: ProgressCallback
): Promise<Uint8Array> {
  const response = await fetch(`${basePath}/${manifest.file}`);
  if (!response.ok) {
    throw new Error(`Failed to load model: ${manifest.file}`);
  }

  // Stream the response for progress tracking
  const reader = response.body!.getReader();
  const chunks: Uint8Array[] = [];
  let loadedSize = 0;

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;

    chunks.push(value);
    loadedSize += value.length;
    onProgress?.(loadedSize, manifest.totalSize);
  }

  // Combine chunks into single buffer
  const combined = new Uint8Array(manifest.totalSize);
  let offset = 0;
  for (const chunk of chunks) {
    combined.set(chunk, offset);
    offset += chunk.length;
  }

  return combined;
}

export async function verifyChecksum(
  data: Uint8Array,
  expectedSha256: string
): Promise<boolean> {
  const hashBuffer = await crypto.subtle.digest('SHA-256', data);
  const hashArray = Array.from(new Uint8Array(hashBuffer));
  const hashHex = hashArray.map(b => b.toString(16).padStart(2, '0')).join('');
  return hashHex === expectedSha256;
}
```

> **Note:** Single file loading is much simpler - no chunk reassembly logic needed!

### IndexedDB Cache

```typescript
// js/model-cache.ts

const DB_NAME = 'tea-llm-cache';
const DB_VERSION = 1;
const STORE_NAME = 'models';

interface CachedModel {
  version: string;
  data: Uint8Array;
  timestamp: number;
}

async function openDB(): Promise<IDBDatabase> {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open(DB_NAME, DB_VERSION);

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result);

    request.onupgradeneeded = (event) => {
      const db = (event.target as IDBOpenDBRequest).result;
      if (!db.objectStoreNames.contains(STORE_NAME)) {
        db.createObjectStore(STORE_NAME, { keyPath: 'version' });
      }
    };
  });
}

export async function getCachedModel(version: string): Promise<Uint8Array | null> {
  try {
    const db = await openDB();
    return new Promise((resolve, reject) => {
      const tx = db.transaction(STORE_NAME, 'readonly');
      const store = tx.objectStore(STORE_NAME);
      const request = store.get(version);

      request.onerror = () => reject(request.error);
      request.onsuccess = () => {
        const result = request.result as CachedModel | undefined;
        resolve(result?.data || null);
      };
    });
  } catch (e) {
    console.warn('Cache read failed:', e);
    return null;
  }
}

export async function cacheModel(version: string, data: Uint8Array): Promise<void> {
  try {
    const db = await openDB();
    return new Promise((resolve, reject) => {
      const tx = db.transaction(STORE_NAME, 'readwrite');
      const store = tx.objectStore(STORE_NAME);
      const request = store.put({
        version,
        data,
        timestamp: Date.now(),
      });

      request.onerror = () => reject(request.error);
      request.onsuccess = () => resolve();
    });
  } catch (e) {
    console.warn('Cache write failed:', e);
  }
}

export async function clearCache(): Promise<void> {
  try {
    const db = await openDB();
    return new Promise((resolve, reject) => {
      const tx = db.transaction(STORE_NAME, 'readwrite');
      const store = tx.objectStore(STORE_NAME);
      const request = store.clear();

      request.onerror = () => reject(request.error);
      request.onsuccess = () => resolve();
    });
  } catch (e) {
    console.warn('Cache clear failed:', e);
  }
}
```

### Integrated Model Loading

```typescript
// js/index.ts (updated)
import { loadModelChunks, ModelManifest, ProgressCallback } from './model-loader';
import { getCachedModel, cacheModel, clearCache } from './model-cache';

export interface TeaLlmConfig {
  modelBasePath?: string;     // Where to load model chunks from
  useCache?: boolean;         // Use IndexedDB cache (default: true)
  onProgress?: ProgressCallback;
}

export async function loadBundledModel(
  config: TeaLlmConfig = {}
): Promise<Uint8Array> {
  const basePath = config.modelBasePath || './models';
  const useCache = config.useCache ?? true;

  // Load manifest
  const manifestResponse = await fetch(`${basePath}/manifest.json`);
  const manifest: ModelManifest = await manifestResponse.json();

  // Check cache first
  if (useCache) {
    const cached = await getCachedModel(manifest.version);
    if (cached) {
      console.log('Model loaded from cache');
      return cached;
    }
  }

  // Load from chunks
  console.log('Loading model from chunks...');
  const modelData = await loadModelChunks(basePath, manifest, config.onProgress);

  // Cache for next time
  if (useCache) {
    await cacheModel(manifest.version, modelData);
    console.log('Model cached');
  }

  return modelData;
}
```

### Error Handling for Corrupted Cache

```typescript
export async function loadBundledModelSafe(
  config: TeaLlmConfig = {}
): Promise<Uint8Array> {
  try {
    return await loadBundledModel(config);
  } catch (e) {
    console.warn('Model load failed, clearing cache and retrying:', e);
    await clearCache();
    return await loadBundledModel({ ...config, useCache: false });
  }
}
```

## Definition of Done

- [ ] Model download script fetches Phi-4-mini Q3_K_S (~1.9GB)
- [ ] `model-manifest.json` generated with SHA256 checksum
- [ ] `loadModel()` loads single GGUF file correctly
- [ ] IndexedDB cache stores/retrieves model
- [ ] Cache hit skips download (verified in DevTools)
- [ ] Version change triggers re-download
- [ ] Corrupted cache recovery works
- [ ] Progress callback fires during load

## Risk and Compatibility Check

**Primary Risk:** IndexedDB storage limits (~50MB-2GB depending on browser)

**Mitigation:**
- Phi-4-mini Q3_K_S (1.9GB) fits within Chrome/Firefox IndexedDB limits
- Safari still has 1GB hard limit - streaming fallback documented
- Provide `clearCache()` for manual cleanup

**Risk Reduction from Original Plan:**
- No chunking logic = fewer failure points
- Single file = simpler download/cache/verify flow
- GitHub Release compatible without external hosting

**Rollback:** Caching is optional; model loads without cache

## Compatibility Verification

- [x] No breaking changes to existing APIs
- [x] Database changes: IndexedDB (browser-local)
- [x] UI changes: None
- [x] Performance impact: Faster subsequent loads

## QA Notes

**Assessment Date:** 2026-01-08
**Assessed By:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Count |
|--------|-------|
| **Total test scenarios** | 24 |
| **Unit tests** | 10 (42%) |
| **Integration tests** | 9 (37%) |
| **E2E tests** | 5 (21%) |
| **Priority distribution** | P0: 8, P1: 10, P2: 6 |
| **AC coverage** | 100% (all 12 acceptance criteria covered) |

### Risk Areas Identified

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| **Safari IndexedDB 1GB limit** | Medium | High | Safari cannot cache 1.9GB model; tests verify graceful fallback to streaming |
| **Corrupted cache causes permanent failure** | Low | Critical | `loadBundledModelSafe()` auto-clears and re-downloads on corruption |
| **Version mismatch leads to stale model** | Medium | High | Version-keyed cache with explicit invalidation on manifest version change |
| **Network timeout during large file download** | Medium | Medium | Progress callbacks enable UI feedback; retry logic recommended |
| **Checksum mismatch undetected** | Low | Critical | SHA256 verification after download; test coverage for checksum logic |

### Recommended Test Scenarios

**Phase 1 - Critical Path (P0):**
1. Core loading function returns correct Uint8Array
2. Real GGUF file loads from local HTTP server
3. No external CDN requests (security/offline validation)
4. IndexedDB successfully stores model after first load
5. Cache hit skips network download on subsequent loads
6. Version change triggers cache invalidation and re-download
7. Cache performance: hit is >10x faster than miss
8. Corrupted cache triggers automatic recovery

**Phase 2 - Core Functionality (P1):**
- Buffer size matches manifest.totalSize
- URL construction with basePath
- Data integrity verification in storage
- Cache retrieval for matching version
- Manifest schema validation
- Build script execution verification
- Progress callback fires during download

**Phase 3 - Edge Cases (P2):**
- CachedModel structure validation
- Version comparison edge cases (v1 vs v1.0)
- SHA256 format validation
- Size bounds verification (1.8GB - 2.0GB range)
- Log output distinguishes cache hit/miss

### Concerns and Blockers

| Type | Description | Recommendation |
|------|-------------|----------------|
| **BLOCKER** | Safari 1GB IndexedDB limit prevents caching 1.9GB model | Document Safari limitation; implement streaming fallback without cache |
| **CONCERN** | Large file E2E tests slow in CI | Use ~10MB mock model for CI; run full 1.9GB tests in nightly builds |
| **CONCERN** | IndexedDB quota varies by browser | Add quota detection and user-friendly error messaging |

### Test Environment Requirements

- **Unit tests:** Jest/Vitest with fake-indexeddb polyfill
- **Integration tests:** Real IndexedDB in browser/polyfill; small (~1MB) mock GGUF
- **E2E tests:** Playwright with Chrome/Firefox; local HTTP server for model assets

### Gate Readiness

```yaml
test_design_complete: true
scenarios_total: 24
p0_critical: 8
coverage_gaps: []
blocking_risks:
  - Safari 1GB limit requires documented workaround before release
```

**Reference:** Full test design at `docs/qa/assessments/TEA-RELEASE-004.3b-test-design-20260108.md`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Split from TEA-RELEASE-004.3 | Bob (SM Agent) |
| 2026-01-08 | 0.2 | **Major simplification:** Changed to Phi-4-mini Q3_K_S (1.9GB), removed chunking | Sarah (PO Agent) |
| 2026-01-08 | 0.3 | Added QA Notes section with test design summary and risk assessment | Quinn (QA Agent) |
| 2026-01-08 | 1.0 | Implementation complete - all 5 tasks done, 31 tests passing | James (Dev Agent) |

---

## Dev Agent Record

### Agent Model Used

claude-opus-4-5-20251101

### Debug Log References

N/A - No blocking issues encountered during implementation.

### Completion Notes

1. **Task 1 (Model Download Script):** Created `scripts/download-model.sh` with HuggingFace download, SHA256 checksum generation, and manifest.json creation. No chunking needed for 1.9GB model.

2. **Task 2 (Model Loader):** Created `js/model-loader.ts` with:
   - `loadModel()` - Streaming download with progress callback
   - `verifyChecksum()` / `calculateChecksum()` - SHA256 verification
   - `fetchManifest()` - Manifest loading and validation
   - `formatBytes()` - Human-readable size formatting

3. **Task 3 (IndexedDB Caching):** Created `js/model-cache.ts` with:
   - `openDB()` - IndexedDB database management
   - `getCachedModel()` / `getCachedModelEntry()` - Cache retrieval
   - `cacheModel()` - Cache storage with metadata
   - `clearCache()` / `deleteCachedModel()` - Cache management
   - `isCached()` / `getCacheStats()` / `listCachedModels()` - Cache introspection
   - `checkStorageCapacity()` - Safari 1GB limit detection

4. **Task 4 (Integration):** Updated `js/index.ts` with:
   - `loadBundledModel()` - Main loading function with cache-first strategy
   - `loadBundledModelSafe()` - Corrupted cache recovery wrapper
   - Re-exports of all model loading/caching functions

5. **Task 5 (Tests):** Created comprehensive test suite:
   - `tests/model-cache.test.ts` - 31 unit tests with fake-indexeddb
   - `tests/model-cache-test.html` - Browser test harness for manual testing
   - All tests passing: 31/31

### File List

**New Files:**
- `rust/tea-wasm-llm/scripts/download-model.sh` - Model download script
- `rust/tea-wasm-llm/js/model-loader.ts` - Model loading module
- `rust/tea-wasm-llm/js/model-cache.ts` - IndexedDB caching module
- `rust/tea-wasm-llm/js/tsconfig.json` - TypeScript configuration
- `rust/tea-wasm-llm/tests/model-cache.test.ts` - Vitest unit tests (31 tests)
- `rust/tea-wasm-llm/tests/model-cache-test.html` - Browser test harness
- `rust/tea-wasm-llm/vitest.config.ts` - Vitest configuration
- `rust/tea-wasm-llm/package.json` - NPM package configuration
- `rust/tea-wasm-llm/.gitignore` - Git ignore rules for models
- `rust/tea-wasm-llm/models/.gitkeep` - Placeholder for model files
- `rust/tea-wasm-llm/models/model-manifest.example.json` - Example manifest

**Modified Files:**
- `rust/tea-wasm-llm/js/index.ts` - Added model loading integration with caching
- `docs/stories/TEA-RELEASE-004.3b-wasm-model-bundling.md` - Story completion updates
