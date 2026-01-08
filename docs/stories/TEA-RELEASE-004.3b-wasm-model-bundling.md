# Story TEA-RELEASE-004.3b: WASM Model Loading and Caching (Phi-4-mini)

## Status

Draft

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

- [ ] Task 1: Create model download script (AC: 7, 8) [SIMPLIFIED]
  - [ ] Create `scripts/download-model.sh` script
  - [ ] Download `microsoft_Phi-4-mini-instruct-Q3_K_S.gguf` (~1.9GB) from HuggingFace
  - [ ] Generate `model-manifest.json` with single file metadata and SHA256 checksum
  - [ ] **No chunking required** - file is under 2GB GitHub limit

- [ ] Task 2: Create model loader TypeScript module (AC: 2, 3, 12) [SIMPLIFIED]
  - [ ] Create `js/model-loader.ts`
  - [ ] Implement `loadModel(manifest)` function (single file, no reassembly)
  - [ ] Add progress callback: `onProgress(loaded, total)`
  - [ ] Verify SHA256 checksum after download

- [ ] Task 3: Implement IndexedDB caching (AC: 4, 5, 6)
  - [ ] Create `js/model-cache.ts`
  - [ ] Implement `openModelCache()` - create/open IndexedDB
  - [ ] Implement `getCachedModel(version)` - retrieve if exists
  - [ ] Implement `cacheModel(version, data)` - store model
  - [ ] Implement `clearCache()` - manual cache clear
  - [ ] Add version check for cache invalidation

- [ ] Task 4: Integrate caching with model loader (AC: 5, 11)
  - [ ] Update `loadBundledModel()` to check cache first
  - [ ] If cache hit: return cached model
  - [ ] If cache miss: load chunks, reassemble, cache, return
  - [ ] Handle corrupted cache: clear and re-download

- [ ] Task 5: Add tests for caching behavior (AC: 10, 11)
  - [ ] Test cache miss scenario (first load)
  - [ ] Test cache hit scenario (second load)
  - [ ] Test cache invalidation (version change)
  - [ ] Test corrupted cache recovery

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

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Split from TEA-RELEASE-004.3 | Bob (SM Agent) |
| 2026-01-08 | 0.2 | **Major simplification:** Changed to Phi-4-mini Q3_K_S (1.9GB), removed chunking | Sarah (PO Agent) |
