# Story TEA-RELEASE-004.3b: WASM Model Bundling and Caching

## Status

Draft

## Story

**As a** developer deploying TEA WASM with offline LLM,
**I want** the Gemma 3n E4B model bundled and cached in IndexedDB,
**So that** users can run LLM workflows without re-downloading the model on each visit.

## Story Context

**Existing System Integration:**

- Integrates with: TEA-RELEASE-004.3a (WASM LLM Core Package)
- Technology: TypeScript, IndexedDB, GitHub Releases
- Follows pattern: Model chunking for GitHub 2GB limit
- Touch points: `rust/tea-wasm-llm/js/`, GitHub Release assets

**Dependency:** TEA-RELEASE-004.3a (WASM LLM Core Package) - must be completed first

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: Model split into chunks <2GB for GitHub Release compatibility
2. **AC-2**: Model chunks reassemble correctly into single GGUF file
3. **AC-3**: Model loads from bundled/local assets without external CDN requests
4. **AC-4**: IndexedDB caching stores model after first load
5. **AC-5**: Subsequent page loads use cached model (skip download)
6. **AC-6**: Cache invalidation works when model version changes

### Build Requirements

7. **AC-7**: `model-manifest.json` describes chunk files and checksums
8. **AC-8**: Build script downloads and splits model automatically
9. **AC-9**: Total bundled size documented (~4.5GB)

### Quality Requirements

10. **AC-10**: Cache hit/miss behavior tested
11. **AC-11**: Corrupted cache recovery works (re-download)
12. **AC-12**: Progress callback during model loading

## Tasks / Subtasks

- [ ] Task 1: Create model chunking infrastructure (AC: 1, 7, 8)
  - [ ] Create `scripts/split-model.sh` script
  - [ ] Download `gemma-3n-E4B-it-Q4_K_M.gguf` from HuggingFace
  - [ ] Split into chunks <2GB using `split` command
  - [ ] Generate `model-manifest.json` with chunk names, sizes, SHA256 checksums
  - [ ] Document chunk naming convention

- [ ] Task 2: Create model loader TypeScript module (AC: 2, 3, 12)
  - [ ] Create `js/model-loader.ts`
  - [ ] Implement `loadModelChunks(manifest)` function
  - [ ] Implement chunk reassembly into single ArrayBuffer
  - [ ] Add progress callback: `onProgress(loaded, total)`
  - [ ] Verify checksum after reassembly

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

### Model Manifest Format

```json
{
  "model": "gemma-3n-E4B-it-Q4_K_M",
  "version": "v1",
  "totalSize": 4540000000,
  "chunks": [
    {
      "name": "gemma-3n-E4B-it-Q4_K_M.chunk-00.bin",
      "size": 2000000000,
      "sha256": "abc123..."
    },
    {
      "name": "gemma-3n-E4B-it-Q4_K_M.chunk-01.bin",
      "size": 2000000000,
      "sha256": "def456..."
    },
    {
      "name": "gemma-3n-E4B-it-Q4_K_M.chunk-02.bin",
      "size": 540000000,
      "sha256": "ghi789..."
    }
  ]
}
```

### Model Splitting Script

```bash
#!/bin/bash
# scripts/split-model.sh
set -e

MODEL_URL="https://huggingface.co/ggml-org/gemma-3n-E4B-it-GGUF/resolve/main/gemma-3n-E4B-it-Q4_K_M.gguf"
MODEL_NAME="gemma-3n-E4B-it-Q4_K_M"
CHUNK_SIZE="2000000000"  # ~2GB
OUTPUT_DIR="models"

mkdir -p "$OUTPUT_DIR"

echo "Downloading model..."
wget -q --show-progress -O "$OUTPUT_DIR/$MODEL_NAME.gguf" "$MODEL_URL"

echo "Splitting into chunks..."
cd "$OUTPUT_DIR"
split -b "$CHUNK_SIZE" -d "$MODEL_NAME.gguf" "$MODEL_NAME.chunk-"

echo "Generating manifest..."
cat > manifest.json << EOF
{
  "model": "$MODEL_NAME",
  "version": "v1",
  "totalSize": $(stat -f%z "$MODEL_NAME.gguf" 2>/dev/null || stat -c%s "$MODEL_NAME.gguf"),
  "chunks": [
EOF

for chunk in $MODEL_NAME.chunk-*; do
  SIZE=$(stat -f%z "$chunk" 2>/dev/null || stat -c%s "$chunk")
  SHA=$(sha256sum "$chunk" | cut -d' ' -f1)
  echo "    {\"name\": \"$chunk\", \"size\": $SIZE, \"sha256\": \"$SHA\"}," >> manifest.json
done

# Remove trailing comma and close JSON
sed -i '$ s/,$//' manifest.json
echo "  ]" >> manifest.json
echo "}" >> manifest.json

echo "Done! Chunks in $OUTPUT_DIR/"
ls -lh "$OUTPUT_DIR"
```

### Model Loader

```typescript
// js/model-loader.ts

export interface ModelManifest {
  model: string;
  version: string;
  totalSize: number;
  chunks: ChunkInfo[];
}

export interface ChunkInfo {
  name: string;
  size: number;
  sha256: string;
}

export type ProgressCallback = (loaded: number, total: number) => void;

export async function loadModelChunks(
  basePath: string,
  manifest: ModelManifest,
  onProgress?: ProgressCallback
): Promise<Uint8Array> {
  const totalSize = manifest.totalSize;
  let loadedSize = 0;

  // Load all chunks
  const chunkBuffers: ArrayBuffer[] = [];

  for (const chunk of manifest.chunks) {
    const response = await fetch(`${basePath}/${chunk.name}`);
    if (!response.ok) {
      throw new Error(`Failed to load chunk: ${chunk.name}`);
    }

    const buffer = await response.arrayBuffer();
    chunkBuffers.push(buffer);

    loadedSize += chunk.size;
    onProgress?.(loadedSize, totalSize);
  }

  // Reassemble into single buffer
  const combined = new Uint8Array(totalSize);
  let offset = 0;

  for (const buffer of chunkBuffers) {
    combined.set(new Uint8Array(buffer), offset);
    offset += buffer.byteLength;
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

- [ ] Model split script creates chunks <2GB
- [ ] `model-manifest.json` generated with checksums
- [ ] `loadModelChunks()` reassembles model correctly
- [ ] IndexedDB cache stores/retrieves model
- [ ] Cache hit skips download (verified in DevTools)
- [ ] Version change triggers re-download
- [ ] Corrupted cache recovery works
- [ ] Progress callback fires during load

## Risk and Compatibility Check

**Primary Risk:** IndexedDB storage limits (~50MB-2GB depending on browser)

**Mitigation:**
- Document browser storage limits
- Provide `clearCache()` for manual cleanup
- Consider compression or partial caching in future

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
