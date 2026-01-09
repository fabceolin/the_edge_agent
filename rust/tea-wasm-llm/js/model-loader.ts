/**
 * TEA WASM LLM - Model Loader Module
 *
 * Handles loading of GGUF model files with progress tracking and checksum verification.
 * Designed for Phi-4-mini Q3_K_S (~1.9GB) which fits GitHub's 2GB limit as a single file.
 *
 * Note: No chunking required - this is a simplified implementation compared to
 * the original Gemma 3n E4B (4.54GB) plan.
 */

import {
  isModelCacheAvailable,
  getCachedModel,
  checkStorageCapacity,
  cacheModel,
} from './model-cache';

/**
 * Model manifest describing the model file and metadata
 */
export interface ModelManifest {
  /** Model name/identifier */
  model: string;

  /** Version string for cache invalidation */
  version: string;

  /** Total file size in bytes */
  totalSize: number;

  /** Filename (single file, no chunks) */
  file: string;

  /** SHA256 checksum for verification */
  sha256: string;

  /** Optional description */
  description?: string;

  /** Optional source URL */
  source?: string;

  /** Optional download timestamp */
  downloadedAt?: string;
}

/**
 * Progress callback type
 * @param loaded - Bytes loaded so far
 * @param total - Total bytes to load
 */
export type ProgressCallback = (loaded: number, total: number) => void;

/**
 * Model loader options
 */
export interface LoadModelOptions {
  /** Progress callback for tracking download */
  onProgress?: ProgressCallback;

  /** Skip checksum verification (not recommended for production) */
  skipChecksum?: boolean;

  /** Request timeout in milliseconds (default: 0 = no timeout) */
  timeout?: number;
}

/**
 * Load a model file from a URL with progress tracking
 *
 * @param basePath - Base path/URL where the model file is located
 * @param manifest - Model manifest describing the file
 * @param options - Loading options
 * @returns Promise resolving to the model data as Uint8Array
 *
 * @example
 * ```typescript
 * const manifest = await fetchManifest('./models');
 * const modelData = await loadModel('./models', manifest, {
 *   onProgress: (loaded, total) => {
 *     console.log(`Loading: ${Math.round(loaded / total * 100)}%`);
 *   }
 * });
 * ```
 */
export async function loadModel(
  basePath: string,
  manifest: ModelManifest,
  options: LoadModelOptions = {}
): Promise<Uint8Array> {
  const { onProgress, skipChecksum = false, timeout = 0 } = options;
  const modelUrl = `${basePath}/${manifest.file}`;

  // Create abort controller for timeout
  const controller = new AbortController();
  let timeoutId: ReturnType<typeof setTimeout> | undefined;

  if (timeout > 0) {
    timeoutId = setTimeout(() => controller.abort(), timeout);
  }

  try {
    const response = await fetch(modelUrl, {
      signal: controller.signal,
    });

    if (!response.ok) {
      throw new Error(
        `Failed to load model: ${manifest.file} (HTTP ${response.status})`
      );
    }

    // Stream the response for progress tracking
    const reader = response.body?.getReader();
    if (!reader) {
      throw new Error('Response body is not readable');
    }

    const chunks: Uint8Array[] = [];
    let loadedSize = 0;

    // eslint-disable-next-line no-constant-condition
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;

      chunks.push(value);
      loadedSize += value.length;

      // Report progress
      onProgress?.(loadedSize, manifest.totalSize);
    }

    // Combine chunks into single buffer
    const combined = new Uint8Array(loadedSize);
    let offset = 0;
    for (const chunk of chunks) {
      combined.set(chunk, offset);
      offset += chunk.length;
    }

    // Verify checksum if not skipped
    if (!skipChecksum && manifest.sha256) {
      const isValid = await verifyChecksum(combined, manifest.sha256);
      if (!isValid) {
        throw new Error(
          `Checksum verification failed for ${manifest.file}. ` +
          'The file may be corrupted or incomplete.'
        );
      }
    }

    return combined;
  } finally {
    if (timeoutId !== undefined) {
      clearTimeout(timeoutId);
    }
  }
}

/**
 * Verify SHA256 checksum of data
 *
 * @param data - Data to verify
 * @param expectedSha256 - Expected SHA256 hash (lowercase hex string)
 * @returns Promise resolving to true if checksum matches
 */
export async function verifyChecksum(
  data: Uint8Array,
  expectedSha256: string
): Promise<boolean> {
  // Use slice to get a copy that's definitely an ArrayBuffer
  const buffer = data.buffer.slice(data.byteOffset, data.byteOffset + data.byteLength) as ArrayBuffer;
  const hashBuffer = await crypto.subtle.digest('SHA-256', buffer);
  const hashArray = Array.from(new Uint8Array(hashBuffer));
  const hashHex = hashArray.map((b) => b.toString(16).padStart(2, '0')).join('');
  return hashHex.toLowerCase() === expectedSha256.toLowerCase();
}

/**
 * Calculate SHA256 hash of data
 *
 * @param data - Data to hash
 * @returns Promise resolving to hex string of SHA256 hash
 */
export async function calculateChecksum(data: Uint8Array): Promise<string> {
  // Use slice to get a copy that's definitely an ArrayBuffer
  const buffer = data.buffer.slice(data.byteOffset, data.byteOffset + data.byteLength) as ArrayBuffer;
  const hashBuffer = await crypto.subtle.digest('SHA-256', buffer);
  const hashArray = Array.from(new Uint8Array(hashBuffer));
  return hashArray.map((b) => b.toString(16).padStart(2, '0')).join('');
}

/**
 * Fetch and parse a model manifest file
 *
 * @param basePath - Base path/URL where manifest.json is located
 * @param manifestFileName - Manifest filename (default: 'model-manifest.json')
 * @returns Promise resolving to parsed ModelManifest
 */
export async function fetchManifest(
  basePath: string,
  manifestFileName: string = 'model-manifest.json'
): Promise<ModelManifest> {
  const manifestUrl = `${basePath}/${manifestFileName}`;
  const response = await fetch(manifestUrl);

  if (!response.ok) {
    throw new Error(
      `Failed to load manifest: ${manifestFileName} (HTTP ${response.status})`
    );
  }

  const manifest: ModelManifest = await response.json();

  // Validate required fields
  if (!manifest.model || !manifest.version || !manifest.file) {
    throw new Error(
      'Invalid manifest: missing required fields (model, version, file)'
    );
  }

  if (typeof manifest.totalSize !== 'number' || manifest.totalSize <= 0) {
    throw new Error(
      'Invalid manifest: totalSize must be a positive number'
    );
  }

  return manifest;
}

/**
 * Format bytes for human-readable display
 *
 * @param bytes - Number of bytes
 * @returns Human-readable string (e.g., "1.9 GB")
 */
export function formatBytes(bytes: number): string {
  const units = ['B', 'KB', 'MB', 'GB', 'TB'];
  let value = bytes;
  let unitIndex = 0;

  while (value >= 1024 && unitIndex < units.length - 1) {
    value /= 1024;
    unitIndex++;
  }

  return `${value.toFixed(unitIndex > 0 ? 1 : 0)} ${units[unitIndex]}`;
}

/**
 * Configuration for loading bundled model
 */
export interface BundledModelConfig {
  /** Base path where model files are located (default: './models') */
  modelBasePath?: string;

  /** Manifest filename (default: 'model-manifest.json') */
  manifestFileName?: string;

  /** Use IndexedDB cache (default: true) */
  useCache?: boolean;

  /** Progress callback during download */
  onProgress?: ProgressCallback;

  /** Skip checksum verification (not recommended) */
  skipChecksum?: boolean;

  /** Request timeout in milliseconds */
  timeout?: number;

  /** Verbose logging */
  verbose?: boolean;
}

/**
 * Load a bundled model with IndexedDB caching
 *
 * This function:
 * 1. Loads the manifest file from the specified path
 * 2. Checks if the model is cached (by version)
 * 3. If cached: returns the cached model immediately
 * 4. If not cached: downloads the model, caches it, returns it
 *
 * @param config - Loading configuration options
 * @returns Promise resolving to the model data as Uint8Array
 *
 * @example
 * ```typescript
 * const modelData = await loadBundledModel({
 *   modelBasePath: './models',
 *   onProgress: (loaded, total) => {
 *     const percent = Math.round(loaded / total * 100);
 *     console.log(`Loading: ${percent}%`);
 *   }
 * });
 * ```
 */
export async function loadBundledModel(
  config: BundledModelConfig = {}
): Promise<Uint8Array> {
  const {
    modelBasePath = './models',
    manifestFileName = 'model-manifest.json',
    useCache = true,
    onProgress,
    skipChecksum = false,
    timeout = 0,
    verbose = false,
  } = config;

  const log = (msg: string) => {
    if (verbose) console.log(`[TEA-LLM] ${msg}`);
  };

  // Load manifest
  log(`Loading manifest from ${modelBasePath}/${manifestFileName}`);
  const manifest = await fetchManifest(modelBasePath, manifestFileName);
  log(`Manifest loaded: ${manifest.model} v${manifest.version} (${formatBytes(manifest.totalSize)})`);

  // Check cache first
  if (useCache) {
    const cacheAvailable = await isModelCacheAvailable();
    if (!cacheAvailable) {
      log('IndexedDB not available, skipping cache');
    } else {
      log(`Checking cache for version: ${manifest.version}`);
      const cached = await getCachedModel(manifest.version);

      if (cached) {
        log('Model loaded from cache (cache hit)');
        return cached;
      }

      log('Cache miss, will download model');
    }
  }

  // Load from network
  log(`Downloading model: ${manifest.file}`);
  const modelData = await loadModel(modelBasePath, manifest, {
    onProgress,
    skipChecksum,
    timeout,
  });
  log(`Download complete: ${formatBytes(modelData.byteLength)}`);

  // Cache for next time
  if (useCache) {
    const cacheAvailable = await isModelCacheAvailable();
    if (cacheAvailable) {
      // Check storage capacity first
      const { canCache, reason } = await checkStorageCapacity(modelData.byteLength);

      if (canCache) {
        log('Caching model for future use');
        try {
          await cacheModel(manifest.version, modelData, manifest.model);
          log('Model cached successfully');
        } catch (e) {
          log(`Cache write failed (non-fatal): ${e}`);
        }
      } else {
        log(`Skipping cache: ${reason}`);
      }
    }
  }

  return modelData;
}

/**
 * Load a bundled model with automatic corrupted cache recovery
 *
 * If loading fails for any reason (including corrupted cache),
 * this function clears the cache and retries the download.
 *
 * @param config - Loading configuration options
 * @returns Promise resolving to the model data as Uint8Array
 */
export async function loadBundledModelSafe(
  config: BundledModelConfig = {}
): Promise<Uint8Array> {
  const { clearCache } = await import('./model-cache');
  const verbose = config.verbose ?? false;
  const log = (msg: string) => {
    if (verbose) console.log(`[TEA-LLM] ${msg}`);
  };

  try {
    return await loadBundledModel(config);
  } catch (e) {
    log(`Model load failed, clearing cache and retrying: ${e}`);
    await clearCache();
    return await loadBundledModel({ ...config, useCache: false });
  }
}
