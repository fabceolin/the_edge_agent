/**
 * TEA WASM LLM - Model Cache Module
 *
 * IndexedDB-based caching for GGUF model files.
 * Stores models after first load to avoid re-downloading on subsequent visits.
 *
 * Features:
 * - Version-based cache invalidation
 * - Automatic cache clearing on corruption
 * - Quota detection and error handling
 *
 * Browser Compatibility Notes:
 * - Chrome/Firefox: ~2GB IndexedDB limit (sufficient for Phi-4-mini 1.9GB)
 * - Safari: 1GB hard limit - model cannot be cached, will fall back to streaming
 */

/**
 * Database constants
 */
const DB_NAME = 'tea-llm-cache';
const DB_VERSION = 1;
const STORE_NAME = 'models';

/**
 * Cached model entry structure
 */
export interface CachedModel {
  /** Version string for cache invalidation */
  version: string;

  /** Model data as Uint8Array */
  data: Uint8Array;

  /** Timestamp when cached (ms since epoch) */
  timestamp: number;

  /** Original model name */
  modelName: string;

  /** Size in bytes (for display/verification) */
  size: number;
}

/**
 * Cache statistics
 */
export interface CacheStats {
  /** Whether the cache database exists and is accessible */
  available: boolean;

  /** Number of cached models */
  modelCount: number;

  /** Total size of cached models in bytes */
  totalSize: number;

  /** Estimated available quota in bytes (if supported) */
  estimatedQuota?: number;

  /** Estimated used quota in bytes (if supported) */
  estimatedUsage?: number;
}

/**
 * Open the IndexedDB database
 * Creates the object store on first open
 */
async function openDB(): Promise<IDBDatabase> {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open(DB_NAME, DB_VERSION);

    request.onerror = () => {
      reject(new Error(`Failed to open cache database: ${request.error?.message}`));
    };

    request.onsuccess = () => {
      resolve(request.result);
    };

    request.onupgradeneeded = (event) => {
      const db = (event.target as IDBOpenDBRequest).result;

      // Create object store if it doesn't exist
      if (!db.objectStoreNames.contains(STORE_NAME)) {
        const store = db.createObjectStore(STORE_NAME, { keyPath: 'version' });
        // Index by modelName for potential future queries
        store.createIndex('modelName', 'modelName', { unique: false });
        store.createIndex('timestamp', 'timestamp', { unique: false });
      }
    };
  });
}

/**
 * Check if the model cache is available in this browser
 *
 * @returns Promise resolving to true if IndexedDB is available
 */
export async function isModelCacheAvailable(): Promise<boolean> {
  if (typeof indexedDB === 'undefined') {
    return false;
  }

  try {
    const db = await openDB();
    db.close();
    return true;
  } catch {
    return false;
  }
}

/**
 * Get a cached model by version
 *
 * @param version - Version string to look up
 * @returns Promise resolving to model data or null if not cached
 */
export async function getCachedModel(version: string): Promise<Uint8Array | null> {
  try {
    const db = await openDB();

    return new Promise((resolve, reject) => {
      const tx = db.transaction(STORE_NAME, 'readonly');
      const store = tx.objectStore(STORE_NAME);
      const request = store.get(version);

      request.onerror = () => {
        db.close();
        reject(new Error(`Failed to read from cache: ${request.error?.message}`));
      };

      request.onsuccess = () => {
        db.close();
        const result = request.result as CachedModel | undefined;
        resolve(result?.data || null);
      };
    });
  } catch (e) {
    console.warn('[TEA-LLM-Cache] Cache read failed:', e);
    return null;
  }
}

/**
 * Get full cached model entry with metadata
 *
 * @param version - Version string to look up
 * @returns Promise resolving to full CachedModel entry or null
 */
export async function getCachedModelEntry(version: string): Promise<CachedModel | null> {
  try {
    const db = await openDB();

    return new Promise((resolve, reject) => {
      const tx = db.transaction(STORE_NAME, 'readonly');
      const store = tx.objectStore(STORE_NAME);
      const request = store.get(version);

      request.onerror = () => {
        db.close();
        reject(new Error(`Failed to read from cache: ${request.error?.message}`));
      };

      request.onsuccess = () => {
        db.close();
        // IndexedDB returns undefined for non-existent keys, normalize to null
        resolve(request.result ?? null);
      };
    });
  } catch (e) {
    console.warn('[TEA-LLM-Cache] Cache read failed:', e);
    return null;
  }
}

/**
 * Store a model in the cache
 *
 * @param version - Version string (cache key)
 * @param data - Model data as Uint8Array
 * @param modelName - Model name for metadata
 */
export async function cacheModel(
  version: string,
  data: Uint8Array,
  modelName: string = 'unknown'
): Promise<void> {
  try {
    const db = await openDB();

    return new Promise((resolve, reject) => {
      const tx = db.transaction(STORE_NAME, 'readwrite');
      const store = tx.objectStore(STORE_NAME);

      const entry: CachedModel = {
        version,
        data,
        timestamp: Date.now(),
        modelName,
        size: data.byteLength,
      };

      const request = store.put(entry);

      request.onerror = () => {
        db.close();
        // Check for quota exceeded
        if (request.error?.name === 'QuotaExceededError') {
          reject(new Error(
            'Storage quota exceeded. The model is too large for this browser\'s IndexedDB limit.'
          ));
        } else {
          reject(new Error(`Failed to write to cache: ${request.error?.message}`));
        }
      };

      request.onsuccess = () => {
        db.close();
        resolve();
      };
    });
  } catch (e) {
    console.warn('[TEA-LLM-Cache] Cache write failed:', e);
    // Don't throw - caching is optional
  }
}

/**
 * Clear all cached models
 */
export async function clearCache(): Promise<void> {
  try {
    const db = await openDB();

    return new Promise((resolve, reject) => {
      const tx = db.transaction(STORE_NAME, 'readwrite');
      const store = tx.objectStore(STORE_NAME);
      const request = store.clear();

      request.onerror = () => {
        db.close();
        reject(new Error(`Failed to clear cache: ${request.error?.message}`));
      };

      request.onsuccess = () => {
        db.close();
        resolve();
      };
    });
  } catch (e) {
    console.warn('[TEA-LLM-Cache] Cache clear failed:', e);
    // Don't throw - clearing is optional
  }
}

/**
 * Delete a specific cached model by version
 *
 * @param version - Version string to delete
 */
export async function deleteCachedModel(version: string): Promise<void> {
  try {
    const db = await openDB();

    return new Promise((resolve, reject) => {
      const tx = db.transaction(STORE_NAME, 'readwrite');
      const store = tx.objectStore(STORE_NAME);
      const request = store.delete(version);

      request.onerror = () => {
        db.close();
        reject(new Error(`Failed to delete from cache: ${request.error?.message}`));
      };

      request.onsuccess = () => {
        db.close();
        resolve();
      };
    });
  } catch (e) {
    console.warn('[TEA-LLM-Cache] Cache delete failed:', e);
  }
}

/**
 * Check if a specific version is cached
 *
 * @param version - Version string to check
 * @returns Promise resolving to true if cached
 */
export async function isCached(version: string): Promise<boolean> {
  const entry = await getCachedModelEntry(version);
  return entry !== null;
}

/**
 * Get cache statistics
 *
 * @returns Promise resolving to cache statistics
 */
export async function getCacheStats(): Promise<CacheStats> {
  const stats: CacheStats = {
    available: false,
    modelCount: 0,
    totalSize: 0,
  };

  // Check storage quota if available
  if ('storage' in navigator && 'estimate' in navigator.storage) {
    try {
      const estimate = await navigator.storage.estimate();
      stats.estimatedQuota = estimate.quota;
      stats.estimatedUsage = estimate.usage;
    } catch {
      // Quota estimation not available
    }
  }

  try {
    const db = await openDB();
    stats.available = true;

    return new Promise((resolve) => {
      const tx = db.transaction(STORE_NAME, 'readonly');
      const store = tx.objectStore(STORE_NAME);
      const request = store.getAll();

      request.onsuccess = () => {
        db.close();
        const entries = request.result as CachedModel[];
        stats.modelCount = entries.length;
        stats.totalSize = entries.reduce((sum, entry) => sum + entry.size, 0);
        resolve(stats);
      };

      request.onerror = () => {
        db.close();
        resolve(stats);
      };
    });
  } catch {
    return stats;
  }
}

/**
 * List all cached model versions
 *
 * @returns Promise resolving to array of cached versions with metadata
 */
export async function listCachedModels(): Promise<Array<{
  version: string;
  modelName: string;
  size: number;
  timestamp: number;
}>> {
  try {
    const db = await openDB();

    return new Promise((resolve, reject) => {
      const tx = db.transaction(STORE_NAME, 'readonly');
      const store = tx.objectStore(STORE_NAME);
      const request = store.getAll();

      request.onerror = () => {
        db.close();
        reject(new Error(`Failed to list cache: ${request.error?.message}`));
      };

      request.onsuccess = () => {
        db.close();
        const entries = request.result as CachedModel[];
        resolve(entries.map(({ version, modelName, size, timestamp }) => ({
          version,
          modelName,
          size,
          timestamp,
        })));
      };
    });
  } catch (e) {
    console.warn('[TEA-LLM-Cache] Cache list failed:', e);
    return [];
  }
}

/**
 * Check if the model size can fit in IndexedDB
 * This is a heuristic check based on known browser limits.
 *
 * @param sizeBytes - Model size in bytes
 * @returns Object with canCache boolean and reason string
 */
export async function checkStorageCapacity(sizeBytes: number): Promise<{
  canCache: boolean;
  reason: string;
}> {
  // Safari has a hard 1GB limit
  const isSafari = /^((?!chrome|android).)*safari/i.test(navigator.userAgent);
  if (isSafari && sizeBytes > 1024 * 1024 * 1024) {
    return {
      canCache: false,
      reason: 'Safari has a 1GB IndexedDB limit. Model will be loaded via streaming without caching.',
    };
  }

  // Check storage quota if available
  if ('storage' in navigator && 'estimate' in navigator.storage) {
    try {
      const estimate = await navigator.storage.estimate();
      const available = (estimate.quota || 0) - (estimate.usage || 0);

      if (sizeBytes > available) {
        return {
          canCache: false,
          reason: `Insufficient storage. Need ${formatBytes(sizeBytes)}, have ${formatBytes(available)}.`,
        };
      }
    } catch {
      // Fall through to optimistic return
    }
  }

  return {
    canCache: true,
    reason: 'Storage capacity appears sufficient.',
  };
}

/**
 * Format bytes for human-readable display
 */
function formatBytes(bytes: number): string {
  const units = ['B', 'KB', 'MB', 'GB', 'TB'];
  let value = bytes;
  let unitIndex = 0;

  while (value >= 1024 && unitIndex < units.length - 1) {
    value /= 1024;
    unitIndex++;
  }

  return `${value.toFixed(unitIndex > 0 ? 1 : 0)} ${units[unitIndex]}`;
}
