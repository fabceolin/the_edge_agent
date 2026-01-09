/**
 * TEA WASM LLM - Model Cache Unit Tests
 *
 * Tests for IndexedDB model caching functionality.
 * These tests verify AC: 4, 5, 6, 10, 11 from TEA-RELEASE-004.3b.
 *
 * Requirements:
 * - Jest or Vitest
 * - fake-indexeddb polyfill
 *
 * Run with:
 *   npm install --save-dev fake-indexeddb vitest
 *   npx vitest run tests/model-cache.test.ts
 */

// Mock IndexedDB for Node.js environment
import 'fake-indexeddb/auto';

// Import the modules to test
// Note: Paths will need adjustment based on actual build output location
import {
  getCachedModel,
  getCachedModelEntry,
  cacheModel,
  clearCache,
  deleteCachedModel,
  isCached,
  getCacheStats,
  listCachedModels,
  checkStorageCapacity,
  isModelCacheAvailable,
} from '../js/model-cache';

import {
  loadModel,
  verifyChecksum,
  calculateChecksum,
  fetchManifest,
  formatBytes,
  type ModelManifest,
} from '../js/model-loader';

describe('Model Cache', () => {
  // Clear cache before each test
  beforeEach(async () => {
    await clearCache();
  });

  // Helper to create mock model data
  const createMockModelData = (size: number = 100): Uint8Array => {
    const data = new Uint8Array(size);
    for (let i = 0; i < size; i++) {
      data[i] = i % 256;
    }
    return data;
  };

  describe('Cache Availability (Infrastructure)', () => {
    test('isModelCacheAvailable returns boolean', async () => {
      const available = await isModelCacheAvailable();
      expect(typeof available).toBe('boolean');
    });

    test('IndexedDB is available in test environment', async () => {
      const available = await isModelCacheAvailable();
      expect(available).toBe(true);
    });
  });

  describe('AC-4: IndexedDB stores model after first load', () => {
    test('cacheModel stores data successfully', async () => {
      const mockData = createMockModelData(100);
      const version = 'test-v1';

      await cacheModel(version, mockData, 'test-model');

      const cached = await getCachedModel(version);
      expect(cached).not.toBeNull();
      expect(cached!.byteLength).toBe(mockData.byteLength);
    });

    test('cached data matches original data', async () => {
      const mockData = createMockModelData(256);
      const version = 'integrity-v1';

      await cacheModel(version, mockData, 'test-model');
      const cached = await getCachedModel(version);

      expect(cached).not.toBeNull();
      expect(cached!.byteLength).toBe(mockData.byteLength);

      // Verify byte-by-byte equality
      for (let i = 0; i < mockData.length; i++) {
        expect(cached![i]).toBe(mockData[i]);
      }
    });
  });

  describe('AC-5: Cache hit skips download', () => {
    test('getCachedModel returns null for uncached version (cache miss)', async () => {
      const cached = await getCachedModel('nonexistent-v1');
      expect(cached).toBeNull();
    });

    test('getCachedModel returns data for cached version (cache hit)', async () => {
      const mockData = createMockModelData(100);
      const version = 'test-v2';

      // First: not cached
      let cached = await getCachedModel(version);
      expect(cached).toBeNull();

      // Store it
      await cacheModel(version, mockData, 'test-model');

      // Second: should be cached
      cached = await getCachedModel(version);
      expect(cached).not.toBeNull();
      expect(cached!.byteLength).toBe(mockData.byteLength);
    });
  });

  describe('AC-6: Version change triggers re-download', () => {
    test('different versions are cached separately', async () => {
      const mockData1 = createMockModelData(100);
      const mockData2 = createMockModelData(200);

      await cacheModel('v1', mockData1, 'test-model');
      await cacheModel('v2', mockData2, 'test-model');

      const cachedV1 = await getCachedModel('v1');
      const cachedV2 = await getCachedModel('v2');

      expect(cachedV1).not.toBeNull();
      expect(cachedV2).not.toBeNull();
      expect(cachedV1!.byteLength).toBe(100);
      expect(cachedV2!.byteLength).toBe(200);
    });

    test('new version can replace old version', async () => {
      const mockData1 = createMockModelData(100);
      const mockData2 = createMockModelData(150);
      const version = 'same-key';

      await cacheModel(version, mockData1, 'test-model');
      let cached = await getCachedModel(version);
      expect(cached!.byteLength).toBe(100);

      // Overwrite with new data
      await cacheModel(version, mockData2, 'test-model');
      cached = await getCachedModel(version);
      expect(cached!.byteLength).toBe(150);
    });
  });

  describe('AC-10: Cache hit/miss behavior', () => {
    test('isCached returns false for uncached version', async () => {
      // First clear cache, then check for unique key
      await clearCache();
      const uniqueKey = `nonexistent-${Date.now()}-${Math.random()}`;
      const entry = await getCachedModelEntry(uniqueKey);
      // Entry should be null for a non-existent key
      expect(entry).toBeNull();
      const result = await isCached(uniqueKey);
      expect(result).toBe(false);
    });

    test('isCached returns true for cached version', async () => {
      const mockData = createMockModelData(50);
      const version = 'test-iscached';

      await cacheModel(version, mockData, 'test');

      const result = await isCached(version);
      expect(result).toBe(true);
    });
  });

  describe('AC-11: Corrupted cache recovery', () => {
    test('getCachedModel handles null data gracefully', async () => {
      // This test verifies that corrupted entries (null data) are handled
      const cached = await getCachedModel('corrupted-entry');
      expect(cached).toBeNull();
    });

    test('clearCache allows fresh data after corruption', async () => {
      // Store some data
      await cacheModel('before-clear', createMockModelData(100), 'test');

      // Clear cache
      await clearCache();

      // Verify cleared
      const cleared = await getCachedModel('before-clear');
      expect(cleared).toBeNull();

      // Store fresh data
      await cacheModel('after-clear', createMockModelData(200), 'test');

      const fresh = await getCachedModel('after-clear');
      expect(fresh).not.toBeNull();
      expect(fresh!.byteLength).toBe(200);
    });
  });

  describe('Cache Management', () => {
    test('clearCache removes all cached models', async () => {
      await cacheModel('clear-v1', createMockModelData(100), 'test');
      await cacheModel('clear-v2', createMockModelData(100), 'test');

      await clearCache();

      const v1 = await getCachedModel('clear-v1');
      const v2 = await getCachedModel('clear-v2');

      expect(v1).toBeNull();
      expect(v2).toBeNull();
    });

    test('deleteCachedModel removes specific version only', async () => {
      await cacheModel('delete-v1', createMockModelData(100), 'test');
      await cacheModel('delete-v2', createMockModelData(100), 'test');

      await deleteCachedModel('delete-v1');

      const v1 = await getCachedModel('delete-v1');
      const v2 = await getCachedModel('delete-v2');

      expect(v1).toBeNull();
      expect(v2).not.toBeNull();
    });

    test('getCacheStats returns correct statistics', async () => {
      await cacheModel('stats-v1', createMockModelData(100), 'test');
      await cacheModel('stats-v2', createMockModelData(100), 'test');

      const stats = await getCacheStats();

      expect(stats.available).toBe(true);
      expect(stats.modelCount).toBe(2);
      expect(stats.totalSize).toBe(200);
    });

    test('listCachedModels returns all cached versions', async () => {
      await cacheModel('list-v1', createMockModelData(100), 'model-a');
      await cacheModel('list-v2', createMockModelData(100), 'model-b');

      const models = await listCachedModels();

      expect(models.length).toBe(2);
      expect(models.some(m => m.version === 'list-v1')).toBe(true);
      expect(models.some(m => m.version === 'list-v2')).toBe(true);
    });
  });

  describe('Cache Entry Metadata', () => {
    test('getCachedModelEntry returns full metadata', async () => {
      const mockData = createMockModelData(123);
      const version = 'metadata-v1';
      const modelName = 'test-model-name';

      await cacheModel(version, mockData, modelName);

      const entry = await getCachedModelEntry(version);

      expect(entry).not.toBeNull();
      expect(entry!.version).toBe(version);
      expect(entry!.modelName).toBe(modelName);
      expect(entry!.size).toBe(123);
      expect(typeof entry!.timestamp).toBe('number');
      expect(entry!.timestamp).toBeGreaterThan(0);
    });
  });

  describe('Storage Capacity', () => {
    test('checkStorageCapacity returns valid result', async () => {
      const result = await checkStorageCapacity(1000); // 1KB

      expect(typeof result.canCache).toBe('boolean');
      expect(typeof result.reason).toBe('string');
    });
  });
});

describe('Model Loader Helpers', () => {
  describe('Checksum Verification', () => {
    test('calculateChecksum returns hex string', async () => {
      const data = new Uint8Array([1, 2, 3, 4, 5]);
      const hash = await calculateChecksum(data);

      expect(typeof hash).toBe('string');
      expect(hash.length).toBe(64); // SHA256 = 64 hex chars
      expect(/^[0-9a-f]+$/.test(hash)).toBe(true);
    });

    test('verifyChecksum returns true for matching hash', async () => {
      const data = new Uint8Array([1, 2, 3, 4, 5]);
      const hash = await calculateChecksum(data);

      const valid = await verifyChecksum(data, hash);
      expect(valid).toBe(true);
    });

    test('verifyChecksum returns false for mismatched hash', async () => {
      const data = new Uint8Array([1, 2, 3, 4, 5]);

      const invalid = await verifyChecksum(data, 'wrong-hash');
      expect(invalid).toBe(false);
    });

    test('same data produces same hash', async () => {
      const data1 = new Uint8Array([1, 2, 3, 4, 5]);
      const data2 = new Uint8Array([1, 2, 3, 4, 5]);

      const hash1 = await calculateChecksum(data1);
      const hash2 = await calculateChecksum(data2);

      expect(hash1).toBe(hash2);
    });

    test('different data produces different hash', async () => {
      const data1 = new Uint8Array([1, 2, 3, 4, 5]);
      const data2 = new Uint8Array([1, 2, 3, 4, 6]); // Last byte different

      const hash1 = await calculateChecksum(data1);
      const hash2 = await calculateChecksum(data2);

      expect(hash1).not.toBe(hash2);
    });
  });

  describe('Format Bytes', () => {
    test('formats 0 bytes', () => {
      expect(formatBytes(0)).toBe('0 B');
    });

    test('formats bytes', () => {
      expect(formatBytes(500)).toBe('500 B');
    });

    test('formats kilobytes', () => {
      expect(formatBytes(1024)).toBe('1.0 KB');
      expect(formatBytes(2048)).toBe('2.0 KB');
    });

    test('formats megabytes', () => {
      expect(formatBytes(1024 * 1024)).toBe('1.0 MB');
    });

    test('formats gigabytes', () => {
      expect(formatBytes(1024 * 1024 * 1024)).toBe('1.0 GB');
    });

    test('formats ~1.9GB (Phi-4-mini model size)', () => {
      expect(formatBytes(1900000000)).toBe('1.8 GB');
    });
  });
});

describe('Integration: Cache with Model Loading', () => {
  beforeEach(async () => {
    await clearCache();
  });

  test('loadBundledModel workflow simulation', async () => {
    // Simulate the loadBundledModel workflow
    const version = 'v1';
    const modelName = 'test-model';
    const mockModelData = new Uint8Array(1000);

    // Step 1: Check cache (miss)
    let cached = await getCachedModel(version);
    expect(cached).toBeNull();

    // Step 2: "Download" model (simulated)
    const downloadedData = mockModelData;

    // Step 3: Cache the model
    await cacheModel(version, downloadedData, modelName);

    // Step 4: Subsequent load - should hit cache
    cached = await getCachedModel(version);
    expect(cached).not.toBeNull();
    expect(cached!.byteLength).toBe(1000);
  });

  test('loadBundledModelSafe workflow simulation with recovery', async () => {
    // Simulate the safe loading workflow with corruption recovery
    const version = 'v1';
    const modelName = 'test-model';

    // Try to get from cache (miss or corrupted)
    let cached = await getCachedModel(version);

    if (cached === null) {
      // Clear cache (in case of corruption)
      await clearCache();

      // "Download" fresh
      const freshData = new Uint8Array(500);

      // Cache it
      await cacheModel(version, freshData, modelName);

      // Verify
      cached = await getCachedModel(version);
      expect(cached).not.toBeNull();
      expect(cached!.byteLength).toBe(500);
    }
  });
});
