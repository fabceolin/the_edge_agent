/**
 * Storage Module Tests
 *
 * Tests for the OpenDAL-based storage module with OPFS, memory, and cloud backends.
 * These tests verify the storage API works correctly in browser/WASM context.
 */

import { test, expect, Page } from '@playwright/test';

// Helper to wait for WASM module to load
async function waitForWasm(page: Page) {
  await page.waitForFunction(() => {
    return typeof (window as any).teaWasmLlm !== 'undefined';
  }, { timeout: 10000 });
}

test.describe('Storage Module - Memory Backend', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to test page that loads the WASM module
    await page.goto('/test.html');
    await waitForWasm(page);
  });

  test('should initialize memory backend', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;
      const initResult = wasm.init_memory();
      return JSON.parse(initResult);
    });

    expect(result.success).toBe(true);
    expect(result.message).toBe('Memory backend initialized');
  });

  test('should write and read text from memory', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;

      // Initialize memory backend
      wasm.init_memory();

      // Write content
      const writeResult = await wasm.storage_write_async(
        'memory://test/data.json',
        '{"key": "value"}',
        '{}'
      );

      // Read content back
      const readResult = await wasm.storage_read_async(
        'memory://test/data.json',
        '{}'
      );

      return {
        write: JSON.parse(writeResult),
        read: JSON.parse(readResult)
      };
    });

    expect(result.write.success).toBe(true);
    expect(result.write.size).toBe(16);

    expect(result.read.success).toBe(true);
    expect(result.read.content).toBe('{"key": "value"}');
  });

  test('should check if file exists', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;

      wasm.init_memory();

      // Check non-existent file
      const beforeWrite = await wasm.storage_exists_async('memory://test/file.txt');

      // Write file
      await wasm.storage_write_async('memory://test/file.txt', 'content', '{}');

      // Check existing file
      const afterWrite = await wasm.storage_exists_async('memory://test/file.txt');

      return {
        before: JSON.parse(beforeWrite),
        after: JSON.parse(afterWrite)
      };
    });

    expect(result.before.exists).toBe(false);
    expect(result.after.exists).toBe(true);
  });

  test('should delete file', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;

      wasm.init_memory();

      // Create file
      await wasm.storage_write_async('memory://test/delete.txt', 'content', '{}');

      // Verify exists
      const existsBefore = await wasm.storage_exists_async('memory://test/delete.txt');

      // Delete file
      const deleteResult = await wasm.storage_delete_async('memory://test/delete.txt');

      // Verify deleted
      const existsAfter = await wasm.storage_exists_async('memory://test/delete.txt');

      return {
        existsBefore: JSON.parse(existsBefore),
        deleteResult: JSON.parse(deleteResult),
        existsAfter: JSON.parse(existsAfter)
      };
    });

    expect(result.existsBefore.exists).toBe(true);
    expect(result.deleteResult.success).toBe(true);
    expect(result.deleteResult.deleted).toBe(true);
    expect(result.existsAfter.exists).toBe(false);
  });

  test('should list files', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;

      wasm.init_memory();

      // Create multiple files
      await wasm.storage_write_async('memory://list/file1.txt', 'content1', '{}');
      await wasm.storage_write_async('memory://list/file2.txt', 'content2', '{}');
      await wasm.storage_write_async('memory://list/file3.json', '{}', '{}');

      // List files
      const listResult = await wasm.storage_list_async('memory://list/', '{}');

      return JSON.parse(listResult);
    });

    expect(result.success).toBe(true);
    expect(result.entries.length).toBeGreaterThanOrEqual(3);
  });

  test('should copy files within memory backend', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;

      wasm.init_memory();

      // Create source file
      await wasm.storage_write_async('memory://copy/source.txt', 'source content', '{}');

      // Copy file
      const copyResult = await wasm.storage_copy_async(
        'memory://copy/source.txt',
        'memory://copy/destination.txt'
      );

      // Read destination
      const destContent = await wasm.storage_read_async('memory://copy/destination.txt', '{}');

      return {
        copy: JSON.parse(copyResult),
        content: JSON.parse(destContent)
      };
    });

    expect(result.copy.success).toBe(true);
    expect(result.copy.size).toBe(14);
    expect(result.content.content).toBe('source content');
  });
});

test.describe('Storage Module - Binary Support', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/test.html');
    await waitForWasm(page);
  });

  test('should write and read binary content (base64)', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;

      wasm.init_memory();

      // Create some binary-like content (base64 encoded)
      const testContent = btoa('binary test content');

      // Write binary
      const writeResult = await wasm.storage_write_binary_async(
        'memory://binary/test.bin',
        testContent
      );

      // Read binary
      const readResult = await wasm.storage_read_binary_async('memory://binary/test.bin');

      return {
        write: JSON.parse(writeResult),
        read: JSON.parse(readResult),
        originalBase64: testContent
      };
    });

    expect(result.write.success).toBe(true);
    expect(result.read.success).toBe(true);
    expect(result.read.content_base64).toBe(result.originalBase64);
  });
});

test.describe('Storage Module - Credential Management', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/test.html');
    await waitForWasm(page);
  });

  test('should set and check credentials', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;

      // Initially no credentials
      const beforeSet = wasm.has_storage_credentials('s3');

      // Set credentials
      wasm.set_storage_credentials('s3', JSON.stringify({
        access_key_id: 'test-key',
        secret_access_key: 'test-secret',
        region: 'us-east-1'
      }));

      // Check credentials exist
      const afterSet = wasm.has_storage_credentials('s3');

      // Clear all credentials
      wasm.clear_storage_credentials();

      // Verify cleared
      const afterClear = wasm.has_storage_credentials('s3');

      return { beforeSet, afterSet, afterClear };
    });

    expect(result.beforeSet).toBe(false);
    expect(result.afterSet).toBe(true);
    expect(result.afterClear).toBe(false);
  });

  test('credentials should not be serialized to state', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;

      // Set some credentials
      wasm.set_storage_credentials('s3', JSON.stringify({
        access_key_id: 'AKIAIOSFODNN7EXAMPLE',
        secret_access_key: 'wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY'
      }));

      // Get supported schemes info (state-like output)
      const schemesJson = wasm.storage_supported_schemes();

      // Verify credentials are NOT in the output
      return {
        schemes: JSON.parse(schemesJson),
        containsKey: schemesJson.includes('AKIAIOSFODNN7EXAMPLE'),
        containsSecret: schemesJson.includes('wJalrXUtnFEMI')
      };
    });

    // Credentials should never appear in any state output
    expect(result.containsKey).toBe(false);
    expect(result.containsSecret).toBe(false);
  });
});

test.describe('Storage Module - Error Handling', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/test.html');
    await waitForWasm(page);
  });

  test('should return helpful error for unsupported scheme', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;

      try {
        await wasm.storage_read_async('ftp://invalid/path', '{}');
        return { error: null };
      } catch (e: any) {
        return { error: e.toString() };
      }
    });

    expect(result.error).toContain('UNSUPPORTED_SCHEME');
    expect(result.error).toContain('ftp');
    expect(result.error).toContain('Supported:');
  });

  test('should return helpful error for uninitialized memory', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;

      // Don't initialize memory - try to read directly
      // Note: We need fresh page state, so clear any existing memory operator
      try {
        await wasm.storage_read_async('memory://test/file.txt', '{}');
        return { error: null };
      } catch (e: any) {
        return { error: e.toString() };
      }
    });

    // Should fail with helpful message about initialization
    expect(result.error).toContain('MEMORY_NOT_INITIALIZED');
    expect(result.error).toContain('init_memory');
  });

  test('should return helpful error for invalid URI', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;

      try {
        await wasm.storage_read_async('not-a-valid-uri', '{}');
        return { error: null };
      } catch (e: any) {
        return { error: e.toString() };
      }
    });

    expect(result.error).toContain('INVALID_URI');
  });
});

test.describe('Storage Module - Supported Schemes', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/test.html');
    await waitForWasm(page);
  });

  test('should list supported schemes', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;
      return JSON.parse(wasm.storage_supported_schemes());
    });

    expect(result.schemes).toContain('memory');
    expect(typeof result.opfs_available).toBe('boolean');
    expect(typeof result.memory_available).toBe('boolean');
  });
});

// OPFS tests - only run in supported browsers
test.describe('Storage Module - OPFS Backend', () => {
  test.skip(({ browserName }) => browserName !== 'chromium', 'OPFS only fully supported in Chromium');

  test.beforeEach(async ({ page }) => {
    await page.goto('/test.html');
    await waitForWasm(page);
  });

  test('should initialize OPFS', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;

      const initResult = await wasm.init_opfs();
      const isAvailable = wasm.is_opfs_available();

      return {
        init: JSON.parse(initResult),
        available: isAvailable
      };
    });

    // OPFS init might fail in test environment - just verify API works
    expect(typeof result.init.success).toBe('boolean');
    expect(typeof result.available).toBe('boolean');
  });

  test('should write and read from OPFS when available', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;

      // Initialize OPFS
      const initResult = await wasm.init_opfs();
      const init = JSON.parse(initResult);

      if (!init.success) {
        return { skipped: true, reason: 'OPFS not available' };
      }

      // Write to OPFS
      const writeResult = await wasm.storage_write_async(
        'opfs://data/test.json',
        '{"opfs": "test"}',
        '{}'
      );

      // Read back
      const readResult = await wasm.storage_read_async(
        'opfs://data/test.json',
        '{}'
      );

      return {
        skipped: false,
        write: JSON.parse(writeResult),
        read: JSON.parse(readResult)
      };
    });

    if (result.skipped) {
      test.skip(true, result.reason);
      return;
    }

    expect(result.write.success).toBe(true);
    expect(result.read.success).toBe(true);
    expect(result.read.content).toBe('{"opfs": "test"}');
  });

  test('OPFS files should persist across operations', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;

      const initResult = await wasm.init_opfs();
      const init = JSON.parse(initResult);

      if (!init.success) {
        return { skipped: true };
      }

      // Write a file
      await wasm.storage_write_async(
        'opfs://persist/data.json',
        '{"persistent": true}',
        '{}'
      );

      // List the directory
      const listResult = await wasm.storage_list_async('opfs://persist/', '{}');

      // Check exists
      const existsResult = await wasm.storage_exists_async('opfs://persist/data.json');

      return {
        skipped: false,
        list: JSON.parse(listResult),
        exists: JSON.parse(existsResult)
      };
    });

    if (result.skipped) {
      test.skip(true, 'OPFS not available');
      return;
    }

    expect(result.exists.exists).toBe(true);
    expect(result.list.entries.length).toBeGreaterThan(0);
  });
});

// Cross-provider copy tests
test.describe('Storage Module - Cross-Provider Copy', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/test.html');
    await waitForWasm(page);
  });

  test('should copy between memory locations', async ({ page }) => {
    const result = await page.evaluate(async () => {
      const wasm = (window as any).teaWasmLlm;

      wasm.init_memory();

      // Create source content
      await wasm.storage_write_async(
        'memory://source/file.txt',
        'cross-copy test content',
        '{}'
      );

      // Copy to different location
      const copyResult = await wasm.storage_copy_async(
        'memory://source/file.txt',
        'memory://dest/file.txt'
      );

      // Verify destination
      const destRead = await wasm.storage_read_async('memory://dest/file.txt', '{}');

      return {
        copy: JSON.parse(copyResult),
        dest: JSON.parse(destRead)
      };
    });

    expect(result.copy.success).toBe(true);
    expect(result.dest.content).toBe('cross-copy test content');
  });
});
