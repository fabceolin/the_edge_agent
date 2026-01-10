/**
 * TEA WASM LLM Playwright E2E Tests
 *
 * These tests verify:
 * - AC-6: Package loads in headless browser
 * - AC-7: Model initialization (with mock handler)
 * - AC-8: LLM workflow execution via executeLlmYaml()
 * - AC-9: IndexedDB caching functionality
 *
 * The tests use a mock LLM handler to avoid requiring actual model downloads
 * in CI. Full model tests should be run manually or on release tags.
 */

import { test, expect, Page } from '@playwright/test';

// Test page URL
const TEST_PAGE = '/tests/e2e/test-page.html';

// Helper to wait for WASM to load
async function waitForWasmLoad(page: Page): Promise<void> {
  await page.waitForFunction(
    () => (window as any).teaLlmLoaded === true,
    { timeout: 30000 }
  );
}

// Helper to initialize TEA LLM
async function initTeaLlm(page: Page): Promise<string> {
  return page.evaluate(async () => {
    return await (window as any).initTeaLlm();
  });
}

// Helper to execute YAML workflow
async function executeLlmYaml(
  page: Page,
  yaml: string,
  initialState: Record<string, unknown> = {}
): Promise<Record<string, unknown>> {
  return page.evaluate(
    async ([yamlStr, state]) => {
      return await (window as any).executeLlmYaml(yamlStr, state);
    },
    [yaml, initialState] as const
  );
}

test.describe('WASM LLM Package Loading', () => {
  test('AC-6: should load package without errors', async ({ page }) => {
    await page.goto(TEST_PAGE);

    // Wait for WASM to load
    await waitForWasmLoad(page);

    // Verify WASM loaded flag is set
    const loaded = await page.evaluate(() => (window as any).teaLlmLoaded);
    expect(loaded).toBe(true);

    // Verify no error occurred
    const error = await page.evaluate(() => (window as any).teaLlmError);
    expect(error).toBeNull();
  });

  test('AC-6: should have SharedArrayBuffer detection', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    // Check that SharedArrayBuffer detection works (result depends on browser/headers)
    const hasSharedArrayBuffer = await page.evaluate(async () => {
      const init = await import('../../pkg/tea_wasm_llm.js');
      await init.default();
      return init.has_shared_array_buffer();
    });

    // The result should be a boolean (true with COOP/COEP headers)
    expect(typeof hasSharedArrayBuffer).toBe('boolean');
  });

  test('AC-6: should report version', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    const version = await page.evaluate(async () => {
      const init = await import('../../pkg/tea_wasm_llm.js');
      await init.default();
      return init.version();
    });

    // Version should be a non-empty string
    expect(typeof version).toBe('string');
    expect(version.length).toBeGreaterThan(0);
  });
});

test.describe('Model Initialization', () => {
  test('AC-7: should initialize with mock handler', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    // Initialize TEA LLM
    const initResult = await initTeaLlm(page);
    expect(initResult).toBe('initialized');

    // Verify initialized flag
    const initialized = await page.evaluate(() => (window as any).teaLlmInitialized);
    expect(initialized).toBe(true);
  });

  test('AC-7: should have LLM handler registered after init', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);
    await initTeaLlm(page);

    // Check handler is registered
    const hasHandler = await page.evaluate(async () => {
      const init = await import('../../pkg/tea_wasm_llm.js');
      return init.has_llm_handler();
    });

    expect(hasHandler).toBe(true);
  });
});

test.describe('LLM Workflow Execution', () => {
  test('AC-8: should execute simple LLM workflow', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);
    await initTeaLlm(page);

    const yaml = `
name: test-workflow
state_schema:
  input: str
  llm_response: any

nodes:
  - name: gen
    action: llm.call
    with:
      prompt: "Say hello"
      max_tokens: 5

edges:
  - from: __start__
    to: gen
  - from: gen
    to: __end__
`;

    const result = await executeLlmYaml(page, yaml, { input: 'test' });

    // Verify result has the expected structure
    expect(result).toHaveProperty('llm_response');
    expect((result as any).llm_response).toHaveProperty('content');
  });

  test('AC-8: should handle workflow with initial state', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);
    await initTeaLlm(page);

    const yaml = `
name: stateful-workflow
state_schema:
  user_input: str
  llm_response: any

nodes:
  - name: process
    action: llm.call
    with:
      prompt: "Process this: {{ state.user_input }}"
      max_tokens: 20

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
`;

    const result = await executeLlmYaml(page, yaml, { user_input: 'hello world' });

    expect(result).toHaveProperty('llm_response');
    expect(result).toHaveProperty('user_input', 'hello world');
  });

  test('AC-8: should return error for invalid YAML', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);
    await initTeaLlm(page);

    const invalidYaml = `
not: valid: yaml: structure
    missing nodes and edges
`;

    await expect(async () => {
      await executeLlmYaml(page, invalidYaml, {});
    }).rejects.toThrow();
  });
});

test.describe('IndexedDB Caching', () => {
  test('AC-9: should support cache operations', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    // Clear any existing cache
    await page.evaluate(async () => {
      return await (window as any).clearCache();
    });

    // Check cache is empty
    const isEmpty = await page.evaluate(async () => {
      return !(await (window as any).checkCache());
    });
    expect(isEmpty).toBe(true);
  });

  test('AC-9: should persist cache across page loads', async ({ page }) => {
    // First load - initialize
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);
    await initTeaLlm(page);

    // Store something in IndexedDB manually to simulate caching
    await page.evaluate(async () => {
      return new Promise<void>((resolve, reject) => {
        const request = indexedDB.open('tea-llm-cache', 1);
        request.onupgradeneeded = () => {
          const db = request.result;
          if (!db.objectStoreNames.contains('models')) {
            db.createObjectStore('models', { keyPath: 'version' });
          }
        };
        request.onsuccess = () => {
          const db = request.result;
          const tx = db.transaction('models', 'readwrite');
          const store = tx.objectStore('models');
          store.put({
            version: 'test-v1',
            model: 'test-model',
            data: new Uint8Array([1, 2, 3, 4]),
            cachedAt: Date.now(),
          });
          tx.oncomplete = () => {
            db.close();
            resolve();
          };
          tx.onerror = () => reject(tx.error);
        };
        request.onerror = () => reject(request.error);
      });
    });

    // Reload page
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    // Check cache persisted
    const cacheExists = await page.evaluate(async () => {
      return await (window as any).checkCache();
    });
    expect(cacheExists).toBe(true);
  });

  test('AC-9: should clear cache correctly', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    // First ensure we have something in cache
    await page.evaluate(async () => {
      return new Promise<void>((resolve, reject) => {
        const request = indexedDB.open('tea-llm-cache', 1);
        request.onupgradeneeded = () => {
          const db = request.result;
          if (!db.objectStoreNames.contains('models')) {
            db.createObjectStore('models', { keyPath: 'version' });
          }
        };
        request.onsuccess = () => {
          const db = request.result;
          const tx = db.transaction('models', 'readwrite');
          const store = tx.objectStore('models');
          store.put({
            version: 'test-v2',
            model: 'test-model',
            data: new Uint8Array([5, 6, 7, 8]),
            cachedAt: Date.now(),
          });
          tx.oncomplete = () => {
            db.close();
            resolve();
          };
          tx.onerror = () => reject(tx.error);
        };
        request.onerror = () => reject(request.error);
      });
    });

    // Clear cache
    await page.evaluate(async () => {
      return await (window as any).clearCache();
    });

    // Verify cleared
    const cacheExists = await page.evaluate(async () => {
      return await (window as any).checkCache();
    });
    expect(cacheExists).toBe(false);
  });

  test('AC-9: should handle second page load with cache', async ({ page }) => {
    // First load - set up cache
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    // Simulate model caching
    await page.evaluate(async () => {
      return new Promise<void>((resolve, reject) => {
        const request = indexedDB.open('tea-llm-cache', 1);
        request.onupgradeneeded = () => {
          const db = request.result;
          if (!db.objectStoreNames.contains('models')) {
            db.createObjectStore('models', { keyPath: 'version' });
          }
        };
        request.onsuccess = () => {
          const db = request.result;
          const tx = db.transaction('models', 'readwrite');
          const store = tx.objectStore('models');
          // Simulate cached model
          store.put({
            version: 'cached-v1',
            model: 'cached-model',
            data: new Uint8Array(1000), // Simulated model data
            cachedAt: Date.now(),
          });
          tx.oncomplete = () => {
            db.close();
            resolve();
          };
          tx.onerror = () => reject(tx.error);
        };
        request.onerror = () => reject(request.error);
      });
    });

    // Second load - should use cache
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    // Verify cache hit
    const cacheExists = await page.evaluate(async () => {
      return await (window as any).checkCache();
    });
    expect(cacheExists).toBe(true);

    // Verify we can still initialize
    const initResult = await initTeaLlm(page);
    expect(initResult).toBe('initialized');
  });
});

test.describe('Error Handling', () => {
  test('should throw if executeLlmYaml called before init', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    // Try to execute without init
    await expect(async () => {
      await page.evaluate(async () => {
        return await (window as any).executeLlmYaml('name: test', {});
      });
    }).rejects.toThrow();
  });

  test('should handle console errors gracefully', async ({ page }) => {
    const consoleErrors: string[] = [];
    page.on('console', (msg) => {
      if (msg.type() === 'error') {
        consoleErrors.push(msg.text());
      }
    });

    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);
    await initTeaLlm(page);

    // Execute a valid workflow
    const yaml = `
name: error-test
nodes:
  - name: gen
    action: llm.call
    with:
      prompt: "test"
edges:
  - from: __start__
    to: gen
  - from: gen
    to: __end__
`;

    await executeLlmYaml(page, yaml, {});

    // Should not have any unexpected console errors
    const unexpectedErrors = consoleErrors.filter(
      (err) => !err.includes('expected') && !err.includes('test')
    );
    expect(unexpectedErrors.length).toBe(0);
  });
});

test.describe('Opik Tracing (TEA-OBS-002)', () => {
  test('should register Opik callback', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    // Check callback not registered initially
    const hasCallbackBefore = await page.evaluate(async () => {
      const init = await import('../../pkg/tea_wasm_llm.js');
      await init.default();
      return init.has_opik_callback();
    });
    expect(hasCallbackBefore).toBe(false);

    // Register callback
    await page.evaluate(async () => {
      const init = await import('../../pkg/tea_wasm_llm.js');
      init.set_opik_callback(() => {
        console.log('Opik trace received');
      });
    });

    // Check callback is registered
    const hasCallbackAfter = await page.evaluate(async () => {
      const init = await import('../../pkg/tea_wasm_llm.js');
      return init.has_opik_callback();
    });
    expect(hasCallbackAfter).toBe(true);
  });

  test('should clear Opik callback', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    // Register and then clear
    await page.evaluate(async () => {
      const init = await import('../../pkg/tea_wasm_llm.js');
      await init.default();
      init.set_opik_callback(() => {});
      init.clear_opik_callback();
    });

    const hasCallback = await page.evaluate(async () => {
      const init = await import('../../pkg/tea_wasm_llm.js');
      return init.has_opik_callback();
    });
    expect(hasCallback).toBe(false);
  });

  test('should configure Opik settings', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    await page.evaluate(async () => {
      const init = await import('../../pkg/tea_wasm_llm.js');
      await init.default();
      init.configure_opik(JSON.stringify({
        project_name: 'test-project',
        enabled: true,
      }));
    });

    const config = await page.evaluate(async () => {
      const init = await import('../../pkg/tea_wasm_llm.js');
      return JSON.parse(init.get_opik_config());
    });

    expect(config.project_name).toBe('test-project');
    expect(config.enabled).toBe(true);
  });

  test('should check is_opik_enabled', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    // Initially disabled (no callback)
    const enabledBefore = await page.evaluate(async () => {
      const init = await import('../../pkg/tea_wasm_llm.js');
      await init.default();
      return init.is_opik_enabled();
    });
    expect(enabledBefore).toBe(false);

    // Enable with callback and config
    await page.evaluate(async () => {
      const init = await import('../../pkg/tea_wasm_llm.js');
      init.configure_opik(JSON.stringify({ enabled: true }));
      init.set_opik_callback(() => {});
    });

    const enabledAfter = await page.evaluate(async () => {
      const init = await import('../../pkg/tea_wasm_llm.js');
      return init.is_opik_enabled();
    });
    expect(enabledAfter).toBe(true);
  });

  test('should create LLM trace', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    const traceJson = await page.evaluate(async () => {
      const init = await import('../../pkg/tea_wasm_llm.js');
      await init.default();
      return init.create_llm_trace(
        'test-node',
        JSON.stringify({ prompt: 'Hello', max_tokens: 10 }),
        JSON.stringify({ content: 'World', usage: { total_tokens: 5 } }),
        '2024-01-15T12:00:00.000Z',
        '2024-01-15T12:00:01.000Z'
      );
    });

    const trace = JSON.parse(traceJson);
    expect(trace.name).toBe('test-node');
    expect(trace.input.prompt).toBe('Hello');
    expect(trace.output.content).toBe('World');
    expect(trace.start_time).toBe('2024-01-15T12:00:00.000Z');
    expect(trace.end_time).toBe('2024-01-15T12:00:01.000Z');
  });

  test('should send trace via callback', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    // Track received traces
    const traces = await page.evaluate(async () => {
      const receivedTraces: string[] = [];

      const init = await import('../../pkg/tea_wasm_llm.js');
      await init.default();

      init.configure_opik(JSON.stringify({ enabled: true }));
      init.set_opik_callback((traceJson: string) => {
        receivedTraces.push(traceJson);
      });

      // Send a trace
      await init.send_opik_trace_async(JSON.stringify({
        id: 'test-trace-id',
        name: 'test-op',
        project_name: 'test-project',
        start_time: '2024-01-15T12:00:00.000Z',
        end_time: '2024-01-15T12:00:01.000Z',
        input: {},
        output: {},
      }));

      return receivedTraces;
    });

    expect(traces.length).toBe(1);
    const trace = JSON.parse(traces[0]);
    expect(trace.id).toBe('test-trace-id');
    expect(trace.name).toBe('test-op');
  });
});
