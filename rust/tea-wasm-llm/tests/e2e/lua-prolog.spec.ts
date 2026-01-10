/**
 * TEA WASM LLM - Lua and Prolog Bridge Tests
 *
 * These tests verify:
 * - AC-1-5: Lua callback registration and lua.eval action
 * - AC-6-10: Prolog handler registration and prolog.query action
 * - AC-11: Both callbacks are optional
 * - AC-14-17: Integration tests with mock handlers
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

test.describe('Lua Callback Bridge', () => {
  test('AC-1: should export set_lua_callback', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    const hasExport = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      return typeof mod.set_lua_callback === 'function';
    });

    expect(hasExport).toBe(true);
  });

  test('AC-1: should register and detect lua callback', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    // Initially no callback
    const hasCallbackBefore = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      return mod.has_lua_callback();
    });
    expect(hasCallbackBefore).toBe(false);

    // Register callback
    await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      mod.set_lua_callback(async (code: string, stateJson: string) => {
        const state = JSON.parse(stateJson);
        // Mock Lua eval - just return the state value doubled
        return JSON.stringify({ result: (state.value || 0) * 2 });
      });
    });

    // Now has callback
    const hasCallbackAfter = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      return mod.has_lua_callback();
    });
    expect(hasCallbackAfter).toBe(true);
  });

  test('AC-2/AC-3: should invoke lua callback with correct parameters', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    // Track callback invocation
    const result = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      await mod.default();

      let capturedCode = '';
      let capturedState: any = null;

      mod.set_lua_callback(async (code: string, stateJson: string) => {
        capturedCode = code;
        capturedState = JSON.parse(stateJson);
        return JSON.stringify({ result: 42 });
      });

      // Initialize LLM handler to allow workflow execution
      mod.set_llm_handler(async () => JSON.stringify({ content: 'test' }));

      const yaml = `
name: lua-test
nodes:
  - name: eval
    action: lua.eval
    with:
      code: "return state.x + 1"
edges:
  - from: __start__
    to: eval
  - from: eval
    to: __end__
`;

      await mod.execute_yaml(yaml, JSON.stringify({ x: 10 }));

      return { capturedCode, capturedState };
    });

    expect(result.capturedCode).toBe('return state.x + 1');
    expect(result.capturedState).toHaveProperty('x', 10);
  });

  test('AC-4: should merge lua result into workflow state', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    const result = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      await mod.default();

      mod.set_lua_callback(async (code: string, stateJson: string) => {
        return JSON.stringify({ result: 'lua_computed_value' });
      });

      mod.set_llm_handler(async () => JSON.stringify({ content: 'test' }));

      const yaml = `
name: lua-result-test
nodes:
  - name: compute
    action: lua.eval
    with:
      code: "return 'test'"
    output: computed
edges:
  - from: __start__
    to: compute
  - from: compute
    to: __end__
`;

      const resultJson = await mod.execute_yaml(yaml, JSON.stringify({}));
      return JSON.parse(resultJson);
    });

    expect(result).toHaveProperty('computed', 'lua_computed_value');
  });

  test('AC-5: should error if lua.eval called without callback', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    const error = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      await mod.default();

      // Clear any existing callback
      mod.clear_lua_callback();

      // Set LLM handler only
      mod.set_llm_handler(async () => JSON.stringify({ content: 'test' }));

      const yaml = `
name: no-lua-handler
nodes:
  - name: eval
    action: lua.eval
    with:
      code: "return 1"
edges:
  - from: __start__
    to: eval
  - from: eval
    to: __end__
`;

      try {
        await mod.execute_yaml(yaml, JSON.stringify({}));
        return null;
      } catch (e: any) {
        return e.toString();
      }
    });

    expect(error).not.toBeNull();
    expect(error).toContain('Lua');
    expect(error).toContain('callback');
  });

  test('should clear lua callback', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    const result = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      await mod.default();

      // Register
      mod.set_lua_callback(async () => JSON.stringify({ result: null }));
      const hasBefore = mod.has_lua_callback();

      // Clear
      mod.clear_lua_callback();
      const hasAfter = mod.has_lua_callback();

      return { hasBefore, hasAfter };
    });

    expect(result.hasBefore).toBe(true);
    expect(result.hasAfter).toBe(false);
  });
});

test.describe('Prolog Handler Bridge', () => {
  test('AC-6: should export set_prolog_handler', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    const hasExport = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      return typeof mod.set_prolog_handler === 'function';
    });

    expect(hasExport).toBe(true);
  });

  test('AC-6: should register and detect prolog handler', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    // Initially no handler
    const hasHandlerBefore = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      return mod.has_prolog_handler();
    });
    expect(hasHandlerBefore).toBe(false);

    // Register handler
    await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      mod.set_prolog_handler(async (queryJson: string) => {
        return JSON.stringify({ bindings: [], success: true });
      });
    });

    // Now has handler
    const hasHandlerAfter = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      return mod.has_prolog_handler();
    });
    expect(hasHandlerAfter).toBe(true);
  });

  test('AC-7/AC-8: should invoke prolog handler with query JSON', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    const result = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      await mod.default();

      let capturedQuery: any = null;

      mod.set_prolog_handler(async (queryJson: string) => {
        capturedQuery = JSON.parse(queryJson);
        return JSON.stringify({
          bindings: [{ X: 'result' }],
          success: true
        });
      });

      mod.set_llm_handler(async () => JSON.stringify({ content: 'test' }));

      const yaml = `
name: prolog-test
nodes:
  - name: query
    action: prolog.query
    with:
      code: "member(X, [a,b,c])"
      facts: "fact(1). fact(2)."
edges:
  - from: __start__
    to: query
  - from: query
    to: __end__
`;

      await mod.execute_yaml(yaml, JSON.stringify({}));

      return capturedQuery;
    });

    expect(result).toHaveProperty('code', 'member(X, [a,b,c])');
    expect(result).toHaveProperty('facts', 'fact(1). fact(2).');
  });

  test('AC-9: should return bindings as JSON array', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    const result = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      await mod.default();

      mod.set_prolog_handler(async (queryJson: string) => {
        return JSON.stringify({
          bindings: [
            { X: 1 },
            { X: 2 },
            { X: 3 }
          ],
          success: true
        });
      });

      mod.set_llm_handler(async () => JSON.stringify({ content: 'test' }));

      const yaml = `
name: prolog-bindings-test
nodes:
  - name: query
    action: prolog.query
    with:
      code: "member(X, [1,2,3])"
    output: members
edges:
  - from: __start__
    to: query
  - from: query
    to: __end__
`;

      const resultJson = await mod.execute_yaml(yaml, JSON.stringify({}));
      return JSON.parse(resultJson);
    });

    expect(result).toHaveProperty('members');
    expect(result.members).toHaveProperty('bindings');
    expect(result.members.bindings).toHaveLength(3);
    expect(result.members.bindings[0]).toEqual({ X: 1 });
  });

  test('AC-10: should error if prolog.query called without handler', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    const error = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      await mod.default();

      // Clear any existing handler
      mod.clear_prolog_handler();

      mod.set_llm_handler(async () => JSON.stringify({ content: 'test' }));

      const yaml = `
name: no-prolog-handler
nodes:
  - name: query
    action: prolog.query
    with:
      code: "true"
edges:
  - from: __start__
    to: query
  - from: query
    to: __end__
`;

      try {
        await mod.execute_yaml(yaml, JSON.stringify({}));
        return null;
      } catch (e: any) {
        return e.toString();
      }
    });

    expect(error).not.toBeNull();
    expect(error).toContain('Prolog');
    expect(error).toContain('handler');
  });

  test('should clear prolog handler', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    const result = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      await mod.default();

      // Register
      mod.set_prolog_handler(async () => JSON.stringify({ bindings: [], success: true }));
      const hasBefore = mod.has_prolog_handler();

      // Clear
      mod.clear_prolog_handler();
      const hasAfter = mod.has_prolog_handler();

      return { hasBefore, hasAfter };
    });

    expect(result.hasBefore).toBe(true);
    expect(result.hasAfter).toBe(false);
  });
});

test.describe('Optional Callbacks (AC-11)', () => {
  test('package should work without lua callback', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    const result = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      await mod.default();

      // Only set LLM handler, no Lua
      mod.set_llm_handler(async () => JSON.stringify({ content: 'works' }));

      const yaml = `
name: no-lua-needed
nodes:
  - name: llm
    action: llm.call
    with:
      prompt: "test"
edges:
  - from: __start__
    to: llm
  - from: llm
    to: __end__
`;

      const resultJson = await mod.execute_yaml(yaml, JSON.stringify({}));
      return JSON.parse(resultJson);
    });

    expect(result).toHaveProperty('llm');
  });

  test('package should work without prolog handler', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    const result = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      await mod.default();

      // Only set LLM handler, no Prolog
      mod.set_llm_handler(async () => JSON.stringify({ content: 'works' }));

      const yaml = `
name: no-prolog-needed
nodes:
  - name: llm
    action: llm.call
    with:
      prompt: "test"
edges:
  - from: __start__
    to: llm
  - from: llm
    to: __end__
`;

      const resultJson = await mod.execute_yaml(yaml, JSON.stringify({}));
      return JSON.parse(resultJson);
    });

    expect(result).toHaveProperty('llm');
  });
});

test.describe('Combined Workflow (LLM + Lua + Prolog)', () => {
  test('should execute workflow with all three runtimes', async ({ page }) => {
    await page.goto(TEST_PAGE);
    await waitForWasmLoad(page);

    const result = await page.evaluate(async () => {
      const mod = await import('../../pkg/tea_wasm_llm.js');
      await mod.default();

      // Set all handlers
      mod.set_llm_handler(async (paramsJson: string) => {
        const params = JSON.parse(paramsJson);
        return JSON.stringify({ content: `LLM: ${params.prompt}` });
      });

      mod.set_lua_callback(async (code: string, stateJson: string) => {
        const state = JSON.parse(stateJson);
        // Mock: return state.value > 10
        return JSON.stringify({ result: (state.value || 0) > 10 });
      });

      mod.set_prolog_handler(async (queryJson: string) => {
        const query = JSON.parse(queryJson);
        return JSON.stringify({
          bindings: [{ result: 'prolog_success' }],
          success: true
        });
      });

      const yaml = `
name: multi-runtime
nodes:
  - name: generate
    action: llm.call
    with:
      prompt: "Generate something"
    output: generated

  - name: validate
    action: lua.eval
    with:
      code: "return state.value > 10"
    output: is_valid

  - name: reason
    action: prolog.query
    with:
      code: "result(X)"
    output: reasoning

edges:
  - from: __start__
    to: generate
  - from: generate
    to: validate
  - from: validate
    to: reason
  - from: reason
    to: __end__
`;

      const resultJson = await mod.execute_yaml(yaml, JSON.stringify({ value: 42 }));
      return JSON.parse(resultJson);
    });

    // All three should have produced results
    expect(result).toHaveProperty('generated');
    expect(result).toHaveProperty('is_valid', true);
    expect(result).toHaveProperty('reasoning');
    expect(result.reasoning).toHaveProperty('success', true);
  });
});
