/**
 * Playwright configuration for TEA WASM LLM E2E tests
 *
 * These tests verify:
 * - WASM package loads correctly in browsers
 * - LLM model initialization works
 * - YAML workflow execution via executeLlmYaml()
 * - IndexedDB model caching
 *
 * The test server provides required COOP/COEP headers for SharedArrayBuffer support.
 */

import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
  testDir: './tests/e2e',
  fullyParallel: false, // Run tests sequentially to avoid port conflicts
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: 1, // Single worker for browser tests with shared state
  reporter: process.env.CI ? 'github' : 'html',

  use: {
    baseURL: 'http://localhost:8080',
    trace: 'on-first-retry',
    video: 'on-first-retry',
  },

  // Timeout for each test (model loading can be slow)
  timeout: 120000,

  // Expected conditions timeout
  expect: {
    timeout: 30000,
  },

  // Start local test server with COOP/COEP headers
  webServer: {
    command: 'node tests/server.js',
    port: 8080,
    reuseExistingServer: !process.env.CI,
    timeout: 10000,
  },

  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
    // Firefox and WebKit don't fully support SharedArrayBuffer in same way
    // Only test Chromium for now since wllama requires it
  ],
});
