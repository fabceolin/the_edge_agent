import { defineConfig } from 'vitest/config';
import path from 'path';

export default defineConfig({
  test: {
    environment: 'node',
    globals: true,
    include: ['tests/**/*.test.ts'],
    setupFiles: [],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['js/**/*.ts'],
      exclude: ['js/**/*.d.ts'],
    },
    // Required for fake-indexeddb
    pool: 'forks',
    testTimeout: 10000,
  },
  resolve: {
    alias: {
      // Map JS imports to source files for testing (use absolute paths)
      '../js/model-cache': path.resolve(__dirname, 'js/model-cache.ts'),
      '../js/model-loader': path.resolve(__dirname, 'js/model-loader.ts'),
    },
  },
});
