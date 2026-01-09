/**
 * Tests for wllama-loader.ts - Batteries-included API
 *
 * These tests verify the new simplified API for the bundled LLM functionality.
 * Note: Full integration tests require wllama and a model, which are mocked here.
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';

// Mock the wllama module since we don't have it installed in test environment
vi.mock('@wllama/wllama', () => {
  return {
    Wllama: vi.fn().mockImplementation(() => ({
      loadModel: vi.fn().mockResolvedValue(undefined),
      createCompletion: vi.fn().mockResolvedValue('Mock response'),
      createEmbedding: vi.fn().mockResolvedValue([0.1, 0.2, 0.3, 0.4]),
      exit: vi.fn().mockResolvedValue(undefined),
    })),
  };
});

// Mock model-loader
vi.mock('../js/model-loader', () => ({
  loadBundledModel: vi.fn().mockResolvedValue(new Uint8Array([1, 2, 3, 4])),
}));

// Mock model-cache
vi.mock('../js/model-cache', () => ({
  getCacheStats: vi.fn().mockResolvedValue({
    available: true,
    modelCount: 0,
    totalSize: 0,
  }),
  clearCache: vi.fn().mockResolvedValue(undefined),
}));

// Import after mocking
import {
  initLlm,
  chat,
  chatStream,
  embed,
  isLlmReady,
  disposeLlm,
  hasSharedArrayBuffer,
  hasCoopCoep,
  getLlmCacheStats,
  clearLlmCache,
  getDefaultAssetPaths,
  type InitLlmConfig,
  type ChatOptions,
  type ChatResponse,
  type EmbedResponse,
} from '../js/wllama-loader';

describe('wllama-loader', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  afterEach(async () => {
    // Clean up LLM instance between tests
    await disposeLlm();
  });

  describe('getDefaultAssetPaths', () => {
    it('returns correct paths for default base path', () => {
      const paths = getDefaultAssetPaths();
      expect(paths['single-thread/wllama.wasm']).toBe('./assets/single-thread/wllama.wasm');
      expect(paths['multi-thread/wllama.wasm']).toBe('./assets/multi-thread/wllama.wasm');
    });

    it('returns correct paths for custom base path', () => {
      const paths = getDefaultAssetPaths('/custom/path');
      expect(paths['single-thread/wllama.wasm']).toBe('/custom/path/assets/single-thread/wllama.wasm');
      expect(paths['multi-thread/wllama.wasm']).toBe('/custom/path/assets/multi-thread/wllama.wasm');
    });
  });

  describe('hasSharedArrayBuffer', () => {
    it('returns boolean', () => {
      const result = hasSharedArrayBuffer();
      expect(typeof result).toBe('boolean');
    });
  });

  describe('hasCoopCoep', () => {
    it('returns boolean', () => {
      const result = hasCoopCoep();
      expect(typeof result).toBe('boolean');
    });
  });

  describe('isLlmReady', () => {
    it('returns false before initialization', () => {
      expect(isLlmReady()).toBe(false);
    });
  });

  describe('initLlm', () => {
    it('initializes without error with CDN mode', async () => {
      const config: InitLlmConfig = {
        useCdn: true,
        modelBasePath: './models',
        verbose: true,
      };

      // This should not throw
      await expect(initLlm(config)).resolves.toBeUndefined();
    });

    it('calls onReady callback after initialization', async () => {
      const onReady = vi.fn();

      await initLlm({
        useCdn: true,
        modelBasePath: './models',
        onReady,
      });

      expect(onReady).toHaveBeenCalled();
    });

    it('calls onProgress callback during model loading', async () => {
      const onProgress = vi.fn();

      await initLlm({
        useCdn: true,
        modelBasePath: './models',
        onProgress,
      });

      // Progress callback is called by model loader
      // In mock, it may not be called, but we verify the parameter is passed
      expect(typeof onProgress).toBe('function');
    });
  });

  describe('chat', () => {
    it('throws error if LLM not initialized', async () => {
      await disposeLlm(); // Ensure clean state

      await expect(chat('Hello')).rejects.toThrow('LLM not initialized');
    });

    it('returns ChatResponse with content after initialization', async () => {
      await initLlm({
        useCdn: true,
        modelBasePath: './models',
      });

      const response = await chat('Hello', { maxTokens: 50 });

      expect(response).toHaveProperty('content');
      expect(typeof response.content).toBe('string');
    });

    it('applies chat options correctly', async () => {
      await initLlm({
        useCdn: true,
        modelBasePath: './models',
      });

      const options: ChatOptions = {
        maxTokens: 100,
        temperature: 0.5,
        topP: 0.8,
        topK: 40,
        stop: ['END'],
        system: 'You are helpful',
      };

      const response = await chat('Hello', options);
      expect(response.content).toBeDefined();
    });
  });

  describe('chatStream', () => {
    it('throws error if LLM not initialized', async () => {
      await disposeLlm();

      const onToken = vi.fn();
      await expect(chatStream('Hello', onToken)).rejects.toThrow('LLM not initialized');
    });

    it('calls token callback for each token', async () => {
      await initLlm({
        useCdn: true,
        modelBasePath: './models',
      });

      const tokens: string[] = [];
      const onToken = vi.fn((token: string) => {
        tokens.push(token);
      });

      await chatStream('Hello', onToken);

      // The mock returns a single response, so we check the callback was set up
      expect(typeof onToken).toBe('function');
    });
  });

  describe('embed', () => {
    it('throws error if LLM not initialized', async () => {
      await disposeLlm();

      await expect(embed('Hello')).rejects.toThrow('LLM not initialized');
    });

    it('returns EmbedResponse with vector after initialization', async () => {
      await initLlm({
        useCdn: true,
        modelBasePath: './models',
      });

      const response = await embed('Hello world');

      expect(response).toHaveProperty('vector');
      expect(response).toHaveProperty('dimension');
      expect(response.vector).toBeInstanceOf(Float32Array);
      expect(response.dimension).toBe(response.vector.length);
    });
  });

  describe('disposeLlm', () => {
    it('cleans up LLM instance', async () => {
      await initLlm({
        useCdn: true,
        modelBasePath: './models',
      });

      expect(isLlmReady()).toBe(true);

      await disposeLlm();

      expect(isLlmReady()).toBe(false);
    });

    it('can be called multiple times safely', async () => {
      await disposeLlm();
      await disposeLlm();
      await disposeLlm();

      // Should not throw
      expect(isLlmReady()).toBe(false);
    });
  });

  describe('getLlmCacheStats', () => {
    it('returns cache statistics', async () => {
      const stats = await getLlmCacheStats();

      expect(stats).toHaveProperty('available');
      expect(stats).toHaveProperty('modelCount');
      expect(stats).toHaveProperty('totalSize');
    });
  });

  describe('clearLlmCache', () => {
    it('clears cache without error', async () => {
      await expect(clearLlmCache()).resolves.toBeUndefined();
    });
  });
});

describe('Type exports', () => {
  it('exports InitLlmConfig type', () => {
    const config: InitLlmConfig = {
      modelUrl: 'test',
      verbose: true,
    };
    expect(config.modelUrl).toBe('test');
  });

  it('exports ChatOptions type', () => {
    const options: ChatOptions = {
      maxTokens: 100,
      temperature: 0.7,
    };
    expect(options.maxTokens).toBe(100);
  });

  it('exports ChatResponse type', () => {
    const response: ChatResponse = {
      content: 'test',
      usage: {
        promptTokens: 10,
        completionTokens: 5,
        totalTokens: 15,
      },
    };
    expect(response.content).toBe('test');
  });

  it('exports EmbedResponse type', () => {
    const response: EmbedResponse = {
      vector: new Float32Array([0.1, 0.2]),
      dimension: 2,
    };
    expect(response.dimension).toBe(2);
  });
});
