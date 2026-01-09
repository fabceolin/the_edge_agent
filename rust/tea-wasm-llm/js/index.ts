/**
 * TEA WASM LLM - TypeScript wrapper for The Edge Agent WASM LLM module
 *
 * This module provides a high-level API for running LLM workflows in the browser
 * using wllama (llama.cpp WASM port).
 *
 * Features:
 * - YAML workflow execution with LLM actions
 * - Model loading with IndexedDB caching
 * - Progress tracking during model download
 * - Automatic cache invalidation on version change
 * - Corrupted cache recovery
 *
 * @example
 * ```typescript
 * import { Wllama } from '@wllama/wllama';
 * import { initTeaLlm, executeLlmYaml, loadBundledModel } from 'tea-wasm-llm';
 *
 * // Load model with caching (first load downloads, subsequent loads use cache)
 * const modelData = await loadBundledModel({
 *   modelBasePath: './models',
 *   useCache: true,
 *   onProgress: (loaded, total) => {
 *     console.log(`Loading: ${Math.round(loaded / total * 100)}%`);
 *   }
 * });
 *
 * // Initialize wllama with the loaded model
 * const wllama = new Wllama(CONFIG_PATHS);
 * await wllama.loadModel(modelData);
 *
 * // Initialize TEA LLM with wllama handler
 * await initTeaLlm({}, async (paramsJson) => {
 *     const params = JSON.parse(paramsJson);
 *     const result = await wllama.createCompletion(params.prompt, {
 *         nPredict: params.max_tokens || 100,
 *     });
 *     return JSON.stringify({ content: result, model: 'local-wllama' });
 * });
 *
 * // Execute a YAML workflow
 * const result = await executeLlmYaml(`
 * name: test
 * nodes:
 *   - name: gen
 *     action: llm.call
 *     with:
 *       prompt: "Once upon a time"
 *       max_tokens: 50
 * edges:
 *   - from: __start__
 *     to: gen
 *   - from: gen
 *     to: __end__
 * `, {});
 *
 * console.log(result.llm_response.content);
 * ```
 */

import init, {
  execute_yaml,
  set_llm_handler,
  clear_llm_handler,
  has_llm_handler,
  llm_call_async,
  llm_embed_async,
  has_shared_array_buffer,
  version,
} from '../pkg/tea_wasm_llm.js';

// Import model loading modules
import {
  loadModel,
  verifyChecksum,
  calculateChecksum,
  fetchManifest,
  formatBytes,
  type ModelManifest,
  type ProgressCallback,
  type LoadModelOptions,
} from './model-loader';

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
  type CachedModel,
  type CacheStats,
} from './model-cache';

/**
 * Configuration options for TEA LLM initialization
 */
export interface TeaLlmConfig {
  /**
   * URL to load the model from (optional, for future use)
   */
  modelUrl?: string;

  /**
   * Number of threads to use (0 = auto-detect)
   */
  threads?: number;

  /**
   * Whether to use verbose logging
   */
  verbose?: boolean;
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
 * LLM request parameters (OpenAI-compatible)
 */
export interface LlmParams {
  /**
   * The prompt to send to the model
   */
  prompt: string;

  /**
   * System prompt (optional)
   */
  system?: string;

  /**
   * Maximum tokens to generate
   */
  max_tokens?: number;

  /**
   * Temperature (0.0 - 2.0)
   */
  temperature?: number;

  /**
   * Top-p nucleus sampling
   */
  top_p?: number;

  /**
   * Top-k sampling (0 = disabled)
   */
  top_k?: number;

  /**
   * Model name (for logging/routing)
   */
  model?: string;

  /**
   * Stop sequences
   */
  stop?: string[];
}

/**
 * LLM response structure
 */
export interface LlmResponse {
  /**
   * Generated text content
   */
  content: string;

  /**
   * Model used (for logging)
   */
  model?: string;

  /**
   * Token usage (optional)
   */
  usage?: {
    prompt_tokens: number;
    completion_tokens: number;
    total_tokens: number;
  };
}

/**
 * Handler function type for LLM calls
 * Receives JSON string with LlmParams, returns Promise<JSON string with LlmResponse>
 */
export type LlmHandler = (paramsJson: string) => Promise<string>;

// Track initialization state
let initialized = false;

/**
 * Initialize TEA LLM with a wllama handler
 *
 * @param config - Configuration options
 * @param llmHandler - Function to handle LLM calls (typically wraps wllama)
 *
 * @example
 * ```typescript
 * await initTeaLlm({}, async (paramsJson) => {
 *     const params = JSON.parse(paramsJson);
 *     const result = await wllama.createCompletion(params.prompt, {
 *         nPredict: params.max_tokens || 100,
 *     });
 *     return JSON.stringify({ content: result });
 * });
 * ```
 */
export async function initTeaLlm(
  config: TeaLlmConfig = {},
  llmHandler: LlmHandler
): Promise<void> {
  // Initialize WASM module if not already done
  if (!initialized) {
    await init();
    initialized = true;

    if (config.verbose) {
      console.log('[TEA-WASM-LLM] WASM module initialized');
      console.log('[TEA-WASM-LLM] Version:', version());
      console.log('[TEA-WASM-LLM] SharedArrayBuffer:', has_shared_array_buffer());
    }
  }

  // Register the LLM handler
  set_llm_handler(llmHandler);

  if (config.verbose) {
    console.log('[TEA-WASM-LLM] LLM handler registered');
  }
}

/**
 * Execute a YAML workflow with LLM actions
 *
 * @param yaml - YAML workflow definition
 * @param initialState - Initial state object
 * @returns Promise resolving to the final state
 *
 * @example
 * ```typescript
 * const result = await executeLlmYaml(`
 * name: test
 * nodes:
 *   - name: gen
 *     action: llm.call
 *     with:
 *       prompt: "Hello, world!"
 *       max_tokens: 50
 * edges:
 *   - from: __start__
 *     to: gen
 *   - from: gen
 *     to: __end__
 * `, { input: "test" });
 * ```
 */
export async function executeLlmYaml(
  yaml: string,
  initialState: Record<string, unknown> = {}
): Promise<Record<string, unknown>> {
  if (!initialized) {
    throw new Error('TEA LLM not initialized. Call initTeaLlm() first.');
  }

  const result = await execute_yaml(yaml, JSON.stringify(initialState));
  return JSON.parse(result);
}

/**
 * Call the LLM directly (low-level API)
 *
 * @param params - LLM parameters
 * @param state - Current state object
 * @returns Promise resolving to updated state with llm_response
 */
export async function callLlm(
  params: LlmParams,
  state: Record<string, unknown> = {}
): Promise<Record<string, unknown>> {
  if (!has_llm_handler()) {
    throw new Error('No LLM handler registered. Call initTeaLlm() first.');
  }

  const result = await llm_call_async(JSON.stringify(params), JSON.stringify(state));
  return JSON.parse(result);
}

/**
 * Generate embeddings (low-level API)
 *
 * @param text - Text to embed
 * @param state - Current state object
 * @returns Promise resolving to updated state with embedding
 */
export async function embedText(
  text: string,
  state: Record<string, unknown> = {}
): Promise<Record<string, unknown>> {
  if (!has_llm_handler()) {
    throw new Error('No LLM handler registered. Call initTeaLlm() first.');
  }

  const result = await llm_embed_async(text, JSON.stringify(state));
  return JSON.parse(result);
}

/**
 * Check if an LLM handler is registered
 */
export function isHandlerRegistered(): boolean {
  return has_llm_handler();
}

/**
 * Check if SharedArrayBuffer is available (for multi-threading)
 */
export function isMultiThreaded(): boolean {
  return has_shared_array_buffer();
}

/**
 * Clear the registered LLM handler
 */
export function clearHandler(): void {
  clear_llm_handler();
}

/**
 * Get the library version
 */
export function getVersion(): string {
  return version();
}

// ============================================================================
// Model Loading with Caching
// ============================================================================

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

/**
 * Check if a model is cached
 *
 * @param version - Version string to check
 * @returns Promise resolving to true if model is cached
 */
export async function isModelCached(version: string): Promise<boolean> {
  return isCached(version);
}

/**
 * Get model cache statistics
 *
 * @returns Promise resolving to cache statistics
 */
export async function getModelCacheStats(): Promise<CacheStats> {
  return getCacheStats();
}

/**
 * Clear the model cache
 */
export async function clearModelCache(): Promise<void> {
  return clearCache();
}

// Re-export raw WASM bindings for advanced use
export {
  init,
  execute_yaml,
  set_llm_handler,
  clear_llm_handler,
  has_llm_handler,
  llm_call_async,
  llm_embed_async,
  has_shared_array_buffer,
  version,
};

// Re-export model loading modules
export {
  // Model loader
  loadModel,
  verifyChecksum,
  calculateChecksum,
  fetchManifest,
  formatBytes,
  type ModelManifest,
  type ProgressCallback,
  type LoadModelOptions,
  // Model cache
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
  type CachedModel,
  type CacheStats,
};
