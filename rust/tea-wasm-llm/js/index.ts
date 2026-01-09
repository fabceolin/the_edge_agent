/**
 * TEA WASM LLM - Batteries-Included Browser LLM
 *
 * This module provides a high-level API for running LLM inference in the browser
 * with wllama bundled internally - zero external npm dependencies required.
 *
 * Features:
 * - Single import, zero external dependencies ("batteries included")
 * - Model loading with IndexedDB caching
 * - Progress tracking during model download
 * - Automatic multi-threading detection
 * - YAML workflow execution with LLM actions
 * - Backward compatible with legacy callback API
 *
 * ## Quick Start (New Batteries-Included API)
 *
 * @example
 * ```typescript
 * import { initLlm, chat, chatStream, embed } from 'tea-wasm-llm';
 *
 * // Initialize and load model (one-time)
 * await initLlm({
 *   modelUrl: 'https://huggingface.co/.../Phi-4-mini-Q3_K_S.gguf',
 *   onProgress: (loaded, total) => console.log(`${Math.round(loaded/total*100)}%`),
 * });
 *
 * // Simple chat
 * const response = await chat("What is 2+2?", { maxTokens: 50 });
 * console.log(response.content);
 *
 * // Streaming chat
 * await chatStream("Tell me a story", (token) => {
 *   process.stdout.write(token);
 * });
 *
 * // Generate embeddings
 * const embedding = await embed("Hello world");
 * console.log(embedding.vector);
 * ```
 *
 * ## Legacy Callback API (Backward Compatible)
 *
 * @example
 * ```typescript
 * import { initTeaLlm, executeLlmYaml } from 'tea-wasm-llm';
 *
 * // Register custom LLM handler (e.g., external wllama instance)
 * await initTeaLlm({}, async (paramsJson) => {
 *     const params = JSON.parse(paramsJson);
 *     const result = await wllama.createCompletion(params.prompt, {...});
 *     return JSON.stringify({ content: result });
 * });
 *
 * // Execute YAML workflow
 * const result = await executeLlmYaml(yaml, {});
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
  loadBundledModel,
  loadBundledModelSafe,
  type ModelManifest,
  type ProgressCallback,
  type LoadModelOptions,
  type BundledModelConfig,
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

// BundledModelConfig is imported from model-loader

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
 * @deprecated Use initLlm() instead for the batteries-included API.
 * This function is maintained for backward compatibility with the callback pattern.
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
 * @deprecated Use chat() or chatStream() instead for the batteries-included API.
 * This function is maintained for backward compatibility with the callback pattern.
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
// loadBundledModel and loadBundledModelSafe are imported from model-loader.ts

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

/**
 * Re-export raw WASM bindings for advanced use.
 *
 * @deprecated For new projects, use the batteries-included API instead:
 * - initLlm() instead of init() + set_llm_handler()
 * - chat() / chatStream() instead of llm_call_async()
 * - embed() instead of llm_embed_async()
 *
 * The callback API (set_llm_handler) is maintained for backward compatibility
 * and for advanced use cases where you need a custom LLM handler.
 */
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

// ============================================================================
// NEW: Batteries-Included API (wllama bundled internally)
// ============================================================================

// Re-export from wllama-loader for the new simplified API
export {
  // Core API
  initLlm,
  chat,
  chatStream,
  embed,
  isLlmReady,
  disposeLlm,
  // Threading detection (renamed to avoid conflict with WASM export)
  hasSharedArrayBuffer as hasSharedArrayBufferJs,
  hasCoopCoep,
  // Cache helpers
  getLlmCacheStats,
  clearLlmCache,
  // Advanced
  getWllamaInstance,
  getDefaultAssetPaths,
  // Types
  type InitLlmConfig,
  type ChatOptions,
  type ChatResponse,
  type EmbedResponse,
  type TokenCallback,
  type WllamaAssetPaths,
} from './wllama-loader';

// Note: initTeaLlm and executeLlmYaml are already exported above with @deprecated notices
