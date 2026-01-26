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
  // Lua callback bridge
  set_lua_callback,
  clear_lua_callback,
  has_lua_callback,
  lua_eval_async,
  // Prolog handler bridge
  set_prolog_handler,
  clear_prolog_handler,
  has_prolog_handler,
  prolog_query_async,
  // Opik tracing bridge (TEA-OBS-002)
  set_opik_callback,
  clear_opik_callback,
  has_opik_callback,
  configure_opik,
  get_opik_config,
  is_opik_enabled,
  send_opik_trace_async,
  create_llm_trace,
  // DuckDB bridge (TEA-WASM-003.2)
  set_duckdb_handler,
  clear_duckdb_handler,
  has_duckdb_handler,
  duckdb_query_async,
  duckdb_execute_async,
  init_duckdb_async,
  load_duckdb_extension_async,
  get_duckdb_extensions_async,
  duckdb_begin_async,
  duckdb_commit_async,
  duckdb_rollback_async,
  // LTM bridge (TEA-WASM-003.3)
  set_ltm_handler,
  clear_ltm_handler,
  has_ltm_handler,
  configure_ltm,
  get_ltm_config,
  ltm_store_async,
  ltm_retrieve_async,
  ltm_delete_async,
  ltm_search_async,
  ltm_list_async,
  ltm_cleanup_expired_async,
  ltm_stats_async,
  has_shared_array_buffer,
  version,
  // Game functions (TEA-GAME-001.7)
  game_init,
  game_start_session,
  game_generate_round,
  game_submit_answer,
  game_submit_to_leaderboard,
  game_get_leaderboard,
  game_get_session_stats,
  game_set_llm_handler,
  game_clear_llm_handler,
  game_has_llm_handler,
  // Game opik functions - note: wasm-bindgen uses original names from game_opik.rs
  set_game_opik_handler,
  clear_game_opik_handler,
  has_game_opik_handler,
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

// Import DuckDB loader (TEA-WASM-003.2)
import {
  initDuckDb,
  initDuckDbWithHandler,
  getConnection as getDuckDbConnection,
  createDuckDbHandler,
  loadExtension as loadDuckDbExtension,
  query as queryDuckDb,
  execute as executeDuckDb,
  closeDuckDb,
  isDuckDbInitialized,
  getDuckDbVersion,
  hasSharedArrayBuffer as hasSAB,
  EXTENSIONS as DUCKDB_EXTENSIONS,
  type DuckDbLoaderOptions,
  type DuckDbQueryResult,
  type DuckDbHandler,
} from './duckdb-loader';

// Import LTM loader (TEA-WASM-003.3)
import {
  initLtm,
  initLtmWithHandler,
  createLtmHandler,
  clearLtmData,
  getLtmStats,
  isIndexedDBAvailable,
  isLtmInitialized,
  closeLtm,
  startSyncWorker,
  stopSyncWorker,
  type LtmLoaderOptions,
  type LtmEntry,
  type LtmHandler,
} from './ltm-loader';

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

// ============================================================================
// Lua Callback Bridge Types
// ============================================================================

/**
 * Lua evaluation parameters
 */
export interface LuaParams {
  /**
   * Lua code to execute
   */
  code: string;

  /**
   * Output key to store result (optional)
   */
  output?: string;
}

/**
 * Lua evaluation response
 */
export interface LuaResponse {
  /**
   * Result from Lua execution
   */
  result: unknown;

  /**
   * Error message if execution failed (optional)
   */
  error?: string;
}

/**
 * Handler function type for Lua evaluation
 * Receives (code: string, stateJson: string) and returns Promise<JSON string with LuaResponse>
 *
 * @example
 * ```typescript
 * import { LuaFactory } from 'wasmoon';
 *
 * const lua = await (new LuaFactory()).createEngine();
 *
 * const luaCallback: LuaCallback = async (code, stateJson) => {
 *   const state = JSON.parse(stateJson);
 *   lua.global.set('state', state);
 *   const result = await lua.doString(code);
 *   return JSON.stringify({ result });
 * };
 * ```
 */
export type LuaCallback = (code: string, stateJson: string) => Promise<string>;

// ============================================================================
// Prolog Handler Bridge Types
// ============================================================================

/**
 * Prolog query parameters
 */
export interface PrologParams {
  /**
   * Prolog query to execute
   */
  code: string;

  /**
   * Optional facts to assert before query
   */
  facts?: string;

  /**
   * Output key to store result (optional)
   */
  output?: string;
}

/**
 * Prolog query response
 */
export interface PrologResponse {
  /**
   * Variable bindings from query results (array of binding objects)
   */
  bindings: Record<string, unknown>[];

  /**
   * Whether the query succeeded
   */
  success: boolean;

  /**
   * Error message if query failed (optional)
   */
  error?: string;
}

/**
 * Handler function type for Prolog queries
 * Receives (queryJson: string) containing code and optional facts
 * Returns Promise<JSON string with PrologResponse>
 *
 * @example
 * ```typescript
 * import { Prolog } from 'trealla';
 *
 * const pl = new Prolog();
 *
 * const prologHandler: PrologHandler = async (queryJson) => {
 *   const { code, facts } = JSON.parse(queryJson);
 *   if (facts) await pl.consultText(facts);
 *   const results = await pl.queryOnce(code);
 *   return JSON.stringify({ bindings: results ? [results] : [], success: !!results });
 * };
 * ```
 */
export type PrologHandler = (queryJson: string) => Promise<string>;

// ============================================================================
// DuckDB Handler Bridge Types (TEA-WASM-003.2)
// ============================================================================

/**
 * Re-export DuckDB types from duckdb-loader
 */
export type { DuckDbLoaderOptions, DuckDbQueryResult, DuckDbHandler };

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
 * Initialize just the YAML workflow engine without requiring an LLM handler.
 *
 * Use this when you only need to run non-LLM workflows (return, lua, prolog, etc.)
 * and don't want to wait for or require an LLM to be available.
 *
 * @param config - Configuration options
 *
 * @example
 * ```typescript
 * // Initialize YAML engine only
 * await initYamlEngine();
 *
 * // Now you can run non-LLM workflows
 * const result = await executeLlmYaml(`
 * name: simple
 * nodes:
 *   - name: greet
 *     action: return
 *     with:
 *       value:
 *         message: "Hello, {{ state.name }}!"
 * edges:
 *   - from: __start__
 *     to: greet
 *   - from: greet
 *     to: __end__
 * `, { name: "World" });
 * ```
 */
export async function initYamlEngine(
  config: { verbose?: boolean } = {}
): Promise<void> {
  if (!initialized) {
    await init();
    initialized = true;

    if (config.verbose) {
      console.log('[TEA-WASM-LLM] WASM module initialized (YAML engine only)');
      console.log('[TEA-WASM-LLM] Version:', version());
    }
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
// Lua Callback Bridge Functions
// ============================================================================

/**
 * Register a Lua evaluation callback
 *
 * The callback receives (code: string, stateJson: string) and should return
 * a Promise that resolves to JSON string with { result: any, error?: string }
 *
 * @param callback - Function to handle Lua evaluation (typically wraps wasmoon)
 *
 * @example
 * ```typescript
 * import { LuaFactory } from 'wasmoon';
 *
 * const lua = await (new LuaFactory()).createEngine();
 *
 * registerLuaCallback(async (code, stateJson) => {
 *   const state = JSON.parse(stateJson);
 *   lua.global.set('state', state);
 *   try {
 *     const result = await lua.doString(code);
 *     return JSON.stringify({ result });
 *   } catch (e) {
 *     return JSON.stringify({ result: null, error: String(e) });
 *   }
 * });
 * ```
 */
export function registerLuaCallback(callback: LuaCallback): void {
  set_lua_callback(callback);
}

/**
 * Clear the registered Lua callback
 */
export function clearLuaCallback(): void {
  clear_lua_callback();
}

/**
 * Check if a Lua callback is registered
 */
export function isLuaCallbackRegistered(): boolean {
  return has_lua_callback();
}

/**
 * Evaluate Lua code (low-level API)
 *
 * @param code - Lua code to execute
 * @param state - Current state object
 * @returns Promise resolving to updated state with lua_result
 */
export async function evalLua(
  code: string,
  state: Record<string, unknown> = {}
): Promise<Record<string, unknown>> {
  if (!has_lua_callback()) {
    throw new Error(
      'No Lua callback registered. Call registerLuaCallback() with a wasmoon handler first.'
    );
  }

  const result = await lua_eval_async(code, JSON.stringify(state));
  return JSON.parse(result);
}

// ============================================================================
// Prolog Handler Bridge Functions
// ============================================================================

/**
 * Register a Prolog query handler
 *
 * The handler receives (queryJson: string) containing { code, facts? } and should
 * return a Promise that resolves to JSON string with { bindings: [], success: boolean, error?: string }
 *
 * @param handler - Function to handle Prolog queries (typically wraps trealla or swipl-wasm)
 *
 * @example
 * ```typescript
 * import { Prolog } from 'trealla';
 *
 * const pl = new Prolog();
 *
 * registerPrologHandler(async (queryJson) => {
 *   const { code, facts } = JSON.parse(queryJson);
 *   try {
 *     if (facts) await pl.consultText(facts);
 *     const results = await pl.queryOnce(code);
 *     return JSON.stringify({
 *       bindings: results ? [results] : [],
 *       success: !!results
 *     });
 *   } catch (e) {
 *     return JSON.stringify({ bindings: [], success: false, error: String(e) });
 *   }
 * });
 * ```
 */
export function registerPrologHandler(handler: PrologHandler): void {
  set_prolog_handler(handler);
}

/**
 * Clear the registered Prolog handler
 */
export function clearPrologHandler(): void {
  clear_prolog_handler();
}

/**
 * Check if a Prolog handler is registered
 */
export function isPrologHandlerRegistered(): boolean {
  return has_prolog_handler();
}

/**
 * Execute a Prolog query (low-level API)
 *
 * @param code - Prolog query to execute
 * @param facts - Optional facts to assert before query
 * @param state - Current state object
 * @returns Promise resolving to updated state with prolog_result
 */
export async function queryProlog(
  code: string,
  facts?: string,
  state: Record<string, unknown> = {}
): Promise<Record<string, unknown>> {
  if (!has_prolog_handler()) {
    throw new Error(
      'No Prolog handler registered. Call registerPrologHandler() with a trealla/swipl handler first.'
    );
  }

  const queryJson = JSON.stringify({ code, facts });
  const result = await prolog_query_async(queryJson, JSON.stringify(state));
  return JSON.parse(result);
}

// ============================================================================
// Opik Tracing Bridge Functions (TEA-OBS-002)
// ============================================================================

/**
 * Opik tracing configuration
 */
export interface OpikTracingConfig {
  /**
   * Opik API key (required for tracing to work)
   * Can also be set via localStorage key 'OPIK_API_KEY'
   */
  apiKey?: string;

  /**
   * Project name for traces
   * Defaults to 'tea-wasm'
   */
  projectName?: string;

  /**
   * Optional workspace name
   */
  workspace?: string;

  /**
   * Custom Opik API URL
   * Defaults to 'https://www.comet.com/opik/api'
   */
  apiUrl?: string;

  /**
   * Whether to enable verbose logging
   */
  verbose?: boolean;
}

/**
 * Opik trace structure (matches Opik REST API)
 */
export interface OpikTrace {
  id: string;
  name: string;
  project_name: string;
  start_time: string;
  end_time: string;
  input: unknown;
  output: unknown;
  usage?: {
    prompt_tokens?: number;
    completion_tokens?: number;
    total_tokens?: number;
  };
  metadata?: Record<string, unknown>;
}

/**
 * Callback type for custom Opik trace handling
 */
export type OpikTraceCallback = (traceJson: string) => Promise<void> | void;

// Store API key and config internally
let opikApiKey: string | null = null;
let opikApiUrl = 'https://www.comet.com/opik/api';
let opikVerbose = false;

/**
 * Initialize Opik tracing
 *
 * Call this function to enable automatic tracing of LLM calls to Opik.
 * Traces are sent to the Opik REST API after each LLM call completes.
 *
 * @param config - Opik tracing configuration
 *
 * @example
 * ```typescript
 * import { initOpikTracing, initLlm, chat } from 'tea-wasm-llm';
 *
 * // Initialize Opik tracing
 * await initOpikTracing({
 *   apiKey: 'your-opik-api-key',
 *   projectName: 'my-agent',
 * });
 *
 * // Initialize LLM
 * await initLlm({ modelUrl: '...' });
 *
 * // Chat - traces are sent automatically
 * const response = await chat("Hello!");
 * ```
 */
export async function initOpikTracing(config: OpikTracingConfig = {}): Promise<void> {
  // Get API key from config, localStorage, or error
  opikApiKey = config.apiKey || localStorage.getItem('OPIK_API_KEY');

  if (!opikApiKey) {
    console.warn('[TEA-OPIK] No API key provided. Opik tracing disabled.');
    console.warn('[TEA-OPIK] Set apiKey in config or store in localStorage as "OPIK_API_KEY"');
    return;
  }

  opikApiUrl = config.apiUrl || 'https://www.comet.com/opik/api';
  opikVerbose = config.verbose || false;

  // Configure the WASM module
  const wasmConfig = {
    project_name: config.projectName || 'tea-wasm',
    workspace: config.workspace,
    enabled: true,
  };
  configure_opik(JSON.stringify(wasmConfig));

  // Register the callback that sends traces to Opik API
  set_opik_callback(async (traceJson: string) => {
    await sendTraceToOpik(traceJson);
  });

  if (opikVerbose) {
    console.log('[TEA-OPIK] Tracing initialized');
    console.log('[TEA-OPIK] Project:', config.projectName || 'tea-wasm');
    console.log('[TEA-OPIK] API URL:', opikApiUrl);
  }
}

/**
 * Internal function to send trace to Opik REST API
 */
async function sendTraceToOpik(traceJson: string): Promise<void> {
  if (!opikApiKey) {
    return; // Silently skip if no API key
  }

  try {
    const response = await fetch(`${opikApiUrl}/v1/private/traces`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        Authorization: `Bearer ${opikApiKey}`,
      },
      body: traceJson,
    });

    if (!response.ok) {
      if (opikVerbose) {
        console.warn(`[TEA-OPIK] Failed to send trace: ${response.status} ${response.statusText}`);
      }
    } else if (opikVerbose) {
      console.log('[TEA-OPIK] Trace sent successfully');
    }
  } catch (error) {
    if (opikVerbose) {
      console.warn('[TEA-OPIK] Error sending trace:', error);
    }
    // Don't throw - graceful degradation
  }
}

/**
 * Disable Opik tracing
 */
export function disableOpikTracing(): void {
  clear_opik_callback();
  opikApiKey = null;

  // Disable in WASM config
  configure_opik(JSON.stringify({ enabled: false }));
}

/**
 * Check if Opik tracing is enabled
 */
export function isOpikTracingEnabled(): boolean {
  return is_opik_enabled();
}

/**
 * Check if Opik callback is registered
 */
export function isOpikCallbackRegistered(): boolean {
  return has_opik_callback();
}

/**
 * Get current Opik configuration
 */
export function getOpikConfig(): Record<string, unknown> {
  return JSON.parse(get_opik_config());
}

/**
 * Register a custom Opik trace callback
 *
 * Use this for advanced use cases where you want to handle traces differently
 * (e.g., batching, custom endpoints, local logging)
 *
 * @param callback - Custom callback to handle traces
 *
 * @example
 * ```typescript
 * registerOpikCallback(async (traceJson) => {
 *   const trace = JSON.parse(traceJson);
 *   // Custom handling - e.g., batch traces, send to different endpoint
 *   console.log('Trace:', trace);
 * });
 * ```
 */
export function registerOpikCallback(callback: OpikTraceCallback): void {
  set_opik_callback(callback);
}

/**
 * Manually send a trace to Opik
 *
 * Useful for tracing custom operations or non-LLM actions
 *
 * @param trace - The trace to send
 */
export async function sendOpikTrace(trace: OpikTrace): Promise<void> {
  if (!has_opik_callback()) {
    throw new Error('No Opik callback registered. Call initOpikTracing() first.');
  }

  await send_opik_trace_async(JSON.stringify(trace));
}

/**
 * Create an LLM trace object (helper function)
 *
 * @param nodeName - Name of the node/operation
 * @param params - LLM parameters (prompt, etc.)
 * @param response - LLM response (content, usage, etc.)
 * @param startTime - ISO 8601 start timestamp
 * @param endTime - ISO 8601 end timestamp
 * @returns Trace JSON string
 */
export function createLlmTrace(
  nodeName: string,
  params: Record<string, unknown>,
  response: Record<string, unknown>,
  startTime: string,
  endTime: string
): string {
  return create_llm_trace(nodeName, JSON.stringify(params), JSON.stringify(response), startTime, endTime);
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
  // LLM handler
  set_llm_handler,
  clear_llm_handler,
  has_llm_handler,
  llm_call_async,
  llm_embed_async,
  // Lua callback bridge
  set_lua_callback,
  clear_lua_callback,
  has_lua_callback,
  lua_eval_async,
  // Prolog handler bridge
  set_prolog_handler,
  clear_prolog_handler,
  has_prolog_handler,
  prolog_query_async,
  // Opik tracing bridge (TEA-OBS-002)
  set_opik_callback,
  clear_opik_callback,
  has_opik_callback,
  configure_opik,
  get_opik_config,
  is_opik_enabled,
  send_opik_trace_async,
  create_llm_trace,
  // DuckDB bridge (TEA-WASM-003.2)
  set_duckdb_handler,
  clear_duckdb_handler,
  has_duckdb_handler,
  duckdb_query_async,
  duckdb_execute_async,
  init_duckdb_async,
  load_duckdb_extension_async,
  get_duckdb_extensions_async,
  duckdb_begin_async,
  duckdb_commit_async,
  duckdb_rollback_async,
  // Utilities
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

// ============================================================================
// DuckDB WASM Integration (TEA-WASM-003.2)
// ============================================================================

// Re-export from duckdb-loader for the DuckDB API
export {
  // Core initialization
  initDuckDb,
  initDuckDbWithHandler,
  getDuckDbConnection,
  createDuckDbHandler,
  // Query helpers
  queryDuckDb,
  executeDuckDb,
  // Extension loading
  loadDuckDbExtension,
  // Lifecycle
  closeDuckDb,
  isDuckDbInitialized,
  getDuckDbVersion,
  // Constants
  DUCKDB_EXTENSIONS,
};

// ============================================================================
// LTM (Long-Term Memory) Integration (TEA-WASM-003.3)
// ============================================================================

// Re-export from ltm-loader for the LTM API
export {
  // Core initialization
  initLtm,
  initLtmWithHandler,
  createLtmHandler,
  // Data operations
  clearLtmData,
  getLtmStats,
  // Sync operations
  startSyncWorker,
  stopSyncWorker,
  // Lifecycle
  isIndexedDBAvailable,
  isLtmInitialized,
  closeLtm,
  // Types
  type LtmLoaderOptions,
  type LtmEntry,
  type LtmHandler,
};

// Re-export raw WASM LTM bindings
export {
  set_ltm_handler,
  clear_ltm_handler,
  has_ltm_handler,
  configure_ltm,
  get_ltm_config,
  ltm_store_async,
  ltm_retrieve_async,
  ltm_delete_async,
  ltm_search_async,
  ltm_list_async,
  ltm_cleanup_expired_async,
  ltm_stats_async,
};

// Note: initTeaLlm and executeLlmYaml are already exported above with @deprecated notices

// Re-export game functions (TEA-GAME-001.7)
export {
  game_init,
  game_start_session,
  game_generate_round,
  game_submit_answer,
  game_submit_to_leaderboard,
  game_get_leaderboard,
  game_get_session_stats,
  game_set_llm_handler,
  game_clear_llm_handler,
  game_has_llm_handler,
  // Re-export with game_ prefix for app.js compatibility
  set_game_opik_handler as game_set_opik_handler,
  clear_game_opik_handler as game_clear_opik_handler,
  has_game_opik_handler as game_has_opik_handler,
};
