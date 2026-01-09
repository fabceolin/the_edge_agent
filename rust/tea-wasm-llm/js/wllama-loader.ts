/**
 * TEA WASM LLM - Internal wllama Loader
 *
 * This module loads wllama internally, eliminating the need for external
 * npm dependencies. The wllama WASM files are bundled as static assets.
 *
 * Architecture:
 * - wllama WASM files are copied to assets/ during build
 * - This loader initializes wllama with the bundled assets
 * - Auto-detects single-thread vs multi-thread support
 * - Provides simplified API: initLlm, chat, embed, chatStream
 */

// @ts-expect-error - wllama types may not be perfect
import { Wllama } from '@wllama/wllama';

// Type for wllama configuration - defined locally to avoid type import issues
interface WllamaConfig {
  n_threads?: number;
  [key: string]: unknown;
}

import {
  loadBundledModel,
  type BundledModelConfig,
} from './model-loader';

import {
  getCacheStats,
  clearCache,
  type CacheStats,
} from './model-cache';

/**
 * Wllama configuration paths for bundled WASM files
 * These files are copied to the package during build
 */
export interface WllamaAssetPaths {
  'single-thread/wllama.wasm': string;
  'multi-thread/wllama.wasm': string;
}

/**
 * Default asset paths relative to package root
 */
export function getDefaultAssetPaths(basePath: string = '.'): WllamaAssetPaths {
  return {
    'single-thread/wllama.wasm': `${basePath}/assets/single-thread/wllama.wasm`,
    'multi-thread/wllama.wasm': `${basePath}/assets/multi-thread/wllama.wasm`,
  };
}

/**
 * LLM initialization configuration
 */
export interface InitLlmConfig {
  /** URL or path to the GGUF model file */
  modelUrl?: string;

  /** Base path for model files (used with manifest) */
  modelBasePath?: string;

  /** Progress callback during model download */
  onProgress?: (loaded: number, total: number) => void;

  /** Callback when LLM is ready */
  onReady?: () => void;

  /** Use IndexedDB cache for model (default: true) */
  useCache?: boolean;

  /** Force single-thread mode (default: auto-detect) */
  singleThread?: boolean;

  /** Number of threads to use (default: auto) */
  nThreads?: number;

  /** Number of GPU layers (-1 = auto, 0 = CPU only) */
  nGpuLayers?: number;

  /** Context size (default: 2048) */
  nCtx?: number;

  /** Verbose logging */
  verbose?: boolean;

  /** Custom asset paths for wllama WASM files */
  assetPaths?: WllamaAssetPaths;

  /** Use CDN for wllama WASM files instead of bundled */
  useCdn?: boolean;
}

/**
 * Chat completion options
 */
export interface ChatOptions {
  /** Maximum tokens to generate (default: 100) */
  maxTokens?: number;

  /** Temperature (0.0 - 2.0, default: 0.7) */
  temperature?: number;

  /** Top-p nucleus sampling (default: 0.9) */
  topP?: number;

  /** Top-k sampling (0 = disabled) */
  topK?: number;

  /** Stop sequences */
  stop?: string[];

  /** System prompt */
  system?: string;
}

/**
 * Chat response
 */
export interface ChatResponse {
  /** Generated text content */
  content: string;

  /** Token usage statistics */
  usage?: {
    promptTokens: number;
    completionTokens: number;
    totalTokens: number;
  };
}

/**
 * Embedding response
 */
export interface EmbedResponse {
  /** Embedding vector */
  vector: Float32Array;

  /** Dimension of the embedding */
  dimension: number;
}

/**
 * Token callback for streaming
 */
export type TokenCallback = (token: string) => void;

// Global wllama instance
let wllamaInstance: Wllama | null = null;
let isInitialized = false;
let modelLoaded = false;

/**
 * Get CDN paths for wllama WASM files
 * Uses jsDelivr CDN with the @wllama/wllama package
 */
function getCdnPaths(): WllamaAssetPaths {
  const CDN_BASE = 'https://cdn.jsdelivr.net/npm/@wllama/wllama/esm';
  return {
    'single-thread/wllama.wasm': `${CDN_BASE}/single-thread/wllama.wasm`,
    'multi-thread/wllama.wasm': `${CDN_BASE}/multi-thread/wllama.wasm`,
  };
}

/**
 * Check if SharedArrayBuffer is available (for multi-threading)
 */
export function hasSharedArrayBuffer(): boolean {
  try {
    return typeof SharedArrayBuffer !== 'undefined';
  } catch {
    return false;
  }
}

/**
 * Check if COOP/COEP headers are properly set for multi-threading
 */
export function hasCoopCoep(): boolean {
  try {
    // Check if we can use SharedArrayBuffer
    if (!hasSharedArrayBuffer()) return false;

    // Try to actually create a SharedArrayBuffer
    new SharedArrayBuffer(1);
    return true;
  } catch {
    return false;
  }
}

/**
 * Initialize the bundled LLM engine
 *
 * This function:
 * 1. Loads the bundled wllama WASM files
 * 2. Downloads/caches the model from modelUrl
 * 3. Initializes wllama with the model
 *
 * @example
 * ```typescript
 * await initLlm({
 *   modelUrl: 'https://huggingface.co/.../Phi-4-mini-Q3_K_S.gguf',
 *   onProgress: (loaded, total) => console.log(`${loaded}/${total}`),
 *   onReady: () => console.log('Ready!'),
 * });
 * ```
 */
export async function initLlm(config: InitLlmConfig = {}): Promise<void> {
  const {
    modelUrl,
    modelBasePath,
    onProgress,
    onReady,
    useCache = true,
    singleThread = false,
    nThreads,
    nGpuLayers = 0,
    nCtx = 2048,
    verbose = false,
    assetPaths,
    useCdn = false,
  } = config;

  const log = (msg: string) => {
    if (verbose) console.log(`[TEA-LLM] ${msg}`);
  };

  // Determine threading capability
  const multiThreadSupported = hasCoopCoep() && !singleThread;
  log(`Multi-threading: ${multiThreadSupported ? 'enabled' : 'disabled'}`);

  // Get wllama asset paths
  const paths = useCdn ? getCdnPaths() : (assetPaths || getDefaultAssetPaths());
  log(`Using wllama assets: ${useCdn ? 'CDN' : 'bundled'}`);

  // Initialize wllama
  if (!wllamaInstance) {
    log('Creating wllama instance...');

    const wllamaConfig: Partial<WllamaConfig> = {
      // Optionally disable multi-threading
      ...(singleThread ? { n_threads: 1 } : {}),
      ...(nThreads ? { n_threads: nThreads } : {}),
    };

    wllamaInstance = new Wllama(paths, wllamaConfig);
    isInitialized = true;
  }

  // Load model if URL provided
  if (modelUrl || modelBasePath) {
    log('Loading model...');

    let modelData: Uint8Array;

    if (modelBasePath) {
      // Use manifest-based loading with caching
      modelData = await loadBundledModel({
        modelBasePath,
        useCache,
        onProgress,
        verbose,
      } as BundledModelConfig);
    } else if (modelUrl) {
      // Use wllama's native URL loading (streams directly, no memory doubling)
      log(`Loading from URL: ${modelUrl}`);

      await wllamaInstance.loadModelFromUrl(modelUrl, {
        n_gpu_layers: nGpuLayers,
        n_ctx: nCtx,
        useCache,
        progressCallback: onProgress
          ? ({ loaded, total }: { loaded: number; total: number }) => {
              onProgress(loaded, total);
            }
          : undefined,
      });

      modelLoaded = true;
      log('Model loaded successfully');
      onReady?.();
      return;
    } else {
      throw new Error('Either modelUrl or modelBasePath must be provided');
    }

    // Load into wllama (only for modelBasePath case)
    log('Loading model into wllama...');
    await wllamaInstance.loadModel(modelData, {
      n_gpu_layers: nGpuLayers,
      n_ctx: nCtx,
    });

    modelLoaded = true;
    log('Model loaded successfully');
  }

  onReady?.();
}

/**
 * Check if the LLM is initialized and ready
 */
export function isLlmReady(): boolean {
  return isInitialized && modelLoaded;
}

/**
 * Generate a chat completion
 *
 * @example
 * ```typescript
 * const response = await chat("What is 2+2?", {
 *   maxTokens: 50,
 *   temperature: 0.7,
 * });
 * console.log(response.content);
 * ```
 */
export async function chat(
  prompt: string,
  options: ChatOptions = {}
): Promise<ChatResponse> {
  if (!wllamaInstance || !modelLoaded) {
    throw new Error('LLM not initialized. Call initLlm() first.');
  }

  const {
    maxTokens = 100,
    temperature = 0.7,
    topP = 0.9,
    topK = 0,
    stop = [],
    system,
  } = options;

  // Build full prompt with system message if provided
  let fullPrompt = prompt;
  if (system) {
    // Use standard chat template format
    fullPrompt = `<|system|>\n${system}\n<|user|>\n${prompt}\n<|assistant|>\n`;
  }

  const result = await wllamaInstance.createCompletion(fullPrompt, {
    nPredict: maxTokens,
    sampling: {
      temp: temperature,
      top_p: topP,
      top_k: topK,
    },
    stopTokens: stop,
  });

  return {
    content: result,
    usage: {
      // wllama doesn't provide token counts directly, estimate from length
      promptTokens: Math.ceil(fullPrompt.length / 4),
      completionTokens: Math.ceil(result.length / 4),
      totalTokens: Math.ceil((fullPrompt.length + result.length) / 4),
    },
  };
}

/**
 * Generate a chat completion with streaming tokens
 *
 * @example
 * ```typescript
 * await chatStream("Tell me a story", (token) => {
 *   process.stdout.write(token);
 * }, { maxTokens: 200 });
 * ```
 */
export async function chatStream(
  prompt: string,
  onToken: TokenCallback,
  options: ChatOptions = {}
): Promise<ChatResponse> {
  if (!wllamaInstance || !modelLoaded) {
    throw new Error('LLM not initialized. Call initLlm() first.');
  }

  const {
    maxTokens = 100,
    temperature = 0.7,
    topP = 0.9,
    topK = 0,
    stop = [],
    system,
  } = options;

  // Build full prompt with system message if provided
  let fullPrompt = prompt;
  if (system) {
    fullPrompt = `<|system|>\n${system}\n<|user|>\n${prompt}\n<|assistant|>\n`;
  }

  let fullContent = '';

  await wllamaInstance.createCompletion(fullPrompt, {
    nPredict: maxTokens,
    sampling: {
      temp: temperature,
      top_p: topP,
      top_k: topK,
    },
    stopTokens: stop,
    onNewToken: (_token: number, piece: string) => {
      fullContent += piece;
      onToken(piece);
    },
  });

  return {
    content: fullContent,
    usage: {
      promptTokens: Math.ceil(fullPrompt.length / 4),
      completionTokens: Math.ceil(fullContent.length / 4),
      totalTokens: Math.ceil((fullPrompt.length + fullContent.length) / 4),
    },
  };
}

/**
 * Generate text embeddings
 *
 * @example
 * ```typescript
 * const embedding = await embed("Hello world");
 * console.log(embedding.vector); // Float32Array
 * ```
 */
export async function embed(text: string): Promise<EmbedResponse> {
  if (!wllamaInstance || !modelLoaded) {
    throw new Error('LLM not initialized. Call initLlm() first.');
  }

  const vector = await wllamaInstance.createEmbedding(text);

  return {
    vector: new Float32Array(vector),
    dimension: vector.length,
  };
}

/**
 * Dispose of the LLM instance and free resources
 */
export async function disposeLlm(): Promise<void> {
  if (wllamaInstance) {
    await wllamaInstance.exit();
    wllamaInstance = null;
    isInitialized = false;
    modelLoaded = false;
  }
}

/**
 * Get the underlying wllama instance for advanced usage
 * (backward compatibility for users migrating from callback API)
 */
export function getWllamaInstance(): Wllama | null {
  return wllamaInstance;
}

/**
 * Get model cache statistics
 */
export async function getLlmCacheStats(): Promise<CacheStats> {
  return getCacheStats();
}

/**
 * Clear the model cache
 */
export async function clearLlmCache(): Promise<void> {
  return clearCache();
}
