/* tslint:disable */
/* eslint-disable */

/**
 * Clear the LLM handler
 */
export function clear_llm_handler(): void;

/**
 * Execute a YAML workflow with LLM actions
 *
 * # Arguments
 * * `yaml` - YAML workflow definition
 * * `initial_state` - Initial state as JSON string
 *
 * # Returns
 * * Result JSON string on success
 */
export function execute_yaml(yaml: string, initial_state: string): Promise<string>;

/**
 * Check if an LLM handler is registered
 */
export function has_llm_handler(): boolean;

/**
 * Check if SharedArrayBuffer is available (for multi-threading detection)
 */
export function has_shared_array_buffer(): boolean;

/**
 * Call the LLM asynchronously
 *
 * # Arguments
 * * `params_json` - JSON string with LlmParams
 * * `state_json` - Current state as JSON string
 *
 * # Returns
 * * Updated state with `llm_response` field containing the LLM response
 */
export function llm_call_async(params_json: string, state_json: string): Promise<string>;

/**
 * Get embeddings from the LLM
 *
 * Note: Requires wllama model with embedding support
 */
export function llm_embed_async(text: string, state_json: string): Promise<string>;

/**
 * Initialize WASM module with panic hook
 */
export function main(): void;

/**
 * Register a JavaScript function to handle LLM calls
 *
 * The function should accept a JSON string (LlmParams) and return a Promise
 * that resolves to a JSON string (LlmResponse).
 *
 * # Example
 *
 * ```javascript
 * set_llm_handler(async (paramsJson) => {
 *     const params = JSON.parse(paramsJson);
 *     const result = await wllama.createCompletion(params.prompt, {
 *         nPredict: params.max_tokens,
 *     });
 *     return JSON.stringify({ content: result });
 * });
 * ```
 */
export function set_llm_handler(handler: Function): void;

/**
 * Get library version
 */
export function version(): string;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly main: () => void;
  readonly execute_yaml: (a: number, b: number, c: number, d: number) => any;
  readonly has_shared_array_buffer: () => number;
  readonly version: () => [number, number];
  readonly set_llm_handler: (a: any) => void;
  readonly clear_llm_handler: () => void;
  readonly has_llm_handler: () => number;
  readonly llm_call_async: (a: number, b: number, c: number, d: number) => any;
  readonly llm_embed_async: (a: number, b: number, c: number, d: number) => any;
  readonly wasm_bindgen__convert__closures_____invoke__h68f9198a4942034a: (a: number, b: number, c: any) => void;
  readonly wasm_bindgen__closure__destroy__h3a7aefe1dd4d59f2: (a: number, b: number) => void;
  readonly wasm_bindgen__convert__closures_____invoke__h3a8deea6e374e4a1: (a: number, b: number, c: any, d: any) => void;
  readonly __wbindgen_malloc: (a: number, b: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
  readonly __wbindgen_exn_store: (a: number) => void;
  readonly __externref_table_alloc: () => number;
  readonly __wbindgen_externrefs: WebAssembly.Table;
  readonly __wbindgen_free: (a: number, b: number, c: number) => void;
  readonly __wbindgen_start: () => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;

/**
* Instantiates the given `module`, which can either be bytes or
* a precompiled `WebAssembly.Module`.
*
* @param {{ module: SyncInitInput }} module - Passing `SyncInitInput` directly is deprecated.
*
* @returns {InitOutput}
*/
export function initSync(module: { module: SyncInitInput } | SyncInitInput): InitOutput;

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {{ module_or_path: InitInput | Promise<InitInput> }} module_or_path - Passing `InitInput` directly is deprecated.
*
* @returns {Promise<InitOutput>}
*/
export default function __wbg_init (module_or_path?: { module_or_path: InitInput | Promise<InitInput> } | InitInput | Promise<InitInput>): Promise<InitOutput>;
