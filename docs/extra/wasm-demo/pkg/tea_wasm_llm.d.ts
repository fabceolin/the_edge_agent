/* tslint:disable */
/* eslint-disable */
/**
* Initialize WASM module with panic hook
*/
export function main(): void;
/**
* Execute a YAML workflow with LLM actions
*
* # Arguments
* * `yaml` - YAML workflow definition
* * `initial_state` - Initial state as JSON string
*
* # Returns
* * Result JSON string on success
* @param {string} yaml
* @param {string} initial_state
* @returns {Promise<string>}
*/
export function execute_yaml(yaml: string, initial_state: string): Promise<string>;
/**
* Check if SharedArrayBuffer is available (for multi-threading detection)
* @returns {boolean}
*/
export function has_shared_array_buffer(): boolean;
/**
* Get library version
* @returns {string}
*/
export function version(): string;
/**
* Register a JavaScript function to handle Lua evaluation
*
* The function should accept (code: string, stateJson: string) and return
* a Promise that resolves to a JSON string (LuaResponse).
*
* # Example
*
* ```javascript
* import { LuaFactory } from 'wasmoon';
*
* const lua = await (new LuaFactory()).createEngine();
*
* set_lua_callback(async (code, stateJson) => {
*     const state = JSON.parse(stateJson);
*     lua.global.set('state', state);
*     const result = await lua.doString(code);
*     return JSON.stringify({ result });
* });
* ```
* @param {Function} callback
*/
export function set_lua_callback(callback: Function): void;
/**
* Clear the Lua callback
*/
export function clear_lua_callback(): void;
/**
* Check if a Lua callback is registered
* @returns {boolean}
*/
export function has_lua_callback(): boolean;
/**
* Evaluate Lua code asynchronously
*
* # Arguments
* * `code` - Lua code to execute
* * `state_json` - Current state as JSON string
*
* # Returns
* * Updated state with result from Lua execution
* @param {string} code
* @param {string} state_json
* @returns {Promise<string>}
*/
export function lua_eval_async(code: string, state_json: string): Promise<string>;
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
* @param {Function} handler
*/
export function set_llm_handler(handler: Function): void;
/**
* Clear the LLM handler
*/
export function clear_llm_handler(): void;
/**
* Check if an LLM handler is registered
* @returns {boolean}
*/
export function has_llm_handler(): boolean;
/**
* Call the LLM asynchronously
*
* # Arguments
* * `params_json` - JSON string with LlmParams
* * `state_json` - Current state as JSON string
*
* # Returns
* * Updated state with `llm_response` field containing the LLM response
* @param {string} params_json
* @param {string} state_json
* @returns {Promise<string>}
*/
export function llm_call_async(params_json: string, state_json: string): Promise<string>;
/**
* Get embeddings from the LLM
*
* Note: Requires wllama model with embedding support
* @param {string} text
* @param {string} state_json
* @returns {Promise<string>}
*/
export function llm_embed_async(text: string, state_json: string): Promise<string>;
/**
* Register a JavaScript function to handle Prolog queries
*
* The function should accept (queryJson: string) and return
* a Promise that resolves to a JSON string (PrologResponse).
*
* The queryJson contains:
* - code: The Prolog query to execute
* - facts: Optional facts to assert before the query
*
* # Example (trealla)
*
* ```javascript
* import { Prolog } from 'trealla';
*
* const pl = new Prolog();
*
* set_prolog_handler(async (queryJson) => {
*     const { code, facts } = JSON.parse(queryJson);
*     if (facts) await pl.consultText(facts);
*     const results = await pl.queryOnce(code);
*     return JSON.stringify({ bindings: results ? [results] : [], success: !!results });
* });
* ```
* @param {Function} handler
*/
export function set_prolog_handler(handler: Function): void;
/**
* Clear the Prolog handler
*/
export function clear_prolog_handler(): void;
/**
* Check if a Prolog handler is registered
* @returns {boolean}
*/
export function has_prolog_handler(): boolean;
/**
* Execute a Prolog query asynchronously
*
* # Arguments
* * `query_json` - JSON string with PrologParams (code, facts)
* * `state_json` - Current state as JSON string
*
* # Returns
* * Updated state with prolog_result containing bindings
* @param {string} query_json
* @param {string} state_json
* @returns {Promise<string>}
*/
export function prolog_query_async(query_json: string, state_json: string): Promise<string>;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly main: () => void;
  readonly execute_yaml: (a: number, b: number, c: number, d: number) => number;
  readonly has_shared_array_buffer: () => number;
  readonly version: (a: number) => void;
  readonly set_lua_callback: (a: number) => void;
  readonly clear_lua_callback: () => void;
  readonly has_lua_callback: () => number;
  readonly lua_eval_async: (a: number, b: number, c: number, d: number) => number;
  readonly set_llm_handler: (a: number) => void;
  readonly clear_llm_handler: () => void;
  readonly has_llm_handler: () => number;
  readonly llm_call_async: (a: number, b: number, c: number, d: number) => number;
  readonly llm_embed_async: (a: number, b: number, c: number, d: number) => number;
  readonly set_prolog_handler: (a: number) => void;
  readonly clear_prolog_handler: () => void;
  readonly has_prolog_handler: () => number;
  readonly prolog_query_async: (a: number, b: number, c: number, d: number) => number;
  readonly __wbindgen_malloc: (a: number, b: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
  readonly __wbindgen_export_2: WebAssembly.Table;
  readonly _dyn_core__ops__function__FnMut__A____Output___R_as_wasm_bindgen__closure__WasmClosure___describe__invoke__ha822a74dc9a4a732: (a: number, b: number, c: number) => void;
  readonly __wbindgen_add_to_stack_pointer: (a: number) => number;
  readonly __wbindgen_free: (a: number, b: number, c: number) => void;
  readonly __wbindgen_exn_store: (a: number) => void;
  readonly wasm_bindgen__convert__closures__invoke2_mut__h1fc9b241df9e2ddd: (a: number, b: number, c: number, d: number) => void;
  readonly __wbindgen_start: () => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;
/**
* Instantiates the given `module`, which can either be bytes or
* a precompiled `WebAssembly.Module`.
*
* @param {SyncInitInput} module
*
* @returns {InitOutput}
*/
export function initSync(module: SyncInitInput): InitOutput;

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {InitInput | Promise<InitInput>} module_or_path
*
* @returns {Promise<InitOutput>}
*/
export default function __wbg_init (module_or_path?: InitInput | Promise<InitInput>): Promise<InitOutput>;
