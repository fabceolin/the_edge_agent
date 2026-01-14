/* tslint:disable */
/* eslint-disable */
/**
 * The `ReadableStreamType` enum.
 *
 * *This API requires the following crate features to be activated: `ReadableStreamType`*
 */

type ReadableStreamType = "bytes";

export class IntoUnderlyingByteSource {
  private constructor();
  free(): void;
  [Symbol.dispose](): void;
  pull(controller: ReadableByteStreamController): Promise<any>;
  start(controller: ReadableByteStreamController): void;
  cancel(): void;
  readonly autoAllocateChunkSize: number;
  readonly type: ReadableStreamType;
}

export class IntoUnderlyingSink {
  private constructor();
  free(): void;
  [Symbol.dispose](): void;
  abort(reason: any): Promise<any>;
  close(): Promise<any>;
  write(chunk: any): Promise<any>;
}

export class IntoUnderlyingSource {
  private constructor();
  free(): void;
  [Symbol.dispose](): void;
  pull(controller: ReadableStreamDefaultController): Promise<any>;
  cancel(): void;
}

/**
 * Clear the DuckDB handler
 */
export function clear_duckdb_handler(): void;

/**
 * Clear the LLM handler
 */
export function clear_llm_handler(): void;

/**
 * Clear LTM handler
 */
export function clear_ltm_handler(): void;

/**
 * Clear the Lua callback
 */
export function clear_lua_callback(): void;

/**
 * Clear the Opik callback
 */
export function clear_opik_callback(): void;

/**
 * Clear the Prolog handler
 */
export function clear_prolog_handler(): void;

/**
 * Clear all stored credentials
 */
export function clear_storage_credentials(): void;

/**
 * Configure LTM backend
 *
 * # Arguments
 * * `config_json` - JSON configuration with storage_uri, inline_threshold, enable_sync
 *
 * # Example (JavaScript)
 * ```javascript
 * configure_ltm(JSON.stringify({
 *     storage_uri: 's3://my-bucket/ltm/',
 *     inline_threshold: 1024,
 *     enable_sync: true
 * }));
 * ```
 */
export function configure_ltm(config_json: string): string;

/**
 * Configure Opik settings
 *
 * # Arguments
 * * `config_json` - JSON string with OpikConfig fields
 */
export function configure_opik(config_json: string): void;

/**
 * Create an OpikTrace from LLM call parameters and response
 *
 * # Arguments
 * * `node_name` - Name of the node executing the LLM call
 * * `params_json` - LLM params as JSON string
 * * `response_json` - LLM response as JSON string
 * * `start_time` - ISO 8601 start timestamp
 * * `end_time` - ISO 8601 end timestamp
 */
export function create_llm_trace(node_name: string, params_json: string, response_json: string, start_time: string, end_time: string): string;

/**
 * Transaction helper: Begin a transaction
 */
export function duckdb_begin_async(): Promise<string>;

/**
 * Transaction helper: Commit a transaction
 */
export function duckdb_commit_async(): Promise<string>;

/**
 * Execute a SQL statement without returning results (DDL/DML)
 *
 * # Arguments
 * * `sql` - SQL statement to execute (CREATE, INSERT, UPDATE, DELETE, etc.)
 *
 * # Returns
 * * JSON string with execution result
 *
 * # Example
 *
 * ```javascript
 * // Create table
 * await duckdb_execute_async("CREATE TABLE users (id INTEGER, name VARCHAR)");
 *
 * // Insert data
 * await duckdb_execute_async("INSERT INTO users VALUES (1, 'Alice')");
 * ```
 */
export function duckdb_execute_async(sql: string): Promise<string>;

/**
 * Execute a SQL query and return results as JSON
 *
 * # Arguments
 * * `sql` - SQL query to execute
 * * `params_json` - Parameters as JSON array string (e.g., "[1, \"hello\"]")
 *
 * # Returns
 * * JSON string with query results (DuckDbQueryResponse format)
 *
 * # Example
 *
 * ```javascript
 * // Simple query
 * const result = await duckdb_query_async("SELECT 1 + 1 as sum", "[]");
 * // { success: true, rows: [{ sum: 2 }], row_count: 1 }
 *
 * // Parameterized query
 * const result = await duckdb_query_async(
 *     "SELECT * FROM users WHERE id = ?",
 *     JSON.stringify([42])
 * );
 * ```
 */
export function duckdb_query_async(sql: string, params_json: string): Promise<string>;

/**
 * Transaction helper: Rollback a transaction
 */
export function duckdb_rollback_async(): Promise<string>;

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
 * Get list of available/loaded extensions
 */
export function get_duckdb_extensions_async(): Promise<string>;

/**
 * Get current LTM configuration
 */
export function get_ltm_config(): string;

/**
 * Get current Opik configuration as JSON
 */
export function get_opik_config(): string;

/**
 * Check if a DuckDB handler is registered
 */
export function has_duckdb_handler(): boolean;

/**
 * Check if an LLM handler is registered
 */
export function has_llm_handler(): boolean;

/**
 * Check if LTM handler is registered
 */
export function has_ltm_handler(): boolean;

/**
 * Check if a Lua callback is registered
 */
export function has_lua_callback(): boolean;

/**
 * Check if an Opik callback is registered
 */
export function has_opik_callback(): boolean;

/**
 * Check if a Prolog handler is registered
 */
export function has_prolog_handler(): boolean;

/**
 * Check if SharedArrayBuffer is available (for multi-threading detection)
 */
export function has_shared_array_buffer(): boolean;

/**
 * Check if credentials are set for a provider
 */
export function has_storage_credentials(provider: string): boolean;

/**
 * Initialize DuckDB (validation that handler is set up)
 *
 * # Arguments
 * * `options_json` - Optional JSON string with DuckDbInitOptions
 *
 * # Returns
 * * JSON string confirming initialization
 */
export function init_duckdb_async(options_json?: string | null): Promise<string>;

/**
 * Initialize memory backend for testing
 */
export function init_memory(): string;

/**
 * Stub for init_opfs when OPFS feature is not enabled or not on WASM
 */
export function init_opfs(): Promise<string>;

/**
 * Check if memory backend is available
 */
export function is_memory_available(): boolean;

/**
 * Check if OPFS is available and initialized
 */
export function is_opfs_available(): boolean;

/**
 * Check if Opik tracing is enabled
 */
export function is_opik_enabled(): boolean;

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
 * Load a DuckDB extension
 *
 * # Arguments
 * * `extension_name` - Name of the extension (e.g., "vss", "fts", "parquet", "httpfs")
 *
 * # Returns
 * * JSON string with load result
 *
 * # Supported Extensions
 *
 * | Extension | Description | Size |
 * |-----------|-------------|------|
 * | parquet | Columnar file format | ~2MB (autoloaded) |
 * | json | JSON operations | ~500KB (autoloaded) |
 * | vss | Vector similarity search (HNSW) | ~1MB |
 * | fts | Full-text search | ~800KB |
 * | spatial | Geospatial operations | ~3MB |
 * | icu | Timezones, collations | ~2MB |
 * | httpfs | Remote file access (CORS) | ~500KB |
 */
export function load_duckdb_extension_async(extension_name: string): Promise<string>;

/**
 * Clean up expired entries
 *
 * # Returns
 * JSON result with cleanup statistics
 */
export function ltm_cleanup_expired_async(): Promise<string>;

/**
 * Delete an entry from LTM
 *
 * # Arguments
 * * `key` - Key to delete
 *
 * # Returns
 * JSON result with deletion status
 */
export function ltm_delete_async(key: string): Promise<string>;

/**
 * List LTM entries by prefix
 *
 * # Arguments
 * * `prefix` - Key prefix to filter
 * * `limit` - Maximum results
 *
 * # Returns
 * JSON result with matching entries (metadata only, no values)
 */
export function ltm_list_async(prefix: string, limit: number): Promise<string>;

/**
 * Retrieve a value from LTM
 *
 * # Arguments
 * * `key` - Key to retrieve
 * * `default_json` - Default value if key not found (JSON string)
 *
 * # Returns
 * JSON result with value, content_hash, and metadata
 */
export function ltm_retrieve_async(key: string, default_json: string): Promise<string>;

/**
 * Search LTM entries with optional FTS
 *
 * # Arguments
 * * `query` - Search query (prefix match or FTS if DuckDB available)
 * * `metadata_filter_json` - JSON filter for metadata fields
 * * `limit` - Maximum results
 *
 * # Returns
 * JSON result with matching entries
 */
export function ltm_search_async(query: string, metadata_filter_json: string, limit: number): Promise<string>;

/**
 * Get LTM statistics
 *
 * # Returns
 * JSON result with entry count, total size, etc.
 */
export function ltm_stats_async(): Promise<string>;

/**
 * Store a value in LTM
 *
 * # Arguments
 * * `key` - Unique key for the entry
 * * `value_json` - JSON-serialized value
 * * `metadata_json` - JSON metadata (can include _cache_expires_at for TTL)
 *
 * # Returns
 * JSON result with success status, content_hash, and deduplication info
 */
export function ltm_store_async(key: string, value_json: string, metadata_json: string): Promise<string>;

/**
 * Evaluate Lua code asynchronously
 *
 * # Arguments
 * * `code` - Lua code to execute
 * * `state_json` - Current state as JSON string
 *
 * # Returns
 * * Updated state with result from Lua execution
 */
export function lua_eval_async(code: string, state_json: string): Promise<string>;

/**
 * Initialize WASM module with panic hook
 */
export function main(): void;

/**
 * Execute a Prolog query asynchronously
 *
 * # Arguments
 * * `query_json` - JSON string with PrologParams (code, facts)
 * * `state_json` - Current state as JSON string
 *
 * # Returns
 * * Updated state with prolog_result containing bindings
 */
export function prolog_query_async(query_json: string, state_json: string): Promise<string>;

/**
 * Send a trace asynchronously (waits for JS Promise)
 *
 * Use this when you need to wait for the trace to be acknowledged.
 */
export function send_opik_trace_async(trace_json: string): Promise<void>;

/**
 * Register a JavaScript function to handle DuckDB queries
 *
 * The function should accept two arguments:
 * 1. SQL query string
 * 2. Parameters JSON string (array)
 *
 * And return a Promise that resolves to a JSON string (DuckDbQueryResponse).
 *
 * # Example
 *
 * ```javascript
 * set_duckdb_handler(async (sql, paramsJson) => {
 *     const params = JSON.parse(paramsJson);
 *     try {
 *         const result = await conn.query(sql, ...params);
 *         const rows = result.toArray().map(row => {
 *             const obj = {};
 *             for (const field of result.schema.fields) {
 *                 obj[field.name] = row[field.name];
 *             }
 *             return obj;
 *         });
 *         return JSON.stringify({
 *             success: true,
 *             rows: rows,
 *             row_count: rows.length,
 *             schema: result.schema.fields.map(f => ({
 *                 name: f.name,
 *                 type: f.type.toString()
 *             }))
 *         });
 *     } catch (error) {
 *         return JSON.stringify({
 *             success: false,
 *             error: error.message,
 *             error_code: classifyError(error)
 *         });
 *     }
 * });
 * ```
 */
export function set_duckdb_handler(handler: Function): void;

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
 * Register IndexedDB handler for LTM catalog operations
 *
 * The handler object should have these methods:
 * - `get(store: string, id: string) -> Promise<string | null>`
 * - `put(store: string, data: string) -> Promise<void>`
 * - `delete(store: string, id: string) -> Promise<void>`
 * - `list(store: string, prefix: string, limit: number) -> Promise<string>`
 * - `store_blob(id: string, data: string) -> Promise<void>`
 * - `get_blob(id: string) -> Promise<string | null>`
 * - `delete_blob(id: string) -> Promise<void>`
 *
 * # Example (JavaScript)
 * ```javascript
 * set_ltm_handler({
 *     get: async (store, id) => {
 *         const tx = db.transaction(store, 'readonly');
 *         const result = await tx.objectStore(store).get(id);
 *         return result ? JSON.stringify(result) : null;
 *     },
 *     put: async (store, data) => {
 *         const obj = JSON.parse(data);
 *         const tx = db.transaction(store, 'readwrite');
 *         await tx.objectStore(store).put(obj);
 *     },
 *     delete: async (store, id) => {
 *         const tx = db.transaction(store, 'readwrite');
 *         await tx.objectStore(store).delete(id);
 *     },
 *     list: async (store, prefix, limit) => {
 *         const results = [];
 *         const tx = db.transaction(store, 'readonly');
 *         const cursor = tx.objectStore(store).openCursor();
 *         // ... iterate and filter by prefix
 *         return JSON.stringify(results);
 *     },
 *     store_blob: async (id, data) => {
 *         const tx = db.transaction('blobs', 'readwrite');
 *         await tx.objectStore('blobs').put({ id, data });
 *     },
 *     get_blob: async (id) => {
 *         const tx = db.transaction('blobs', 'readonly');
 *         const result = await tx.objectStore('blobs').get(id);
 *         return result?.data || null;
 *     },
 *     delete_blob: async (id) => {
 *         const tx = db.transaction('blobs', 'readwrite');
 *         await tx.objectStore('blobs').delete(id);
 *     }
 * });
 * ```
 */
export function set_ltm_handler(handler: object): void;

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
 */
export function set_lua_callback(callback: Function): void;

/**
 * Register a JavaScript function to handle Opik traces
 *
 * The function should accept a JSON string (OpikTrace) and optionally return
 * a Promise for async sending.
 *
 * # Example
 *
 * ```javascript
 * set_opik_callback(async (traceJson) => {
 *     const trace = JSON.parse(traceJson);
 *     await sendToOpik(trace);
 * });
 * ```
 */
export function set_opik_callback(callback: Function): void;

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
 */
export function set_prolog_handler(handler: Function): void;

/**
 * Set credentials for a storage provider
 *
 * # Arguments
 * * `provider` - Provider name: "s3", "gcs", "azblob"
 * * `credentials_json` - JSON object with provider-specific credentials
 *
 * # Example (JavaScript)
 * ```javascript
 * set_storage_credentials('s3', JSON.stringify({
 *     access_key_id: 'AKIA...',
 *     secret_access_key: '...',
 *     region: 'us-east-1',
 *     endpoint: 'https://s3.amazonaws.com'  // optional, for MinIO/R2
 * }));
 * ```
 */
export function set_storage_credentials(provider: string, credentials_json: string): void;

/**
 * Copy content from one URI to another (cross-provider supported)
 *
 * This enables workflows like:
 * - Download from S3 to OPFS for offline use
 * - Upload from OPFS to S3 for cloud backup
 * - Transfer between cloud providers
 */
export function storage_copy_async(source_uri: string, dest_uri: string): Promise<string>;

/**
 * Delete object at URI
 */
export function storage_delete_async(uri: string): Promise<string>;

/**
 * Check if object exists at URI
 */
export function storage_exists_async(uri: string): Promise<string>;

/**
 * List objects at URI with optional prefix filtering
 *
 * # Arguments
 * * `uri` - Storage URI (directory/prefix to list)
 * * `options_json` - JSON options: { "limit": 1000 }
 */
export function storage_list_async(uri: string, options_json: string): Promise<string>;

/**
 * Read text content from URI
 *
 * # Arguments
 * * `uri` - Storage URI (s3://, gs://, az://, http://, opfs://, memory://)
 * * `_state_json` - Agent state (unused, for API compatibility)
 *
 * # Returns
 * JSON result with content or error
 */
export function storage_read_async(uri: string, _state_json: string): Promise<string>;

/**
 * Read binary content from URI (returns base64 encoded)
 *
 * # Arguments
 * * `uri` - Storage URI
 *
 * # Returns
 * JSON result with base64-encoded content
 */
export function storage_read_binary_async(uri: string): Promise<string>;

/**
 * Get information about supported storage schemes
 */
export function storage_supported_schemes(): string;

/**
 * Write text content to URI
 *
 * # Arguments
 * * `uri` - Storage URI
 * * `content` - Text content to write
 * * `_state_json` - Agent state (unused, for API compatibility)
 */
export function storage_write_async(uri: string, content: string, _state_json: string): Promise<string>;

/**
 * Write binary content to URI (expects base64 encoded input)
 *
 * # Arguments
 * * `uri` - Storage URI
 * * `content_base64` - Base64-encoded binary content
 */
export function storage_write_binary_async(uri: string, content_base64: string): Promise<string>;

/**
 * Get library version
 */
export function version(): string;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly execute_yaml: (a: number, b: number, c: number, d: number) => any;
  readonly has_shared_array_buffer: () => number;
  readonly main: () => void;
  readonly version: () => [number, number];
  readonly clear_storage_credentials: () => void;
  readonly has_storage_credentials: (a: number, b: number) => number;
  readonly init_memory: () => [number, number, number, number];
  readonly init_opfs: () => any;
  readonly is_memory_available: () => number;
  readonly is_opfs_available: () => number;
  readonly set_storage_credentials: (a: number, b: number, c: number, d: number) => [number, number];
  readonly storage_copy_async: (a: number, b: number, c: number, d: number) => any;
  readonly storage_delete_async: (a: number, b: number) => any;
  readonly storage_exists_async: (a: number, b: number) => any;
  readonly storage_list_async: (a: number, b: number, c: number, d: number) => any;
  readonly storage_read_async: (a: number, b: number, c: number, d: number) => any;
  readonly storage_read_binary_async: (a: number, b: number) => any;
  readonly storage_supported_schemes: () => [number, number];
  readonly storage_write_async: (a: number, b: number, c: number, d: number, e: number, f: number) => any;
  readonly storage_write_binary_async: (a: number, b: number, c: number, d: number) => any;
  readonly clear_ltm_handler: () => void;
  readonly configure_ltm: (a: number, b: number) => [number, number, number, number];
  readonly get_ltm_config: () => [number, number];
  readonly has_ltm_handler: () => number;
  readonly ltm_cleanup_expired_async: () => any;
  readonly ltm_delete_async: (a: number, b: number) => any;
  readonly ltm_list_async: (a: number, b: number, c: number) => any;
  readonly ltm_retrieve_async: (a: number, b: number, c: number, d: number) => any;
  readonly ltm_search_async: (a: number, b: number, c: number, d: number, e: number) => any;
  readonly ltm_stats_async: () => any;
  readonly ltm_store_async: (a: number, b: number, c: number, d: number, e: number, f: number) => any;
  readonly set_ltm_handler: (a: any) => void;
  readonly clear_lua_callback: () => void;
  readonly clear_opik_callback: () => void;
  readonly clear_prolog_handler: () => void;
  readonly configure_opik: (a: number, b: number) => [number, number];
  readonly create_llm_trace: (a: number, b: number, c: number, d: number, e: number, f: number, g: number, h: number, i: number, j: number) => [number, number, number, number];
  readonly get_opik_config: () => [number, number];
  readonly has_lua_callback: () => number;
  readonly has_opik_callback: () => number;
  readonly has_prolog_handler: () => number;
  readonly is_opik_enabled: () => number;
  readonly lua_eval_async: (a: number, b: number, c: number, d: number) => any;
  readonly prolog_query_async: (a: number, b: number, c: number, d: number) => any;
  readonly send_opik_trace_async: (a: number, b: number) => any;
  readonly set_lua_callback: (a: any) => void;
  readonly set_opik_callback: (a: any) => void;
  readonly set_prolog_handler: (a: any) => void;
  readonly clear_llm_handler: () => void;
  readonly has_llm_handler: () => number;
  readonly llm_call_async: (a: number, b: number, c: number, d: number) => any;
  readonly llm_embed_async: (a: number, b: number, c: number, d: number) => any;
  readonly set_llm_handler: (a: any) => void;
  readonly clear_duckdb_handler: () => void;
  readonly duckdb_begin_async: () => any;
  readonly duckdb_commit_async: () => any;
  readonly duckdb_execute_async: (a: number, b: number) => any;
  readonly duckdb_query_async: (a: number, b: number, c: number, d: number) => any;
  readonly duckdb_rollback_async: () => any;
  readonly get_duckdb_extensions_async: () => any;
  readonly has_duckdb_handler: () => number;
  readonly init_duckdb_async: (a: number, b: number) => any;
  readonly load_duckdb_extension_async: (a: number, b: number) => any;
  readonly set_duckdb_handler: (a: any) => void;
  readonly __wbg_intounderlyingbytesource_free: (a: number, b: number) => void;
  readonly intounderlyingbytesource_autoAllocateChunkSize: (a: number) => number;
  readonly intounderlyingbytesource_cancel: (a: number) => void;
  readonly intounderlyingbytesource_pull: (a: number, b: any) => any;
  readonly intounderlyingbytesource_start: (a: number, b: any) => void;
  readonly intounderlyingbytesource_type: (a: number) => number;
  readonly __wbg_intounderlyingsink_free: (a: number, b: number) => void;
  readonly __wbg_intounderlyingsource_free: (a: number, b: number) => void;
  readonly intounderlyingsink_abort: (a: number, b: any) => any;
  readonly intounderlyingsink_close: (a: number) => any;
  readonly intounderlyingsink_write: (a: number, b: any) => any;
  readonly intounderlyingsource_cancel: (a: number) => void;
  readonly intounderlyingsource_pull: (a: number, b: any) => any;
  readonly wasm_bindgen__convert__closures_____invoke__h993e6cec9bc4ab3e: (a: number, b: number, c: any) => void;
  readonly wasm_bindgen__closure__destroy__ha9e5309a8e8dc2f5: (a: number, b: number) => void;
  readonly wasm_bindgen__convert__closures_____invoke__hde2d95ef604b0a7f: (a: number, b: number) => void;
  readonly wasm_bindgen__closure__destroy__h9eb6f956610eaa1e: (a: number, b: number) => void;
  readonly wasm_bindgen__convert__closures_____invoke__hc599d4e810efe325: (a: number, b: number, c: any, d: any) => void;
  readonly __wbindgen_malloc: (a: number, b: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
  readonly __wbindgen_exn_store: (a: number) => void;
  readonly __externref_table_alloc: () => number;
  readonly __wbindgen_externrefs: WebAssembly.Table;
  readonly __wbindgen_free: (a: number, b: number, c: number) => void;
  readonly __externref_table_dealloc: (a: number) => void;
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
