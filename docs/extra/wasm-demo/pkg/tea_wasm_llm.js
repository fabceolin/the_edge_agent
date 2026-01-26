let wasm;

function addToExternrefTable0(obj) {
    const idx = wasm.__externref_table_alloc();
    wasm.__wbindgen_externrefs.set(idx, obj);
    return idx;
}

const CLOSURE_DTORS = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(state => state.dtor(state.a, state.b));

function debugString(val) {
    // primitive types
    const type = typeof val;
    if (type == 'number' || type == 'boolean' || val == null) {
        return  `${val}`;
    }
    if (type == 'string') {
        return `"${val}"`;
    }
    if (type == 'symbol') {
        const description = val.description;
        if (description == null) {
            return 'Symbol';
        } else {
            return `Symbol(${description})`;
        }
    }
    if (type == 'function') {
        const name = val.name;
        if (typeof name == 'string' && name.length > 0) {
            return `Function(${name})`;
        } else {
            return 'Function';
        }
    }
    // objects
    if (Array.isArray(val)) {
        const length = val.length;
        let debug = '[';
        if (length > 0) {
            debug += debugString(val[0]);
        }
        for(let i = 1; i < length; i++) {
            debug += ', ' + debugString(val[i]);
        }
        debug += ']';
        return debug;
    }
    // Test for built-in
    const builtInMatches = /\[object ([^\]]+)\]/.exec(toString.call(val));
    let className;
    if (builtInMatches && builtInMatches.length > 1) {
        className = builtInMatches[1];
    } else {
        // Failed to match the standard '[object ClassName]'
        return toString.call(val);
    }
    if (className == 'Object') {
        // we're a user defined class or Object
        // JSON.stringify avoids problems with cycles, and is generally much
        // easier than looping through ownProperties of `val`.
        try {
            return 'Object(' + JSON.stringify(val) + ')';
        } catch (_) {
            return 'Object';
        }
    }
    // errors
    if (val instanceof Error) {
        return `${val.name}: ${val.message}\n${val.stack}`;
    }
    // TODO we could test for more things here, like `Set`s and `Map`s.
    return className;
}

function getArrayU8FromWasm0(ptr, len) {
    ptr = ptr >>> 0;
    return getUint8ArrayMemory0().subarray(ptr / 1, ptr / 1 + len);
}

let cachedDataViewMemory0 = null;
function getDataViewMemory0() {
    if (cachedDataViewMemory0 === null || cachedDataViewMemory0.buffer.detached === true || (cachedDataViewMemory0.buffer.detached === undefined && cachedDataViewMemory0.buffer !== wasm.memory.buffer)) {
        cachedDataViewMemory0 = new DataView(wasm.memory.buffer);
    }
    return cachedDataViewMemory0;
}

function getStringFromWasm0(ptr, len) {
    ptr = ptr >>> 0;
    return decodeText(ptr, len);
}

let cachedUint8ArrayMemory0 = null;
function getUint8ArrayMemory0() {
    if (cachedUint8ArrayMemory0 === null || cachedUint8ArrayMemory0.byteLength === 0) {
        cachedUint8ArrayMemory0 = new Uint8Array(wasm.memory.buffer);
    }
    return cachedUint8ArrayMemory0;
}

function handleError(f, args) {
    try {
        return f.apply(this, args);
    } catch (e) {
        const idx = addToExternrefTable0(e);
        wasm.__wbindgen_exn_store(idx);
    }
}

function isLikeNone(x) {
    return x === undefined || x === null;
}

function makeMutClosure(arg0, arg1, dtor, f) {
    const state = { a: arg0, b: arg1, cnt: 1, dtor };
    const real = (...args) => {

        // First up with a closure we increment the internal reference
        // count. This ensures that the Rust closure environment won't
        // be deallocated while we're invoking it.
        state.cnt++;
        const a = state.a;
        state.a = 0;
        try {
            return f(a, state.b, ...args);
        } finally {
            state.a = a;
            real._wbg_cb_unref();
        }
    };
    real._wbg_cb_unref = () => {
        if (--state.cnt === 0) {
            state.dtor(state.a, state.b);
            state.a = 0;
            CLOSURE_DTORS.unregister(state);
        }
    };
    CLOSURE_DTORS.register(real, state, state);
    return real;
}

function passStringToWasm0(arg, malloc, realloc) {
    if (realloc === undefined) {
        const buf = cachedTextEncoder.encode(arg);
        const ptr = malloc(buf.length, 1) >>> 0;
        getUint8ArrayMemory0().subarray(ptr, ptr + buf.length).set(buf);
        WASM_VECTOR_LEN = buf.length;
        return ptr;
    }

    let len = arg.length;
    let ptr = malloc(len, 1) >>> 0;

    const mem = getUint8ArrayMemory0();

    let offset = 0;

    for (; offset < len; offset++) {
        const code = arg.charCodeAt(offset);
        if (code > 0x7F) break;
        mem[ptr + offset] = code;
    }
    if (offset !== len) {
        if (offset !== 0) {
            arg = arg.slice(offset);
        }
        ptr = realloc(ptr, len, len = offset + arg.length * 3, 1) >>> 0;
        const view = getUint8ArrayMemory0().subarray(ptr + offset, ptr + len);
        const ret = cachedTextEncoder.encodeInto(arg, view);

        offset += ret.written;
        ptr = realloc(ptr, len, offset, 1) >>> 0;
    }

    WASM_VECTOR_LEN = offset;
    return ptr;
}

function takeFromExternrefTable0(idx) {
    const value = wasm.__wbindgen_externrefs.get(idx);
    wasm.__externref_table_dealloc(idx);
    return value;
}

let cachedTextDecoder = new TextDecoder('utf-8', { ignoreBOM: true, fatal: true });
cachedTextDecoder.decode();
const MAX_SAFARI_DECODE_BYTES = 2146435072;
let numBytesDecoded = 0;
function decodeText(ptr, len) {
    numBytesDecoded += len;
    if (numBytesDecoded >= MAX_SAFARI_DECODE_BYTES) {
        cachedTextDecoder = new TextDecoder('utf-8', { ignoreBOM: true, fatal: true });
        cachedTextDecoder.decode();
        numBytesDecoded = len;
    }
    return cachedTextDecoder.decode(getUint8ArrayMemory0().subarray(ptr, ptr + len));
}

const cachedTextEncoder = new TextEncoder();

if (!('encodeInto' in cachedTextEncoder)) {
    cachedTextEncoder.encodeInto = function (arg, view) {
        const buf = cachedTextEncoder.encode(arg);
        view.set(buf);
        return {
            read: arg.length,
            written: buf.length
        };
    }
}

let WASM_VECTOR_LEN = 0;

function wasm_bindgen__convert__closures_____invoke__h993e6cec9bc4ab3e(arg0, arg1, arg2) {
    wasm.wasm_bindgen__convert__closures_____invoke__h993e6cec9bc4ab3e(arg0, arg1, arg2);
}

function wasm_bindgen__convert__closures_____invoke__hde2d95ef604b0a7f(arg0, arg1) {
    wasm.wasm_bindgen__convert__closures_____invoke__hde2d95ef604b0a7f(arg0, arg1);
}

function wasm_bindgen__convert__closures_____invoke__hc599d4e810efe325(arg0, arg1, arg2, arg3) {
    wasm.wasm_bindgen__convert__closures_____invoke__hc599d4e810efe325(arg0, arg1, arg2, arg3);
}

const __wbindgen_enum_ReadableStreamType = ["bytes"];

const __wbindgen_enum_RequestCache = ["default", "no-store", "reload", "no-cache", "force-cache", "only-if-cached"];

const __wbindgen_enum_RequestCredentials = ["omit", "same-origin", "include"];

const __wbindgen_enum_RequestMode = ["same-origin", "no-cors", "cors", "navigate"];

const IntoUnderlyingByteSourceFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_intounderlyingbytesource_free(ptr >>> 0, 1));

const IntoUnderlyingSinkFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_intounderlyingsink_free(ptr >>> 0, 1));

const IntoUnderlyingSourceFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_intounderlyingsource_free(ptr >>> 0, 1));

const MarkdownParseResultFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_markdownparseresult_free(ptr >>> 0, 1));

export class IntoUnderlyingByteSource {
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        IntoUnderlyingByteSourceFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_intounderlyingbytesource_free(ptr, 0);
    }
    /**
     * @returns {number}
     */
    get autoAllocateChunkSize() {
        const ret = wasm.intounderlyingbytesource_autoAllocateChunkSize(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
     * @param {ReadableByteStreamController} controller
     * @returns {Promise<any>}
     */
    pull(controller) {
        const ret = wasm.intounderlyingbytesource_pull(this.__wbg_ptr, controller);
        return ret;
    }
    /**
     * @param {ReadableByteStreamController} controller
     */
    start(controller) {
        wasm.intounderlyingbytesource_start(this.__wbg_ptr, controller);
    }
    /**
     * @returns {ReadableStreamType}
     */
    get type() {
        const ret = wasm.intounderlyingbytesource_type(this.__wbg_ptr);
        return __wbindgen_enum_ReadableStreamType[ret];
    }
    cancel() {
        const ptr = this.__destroy_into_raw();
        wasm.intounderlyingbytesource_cancel(ptr);
    }
}
if (Symbol.dispose) IntoUnderlyingByteSource.prototype[Symbol.dispose] = IntoUnderlyingByteSource.prototype.free;

export class IntoUnderlyingSink {
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        IntoUnderlyingSinkFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_intounderlyingsink_free(ptr, 0);
    }
    /**
     * @param {any} reason
     * @returns {Promise<any>}
     */
    abort(reason) {
        const ptr = this.__destroy_into_raw();
        const ret = wasm.intounderlyingsink_abort(ptr, reason);
        return ret;
    }
    /**
     * @returns {Promise<any>}
     */
    close() {
        const ptr = this.__destroy_into_raw();
        const ret = wasm.intounderlyingsink_close(ptr);
        return ret;
    }
    /**
     * @param {any} chunk
     * @returns {Promise<any>}
     */
    write(chunk) {
        const ret = wasm.intounderlyingsink_write(this.__wbg_ptr, chunk);
        return ret;
    }
}
if (Symbol.dispose) IntoUnderlyingSink.prototype[Symbol.dispose] = IntoUnderlyingSink.prototype.free;

export class IntoUnderlyingSource {
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        IntoUnderlyingSourceFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_intounderlyingsource_free(ptr, 0);
    }
    /**
     * @param {ReadableStreamDefaultController} controller
     * @returns {Promise<any>}
     */
    pull(controller) {
        const ret = wasm.intounderlyingsource_pull(this.__wbg_ptr, controller);
        return ret;
    }
    cancel() {
        const ptr = this.__destroy_into_raw();
        wasm.intounderlyingsource_cancel(ptr);
    }
}
if (Symbol.dispose) IntoUnderlyingSource.prototype[Symbol.dispose] = IntoUnderlyingSource.prototype.free;

/**
 * Parsed document result for JavaScript
 */
export class MarkdownParseResult {
    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(MarkdownParseResult.prototype);
        obj.__wbg_ptr = ptr;
        MarkdownParseResultFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        MarkdownParseResultFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_markdownparseresult_free(ptr, 0);
    }
    /**
     * Document title (from first H1)
     * @returns {string | undefined}
     */
    get title() {
        const ret = wasm.__wbg_get_markdownparseresult_title(this.__wbg_ptr);
        let v1;
        if (ret[0] !== 0) {
            v1 = getStringFromWasm0(ret[0], ret[1]).slice();
            wasm.__wbindgen_free(ret[0], ret[1] * 1, 1);
        }
        return v1;
    }
    /**
     * Document title (from first H1)
     * @param {string | null} [arg0]
     */
    set title(arg0) {
        var ptr0 = isLikeNone(arg0) ? 0 : passStringToWasm0(arg0, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_markdownparseresult_title(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * JSON string of parsed sections
     * @returns {string}
     */
    get sections_json() {
        let deferred1_0;
        let deferred1_1;
        try {
            const ret = wasm.__wbg_get_markdownparseresult_sections_json(this.__wbg_ptr);
            deferred1_0 = ret[0];
            deferred1_1 = ret[1];
            return getStringFromWasm0(ret[0], ret[1]);
        } finally {
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
     * JSON string of parsed sections
     * @param {string} arg0
     */
    set sections_json(arg0) {
        const ptr0 = passStringToWasm0(arg0, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_markdownparseresult_sections_json(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * JSON string of variable names
     * @returns {string}
     */
    get variables_json() {
        let deferred1_0;
        let deferred1_1;
        try {
            const ret = wasm.__wbg_get_markdownparseresult_variables_json(this.__wbg_ptr);
            deferred1_0 = ret[0];
            deferred1_1 = ret[1];
            return getStringFromWasm0(ret[0], ret[1]);
        } finally {
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
     * JSON string of variable names
     * @param {string} arg0
     */
    set variables_json(arg0) {
        const ptr0 = passStringToWasm0(arg0, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_markdownparseresult_variables_json(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * JSON string of frontmatter (if present)
     * @returns {string}
     */
    get frontmatter_json() {
        let deferred1_0;
        let deferred1_1;
        try {
            const ret = wasm.__wbg_get_markdownparseresult_frontmatter_json(this.__wbg_ptr);
            deferred1_0 = ret[0];
            deferred1_1 = ret[1];
            return getStringFromWasm0(ret[0], ret[1]);
        } finally {
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
     * JSON string of frontmatter (if present)
     * @param {string} arg0
     */
    set frontmatter_json(arg0) {
        const ptr0 = passStringToWasm0(arg0, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_markdownparseresult_frontmatter_json(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * JSON string of checklist items
     * @returns {string}
     */
    get tasks_json() {
        let deferred1_0;
        let deferred1_1;
        try {
            const ret = wasm.__wbg_get_markdownparseresult_tasks_json(this.__wbg_ptr);
            deferred1_0 = ret[0];
            deferred1_1 = ret[1];
            return getStringFromWasm0(ret[0], ret[1]);
        } finally {
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
     * JSON string of checklist items
     * @param {string} arg0
     */
    set tasks_json(arg0) {
        const ptr0 = passStringToWasm0(arg0, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_markdownparseresult_tasks_json(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * JSON string of task summary
     * @returns {string}
     */
    get task_summary_json() {
        let deferred1_0;
        let deferred1_1;
        try {
            const ret = wasm.__wbg_get_markdownparseresult_task_summary_json(this.__wbg_ptr);
            deferred1_0 = ret[0];
            deferred1_1 = ret[1];
            return getStringFromWasm0(ret[0], ret[1]);
        } finally {
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
     * JSON string of task summary
     * @param {string} arg0
     */
    set task_summary_json(arg0) {
        const ptr0 = passStringToWasm0(arg0, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_markdownparseresult_task_summary_json(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * JSON string of section edges
     * @returns {string}
     */
    get edges_json() {
        let deferred1_0;
        let deferred1_1;
        try {
            const ret = wasm.__wbg_get_markdownparseresult_edges_json(this.__wbg_ptr);
            deferred1_0 = ret[0];
            deferred1_1 = ret[1];
            return getStringFromWasm0(ret[0], ret[1]);
        } finally {
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
     * JSON string of section edges
     * @param {string} arg0
     */
    set edges_json(arg0) {
        const ptr0 = passStringToWasm0(arg0, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_markdownparseresult_edges_json(this.__wbg_ptr, ptr0, len0);
    }
}
if (Symbol.dispose) MarkdownParseResult.prototype[Symbol.dispose] = MarkdownParseResult.prototype.free;

/**
 * Clear the DuckDB handler
 */
export function clear_duckdb_handler() {
    wasm.clear_duckdb_handler();
}

/**
 * Clear the game Opik handler (WASM export).
 */
export function clear_game_opik_handler() {
    wasm.clear_game_opik_handler();
}

/**
 * Clear the LLM handler
 */
export function clear_llm_handler() {
    wasm.clear_llm_handler();
}

/**
 * Clear LTM handler
 */
export function clear_ltm_handler() {
    wasm.clear_ltm_handler();
}

/**
 * Clear the Lua callback
 */
export function clear_lua_callback() {
    wasm.clear_lua_callback();
}

/**
 * Clear the Opik callback
 */
export function clear_opik_callback() {
    wasm.clear_opik_callback();
}

/**
 * Clear the Prolog handler
 */
export function clear_prolog_handler() {
    wasm.clear_prolog_handler();
}

/**
 * Clear all stored credentials
 */
export function clear_storage_credentials() {
    wasm.clear_storage_credentials();
}

/**
 * Configure game Opik settings (WASM export).
 * @param {string} config_json
 */
export function configure_game_opik(config_json) {
    const ptr0 = passStringToWasm0(config_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ret = wasm.configure_game_opik(ptr0, len0);
    if (ret[1]) {
        throw takeFromExternrefTable0(ret[0]);
    }
}

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
 * @param {string} config_json
 * @returns {string}
 */
export function configure_ltm(config_json) {
    let deferred3_0;
    let deferred3_1;
    try {
        const ptr0 = passStringToWasm0(config_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.configure_ltm(ptr0, len0);
        var ptr2 = ret[0];
        var len2 = ret[1];
        if (ret[3]) {
            ptr2 = 0; len2 = 0;
            throw takeFromExternrefTable0(ret[2]);
        }
        deferred3_0 = ptr2;
        deferred3_1 = len2;
        return getStringFromWasm0(ptr2, len2);
    } finally {
        wasm.__wbindgen_free(deferred3_0, deferred3_1, 1);
    }
}

/**
 * Configure Opik settings
 *
 * # Arguments
 * * `config_json` - JSON string with OpikConfig fields
 * @param {string} config_json
 */
export function configure_opik(config_json) {
    const ptr0 = passStringToWasm0(config_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ret = wasm.configure_opik(ptr0, len0);
    if (ret[1]) {
        throw takeFromExternrefTable0(ret[0]);
    }
}

/**
 * Create an OpikTrace from LLM call parameters and response
 *
 * # Arguments
 * * `node_name` - Name of the node executing the LLM call
 * * `params_json` - LLM params as JSON string
 * * `response_json` - LLM response as JSON string
 * * `start_time` - ISO 8601 start timestamp
 * * `end_time` - ISO 8601 end timestamp
 * @param {string} node_name
 * @param {string} params_json
 * @param {string} response_json
 * @param {string} start_time
 * @param {string} end_time
 * @returns {string}
 */
export function create_llm_trace(node_name, params_json, response_json, start_time, end_time) {
    let deferred7_0;
    let deferred7_1;
    try {
        const ptr0 = passStringToWasm0(node_name, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ptr1 = passStringToWasm0(params_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len1 = WASM_VECTOR_LEN;
        const ptr2 = passStringToWasm0(response_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len2 = WASM_VECTOR_LEN;
        const ptr3 = passStringToWasm0(start_time, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len3 = WASM_VECTOR_LEN;
        const ptr4 = passStringToWasm0(end_time, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len4 = WASM_VECTOR_LEN;
        const ret = wasm.create_llm_trace(ptr0, len0, ptr1, len1, ptr2, len2, ptr3, len3, ptr4, len4);
        var ptr6 = ret[0];
        var len6 = ret[1];
        if (ret[3]) {
            ptr6 = 0; len6 = 0;
            throw takeFromExternrefTable0(ret[2]);
        }
        deferred7_0 = ptr6;
        deferred7_1 = len6;
        return getStringFromWasm0(ptr6, len6);
    } finally {
        wasm.__wbindgen_free(deferred7_0, deferred7_1, 1);
    }
}

/**
 * Transaction helper: Begin a transaction
 * @returns {Promise<string>}
 */
export function duckdb_begin_async() {
    const ret = wasm.duckdb_begin_async();
    return ret;
}

/**
 * Transaction helper: Commit a transaction
 * @returns {Promise<string>}
 */
export function duckdb_commit_async() {
    const ret = wasm.duckdb_commit_async();
    return ret;
}

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
 * @param {string} sql
 * @returns {Promise<string>}
 */
export function duckdb_execute_async(sql) {
    const ptr0 = passStringToWasm0(sql, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ret = wasm.duckdb_execute_async(ptr0, len0);
    return ret;
}

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
 * @param {string} sql
 * @param {string} params_json
 * @returns {Promise<string>}
 */
export function duckdb_query_async(sql, params_json) {
    const ptr0 = passStringToWasm0(sql, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(params_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ret = wasm.duckdb_query_async(ptr0, len0, ptr1, len1);
    return ret;
}

/**
 * Transaction helper: Rollback a transaction
 * @returns {Promise<string>}
 */
export function duckdb_rollback_async() {
    const ret = wasm.duckdb_rollback_async();
    return ret;
}

/**
 * Execute a YAML workflow with LLM actions
 *
 * This function delegates to the full executor which supports:
 * - Parallel fan-out/fan-in with configurable merge strategies
 * - Conditional routing with goto expressions
 * - All built-in actions (llm.call, return, storage, etc.)
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
export function execute_yaml(yaml, initial_state) {
    const ptr0 = passStringToWasm0(yaml, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(initial_state, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ret = wasm.execute_yaml(ptr0, len0, ptr1, len1);
    return ret;
}

/**
 * WASM binding: Execute a YAML workflow
 *
 * # Arguments
 * * `yaml` - YAML workflow configuration
 * * `initial_state` - Initial state as JSON string
 *
 * # Returns
 * * Promise resolving to final state JSON string
 * @param {string} yaml
 * @param {string} initial_state
 * @returns {Promise<any>}
 */
export function execute_yaml_workflow(yaml, initial_state) {
    const ptr0 = passStringToWasm0(yaml, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(initial_state, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ret = wasm.execute_yaml_workflow(ptr0, len0, ptr1, len1);
    return ret;
}

/**
 * WASM binding: Execute a YAML workflow with variables
 *
 * # Arguments
 * * `yaml` - YAML workflow configuration
 * * `initial_state` - Initial state as JSON string
 * * `variables` - Variables as JSON string
 *
 * # Returns
 * * Promise resolving to final state JSON string
 * @param {string} yaml
 * @param {string} initial_state
 * @param {string} variables
 * @returns {Promise<any>}
 */
export function execute_yaml_workflow_with_vars(yaml, initial_state, variables) {
    const ptr0 = passStringToWasm0(yaml, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(initial_state, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ptr2 = passStringToWasm0(variables, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len2 = WASM_VECTOR_LEN;
    const ret = wasm.execute_yaml_workflow_with_vars(ptr0, len0, ptr1, len1, ptr2, len2);
    return ret;
}

/**
 * Clear the LLM callback
 */
export function game_clear_llm_handler() {
    wasm.game_clear_llm_handler();
}

/**
 * Generate a new game round (AC-1)
 *
 * This function requires an LLM callback to be set via `game_set_llm_handler`.
 * The LLM is called to generate a phrase with a missing word.
 *
 * Returns a Promise that resolves to a JSON string with round info:
 * ```json
 * {"success": true, "data": {"id": "...", "phrase": "The ___ is bright.", "choices": [...]}}
 * ```
 * @returns {Promise<string>}
 */
export function game_generate_round() {
    const ret = wasm.game_generate_round();
    return ret;
}

/**
 * Get the leaderboard (AC-1)
 *
 * # Arguments
 * * `limit` - Maximum number of entries to return (default: 10)
 *
 * Returns a JSON string with the leaderboard:
 * ```json
 * {"success": true, "data": [{"rank": 1, "username": "SwiftFox42", "score": 0.95, ...}]}
 * ```
 * @param {number} limit
 * @returns {string}
 */
export function game_get_leaderboard(limit) {
    let deferred1_0;
    let deferred1_1;
    try {
        const ret = wasm.game_get_leaderboard(limit);
        deferred1_0 = ret[0];
        deferred1_1 = ret[1];
        return getStringFromWasm0(ret[0], ret[1]);
    } finally {
        wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
    }
}

/**
 * Get current session stats (AC-1)
 *
 * Returns a JSON string with session statistics:
 * ```json
 * {"success": true, "data": {"id": "...", "username": "...", "score": 0.5, ...}}
 * ```
 * @returns {string}
 */
export function game_get_session_stats() {
    let deferred1_0;
    let deferred1_1;
    try {
        const ret = wasm.game_get_session_stats();
        deferred1_0 = ret[0];
        deferred1_1 = ret[1];
        return getStringFromWasm0(ret[0], ret[1]);
    } finally {
        wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
    }
}

/**
 * Check if LLM handler is set
 * @returns {boolean}
 */
export function game_has_llm_handler() {
    const ret = wasm.game_has_llm_handler();
    return ret !== 0;
}

/**
 * Initialize the game engine
 *
 * Must be called before any other game functions.
 * This creates the in-memory game engine instance.
 * @returns {string}
 */
export function game_init() {
    let deferred1_0;
    let deferred1_1;
    try {
        const ret = wasm.game_init();
        deferred1_0 = ret[0];
        deferred1_1 = ret[1];
        return getStringFromWasm0(ret[0], ret[1]);
    } finally {
        wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
    }
}

/**
 * Set the LLM callback for phrase generation (AC-3)
 *
 * The callback should be an async function that takes a JSON string
 * with the prompt and returns a JSON string with the response.
 * @param {Function} callback
 */
export function game_set_llm_handler(callback) {
    wasm.game_set_llm_handler(callback);
}

/**
 * Start a new game session (AC-1)
 *
 * Returns a Promise that resolves to a JSON string with session info:
 * ```json
 * {"success": true, "data": {"id": "...", "username": "SwiftFox42", ...}}
 * ```
 * @returns {string}
 */
export function game_start_session() {
    let deferred1_0;
    let deferred1_1;
    try {
        const ret = wasm.game_start_session();
        deferred1_0 = ret[0];
        deferred1_1 = ret[1];
        return getStringFromWasm0(ret[0], ret[1]);
    } finally {
        wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
    }
}

/**
 * Submit an answer for the current round (AC-1)
 *
 * # Arguments
 * * `choice` - The word the player selected
 * * `time_ms` - Time taken to answer in milliseconds
 *
 * Returns a JSON string with the result:
 * ```json
 * {"success": true, "data": {"is_correct": true, "correct_word": "sun", ...}}
 * ```
 * @param {string} choice
 * @param {number} time_ms
 * @returns {string}
 */
export function game_submit_answer(choice, time_ms) {
    let deferred2_0;
    let deferred2_1;
    try {
        const ptr0 = passStringToWasm0(choice, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.game_submit_answer(ptr0, len0, time_ms);
        deferred2_0 = ret[0];
        deferred2_1 = ret[1];
        return getStringFromWasm0(ret[0], ret[1]);
    } finally {
        wasm.__wbindgen_free(deferred2_0, deferred2_1, 1);
    }
}

/**
 * Submit the current session to the leaderboard (AC-1)
 *
 * Returns a JSON string with the result:
 * ```json
 * {"success": true, "data": {"rank": 5, "is_new_best": true, "score": 0.75}}
 * ```
 * @returns {string}
 */
export function game_submit_to_leaderboard() {
    let deferred1_0;
    let deferred1_1;
    try {
        const ret = wasm.game_submit_to_leaderboard();
        deferred1_0 = ret[0];
        deferred1_1 = ret[1];
        return getStringFromWasm0(ret[0], ret[1]);
    } finally {
        wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
    }
}

/**
 * Get list of available/loaded extensions
 * @returns {Promise<string>}
 */
export function get_duckdb_extensions_async() {
    const ret = wasm.get_duckdb_extensions_async();
    return ret;
}

/**
 * Get current game Opik configuration as JSON (WASM export).
 * @returns {string}
 */
export function get_game_opik_config() {
    let deferred1_0;
    let deferred1_1;
    try {
        const ret = wasm.get_game_opik_config();
        deferred1_0 = ret[0];
        deferred1_1 = ret[1];
        return getStringFromWasm0(ret[0], ret[1]);
    } finally {
        wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
    }
}

/**
 * Get current LTM configuration
 * @returns {string}
 */
export function get_ltm_config() {
    let deferred1_0;
    let deferred1_1;
    try {
        const ret = wasm.get_ltm_config();
        deferred1_0 = ret[0];
        deferred1_1 = ret[1];
        return getStringFromWasm0(ret[0], ret[1]);
    } finally {
        wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
    }
}

/**
 * Get current Opik configuration as JSON
 * @returns {string}
 */
export function get_opik_config() {
    let deferred1_0;
    let deferred1_1;
    try {
        const ret = wasm.get_opik_config();
        deferred1_0 = ret[0];
        deferred1_1 = ret[1];
        return getStringFromWasm0(ret[0], ret[1]);
    } finally {
        wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
    }
}

/**
 * Check if a DuckDB handler is registered
 * @returns {boolean}
 */
export function has_duckdb_handler() {
    const ret = wasm.has_duckdb_handler();
    return ret !== 0;
}

/**
 * Check if a game Opik handler is registered (WASM export) (AC-6).
 * @returns {boolean}
 */
export function has_game_opik_handler() {
    const ret = wasm.has_game_opik_handler();
    return ret !== 0;
}

/**
 * Check if an LLM handler is registered
 * @returns {boolean}
 */
export function has_llm_handler() {
    const ret = wasm.has_llm_handler();
    return ret !== 0;
}

/**
 * Check if LTM handler is registered
 * @returns {boolean}
 */
export function has_ltm_handler() {
    const ret = wasm.has_ltm_handler();
    return ret !== 0;
}

/**
 * Check if a Lua callback is registered
 * @returns {boolean}
 */
export function has_lua_callback() {
    const ret = wasm.has_lua_callback();
    return ret !== 0;
}

/**
 * Check if an Opik callback is registered
 * @returns {boolean}
 */
export function has_opik_callback() {
    const ret = wasm.has_opik_callback();
    return ret !== 0;
}

/**
 * Check if a Prolog handler is registered
 * @returns {boolean}
 */
export function has_prolog_handler() {
    const ret = wasm.has_prolog_handler();
    return ret !== 0;
}

/**
 * Check if SharedArrayBuffer is available (for multi-threading detection)
 * @returns {boolean}
 */
export function has_shared_array_buffer() {
    const ret = wasm.has_shared_array_buffer();
    return ret !== 0;
}

/**
 * Check if credentials are set for a provider
 * @param {string} provider
 * @returns {boolean}
 */
export function has_storage_credentials(provider) {
    const ptr0 = passStringToWasm0(provider, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ret = wasm.has_storage_credentials(ptr0, len0);
    return ret !== 0;
}

/**
 * Initialize DuckDB (validation that handler is set up)
 *
 * # Arguments
 * * `options_json` - Optional JSON string with DuckDbInitOptions
 *
 * # Returns
 * * JSON string confirming initialization
 * @param {string | null} [options_json]
 * @returns {Promise<string>}
 */
export function init_duckdb_async(options_json) {
    var ptr0 = isLikeNone(options_json) ? 0 : passStringToWasm0(options_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    var len0 = WASM_VECTOR_LEN;
    const ret = wasm.init_duckdb_async(ptr0, len0);
    return ret;
}

/**
 * Initialize memory backend for testing
 * @returns {string}
 */
export function init_memory() {
    let deferred2_0;
    let deferred2_1;
    try {
        const ret = wasm.init_memory();
        var ptr1 = ret[0];
        var len1 = ret[1];
        if (ret[3]) {
            ptr1 = 0; len1 = 0;
            throw takeFromExternrefTable0(ret[2]);
        }
        deferred2_0 = ptr1;
        deferred2_1 = len1;
        return getStringFromWasm0(ptr1, len1);
    } finally {
        wasm.__wbindgen_free(deferred2_0, deferred2_1, 1);
    }
}

/**
 * Stub for init_opfs when OPFS feature is not enabled or not on WASM
 * @returns {Promise<string>}
 */
export function init_opfs() {
    const ret = wasm.init_opfs();
    return ret;
}

/**
 * Check if memory backend is available
 * @returns {boolean}
 */
export function is_memory_available() {
    const ret = wasm.is_memory_available();
    return ret !== 0;
}

/**
 * Check if OPFS is available and initialized
 * @returns {boolean}
 */
export function is_opfs_available() {
    const ret = wasm.is_opfs_available();
    return ret !== 0;
}

/**
 * Check if Opik tracing is enabled
 * @returns {boolean}
 */
export function is_opik_enabled() {
    const ret = wasm.is_opik_enabled();
    return ret !== 0;
}

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
export function llm_call_async(params_json, state_json) {
    const ptr0 = passStringToWasm0(params_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(state_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ret = wasm.llm_call_async(ptr0, len0, ptr1, len1);
    return ret;
}

/**
 * Get embeddings from the LLM
 *
 * Note: Requires wllama model with embedding support
 * @param {string} text
 * @param {string} state_json
 * @returns {Promise<string>}
 */
export function llm_embed_async(text, state_json) {
    const ptr0 = passStringToWasm0(text, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(state_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ret = wasm.llm_embed_async(ptr0, len0, ptr1, len1);
    return ret;
}

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
 * @param {string} extension_name
 * @returns {Promise<string>}
 */
export function load_duckdb_extension_async(extension_name) {
    const ptr0 = passStringToWasm0(extension_name, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ret = wasm.load_duckdb_extension_async(ptr0, len0);
    return ret;
}

/**
 * Clean up expired entries
 *
 * # Returns
 * JSON result with cleanup statistics
 * @returns {Promise<string>}
 */
export function ltm_cleanup_expired_async() {
    const ret = wasm.ltm_cleanup_expired_async();
    return ret;
}

/**
 * Delete an entry from LTM
 *
 * # Arguments
 * * `key` - Key to delete
 *
 * # Returns
 * JSON result with deletion status
 * @param {string} key
 * @returns {Promise<string>}
 */
export function ltm_delete_async(key) {
    const ptr0 = passStringToWasm0(key, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ret = wasm.ltm_delete_async(ptr0, len0);
    return ret;
}

/**
 * List LTM entries by prefix
 *
 * # Arguments
 * * `prefix` - Key prefix to filter
 * * `limit` - Maximum results
 *
 * # Returns
 * JSON result with matching entries (metadata only, no values)
 * @param {string} prefix
 * @param {number} limit
 * @returns {Promise<string>}
 */
export function ltm_list_async(prefix, limit) {
    const ptr0 = passStringToWasm0(prefix, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ret = wasm.ltm_list_async(ptr0, len0, limit);
    return ret;
}

/**
 * Retrieve a value from LTM
 *
 * # Arguments
 * * `key` - Key to retrieve
 * * `default_json` - Default value if key not found (JSON string)
 *
 * # Returns
 * JSON result with value, content_hash, and metadata
 * @param {string} key
 * @param {string} default_json
 * @returns {Promise<string>}
 */
export function ltm_retrieve_async(key, default_json) {
    const ptr0 = passStringToWasm0(key, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(default_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ret = wasm.ltm_retrieve_async(ptr0, len0, ptr1, len1);
    return ret;
}

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
 * @param {string} query
 * @param {string} metadata_filter_json
 * @param {number} limit
 * @returns {Promise<string>}
 */
export function ltm_search_async(query, metadata_filter_json, limit) {
    const ptr0 = passStringToWasm0(query, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(metadata_filter_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ret = wasm.ltm_search_async(ptr0, len0, ptr1, len1, limit);
    return ret;
}

/**
 * Get LTM statistics
 *
 * # Returns
 * JSON result with entry count, total size, etc.
 * @returns {Promise<string>}
 */
export function ltm_stats_async() {
    const ret = wasm.ltm_stats_async();
    return ret;
}

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
 * @param {string} key
 * @param {string} value_json
 * @param {string} metadata_json
 * @returns {Promise<string>}
 */
export function ltm_store_async(key, value_json, metadata_json) {
    const ptr0 = passStringToWasm0(key, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(value_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ptr2 = passStringToWasm0(metadata_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len2 = WASM_VECTOR_LEN;
    const ret = wasm.ltm_store_async(ptr0, len0, ptr1, len1, ptr2, len2);
    return ret;
}

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
export function lua_eval_async(code, state_json) {
    const ptr0 = passStringToWasm0(code, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(state_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ret = wasm.lua_eval_async(ptr0, len0, ptr1, len1);
    return ret;
}

/**
 * Initialize WASM module with panic hook
 */
export function main() {
    wasm.main();
}

/**
 * Extract checklist items from Markdown content.
 *
 * # Arguments
 * * `content` - Raw Markdown string
 *
 * # Returns
 * JSON string with tasks array and summary
 * @param {string} content
 * @returns {string}
 */
export function markdown_extract_tasks(content) {
    let deferred3_0;
    let deferred3_1;
    try {
        const ptr0 = passStringToWasm0(content, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.markdown_extract_tasks(ptr0, len0);
        var ptr2 = ret[0];
        var len2 = ret[1];
        if (ret[3]) {
            ptr2 = 0; len2 = 0;
            throw takeFromExternrefTable0(ret[2]);
        }
        deferred3_0 = ptr2;
        deferred3_1 = len2;
        return getStringFromWasm0(ptr2, len2);
    } finally {
        wasm.__wbindgen_free(deferred3_0, deferred3_1, 1);
    }
}

/**
 * Extract template variables from Markdown content.
 *
 * # Arguments
 * * `content` - Raw Markdown string
 *
 * # Returns
 * JSON array of unique variable names
 * @param {string} content
 * @returns {string}
 */
export function markdown_extract_variables(content) {
    let deferred3_0;
    let deferred3_1;
    try {
        const ptr0 = passStringToWasm0(content, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.markdown_extract_variables(ptr0, len0);
        var ptr2 = ret[0];
        var len2 = ret[1];
        if (ret[3]) {
            ptr2 = 0; len2 = 0;
            throw takeFromExternrefTable0(ret[2]);
        }
        deferred3_0 = ptr2;
        deferred3_1 = len2;
        return getStringFromWasm0(ptr2, len2);
    } finally {
        wasm.__wbindgen_free(deferred3_0, deferred3_1, 1);
    }
}

/**
 * Parse Markdown content into structured document.
 *
 * # Arguments
 * * `content` - Raw Markdown string to parse
 *
 * # Returns
 * MarkdownParseResult with JSON strings for each component
 * @param {string} content
 * @returns {MarkdownParseResult}
 */
export function markdown_parse(content) {
    const ptr0 = passStringToWasm0(content, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ret = wasm.markdown_parse(ptr0, len0);
    if (ret[2]) {
        throw takeFromExternrefTable0(ret[1]);
    }
    return MarkdownParseResult.__wrap(ret[0]);
}

/**
 * Parse Markdown and return complete result as JSON string.
 *
 * This is a convenience function for JavaScript that returns everything
 * as a single JSON object.
 * @param {string} content
 * @returns {string}
 */
export function markdown_parse_json(content) {
    let deferred3_0;
    let deferred3_1;
    try {
        const ptr0 = passStringToWasm0(content, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.markdown_parse_json(ptr0, len0);
        var ptr2 = ret[0];
        var len2 = ret[1];
        if (ret[3]) {
            ptr2 = 0; len2 = 0;
            throw takeFromExternrefTable0(ret[2]);
        }
        deferred3_0 = ptr2;
        deferred3_1 = len2;
        return getStringFromWasm0(ptr2, len2);
    } finally {
        wasm.__wbindgen_free(deferred3_0, deferred3_1, 1);
    }
}

/**
 * Parse YAML and return JSON-serialized config (WASM binding)
 *
 * # Arguments
 * * `yaml` - YAML configuration string
 *
 * # Returns
 * * JSON string of WasmYamlConfig on success
 * * JsValue error on failure
 * @param {string} yaml
 * @returns {string}
 */
export function parse_yaml(yaml) {
    let deferred3_0;
    let deferred3_1;
    try {
        const ptr0 = passStringToWasm0(yaml, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.parse_yaml(ptr0, len0);
        var ptr2 = ret[0];
        var len2 = ret[1];
        if (ret[3]) {
            ptr2 = 0; len2 = 0;
            throw takeFromExternrefTable0(ret[2]);
        }
        deferred3_0 = ptr2;
        deferred3_1 = len2;
        return getStringFromWasm0(ptr2, len2);
    } finally {
        wasm.__wbindgen_free(deferred3_0, deferred3_1, 1);
    }
}

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
export function prolog_query_async(query_json, state_json) {
    const ptr0 = passStringToWasm0(query_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(state_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ret = wasm.prolog_query_async(ptr0, len0, ptr1, len1);
    return ret;
}

/**
 * WASM binding: Render a template string
 * @param {string} template
 * @param {string} state_json
 * @param {string} variables_json
 * @returns {string}
 */
export function render_template_wasm(template, state_json, variables_json) {
    let deferred5_0;
    let deferred5_1;
    try {
        const ptr0 = passStringToWasm0(template, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ptr1 = passStringToWasm0(state_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len1 = WASM_VECTOR_LEN;
        const ptr2 = passStringToWasm0(variables_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len2 = WASM_VECTOR_LEN;
        const ret = wasm.render_template_wasm(ptr0, len0, ptr1, len1, ptr2, len2);
        var ptr4 = ret[0];
        var len4 = ret[1];
        if (ret[3]) {
            ptr4 = 0; len4 = 0;
            throw takeFromExternrefTable0(ret[2]);
        }
        deferred5_0 = ptr4;
        deferred5_1 = len4;
        return getStringFromWasm0(ptr4, len4);
    } finally {
        wasm.__wbindgen_free(deferred5_0, deferred5_1, 1);
    }
}

/**
 * Send a trace asynchronously (waits for JS Promise)
 *
 * Use this when you need to wait for the trace to be acknowledged.
 * @param {string} trace_json
 * @returns {Promise<void>}
 */
export function send_opik_trace_async(trace_json) {
    const ptr0 = passStringToWasm0(trace_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ret = wasm.send_opik_trace_async(ptr0, len0);
    return ret;
}

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
 * @param {Function} handler
 */
export function set_duckdb_handler(handler) {
    wasm.set_duckdb_handler(handler);
}

/**
 * Set the game Opik handler (WASM export) (AC-5).
 *
 * The handler should accept a JSON string (OpikGameSpan) and optionally
 * return a Promise for async sending.
 * @param {Function} handler
 */
export function set_game_opik_handler(handler) {
    wasm.set_game_opik_handler(handler);
}

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
export function set_llm_handler(handler) {
    wasm.set_llm_handler(handler);
}

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
 * @param {object} handler
 */
export function set_ltm_handler(handler) {
    wasm.set_ltm_handler(handler);
}

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
export function set_lua_callback(callback) {
    wasm.set_lua_callback(callback);
}

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
 * @param {Function} callback
 */
export function set_opik_callback(callback) {
    wasm.set_opik_callback(callback);
}

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
export function set_prolog_handler(handler) {
    wasm.set_prolog_handler(handler);
}

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
 * @param {string} provider
 * @param {string} credentials_json
 */
export function set_storage_credentials(provider, credentials_json) {
    const ptr0 = passStringToWasm0(provider, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(credentials_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ret = wasm.set_storage_credentials(ptr0, len0, ptr1, len1);
    if (ret[1]) {
        throw takeFromExternrefTable0(ret[0]);
    }
}

/**
 * @param {string} s
 * @returns {string}
 */
export function slugify(s) {
    let deferred2_0;
    let deferred2_1;
    try {
        const ptr0 = passStringToWasm0(s, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.slugify(ptr0, len0);
        deferred2_0 = ret[0];
        deferred2_1 = ret[1];
        return getStringFromWasm0(ret[0], ret[1]);
    } finally {
        wasm.__wbindgen_free(deferred2_0, deferred2_1, 1);
    }
}

/**
 * Copy content from one URI to another (cross-provider supported)
 *
 * This enables workflows like:
 * - Download from S3 to OPFS for offline use
 * - Upload from OPFS to S3 for cloud backup
 * - Transfer between cloud providers
 * @param {string} source_uri
 * @param {string} dest_uri
 * @returns {Promise<string>}
 */
export function storage_copy_async(source_uri, dest_uri) {
    const ptr0 = passStringToWasm0(source_uri, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(dest_uri, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ret = wasm.storage_copy_async(ptr0, len0, ptr1, len1);
    return ret;
}

/**
 * Delete object at URI
 * @param {string} uri
 * @returns {Promise<string>}
 */
export function storage_delete_async(uri) {
    const ptr0 = passStringToWasm0(uri, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ret = wasm.storage_delete_async(ptr0, len0);
    return ret;
}

/**
 * Check if object exists at URI
 * @param {string} uri
 * @returns {Promise<string>}
 */
export function storage_exists_async(uri) {
    const ptr0 = passStringToWasm0(uri, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ret = wasm.storage_exists_async(ptr0, len0);
    return ret;
}

/**
 * List objects at URI with optional prefix filtering
 *
 * # Arguments
 * * `uri` - Storage URI (directory/prefix to list)
 * * `options_json` - JSON options: { "limit": 1000 }
 * @param {string} uri
 * @param {string} options_json
 * @returns {Promise<string>}
 */
export function storage_list_async(uri, options_json) {
    const ptr0 = passStringToWasm0(uri, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(options_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ret = wasm.storage_list_async(ptr0, len0, ptr1, len1);
    return ret;
}

/**
 * Read text content from URI
 *
 * # Arguments
 * * `uri` - Storage URI (s3://, gs://, az://, http://, opfs://, memory://)
 * * `_state_json` - Agent state (unused, for API compatibility)
 *
 * # Returns
 * JSON result with content or error
 * @param {string} uri
 * @param {string} _state_json
 * @returns {Promise<string>}
 */
export function storage_read_async(uri, _state_json) {
    const ptr0 = passStringToWasm0(uri, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(_state_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ret = wasm.storage_read_async(ptr0, len0, ptr1, len1);
    return ret;
}

/**
 * Read binary content from URI (returns base64 encoded)
 *
 * # Arguments
 * * `uri` - Storage URI
 *
 * # Returns
 * JSON result with base64-encoded content
 * @param {string} uri
 * @returns {Promise<string>}
 */
export function storage_read_binary_async(uri) {
    const ptr0 = passStringToWasm0(uri, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ret = wasm.storage_read_binary_async(ptr0, len0);
    return ret;
}

/**
 * Get information about supported storage schemes
 * @returns {string}
 */
export function storage_supported_schemes() {
    let deferred1_0;
    let deferred1_1;
    try {
        const ret = wasm.storage_supported_schemes();
        deferred1_0 = ret[0];
        deferred1_1 = ret[1];
        return getStringFromWasm0(ret[0], ret[1]);
    } finally {
        wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
    }
}

/**
 * Write text content to URI
 *
 * # Arguments
 * * `uri` - Storage URI
 * * `content` - Text content to write
 * * `_state_json` - Agent state (unused, for API compatibility)
 * @param {string} uri
 * @param {string} content
 * @param {string} _state_json
 * @returns {Promise<string>}
 */
export function storage_write_async(uri, content, _state_json) {
    const ptr0 = passStringToWasm0(uri, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(content, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ptr2 = passStringToWasm0(_state_json, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len2 = WASM_VECTOR_LEN;
    const ret = wasm.storage_write_async(ptr0, len0, ptr1, len1, ptr2, len2);
    return ret;
}

/**
 * Write binary content to URI (expects base64 encoded input)
 *
 * # Arguments
 * * `uri` - Storage URI
 * * `content_base64` - Base64-encoded binary content
 * @param {string} uri
 * @param {string} content_base64
 * @returns {Promise<string>}
 */
export function storage_write_binary_async(uri, content_base64) {
    const ptr0 = passStringToWasm0(uri, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ptr1 = passStringToWasm0(content_base64, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    const ret = wasm.storage_write_binary_async(ptr0, len0, ptr1, len1);
    return ret;
}

/**
 * Validate a YAML configuration without returning the parsed result
 *
 * # Arguments
 * * `yaml` - YAML configuration string
 *
 * # Returns
 * * `true` if valid
 * * JsValue error if invalid
 * @param {string} yaml
 * @returns {boolean}
 */
export function validate_yaml(yaml) {
    const ptr0 = passStringToWasm0(yaml, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    const ret = wasm.validate_yaml(ptr0, len0);
    if (ret[2]) {
        throw takeFromExternrefTable0(ret[1]);
    }
    return ret[0] !== 0;
}

/**
 * Get library version
 * @returns {string}
 */
export function version() {
    let deferred1_0;
    let deferred1_1;
    try {
        const ret = wasm.version();
        deferred1_0 = ret[0];
        deferred1_1 = ret[1];
        return getStringFromWasm0(ret[0], ret[1]);
    } finally {
        wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
    }
}

const EXPECTED_RESPONSE_TYPES = new Set(['basic', 'cors', 'default']);

async function __wbg_load(module, imports) {
    if (typeof Response === 'function' && module instanceof Response) {
        if (typeof WebAssembly.instantiateStreaming === 'function') {
            try {
                return await WebAssembly.instantiateStreaming(module, imports);
            } catch (e) {
                const validResponse = module.ok && EXPECTED_RESPONSE_TYPES.has(module.type);

                if (validResponse && module.headers.get('Content-Type') !== 'application/wasm') {
                    console.warn("`WebAssembly.instantiateStreaming` failed because your server does not serve Wasm with `application/wasm` MIME type. Falling back to `WebAssembly.instantiate` which is slower. Original error:\n", e);

                } else {
                    throw e;
                }
            }
        }

        const bytes = await module.arrayBuffer();
        return await WebAssembly.instantiate(bytes, imports);
    } else {
        const instance = await WebAssembly.instantiate(module, imports);

        if (instance instanceof WebAssembly.Instance) {
            return { instance, module };
        } else {
            return instance;
        }
    }
}

function __wbg_get_imports() {
    const imports = {};
    imports.wbg = {};
    imports.wbg.__wbg_Error_52673b7de5a0ca89 = function(arg0, arg1) {
        const ret = Error(getStringFromWasm0(arg0, arg1));
        return ret;
    };
    imports.wbg.__wbg___wbindgen_debug_string_adfb662ae34724b6 = function(arg0, arg1) {
        const ret = debugString(arg1);
        const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len1 = WASM_VECTOR_LEN;
        getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
        getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
    };
    imports.wbg.__wbg___wbindgen_is_function_8d400b8b1af978cd = function(arg0) {
        const ret = typeof(arg0) === 'function';
        return ret;
    };
    imports.wbg.__wbg___wbindgen_is_null_dfda7d66506c95b5 = function(arg0) {
        const ret = arg0 === null;
        return ret;
    };
    imports.wbg.__wbg___wbindgen_is_object_ce774f3490692386 = function(arg0) {
        const val = arg0;
        const ret = typeof(val) === 'object' && val !== null;
        return ret;
    };
    imports.wbg.__wbg___wbindgen_is_string_704ef9c8fc131030 = function(arg0) {
        const ret = typeof(arg0) === 'string';
        return ret;
    };
    imports.wbg.__wbg___wbindgen_is_undefined_f6b95eab589e0269 = function(arg0) {
        const ret = arg0 === undefined;
        return ret;
    };
    imports.wbg.__wbg___wbindgen_string_get_a2a31e16edf96e42 = function(arg0, arg1) {
        const obj = arg1;
        const ret = typeof(obj) === 'string' ? obj : undefined;
        var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len1 = WASM_VECTOR_LEN;
        getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
        getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
    };
    imports.wbg.__wbg___wbindgen_throw_dd24417ed36fc46e = function(arg0, arg1) {
        throw new Error(getStringFromWasm0(arg0, arg1));
    };
    imports.wbg.__wbg__wbg_cb_unref_87dfb5aaa0cbcea7 = function(arg0) {
        arg0._wbg_cb_unref();
    };
    imports.wbg.__wbg_abort_07646c894ebbf2bd = function(arg0) {
        arg0.abort();
    };
    imports.wbg.__wbg_abort_399ecbcfd6ef3c8e = function(arg0, arg1) {
        arg0.abort(arg1);
    };
    imports.wbg.__wbg_append_c5cbdf46455cc776 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4) {
        arg0.append(getStringFromWasm0(arg1, arg2), getStringFromWasm0(arg3, arg4));
    }, arguments) };
    imports.wbg.__wbg_body_947b901c33f7fe32 = function(arg0) {
        const ret = arg0.body;
        return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
    };
    imports.wbg.__wbg_buffer_6cb2fecb1f253d71 = function(arg0) {
        const ret = arg0.buffer;
        return ret;
    };
    imports.wbg.__wbg_byobRequest_f8e3517f5f8ad284 = function(arg0) {
        const ret = arg0.byobRequest;
        return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
    };
    imports.wbg.__wbg_byteLength_faa9938885bdeee6 = function(arg0) {
        const ret = arg0.byteLength;
        return ret;
    };
    imports.wbg.__wbg_byteOffset_3868b6a19ba01dea = function(arg0) {
        const ret = arg0.byteOffset;
        return ret;
    };
    imports.wbg.__wbg_call_3020136f7a2d6e44 = function() { return handleError(function (arg0, arg1, arg2) {
        const ret = arg0.call(arg1, arg2);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_call_78f94eb02ec7f9b2 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4) {
        const ret = arg0.call(arg1, arg2, arg3, arg4);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_call_abb4ff46ce38be40 = function() { return handleError(function (arg0, arg1) {
        const ret = arg0.call(arg1);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_call_c8baa5c5e72d274e = function() { return handleError(function (arg0, arg1, arg2, arg3) {
        const ret = arg0.call(arg1, arg2, arg3);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_cancel_a65cf45dca50ba4c = function(arg0) {
        const ret = arg0.cancel();
        return ret;
    };
    imports.wbg.__wbg_catch_b9db41d97d42bd02 = function(arg0, arg1) {
        const ret = arg0.catch(arg1);
        return ret;
    };
    imports.wbg.__wbg_clearTimeout_42d9ccd50822fd3a = function(arg0) {
        const ret = clearTimeout(arg0);
        return ret;
    };
    imports.wbg.__wbg_close_0af5661bf3d335f2 = function() { return handleError(function (arg0) {
        arg0.close();
    }, arguments) };
    imports.wbg.__wbg_close_3ec111e7b23d94d8 = function() { return handleError(function (arg0) {
        arg0.close();
    }, arguments) };
    imports.wbg.__wbg_crypto_86f2631e91b51511 = function(arg0) {
        const ret = arg0.crypto;
        return ret;
    };
    imports.wbg.__wbg_done_62ea16af4ce34b24 = function(arg0) {
        const ret = arg0.done;
        return ret;
    };
    imports.wbg.__wbg_enqueue_a7e6b1ee87963aad = function() { return handleError(function (arg0, arg1) {
        arg0.enqueue(arg1);
    }, arguments) };
    imports.wbg.__wbg_error_7534b8e9a36f1ab4 = function(arg0, arg1) {
        let deferred0_0;
        let deferred0_1;
        try {
            deferred0_0 = arg0;
            deferred0_1 = arg1;
            console.error(getStringFromWasm0(arg0, arg1));
        } finally {
            wasm.__wbindgen_free(deferred0_0, deferred0_1, 1);
        }
    };
    imports.wbg.__wbg_error_7bc7d576a6aaf855 = function(arg0) {
        console.error(arg0);
    };
    imports.wbg.__wbg_fetch_6bbc32f991730587 = function(arg0) {
        const ret = fetch(arg0);
        return ret;
    };
    imports.wbg.__wbg_fetch_90447c28cc0b095e = function(arg0, arg1) {
        const ret = arg0.fetch(arg1);
        return ret;
    };
    imports.wbg.__wbg_getRandomValues_9b655bdd369112f2 = function() { return handleError(function (arg0, arg1) {
        globalThis.crypto.getRandomValues(getArrayU8FromWasm0(arg0, arg1));
    }, arguments) };
    imports.wbg.__wbg_getRandomValues_b3f15fcbfabb0f8b = function() { return handleError(function (arg0, arg1) {
        arg0.getRandomValues(arg1);
    }, arguments) };
    imports.wbg.__wbg_getReader_48e00749fe3f6089 = function() { return handleError(function (arg0) {
        const ret = arg0.getReader();
        return ret;
    }, arguments) };
    imports.wbg.__wbg_getUTCDate_2bba81cf0ce2ef57 = function(arg0) {
        const ret = arg0.getUTCDate();
        return ret;
    };
    imports.wbg.__wbg_getUTCFullYear_8523c9e9544c9f0e = function(arg0) {
        const ret = arg0.getUTCFullYear();
        return ret;
    };
    imports.wbg.__wbg_getUTCHours_726a1adc9fc06ad7 = function(arg0) {
        const ret = arg0.getUTCHours();
        return ret;
    };
    imports.wbg.__wbg_getUTCMilliseconds_5f88f742af10a9dc = function(arg0) {
        const ret = arg0.getUTCMilliseconds();
        return ret;
    };
    imports.wbg.__wbg_getUTCMinutes_e361259948f2ea42 = function(arg0) {
        const ret = arg0.getUTCMinutes();
        return ret;
    };
    imports.wbg.__wbg_getUTCMonth_dd5805ff06b70e4b = function(arg0) {
        const ret = arg0.getUTCMonth();
        return ret;
    };
    imports.wbg.__wbg_getUTCSeconds_97b72aa152b4cc6d = function(arg0) {
        const ret = arg0.getUTCSeconds();
        return ret;
    };
    imports.wbg.__wbg_get_af9dab7e9603ea93 = function() { return handleError(function (arg0, arg1) {
        const ret = Reflect.get(arg0, arg1);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_get_done_f98a6e62c4e18fb9 = function(arg0) {
        const ret = arg0.done;
        return isLikeNone(ret) ? 0xFFFFFF : ret ? 1 : 0;
    };
    imports.wbg.__wbg_get_value_63e39884ef11812e = function(arg0) {
        const ret = arg0.value;
        return ret;
    };
    imports.wbg.__wbg_has_0e670569d65d3a45 = function() { return handleError(function (arg0, arg1) {
        const ret = Reflect.has(arg0, arg1);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_headers_654c30e1bcccc552 = function(arg0) {
        const ret = arg0.headers;
        return ret;
    };
    imports.wbg.__wbg_instanceof_Promise_eca6c43a2610558d = function(arg0) {
        let result;
        try {
            result = arg0 instanceof Promise;
        } catch (_) {
            result = false;
        }
        const ret = result;
        return ret;
    };
    imports.wbg.__wbg_instanceof_Response_cd74d1c2ac92cb0b = function(arg0) {
        let result;
        try {
            result = arg0 instanceof Response;
        } catch (_) {
            result = false;
        }
        const ret = result;
        return ret;
    };
    imports.wbg.__wbg_iterator_27b7c8b35ab3e86b = function() {
        const ret = Symbol.iterator;
        return ret;
    };
    imports.wbg.__wbg_length_22ac23eaec9d8053 = function(arg0) {
        const ret = arg0.length;
        return ret;
    };
    imports.wbg.__wbg_log_1d990106d99dacb7 = function(arg0) {
        console.log(arg0);
    };
    imports.wbg.__wbg_msCrypto_d562bbe83e0d4b91 = function(arg0) {
        const ret = arg0.msCrypto;
        return ret;
    };
    imports.wbg.__wbg_new_0_23cedd11d9b40c9d = function() {
        const ret = new Date();
        return ret;
    };
    imports.wbg.__wbg_new_1ba21ce319a06297 = function() {
        const ret = new Object();
        return ret;
    };
    imports.wbg.__wbg_new_3c79b3bb1b32b7d3 = function() { return handleError(function () {
        const ret = new Headers();
        return ret;
    }, arguments) };
    imports.wbg.__wbg_new_6421f6084cc5bc5a = function(arg0) {
        const ret = new Uint8Array(arg0);
        return ret;
    };
    imports.wbg.__wbg_new_881a222c65f168fc = function() { return handleError(function () {
        const ret = new AbortController();
        return ret;
    }, arguments) };
    imports.wbg.__wbg_new_8a6f238a6ece86ea = function() {
        const ret = new Error();
        return ret;
    };
    imports.wbg.__wbg_new_b2db8aa2650f793a = function(arg0) {
        const ret = new Date(arg0);
        return ret;
    };
    imports.wbg.__wbg_new_df1173567d5ff028 = function(arg0, arg1) {
        const ret = new Error(getStringFromWasm0(arg0, arg1));
        return ret;
    };
    imports.wbg.__wbg_new_ff12d2b041fb48f1 = function(arg0, arg1) {
        try {
            var state0 = {a: arg0, b: arg1};
            var cb0 = (arg0, arg1) => {
                const a = state0.a;
                state0.a = 0;
                try {
                    return wasm_bindgen__convert__closures_____invoke__hc599d4e810efe325(a, state0.b, arg0, arg1);
                } finally {
                    state0.a = a;
                }
            };
            const ret = new Promise(cb0);
            return ret;
        } finally {
            state0.a = state0.b = 0;
        }
    };
    imports.wbg.__wbg_new_from_slice_f9c22b9153b26992 = function(arg0, arg1) {
        const ret = new Uint8Array(getArrayU8FromWasm0(arg0, arg1));
        return ret;
    };
    imports.wbg.__wbg_new_no_args_cb138f77cf6151ee = function(arg0, arg1) {
        const ret = new Function(getStringFromWasm0(arg0, arg1));
        return ret;
    };
    imports.wbg.__wbg_new_with_byte_offset_and_length_d85c3da1fd8df149 = function(arg0, arg1, arg2) {
        const ret = new Uint8Array(arg0, arg1 >>> 0, arg2 >>> 0);
        return ret;
    };
    imports.wbg.__wbg_new_with_length_aa5eaf41d35235e5 = function(arg0) {
        const ret = new Uint8Array(arg0 >>> 0);
        return ret;
    };
    imports.wbg.__wbg_new_with_str_and_init_c5748f76f5108934 = function() { return handleError(function (arg0, arg1, arg2) {
        const ret = new Request(getStringFromWasm0(arg0, arg1), arg2);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_next_138a17bbf04e926c = function(arg0) {
        const ret = arg0.next;
        return ret;
    };
    imports.wbg.__wbg_next_3cfe5c0fe2a4cc53 = function() { return handleError(function (arg0) {
        const ret = arg0.next();
        return ret;
    }, arguments) };
    imports.wbg.__wbg_node_e1f24f89a7336c2e = function(arg0) {
        const ret = arg0.node;
        return ret;
    };
    imports.wbg.__wbg_now_69d776cd24f5215b = function() {
        const ret = Date.now();
        return ret;
    };
    imports.wbg.__wbg_process_3975fd6c72f520aa = function(arg0) {
        const ret = arg0.process;
        return ret;
    };
    imports.wbg.__wbg_prototypesetcall_dfe9b766cdc1f1fd = function(arg0, arg1, arg2) {
        Uint8Array.prototype.set.call(getArrayU8FromWasm0(arg0, arg1), arg2);
    };
    imports.wbg.__wbg_queueMicrotask_9b549dfce8865860 = function(arg0) {
        const ret = arg0.queueMicrotask;
        return ret;
    };
    imports.wbg.__wbg_queueMicrotask_fca69f5bfad613a5 = function(arg0) {
        queueMicrotask(arg0);
    };
    imports.wbg.__wbg_randomFillSync_f8c153b79f285817 = function() { return handleError(function (arg0, arg1) {
        arg0.randomFillSync(arg1);
    }, arguments) };
    imports.wbg.__wbg_read_39c4b35efcd03c25 = function(arg0) {
        const ret = arg0.read();
        return ret;
    };
    imports.wbg.__wbg_releaseLock_a5912f590b185180 = function(arg0) {
        arg0.releaseLock();
    };
    imports.wbg.__wbg_require_b74f47fc2d022fd6 = function() { return handleError(function () {
        const ret = module.require;
        return ret;
    }, arguments) };
    imports.wbg.__wbg_resolve_fd5bfbaa4ce36e1e = function(arg0) {
        const ret = Promise.resolve(arg0);
        return ret;
    };
    imports.wbg.__wbg_respond_9f7fc54636c4a3af = function() { return handleError(function (arg0, arg1) {
        arg0.respond(arg1 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_setTimeout_4ec014681668a581 = function(arg0, arg1) {
        const ret = setTimeout(arg0, arg1);
        return ret;
    };
    imports.wbg.__wbg_set_169e13b608078b7b = function(arg0, arg1, arg2) {
        arg0.set(getArrayU8FromWasm0(arg1, arg2));
    };
    imports.wbg.__wbg_set_body_8e743242d6076a4f = function(arg0, arg1) {
        arg0.body = arg1;
    };
    imports.wbg.__wbg_set_cache_0e437c7c8e838b9b = function(arg0, arg1) {
        arg0.cache = __wbindgen_enum_RequestCache[arg1];
    };
    imports.wbg.__wbg_set_credentials_55ae7c3c106fd5be = function(arg0, arg1) {
        arg0.credentials = __wbindgen_enum_RequestCredentials[arg1];
    };
    imports.wbg.__wbg_set_headers_5671cf088e114d2b = function(arg0, arg1) {
        arg0.headers = arg1;
    };
    imports.wbg.__wbg_set_method_76c69e41b3570627 = function(arg0, arg1, arg2) {
        arg0.method = getStringFromWasm0(arg1, arg2);
    };
    imports.wbg.__wbg_set_mode_611016a6818fc690 = function(arg0, arg1) {
        arg0.mode = __wbindgen_enum_RequestMode[arg1];
    };
    imports.wbg.__wbg_set_signal_e89be862d0091009 = function(arg0, arg1) {
        arg0.signal = arg1;
    };
    imports.wbg.__wbg_signal_3c14fbdc89694b39 = function(arg0) {
        const ret = arg0.signal;
        return ret;
    };
    imports.wbg.__wbg_stack_0ed75d68575b0f3c = function(arg0, arg1) {
        const ret = arg1.stack;
        const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len1 = WASM_VECTOR_LEN;
        getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
        getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
    };
    imports.wbg.__wbg_static_accessor_GLOBAL_769e6b65d6557335 = function() {
        const ret = typeof global === 'undefined' ? null : global;
        return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
    };
    imports.wbg.__wbg_static_accessor_GLOBAL_THIS_60cf02db4de8e1c1 = function() {
        const ret = typeof globalThis === 'undefined' ? null : globalThis;
        return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
    };
    imports.wbg.__wbg_static_accessor_SELF_08f5a74c69739274 = function() {
        const ret = typeof self === 'undefined' ? null : self;
        return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
    };
    imports.wbg.__wbg_static_accessor_WINDOW_a8924b26aa92d024 = function() {
        const ret = typeof window === 'undefined' ? null : window;
        return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
    };
    imports.wbg.__wbg_status_9bfc680efca4bdfd = function(arg0) {
        const ret = arg0.status;
        return ret;
    };
    imports.wbg.__wbg_stringify_655a6390e1f5eb6b = function() { return handleError(function (arg0) {
        const ret = JSON.stringify(arg0);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_subarray_845f2f5bce7d061a = function(arg0, arg1, arg2) {
        const ret = arg0.subarray(arg1 >>> 0, arg2 >>> 0);
        return ret;
    };
    imports.wbg.__wbg_then_429f7caf1026411d = function(arg0, arg1, arg2) {
        const ret = arg0.then(arg1, arg2);
        return ret;
    };
    imports.wbg.__wbg_then_4f95312d68691235 = function(arg0, arg1) {
        const ret = arg0.then(arg1);
        return ret;
    };
    imports.wbg.__wbg_toISOString_eca15cbe422eeea5 = function(arg0) {
        const ret = arg0.toISOString();
        return ret;
    };
    imports.wbg.__wbg_url_b6d11838a4f95198 = function(arg0, arg1) {
        const ret = arg1.url;
        const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len1 = WASM_VECTOR_LEN;
        getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
        getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
    };
    imports.wbg.__wbg_value_57b7b035e117f7ee = function(arg0) {
        const ret = arg0.value;
        return ret;
    };
    imports.wbg.__wbg_versions_4e31226f5e8dc909 = function(arg0) {
        const ret = arg0.versions;
        return ret;
    };
    imports.wbg.__wbg_view_788aaf149deefd2f = function(arg0) {
        const ret = arg0.view;
        return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
    };
    imports.wbg.__wbg_warn_6e567d0d926ff881 = function(arg0) {
        console.warn(arg0);
    };
    imports.wbg.__wbindgen_cast_2241b6af4c4b2941 = function(arg0, arg1) {
        // Cast intrinsic for `Ref(String) -> Externref`.
        const ret = getStringFromWasm0(arg0, arg1);
        return ret;
    };
    imports.wbg.__wbindgen_cast_7c310da1b4e21bc9 = function(arg0, arg1) {
        // Cast intrinsic for `Closure(Closure { dtor_idx: 1889, function: Function { arguments: [Externref], shim_idx: 1890, ret: Unit, inner_ret: Some(Unit) }, mutable: true }) -> Externref`.
        const ret = makeMutClosure(arg0, arg1, wasm.wasm_bindgen__closure__destroy__ha9e5309a8e8dc2f5, wasm_bindgen__convert__closures_____invoke__h993e6cec9bc4ab3e);
        return ret;
    };
    imports.wbg.__wbindgen_cast_a05c46e21695cc12 = function(arg0, arg1) {
        // Cast intrinsic for `Closure(Closure { dtor_idx: 976, function: Function { arguments: [], shim_idx: 977, ret: Unit, inner_ret: Some(Unit) }, mutable: true }) -> Externref`.
        const ret = makeMutClosure(arg0, arg1, wasm.wasm_bindgen__closure__destroy__h9eb6f956610eaa1e, wasm_bindgen__convert__closures_____invoke__hde2d95ef604b0a7f);
        return ret;
    };
    imports.wbg.__wbindgen_cast_cb9088102bce6b30 = function(arg0, arg1) {
        // Cast intrinsic for `Ref(Slice(U8)) -> NamedExternref("Uint8Array")`.
        const ret = getArrayU8FromWasm0(arg0, arg1);
        return ret;
    };
    imports.wbg.__wbindgen_cast_d6cd19b81560fd6e = function(arg0) {
        // Cast intrinsic for `F64 -> Externref`.
        const ret = arg0;
        return ret;
    };
    imports.wbg.__wbindgen_init_externref_table = function() {
        const table = wasm.__wbindgen_externrefs;
        const offset = table.grow(4);
        table.set(0, undefined);
        table.set(offset + 0, undefined);
        table.set(offset + 1, null);
        table.set(offset + 2, true);
        table.set(offset + 3, false);
    };

    return imports;
}

function __wbg_finalize_init(instance, module) {
    wasm = instance.exports;
    __wbg_init.__wbindgen_wasm_module = module;
    cachedDataViewMemory0 = null;
    cachedUint8ArrayMemory0 = null;


    wasm.__wbindgen_start();
    return wasm;
}

function initSync(module) {
    if (wasm !== undefined) return wasm;


    if (typeof module !== 'undefined') {
        if (Object.getPrototypeOf(module) === Object.prototype) {
            ({module} = module)
        } else {
            console.warn('using deprecated parameters for `initSync()`; pass a single object instead')
        }
    }

    const imports = __wbg_get_imports();
    if (!(module instanceof WebAssembly.Module)) {
        module = new WebAssembly.Module(module);
    }
    const instance = new WebAssembly.Instance(module, imports);
    return __wbg_finalize_init(instance, module);
}

async function __wbg_init(module_or_path) {
    if (wasm !== undefined) return wasm;


    if (typeof module_or_path !== 'undefined') {
        if (Object.getPrototypeOf(module_or_path) === Object.prototype) {
            ({module_or_path} = module_or_path)
        } else {
            console.warn('using deprecated parameters for the initialization function; pass a single object instead')
        }
    }

    if (typeof module_or_path === 'undefined') {
        module_or_path = new URL('tea_wasm_llm_bg.wasm', import.meta.url);
    }
    const imports = __wbg_get_imports();

    if (typeof module_or_path === 'string' || (typeof Request === 'function' && module_or_path instanceof Request) || (typeof URL === 'function' && module_or_path instanceof URL)) {
        module_or_path = fetch(module_or_path);
    }

    const { instance, module } = await __wbg_load(await module_or_path, imports);

    return __wbg_finalize_init(instance, module);
}

export { initSync };
export default __wbg_init;
