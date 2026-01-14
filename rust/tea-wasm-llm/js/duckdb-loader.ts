/**
 * DuckDB WASM Loader for TEA (The Edge Agent)
 *
 * This module provides initialization and handler registration for DuckDB WASM,
 * following the same callback bridge pattern as wllama-loader.
 *
 * @module duckdb-loader
 */

import * as duckdb from '@duckdb/duckdb-wasm';

// Re-export duckdb types for convenience
export type { AsyncDuckDB, AsyncDuckDBConnection } from '@duckdb/duckdb-wasm';

/**
 * DuckDB initialization options
 */
export interface DuckDbLoaderOptions {
    /** Memory limit (e.g., "512MB"). Default: none */
    memoryLimit?: string;

    /** Number of threads (0 = auto, 1 = single-thread). Default: auto */
    threads?: number;

    /** Extensions to preload on initialization */
    extensions?: string[];

    /** Enable console logging. Default: true */
    logging?: boolean;

    /** Custom bundle URLs (for self-hosting) */
    bundleUrls?: {
        mainModule?: string;
        mainWorker?: string;
        pthreadWorker?: string;
    };

    /** Use single-threaded mode even if SharedArrayBuffer is available */
    forceSingleThread?: boolean;
}

/**
 * DuckDB query result
 */
export interface DuckDbQueryResult {
    success: boolean;
    rows: Record<string, unknown>[];
    row_count: number;
    schema?: Array<{ name: string; type: string }>;
    error?: string;
    error_code?: string;
}

/**
 * DuckDB handler function type
 */
export type DuckDbHandler = (sql: string, paramsJson: string) => Promise<string>;

// Singleton instances
let dbInstance: duckdb.AsyncDuckDB | null = null;
let connectionInstance: duckdb.AsyncDuckDBConnection | null = null;
let workerInstance: Worker | null = null;

/**
 * Check if SharedArrayBuffer is available (required for multi-threading)
 */
export function hasSharedArrayBuffer(): boolean {
    try {
        return typeof SharedArrayBuffer !== 'undefined';
    } catch {
        return false;
    }
}

/**
 * Get the appropriate DuckDB bundle based on browser capabilities
 */
async function selectBundle(options: DuckDbLoaderOptions): Promise<duckdb.DuckDBBundle> {
    if (options.bundleUrls) {
        // Custom bundle URLs
        return {
            mainModule: options.bundleUrls.mainModule || '',
            mainWorker: options.bundleUrls.mainWorker || '',
            pthreadWorker: options.bundleUrls.pthreadWorker,
        };
    }

    // Use jsdelivr bundles (auto-select based on browser)
    const bundles = duckdb.getJsDelivrBundles();
    return duckdb.selectBundle(bundles);
}

/**
 * Initialize DuckDB WASM instance
 *
 * @param options - Initialization options
 * @returns The initialized AsyncDuckDB instance
 *
 * @example
 * ```typescript
 * import { initDuckDb } from 'tea-wasm-llm/duckdb-loader';
 *
 * const db = await initDuckDb({
 *     memoryLimit: '512MB',
 *     extensions: ['vss', 'fts']
 * });
 * ```
 */
export async function initDuckDb(options: DuckDbLoaderOptions = {}): Promise<duckdb.AsyncDuckDB> {
    if (dbInstance) {
        console.log('[TEA-DUCKDB] Using existing DuckDB instance');
        return dbInstance;
    }

    const logging = options.logging !== false;

    if (logging) {
        console.log('[TEA-DUCKDB] Initializing DuckDB WASM...');
    }

    // Select appropriate bundle
    const bundle = await selectBundle(options);

    // Create logger
    const logger = logging ? new duckdb.ConsoleLogger() : new duckdb.VoidLogger();

    // Create worker
    const mainWorkerUrl = bundle.mainWorker;
    if (!mainWorkerUrl) {
        throw new Error('DuckDB bundle missing mainWorker URL');
    }

    workerInstance = new Worker(mainWorkerUrl);
    dbInstance = new duckdb.AsyncDuckDB(logger, workerInstance);

    // Instantiate the database
    await dbInstance.instantiate(bundle.mainModule, bundle.pthreadWorker);

    // Open database (in-memory by default)
    await dbInstance.open({
        path: ':memory:',
        query: {
            castBigIntToDouble: true,
        },
    });

    if (logging) {
        console.log('[TEA-DUCKDB] DuckDB instantiated');
    }

    // Apply configuration settings
    const conn = await dbInstance.connect();

    if (options.memoryLimit) {
        try {
            await conn.query(`SET memory_limit = '${options.memoryLimit}'`);
            if (logging) {
                console.log(`[TEA-DUCKDB] Memory limit set to ${options.memoryLimit}`);
            }
        } catch (e) {
            console.warn('[TEA-DUCKDB] Failed to set memory limit:', e);
        }
    }

    if (options.threads !== undefined) {
        try {
            await conn.query(`SET threads = ${options.threads}`);
            if (logging) {
                console.log(`[TEA-DUCKDB] Threads set to ${options.threads}`);
            }
        } catch (e) {
            console.warn('[TEA-DUCKDB] Failed to set threads:', e);
        }
    }

    // Load requested extensions
    if (options.extensions && options.extensions.length > 0) {
        for (const ext of options.extensions) {
            try {
                await conn.query(`INSTALL ${ext}`);
                await conn.query(`LOAD ${ext}`);
                if (logging) {
                    console.log(`[TEA-DUCKDB] Extension loaded: ${ext}`);
                }
            } catch (e) {
                console.warn(`[TEA-DUCKDB] Failed to load extension ${ext}:`, e);
            }
        }
    }

    await conn.close();

    if (logging) {
        console.log('[TEA-DUCKDB] DuckDB ready');
    }

    return dbInstance;
}

/**
 * Get a database connection (creates one if needed)
 */
export async function getConnection(): Promise<duckdb.AsyncDuckDBConnection> {
    if (!dbInstance) {
        throw new Error('DuckDB not initialized. Call initDuckDb() first.');
    }

    if (!connectionInstance) {
        connectionInstance = await dbInstance.connect();
    }

    return connectionInstance;
}

/**
 * Classify DuckDB errors into categories
 */
function classifyError(error: Error): string {
    const msg = error.message.toLowerCase();

    if (msg.includes('syntax error') || msg.includes('parser error')) {
        return 'SYNTAX_ERROR';
    }
    if (msg.includes('extension') || msg.includes('not found')) {
        return 'EXTENSION_ERROR';
    }
    if (msg.includes('cors') || msg.includes('access-control')) {
        return 'CORS_ERROR';
    }
    if (msg.includes('memory') || msg.includes('out of')) {
        return 'MEMORY_ERROR';
    }
    if (msg.includes('network') || msg.includes('fetch') || msg.includes('connection')) {
        return 'NETWORK_ERROR';
    }
    if (msg.includes('type') && (msg.includes('mismatch') || msg.includes('cannot'))) {
        return 'TYPE_ERROR';
    }
    if (msg.includes('does not exist') || msg.includes('unknown')) {
        return 'NOT_FOUND_ERROR';
    }

    return 'UNKNOWN_ERROR';
}

/**
 * Create a DuckDB handler function for the Rust WASM bridge
 *
 * @param conn - DuckDB connection to use
 * @returns Handler function that accepts SQL and params, returns JSON result
 *
 * @example
 * ```typescript
 * import { initDuckDb, createDuckDbHandler } from 'tea-wasm-llm/duckdb-loader';
 * import { set_duckdb_handler } from 'tea-wasm-llm';
 *
 * const db = await initDuckDb();
 * const conn = await db.connect();
 * set_duckdb_handler(createDuckDbHandler(conn));
 * ```
 */
export function createDuckDbHandler(conn: duckdb.AsyncDuckDBConnection): DuckDbHandler {
    return async (sql: string, paramsJson: string): Promise<string> => {
        try {
            const params = JSON.parse(paramsJson) as unknown[];

            // Execute query with parameters
            const result = await conn.query(sql, ...params);

            // Convert Arrow table to JSON-serializable objects
            const rows: Record<string, unknown>[] = [];
            const schema = result.schema.fields.map(f => ({
                name: f.name,
                type: f.type.toString(),
            }));

            // Convert each row
            for (const row of result.toArray()) {
                const obj: Record<string, unknown> = {};
                for (const field of result.schema.fields) {
                    const value = row[field.name];
                    // Handle BigInt conversion
                    if (typeof value === 'bigint') {
                        obj[field.name] = Number(value);
                    } else if (value instanceof Date) {
                        obj[field.name] = value.toISOString();
                    } else if (Array.isArray(value)) {
                        // Handle arrays (like vector embeddings)
                        obj[field.name] = value.map(v =>
                            typeof v === 'bigint' ? Number(v) : v
                        );
                    } else {
                        obj[field.name] = value;
                    }
                }
                rows.push(obj);
            }

            const response: DuckDbQueryResult = {
                success: true,
                rows,
                row_count: rows.length,
                schema,
            };

            return JSON.stringify(response);
        } catch (error) {
            const err = error as Error;
            const response: DuckDbQueryResult = {
                success: false,
                rows: [],
                row_count: 0,
                error: err.message,
                error_code: classifyError(err),
            };

            return JSON.stringify(response);
        }
    };
}

/**
 * Initialize DuckDB and register the handler with the Rust WASM module
 *
 * @param setHandler - The set_duckdb_handler function from tea-wasm-llm
 * @param options - Initialization options
 * @returns Object with db instance and connection
 *
 * @example
 * ```typescript
 * import { initDuckDbWithHandler } from 'tea-wasm-llm/duckdb-loader';
 * import { set_duckdb_handler } from 'tea-wasm-llm';
 *
 * const { db, connection } = await initDuckDbWithHandler(set_duckdb_handler, {
 *     memoryLimit: '512MB',
 *     extensions: ['vss']
 * });
 * ```
 */
export async function initDuckDbWithHandler(
    setHandler: (handler: DuckDbHandler) => void,
    options: DuckDbLoaderOptions = {}
): Promise<{ db: duckdb.AsyncDuckDB; connection: duckdb.AsyncDuckDBConnection }> {
    // Initialize DuckDB
    const db = await initDuckDb(options);

    // Get connection
    const connection = await getConnection();

    // Create and register handler
    const handler = createDuckDbHandler(connection);
    setHandler(handler);

    console.log('[TEA-DUCKDB] Handler registered with Rust WASM');

    return { db, connection };
}

/**
 * Load a DuckDB extension
 *
 * @param extensionName - Name of the extension to load
 *
 * @example
 * ```typescript
 * await loadExtension('vss');
 * await loadExtension('fts');
 * ```
 */
export async function loadExtension(extensionName: string): Promise<void> {
    const conn = await getConnection();

    console.log(`[TEA-DUCKDB] Loading extension: ${extensionName}`);

    try {
        await conn.query(`INSTALL ${extensionName}`);
    } catch (e) {
        // Extension might already be installed
        console.warn(`[TEA-DUCKDB] Install warning for ${extensionName}:`, e);
    }

    await conn.query(`LOAD ${extensionName}`);
    console.log(`[TEA-DUCKDB] Extension loaded: ${extensionName}`);
}

/**
 * Execute a query and return results as typed array
 *
 * @param sql - SQL query
 * @param params - Query parameters
 * @returns Query results
 */
export async function query<T = Record<string, unknown>>(
    sql: string,
    params: unknown[] = []
): Promise<T[]> {
    const conn = await getConnection();
    const handler = createDuckDbHandler(conn);
    const resultJson = await handler(sql, JSON.stringify(params));
    const result = JSON.parse(resultJson) as DuckDbQueryResult;

    if (!result.success) {
        throw new Error(result.error || 'Query failed');
    }

    return result.rows as T[];
}

/**
 * Execute a statement without returning results
 *
 * @param sql - SQL statement (CREATE, INSERT, UPDATE, DELETE, etc.)
 */
export async function execute(sql: string): Promise<void> {
    const conn = await getConnection();
    await conn.query(sql);
}

/**
 * Close DuckDB connection and terminate worker
 */
export async function closeDuckDb(): Promise<void> {
    if (connectionInstance) {
        await connectionInstance.close();
        connectionInstance = null;
    }

    if (dbInstance) {
        await dbInstance.terminate();
        dbInstance = null;
    }

    if (workerInstance) {
        workerInstance.terminate();
        workerInstance = null;
    }

    console.log('[TEA-DUCKDB] DuckDB closed');
}

/**
 * Check if DuckDB is initialized
 */
export function isDuckDbInitialized(): boolean {
    return dbInstance !== null;
}

/**
 * Get DuckDB version information
 */
export async function getDuckDbVersion(): Promise<string> {
    const rows = await query<{ version: string }>('SELECT version() as version');
    return rows[0]?.version || 'unknown';
}

// Extension constants for convenience
export const EXTENSIONS = {
    /** Vector similarity search with HNSW indexes */
    VSS: 'vss',
    /** Full-text search */
    FTS: 'fts',
    /** Parquet file support (usually autoloaded) */
    PARQUET: 'parquet',
    /** JSON operations (usually autoloaded) */
    JSON: 'json',
    /** Geospatial operations */
    SPATIAL: 'spatial',
    /** Timezones and collations */
    ICU: 'icu',
    /** Remote file access (requires CORS) */
    HTTPFS: 'httpfs',
} as const;
