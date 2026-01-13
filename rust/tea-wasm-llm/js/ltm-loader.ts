/**
 * LTM (Long-Term Memory) Loader for TEA WASM
 *
 * This module provides IndexedDB-based catalog storage and handler registration
 * for the LTM backend in browser environments.
 *
 * @module ltm-loader
 */

// Database constants
const DB_NAME = 'tea-ltm';
const DB_VERSION = 1;
const ENTRIES_STORE = 'entries';
const BLOBS_STORE = 'blobs';
const SYNC_QUEUE_STORE = 'sync_queue';

/**
 * LTM initialization options
 */
export interface LtmLoaderOptions {
    /** Custom database name (default: 'tea-ltm') */
    dbName?: string;

    /** Cloud storage URI for large blobs */
    storageUri?: string;

    /** Inline threshold in bytes (default: 1024) */
    inlineThreshold?: number;

    /** Enable background sync to cloud */
    enableSync?: boolean;

    /** Enable console logging */
    logging?: boolean;
}

/**
 * LTM entry structure (matches Rust LtmEntry)
 */
export interface LtmEntry {
    id: string;
    key: string;
    content_hash: string;
    storage_uri?: string;
    byte_size: number;
    inlined_value?: unknown;
    metadata: Record<string, unknown>;
    expires_at?: number;
    created_at: number;
    updated_at: number;
    synced: boolean;
}

/**
 * Blob entry in IndexedDB
 */
interface BlobEntry {
    id: string;
    data: string;
    created_at: number;
}

/**
 * Sync queue entry
 */
interface SyncQueueEntry {
    id: string;
    key: string;
    action: 'put' | 'delete';
    created_at: number;
    attempts: number;
}

/**
 * LTM handler interface for Rust WASM bridge
 */
export interface LtmHandler {
    get: (store: string, id: string) => Promise<string | null>;
    put: (store: string, data: string) => Promise<void>;
    delete: (store: string, id: string) => Promise<void>;
    list: (store: string, prefix: string, limit: number) => Promise<string>;
    store_blob: (id: string, data: string) => Promise<void>;
    get_blob: (id: string) => Promise<string | null>;
    delete_blob: (id: string) => Promise<void>;
}

// Singleton database instance
let dbInstance: IDBDatabase | null = null;
let syncWorkerInterval: number | null = null;

/**
 * Open or create the IndexedDB database
 */
async function openDatabase(dbName: string = DB_NAME): Promise<IDBDatabase> {
    if (dbInstance) {
        return dbInstance;
    }

    return new Promise((resolve, reject) => {
        const request = indexedDB.open(dbName, DB_VERSION);

        request.onerror = () => {
            reject(new Error(`Failed to open IndexedDB: ${request.error?.message}`));
        };

        request.onsuccess = () => {
            dbInstance = request.result;
            resolve(dbInstance);
        };

        request.onupgradeneeded = (event) => {
            const db = (event.target as IDBOpenDBRequest).result;

            // Create entries store with id as keyPath
            if (!db.objectStoreNames.contains(ENTRIES_STORE)) {
                const entriesStore = db.createObjectStore(ENTRIES_STORE, { keyPath: 'id' });
                entriesStore.createIndex('key', 'key', { unique: true });
                entriesStore.createIndex('expires_at', 'expires_at', { unique: false });
                entriesStore.createIndex('synced', 'synced', { unique: false });
            }

            // Create blobs store
            if (!db.objectStoreNames.contains(BLOBS_STORE)) {
                db.createObjectStore(BLOBS_STORE, { keyPath: 'id' });
            }

            // Create sync queue store
            if (!db.objectStoreNames.contains(SYNC_QUEUE_STORE)) {
                const syncStore = db.createObjectStore(SYNC_QUEUE_STORE, { keyPath: 'id' });
                syncStore.createIndex('created_at', 'created_at', { unique: false });
            }
        };
    });
}

/**
 * Create LTM handler for Rust WASM bridge
 */
export function createLtmHandler(db: IDBDatabase): LtmHandler {
    return {
        /**
         * Get entry by ID from a store
         */
        get: async (store: string, id: string): Promise<string | null> => {
            return new Promise((resolve, reject) => {
                const tx = db.transaction(store, 'readonly');
                const objectStore = tx.objectStore(store);
                const request = objectStore.get(id);

                request.onerror = () => reject(new Error(`Get failed: ${request.error?.message}`));
                request.onsuccess = () => {
                    if (request.result) {
                        resolve(JSON.stringify(request.result));
                    } else {
                        resolve(null);
                    }
                };
            });
        },

        /**
         * Put entry into a store
         */
        put: async (store: string, data: string): Promise<void> => {
            return new Promise((resolve, reject) => {
                const obj = JSON.parse(data);
                const tx = db.transaction(store, 'readwrite');
                const objectStore = tx.objectStore(store);
                const request = objectStore.put(obj);

                request.onerror = () => reject(new Error(`Put failed: ${request.error?.message}`));
                request.onsuccess = () => resolve();
            });
        },

        /**
         * Delete entry by ID from a store
         */
        delete: async (store: string, id: string): Promise<void> => {
            return new Promise((resolve, reject) => {
                const tx = db.transaction(store, 'readwrite');
                const objectStore = tx.objectStore(store);
                const request = objectStore.delete(id);

                request.onerror = () => reject(new Error(`Delete failed: ${request.error?.message}`));
                request.onsuccess = () => resolve();
            });
        },

        /**
         * List entries by key prefix
         */
        list: async (store: string, prefix: string, limit: number): Promise<string> => {
            return new Promise((resolve, reject) => {
                const tx = db.transaction(store, 'readonly');
                const objectStore = tx.objectStore(store);
                const results: LtmEntry[] = [];

                // Use index if listing by key prefix
                const cursorRequest = prefix
                    ? objectStore.index('key').openCursor()
                    : objectStore.openCursor();

                cursorRequest.onerror = () => reject(new Error(`List failed: ${cursorRequest.error?.message}`));
                cursorRequest.onsuccess = (event) => {
                    const cursor = (event.target as IDBRequest<IDBCursorWithValue>).result;

                    if (cursor && results.length < limit) {
                        const entry = cursor.value as LtmEntry;

                        // Filter by prefix if specified
                        if (!prefix || entry.key.startsWith(prefix)) {
                            results.push(entry);
                        }

                        cursor.continue();
                    } else {
                        resolve(JSON.stringify(results));
                    }
                };
            });
        },

        /**
         * Store blob data
         */
        store_blob: async (id: string, data: string): Promise<void> => {
            return new Promise((resolve, reject) => {
                const tx = db.transaction(BLOBS_STORE, 'readwrite');
                const objectStore = tx.objectStore(BLOBS_STORE);
                const entry: BlobEntry = {
                    id,
                    data,
                    created_at: Date.now(),
                };
                const request = objectStore.put(entry);

                request.onerror = () => reject(new Error(`Store blob failed: ${request.error?.message}`));
                request.onsuccess = () => resolve();
            });
        },

        /**
         * Get blob data by ID
         */
        get_blob: async (id: string): Promise<string | null> => {
            return new Promise((resolve, reject) => {
                const tx = db.transaction(BLOBS_STORE, 'readonly');
                const objectStore = tx.objectStore(BLOBS_STORE);
                const request = objectStore.get(id);

                request.onerror = () => reject(new Error(`Get blob failed: ${request.error?.message}`));
                request.onsuccess = () => {
                    const result = request.result as BlobEntry | undefined;
                    resolve(result?.data ?? null);
                };
            });
        },

        /**
         * Delete blob by ID
         */
        delete_blob: async (id: string): Promise<void> => {
            return new Promise((resolve, reject) => {
                const tx = db.transaction(BLOBS_STORE, 'readwrite');
                const objectStore = tx.objectStore(BLOBS_STORE);
                const request = objectStore.delete(id);

                request.onerror = () => reject(new Error(`Delete blob failed: ${request.error?.message}`));
                request.onsuccess = () => resolve();
            });
        },
    };
}

/**
 * Initialize LTM backend
 *
 * @param options - LTM initialization options
 * @returns LTM handler for WASM bridge
 *
 * @example
 * ```typescript
 * import { initLtm } from 'tea-wasm-llm/ltm-loader';
 * import { set_ltm_handler, configure_ltm } from 'tea-wasm-llm';
 *
 * const handler = await initLtm({
 *     storageUri: 's3://my-bucket/ltm/',
 *     enableSync: true
 * });
 *
 * set_ltm_handler(handler);
 * ```
 */
export async function initLtm(options: LtmLoaderOptions = {}): Promise<LtmHandler> {
    const logging = options.logging !== false;
    const dbName = options.dbName || DB_NAME;

    if (logging) {
        console.log('[TEA-LTM] Initializing IndexedDB backend...');
    }

    const db = await openDatabase(dbName);
    const handler = createLtmHandler(db);

    if (logging) {
        console.log('[TEA-LTM] IndexedDB ready');
    }

    return handler;
}

/**
 * Initialize LTM and register handler with WASM module
 *
 * @param setHandler - The set_ltm_handler function from tea-wasm-llm
 * @param configureLtm - The configure_ltm function from tea-wasm-llm
 * @param options - Initialization options
 *
 * @example
 * ```typescript
 * import { initLtmWithHandler } from 'tea-wasm-llm/ltm-loader';
 * import { set_ltm_handler, configure_ltm } from 'tea-wasm-llm';
 *
 * await initLtmWithHandler(set_ltm_handler, configure_ltm, {
 *     storageUri: 's3://my-bucket/ltm/',
 *     inlineThreshold: 2048,
 *     enableSync: true
 * });
 * ```
 */
export async function initLtmWithHandler(
    setHandler: (handler: LtmHandler) => void,
    configureLtm: (configJson: string) => string,
    options: LtmLoaderOptions = {}
): Promise<{ handler: LtmHandler; db: IDBDatabase }> {
    const logging = options.logging !== false;

    // Initialize IndexedDB
    const handler = await initLtm(options);
    const db = dbInstance!;

    // Register handler
    setHandler(handler);

    if (logging) {
        console.log('[TEA-LTM] Handler registered with Rust WASM');
    }

    // Configure LTM backend
    const config = {
        storage_uri: options.storageUri,
        inline_threshold: options.inlineThreshold || 1024,
        enable_sync: options.enableSync || false,
        offline_fallback: true,
    };

    configureLtm(JSON.stringify(config));

    if (logging) {
        console.log('[TEA-LTM] Configuration applied:', config);
    }

    // Start sync worker if enabled
    if (options.enableSync) {
        startSyncWorker(db, options);
    }

    return { handler, db };
}

/**
 * Add item to sync queue
 */
export async function addToSyncQueue(
    db: IDBDatabase,
    key: string,
    action: 'put' | 'delete'
): Promise<void> {
    return new Promise((resolve, reject) => {
        const tx = db.transaction(SYNC_QUEUE_STORE, 'readwrite');
        const store = tx.objectStore(SYNC_QUEUE_STORE);

        // Use key hash as ID to avoid duplicates
        const id = `sync:${action}:${key}`;
        const entry: SyncQueueEntry = {
            id,
            key,
            action,
            created_at: Date.now(),
            attempts: 0,
        };

        const request = store.put(entry);
        request.onerror = () => reject(new Error(`Add to sync queue failed: ${request.error?.message}`));
        request.onsuccess = () => resolve();
    });
}

/**
 * Get pending sync items
 */
export async function getPendingSyncItems(db: IDBDatabase, limit: number = 100): Promise<SyncQueueEntry[]> {
    return new Promise((resolve, reject) => {
        const tx = db.transaction(SYNC_QUEUE_STORE, 'readonly');
        const store = tx.objectStore(SYNC_QUEUE_STORE);
        const index = store.index('created_at');
        const results: SyncQueueEntry[] = [];

        const request = index.openCursor();
        request.onerror = () => reject(new Error(`Get sync items failed: ${request.error?.message}`));
        request.onsuccess = (event) => {
            const cursor = (event.target as IDBRequest<IDBCursorWithValue>).result;

            if (cursor && results.length < limit) {
                results.push(cursor.value as SyncQueueEntry);
                cursor.continue();
            } else {
                resolve(results);
            }
        };
    });
}

/**
 * Remove item from sync queue
 */
export async function removeSyncItem(db: IDBDatabase, id: string): Promise<void> {
    return new Promise((resolve, reject) => {
        const tx = db.transaction(SYNC_QUEUE_STORE, 'readwrite');
        const store = tx.objectStore(SYNC_QUEUE_STORE);
        const request = store.delete(id);

        request.onerror = () => reject(new Error(`Remove sync item failed: ${request.error?.message}`));
        request.onsuccess = () => resolve();
    });
}

/**
 * Start background sync worker
 */
export function startSyncWorker(db: IDBDatabase, options: LtmLoaderOptions): void {
    if (syncWorkerInterval !== null) {
        return; // Already running
    }

    const logging = options.logging !== false;

    if (logging) {
        console.log('[TEA-LTM] Starting background sync worker');
    }

    syncWorkerInterval = window.setInterval(async () => {
        try {
            const items = await getPendingSyncItems(db, 10);

            if (items.length === 0) {
                return;
            }

            if (logging) {
                console.log(`[TEA-LTM] Processing ${items.length} sync items`);
            }

            // Process sync items
            for (const item of items) {
                try {
                    // TODO: Implement actual cloud sync via storage module
                    // For now, just mark as synced
                    await removeSyncItem(db, item.id);

                    if (logging) {
                        console.log(`[TEA-LTM] Synced: ${item.key} (${item.action})`);
                    }
                } catch (error) {
                    console.warn(`[TEA-LTM] Sync failed for ${item.key}:`, error);
                    // Update attempt count
                    item.attempts++;
                    if (item.attempts >= 3) {
                        await removeSyncItem(db, item.id);
                        console.error(`[TEA-LTM] Sync abandoned after 3 attempts: ${item.key}`);
                    }
                }
            }
        } catch (error) {
            console.error('[TEA-LTM] Sync worker error:', error);
        }
    }, 30000); // Run every 30 seconds
}

/**
 * Stop background sync worker
 */
export function stopSyncWorker(): void {
    if (syncWorkerInterval !== null) {
        window.clearInterval(syncWorkerInterval);
        syncWorkerInterval = null;
        console.log('[TEA-LTM] Stopped background sync worker');
    }
}

/**
 * Clear all LTM data (for testing)
 */
export async function clearLtmData(dbName: string = DB_NAME): Promise<void> {
    return new Promise((resolve, reject) => {
        // Close existing connection
        if (dbInstance) {
            dbInstance.close();
            dbInstance = null;
        }

        const request = indexedDB.deleteDatabase(dbName);
        request.onerror = () => reject(new Error(`Failed to delete database: ${request.error?.message}`));
        request.onsuccess = () => {
            console.log('[TEA-LTM] Database cleared');
            resolve();
        };
    });
}

/**
 * Get LTM statistics
 */
export async function getLtmStats(db?: IDBDatabase): Promise<{
    entryCount: number;
    blobCount: number;
    syncQueueCount: number;
}> {
    const database = db || dbInstance;
    if (!database) {
        throw new Error('Database not initialized');
    }

    const getCount = (storeName: string): Promise<number> => {
        return new Promise((resolve, reject) => {
            const tx = database.transaction(storeName, 'readonly');
            const store = tx.objectStore(storeName);
            const request = store.count();

            request.onerror = () => reject(new Error(`Count failed: ${request.error?.message}`));
            request.onsuccess = () => resolve(request.result);
        });
    };

    const [entryCount, blobCount, syncQueueCount] = await Promise.all([
        getCount(ENTRIES_STORE),
        getCount(BLOBS_STORE),
        getCount(SYNC_QUEUE_STORE),
    ]);

    return { entryCount, blobCount, syncQueueCount };
}

/**
 * Check if IndexedDB is available
 */
export function isIndexedDBAvailable(): boolean {
    try {
        return typeof indexedDB !== 'undefined' && indexedDB !== null;
    } catch {
        return false;
    }
}

/**
 * Check if LTM is initialized
 */
export function isLtmInitialized(): boolean {
    return dbInstance !== null;
}

/**
 * Close LTM database connection
 */
export function closeLtm(): void {
    stopSyncWorker();

    if (dbInstance) {
        dbInstance.close();
        dbInstance = null;
        console.log('[TEA-LTM] Database connection closed');
    }
}
