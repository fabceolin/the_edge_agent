/**
 * LTM (Long-Term Memory) Module Tests
 *
 * Tests for the LTM backend with IndexedDB catalog and blob storage.
 * These tests verify the LTM API works correctly in browser/WASM context.
 */

import { test, expect, Page } from '@playwright/test';

// Helper to wait for WASM module to load
async function waitForWasm(page: Page) {
    await page.waitForFunction(() => {
        return typeof (window as any).teaWasmLlm !== 'undefined';
    }, { timeout: 10000 });
}

// Mock LTM handler for testing without full IndexedDB
function createMockLtmHandler() {
    const entries = new Map<string, string>();
    const blobs = new Map<string, string>();

    return {
        get: async (store: string, id: string) => {
            return entries.get(`${store}:${id}`) || null;
        },
        put: async (store: string, data: string) => {
            const obj = JSON.parse(data);
            entries.set(`${store}:${obj.id}`, data);
        },
        delete: async (store: string, id: string) => {
            entries.delete(`${store}:${id}`);
        },
        list: async (store: string, prefix: string, limit: number) => {
            const results: string[] = [];
            for (const [key, value] of entries) {
                if (key.startsWith(`${store}:`)) {
                    const entry = JSON.parse(value);
                    if (!prefix || entry.key.startsWith(prefix)) {
                        results.push(value);
                    }
                    if (results.length >= limit) break;
                }
            }
            return JSON.stringify(results.map(r => JSON.parse(r)));
        },
        store_blob: async (id: string, data: string) => {
            blobs.set(id, data);
        },
        get_blob: async (id: string) => {
            return blobs.get(id) || null;
        },
        delete_blob: async (id: string) => {
            blobs.delete(id);
        },
    };
}

test.describe('LTM Module - Basic CRUD Operations', () => {
    test.beforeEach(async ({ page }) => {
        await page.goto('/test.html');
        await waitForWasm(page);

        // Register mock handler
        await page.evaluate(() => {
            const wasm = (window as any).teaWasmLlm;
            const entries = new Map();
            const blobs = new Map();

            wasm.set_ltm_handler({
                get: async (store: string, id: string) => {
                    return entries.get(`${store}:${id}`) || null;
                },
                put: async (store: string, data: string) => {
                    const obj = JSON.parse(data);
                    entries.set(`${store}:${obj.id}`, data);
                },
                delete: async (store: string, id: string) => {
                    entries.delete(`${store}:${id}`);
                },
                list: async (store: string, prefix: string, limit: number) => {
                    const results: string[] = [];
                    for (const [key, value] of entries) {
                        if (key.startsWith(`${store}:`)) {
                            const entry = JSON.parse(value);
                            if (!prefix || entry.key.startsWith(prefix)) {
                                results.push(value);
                            }
                            if (results.length >= limit) break;
                        }
                    }
                    return JSON.stringify(results.map(r => JSON.parse(r)));
                },
                store_blob: async (id: string, data: string) => {
                    blobs.set(id, data);
                },
                get_blob: async (id: string) => {
                    return blobs.get(id) || null;
                },
                delete_blob: async (id: string) => {
                    blobs.delete(id);
                },
            });
        });
    });

    test('should check handler registration', async ({ page }) => {
        const result = await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;
            return wasm.has_ltm_handler();
        });

        expect(result).toBe(true);
    });

    test('should store and retrieve a small value (inlined)', async ({ page }) => {
        const result = await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;

            // Configure with default settings
            wasm.configure_ltm(JSON.stringify({ inline_threshold: 1024 }));

            // Store a small value
            const storeResult = await wasm.ltm_store_async(
                'test:key1',
                JSON.stringify({ data: 'hello world' }),
                JSON.stringify({ type: 'test' })
            );

            // Retrieve it
            const retrieveResult = await wasm.ltm_retrieve_async(
                'test:key1',
                'null'
            );

            return {
                store: JSON.parse(storeResult),
                retrieve: JSON.parse(retrieveResult)
            };
        });

        expect(result.store.success).toBe(true);
        expect(result.store.stored).toBe(true);
        expect(result.store.inlined).toBe(true);
        expect(result.store.content_hash).toContain('sha256:');

        expect(result.retrieve.success).toBe(true);
        expect(result.retrieve.found).toBe(true);
        expect(result.retrieve.value).toEqual({ data: 'hello world' });
    });

    test('should return default when key not found', async ({ page }) => {
        const result = await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;

            const retrieveResult = await wasm.ltm_retrieve_async(
                'nonexistent:key',
                JSON.stringify({ default: 'value' })
            );

            return JSON.parse(retrieveResult);
        });

        expect(result.success).toBe(true);
        expect(result.found).toBe(false);
        expect(result.value).toEqual({ default: 'value' });
    });

    test('should delete an entry', async ({ page }) => {
        const result = await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;

            // Store a value
            await wasm.ltm_store_async(
                'delete:test',
                JSON.stringify({ data: 'to delete' }),
                '{}'
            );

            // Verify it exists
            const beforeDelete = await wasm.ltm_retrieve_async('delete:test', 'null');

            // Delete it
            const deleteResult = await wasm.ltm_delete_async('delete:test');

            // Verify it's gone
            const afterDelete = await wasm.ltm_retrieve_async('delete:test', 'null');

            return {
                before: JSON.parse(beforeDelete),
                delete: JSON.parse(deleteResult),
                after: JSON.parse(afterDelete)
            };
        });

        expect(result.before.found).toBe(true);
        expect(result.delete.success).toBe(true);
        expect(result.delete.deleted).toBe(true);
        expect(result.after.found).toBe(false);
    });

    test('should deduplicate identical content', async ({ page }) => {
        const result = await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;

            const data = JSON.stringify({ same: 'content' });

            // Store same value twice
            const first = await wasm.ltm_store_async('dedup:key', data, '{}');
            const second = await wasm.ltm_store_async('dedup:key', data, '{}');

            return {
                first: JSON.parse(first),
                second: JSON.parse(second)
            };
        });

        expect(result.first.stored).toBe(true);
        expect(result.first.deduplicated).toBe(false);

        expect(result.second.stored).toBe(false);
        expect(result.second.deduplicated).toBe(true);
        expect(result.second.content_hash).toBe(result.first.content_hash);
    });
});

test.describe('LTM Module - Large Value Storage (Blob)', () => {
    test.beforeEach(async ({ page }) => {
        await page.goto('/test.html');
        await waitForWasm(page);

        // Register mock handler with blob support
        await page.evaluate(() => {
            const wasm = (window as any).teaWasmLlm;
            const entries = new Map();
            const blobs = new Map();

            wasm.set_ltm_handler({
                get: async (store: string, id: string) => entries.get(`${store}:${id}`) || null,
                put: async (store: string, data: string) => {
                    const obj = JSON.parse(data);
                    entries.set(`${store}:${obj.id}`, data);
                },
                delete: async (store: string, id: string) => entries.delete(`${store}:${id}`),
                list: async (store: string, prefix: string, limit: number) => {
                    const results: string[] = [];
                    for (const [key, value] of entries) {
                        if (key.startsWith(`${store}:`)) {
                            const entry = JSON.parse(value);
                            if (!prefix || entry.key.startsWith(prefix)) {
                                results.push(value);
                            }
                            if (results.length >= limit) break;
                        }
                    }
                    return JSON.stringify(results.map(r => JSON.parse(r)));
                },
                store_blob: async (id: string, data: string) => blobs.set(id, data),
                get_blob: async (id: string) => blobs.get(id) || null,
                delete_blob: async (id: string) => blobs.delete(id),
            });

            // Set low threshold to test blob storage
            wasm.configure_ltm(JSON.stringify({ inline_threshold: 50 }));
        });
    });

    test('should store large value as blob', async ({ page }) => {
        const result = await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;

            // Create a value larger than threshold (50 bytes)
            const largeData = JSON.stringify({ content: 'x'.repeat(100) });

            const storeResult = await wasm.ltm_store_async(
                'blob:large',
                largeData,
                '{}'
            );

            const retrieveResult = await wasm.ltm_retrieve_async(
                'blob:large',
                'null'
            );

            return {
                store: JSON.parse(storeResult),
                retrieve: JSON.parse(retrieveResult)
            };
        });

        expect(result.store.success).toBe(true);
        expect(result.store.inlined).toBe(false);

        expect(result.retrieve.success).toBe(true);
        expect(result.retrieve.found).toBe(true);
        expect(result.retrieve.value.content).toContain('x'.repeat(100));
    });
});

test.describe('LTM Module - Listing and Search', () => {
    test.beforeEach(async ({ page }) => {
        await page.goto('/test.html');
        await waitForWasm(page);

        // Register mock handler and populate test data
        await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;
            const entries = new Map();
            const blobs = new Map();

            wasm.set_ltm_handler({
                get: async (store: string, id: string) => entries.get(`${store}:${id}`) || null,
                put: async (store: string, data: string) => {
                    const obj = JSON.parse(data);
                    entries.set(`${store}:${obj.id}`, data);
                },
                delete: async (store: string, id: string) => entries.delete(`${store}:${id}`),
                list: async (store: string, prefix: string, limit: number) => {
                    const results: string[] = [];
                    for (const [key, value] of entries) {
                        if (key.startsWith(`${store}:`)) {
                            const entry = JSON.parse(value);
                            if (!prefix || entry.key.startsWith(prefix)) {
                                results.push(value);
                            }
                            if (results.length >= limit) break;
                        }
                    }
                    return JSON.stringify(results.map(r => JSON.parse(r)));
                },
                store_blob: async (id: string, data: string) => blobs.set(id, data),
                get_blob: async (id: string) => blobs.get(id) || null,
                delete_blob: async (id: string) => blobs.delete(id),
            });

            // Populate test data
            await wasm.ltm_store_async('cache:user:1', '{"name": "Alice"}', '{"type": "user"}');
            await wasm.ltm_store_async('cache:user:2', '{"name": "Bob"}', '{"type": "user"}');
            await wasm.ltm_store_async('cache:product:1', '{"name": "Widget"}', '{"type": "product"}');
            await wasm.ltm_store_async('memory:agent:1', '{"history": []}', '{"type": "agent"}');
        });
    });

    test('should list entries by prefix', async ({ page }) => {
        const result = await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;

            const listResult = await wasm.ltm_list_async('cache:user:', 10);

            return JSON.parse(listResult);
        });

        expect(result.success).toBe(true);
        expect(result.count).toBe(2);
        expect(result.entries.length).toBe(2);
        expect(result.entries.every((e: any) => e.key.startsWith('cache:user:'))).toBe(true);
    });

    test('should search entries by prefix', async ({ page }) => {
        const result = await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;

            const searchResult = await wasm.ltm_search_async('cache:', '{}', 10);

            return JSON.parse(searchResult);
        });

        expect(result.success).toBe(true);
        expect(result.count).toBe(3); // 2 users + 1 product
        expect(result.entries.every((e: any) => e.key.startsWith('cache:'))).toBe(true);
    });

    test('should filter search by metadata', async ({ page }) => {
        const result = await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;

            const searchResult = await wasm.ltm_search_async(
                '',
                JSON.stringify({ type: 'user' }),
                10
            );

            return JSON.parse(searchResult);
        });

        expect(result.success).toBe(true);
        expect(result.count).toBe(2);
        expect(result.entries.every((e: any) => e.metadata?.type === 'user')).toBe(true);
    });
});

test.describe('LTM Module - TTL and Expiration', () => {
    test.beforeEach(async ({ page }) => {
        await page.goto('/test.html');
        await waitForWasm(page);

        await page.evaluate(() => {
            const wasm = (window as any).teaWasmLlm;
            const entries = new Map();
            const blobs = new Map();

            wasm.set_ltm_handler({
                get: async (store: string, id: string) => entries.get(`${store}:${id}`) || null,
                put: async (store: string, data: string) => {
                    const obj = JSON.parse(data);
                    entries.set(`${store}:${obj.id}`, data);
                },
                delete: async (store: string, id: string) => entries.delete(`${store}:${id}`),
                list: async (store: string, prefix: string, limit: number) => {
                    const results: string[] = [];
                    for (const [key, value] of entries) {
                        if (key.startsWith(`${store}:`)) {
                            const entry = JSON.parse(value);
                            if (!prefix || entry.key.startsWith(prefix)) {
                                results.push(value);
                            }
                            if (results.length >= limit) break;
                        }
                    }
                    return JSON.stringify(results.map(r => JSON.parse(r)));
                },
                store_blob: async (id: string, data: string) => blobs.set(id, data),
                get_blob: async (id: string) => blobs.get(id) || null,
                delete_blob: async (id: string) => blobs.delete(id),
            });
        });
    });

    test('should store value with TTL', async ({ page }) => {
        const result = await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;

            // Store with 1 hour TTL (3600 seconds)
            const storeResult = await wasm.ltm_store_async(
                'ttl:test',
                JSON.stringify({ data: 'expires' }),
                JSON.stringify({ _cache_ttl: 3600 })
            );

            const retrieveResult = await wasm.ltm_retrieve_async('ttl:test', 'null');

            return {
                store: JSON.parse(storeResult),
                retrieve: JSON.parse(retrieveResult)
            };
        });

        expect(result.store.success).toBe(true);
        expect(result.retrieve.found).toBe(true);
    });

    test('should expire value based on _cache_expires_at', async ({ page }) => {
        const result = await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;

            // Store with past expiration (already expired)
            const pastTime = Date.now() - 1000; // 1 second ago
            const storeResult = await wasm.ltm_store_async(
                'expired:test',
                JSON.stringify({ data: 'expired' }),
                JSON.stringify({ _cache_expires_at: pastTime })
            );

            // Try to retrieve - should not find it (expired)
            const retrieveResult = await wasm.ltm_retrieve_async(
                'expired:test',
                JSON.stringify({ default: 'fallback' })
            );

            return {
                store: JSON.parse(storeResult),
                retrieve: JSON.parse(retrieveResult)
            };
        });

        expect(result.store.success).toBe(true);
        expect(result.retrieve.found).toBe(false);
        expect(result.retrieve.value).toEqual({ default: 'fallback' });
    });
});

test.describe('LTM Module - Configuration', () => {
    test.beforeEach(async ({ page }) => {
        await page.goto('/test.html');
        await waitForWasm(page);
    });

    test('should configure LTM backend', async ({ page }) => {
        const result = await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;

            const configResult = wasm.configure_ltm(JSON.stringify({
                storage_uri: 's3://test-bucket/ltm/',
                inline_threshold: 2048,
                enable_sync: true
            }));

            const getConfig = wasm.get_ltm_config();

            return {
                configure: JSON.parse(configResult),
                config: JSON.parse(getConfig)
            };
        });

        expect(result.configure.success).toBe(true);
        expect(result.config.storage_uri).toBe('s3://test-bucket/ltm/');
        expect(result.config.inline_threshold).toBe(2048);
        expect(result.config.enable_sync).toBe(true);
    });

    test('should get LTM statistics', async ({ page }) => {
        const result = await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;

            // Register handler
            const entries = new Map();
            wasm.set_ltm_handler({
                get: async (store: string, id: string) => entries.get(`${store}:${id}`) || null,
                put: async (store: string, data: string) => {
                    const obj = JSON.parse(data);
                    entries.set(`${store}:${obj.id}`, data);
                },
                delete: async (store: string, id: string) => entries.delete(`${store}:${id}`),
                list: async (store: string, prefix: string, limit: number) => {
                    const results: string[] = [];
                    for (const [key, value] of entries) {
                        if (key.startsWith(`${store}:`)) {
                            results.push(value);
                        }
                        if (results.length >= limit) break;
                    }
                    return JSON.stringify(results.map(r => JSON.parse(r)));
                },
                store_blob: async (id: string, data: string) => {},
                get_blob: async (id: string) => null,
                delete_blob: async (id: string) => {},
            });

            // Store some data
            await wasm.ltm_store_async('stats:1', '{"a": 1}', '{}');
            await wasm.ltm_store_async('stats:2', '{"b": 2}', '{}');

            // Get stats
            const statsResult = await wasm.ltm_stats_async();

            return JSON.parse(statsResult);
        });

        expect(result.success).toBe(true);
        expect(result.total_entries).toBe(2);
        expect(result.total_size_bytes).toBeGreaterThan(0);
    });
});

test.describe('LTM Module - Error Handling', () => {
    test.beforeEach(async ({ page }) => {
        await page.goto('/test.html');
        await waitForWasm(page);
    });

    test('should error when no handler registered', async ({ page }) => {
        const result = await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;

            // Clear any existing handler
            wasm.clear_ltm_handler();

            try {
                await wasm.ltm_store_async('test:key', '{}', '{}');
                return { error: null };
            } catch (e: any) {
                return { error: e.toString() };
            }
        });

        expect(result.error).toContain('No LTM handler registered');
    });

    test('should handle handler registration and clearing', async ({ page }) => {
        const result = await page.evaluate(async () => {
            const wasm = (window as any).teaWasmLlm;

            // Clear handler
            wasm.clear_ltm_handler();
            const before = wasm.has_ltm_handler();

            // Register mock handler
            wasm.set_ltm_handler({
                get: async () => null,
                put: async () => {},
                delete: async () => {},
                list: async () => '[]',
                store_blob: async () => {},
                get_blob: async () => null,
                delete_blob: async () => {},
            });
            const after = wasm.has_ltm_handler();

            // Clear again
            wasm.clear_ltm_handler();
            const afterClear = wasm.has_ltm_handler();

            return { before, after, afterClear };
        });

        expect(result.before).toBe(false);
        expect(result.after).toBe(true);
        expect(result.afterClear).toBe(false);
    });
});
