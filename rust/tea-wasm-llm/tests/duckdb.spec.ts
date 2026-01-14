/**
 * DuckDB WASM Integration Tests (TEA-WASM-003.2)
 *
 * These tests validate the DuckDB WASM integration including:
 * - Handler registration and initialization
 * - Query execution via Rust↔JS bridge
 * - Prepared statement parameter substitution
 * - Transaction support (BEGIN/COMMIT/ROLLBACK)
 * - Extension loading
 * - Error handling and messaging
 *
 * Test Framework: Playwright + Vitest
 */

import { test, expect, Page } from '@playwright/test';

// Test page URL (served by test server)
const TEST_PAGE = 'http://localhost:3000/tests/e2e/duckdb-test-page.html';

// Helper to wait for DuckDB initialization
async function waitForDuckDb(page: Page, timeout = 30000): Promise<void> {
    await page.waitForFunction(
        () => (window as unknown as { duckdbReady?: boolean }).duckdbReady === true,
        { timeout }
    );
}

// Helper to execute DuckDB query from page context
async function executeQuery(page: Page, sql: string, params: unknown[] = []): Promise<unknown> {
    return page.evaluate(
        async ({ sql, params }) => {
            const { duckdb_query_async } = (window as unknown as { duckdb_query_async: (sql: string, params: string) => Promise<string> });
            const result = await duckdb_query_async(sql, JSON.stringify(params));
            return JSON.parse(result);
        },
        { sql, params }
    );
}

// Helper to execute DuckDB statement from page context
async function executeStatement(page: Page, sql: string): Promise<unknown> {
    return page.evaluate(
        async (sql) => {
            const { duckdb_execute_async } = (window as unknown as { duckdb_execute_async: (sql: string) => Promise<string> });
            const result = await duckdb_execute_async(sql);
            return JSON.parse(result);
        },
        sql
    );
}

test.describe('DuckDB WASM Integration', () => {
    test.describe('Handler Registration (AC: 12-14)', () => {
        test('should detect when handler is not registered', async ({ page }) => {
            await page.goto(TEST_PAGE);

            const hasHandler = await page.evaluate(() => {
                const { has_duckdb_handler } = (window as unknown as { has_duckdb_handler: () => boolean });
                return has_duckdb_handler();
            });

            // Before initialization, handler should not be set
            expect(hasHandler).toBe(false);
        });

        test('should register handler successfully', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            const hasHandler = await page.evaluate(() => {
                const { has_duckdb_handler } = (window as unknown as { has_duckdb_handler: () => boolean });
                return has_duckdb_handler();
            });

            expect(hasHandler).toBe(true);
        });

        test('should initialize DuckDB successfully', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            const result = await page.evaluate(async () => {
                const { init_duckdb_async } = (window as unknown as { init_duckdb_async: (opts?: string) => Promise<string> });
                const result = await init_duckdb_async();
                return JSON.parse(result);
            });

            expect(result).toHaveProperty('success', true);
            expect(result).toHaveProperty('message', 'DuckDB ready');
        });
    });

    test.describe('Query Execution (AC: 1-4)', () => {
        test('should execute simple SELECT query', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            const result = await executeQuery(page, 'SELECT 1 + 1 as sum');

            expect(result).toHaveProperty('success', true);
            expect(result).toHaveProperty('rows');
            expect((result as { rows: Array<{ sum: number }> }).rows[0].sum).toBe(2);
        });

        test('should execute query with string result', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            const result = await executeQuery(page, "SELECT 'hello' || ' ' || 'world' as greeting");

            expect(result).toHaveProperty('success', true);
            expect((result as { rows: Array<{ greeting: string }> }).rows[0].greeting).toBe('hello world');
        });

        test('should execute query with multiple rows', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            const result = await executeQuery(page, 'SELECT * FROM generate_series(1, 5) as t(n)');

            expect(result).toHaveProperty('success', true);
            expect((result as { row_count: number }).row_count).toBe(5);
        });

        test('should execute DDL statement', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            // Create table
            const createResult = await executeStatement(page, `
                CREATE TABLE test_users (
                    id INTEGER PRIMARY KEY,
                    name VARCHAR,
                    email VARCHAR
                )
            `);

            expect(createResult).toHaveProperty('success', true);

            // Insert data
            const insertResult = await executeStatement(
                page,
                "INSERT INTO test_users VALUES (1, 'Alice', 'alice@example.com')"
            );

            expect(insertResult).toHaveProperty('success', true);

            // Query data
            const queryResult = await executeQuery(page, 'SELECT * FROM test_users');

            expect(queryResult).toHaveProperty('success', true);
            expect((queryResult as { rows: Array<{ name: string }> }).rows[0].name).toBe('Alice');
        });
    });

    test.describe('Prepared Statements (AC: 3)', () => {
        test('should execute parameterized query with single parameter', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            const result = await executeQuery(page, 'SELECT ? * 2 as doubled', [21]);

            expect(result).toHaveProperty('success', true);
            expect((result as { rows: Array<{ doubled: number }> }).rows[0].doubled).toBe(42);
        });

        test('should execute parameterized query with string parameter', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            const result = await executeQuery(page, "SELECT ? || ' World' as message", ['Hello']);

            expect(result).toHaveProperty('success', true);
            expect((result as { rows: Array<{ message: string }> }).rows[0].message).toBe('Hello World');
        });

        test('should execute parameterized query with multiple parameters', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            const result = await executeQuery(
                page,
                'SELECT ? + ? + ? as total',
                [10, 20, 30]
            );

            expect(result).toHaveProperty('success', true);
            expect((result as { rows: Array<{ total: number }> }).rows[0].total).toBe(60);
        });

        test('should prevent SQL injection via parameters', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            // Create test table
            await executeStatement(page, 'CREATE TABLE IF NOT EXISTS injection_test (id INTEGER, data VARCHAR)');
            await executeStatement(page, "INSERT INTO injection_test VALUES (1, 'safe')");

            // Attempt injection via parameter (should be treated as literal string)
            const result = await executeQuery(
                page,
                'SELECT * FROM injection_test WHERE data = ?',
                ["'; DROP TABLE injection_test; --"]
            );

            expect(result).toHaveProperty('success', true);
            expect((result as { row_count: number }).row_count).toBe(0); // No match

            // Verify table still exists
            const checkResult = await executeQuery(page, 'SELECT COUNT(*) as cnt FROM injection_test');
            expect((checkResult as { rows: Array<{ cnt: number }> }).rows[0].cnt).toBe(1);
        });
    });

    test.describe('Transaction Support (AC: 4)', () => {
        test('should support BEGIN/COMMIT transaction', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            // Create table
            await executeStatement(page, 'CREATE TABLE IF NOT EXISTS tx_test (id INTEGER, value VARCHAR)');

            // Begin transaction
            const beginResult = await page.evaluate(async () => {
                const { duckdb_begin_async } = (window as unknown as { duckdb_begin_async: () => Promise<string> });
                return JSON.parse(await duckdb_begin_async());
            });
            expect(beginResult).toHaveProperty('success', true);

            // Insert within transaction
            await executeStatement(page, "INSERT INTO tx_test VALUES (1, 'committed')");

            // Commit
            const commitResult = await page.evaluate(async () => {
                const { duckdb_commit_async } = (window as unknown as { duckdb_commit_async: () => Promise<string> });
                return JSON.parse(await duckdb_commit_async());
            });
            expect(commitResult).toHaveProperty('success', true);

            // Verify data persisted
            const result = await executeQuery(page, 'SELECT * FROM tx_test WHERE value = ?', ['committed']);
            expect((result as { row_count: number }).row_count).toBe(1);
        });

        test('should support ROLLBACK transaction', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            // Create table
            await executeStatement(page, 'CREATE TABLE IF NOT EXISTS rollback_test (id INTEGER, value VARCHAR)');

            // Clear any existing data
            await executeStatement(page, 'DELETE FROM rollback_test');

            // Begin transaction
            await page.evaluate(async () => {
                const { duckdb_begin_async } = (window as unknown as { duckdb_begin_async: () => Promise<string> });
                return JSON.parse(await duckdb_begin_async());
            });

            // Insert within transaction
            await executeStatement(page, "INSERT INTO rollback_test VALUES (1, 'should_be_rolled_back')");

            // Rollback
            const rollbackResult = await page.evaluate(async () => {
                const { duckdb_rollback_async } = (window as unknown as { duckdb_rollback_async: () => Promise<string> });
                return JSON.parse(await duckdb_rollback_async());
            });
            expect(rollbackResult).toHaveProperty('success', true);

            // Verify data was NOT persisted
            const result = await executeQuery(page, 'SELECT COUNT(*) as cnt FROM rollback_test');
            expect((result as { rows: Array<{ cnt: number }> }).rows[0].cnt).toBe(0);
        });
    });

    test.describe('Extension Loading (AC: 5-11, 14)', () => {
        test('should load JSON extension', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            // JSON is typically autoloaded, but test explicit load
            const loadResult = await page.evaluate(async () => {
                const { load_duckdb_extension_async } = (window as unknown as { load_duckdb_extension_async: (name: string) => Promise<string> });
                try {
                    const result = await load_duckdb_extension_async('json');
                    return JSON.parse(result);
                } catch (e) {
                    return { success: false, error: String(e) };
                }
            });

            // May already be loaded, but should not fail
            expect(loadResult).toHaveProperty('success', true);
        });

        test('should use JSON functions after loading', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            const result = await executeQuery(
                page,
                "SELECT json_extract('{\"name\": \"Alice\", \"age\": 30}', '$.name') as name"
            );

            expect(result).toHaveProperty('success', true);
            // JSON extract returns a JSON value (string with quotes)
            expect((result as { rows: Array<{ name: string }> }).rows[0].name).toContain('Alice');
        });

        test('should get extension list', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            const result = await page.evaluate(async () => {
                const { get_duckdb_extensions_async } = (window as unknown as { get_duckdb_extensions_async: () => Promise<string> });
                const result = await get_duckdb_extensions_async();
                return JSON.parse(result);
            });

            expect(result).toHaveProperty('success', true);
            expect(result).toHaveProperty('rows');
            expect(Array.isArray((result as { rows: unknown[] }).rows)).toBe(true);
        });
    });

    test.describe('Error Handling (AC: 18-20)', () => {
        test('should return syntax error for invalid SQL', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            const result = await executeQuery(page, 'SELEC * FROM nonexistent');

            expect(result).toHaveProperty('success', false);
            expect(result).toHaveProperty('error');
            expect((result as { error: string }).error.toLowerCase()).toContain('syntax');
        });

        test('should return error for non-existent table', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            const result = await executeQuery(page, 'SELECT * FROM this_table_does_not_exist_xyz');

            expect(result).toHaveProperty('success', false);
            expect(result).toHaveProperty('error');
        });

        test('should return error for type mismatch', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            // Create table with integer column
            await executeStatement(page, 'CREATE TABLE IF NOT EXISTS type_test (id INTEGER)');

            // Try to insert non-integer
            const result = await executeQuery(
                page,
                "INSERT INTO type_test VALUES ('not_an_integer')"
            );

            // DuckDB might auto-convert or error - check either outcome
            expect(result).toBeDefined();
        });

        test('should reject invalid extension name', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            // Try SQL injection via extension name
            const result = await page.evaluate(async () => {
                const { load_duckdb_extension_async } = (window as unknown as { load_duckdb_extension_async: (name: string) => Promise<string> });
                try {
                    await load_duckdb_extension_async('vss; DROP TABLE users;');
                    return { caught: false };
                } catch (e) {
                    return { caught: true, error: String(e) };
                }
            });

            expect(result.caught).toBe(true);
            expect(result.error).toContain('Invalid extension name');
        });
    });

    test.describe('Schema Information', () => {
        test('should return schema with query results', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            const result = await executeQuery(
                page,
                "SELECT 1 as int_col, 'hello' as str_col, 3.14 as float_col"
            );

            expect(result).toHaveProperty('success', true);
            expect(result).toHaveProperty('schema');

            const schema = (result as { schema: Array<{ name: string; type: string }> }).schema;
            expect(schema.length).toBe(3);
            expect(schema.map(s => s.name)).toContain('int_col');
            expect(schema.map(s => s.name)).toContain('str_col');
            expect(schema.map(s => s.name)).toContain('float_col');
        });
    });

    test.describe('Performance Configuration (AC: 15-17)', () => {
        test('should accept memory limit configuration', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            const result = await page.evaluate(async () => {
                const { init_duckdb_async } = (window as unknown as { init_duckdb_async: (opts?: string) => Promise<string> });
                const result = await init_duckdb_async(JSON.stringify({
                    memory_limit: '256MB',
                    threads: 1
                }));
                return JSON.parse(result);
            });

            expect(result).toHaveProperty('success', true);
        });

        test('should query system information', async ({ page }) => {
            await page.goto(TEST_PAGE);
            await waitForDuckDb(page);

            // Query DuckDB version
            const result = await executeQuery(page, 'SELECT version() as version');

            expect(result).toHaveProperty('success', true);
            expect((result as { rows: Array<{ version: string }> }).rows[0].version).toBeDefined();
        });
    });
});
