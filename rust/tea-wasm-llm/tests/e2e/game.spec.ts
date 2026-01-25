/**
 * Playwright E2E tests for TEA Game WASM (TEA-GAME-001.6)
 *
 * Tests the game engine WASM exports:
 * - game_init, game_start_session, game_generate_round
 * - game_submit_answer, game_submit_to_leaderboard
 * - game_get_leaderboard, game_get_session_stats
 * - LLM and Opik callback wiring
 * - Error handling with structured JSON responses
 */

import { test, expect } from '@playwright/test';

test.describe('TEA Game WASM E2E Tests', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to test page which loads WASM
    await page.goto('/tests/e2e/game-test-page.html');
  });

  test('WASM module loads successfully', async ({ page }) => {
    // Wait for test results
    await page.waitForFunction(
      () => (window as any).testResults?.total > 0,
      { timeout: 60000 }
    );

    const testResults = await page.evaluate(() => (window as any).testResults);
    expect(testResults).toBeDefined();
    expect(testResults.total).toBeGreaterThan(0);
  });

  test('All game tests pass', async ({ page }) => {
    // Wait for all tests to complete
    await page.waitForFunction(
      () => {
        const summary = document.querySelector('#summary');
        return summary?.textContent?.includes('Tests completed');
      },
      { timeout: 120000 }
    );

    const testResults = await page.evaluate(() => (window as any).testResults);

    console.log(`Test Results: ${testResults.passed} passed, ${testResults.failed} failed`);

    // Log failed tests for debugging
    testResults.results
      .filter((r: any) => !r.passed)
      .forEach((r: any) => {
        console.log(`FAILED: ${r.name} - ${r.message}`);
      });

    expect(testResults.failed).toBe(0);
    expect(testResults.passed).toBeGreaterThan(0);
  });

  test('game_init returns success response', async ({ page }) => {
    await page.waitForFunction(
      () => typeof (window as any).wasm?.game_init === 'function',
      { timeout: 60000 }
    );

    const result = await page.evaluate(() => {
      return JSON.parse((window as any).wasm.game_init());
    });

    expect(result.success).toBe(true);
    expect(result.data.initialized).toBe(true);
  });

  test('game_start_session creates session with username', async ({ page }) => {
    await page.waitForFunction(
      () => typeof (window as any).wasm?.game_start_session === 'function',
      { timeout: 60000 }
    );

    // Initialize first
    await page.evaluate(() => (window as any).wasm.game_init());

    const result = await page.evaluate(() => {
      return JSON.parse((window as any).wasm.game_start_session());
    });

    expect(result.success).toBe(true);
    expect(result.data.id).toBeTruthy();
    expect(result.data.username).toMatch(/^[A-Z][a-z]+[A-Z][a-z]+\d{2}$/); // SwiftFox42 pattern
    expect(result.data.total_answers).toBe(0);
    expect(result.data.correct_answers).toBe(0);
    expect(result.data.current_difficulty).toBeCloseTo(0.5, 1);
  });

  test('LLM callback wiring works', async ({ page }) => {
    await page.waitForFunction(
      () => typeof (window as any).wasm?.game_set_llm_handler === 'function',
      { timeout: 60000 }
    );

    // Check initial state
    let hasHandler = await page.evaluate(() => (window as any).wasm.game_has_llm_handler());
    expect(hasHandler).toBe(false);

    // Set handler
    await page.evaluate(() => {
      (window as any).wasm.game_set_llm_handler(async () => '{}');
    });

    hasHandler = await page.evaluate(() => (window as any).wasm.game_has_llm_handler());
    expect(hasHandler).toBe(true);

    // Clear handler
    await page.evaluate(() => (window as any).wasm.game_clear_llm_handler());

    hasHandler = await page.evaluate(() => (window as any).wasm.game_has_llm_handler());
    expect(hasHandler).toBe(false);
  });

  test('Opik callback wiring works', async ({ page }) => {
    await page.waitForFunction(
      () => typeof (window as any).wasm?.game_set_opik_handler === 'function',
      { timeout: 60000 }
    );

    // Check initial state
    let hasHandler = await page.evaluate(() => (window as any).wasm.game_has_opik_handler());
    expect(hasHandler).toBe(false);

    // Set handler
    await page.evaluate(() => {
      (window as any).wasm.game_set_opik_handler(() => {});
    });

    hasHandler = await page.evaluate(() => (window as any).wasm.game_has_opik_handler());
    expect(hasHandler).toBe(true);

    // Clear handler
    await page.evaluate(() => (window as any).wasm.game_clear_opik_handler());

    hasHandler = await page.evaluate(() => (window as any).wasm.game_has_opik_handler());
    expect(hasHandler).toBe(false);
  });

  test('Error responses have structured format', async ({ page }) => {
    await page.waitForFunction(
      () => typeof (window as any).wasm?.game_get_session_stats === 'function',
      { timeout: 60000 }
    );

    // Initialize but don't start session
    await page.evaluate(() => (window as any).wasm.game_init());

    const result = await page.evaluate(() => {
      return JSON.parse((window as any).wasm.game_get_session_stats());
    });

    expect(result.success).toBe(false);
    expect(result.error).toBeTruthy();
    expect(result.error_type).toBe('session_error');
  });

  test('Full game flow works', async ({ page }) => {
    await page.waitForFunction(
      () => typeof (window as any).wasm?.game_generate_round === 'function',
      { timeout: 60000 }
    );

    // Setup
    await page.evaluate(() => {
      const wasm = (window as any).wasm;
      wasm.game_init();
      wasm.game_set_llm_handler(async () => JSON.stringify({
        phrase: "The ___ is bright.",
        word: "sun",
        distractors: ["moon", "star", "sky", "day"]
      }));
    });

    // Start session
    const sessionResult = await page.evaluate(() => {
      return JSON.parse((window as any).wasm.game_start_session());
    });
    expect(sessionResult.success).toBe(true);

    // Generate round
    const roundResult = await page.evaluate(async () => {
      return JSON.parse(await (window as any).wasm.game_generate_round());
    });
    expect(roundResult.success).toBe(true);
    expect(roundResult.data.phrase).toBe("The ___ is bright.");
    expect(roundResult.data.choices).toHaveLength(5);
    expect(roundResult.data.choices).toContain("sun");

    // Submit correct answer
    const answerResult = await page.evaluate(() => {
      return JSON.parse((window as any).wasm.game_submit_answer("sun", 1500));
    });
    expect(answerResult.success).toBe(true);
    expect(answerResult.data.is_correct).toBe(true);
    expect(answerResult.data.correct_word).toBe("sun");

    // Check stats
    const statsResult = await page.evaluate(() => {
      return JSON.parse((window as any).wasm.game_get_session_stats());
    });
    expect(statsResult.success).toBe(true);
    expect(statsResult.data.total_answers).toBe(1);
    expect(statsResult.data.correct_answers).toBe(1);

    // Submit to leaderboard
    const leaderboardResult = await page.evaluate(() => {
      return JSON.parse((window as any).wasm.game_submit_to_leaderboard());
    });
    expect(leaderboardResult.success).toBe(true);
    expect(leaderboardResult.rank).toBeDefined();

    // Get leaderboard
    const getLeaderboardResult = await page.evaluate(() => {
      return JSON.parse((window as any).wasm.game_get_leaderboard(10));
    });
    expect(getLeaderboardResult.success).toBe(true);
    expect(Array.isArray(getLeaderboardResult.data)).toBe(true);
  });

  test('Invalid choice returns error', async ({ page }) => {
    await page.waitForFunction(
      () => typeof (window as any).wasm?.game_submit_answer === 'function',
      { timeout: 60000 }
    );

    // Setup with round
    await page.evaluate(async () => {
      const wasm = (window as any).wasm;
      wasm.game_init();
      wasm.game_set_llm_handler(async () => JSON.stringify({
        phrase: "Test",
        word: "correct",
        distractors: ["a", "b", "c", "d"]
      }));
      wasm.game_start_session();
      await wasm.game_generate_round();
    });

    // Submit invalid choice
    const result = await page.evaluate(() => {
      return JSON.parse((window as any).wasm.game_submit_answer("not_in_list", 1000));
    });

    expect(result.success).toBe(false);
    expect(result.error_type).toBe('invalid_choice');
    expect(result.error).toContain('not_in_list');
  });

  test('Double leaderboard submission is blocked', async ({ page }) => {
    await page.waitForFunction(
      () => typeof (window as any).wasm?.game_submit_to_leaderboard === 'function',
      { timeout: 60000 }
    );

    // Setup, play, and submit
    await page.evaluate(async () => {
      const wasm = (window as any).wasm;
      wasm.game_init();
      wasm.game_set_llm_handler(async () => JSON.stringify({
        phrase: "Test",
        word: "x",
        distractors: ["a", "b", "c", "d"]
      }));
      wasm.game_start_session();
      await wasm.game_generate_round();
      wasm.game_submit_answer("x", 1000);
      wasm.game_submit_to_leaderboard();
    });

    // Try to submit again
    const result = await page.evaluate(() => {
      return JSON.parse((window as any).wasm.game_submit_to_leaderboard());
    });

    expect(result.success).toBe(false);
    expect(result.error_type).toBe('already_submitted');
  });

  // =================================================================
  // TEA-GAME-001.8: Opik Integration Tests (AC-7)
  // =================================================================

  test('Opik spans have correct schema (AC-1, AC-2, AC-3, AC-4)', async ({ page }) => {
    await page.waitForFunction(
      () => typeof (window as any).wasm?.game_set_opik_handler === 'function',
      { timeout: 60000 }
    );

    // Setup and capture spans
    const spans = await page.evaluate(async () => {
      const wasm = (window as any).wasm;
      const captured: any[] = [];

      wasm.game_init();
      wasm.game_set_opik_handler((spanJson: string) => {
        captured.push(JSON.parse(spanJson));
      });
      wasm.game_set_llm_handler(async () => JSON.stringify({
        phrase: "The ___ is bright.",
        word: "sun",
        distractors: ["moon", "star", "sky", "day"]
      }));

      // Play full game flow
      wasm.game_start_session();
      await wasm.game_generate_round();
      wasm.game_submit_answer("sun", 1500);
      wasm.game_submit_to_leaderboard();

      return captured;
    });

    // Verify we captured spans
    expect(spans.length).toBeGreaterThan(0);

    // Find and verify session span (AC-2)
    const sessionSpan = spans.find((s: any) => s.span_type === 'session');
    expect(sessionSpan).toBeDefined();
    expect(sessionSpan.name).toBe('game_session');
    expect(sessionSpan.span_id).toBeTruthy();
    expect(sessionSpan.trace_id).toBeTruthy();
    expect(sessionSpan.metadata).toBeDefined();
    expect(sessionSpan.metadata.username).toBeTruthy();

    // Find and verify round span (AC-1)
    const roundSpans = spans.filter((s: any) => s.span_type === 'round');
    expect(roundSpans.length).toBeGreaterThan(0);

    // Find answer span (has is_correct field)
    const answerSpan = roundSpans.find((s: any) => s.metadata.is_correct !== undefined);
    expect(answerSpan).toBeDefined();
    expect(answerSpan.metadata.correct_word).toBeTruthy();
    expect(answerSpan.metadata.selected_word).toBeTruthy();
    expect(typeof answerSpan.metadata.is_correct).toBe('boolean');
    expect(typeof answerSpan.metadata.response_time_ms).toBe('number');
    expect(typeof answerSpan.metadata.difficulty).toBe('number');

    // Find and verify leaderboard span (AC-4)
    const leaderboardSpan = spans.find((s: any) => s.span_type === 'leaderboard_submit');
    expect(leaderboardSpan).toBeDefined();
    expect(leaderboardSpan.name).toBe('leaderboard_submit');
    expect(leaderboardSpan.metadata.username).toBeTruthy();
    expect(typeof leaderboardSpan.metadata.final_score).toBe('number');
    expect(typeof leaderboardSpan.metadata.accuracy).toBe('number');

    // Verify trace_id correlation (AC-2)
    const sessionTraceId = sessionSpan.span_id;
    roundSpans.forEach((span: any) => {
      expect(span.trace_id).toBe(sessionTraceId);
    });
    expect(leaderboardSpan.trace_id).toBe(sessionTraceId);
  });

  test('Game functions normally without Opik (AC-6 graceful degradation)', async ({ page }) => {
    await page.waitForFunction(
      () => typeof (window as any).wasm?.game_init === 'function',
      { timeout: 60000 }
    );

    // Setup WITHOUT Opik handler
    const result = await page.evaluate(async () => {
      const wasm = (window as any).wasm;

      wasm.game_init();
      wasm.game_clear_opik_handler(); // Ensure no handler

      wasm.game_set_llm_handler(async () => JSON.stringify({
        phrase: "Test phrase",
        word: "test",
        distractors: ["a", "b", "c", "d"]
      }));

      // Verify no handler
      const hasHandler = wasm.game_has_opik_handler();

      // Full game flow should work without errors
      const session = JSON.parse(wasm.game_start_session());
      const round = JSON.parse(await wasm.game_generate_round());
      const answer = JSON.parse(wasm.game_submit_answer("test", 1000));
      const leaderboard = JSON.parse(wasm.game_submit_to_leaderboard());

      return {
        hasHandler,
        sessionSuccess: session.success,
        roundSuccess: round.success,
        answerSuccess: answer.success,
        leaderboardSuccess: leaderboard.success
      };
    });

    expect(result.hasHandler).toBe(false);
    expect(result.sessionSuccess).toBe(true);
    expect(result.roundSuccess).toBe(true);
    expect(result.answerSuccess).toBe(true);
    expect(result.leaderboardSuccess).toBe(true);
  });
});
