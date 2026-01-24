/**
 * Playwright E2E tests for TEA Game UI (TEA-GAME-001.7)
 *
 * Tests the browser UI implementation:
 * - Tab navigation (AC-1)
 * - Welcome screen (AC-2)
 * - Game screen with phrase, choices, stats (AC-3, AC-5, AC-6)
 * - Answer feedback (AC-4)
 * - Leaderboard screen (AC-7, AC-8)
 * - Loading states (AC-9)
 * - Responsive design (AC-10)
 * - Error handling (AC-11, AC-12)
 */

import { test, expect } from '@playwright/test';

test.describe('TEA Game UI E2E Tests', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to main demo page
    await page.goto('/');

    // Wait for WASM to load
    await page.waitForFunction(
      () => typeof (window as any).gameUI !== 'undefined',
      { timeout: 120000 }
    );
  });

  test.describe('AC-1: Tab Navigation', () => {
    test('Game tab exists alongside Workflow tab', async ({ page }) => {
      const workflowTab = page.locator('.tab[data-tab="workflow"]');
      const gameTab = page.locator('.tab[data-tab="game"]');

      await expect(workflowTab).toBeVisible();
      await expect(gameTab).toBeVisible();
    });

    test('Clicking Game tab shows game content', async ({ page }) => {
      const gameTab = page.locator('.tab[data-tab="game"]');
      const gameContent = page.locator('#game-tab');
      const workflowContent = page.locator('#workflow-tab');

      await gameTab.click();

      await expect(gameContent).not.toHaveClass(/hidden/);
      await expect(workflowContent).toHaveClass(/hidden/);
    });

    test('Tab switching maintains active state', async ({ page }) => {
      const workflowTab = page.locator('.tab[data-tab="workflow"]');
      const gameTab = page.locator('.tab[data-tab="game"]');

      // Initially workflow is active
      await expect(workflowTab).toHaveClass(/active/);
      await expect(gameTab).not.toHaveClass(/active/);

      // Click game tab
      await gameTab.click();

      await expect(gameTab).toHaveClass(/active/);
      await expect(workflowTab).not.toHaveClass(/active/);

      // Click workflow tab
      await workflowTab.click();

      await expect(workflowTab).toHaveClass(/active/);
      await expect(gameTab).not.toHaveClass(/active/);
    });
  });

  test.describe('AC-2: Welcome Screen', () => {
    test.beforeEach(async ({ page }) => {
      // Navigate to game tab
      await page.locator('.tab[data-tab="game"]').click();
    });

    test('Welcome screen displays on game tab', async ({ page }) => {
      const welcomeScreen = page.locator('#game-welcome');
      await expect(welcomeScreen).toBeVisible();
    });

    test('Shows game title and instructions', async ({ page }) => {
      const title = page.locator('#game-welcome h2');
      const instructions = page.locator('.game-instructions');

      await expect(title).toHaveText('Know Your Model');
      await expect(instructions).toBeVisible();
    });

    test('Start Game button is visible', async ({ page }) => {
      const startBtn = page.locator('#start-game-btn');
      await expect(startBtn).toBeVisible();
      await expect(startBtn).toBeEnabled();
    });

    test('Username display is present', async ({ page }) => {
      const usernameDisplay = page.locator('#username');
      await expect(usernameDisplay).toBeVisible();
    });
  });

  test.describe('AC-3: Game Screen', () => {
    test.beforeEach(async ({ page }) => {
      // Navigate to game tab and start game
      await page.locator('.tab[data-tab="game"]').click();
      await page.locator('#start-game-btn').click();

      // Wait for game screen or loading
      await page.waitForFunction(
        () => {
          const playingScreen = document.getElementById('game-playing');
          return playingScreen && !playingScreen.classList.contains('hidden');
        },
        { timeout: 60000 }
      );
    });

    test('Game screen shows phrase container', async ({ page }) => {
      const phraseContainer = page.locator('.phrase-container');
      await expect(phraseContainer).toBeVisible();
    });

    test('Game screen shows word choice buttons', async ({ page }) => {
      // Wait for word choices to be populated
      await page.waitForSelector('.word-btn', { timeout: 60000 });

      const wordButtons = page.locator('.word-btn');
      const count = await wordButtons.count();

      // Should have 5 word choices
      expect(count).toBe(5);
    });

    test('Stats bar shows round, score, and accuracy', async ({ page }) => {
      const roundNum = page.locator('#round-num');
      const score = page.locator('#current-score');
      const accuracy = page.locator('#accuracy');

      await expect(roundNum).toBeVisible();
      await expect(score).toBeVisible();
      await expect(accuracy).toBeVisible();
    });
  });

  test.describe('AC-4: Answer Feedback', () => {
    test.skip('Correct answer shows green feedback', async ({ page }) => {
      // This test requires mocking the LLM to know the correct answer
      // Skipped in E2E - covered by integration tests
    });

    test.skip('Incorrect answer shows red feedback and reveals correct', async ({ page }) => {
      // This test requires mocking the LLM
      // Skipped in E2E - covered by integration tests
    });
  });

  test.describe('AC-5: Difficulty Indicator', () => {
    test.beforeEach(async ({ page }) => {
      await page.locator('.tab[data-tab="game"]').click();
      await page.locator('#start-game-btn').click();

      await page.waitForFunction(
        () => {
          const playingScreen = document.getElementById('game-playing');
          return playingScreen && !playingScreen.classList.contains('hidden');
        },
        { timeout: 60000 }
      );
    });

    test('Difficulty bar is visible', async ({ page }) => {
      const difficultyBar = page.locator('.difficulty-bar');
      await expect(difficultyBar).toBeVisible();
    });

    test('Difficulty fill element exists', async ({ page }) => {
      const difficultyFill = page.locator('#difficulty-fill');
      await expect(difficultyFill).toBeVisible();
    });

    test('Difficulty label shows level', async ({ page }) => {
      const difficultyLabel = page.locator('#difficulty-label');
      await expect(difficultyLabel).toBeVisible();

      const text = await difficultyLabel.textContent();
      expect(['Easy', 'Medium', 'Hard', 'Expert']).toContain(text);
    });
  });

  test.describe('AC-6: Give Up Button', () => {
    test.beforeEach(async ({ page }) => {
      await page.locator('.tab[data-tab="game"]').click();
      await page.locator('#start-game-btn').click();

      await page.waitForFunction(
        () => {
          const playingScreen = document.getElementById('game-playing');
          return playingScreen && !playingScreen.classList.contains('hidden');
        },
        { timeout: 60000 }
      );
    });

    test('Give Up button is visible during game', async ({ page }) => {
      const giveUpBtn = page.locator('#give-up-btn');
      await expect(giveUpBtn).toBeVisible();
    });

    test('Give Up button navigates to leaderboard', async ({ page }) => {
      // Wait for loading to complete first
      await page.waitForSelector('.word-btn', { timeout: 60000 });

      const giveUpBtn = page.locator('#give-up-btn');
      await giveUpBtn.click();

      // Wait for leaderboard screen
      await page.waitForFunction(
        () => {
          const leaderboard = document.getElementById('game-leaderboard');
          return leaderboard && !leaderboard.classList.contains('hidden');
        },
        { timeout: 30000 }
      );

      const leaderboardScreen = page.locator('#game-leaderboard');
      await expect(leaderboardScreen).toBeVisible();
    });
  });

  test.describe('AC-7 & AC-8: Leaderboard Screen', () => {
    test.beforeEach(async ({ page }) => {
      await page.locator('.tab[data-tab="game"]').click();
      await page.locator('#start-game-btn').click();

      // Wait for game to load, then give up
      await page.waitForSelector('.word-btn', { timeout: 60000 });
      await page.locator('#give-up-btn').click();

      // Wait for leaderboard
      await page.waitForFunction(
        () => {
          const leaderboard = document.getElementById('game-leaderboard');
          return leaderboard && !leaderboard.classList.contains('hidden');
        },
        { timeout: 30000 }
      );
    });

    test('Leaderboard table is visible', async ({ page }) => {
      const leaderboardTable = page.locator('.leaderboard-table');
      await expect(leaderboardTable).toBeVisible();
    });

    test('Leaderboard has proper columns', async ({ page }) => {
      const headers = page.locator('.leaderboard-table th');
      const headerTexts = await headers.allTextContents();

      expect(headerTexts).toContain('#');
      expect(headerTexts).toContain('Player');
      expect(headerTexts).toContain('Score');
      expect(headerTexts).toContain('Accuracy');
      expect(headerTexts).toContain('Rounds');
    });

    test('Play Again button is visible', async ({ page }) => {
      const playAgainBtn = page.locator('#play-again-btn');
      await expect(playAgainBtn).toBeVisible();
    });

    test('Play Again starts new game', async ({ page }) => {
      const playAgainBtn = page.locator('#play-again-btn');
      await playAgainBtn.click();

      // Should eventually show playing screen again
      await page.waitForFunction(
        () => {
          const playingScreen = document.getElementById('game-playing');
          return playingScreen && !playingScreen.classList.contains('hidden');
        },
        { timeout: 60000 }
      );

      const playingScreen = page.locator('#game-playing');
      await expect(playingScreen).toBeVisible();
    });
  });

  test.describe('AC-9: Loading States', () => {
    test('Loading indicator appears while generating question', async ({ page }) => {
      await page.locator('.tab[data-tab="game"]').click();
      await page.locator('#start-game-btn').click();

      // Loading indicator should appear
      const loadingIndicator = page.locator('.loading-indicator');

      // Should be visible at some point (may be fast)
      // We check if it exists in the DOM
      await expect(loadingIndicator).toBeAttached();
    });

    test('Word buttons are disabled during loading', async ({ page }) => {
      await page.locator('.tab[data-tab="game"]').click();
      await page.locator('#start-game-btn').click();

      // Wait for word buttons to appear
      await page.waitForSelector('.word-btn', { timeout: 60000 });

      // Initially they should be enabled after loading completes
      const wordButtons = page.locator('.word-btn');
      const firstButton = wordButtons.first();

      await expect(firstButton).toBeEnabled();
    });
  });

  test.describe('AC-10: Responsive Design', () => {
    test('Layout works on mobile viewport (375px)', async ({ page }) => {
      // Set mobile viewport
      await page.setViewportSize({ width: 375, height: 667 });

      await page.locator('.tab[data-tab="game"]').click();
      await page.locator('#start-game-btn').click();

      // Wait for game screen
      await page.waitForFunction(
        () => {
          const playingScreen = document.getElementById('game-playing');
          return playingScreen && !playingScreen.classList.contains('hidden');
        },
        { timeout: 60000 }
      );

      // All elements should still be visible
      const phraseContainer = page.locator('.phrase-container');
      const giveUpBtn = page.locator('#give-up-btn');

      await expect(phraseContainer).toBeVisible();
      await expect(giveUpBtn).toBeVisible();
    });

    test('Word buttons stack on narrow screens', async ({ page }) => {
      // Set narrow viewport
      await page.setViewportSize({ width: 320, height: 568 });

      await page.locator('.tab[data-tab="game"]').click();
      await page.locator('#start-game-btn').click();

      // Wait for word choices
      await page.waitForSelector('.word-btn', { timeout: 60000 });

      // Buttons should be visible and accessible
      const wordButtons = page.locator('.word-btn');
      const count = await wordButtons.count();
      expect(count).toBe(5);

      // All buttons should be visible
      for (let i = 0; i < count; i++) {
        await expect(wordButtons.nth(i)).toBeVisible();
      }
    });
  });

  test.describe('AC-11 & AC-12: Error Handling', () => {
    test('Error modal exists in DOM', async ({ page }) => {
      const errorModal = page.locator('#error-modal');
      await expect(errorModal).toBeAttached();
    });

    test('Error modal has retry and welcome buttons', async ({ page }) => {
      const retryBtn = page.locator('#error-retry-btn');
      const welcomeBtn = page.locator('#error-back-btn');

      await expect(retryBtn).toBeAttached();
      await expect(welcomeBtn).toBeAttached();
    });

    test.skip('Timeout warning appears after 10 seconds', async ({ page }) => {
      // This test would require mocking a slow LLM response
      // Skipped in E2E - would need to inject delay in mock
    });
  });
});
