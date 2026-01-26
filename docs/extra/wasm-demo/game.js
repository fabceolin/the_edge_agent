/**
 * TEA WASM LLM - Know Your Model Game UI
 *
 * State machine managing game screens and WASM integration.
 * Implements TEA-GAME-001.7 acceptance criteria.
 */

// Game states
export const GameState = {
  WELCOME: 'welcome',
  PLAYING: 'playing',
  LOADING: 'loading',
  LEADERBOARD: 'leaderboard',
  ERROR: 'error',
};

// Timeout constants (AC-12)
const LLM_WARNING_TIMEOUT_MS = 15000; // Show "Taking longer than expected..." after 15s
const LLM_CALL_TIMEOUT_MS = 90000; // 90 second timeout for LLM calls (browser inference is slow)

/**
 * GameUI - Main game controller class
 * Manages screen transitions, WASM calls, and UI updates
 */
export class GameUI {
  constructor(wasmModule) {
    // WASM module with game functions
    this.wasm = wasmModule;

    // Game state
    this.state = GameState.WELCOME;
    this.session = null;
    this.currentRound = null;
    this.roundNumber = 0;
    this.roundStartTime = null;

    // Timeout handles
    this.warningTimeoutId = null;
    this.callTimeoutId = null;

    // Initialize DOM elements and bind events
    this.initElements();
    this.bindEvents();

    console.log('[TEA-GAME] GameUI initialized');
  }

  /**
   * Initialize DOM element references
   */
  initElements() {
    // Screens
    this.welcomeScreen = document.getElementById('game-welcome');
    this.playingScreen = document.getElementById('game-playing');
    this.leaderboardScreen = document.getElementById('game-leaderboard');

    // Welcome screen elements
    this.usernameEl = document.getElementById('username');
    this.startGameBtn = document.getElementById('start-game-btn');

    // Playing screen elements
    this.phraseEl = document.getElementById('phrase-text');
    this.choicesEl = document.getElementById('word-choices');
    this.loadingEl = document.getElementById('loading-indicator');
    this.loadingText = this.loadingEl?.querySelector('span');
    this.roundNumEl = document.getElementById('round-num');
    this.currentScoreEl = document.getElementById('current-score');
    this.accuracyEl = document.getElementById('accuracy');
    this.difficultyFillEl = document.getElementById('difficulty-fill');
    this.difficultyLabelEl = document.getElementById('difficulty-label');
    this.giveUpBtn = document.getElementById('give-up-btn');

    // Leaderboard screen elements
    this.leaderboardBody = document.getElementById('leaderboard-body');
    this.playAgainBtn = document.getElementById('play-again-btn');
    this.yourScoreSummary = document.querySelector('.your-score-summary');

    // Error modal elements
    this.errorModal = document.getElementById('error-modal');
    this.errorMessage = document.getElementById('error-message');
    this.errorRetryBtn = document.getElementById('error-retry-btn');
    this.errorWelcomeBtn = document.getElementById('error-welcome-btn');

    // Validate required elements
    if (!this.welcomeScreen || !this.playingScreen || !this.leaderboardScreen) {
      console.error('[TEA-GAME] Missing required screen elements');
    }
  }

  /**
   * Bind event listeners
   */
  bindEvents() {
    // Start game button
    if (this.startGameBtn) {
      this.startGameBtn.addEventListener('click', () => this.startGame());
    }

    // Give up button
    if (this.giveUpBtn) {
      this.giveUpBtn.addEventListener('click', () => this.giveUp());
    }

    // Play again button
    if (this.playAgainBtn) {
      this.playAgainBtn.addEventListener('click', () => this.playAgain());
    }

    // Word choice buttons (event delegation)
    if (this.choicesEl) {
      this.choicesEl.addEventListener('click', (e) => {
        if (e.target.classList.contains('word-btn') && this.state === GameState.PLAYING) {
          this.submitAnswer(e.target.textContent);
        }
      });
    }

    // Error modal buttons
    if (this.errorRetryBtn) {
      this.errorRetryBtn.addEventListener('click', () => this.retryLastAction());
    }
    // Error welcome button (may be 'error-welcome-btn' or 'error-back-btn')
    const errorWelcomeBtn = document.getElementById('error-welcome-btn') || document.getElementById('error-back-btn');
    if (errorWelcomeBtn) {
      errorWelcomeBtn.addEventListener('click', () => this.returnToWelcome());
    }
  }

  /**
   * Called when the game tab is activated
   */
  onTabActivated() {
    console.log('[TEA-GAME] Game tab activated');
    // If no session exists, start fresh on welcome screen
    if (!this.session && this.state !== GameState.WELCOME) {
      this.showScreen(GameState.WELCOME);
    }
  }

  /**
   * Start a new game session
   */
  async startGame() {
    console.log('[TEA-GAME] Starting game...');
    this.lastAction = 'startGame';

    try {
      // Disable button during WASM call
      if (this.startGameBtn) {
        this.startGameBtn.disabled = true;
        this.startGameBtn.textContent = 'Starting...';
      }

      // Call WASM to start session
      const resultJson = this.wasm.game_start_session();
      const result = JSON.parse(resultJson);

      if (result.success) {
        this.session = result.data;
        this.roundNumber = 0;

        // Display username
        if (this.usernameEl) {
          this.usernameEl.textContent = this.session.username;
        }

        console.log(`[TEA-GAME] Session started: ${this.session.username}`);

        // Switch to playing screen and generate first round
        this.showScreen(GameState.PLAYING);
        await this.nextRound();
      } else {
        throw new Error(result.error || 'Failed to start session');
      }
    } catch (error) {
      console.error('[TEA-GAME] Start game error:', error);
      this.showError(error.message);
    } finally {
      // Re-enable button
      if (this.startGameBtn) {
        this.startGameBtn.disabled = false;
        this.startGameBtn.textContent = 'Start Game';
      }
    }
  }

  /**
   * Generate and display the next round
   */
  async nextRound() {
    console.log('[TEA-GAME] Generating next round...');
    this.lastAction = 'nextRound';

    // Show loading state (AC-9)
    this.showLoading(true);
    this.disableChoices(true);

    try {
      // Call WASM to generate round (with timeout - AC-12)
      const resultJson = await this.callWithTimeout(
        () => this.wasm.game_generate_round(),
        LLM_CALL_TIMEOUT_MS,
        'round generation'
      );

      const result = JSON.parse(resultJson);

      if (result.success) {
        this.currentRound = result.data;
        this.roundNumber++;
        this.roundStartTime = performance.now();

        // Render the round
        this.renderRound();

        // Update stats display
        this.updateStats();

        // Enable word choice buttons
        this.disableChoices(false);

        console.log(`[TEA-GAME] Round ${this.roundNumber} ready`);
      } else {
        throw new Error(result.error || 'Failed to generate round');
      }
    } catch (error) {
      console.error('[TEA-GAME] Round generation error:', error);
      this.showError(error.message);
    } finally {
      this.showLoading(false);
    }
  }

  /**
   * Render the current round (phrase and word choices)
   */
  renderRound() {
    if (!this.currentRound) return;

    // Render phrase with styled blank
    if (this.phraseEl) {
      const phrase = this.currentRound.phrase.replace(
        '___',
        '<span class="blank">___</span>'
      );
      this.phraseEl.innerHTML = phrase;
    }

    // Render word choice buttons
    if (this.choicesEl) {
      this.choicesEl.innerHTML = '';
      this.currentRound.choices.forEach((word) => {
        const btn = document.createElement('button');
        btn.className = 'word-btn';
        btn.textContent = word;
        this.choicesEl.appendChild(btn);
      });
    }
  }

  /**
   * Submit an answer
   * @param {string} choice - The selected word
   */
  async submitAnswer(choice) {
    console.log(`[TEA-GAME] Submitting answer: ${choice}`);
    this.lastAction = 'submitAnswer';
    this.lastChoice = choice;

    // Calculate response time
    const timeMs = Math.round(performance.now() - (this.roundStartTime || 0));

    // Disable buttons while processing
    this.disableChoices(true);

    try {
      // Call WASM to submit answer
      const resultJson = this.wasm.game_submit_answer(choice, timeMs);
      const result = JSON.parse(resultJson);

      if (result.success) {
        const answerResult = result.data;

        // Show visual feedback (AC-4)
        this.showAnswerFeedback(choice, answerResult.is_correct, answerResult.correct_word);

        // Update stats
        this.updateStats();

        // Wait for feedback animation, then continue to next round
        await this.delay(1500);
        await this.nextRound();
      } else {
        throw new Error(result.error || 'Failed to submit answer');
      }
    } catch (error) {
      console.error('[TEA-GAME] Submit answer error:', error);
      this.showError(error.message);
      this.disableChoices(false);
    }
  }

  /**
   * Show visual feedback for answer (AC-4)
   * @param {string} choice - The selected word
   * @param {boolean} isCorrect - Whether the answer was correct
   * @param {string} correctWord - The correct answer
   */
  showAnswerFeedback(choice, isCorrect, correctWord) {
    const buttons = this.choicesEl?.querySelectorAll('.word-btn');
    if (!buttons) return;

    buttons.forEach((btn) => {
      const word = btn.textContent;

      if (word === choice) {
        // User's choice
        btn.classList.add(isCorrect ? 'correct' : 'incorrect');
      }

      if (!isCorrect && word === correctWord) {
        // Reveal correct answer (AC-4: reveal correct answer on wrong guess)
        btn.classList.add('reveal-correct');
      }
    });
  }

  /**
   * Give up and submit to leaderboard (AC-6)
   */
  async giveUp() {
    console.log('[TEA-GAME] Giving up...');
    this.lastAction = 'giveUp';

    try {
      // Disable button
      if (this.giveUpBtn) {
        this.giveUpBtn.disabled = true;
        this.giveUpBtn.textContent = 'Submitting...';
      }

      // Submit to leaderboard
      // Submit to leaderboard (result logged for debugging)
      const resultJson = this.wasm.game_submit_to_leaderboard();
      const result = JSON.parse(resultJson);
      console.log('[TEA-GAME] Leaderboard submission:', result);

      // Show leaderboard regardless of submission result
      await this.showLeaderboard();
    } catch (error) {
      console.error('[TEA-GAME] Give up error:', error);
      // Still try to show leaderboard
      await this.showLeaderboard();
    } finally {
      if (this.giveUpBtn) {
        this.giveUpBtn.disabled = false;
        this.giveUpBtn.textContent = 'Give Up & Submit to Leaderboard';
      }
    }
  }

  /**
   * Show the leaderboard screen (AC-7)
   */
  async showLeaderboard() {
    console.log('[TEA-GAME] Showing leaderboard...');

    try {
      // Get leaderboard data
      const leaderboardJson = this.wasm.game_get_leaderboard(10);
      const leaderboardResult = JSON.parse(leaderboardJson);

      // Get current session stats
      const statsJson = this.wasm.game_get_session_stats();
      const statsResult = JSON.parse(statsJson);

      // Render leaderboard
      if (leaderboardResult.success) {
        this.renderLeaderboard(
          leaderboardResult.data,
          statsResult.success ? statsResult.data : null
        );
      }

      // Show leaderboard screen
      this.showScreen(GameState.LEADERBOARD);
    } catch (error) {
      console.error('[TEA-GAME] Leaderboard error:', error);
      this.showError(error.message);
    }
  }

  /**
   * Render the leaderboard table (AC-7, AC-8)
   * @param {Array} entries - Leaderboard entries
   * @param {Object} currentSession - Current user's session data
   */
  renderLeaderboard(entries, currentSession) {
    if (!this.leaderboardBody) return;

    // Clear existing rows
    this.leaderboardBody.innerHTML = '';

    // Add entries
    entries.forEach((entry) => {
      const row = document.createElement('tr');

      // Highlight current user (AC-7)
      if (currentSession && entry.username === currentSession.username) {
        row.classList.add('current-user');
      }

      row.innerHTML = `
        <td>${entry.rank}</td>
        <td>${this.escapeHtml(entry.username)}</td>
        <td>${entry.score.toFixed(2)}</td>
        <td>${(entry.accuracy * 100).toFixed(0)}%</td>
        <td>${entry.total_answers}</td>
      `;

      this.leaderboardBody.appendChild(row);
    });

    // Update summary if session exists
    if (currentSession && this.yourScoreSummary) {
      const accuracy = currentSession.total_answers > 0
        ? (currentSession.correct_answers / currentSession.total_answers * 100).toFixed(0)
        : 0;

      this.yourScoreSummary.innerHTML = `
        Your score: <strong>${currentSession.score.toFixed(2)}</strong> |
        Accuracy: <strong>${accuracy}%</strong> |
        Rounds: <strong>${currentSession.total_answers}</strong>
      `;
      // Show the summary
      this.yourScoreSummary.classList.remove('hidden');
    }
  }

  /**
   * Play again (AC-8)
   */
  async playAgain() {
    console.log('[TEA-GAME] Playing again...');

    // Reset session
    this.session = null;
    this.currentRound = null;
    this.roundNumber = 0;

    // Return to welcome and start new game
    this.showScreen(GameState.WELCOME);
    await this.startGame();
  }

  /**
   * Update stats display (score, accuracy, difficulty)
   */
  updateStats() {
    try {
      // Get current session stats
      const statsJson = this.wasm.game_get_session_stats();
      const result = JSON.parse(statsJson);

      if (result.success) {
        const stats = result.data;

        // Update round number
        if (this.roundNumEl) {
          this.roundNumEl.textContent = this.roundNumber;
        }

        // Update score
        if (this.currentScoreEl) {
          this.currentScoreEl.textContent = stats.score.toFixed(2);
        }

        // Update accuracy
        if (this.accuracyEl) {
          const accuracy = stats.total_answers > 0
            ? (stats.correct_answers / stats.total_answers * 100).toFixed(0)
            : 0;
          this.accuracyEl.textContent = `${accuracy}%`;
        }

        // Update difficulty indicator (AC-5)
        this.updateDifficulty(stats.current_difficulty);
      }
    } catch (error) {
      console.warn('[TEA-GAME] Stats update error:', error);
    }
  }

  /**
   * Update difficulty indicator (AC-5)
   * @param {number} difficulty - Difficulty value (0.1 to 0.95)
   */
  updateDifficulty(difficulty) {
    // Update progress bar fill
    if (this.difficultyFillEl) {
      const percent = Math.round(difficulty * 100);
      this.difficultyFillEl.style.width = `${percent}%`;
    }

    // Update label
    if (this.difficultyLabelEl) {
      let label = 'Medium';
      if (difficulty < 0.3) label = 'Easy';
      else if (difficulty < 0.6) label = 'Medium';
      else if (difficulty < 0.8) label = 'Hard';
      else label = 'Expert';

      this.difficultyLabelEl.textContent = label;
    }
  }

  /**
   * Show/hide loading indicator (AC-9)
   * @param {boolean} show - Whether to show loading
   */
  showLoading(show) {
    if (this.loadingEl) {
      this.loadingEl.classList.toggle('hidden', !show);

      // Reset loading text
      if (this.loadingText) {
        this.loadingText.textContent = 'Generating question...';
      }
    }

    // Clear any existing timeouts
    this.clearTimeouts();

    if (show) {
      // Set warning timeout (AC-12: show message after 10s)
      this.warningTimeoutId = setTimeout(() => {
        if (this.loadingText) {
          this.loadingText.textContent = 'Taking longer than expected...';
        }
      }, LLM_WARNING_TIMEOUT_MS);
    }
  }

  /**
   * Call a WASM function with timeout (AC-12)
   * @param {Function} fn - Function that returns a promise
   * @param {number} timeoutMs - Timeout in milliseconds
   * @param {string} operation - Description of the operation
   * @returns {Promise} - Result or timeout error
   */
  async callWithTimeout(fn, timeoutMs, operation) {
    return new Promise(async (resolve, reject) => {
      // Set timeout
      this.callTimeoutId = setTimeout(() => {
        reject(new Error(`${operation} timed out after ${timeoutMs / 1000} seconds`));
      }, timeoutMs);

      try {
        const result = await fn();
        this.clearTimeouts();
        resolve(result);
      } catch (error) {
        this.clearTimeouts();
        reject(error);
      }
    });
  }

  /**
   * Clear all timeout handles
   */
  clearTimeouts() {
    if (this.warningTimeoutId) {
      clearTimeout(this.warningTimeoutId);
      this.warningTimeoutId = null;
    }
    if (this.callTimeoutId) {
      clearTimeout(this.callTimeoutId);
      this.callTimeoutId = null;
    }
  }

  /**
   * Enable/disable word choice buttons
   * @param {boolean} disabled - Whether buttons should be disabled
   */
  disableChoices(disabled) {
    const buttons = this.choicesEl?.querySelectorAll('.word-btn');
    buttons?.forEach((btn) => {
      btn.disabled = disabled;
    });
  }

  /**
   * Show error modal (AC-11)
   * @param {string} message - Error message to display
   */
  showError(message) {
    console.error('[TEA-GAME] Error:', message);
    this.state = GameState.ERROR;

    if (this.errorModal && this.errorMessage) {
      this.errorMessage.textContent = message;
      this.errorModal.classList.remove('hidden');
    } else {
      // Fallback: alert if no error modal
      alert(`Error: ${message}`);
    }
  }

  /**
   * Hide error modal
   */
  hideError() {
    if (this.errorModal) {
      this.errorModal.classList.add('hidden');
    }
  }

  /**
   * Retry the last failed action (AC-11)
   */
  async retryLastAction() {
    this.hideError();

    switch (this.lastAction) {
      case 'startGame':
        await this.startGame();
        break;
      case 'nextRound':
        await this.nextRound();
        break;
      case 'submitAnswer':
        if (this.lastChoice) {
          await this.submitAnswer(this.lastChoice);
        }
        break;
      case 'giveUp':
        await this.giveUp();
        break;
      default:
        this.showScreen(GameState.WELCOME);
    }
  }

  /**
   * Return to welcome screen (AC-11)
   */
  returnToWelcome() {
    this.hideError();
    this.session = null;
    this.currentRound = null;
    this.roundNumber = 0;
    this.showScreen(GameState.WELCOME);
  }

  /**
   * Show a specific screen
   * @param {string} state - The GameState to show
   */
  showScreen(state) {
    this.state = state;

    // Toggle screen visibility
    this.welcomeScreen?.classList.toggle(
      'hidden',
      state !== GameState.WELCOME
    );
    this.playingScreen?.classList.toggle(
      'hidden',
      state !== GameState.PLAYING && state !== GameState.LOADING
    );
    this.leaderboardScreen?.classList.toggle(
      'hidden',
      state !== GameState.LEADERBOARD
    );

    console.log(`[TEA-GAME] Screen: ${state}`);
  }

  /**
   * Utility: delay for specified milliseconds
   * @param {number} ms - Milliseconds to wait
   * @returns {Promise}
   */
  delay(ms) {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }

  /**
   * Utility: escape HTML to prevent XSS
   * @param {string} str - String to escape
   * @returns {string} - Escaped string
   */
  escapeHtml(str) {
    const div = document.createElement('div');
    div.textContent = str;
    return div.innerHTML;
  }
}

/**
 * Initialize the game UI with WASM module
 * @param {Object} wasmModule - WASM module with game functions
 * @returns {GameUI} - Initialized GameUI instance
 */
export function initGameUI(wasmModule) {
  const gameUI = new GameUI(wasmModule);
  window.gameUI = gameUI;
  window.dispatchEvent(new Event('game-ready'));
  return gameUI;
}

export default GameUI;
