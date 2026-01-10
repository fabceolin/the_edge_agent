# Story TEA-GAME-001.7: Browser UI Implementation

## Status

Needs Revision

**Notes:** Story has identified blockers from QA assessment:
- BLOCKER: Dependency on Story 6 (TEA-GAME-001.6) WASM exports - cannot execute integration/E2E tests until WASM functions are available
- CONCERN: Story lacks AC for WASM call failures - recommend adding error states for robustness
- CONCERN: LLM call timeouts need specification (10+ second timeouts for `game_generate_round()`)

Story should be revisited after Story 6 is complete and error handling ACs are added.

## Story

**As a** user,
**I want** a game tab in the WASM demo with interactive UI,
**So that** I can play "Know Your Model" in my browser.

## Story Context

**Existing System Integration:**

- Integrates with: WASM demo (`docs/extra/wasm-demo/`)
- Technology: HTML, CSS, JavaScript (ES modules), CodeMirror
- Follows pattern: Existing demo styling in `style.css`
- Touch points: `index.html`, new `game.js`, `style.css`

**Dependencies:**

- Story 6 (TEA-GAME-001.6): WASM exports for game functions

## Acceptance Criteria

1. **AC-1**: New "Game" tab in `index.html` alongside YAML workflow tab
2. **AC-2**: Welcome screen with random username display and "Start Game" button
3. **AC-3**: Game screen shows phrase with blank, 5 word buttons, current stats
4. **AC-4**: Visual feedback on correct/incorrect answer (green/red flash)
5. **AC-5**: Difficulty indicator (progress bar or gauge)
6. **AC-6**: "Give Up & Submit" button always visible during game
7. **AC-7**: Leaderboard screen shows top 10 with user's position highlighted
8. **AC-8**: "Play Again" button from leaderboard screen
9. **AC-9**: Loading states during LLM calls
10. **AC-10**: Responsive design matching existing demo style

## Tasks / Subtasks

- [ ] Add tab navigation (AC-1)
  - [ ] Add tab bar to `index.html` header
  - [ ] Create "Workflow" and "Game" tabs
  - [ ] Implement tab switching JavaScript
  - [ ] Style tabs to match existing design

- [ ] Create welcome screen (AC-2)
  - [ ] Add welcome container HTML
  - [ ] Display generated username prominently
  - [ ] Add "Start Game" button
  - [ ] Add brief game instructions

- [ ] Create game screen (AC-3, AC-5, AC-6)
  - [ ] Add game container HTML (hidden by default)
  - [ ] Phrase display with styled blank
  - [ ] 5 word choice buttons (flexbox/grid layout)
  - [ ] Stats bar (Round #, Score, Accuracy)
  - [ ] Difficulty indicator (progress bar with label)
  - [ ] "Give Up & Submit" button

- [ ] Implement answer feedback (AC-4)
  - [ ] Green flash/border for correct answer
  - [ ] Red flash/border for incorrect answer
  - [ ] Brief delay before next round (1-2 seconds)
  - [ ] Reveal correct answer on wrong guess

- [ ] Create leaderboard screen (AC-7, AC-8)
  - [ ] Add leaderboard container HTML
  - [ ] Table with rank, username, score, stats
  - [ ] Highlight current user's row
  - [ ] "Play Again" button
  - [ ] "Back to Welcome" button

- [ ] Implement loading states (AC-9)
  - [ ] Spinner during LLM phrase generation
  - [ ] Disable buttons while loading
  - [ ] "Generating question..." text

- [ ] Create `game.js` module
  - [ ] State machine (welcome, playing, leaderboard)
  - [ ] Wire WASM function calls
  - [ ] Handle all button events
  - [ ] Manage screen transitions

- [ ] Style for responsive design (AC-10)
  - [ ] Mobile-friendly button sizes
  - [ ] Stack layout on narrow screens
  - [ ] Match existing demo dark theme
  - [ ] Test on mobile viewport

## Dev Notes

### HTML Structure

```html
<!-- Tab Bar -->
<nav class="tab-bar">
  <button class="tab active" data-tab="workflow">Workflow</button>
  <button class="tab" data-tab="game">Game</button>
</nav>

<!-- Workflow Tab Content (existing) -->
<div id="workflow-tab" class="tab-content active">
  <!-- existing YAML editor content -->
</div>

<!-- Game Tab Content -->
<div id="game-tab" class="tab-content hidden">
  <!-- Welcome Screen -->
  <div id="game-welcome" class="game-screen">
    <h2>Know Your Model</h2>
    <p class="game-instructions">
      Can you guess which word the AI chose?<br>
      You'll see a phrase with a blank and 5 word choices.<br>
      One word is from the AI - find it!
    </p>
    <div class="username-display">
      Playing as: <span id="username">SwiftFox42</span>
    </div>
    <button id="start-game-btn" class="primary-btn">Start Game</button>
  </div>

  <!-- Game Screen -->
  <div id="game-playing" class="game-screen hidden">
    <div class="game-stats-bar">
      <span>Round: <strong id="round-num">1</strong></span>
      <span>Score: <strong id="current-score">0.00</strong></span>
      <span>Accuracy: <strong id="accuracy">0%</strong></span>
    </div>

    <div class="difficulty-bar">
      <label>Difficulty:</label>
      <div class="progress-bar">
        <div id="difficulty-fill" class="progress-fill" style="width: 50%"></div>
      </div>
      <span id="difficulty-label">Medium</span>
    </div>

    <div class="phrase-container">
      <p id="phrase-text">The <span class="blank">___</span> shines brightly.</p>
    </div>

    <div id="word-choices" class="word-choices">
      <button class="word-btn">sun</button>
      <button class="word-btn">moon</button>
      <button class="word-btn">star</button>
      <button class="word-btn">light</button>
      <button class="word-btn">lamp</button>
    </div>

    <div id="loading-indicator" class="hidden">
      <div class="spinner"></div>
      <span>Generating question...</span>
    </div>

    <button id="give-up-btn" class="secondary-btn">Give Up & Submit to Leaderboard</button>
  </div>

  <!-- Leaderboard Screen -->
  <div id="game-leaderboard" class="game-screen hidden">
    <h2>Leaderboard</h2>
    <table class="leaderboard-table">
      <thead>
        <tr>
          <th>#</th>
          <th>Player</th>
          <th>Score</th>
          <th>Accuracy</th>
          <th>Rounds</th>
        </tr>
      </thead>
      <tbody id="leaderboard-body">
        <!-- Populated dynamically -->
      </tbody>
    </table>
    <div class="leaderboard-actions">
      <button id="play-again-btn" class="primary-btn">Play Again</button>
    </div>
  </div>
</div>
```

### CSS Additions

```css
/* Tab Bar */
.tab-bar {
  display: flex;
  gap: 0;
  margin-bottom: 1rem;
  border-bottom: 2px solid #3a3a3a;
}

.tab {
  padding: 0.75rem 1.5rem;
  background: transparent;
  border: none;
  color: #888;
  cursor: pointer;
  font-size: 1rem;
  border-bottom: 2px solid transparent;
  margin-bottom: -2px;
}

.tab:hover {
  color: #ccc;
}

.tab.active {
  color: #4ade80;
  border-bottom-color: #4ade80;
}

/* Game Screens */
.game-screen {
  text-align: center;
  padding: 2rem;
}

.game-screen.hidden {
  display: none;
}

.username-display {
  font-size: 1.25rem;
  margin: 2rem 0;
  color: #4ade80;
}

/* Word Choice Buttons */
.word-choices {
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  gap: 1rem;
  margin: 2rem 0;
}

.word-btn {
  padding: 1rem 2rem;
  font-size: 1.1rem;
  background: #2a2a2a;
  border: 2px solid #4a4a4a;
  border-radius: 8px;
  color: #fff;
  cursor: pointer;
  transition: all 0.2s;
  min-width: 120px;
}

.word-btn:hover {
  background: #3a3a3a;
  border-color: #5a5a5a;
}

.word-btn.correct {
  background: #166534;
  border-color: #22c55e;
  animation: flash-correct 0.5s;
}

.word-btn.incorrect {
  background: #7f1d1d;
  border-color: #ef4444;
  animation: flash-incorrect 0.5s;
}

@keyframes flash-correct {
  0%, 100% { transform: scale(1); }
  50% { transform: scale(1.05); }
}

@keyframes flash-incorrect {
  0%, 50%, 100% { transform: translateX(0); }
  25% { transform: translateX(-5px); }
  75% { transform: translateX(5px); }
}

/* Phrase Display */
.phrase-container {
  font-size: 1.5rem;
  margin: 2rem 0;
  padding: 1.5rem;
  background: #1a1a1a;
  border-radius: 8px;
}

.blank {
  display: inline-block;
  min-width: 80px;
  border-bottom: 3px solid #4ade80;
  margin: 0 0.25rem;
}

/* Difficulty Bar */
.difficulty-bar {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 1rem;
  margin: 1rem 0;
}

.progress-bar {
  width: 150px;
  height: 8px;
  background: #2a2a2a;
  border-radius: 4px;
  overflow: hidden;
}

.progress-fill {
  height: 100%;
  background: linear-gradient(90deg, #22c55e, #eab308, #ef4444);
  transition: width 0.3s;
}

/* Leaderboard */
.leaderboard-table {
  width: 100%;
  max-width: 600px;
  margin: 1rem auto;
  border-collapse: collapse;
}

.leaderboard-table th,
.leaderboard-table td {
  padding: 0.75rem;
  text-align: left;
  border-bottom: 1px solid #3a3a3a;
}

.leaderboard-table tr.current-user {
  background: #1e3a1e;
}

/* Loading */
.spinner {
  width: 24px;
  height: 24px;
  border: 3px solid #3a3a3a;
  border-top-color: #4ade80;
  border-radius: 50%;
  animation: spin 1s linear infinite;
  display: inline-block;
  margin-right: 0.5rem;
}

@keyframes spin {
  to { transform: rotate(360deg); }
}

/* Responsive */
@media (max-width: 600px) {
  .word-choices {
    flex-direction: column;
  }

  .word-btn {
    width: 100%;
  }

  .phrase-container {
    font-size: 1.2rem;
  }
}
```

### JavaScript State Machine

```javascript
// game.js

const GameState = {
  WELCOME: 'welcome',
  PLAYING: 'playing',
  LOADING: 'loading',
  LEADERBOARD: 'leaderboard',
};

class GameUI {
  constructor() {
    this.state = GameState.WELCOME;
    this.session = null;
    this.currentRound = null;
    this.roundNumber = 0;

    this.initElements();
    this.bindEvents();
  }

  initElements() {
    this.welcomeScreen = document.getElementById('game-welcome');
    this.playingScreen = document.getElementById('game-playing');
    this.leaderboardScreen = document.getElementById('game-leaderboard');
    this.usernameEl = document.getElementById('username');
    this.phraseEl = document.getElementById('phrase-text');
    this.choicesEl = document.getElementById('word-choices');
    this.loadingEl = document.getElementById('loading-indicator');
    // ... more elements
  }

  bindEvents() {
    document.getElementById('start-game-btn').addEventListener('click', () => this.startGame());
    document.getElementById('give-up-btn').addEventListener('click', () => this.giveUp());
    document.getElementById('play-again-btn').addEventListener('click', () => this.playAgain());

    this.choicesEl.addEventListener('click', (e) => {
      if (e.target.classList.contains('word-btn') && this.state === GameState.PLAYING) {
        this.submitAnswer(e.target.textContent);
      }
    });
  }

  async startGame() {
    const result = await game_start_session();
    if (result.success) {
      this.session = result.data;
      this.usernameEl.textContent = this.session.username;
      this.roundNumber = 0;
      this.showScreen(GameState.PLAYING);
      await this.nextRound();
    }
  }

  async nextRound() {
    this.setState(GameState.LOADING);
    this.loadingEl.classList.remove('hidden');
    this.disableChoices(true);

    const result = await game_generate_round();

    this.loadingEl.classList.add('hidden');

    if (result.success) {
      this.currentRound = result.data;
      this.roundNumber++;
      this.renderRound();
      this.setState(GameState.PLAYING);
      this.disableChoices(false);
    } else {
      console.error('Round generation failed:', result.error);
    }
  }

  renderRound() {
    // Render phrase with blank
    const phrase = this.currentRound.phrase.replace('___', '<span class="blank">___</span>');
    this.phraseEl.innerHTML = phrase;

    // Render choices
    this.choicesEl.innerHTML = '';
    this.currentRound.choices.forEach(word => {
      const btn = document.createElement('button');
      btn.className = 'word-btn';
      btn.textContent = word;
      this.choicesEl.appendChild(btn);
    });

    this.updateStats();
  }

  async submitAnswer(choice) {
    const startTime = performance.now();
    // ... calculate time, call WASM, show feedback
  }

  showScreen(state) {
    this.welcomeScreen.classList.toggle('hidden', state !== GameState.WELCOME);
    this.playingScreen.classList.toggle('hidden', state !== GameState.PLAYING && state !== GameState.LOADING);
    this.leaderboardScreen.classList.toggle('hidden', state !== GameState.LEADERBOARD);
    this.state = state;
  }
}

// Initialize when WASM is ready
document.addEventListener('DOMContentLoaded', () => {
  if (window.gameReady) {
    window.gameUI = new GameUI();
  } else {
    window.addEventListener('game-ready', () => {
      window.gameUI = new GameUI();
    });
  }
});
```

### Relevant Source Tree

```
docs/extra/wasm-demo/
├── index.html       # MODIFIED: Add tabs, game screens
├── style.css        # MODIFIED: Add game styles
├── app.js           # MODIFIED: Tab switching, init game
├── game.js          # NEW: Game UI state machine
└── pkg/
    └── ...          # WASM package
```

## Definition of Done

- [ ] Tab navigation works correctly
- [ ] All three screens (welcome, playing, leaderboard) render properly
- [ ] Word buttons respond to clicks
- [ ] Correct/incorrect feedback displays
- [ ] Difficulty bar updates with game progress
- [ ] Leaderboard displays and highlights user
- [ ] Loading states show during LLM calls
- [ ] Responsive on mobile devices
- [ ] Matches existing demo visual style

---

## QA Notes

**Test Design Assessment:** 2026-01-10 | Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 38 |
| Unit Tests | 12 (32%) |
| Integration Tests | 14 (37%) |
| E2E Tests | 12 (31%) |

**Priority Distribution:** P0: 8 (21%) | P1: 16 (42%) | P2: 10 (26%) | P3: 4 (11%)

All 10 Acceptance Criteria have complete test coverage with appropriate test levels.

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **WASM bridge failure** | HIGH | P0 integration tests for all 4 WASM functions: `game_start_session`, `game_generate_round`, `game_submit_answer`, `game_get_leaderboard` |
| **State corruption during screen transitions** | MEDIUM | Integration tests for state reset and transition logic |
| **LLM timeout/failure** | MEDIUM | Loading state tests (P0) ensure user feedback during async operations |
| **User unable to complete game** | HIGH | "Give Up" flow is P0 - it's the ONLY way to save progress and end a game |
| **Mobile usability issues** | LOW | P2 responsive design tests for 375px viewport |

### Recommended Test Scenarios (Critical Path)

**Phase 1 - P0 (Must Pass for Release):**
1. `TEA-GAME-001.7-INT-002` - Start game WASM call succeeds
2. `TEA-GAME-001.7-INT-004` - Round generation returns valid phrase and 5 choices
3. `TEA-GAME-001.7-INT-009` - Give Up button triggers score submission
4. `TEA-GAME-001.7-INT-014` - Loading indicator displays during LLM calls
5. `TEA-GAME-001.7-E2E-003` - Game screen renders phrase, choices, and stats
6. `TEA-GAME-001.7-E2E-007` - User can give up and see leaderboard

**Phase 2 - P1 (Core Functionality):**
- Tab navigation and screen transitions
- Answer feedback (correct/incorrect visual states)
- Leaderboard display with user highlighting
- Play Again replayability flow

### Concerns and Blockers

| Item | Status | Notes |
|------|--------|-------|
| **Dependency on Story 6 (WASM exports)** | BLOCKER | Cannot execute integration/E2E tests until WASM functions are available |
| **LLM call timeouts** | CONCERN | Integration tests need 10+ second timeouts for `game_generate_round()` |
| **No error handling specs** | CONCERN | Story lacks AC for WASM call failures - recommend adding error states |
| **Mock data requirements** | INFO | Need mock session, round, and leaderboard data for unit/integration tests |

### Testing Technology Recommendations

| Level | Tool | Rationale |
|-------|------|-----------|
| Unit | Vitest/Jest | Fast JS testing with jsdom |
| Integration | Playwright Component | Real browser with WASM support |
| E2E | Playwright | Full stack validation in browser |

### Test Design Reference

Full test matrix: `docs/qa/assessments/TEA-GAME-001.7-test-design-20260110.md`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-10 | 0.1 | Initial story creation | Sarah (PO Agent) |
| 2026-01-10 | 0.2 | Added QA Notes section | Quinn (Test Architect) |
