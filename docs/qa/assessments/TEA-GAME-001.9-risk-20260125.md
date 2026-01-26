# Risk Profile: Story TEA-GAME-001.9

Date: 2026-01-25
Reviewer: Quinn (Test Architect)

## Executive Summary

- Total Risks Identified: 7
- Critical Risks: 0
- High Risks: 2
- Medium Risks: 3
- Low Risks: 2
- Risk Score: 78/100 (calculated: 100 - 10*2 - 2*0)

## Risk Matrix

| Risk ID   | Description                                   | Probability | Impact    | Score | Priority |
|-----------|-----------------------------------------------|-------------|-----------|-------|----------|
| TECH-001  | WASM bundle size increase from embedded JSON  | Medium (2)  | High (3)  | 6     | High     |
| TECH-002  | Game round JSON format incompatibility        | Medium (2)  | High (3)  | 6     | High     |
| PERF-001  | DuckDB phrase query performance at scale      | Low (1)     | Medium (2)| 2     | Low      |
| DATA-001  | Phrase exhaustion within single session       | Medium (2)  | Medium (2)| 4     | Medium   |
| TECH-003  | DuckDB schema migration on existing users     | Medium (2)  | Medium (2)| 4     | Medium   |
| OPS-001   | Phrase database not synced with WASM build    | Low (1)     | Medium (2)| 2     | Low      |
| TECH-004  | Thread-local state for used_phrase_ids        | Medium (2)  | Medium (2)| 4     | Medium   |

## High Risks Requiring Attention

### 1. TECH-001: WASM Bundle Size Increase

**Score: 6 (High)**

**Probability**: Medium - Embedding 1039 phrases (~300-500KB JSON) via `include_str!` will definitely increase bundle size.

**Impact**: High - Degraded load times on slow connections; browser LLM inference is already slow (90s timeout in game.js).

**Mitigation**:
- Compress JSON before embedding (gzip/deflate)
- Lazy load phrases on first game start
- Measure and set target: <500KB increase

**Testing Focus**:
- Measure WASM bundle size before/after
- Test load times on 3G throttled connection
- Verify first game round latency acceptable

### 2. TECH-002: Game Round JSON Format Incompatibility

**Score: 6 (High)**

**Probability**: Medium - Story adds `phrase_id` and modifies round generation, but claims backward compatibility.

**Impact**: High - If `game.js` receives unexpected format, `renderRound()` fails and game is broken.

**Mitigation**:
- Keep `GameRoundInfo` struct unchanged (id, phrase, choices only)
- Add new fields as internal-only, not exposed to JS
- Integration test with existing `game.js` before merge

**Testing Focus**:
- E2E test: full game flow in browser after changes
- Verify `this.currentRound.phrase` and `this.currentRound.choices` work
- Test `submitAnswer()` flow end-to-end

## Risk Distribution

### By Category

- Technical (TECH): 4 risks (2 high, 2 medium)
- Performance (PERF): 1 risk (0 high)
- Data (DATA): 1 risk (0 high)
- Operational (OPS): 1 risk (0 high)

### By Component

- Rust game engine (`game.rs`): 4 risks
- WASM build/bundle: 2 risks
- Database (`db.rs`): 1 risk

## Detailed Risk Register

### TECH-003: DuckDB Schema Migration (Score: 4 - Medium)

**Description**: Adding new `phrases` table requires schema initialization on existing installations.

**Mitigation**:
- Use `CREATE TABLE IF NOT EXISTS` (already specified in story)
- Initialize phrases on every `game_init()` call
- No breaking changes (additive only)

**Testing**: Test with both fresh and existing DuckDB instances.

### DATA-001: Phrase Exhaustion (Score: 4 - Medium)

**Description**: With 1039 phrases and difficulty filtering, long sessions may exhaust available phrases at a difficulty level.

**Mitigation**:
- Widen difficulty range when no phrases available
- Story mentions `exclude_ids` but needs edge case handling
- Consider phrase reuse with warning after exhaustion

**Testing**: Test with artificially small phrase set to verify edge case behavior.

### TECH-004: Thread-Local State Growth (Score: 4 - Medium)

**Description**: `used_phrase_ids: Vec<String>` grows with each round, increasing memory per session.

**Mitigation**:
- Cap to last N entries (e.g., 100)
- Clear on session restart (already happens via `start_session()`)

**Testing**: Play 50+ rounds, monitor memory in DevTools.

### PERF-001: DuckDB Query Performance (Score: 2 - Low)

**Description**: Random phrase selection with difficulty filter and exclusion list could be slow.

**Mitigation**:
- Index on difficulty column (specified in story)
- Benchmark with full dataset

**Testing**: Measure `get_random_phrase()` latency with various exclusion sizes.

### OPS-001: Phrase Database Sync (Score: 2 - Low)

**Description**: Phrases are compile-time embedded; changes require WASM rebuild.

**Mitigation**:
- Document rebuild requirement
- Version number in JSON (already present: "1.0")

**Testing**: Verify embedded JSON matches source after rebuild.

## Risk-Based Testing Strategy

### Priority 1: High Risk Tests (Score 6)

1. **Bundle Size Verification**
   - Measure `tea_wasm_llm_bg.wasm` size before and after
   - Acceptance: <500KB increase
   - Test load time on throttled network

2. **UI Compatibility Integration Test**
   - Full E2E game flow in browser
   - Verify `game.js` renderRound() works unchanged
   - Test all game states: welcome → playing → leaderboard

### Priority 2: Medium Risk Tests (Score 4)

3. **Schema Migration Test**
   - Fresh DB initialization
   - Existing DB with sessions table only
   - Verify `phrases` table created correctly

4. **Phrase Exhaustion Test**
   - Create test with 5-10 phrases only
   - Play until exhausted
   - Verify graceful handling

5. **Memory Growth Test**
   - 50-round session
   - Monitor `used_phrase_ids` growth
   - Verify cleanup on new session

### Priority 3: Low Risk Tests (Score 2)

6. **Query Performance Benchmark**
   - `get_random_phrase()` with 0, 100, 500 exclusions
   - Target: <10ms per query

7. **Build Sync Verification**
   - Modify JSON, rebuild, verify embedded

## Risk Acceptance Criteria

### Must Fix Before Production

- All high risks (score 6) must have verified mitigations
- TECH-001: Bundle size verified acceptable (<500KB increase)
- TECH-002: E2E test passing with existing game.js

### Can Deploy with Mitigation

- Medium risks (score 4) with documented handling
- Edge cases (exhaustion, memory) have graceful degradation

### Accepted Risks

- Low risks (score 2) accepted with monitoring
- OPS-001: Rebuild requirement documented in story

## Monitoring Requirements

Post-deployment monitoring:
- WASM load time metrics (P50, P95)
- Game round generation latency
- Error rates for phrase selection failures
- Session length distribution (for exhaustion analysis)

## Risk Review Triggers

Review and update risk profile when:
- Phrase database size changes significantly (>2x)
- New difficulty bands added
- Browser LLM inference time changes
- User reports of performance issues

---

## Gate YAML Block

```yaml
# risk_summary (paste into gate file):
risk_summary:
  totals:
    critical: 0
    high: 2
    medium: 3
    low: 2
  highest:
    id: TECH-001
    score: 6
    title: 'WASM bundle size increase from embedded JSON'
  recommendations:
    must_fix:
      - 'Verify WASM bundle size increase <500KB'
      - 'E2E integration test proving game.js compatibility'
    monitor:
      - 'Track WASM load times post-deployment'
      - 'Monitor phrase exhaustion in long sessions'
```

---

Risk profile: docs/qa/assessments/TEA-GAME-001.9-risk-20260125.md
