# Story TEA-AGENT-001.1-rust: Multi-Agent Collaboration (Rust/Embedded)

## Status

**Ready for Development**

## Story

**As a** developer deploying autonomous agents to edge computing environments,
**I want** built-in multi-agent collaboration primitives in the Rust runtime,
**so that** I can orchestrate multiple specialized agents within a single static binary without Python dependencies.

## Background

This is the Rust adaptation of TEA-AGENT-001.1, optimized for embedded/offline execution. Key differences from Python version:

| Aspect | Python Version | Rust Version |
|--------|---------------|--------------|
| **Parallelism** | ThreadPoolExecutor | rayon thread pool |
| **LLM Provider** | Any (OpenAI, Anthropic, etc.) | Ollama, OpenAI-compatible only |
| **Tool Bridges** | MCP, CrewAI, LangChain | Rust-native actions only |
| **Custom Logic** | Python inline | Lua 5.4 inline |
| **CrewAI Delegation** | `agent.crewai_delegate` | NOT SUPPORTED |

## Scope

### In Scope
- `agent.dispatch` - Single agent execution via Ollama/OpenAI-compatible
- `agent.parallel` - Parallel agent execution via rayon
- `agent.sequential` - Sequential agent chaining
- Agent registry from YAML settings
- Aggregation strategies: `collect`, `vote`, `consensus`, `first`

### Out of Scope (Python-only)
- `agent.coordinate` - Complex state management deferred
- `agent.crewai_delegate` - Python ecosystem dependency
- MCP/CrewAI/LangChain tool bridges

## Acceptance Criteria

### AC1: Agent Definition in YAML Settings
1. Agents defined in `settings.agents` with name, model, system_prompt, temperature
2. Agent definitions validated at YAML parse time (serde validation)
3. Model must be Ollama model name or OpenAI-compatible endpoint
4. Default agent inherits from `settings.llm` if not specified

### AC2: `agent.dispatch` Action (Rust Native)
1. Dispatches task to single named agent via `reqwest` HTTP client
2. Supports Ollama (`http://localhost:11434`) and OpenAI-compatible endpoints
3. Task templating via Tera (`{{ state.key }}`)
4. Timeout configurable per dispatch (default: 60s)
5. Retry with exponential backoff on transient failures

### AC3: `agent.parallel` Action (Rayon-based)
1. Dispatches same task to multiple agents via `rayon::scope`
2. Each agent receives deep copy of state (serde clone)
3. Aggregation strategies:
   - `collect`: Returns `Vec<Response>` of all agent responses
   - `vote`: Returns majority response (deterministic tie-breaking by agent order)
   - `consensus`: Retries until N agents agree (configurable threshold)
   - `first`: Returns first successful response, cancels others
4. Configurable `max_concurrent` (maps to rayon thread count)
5. Per-agent timeout enforcement

### AC4: `agent.sequential` Action
1. Chains agents where output feeds next agent's input
2. State threading via Lua table merge
3. Early exit on agent failure (configurable: `continue` or `abort`)
4. Returns accumulated state from all agents

### AC5: Ollama Integration
1. Native support for Ollama REST API (`/api/generate`, `/api/chat`)
2. Model specification: `ollama:llama3.2`, `ollama:mistral`, etc.
3. Streaming response support for `agent.dispatch`
4. Connection pooling via `reqwest::Client`

### AC6: OpenAI-Compatible Endpoints
1. Support for any OpenAI-compatible API (vLLM, LocalAI, etc.)
2. Configuration: `api_base`, `api_key` (from env or settings)
3. Model specification: `openai:gpt-4`, `openai:custom-model`

### AC7: Error Handling (Embedded-Safe)
1. Agent not found: `AgentError::NotFound` with agent name
2. LLM timeout: `AgentError::Timeout` with duration
3. LLM connection failure: `AgentError::ConnectionFailed` with retry info
4. All errors are recoverable (no panics)
5. Checkpoint saved on unrecoverable error (unattended mode)

### AC8: Feature Flag
1. Actions behind `--features agent` cargo flag
2. No additional binary size when disabled
3. Graceful error when actions used without feature

## Tasks / Subtasks

- [ ] **Task 1: Agent Registry (Rust)** (AC: 1)
  - [ ] Define `AgentConfig` struct with serde derive
  - [ ] Implement `AgentRegistry` with `HashMap<String, AgentConfig>`
  - [ ] Parse `settings.agents` in YAML loader
  - [ ] Validation: model format, required fields
  - [ ] Unit tests for registry

- [ ] **Task 2: `agent.dispatch` Action** (AC: 2, 5, 6)
  - [ ] Implement `dispatch_action` function
  - [ ] Ollama provider implementation
  - [ ] OpenAI-compatible provider implementation
  - [ ] Tera template rendering for task
  - [ ] Timeout and retry logic
  - [ ] Unit tests with mock HTTP

- [ ] **Task 3: `agent.parallel` Action** (AC: 3)
  - [ ] Implement `parallel_action` with rayon::scope
  - [ ] Deep state cloning via serde
  - [ ] Implement `collect` aggregation
  - [ ] Implement `vote` aggregation with deterministic tie-break
  - [ ] Implement `consensus` aggregation with retry
  - [ ] Implement `first` aggregation with cancellation
  - [ ] Unit and integration tests

- [ ] **Task 4: `agent.sequential` Action** (AC: 4)
  - [ ] Implement `sequential_action`
  - [ ] State threading between agents
  - [ ] Early exit configuration
  - [ ] Unit tests

- [ ] **Task 5: Error Handling** (AC: 7)
  - [ ] Define `AgentError` enum
  - [ ] Implement recoverable error patterns
  - [ ] Checkpoint on unrecoverable error
  - [ ] Unit tests for error paths

- [ ] **Task 6: Feature Flag & Integration** (AC: 8)
  - [ ] Add `agent` feature to Cargo.toml
  - [ ] Conditional compilation for agent actions
  - [ ] Register actions in registry
  - [ ] Integration tests

## Dev Notes

### Source Tree

```
rust/src/
├── engine/
│   ├── mod.rs
│   ├── actions/
│   │   ├── mod.rs
│   │   └── agent.rs          # NEW: Multi-agent actions
│   └── agent_registry.rs     # NEW: Agent configuration
├── providers/
│   ├── mod.rs
│   ├── ollama.rs             # Ollama REST client
│   └── openai_compat.rs      # OpenAI-compatible client
```

### YAML Syntax

```yaml
settings:
  llm:
    provider: ollama
    model: llama3.2

  agents:
    researcher:
      model: ollama:llama3.2
      system_prompt: "You are a research specialist..."
      temperature: 0.7
      timeout: 120

    summarizer:
      model: ollama:mistral
      system_prompt: "You summarize research findings..."
      temperature: 0.3

nodes:
  - name: parallel_research
    action: agent.parallel
    with:
      agents: [researcher, summarizer]
      task: "{{ state.research_question }}"
      aggregation: collect
      max_concurrent: 2

  - name: final_summary
    action: agent.dispatch
    with:
      agent: summarizer
      task: "Summarize: {{ state.parallel_results | json }}"
```

### Aggregation Strategy Details

| Strategy | Rust Implementation | Thread Safety |
|----------|---------------------|---------------|
| `collect` | `rayon::join` collecting results | Arc<Mutex<Vec>> |
| `vote` | Count responses, sort by frequency | Atomic counters |
| `consensus` | Retry loop until threshold | Shared state via Arc |
| `first` | `rayon::scope` with early return | AtomicBool for cancellation |

### Dependencies

```toml
[dependencies]
rayon = "1.8"
reqwest = { version = "0.11", features = ["json", "rustls-tls"] }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
tokio = { version = "1", features = ["rt-multi-thread"] }

[features]
agent = []
```

### Related Stories
- TEA-RUST-001: Rust Migration Epic
- TEA-RUST-014: Library API
- TEA-AGENT-001.1: Python version (reference)

## Testing

### Test File Location
- `rust/tests/test_agent_actions.rs`

### Test Categories

| Category | Count | Priority |
|----------|-------|----------|
| Agent Registry | 6 | P0 |
| agent.dispatch | 8 | P0 |
| agent.parallel | 10 | P0 |
| agent.sequential | 4 | P1 |
| Error Handling | 6 | P0 |
| Ollama Integration | 4 | P1 |

### Key Test Scenarios

1. **Registry validation** - Invalid agent config rejected at parse time
2. **Parallel state isolation** - Agents don't share mutable state
3. **Vote tie-breaking** - Deterministic result on tie
4. **Timeout enforcement** - Slow agent times out correctly
5. **Ollama connectivity** - Graceful handling of Ollama down
6. **Checkpoint on failure** - Unattended mode saves state

## QA Notes

**Assessment Date:** 2026-01-05
**Test Architect:** Quinn (QA)
**Test Design Document:** `docs/qa/assessments/TEA-AGENT-001.1-rust-test-design-20260105.md`

### Test Coverage Summary

| Metric | Count |
|--------|-------|
| **Total Test Scenarios** | 48 |
| **Unit Tests** | 26 (54%) |
| **Integration Tests** | 16 (33%) |
| **E2E Tests** | 6 (13%) |

**Priority Distribution:**
- P0 (Critical): 24 tests
- P1 (High): 16 tests
- P2 (Medium): 6 tests
- P3 (Low): 2 tests

**All 8 Acceptance Criteria have full test coverage.**

### Risk Areas Identified

| Risk | Severity | Mitigating Tests |
|------|----------|------------------|
| **State corruption in parallel execution** | HIGH | UNIT-009, INT-009 - Deep clone verification critical for thread safety |
| **Non-deterministic voting results** | MEDIUM | UNIT-012 - Deterministic tie-breaking must be verified |
| **Unbounded resource consumption** | HIGH | INT-010, INT-011 - max_concurrent and timeout enforcement |
| **Panic in embedded environment** | CRITICAL | UNIT-024 - All error paths must be recoverable (no panics) |
| **Data loss on failure** | HIGH | INT-018 - Checkpoint on unrecoverable error |
| **Ollama connection flakiness** | MEDIUM | INT-006, E2E-006 - Retry and graceful degradation |

### Recommended Test Scenarios (Priority Order)

**Phase 1 - P0 Critical Path (Must Pass Before Development Complete):**
1. AgentConfig serde validation (UNIT-001 to 005)
2. Tera template rendering with state (UNIT-006, 007)
3. Aggregation algorithms - collect, vote, consensus, first (UNIT-009 to 014)
4. AgentError types are recoverable, no panics (UNIT-021 to 024)
5. YAML parsing of settings.agents (INT-001, INT-003)
6. HTTP dispatch with timeout enforcement (INT-004, 005, 007)
7. Parallel state isolation verification (INT-008 to 011)
8. Checkpoint saved on unrecoverable error (INT-018)
9. agent.parallel full E2E workflow (E2E-002)

**Phase 2 - P1 Important Scenarios:**
1. Default agent inheritance from settings.llm
2. Retry with exponential backoff
3. Sequential chaining with state threading
4. Ollama streaming response handling
5. Consensus retry until threshold

**Phase 3 - P2/P3 As Time Permits:**
1. Feature flag compilation verification
2. Real Ollama server E2E tests
3. Binary size validation when feature disabled

### Concerns / Blockers

| Type | Description | Recommendation |
|------|-------------|----------------|
| **CONCERN** | Thread safety in `first` aggregation with early cancellation | Implement AtomicBool cancellation pattern carefully; add stress tests |
| **CONCERN** | Consensus retry could loop indefinitely | Implement max_retries configuration with sensible default (5) |
| **BLOCKER (potential)** | E2E tests require local Ollama instance | Document required test environment; consider CI with Ollama container |
| **CONCERN** | Tera template injection risk | Validate template expressions at parse time; document security model |

### Test Environment Requirements

| Level | Environment | External Dependencies |
|-------|-------------|----------------------|
| Unit | `cargo test` | None - mocked HTTP |
| Integration | `cargo test --features agent` | wiremock-rs for mock HTTP |
| E2E | Real Ollama | Local Ollama instance required |

### QA Recommendation

**GATE STATUS: READY FOR DEVELOPMENT**

Story is well-defined with clear acceptance criteria. Test design provides comprehensive coverage of all ACs with appropriate risk mitigations. Key thread-safety and embedded-safety scenarios are addressed.

**Pre-merge Checklist:**
- [ ] All P0 tests passing
- [ ] No panics in any error path (embedded-safe requirement)
- [ ] Parallel state isolation verified with race condition tests
- [ ] Checkpoint persistence verified on failure scenarios

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-05 | 0.1 | Initial Rust adaptation from TEA-AGENT-001.1 | Sarah (PO) |
| 2026-01-05 | 0.2 | Added QA Notes from test design review | Quinn (QA) |
