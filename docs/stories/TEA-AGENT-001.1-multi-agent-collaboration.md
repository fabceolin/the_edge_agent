# Story TEA-AGENT-001.1: Multi-Agent Collaboration Primitives

## Status

**Ready for Development**

*Status updated: 2026-01-05 - All checklist criteria passed. QA test design complete with 63 test scenarios covering all 9 acceptance criteria (including AC9: CrewAI Delegation Mode).*

## Story

**As a** YAML agent developer,
**I want** built-in multi-agent collaboration primitives,
**so that** I can create sophisticated multi-agent workflows without manually wiring complex graph topologies.

## Background

The Edge Agent currently supports parallel execution via `add_parallel_edge()` but lacks higher-level abstractions for multi-agent patterns. Users must manually:

1. Define multiple LLM call nodes
2. Wire parallel edges for concurrent execution
3. Implement custom fan-in logic for aggregation
4. Handle agent-specific configuration per node

This story introduces agent-level abstractions that simplify multi-agent workflows.

## Acceptance Criteria

### AC1: Agent Definition in YAML Settings
1. Agents can be defined in `settings.agents` with name, model, system_prompt, tools, and temperature
2. Agent definitions are validated at YAML load time
3. Agents can reference existing tool bridges (MCP, CrewAI, LangChain)
4. Default agent inherits from `settings.llm` if not specified

### AC2: `agent.dispatch` Action
1. Dispatches a task to a single named agent
2. Returns agent response in state
3. Supports task templating with Jinja2
4. Respects agent's model, system_prompt, and tools configuration
5. Timeout and retry configurable per dispatch

### AC3: `agent.parallel` Action
1. Dispatches same task to multiple agents in parallel
2. Aggregation strategies: `collect`, `vote`, `consensus`, `first`
3. `collect`: Returns list of all responses
4. `vote`: Returns majority response (for classification tasks)
5. `consensus`: Retries until agreement threshold met
6. `first`: Returns first successful response
7. Configurable `max_concurrent` limit

### AC4: `agent.sequential` Action
1. Chains multiple agents where output feeds next agent's input
2. State threading: each agent receives accumulated state
3. Optional transformation between agents
4. Early exit on agent failure (configurable)

### AC5: `agent.coordinate` Action
1. Implements coordinator pattern with leader agent
2. Leader dispatches subtasks to worker agents
3. Leader aggregates and validates results
4. Re-dispatch on validation failure
5. Maximum coordination rounds configurable

### AC6: Tool Bridge Integration
1. Agents can use tools from `tools.mcp` bridge
2. Agents can use tools from `tools.crewai` bridge
3. Agents can use tools from `tools.langchain` bridge
4. Tool availability checked at agent initialization

### AC7: Python Implementation
1. New module: `python/src/the_edge_agent/actions/agent_actions.py`
2. Agent registry class for managing agent definitions
3. All actions registered in `build_actions_registry()`
4. Test coverage >90%

### AC8: Rust Implementation
1. New module: `rust/src/engine/agent_actions.rs`
2. Feature parity with Python for core actions
3. `agent.coordinate` may be design-doc only (complex state management)

### AC9: CrewAI Delegation Mode (Optional)
1. `agent.crewai_delegate` action for complex multi-agent workflows via CrewAI
2. Bridges to CrewAI's hierarchical process when `backend: crewai` specified
3. Automatic tool mapping between TEA and CrewAI tool definitions
4. Graceful fallback to native TEA multi-agent when CrewAI unavailable
5. Requires `tools-crewai` optional dependency

## Tasks / Subtasks

- [ ] **Task 1: Agent Registry Implementation** (AC: 1)
  - [ ] Create `AgentRegistry` class in Python
  - [ ] Parse `settings.agents` from YAML
  - [ ] Validate agent configuration schema
  - [ ] Support inheritance from `settings.llm`
  - [ ] Unit tests for registry

- [ ] **Task 2: `agent.dispatch` Action** (AC: 2)
  - [ ] Implement dispatch action in Python
  - [ ] Add Jinja2 task templating
  - [ ] Integrate with existing `llm.call` infrastructure
  - [ ] Add timeout and retry configuration
  - [ ] Unit and integration tests

- [ ] **Task 3: `agent.parallel` Action** (AC: 3)
  - [ ] Implement parallel dispatch using ThreadPoolExecutor
  - [ ] Implement `collect` aggregation
  - [ ] Implement `vote` aggregation with tie-breaking
  - [ ] Implement `consensus` with retry loop
  - [ ] Implement `first` with cancellation
  - [ ] Unit and integration tests

- [ ] **Task 4: `agent.sequential` Action** (AC: 4)
  - [ ] Implement sequential chaining
  - [ ] State threading between agents
  - [ ] Optional transformation function
  - [ ] Early exit configuration
  - [ ] Unit and integration tests

- [ ] **Task 5: `agent.coordinate` Action** (AC: 5)
  - [ ] Implement coordinator pattern
  - [ ] Leader agent dispatch logic
  - [ ] Result aggregation and validation
  - [ ] Re-dispatch on failure
  - [ ] Unit and integration tests

- [ ] **Task 6: Tool Bridge Integration** (AC: 6)
  - [ ] Integrate with `tools.mcp` bridge
  - [ ] Integrate with `tools.crewai` bridge
  - [ ] Integrate with `tools.langchain` bridge
  - [ ] Tool discovery per agent
  - [ ] Integration tests

- [ ] **Task 7: Rust Implementation** (AC: 7, 8)
  - [ ] Create `agent_actions.rs` module
  - [ ] Implement `AgentRegistry` struct
  - [ ] Implement `agent.dispatch` action
  - [ ] Implement `agent.parallel` action
  - [ ] Implement `agent.sequential` action
  - [ ] Design doc for `agent.coordinate`
  - [ ] Unit and integration tests

- [ ] **Task 8: Documentation & Examples**
  - [ ] Update YAML_REFERENCE.md
  - [ ] Create example: multi-agent-research.yaml
  - [ ] Create example: consensus-voting.yaml
  - [ ] Create example: coordinator-pattern.yaml

- [ ] **Task 9: CrewAI Delegation Bridge** (AC: 9)
  - [ ] Implement `agent.crewai_delegate` action
  - [ ] CrewAI process mapping (sequential, hierarchical)
  - [ ] Tool bridge integration between TEA and CrewAI
  - [ ] Graceful fallback when CrewAI unavailable
  - [ ] Integration tests with mocked CrewAI

## Dev Notes

### Source Tree Context

**Python:**
```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py           # Add agent_actions to registry
│   ├── llm_actions.py        # Reference: llm.call implementation
│   ├── agent_actions.py      # NEW: Multi-agent actions
│   └── ...
├── yaml_engine.py            # settings.agents parsing
└── parallel.py               # Reference: parallel execution
```

**Rust:**
```
rust/src/
├── engine/
│   ├── mod.rs                # Add agent_actions module
│   ├── actions/
│   │   ├── llm_actions.rs    # Reference: llm actions
│   │   └── agent_actions.rs  # NEW: Multi-agent actions
│   └── yaml.rs               # settings.agents parsing
```

### Agent Definition Schema

```yaml
settings:
  agents:
    <agent_name>:
      model: str              # Required: model identifier
      system_prompt: str      # Optional: system prompt
      temperature: float      # Optional: 0.0-2.0, default 0.7
      max_tokens: int         # Optional: max response tokens
      tools: list[str]        # Optional: tool names from bridges
      timeout: int            # Optional: seconds, default 60
      retry:
        max_attempts: int     # Optional: default 3
        backoff: float        # Optional: multiplier, default 2.0
```

### Aggregation Strategy Details

| Strategy | Use Case | Returns |
|----------|----------|---------|
| `collect` | Diverse perspectives | `list[response]` |
| `vote` | Classification consensus | `str` (majority) |
| `consensus` | Agreement required | `str` (agreed value) |
| `first` | Latency optimization | First successful response |

### Related Stories
- TEA-BUILTIN-001.2: LLM Enhanced Actions
- TEA-BUILTIN-002.3: Tools Bridge Actions
- TD.13: Parallel Reliability Enhancement

## Testing

### Test File Locations
- Python: `python/tests/test_agent_actions.py`
- Rust: `rust/tests/test_agent_actions.rs`

### Test Categories

| Category | Count | Priority |
|----------|-------|----------|
| Agent Registry | 8 | P0 |
| agent.dispatch | 6 | P0 |
| agent.parallel | 12 | P0 |
| agent.sequential | 6 | P1 |
| agent.coordinate | 8 | P1 |
| Tool Integration | 6 | P1 |
| Error Handling | 8 | P0 |

### Key Test Scenarios

1. **Agent registry validation** - Invalid config raises clear error
2. **Parallel timeout** - Slow agents timeout correctly
3. **Vote tie-breaking** - Deterministic tie resolution
4. **Consensus retry** - Retries until threshold or max iterations
5. **Tool bridge availability** - Clear error when bridge unavailable
6. **State isolation** - Parallel agents don't share mutable state

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-04 | 0.1 | Initial story draft | Sarah (PO) |

## QA Notes

**Reviewed by:** Quinn (Test Architect)
**Review Date:** 2026-01-05 (Updated)
**Test Design Reference:** `docs/qa/assessments/TEA-AGENT-001.1-test-design-20260105.md`

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 63 |
| Unit tests | 30 (48%) |
| Integration tests | 24 (38%) |
| E2E tests | 9 (14%) |
| P0 (Critical) | 22 scenarios |
| AC coverage | 100% (AC1-AC9) |

All 9 acceptance criteria have explicit test coverage with appropriate test levels (unit for pure logic, integration for component interaction, E2E for critical paths). AC9 (CrewAI Delegation Mode) adds 9 scenarios focusing on graceful fallback when CrewAI is unavailable.

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **State mutation in parallel agents** | HIGH | Explicit isolation test (001.1-INT-008) validates deep copy behavior |
| **Timeout enforcement in parallel** | HIGH | Dedicated tests (001.1-INT-005, 001.1-INT-009) verify timeout behavior |
| **Vote tie-breaking determinism** | MEDIUM | Test 001.1-UNIT-014 validates consistent tie resolution |
| **Python/Rust parity drift** | MEDIUM | Cross-runtime test (001.1-E2E-008) ensures behavioral consistency |
| **Tool bridge failure modes** | MEDIUM | Tests distinguish unavailable vs failed states (001.1-INT-ERR-003) |
| **CrewAI unavailable at runtime** | MEDIUM | Graceful fallback test (001.1-INT-026) ensures native TEA execution |
| **Tool mapping failures (CrewAI)** | MEDIUM | Tests (001.1-UNIT-028, 001.1-INT-027) validate cross-bridge mapping |

### Recommended Test Scenarios

**P0 (Must implement before merge):**
1. Agent registry validation and inheritance from `settings.llm`
2. `agent.dispatch` core execution with state update
3. `agent.parallel` with `collect` aggregation and state isolation
4. Timeout enforcement across dispatch and parallel actions
5. Error handling with clear messaging (agent not found, validation failures)

**P1 (Should implement for release):**
1. All aggregation strategies (`vote`, `consensus`, `first`)
2. `agent.sequential` state threading
3. `agent.coordinate` leader/worker pattern
4. Tool bridge integration (MCP, CrewAI, LangChain)
5. Rust feature parity validation
6. `agent.crewai_delegate` configuration and fallback behavior

### Concerns and Blockers

| Type | Description | Recommendation |
|------|-------------|----------------|
| **Technical Risk** | `agent.coordinate` involves complex state management | Consider design-doc-only for Rust (as noted in AC8) |
| **Testing Complexity** | LLM calls require mocking infrastructure | Establish mock response fixtures before implementation |
| **Dependency** | Tool bridges (MCP, CrewAI, LangChain) must be stable | Integration tests should use bridge mocks initially |
| **Optional Dependency** | AC9 requires `tools-crewai` optional dependency | Ensure graceful import handling when CrewAI unavailable |

### Quality Gate Criteria

For this story to pass QA gate:
- [ ] All P0 unit tests passing (22 scenarios)
- [ ] State isolation verified in parallel execution
- [ ] Timeout/retry behavior validated
- [ ] Error messages include agent name and context
- [ ] Python test coverage >90% for `agent_actions.py`
- [ ] Rust module compiles and basic parity tests pass
- [ ] CrewAI fallback works gracefully when dependency unavailable (AC9)
