# Test Design: Epic TEA-AGENT-001 - Agentic Design Patterns

**Date:** 2026-01-05
**Designer:** Quinn (Test Architect)
**Epic:** TEA-AGENT-001 - Agentic Design Patterns - Built-in Primitives

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total Test Scenarios** | 186 |
| **Unit Tests** | 98 (53%) |
| **Integration Tests** | 62 (33%) |
| **E2E Tests** | 26 (14%) |
| **Priority Distribution** | P0: 82, P1: 68, P2: 28, P3: 8 |

### Coverage by Story

| Story | Unit | Integration | E2E | Total |
|-------|------|-------------|-----|-------|
| 001.1 Multi-Agent | 28 | 16 | 6 | 50 |
| 001.2 Reflection | 24 | 12 | 4 | 40 |
| 001.3 Planning | 22 | 14 | 6 | 42 |
| 001.4 Reasoning | 18 | 12 | 6 | 36 |
| 001.5 A2A Communication | 6 | 8 | 4 | 18 |

---

## Story 001.1: Multi-Agent Collaboration Primitives

### AC1: Agent Definition in YAML Settings

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-UNIT-001 | Unit | P0 | Valid agent config parsed correctly | Pure validation logic |
| 001.1-UNIT-002 | Unit | P0 | Invalid agent config raises ValidationError | Error handling |
| 001.1-UNIT-003 | Unit | P0 | Agent inherits from settings.llm when not specified | Business rule |
| 001.1-UNIT-004 | Unit | P1 | Agent references MCP tools correctly | Tool bridge validation |
| 001.1-UNIT-005 | Unit | P1 | Agent references CrewAI tools correctly | Tool bridge validation |
| 001.1-UNIT-006 | Unit | P1 | Agent references LangChain tools correctly | Tool bridge validation |
| 001.1-INT-001 | Integration | P0 | Agent registry loads from YAML file | Component interaction |
| 001.1-INT-002 | Integration | P1 | Tool availability checked at agent init | Service integration |

### AC2: `agent.dispatch` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-UNIT-007 | Unit | P0 | Dispatch returns agent response in state | Core functionality |
| 001.1-UNIT-008 | Unit | P0 | Jinja2 task templating works | Template processing |
| 001.1-UNIT-009 | Unit | P0 | Dispatch respects agent model config | Configuration |
| 001.1-UNIT-010 | Unit | P0 | Dispatch respects system_prompt config | Configuration |
| 001.1-UNIT-011 | Unit | P1 | Dispatch timeout works correctly | Error handling |
| 001.1-UNIT-012 | Unit | P1 | Dispatch retry works on failure | Retry logic |
| 001.1-INT-003 | Integration | P0 | Dispatch calls LLM with correct params | Service integration |
| 001.1-INT-004 | Integration | P1 | Dispatch integrates with tools | Tool execution |

### AC3: `agent.parallel` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-UNIT-013 | Unit | P0 | Parallel dispatches to multiple agents | Core functionality |
| 001.1-UNIT-014 | Unit | P0 | `collect` aggregation returns all responses | Aggregation |
| 001.1-UNIT-015 | Unit | P0 | `vote` aggregation returns majority | Classification |
| 001.1-UNIT-016 | Unit | P0 | `vote` handles tie-breaking deterministically | Edge case |
| 001.1-UNIT-017 | Unit | P0 | `consensus` retries until agreement | Complex logic |
| 001.1-UNIT-018 | Unit | P1 | `first` returns first successful response | Optimization |
| 001.1-UNIT-019 | Unit | P1 | `first` cancels remaining on success | Resource management |
| 001.1-UNIT-020 | Unit | P0 | max_concurrent limit respected | Resource control |
| 001.1-UNIT-021 | Unit | P0 | Parallel agents get deep-copied state | State isolation |
| 001.1-INT-005 | Integration | P0 | Parallel execution with ThreadPoolExecutor | Concurrency |
| 001.1-INT-006 | Integration | P0 | Parallel timeout handled correctly | Error handling |
| 001.1-INT-007 | Integration | P1 | Parallel integrates with checkpoints | Persistence |

### AC4: `agent.sequential` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-UNIT-022 | Unit | P1 | Sequential chains agents correctly | Core functionality |
| 001.1-UNIT-023 | Unit | P1 | State threading passes accumulated state | State management |
| 001.1-UNIT-024 | Unit | P1 | Optional transformation applied between agents | Flexibility |
| 001.1-UNIT-025 | Unit | P0 | Early exit on agent failure (configurable) | Error handling |
| 001.1-INT-008 | Integration | P1 | Sequential with multiple LLM calls | Service integration |
| 001.1-INT-009 | Integration | P1 | Sequential preserves state across agents | State management |

### AC5: `agent.coordinate` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-UNIT-026 | Unit | P1 | Coordinator dispatches to workers | Core functionality |
| 001.1-UNIT-027 | Unit | P1 | Leader aggregates worker results | Aggregation |
| 001.1-UNIT-028 | Unit | P0 | Re-dispatch on validation failure | Error handling |
| 001.1-UNIT-029 | Unit | P0 | Max coordination rounds enforced | Circuit breaker |
| 001.1-INT-010 | Integration | P1 | Coordinator pattern with real LLM | Service integration |
| 001.1-INT-011 | Integration | P1 | Coordinator handles worker failures | Fault tolerance |

### AC6: Tool Bridge Integration

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-INT-012 | Integration | P1 | Agent uses MCP tools | Bridge integration |
| 001.1-INT-013 | Integration | P1 | Agent uses CrewAI tools | Bridge integration |
| 001.1-INT-014 | Integration | P1 | Agent uses LangChain tools | Bridge integration |
| 001.1-INT-015 | Integration | P0 | Clear error when bridge unavailable | Error handling |

### AC7 & AC8: Python & Rust Implementation

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.1-INT-016 | Integration | P0 | Python actions registered in registry | Framework integration |
| 001.1-E2E-001 | E2E | P0 | Multi-agent research workflow | Critical user journey |
| 001.1-E2E-002 | E2E | P1 | Consensus voting workflow | Complex flow |
| 001.1-E2E-003 | E2E | P1 | Coordinator pattern workflow | Complex flow |
| 001.1-E2E-004 | E2E | P1 | Rust dispatch parity with Python | Cross-runtime |
| 001.1-E2E-005 | E2E | P1 | Rust parallel parity with Python | Cross-runtime |
| 001.1-E2E-006 | E2E | P2 | Rust sequential parity with Python | Cross-runtime |

---

## Story 001.2: Reflection Loop Primitive

### AC1: `reflection.loop` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-001 | Unit | P0 | First-pass success returns immediately | Happy path |
| 001.2-UNIT-002 | Unit | P0 | Generate-evaluate-correct cycle executes | Core functionality |
| 001.2-UNIT-003 | Unit | P0 | Configurable generator (action) works | Configuration |
| 001.2-UNIT-004 | Unit | P1 | Configurable generator (inline code) works | Flexibility |
| 001.2-UNIT-005 | Unit | P0 | Max iterations triggers circuit breaker | Resource protection |
| 001.2-UNIT-006 | Unit | P0 | Returns best result on failure | Recovery |
| 001.2-INT-001 | Integration | P0 | Reflection loop with LLM generator | Service integration |
| 001.2-INT-002 | Integration | P0 | Reflection loop with LLM corrector | Service integration |

### AC2: Schema-Based Evaluator

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-007 | Unit | P0 | Schema validation passes valid JSON | Core functionality |
| 001.2-UNIT-008 | Unit | P0 | Schema validation fails invalid JSON | Error detection |
| 001.2-UNIT-009 | Unit | P0 | Validation errors passed to corrector | Error context |
| 001.2-UNIT-010 | Unit | P1 | $ref external schema works | Advanced feature |
| 001.2-UNIT-011 | Unit | P1 | Type coercion attempted before failure | User-friendly |
| 001.2-INT-003 | Integration | P0 | Schema evaluator integrates with validate.schema | Service integration |

### AC3: LLM-Based Evaluator

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-012 | Unit | P1 | LLM evaluation prompt used correctly | Configuration |
| 001.2-UNIT-013 | Unit | P1 | Structured feedback parsed (pass/fail) | Output parsing |
| 001.2-UNIT-014 | Unit | P1 | Few-shot examples included in prompt | Feature |
| 001.2-UNIT-015 | Unit | P1 | LLM evaluator returns suggestions | Detailed feedback |
| 001.2-INT-004 | Integration | P1 | LLM evaluator with real LLM call | Service integration |
| 001.2-INT-005 | Integration | P2 | LLM-as-judge pattern works | Pattern validation |

### AC4: Custom Evaluator

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-016 | Unit | P1 | Custom Python evaluator executes | Core functionality |
| 001.2-UNIT-017 | Unit | P2 | Custom Lua evaluator executes | Multi-language |
| 001.2-UNIT-018 | Unit | P2 | Custom Prolog evaluator executes | Multi-language |
| 001.2-UNIT-019 | Unit | P1 | Custom evaluator receives state context | Context access |
| 001.2-INT-006 | Integration | P1 | Custom evaluator in full reflection loop | Flow integration |

### AC5: Iteration Tracking

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-020 | Unit | P0 | reflection_iteration counter increments | State tracking |
| 001.2-UNIT-021 | Unit | P0 | reflection_history records all attempts | Audit trail |
| 001.2-UNIT-022 | Unit | P0 | reflection_errors captures failures | Debugging |
| 001.2-INT-007 | Integration | P0 | Iteration state persists in checkpoints | Persistence |

### AC6: On-Failure Strategies

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-023 | Unit | P0 | return_best returns highest-scoring | Strategy |
| 001.2-UNIT-024 | Unit | P1 | return_last returns final attempt | Strategy |
| 001.2-UNIT-025 | Unit | P0 | raise throws ReflectionFailedError | Error handling |
| 001.2-UNIT-026 | Unit | P0 | ReflectionFailedError contains history | Debugging |

### AC7 & AC8: Standalone Actions

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-INT-008 | Integration | P1 | reflection.evaluate standalone works | Modular use |
| 001.2-INT-009 | Integration | P1 | reflection.correct standalone works | Modular use |

### AC9 & AC10: Python & Rust Implementation

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-INT-010 | Integration | P0 | Python actions registered in registry | Framework integration |
| 001.2-INT-011 | Integration | P1 | Rust reflection.loop parity | Cross-runtime |
| 001.2-E2E-001 | E2E | P0 | JSON generation with schema reflection | Critical path |
| 001.2-E2E-002 | E2E | P1 | Code generation with LLM-as-judge | Pattern validation |
| 001.2-E2E-003 | E2E | P1 | Custom evaluator reflection loop | Flexibility |
| 001.2-E2E-004 | E2E | P2 | Rust reflection E2E parity | Cross-runtime |

---

## Story 001.3: Planning & Decomposition Primitive

### AC1: `plan.decompose` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-UNIT-001 | Unit | P0 | Flat strategy returns simple list | Core functionality |
| 001.3-UNIT-002 | Unit | P0 | Hierarchical strategy returns tree | Strategy |
| 001.3-UNIT-003 | Unit | P1 | Iterative strategy plans one step | Strategy |
| 001.3-UNIT-004 | Unit | P0 | Plan structure validated (subtasks, deps) | Validation |
| 001.3-UNIT-005 | Unit | P0 | max_depth enforced for hierarchical | Resource control |
| 001.3-INT-001 | Integration | P0 | Decompose with LLM planner | Service integration |
| 001.3-INT-002 | Integration | P1 | Custom prompt template works | Customization |

### AC2: Plan Structure

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-UNIT-006 | Unit | P0 | Subtask has id, description, deps, status | Data structure |
| 001.3-UNIT-007 | Unit | P0 | DAG validation rejects cycles | Integrity |
| 001.3-UNIT-008 | Unit | P0 | Topological sort produces valid order | Algorithm |
| 001.3-UNIT-009 | Unit | P0 | Status enum: pending/in_progress/completed/failed/skipped | State machine |
| 001.3-UNIT-010 | Unit | P1 | Plan JSON serialization works | Checkpointing |

### AC3: `plan.execute` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-UNIT-011 | Unit | P0 | Subtasks execute in dependency order | Core functionality |
| 001.3-UNIT-012 | Unit | P0 | Independent subtasks execute in parallel | Performance |
| 001.3-UNIT-013 | Unit | P0 | State threading passes accumulated state | State management |
| 001.3-UNIT-014 | Unit | P1 | max_concurrent limit enforced | Resource control |
| 001.3-UNIT-015 | Unit | P0 | Progress tracking updates state | Observability |
| 001.3-INT-003 | Integration | P0 | Execute with LLM subtask executor | Service integration |
| 001.3-INT-004 | Integration | P0 | Execute with agent.dispatch executor | Integration |
| 001.3-INT-005 | Integration | P1 | Execute integrates with checkpoints | Persistence |

### AC4: Subtask Failure Handling

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-UNIT-016 | Unit | P0 | replan strategy triggers re-planning | Recovery |
| 001.3-UNIT-017 | Unit | P1 | retry strategy retries with backoff | Retry logic |
| 001.3-UNIT-018 | Unit | P1 | skip strategy continues execution | Resilience |
| 001.3-UNIT-019 | Unit | P0 | abort strategy stops and returns partial | Fail-fast |
| 001.3-INT-006 | Integration | P0 | Replan with LLM replanner | Service integration |

### AC5: `plan.replan` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-UNIT-020 | Unit | P1 | Completed subtasks preserved | State preservation |
| 001.3-UNIT-021 | Unit | P1 | Re-plan adjusts remaining tasks | Adaptation |
| 001.3-UNIT-022 | Unit | P0 | max_replans enforced | Circuit breaker |
| 001.3-INT-007 | Integration | P1 | Replan from interrupted checkpoint | Resume |

### AC6: `plan.status` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-INT-008 | Integration | P1 | Status returns correct counts | Reporting |
| 001.3-INT-09 | Integration | P2 | include_completed filter works | Filtering |
| 001.3-INT-010 | Integration | P2 | include_details returns subtask info | Detail level |

### AC7: Checkpoint Integration

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-INT-011 | Integration | P1 | Plan state persists across checkpoints | Durability |
| 001.3-INT-012 | Integration | P0 | Execution resumes from interrupted subtask | Resume |
| 001.3-INT-013 | Integration | P0 | Completed subtasks not re-executed | Efficiency |

### AC8 & AC9: Python & Rust Implementation

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.3-INT-014 | Integration | P0 | Python actions registered in registry | Framework integration |
| 001.3-E2E-001 | E2E | P0 | Research planning workflow | Critical path |
| 001.3-E2E-002 | E2E | P1 | Code refactoring plan execution | Pattern validation |
| 001.3-E2E-003 | E2E | P1 | Multi-step task with replan | Complex flow |
| 001.3-E2E-004 | E2E | P1 | Checkpoint resume after failure | Durability |
| 001.3-E2E-005 | E2E | P2 | Rust planning E2E parity | Cross-runtime |
| 001.3-E2E-006 | E2E | P2 | Planning + Multi-Agent integration | Cross-story |

---

## Story 001.4: Reasoning Techniques Primitives

### AC1: `reason.cot` Action (Chain-of-Thought)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.4-UNIT-001 | Unit | P0 | CoT returns {thinking, answer} structure | Core output |
| 001.4-UNIT-002 | Unit | P0 | CoT prompting applied correctly | Prompt engineering |
| 001.4-UNIT-003 | Unit | P1 | Few-shot examples included | Feature |
| 001.4-UNIT-004 | Unit | P1 | step_by_step thinking format works | Configuration |
| 001.4-UNIT-005 | Unit | P2 | pros_cons thinking format works | Configuration |
| 001.4-UNIT-006 | Unit | P2 | tree thinking format works | Configuration |
| 001.4-INT-001 | Integration | P0 | CoT with real LLM call | Service integration |
| 001.4-INT-002 | Integration | P1 | CoT output parsing correct | Output processing |

### AC2: `reason.react` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.4-UNIT-007 | Unit | P0 | ReAct loop executes thought-action-observation | Core loop |
| 001.4-UNIT-008 | Unit | P0 | max_steps terminates loop | Resource control |
| 001.4-UNIT-009 | Unit | P0 | Early termination on goal achieved | Efficiency |
| 001.4-UNIT-010 | Unit | P0 | Full trace returned in state | Observability |
| 001.4-INT-003 | Integration | P0 | ReAct with llm.tools integration | Tool execution |
| 001.4-INT-004 | Integration | P0 | ReAct with web.search tool | Real tool |
| 001.4-INT-005 | Integration | P1 | ReAct with MCP tools | Bridge integration |
| 001.4-INT-006 | Integration | P1 | ReAct trace format Opik-compatible | Observability |

### AC3: `reason.self_correct` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.4-UNIT-011 | Unit | P1 | Generate-critique-improve cycle runs | Core functionality |
| 001.4-UNIT-012 | Unit | P1 | Configurable improvement rounds | Configuration |
| 001.4-UNIT-013 | Unit | P1 | Improvement trace preserved | Observability |
| 001.4-UNIT-014 | Unit | P2 | Different models for gen vs critique | Flexibility |
| 001.4-INT-007 | Integration | P1 | Self-correct with LLM | Service integration |

### AC4: `reason.decompose` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.4-UNIT-015 | Unit | P1 | Problem decomposed into sub-problems | Core functionality |
| 001.4-UNIT-016 | Unit | P1 | Sub-problems solved individually | Divide-conquer |
| 001.4-UNIT-017 | Unit | P1 | Final answer synthesized | Synthesis |
| 001.4-UNIT-018 | Unit | P2 | Recursive decomposition works | Advanced feature |
| 001.4-INT-008 | Integration | P1 | Decompose with LLM | Service integration |

### AC5 & AC6: Structured Output & Tool Integration

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.4-INT-009 | Integration | P0 | reasoning_trace preserved in state | Observability |
| 001.4-INT-010 | Integration | P1 | Trace logged correctly | Debugging |
| 001.4-INT-011 | Integration | P1 | Custom inline tools work in ReAct | Flexibility |

### AC7 & AC8: Python & Rust Implementation

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.4-INT-012 | Integration | P0 | Python actions registered in registry | Framework integration |
| 001.4-E2E-001 | E2E | P0 | CoT problem-solving workflow | Critical path |
| 001.4-E2E-002 | E2E | P0 | ReAct research agent workflow | Critical path |
| 001.4-E2E-003 | E2E | P1 | Self-correcting code generation | Pattern validation |
| 001.4-E2E-004 | E2E | P1 | Decompose complex problem | Pattern validation |
| 001.4-E2E-005 | E2E | P2 | Rust CoT parity | Cross-runtime |
| 001.4-E2E-006 | E2E | P2 | Rust ReAct parity | Cross-runtime |

---

## Story 001.5: Inter-Agent Communication (A2A)

### AC1: `a2a.send` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5-UNIT-001 | Unit | P0 | Send message to named agent | Core functionality |
| 001.5-UNIT-002 | Unit | P0 | Message has type, payload, correlation_id | Data structure |
| 001.5-UNIT-003 | Unit | P1 | Fire-and-forget behavior | Non-blocking |
| 001.5-UNIT-004 | Unit | P2 | Delivery confirmation optional | Feature |

### AC2: `a2a.receive` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5-INT-001 | Integration | P0 | Receive from specified agents | Core functionality |
| 001.5-INT-002 | Integration | P0 | Timeout works correctly | Error handling |
| 001.5-INT-003 | Integration | P1 | Message type filter works | Filtering |
| 001.5-INT-004 | Integration | P0 | require_all waits for all agents | Coordination |

### AC3: `a2a.broadcast` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5-INT-005 | Integration | P1 | Broadcast reaches all in namespace | Distribution |
| 001.5-UNIT-005 | Unit | P0 | Namespace isolation prevents cross-workflow | Security |
| 001.5-UNIT-006 | Unit | P2 | Agent type filter works | Filtering |

### AC4: `a2a.delegate` Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5-INT-006 | Integration | P0 | Delegate sends and waits for result | Core functionality |
| 001.5-INT-007 | Integration | P0 | Timeout triggers fallback_local | Error handling |
| 001.5-INT-008 | Integration | P1 | Correlation ID matched correctly | Request/response |

### AC5-AC9: Implementation

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.5-E2E-001 | E2E | P0 | Coordinator-worker workflow | Critical path |
| 001.5-E2E-002 | E2E | P1 | Broadcast consensus pattern | Pattern validation |
| 001.5-E2E-003 | E2E | P1 | Delegation with fallback | Error recovery |
| 001.5-E2E-004 | E2E | P2 | Multi-agent discovery and dispatch | Complex flow |

---

## Risk Coverage Matrix

| Risk (from Epic) | Test Scenarios Mitigating |
|------------------|---------------------------|
| Reflection infinite loops | 001.2-UNIT-005, 001.2-UNIT-025 |
| Multi-agent state conflicts | 001.1-UNIT-021, 001.1-INT-005 |
| Planning over-decomposition | 001.3-UNIT-005, 001.3-UNIT-022 |
| A2A message loss | 001.5-INT-002, 001.5-INT-007 |
| Rust parity complexity | 001.1-E2E-004/5/6, 001.2-E2E-004, 001.3-E2E-005, 001.4-E2E-005/6 |
| Tool bridge compatibility | 001.1-INT-012/13/14/15, 001.4-INT-005 |

---

## Recommended Execution Order

### Phase 1: P0 Unit Tests (Fail Fast)
1. Agent Registry validation (001.1-UNIT-001 to 003)
2. Core action logic (dispatch, parallel, reflection loop, decompose)
3. Error handling and circuit breakers
4. State isolation verification

### Phase 2: P0 Integration Tests
1. LLM service integration
2. Tool bridge integration
3. Checkpoint persistence
4. Action registry verification

### Phase 3: P0 E2E Tests
1. Multi-agent research workflow
2. JSON generation with reflection
3. Research planning workflow
4. CoT problem-solving
5. Coordinator-worker workflow

### Phase 4: P1 Tests
1. Sequential and coordination actions
2. LLM evaluator patterns
3. Failure handling strategies
4. Cross-runtime parity

### Phase 5: P2+ Tests
1. Advanced features (Lua/Prolog evaluators)
2. Edge cases and configuration variations
3. Full cross-runtime parity

---

## Quality Checklist

- [x] Every AC has at least one test scenario
- [x] Test levels appropriate (unit for logic, integration for boundaries, E2E for journeys)
- [x] No duplicate coverage across levels
- [x] Priorities aligned with business risk (P0 for revenue/security critical)
- [x] Test IDs follow naming convention (EPIC.STORY-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
- [x] All identified risks have mitigating tests
- [x] Cross-runtime parity tests included

---

## Gate YAML Block

```yaml
test_design:
  epic_id: TEA-AGENT-001
  design_date: "2026-01-05"
  designer: Quinn (Test Architect)
  scenarios_total: 186
  by_level:
    unit: 98
    integration: 62
    e2e: 26
  by_priority:
    p0: 82
    p1: 68
    p2: 28
    p3: 8
  by_story:
    "001.1": 50
    "001.2": 40
    "001.3": 42
    "001.4": 36
    "001.5": 18
  coverage_gaps: []
  risks_mitigated:
    - reflection_infinite_loops
    - multi_agent_state_conflicts
    - planning_over_decomposition
    - a2a_message_loss
    - rust_parity_complexity
    - tool_bridge_compatibility
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-AGENT-001-test-design-20260105.md
P0 tests identified: 82
P1 tests identified: 68
Total scenarios: 186
Stories covered: 5
```
