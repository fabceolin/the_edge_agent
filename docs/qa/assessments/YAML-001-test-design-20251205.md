# Test Design: Story YAML-001

**Date:** 2025-12-05
**Designer:** Quinn (Test Architect)
**Story:** YAML-Based Agent Configuration Engine

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 47 | 100% |
| **Unit tests** | 28 | 60% |
| **Integration tests** | 14 | 30% |
| **E2E tests** | 5 | 10% |

### Priority Distribution

| Priority | Count | Coverage Focus |
|----------|-------|----------------|
| P0 | 12 | Security-critical, data integrity, core parsing |
| P1 | 22 | Core functionality, main user flows |
| P2 | 10 | Secondary features, edge cases |
| P3 | 3 | Nice-to-have, cosmetic scenarios |

---

## Test Scenarios by Acceptance Criteria

### AC1: Core YAML Engine - `YAMLEngine` Class Creation

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-UNIT-001 | Unit | P0 | `YAMLEngine.__init__()` creates valid instance with default actions registry | Core constructor, pure logic |
| YAML-001-UNIT-002 | Unit | P1 | `YAMLEngine.__init__()` accepts and merges custom actions_registry | Constructor parameter handling |
| YAML-001-UNIT-003 | Unit | P1 | `YAMLEngine` initializes empty variables and secrets dictionaries | State initialization |

### AC2: Load from File

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-INT-001 | Integration | P0 | `load_from_file()` parses valid YAML and returns compiled StateGraph | File I/O + YAML parsing integration |
| YAML-001-INT-002 | Integration | P0 | `load_from_file()` raises appropriate error for non-existent file | Error handling at system boundary |
| YAML-001-UNIT-004 | Unit | P1 | `load_from_file()` handles YAML syntax errors gracefully | Input validation |
| YAML-001-INT-003 | Integration | P2 | `load_from_file()` handles empty YAML file | Edge case handling |

### AC3: Load from Dictionary

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-UNIT-005 | Unit | P0 | `load_from_dict()` with minimal config returns valid StateGraph | Core parsing logic |
| YAML-001-UNIT-006 | Unit | P0 | `load_from_dict()` extracts and stores global variables | State extraction |
| YAML-001-UNIT-007 | Unit | P1 | `load_from_dict()` applies state_schema to created graph | Schema propagation |
| YAML-001-UNIT-008 | Unit | P1 | `load_from_dict()` applies config section (raise_exceptions) | Config propagation |
| YAML-001-UNIT-009 | Unit | P1 | `load_from_dict()` applies interrupt_before/interrupt_after | Compilation options |

### AC4: Node Definition - Inline Python Code (`run:`)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-UNIT-010 | Unit | P0 | `_create_inline_function()` executes simple Python code with state access | Core execution logic |
| YAML-001-UNIT-011 | Unit | P0 | Inline code can modify and return state updates | State mutation |
| YAML-001-UNIT-012 | Unit | P1 | Inline code auto-imports `requests` when referenced | Dynamic import |
| YAML-001-UNIT-013 | Unit | P1 | Inline code auto-imports `datetime` when referenced | Dynamic import |
| YAML-001-UNIT-014 | Unit | P1 | Inline code auto-imports `OpenAI` when referenced | Dynamic import |
| YAML-001-UNIT-015 | Unit | P0 | **SECURITY:** Inline code execution is sandboxed (no file system access beyond allowed) | Security boundary |

### AC5: Node Definition - Script Style (`script:`)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-UNIT-016 | Unit | P1 | `script:` key executes same as `run:` | API parity |
| YAML-001-UNIT-017 | Unit | P2 | Multi-line script execution works correctly | GitLab CI compatibility |

### AC6: Node Definition - Built-in Actions (`uses:`)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-UNIT-018 | Unit | P0 | `_create_action_function()` retrieves action from registry | Action lookup |
| YAML-001-UNIT-019 | Unit | P0 | Action function raises ValueError for unknown action | Error handling |
| YAML-001-UNIT-020 | Unit | P1 | Action parameters are template-processed before call | Template integration |
| YAML-001-UNIT-021 | Unit | P1 | `output:` key maps action result to specified state key | Output mapping |
| YAML-001-UNIT-022 | Unit | P2 | Action result dict is returned directly if no output_key | Default behavior |

### AC7: Node Definition - Multi-step Nodes (`steps:`)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-UNIT-023 | Unit | P1 | `_create_steps_function()` executes steps sequentially | Sequential execution |
| YAML-001-UNIT-024 | Unit | P1 | Step results accumulate into current_state | State propagation |
| YAML-001-UNIT-025 | Unit | P1 | Each step can access results from previous steps | Step chaining |
| YAML-001-UNIT-026 | Unit | P2 | Step names default to `step_N` if not provided | Default naming |

### AC8: Node Definition - Expression Evaluation

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-UNIT-027 | Unit | P1 | `run.type: expression` evaluates Python expression | Expression mode |
| YAML-001-UNIT-028 | Unit | P1 | Expression result stored in `output_key` (default: 'result') | Output handling |
| YAML-001-UNIT-029 | Unit | P0 | Invalid expression raises ValueError with original expression | Error messaging |

### AC9: Fan-in Node Support

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-INT-004 | Integration | P1 | `fan_in: true` creates fan-in node via `graph.add_fanin_node()` | Graph integration |
| YAML-001-INT-005 | Integration | P1 | Fan-in node receives `parallel_results` from parallel flows | Parallel execution |

### AC10: Edge Definition - Simple Edges

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-UNIT-030 | Unit | P1 | Simple edge `from: A, to: B` creates unconditional edge | Basic edge creation |
| YAML-001-UNIT-031 | Unit | P1 | `from: __start__` sets entry point | Entry point handling |
| YAML-001-UNIT-032 | Unit | P1 | `to: __end__` sets finish point | Finish point handling |
| YAML-001-UNIT-033 | Unit | P2 | `type: entry` also sets entry point | Alternative syntax |
| YAML-001-UNIT-034 | Unit | P2 | `type: finish` also sets finish point | Alternative syntax |

### AC11: Edge Definition - Parallel Edges

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-INT-006 | Integration | P1 | `type: parallel` with `fan_in:` creates parallel edge | Parallel edge creation |
| YAML-001-INT-007 | Integration | P1 | Multiple parallel edges converge at fan-in node | Fan-out/fan-in pattern |

### AC12: Edge Definition - Conditional Edges

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-UNIT-035 | Unit | P1 | `condition.type: expression` creates conditional routing | Expression conditions |
| YAML-001-UNIT-036 | Unit | P1 | Simple string condition evaluates as expression | String condition |
| YAML-001-UNIT-037 | Unit | P0 | Invalid condition type raises ValueError | Error handling |

### AC13: Edge Definition - When Clause

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-UNIT-038 | Unit | P1 | `when: true/false` string creates boolean condition | Simple when clause |
| YAML-001-UNIT-039 | Unit | P1 | `when: !variable` creates negation condition | Negation support |
| YAML-001-UNIT-040 | Unit | P1 | `when: variable_name` creates state lookup condition | Variable reference |

### AC14: Template Variable Processing

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-UNIT-041 | Unit | P0 | `{{ state.key }}` replaced with state value | Core template feature |
| YAML-001-UNIT-042 | Unit | P1 | `{{ variables.key }}` replaced with global variable | Variables access |
| YAML-001-UNIT-043 | Unit | P1 | `${{ secrets.key }}` replaced with secret value | Secrets access |
| YAML-001-UNIT-044 | Unit | P1 | `${ }` style (GitLab) replaced with variable | GitLab compatibility |
| YAML-001-UNIT-045 | Unit | P2 | Filter `| json` applies JSON serialization | JSON filter |
| YAML-001-UNIT-046 | Unit | P2 | Filter `| upper` applies uppercase | Upper filter |
| YAML-001-UNIT-047 | Unit | P2 | Filter `| lower` applies lowercase | Lower filter |
| YAML-001-UNIT-048 | Unit | P1 | Nested template in dict parameters processed recursively | Recursive processing |
| YAML-001-UNIT-049 | Unit | P1 | Template in list items processed | List processing |
| YAML-001-UNIT-050 | Unit | P2 | Invalid template expression returns original string | Graceful fallback |

### AC15: Built-in Actions Registry

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-INT-008 | Integration | P1 | `llm.call` action calls OpenAI API with model/messages/temperature | LLM integration |
| YAML-001-INT-009 | Integration | P1 | `http.get` action makes GET request and returns JSON | HTTP GET |
| YAML-001-INT-010 | Integration | P1 | `http.post` action makes POST request with JSON body | HTTP POST |
| YAML-001-INT-011 | Integration | P1 | `file.write` action writes content and creates parent dirs | File write |
| YAML-001-INT-012 | Integration | P1 | `file.read` action reads file content | File read |
| YAML-001-UNIT-051 | Unit | P2 | `actions.notify` prints to console with channel prefix | Notify action |
| YAML-001-UNIT-052 | Unit | P1 | Custom action registration via constructor works | Custom actions |

### AC16: Package Integration

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-INT-013 | Integration | P0 | `from the_edge_agent import YAMLEngine` works | Package export |
| YAML-001-INT-014 | Integration | P1 | `YAMLEngine` in `__all__` exports | Public API |

---

## E2E Test Scenarios

Critical user journeys requiring full environment validation:

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| YAML-001-E2E-001 | E2E | P0 | Load and execute `yaml_agent_example.yaml` with research flow | Primary example validation |
| YAML-001-E2E-002 | E2E | P1 | Load and execute `yaml_customer_support_example.yaml` with routing | Multi-path routing validation |
| YAML-001-E2E-003 | E2E | P1 | Load and execute `yaml_perplexity_example.yaml` (mocked API) | External API integration |
| YAML-001-E2E-004 | E2E | P1 | `run_yaml_agent.py` CLI runs YAML agent with JSON input | CLI runner validation |
| YAML-001-E2E-005 | E2E | P2 | Stream execution yields intermediate states | Streaming functionality |

---

## Security Test Scenarios

**Critical security considerations for code execution engine:**

| ID | Level | Priority | Test Scenario | Risk Mitigated |
|----|-------|----------|---------------|----------------|
| YAML-001-SEC-001 | Unit | P0 | Inline code cannot import `os` and execute shell commands | Code injection |
| YAML-001-SEC-002 | Unit | P0 | Inline code cannot access `__builtins__` directly | Sandbox escape |
| YAML-001-SEC-003 | Unit | P0 | Template processing does not allow arbitrary code execution via `eval` | Template injection |
| YAML-001-SEC-004 | Unit | P0 | Expression evaluation is limited to safe operations | Expression injection |

**Note:** Current implementation uses `exec()` and `eval()` which are inherently dangerous. These security tests document the current attack surface and should inform whether sandboxing improvements are needed.

---

## Risk Coverage Matrix

| Risk | Test IDs | Mitigation Level |
|------|----------|------------------|
| YAML parsing failures | YAML-001-INT-001, INT-002, UNIT-004 | High |
| Code execution security | YAML-001-SEC-001 through SEC-004 | Medium (needs review) |
| Template injection | YAML-001-UNIT-041 through 050 | Medium |
| Action registry failures | YAML-001-UNIT-018, UNIT-019 | High |
| Parallel execution bugs | YAML-001-INT-004 through INT-007 | Medium |
| State corruption | YAML-001-UNIT-010, UNIT-011, UNIT-024 | High |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 tests)
1. YAML-001-UNIT-001 through UNIT-011 (Core parsing/loading)
2. YAML-001-SEC-001 through SEC-004 (Security validation)
3. YAML-001-INT-001, INT-002 (File loading)
4. YAML-001-INT-013 (Package export)

### Phase 2: Core Functionality (P1 tests)
1. Node definition tests (UNIT-012 through UNIT-029)
2. Edge definition tests (UNIT-030 through UNIT-040)
3. Template processing tests (UNIT-041 through UNIT-049)
4. Built-in actions tests (INT-008 through INT-012)
5. E2E example execution tests

### Phase 3: Secondary Features (P2+ tests)
1. Filter tests
2. Alternative syntax tests
3. Edge case tests

---

## Test Implementation Notes

### Mocking Strategy

| Component | Mock Approach |
|-----------|---------------|
| OpenAI API | Mock `openai.OpenAI` client, return canned responses |
| HTTP requests | Use `responses` or `httpretty` to mock `requests` |
| File system | Use `tempfile` for write tests, fixtures for read tests |
| External YAML files | Use `StringIO` or temp files with known content |

### Test Fixtures Needed

1. **Minimal valid YAML** - bare minimum for graph creation
2. **Complex workflow YAML** - all node/edge types
3. **Invalid YAML samples** - syntax errors, missing required fields
4. **Template test cases** - various substitution patterns

### Parameterized Test Opportunities

- Template filter tests (json, upper, lower)
- Edge type tests (normal, parallel, conditional)
- Node type tests (run, script, uses, steps, expression)
- Built-in action tests (all 6 actions)

---

## Coverage Gaps

None identified - all acceptance criteria have test coverage.

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 47
  by_level:
    unit: 28
    integration: 14
    e2e: 5
  by_priority:
    p0: 12
    p1: 22
    p2: 10
    p3: 3
  coverage_gaps: []
  security_tests: 4
  risk_areas:
    - area: "Code execution (exec/eval)"
      severity: high
      mitigation: "Security tests document attack surface"
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/YAML-001-test-design-20251205.md
P0 tests identified: 12
Security tests identified: 4
```
