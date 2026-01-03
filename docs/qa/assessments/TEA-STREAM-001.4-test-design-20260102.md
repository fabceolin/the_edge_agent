# Test Design: Story TEA-STREAM-001.4 - YAML Integration

Date: 2026-01-02
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 25
- Unit tests: 17 (68%)
- Integration tests: 8 (32%)
- E2E tests: 0 (0%)
- Priority distribution: P0: 12, P1: 10, P2: 3

### Rationale

This story focuses on YAML parsing, validation, and configuration—all operations that can be tested effectively at unit and integration levels. E2E tests are deferred to TEA-STREAM-001.5 (Integration & Documentation) which validates full streaming workflows. The test strategy emphasizes:

1. **Shift-left approach**: Pure parsing logic tested at unit level
2. **Validation isolation**: Each validation rule tested independently
3. **Error message quality**: Explicit tests for actionable error messages
4. **Integration boundaries**: Registry building tested as component integration

---

## Test Scenarios by Acceptance Criteria

### AC1: `streams:` Block Parsed on Nodes

**Requirement**: Parse stdin, stdout, stderr stream channel names from node configuration.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-001 | Unit | P0 | Parse `streams.stdin` channel name from node config | Core parsing - consumer configuration |
| 001.4-UNIT-002 | Unit | P0 | Parse `streams.stdout` channel name from node config | Core parsing - producer configuration |
| 001.4-UNIT-003 | Unit | P1 | Parse `streams.stderr` channel name from node config | Secondary stream type |
| 001.4-UNIT-004 | Unit | P1 | Node without `streams:` block returns None/empty config | Default behavior verification |
| 001.4-UNIT-005 | Unit | P2 | Parse node with all three stream types (stdin, stdout, stderr) | Transform node pattern |

**Given-When-Then Patterns**:

```gherkin
# 001.4-UNIT-001
Given a node configuration with streams.stdin: "input_channel"
When the node streams are parsed
Then NodeStreamsConfig.stdin equals "input_channel"

# 001.4-UNIT-002
Given a node configuration with streams.stdout: "output_channel"
When the node streams are parsed
Then NodeStreamsConfig.stdout equals "output_channel"

# 001.4-UNIT-004
Given a node configuration without a streams block
When the node streams are parsed
Then the result is None or an empty NodeStreamsConfig
```

---

### AC2: `stream_mode:` Parsed on Edges

**Requirement**: Parse stream_mode: broadcast | direct from edge configuration.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-006 | Unit | P0 | Parse `stream_mode: direct` returns "direct" | Default/explicit mode |
| 001.4-UNIT-007 | Unit | P0 | Parse `stream_mode: broadcast` returns "broadcast" | Multi-consumer mode |
| 001.4-UNIT-008 | Unit | P1 | Edge without stream_mode defaults to "direct" | Default behavior |
| 001.4-UNIT-009 | Unit | P1 | Invalid stream_mode (e.g., "unicast") raises ValueError | Input validation |

**Given-When-Then Patterns**:

```gherkin
# 001.4-UNIT-006
Given an edge configuration with stream_mode: direct
When the edge stream mode is parsed
Then the result is "direct"

# 001.4-UNIT-009
Given an edge configuration with stream_mode: "invalid"
When the edge stream mode is parsed
Then a ValueError is raised with message containing "direct" and "broadcast"
```

---

### AC3: Stream Channels Auto-Registered During YAML Compilation

**Requirement**: StreamRegistry populated with all declared channels during compilation.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-INT-001 | Integration | P0 | Registry contains all declared stream channels after build | Core registry building |
| 001.4-INT-002 | Integration | P0 | Producer channels (stdout) registered with STDOUT direction | Direction assignment |
| 001.4-INT-003 | Integration | P0 | Consumer channels (stdin) registered with STDIN direction | Direction assignment |
| 001.4-INT-004 | Integration | P1 | Error channels (stderr) registered with STDERR direction | Error stream handling |
| 001.4-INT-005 | Integration | P1 | Buffer size from settings applied to all channels | Settings propagation |
| 001.4-INT-006 | Integration | P2 | Empty registry returned when streams.enabled: false | Feature toggle |

**Given-When-Then Patterns**:

```gherkin
# 001.4-INT-001
Given YAML with nodes declaring streams: stdout: "data_stream" and stdin: "data_stream"
And settings.parallel.streams.enabled: true
When the stream registry is built
Then the registry contains channel "data_stream"

# 001.4-INT-002
Given YAML with node "producer" declaring streams.stdout: "output"
When the stream registry is built
Then channel "output" has direction STDOUT and node_name "producer"

# 001.4-INT-006
Given YAML with stream nodes but settings.parallel.streams.enabled: false
When the stream registry is built
Then the registry is empty (no channels)
```

---

### AC4: Validation - Stream Name Referenced by Consumer Must Exist

**Requirement**: Error if a node consumes a stream that no node produces.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-010 | Unit | P0 | Error raised when stdin references non-existent stream | Missing producer detection |
| 001.4-UNIT-011 | Unit | P0 | Error raised when multiple nodes produce same stream | Duplicate producer detection |
| 001.4-UNIT-012 | Unit | P1 | Valid producer-consumer pair passes validation | Happy path |
| 001.4-INT-007 | Integration | P1 | Complex graph with multiple valid streams validates successfully | Multi-stream topology |

**Given-When-Then Patterns**:

```gherkin
# 001.4-UNIT-010
Given a node with streams.stdin: "missing_stream"
And no node produces "missing_stream" via stdout
When the registry is built
Then a ValueError is raised with message containing "missing_stream" and "no producer"

# 001.4-UNIT-011
Given node "A" with streams.stdout: "data"
And node "B" with streams.stdout: "data"
When the registry is built
Then a ValueError is raised with message containing "multiple producers"
```

---

### AC5: Validation - Streams Only Valid with `parallel_strategy: process`

**Requirement**: Stream configuration requires process-based parallelism.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-013 | Unit | P0 | Error raised when streams enabled with strategy: thread | Strategy incompatibility |
| 001.4-UNIT-014 | Unit | P1 | Streams work with explicit strategy: process | Happy path validation |
| 001.4-UNIT-015 | Unit | P1 | Error message suggests fix: add strategy: process | Actionable guidance |

**Given-When-Then Patterns**:

```gherkin
# 001.4-UNIT-013
Given YAML with settings.parallel.strategy: thread
And settings.parallel.streams.enabled: true
When the registry is built
Then a ValueError is raised with message "Streams require parallel_strategy: process"

# 001.4-UNIT-015
Given the error for thread strategy with streams
Then the error message contains "Add 'settings.parallel.strategy: process'"
```

---

### AC6: Settings Parsing (`settings.parallel.streams.*`)

**Requirement**: Parse enabled, buffer_size, and timeout from settings.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-016 | Unit | P0 | Parse `streams.enabled` with default false | Feature toggle default |
| 001.4-UNIT-017 | Unit | P0 | Parse `streams.enabled: true` enables streaming | Feature toggle enable |
| 001.4-UNIT-018 | Unit | P1 | Parse `streams.buffer_size` with default 65536 | Buffer configuration |
| 001.4-UNIT-019 | Unit | P1 | Parse custom `streams.buffer_size: 131072` | Custom buffer |
| 001.4-UNIT-020 | Unit | P2 | Parse `streams.timeout` with default None | Timeout configuration |

**Given-When-Then Patterns**:

```gherkin
# 001.4-UNIT-016
Given YAML settings without streams configuration
When stream settings are parsed
Then enabled is false

# 001.4-UNIT-018
Given YAML settings without buffer_size
When stream settings are parsed
Then buffer_size is 65536

# 001.4-UNIT-019
Given YAML settings with streams.buffer_size: 131072
When stream settings are parsed
Then buffer_size is 131072
```

---

### AC7: Error Messages Guide User to Correct Configuration

**Requirement**: All error messages include actionable fix suggestions.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-021 | Unit | P0 | Missing producer error includes "Add a node with 'streams.stdout:'" | Actionable fix |
| 001.4-UNIT-022 | Unit | P0 | Strategy error includes exact YAML to add | Actionable fix |
| 001.4-UNIT-023 | Unit | P1 | Multiple producers error lists both node names | Debugging context |
| 001.4-UNIT-024 | Unit | P1 | Direct mode multi-target error suggests broadcast | Alternative solution |
| 001.4-INT-008 | Integration | P1 | Platform error (Windows) is clear about OS requirement | Platform guidance |

**Given-When-Then Patterns**:

```gherkin
# 001.4-UNIT-021
Given a consumer node referencing non-existent stream "data"
When validation fails
Then error message contains "Add a node with 'streams.stdout: data'"

# 001.4-UNIT-024
Given an edge with stream_mode: direct and to: [node_a, node_b]
When validation runs
Then error message contains "Use stream_mode: broadcast for multiple targets"
```

---

## Additional Coverage: Interrupt Point Validation

**Derived from Technical Design**: Stream nodes cannot have checkpoints.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-025 | Unit | P0 | Error if stream node has interrupt_before: true | Checkpoint incompatibility |
| 001.4-UNIT-026 | Unit | P0 | Error if stream node has interrupt_after: true | Checkpoint incompatibility |

**Given-When-Then Patterns**:

```gherkin
# 001.4-UNIT-025
Given a node with streams.stdout: "data" and interrupt_before: true
When the YAML is compiled
Then a ValueError is raised with message containing "Stream nodes cannot be checkpointed"
```

---

## Risk Coverage Matrix

| Risk (from Story) | Mitigated By |
|-------------------|--------------|
| YAML schema breaking change | 001.4-UNIT-004, 001.4-INT-006 (backward compat via opt-in) |
| Complex validation logic | 001.4-UNIT-010 through 001.4-UNIT-015 (isolated validation tests) |
| Error messages unclear | 001.4-UNIT-021 through 001.4-INT-008 (explicit message tests) |

---

## Test Implementation Notes

### Test File Location

`python/tests/test_yaml_streams.py`

### Test Fixtures Needed

```python
@pytest.fixture
def minimal_stream_yaml():
    """Minimal valid streaming YAML configuration."""
    return {
        "settings": {
            "parallel": {
                "strategy": "process",
                "streams": {"enabled": True}
            }
        },
        "nodes": [
            {"name": "producer", "run": "pass", "streams": {"stdout": "data"}},
            {"name": "consumer", "run": "pass", "streams": {"stdin": "data"}}
        ],
        "edges": []
    }

@pytest.fixture
def non_stream_yaml():
    """YAML without any streaming configuration."""
    return {
        "nodes": [{"name": "simple", "run": "pass"}],
        "edges": []
    }
```

### Mock Requirements

- Mock `validate_platform()` to test Windows error path
- No external dependencies needed for parsing tests

---

## Recommended Execution Order

1. **P0 Unit tests** (12 tests) - Fail fast on core parsing/validation
2. **P0 Integration tests** (0 tests in P0) - N/A
3. **P1 Unit tests** (8 tests) - Secondary parsing and defaults
4. **P1 Integration tests** (5 tests) - Registry building
5. **P2 Unit tests** (2 tests) - Edge cases
6. **P2 Integration tests** (1 test) - Feature toggle

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 26
  by_level:
    unit: 18
    integration: 8
    e2e: 0
  by_priority:
    p0: 12
    p1: 11
    p2: 3
  coverage_gaps: []
  notes:
    - E2E deferred to TEA-STREAM-001.5 which validates full streaming workflows
    - Added 2 tests for interrupt point validation (derived from technical design)
```

---

## Quality Checklist

- [x] Every AC has test coverage (AC1-AC7 all covered)
- [x] Test levels are appropriate (unit for parsing, integration for registry)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (P0 for core validation)
- [x] Test IDs follow naming convention (001.4-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Given-When-Then patterns provided for key tests
- [x] Risk mitigations mapped to specific tests
