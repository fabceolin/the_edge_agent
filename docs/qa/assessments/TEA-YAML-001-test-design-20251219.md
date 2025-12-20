# Test Design: Story TEA-YAML-001 - Jinja2 Template Engine

**Date:** 2024-12-19
**Designer:** Quinn (Test Architect)
**Story:** Jinja2 Template Engine
**Status:** Draft

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 28 |
| **Unit tests** | 14 (50%) |
| **Integration tests** | 10 (36%) |
| **E2E tests** | 4 (14%) |
| **P0 tests** | 8 |
| **P1 tests** | 12 |
| **P2 tests** | 8 |

### Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Breaking existing YAML agents | Medium | High | Comprehensive backward compat tests |
| Object passthrough regression | Medium | High | Dedicated unit tests for type preservation |
| Performance degradation | Low | Medium | Benchmark against current eval() |
| StrictUndefined too strict | Low | Low | Document expected behavior changes |

---

## Test Scenarios by Acceptance Criteria

### AC1: Template processing uses Jinja2 instead of Python eval()

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML-001-UNIT-001 | Unit | P0 | Basic interpolation `{{ state.key }}` returns correct value | Core functionality, pure logic |
| YAML-001-UNIT-002 | Unit | P0 | Nested access `{{ state.user.name }}` works | Core functionality |
| YAML-001-UNIT-003 | Unit | P1 | Multiple interpolations in single string | Common usage pattern |
| YAML-001-INT-001 | Integration | P0 | `_process_template()` replaced eval with Jinja2 | Validates architectural change |

### AC2: Custom filter `fromjson` added for JSON parsing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML-001-UNIT-004 | Unit | P1 | `fromjson` filter parses valid JSON string | Custom filter logic |
| YAML-001-UNIT-005 | Unit | P1 | `fromjson` filter handles nested JSON | Edge case handling |
| YAML-001-UNIT-006 | Unit | P1 | `fromjson` filter raises on invalid JSON with clear error | Error handling |
| YAML-001-UNIT-007 | Unit | P2 | `fromjson` combined with other filters `{{ data \| fromjson \| length }}` | Filter chaining |

### AC3: Standard Jinja2 constructs work (if/for/filters)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML-001-UNIT-008 | Unit | P0 | `{% if %}...{% endif %}` conditionals render correctly | Core Jinja2 feature |
| YAML-001-UNIT-009 | Unit | P0 | `{% for %}...{% endfor %}` loops iterate correctly | Core Jinja2 feature |
| YAML-001-UNIT-010 | Unit | P1 | Native filters `\| upper`, `\| lower`, `\| length` work | Standard filters |
| YAML-001-UNIT-011 | Unit | P1 | `\| tojson` filter serializes objects | JSON output |
| YAML-001-UNIT-012 | Unit | P2 | Nested conditionals and loops | Complex templates |
| YAML-001-INT-002 | Integration | P1 | Jinja2 constructs work in `run:` inline code | Integration with exec() |

### AC4: Template variables accessible (state, variables, secrets, checkpoint)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML-001-INT-003 | Integration | P0 | `state` dict accessible in templates | Core context variable |
| YAML-001-INT-004 | Integration | P1 | `variables` from YAML accessible | Global variables |
| YAML-001-INT-005 | Integration | P1 | `secrets` dict accessible (not logged) | Security-sensitive |
| YAML-001-INT-006 | Integration | P2 | `checkpoint.dir` and `checkpoint.last` accessible | Checkpoint integration |

### AC5: Single-expression templates return native Python objects

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML-001-UNIT-013 | Unit | P0 | `"{{ state.data }}"` returns dict, not string | Critical behavior change |
| YAML-001-UNIT-014 | Unit | P0 | `"{{ state.items }}"` returns list, not string | Critical behavior change |
| YAML-001-INT-007 | Integration | P0 | Object passthrough works in action `with:` params | Real usage scenario |

### AC6: `run:` inline Python code continues to work

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML-001-INT-008 | Integration | P1 | `run:` with Python code executes via exec() | Backward compatibility |
| YAML-001-INT-009 | Integration | P1 | `run:` with Jinja2 interpolation + Python code | Hybrid usage |
| YAML-001-E2E-001 | E2E | P1 | Complete YAML agent with `run:` nodes works | Real-world validation |

### AC7: StrictUndefined mode catches undefined variables

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML-001-UNIT-015 | Unit | P1 | Undefined variable raises `UndefinedError` | Error detection |
| YAML-001-UNIT-016 | Unit | P2 | Error message includes variable name | Debuggability |
| YAML-001-INT-010 | Integration | P2 | `\| default(value)` filter prevents UndefinedError | Escape hatch |

### AC8: Performance - Template compilation cached

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML-001-UNIT-017 | Unit | P2 | Same template string uses cached compiled template | Performance optimization |
| YAML-001-PERF-001 | Performance | P2 | Jinja2 vs eval() benchmark (within 2x baseline) | Non-functional requirement |

### AC9: All existing tests pass (backward compatibility)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML-001-E2E-002 | E2E | P0 | All existing `test_yaml_engine.py` tests pass | Regression prevention |
| YAML-001-E2E-003 | E2E | P1 | Example YAML agents in `examples/` work unchanged | Real-world validation |

### AC10: Documentation updated

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML-001-DOC-001 | Manual | P2 | YAML_REFERENCE.md has Jinja2 section | Documentation completeness |
| YAML-001-E2E-004 | E2E | P2 | Documentation examples are executable | Doc accuracy |

---

## Test Scenarios Summary by Level

### Unit Tests (14 scenarios)

```python
# test_jinja2_templates.py

class TestBasicInterpolation:
    def test_simple_state_access(self):
        """YAML-001-UNIT-001: {{ state.key }} returns value"""

    def test_nested_state_access(self):
        """YAML-001-UNIT-002: {{ state.user.name }} works"""

    def test_multiple_interpolations(self):
        """YAML-001-UNIT-003: Multiple {{ }} in string"""

class TestFromJsonFilter:
    def test_parse_valid_json(self):
        """YAML-001-UNIT-004: fromjson parses JSON string"""

    def test_parse_nested_json(self):
        """YAML-001-UNIT-005: fromjson handles nested objects"""

    def test_invalid_json_error(self):
        """YAML-001-UNIT-006: fromjson raises on invalid JSON"""

    def test_filter_chaining(self):
        """YAML-001-UNIT-007: fromjson | length works"""

class TestJinja2Constructs:
    def test_if_conditional(self):
        """YAML-001-UNIT-008: {% if %}...{% endif %}"""

    def test_for_loop(self):
        """YAML-001-UNIT-009: {% for %}...{% endfor %}"""

    def test_native_filters(self):
        """YAML-001-UNIT-010: | upper, | lower, | length"""

    def test_tojson_filter(self):
        """YAML-001-UNIT-011: | tojson serializes"""

    def test_nested_constructs(self):
        """YAML-001-UNIT-012: Nested if/for"""

class TestObjectPassthrough:
    def test_dict_passthrough(self):
        """YAML-001-UNIT-013: {{ state.data }} returns dict"""

    def test_list_passthrough(self):
        """YAML-001-UNIT-014: {{ state.items }} returns list"""

class TestStrictUndefined:
    def test_undefined_raises_error(self):
        """YAML-001-UNIT-015: Undefined variable raises"""

    def test_error_includes_varname(self):
        """YAML-001-UNIT-016: Error shows variable name"""

class TestCaching:
    def test_template_cached(self):
        """YAML-001-UNIT-017: Same string uses cache"""
```

### Integration Tests (10 scenarios)

```python
# test_jinja2_integration.py

class TestArchitecturalChange:
    def test_process_template_uses_jinja2(self):
        """YAML-001-INT-001: _process_template uses Jinja2"""

class TestJinja2InRun:
    def test_jinja2_in_run_block(self):
        """YAML-001-INT-002: Jinja2 constructs in run:"""

class TestContextVariables:
    def test_state_accessible(self):
        """YAML-001-INT-003: state dict in templates"""

    def test_variables_accessible(self):
        """YAML-001-INT-004: variables from YAML"""

    def test_secrets_accessible(self):
        """YAML-001-INT-005: secrets dict accessible"""

    def test_checkpoint_accessible(self):
        """YAML-001-INT-006: checkpoint.dir/last"""

class TestObjectPassthroughIntegration:
    def test_passthrough_in_action_params(self):
        """YAML-001-INT-007: Object passthrough in with:"""

class TestRunBlockCompatibility:
    def test_run_with_python_code(self):
        """YAML-001-INT-008: run: executes Python"""

    def test_run_with_jinja2_and_python(self):
        """YAML-001-INT-009: Jinja2 + Python in run:"""

class TestDefaultFilter:
    def test_default_prevents_undefined(self):
        """YAML-001-INT-010: | default(val) works"""
```

### E2E Tests (4 scenarios)

```python
# test_jinja2_e2e.py

class TestBackwardCompatibility:
    def test_existing_tests_pass(self):
        """YAML-001-E2E-002: All existing tests pass"""

    def test_example_agents_work(self):
        """YAML-001-E2E-003: Example YAML agents unchanged"""

class TestRealWorldUsage:
    def test_complete_agent_with_run(self):
        """YAML-001-E2E-001: Agent with run: nodes"""

    def test_documentation_examples(self):
        """YAML-001-E2E-004: Doc examples executable"""
```

---

## Risk Coverage Matrix

| Risk | Test IDs |
|------|----------|
| Breaking existing YAML agents | YAML-001-E2E-002, YAML-001-E2E-003 |
| Object passthrough regression | YAML-001-UNIT-013, YAML-001-UNIT-014, YAML-001-INT-007 |
| Performance degradation | YAML-001-PERF-001, YAML-001-UNIT-017 |
| StrictUndefined issues | YAML-001-UNIT-015, YAML-001-INT-010 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fast feedback on core logic)
   - YAML-001-UNIT-001, 002, 008, 009, 013, 014
2. **P0 Integration tests** (validate architecture)
   - YAML-001-INT-001, 003, 007
3. **P0 E2E tests** (regression check)
   - YAML-001-E2E-002
4. **P1 tests** (core functionality coverage)
5. **P2 tests** (edge cases, performance)

---

## Quality Gate Block

```yaml
test_design:
  story_id: TEA-YAML-001
  scenarios_total: 28
  by_level:
    unit: 14
    integration: 10
    e2e: 4
  by_priority:
    p0: 8
    p1: 12
    p2: 8
  coverage_gaps: []
  key_risks:
    - id: RISK-001
      description: Breaking existing YAML agents
      mitigated_by: [YAML-001-E2E-002, YAML-001-E2E-003]
    - id: RISK-002
      description: Object passthrough regression
      mitigated_by: [YAML-001-UNIT-013, YAML-001-UNIT-014, YAML-001-INT-007]
  recommendation: PASS
  notes: |
    Comprehensive coverage for a medium-risk refactor.
    Focus on backward compatibility tests first.
    Performance benchmark is P2 but should be monitored.
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (shift-left applied)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Backward compatibility is P0 priority

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-YAML-001-test-design-20251219.md
P0 tests identified: 8
P1 tests identified: 12
Coverage: 10 ACs → 28 test scenarios
```
