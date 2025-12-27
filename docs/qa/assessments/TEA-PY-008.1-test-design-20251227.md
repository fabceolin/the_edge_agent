# Test Design Document: TEA-PY-008.1

**Story**: Extract Template Processing Module
**Date**: 2025-12-27
**Test Architect**: Quinn (QA Agent)

## Executive Summary

This test design covers the extraction of template processing logic from `yaml_engine.py` into a dedicated `yaml_templates.py` module. The refactoring requires **zero behavioral changes**, making this primarily a regression testing effort with strategic unit tests for the new module interface.

**Testing Strategy**:
- **Primary Focus**: Regression (existing tests must pass)
- **Secondary Focus**: Unit tests for new TemplateProcessor class isolation
- **Risk Area**: Template caching state leakage, checkpoint context passing

## Test Level Distribution

| Level | Count | Priority Breakdown |
|-------|-------|-------------------|
| Unit | 18 | P0: 6, P1: 8, P2: 4 |
| Integration | 8 | P0: 4, P1: 3, P2: 1 |
| E2E | 2 | P0: 1, P1: 1 |
| **Total** | **28** | **P0: 11, P1: 12, P2: 5** |

---

## Test Scenarios

### UNIT LEVEL - TemplateProcessor Class (18 tests)

#### Template Processing Core (AC 10-16)

**TEA-PY-008.1-UNIT-001**
- **Priority**: P0 (critical)
- **Requirement**: AC 10 - `{{ state.key }}` variable access works identically
- **Description**: Verify TemplateProcessor.process_template() correctly renders state variables using Jinja2
- **Justification**: Core template functionality. Unit level because no YAMLEngine dependencies needed.
- **Test Cases**:
  - Simple state access: `{{ state.count }}` returns integer value
  - Nested state access: `{{ state.user.name }}` returns nested value
  - DotDict wrapping: nested dicts become DotDict for attribute access

**TEA-PY-008.1-UNIT-002**
- **Priority**: P0 (critical)
- **Requirement**: AC 11 - `{{ variables.key }}` and `{{ secrets.key }}` access works
- **Description**: Verify TemplateProcessor renders variables and secrets passed in constructor
- **Justification**: Critical configuration access. Unit level tests constructor parameters directly.
- **Test Cases**:
  - `{{ variables.api_url }}` renders from variables dict
  - `{{ secrets.api_key }}` renders from secrets dict
  - Combined template: `{{ variables.host }}/{{ secrets.token }}`

**TEA-PY-008.1-UNIT-003**
- **Priority**: P1 (core)
- **Requirement**: AC 12 - `{{ checkpoint.dir }}` and `{{ checkpoint.last }}` work
- **Description**: Verify checkpoint context parameters passed to process_template()
- **Justification**: Unit level because checkpoint context is just parameter passing, no filesystem I/O.
- **Test Cases**:
  - `{{ checkpoint.dir }}` renders checkpoint_dir parameter
  - `{{ checkpoint.last }}` renders last_checkpoint parameter
  - None values render as empty string or null

**TEA-PY-008.1-UNIT-004**
- **Priority**: P1 (core)
- **Requirement**: AC 13 - Jinja2 filters (`| tojson`, `| upper`, `| fromjson`) work
- **Description**: Verify Jinja2 environment filters are accessible in templates
- **Justification**: Unit level - pure template rendering, no engine coupling.
- **Test Cases**:
  - `{{ state.data | tojson }}` serializes dict to JSON string
  - `{{ state.name | upper }}` uppercases string
  - `{{ state.json_str | fromjson }}` deserializes JSON string
  - `{{ state.value | default('fallback') }}` uses default filter
  - `{{ state.list | length }}` returns list length

**TEA-PY-008.1-UNIT-005**
- **Priority**: P0 (critical)
- **Requirement**: AC 14 - Single expression object passthrough preserved
- **Description**: Verify templates with single Jinja2 expression return native Python objects
- **Justification**: Critical for type preservation (AC 5 from TEA-YAML-001). Unit level for isolation.
- **Test Cases**:
  - `{{ state.count }}` returns int, not string
  - `{{ state.enabled }}` returns bool True/False
  - `{{ state.items }}` returns list object
  - `{{ state.config }}` returns dict object
  - Mixed templates `"Count: {{ state.count }}"` return string

**TEA-PY-008.1-UNIT-006**
- **Priority**: P1 (core)
- **Requirement**: AC 15 - Template caching performance optimization preserved
- **Description**: Verify template cache reduces Jinja2 compilation overhead
- **Justification**: Unit level - test cache attribute directly on TemplateProcessor instance.
- **Test Cases**:
  - First render caches template in `_template_cache`
  - Second render with same template uses cached version
  - Cache keyed by template text
  - Cache isolated per TemplateProcessor instance

**TEA-PY-008.1-UNIT-007**
- **Priority**: P1 (core)
- **Requirement**: AC 16 - StrictUndefined error messages preserved
- **Description**: Verify helpful error messages when accessing undefined template variables
- **Justification**: Unit level - error handling doesn't require engine integration.
- **Test Cases**:
  - `{{ state.missing_key }}` raises UndefinedError with clear message
  - Error message includes variable name 'missing_key'
  - `{{ variables.undefined }}` raises UndefinedError
  - Nested undefined: `{{ state.obj.missing }}` provides path context

#### Parameter Processing (AC 7)

**TEA-PY-008.1-UNIT-008**
- **Priority**: P0 (critical)
- **Requirement**: AC 7 - `process_params(params, state)` method implemented
- **Description**: Verify recursive parameter processing replaces template variables in nested structures
- **Justification**: Core method. Unit level for isolated parameter dict testing.
- **Test Cases**:
  - Flat dict: `{"url": "{{ state.host }}"}` processes correctly
  - Nested dict: `{"config": {"key": "{{ state.value }}"}}` recurses
  - List values: `["{{ state.item1 }}", "{{ state.item2 }}"]` processes each
  - Mixed types: integers, bools, nulls pass through unchanged

**TEA-PY-008.1-UNIT-009**
- **Priority**: P1 (core)
- **Requirement**: AC 7 - process_params preserves checkpoint context
- **Description**: Verify process_params passes checkpoint_dir/last_checkpoint to nested templates
- **Justification**: Unit level - parameter passing logic.
- **Test Cases**:
  - Nested template with `{{ checkpoint.dir }}` in params dict
  - process_params() forwards checkpoint_dir parameter correctly

#### Condition Evaluation (AC 8)

**TEA-PY-008.1-UNIT-010**
- **Priority**: P0 (critical)
- **Requirement**: AC 8 - `evaluate_condition(expr, state)` method implemented
- **Description**: Verify condition expression evaluation returns boolean using Jinja2
- **Justification**: Critical for conditional edges. Unit level for pure logic testing.
- **Test Cases**:
  - Simple comparison: `state.count > 5` returns True/False
  - Boolean expression: `state.enabled and state.ready` evaluates correctly
  - String comparison: `state.status == 'complete'`
  - Complex expression: `state.count > 0 and state.status in ['pending', 'active']`
  - Falsy values: `state.count == 0` returns True when count is 0

**TEA-PY-008.1-UNIT-011**
- **Priority**: P2 (secondary)
- **Requirement**: AC 9 - `_convert_simple_expression(expr)` preserved for backward compatibility
- **Description**: Verify legacy expression conversion for old syntax `state['key']` to `state.key`
- **Justification**: Unit level - pure string transformation logic.
- **Test Cases**:
  - `state['count']` converts to `state.count`
  - `state['user']['name']` converts to `state.user.name`
  - Already converted expressions pass through unchanged
  - Private method (not in public API)

#### Constructor and Initialization (AC 4-5)

**TEA-PY-008.1-UNIT-012**
- **Priority**: P0 (critical)
- **Requirement**: AC 4-5 - TemplateProcessor class with engine reference pattern
- **Description**: Verify TemplateProcessor constructor accepts jinja_env, variables, secrets and stores references
- **Justification**: Unit level - constructor contract testing.
- **Test Cases**:
  - Constructor accepts Environment object
  - Constructor accepts variables dict
  - Constructor accepts secrets dict
  - Attributes `_jinja_env`, `_variables`, `_secrets` set correctly
  - `_template_cache` initialized as empty dict

**TEA-PY-008.1-UNIT-013**
- **Priority**: P1 (core)
- **Requirement**: AC 5 - Constructor parameter types validated
- **Description**: Verify TemplateProcessor validates constructor parameter types match type hints
- **Justification**: Unit level - input validation.
- **Test Cases**:
  - jinja_env must be Jinja2 Environment instance
  - variables can be dict or None
  - secrets can be dict or None
  - Type hints present: `Environment`, `Dict[str, Any]`, `Optional`

#### Error Handling and Edge Cases

**TEA-PY-008.1-UNIT-014**
- **Priority**: P1 (core)
- **Requirement**: AC 10-16 - Template error handling
- **Description**: Verify graceful error handling for malformed templates and runtime errors
- **Justification**: Unit level - isolated error scenarios.
- **Test Cases**:
  - Syntax error in template: `{{ state.count }` (missing `}}`) raises TemplateSyntaxError
  - Type error: `{{ state.count | upper }}` (filter on wrong type) provides clear error
  - Null state handling: templates work when state values are None
  - Empty template string returns empty result

**TEA-PY-008.1-UNIT-015**
- **Priority**: P2 (secondary)
- **Requirement**: AC 15 - Cache invalidation and memory management
- **Description**: Verify template cache doesn't grow unbounded or leak across instances
- **Justification**: Unit level - cache lifecycle testing.
- **Test Cases**:
  - Cache isolated per TemplateProcessor instance
  - Creating new TemplateProcessor creates new cache
  - Cache size grows with unique templates (no automatic eviction)
  - Same template text caches once (keyed by content)

#### DotDict Helper Class

**TEA-PY-008.1-UNIT-016**
- **Priority**: P1 (core)
- **Requirement**: AC 10 - DotDict moved to yaml_templates module
- **Description**: Verify DotDict class provides attribute-style dict access for template contexts
- **Justification**: Unit level - utility class with no external dependencies.
- **Test Cases**:
  - `DotDict({'key': 'value'}).key` returns 'value'
  - Nested dicts auto-wrap: `DotDict({'obj': {'nested': 1}}).obj.nested` works
  - Missing key raises AttributeError with clear message
  - `__setattr__` allows `d.key = 'val'` assignment

**TEA-PY-008.1-UNIT-017**
- **Priority**: P2 (secondary)
- **Requirement**: AC 1-3 - Module structure and documentation
- **Description**: Verify yaml_templates.py module has proper structure and docstrings
- **Justification**: Unit level - static code analysis.
- **Test Cases**:
  - Module docstring present with usage examples
  - TemplateProcessor class docstring explains purpose
  - All public methods have docstrings
  - Module under 300 lines (AC 3)
  - Required imports: re, json, typing, jinja2

**TEA-PY-008.1-UNIT-018**
- **Priority**: P2 (secondary)
- **Requirement**: AC 25 - Type hints on all methods
- **Description**: Verify comprehensive type annotations on TemplateProcessor methods
- **Justification**: Unit level - static type checking with mypy.
- **Test Cases**:
  - `process_template()` has full type signature with return type `Any`
  - `process_params()` returns `Dict[str, Any]`
  - `evaluate_condition()` returns `bool`
  - Constructor parameters annotated: `Environment`, `Dict`, `Optional`
  - No `# type: ignore` comments without justification

---

### INTEGRATION LEVEL - YAMLEngine Integration (8 tests)

#### Delegation Pattern (AC 17-21)

**TEA-PY-008.1-INTEGRATION-001**
- **Priority**: P0 (critical)
- **Requirement**: AC 17-18 - YAMLEngine._template_processor attribute and delegation
- **Description**: Verify YAMLEngine creates TemplateProcessor instance and delegates _process_template()
- **Justification**: Integration level - tests cross-module interaction between YAMLEngine and TemplateProcessor.
- **Test Cases**:
  - YAMLEngine.__init__() creates _template_processor instance
  - _template_processor initialized with YAMLEngine's jinja_env
  - _template_processor receives variables and secrets from YAMLEngine
  - YAMLEngine._process_template() calls _template_processor.process_template()
  - Return value from processor passed through unchanged

**TEA-PY-008.1-INTEGRATION-002**
- **Priority**: P0 (critical)
- **Requirement**: AC 19 - YAMLEngine._process_params() delegates to processor
- **Description**: Verify parameter processing delegation preserves behavior
- **Justification**: Integration level - parameter flow through YAMLEngine to TemplateProcessor.
- **Test Cases**:
  - YAMLEngine._process_params() calls _template_processor.process_params()
  - State and params passed correctly to processor
  - Checkpoint context forwarded (checkpoint_dir, last_checkpoint)
  - Processed params returned and used in action functions

**TEA-PY-008.1-INTEGRATION-003**
- **Priority**: P0 (critical)
- **Requirement**: AC 20 - YAMLEngine._evaluate_condition() delegates to processor
- **Description**: Verify condition evaluation delegation for conditional edges
- **Justification**: Integration level - condition evaluation affects edge traversal logic.
- **Test Cases**:
  - YAMLEngine._evaluate_condition() calls _template_processor.evaluate_condition()
  - State passed to processor correctly
  - Boolean result determines edge selection
  - Conditional edge workflows execute correctly

**TEA-PY-008.1-INTEGRATION-004**
- **Priority**: P1 (core)
- **Requirement**: AC 21 - Jinja2 environment stays in YAMLEngine
- **Description**: Verify Jinja2 environment shared initialization remains in YAMLEngine
- **Justification**: Integration level - shared resource management between modules.
- **Test Cases**:
  - Jinja2 Environment created once in YAMLEngine.__init__()
  - Same Environment instance passed to TemplateProcessor
  - Custom filters registered in YAMLEngine available in TemplateProcessor
  - StrictUndefined configuration from YAMLEngine propagates

#### Checkpoint Context Passing

**TEA-PY-008.1-INTEGRATION-005**
- **Priority**: P1 (core)
- **Requirement**: AC 12 - Checkpoint context in template processing
- **Description**: Verify checkpoint_dir and last_checkpoint flow from YAMLEngine to TemplateProcessor
- **Justification**: Integration level - tests data flow across module boundary with YAMLEngine state.
- **Test Cases**:
  - YAMLEngine sets _checkpoint_dir attribute
  - _process_template() passes checkpoint_dir to processor
  - _process_params() passes checkpoint context to processor
  - Templates in YAML with `{{ checkpoint.dir }}` render correctly
  - Checkpoint context updates when YAMLEngine checkpoint changes

**TEA-PY-008.1-INTEGRATION-006**
- **Priority**: P1 (core)
- **Requirement**: AC 10-16 - Template features in YAML workflow context
- **Description**: Verify all template features work end-to-end through YAMLEngine.load_from_dict()
- **Justification**: Integration level - templates processed during YAML parsing and execution.
- **Test Cases**:
  - YAML node with `run: "{{ state.value }}"` executes correctly
  - Action params with templates: `url: "{{ variables.api_url }}"` process
  - Conditional edges with `{{ state.count > 5 }}` evaluate
  - Single expression object passthrough in action params
  - Jinja2 filters work in YAML templates

**TEA-PY-008.1-INTEGRATION-007**
- **Priority**: P2 (secondary)
- **Requirement**: AC 26 - No circular imports
- **Description**: Verify import structure prevents circular dependencies
- **Justification**: Integration level - import relationships tested during module loading.
- **Test Cases**:
  - `from the_edge_agent.yaml_templates import TemplateProcessor` succeeds
  - `from the_edge_agent.yaml_engine import YAMLEngine` succeeds
  - yaml_engine.py imports from yaml_templates.py (one-way dependency)
  - No circular import errors when loading both modules
  - DotDict importable from yaml_templates without circular dependency

**TEA-PY-008.1-INTEGRATION-008**
- **Priority**: P1 (core)
- **Requirement**: AC 23 - No public API changes to YAMLEngine
- **Description**: Verify YAMLEngine public API remains unchanged after refactoring
- **Justification**: Integration level - tests backward compatibility at API boundary.
- **Test Cases**:
  - YAMLEngine constructor signature unchanged
  - load_from_dict() signature unchanged
  - load_from_file() signature unchanged
  - load_from_string() signature unchanged
  - Private methods (_process_template, etc.) can change implementation

---

### E2E LEVEL - Full Workflow Validation (2 tests)

**TEA-PY-008.1-E2E-001**
- **Priority**: P0 (critical)
- **Requirement**: AC 22 - All existing tests pass without modification
- **Description**: Run full regression test suite to verify zero behavioral changes
- **Justification**: E2E level - validates entire system behavior unchanged across all 320+ existing tests.
- **Test Cases**:
  - `pytest tests/test_yaml_engine*.py` passes 100%
  - No test modifications required
  - All template-related tests pass (state, variables, secrets, filters)
  - Checkpoint-related tests pass
  - Performance tests show no regression

**TEA-PY-008.1-E2E-002**
- **Priority**: P1 (core)
- **Requirement**: AC 24 - Template behavior identical before/after extraction
- **Description**: Execute representative YAML workflows to validate end-to-end template functionality
- **Justification**: E2E level - real-world usage scenarios across module boundary.
- **Test Cases**:
  - Complex YAML with nested templates in multiple nodes
  - Workflow with conditional edges using template expressions
  - Parallel fan-out with templated parameters
  - Interrupt/resume workflow with checkpoint templates
  - LLM action with templated prompt and parameters

---

## Test Execution Strategy

### Phase 1: Pre-Refactoring Baseline
1. Run full test suite: `cd python && pytest tests/test_yaml_engine*.py -v`
2. Document baseline: All tests passing, record count (320+)
3. Capture template-specific test list for focused regression

### Phase 2: Unit Test Development
1. Create `tests/test_yaml_templates.py` for new TemplateProcessor tests
2. Implement P0 unit tests first (UNIT-001, 002, 005, 010, 012)
3. Run isolated: `pytest tests/test_yaml_templates.py -v`
4. Ensure 100% unit test pass before refactoring

### Phase 3: Refactoring Execution
1. Extract template methods to yaml_templates.py module
2. Update YAMLEngine delegation
3. Run unit tests continuously during refactoring

### Phase 4: Integration Validation
1. Run integration tests (INTEGRATION-001 through 008)
2. Verify delegation and context passing
3. Check for circular imports

### Phase 5: Full Regression (E2E)
1. Run complete test suite: `pytest tests/test_yaml_engine*.py`
2. Compare results to baseline (Phase 1)
3. Investigate any failures - should be ZERO

### Phase 6: Quality Gates
1. Type checking: `mypy python/src/the_edge_agent/yaml_templates.py`
2. Line count check: `wc -l yaml_templates.py` (must be < 300)
3. Import verification: `python -c "from the_edge_agent import YAMLEngine, yaml_templates"`

---

## Risk-Based Test Prioritization

### Critical Path (Must Pass - P0)
These tests guard against breaking changes:
- UNIT-001, 002, 005, 010, 012 (core template functionality)
- INTEGRATION-001, 002, 003 (delegation contract)
- E2E-001 (full regression)

**Failure Impact**: System unusable, all YAML workflows broken
**Execution Time**: ~5 minutes
**Run Frequency**: Every commit

### Core Functionality (P1)
Ensures feature completeness:
- UNIT-003, 004, 006, 007, 008, 009 (template features)
- INTEGRATION-004, 005, 006, 008 (integration points)
- E2E-002 (workflow validation)

**Failure Impact**: Feature gaps, degraded functionality
**Execution Time**: ~10 minutes
**Run Frequency**: Pre-merge

### Secondary/Quality (P2)
Code quality and edge cases:
- UNIT-011, 014, 015, 017, 018 (edge cases, quality)
- INTEGRATION-007 (import structure)

**Failure Impact**: Technical debt, edge case bugs
**Execution Time**: ~3 minutes
**Run Frequency**: Pre-release

---

## Test Data Requirements

### Minimal Test Fixtures

**TemplateProcessor Unit Tests:**
```python
# Fixture: mock_jinja_env
from jinja2 import Environment, StrictUndefined
env = Environment(undefined=StrictUndefined)
env.filters['fromjson'] = json.loads

# Fixture: sample_state
{"count": 42, "user": {"name": "Alice"}, "enabled": True}

# Fixture: sample_variables
{"api_url": "https://api.example.com", "timeout": 30}

# Fixture: sample_secrets
{"api_key": "secret-key-123", "token": "bearer-token"}
```

**Integration Tests:**
```yaml
# minimal_template_workflow.yaml
name: template-test
nodes:
  - name: process
    run: |
      return {"result": f"Processed {state['input']}"}
    params:
      url: "{{ variables.api_url }}/endpoint"
edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
    condition: "{{ state.result != None }}"
```

---

## Coverage Requirements

### Unit Test Coverage
- **Target**: 100% line coverage for yaml_templates.py
- **Tools**: `pytest --cov=the_edge_agent.yaml_templates --cov-report=html`
- **Critical paths**: All public methods, error handling branches

### Integration Test Coverage
- **Target**: 90% branch coverage for delegation paths in YAMLEngine
- **Focus**: _process_template(), _process_params(), _evaluate_condition() delegation

### Mutation Testing (Optional)
- **Tool**: `mutmut run --paths-to-mutate=python/src/the_edge_agent/yaml_templates.py`
- **Target**: 80% mutation score
- **Purpose**: Verify test quality catches logic errors

---

## Test Automation and CI

### GitHub Actions Integration
```yaml
# .github/workflows/python-tests.yaml (existing)
# Add specific job for template module

- name: Test Template Module (Isolated)
  run: |
    cd python
    pytest tests/test_yaml_templates.py -v --cov=the_edge_agent.yaml_templates

- name: Full Regression Suite
  run: |
    cd python
    pytest tests/test_yaml_engine*.py -v
```

### Pre-Commit Hooks
```bash
# Add to .pre-commit-config.yaml
- id: yaml-templates-tests
  name: YAML Templates Unit Tests
  entry: bash -c 'cd python && pytest tests/test_yaml_templates.py -q'
  language: system
  pass_filenames: false
```

---

## Acceptance Criteria Coverage Matrix

| AC | Description | Test IDs | Level | Priority |
|----|-------------|----------|-------|----------|
| 1 | yaml_templates.py created | UNIT-017 | Unit | P2 |
| 2 | Comprehensive docstring | UNIT-017 | Unit | P2 |
| 3 | Module under 300 lines | UNIT-017 | Unit | P2 |
| 4 | TemplateProcessor class | UNIT-012 | Unit | P0 |
| 5 | Constructor signature | UNIT-012, 013 | Unit | P0, P1 |
| 6 | process_template() method | UNIT-001, 002, 003, 004, 005, 006, 007, 014 | Unit | P0, P1 |
| 7 | process_params() method | UNIT-008, 009 | Unit | P0, P1 |
| 8 | evaluate_condition() method | UNIT-010 | Unit | P0 |
| 9 | _convert_simple_expression() | UNIT-011 | Unit | P2 |
| 10 | {{ state.key }} access | UNIT-001, INTEGRATION-006 | Unit, Integration | P0, P1 |
| 11 | {{ variables/secrets }} | UNIT-002, INTEGRATION-006 | Unit, Integration | P0, P1 |
| 12 | {{ checkpoint }} access | UNIT-003, INTEGRATION-005 | Unit, Integration | P1 |
| 13 | Jinja2 filters | UNIT-004, INTEGRATION-006 | Unit, Integration | P1 |
| 14 | Object passthrough | UNIT-005, INTEGRATION-006 | Unit, Integration | P0, P1 |
| 15 | Template caching | UNIT-006, 015 | Unit | P1, P2 |
| 16 | StrictUndefined errors | UNIT-007 | Unit | P1 |
| 17 | _template_processor attr | INTEGRATION-001 | Integration | P0 |
| 18 | Delegation pattern | INTEGRATION-001, 002, 003 | Integration | P0 |
| 19 | _process_params delegate | INTEGRATION-002 | Integration | P0 |
| 20 | _evaluate_condition delegate | INTEGRATION-003 | Integration | P0 |
| 21 | Jinja env in YAMLEngine | INTEGRATION-004 | Integration | P1 |
| 22 | All tests pass | E2E-001 | E2E | P0 |
| 23 | No public API changes | INTEGRATION-008 | Integration | P1 |
| 24 | Identical behavior | E2E-001, E2E-002 | E2E | P0, P1 |
| 25 | Type hints | UNIT-018 | Unit | P2 |
| 26 | No circular imports | INTEGRATION-007 | Integration | P2 |
| 27 | Comprehensive docstrings | UNIT-017 | Unit | P2 |

**Coverage**: 27/27 ACs covered (100%)

---

## Potential Test Gaps and Mitigations

### Identified Gaps

1. **Performance Regression**
   - **Gap**: No explicit performance benchmarks for template rendering
   - **Risk**: Refactoring could introduce overhead in hot path
   - **Mitigation**: Add optional benchmark test comparing before/after render times
   - **Test**: UNIT-019 (optional) - Template rendering performance baseline

2. **Thread Safety**
   - **Gap**: TemplateProcessor instance shared across YAMLEngine workflow executions
   - **Risk**: Template cache race conditions in parallel workflows
   - **Mitigation**: Review cache implementation, ensure thread-local or immutable
   - **Test**: INTEGRATION-009 (optional) - Concurrent template processing

3. **Memory Leaks**
   - **Gap**: Template cache unbounded growth over long-running processes
   - **Risk**: Memory exhaustion in long-lived YAMLEngine instances
   - **Mitigation**: Document cache lifecycle, consider LRU eviction in future
   - **Test**: UNIT-015 covers basic isolation, long-running test needed for leak detection

### Gap Prioritization
- Performance: **P1** (add if regression detected in E2E-001)
- Thread Safety: **P2** (defer to future story if parallel workflows not yet supported)
- Memory Leaks: **P3** (document limitation, address in future optimization story)

---

## Test Maintenance Strategy

### Test Ownership
- **Unit tests** (`test_yaml_templates.py`): New file, owned by this story
- **Integration tests**: Add to existing `test_yaml_engine_core.py`
- **E2E regression**: Existing tests in `test_yaml_engine*.py` (no changes)

### Test Longevity
- **Short-term** (this story): Ensure 100% pass rate for merge
- **Medium-term** (next 3 stories): Unit tests remain stable as template features expand
- **Long-term**: Integration tests may need updates if YAMLEngine refactored further

### Regression Detection
If E2E-001 fails after refactoring:
1. Bisect failure to specific test case
2. Add equivalent unit/integration test to catch earlier
3. Root cause: delegation bug, state mutation, or context loss

---

## Definition of Done (Testing Perspective)

- [ ] All 18 unit tests implemented and passing (100% for yaml_templates.py)
- [ ] All 8 integration tests passing (delegation verified)
- [ ] E2E-001 regression suite passes (320+ tests, zero modifications)
- [ ] E2E-002 workflow validation passes
- [ ] Coverage: 100% line coverage for yaml_templates.py
- [ ] Type checking: `mypy` passes with no errors
- [ ] No circular imports detected
- [ ] Module size verified under 300 lines
- [ ] Documentation: All test scenarios documented in this file

---

## Appendix: Test Case Examples

### Example Unit Test (UNIT-001)

```python
# tests/test_yaml_templates.py
import pytest
from jinja2 import Environment, StrictUndefined
from the_edge_agent.yaml_templates import TemplateProcessor, DotDict

def test_process_template_state_access():
    """TEA-PY-008.1-UNIT-001: Verify {{ state.key }} variable access"""
    env = Environment(undefined=StrictUndefined)
    processor = TemplateProcessor(env, {}, {})

    state = {"count": 42, "user": {"name": "Alice"}}

    # Simple state access
    result = processor.process_template("{{ state.count }}", state)
    assert result == 42  # Object passthrough for single expression

    # Nested state access
    result = processor.process_template("{{ state.user.name }}", state)
    assert result == "Alice"

    # Mixed template (returns string)
    result = processor.process_template("Count is {{ state.count }}", state)
    assert result == "Count is 42"
```

### Example Integration Test (INTEGRATION-001)

```python
# tests/test_yaml_engine_core.py (add to existing file)
def test_yaml_engine_template_processor_delegation():
    """TEA-PY-008.1-INTEGRATION-001: Verify YAMLEngine delegates to TemplateProcessor"""
    engine = YAMLEngine(
        config={
            "name": "test",
            "nodes": [{"name": "start", "run": "return {}"}],
            "edges": [{"from": "__start__", "to": "start"}]
        },
        variables={"api_url": "https://api.test.com"}
    )

    # Verify TemplateProcessor created
    assert hasattr(engine, '_template_processor')
    assert engine._template_processor is not None

    # Verify delegation works
    state = {"value": 123}
    result = engine._process_template("{{ state.value }}", state)
    assert result == 123

    # Verify variables passed to processor
    result = engine._process_template("{{ variables.api_url }}", state)
    assert result == "https://api.test.com"
```

---

**End of Test Design Document**

**Next Steps**:
1. Review with Dev team for implementation feasibility
2. Create test stubs in `tests/test_yaml_templates.py`
3. Execute Phase 1: Pre-refactoring baseline
4. Proceed with test-driven refactoring per execution strategy
