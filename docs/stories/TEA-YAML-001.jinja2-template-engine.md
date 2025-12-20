# Story TEA-YAML-001: Jinja2 Template Engine

## Status

Draft

## Story

**As a** YAML agent developer,
**I want** to use Jinja2 templates instead of Python eval(),
**so that** I can write declarative workflows with familiar syntax, use conditionals/loops when needed, and have a path toward edge computing portability.

## Context & Motivation

### Current State
The YAMLEngine uses Python's `eval()` for template processing (`{{ state.key }}`). While functional, this approach:
- Ties the engine to Python runtime, limiting edge computing portability
- Uses `eval()` which is powerful but less structured than a proper template engine
- Lacks built-in conditionals and loops for complex formatting

### Why Jinja2?
1. **Familiar syntax**: Widely known by developers (Flask, Ansible, dbt)
2. **Portable**: MiniJinja (Rust), Tera (Rust), Jinja2cpp exist for other runtimes
3. **Powerful**: Built-in conditionals, loops, filters
4. **Retrocompatible**: `{{ state.key }}` syntax remains unchanged
5. **Already solves GHA parity**: Native operators and filters cover most needs

### Jinja2 Native Capabilities

| GitHub Actions Style | Jinja2 Native Equivalent |
|---------------------|--------------------------|
| `contains(s, v)` | `'v' in s` |
| `startsWith(s, p)` | `s.startswith('p')` |
| `endsWith(s, x)` | `s.endswith('x')` |
| `join(arr, sep)` | `arr \| join(sep)` |
| `toJSON(v)` | `v \| tojson` |
| `len(c)` | `c \| length` |
| `format('{0}', a)` | `'%s' % a` or inline `{{ a }}` |

Only `fromJSON` needs to be added as a custom filter.

## Acceptance Criteria

1. Template processing uses Jinja2 instead of Python `eval()` for string interpolation
2. Custom filter `fromjson` added for JSON parsing
3. Standard Jinja2 constructs work in templates:
   - `{% if %}...{% endif %}` conditionals
   - `{% for %}...{% endfor %}` loops
   - `{{ value | filter }}` filter syntax
4. Template variables accessible: `state`, `variables`, `secrets`, `checkpoint`
5. Single-expression templates return native Python objects (not strings):
   - `"{{ state.data }}"` returns the actual dict/list, not string representation
6. `run:` inline Python code continues to work (escape hatch)
7. `StrictUndefined` mode catches undefined variable access with helpful errors
8. Performance: Template compilation cached for repeated use
9. All existing tests pass (backward compatibility)
10. Documentation updated with Jinja2 examples and native equivalents table

## Dependencies

**Blocked By:** None (can start immediately)

**Blocks:**
- Future DSL-only mode (disable `run:` for security)
- Edge runtime portability work

**External Dependencies:**
- `jinja2` package (add to setup.py dependencies)

**Internal Dependencies:**
- `YAMLEngine._process_template()` - primary refactor target
- `YAMLEngine._process_params()` - uses _process_template
- `YAMLEngine._add_edge_from_config()` - condition evaluation

## Tasks / Subtasks

### Task 1: Add Jinja2 Dependency (AC: 10)
- [ ] Add `jinja2>=3.0` to `setup.py` install_requires
- [ ] Verify compatibility with existing dependencies

### Task 2: Initialize Jinja2 Environment (AC: 1, 2, 7, 8)
- [ ] Create Jinja2 Environment in `YAMLEngine.__init__()`:
  ```python
  from jinja2 import Environment, BaseLoader, StrictUndefined

  self._jinja_env = Environment(
      loader=BaseLoader(),
      undefined=StrictUndefined,
      keep_trailing_newline=True,
  )
  # Only custom filter needed
  self._jinja_env.filters['fromjson'] = json.loads
  ```
- [ ] Implement template caching (LRU cache by template string hash)

### Task 3: Refactor _process_template() (AC: 1, 3, 4, 5)
- [ ] Replace `eval()` with Jinja2 template rendering
- [ ] Build render context:
  ```python
  context = {
      'state': DotDict(state),
      'variables': DotDict(self.variables),
      'secrets': DotDict(self.secrets),
      'checkpoint': DotDict({
          'dir': self._checkpoint_dir or '',
          'last': self._last_checkpoint_path or ''
      }),
  }
  ```
- [ ] Handle single-expression case for object passthrough:
  - Detect `{{ expr }}` without surrounding text
  - Use `compile_expression()` to return native object
- [ ] Preserve GitLab CI `${ }` syntax support
- [ ] Handle template errors gracefully with helpful messages

### Task 4: Update Condition Evaluation (AC: 1, 3)
- [ ] Refactor `_add_edge_from_config()` condition handling
- [ ] Use Jinja2 for `when:` expression evaluation
- [ ] Remove `_convert_simple_expression()` if no longer needed

### Task 5: Maintain Backward Compatibility (AC: 6, 9)
- [ ] Ensure `run:` inline code still uses `exec()`:
  - Jinja2 for template interpolation within code
  - `exec()` for executing the code itself
- [ ] Test all existing YAML examples still work
- [ ] Verify existing filters (`| json`, `| upper`, `| lower`) map to Jinja2 equivalents

### Task 6: Write Tests (AC: 9)
- [ ] Test basic interpolation: `{{ state.key }}`
- [ ] Test Jinja2 operators: `in`, `.startswith()`, `| length`
- [ ] Test Jinja2 conditionals: `{% if %}...{% endif %}`
- [ ] Test Jinja2 loops: `{% for %}...{% endfor %}`
- [ ] Test object passthrough (single expression returns native type)
- [ ] Test filter chaining: `{{ state.name | upper | truncate(10) }}`
- [ ] Test `fromjson` filter
- [ ] Test undefined variable error handling (StrictUndefined)
- [ ] Test `when:` edge conditions
- [ ] Test `run:` code with Jinja2 interpolation
- [ ] Performance benchmark vs current eval()

### Task 7: Update Documentation (AC: 10)
- [ ] Update `docs/YAML_REFERENCE.md`:
  - Add Jinja2 section under Template Syntax
  - Add GHA ↔ Jinja2 equivalents table
  - Document available filters (native + fromjson)
  - Add examples of conditionals/loops
- [ ] Update `CLAUDE.md` with Jinja2 mention

## Technical Notes

### Object Passthrough Logic

```python
def _process_template(self, text: str, state: Dict) -> Any:
    if not isinstance(text, str):
        return text

    text_stripped = text.strip()

    # Single expression: return native object
    single_expr = re.match(r'^\{\{\s*(.+?)\s*\}\}$', text_stripped)
    if single_expr and '{%' not in text_stripped:
        expr = single_expr.group(1)
        compiled = self._jinja_env.compile_expression(expr)
        return compiled(**context)

    # Multi-expression or mixed: render as string
    template = self._jinja_env.from_string(text)
    return template.render(**context)
```

### Example Usage After Implementation

```yaml
nodes:
  - name: process
    uses: http.get
    with:
      url: "{{ variables.api_url }}/users/{{ state.user_id }}"

  - name: check_priority
    run: |
      {% if 'urgent' in state.tags and state.priority | int > 5 %}
      return {"escalate": True}
      {% else %}
      return {"escalate": False}
      {% endif %}

  - name: format_report
    uses: template.render
    with:
      template: |
        Report for {{ state.user }}:
        {% for item in state.items[:5] %}
        - {{ item.name }}: {{ item.value | tojson }}
        {% endfor %}
        Total: {{ state.items | length }} items

edges:
  - from: router
    to: escalate
    when: "{{ 'urgent' in state.tags }}"

  - from: validate
    to: process
    when: "{{ state.data | length > 0 and state.status == 'ready' }}"
```

## Definition of Done

- [ ] All acceptance criteria met
- [ ] All tests passing (existing + new)
- [ ] Documentation updated
- [ ] No regression in existing YAML agent examples
- [ ] Performance within acceptable range of current implementation
- [ ] Code reviewed and approved

## Future Considerations

This story enables:
- **TEA-YAML-002**: DSL-only mode (disable `run:` for security/sandboxing)
- **TEA-YAML-003**: Declarative condition syntax (`when: {in: [state.tags, 'urgent']}`)
- **TEA-EDGE-001**: Non-Python runtime with MiniJinja or equivalent

---

## QA Results

**Reviewed by:** Quinn (Test Architect)
**Date:** 2024-12-19
**Test Design:** `docs/qa/assessments/TEA-YAML-001-test-design-20251219.md`

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total scenarios | 28 |
| Unit tests | 14 (50%) |
| Integration tests | 10 (36%) |
| E2E tests | 4 (14%) |
| P0 (Critical) | 8 |
| P1 (High) | 12 |
| P2 (Medium) | 8 |

### Key Risks Identified

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Breaking existing YAML agents | Medium | High | YAML-001-E2E-002, E2E-003 |
| Object passthrough regression | Medium | High | YAML-001-UNIT-013, UNIT-014, INT-007 |
| Performance degradation | Low | Medium | YAML-001-PERF-001 benchmark |
| StrictUndefined too strict | Low | Low | Document `| default` filter |

### Critical Tests (P0)

1. **YAML-001-UNIT-001** - Basic interpolation `{{ state.key }}`
2. **YAML-001-UNIT-008** - Conditionals `{% if %}...{% endif %}`
3. **YAML-001-UNIT-013** - Object passthrough returns dict
4. **YAML-001-UNIT-014** - Object passthrough returns list
5. **YAML-001-INT-001** - Architecture uses Jinja2 not eval()
6. **YAML-001-INT-003** - `state` accessible in templates
7. **YAML-001-INT-007** - Passthrough works in action `with:` params
8. **YAML-001-E2E-002** - All existing tests pass (regression)

### QA Recommendation

**Status:** ✅ **PASS** - Ready for development

**Notes:**
- Comprehensive test coverage designed for medium-risk refactor
- Backward compatibility tests prioritized as P0
- Recommend running existing test suite first after implementation
- Performance benchmark (P2) should be monitored but not blocking
