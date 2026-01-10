# Template Syntax

> **Parent document:** [YAML Reference](../YAML_REFERENCE.md)
> **Epic:** DOC-002 (YAML Reference Modularization)

## Overview

Templates use **Jinja2** (TEA-YAML-001), providing familiar syntax used in Flask, Ansible, and dbt. This allows variable interpolation, conditionals, loops, and filters within YAML configurations.

---

## Table of Contents

- [Basic Substitution](#basic-substitution)
- [Jinja2 Filters](#jinja2-filters)
- [Jinja2 Constructs](#jinja2-constructs)
  - [Conditionals](#conditionals)
  - [Loops](#loops)
- [GitHub Actions to Jinja2 Equivalents](#github-actions-to-jinja2-equivalents)
- [Object Passthrough](#object-passthrough)
- [Undefined Variable Handling](#undefined-variable-handling)
- [Security Note](#security-note)
- [Template in Different Contexts](#template-in-different-contexts)
- [Nested Access](#nested-access)

---

## Basic Substitution

| Syntax | Description | Example |
|--------|-------------|---------|
| `{{ state.key }}` | State value | `{{ state.user_name }}` |
| `{{ variables.key }}` | Global variable | `{{ variables.api_url }}` |
| `{{ secrets.key }}` | Secret value | `{{ secrets.api_key }}` |
| `{{ checkpoint.dir }}` | Checkpoint directory | `{{ checkpoint.dir }}/backup.pkl` |
| `{{ checkpoint.last }}` | Last checkpoint path | `{{ checkpoint.last }}` |
| `${ key }` | GitLab CI style | `${ CI_COMMIT_SHA }` |

---

## Jinja2 Filters

All standard Jinja2 filters are available, plus custom filters:

| Filter | Description | Example |
|--------|-------------|---------|
| `tojson` | JSON serialize | `{{ state.data \| tojson }}` |
| `json` | Alias for tojson | `{{ state.data \| json }}` |
| `fromjson` | Parse JSON string | `{{ state.json_str \| fromjson }}` |
| `upper` | Uppercase | `{{ state.name \| upper }}` |
| `lower` | Lowercase | `{{ state.name \| lower }}` |
| `length` | Get length | `{{ state.items \| length }}` |
| `default` | Default for undefined | `{{ state.missing \| default("N/A") }}` |
| `truncate` | Truncate string | `{{ state.text \| truncate(50) }}` |
| `join` | Join list items | `{{ state.tags \| join(", ") }}` |
| `first` | First item | `{{ state.items \| first }}` |
| `last` | Last item | `{{ state.items \| last }}` |
| `sha256` | Compute SHA256 hash | `{{ state.content \| sha256 }}` |

---

## Jinja2 Constructs

### Conditionals

```yaml
- name: format_message
  uses: template.render
  with:
    template: |
      {% if state.priority == 'high' %}
      URGENT: {{ state.message }}
      {% elif state.priority == 'medium' %}
      {{ state.message }}
      {% else %}
      {{ state.message }}
      {% endif %}
```

### Loops

```yaml
- name: format_report
  uses: template.render
  with:
    template: |
      Report Items:
      {% for item in state.items %}
      - {{ item.name }}: {{ item.value | tojson }}
      {% endfor %}
      Total: {{ state.items | length }} items
```

---

## GitHub Actions to Jinja2 Equivalents

| GitHub Actions Style | Jinja2 Native Equivalent |
|---------------------|--------------------------|
| `contains(s, v)` | `'v' in s` |
| `startsWith(s, p)` | `s.startswith('p')` |
| `endsWith(s, x)` | `s.endswith('x')` |
| `join(arr, sep)` | `arr \| join(sep)` |
| `toJSON(v)` | `v \| tojson` |
| `fromJSON(s)` | `s \| fromjson` |
| `len(c)` | `c \| length` |
| `format('{0}', a)` | `'%s' % a` or inline `{{ a }}` |

---

## Object Passthrough

When a template is a single expression (e.g., `"{{ state.data }}"`), it returns the actual Python object, not a string representation. This enables passing complex objects between actions:

```yaml
- name: get_data
  uses: http.get
  with:
    url: "{{ variables.api_url }}"
  output: response

- name: process
  uses: json.transform
  with:
    # data receives the actual dict, not a string
    data: "{{ state.response }}"
    expression: "items[*].name"
```

This is particularly useful when:
- Passing dictionaries or lists between actions
- Chaining action results without serialization overhead
- Working with structured data from API responses

---

## Undefined Variable Handling

Templates use `StrictUndefined` mode. Undefined variables:
- **Single-level access** (`{{ state.missing }}`) returns `None`
- **Nested access** (`{{ state.missing.deep }}`) raises `ValueError`
- Use `| default("fallback")` filter for graceful fallbacks:

```yaml
url: "{{ state.custom_url | default(variables.default_url) }}"
```

---

## Security Note

Template processing uses Jinja2's sandboxed environment. Unlike the old `eval()` approach:
- `__import__` and dangerous builtins are **blocked** in templates
- This improves security for template expressions
- `run:` blocks still use `exec()` with full Python access (by design)

---

## Template in Different Contexts

```yaml
variables:
  base_url: https://api.example.com

nodes:
  - name: example
    uses: http.post
    with:
      # Template in URL
      url: "{{ variables.base_url }}/users/{{ state.user_id }}"

      # Template in headers
      headers:
        Authorization: "Bearer {{ secrets.token }}"
        X-Request-ID: "{{ state.request_id }}"

      # Template in JSON body
      json:
        name: "{{ state.name | upper }}"
        data: "{{ state.payload | json }}"
```

---

## Nested Access

```yaml
# Access nested state values
{{ state.user.profile.name }}

# Access nested variables
{{ variables.config.timeout }}

# Combine with filters
{{ state.response.data.items | length }}
```

---

## See Also

- [Node Specification](./nodes.md) - Using templates in node configuration
- [Navigation & Flow](./navigation.md) - Templates in conditions
- [Actions Overview](./actions/README.md) - Action parameters with templates
