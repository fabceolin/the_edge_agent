# Data Processing Actions

> **Parent document:** [Actions Overview](./README.md)
> **Related:** [Node Specification](../nodes.md)
> **Epic:** DOC-002 (YAML Reference Modularization)

## Overview

Data processing actions handle JSON/CSV parsing, transformation, validation, filtering, and code execution. Includes tabular data operations with hybrid storage.

---

## Table of Contents

- [JSON Actions](#json-actions)
  - [json.parse](#jsonparse)
  - [json.transform](#jsontransform)
  - [json.stringify](#jsonstringify)
- [CSV Actions](#csv-actions)
  - [csv.parse](#csvparse)
  - [csv.stringify](#csvstringify)
- [Data Actions](#data-actions)
  - [data.validate](#datavalidate)
  - [data.merge](#datamerge)
  - [data.filter](#datafilter)
- [Code Execution Actions](#code-execution-actions)
  - [code.execute](#codeexecute)
  - [code.sandbox](#codesandbox)
- [Tabular Data Actions](#tabular-data-actions)
  - [data.create_table](#datacreate_table)
  - [data.insert](#datainsert)
  - [data.update](#dataupdate)
  - [data.delete](#datadelete)
  - [data.query](#dataquery)
  - [data.consolidate](#dataconsolidate)

---

## JSON Actions

### `json.parse`

Parse JSON string to Python object:

```yaml
- name: parse_response
  uses: json.parse
  with:
    text: "{{ state.raw_response }}"  # Required
    strict: true                       # Optional (default: true)
    default: {}                        # Optional fallback (requires strict: false)
  output: parsed_data
```

**Returns:**
- Success: `{"data": any, "success": true}`
- Failure: `{"error": str, "success": false, "error_type": "parse", "position": {"line": int, "column": int}}`

### `json.transform`

Transform data with JMESPath or JSONPath expressions:

```yaml
- name: extract_users
  uses: json.transform
  with:
    data: "{{ state.api_response }}"                          # Required
    expression: "users[?status=='active'].{name: name, email: email}"  # Required
    engine: jmespath                                           # Optional: "jmespath" or "jsonpath"
  output: transformed_data
```

**Common JMESPath expressions:**
- `user.profile.name` - Extract nested value
- `users[?status=='active']` - Filter array
- `users[*].name` - Extract all names
- `{names: users[].name, count: length(users)}` - Project new structure

**Returns:** `{"result": any, "expression": str, "success": true}`

### `json.stringify`

Convert Python object to JSON string:

```yaml
- name: serialize
  uses: json.stringify
  with:
    data: "{{ state.result }}"    # Required
    indent: 2                      # Optional
    sort_keys: true                # Optional
  output: json_string
```

**Returns:** `{"text": str, "success": true}`

---

## CSV Actions

### `csv.parse`

Parse CSV from text or file:

```yaml
- name: parse_csv
  uses: csv.parse
  with:
    text: "{{ state.csv_content }}"  # Required (or use path)
    # path: ./data/input.csv         # Alternative: read from file
    delimiter: ","                    # Optional (default: ",")
    has_header: true                  # Optional (default: true)
  output: csv_data
```

**Returns:**
- With header: `{"data": [{"col1": "val1", ...}], "headers": ["col1", ...], "row_count": int, "success": true}`
- Without header: `{"data": [["val1", "val2"]], "headers": null, "row_count": int, "success": true}`

### `csv.stringify`

Convert list to CSV string:

```yaml
- name: export_csv
  uses: csv.stringify
  with:
    data: "{{ state.records }}"           # Required
    headers: ["name", "email", "status"]  # Optional (auto-detected from dicts)
    delimiter: ","                         # Optional
  output: csv_text
```

**Returns:** `{"text": str, "row_count": int, "success": true}`

---

## Data Actions

### `data.validate`

Validate data against JSON Schema:

```yaml
- name: validate_input
  uses: data.validate
  with:
    data: "{{ state.user_input }}"        # Required
    schema:                                # Required
      type: object
      properties:
        name:
          type: string
          minLength: 1
        email:
          type: string
          format: email
      required: ["name", "email"]
  output: validation_result
```

**Returns:**
- Valid: `{"valid": true, "errors": [], "success": true}`
- Invalid: `{"valid": false, "errors": [{"path": str, "message": str}], "success": true}`

### `data.merge`

Merge multiple dictionaries:

```yaml
- name: combine_configs
  uses: data.merge
  with:
    sources:                              # Required
      - "{{ state.default_config }}"
      - "{{ state.user_config }}"
      - "{{ state.override_config }}"
    strategy: deep                        # Optional: "deep", "shallow", "replace"
  output: merged_config
```

**Strategies:**
- `deep`: Recursively merge nested dictionaries
- `shallow`: Only merge top-level keys
- `replace`: Later sources completely replace earlier ones

**Returns:** `{"result": dict, "source_count": int, "success": true}`

### `data.filter`

Filter list items with predicates:

```yaml
- name: filter_users
  uses: data.filter
  with:
    data: "{{ state.users }}"             # Required
    predicate:                             # Required
      field: status
      op: eq
      value: active
  output: filtered_users

# Multiple predicates (AND logic)
- name: filter_premium
  uses: data.filter
  with:
    data: "{{ state.users }}"
    predicate:
      - field: status
        op: eq
        value: active
      - field: subscription
        op: in
        value: ["premium", "enterprise"]
```

**Operators:** `eq`, `ne`, `gt`, `gte`, `lt`, `lte`, `in`, `not_in`, `contains`, `startswith`, `endswith`

**Returns:** `{"result": list, "original_count": int, "filtered_count": int, "success": true}`

---

## Code Execution Actions

> **Security Warning:** Code execution is DISABLED by default. Enable with `YAMLEngine(enable_code_execution=True)`.
> Uses RestrictedPython sandbox - not suitable for arbitrary untrusted code.

**Required:** `pip install RestrictedPython`

### `code.execute`

Execute Python code in sandboxed environment:

```yaml
- name: compute
  uses: code.execute
  with:
    code: |                               # Required
      x = 1 + 2
      y = x * 10
      result = y  # Set 'result' to return a value
    timeout: 30                           # Optional (default: 30 seconds)
    max_output_bytes: 65536               # Optional (default: 64KB)
  output: execution_result
```

**Returns:**
- Success: `{"success": true, "stdout": str, "stderr": str, "return_value": any, "execution_time_ms": float}`
- Failure: `{"success": false, "error": str, "stdout": "", "stderr": "", "return_value": null}`

**Allowed:** Math, types, iteration, list/dict operations, try/except
**Blocked:** imports, file access, network, exec/eval, dangerous dunders

### `code.sandbox`

Manage persistent sandbox sessions:

```yaml
# Create session
- name: create_session
  uses: code.sandbox
  with:
    action: create
  output: sandbox_info

# Execute in session (variables persist)
- name: run_code
  uses: code.sandbox
  with:
    action: execute
    sandbox_id: "{{ state.sandbox_info.sandbox_id }}"
    code: |
      counter += 1
      result = counter

# Destroy session
- name: cleanup
  uses: code.sandbox
  with:
    action: destroy
    sandbox_id: "{{ state.sandbox_info.sandbox_id }}"
```

**Actions:** `create`, `execute`, `list`, `destroy`

---

## Tabular Data Actions

Hybrid storage for structured tabular data using DuckLake catalog.

**Required:** `pip install duckdb`

**Storage Strategy:**
- **Inline** (<1KB): Small batches stored in metadata store for low-latency access
- **Parquet** (>=1KB): Large batches stored as Parquet files in blob storage
- **LWW Merge**: Queries merge both sources, keeping highest `_version` per primary key

### `data.create_table`

Create table with schema and primary key:

```yaml
- name: create_scores_table
  uses: data.create_table
  with:
    name: "firm_scores"                       # Required
    schema:                                   # Required
      firm_id: "string"
      score: "float"
      category: "string"
    primary_key:                              # Required
      - "firm_id"
  output: table_result
```

**Schema types:** `string`, `integer`, `float`, `boolean`, `timestamp`, `json`

**Returns:** `{"success": true, "table": str, "schema": dict}`

### `data.insert`

Insert rows (auto-selects inline vs Parquet based on size):

```yaml
- name: insert_scores
  uses: data.insert
  with:
    table: "firm_scores"                      # Required
    rows:                                     # Required
      - firm_id: "f1"
        score: 85.5
        category: "A"
      - firm_id: "f2"
        score: 92.0
        category: "A+"
  output: insert_result
```

**Returns:** `{"success": true, "table": str, "row_count": int, "storage": "inlined"|"parquet"}`

### `data.update`

Update rows by primary key (append-only versioning):

```yaml
- name: update_score
  uses: data.update
  with:
    table: "firm_scores"                      # Required
    where:                                    # Required (must include PK)
      firm_id: "f1"
    updates:                                  # Required
      score: 90.0
      category: "A+"
  output: update_result
```

**Returns:** `{"success": true, "table": str, "status": "updated", "row_count": int}`

### `data.delete`

Delete rows by primary key (creates tombstone):

```yaml
- name: delete_firm
  uses: data.delete
  with:
    table: "firm_scores"                      # Required
    where:                                    # Required (must include PK)
      firm_id: "f2"
  output: delete_result
```

**Returns:** `{"success": true, "table": str, "status": "deleted", "row_count": int}`

### `data.query`

SQL query with Last-Write-Wins merge:

```yaml
- name: query_top_scores
  uses: data.query
  with:
    table: "firm_scores"                      # Required
    sql: "SELECT * FROM data WHERE score > 80 ORDER BY score DESC"  # Required
  output: query_result
```

**Note:** Table is aliased as `data` in SQL queries.

**Returns:** `{"success": true, "table": str, "rows": list, "row_count": int}`

### `data.consolidate`

Compact inlined rows into Parquet files:

```yaml
- name: consolidate_scores
  uses: data.consolidate
  with:
    table: "firm_scores"                      # Required
  output: consolidate_result
```

**Returns:** `{"success": true, "table": str, "status": "consolidated", "parquet_path": str}`

---

## Dual Namespace

All code actions are available via dual namespaces: `code.*` and `actions.code_*`.
All tabular data actions use dual namespaces: `data.*` and `actions.data_*`.

---

## See Also

- [Actions Overview](./README.md)
- [LLM Actions](./llm.md)
- [I/O Actions](./io.md)
- [Memory Actions](./memory.md) - Persistent storage
