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
- [DuckDB WASM Actions (Browser)](#duckdb-wasm-actions-browser)
  - [duckdb.query](#duckdbquery)
  - [duckdb.execute](#duckdbexecute)
  - [Vector Similarity Search](#vector-similarity-search)
  - [Full-Text Search](#full-text-search)
  - [Parquet Support](#parquet-support)

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

## DuckDB WASM Actions (Browser)

> **Story:** TEA-WASM-003.2 - DuckDB WASM Integration
> **Platform:** Browser only (WebAssembly)

For browser-based analytics agents, DuckDB WASM provides SQL query capabilities with vector similarity search, full-text search, and parquet support directly in the browser.

**Required:** DuckDB handler must be registered via JavaScript before use.

### `duckdb.query`

Execute SQL query and return results:

```yaml
- name: fetch_users
  uses: duckdb.query
  with:
    sql: "SELECT * FROM users WHERE status = ?"   # Required
    params:                                        # Optional (for prepared statements)
      - "active"
  output: users
```

**Returns:**
- Success: Results stored in `state.users` (rows array)
- Also stores: `state.users_count` (row count), `state.users_schema` (column types)
- Failure: `state.users` = null, `state.users_error` = error message

**Template support:**

```yaml
- name: dynamic_query
  uses: duckdb.query
  with:
    sql: |
      SELECT *
      FROM {{ state.table_name }}
      WHERE category = ?
      LIMIT {{ state.limit | default(100) }}
    params:
      - "{{ state.selected_category }}"
```

### `duckdb.execute`

Execute DDL/DML statement without returning rows:

```yaml
- name: create_table
  uses: duckdb.execute
  with:
    sql: |
      CREATE TABLE IF NOT EXISTS documents (
        id INTEGER PRIMARY KEY,
        content VARCHAR,
        embedding FLOAT[1536],
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )

- name: insert_doc
  uses: duckdb.execute
  with:
    sql: |
      INSERT INTO documents (id, content, embedding)
      VALUES ({{ state.doc_id }}, '{{ state.content }}', {{ state.embedding | tojson }})
```

**Returns:** `{"success": true, "message": str, "rows_affected": int}`

### Vector Similarity Search

DuckDB WASM supports vector operations via the VSS extension:

```yaml
name: semantic-search-agent
state_schema:
  query_embedding: list
  similar_docs: list

nodes:
  - name: setup_vss
    uses: duckdb.execute
    with:
      sql: |
        INSTALL vss;
        LOAD vss;
        CREATE INDEX IF NOT EXISTS idx_docs_embedding
        ON documents USING HNSW (embedding)
        WITH (metric = 'cosine')

  - name: search
    uses: duckdb.query
    with:
      sql: |
        SELECT id, content,
               array_cosine_distance(embedding, ?::FLOAT[1536]) as distance
        FROM documents
        ORDER BY distance
        LIMIT 10
      params:
        - "{{ state.query_embedding }}"
    output: similar_docs
```

### Full-Text Search

Use the FTS extension for text search:

```yaml
- name: setup_fts
  uses: duckdb.execute
  with:
    sql: |
      INSTALL fts;
      LOAD fts;
      PRAGMA create_fts_index('documents', 'id', 'content')

- name: text_search
  uses: duckdb.query
  with:
    sql: |
      SELECT *, fts_main_documents.match_bm25(id, ?, 'content') as score
      FROM documents
      WHERE score IS NOT NULL
      ORDER BY score DESC
      LIMIT 10
    params:
      - "{{ state.search_query }}"
  output: search_results
```

### Parquet Support

Read and write Parquet files:

```yaml
- name: load_parquet
  uses: duckdb.query
  with:
    sql: "SELECT * FROM read_parquet('{{ state.data_url }}') LIMIT 100"
  output: parquet_data

- name: export_parquet
  uses: duckdb.execute
  with:
    sql: "COPY (SELECT * FROM results) TO 'output.parquet' (FORMAT PARQUET)"
```

### Transaction Support

```yaml
- name: begin_tx
  uses: duckdb.execute
  with:
    sql: "BEGIN TRANSACTION"

- name: batch_insert
  uses: duckdb.execute
  with:
    sql: |
      INSERT INTO audit_log VALUES (1, 'action1');
      INSERT INTO audit_log VALUES (2, 'action2');

- name: commit_tx
  uses: duckdb.execute
  with:
    sql: "COMMIT"
```

### Available Extensions

| Extension | Use Case | Size |
|-----------|----------|------|
| `parquet` | Columnar file format | ~2MB (autoloaded) |
| `json` | JSON operations | ~500KB (autoloaded) |
| `vss` | Vector similarity (HNSW) | ~1MB |
| `fts` | Full-text search | ~800KB |
| `spatial` | Geospatial operations | ~3MB |
| `icu` | Timezones, collations | ~2MB |
| `httpfs` | Remote file access | ~500KB |

### Error Handling

DuckDB actions return structured errors:

```yaml
- name: safe_query
  uses: duckdb.query
  with:
    sql: "SELECT * FROM maybe_missing_table"
  output: result

- name: check_error
  condition: "{{ state.result_error is not none }}"
  run: |
    # Handle error - result_error contains helpful message
    return {"status": "error", "message": state.result_error}
```

**Error categories:**
- `SYNTAX_ERROR` - SQL grammar errors
- `NOT_FOUND_ERROR` - Missing tables/columns
- `TYPE_ERROR` - Type mismatches
- `EXTENSION_ERROR` - Extension load failures
- `CORS_ERROR` - Remote file access blocked (includes guidance)
- `MEMORY_ERROR` - Out of memory (includes remediation)

### CORS Requirements for Remote Files

When using `httpfs` to read remote files, the server must include:

```
Access-Control-Allow-Origin: https://your-app.com
Access-Control-Allow-Methods: GET, HEAD
Access-Control-Allow-Headers: Range
Access-Control-Expose-Headers: Content-Range, Content-Length
```

---

## Dual Namespace

All code actions are available via dual namespaces: `code.*` and `actions.code_*`.
All tabular data actions use dual namespaces: `data.*` and `actions.data_*`.
DuckDB actions (WASM only): `duckdb.*` and `actions.duckdb_*`.

---

## See Also

- [Actions Overview](./README.md)
- [LLM Actions](./llm.md)
- [I/O Actions](./io.md)
- [Memory Actions](./memory.md) - Persistent storage
