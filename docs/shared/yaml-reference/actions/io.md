# I/O Actions

> **Parent document:** [Actions Overview](./README.md)
> **Related:** [Node Specification](../nodes.md)
> **Epic:** DOC-002 (YAML Reference Modularization)

## Overview

I/O actions provide HTTP requests, file operations, and cloud storage integration. File and storage actions support both local paths and remote URIs via fsspec (S3, GCS, Azure, etc.).

---

## Table of Contents

- [HTTP Actions](#http-actions)
  - [http.get](#httpget)
  - [http.post](#httppost)
- [File Actions](#file-actions)
  - [file.read](#fileread)
  - [file.write](#filewrite)
- [Storage Actions](#storage-actions)
  - [storage.list](#storagelist)
  - [storage.exists](#storageexists)
  - [storage.info](#storageinfo)
  - [storage.copy](#storagecopy)
  - [storage.delete](#storagedelete)
  - [storage.mkdir](#storagemkdir)
  - [storage.native](#storagenative)
  - [storage.hash](#storagehash)

---

## HTTP Actions

### `http.get`

```yaml
- name: fetch
  uses: http.get
  with:
    url: https://api.example.com/data    # Required
    headers:                              # Optional
      Authorization: Bearer token
  output: response_data
```

### `http.post`

```yaml
- name: submit
  uses: http.post
  with:
    url: https://api.example.com/submit  # Required
    json:                                 # Optional: JSON body
      key: value
    headers:                              # Optional
      Content-Type: application/json
  output: response_data
```

---

## File Actions

File actions support both local paths and remote URIs via fsspec.

**Supported URI schemes:**
- Local: `./path`, `/abs/path`, `file:///path`
- AWS S3: `s3://bucket/path` (requires `pip install s3fs`)
- GCS: `gs://bucket/path` (requires `pip install gcsfs`)
- Azure: `az://container/path` (requires `pip install adlfs`)
- Memory: `memory://path` (for testing)

### `file.read`

```yaml
# Local file
- name: load_local
  uses: file.read
  with:
    path: ./data/input.txt               # Required
  output: file_content

# Remote file (S3)
- name: load_s3
  uses: file.read
  with:
    path: s3://my-bucket/data/input.txt
    cache: simple                        # Optional: "simple", "file", "block"
  output: file_content
```

**Returns:**
- Success: `{"content": str, "success": true}`
- Failure: `{"success": false, "error": str, "error_type": str}`

### `file.write`

```yaml
# Local file
- name: save_local
  uses: file.write
  with:
    path: "./output/{{ state.filename }}.txt"  # Required
    content: "{{ state.data }}"                 # Required

# Remote file (GCS)
- name: save_gcs
  uses: file.write
  with:
    path: gs://my-bucket/output/result.json
    content: "{{ state.data | json }}"
```

**Returns:**
- Success: `{"path": str, "success": true}`
- Failure: `{"success": false, "error": str, "error_type": str}`

---

## Storage Actions

Advanced storage operations for cloud and local filesystems.

### `storage.list`

```yaml
- name: list_files
  uses: storage.list
  with:
    path: s3://my-bucket/data/            # Required
    detail: true                          # Optional (include metadata)
    max_results: 100                      # Optional
  output: files_list
```

**Returns:** `{"files": list, "count": int, "success": true}`

### `storage.exists`

```yaml
- name: check_file
  uses: storage.exists
  with:
    path: s3://my-bucket/data/file.json   # Required
  output: exists_result
```

**Returns:** `{"exists": bool, "path": str, "success": true}`

### `storage.info`

```yaml
- name: get_info
  uses: storage.info
  with:
    path: s3://my-bucket/data/file.json   # Required
  output: file_info
```

**Returns:** `{"info": {"name": str, "size": int, "type": str, ...}, "success": true}`

### `storage.copy`

```yaml
- name: copy_to_gcs
  uses: storage.copy
  with:
    source: s3://source-bucket/file.json       # Required
    destination: gs://dest-bucket/file.json    # Required
  output: copy_result
```

**Returns:** `{"copied": true, "source": str, "destination": str, "success": true}`

### `storage.delete`

```yaml
- name: cleanup
  uses: storage.delete
  with:
    path: s3://my-bucket/temp/file.json   # Required
    recursive: false                       # Optional (for directories)
  output: delete_result
```

**Returns:** `{"deleted": true, "path": str, "success": true}`

### `storage.mkdir`

```yaml
- name: make_dir
  uses: storage.mkdir
  with:
    path: s3://my-bucket/new-folder/      # Required
    exist_ok: true                         # Optional
  output: mkdir_result
```

**Returns:** `{"created": true, "path": str, "success": true}`

### `storage.native`

Access provider-specific operations:

```yaml
- name: set_acl
  uses: storage.native
  with:
    path: s3://my-bucket/file.json        # Required
    operation: put_object_acl             # Required
    ACL: public-read                      # Operation-specific params
  output: native_result
```

**Returns:** `{"result": any, "operation": str, "success": true}`

### `storage.hash`

Compute SHA256 hash of file content from any URI:

```yaml
- name: hash_document
  uses: storage.hash
  with:
    path: "s3://bucket/document.pdf"       # Any fsspec URI
    algorithm: sha256                       # sha256, md5, or blake2b
  output: hash_result
```

**Returns:** `{"success": true, "hash": str, "algorithm": str, "size_bytes": int, "path": str}`

---

## Dual Namespace

All storage actions are available via dual namespaces: `storage.*` and `actions.storage_*`.

---

## WASM/Browser Storage (Rust Implementation)

When running in browser/WASM environments, the Rust implementation (`tea-wasm-llm`) provides storage actions via Apache OpenDAL with additional browser-specific backends.

### Browser-Specific URI Schemes

| Scheme | Description | Persistence | Notes |
|--------|-------------|-------------|-------|
| `opfs://` | Origin Private File System | Per-origin | Chrome 102+, Edge 102+, partial Firefox/Safari |
| `memory://` | In-memory storage | Session only | Testing/development |

### OPFS (Origin Private File System)

OPFS provides persistent browser-local storage that:
- Persists across browser sessions
- Is isolated per-origin (domain)
- Can be read by DuckDB WASM for analytics
- Supports binary files (parquet, images)

**Initializing OPFS (JavaScript):**

```javascript
import { init_opfs, is_opfs_available } from 'tea-wasm-llm';

// Initialize OPFS at startup
await init_opfs();
console.log('OPFS available:', is_opfs_available());
```

**Using OPFS in YAML:**

```yaml
name: browser-analytics
nodes:
  # Cache data from cloud to browser
  - name: cache_data
    action: storage.copy
    with:
      source: "s3://{{ state.bucket }}/{{ state.file }}"
      destination: "opfs://cache/{{ state.file }}"

  # Query cached data with DuckDB
  - name: analyze
    action: duckdb.query
    with:
      sql: |
        SELECT region, SUM(sales)
        FROM read_parquet('opfs://cache/{{ state.file }}')
        GROUP BY region

  # Upload results back to cloud
  - name: upload
    action: storage.copy
    with:
      source: "opfs://output/results.parquet"
      destination: "s3://{{ state.bucket }}/results/{{ state.run_id }}.parquet"
```

### WASM Storage Actions

All Python storage actions are available in WASM with the same API:

| Action | WASM Support | Notes |
|--------|--------------|-------|
| `storage.read` | Yes | Binary via `binary: true` |
| `storage.write` | Yes | Binary via `binary: true` |
| `storage.exists` | Yes | |
| `storage.delete` | Yes | |
| `storage.list` | Yes | With `limit` option |
| `storage.copy` | Yes | Cross-provider supported |

### Credential Management (WASM)

Credentials in WASM are:
- Stored in-memory only
- **Never** serialized to state or checkpoints
- Must be set from JavaScript before use

```javascript
import { set_storage_credentials } from 'tea-wasm-llm';

// Set S3 credentials
set_storage_credentials('s3', JSON.stringify({
    access_key_id: 'AKIA...',
    secret_access_key: '...',
    region: 'us-east-1',
    endpoint: 'https://s3.amazonaws.com'  // optional, for MinIO/R2
}));

// Set GCS credentials
set_storage_credentials('gcs', JSON.stringify({
    credential: '{ service account JSON }'
}));

// Clear all credentials
clear_storage_credentials();
```

### CORS Requirements

For browser-based cloud storage access, CORS must be configured:

**S3 CORS Configuration:**
```json
{
    "CORSRules": [{
        "AllowedOrigins": ["https://your-app.com"],
        "AllowedMethods": ["GET", "PUT", "DELETE", "HEAD"],
        "AllowedHeaders": ["*"],
        "ExposeHeaders": ["ETag", "x-amz-meta-*"]
    }]
}
```

**Note:** OPFS does not require CORS as it's browser-local storage.

### Feature Flags

The WASM storage module uses feature flags to control bundle size:

| Feature | Size Impact | Description |
|---------|-------------|-------------|
| `storage-memory` | +50KB | In-memory (testing) |
| `storage-http` | +100KB | HTTP(S) read-only |
| `storage-opfs` | +100KB | Browser OPFS |
| `storage-s3` | +300KB | AWS S3, MinIO, R2 |
| `storage-gcs` | +250KB | Google Cloud Storage |
| `storage-azblob` | +280KB | Azure Blob Storage |

Default features: `storage-memory`, `storage-http`

---

## See Also

- [Actions Overview](./README.md)
- [LLM Actions](./llm.md)
- [Data Processing](./data.md)
- [Template Syntax](../templates.md) - Variable interpolation in paths
