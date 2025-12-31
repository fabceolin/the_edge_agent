# I/O Actions

> **Parent document:** [Actions Overview](./README.md)
> **Related:** [Node Specification](../nodes.md)
> **Epic:** [DOC-002](../../../stories/DOC-002-yaml-reference-modularization.md)

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

## See Also

- [Actions Overview](./README.md)
- [LLM Actions](./llm.md)
- [Data Processing](./data.md)
- [Template Syntax](../templates.md) - Variable interpolation in paths
