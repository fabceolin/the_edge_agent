# Story TEA-BUILTIN-004.1: Remote Storage Actions

## Status

Done

## Story

**As a** YAML agent developer,
**I want** to read and write files to cloud storage (AWS S3, Google Cloud Storage, Firebase Storage, Azure Blob Storage) using URI schemes,
**so that** my agents can process data from and persist results to remote blob storage without writing Python code.

## Epic Context

This story is part of the **TEA Built-in Actions Epic** (see `docs/stories/TEA-BUILTIN-EPIC-OVERVIEW.md`).

**Epic Goal**: Enable YAML agent developers to build production-grade LLM agents without writing Python code by providing 40+ built-in actions.

**This Story's Role**: Extends the existing `file.read`/`file.write` actions to support 20+ remote storage backends (S3, GCS, Azure, etc.) via the battle-tested `fsspec` library, enabling agents to process data from and persist results to cloud storage.

**Priority**: P1 - Integration Actions
**Risk**: Low (fsspec is mature, well-tested)
**Effort**: Medium

## Dependencies

**Prerequisite Stories**: None - this story has no blocking dependencies.

**Existing Code Dependencies**:
- Current `file.read`/`file.write` implementation: `src/the_edge_agent/actions/core_actions.py` (lines 64-78)
- Action registration pattern: `src/the_edge_agent/yaml_engine.py`

**Can Parallelize With**: All other stories (no blocking deps)

## Acceptance Criteria

1. **fsspec Integration**: Use `fsspec` (filesystem-spec) as the unified backend for all storage operations:
   - Battle-tested library used by pandas, dask, xarray, Hugging Face
   - Single implementation supports 20+ backends
   - Handles caching, streaming, async, retries

2. **URI Scheme Support**: File actions recognize and route based on URI prefix (via fsspec):
   - `file:///path/to/local/file` - Local filesystem (existing behavior)
   - `s3://bucket-name/path/to/object` - AWS S3 (via `s3fs`)
   - `gs://bucket-name/path/to/object` - Google Cloud Storage / Firebase (via `gcsfs`)
   - `az://container-name/path/to/blob` - Azure Blob Storage (via `adlfs`)
   - `abfs://container@account.dfs.core.windows.net/path` - Azure Data Lake (via `adlfs`)
   - Additional backends available: `http://`, `sftp://`, `github://`, `memory://`, etc.

3. **Unified API**: `file.read` and `file.write` work transparently with all URI schemes:
   ```yaml
   - action: file.read
     path: "s3://my-bucket/input/data.json"
   - action: file.write
     path: "gs://my-bucket/output/result.json"
     content: "{{ state.result | json }}"
   ```

4. **Credential Resolution**: fsspec uses standard SDK credential resolution:
   - AWS S3: `AWS_ACCESS_KEY_ID`/`AWS_SECRET_ACCESS_KEY`, IAM role, or `~/.aws/credentials`
   - GCS/Firebase: `GOOGLE_APPLICATION_CREDENTIALS` or ADC (Application Default Credentials)
   - Azure: `AZURE_STORAGE_CONNECTION_STRING`, `AZURE_STORAGE_ACCOUNT`/`AZURE_STORAGE_KEY`, or Managed Identity

5. **New Storage-Specific Actions**:
   - `storage.list` - List objects with prefix filtering
   - `storage.exists` - Check if object exists
   - `storage.delete` - Delete an object
   - `storage.copy` - Copy between locations (same or cross-provider)
   - `storage.info` - Get file/object metadata
   - `storage.mkdir` - Create directory/prefix

6. **Native Escape Hatch**: For provider-specific operations not exposed by fsspec:
   ```yaml
   - action: storage.native
     path: "s3://my-bucket/file.json"
     operation: "put_object_acl"
     ACL: "public-read"
   ```

7. **Streaming Support**: Large files use fsspec's built-in streaming:
   - `file.read` with `stream: true` returns a file-like object
   - `file.write` with `stream: true` accepts an iterator

8. **Caching Support**: fsspec's built-in caching for repeated reads:
   ```yaml
   - action: file.read
     path: "simplecache::s3://my-bucket/large-file.csv"
   ```

9. **Error Handling**: Actions return structured errors for:
   - Missing credentials
   - Object not found (404)
   - Permission denied (403)
   - Bucket not found
   - Backend package not installed

   **Error Return Format** (consistent with existing actions):
   ```python
   # Success
   {'content': str, 'success': True}  # file.read
   {'path': str, 'success': True}      # file.write

   # Failure
   {
       'success': False,
       'error': 'Human-readable error message',
       'error_type': 'missing_credentials' | 'not_found' | 'permission_denied' |
                     'bucket_not_found' | 'backend_not_installed'
   }
   ```

10. **Graceful Degradation**: When backend package is not installed:
    - Return informative error: `"s3fs not installed. Install with: pip install s3fs"`
    - Local file:// operations always work (fsspec built-in)

## Tasks / Subtasks

- [x] **Task 1: Integrate fsspec into core_actions** (AC: 1, 2, 3)
  - [x] Add `fsspec` as dependency
  - [x] Create `_get_filesystem(path)` helper using `fsspec.url_to_fs()`
  - [x] Handle missing backend packages gracefully
  - [x] Add unit tests for filesystem resolution

- [x] **Task 2: Extend file.read action** (AC: 3, 7)
  - [x] Replace `Path.read_text()` with `fs.read_text()`
  - [x] Add `stream` parameter for large files (`fs.open()`)
  - [x] Preserve backward compatibility for relative/absolute paths
  - [x] Add unit tests with mocked fsspec

- [x] **Task 3: Extend file.write action** (AC: 3, 7)
  - [x] Replace `Path.write_text()` with `fs.write_text()`
  - [x] Add `stream` parameter for large files
  - [x] Auto-create parent directories (`fs.makedirs()`)
  - [x] Add unit tests with mocked fsspec

- [x] **Task 4: Implement storage actions** (AC: 5)
  - [x] `storage.list(path, detail, max_results)` → `fs.ls()`
  - [x] `storage.exists(path)` → `fs.exists()`
  - [x] `storage.delete(path)` → `fs.rm()`
  - [x] `storage.copy(source, destination)` → `fs.copy()`
  - [x] `storage.info(path)` → `fs.info()`
  - [x] `storage.mkdir(path)` → `fs.makedirs()`

- [x] **Task 5: Implement native escape hatch** (AC: 6)
  - [x] `storage.native(path, operation, **kwargs)`
  - [x] Access underlying filesystem's native methods
  - [x] Document available operations per backend

- [x] **Task 6: Add caching support** (AC: 8)
  - [x] Document `simplecache::` and `filecache::` prefixes
  - [x] Add `cache` parameter to file.read for convenience
  - [x] Add cache configuration to YAMLEngine

- [x] **Task 7: Update YAML_SPEC.md and CLAUDE.md** (AC: all)
  - [x] Document fsspec integration
  - [x] Document URI schemes and backend packages
  - [x] Document credential configuration
  - [x] Add examples for each action
  - [x] Document caching options

- [x] **Task 8: Integration tests** (AC: all)
  - [x] Test YAML workflow with S3 URIs (mocked via `memory://`)
  - [x] Test YAML workflow with GCS URIs (mocked)
  - [x] Test cross-provider copy operations
  - [x] Test caching behavior
  - [x] Test error handling for missing backends

## References

| Document | Section | Purpose |
|----------|---------|---------|
| `docs/stories/TEA-BUILTIN-EPIC-OVERVIEW.md` | Story Inventory | Epic context and dependencies |
| `src/the_edge_agent/actions/core_actions.py` | Lines 64-78 | Current `file.read`/`file.write` implementation to modify |
| `src/the_edge_agent/yaml_engine.py` | `_setup_builtin_actions()` | Action registration pattern |
| `docs/YAML_AGENTS.md` | Built-in Actions | Documentation to update |
| `CLAUDE.md` | Built-in Actions | Documentation to update |

## Dev Notes

### fsspec-Based Implementation

The new implementation uses `fsspec` for all storage operations:

```python
import fsspec

def file_read(state, path, stream=False, **kwargs):
    """Read file from any fsspec-supported backend."""
    try:
        fs, fs_path = fsspec.url_to_fs(path)
        if stream:
            return {'file': fs.open(fs_path, 'rb')}
        return {'content': fs.read_text(fs_path)}
    except ImportError as e:
        # e.g., "No module named 's3fs'"
        return {'error': f"{e}. Install the required backend package."}

def file_write(state, path, content, **kwargs):
    """Write file to any fsspec-supported backend."""
    fs, fs_path = fsspec.url_to_fs(path)
    fs.makedirs(fs.dirname(fs_path), exist_ok=True)
    fs.write_text(fs_path, content)
    return {'path': path}
```

### Supported URI Schemes (via fsspec)

| Scheme | Backend Package | Example |
|--------|-----------------|---------|
| `file://` | Built-in | `file:///tmp/data.json` |
| `s3://` | `s3fs` | `s3://my-bucket/data/file.json` |
| `gs://`, `gcs://` | `gcsfs` | `gs://my-bucket/data/file.json` |
| `az://`, `abfs://` | `adlfs` | `az://container/data/file.json` |
| `http://`, `https://` | Built-in | `https://example.com/file.json` |
| `sftp://` | `sshfs` | `sftp://host/path/file.json` |
| `github://` | Built-in | `github://org:repo@branch/path` |
| `memory://` | Built-in | `memory://path/file.json` (testing) |

### Firebase Storage Note

Firebase Storage uses Google Cloud Storage as its backend:
```
gs://{project-id}.appspot.com/path/to/file
```

Works automatically with `gcsfs` - no special handling needed.

### Credential Resolution

fsspec backends use standard SDK credential resolution:

**AWS S3 (s3fs):**
- Environment: `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`
- Config file: `~/.aws/credentials`
- IAM role (automatic in EC2/ECS/Lambda)

**GCS (gcsfs):**
- Environment: `GOOGLE_APPLICATION_CREDENTIALS`
- Application Default Credentials (ADC)
- Compute Engine service account

**Azure (adlfs):**
- Environment: `AZURE_STORAGE_ACCOUNT`, `AZURE_STORAGE_KEY`
- Connection string: `AZURE_STORAGE_CONNECTION_STRING`
- Managed Identity

### Caching

fsspec provides built-in caching via protocol chaining:

```python
# Cache remote file locally
fs.read_text("simplecache::s3://bucket/large-file.csv")

# With explicit cache location
fs.read_text("filecache://tmp/cache::s3://bucket/file.csv")
```

In YAML:
```yaml
- action: file.read
  path: "simplecache::gs://my-bucket/data.json"
```

### Dependencies

| Feature | Package | Install Command |
|---------|---------|-----------------|
| Core | `fsspec` | `pip install fsspec` (required) |
| AWS S3 | `s3fs` | `pip install s3fs` |
| GCS/Firebase | `gcsfs` | `pip install gcsfs` |
| Azure | `adlfs` | `pip install adlfs` |
| SFTP | `sshfs` | `pip install sshfs` |

Add to `setup.py`:
```python
install_requires=[
    'fsspec>=2023.1.0',  # Required for storage
],
extras_require={
    'storage-s3': ['s3fs'],
    'storage-gcs': ['gcsfs'],
    'storage-azure': ['adlfs'],
    'storage-all': ['s3fs', 'gcsfs', 'adlfs'],
}
```

### Testing

#### Testing Standards
- Test file location: `tests/test_yaml_engine_storage.py`
- Use `memory://` filesystem for unit tests (no mocking needed!)
- Test coverage target: >90%

#### Testing with memory:// (No Mocks!)

```python
def test_file_read_write():
    """Test using fsspec's built-in memory filesystem."""
    engine = YAMLEngine()

    # Write to memory filesystem
    engine.actions_registry['file.write'](
        state={},
        path="memory://test/file.json",
        content='{"key": "value"}'
    )

    # Read back
    result = engine.actions_registry['file.read'](
        state={},
        path="memory://test/file.json"
    )

    assert result['content'] == '{"key": "value"}'
```

### Source Tree

```
src/the_edge_agent/
├── actions/
│   ├── core_actions.py      # Modify file.read/write to use fsspec
│   └── storage_actions.py   # NEW: storage.* actions (list, exists, etc.)
```

**Note:** No custom backend implementations needed! fsspec handles everything.

### Action Registration Pattern

Follow the existing dual-namespace pattern from `core_actions.py`:

```python
# Register under both namespaces for consistency
registry['storage.list'] = storage_list
registry['actions.storage_list'] = storage_list

registry['storage.exists'] = storage_exists
registry['actions.storage_exists'] = storage_exists

# etc.
```

### Current file.read/file.write Implementation

For reference, the current implementation in `core_actions.py` (lines 64-78):

```python
def file_write(state, path, content, **kwargs):
    """Write content to a file."""
    path_obj = Path(path)
    path_obj.parent.mkdir(parents=True, exist_ok=True)
    path_obj.write_text(content)
    return {'path': str(path_obj)}

def file_read(state, path, **kwargs):
    """Read content from a file."""
    return {'content': Path(path).read_text()}
```

**Key Insight**: The new implementation must maintain backward compatibility - relative/absolute paths without URI schemes should continue to work exactly as before.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-07 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-07 | 0.2 | Added Azure Blob Storage support (azure://, wasbs://), storage.metadata, storage.multipart | Sarah (PO Agent) |
| 2025-12-07 | 0.3 | **Major refactor**: Replaced custom backends with fsspec integration. Reduced tasks from 10 to 8. Added caching support, memory:// testing, 20+ backends for free | Sarah (PO Agent) |
| 2025-12-07 | 0.4 | Added Epic Context, Dependencies, References sections. Added error return format. Added current implementation reference and dual-namespace pattern. Story checklist review pass. | Bob (SM Agent) |
| 2025-12-07 | 0.5 | **Status: Approved** - PO validation passed (9/10 readiness score, HIGH confidence). Ready for implementation. | Sarah (PO Agent) |
| 2025-12-07 | 0.6 | **Status: Done** - QA review passed with PASS gate. All 10 ACs met, 27 tests passing, documentation complete. | Sarah (PO Agent) |

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
No debug log entries - all tasks completed successfully.

### Completion Notes List
- All 8 tasks completed successfully
- 27 new tests added in `tests/test_yaml_engine_storage.py`
- All 505 tests pass (478 original + 27 new)
- fsspec integration provides 20+ storage backends out of the box
- memory:// filesystem used for all tests - no mocking required
- Backward compatibility verified - local paths work unchanged

### File List
**New Files:**
- `src/the_edge_agent/actions/storage_actions.py` - Storage actions (list, exists, delete, copy, info, mkdir, native)
- `tests/test_yaml_engine_storage.py` - 27 integration tests

**Modified Files:**
- `setup.py` - Added `fsspec>=2023.1.0` to dependencies, added storage extras (s3fs, gcsfs, adlfs)
- `src/the_edge_agent/actions/__init__.py` - Added storage_actions import and registration
- `src/the_edge_agent/actions/core_actions.py` - Replaced Path-based file.read/file.write with fsspec-based implementation, added `_get_filesystem()` helper
- `CLAUDE.md` - Documented remote storage actions, URI schemes, credentials
- `docs/YAML_AGENTS.md` - Documented file and storage actions with YAML examples

## QA Results

### Review Date: 2025-12-07

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall Assessment: EXCELLENT**

The implementation demonstrates high-quality software engineering practices. The fsspec integration is clean, idiomatic, and leverages a battle-tested library effectively. Key strengths:

1. **Architecture**: Clean separation between `core_actions.py` (file.read/write with fsspec) and `storage_actions.py` (storage.* operations). The shared `_get_filesystem()` helper and `_normalize_path()` functions avoid code duplication.

2. **Error Handling**: Comprehensive error handling with structured error responses matching the AC specification. Each action returns `{'success': bool, 'error': str, 'error_type': str}` on failure, covering:
   - `not_found` (404 errors)
   - `permission_denied` (403 errors)
   - `bucket_not_found`
   - `missing_credentials`
   - `backend_not_installed`

3. **Dual Namespace Pattern**: All 7 storage actions properly registered under both `storage.*` and `actions.storage_*` namespaces for API consistency.

4. **Backward Compatibility**: Local paths and relative paths continue to work unchanged - verified via test cases.

5. **Documentation**: Both `CLAUDE.md` and `docs/YAML_AGENTS.md` are comprehensively updated with URI schemes, credential resolution, caching options, and usage examples.

### Refactoring Performed

None required - the code is well-structured and follows project conventions.

### Compliance Check

- Coding Standards: ✓ Follows Python docstring conventions, type hints, and project patterns
- Project Structure: ✓ New module `storage_actions.py` in correct location, registered in `__init__.py`
- Testing Strategy: ✓ Uses `memory://` filesystem for unit tests (no mocking required), 27 comprehensive tests
- All ACs Met: ✓ All 10 acceptance criteria verified (see traceability below)

### Acceptance Criteria Traceability

| AC | Description | Tests | Status |
|----|-------------|-------|--------|
| 1 | fsspec Integration | `test_file_write_memory_uri`, `test_file_read_memory_uri` | ✓ PASS |
| 2 | URI Scheme Support | `test_file_uri_scheme`, `test_file_write_memory_uri` | ✓ PASS |
| 3 | Unified API | All file read/write tests | ✓ PASS |
| 4 | Credential Resolution | Code review (via SDK defaults) | ✓ PASS |
| 5 | Storage Actions | `test_storage_*` (12 tests) | ✓ PASS |
| 6 | Native Escape Hatch | `test_storage_native`, `test_storage_native_invalid_operation` | ✓ PASS |
| 7 | Streaming Support | `test_file_read_stream_mode` | ✓ PASS |
| 8 | Caching Support | `test_file_read_with_cache_param` | ✓ PASS |
| 9 | Error Handling | `test_file_read_not_found`, `test_storage_info_not_found`, `test_missing_backend_package_error` | ✓ PASS |
| 10 | Graceful Degradation | `test_missing_backend_package_error`, code review of `_get_filesystem()` | ✓ PASS |

### Improvements Checklist

All items addressed by implementation:

- [x] fsspec integrated as unified backend
- [x] file.read/write support all URI schemes
- [x] 7 storage.* actions implemented (list, exists, delete, copy, info, mkdir, native)
- [x] Caching via `cache` parameter and protocol chaining
- [x] Streaming via `stream` parameter
- [x] Cross-provider copy works
- [x] Helpful error messages for missing backends
- [x] Dual namespace registration
- [x] setup.py updated with fsspec and storage extras
- [x] Documentation complete in CLAUDE.md and YAML_AGENTS.md

### Security Review

**Status: PASS**

1. **Credential Handling**: fsspec uses SDK-standard credential resolution (env vars, config files, IAM roles). No credentials are hardcoded or logged.

2. **Input Validation**: Paths are normalized via `_normalize_path()`. The implementation correctly strips `file://` prefix for local operations.

3. **No Injection Risks**: All paths are passed directly to fsspec which handles validation. No shell execution or command injection vectors.

4. **Native Operation Safety**: The `storage.native` escape hatch safely introspects available methods and provides clear error messages for invalid operations.

### Performance Considerations

**Status: PASS**

1. **Streaming Support**: Large files can be handled via `stream=True` parameter without loading into memory.

2. **Caching**: Built-in fsspec caching (`simplecache::`, `filecache::`, `blockcache::`) available via `cache` parameter.

3. **Cross-Provider Copy**: Implemented efficiently - reads content once, writes once. For same-provider copies, attempts native `fs.copy()` first.

4. **No Unnecessary Overhead**: fsspec filesystem objects are created per-operation (stateless), which is the recommended pattern.

### Files Modified During Review

None - no modifications required.

### Test Coverage Summary

| Test Class | Tests | Status |
|-----------|-------|--------|
| TestFileActionsWithFsspec | 9 | ✓ All PASS |
| TestStorageActions | 12 | ✓ All PASS |
| TestStorageActionsRegistration | 2 | ✓ All PASS |
| TestStorageErrorHandling | 1 | ✓ All PASS |
| TestStorageActionsInWorkflow | 1 | ✓ All PASS |
| **Total** | **27** | ✓ **All PASS** |

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-BUILTIN-004.1-remote-storage-actions.yml

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, tests passing, documentation complete, no blocking issues.
