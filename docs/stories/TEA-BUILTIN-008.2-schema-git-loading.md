# Story TEA-BUILTIN-008.2: Schema Loading with Git Refs and Remote Storage

## Status: Complete ✅

**Story Checklist**: PASSED (2024-12-22) - Clarity Score: 10/10

## QA Notes

**Test Design**: [`docs/qa/assessments/TEA-BUILTIN-008.2-test-design-20251222.md`](../qa/assessments/TEA-BUILTIN-008.2-test-design-20251222.md)

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 60 |
| Unit Tests | 32 (53%) |
| Integration Tests | 22 (37%) |
| E2E Tests | 6 (10%) |
| Priority Distribution | P0: 22, P1: 25, P2: 13 |

**Test Coverage by AC**:
- AC1-2 (Git refs): 20 unit tests for short/full URL parsing (Python + Rust)
- AC3 (fsspec): 7 unit tests for URI detection, 7 integration tests for fetching
- AC4 (Deep merge): 6 integration tests for mixed sources
- AC5 (SSH auth): 6 tests for private repo access
- AC6 (Caching): 5 tests for TTL and cache behavior
- AC7 (Errors): 5 tests for error handling
- AC10 (Parity): 2 cross-runtime parity tests
- E2E: 4 tests covering GitHub, SSH, S3, and mixed sources

## QA Results

**Gate Review**: [`docs/qa/gates/TEA-BUILTIN-008.2-schema-git-loading.yml`](../qa/gates/TEA-BUILTIN-008.2-schema-git-loading.yml)

| Criterion | Status |
|-----------|--------|
| Gate Decision | **PASS** |
| Quality Score | 97/100 |
| Tests Passing | 35/36 (1 skipped) |
| AC Coverage | 17/17 |

**Summary**: Unified schema loader with Git short/full URL and fsspec URI support. Caching with TTL, SSH key authentication, and comprehensive error handling.

## Story

**As a** TEA agent developer,
**I want** to load JSON Schemas from Git repositories and remote storage (S3, GCS, Azure, HTTP) using a unified `uses:` syntax,
**so that** I can version and share extraction schemas across projects from any storage backend.

## Acceptance Criteria

### Functional Requirements

1. **Short form Git reference**: Support `owner/repo@ref#path` syntax
   ```yaml
   uses: company/schemas@v1.0.0#invoice/schema.json
   ```

2. **Full URL Git reference**: Support `git+protocol://host/repo.git@ref#path` syntax
   ```yaml
   uses: git+https://github.com/company/schemas.git@main#schema.json
   uses: git+ssh://git@github.com/company/private.git@v2.0.0#schema.json
   ```

3. **fsspec URI support**: Support all fsspec-compatible storage backends
   ```yaml
   uses:
     # AWS S3
     - s3://bucket-name/schemas/base.json
     # Google Cloud Storage
     - gs://bucket-name/schemas/overlay.json
     # Azure Blob Storage
     - az://container/schemas/custom.json
     # HTTP/HTTPS (read-only)
     - https://example.com/schemas/public.json
     # Local filesystem
     - file:///absolute/path/schema.json
   ```

4. **Multiple schemas with deep merge**: Support list of `uses:` entries (mixed sources)
   ```yaml
   uses:
     - base/schemas@v1.0.0#common/base.json       # Git ref (lowest priority)
     - s3://company-schemas/invoice/fields.json   # S3
     - gs://shared-schemas/overlay.json           # GCS
     - company/private@main#overrides.json        # Git (highest priority - last wins)
   ```

5. **Private repository access**: Support SSH key authentication via environment variables
   - `GIT_SSH_KEY`: Raw SSH private key content
   - `GIT_SSH_KEY_PATH`: Path to SSH key file

6. **Cloud storage authentication**: Use existing fsspec credential mechanisms
   - AWS: `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, IAM roles
   - GCS: `GOOGLE_APPLICATION_CREDENTIALS`, service account
   - Azure: `AZURE_STORAGE_CONNECTION_STRING`, `AZURE_STORAGE_ACCOUNT_KEY`

7. **Schema caching**: Cache fetched schemas to avoid repeated operations
   - Cache key: normalized URI
   - Cache invalidation: TTL-based or manual clear

8. **Error handling**: Clear error messages for:
   - Invalid reference syntax
   - Repository/bucket not found
   - Path not found
   - Authentication failures
   - Network errors

### Integration Requirements

9. Integrates with `llamaextract.extract` action's `schema.uses` parameter
10. Works with `schema.merge` action for standalone merging
11. Python and Rust implementations produce identical merged schemas
12. Reuses existing `_get_filesystem()` from `core_actions.py` for fsspec

### Quality Requirements

13. Unit tests for reference parsing (Git and fsspec)
14. Integration tests with local Git repositories
15. Integration tests with mocked cloud storage
16. Documentation with examples
17. Error message quality tests

## Tasks / Subtasks

- [ ] **Task 1: Unified Reference Parser** (AC: 1-3, 8)
  - [ ] Python: Create `python/src/the_edge_agent/schema/schema_loader.py`
  - [ ] Python: Implement `parse_schema_reference()` dispatcher
  - [ ] Python: Detect Git refs vs fsspec URIs
  - [ ] Python: Support short form Git `owner/repo@ref#path`
  - [ ] Python: Support full URL Git `git+protocol://...`
  - [ ] Python: Support fsspec URIs (s3://, gs://, az://, https://, file://)
  - [ ] Rust: Create `rust/src/schema/schema_loader.rs`
  - [ ] Rust: Implement reference parsing with same syntax

- [ ] **Task 2: Git Fetcher** (AC: 1-2, 5, 8)
  - [ ] Python: Implement `fetch_schema_from_git()` function
  - [ ] Python: Support HTTPS public repositories
  - [ ] Python: Support SSH with key from `GIT_SSH_KEY` env var
  - [ ] Python: Support SSH with key file from `GIT_SSH_KEY_PATH`
  - [ ] Python: Implement caching with TTL
  - [ ] Rust: Implement equivalent functionality using `git2` crate

- [ ] **Task 3: fsspec Fetcher** (AC: 3, 6, 7, 8)
  - [ ] Python: Implement `fetch_schema_from_fsspec()` using existing `_get_filesystem()`
  - [ ] Python: Support S3 with AWS credentials
  - [ ] Python: Support GCS with service account
  - [ ] Python: Support Azure with connection string
  - [ ] Python: Support HTTP/HTTPS (read-only)
  - [ ] Python: Implement caching with TTL
  - [ ] Rust: Implement using `object_store` crate

- [ ] **Task 4: Deep Merge Integration** (AC: 4, 9-10)
  - [ ] Python: Implement `resolve_schema_uses()` for multiple refs (mixed sources)
  - [ ] Python: Call deep merge (from Story 008.3) for combining schemas
  - [ ] Python: Wire into `llamaextract.extract` action
  - [ ] Rust: Implement equivalent with deep merge

- [ ] **Task 5: Testing** (AC: 13-15)
  - [ ] Python unit tests for reference type detection
  - [ ] Python unit tests for Git reference parsing
  - [ ] Python unit tests for fsspec URI parsing
  - [ ] Python integration tests with local bare Git repo
  - [ ] Python integration tests with mocked S3/GCS/Azure
  - [ ] Rust unit tests for reference parsing
  - [ ] Cross-runtime parity tests

- [ ] **Task 6: Documentation** (AC: 16)
  - [ ] Add Schema Loading section to `docs/shared/YAML_REFERENCE.md`
  - [ ] Document Git reference syntax
  - [ ] Document fsspec URI syntax
  - [ ] Document authentication options (Git SSH + cloud credentials)
  - [ ] Add troubleshooting section

## Dev Notes

### Git Reference Syntax Specification

```
SHORT_FORM:
  owner/repo@ref#path

  Examples:
    company/schemas@v1.0.0#invoice/schema.json
    company/schemas@main#base.json
    my-org/private-schemas@feature-branch#custom/fields.json

FULL_URL_FORM:
  git+protocol://host/path.git@ref#filepath

  Protocols:
    https://  - Public or token-authenticated
    ssh://    - SSH key authenticated

  Examples:
    git+https://github.com/company/schemas.git@v1.0.0#schema.json
    git+ssh://git@github.com/company/private.git@main#schema.json
    git+https://gitlab.com/org/project.git@v2.0.0#schemas/invoice.json
```

### Reference Parsing (Python)

```python
import re
from dataclasses import dataclass
from typing import Optional

@dataclass
class GitReference:
    """Parsed Git schema reference."""
    host: str           # github.com, gitlab.com, etc.
    owner: str          # Repository owner/org
    repo: str           # Repository name
    ref: str            # Branch, tag, or commit
    path: str           # Path to schema file
    protocol: str       # https or ssh
    original: str       # Original reference string

def parse_git_reference(ref: str) -> GitReference:
    """Parse a Git schema reference.

    Args:
        ref: Reference string in short or full URL form

    Returns:
        GitReference with parsed components

    Raises:
        ValueError: If reference format is invalid
    """
    # Short form: owner/repo@ref#path
    short_pattern = r'^([^/]+)/([^@]+)@([^#]+)#(.+)$'

    # Full URL: git+protocol://host/owner/repo.git@ref#path
    full_pattern = r'^git\+(https?|ssh)://([^/]+)/(.+?)(?:\.git)?@([^#]+)#(.+)$'

    if match := re.match(short_pattern, ref):
        owner, repo, git_ref, path = match.groups()
        return GitReference(
            host='github.com',  # Default for short form
            owner=owner,
            repo=repo,
            ref=git_ref,
            path=path,
            protocol='https',
            original=ref
        )
    elif match := re.match(full_pattern, ref):
        protocol, host, repo_path, git_ref, path = match.groups()
        # Parse owner/repo from repo_path
        parts = repo_path.rsplit('/', 1)
        owner = parts[0] if len(parts) > 1 else ''
        repo = parts[-1]
        return GitReference(
            host=host,
            owner=owner,
            repo=repo,
            ref=git_ref,
            path=path,
            protocol=protocol,
            original=ref
        )
    else:
        raise ValueError(
            f"Invalid Git reference format: {ref}\n"
            "Expected: owner/repo@ref#path or git+protocol://host/repo.git@ref#path"
        )
```

### Git Fetcher (Python)

```python
import os
import tempfile
import subprocess
from pathlib import Path
from typing import Dict, Any, Optional
import json

class GitSchemaFetcher:
    """Fetch schemas from Git repositories."""

    def __init__(self, cache_dir: Optional[Path] = None):
        self.cache_dir = cache_dir or Path(tempfile.gettempdir()) / 'tea-schema-cache'
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        self._cache: Dict[str, Any] = {}

    def fetch(self, ref: GitReference) -> Dict[str, Any]:
        """Fetch schema from Git repository.

        Args:
            ref: Parsed Git reference

        Returns:
            Parsed JSON schema
        """
        cache_key = f"{ref.host}/{ref.owner}/{ref.repo}@{ref.ref}#{ref.path}"

        # Check memory cache
        if cache_key in self._cache:
            return self._cache[cache_key]

        # Clone/fetch repo
        repo_dir = self._ensure_repo(ref)

        # Read schema file
        schema_path = repo_dir / ref.path
        if not schema_path.exists():
            raise FileNotFoundError(f"Schema not found: {ref.path} in {ref.original}")

        with open(schema_path) as f:
            schema = json.load(f)

        self._cache[cache_key] = schema
        return schema

    def _ensure_repo(self, ref: GitReference) -> Path:
        """Ensure repository is cloned and at correct ref."""
        repo_dir = self.cache_dir / ref.host / ref.owner / ref.repo

        if repo_dir.exists():
            # Fetch and checkout
            self._run_git(['fetch', 'origin', ref.ref], cwd=repo_dir)
            self._run_git(['checkout', ref.ref], cwd=repo_dir)
        else:
            # Clone
            repo_dir.parent.mkdir(parents=True, exist_ok=True)
            clone_url = self._build_clone_url(ref)
            self._run_git(['clone', '--depth=1', '-b', ref.ref, clone_url, str(repo_dir)])

        return repo_dir

    def _build_clone_url(self, ref: GitReference) -> str:
        """Build Git clone URL with authentication."""
        if ref.protocol == 'ssh':
            return f"git@{ref.host}:{ref.owner}/{ref.repo}.git"
        else:
            return f"https://{ref.host}/{ref.owner}/{ref.repo}.git"

    def _run_git(self, args: list, cwd: Optional[Path] = None) -> str:
        """Run Git command with SSH key if configured."""
        env = os.environ.copy()

        # Handle SSH key authentication
        ssh_key = os.environ.get('GIT_SSH_KEY')
        ssh_key_path = os.environ.get('GIT_SSH_KEY_PATH')

        if ssh_key:
            # Write key to temp file
            key_file = self.cache_dir / '.ssh_key'
            key_file.write_text(ssh_key)
            key_file.chmod(0o600)
            env['GIT_SSH_COMMAND'] = f'ssh -i {key_file} -o StrictHostKeyChecking=no'
        elif ssh_key_path:
            env['GIT_SSH_COMMAND'] = f'ssh -i {ssh_key_path} -o StrictHostKeyChecking=no'

        result = subprocess.run(
            ['git'] + args,
            cwd=cwd,
            env=env,
            capture_output=True,
            text=True
        )

        if result.returncode != 0:
            raise RuntimeError(f"Git command failed: {result.stderr}")

        return result.stdout
```

### fsspec Fetcher (Python)

```python
import json
from typing import Dict, Any, Optional
from pathlib import Path

# Reuse existing fsspec infrastructure
from the_edge_agent.actions.core_actions import _get_filesystem, _normalize_path

class FsspecSchemaFetcher:
    """Fetch schemas from any fsspec-compatible storage backend."""

    def __init__(self, cache_dir: Optional[Path] = None):
        self.cache_dir = cache_dir or Path(tempfile.gettempdir()) / 'tea-schema-cache'
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        self._cache: Dict[str, Any] = {}

    def fetch(self, uri: str) -> Dict[str, Any]:
        """Fetch schema from fsspec URI.

        Supported URIs:
            - s3://bucket/path/schema.json
            - gs://bucket/path/schema.json
            - az://container/path/schema.json
            - https://example.com/schema.json
            - file:///absolute/path/schema.json

        Args:
            uri: fsspec-compatible URI

        Returns:
            Parsed JSON schema
        """
        # Check cache
        if uri in self._cache:
            return self._cache[uri]

        # Get filesystem using existing infrastructure
        path = _normalize_path(uri)
        fs, fs_path, err = _get_filesystem(path)
        if err:
            raise RuntimeError(f"Failed to access {uri}: {err['error']}")

        # Read and parse schema
        try:
            with fs.open(fs_path, 'r') as f:
                schema = json.load(f)
        except FileNotFoundError:
            raise FileNotFoundError(f"Schema not found: {uri}")
        except json.JSONDecodeError as e:
            raise ValueError(f"Invalid JSON in schema {uri}: {e}")

        self._cache[uri] = schema
        return schema


def is_fsspec_uri(ref: str) -> bool:
    """Check if reference is an fsspec URI (not a Git ref)."""
    fsspec_schemes = ('s3://', 'gs://', 'az://', 'abfs://',
                      'http://', 'https://', 'file://', 'memory://')
    return ref.startswith(fsspec_schemes)


def fetch_schema(ref: str, git_fetcher=None, fsspec_fetcher=None) -> Dict[str, Any]:
    """Unified schema fetcher - routes to Git or fsspec based on URI format."""
    if is_fsspec_uri(ref):
        fetcher = fsspec_fetcher or FsspecSchemaFetcher()
        return fetcher.fetch(ref)
    else:
        fetcher = git_fetcher or GitSchemaFetcher()
        parsed = parse_git_reference(ref)
        return fetcher.fetch(parsed)
```

### YAML Integration Example

```yaml
name: invoice-extractor
description: Extract invoice data using versioned schema from multiple sources

nodes:
  - name: extract-invoice
    action: llamaextract.extract
    with:
      file: "{{ state.document_url }}"
      schema:
        uses:
          # Git repository (versioned)
          - acme/schemas@v1.0.0#common/base.json
          # AWS S3
          - s3://company-schemas/invoice/fields.json
          # Google Cloud Storage
          - gs://shared-schemas/overlay.json
          # Private Git repo (SSH)
          - git+ssh://git@github.com/acme/private@main#overrides.json
          # Public HTTP
          - https://json-schema.org/draft/2020-12/schema
      mode: PREMIUM
```

### Source Tree

```
python/src/the_edge_agent/
├── schema/
│   ├── __init__.py
│   ├── schema_loader.py   # NEW: Unified dispatcher (Git + fsspec)
│   ├── git_loader.py      # NEW: Git reference parser + fetcher
│   ├── fsspec_loader.py   # NEW: fsspec URI fetcher (uses core_actions)
│   └── deep_merge.py      # From Story 008.3
└── actions/
    ├── core_actions.py    # EXISTING: _get_filesystem() reused
    └── llamaextract_actions.py  # Uses schema_loader

rust/src/
├── schema/
│   ├── mod.rs
│   ├── schema_loader.rs   # NEW: Unified dispatcher
│   ├── git_loader.rs      # NEW: Git reference parser + fetcher
│   ├── fsspec_loader.rs   # NEW: Uses object_store crate
│   └── deep_merge.rs      # From Story 008.3
└── ...
```

### Rust Dependencies

```toml
[dependencies]
git2 = "0.18"              # Git operations
object_store = "0.9"       # S3, GCS, Azure (fsspec equivalent)
serde_json = "1.0"         # JSON parsing
regex = "1.0"              # Reference parsing
url = "2.5"                # URI parsing
```

## Testing

### Test File Location
- Python: `python/tests/test_schema_loader.py`
- Python: `python/tests/test_git_loader.py`
- Python: `python/tests/test_fsspec_loader.py`
- Rust: `rust/tests/test_schema_loader.rs`

### Test Standards
- Unit tests for reference type detection (Git vs fsspec)
- Unit tests for Git reference parsing (valid and invalid inputs)
- Unit tests for fsspec URI parsing
- Integration tests with local bare Git repositories
- Integration tests with mocked S3/GCS/Azure (using `moto`, `gcsfs` mock, `azurite`)
- Test SSH key authentication (with mock keys)
- Test cloud credential handling
- Test caching behavior (both Git and fsspec)
- Cross-runtime parity: Same references resolve to identical schemas

### Test Setup (Local Git Repo)

```python
@pytest.fixture
def local_git_repo(tmp_path):
    """Create a local Git repo for testing."""
    repo_dir = tmp_path / 'test-schemas'
    repo_dir.mkdir()

    # Initialize repo
    subprocess.run(['git', 'init'], cwd=repo_dir)

    # Create schema file
    schema_dir = repo_dir / 'schemas'
    schema_dir.mkdir()
    (schema_dir / 'test.json').write_text(json.dumps({
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "type": "object",
        "properties": {"name": {"type": "string"}}
    }))

    # Commit
    subprocess.run(['git', 'add', '.'], cwd=repo_dir)
    subprocess.run(['git', 'commit', '-m', 'Initial'], cwd=repo_dir)
    subprocess.run(['git', 'tag', 'v1.0.0'], cwd=repo_dir)

    return repo_dir
```

## Definition of Done

- [ ] Reference parser implemented in Python (AC 1-2)
- [ ] Reference parser implemented in Rust (AC 1-2)
- [ ] Git fetcher with caching in Python (AC 4-5)
- [ ] Git fetcher with caching in Rust (AC 4-5)
- [ ] Multiple `uses:` entries with deep merge (AC 3)
- [ ] SSH authentication working (AC 4)
- [ ] Error handling with clear messages (AC 6)
- [ ] Integration with llamaextract.extract (AC 7)
- [ ] Cross-runtime parity verified (AC 9)
- [ ] Tests passing (AC 10-11)
- [ ] Documentation complete (AC 12)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-22 | 0.1.0 | Initial story creation | Sarah (PO) |
| 2024-12-22 | 0.2.0 | Added fsspec URI support (S3, GCS, Azure, HTTP) | Sarah (PO) |
| 2024-12-22 | 0.3.0 | Test design complete (60 scenarios), status → Ready for Dev | Quinn (QA) |
| 2024-12-22 | 0.4.0 | Story checklist PASSED (10/10) | Bob (SM) |
| 2024-12-22 | 1.0.0 | Implementation complete, QA gate PASS (97/100), status → Complete | Quinn (QA) |
