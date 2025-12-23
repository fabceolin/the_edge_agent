"""
Unified Schema Loader for Git References and fsspec URIs.

This module provides schema loading from:
- Git repositories (short form: owner/repo@ref#path)
- Git repositories (full URL: git+protocol://host/repo.git@ref#path)
- fsspec URIs (s3://, gs://, az://, https://, file://)

TEA-BUILTIN-008.2: Schema Loading with Git Refs and Remote Storage

Example:
    >>> from the_edge_agent.schema import schema_loader
    >>>
    >>> # Git short form
    >>> schema = schema_loader.fetch_schema("company/schemas@v1.0.0#invoice.json")
    >>>
    >>> # Git full URL
    >>> schema = schema_loader.fetch_schema("git+https://github.com/co/schemas.git@main#base.json")
    >>>
    >>> # fsspec URI
    >>> schema = schema_loader.fetch_schema("s3://bucket/schemas/invoice.json")
"""

import json
import os
import re
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple
import time


# Cache TTL in seconds (default: 5 minutes)
DEFAULT_CACHE_TTL = 300


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
    """
    Parse a Git schema reference.

    Supports two forms:
    - Short form: owner/repo@ref#path (defaults to github.com)
    - Full URL: git+protocol://host/owner/repo.git@ref#path

    Args:
        ref: Reference string in short or full URL form

    Returns:
        GitReference with parsed components

    Raises:
        ValueError: If reference format is invalid

    Examples:
        >>> parse_git_reference("company/schemas@v1.0.0#invoice/schema.json")
        GitReference(host='github.com', owner='company', repo='schemas', ...)

        >>> parse_git_reference("git+https://gitlab.com/org/project.git@main#schema.json")
        GitReference(host='gitlab.com', owner='org', repo='project', ...)
    """
    # Full URL: git+protocol://host/path/repo.git@ref#filepath
    # Handle both with and without .git suffix
    # Check full URL FIRST to avoid short pattern matching URLs
    full_pattern = r'^git\+(https?|ssh)://([^/]+)/(.+?)(?:\.git)?@([^#]+)#(.+)$'

    # Short form: owner/repo@ref#path
    short_pattern = r'^([^/@]+)/([^@]+)@([^#]+)#(.+)$'

    if match := re.match(full_pattern, ref):
        protocol, host, repo_path, git_ref, path = match.groups()
        # Parse owner/repo from repo_path
        parts = repo_path.rsplit('/', 1)
        if len(parts) > 1:
            owner = parts[0]
            repo = parts[1]
        else:
            owner = ''
            repo = parts[0]
        return GitReference(
            host=host,
            owner=owner,
            repo=repo,
            ref=git_ref,
            path=path,
            protocol=protocol,
            original=ref
        )
    elif match := re.match(short_pattern, ref):
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
    else:
        raise ValueError(
            f"Invalid Git reference format: {ref}\n"
            "Expected: owner/repo@ref#path or git+protocol://host/repo.git@ref#path"
        )


def is_git_reference(ref: str) -> bool:
    """
    Check if reference is a Git reference (not an fsspec URI).

    Args:
        ref: Reference string to check

    Returns:
        True if it looks like a Git reference
    """
    # fsspec URIs start with scheme://
    fsspec_schemes = ('s3://', 'gs://', 'az://', 'abfs://',
                      'http://', 'https://', 'file://', 'memory://')

    if ref.startswith(fsspec_schemes):
        return False

    # Git full URL starts with git+
    if ref.startswith('git+'):
        return True

    # Short form has owner/repo@ref#path pattern
    short_pattern = r'^[^/@]+/[^@]+@[^#]+#.+'
    return bool(re.match(short_pattern, ref))


def is_fsspec_uri(ref: str) -> bool:
    """
    Check if reference is an fsspec URI (not a Git ref).

    Args:
        ref: Reference string to check

    Returns:
        True if it's an fsspec URI
    """
    fsspec_schemes = ('s3://', 'gs://', 'az://', 'abfs://',
                      'http://', 'https://', 'file://', 'memory://')
    return ref.startswith(fsspec_schemes)


class SchemaCache:
    """In-memory cache for fetched schemas with TTL."""

    def __init__(self, ttl: int = DEFAULT_CACHE_TTL):
        self.ttl = ttl
        self._cache: Dict[str, Tuple[Dict[str, Any], float]] = {}

    def get(self, key: str) -> Optional[Dict[str, Any]]:
        """Get cached schema if not expired."""
        if key in self._cache:
            schema, timestamp = self._cache[key]
            if time.time() - timestamp < self.ttl:
                return schema
            else:
                del self._cache[key]
        return None

    def set(self, key: str, schema: Dict[str, Any]) -> None:
        """Cache a schema."""
        self._cache[key] = (schema, time.time())

    def clear(self) -> None:
        """Clear all cached schemas."""
        self._cache.clear()


class GitSchemaFetcher:
    """Fetch schemas from Git repositories."""

    def __init__(
        self,
        cache_dir: Optional[Path] = None,
        cache: Optional[SchemaCache] = None
    ):
        self.cache_dir = cache_dir or Path(tempfile.gettempdir()) / 'tea-schema-cache'
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        self.cache = cache or SchemaCache()

    def fetch(self, ref: GitReference) -> Dict[str, Any]:
        """
        Fetch schema from Git repository.

        Args:
            ref: Parsed Git reference

        Returns:
            Parsed JSON schema

        Raises:
            FileNotFoundError: If schema file not found in repo
            RuntimeError: If Git operations fail
        """
        cache_key = f"git:{ref.host}/{ref.owner}/{ref.repo}@{ref.ref}#{ref.path}"

        # Check cache
        cached = self.cache.get(cache_key)
        if cached is not None:
            return cached

        # Clone/fetch repo
        repo_dir = self._ensure_repo(ref)

        # Read schema file
        schema_path = repo_dir / ref.path
        if not schema_path.exists():
            raise FileNotFoundError(
                f"Schema not found: {ref.path} in {ref.original}"
            )

        content = schema_path.read_text()
        if schema_path.suffix in ('.yaml', '.yml'):
            import yaml
            schema = yaml.safe_load(content)
        else:
            schema = json.loads(content)

        self.cache.set(cache_key, schema)
        return schema

    def _ensure_repo(self, ref: GitReference) -> Path:
        """Ensure repository is cloned and at correct ref."""
        repo_dir = self.cache_dir / ref.host / ref.owner / ref.repo

        if repo_dir.exists():
            # Fetch and checkout
            try:
                self._run_git(['fetch', 'origin', ref.ref], cwd=repo_dir)
                self._run_git(['checkout', f'origin/{ref.ref}'], cwd=repo_dir)
            except RuntimeError:
                # Try checking out as tag or commit directly
                self._run_git(['checkout', ref.ref], cwd=repo_dir)
        else:
            # Clone
            repo_dir.parent.mkdir(parents=True, exist_ok=True)
            clone_url = self._build_clone_url(ref)

            try:
                # Try shallow clone with branch
                self._run_git([
                    'clone', '--depth=1', '-b', ref.ref,
                    clone_url, str(repo_dir)
                ])
            except RuntimeError:
                # If branch clone fails, do full clone and checkout
                self._run_git(['clone', clone_url, str(repo_dir)])
                self._run_git(['checkout', ref.ref], cwd=repo_dir)

        return repo_dir

    def _build_clone_url(self, ref: GitReference) -> str:
        """Build Git clone URL with authentication."""
        if ref.protocol == 'ssh':
            return f"git@{ref.host}:{ref.owner}/{ref.repo}.git"
        else:
            return f"https://{ref.host}/{ref.owner}/{ref.repo}.git"

    def _run_git(self, args: List[str], cwd: Optional[Path] = None) -> str:
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


class FsspecSchemaFetcher:
    """Fetch schemas from any fsspec-compatible storage backend."""

    def __init__(self, cache: Optional[SchemaCache] = None):
        self.cache = cache or SchemaCache()

    def fetch(self, uri: str) -> Dict[str, Any]:
        """
        Fetch schema from fsspec URI.

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

        Raises:
            RuntimeError: If filesystem access fails
            FileNotFoundError: If schema not found
            ValueError: If JSON is invalid
        """
        # Check cache
        cached = self.cache.get(uri)
        if cached is not None:
            return cached

        # Try to use existing core_actions infrastructure
        try:
            from the_edge_agent.actions.core_actions import _get_filesystem, _normalize_path
            use_core_actions = True
        except ImportError:
            use_core_actions = False

        if use_core_actions:
            path = _normalize_path(uri)
            fs, fs_path, err = _get_filesystem(path)
            if err:
                raise RuntimeError(f"Failed to access {uri}: {err['error']}")
        else:
            # Fallback to direct fsspec
            try:
                import fsspec
            except ImportError:
                raise ImportError(
                    "fsspec package not installed. "
                    "Install with: pip install fsspec"
                )

            fs = fsspec.filesystem(uri.split('://')[0])
            fs_path = uri.split('://', 1)[1] if '://' in uri else uri

        # Read and parse schema
        try:
            with fs.open(fs_path, 'r') as f:
                content = f.read()
        except FileNotFoundError:
            raise FileNotFoundError(f"Schema not found: {uri}")

        # Parse based on extension
        if fs_path.endswith(('.yaml', '.yml')):
            import yaml
            schema = yaml.safe_load(content)
        else:
            try:
                schema = json.loads(content)
            except json.JSONDecodeError as e:
                raise ValueError(f"Invalid JSON in schema {uri}: {e}")

        self.cache.set(uri, schema)
        return schema


# Global fetcher instances (with shared cache)
_cache = SchemaCache()
_git_fetcher: Optional[GitSchemaFetcher] = None
_fsspec_fetcher: Optional[FsspecSchemaFetcher] = None


def _get_git_fetcher() -> GitSchemaFetcher:
    """Get or create global Git fetcher."""
    global _git_fetcher
    if _git_fetcher is None:
        _git_fetcher = GitSchemaFetcher(cache=_cache)
    return _git_fetcher


def _get_fsspec_fetcher() -> FsspecSchemaFetcher:
    """Get or create global fsspec fetcher."""
    global _fsspec_fetcher
    if _fsspec_fetcher is None:
        _fsspec_fetcher = FsspecSchemaFetcher(cache=_cache)
    return _fsspec_fetcher


def fetch_schema(ref: str) -> Dict[str, Any]:
    """
    Unified schema fetcher - routes to Git or fsspec based on URI format.

    Args:
        ref: Schema reference (Git ref or fsspec URI)

    Returns:
        Parsed JSON schema

    Raises:
        ValueError: If reference format is invalid
        FileNotFoundError: If schema not found
        RuntimeError: If fetch fails

    Examples:
        >>> # Git short form
        >>> fetch_schema("company/schemas@v1.0.0#invoice.json")

        >>> # Git full URL
        >>> fetch_schema("git+https://github.com/co/schemas.git@main#base.json")

        >>> # S3
        >>> fetch_schema("s3://bucket/schemas/invoice.json")

        >>> # HTTPS
        >>> fetch_schema("https://example.com/schemas/base.json")
    """
    if is_fsspec_uri(ref):
        fetcher = _get_fsspec_fetcher()
        return fetcher.fetch(ref)
    elif is_git_reference(ref):
        fetcher = _get_git_fetcher()
        parsed = parse_git_reference(ref)
        return fetcher.fetch(parsed)
    else:
        raise ValueError(
            f"Unknown reference format: {ref}\n"
            "Expected Git ref (owner/repo@ref#path) or fsspec URI (s3://...)"
        )


def resolve_schema_uses(
    uses: List[str],
    git_fetcher: Optional[GitSchemaFetcher] = None,
    fsspec_fetcher: Optional[FsspecSchemaFetcher] = None
) -> List[Dict[str, Any]]:
    """
    Resolve multiple schema references and return list of schemas.

    Args:
        uses: List of schema references (Git refs and/or fsspec URIs)
        git_fetcher: Optional custom Git fetcher
        fsspec_fetcher: Optional custom fsspec fetcher

    Returns:
        List of parsed schemas in order (for deep merge)

    Raises:
        ValueError: If any reference is invalid
        FileNotFoundError: If any schema not found
    """
    schemas = []

    for ref in uses:
        if is_fsspec_uri(ref):
            fetcher = fsspec_fetcher or _get_fsspec_fetcher()
            schemas.append(fetcher.fetch(ref))
        else:
            fetcher = git_fetcher or _get_git_fetcher()
            parsed = parse_git_reference(ref)
            schemas.append(fetcher.fetch(parsed))

    return schemas


def clear_cache() -> None:
    """Clear the global schema cache."""
    _cache.clear()
