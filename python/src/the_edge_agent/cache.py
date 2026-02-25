"""
Remote file cache manager for TEA CLI.

This module provides caching for remote files fetched via URLs (s3://, gs://, github://, etc.)
to reduce network calls and enable offline mode.

Cache key algorithm:
- For non-git URLs: SHA256(canonical_url)[:16]
- For git URLs: SHA256(f"{provider}://{owner}/{repo}@{ref}/{path}")[:16]
- Branch refs include TTL check, SHA/tags are permanent

Security Mitigations (from QA Risk Profile):
- SEC-001: Credentials are masked in all log output using mask_credentials()
- SEC-002: Path traversal prevention via _validate_path_containment()
- SEC-003: SSRF protection via _validate_url_safe() with protocol whitelist

Cache Structure:
    ~/.cache/tea/remote/
    ├── manifest.json          # URL -> cache entry mapping with TTL
    └── files/
        └── {cache_key}/       # One directory per cached URL
            └── {filename}     # Actual cached file

Environment Variables:
    TEA_CACHE_TTL: Cache TTL in seconds (default: 3600 = 1 hour)
    TEA_CACHE_MAX_SIZE: Maximum cache size in bytes (default: 1GB)
    TEA_FETCH_TIMEOUT: Network fetch timeout in seconds (default: 30)
    XDG_CACHE_HOME: Base cache directory (default: ~/.cache)
"""

import hashlib
import json
import os
import re
import time
import logging
import ipaddress
from pathlib import Path
from datetime import datetime, timedelta
from typing import Any, Callable, Dict, List, Optional, Set, Tuple
from urllib.parse import urlparse

logger = logging.getLogger(__name__)

# Default configuration
DEFAULT_TTL_SECONDS = 3600  # 1 hour
DEFAULT_MAX_SIZE_BYTES = 1024 * 1024 * 1024  # 1GB
DEFAULT_FETCH_TIMEOUT = 30  # seconds
MANIFEST_VERSION = 1

# Allowed protocols (SEC-003: SSRF protection whitelist)
ALLOWED_PROTOCOLS: Set[str] = {
    "s3",
    "gs",
    "gcs",
    "az",
    "abfs",
    "abfss",
    "github",
    "gitlab",
    "https",
    "http",  # Allow http but warn
    "file",  # Local files (with restrictions)
}

# Git reference patterns
GIT_SHA_PATTERN = re.compile(r"^[0-9a-f]{7,40}$", re.IGNORECASE)
GIT_TAG_PATTERN = re.compile(r"^v?\d+\.\d+(\.\d+)?(-[a-zA-Z0-9.]+)?$")


class CacheError(Exception):
    """Base exception for cache operations."""

    pass


class SecurityError(CacheError):
    """Exception raised for security violations (path traversal, SSRF, etc.)."""

    pass


def mask_credentials(text: str) -> str:
    """
    Mask sensitive credentials in text for safe logging (SEC-001).

    Masks:
    - Environment variable values that match sensitive patterns
    - Tokens in URLs (e.g., github://...?token=xxx)
    - Bearer tokens in Authorization headers

    Args:
        text: Text that may contain credentials

    Returns:
        Text with credentials replaced by '***'
    """
    if not text:
        return text

    result = text

    # Mask URL query parameters with sensitive names
    # Pattern: key=value in query strings
    result = re.sub(
        r"([?&])(token|key|secret|password|auth|credential|api_key)=([^&\s]+)",
        r"\1\2=***",
        result,
        flags=re.IGNORECASE,
    )

    # Mask Bearer tokens
    result = re.sub(
        r"(Bearer\s+)([A-Za-z0-9_.-]+)", r"\1***", result, flags=re.IGNORECASE
    )

    # Mask common token patterns in URLs (e.g., ghp_xxx for GitHub)
    result = re.sub(r"(ghp_|gho_|ghr_|ghs_|github_pat_)[A-Za-z0-9_]+", r"\1***", result)

    # Mask gitlab tokens
    result = re.sub(r"(glpat-)[A-Za-z0-9_-]+", r"\1***", result)

    return result


def _is_private_ip(hostname: str) -> bool:
    """
    Check if hostname resolves to a private/internal IP (SEC-003).

    Args:
        hostname: Hostname or IP address

    Returns:
        True if private/internal, False otherwise
    """
    try:
        # Try parsing as IP directly
        ip = ipaddress.ip_address(hostname)
        return ip.is_private or ip.is_loopback or ip.is_reserved or ip.is_link_local
    except ValueError:
        # Not an IP, check for localhost patterns
        hostname_lower = hostname.lower()
        if hostname_lower in ("localhost", "127.0.0.1", "::1", "0.0.0.0"):
            return True
        # Check for internal naming patterns
        if hostname_lower.endswith(".local") or hostname_lower.endswith(".internal"):
            return True
        # AWS metadata endpoint
        if hostname_lower == "169.254.169.254":
            return True
        return False


def _validate_url_safe(
    url: str, allowed_protocols: Optional[Set[str]] = None
) -> Tuple[bool, str]:
    """
    Validate URL is safe to fetch (SEC-003: SSRF protection).

    Args:
        url: URL to validate
        allowed_protocols: Set of allowed protocol schemes

    Returns:
        Tuple of (is_safe, error_message)
    """
    if allowed_protocols is None:
        allowed_protocols = ALLOWED_PROTOCOLS

    try:
        parsed = urlparse(url)
    except Exception as e:
        return False, f"Invalid URL format: {e}"

    # Check protocol
    protocol = parsed.scheme.lower()
    if protocol not in allowed_protocols:
        return (
            False,
            f"Protocol '{protocol}' not in allowed list: {sorted(allowed_protocols)}",
        )

    # For http/https/file, check for SSRF
    if protocol in ("http", "https"):
        hostname = parsed.hostname or ""
        if _is_private_ip(hostname):
            return False, f"Internal/private IP addresses not permitted: {hostname}"

        # Block AWS metadata endpoint explicitly
        if hostname == "169.254.169.254":
            return False, "Cloud metadata endpoints are blocked for security"

    # For file:// protocol, restrict to relative paths or explicit local paths
    if protocol == "file":
        # Block localhost file:// URLs
        if parsed.hostname and parsed.hostname.lower() in ("localhost", "127.0.0.1"):
            return False, "localhost file:// URLs not permitted"

    return True, ""


def _validate_path_containment(resolved_path: Path, base_dir: Path) -> bool:
    """
    Validate that resolved_path is contained within base_dir (SEC-002).

    Prevents path traversal attacks like '../../../etc/passwd'.

    Args:
        resolved_path: Fully resolved path to check
        base_dir: Base directory that must contain the path

    Returns:
        True if path is safely contained, False otherwise
    """
    try:
        resolved_path = resolved_path.resolve()
        base_dir = base_dir.resolve()
        # Check if resolved path starts with base directory
        return str(resolved_path).startswith(str(base_dir))
    except (OSError, ValueError):
        return False


def _sanitize_filename(filename: str) -> str:
    """
    Sanitize filename to prevent path traversal (SEC-002).

    Args:
        filename: Original filename

    Returns:
        Sanitized filename safe for filesystem
    """
    # Remove path separators
    filename = filename.replace("/", "_").replace("\\", "_")
    # Remove parent directory references
    filename = filename.replace("..", "__")
    # Remove null bytes
    filename = filename.replace("\x00", "")
    # Limit length
    if len(filename) > 200:
        filename = filename[:200]
    # Ensure not empty
    if not filename:
        filename = "unnamed"
    return filename


def is_url(path: str) -> bool:
    """
    Check if path is a URL (has protocol scheme).

    Args:
        path: Path or URL string

    Returns:
        True if path has a URL scheme, False for local paths
    """
    if not path:
        return False
    # Check for common URL schemes
    return "://" in path and not path.startswith("file://")


def is_git_permanent_ref(ref: str) -> bool:
    """
    Check if a git reference is permanent (SHA or tag) vs mutable (branch).

    Permanent refs (SHA, tags) can be cached indefinitely.
    Mutable refs (branches) should respect TTL.

    Args:
        ref: Git reference (branch name, tag, or commit SHA)

    Returns:
        True if reference is permanent (cacheable indefinitely)
    """
    if not ref:
        return False
    # Full or partial SHA
    if GIT_SHA_PATTERN.match(ref):
        return True
    # Semantic version tags
    if GIT_TAG_PATTERN.match(ref):
        return True
    return False


def parse_git_url(url: str) -> Optional[Dict[str, str]]:
    """
    Parse git:// style URLs into components.

    Supported formats:
    - github://user/repo@ref/path/to/file
    - gitlab://user/repo@ref/path/to/file

    Args:
        url: Git URL to parse

    Returns:
        Dict with keys: provider, owner, repo, ref, path
        None if not a valid git URL
    """
    for provider in ("github", "gitlab"):
        prefix = f"{provider}://"
        if url.startswith(prefix):
            remainder = url[len(prefix) :]

            # Split on @ to get ref
            if "@" in remainder:
                repo_part, rest = remainder.split("@", 1)
                if "/" in rest:
                    ref, file_path = rest.split("/", 1)
                else:
                    ref = rest
                    file_path = ""
            else:
                # No ref specified, default to main
                ref = "main"
                if "/" in remainder:
                    parts = remainder.split("/", 2)
                    if len(parts) >= 3:
                        repo_part = f"{parts[0]}/{parts[1]}"
                        file_path = parts[2]
                    else:
                        repo_part = remainder
                        file_path = ""
                else:
                    repo_part = remainder
                    file_path = ""

            # Parse owner/repo
            if "/" in repo_part:
                owner, repo = repo_part.split("/", 1)
            else:
                return None  # Invalid format

            return {
                "provider": provider,
                "owner": owner,
                "repo": repo,
                "ref": ref,
                "path": file_path,
            }

    return None


def parse_duration(duration_str: str) -> timedelta:
    """
    Parse duration string to timedelta.

    Supported formats:
    - "7d" - 7 days
    - "24h" - 24 hours
    - "30m" - 30 minutes
    - "3600" or "3600s" - 3600 seconds

    Args:
        duration_str: Duration string

    Returns:
        timedelta object

    Raises:
        ValueError: If format is invalid
    """
    duration_str = duration_str.strip().lower()

    if duration_str.endswith("d"):
        return timedelta(days=int(duration_str[:-1]))
    elif duration_str.endswith("h"):
        return timedelta(hours=int(duration_str[:-1]))
    elif duration_str.endswith("m"):
        return timedelta(minutes=int(duration_str[:-1]))
    elif duration_str.endswith("s"):
        return timedelta(seconds=int(duration_str[:-1]))
    else:
        # Assume seconds if no unit
        return timedelta(seconds=int(duration_str))


class RemoteFileCache:
    """
    Manager for caching remote files fetched via URLs.

    Provides:
    - XDG-compliant cache directory (~/.cache/tea/remote/)
    - SHA256-based cache keys
    - TTL-based expiration for mutable refs
    - Manifest tracking with metadata
    - Path traversal protection
    - Credential masking in logs

    Example:
        >>> cache = RemoteFileCache()
        >>> if cache.has_valid("s3://bucket/file.yaml"):
        ...     path = cache.get_path("s3://bucket/file.yaml")
        ... else:
        ...     path = cache.fetch_and_cache(fs, fs_path, "s3://bucket/file.yaml")
    """

    def __init__(
        self,
        cache_dir: Optional[Path] = None,
        ttl_seconds: Optional[int] = None,
        max_size_bytes: Optional[int] = None,
        fetch_timeout: Optional[int] = None,
        verbose: bool = False,
    ):
        """
        Initialize the cache manager.

        Args:
            cache_dir: Override cache directory (default: XDG cache dir)
            ttl_seconds: Cache TTL (default: TEA_CACHE_TTL env or 3600)
            max_size_bytes: Max cache size (default: TEA_CACHE_MAX_SIZE env or 1GB)
            fetch_timeout: Fetch timeout (default: TEA_FETCH_TIMEOUT env or 30s)
            verbose: Enable verbose logging
        """
        self.cache_dir = cache_dir or self._default_cache_dir()
        self.files_dir = self.cache_dir / "files"
        self.manifest_path = self.cache_dir / "manifest.json"

        # Load configuration from environment or use defaults
        self.ttl_seconds = ttl_seconds or int(
            os.environ.get("TEA_CACHE_TTL", DEFAULT_TTL_SECONDS)
        )
        self.max_size_bytes = max_size_bytes or self._parse_size(
            os.environ.get("TEA_CACHE_MAX_SIZE", str(DEFAULT_MAX_SIZE_BYTES))
        )
        self.fetch_timeout = fetch_timeout or int(
            os.environ.get("TEA_FETCH_TIMEOUT", DEFAULT_FETCH_TIMEOUT)
        )
        self.verbose = verbose

        # Ensure directories exist
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        self.files_dir.mkdir(parents=True, exist_ok=True)

    @staticmethod
    def _default_cache_dir() -> Path:
        """Get XDG-compliant cache directory."""
        xdg_cache = os.environ.get("XDG_CACHE_HOME")
        if xdg_cache:
            base = Path(xdg_cache)
        else:
            base = Path.home() / ".cache"
        return base / "tea" / "remote"

    @staticmethod
    def _parse_size(size_str: str) -> int:
        """Parse size string (e.g., '1GB', '500MB') to bytes."""
        size_str = size_str.strip().upper()
        multipliers = {
            "B": 1,
            "KB": 1024,
            "MB": 1024 * 1024,
            "GB": 1024 * 1024 * 1024,
            "TB": 1024 * 1024 * 1024 * 1024,
        }
        for suffix, mult in multipliers.items():
            if size_str.endswith(suffix):
                return int(size_str[: -len(suffix)]) * mult
        return int(size_str)  # Assume bytes

    def _cache_key(self, url: str) -> str:
        """
        Generate cache key from URL.

        For git URLs, uses repo@ref/path for per-file caching.
        For other URLs, uses the full canonical URL.

        Args:
            url: URL to generate key for

        Returns:
            16-character hex string
        """
        git_parts = parse_git_url(url)
        if git_parts:
            # Use repo@ref/path for git URLs (per-file caching)
            key_source = f"{git_parts['provider']}://{git_parts['owner']}/{git_parts['repo']}@{git_parts['ref']}/{git_parts['path']}"
        else:
            # Use full URL for other protocols
            key_source = url

        return hashlib.sha256(key_source.encode()).hexdigest()[:16]

    def _load_manifest(self) -> Dict[str, Any]:
        """Load manifest from disk or create empty one."""
        if self.manifest_path.exists():
            try:
                with open(self.manifest_path, "r") as f:
                    data = json.load(f)
                    if data.get("version") == MANIFEST_VERSION:
                        return data
            except (json.JSONDecodeError, OSError) as e:
                logger.warning(f"Failed to load manifest: {e}, creating new one")

        return {"version": MANIFEST_VERSION, "entries": {}}

    def _save_manifest(self, manifest: Dict[str, Any]) -> None:
        """Save manifest to disk atomically."""
        # Write to temp file first, then rename for atomicity
        temp_path = self.manifest_path.with_suffix(".tmp")
        try:
            with open(temp_path, "w") as f:
                json.dump(manifest, f, indent=2)
            temp_path.rename(self.manifest_path)
        except OSError as e:
            logger.error(f"Failed to save manifest: {e}")
            if temp_path.exists():
                temp_path.unlink()
            raise CacheError(f"Failed to save manifest: {e}")

    def _is_expired(self, entry: Dict[str, Any]) -> bool:
        """Check if cache entry has expired based on TTL."""
        if entry.get("is_permanent", False):
            return False

        created_at = entry.get("created_at", 0)
        ttl = entry.get("ttl_seconds", self.ttl_seconds)

        return time.time() > (created_at + ttl)

    def has_valid(self, url: str, ttl: Optional[int] = None) -> bool:
        """
        Check if valid (non-expired) cache entry exists.

        Args:
            url: URL to check
            ttl: Override TTL for this check

        Returns:
            True if valid cache entry exists
        """
        cache_key = self._cache_key(url)
        manifest = self._load_manifest()

        entry = manifest.get("entries", {}).get(cache_key)
        if not entry:
            return False

        # Check if file exists
        local_path = self.cache_dir / entry.get("local_path", "")
        if not local_path.exists():
            return False

        # Check TTL
        if ttl is not None:
            entry = entry.copy()
            entry["ttl_seconds"] = ttl

        return not self._is_expired(entry)

    def has_expired(self, url: str) -> bool:
        """
        Check if an expired cache entry exists (for --cache-only hint).

        Args:
            url: URL to check

        Returns:
            True if expired entry exists (can suggest --cache-only)
        """
        cache_key = self._cache_key(url)
        manifest = self._load_manifest()

        entry = manifest.get("entries", {}).get(cache_key)
        if not entry:
            return False

        # Check if file exists
        local_path = self.cache_dir / entry.get("local_path", "")
        if not local_path.exists():
            return False

        return self._is_expired(entry)

    def get_path(self, url: str) -> Path:
        """
        Get local path for cached URL.

        Args:
            url: URL to get path for

        Returns:
            Path to cached file

        Raises:
            CacheError: If entry not found
        """
        cache_key = self._cache_key(url)
        manifest = self._load_manifest()

        entry = manifest.get("entries", {}).get(cache_key)
        if not entry:
            raise CacheError(f"No cache entry for URL: {mask_credentials(url)}")

        local_path = self.cache_dir / entry["local_path"]

        # Security check (SEC-002)
        if not _validate_path_containment(local_path, self.cache_dir):
            raise SecurityError(
                f"Path traversal detected in cached path: {mask_credentials(str(local_path))}"
            )

        if not local_path.exists():
            raise CacheError(f"Cached file missing: {mask_credentials(url)}")

        return local_path

    def fetch_and_cache(
        self,
        fs: Any,
        fs_path: str,
        url: str,
        progress_callback: Optional[Callable[[int, int], None]] = None,
    ) -> Path:
        """
        Fetch from remote filesystem and cache locally.

        Args:
            fs: fsspec filesystem instance
            fs_path: Path within the filesystem
            url: Original URL (for cache key)
            progress_callback: Optional callback(bytes_done, total_bytes)

        Returns:
            Path to cached file

        Raises:
            SecurityError: If URL fails security validation
            CacheError: If fetch or cache operation fails
        """
        # Security validation (SEC-003)
        is_safe, error = _validate_url_safe(url)
        if not is_safe:
            raise SecurityError(f"URL security check failed: {error}")

        cache_key = self._cache_key(url)

        # Determine filename from URL
        if "/" in fs_path:
            filename = fs_path.rsplit("/", 1)[-1]
        else:
            filename = fs_path or "file"

        filename = _sanitize_filename(filename)

        # Create cache directory for this entry
        entry_dir = self.files_dir / cache_key
        entry_dir.mkdir(parents=True, exist_ok=True)

        local_path = entry_dir / filename

        # Validate containment (SEC-002)
        if not _validate_path_containment(local_path, self.cache_dir):
            raise SecurityError(
                f"Path traversal attempt detected for: {mask_credentials(url)}"
            )

        # Fetch file with progress
        try:
            if self.verbose:
                logger.info(f"Fetching: {mask_credentials(url)}")

            # Get file size for progress
            try:
                file_info = fs.info(fs_path)
                total_size = file_info.get("size", 0)
            except (AttributeError, KeyError):
                total_size = 0

            # Fetch content
            with fs.open(fs_path, "rb") as remote_f:
                with open(local_path, "wb") as local_f:
                    bytes_done = 0
                    while True:
                        chunk = remote_f.read(8192)
                        if not chunk:
                            break
                        local_f.write(chunk)
                        bytes_done += len(chunk)
                        if progress_callback and total_size > 0:
                            progress_callback(bytes_done, total_size)

            if self.verbose:
                logger.info(f"Cached: {mask_credentials(url)} -> {local_path}")

        except Exception as e:
            # Clean up partial download
            if local_path.exists():
                local_path.unlink()
            raise CacheError(f"Failed to fetch {mask_credentials(url)}: {e}")

        # Determine if permanent cache (git SHA/tag)
        git_parts = parse_git_url(url)
        is_permanent = git_parts and is_git_permanent_ref(git_parts["ref"])

        # Update manifest
        manifest = self._load_manifest()
        manifest["entries"][cache_key] = {
            "url": url,
            "local_path": f"files/{cache_key}/{filename}",
            "created_at": time.time(),
            "ttl_seconds": self.ttl_seconds,
            "is_permanent": is_permanent,
            "size_bytes": local_path.stat().st_size,
        }
        self._save_manifest(manifest)

        # Enforce cache size limit
        self._enforce_size_limit()

        return local_path

    def _enforce_size_limit(self) -> None:
        """Enforce cache size limit by removing oldest entries."""
        manifest = self._load_manifest()
        entries = manifest.get("entries", {})

        # Calculate total size
        total_size = sum(e.get("size_bytes", 0) for e in entries.values())

        if total_size <= self.max_size_bytes:
            return

        # Sort by creation time, oldest first
        sorted_entries = sorted(
            entries.items(), key=lambda x: x[1].get("created_at", 0)
        )

        # Remove oldest until under limit
        removed = 0
        for cache_key, entry in sorted_entries:
            if total_size <= self.max_size_bytes:
                break

            local_path = self.cache_dir / entry.get("local_path", "")
            size = entry.get("size_bytes", 0)

            try:
                if local_path.exists():
                    local_path.unlink()
                # Remove entry directory if empty
                entry_dir = local_path.parent
                if entry_dir.exists() and not any(entry_dir.iterdir()):
                    entry_dir.rmdir()

                del manifest["entries"][cache_key]
                total_size -= size
                removed += 1
            except OSError as e:
                logger.warning(f"Failed to remove cache entry: {e}")

        if removed > 0:
            self._save_manifest(manifest)
            logger.info(f"Removed {removed} cache entries to enforce size limit")

    def clear(self, older_than: Optional[timedelta] = None) -> int:
        """
        Clear cache entries, optionally filtering by age.

        Args:
            older_than: Only clear entries older than this duration

        Returns:
            Number of entries cleared
        """
        manifest = self._load_manifest()
        entries = manifest.get("entries", {})

        cutoff_time = None
        if older_than:
            cutoff_time = time.time() - older_than.total_seconds()

        removed = 0
        keys_to_remove = []

        for cache_key, entry in entries.items():
            # Check age filter
            if cutoff_time and entry.get("created_at", 0) > cutoff_time:
                continue

            local_path = self.cache_dir / entry.get("local_path", "")

            try:
                if local_path.exists():
                    local_path.unlink()
                # Remove entry directory if empty
                entry_dir = local_path.parent
                if entry_dir.exists() and not any(entry_dir.iterdir()):
                    entry_dir.rmdir()

                keys_to_remove.append(cache_key)
                removed += 1
            except OSError as e:
                logger.warning(f"Failed to remove cache entry: {e}")

        for key in keys_to_remove:
            del manifest["entries"][key]

        self._save_manifest(manifest)
        return removed

    def list_entries(self) -> List[Dict[str, Any]]:
        """
        List all cache entries with metadata.

        Returns:
            List of entry dicts with: url, size, age, expired, path
        """
        manifest = self._load_manifest()
        entries = manifest.get("entries", {})

        result = []
        now = time.time()

        for cache_key, entry in entries.items():
            local_path = self.cache_dir / entry.get("local_path", "")
            created_at = entry.get("created_at", 0)

            result.append(
                {
                    "url": entry.get("url", ""),
                    "cache_key": cache_key,
                    "size_bytes": entry.get("size_bytes", 0),
                    "created_at": datetime.fromtimestamp(created_at).isoformat(),
                    "age_seconds": int(now - created_at),
                    "expired": self._is_expired(entry),
                    "is_permanent": entry.get("is_permanent", False),
                    "local_path": str(local_path),
                    "exists": local_path.exists(),
                }
            )

        # Sort by creation time, newest first
        result.sort(key=lambda x: x["age_seconds"])
        return result

    def info(self) -> Dict[str, Any]:
        """
        Get cache statistics.

        Returns:
            Dict with: location, total_size, entry_count, max_size
        """
        manifest = self._load_manifest()
        entries = manifest.get("entries", {})

        total_size = sum(e.get("size_bytes", 0) for e in entries.values())
        valid_count = sum(1 for e in entries.values() if not self._is_expired(e))
        expired_count = len(entries) - valid_count

        return {
            "location": str(self.cache_dir),
            "manifest_path": str(self.manifest_path),
            "total_entries": len(entries),
            "valid_entries": valid_count,
            "expired_entries": expired_count,
            "total_size_bytes": total_size,
            "total_size_human": self._format_size(total_size),
            "max_size_bytes": self.max_size_bytes,
            "max_size_human": self._format_size(self.max_size_bytes),
            "usage_percent": round(total_size / self.max_size_bytes * 100, 1)
            if self.max_size_bytes > 0
            else 0,
            "ttl_seconds": self.ttl_seconds,
            "fetch_timeout_seconds": self.fetch_timeout,
        }

    @staticmethod
    def _format_size(size_bytes: int) -> str:
        """Format size in human-readable form."""
        size_float = float(size_bytes)
        for unit in ("B", "KB", "MB", "GB", "TB"):
            if size_float < 1024:
                return f"{size_float:.1f} {unit}"
            size_float /= 1024
        return f"{size_float:.1f} PB"
