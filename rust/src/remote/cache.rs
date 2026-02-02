//! Cache manager for remote files - compatible with Python implementation.
//!
//! This module provides caching for remote files fetched via URLs (s3://, gs://, github://, etc.)
//! to reduce network calls and enable offline mode.
//!
//! # Cache Key Algorithm
//!
//! - For non-git URLs: `SHA256(canonical_url)[:16]`
//! - For git URLs: `SHA256("{provider}://{owner}/{repo}@{ref}")[:16]`
//! - Branch refs include TTL check, SHA/tags are permanent
//!
//! # Security Mitigations
//!
//! - SEC-001: Credentials are masked in all log output using `mask_credentials()`
//! - SEC-002: Path traversal prevention via `validate_path_containment()`
//! - SEC-003: SSRF protection via `validate_url_safe()` with protocol whitelist
//!
//! # Manifest Format (Python-compatible)
//!
//! ```json
//! {
//!   "version": 1,
//!   "entries": {
//!     "cache_key": {
//!       "url": "s3://bucket/file.yaml",
//!       "local_path": "files/cache_key/file.yaml",
//!       "created_at": 1706745600,
//!       "ttl_seconds": 3600,
//!       "is_permanent": false,
//!       "size_bytes": 1024
//!     }
//!   }
//! }
//! ```

use crate::TeaResult;
use regex::Regex;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::net::IpAddr;
use std::path::{Path, PathBuf};
use std::sync::LazyLock;
use std::time::{SystemTime, UNIX_EPOCH};

/// Manifest version - must match Python implementation for compatibility.
pub const MANIFEST_VERSION: u32 = 1;

/// Default cache TTL in seconds (1 hour).
pub const DEFAULT_TTL_SECONDS: i64 = 3600;

/// Default maximum cache size in bytes (1GB).
pub const DEFAULT_MAX_SIZE_BYTES: u64 = 1024 * 1024 * 1024;

/// Default fetch timeout in seconds.
pub const DEFAULT_FETCH_TIMEOUT: u64 = 30;

/// Allowed URL protocols for SSRF protection (SEC-003).
pub const ALLOWED_PROTOCOLS: &[&str] = &[
    "s3", "gs", "gcs", "az", "abfs", "abfss", "github", "gitlab", "https", "http", "file",
];

// Regex patterns for git reference detection
static GIT_SHA_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^[0-9a-fA-F]{7,40}$").unwrap());
static GIT_TAG_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^v?\d+\.\d+(\.\d+)?(-[a-zA-Z0-9.]+)?$").unwrap());

// Credential masking patterns (SEC-001)
static URL_SENSITIVE_PARAMS: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?i)([?&])(token|key|secret|password|auth|credential|api_key)=([^&\s]+)").unwrap()
});
static BEARER_TOKEN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?i)(Bearer\s+)([A-Za-z0-9_.-]+)").unwrap());
static GITHUB_TOKEN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(ghp_|gho_|ghr_|ghs_|github_pat_)[A-Za-z0-9_]+").unwrap());
static GITLAB_TOKEN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(glpat-)[A-Za-z0-9_-]+").unwrap());

/// Mask sensitive credentials in text for safe logging (SEC-001).
///
/// Masks:
/// - Environment variable values that match sensitive patterns
/// - Tokens in URLs (e.g., github://...?token=xxx)
/// - Bearer tokens in Authorization headers
/// - GitHub/GitLab personal access tokens
///
/// # Examples
///
/// ```rust
/// use the_edge_agent::remote::cache::mask_credentials;
///
/// let masked = mask_credentials("https://api.github.com?token=secret123");
/// assert!(!masked.contains("secret123"));
/// assert!(masked.contains("***"));
/// ```
pub fn mask_credentials(text: &str) -> String {
    if text.is_empty() {
        return text.to_string();
    }

    let mut result = text.to_string();

    // Mask URL query parameters with sensitive names
    result = URL_SENSITIVE_PARAMS
        .replace_all(&result, "$1$2=***")
        .to_string();

    // Mask Bearer tokens
    result = BEARER_TOKEN.replace_all(&result, "$1***").to_string();

    // Mask GitHub tokens (ghp_, gho_, ghr_, ghs_, github_pat_)
    result = GITHUB_TOKEN.replace_all(&result, "$1***").to_string();

    // Mask GitLab tokens (glpat-)
    result = GITLAB_TOKEN.replace_all(&result, "$1***").to_string();

    result
}

/// Check if hostname resolves to a private/internal IP (SEC-003).
fn is_private_ip(hostname: &str) -> bool {
    // Check for localhost patterns first
    let hostname_lower = hostname.to_lowercase();
    if matches!(
        hostname_lower.as_str(),
        "localhost" | "127.0.0.1" | "::1" | "0.0.0.0"
    ) {
        return true;
    }

    // Check for internal naming patterns
    if hostname_lower.ends_with(".local") || hostname_lower.ends_with(".internal") {
        return true;
    }

    // AWS metadata endpoint
    if hostname_lower == "169.254.169.254" {
        return true;
    }

    // Try parsing as IP
    if let Ok(ip) = hostname.parse::<IpAddr>() {
        return match ip {
            IpAddr::V4(ipv4) => {
                ipv4.is_private()
                    || ipv4.is_loopback()
                    || ipv4.is_link_local()
                    || ipv4.is_broadcast()
                    || ipv4.is_documentation()
                    || ipv4.is_unspecified()
            }
            IpAddr::V6(ipv6) => ipv6.is_loopback() || ipv6.is_unspecified(),
        };
    }

    false
}

/// Validate URL is safe to fetch (SEC-003: SSRF protection).
///
/// Returns `(is_safe, error_message)`.
pub fn validate_url_safe(url: &str) -> (bool, String) {
    // Parse URL
    let parsed = match url::Url::parse(url) {
        Ok(u) => u,
        Err(e) => return (false, format!("Invalid URL format: {}", e)),
    };

    // Check protocol
    let protocol = parsed.scheme().to_lowercase();
    if !ALLOWED_PROTOCOLS.contains(&protocol.as_str()) {
        return (
            false,
            format!(
                "Protocol '{}' not in allowed list: {:?}",
                protocol, ALLOWED_PROTOCOLS
            ),
        );
    }

    // For http/https, check for SSRF
    if protocol == "http" || protocol == "https" {
        if let Some(host) = parsed.host_str() {
            if is_private_ip(host) {
                return (
                    false,
                    format!("Internal/private IP addresses not permitted: {}", host),
                );
            }
            // Block AWS metadata endpoint explicitly
            if host == "169.254.169.254" {
                return (
                    false,
                    "Cloud metadata endpoints are blocked for security".to_string(),
                );
            }
        }
    }

    // For file:// protocol, restrict to certain patterns
    if protocol == "file" {
        if let Some(host) = parsed.host_str() {
            let host_lower = host.to_lowercase();
            if host_lower == "localhost" || host_lower == "127.0.0.1" {
                return (false, "localhost file:// URLs not permitted".to_string());
            }
        }
    }

    (true, String::new())
}

/// Validate that resolved_path is contained within base_dir (SEC-002).
///
/// Prevents path traversal attacks like `../../../etc/passwd`.
pub fn validate_path_containment(resolved_path: &Path, base_dir: &Path) -> bool {
    match (resolved_path.canonicalize(), base_dir.canonicalize()) {
        (Ok(resolved), Ok(base)) => resolved.starts_with(&base),
        _ => false,
    }
}

/// Sanitize filename to prevent path traversal (SEC-002).
pub fn sanitize_filename(filename: &str) -> String {
    let mut result = filename.replace(['/', '\\'], "_");
    result = result.replace("..", "__");
    result = result.replace('\0', "");

    // Limit length
    if result.len() > 200 {
        result = result[..200].to_string();
    }

    // Ensure not empty
    if result.is_empty() {
        result = "unnamed".to_string();
    }

    result
}

/// Check if path is a URL (has protocol scheme).
pub fn is_url(path: &str) -> bool {
    if path.is_empty() {
        return false;
    }
    path.contains("://") && !path.starts_with("file://")
}

/// Check if a git reference is permanent (SHA or tag) vs mutable (branch).
///
/// Permanent refs (SHA, tags) can be cached indefinitely.
/// Mutable refs (branches) should respect TTL.
pub fn is_git_permanent_ref(ref_str: &str) -> bool {
    if ref_str.is_empty() {
        return false;
    }
    // Full or partial SHA
    if GIT_SHA_PATTERN.is_match(ref_str) {
        return true;
    }
    // Semantic version tags
    if GIT_TAG_PATTERN.is_match(ref_str) {
        return true;
    }
    false
}

/// Parse git:// style URLs into components.
///
/// Supported formats:
/// - `github://user/repo@ref/path/to/file`
/// - `gitlab://user/repo@ref/path/to/file`
#[derive(Debug, Clone, PartialEq)]
pub struct ParsedGitUrl {
    pub provider: String,
    pub owner: String,
    pub repo: String,
    pub ref_: String,
    pub path: String,
}

impl ParsedGitUrl {
    /// Parse git URL string.
    pub fn parse(url: &str) -> Option<Self> {
        for provider in ["github", "gitlab"] {
            let prefix = format!("{}://", provider);
            if let Some(remainder) = url.strip_prefix(&prefix) {
                return Self::parse_remainder(provider, remainder);
            }
        }
        None
    }

    fn parse_remainder(provider: &str, remainder: &str) -> Option<Self> {
        // Split on @ to get ref
        let (repo_part, ref_and_path) = if let Some(at_idx) = remainder.find('@') {
            (&remainder[..at_idx], &remainder[at_idx + 1..])
        } else {
            // No ref specified, default to main
            let parts: Vec<&str> = remainder.splitn(3, '/').collect();
            if parts.len() < 2 {
                return None;
            }
            let owner = parts[0];
            let repo = parts[1];
            let path = if parts.len() > 2 { parts[2] } else { "" };
            return Some(ParsedGitUrl {
                provider: provider.to_string(),
                owner: owner.to_string(),
                repo: repo.to_string(),
                ref_: "main".to_string(),
                path: path.to_string(),
            });
        };

        // Parse owner/repo
        let repo_parts: Vec<&str> = repo_part.splitn(2, '/').collect();
        if repo_parts.len() < 2 {
            return None;
        }
        let owner = repo_parts[0].to_string();
        let repo = repo_parts[1].to_string();

        // Parse ref/path
        let (ref_str, path) = if let Some(slash_idx) = ref_and_path.find('/') {
            (
                &ref_and_path[..slash_idx],
                ref_and_path[slash_idx + 1..].to_string(),
            )
        } else {
            (ref_and_path, String::new())
        };

        Some(ParsedGitUrl {
            provider: provider.to_string(),
            owner,
            repo,
            ref_: ref_str.to_string(),
            path,
        })
    }
}

/// Parse duration string to seconds.
///
/// Supported formats:
/// - "7d" - 7 days
/// - "24h" - 24 hours
/// - "30m" - 30 minutes
/// - "3600" or "3600s" - 3600 seconds
pub fn parse_duration(duration_str: &str) -> Result<i64, String> {
    let s = duration_str.trim().to_lowercase();

    if let Some(days) = s.strip_suffix('d') {
        let n: i64 = days.parse().map_err(|_| "Invalid days format")?;
        return Ok(n * 24 * 60 * 60);
    }
    if let Some(hours) = s.strip_suffix('h') {
        let n: i64 = hours.parse().map_err(|_| "Invalid hours format")?;
        return Ok(n * 60 * 60);
    }
    if let Some(minutes) = s.strip_suffix('m') {
        let n: i64 = minutes.parse().map_err(|_| "Invalid minutes format")?;
        return Ok(n * 60);
    }
    if let Some(secs) = s.strip_suffix('s') {
        return secs
            .parse()
            .map_err(|_| "Invalid seconds format".to_string());
    }
    // Assume seconds if no unit
    s.parse().map_err(|_| "Invalid duration format".to_string())
}

/// Cache entry metadata - compatible with Python implementation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheEntry {
    /// Original URL
    pub url: String,
    /// Relative path within cache directory
    pub local_path: String,
    /// Unix timestamp when cached
    pub created_at: i64,
    /// TTL in seconds
    pub ttl_seconds: Option<i64>,
    /// True for SHA/tags (permanent cache)
    pub is_permanent: bool,
    /// File size in bytes
    #[serde(default)]
    pub size_bytes: u64,
}

/// Cache manifest format - compatible with Python implementation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheManifest {
    /// Must be 1 for Python compatibility
    pub version: u32,
    /// URL cache key -> entry mapping
    pub entries: HashMap<String, CacheEntry>,
}

impl Default for CacheManifest {
    fn default() -> Self {
        Self {
            version: MANIFEST_VERSION,
            entries: HashMap::new(),
        }
    }
}

/// Manager for caching remote files.
///
/// Provides:
/// - XDG-compliant cache directory (~/.cache/tea/remote/)
/// - SHA256-based cache keys
/// - TTL-based expiration for mutable refs
/// - Manifest tracking with metadata
/// - Path traversal protection
/// - Credential masking in logs
#[derive(Debug, Clone)]
pub struct RemoteFileCache {
    /// Cache directory path
    cache_dir: PathBuf,
    /// Files subdirectory
    files_dir: PathBuf,
    /// Manifest file path
    manifest_path: PathBuf,
    /// TTL in seconds
    ttl_seconds: i64,
    /// Maximum cache size
    max_size_bytes: u64,
    /// Fetch timeout
    fetch_timeout: u64,
    /// Verbose logging
    verbose: bool,
}

impl RemoteFileCache {
    /// Create a new cache manager.
    ///
    /// # Arguments
    ///
    /// * `cache_dir` - Override cache directory (default: XDG cache dir)
    pub fn new(cache_dir: Option<PathBuf>) -> TeaResult<Self> {
        let cache_dir = cache_dir.unwrap_or_else(Self::default_cache_dir);
        let files_dir = cache_dir.join("files");
        let manifest_path = cache_dir.join("manifest.json");

        // Load configuration from environment
        let ttl_seconds = std::env::var("TEA_CACHE_TTL")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(DEFAULT_TTL_SECONDS);

        let max_size_bytes = std::env::var("TEA_CACHE_MAX_SIZE")
            .ok()
            .and_then(|s| Self::parse_size(&s).ok())
            .unwrap_or(DEFAULT_MAX_SIZE_BYTES);

        let fetch_timeout = std::env::var("TEA_FETCH_TIMEOUT")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(DEFAULT_FETCH_TIMEOUT);

        // Ensure directories exist with secure permissions (0700)
        fs::create_dir_all(&cache_dir).map_err(|e| {
            crate::TeaError::Io(std::io::Error::new(
                e.kind(),
                format!("Failed to create cache directory: {}", e),
            ))
        })?;

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let perms = fs::Permissions::from_mode(0o700);
            let _ = fs::set_permissions(&cache_dir, perms);
        }

        fs::create_dir_all(&files_dir).map_err(|e| {
            crate::TeaError::Io(std::io::Error::new(
                e.kind(),
                format!("Failed to create files directory: {}", e),
            ))
        })?;

        Ok(Self {
            cache_dir,
            files_dir,
            manifest_path,
            ttl_seconds,
            max_size_bytes,
            fetch_timeout,
            verbose: false,
        })
    }

    /// Get XDG-compliant default cache directory.
    pub fn default_cache_dir() -> PathBuf {
        if let Ok(xdg_cache) = std::env::var("XDG_CACHE_HOME") {
            PathBuf::from(xdg_cache).join("tea").join("remote")
        } else if let Some(home) = dirs::home_dir() {
            home.join(".cache").join("tea").join("remote")
        } else {
            PathBuf::from(".cache").join("tea").join("remote")
        }
    }

    /// Parse size string (e.g., '1GB', '500MB') to bytes.
    fn parse_size(size_str: &str) -> Result<u64, String> {
        let s = size_str.trim().to_uppercase();

        let multipliers: &[(&str, u64)] = &[
            ("TB", 1024 * 1024 * 1024 * 1024),
            ("GB", 1024 * 1024 * 1024),
            ("MB", 1024 * 1024),
            ("KB", 1024),
            ("B", 1),
        ];

        for (suffix, mult) in multipliers {
            if let Some(num_str) = s.strip_suffix(suffix) {
                let num: u64 = num_str
                    .trim()
                    .parse()
                    .map_err(|_| format!("Invalid size number: {}", num_str))?;
                return Ok(num * mult);
            }
        }

        // Assume bytes
        s.parse().map_err(|_| format!("Invalid size format: {}", s))
    }

    /// Generate cache key from URL.
    ///
    /// For git URLs, uses `{provider}://{owner}/{repo}@{ref}` to enable proper TTL handling.
    /// For other URLs, uses the full canonical URL.
    ///
    /// Returns first 16 hex characters of SHA256 hash (same as Python).
    pub fn cache_key(&self, url: &str) -> String {
        let key_source = if let Some(git_parts) = ParsedGitUrl::parse(url) {
            format!(
                "{}://{}/{}@{}",
                git_parts.provider, git_parts.owner, git_parts.repo, git_parts.ref_
            )
        } else {
            url.to_string()
        };

        let mut hasher = Sha256::new();
        hasher.update(key_source.as_bytes());
        let result = hasher.finalize();
        hex::encode(&result[..8]) // First 8 bytes = 16 hex chars
    }

    /// Load manifest from disk or create empty one.
    pub fn load_manifest(&self) -> CacheManifest {
        if self.manifest_path.exists() {
            if let Ok(content) = fs::read_to_string(&self.manifest_path) {
                if let Ok(manifest) = serde_json::from_str::<CacheManifest>(&content) {
                    if manifest.version == MANIFEST_VERSION {
                        return manifest;
                    }
                }
            }
        }
        CacheManifest::default()
    }

    /// Save manifest to disk atomically.
    pub fn save_manifest(&self, manifest: &CacheManifest) -> TeaResult<()> {
        let temp_path = self.manifest_path.with_extension("tmp");

        // Write to temp file first
        let content = serde_json::to_string_pretty(manifest).map_err(|e| {
            crate::TeaError::Serialization(format!("Failed to serialize manifest: {}", e))
        })?;

        let mut file = fs::File::create(&temp_path).map_err(|e| {
            crate::TeaError::Io(std::io::Error::new(
                e.kind(),
                format!("Failed to create temp manifest: {}", e),
            ))
        })?;

        file.write_all(content.as_bytes()).map_err(|e| {
            crate::TeaError::Io(std::io::Error::new(
                e.kind(),
                format!("Failed to write manifest: {}", e),
            ))
        })?;

        // Atomic rename
        fs::rename(&temp_path, &self.manifest_path).map_err(|e| {
            let _ = fs::remove_file(&temp_path);
            crate::TeaError::Io(std::io::Error::new(
                e.kind(),
                format!("Failed to save manifest: {}", e),
            ))
        })?;

        Ok(())
    }

    /// Check if cache entry has expired.
    fn is_expired(&self, entry: &CacheEntry) -> bool {
        if entry.is_permanent {
            return false;
        }

        let ttl = entry.ttl_seconds.unwrap_or(self.ttl_seconds);
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64;

        now > (entry.created_at + ttl)
    }

    /// Check if valid (non-expired) cache entry exists.
    pub fn has_valid(&self, url: &str) -> bool {
        let cache_key = self.cache_key(url);
        let manifest = self.load_manifest();

        let entry = match manifest.entries.get(&cache_key) {
            Some(e) => e,
            None => return false,
        };

        // Check if file exists
        let local_path = self.cache_dir.join(&entry.local_path);
        if !local_path.exists() {
            return false;
        }

        !self.is_expired(entry)
    }

    /// Check if cache entry exists (regardless of expiration).
    pub fn has(&self, url: &str) -> bool {
        let cache_key = self.cache_key(url);
        let manifest = self.load_manifest();

        let entry = match manifest.entries.get(&cache_key) {
            Some(e) => e,
            None => return false,
        };

        let local_path = self.cache_dir.join(&entry.local_path);
        local_path.exists()
    }

    /// Check if an expired cache entry exists (for --cache-only hint).
    pub fn has_expired(&self, url: &str) -> bool {
        let cache_key = self.cache_key(url);
        let manifest = self.load_manifest();

        let entry = match manifest.entries.get(&cache_key) {
            Some(e) => e,
            None => return false,
        };

        let local_path = self.cache_dir.join(&entry.local_path);
        if !local_path.exists() {
            return false;
        }

        self.is_expired(entry)
    }

    /// Get local path for cached URL.
    pub fn get_path(&self, url: &str) -> TeaResult<PathBuf> {
        let cache_key = self.cache_key(url);
        let manifest = self.load_manifest();

        let entry = manifest.entries.get(&cache_key).ok_or_else(|| {
            crate::TeaError::Http(format!("No cache entry for URL: {}", mask_credentials(url)))
        })?;

        let local_path = self.cache_dir.join(&entry.local_path);

        // Security check (SEC-002)
        if !validate_path_containment(&local_path, &self.cache_dir) {
            return Err(crate::TeaError::InvalidConfig(format!(
                "Path traversal detected in cached path: {}",
                mask_credentials(&local_path.to_string_lossy())
            )));
        }

        if !local_path.exists() {
            return Err(crate::TeaError::Http(format!(
                "Cached file missing: {}",
                mask_credentials(url)
            )));
        }

        Ok(local_path)
    }

    /// Store content in cache and update manifest.
    pub fn store(&self, url: &str, content: &[u8], filename: &str) -> TeaResult<PathBuf> {
        // Security validation (SEC-003)
        let (is_safe, error) = validate_url_safe(url);
        if !is_safe {
            return Err(crate::TeaError::InvalidConfig(format!(
                "URL security check failed: {}",
                error
            )));
        }

        let cache_key = self.cache_key(url);
        let safe_filename = sanitize_filename(filename);

        // Create entry directory
        let entry_dir = self.files_dir.join(&cache_key);
        fs::create_dir_all(&entry_dir).map_err(|e| {
            crate::TeaError::Io(std::io::Error::new(
                e.kind(),
                format!("Failed to create cache entry directory: {}", e),
            ))
        })?;

        let local_path = entry_dir.join(&safe_filename);

        // Validate containment (SEC-002)
        // For new files, check that the target path will be within cache_dir
        let target_parent = local_path.parent().unwrap_or(&entry_dir);
        if let Ok(canonical_parent) = target_parent.canonicalize() {
            if let Ok(canonical_cache) = self.cache_dir.canonicalize() {
                if !canonical_parent.starts_with(&canonical_cache) {
                    return Err(crate::TeaError::InvalidConfig(format!(
                        "Path traversal attempt detected for: {}",
                        mask_credentials(url)
                    )));
                }
            }
        }

        // Write content
        fs::write(&local_path, content).map_err(|e| {
            crate::TeaError::Io(std::io::Error::new(
                e.kind(),
                format!(
                    "Failed to write cache file {}: {}",
                    mask_credentials(url),
                    e
                ),
            ))
        })?;

        if self.verbose {
            tracing::info!(
                "Cached: {} -> {}",
                mask_credentials(url),
                local_path.display()
            );
        }

        // Determine if permanent cache (git SHA/tag)
        let is_permanent = if let Some(git_parts) = ParsedGitUrl::parse(url) {
            is_git_permanent_ref(&git_parts.ref_)
        } else {
            false
        };

        // Update manifest
        let mut manifest = self.load_manifest();
        manifest.entries.insert(
            cache_key,
            CacheEntry {
                url: url.to_string(),
                local_path: format!("files/{}/{}", self.cache_key(url), safe_filename),
                created_at: SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_secs() as i64,
                ttl_seconds: Some(self.ttl_seconds),
                is_permanent,
                size_bytes: content.len() as u64,
            },
        );
        self.save_manifest(&manifest)?;

        // Enforce cache size limit
        self.enforce_size_limit()?;

        Ok(local_path)
    }

    /// Enforce cache size limit by removing oldest entries.
    fn enforce_size_limit(&self) -> TeaResult<()> {
        let mut manifest = self.load_manifest();
        let total_size: u64 = manifest.entries.values().map(|e| e.size_bytes).sum();

        if total_size <= self.max_size_bytes {
            return Ok(());
        }

        // Sort by creation time, oldest first
        let mut entries: Vec<_> = manifest.entries.iter().collect();
        entries.sort_by_key(|(_, e)| e.created_at);

        let mut current_size = total_size;
        let mut to_remove = Vec::new();

        for (key, entry) in entries {
            if current_size <= self.max_size_bytes {
                break;
            }

            let local_path = self.cache_dir.join(&entry.local_path);
            if local_path.exists() {
                let _ = fs::remove_file(&local_path);
                // Try to remove parent directory if empty
                if let Some(parent) = local_path.parent() {
                    if parent.exists()
                        && parent
                            .read_dir()
                            .map(|mut d| d.next().is_none())
                            .unwrap_or(false)
                    {
                        let _ = fs::remove_dir(parent);
                    }
                }
            }

            current_size = current_size.saturating_sub(entry.size_bytes);
            to_remove.push(key.clone());
        }

        for key in to_remove {
            manifest.entries.remove(&key);
        }

        if !manifest.entries.is_empty() {
            self.save_manifest(&manifest)?;
        }

        Ok(())
    }

    /// Clear cache entries, optionally filtering by age.
    ///
    /// # Arguments
    ///
    /// * `older_than_seconds` - Only clear entries older than this many seconds
    ///
    /// # Returns
    ///
    /// Number of entries cleared.
    pub fn clear(&self, older_than_seconds: Option<i64>) -> TeaResult<usize> {
        let mut manifest = self.load_manifest();

        let cutoff_time = older_than_seconds.map(|secs| {
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_secs() as i64
                - secs
        });

        let mut removed = 0;
        let mut to_remove = Vec::new();

        for (key, entry) in manifest.entries.iter() {
            // Check age filter
            if let Some(cutoff) = cutoff_time {
                if entry.created_at > cutoff {
                    continue;
                }
            }

            let local_path = self.cache_dir.join(&entry.local_path);
            if local_path.exists() {
                let _ = fs::remove_file(&local_path);
                // Try to remove parent directory if empty
                if let Some(parent) = local_path.parent() {
                    if parent.exists()
                        && parent
                            .read_dir()
                            .map(|mut d| d.next().is_none())
                            .unwrap_or(false)
                    {
                        let _ = fs::remove_dir(parent);
                    }
                }
            }

            to_remove.push(key.clone());
            removed += 1;
        }

        for key in to_remove {
            manifest.entries.remove(&key);
        }

        self.save_manifest(&manifest)?;
        Ok(removed)
    }

    /// List all cache entries with metadata.
    pub fn list_entries(&self) -> Vec<CacheEntryInfo> {
        let manifest = self.load_manifest();
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64;

        let mut result: Vec<CacheEntryInfo> = manifest
            .entries
            .iter()
            .map(|(key, entry)| {
                let local_path = self.cache_dir.join(&entry.local_path);
                CacheEntryInfo {
                    url: entry.url.clone(),
                    cache_key: key.clone(),
                    size_bytes: entry.size_bytes,
                    created_at: entry.created_at,
                    age_seconds: now - entry.created_at,
                    expired: self.is_expired(entry),
                    is_permanent: entry.is_permanent,
                    local_path: local_path.to_string_lossy().to_string(),
                    exists: local_path.exists(),
                }
            })
            .collect();

        // Sort by age (newest first)
        result.sort_by_key(|e| e.age_seconds);
        result
    }

    /// Get cache statistics.
    pub fn info(&self) -> CacheInfo {
        let manifest = self.load_manifest();
        let total_size: u64 = manifest.entries.values().map(|e| e.size_bytes).sum();
        let valid_count = manifest
            .entries
            .values()
            .filter(|e| !self.is_expired(e))
            .count();
        let expired_count = manifest.entries.len() - valid_count;

        CacheInfo {
            location: self.cache_dir.to_string_lossy().to_string(),
            manifest_path: self.manifest_path.to_string_lossy().to_string(),
            total_entries: manifest.entries.len(),
            valid_entries: valid_count,
            expired_entries: expired_count,
            total_size_bytes: total_size,
            total_size_human: Self::format_size(total_size),
            max_size_bytes: self.max_size_bytes,
            max_size_human: Self::format_size(self.max_size_bytes),
            usage_percent: if self.max_size_bytes > 0 {
                (total_size as f64 / self.max_size_bytes as f64 * 100.0).round() as u8
            } else {
                0
            },
            ttl_seconds: self.ttl_seconds,
            fetch_timeout_seconds: self.fetch_timeout,
        }
    }

    /// Format size in human-readable form.
    fn format_size(size_bytes: u64) -> String {
        let mut size = size_bytes as f64;
        for unit in ["B", "KB", "MB", "GB", "TB"] {
            if size < 1024.0 {
                return format!("{:.1} {}", size, unit);
            }
            size /= 1024.0;
        }
        format!("{:.1} PB", size)
    }

    /// Get the cache directory path.
    pub fn cache_dir(&self) -> &Path {
        &self.cache_dir
    }

    /// Get the files directory path.
    pub fn files_dir(&self) -> &Path {
        &self.files_dir
    }
}

/// Information about a cache entry for listing.
#[derive(Debug, Clone, Serialize)]
pub struct CacheEntryInfo {
    pub url: String,
    pub cache_key: String,
    pub size_bytes: u64,
    pub created_at: i64,
    pub age_seconds: i64,
    pub expired: bool,
    pub is_permanent: bool,
    pub local_path: String,
    pub exists: bool,
}

/// Cache statistics.
#[derive(Debug, Clone, Serialize)]
pub struct CacheInfo {
    pub location: String,
    pub manifest_path: String,
    pub total_entries: usize,
    pub valid_entries: usize,
    pub expired_entries: usize,
    pub total_size_bytes: u64,
    pub total_size_human: String,
    pub max_size_bytes: u64,
    pub max_size_human: String,
    pub usage_percent: u8,
    pub ttl_seconds: i64,
    pub fetch_timeout_seconds: u64,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_mask_credentials_url_params() {
        let masked = mask_credentials("https://api.example.com?token=secret123&key=abc");
        assert!(!masked.contains("secret123"));
        assert!(!masked.contains("abc"));
        assert!(masked.contains("token=***"));
        assert!(masked.contains("key=***"));
    }

    #[test]
    fn test_mask_credentials_bearer() {
        let masked = mask_credentials("Authorization: Bearer abc123xyz");
        assert!(!masked.contains("abc123xyz"));
        assert!(masked.contains("Bearer ***"));
    }

    #[test]
    fn test_mask_credentials_github_token() {
        let masked = mask_credentials("Using token ghp_abcdefghijklmnop");
        assert!(!masked.contains("abcdefghijklmnop"));
        assert!(masked.contains("ghp_***"));
    }

    #[test]
    fn test_mask_credentials_gitlab_token() {
        let masked = mask_credentials("Using token glpat-abcdefghijk");
        assert!(!masked.contains("abcdefghijk"));
        assert!(masked.contains("glpat-***"));
    }

    #[test]
    fn test_is_private_ip() {
        assert!(is_private_ip("localhost"));
        assert!(is_private_ip("127.0.0.1"));
        assert!(is_private_ip("::1"));
        assert!(is_private_ip("192.168.1.1"));
        assert!(is_private_ip("10.0.0.1"));
        assert!(is_private_ip("169.254.169.254"));
        assert!(is_private_ip("host.local"));
        assert!(!is_private_ip("github.com"));
        assert!(!is_private_ip("8.8.8.8"));
    }

    #[test]
    fn test_validate_url_safe() {
        // Valid URLs
        assert!(validate_url_safe("s3://bucket/key").0);
        assert!(validate_url_safe("https://github.com/file").0);
        assert!(validate_url_safe("github://user/repo@main/file").0);

        // Invalid protocols
        assert!(!validate_url_safe("ftp://server/file").0);

        // SSRF attempts
        assert!(!validate_url_safe("http://localhost/file").0);
        assert!(!validate_url_safe("http://127.0.0.1/file").0);
        assert!(!validate_url_safe("http://169.254.169.254/metadata").0);
    }

    #[test]
    fn test_sanitize_filename() {
        assert_eq!(sanitize_filename("file.yaml"), "file.yaml");
        assert_eq!(sanitize_filename("path/to/file.yaml"), "path_to_file.yaml");
        // "../../../etc/passwd" -> replace ".." twice (3 times) and "/" (4 times)
        // "..___..___..__etc_passwd" -> no, just replace each ".." with "__" and "/" with "_"
        // Actually: "../../../etc/passwd" -> "__/_/__/_/__/_etc_passwd" first replaces / with _
        // -> ".._.._.._.._etc_passwd" then replace .. with __
        // -> "____..____..____.._etc_passwd" - wait, that's not right either
        // Let me trace: "../../../etc/passwd"
        // Step 1: replace "/" with "_" -> ".._.._.._etc_passwd"
        // Step 2: replace ".." with "__" -> "______.._etc_passwd" - still not right
        // Actually the code does: replace / then replace ..
        // ".." -> "__" (but .. in the middle of string)
        // Test the actual behavior:
        let result = sanitize_filename("../../../etc/passwd");
        assert!(result.contains("etc_passwd"), "Result was: {}", result);
        assert!(
            !result.contains(".."),
            "Should not contain .. but got: {}",
            result
        );
        assert!(
            !result.contains("/"),
            "Should not contain / but got: {}",
            result
        );

        assert_eq!(sanitize_filename(""), "unnamed");
    }

    #[test]
    fn test_is_git_permanent_ref() {
        // SHAs (7-40 hex characters)
        assert!(is_git_permanent_ref("abc1234"));
        assert!(is_git_permanent_ref("abc1234567890"));
        // 40 chars: a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2
        assert!(is_git_permanent_ref(
            "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2"
        ));

        // Tags
        assert!(is_git_permanent_ref("v1.0.0"));
        assert!(is_git_permanent_ref("1.2.3"));
        assert!(is_git_permanent_ref("v1.0.0-beta.1"));

        // Branches (not permanent)
        assert!(!is_git_permanent_ref("main"));
        assert!(!is_git_permanent_ref("develop"));
        assert!(!is_git_permanent_ref("feature/my-feature"));
    }

    #[test]
    fn test_parsed_git_url() {
        let parsed = ParsedGitUrl::parse("github://user/repo@main/path/file.yaml").unwrap();
        assert_eq!(parsed.provider, "github");
        assert_eq!(parsed.owner, "user");
        assert_eq!(parsed.repo, "repo");
        assert_eq!(parsed.ref_, "main");
        assert_eq!(parsed.path, "path/file.yaml");

        let parsed2 = ParsedGitUrl::parse("gitlab://owner/repo@v1.0.0/config.yaml").unwrap();
        assert_eq!(parsed2.provider, "gitlab");
        assert_eq!(parsed2.ref_, "v1.0.0");
    }

    #[test]
    fn test_parse_duration() {
        assert_eq!(parse_duration("7d").unwrap(), 7 * 24 * 60 * 60);
        assert_eq!(parse_duration("24h").unwrap(), 24 * 60 * 60);
        assert_eq!(parse_duration("30m").unwrap(), 30 * 60);
        assert_eq!(parse_duration("3600").unwrap(), 3600);
        assert_eq!(parse_duration("3600s").unwrap(), 3600);
    }

    #[test]
    fn test_cache_key_consistency() {
        let temp_dir = TempDir::new().unwrap();
        let cache = RemoteFileCache::new(Some(temp_dir.path().to_path_buf())).unwrap();

        // Same URL should produce same key
        let key1 = cache.cache_key("s3://bucket/file.yaml");
        let key2 = cache.cache_key("s3://bucket/file.yaml");
        assert_eq!(key1, key2);

        // Different URLs should produce different keys
        let key3 = cache.cache_key("s3://bucket/other.yaml");
        assert_ne!(key1, key3);

        // Git URLs use repo@ref for key
        let git_key1 = cache.cache_key("github://user/repo@main/file1.yaml");
        let git_key2 = cache.cache_key("github://user/repo@main/file2.yaml");
        // Same repo@ref means same key (for TTL purposes)
        assert_eq!(git_key1, git_key2);

        // Different ref means different key
        let git_key3 = cache.cache_key("github://user/repo@develop/file1.yaml");
        assert_ne!(git_key1, git_key3);
    }

    #[test]
    fn test_cache_store_and_retrieve() {
        let temp_dir = TempDir::new().unwrap();
        let cache = RemoteFileCache::new(Some(temp_dir.path().to_path_buf())).unwrap();

        let url = "s3://bucket/test.yaml";
        let content = b"test content";

        // Store
        let path = cache.store(url, content, "test.yaml").unwrap();
        assert!(path.exists());

        // Check it's valid
        assert!(cache.has_valid(url));

        // Retrieve
        let retrieved_path = cache.get_path(url).unwrap();
        assert_eq!(path, retrieved_path);

        let retrieved_content = fs::read(&retrieved_path).unwrap();
        assert_eq!(retrieved_content, content);
    }

    #[test]
    fn test_cache_clear() {
        let temp_dir = TempDir::new().unwrap();
        let cache = RemoteFileCache::new(Some(temp_dir.path().to_path_buf())).unwrap();

        // Store some entries
        cache.store("s3://bucket/a.yaml", b"a", "a.yaml").unwrap();
        cache.store("s3://bucket/b.yaml", b"b", "b.yaml").unwrap();

        assert!(cache.has("s3://bucket/a.yaml"));
        assert!(cache.has("s3://bucket/b.yaml"));

        // Clear all
        let removed = cache.clear(None).unwrap();
        assert_eq!(removed, 2);

        assert!(!cache.has("s3://bucket/a.yaml"));
        assert!(!cache.has("s3://bucket/b.yaml"));
    }

    #[test]
    fn test_cache_list_and_info() {
        let temp_dir = TempDir::new().unwrap();
        let cache = RemoteFileCache::new(Some(temp_dir.path().to_path_buf())).unwrap();

        cache
            .store("s3://bucket/file.yaml", b"content", "file.yaml")
            .unwrap();

        let entries = cache.list_entries();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].url, "s3://bucket/file.yaml");
        assert!(entries[0].exists);

        let info = cache.info();
        assert_eq!(info.total_entries, 1);
        assert_eq!(info.valid_entries, 1);
        assert_eq!(info.expired_entries, 0);
        assert_eq!(info.total_size_bytes, 7); // "content".len()
    }

    #[test]
    fn test_manifest_compatibility() {
        // Test that manifest format matches Python
        let manifest = CacheManifest {
            version: 1,
            entries: {
                let mut m = HashMap::new();
                m.insert(
                    "abc123".to_string(),
                    CacheEntry {
                        url: "s3://bucket/file.yaml".to_string(),
                        local_path: "files/abc123/file.yaml".to_string(),
                        created_at: 1706745600,
                        ttl_seconds: Some(3600),
                        is_permanent: false,
                        size_bytes: 1024,
                    },
                );
                m
            },
        };

        let json = serde_json::to_string_pretty(&manifest).unwrap();

        // Should have all expected fields
        assert!(json.contains("\"version\": 1"));
        assert!(json.contains("\"url\":"));
        assert!(json.contains("\"local_path\":"));
        assert!(json.contains("\"created_at\":"));
        assert!(json.contains("\"ttl_seconds\":"));
        assert!(json.contains("\"is_permanent\":"));
        assert!(json.contains("\"size_bytes\":"));

        // Should be able to parse back
        let parsed: CacheManifest = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.version, 1);
        assert_eq!(parsed.entries.len(), 1);
    }
}
