//! Remote file handling for TEA CLI.
//!
//! This module provides URL-based file input support for the TEA CLI,
//! enabling workflows to reference files from remote sources:
//! - Cloud storage: `s3://`, `gs://`, `az://`
//! - Git repositories: `github://`, `gitlab://`
//! - HTTP(S): `https://`, `http://`
//! - Local files: `file://` or plain paths
//!
//! # Cache Structure
//!
//! Remote files are cached locally at `~/.cache/tea/remote/` to reduce
//! network calls and enable offline mode. The cache is shared with the
//! Python CLI implementation for cross-language compatibility.
//!
//! ```text
//! ~/.cache/tea/remote/
//! ├── manifest.json          # URL -> cache entry mapping with TTL
//! └── files/
//!     └── {cache_key}/       # One directory per cached URL
//!         └── {filename}     # Actual cached file
//! ```
//!
//! # Security
//!
//! This module implements security mitigations:
//! - SEC-001: Credential masking in all log output
//! - SEC-002: Path traversal prevention via containment validation
//! - SEC-003: SSRF protection via URL validation
//!
//! # Example
//!
//! ```rust,ignore
//! use the_edge_agent::remote::{RemoteFile, DefaultRemoteFileSystem};
//!
//! let fs = DefaultRemoteFileSystem::new(None, false, false)?;
//!
//! // Fetch a file from GitHub
//! let local_path = fs.fetch("github://owner/repo@main/workflow.yaml")?;
//!
//! // Fetch from S3
//! let s3_path = fs.fetch("s3://bucket/path/to/file.yaml")?;
//! ```

pub mod cache;
mod cloud;
mod git;
mod traits;

pub use cache::{
    mask_credentials, parse_duration, validate_path_containment, validate_url_safe, CacheEntry,
    CacheEntryInfo, CacheInfo, CacheManifest, RemoteFileCache,
};
pub use cloud::CloudFetcher;
pub use git::{GitFetcher, GitHost, GitRef, GitUrl};
pub use traits::{MockRemoteFileSystem, RemoteFileSystem};

use crate::TeaResult;
use std::path::PathBuf;

/// Parsed remote file URL with protocol-specific components.
#[derive(Debug, Clone, PartialEq)]
pub enum RemoteFile {
    /// Local file path (passthrough)
    Local(PathBuf),
    /// Amazon S3
    S3 { bucket: String, key: String },
    /// Google Cloud Storage
    Gcs { bucket: String, key: String },
    /// Azure Blob Storage
    Azure { container: String, path: String },
    /// HTTP/HTTPS URL
    Http { url: String },
    /// Git repository (GitHub/GitLab)
    Git {
        host: GitHost,
        owner: String,
        repo: String,
        ref_: GitRef,
        path: String,
    },
}

impl RemoteFile {
    /// Parse URL string into RemoteFile.
    ///
    /// # Supported Formats
    ///
    /// - Local paths: `/path/to/file`, `./relative/path`, `file:///absolute/path`
    /// - S3: `s3://bucket/key/path`
    /// - GCS: `gs://bucket/key/path` or `gcs://bucket/key/path`
    /// - Azure: `az://container/path` or `abfs://container/path`
    /// - HTTP: `https://example.com/file`, `http://example.com/file`
    /// - GitHub: `github://owner/repo@ref/path/to/file`
    /// - GitLab: `gitlab://owner/repo@ref/path/to/file`
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// let local = RemoteFile::parse("/home/user/file.yaml")?;
    /// let s3 = RemoteFile::parse("s3://my-bucket/workflows/agent.yaml")?;
    /// let github = RemoteFile::parse("github://user/repo@main/workflow.yaml")?;
    /// ```
    pub fn parse(url: &str) -> TeaResult<Self> {
        let url_trimmed = url.trim();

        // Check for local paths first
        if !url_trimmed.contains("://") {
            return Ok(RemoteFile::Local(PathBuf::from(url_trimmed)));
        }

        // Parse URL scheme
        let parts: Vec<&str> = url_trimmed.splitn(2, "://").collect();
        if parts.len() != 2 {
            return Ok(RemoteFile::Local(PathBuf::from(url_trimmed)));
        }

        let scheme = parts[0].to_lowercase();
        let remainder = parts[1];

        match scheme.as_str() {
            "file" => {
                // file:///path/to/file
                let path = if remainder.starts_with('/') {
                    remainder.to_string()
                } else {
                    format!("/{}", remainder)
                };
                Ok(RemoteFile::Local(PathBuf::from(path)))
            }
            "s3" => {
                // s3://bucket/key
                let (bucket, key) = Self::parse_bucket_key(remainder)?;
                Ok(RemoteFile::S3 { bucket, key })
            }
            "gs" | "gcs" => {
                // gs://bucket/key or gcs://bucket/key
                let (bucket, key) = Self::parse_bucket_key(remainder)?;
                Ok(RemoteFile::Gcs { bucket, key })
            }
            "az" | "abfs" | "abfss" => {
                // az://container/path
                let (container, path) = Self::parse_bucket_key(remainder)?;
                Ok(RemoteFile::Azure { container, path })
            }
            "http" | "https" => Ok(RemoteFile::Http {
                url: url_trimmed.to_string(),
            }),
            "github" => Self::parse_git_url(remainder, GitHost::GitHub),
            "gitlab" => Self::parse_git_url(remainder, GitHost::GitLab),
            _ => Err(crate::TeaError::InvalidConfig(format!(
                "Unsupported URL scheme: {}",
                scheme
            ))),
        }
    }

    /// Parse bucket/key format: bucket/key/path
    fn parse_bucket_key(remainder: &str) -> TeaResult<(String, String)> {
        match remainder.find('/') {
            Some(idx) => {
                let bucket = remainder[..idx].to_string();
                let key = remainder[idx + 1..].to_string();
                if bucket.is_empty() {
                    return Err(crate::TeaError::InvalidConfig(
                        "Empty bucket name".to_string(),
                    ));
                }
                Ok((bucket, key))
            }
            None => {
                if remainder.is_empty() {
                    return Err(crate::TeaError::InvalidConfig(
                        "Empty bucket name".to_string(),
                    ));
                }
                Ok((remainder.to_string(), String::new()))
            }
        }
    }

    /// Parse git URL format: owner/repo@ref/path
    fn parse_git_url(remainder: &str, host: GitHost) -> TeaResult<Self> {
        // Format: owner/repo@ref/path/to/file
        let (repo_part, ref_and_path) = if let Some(at_idx) = remainder.find('@') {
            (&remainder[..at_idx], &remainder[at_idx + 1..])
        } else {
            // No @, assume main branch
            // owner/repo/path/to/file
            // Need to find where repo ends and path begins
            let parts: Vec<&str> = remainder.splitn(3, '/').collect();
            if parts.len() < 2 {
                return Err(crate::TeaError::InvalidConfig(format!(
                    "Invalid git URL format: {}",
                    remainder
                )));
            }
            let owner = parts[0];
            let repo = parts[1];
            let path = if parts.len() > 2 { parts[2] } else { "" };
            return Ok(RemoteFile::Git {
                host,
                owner: owner.to_string(),
                repo: repo.to_string(),
                ref_: GitRef::Branch("main".to_string()),
                path: path.to_string(),
            });
        };

        // Parse owner/repo
        let repo_parts: Vec<&str> = repo_part.splitn(2, '/').collect();
        if repo_parts.len() < 2 {
            return Err(crate::TeaError::InvalidConfig(format!(
                "Invalid git URL format: missing repo: {}",
                remainder
            )));
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

        let ref_ = GitRef::parse(ref_str);

        Ok(RemoteFile::Git {
            host,
            owner,
            repo,
            ref_,
            path,
        })
    }

    /// Check if this is a local path.
    pub fn is_local(&self) -> bool {
        matches!(self, RemoteFile::Local(_))
    }

    /// Get the original URL representation.
    pub fn to_url(&self) -> String {
        match self {
            RemoteFile::Local(path) => path.to_string_lossy().to_string(),
            RemoteFile::S3 { bucket, key } => format!("s3://{}/{}", bucket, key),
            RemoteFile::Gcs { bucket, key } => format!("gs://{}/{}", bucket, key),
            RemoteFile::Azure { container, path } => format!("az://{}/{}", container, path),
            RemoteFile::Http { url } => url.clone(),
            RemoteFile::Git {
                host,
                owner,
                repo,
                ref_,
                path,
            } => {
                let host_str = match host {
                    GitHost::GitHub => "github",
                    GitHost::GitLab => "gitlab",
                };
                if path.is_empty() {
                    format!("{}://{}/{}@{}", host_str, owner, repo, ref_)
                } else {
                    format!("{}://{}/{}@{}/{}", host_str, owner, repo, ref_, path)
                }
            }
        }
    }

    /// Fetch the file to a local path using the provided filesystem.
    pub fn fetch<F: RemoteFileSystem>(&self, fs: &F) -> TeaResult<PathBuf> {
        match self {
            RemoteFile::Local(path) => Ok(path.clone()),
            _ => fs.fetch(&self.to_url()),
        }
    }
}

/// Default implementation of RemoteFileSystem using object_store + git2.
pub struct DefaultRemoteFileSystem {
    cache: RemoteFileCache,
    no_cache: bool,
    cache_only: bool,
}

impl DefaultRemoteFileSystem {
    /// Create a new DefaultRemoteFileSystem.
    ///
    /// # Arguments
    ///
    /// * `cache_dir` - Override default cache directory
    /// * `no_cache` - Bypass cache, always fetch fresh
    /// * `cache_only` - Only use cache, fail if not cached
    pub fn new(cache_dir: Option<PathBuf>, no_cache: bool, cache_only: bool) -> TeaResult<Self> {
        let cache = RemoteFileCache::new(cache_dir)?;
        Ok(Self {
            cache,
            no_cache,
            cache_only,
        })
    }

    /// Get reference to the cache manager.
    pub fn cache(&self) -> &RemoteFileCache {
        &self.cache
    }
}

impl RemoteFileSystem for DefaultRemoteFileSystem {
    fn fetch(&self, url: &str) -> TeaResult<PathBuf> {
        let parsed = RemoteFile::parse(url)?;

        // Local paths are returned directly
        if let RemoteFile::Local(path) = parsed {
            return Ok(path);
        }

        // Cache-only mode: fail if not cached
        if self.cache_only {
            return self.fetch_cache_only(url);
        }

        // No-cache mode: always fetch fresh
        if self.no_cache {
            return self.fetch_no_cache(url);
        }

        // Normal mode: check cache first
        if self.cache.has_valid(url) {
            tracing::debug!("Cache hit: {}", cache::mask_credentials(url));
            return self.cache.get_path(url);
        }

        // Fetch and cache
        self.fetch_and_cache(url, &parsed)
    }

    fn supports_protocol(&self, protocol: &str) -> bool {
        matches!(
            protocol.to_lowercase().as_str(),
            "s3" | "gs"
                | "gcs"
                | "az"
                | "abfs"
                | "abfss"
                | "http"
                | "https"
                | "github"
                | "gitlab"
                | "file"
        )
    }

    fn fetch_no_cache(&self, url: &str) -> TeaResult<PathBuf> {
        let parsed = RemoteFile::parse(url)?;

        if let RemoteFile::Local(path) = parsed {
            return Ok(path);
        }

        // Fetch and update cache
        self.fetch_and_cache(url, &parsed)
    }

    fn fetch_cache_only(&self, url: &str) -> TeaResult<PathBuf> {
        let parsed = RemoteFile::parse(url)?;

        if let RemoteFile::Local(path) = parsed {
            return Ok(path);
        }

        // Try to get from cache
        if self.cache.has(url) {
            return self.cache.get_path(url);
        }

        // Provide helpful error message
        let has_expired = self.cache.has_expired(url);
        if has_expired {
            Err(crate::TeaError::Http(format!(
                "Cache entry expired for: {}. Try without --cache-only to refresh.",
                cache::mask_credentials(url)
            )))
        } else {
            Err(crate::TeaError::Http(format!(
                "Not found in cache: {}. Try without --cache-only to fetch.",
                cache::mask_credentials(url)
            )))
        }
    }
}

impl DefaultRemoteFileSystem {
    /// Fetch from remote and store in cache.
    fn fetch_and_cache(&self, url: &str, parsed: &RemoteFile) -> TeaResult<PathBuf> {
        tracing::debug!("Fetching: {}", cache::mask_credentials(url));

        match parsed {
            RemoteFile::Git {
                host,
                owner,
                repo,
                ref_,
                path,
            } => {
                let fetcher = GitFetcher::new(self.cache.clone());
                fetcher.fetch(*host, owner, repo, ref_, path, url)
            }
            RemoteFile::S3 { bucket, key }
            | RemoteFile::Gcs { bucket, key }
            | RemoteFile::Azure {
                container: bucket,
                path: key,
            } => {
                let fetcher = CloudFetcher::new(self.cache.clone());
                fetcher.fetch(parsed, bucket, key, url)
            }
            RemoteFile::Http { url: http_url } => {
                let fetcher = CloudFetcher::new(self.cache.clone());
                fetcher.fetch_http(http_url, url)
            }
            RemoteFile::Local(path) => Ok(path.clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_local_path() {
        let rf = RemoteFile::parse("/home/user/file.yaml").unwrap();
        assert!(matches!(rf, RemoteFile::Local(_)));
        if let RemoteFile::Local(p) = rf {
            assert_eq!(p.to_string_lossy(), "/home/user/file.yaml");
        }
    }

    #[test]
    fn test_parse_relative_path() {
        let rf = RemoteFile::parse("./relative/path.yaml").unwrap();
        assert!(matches!(rf, RemoteFile::Local(_)));
    }

    #[test]
    fn test_parse_file_url() {
        let rf = RemoteFile::parse("file:///home/user/file.yaml").unwrap();
        assert!(matches!(rf, RemoteFile::Local(_)));
        if let RemoteFile::Local(p) = rf {
            assert_eq!(p.to_string_lossy(), "/home/user/file.yaml");
        }
    }

    #[test]
    fn test_parse_s3_url() {
        let rf = RemoteFile::parse("s3://my-bucket/path/to/file.yaml").unwrap();
        assert!(matches!(rf, RemoteFile::S3 { .. }));
        if let RemoteFile::S3 { bucket, key } = rf {
            assert_eq!(bucket, "my-bucket");
            assert_eq!(key, "path/to/file.yaml");
        }
    }

    #[test]
    fn test_parse_gs_url() {
        let rf = RemoteFile::parse("gs://my-bucket/file.yaml").unwrap();
        assert!(matches!(rf, RemoteFile::Gcs { .. }));
    }

    #[test]
    fn test_parse_gcs_url() {
        let rf = RemoteFile::parse("gcs://my-bucket/file.yaml").unwrap();
        assert!(matches!(rf, RemoteFile::Gcs { .. }));
    }

    #[test]
    fn test_parse_azure_url() {
        let rf = RemoteFile::parse("az://my-container/path/file.yaml").unwrap();
        assert!(matches!(rf, RemoteFile::Azure { .. }));
    }

    #[test]
    fn test_parse_http_url() {
        let rf = RemoteFile::parse("https://example.com/file.yaml").unwrap();
        assert!(matches!(rf, RemoteFile::Http { .. }));
    }

    #[test]
    fn test_parse_github_url_with_ref() {
        let rf = RemoteFile::parse("github://user/repo@main/path/to/file.yaml").unwrap();
        if let RemoteFile::Git {
            host,
            owner,
            repo,
            ref_,
            path,
        } = rf
        {
            assert_eq!(host, GitHost::GitHub);
            assert_eq!(owner, "user");
            assert_eq!(repo, "repo");
            assert!(matches!(ref_, GitRef::Branch(b) if b == "main"));
            assert_eq!(path, "path/to/file.yaml");
        } else {
            panic!("Expected Git variant");
        }
    }

    #[test]
    fn test_parse_github_url_with_sha() {
        let rf = RemoteFile::parse("github://user/repo@abc1234/file.yaml").unwrap();
        if let RemoteFile::Git { ref_, .. } = rf {
            assert!(matches!(ref_, GitRef::Sha(_)));
        } else {
            panic!("Expected Git variant");
        }
    }

    #[test]
    fn test_parse_gitlab_url() {
        let rf = RemoteFile::parse("gitlab://owner/repo@v1.0.0/config.yaml").unwrap();
        if let RemoteFile::Git {
            host,
            owner,
            repo,
            ref_,
            path,
        } = rf
        {
            assert_eq!(host, GitHost::GitLab);
            assert_eq!(owner, "owner");
            assert_eq!(repo, "repo");
            assert!(matches!(ref_, GitRef::Tag(_)));
            assert_eq!(path, "config.yaml");
        } else {
            panic!("Expected Git variant");
        }
    }

    #[test]
    fn test_to_url_roundtrip() {
        let urls = vec![
            "s3://bucket/key/path",
            "gs://bucket/file",
            "az://container/path",
            "https://example.com/file.yaml",
            "github://user/repo@main/path/file.yaml",
            "gitlab://owner/repo@v1.0.0/config.yaml",
        ];

        for url in urls {
            let parsed = RemoteFile::parse(url).unwrap();
            // Note: Some URLs may have slightly different canonical forms
            let reparsed = RemoteFile::parse(&parsed.to_url()).unwrap();
            assert_eq!(parsed, reparsed, "Roundtrip failed for: {}", url);
        }
    }
}
