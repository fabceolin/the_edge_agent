//! Traits for remote file system operations.
//!
//! This module defines the `RemoteFileSystem` trait that enables mocking
//! in tests while providing a consistent interface for fetching remote files.

use crate::TeaResult;
use std::collections::HashMap;
use std::path::PathBuf;

/// Trait for remote file resolution - enables mocking in tests.
///
/// Implementations of this trait handle fetching remote URLs to local paths,
/// managing caching, and protocol support.
///
/// # Example
///
/// ```rust,ignore
/// use the_edge_agent::remote::RemoteFileSystem;
///
/// fn process_workflow<F: RemoteFileSystem>(fs: &F, url: &str) -> Result<String, Error> {
///     let local_path = fs.fetch(url)?;
///     std::fs::read_to_string(local_path)
/// }
/// ```
pub trait RemoteFileSystem: Send + Sync {
    /// Fetch remote URL to local path, using cache if available.
    ///
    /// # Arguments
    ///
    /// * `url` - URL to fetch (s3://, gs://, github://, etc.)
    ///
    /// # Returns
    ///
    /// Path to the locally cached file.
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - URL protocol is not supported
    /// - Network fetch fails
    /// - Cache write fails
    /// - Path traversal detected
    fn fetch(&self, url: &str) -> TeaResult<PathBuf>;

    /// Check if protocol is supported.
    ///
    /// # Arguments
    ///
    /// * `protocol` - Protocol scheme (e.g., "s3", "github")
    fn supports_protocol(&self, protocol: &str) -> bool;

    /// Force refresh, bypassing cache.
    ///
    /// Always fetches from remote and updates the cache.
    fn fetch_no_cache(&self, url: &str) -> TeaResult<PathBuf>;

    /// Use cache only, fail if not cached.
    ///
    /// Returns error if URL is not in cache or cache entry is expired.
    fn fetch_cache_only(&self, url: &str) -> TeaResult<PathBuf>;
}

/// Mock implementation for testing.
///
/// Allows tests to define expected URL -> path mappings without
/// network access or real filesystem operations.
///
/// # Example
///
/// ```rust
/// use the_edge_agent::remote::{MockRemoteFileSystem, RemoteFileSystem};
/// use std::path::PathBuf;
///
/// let mut mock = MockRemoteFileSystem::new();
/// mock.add_response("s3://bucket/file.yaml", PathBuf::from("/tmp/cached.yaml"));
///
/// // Now fetch will return the mocked path
/// let result = mock.fetch("s3://bucket/file.yaml");
/// assert!(result.is_ok());
/// ```
#[derive(Debug, Default)]
pub struct MockRemoteFileSystem {
    /// URL -> local path mappings
    responses: HashMap<String, PathBuf>,
    /// URLs that should return errors
    errors: HashMap<String, String>,
    /// Track which URLs were fetched
    fetch_log: std::sync::Mutex<Vec<String>>,
}

impl MockRemoteFileSystem {
    /// Create a new empty mock filesystem.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a response mapping: URL -> local path.
    pub fn add_response(&mut self, url: impl Into<String>, path: PathBuf) {
        self.responses.insert(url.into(), path);
    }

    /// Add an error response for a URL.
    pub fn add_error(&mut self, url: impl Into<String>, error: impl Into<String>) {
        self.errors.insert(url.into(), error.into());
    }

    /// Get the list of URLs that were fetched (for test assertions).
    pub fn fetch_log(&self) -> Vec<String> {
        self.fetch_log.lock().unwrap().clone()
    }

    /// Clear the fetch log.
    pub fn clear_log(&self) {
        self.fetch_log.lock().unwrap().clear();
    }
}

impl RemoteFileSystem for MockRemoteFileSystem {
    fn fetch(&self, url: &str) -> TeaResult<PathBuf> {
        // Log the fetch
        self.fetch_log.lock().unwrap().push(url.to_string());

        // Check for error response first
        if let Some(error) = self.errors.get(url) {
            return Err(crate::TeaError::Http(error.clone()));
        }

        // Check for mapped response
        if let Some(path) = self.responses.get(url) {
            return Ok(path.clone());
        }

        // Default: error for unmapped URLs
        Err(crate::TeaError::Http(format!(
            "Mock: No response configured for URL: {}",
            url
        )))
    }

    fn supports_protocol(&self, protocol: &str) -> bool {
        // Mock supports all common protocols
        matches!(
            protocol.to_lowercase().as_str(),
            "s3" | "gs" | "gcs" | "az" | "http" | "https" | "github" | "gitlab" | "file"
        )
    }

    fn fetch_no_cache(&self, url: &str) -> TeaResult<PathBuf> {
        // Same behavior as fetch in mock
        self.fetch(url)
    }

    fn fetch_cache_only(&self, url: &str) -> TeaResult<PathBuf> {
        // Same behavior as fetch in mock
        self.fetch(url)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mock_add_response() {
        let mut mock = MockRemoteFileSystem::new();
        mock.add_response("s3://bucket/file.yaml", PathBuf::from("/tmp/cached.yaml"));

        let result = mock.fetch("s3://bucket/file.yaml");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), PathBuf::from("/tmp/cached.yaml"));
    }

    #[test]
    fn test_mock_add_error() {
        let mut mock = MockRemoteFileSystem::new();
        mock.add_error("s3://bucket/missing.yaml", "File not found");

        let result = mock.fetch("s3://bucket/missing.yaml");
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("File not found"));
    }

    #[test]
    fn test_mock_fetch_log() {
        let mut mock = MockRemoteFileSystem::new();
        mock.add_response("s3://bucket/a.yaml", PathBuf::from("/tmp/a.yaml"));
        mock.add_response("s3://bucket/b.yaml", PathBuf::from("/tmp/b.yaml"));

        let _ = mock.fetch("s3://bucket/a.yaml");
        let _ = mock.fetch("s3://bucket/b.yaml");
        let _ = mock.fetch("s3://bucket/a.yaml");

        let log = mock.fetch_log();
        assert_eq!(log.len(), 3);
        assert_eq!(log[0], "s3://bucket/a.yaml");
        assert_eq!(log[1], "s3://bucket/b.yaml");
        assert_eq!(log[2], "s3://bucket/a.yaml");
    }

    #[test]
    fn test_mock_unmapped_url() {
        let mock = MockRemoteFileSystem::new();
        let result = mock.fetch("s3://bucket/unknown.yaml");
        assert!(result.is_err());
    }

    #[test]
    fn test_mock_supports_protocol() {
        let mock = MockRemoteFileSystem::new();
        assert!(mock.supports_protocol("s3"));
        assert!(mock.supports_protocol("gs"));
        assert!(mock.supports_protocol("github"));
        assert!(mock.supports_protocol("gitlab"));
        assert!(mock.supports_protocol("http"));
        assert!(mock.supports_protocol("https"));
        assert!(!mock.supports_protocol("ftp"));
    }
}
