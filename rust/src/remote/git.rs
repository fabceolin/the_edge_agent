//! Git protocol handler for GitHub and GitLab repositories.
//!
//! This module provides functionality to fetch files from git repositories
//! using the `github://` and `gitlab://` URL schemes.
//!
//! # URL Format
//!
//! ```text
//! github://owner/repo@ref/path/to/file.yaml
//! gitlab://owner/repo@ref/path/to/file.yaml
//! ```
//!
//! Where `ref` can be:
//! - Branch name: `main`, `develop`, `feature/xyz`
//! - Tag: `v1.0.0`, `v1.2.3-beta`
//! - Commit SHA: `abc1234`, `abc1234567890abcdef...`
//!
//! # Authentication
//!
//! Authentication is handled via environment variables:
//! - `GITHUB_TOKEN` - GitHub personal access token
//! - `GITLAB_TOKEN` - GitLab personal access token
//! - `GIT_TOKEN` - Generic fallback token
//!
//! For public repositories, no authentication is required.
//!
//! # Caching
//!
//! - SHA and tag refs are cached permanently (immutable)
//! - Branch refs respect TTL and can be refreshed
//!
//! # Example
//!
//! ```rust,ignore
//! use the_edge_agent::remote::git::{GitFetcher, GitHost, GitRef};
//!
//! let fetcher = GitFetcher::new(cache);
//! let local_path = fetcher.fetch(
//!     GitHost::GitHub,
//!     "owner",
//!     "repo",
//!     &GitRef::Branch("main".to_string()),
//!     "path/to/file.yaml",
//!     "github://owner/repo@main/path/to/file.yaml"
//! )?;
//! ```

use crate::remote::cache::{is_git_permanent_ref, mask_credentials, RemoteFileCache};
use crate::TeaResult;
use std::path::PathBuf;

/// Git hosting provider.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GitHost {
    GitHub,
    GitLab,
}

impl GitHost {
    /// Get the raw content URL base for this host.
    pub fn raw_url_base(&self) -> &'static str {
        match self {
            GitHost::GitHub => "https://raw.githubusercontent.com",
            GitHost::GitLab => "https://gitlab.com",
        }
    }

    /// Get the environment variable name for the authentication token.
    pub fn token_env_var(&self) -> &'static str {
        match self {
            GitHost::GitHub => "GITHUB_TOKEN",
            GitHost::GitLab => "GITLAB_TOKEN",
        }
    }
}

/// Git reference type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GitRef {
    /// Branch name (mutable, respects TTL)
    Branch(String),
    /// Tag (immutable, cached permanently)
    Tag(String),
    /// Commit SHA (immutable, cached permanently)
    Sha(String),
}

impl GitRef {
    /// Parse a reference string into a GitRef.
    ///
    /// Heuristics:
    /// - 7-40 hex characters -> SHA
    /// - Starts with v followed by digits and dots -> Tag
    /// - Digits and dots matching semver -> Tag
    /// - Everything else -> Branch
    pub fn parse(ref_str: &str) -> Self {
        if is_git_permanent_ref(ref_str) {
            // Check if it looks like a SHA or a tag
            if ref_str.chars().all(|c| c.is_ascii_hexdigit()) && ref_str.len() >= 7 {
                GitRef::Sha(ref_str.to_string())
            } else {
                GitRef::Tag(ref_str.to_string())
            }
        } else {
            GitRef::Branch(ref_str.to_string())
        }
    }

    /// Check if this reference is permanent (SHA or tag).
    pub fn is_permanent(&self) -> bool {
        matches!(self, GitRef::Tag(_) | GitRef::Sha(_))
    }

    /// Get the reference string.
    pub fn as_str(&self) -> &str {
        match self {
            GitRef::Branch(s) | GitRef::Tag(s) | GitRef::Sha(s) => s,
        }
    }
}

impl std::fmt::Display for GitRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Parsed git URL components.
#[derive(Debug, Clone)]
pub struct GitUrl {
    pub host: GitHost,
    pub owner: String,
    pub repo: String,
    pub ref_: GitRef,
    pub path: String,
}

impl GitUrl {
    /// Parse a git URL string.
    ///
    /// # Format
    ///
    /// ```text
    /// github://owner/repo@ref/path/to/file
    /// gitlab://owner/repo@ref/path/to/file
    /// ```
    pub fn parse(url: &str) -> Option<Self> {
        let (host, remainder) = if let Some(r) = url.strip_prefix("github://") {
            (GitHost::GitHub, r)
        } else if let Some(r) = url.strip_prefix("gitlab://") {
            (GitHost::GitLab, r)
        } else {
            return None;
        };

        // Split on @ to get ref
        let (repo_part, ref_and_path) = if let Some(at_idx) = remainder.find('@') {
            (&remainder[..at_idx], &remainder[at_idx + 1..])
        } else {
            // No @ specified, default to main branch
            let parts: Vec<&str> = remainder.splitn(3, '/').collect();
            if parts.len() < 2 {
                return None;
            }
            return Some(GitUrl {
                host,
                owner: parts[0].to_string(),
                repo: parts[1].to_string(),
                ref_: GitRef::Branch("main".to_string()),
                path: if parts.len() > 2 {
                    parts[2].to_string()
                } else {
                    String::new()
                },
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

        let ref_ = GitRef::parse(ref_str);

        Some(GitUrl {
            host,
            owner,
            repo,
            ref_,
            path,
        })
    }

    /// Construct the raw file URL for direct HTTP fetch.
    pub fn raw_url(&self) -> String {
        match self.host {
            GitHost::GitHub => {
                // https://raw.githubusercontent.com/owner/repo/ref/path
                format!(
                    "{}/{}/{}/{}/{}",
                    self.host.raw_url_base(),
                    self.owner,
                    self.repo,
                    self.ref_.as_str(),
                    self.path
                )
            }
            GitHost::GitLab => {
                // https://gitlab.com/owner/repo/-/raw/ref/path
                format!(
                    "{}/{}/{}/-/raw/{}/{}",
                    self.host.raw_url_base(),
                    self.owner,
                    self.repo,
                    self.ref_.as_str(),
                    self.path
                )
            }
        }
    }
}

/// Fetcher for git repository files.
pub struct GitFetcher {
    cache: RemoteFileCache,
}

impl GitFetcher {
    /// Create a new GitFetcher with the given cache.
    pub fn new(cache: RemoteFileCache) -> Self {
        Self { cache }
    }

    /// Fetch a file from a git repository.
    ///
    /// # Arguments
    ///
    /// * `host` - Git hosting provider (GitHub/GitLab)
    /// * `owner` - Repository owner/organization
    /// * `repo` - Repository name
    /// * `ref_` - Git reference (branch, tag, or SHA)
    /// * `path` - Path to file within repository
    /// * `original_url` - Original URL for cache key
    ///
    /// # Returns
    ///
    /// Path to the locally cached file.
    pub fn fetch(
        &self,
        host: GitHost,
        owner: &str,
        repo: &str,
        ref_: &GitRef,
        path: &str,
        original_url: &str,
    ) -> TeaResult<PathBuf> {
        // Construct the raw URL for direct HTTP fetch
        let raw_url = match host {
            GitHost::GitHub => {
                format!(
                    "https://raw.githubusercontent.com/{}/{}/{}/{}",
                    owner,
                    repo,
                    ref_.as_str(),
                    path
                )
            }
            GitHost::GitLab => {
                format!(
                    "https://gitlab.com/{}/{}/-/raw/{}/{}",
                    owner,
                    repo,
                    ref_.as_str(),
                    path
                )
            }
        };

        tracing::debug!(
            "Fetching git file: {} -> {}",
            mask_credentials(original_url),
            mask_credentials(&raw_url)
        );

        // Get authentication token
        let token = self.get_auth_token(host);

        // Build HTTP client
        let client = reqwest::blocking::Client::builder()
            .timeout(std::time::Duration::from_secs(30))
            .build()
            .map_err(|e| crate::TeaError::Http(format!("Failed to build HTTP client: {}", e)))?;

        // Build request with auth if available
        let mut request = client.get(&raw_url);
        if let Some(token) = &token {
            request = match host {
                GitHost::GitHub => request.header("Authorization", format!("Bearer {}", token)),
                GitHost::GitLab => request.header("PRIVATE-TOKEN", token.as_str()),
            };
        }

        // Execute request
        let response = request
            .send()
            .map_err(|e| self.map_network_error(e, original_url, token.is_some()))?;

        // Check response status
        let status = response.status();
        if !status.is_success() {
            return Err(self.map_http_error(status.as_u16(), original_url, token.is_some()));
        }

        // Read content
        let content = response
            .bytes()
            .map_err(|e| crate::TeaError::Http(format!("Failed to read response: {}", e)))?;

        // Determine filename from path
        let filename = path.rsplit('/').next().unwrap_or("file");

        // Store in cache
        let cached_path = self.cache.store(original_url, &content, filename)?;

        tracing::debug!(
            "Cached git file: {} -> {}",
            mask_credentials(original_url),
            cached_path.display()
        );

        Ok(cached_path)
    }

    /// Get authentication token from environment.
    fn get_auth_token(&self, host: GitHost) -> Option<String> {
        // Try host-specific token first
        let host_token = std::env::var(host.token_env_var()).ok();
        if host_token.is_some() {
            return host_token;
        }

        // Try generic GIT_TOKEN
        std::env::var("GIT_TOKEN").ok()
    }

    /// Map network errors to user-friendly messages.
    fn map_network_error(&self, err: reqwest::Error, url: &str, has_auth: bool) -> crate::TeaError {
        let masked_url = mask_credentials(url);

        if err.is_timeout() {
            return crate::TeaError::Http(format!(
                "Timeout fetching {}: request took too long. Check network connectivity.",
                masked_url
            ));
        }

        if err.is_connect() {
            return crate::TeaError::Http(format!(
                "Failed to connect to {}: {}. Check network connectivity and URL validity.",
                masked_url, err
            ));
        }

        // Generic network error with auth hint
        let auth_hint = if !has_auth {
            " For private repositories, set GITHUB_TOKEN or GITLAB_TOKEN environment variable."
        } else {
            ""
        };

        crate::TeaError::Http(format!(
            "Network error fetching {}: {}.{}",
            masked_url, err, auth_hint
        ))
    }

    /// Map HTTP status codes to user-friendly messages.
    fn map_http_error(&self, status: u16, url: &str, has_auth: bool) -> crate::TeaError {
        let masked_url = mask_credentials(url);

        match status {
            401 => {
                if has_auth {
                    crate::TeaError::Http(format!(
                        "Authentication failed for {}: token may be invalid or expired. \
                         Check GITHUB_TOKEN/GITLAB_TOKEN value.",
                        masked_url
                    ))
                } else {
                    crate::TeaError::Http(format!(
                        "Authentication required for {}: set GITHUB_TOKEN or GITLAB_TOKEN \
                         environment variable for private repository access.",
                        masked_url
                    ))
                }
            }
            403 => crate::TeaError::Http(format!(
                "Access forbidden for {}: check repository permissions and token scopes.",
                masked_url
            )),
            404 => crate::TeaError::Http(format!(
                "File not found: {}. Verify repository, branch/tag, and file path.",
                masked_url
            )),
            429 => crate::TeaError::Http(format!(
                "Rate limited for {}: too many requests. Try again later or use authentication.",
                masked_url
            )),
            500..=599 => crate::TeaError::Http(format!(
                "Server error ({}) for {}: the git hosting service may be experiencing issues.",
                status, masked_url
            )),
            _ => crate::TeaError::Http(format!(
                "HTTP error {} for {}: unexpected response from server.",
                status, masked_url
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_git_ref_parse_branch() {
        assert!(matches!(GitRef::parse("main"), GitRef::Branch(_)));
        assert!(matches!(GitRef::parse("develop"), GitRef::Branch(_)));
        assert!(matches!(
            GitRef::parse("feature/my-feature"),
            GitRef::Branch(_)
        ));
    }

    #[test]
    fn test_git_ref_parse_sha() {
        assert!(matches!(GitRef::parse("abc1234"), GitRef::Sha(_)));
        assert!(matches!(GitRef::parse("abc1234567890"), GitRef::Sha(_)));
        // 40-char SHA: a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2
        assert!(matches!(
            GitRef::parse("a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2"),
            GitRef::Sha(_)
        ));
    }

    #[test]
    fn test_git_ref_parse_tag() {
        assert!(matches!(GitRef::parse("v1.0.0"), GitRef::Tag(_)));
        assert!(matches!(GitRef::parse("1.2.3"), GitRef::Tag(_)));
        assert!(matches!(GitRef::parse("v1.0.0-beta.1"), GitRef::Tag(_)));
    }

    #[test]
    fn test_git_ref_is_permanent() {
        assert!(!GitRef::Branch("main".to_string()).is_permanent());
        assert!(GitRef::Tag("v1.0.0".to_string()).is_permanent());
        assert!(GitRef::Sha("abc1234".to_string()).is_permanent());
    }

    #[test]
    fn test_git_url_parse_github() {
        let url = GitUrl::parse("github://owner/repo@main/path/to/file.yaml").unwrap();
        assert_eq!(url.host, GitHost::GitHub);
        assert_eq!(url.owner, "owner");
        assert_eq!(url.repo, "repo");
        assert!(matches!(url.ref_, GitRef::Branch(ref b) if b == "main"));
        assert_eq!(url.path, "path/to/file.yaml");
    }

    #[test]
    fn test_git_url_parse_gitlab() {
        let url = GitUrl::parse("gitlab://group/project@v1.0.0/config.yaml").unwrap();
        assert_eq!(url.host, GitHost::GitLab);
        assert_eq!(url.owner, "group");
        assert_eq!(url.repo, "project");
        assert!(matches!(url.ref_, GitRef::Tag(_)));
        assert_eq!(url.path, "config.yaml");
    }

    #[test]
    fn test_git_url_parse_sha() {
        let url = GitUrl::parse("github://user/repo@abc1234/file.yaml").unwrap();
        assert!(matches!(url.ref_, GitRef::Sha(ref s) if s == "abc1234"));
    }

    #[test]
    fn test_git_url_parse_no_ref() {
        let url = GitUrl::parse("github://user/repo/path/file.yaml").unwrap();
        assert!(matches!(url.ref_, GitRef::Branch(ref b) if b == "main"));
        assert_eq!(url.path, "path/file.yaml");
    }

    #[test]
    fn test_git_url_raw_url_github() {
        let url = GitUrl::parse("github://owner/repo@main/path/file.yaml").unwrap();
        let raw = url.raw_url();
        assert_eq!(
            raw,
            "https://raw.githubusercontent.com/owner/repo/main/path/file.yaml"
        );
    }

    #[test]
    fn test_git_url_raw_url_gitlab() {
        let url = GitUrl::parse("gitlab://group/project@main/config.yaml").unwrap();
        let raw = url.raw_url();
        assert_eq!(
            raw,
            "https://gitlab.com/group/project/-/raw/main/config.yaml"
        );
    }

    #[test]
    fn test_git_host_token_env_var() {
        assert_eq!(GitHost::GitHub.token_env_var(), "GITHUB_TOKEN");
        assert_eq!(GitHost::GitLab.token_env_var(), "GITLAB_TOKEN");
    }
}
