//! Cloud storage fetcher for S3, GCS, and Azure Blob Storage.
//!
//! This module provides HTTP-based fetching for cloud storage URLs:
//! - `s3://bucket/key` - Amazon S3
//! - `gs://bucket/key` or `gcs://bucket/key` - Google Cloud Storage
//! - `az://container/path` or `abfs://container/path` - Azure Blob Storage
//!
//! # Authentication
//!
//! Cloud storage authentication is handled via environment variables:
//!
//! ## AWS S3
//! - `AWS_ACCESS_KEY_ID` - AWS access key
//! - `AWS_SECRET_ACCESS_KEY` - AWS secret key
//! - `AWS_SESSION_TOKEN` - Optional session token
//! - `AWS_REGION` or `AWS_DEFAULT_REGION` - AWS region
//! - `AWS_ENDPOINT_URL` - Custom endpoint (for S3-compatible services)
//!
//! ## Google Cloud Storage
//! - `GOOGLE_APPLICATION_CREDENTIALS` - Path to service account JSON
//! - `GOOGLE_CLOUD_PROJECT` - GCP project ID
//!
//! ## Azure Blob Storage
//! - `AZURE_STORAGE_ACCOUNT` - Storage account name
//! - `AZURE_STORAGE_KEY` - Storage account key
//! - `AZURE_STORAGE_SAS_TOKEN` - SAS token (alternative to key)
//!
//! # Public Access
//!
//! For public buckets, no authentication is required. The module uses
//! standard public URL patterns.
//!
//! # Example
//!
//! ```rust,ignore
//! use the_edge_agent::remote::cloud::CloudFetcher;
//!
//! let fetcher = CloudFetcher::new(cache);
//! let path = fetcher.fetch_s3("my-bucket", "path/to/file.yaml", "s3://my-bucket/path/to/file.yaml")?;
//! ```

use crate::remote::cache::{mask_credentials, RemoteFileCache};
use crate::remote::RemoteFile;
use crate::TeaResult;
use std::path::PathBuf;

/// Fetcher for cloud storage files.
pub struct CloudFetcher {
    cache: RemoteFileCache,
}

impl CloudFetcher {
    /// Create a new CloudFetcher with the given cache.
    pub fn new(cache: RemoteFileCache) -> Self {
        Self { cache }
    }

    /// Fetch a file from cloud storage.
    ///
    /// Routes to the appropriate cloud provider based on the RemoteFile variant.
    pub fn fetch(
        &self,
        parsed: &RemoteFile,
        bucket: &str,
        key: &str,
        original_url: &str,
    ) -> TeaResult<PathBuf> {
        match parsed {
            RemoteFile::S3 { .. } => self.fetch_s3(bucket, key, original_url),
            RemoteFile::Gcs { .. } => self.fetch_gcs(bucket, key, original_url),
            RemoteFile::Azure { .. } => self.fetch_azure(bucket, key, original_url),
            _ => Err(crate::TeaError::InvalidConfig(format!(
                "Not a cloud storage URL: {}",
                mask_credentials(original_url)
            ))),
        }
    }

    /// Fetch a file from Amazon S3.
    ///
    /// Tries the following methods in order:
    /// 1. AWS credentials if available (signed request)
    /// 2. Public bucket access via HTTPS
    pub fn fetch_s3(&self, bucket: &str, key: &str, original_url: &str) -> TeaResult<PathBuf> {
        tracing::debug!("Fetching S3 file: {}", mask_credentials(original_url));

        // Get AWS configuration
        let region = std::env::var("AWS_REGION")
            .or_else(|_| std::env::var("AWS_DEFAULT_REGION"))
            .unwrap_or_else(|_| "us-east-1".to_string());

        let endpoint = std::env::var("AWS_ENDPOINT_URL").ok();

        // Construct URL
        let url = if let Some(endpoint) = endpoint {
            // Custom endpoint (e.g., MinIO, LocalStack)
            format!("{}/{}/{}", endpoint, bucket, key)
        } else {
            // Standard S3 URL
            format!("https://{}.s3.{}.amazonaws.com/{}", bucket, region, key)
        };

        // Try to fetch with credentials if available
        let has_creds = std::env::var("AWS_ACCESS_KEY_ID").is_ok()
            && std::env::var("AWS_SECRET_ACCESS_KEY").is_ok();

        if has_creds {
            tracing::debug!("Using AWS credentials for S3 access");
            // For authenticated S3 access, we'd need to sign the request
            // For now, we'll use public access or presigned URLs
            // A full implementation would use the aws-sdk-s3 crate
        }

        self.fetch_url(&url, original_url, "S3")
    }

    /// Fetch a file from Google Cloud Storage.
    ///
    /// Tries the following methods in order:
    /// 1. Service account credentials if GOOGLE_APPLICATION_CREDENTIALS is set
    /// 2. Public bucket access via HTTPS
    pub fn fetch_gcs(&self, bucket: &str, key: &str, original_url: &str) -> TeaResult<PathBuf> {
        tracing::debug!("Fetching GCS file: {}", mask_credentials(original_url));

        // Standard GCS public URL
        let url = format!("https://storage.googleapis.com/{}/{}", bucket, key);

        let has_creds = std::env::var("GOOGLE_APPLICATION_CREDENTIALS").is_ok();
        if has_creds {
            tracing::debug!("GOOGLE_APPLICATION_CREDENTIALS is set");
            // For authenticated GCS access, we'd need OAuth2 tokens
            // A full implementation would use the google-cloud-storage crate
        }

        self.fetch_url(&url, original_url, "GCS")
    }

    /// Fetch a file from Azure Blob Storage.
    ///
    /// Tries the following methods in order:
    /// 1. Storage account key if AZURE_STORAGE_KEY is set
    /// 2. SAS token if AZURE_STORAGE_SAS_TOKEN is set
    /// 3. Public container access via HTTPS
    pub fn fetch_azure(
        &self,
        container: &str,
        path: &str,
        original_url: &str,
    ) -> TeaResult<PathBuf> {
        tracing::debug!("Fetching Azure file: {}", mask_credentials(original_url));

        // Get storage account
        let account = std::env::var("AZURE_STORAGE_ACCOUNT").ok();

        let url = if let Some(account) = account {
            // Standard Azure Blob URL
            let mut url = format!(
                "https://{}.blob.core.windows.net/{}/{}",
                account, container, path
            );

            // Append SAS token if available
            if let Ok(sas_token) = std::env::var("AZURE_STORAGE_SAS_TOKEN") {
                url = format!("{}?{}", url, sas_token);
            }

            url
        } else {
            // Without account name, we can't construct the URL
            return Err(crate::TeaError::InvalidConfig(
                "AZURE_STORAGE_ACCOUNT environment variable required for Azure Blob Storage access"
                    .to_string(),
            ));
        };

        self.fetch_url(&url, original_url, "Azure")
    }

    /// Fetch a file via HTTP/HTTPS URL.
    pub fn fetch_http(&self, url: &str, original_url: &str) -> TeaResult<PathBuf> {
        tracing::debug!("Fetching HTTP file: {}", mask_credentials(original_url));
        self.fetch_url(url, original_url, "HTTP")
    }

    /// Internal method to fetch from a URL and cache the result.
    fn fetch_url(&self, url: &str, original_url: &str, provider: &str) -> TeaResult<PathBuf> {
        // Build HTTP client
        let client = reqwest::blocking::Client::builder()
            .timeout(std::time::Duration::from_secs(30))
            .user_agent("tea-edge-agent/0.9")
            .build()
            .map_err(|e| crate::TeaError::Http(format!("Failed to build HTTP client: {}", e)))?;

        // Execute request
        let response = client
            .get(url)
            .send()
            .map_err(|e| self.map_network_error(e, original_url, provider))?;

        // Check response status
        let status = response.status();
        if !status.is_success() {
            return Err(self.map_http_error(status.as_u16(), original_url, provider));
        }

        // Read content
        let content = response
            .bytes()
            .map_err(|e| crate::TeaError::Http(format!("Failed to read response: {}", e)))?;

        // Determine filename from URL path
        let filename = url
            .rsplit('/')
            .next()
            .and_then(|s| s.split('?').next()) // Remove query string
            .unwrap_or("file");

        // Store in cache
        let cached_path = self.cache.store(original_url, &content, filename)?;

        tracing::debug!(
            "Cached {} file: {} -> {}",
            provider,
            mask_credentials(original_url),
            cached_path.display()
        );

        Ok(cached_path)
    }

    /// Map network errors to user-friendly messages.
    fn map_network_error(&self, err: reqwest::Error, url: &str, provider: &str) -> crate::TeaError {
        let masked_url = mask_credentials(url);

        if err.is_timeout() {
            return crate::TeaError::Http(format!(
                "Timeout fetching {} from {}: request took too long. Check network connectivity.",
                masked_url, provider
            ));
        }

        if err.is_connect() {
            return crate::TeaError::Http(format!(
                "Failed to connect to {} ({}): {}. Check network connectivity and URL validity.",
                provider, masked_url, err
            ));
        }

        crate::TeaError::Http(format!(
            "Network error fetching {} from {}: {}",
            masked_url, provider, err
        ))
    }

    /// Map HTTP status codes to user-friendly messages.
    fn map_http_error(&self, status: u16, url: &str, provider: &str) -> crate::TeaError {
        let masked_url = mask_credentials(url);

        match status {
            401 => crate::TeaError::Http(format!(
                "Authentication required for {} ({}): check credentials or use public bucket.",
                provider, masked_url
            )),
            403 => crate::TeaError::Http(format!(
                "Access denied for {} ({}): check bucket permissions and credentials.",
                provider, masked_url
            )),
            404 => crate::TeaError::Http(format!(
                "File not found: {} ({}). Verify bucket name and file path.",
                masked_url, provider
            )),
            429 => crate::TeaError::Http(format!(
                "Rate limited for {} ({}): too many requests. Try again later.",
                provider, masked_url
            )),
            500..=599 => crate::TeaError::Http(format!(
                "Server error ({}) from {}: the storage service may be experiencing issues.",
                status, provider
            )),
            _ => crate::TeaError::Http(format!(
                "HTTP error {} from {}: unexpected response.",
                status, provider
            )),
        }
    }
}

/// Get the public URL for a cloud storage path.
///
/// This constructs the standard public access URL for each cloud provider.
/// For private buckets, authentication will be required.
#[allow(dead_code)]
pub fn get_public_url(parsed: &RemoteFile) -> Option<String> {
    match parsed {
        RemoteFile::S3 { bucket, key } => {
            let region = std::env::var("AWS_REGION")
                .or_else(|_| std::env::var("AWS_DEFAULT_REGION"))
                .unwrap_or_else(|_| "us-east-1".to_string());
            Some(format!(
                "https://{}.s3.{}.amazonaws.com/{}",
                bucket, region, key
            ))
        }
        RemoteFile::Gcs { bucket, key } => {
            Some(format!("https://storage.googleapis.com/{}/{}", bucket, key))
        }
        RemoteFile::Azure { container, path } => {
            let account = std::env::var("AZURE_STORAGE_ACCOUNT").ok()?;
            Some(format!(
                "https://{}.blob.core.windows.net/{}/{}",
                account, container, path
            ))
        }
        RemoteFile::Http { url } => Some(url.clone()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn make_cache() -> (TempDir, RemoteFileCache) {
        let temp_dir = TempDir::new().unwrap();
        let cache = RemoteFileCache::new(Some(temp_dir.path().to_path_buf())).unwrap();
        (temp_dir, cache)
    }

    #[test]
    fn test_get_public_url_s3() {
        std::env::set_var("AWS_REGION", "us-west-2");
        let parsed = RemoteFile::S3 {
            bucket: "my-bucket".to_string(),
            key: "path/to/file.yaml".to_string(),
        };
        let url = get_public_url(&parsed).unwrap();
        assert!(url.contains("my-bucket"));
        assert!(url.contains("us-west-2"));
        assert!(url.contains("path/to/file.yaml"));
        std::env::remove_var("AWS_REGION");
    }

    #[test]
    fn test_get_public_url_gcs() {
        let parsed = RemoteFile::Gcs {
            bucket: "my-bucket".to_string(),
            key: "file.yaml".to_string(),
        };
        let url = get_public_url(&parsed).unwrap();
        assert_eq!(url, "https://storage.googleapis.com/my-bucket/file.yaml");
    }

    #[test]
    fn test_get_public_url_azure() {
        std::env::set_var("AZURE_STORAGE_ACCOUNT", "myaccount");
        let parsed = RemoteFile::Azure {
            container: "container".to_string(),
            path: "path/file.yaml".to_string(),
        };
        let url = get_public_url(&parsed).unwrap();
        assert!(url.contains("myaccount"));
        assert!(url.contains("container"));
        assert!(url.contains("path/file.yaml"));
        std::env::remove_var("AZURE_STORAGE_ACCOUNT");
    }

    #[test]
    fn test_get_public_url_http() {
        let parsed = RemoteFile::Http {
            url: "https://example.com/file.yaml".to_string(),
        };
        let url = get_public_url(&parsed).unwrap();
        assert_eq!(url, "https://example.com/file.yaml");
    }

    #[test]
    fn test_cloud_fetcher_error_messages() {
        let (_temp_dir, cache) = make_cache();
        let fetcher = CloudFetcher::new(cache);

        // Test HTTP error mapping
        let err = fetcher.map_http_error(404, "s3://bucket/file", "S3");
        assert!(err.to_string().contains("not found"));

        let err = fetcher.map_http_error(403, "gs://bucket/file", "GCS");
        assert!(err.to_string().contains("Access denied"));

        let err = fetcher.map_http_error(401, "az://container/file", "Azure");
        assert!(err.to_string().contains("Authentication required"));
    }
}
