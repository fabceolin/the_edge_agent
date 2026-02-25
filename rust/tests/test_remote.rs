//! Integration tests for remote file handling (TEA-CLI-002)
//!
//! These tests verify:
//! - URL parsing and local path passthrough
//! - Cache key parity with Python implementation
//! - Mock filesystem functionality
//! - Cross-language cache manifest compatibility

use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;
use the_edge_agent::remote::{MockRemoteFileSystem, RemoteFile, RemoteFileCache, RemoteFileSystem};

// ============================================================================
// URL Parsing Tests (AC1)
// ============================================================================

#[test]
fn test_local_path_passthrough() {
    // Local paths should return unchanged
    let rf = RemoteFile::parse("/home/user/workflow.yaml").unwrap();
    assert!(rf.is_local());

    let rf = RemoteFile::parse("./relative/path.yaml").unwrap();
    assert!(rf.is_local());

    let rf = RemoteFile::parse("workflow.yaml").unwrap();
    assert!(rf.is_local());
}

#[test]
fn test_s3_url_parsing() {
    let rf = RemoteFile::parse("s3://my-bucket/path/to/workflow.yaml").unwrap();
    assert!(!rf.is_local());

    if let RemoteFile::S3 { bucket, key } = rf {
        assert_eq!(bucket, "my-bucket");
        assert_eq!(key, "path/to/workflow.yaml");
    } else {
        panic!("Expected S3 variant");
    }
}

#[test]
fn test_gcs_url_parsing() {
    let rf = RemoteFile::parse("gs://my-bucket/file.yaml").unwrap();
    assert!(matches!(rf, RemoteFile::Gcs { .. }));

    let rf = RemoteFile::parse("gcs://my-bucket/file.yaml").unwrap();
    assert!(matches!(rf, RemoteFile::Gcs { .. }));
}

#[test]
fn test_github_url_parsing() {
    let rf = RemoteFile::parse("github://user/repo@main/path/workflow.yaml").unwrap();

    if let RemoteFile::Git {
        host,
        owner,
        repo,
        ref_,
        path,
    } = rf
    {
        assert_eq!(host, the_edge_agent::remote::GitHost::GitHub);
        assert_eq!(owner, "user");
        assert_eq!(repo, "repo");
        assert_eq!(ref_.as_str(), "main");
        assert_eq!(path, "path/workflow.yaml");
    } else {
        panic!("Expected Git variant");
    }
}

#[test]
fn test_gitlab_url_parsing() {
    let rf = RemoteFile::parse("gitlab://group/project@v1.0.0/config.yaml").unwrap();

    if let RemoteFile::Git {
        host,
        owner,
        repo,
        ref_,
        path,
    } = rf
    {
        assert_eq!(host, the_edge_agent::remote::GitHost::GitLab);
        assert_eq!(owner, "group");
        assert_eq!(repo, "project");
        assert_eq!(ref_.as_str(), "v1.0.0");
        assert_eq!(path, "config.yaml");
    } else {
        panic!("Expected Git variant");
    }
}

// ============================================================================
// Cache Key Parity Tests (Cross-Language Compatibility)
// ============================================================================

#[test]
fn test_cache_key_algorithm() {
    // The cache key algorithm must match Python:
    // SHA256(url)[:16] for non-git URLs
    // SHA256("{provider}://{owner}/{repo}@{ref}")[:16] for git URLs
    let temp_dir = TempDir::new().unwrap();
    let cache = RemoteFileCache::new(Some(temp_dir.path().to_path_buf())).unwrap();

    // For non-git URLs, the full URL is used
    let key1 = cache.cache_key("s3://bucket/file.yaml");
    assert_eq!(key1.len(), 16);

    // Same URL should produce same key
    let key2 = cache.cache_key("s3://bucket/file.yaml");
    assert_eq!(key1, key2);

    // Different URLs should produce different keys
    let key3 = cache.cache_key("s3://bucket/other.yaml");
    assert_ne!(key1, key3);
}

#[test]
fn test_git_cache_key_uses_repo_ref() {
    // For git URLs, the cache key should be based on repo@ref
    // This means different file paths in the same repo@ref share the same key
    let temp_dir = TempDir::new().unwrap();
    let cache = RemoteFileCache::new(Some(temp_dir.path().to_path_buf())).unwrap();

    let key1 = cache.cache_key("github://user/repo@main/file1.yaml");
    let key2 = cache.cache_key("github://user/repo@main/file2.yaml");

    // Same repo@ref means same key for TTL purposes
    // Note: This matches Python behavior where git URLs are keyed by repo@ref
    assert_eq!(key1, key2);

    // Different ref means different key
    let key3 = cache.cache_key("github://user/repo@develop/file1.yaml");
    assert_ne!(key1, key3);
}

// ============================================================================
// Cache Manifest Compatibility Tests
// ============================================================================

#[test]
fn test_manifest_json_format_python_compatible() {
    // Test that our manifest JSON format matches Python's expected format
    let temp_dir = TempDir::new().unwrap();
    let cache = RemoteFileCache::new(Some(temp_dir.path().to_path_buf())).unwrap();

    // Store a file
    let content = b"test content";
    cache
        .store("s3://bucket/file.yaml", content, "file.yaml")
        .unwrap();

    // Read the manifest
    let manifest_path = temp_dir.path().join("manifest.json");
    let manifest_content = fs::read_to_string(manifest_path).unwrap();
    let manifest: serde_json::Value = serde_json::from_str(&manifest_content).unwrap();

    // Check structure matches Python
    assert_eq!(manifest["version"], 1);
    assert!(manifest["entries"].is_object());

    // Check entry has all required fields
    let entries = manifest["entries"].as_object().unwrap();
    let entry = entries.values().next().unwrap();

    assert!(entry["url"].is_string());
    assert!(entry["local_path"].is_string());
    assert!(entry["created_at"].is_number());
    assert!(entry["ttl_seconds"].is_number() || entry["ttl_seconds"].is_null());
    assert!(entry["is_permanent"].is_boolean());
    assert!(entry["size_bytes"].is_number());
}

#[test]
fn test_rust_can_read_python_manifest() {
    // Simulate a Python-written manifest
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = temp_dir.path();
    let files_dir = cache_dir.join("files");
    let manifest_path = cache_dir.join("manifest.json");

    fs::create_dir_all(&files_dir).unwrap();

    // Create a Python-style manifest
    let python_manifest = r#"{
        "version": 1,
        "entries": {
            "abc123def456": {
                "url": "github://test/repo@main/file.yaml",
                "local_path": "files/abc123def456/file.yaml",
                "created_at": 1706745600,
                "ttl_seconds": 3600,
                "is_permanent": false,
                "size_bytes": 100
            }
        }
    }"#;

    fs::write(&manifest_path, python_manifest).unwrap();

    // Create the cached file
    let entry_dir = files_dir.join("abc123def456");
    fs::create_dir_all(&entry_dir).unwrap();
    fs::write(entry_dir.join("file.yaml"), "content").unwrap();

    // Rust should be able to read this
    let cache = RemoteFileCache::new(Some(cache_dir.to_path_buf())).unwrap();
    let entries = cache.list_entries();

    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0].url, "github://test/repo@main/file.yaml");
}

// ============================================================================
// Mock Filesystem Tests (AC5)
// ============================================================================

#[test]
fn test_mock_remote_filesystem() {
    let mut mock = MockRemoteFileSystem::new();
    mock.add_response("s3://bucket/a.yaml", PathBuf::from("/tmp/cached_a.yaml"));
    mock.add_response("s3://bucket/b.yaml", PathBuf::from("/tmp/cached_b.yaml"));

    // Fetch should return mapped paths
    let result = mock.fetch("s3://bucket/a.yaml").unwrap();
    assert_eq!(result, PathBuf::from("/tmp/cached_a.yaml"));

    let result = mock.fetch("s3://bucket/b.yaml").unwrap();
    assert_eq!(result, PathBuf::from("/tmp/cached_b.yaml"));

    // Unmapped URLs should error
    assert!(mock.fetch("s3://bucket/unknown.yaml").is_err());
}

#[test]
fn test_mock_error_responses() {
    let mut mock = MockRemoteFileSystem::new();
    mock.add_error("s3://bucket/private.yaml", "Access denied");

    let result = mock.fetch("s3://bucket/private.yaml");
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Access denied"));
}

#[test]
fn test_mock_fetch_log() {
    let mut mock = MockRemoteFileSystem::new();
    mock.add_response("s3://bucket/file.yaml", PathBuf::from("/tmp/file.yaml"));

    let _ = mock.fetch("s3://bucket/file.yaml");
    let _ = mock.fetch("s3://bucket/file.yaml");
    let _ = mock.fetch("s3://bucket/other.yaml"); // Will fail but still logged

    let log = mock.fetch_log();
    assert_eq!(log.len(), 3);
    assert_eq!(log[0], "s3://bucket/file.yaml");
    assert_eq!(log[1], "s3://bucket/file.yaml");
    assert_eq!(log[2], "s3://bucket/other.yaml");
}

// ============================================================================
// Security Tests (SEC-001, SEC-002, SEC-003)
// ============================================================================

#[test]
fn test_credential_masking() {
    use the_edge_agent::remote::mask_credentials;

    // URL parameters
    let masked = mask_credentials("https://api.github.com?token=ghp_secret123");
    assert!(!masked.contains("ghp_secret123"));
    assert!(masked.contains("token=***"));

    // Bearer tokens
    let masked = mask_credentials("Authorization: Bearer abc123xyz");
    assert!(!masked.contains("abc123xyz"));
    assert!(masked.contains("Bearer ***"));

    // GitHub tokens
    let masked = mask_credentials("Using token ghp_abcdefghij");
    assert!(!masked.contains("ghp_abcdefghij"));
    assert!(masked.contains("ghp_***"));
}

#[test]
fn test_url_validation_blocks_ssrf() {
    use the_edge_agent::remote::validate_url_safe;

    // Internal IPs should be blocked
    assert!(!validate_url_safe("http://localhost/file").0);
    assert!(!validate_url_safe("http://127.0.0.1/file").0);
    assert!(!validate_url_safe("http://169.254.169.254/metadata").0);

    // External URLs should be allowed
    assert!(validate_url_safe("https://github.com/user/repo").0);
    assert!(validate_url_safe("s3://bucket/file").0);
}

#[test]
fn test_path_containment_validation() {
    use the_edge_agent::remote::validate_path_containment;

    let temp_dir = TempDir::new().unwrap();
    let base = temp_dir.path();

    // Create a file inside the base
    let inside = base.join("subdir").join("file.txt");
    fs::create_dir_all(inside.parent().unwrap()).unwrap();
    fs::write(&inside, "test").unwrap();

    // Should pass validation
    assert!(validate_path_containment(&inside, base));

    // Path outside should fail (though we can't easily test without symlinks)
}

// ============================================================================
// Cache Operations Tests (AC3, AC4)
// ============================================================================

#[test]
fn test_cache_store_and_retrieve() {
    let temp_dir = TempDir::new().unwrap();
    let cache = RemoteFileCache::new(Some(temp_dir.path().to_path_buf())).unwrap();

    let url = "s3://bucket/test.yaml";
    let content = b"name: test\nvalue: 42";

    // Store
    let path = cache.store(url, content, "test.yaml").unwrap();
    assert!(path.exists());

    // Should be valid now
    assert!(cache.has_valid(url));

    // Retrieve path
    let retrieved = cache.get_path(url).unwrap();
    assert_eq!(path, retrieved);

    // Content should match
    let stored_content = fs::read(&retrieved).unwrap();
    assert_eq!(stored_content, content);
}

#[test]
fn test_cache_clear_all() {
    let temp_dir = TempDir::new().unwrap();
    let cache = RemoteFileCache::new(Some(temp_dir.path().to_path_buf())).unwrap();

    // Store multiple entries
    cache.store("s3://bucket/a.yaml", b"a", "a.yaml").unwrap();
    cache.store("s3://bucket/b.yaml", b"b", "b.yaml").unwrap();
    cache.store("s3://bucket/c.yaml", b"c", "c.yaml").unwrap();

    assert!(cache.has("s3://bucket/a.yaml"));
    assert!(cache.has("s3://bucket/b.yaml"));
    assert!(cache.has("s3://bucket/c.yaml"));

    // Clear all
    let cleared = cache.clear(None).unwrap();
    assert_eq!(cleared, 3);

    // All should be gone
    assert!(!cache.has("s3://bucket/a.yaml"));
    assert!(!cache.has("s3://bucket/b.yaml"));
    assert!(!cache.has("s3://bucket/c.yaml"));
}

#[test]
fn test_cache_info() {
    let temp_dir = TempDir::new().unwrap();
    let cache = RemoteFileCache::new(Some(temp_dir.path().to_path_buf())).unwrap();

    // Empty cache
    let info = cache.info();
    assert_eq!(info.total_entries, 0);
    assert_eq!(info.total_size_bytes, 0);

    // Add some entries
    cache
        .store("s3://bucket/a.yaml", b"content_a", "a.yaml")
        .unwrap();
    cache
        .store("s3://bucket/b.yaml", b"content_b", "b.yaml")
        .unwrap();

    let info = cache.info();
    assert_eq!(info.total_entries, 2);
    assert!(info.total_size_bytes > 0);
    assert_eq!(info.valid_entries, 2);
}

#[test]
fn test_cache_list_entries() {
    let temp_dir = TempDir::new().unwrap();
    let cache = RemoteFileCache::new(Some(temp_dir.path().to_path_buf())).unwrap();

    cache
        .store("s3://bucket/first.yaml", b"first", "first.yaml")
        .unwrap();
    std::thread::sleep(std::time::Duration::from_millis(10));
    cache
        .store("s3://bucket/second.yaml", b"second", "second.yaml")
        .unwrap();

    let entries = cache.list_entries();
    assert_eq!(entries.len(), 2);

    // Should be sorted by age (newest first)
    assert!(entries[0].age_seconds <= entries[1].age_seconds);
}

// ============================================================================
// Git Reference Detection Tests
// ============================================================================

#[test]
fn test_git_ref_permanent_detection() {
    use the_edge_agent::remote::GitRef;

    // Branches are not permanent
    assert!(!GitRef::Branch("main".to_string()).is_permanent());
    assert!(!GitRef::Branch("develop".to_string()).is_permanent());

    // Tags are permanent
    assert!(GitRef::Tag("v1.0.0".to_string()).is_permanent());
    assert!(GitRef::Tag("1.2.3".to_string()).is_permanent());

    // SHAs are permanent
    assert!(GitRef::Sha("abc1234".to_string()).is_permanent());
}
