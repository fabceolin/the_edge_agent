# Remote File Support

TEA supports loading workflow YAML files from remote storage and git repositories, enabling teams to share workflows without manual downloads.

## Supported Protocols

| Protocol | Format | Required Package |
|----------|--------|------------------|
| AWS S3 | `s3://bucket/path/file.yaml` | `pip install s3fs` |
| Google Cloud Storage | `gs://bucket/path/file.yaml` | `pip install gcsfs` |
| Azure Blob | `az://container/path/file.yaml` | `pip install adlfs` |
| GitHub | `github://user/repo@ref/path/file.yaml` | (built-in) |
| GitLab | `gitlab://user/repo@ref/path/file.yaml` | (built-in) |
| HTTPS | `https://example.com/file.yaml` | (built-in) |

## Quick Start

```bash
# Run workflow directly from GitHub
tea run github://fabceolin/the_edge_agent@main/examples/workflows/agent.yaml

# Use S3-hosted workflow
tea run s3://my-bucket/workflows/pipeline.yaml

# Resume from cloud-stored checkpoint
tea run workflow.yaml --checkpoint gs://checkpoints/run-123.pkl
```

## Git Protocol Details

### URL Format

```
github://owner/repo@ref/path/to/file.yaml
gitlab://owner/repo@ref/path/to/file.yaml
```

Components:
- `owner`: GitHub/GitLab username or organization
- `repo`: Repository name
- `ref`: Branch name, tag, or commit SHA
- `path`: Path to file within repository

### Reference Types

| Ref Type | Example | Cache Behavior |
|----------|---------|----------------|
| Branch | `@main`, `@develop` | TTL-based expiry |
| Tag | `@v1.0.0`, `@release` | Cached permanently |
| SHA | `@abc1234` | Cached permanently |

### Examples

```bash
# Latest from main branch
tea run github://user/repo@main/workflow.yaml

# Specific version tag
tea run github://user/repo@v2.1.0/workflow.yaml

# Specific commit
tea run github://user/repo@a1b2c3d4e5f6/workflow.yaml

# Feature branch
tea run github://user/repo@feature/new-thing/workflow.yaml
```

## Authentication

### Environment Variables

| Provider | Variable | Fallback |
|----------|----------|----------|
| AWS S3 | `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY` | IAM role |
| Google Cloud | `GOOGLE_APPLICATION_CREDENTIALS` | Default credentials |
| Azure | `AZURE_STORAGE_ACCOUNT_NAME`, `AZURE_STORAGE_ACCOUNT_KEY` | Managed identity |
| GitHub | `GITHUB_TOKEN` | `GIT_TOKEN` |
| GitLab | `GITLAB_TOKEN` | `GIT_TOKEN` |

### Example: Private Repository

```bash
# Set token for private GitHub repo
export GITHUB_TOKEN=ghp_xxxxxxxxxxxx

# Now you can access private repos
tea run github://myorg/private-repo@main/workflow.yaml
```

### Error Messages

TEA provides clear error messages when credentials are missing:

```
Error: Authentication required for private repository.
Set GITHUB_TOKEN environment variable.
```

## Caching System

Remote files are cached locally to reduce network calls and enable offline mode.

### Cache Location

```
~/.cache/tea/remote/
├── manifest.json          # URL -> cache entry mapping
└── files/
    └── {cache_key}/       # One directory per cached URL
        └── {filename}     # Actual cached file
```

The cache location follows [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html). Override with `XDG_CACHE_HOME` environment variable.

### Cache Configuration

| Variable | Description | Default |
|----------|-------------|---------|
| `TEA_CACHE_TTL` | Cache TTL in seconds | 3600 (1 hour) |
| `TEA_CACHE_MAX_SIZE` | Maximum cache size | 1GB |
| `TEA_FETCH_TIMEOUT` | Network timeout in seconds | 30 |

### CLI Flags

```bash
# Disable caching - always fetch fresh
tea run s3://bucket/workflow.yaml --no-cache

# Offline mode - fail if not cached
tea run s3://bucket/workflow.yaml --cache-only

# Custom cache directory
tea run s3://bucket/workflow.yaml --cache-dir /custom/cache
```

## Cache Management Commands

### List Cached Files

```bash
# Show all cached files
tea cache list

# Show as JSON
tea cache list --json

# Show only expired entries
tea cache list --expired

# Show only valid entries
tea cache list --valid
```

### Clear Cache

```bash
# Clear all cached files (with confirmation)
tea cache clear

# Clear without confirmation
tea cache clear --force

# Clear files older than 7 days
tea cache clear --older-than 7d

# Clear files older than 24 hours
tea cache clear --older-than 24h
```

### Cache Info

```bash
# Show cache statistics
tea cache info

# Show as JSON
tea cache info --json
```

Example output:

```
Remote File Cache
========================================
Location:       /home/user/.cache/tea/remote
Entries:        15 (12 valid, 3 expired)
Size:           45.2 MB / 1.0 GB (4.5%)
TTL:            3600s
Fetch Timeout:  30s
```

## Relative Path Resolution

When a remote YAML references other files with relative paths, TEA resolves them relative to the remote base URL.

### Example

Given `github://user/repo@main/workflows/main.yaml`:

```yaml
# workflows/main.yaml
name: main-workflow
nodes:
  - name: sub
    uses: ./sub/helper.yaml  # Resolves to github://user/repo@main/workflows/sub/helper.yaml
```

## Security

### SSRF Protection

TEA blocks requests to internal/private IP addresses:
- `localhost`, `127.0.0.1`, `::1`
- Private IP ranges (10.x.x.x, 192.168.x.x, 172.16-31.x.x)
- Cloud metadata endpoints (169.254.169.254)

### Path Traversal Prevention

All cache paths are validated to prevent directory traversal attacks. Attempts to escape the cache directory (e.g., `../../etc/passwd`) are blocked.

### Credential Masking

Credentials are never logged. When using `--verbose`, URLs with tokens appear as:

```
Cache MISS: github://user/repo@main/file.yaml?token=***
```

## Protocol Whitelist

By default, only these protocols are allowed:

- `s3://`
- `gs://` / `gcs://`
- `az://` / `abfs://`
- `github://`
- `gitlab://`
- `https://`
- `http://` (allowed with warning)

## Troubleshooting

### Missing Backend Package

```
Error: Backend package not installed for 's3://'. Install with: pip install s3fs
```

Solution: Install the required package for your cloud provider.

### Authentication Failure

```
Error: Authentication required for private repository. Set GITHUB_TOKEN environment variable.
```

Solution: Set the appropriate token in your environment.

### Network Failure with Cached Version

```
Error: Connection failed to s3://bucket/file.yaml
Tip: A cached version exists. Use --cache-only to use it.
```

Solution: Use `--cache-only` flag to work offline with cached files.

### Cache Full

The cache automatically removes oldest entries when it exceeds `TEA_CACHE_MAX_SIZE`. You can manually clear old entries:

```bash
tea cache clear --older-than 7d
```

## Best Practices

1. **Pin versions for production**: Use tags or SHAs instead of branches for reproducible deployments.

2. **Use cache for CI/CD**: Pre-warm the cache in CI environments to reduce build times.

3. **Set appropriate TTL**: For development, shorter TTL (e.g., 300s) ensures fresh files. For production, longer TTL (e.g., 86400s) reduces network calls.

4. **Monitor cache size**: Periodically check cache usage with `tea cache info` and clean up with `tea cache clear --older-than 30d`.

5. **Use organization tokens**: For CI/CD, create a dedicated token with minimal required permissions.
