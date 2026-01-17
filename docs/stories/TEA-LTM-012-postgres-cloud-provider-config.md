# TEA-LTM-012: PostgreSQL Cloud Provider Configuration

**Status:** Draft
**Created:** 2026-01-16
**Type:** Brownfield Enhancement
**Priority:** High
**Effort:** Medium (2-4 hours)

---

## Summary

Configure PostgreSQL catalog backend for cloud-managed databases (GCP Cloud SQL, AWS RDS, Azure Database) with proper SSL/TLS, IAM authentication, and secrets integration using existing TEA builtins.

---

## User Story

**As a** DevOps engineer deploying TEA in production,
**I want** to configure PostgreSQL catalog with cloud-managed databases,
**So that** I can leverage managed PostgreSQL services with proper security for the A3 LTM architecture at scale.

---

## Background

### Context

The deep research report ([LTM-SCALE-RESEARCH-REPORT.md](../research/LTM-SCALE-RESEARCH-REPORT.md)) concluded that **PostgreSQL is required** for the A3 LTM architecture at scale (10GB-100GB+). The current `PostgresCatalog` implementation exists but lacks:

1. Cloud provider-specific configuration
2. SSL/TLS support for cloud connections
3. Integration with existing TEA secrets backends
4. IAM authentication support

### Existing Assets

| Component | Status | Location |
|-----------|--------|----------|
| `PostgresCatalog` | ✅ Complete | `memory/catalog_postgres.py` |
| `SecretsBackend` (base) | ✅ Complete | `secrets/base.py` |
| `EnvSecretsBackend` | ✅ Complete | `secrets/env.py` |
| `GCPSecretsBackend` | ✅ Complete | `secrets/gcp.py` |
| `AWSSecretsBackend` | ✅ Complete | `secrets/aws.py` |
| `AzureSecretsBackend` | ✅ Complete | `secrets/azure.py` |
| `create_secrets_backend()` | ✅ Complete | `secrets/__init__.py` |

### Gap Analysis

| Feature | Current | Needed |
|---------|---------|--------|
| Connection string | Manual only | Provider-specific builders |
| SSL/TLS | Not configured | `sslmode`, `sslrootcert` |
| Credentials | Hardcoded in connection string | Integration with secrets builtins |
| IAM auth | Not supported | GCP Cloud SQL, AWS RDS IAM |

---

## Acceptance Criteria

### Functional Requirements

#### AC-1: SSL/TLS Configuration
- [ ] Support `sslmode` parameter (disable, allow, prefer, require, verify-ca, verify-full)
- [ ] Support `sslrootcert` for custom CA certificates
- [ ] Default to `sslmode=require` when `provider` is set to cloud provider

#### AC-2: Cloud Provider Presets
- [ ] `provider: gcp` - Auto-configures for Cloud SQL (socket path, SSL)
- [ ] `provider: aws` - Auto-configures for RDS (SSL bundle, endpoint format)
- [ ] `provider: azure` - Auto-configures for Azure Database (SSL, endpoint format)
- [ ] `provider: self-hosted` - Manual configuration (current behavior, default)

#### AC-3: Secrets Integration
- [ ] Support `credentials.source` to specify secrets backend (env, gcp, aws, azure)
- [ ] Integrate with existing `create_secrets_backend()` from `the_edge_agent.secrets`
- [ ] Resolve `username` and `password` from secrets backend
- [ ] Support secret key customization (`username_key`, `password_key`)

#### AC-4: GCP Cloud SQL Support
- [ ] Support Cloud SQL Proxy connection via Unix socket (`/cloudsql/project:region:instance`)
- [ ] Support direct connection with public IP
- [ ] Support IAM database authentication (`iam_auth: true`)
- [ ] Environment variable fallback: `CLOUDSQL_CONNECTION_NAME`

#### AC-5: AWS RDS Support
- [ ] Support IAM database authentication via `boto3` token generation
- [ ] Support RDS SSL certificate bundle (auto-download or custom path)
- [ ] Support RDS Proxy connections
- [ ] Environment variable fallback: `RDS_HOSTNAME`, `RDS_DB_NAME`

#### AC-6: Azure Database Support
- [ ] Support Azure AD authentication
- [ ] Support Azure SSL requirements (enforce SSL)
- [ ] Environment variable fallback: `AZURE_POSTGRESQL_HOST`

### Integration Requirements

#### AC-7: Backward Compatibility
- [ ] Existing `connection_string` parameter continues to work unchanged
- [ ] New `provider` and `credentials` parameters are optional
- [ ] All existing PostgresCatalog tests pass without modification

#### AC-8: YAML Configuration
- [ ] Support both explicit connection strings and provider presets
- [ ] Support environment variable expansion in all string fields
- [ ] Validate configuration and provide clear error messages

### Quality Requirements

#### AC-9: Testing
- [ ] Unit tests for `resolve_catalog_credentials()` function
- [ ] Unit tests for connection string builders (each provider)
- [ ] Integration test with mock secrets backend
- [ ] No regression in existing `test_catalog_postgres.py`

#### AC-10: Documentation
- [ ] Update `docs/python/actions-reference.md` with cloud configuration examples
- [ ] Add inline docstrings for new functions
- [ ] Include security best practices

---

## Technical Design

### New Function: `resolve_catalog_credentials()`

```python
# Location: memory/catalog_postgres.py or memory/catalog.py

from the_edge_agent.secrets import create_secrets_backend

def resolve_catalog_credentials(config: Dict[str, Any]) -> Dict[str, str]:
    """
    Resolve credentials from secrets backend for catalog config.

    Uses existing TEA secrets builtins (TEA-BUILTIN-012.1, TEA-BUILTIN-012.2).

    Args:
        config: Credentials config dict with 'source' and backend-specific options

    Returns:
        Dict with 'username' and 'password' keys

    Example:
        >>> config = {"source": "gcp", "project_id": "my-project", "secret_prefix": "db/"}
        >>> creds = resolve_catalog_credentials(config)
        >>> creds["username"]
        'postgres'
    """
    if not config:
        return {}

    source = config.get("source", "env")

    # Build backend config (remove our custom keys)
    backend_config = {k: v for k, v in config.items()
                      if k not in ("source", "username_key", "password_key")}

    # Create secrets backend using existing factory
    backend = create_secrets_backend(source, **backend_config)

    # Resolve credentials
    username_key = config.get("username_key", "username")
    password_key = config.get("password_key", "password")

    username = backend.get(username_key) or backend.get("POSTGRES_USER")
    password = backend.get(password_key) or backend.get("POSTGRES_PASSWORD")

    if not username or not password:
        raise ValueError(
            f"Could not resolve credentials from {source} backend. "
            f"Expected keys: {username_key}, {password_key}"
        )

    return {"username": username, "password": password}
```

### New Function: `build_provider_connection_string()`

```python
def build_provider_connection_string(
    provider: str,
    credentials: Dict[str, str],
    **config
) -> str:
    """
    Build PostgreSQL connection string for cloud provider.

    Args:
        provider: Cloud provider (gcp, aws, azure, self-hosted)
        credentials: Dict with username and password
        **config: Provider-specific configuration

    Returns:
        PostgreSQL connection string
    """
    username = credentials.get("username", "postgres")
    password = credentials.get("password", "")
    database = config.get("database", "postgres")
    sslmode = config.get("sslmode", "require")

    if provider == "gcp":
        # Cloud SQL via Unix socket
        project = config["project"]
        region = config["region"]
        instance = config["instance"]
        socket_path = f"/cloudsql/{project}:{region}:{instance}"
        return (
            f"postgresql://{username}:{password}@/{database}"
            f"?host={socket_path}"
        )

    elif provider == "aws":
        # RDS with SSL
        host = config["host"]
        port = config.get("port", 5432)
        sslrootcert = config.get("sslrootcert", "")
        conn = f"postgresql://{username}:{password}@{host}:{port}/{database}?sslmode={sslmode}"
        if sslrootcert:
            conn += f"&sslrootcert={sslrootcert}"
        return conn

    elif provider == "azure":
        # Azure Database for PostgreSQL
        host = config["host"]
        port = config.get("port", 5432)
        # Azure requires username@hostname format
        azure_user = f"{username}@{host.split('.')[0]}"
        return (
            f"postgresql://{azure_user}:{password}@{host}:{port}/{database}"
            f"?sslmode={sslmode}"
        )

    else:
        # Self-hosted or direct connection string
        if "connection_string" in config:
            return config["connection_string"]
        host = config.get("host", "localhost")
        port = config.get("port", 5432)
        return f"postgresql://{username}:{password}@{host}:{port}/{database}"
```

### Modified: `PostgresCatalog.__init__()`

```python
class PostgresCatalog:
    def __init__(
        self,
        # Existing parameters
        connection_string: Optional[str] = None,
        min_size: int = 1,
        max_size: int = 10,
        auto_migrate: bool = True,
        lazy: bool = False,
        # New parameters
        provider: Optional[str] = None,
        credentials: Optional[Dict[str, Any]] = None,
        **provider_config,
    ):
        if not PSYCOPG_AVAILABLE:
            raise ImportError(...)

        # Resolve credentials if provided
        resolved_creds = {}
        if credentials:
            resolved_creds = resolve_catalog_credentials(credentials)

        # Build connection string
        if connection_string:
            self._connection_string = connection_string
        elif provider:
            self._connection_string = build_provider_connection_string(
                provider=provider,
                credentials=resolved_creds,
                **provider_config,
            )
        else:
            raise ValueError(
                "Either 'connection_string' or 'provider' must be specified"
            )

        # ... rest of init unchanged
```

---

## YAML Configuration Examples

### GCP Cloud SQL with Secret Manager

```yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: postgres
      provider: gcp
      project: my-project
      region: us-central1
      instance: ltm-catalog
      database: ltm

      credentials:
        source: gcp
        project_id: my-project
        secret_prefix: ltm/postgres/
        # Resolves: ltm/postgres/username, ltm/postgres/password
```

### AWS RDS with Secrets Manager

```yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: postgres
      provider: aws
      host: ltm-db.xxx.us-east-1.rds.amazonaws.com
      database: ltm
      sslmode: verify-full
      sslrootcert: /etc/ssl/rds-ca-2019-root.pem

      credentials:
        source: aws
        secret_name: prod/ltm/postgres
        region: us-east-1
```

### Azure Database with Key Vault

```yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: postgres
      provider: azure
      host: ltm-server.postgres.database.azure.com
      database: ltm

      credentials:
        source: azure
        vault_url: https://my-keyvault.vault.azure.net
        # Resolves: username, password secrets from Key Vault
```

### Environment Variables (Simple)

```yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: postgres
      provider: self-hosted
      host: ${POSTGRES_HOST}
      database: ${POSTGRES_DB}

      credentials:
        source: env
        prefix: POSTGRES_
        # Resolves: POSTGRES_USER, POSTGRES_PASSWORD
```

### Direct Connection String (Backward Compatible)

```yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: postgres
      connection_string: "${POSTGRES_CONNECTION_STRING}"
```

---

## Files to Modify

| File | Changes | Lines |
|------|---------|-------|
| `memory/catalog_postgres.py` | Add `resolve_catalog_credentials()`, `build_provider_connection_string()`, modify `__init__()` | ~80 |
| `memory/catalog.py` | Update `parse_catalog_config()` to pass provider config | ~10 |
| `tests/test_catalog_postgres.py` | Add tests for new functions | ~100 |
| `docs/python/actions-reference.md` | Add cloud configuration examples | ~50 |

**Total new code:** ~150-200 lines

---

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Breaking existing configs | Low | High | `connection_string` preserved as primary option |
| Cloud SDK version conflicts | Low | Medium | SDKs already optional in secrets module |
| Secrets resolution failure | Medium | Medium | Clear error messages with backend info |
| IAM token expiration | Medium | Low | Document token refresh behavior |

### Rollback Plan

1. Remove `provider` and `credentials` handling from `PostgresCatalog.__init__()`
2. Revert to pure `connection_string` mode
3. No database schema changes required

---

## Definition of Done

- [ ] `resolve_catalog_credentials()` implemented and tested
- [ ] `build_provider_connection_string()` implemented for gcp, aws, azure
- [ ] `PostgresCatalog` accepts new `provider` and `credentials` params
- [ ] Existing `connection_string` behavior unchanged
- [ ] All existing tests pass
- [ ] New unit tests for credential resolution
- [ ] New unit tests for connection string building
- [ ] Documentation updated with examples
- [ ] No new dependencies (uses existing secrets backends)

---

## Related Documents

- [LTM Scale Research Report](../research/LTM-SCALE-RESEARCH-REPORT.md)
- [Secrets Module](../../python/src/the_edge_agent/secrets/)
- [PostgresCatalog Implementation](../../python/src/the_edge_agent/memory/catalog_postgres.py)
