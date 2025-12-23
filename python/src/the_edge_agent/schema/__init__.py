"""
Schema utilities for The Edge Agent.

This package provides schema manipulation tools:
- deep_merge: kubectl-style deep merge for JSON Schemas
- schema_loader: Load schemas from Git refs and fsspec URIs

TEA-BUILTIN-008: LlamaExtract Integration Epic
"""

from .deep_merge import deep_merge, merge_all, validate_json_schema
from .schema_loader import (
    fetch_schema,
    parse_git_reference,
    is_git_reference,
    is_fsspec_uri,
    resolve_schema_uses,
    clear_cache,
    GitReference,
    GitSchemaFetcher,
    FsspecSchemaFetcher,
    SchemaCache,
)

__all__ = [
    # Deep merge
    'deep_merge',
    'merge_all',
    'validate_json_schema',
    # Schema loader
    'fetch_schema',
    'parse_git_reference',
    'is_git_reference',
    'is_fsspec_uri',
    'resolve_schema_uses',
    'clear_cache',
    'GitReference',
    'GitSchemaFetcher',
    'FsspecSchemaFetcher',
    'SchemaCache',
]
