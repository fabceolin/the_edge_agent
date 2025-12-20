"""
Cloud Memory Actions for TEA YAMLEngine (TEA-BUILTIN-006).

Provides custom actions for storing and retrieving agent memory artifacts
in cloud storage with searchable metadata.

Migrated from: firebase/functions-agents/actions/cloud_memory.py
Uses MetadataStore and BlobStorage ABCs for provider portability.

Risk Mitigations Implemented:
- SEC-001: Path traversal prevention via _sanitize_path()
- DATA-001: Storage-first pattern with metadata rollback
- SEC-002: Project-level authorization enforcement

Actions:
- memory.cloud_store: Store artifacts with metadata and embeddings
- memory.cloud_retrieve: Retrieve artifacts
- memory.cloud_list: List with filters
- memory.manifest_update: Update metadata only
- memory.manifest_search: Search by anchors
"""

import hashlib
import json
import logging
import os
import re
import uuid
from datetime import datetime, timezone
from typing import Any, Callable, Dict, List, Optional, Tuple
from urllib.parse import unquote

from ..memory.metadata import MetadataStore, MetadataQuery
from ..memory.blob import BlobStorage

# Configure logging
logger = logging.getLogger(__name__)


# =============================================================================
# CONSTANTS
# =============================================================================

# Firestore collection name
AGENT_MEMORY_COLLECTION = "agent_memory"

# File size limits (BMAD sharding limits)
MAX_TOKENS = 4000
MAX_LINES = 500
MAX_BYTES = 100 * 1024  # 100KB hard limit

# Path limits
MAX_PATH_LENGTH = 1000
MAX_PATH_DEPTH = 20

# Allowed file extensions
ALLOWED_EXTENSIONS = {".yaml", ".yml", ".json", ".md"}

# Disallowed extensions (security)
DISALLOWED_EXTENSIONS = {
    ".exe", ".sh", ".bat", ".cmd", ".ps1", ".py", ".js", ".ts",
    ".php", ".rb", ".pl", ".bash", ".zsh", ".csh", ".ksh"
}

# Character allowlist pattern (alphanumeric, hyphen, underscore, dot, slash)
PATH_CHAR_PATTERN = re.compile(r'^[a-zA-Z0-9_\-./]+$')

# Project ID pattern (alphanumeric and hyphens only)
PROJECT_ID_PATTERN = re.compile(r'^[a-zA-Z0-9\-]+$')


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def _get_metadata_store(state: Dict[str, Any], kwargs: Dict[str, Any]) -> MetadataStore:
    """Get MetadataStore from state or kwargs."""
    store = kwargs.get('metadata_store')
    if store is not None:
        return store

    store = state.get('_metadata_store')
    if store is not None:
        return store

    store = state.get('metadata_store')
    if store is not None:
        return store

    raise ValueError(
        "No MetadataStore available. Pass metadata_store in kwargs or state."
    )


def _get_blob_storage(state: Dict[str, Any], kwargs: Dict[str, Any]) -> BlobStorage:
    """Get BlobStorage from state or kwargs."""
    storage = kwargs.get('blob_storage')
    if storage is not None:
        return storage

    storage = state.get('_blob_storage')
    if storage is not None:
        return storage

    storage = state.get('blob_storage')
    if storage is not None:
        return storage

    raise ValueError(
        "No BlobStorage available. Pass blob_storage in kwargs or state."
    )


# =============================================================================
# PATH SANITIZATION (SEC-001)
# =============================================================================

def _sanitize_path(path: str, project_id: str) -> Tuple[bool, str, Optional[str]]:
    """
    Sanitize and validate a storage path.

    Implements SEC-001 mitigation: Strict allowlist validation for path traversal prevention.

    Args:
        path: User-provided path (e.g., "shards/analysis/task-123.yaml")
        project_id: Authenticated project ID for prefix enforcement

    Returns:
        Tuple of (is_valid, sanitized_path_or_error, full_storage_path)
    """
    # Validate project_id first
    if not project_id or not PROJECT_ID_PATTERN.match(project_id):
        return False, "Invalid project_id: must be alphanumeric with hyphens only", None

    if ".." in project_id or "/" in project_id or "\\" in project_id:
        return False, "Invalid project_id: contains invalid characters", None

    # Empty or whitespace check
    if not path or not path.strip():
        return False, "Invalid path: empty or whitespace-only", None

    path = path.strip()

    # Reject single dot (current directory reference)
    if path == "." or path == "./":
        return False, "Invalid path: current directory reference not allowed", None

    # Length check
    if len(path) > MAX_PATH_LENGTH:
        return False, f"Invalid path: exceeds {MAX_PATH_LENGTH} character limit", None

    # URL decode to catch encoded attacks (%2e%2e = ..)
    decoded = path
    for _ in range(3):
        prev = decoded
        try:
            decoded = unquote(decoded)
        except Exception:
            pass
        if decoded == prev:
            break

    # Check for null bytes
    if '\x00' in path or '\x00' in decoded or '%00' in path.lower():
        return False, "Invalid path: contains null bytes", None

    # Check for path traversal patterns
    traversal_patterns = [
        "..", "..\\", "../", "..;", ".../", "..../",
        ". /", ".. ", "\\..",
    ]

    for pattern in traversal_patterns:
        if pattern in path or pattern in decoded:
            return False, f"Invalid path: path traversal detected ({pattern!r})", None

    # Check for absolute paths
    if path.startswith('/') or decoded.startswith('/'):
        return False, "Invalid path: absolute paths not allowed", None

    # Check for Windows drive letters
    if len(path) >= 2 and path[1] == ':':
        return False, "Invalid path: Windows drive paths not allowed", None

    # Check for UNC paths
    if path.startswith('\\\\') or decoded.startswith('\\\\'):
        return False, "Invalid path: UNC paths not allowed", None

    # Check for protocol prefixes
    protocol_prefixes = ["gs://", "s3://", "http://", "https://", "file://", "ftp://"]
    for prefix in protocol_prefixes:
        if path.lower().startswith(prefix) or decoded.lower().startswith(prefix):
            return False, f"Invalid path: protocol prefix not allowed ({prefix})", None

    # Check for dangerous characters
    dangerous_chars = ['$', '`', '|', ';', '&', '<', '>', '!', '@', '#', '*', '?', '\\',
                       '\n', '\r', '\t', "'", '"']
    for char in dangerous_chars:
        if char in path:
            return False, f"Invalid path: contains dangerous character ({char!r})", None

    # Check character allowlist
    if not PATH_CHAR_PATTERN.match(path):
        return False, "Invalid path: contains invalid characters", None

    # Check for path depth
    path_parts = [p for p in path.split('/') if p]
    if len(path_parts) > MAX_PATH_DEPTH:
        return False, f"Invalid path: exceeds maximum depth of {MAX_PATH_DEPTH}", None

    # Normalize consecutive slashes
    normalized = re.sub(r'/+', '/', path)
    normalized = normalized.rstrip('/')

    # Check file extension
    if '.' in normalized:
        ext = '.' + normalized.rsplit('.', 1)[-1].lower()
        if ext in DISALLOWED_EXTENSIONS:
            return False, f"Invalid path: extension {ext} not allowed (security risk)", None

    # Construct full storage path
    full_path = f"agent-memory/{project_id}/{normalized}"

    return True, normalized, full_path


def _validate_file_size(
    content: str,
    max_tokens: int = MAX_TOKENS,
    max_lines: int = MAX_LINES
) -> Tuple[bool, str]:
    """Validate file size against BMAD sharding limits."""
    byte_size = len(content.encode('utf-8'))
    if byte_size > MAX_BYTES:
        return False, f"File exceeds {MAX_BYTES // 1024}KB limit ({byte_size} bytes)"

    lines = content.split('\n')
    line_count = len(lines)
    if line_count > max_lines:
        return False, f"File exceeds {max_lines} lines ({line_count} lines)"

    token_estimate = len(content) // 4
    if token_estimate > max_tokens:
        return False, f"File exceeds {max_tokens} tokens (~{token_estimate} tokens)"

    return True, ""


def _detect_content_type(path: str, explicit_type: Optional[str] = None) -> str:
    """Detect or validate content type based on path extension."""
    if explicit_type and explicit_type in ("yaml", "json", "md"):
        return explicit_type

    ext = path.rsplit('.', 1)[-1].lower() if '.' in path else ""

    if ext in ("yaml", "yml"):
        return "yaml"
    elif ext == "json":
        return "json"
    elif ext == "md":
        return "md"

    return "yaml"


def _compute_content_hash(content: str | bytes) -> str:
    """Compute SHA-256 hash of content."""
    if isinstance(content, str):
        content = content.encode('utf-8')
    digest = hashlib.sha256(content).hexdigest()
    return f"sha256:{digest}"


def _generate_doc_id(project_id: str, file_path: str) -> str:
    """Generate deterministic document ID from project and path."""
    combined = f"{project_id}/{file_path}"
    return hashlib.sha256(combined.encode()).hexdigest()[:20]


# =============================================================================
# TEA CUSTOM ACTIONS
# =============================================================================

def memory_cloud_store(
    state: Dict[str, Any],
    path: str,
    content: str,
    content_type: str = None,
    anchors: List[str] = None,
    metadata: Dict[str, Any] = None,
    skip_embedding: bool = False,
    **kwargs
) -> Dict[str, Any]:
    """
    Store an artifact in cloud storage with metadata and embedding.

    TEA Custom Action: memory.cloud_store

    Args:
        state: Current agent state (must contain project_id)
        path: Relative path for the file
        content: Content to store
        content_type: Optional explicit content type (yaml, json, md)
        anchors: List of anchor tags for searching
        metadata: Additional metadata to store
        skip_embedding: If True, skip embedding generation
        **kwargs: May contain metadata_store and blob_storage

    Returns:
        Dict with success, file_path, doc_id, or error
    """
    correlation_id = str(uuid.uuid4())[:8]
    logger.info(f"[{correlation_id}] memory.cloud_store starting: path={path}")

    project_id = state.get("project_id", "rankellix")
    metadata = metadata or {}

    # Get backends
    try:
        store = _get_metadata_store(state, kwargs)
        storage = _get_blob_storage(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    # Step 1: Validate and sanitize path (SEC-001)
    is_valid, path_or_error, full_storage_path = _sanitize_path(path, project_id)
    if not is_valid:
        logger.warning(f"[{correlation_id}] Path sanitization failed: {path_or_error}")
        return {"success": False, "error": path_or_error, "error_type": "validation_error"}

    sanitized_path = path_or_error

    # Step 2: Validate file size
    is_valid_size, size_error = _validate_file_size(content)
    if not is_valid_size:
        logger.warning(f"[{correlation_id}] Size validation failed: {size_error}")
        return {"success": False, "error": size_error, "error_type": "validation_error"}

    # Step 3: Detect content type
    detected_type = _detect_content_type(sanitized_path, content_type)

    # Calculate metrics
    byte_size = len(content.encode('utf-8'))
    line_count = len(content.split('\n'))
    token_estimate = len(content) // 4

    # Compute content hash
    content_hash = _compute_content_hash(content)
    logger.debug(f"[{correlation_id}] Content hash computed: {content_hash[:20]}...")

    # Handle embeddings (check cache, generate if needed)
    embedding = None
    embedding_model = None
    embedding_tokens = 0
    embedding_status = "skipped"

    if not skip_embedding:
        doc_id_check = _generate_doc_id(project_id, sanitized_path)
        existing = store.get_document(AGENT_MEMORY_COLLECTION, doc_id_check)

        if existing.get("success") and existing.get("data"):
            existing_data = existing["data"]
            if existing_data.get("content_hash") == content_hash and existing_data.get("embedding"):
                embedding = existing_data.get("embedding")
                embedding_model = existing_data.get("embedding_model")
                embedding_status = "cached"
                logger.debug(f"[{correlation_id}] Reusing cached embedding")

        # Generate new embedding if needed (via action registry if available)
        if embedding is None:
            embed_action = kwargs.get('embed_action')
            if embed_action:
                try:
                    embed_result = embed_action(state, content)
                    if embed_result.get("success"):
                        embedding = embed_result["embedding"]
                        embedding_model = embed_result.get("model")
                        embedding_tokens = embed_result.get("tokens", 0)
                        embedding_status = "generated"
                except Exception as e:
                    embedding_status = "failed:exception"
                    logger.warning(f"[{correlation_id}] Embedding generation failed: {e}")

    # Step 4: Upload to Storage FIRST (DATA-001)
    now = datetime.now(timezone.utc)

    upload_result = storage.upload(
        path=full_storage_path,
        content=content,
        content_type=detected_type,
        metadata={"project_id": project_id}
    )

    if not upload_result.get("success"):
        logger.error(f"[{correlation_id}] Storage upload failed: {upload_result.get('error')}")
        return {
            "success": False,
            "error": f"Storage upload failed: {upload_result.get('error')}",
            "error_type": upload_result.get("error_type", "connection_error")
        }

    storage_uri = upload_result.get("uri", storage.get_uri(full_storage_path))
    logger.info(f"[{correlation_id}] Storage upload successful: {storage_uri}")

    # Step 5: Create metadata document
    doc_id = _generate_doc_id(project_id, sanitized_path)

    doc_data = {
        "file_path": sanitized_path,
        "content_type": detected_type,
        "storage_uri": storage_uri,
        "anchors": anchors or [],
        "status": metadata.get("status", "active"),
        "summary": metadata.get("summary", ""),
        "byte_size": byte_size,
        "line_count": line_count,
        "token_estimate": token_estimate,
        "created_at": now.isoformat(),
        "updated_at": now.isoformat(),
        "created_by": metadata.get("created_by", "agent:unknown"),
        "project_id": project_id,
        "content_hash": content_hash,
        "synced_at": None,
        "embedding": embedding,
        "embedding_model": embedding_model,
    }

    set_result = store.set_document(AGENT_MEMORY_COLLECTION, doc_id, doc_data)

    if not set_result.get("success"):
        # ROLLBACK: Delete Storage blob (DATA-001)
        logger.error(f"[{correlation_id}] Metadata write failed, rolling back Storage")
        storage.delete(full_storage_path)
        return {
            "success": False,
            "error": f"Metadata write failed: {set_result.get('error')}",
            "error_type": set_result.get("error_type", "connection_error")
        }

    logger.info(f"[{correlation_id}] Metadata created: doc_id={doc_id}")

    return {
        "success": True,
        "file_path": sanitized_path,
        "doc_id": doc_id,
        "storage_uri": storage_uri,
        "byte_size": byte_size,
        "line_count": line_count,
        "token_estimate": token_estimate,
        "content_hash": content_hash,
        "embedding_status": embedding_status,
        "embedding_tokens": embedding_tokens
    }


def memory_cloud_retrieve(
    state: Dict[str, Any],
    path: str = None,
    doc_id: str = None,
    parse: bool = False,
    **kwargs
) -> Dict[str, Any]:
    """
    Retrieve an artifact from cloud storage.

    TEA Custom Action: memory.cloud_retrieve

    Args:
        state: Current agent state (must contain project_id)
        path: Relative path of the file (optional if doc_id provided)
        doc_id: Document ID (optional if path provided)
        parse: If True, auto-parse YAML/JSON content

    Returns:
        Dict with success, content, metadata, or error
    """
    project_id = state.get("project_id", "rankellix")

    if not path and not doc_id:
        return {
            "success": False,
            "error": "Either 'path' or 'doc_id' must be provided",
            "error_type": "validation_error"
        }

    try:
        store = _get_metadata_store(state, kwargs)
        storage = _get_blob_storage(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    # Resolve doc_id from path if needed
    if not doc_id and path:
        is_valid, path_or_error, _ = _sanitize_path(path, project_id)
        if not is_valid:
            return {"success": False, "error": path_or_error, "error_type": "validation_error"}
        doc_id = _generate_doc_id(project_id, path_or_error)

    # Get metadata
    result = store.get_document(AGENT_MEMORY_COLLECTION, doc_id)
    if not result.get("success") or not result.get("data"):
        return {
            "success": False,
            "error": "Document not found in metadata store",
            "error_type": "not_found"
        }

    metadata = result["data"]

    # Verify project isolation
    if metadata.get("project_id") != project_id:
        return {
            "success": False,
            "error": "Access denied: project mismatch",
            "error_type": "permission_denied"
        }

    # Download from Storage
    storage_uri = metadata.get("storage_uri", "")
    if not storage_uri:
        return {
            "success": False,
            "error": "Invalid storage URI in metadata",
            "error_type": "validation_error"
        }

    # Parse storage URI to get path
    uri_info = storage.parse_uri(storage_uri)
    if "error" in uri_info:
        return {
            "success": False,
            "error": uri_info["error"],
            "error_type": "validation_error"
        }

    download_result = storage.download(uri_info["path"])
    if not download_result.get("success"):
        return {
            "success": False,
            "error": download_result.get("error", "Download failed"),
            "error_type": download_result.get("error_type", "connection_error"),
            "metadata": metadata
        }

    content = download_result["content"]

    # Parse content if requested
    parsed = None
    if parse:
        content_type = metadata.get("content_type", "yaml")
        try:
            if content_type in ("yaml", "yml"):
                import yaml
                parsed = yaml.safe_load(content)
            elif content_type == "json":
                parsed = json.loads(content)
        except Exception as e:
            logger.warning(f"Content parsing failed: {e}")

    return {
        "success": True,
        "content": content,
        "parsed": parsed,
        "metadata": {
            "file_path": metadata.get("file_path"),
            "content_type": metadata.get("content_type"),
            "anchors": metadata.get("anchors", []),
            "status": metadata.get("status"),
            "summary": metadata.get("summary"),
            "byte_size": metadata.get("byte_size"),
            "line_count": metadata.get("line_count"),
            "token_estimate": metadata.get("token_estimate"),
            "created_at": metadata.get("created_at"),
            "updated_at": metadata.get("updated_at"),
        }
    }


def memory_cloud_list(
    state: Dict[str, Any],
    prefix: str = None,
    anchors: List[str] = None,
    status: str = None,
    content_type: str = None,
    limit: int = 100,
    offset: int = 0,
    **kwargs
) -> Dict[str, Any]:
    """
    List artifacts with filtering.

    TEA Custom Action: memory.cloud_list

    Args:
        state: Current agent state (must contain project_id)
        prefix: Filter by path prefix
        anchors: Filter by anchors (OR logic)
        status: Filter by status (active, archived, draft)
        content_type: Filter by content type
        limit: Maximum results (default 100)
        offset: Skip first N results

    Returns:
        Dict with success and files list
    """
    project_id = state.get("project_id", "rankellix")

    try:
        store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    # Build filters
    filters = [("project_id", "==", project_id)]
    if status:
        filters.append(("status", "==", status))
    if content_type:
        filters.append(("content_type", "==", content_type))

    query = MetadataQuery(
        collection=AGENT_MEMORY_COLLECTION,
        filters=filters,
        limit=limit + offset  # Get extra for offset
    )

    result = store.query(query)

    if not result.get("success"):
        return result

    # Post-filter for anchors and prefix
    files = []
    skipped = 0

    for doc in result.get("documents", []):
        data = doc.get("data", {})

        # Filter by anchors (OR logic)
        if anchors:
            doc_anchors = set(data.get("anchors", []))
            if not any(a in doc_anchors for a in anchors):
                continue

        # Filter by prefix
        if prefix:
            if not data.get("file_path", "").startswith(prefix):
                continue

        # Apply offset
        if skipped < offset:
            skipped += 1
            continue

        files.append({
            "path": data.get("file_path"),
            "doc_id": doc.get("id"),
            "anchors": data.get("anchors", []),
            "status": data.get("status"),
            "summary": data.get("summary", ""),
            "content_type": data.get("content_type"),
            "byte_size": data.get("byte_size"),
            "updated_at": data.get("updated_at"),
        })

        if len(files) >= limit:
            break

    return {
        "success": True,
        "files": files,
        "count": len(files),
        "limit": limit,
        "offset": offset
    }


def memory_manifest_update(
    state: Dict[str, Any],
    doc_id: str,
    anchors: List[str] = None,
    anchors_add: List[str] = None,
    anchors_remove: List[str] = None,
    status: str = None,
    summary: str = None,
    **kwargs
) -> Dict[str, Any]:
    """
    Update metadata only (not file content).

    TEA Custom Action: memory.manifest_update

    Args:
        state: Current agent state (must contain project_id)
        doc_id: Document ID to update
        anchors: Replace all anchors (if provided)
        anchors_add: Anchors to add
        anchors_remove: Anchors to remove
        status: New status value
        summary: New summary text

    Returns:
        Dict with success status
    """
    project_id = state.get("project_id", "rankellix")

    try:
        store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    # Verify document exists and belongs to project
    result = store.get_document(AGENT_MEMORY_COLLECTION, doc_id)
    if not result.get("success") or not result.get("data"):
        return {
            "success": False,
            "error": "Document not found",
            "error_type": "not_found"
        }

    doc_data = result["data"]
    if doc_data.get("project_id") != project_id:
        return {
            "success": False,
            "error": "Access denied: project mismatch",
            "error_type": "permission_denied"
        }

    # Build update
    now = datetime.now(timezone.utc)
    updates = {"updated_at": now.isoformat()}

    if anchors is not None:
        updates["anchors"] = anchors
    elif anchors_add or anchors_remove:
        # Get current anchors and modify
        current_anchors = set(doc_data.get("anchors", []))
        if anchors_add:
            current_anchors.update(anchors_add)
        if anchors_remove:
            current_anchors -= set(anchors_remove)
        updates["anchors"] = list(current_anchors)

    if status is not None:
        updates["status"] = status
    if summary is not None:
        updates["summary"] = summary

    update_result = store.update_document(AGENT_MEMORY_COLLECTION, doc_id, updates)

    if not update_result.get("success"):
        return update_result

    return {"success": True, "doc_id": doc_id}


def memory_manifest_search(
    state: Dict[str, Any],
    anchor: str = None,
    anchors_any: List[str] = None,
    status: str = None,
    limit: int = 100,
    offset: int = 0,
    **kwargs
) -> Dict[str, Any]:
    """
    Search documents by anchors.

    TEA Custom Action: memory.manifest_search

    Args:
        state: Current agent state (must contain project_id)
        anchor: Single anchor for exact match
        anchors_any: List of anchors for OR search
        status: Filter by status
        limit: Maximum results
        offset: Pagination offset

    Returns:
        Dict with success and matching documents
    """
    project_id = state.get("project_id", "rankellix")

    # Build anchor list
    search_anchors = []
    if anchor:
        search_anchors.append(anchor)
    if anchors_any:
        search_anchors.extend(anchors_any)

    if not search_anchors:
        return {
            "success": False,
            "error": "Either 'anchor' or 'anchors_any' must be provided",
            "error_type": "validation_error"
        }

    try:
        store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    # Build query
    filters = [("project_id", "==", project_id)]
    if status:
        filters.append(("status", "==", status))

    query = MetadataQuery(
        collection=AGENT_MEMORY_COLLECTION,
        filters=filters,
        limit=limit + offset
    )

    result = store.query(query)

    if not result.get("success"):
        return result

    # Post-filter for anchors
    documents = []
    skipped = 0

    for doc in result.get("documents", []):
        data = doc.get("data", {})
        doc_anchors = set(data.get("anchors", []))

        # OR logic: any anchor matches
        if not any(a in doc_anchors for a in search_anchors):
            continue

        # Apply offset
        if skipped < offset:
            skipped += 1
            continue

        documents.append({
            "doc_id": doc.get("id"),
            "file_path": data.get("file_path"),
            "anchors": data.get("anchors", []),
            "status": data.get("status"),
            "summary": data.get("summary", ""),
            "content_type": data.get("content_type"),
            "updated_at": data.get("updated_at"),
        })

        if len(documents) >= limit:
            break

    return {
        "success": True,
        "documents": documents,
        "count": len(documents),
        "limit": limit,
        "offset": offset
    }


# =============================================================================
# ACTION REGISTRATION
# =============================================================================

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register cloud memory actions with the TEA YAMLEngine.

    Args:
        registry: Action registry dictionary
        engine: YAMLEngine instance
    """
    # Wrapper to inject engine backends into kwargs
    def wrap_with_backends(fn: Callable) -> Callable:
        """Wrap action to inject engine's metadata_store and blob_storage if not provided."""
        def wrapped(state: Dict[str, Any], *args, **kwargs) -> Dict[str, Any]:
            # Inject metadata_store from engine if not already provided
            if 'metadata_store' not in kwargs:
                if hasattr(engine, '_metadata_store') and engine._metadata_store is not None:
                    kwargs['metadata_store'] = engine._metadata_store
            # Inject blob_storage from engine if not already provided
            if 'blob_storage' not in kwargs:
                if hasattr(engine, '_blob_storage') and engine._blob_storage is not None:
                    kwargs['blob_storage'] = engine._blob_storage
            return fn(state, *args, **kwargs)
        # Preserve function metadata for introspection
        wrapped.__name__ = fn.__name__
        wrapped.__doc__ = fn.__doc__
        return wrapped

    registry["memory.cloud_store"] = wrap_with_backends(memory_cloud_store)
    registry["memory.cloud_retrieve"] = wrap_with_backends(memory_cloud_retrieve)
    registry["memory.cloud_list"] = wrap_with_backends(memory_cloud_list)
    registry["memory.manifest_update"] = wrap_with_backends(memory_manifest_update)
    registry["memory.manifest_search"] = wrap_with_backends(memory_manifest_search)

    logger.info(
        "Cloud memory actions registered: "
        "memory.cloud_store, memory.cloud_retrieve, memory.cloud_list, "
        "memory.manifest_update, memory.manifest_search"
    )
