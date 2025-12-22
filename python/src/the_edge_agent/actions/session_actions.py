"""
Session Lifecycle Actions for TEA YAMLEngine.

Migrated from: firebase/functions-agents/actions/session.py
Story: TEA-BUILTIN-006 (Firebase Agent Memory Layer)

Provides custom actions for session memory lifecycle management with
archive-based expiration (instead of deletion).

Actions:
- session.create: Create a new session with expiration
- session.end: End session and archive its memory
- session.archive: Archive session with custom reason
- session.restore: Restore archived session
- session.get: Get session metadata
- session.list: List sessions with filtering

Key Design:
- Archive model: Sessions are archived, not deleted
- File movement: Files moved from sessions/{id}/* to archived-sessions/{id}/*
- Context exclusion: Archived sessions excluded from context.assemble
- Analytics access: Archived sessions still queryable via memory.sql_query

Provider Abstraction:
    This module uses MetadataStore and BlobStorage ABCs for provider-agnostic
    operations. Backend implementations (Firestore, GCS, etc.) are injected
    via state or kwargs.
"""

import os
import logging
from datetime import datetime, timezone, timedelta
from typing import Dict, Any, Callable, Optional, List
import uuid

from ..memory.metadata import MetadataStore, MetadataQuery
from ..memory.blob import BlobStorage

# Configure logging
logger = logging.getLogger(__name__)


# =============================================================================
# CONSTANTS
# =============================================================================

# Collection names
SESSIONS_COLLECTION = "sessions"
AGENT_MEMORY_COLLECTION = "agent_memory"

# Default session TTL (hours)
DEFAULT_TTL_HOURS = 24


# =============================================================================
# DEPENDENCY INJECTION HELPERS
# =============================================================================

def _get_metadata_store(state: Dict[str, Any], kwargs: Dict[str, Any]) -> MetadataStore:
    """
    Get MetadataStore from state or kwargs.

    Resolution order:
    1. kwargs['metadata_store'] (explicit parameter)
    2. state['_metadata_store'] (private convention)
    3. state['metadata_store'] (public state)

    Args:
        state: Current agent state
        kwargs: Action keyword arguments

    Returns:
        MetadataStore instance

    Raises:
        ValueError: If no MetadataStore is available
    """
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
        "No MetadataStore available. Provide via kwargs['metadata_store'], "
        "state['_metadata_store'], or state['metadata_store']. "
        "Create with: create_metadata_store('firestore')"
    )


def _get_blob_storage(state: Dict[str, Any], kwargs: Dict[str, Any]) -> BlobStorage:
    """
    Get BlobStorage from state or kwargs.

    Resolution order:
    1. kwargs['blob_storage'] (explicit parameter)
    2. state['_blob_storage'] (private convention)
    3. state['blob_storage'] (public state)

    Args:
        state: Current agent state
        kwargs: Action keyword arguments

    Returns:
        BlobStorage instance

    Raises:
        ValueError: If no BlobStorage is available
    """
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
        "No BlobStorage available. Provide via kwargs['blob_storage'], "
        "state['_blob_storage'], or state['blob_storage']. "
        "Create with: create_blob_storage('gcs')"
    )


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def _get_default_ttl() -> int:
    """
    Get default TTL from environment or config.

    Returns:
        TTL in hours (default 24)
    """
    try:
        return int(os.environ.get("SESSION_DEFAULT_TTL_HOURS", DEFAULT_TTL_HOURS))
    except (ValueError, TypeError):
        return DEFAULT_TTL_HOURS


def _generate_session_id() -> str:
    """
    Generate a unique session ID.

    Returns:
        Session ID in format "sess_{12-char hex}"
    """
    return f"sess_{uuid.uuid4().hex[:12]}"


def _move_session_files(
    metadata_store: MetadataStore,
    blob_storage: BlobStorage,
    session_id: str,
    from_prefix: str,
    to_prefix: str,
    restore: bool = False
) -> Dict[str, Any]:
    """
    Move session files between prefixes in storage.

    Also updates agent_memory docs with new paths.

    Implements AC2, AC3, AC7 file movement with atomic copy-then-delete.

    Args:
        metadata_store: MetadataStore instance
        blob_storage: BlobStorage instance
        session_id: Session ID
        from_prefix: Source path prefix (e.g., "sessions/xyz")
        to_prefix: Destination path prefix (e.g., "archived-sessions/xyz")
        restore: If True, updating status to active; else to archived

    Returns:
        Dict with success, moved_count, errors
    """
    moved_count = 0
    error_count = 0
    errors = []

    try:
        # Query agent_memory for session files by file_path prefix
        query = MetadataQuery(
            collection=AGENT_MEMORY_COLLECTION,
            filters=[
                {"field": "file_path", "op": ">=", "value": from_prefix},
                {"field": "file_path", "op": "<", "value": from_prefix + "\uffff"}
            ]
        )

        query_result = metadata_store.query(query)

        if not query_result.get("success"):
            logger.error(f"Failed to query files for session {session_id}: {query_result.get('error')}")
            return {
                "success": False,
                "moved_count": 0,
                "error_count": 1,
                "errors": [query_result.get("error", "Query failed")]
            }

        docs = query_result.get("results", [])

        if not docs:
            logger.info(f"No files found for session {session_id} at prefix {from_prefix}")
            return {"success": True, "moved_count": 0, "errors": []}

        logger.info(f"Found {len(docs)} files to move for session {session_id}")

        for doc in docs:
            doc_id = doc.get("_id") or doc.get("id") or doc.get("doc_id")
            old_path = doc.get("file_path", "")
            old_uri = doc.get("storage_uri", "")

            if not old_path:
                continue

            # Calculate new path by replacing prefix
            new_path = old_path.replace(from_prefix, to_prefix, 1)
            new_uri = old_uri.replace(from_prefix, to_prefix, 1) if old_uri else ""

            # Step 1: Copy file in blob storage
            if old_uri:
                try:
                    copy_result = blob_storage.copy(old_uri, new_uri)

                    if not copy_result.get("success"):
                        logger.warning(f"Failed to copy blob {old_uri}: {copy_result.get('error')}")
                        errors.append(f"Copy failed: {old_path} - {copy_result.get('error')}")
                        error_count += 1
                        continue

                    # Delete original after successful copy
                    delete_result = blob_storage.delete(old_uri)
                    if not delete_result.get("success"):
                        logger.warning(f"Failed to delete original blob {old_uri}: {delete_result.get('error')}")
                        # Not a fatal error - file was copied successfully

                    logger.debug(f"Moved blob: {old_uri} -> {new_uri}")

                except Exception as e:
                    logger.error(f"Failed to move blob {old_uri}: {e}")
                    errors.append(f"Blob move failed: {old_path} - {str(e)}")
                    error_count += 1
                    continue

            # Step 2: Update agent_memory doc
            try:
                update_data = {
                    "file_path": new_path,
                    "storage_uri": new_uri,
                    "updated_at": datetime.now(timezone.utc).isoformat(),
                }

                if restore:
                    update_data["status"] = "active"
                    update_data["archived_at"] = None
                    update_data["archive_reason"] = None
                else:
                    update_data["status"] = "archived"
                    update_data["archived_at"] = datetime.now(timezone.utc).isoformat()
                    # archive_reason is set by the calling function

                update_result = metadata_store.update(
                    collection=AGENT_MEMORY_COLLECTION,
                    doc_id=doc_id,
                    data=update_data
                )

                if update_result.get("success"):
                    moved_count += 1
                else:
                    logger.error(f"Failed to update agent_memory doc {doc_id}: {update_result.get('error')}")
                    errors.append(f"Metadata update failed: {old_path} - {update_result.get('error')}")
                    error_count += 1

            except Exception as e:
                logger.error(f"Failed to update agent_memory doc {doc_id}: {e}")
                errors.append(f"Metadata update failed: {old_path} - {str(e)}")
                error_count += 1

        return {
            "success": error_count == 0,
            "moved_count": moved_count,
            "error_count": error_count,
            "errors": errors
        }

    except Exception as e:
        logger.error(f"File movement failed for session {session_id}: {e}")
        return {
            "success": False,
            "moved_count": moved_count,
            "error_count": error_count + 1,
            "errors": errors + [str(e)]
        }


def _archive_session_internal(
    metadata_store: MetadataStore,
    blob_storage: BlobStorage,
    session_id: str,
    reason: str
) -> Dict[str, Any]:
    """
    Archive a session (internal helper).

    Moves files from sessions/{id}/* to archived-sessions/{id}/*
    and updates session metadata.

    Args:
        metadata_store: MetadataStore instance
        blob_storage: BlobStorage instance
        session_id: Session ID to archive
        reason: Archive reason ("session_end", "expired", "manual", or custom)

    Returns:
        Dict with success, archived_files, or error
    """
    # Get session doc
    get_result = metadata_store.get(
        collection=SESSIONS_COLLECTION,
        doc_id=session_id
    )

    if not get_result.get("success") or not get_result.get("exists"):
        logger.warning(f"Session {session_id} not found for archival")
        return {"success": False, "error": f"Session {session_id} not found"}

    session_data = get_result.get("data", {})

    if session_data.get("status") == "archived":
        logger.info(f"Session {session_id} already archived")
        return {"success": False, "error": f"Session {session_id} already archived"}

    # Move files from sessions/{id}/* to archived-sessions/{id}/*
    from_prefix = f"sessions/{session_id}"
    to_prefix = f"archived-sessions/{session_id}"

    move_result = _move_session_files(
        metadata_store=metadata_store,
        blob_storage=blob_storage,
        session_id=session_id,
        from_prefix=from_prefix,
        to_prefix=to_prefix,
        restore=False
    )

    # Update agent_memory docs with archive_reason
    if move_result["moved_count"] > 0:
        try:
            # Query and update archive_reason for all moved docs
            query = MetadataQuery(
                collection=AGENT_MEMORY_COLLECTION,
                filters=[
                    {"field": "file_path", "op": ">=", "value": to_prefix},
                    {"field": "file_path", "op": "<", "value": to_prefix + "\uffff"}
                ]
            )

            query_result = metadata_store.query(query)
            for doc in query_result.get("results", []):
                doc_id = doc.get("_id") or doc.get("id") or doc.get("doc_id")
                if doc_id:
                    metadata_store.update(
                        collection=AGENT_MEMORY_COLLECTION,
                        doc_id=doc_id,
                        data={"archive_reason": reason}
                    )
        except Exception as e:
            logger.warning(f"Failed to set archive_reason on docs: {e}")

    # Update session doc
    try:
        update_result = metadata_store.update(
            collection=SESSIONS_COLLECTION,
            doc_id=session_id,
            data={
                "status": "archived",
                "archived_at": datetime.now(timezone.utc).isoformat(),
                "archive_reason": reason,
            }
        )

        if not update_result.get("success"):
            logger.error(f"Failed to update session doc {session_id}: {update_result.get('error')}")
            return {
                "success": False,
                "error": f"Session metadata update failed: {update_result.get('error')}",
                "archived_files": move_result["moved_count"]
            }

        logger.info(f"Archived session {session_id}, reason: {reason}, "
                    f"{move_result['moved_count']} files moved")

    except Exception as e:
        logger.error(f"Failed to update session doc {session_id}: {e}")
        return {
            "success": False,
            "error": f"Session metadata update failed: {str(e)}",
            "archived_files": move_result["moved_count"]
        }

    return {
        "success": True,
        "archived_files": move_result["moved_count"],
        "errors": move_result.get("errors", [])
    }


# =============================================================================
# TEA CUSTOM ACTIONS
# =============================================================================

def session_create(
    state: Dict[str, Any],
    user_id: str,
    ttl_hours: int = None,
    **kwargs
) -> Dict[str, Any]:
    """
    Create a new session with expiration.

    TEA Custom Action: session.create

    AC1: Create new session with expiration:
    - Accepts user_id, ttl_hours (default from config, typically 24)
    - Generates unique session_id
    - Sets expires_at = now() + ttl_hours
    - Creates session metadata doc
    - Returns {success: bool, session_id: str, expires_at: timestamp}

    Args:
        state: Current agent state
        user_id: User ID owning this session
        ttl_hours: Hours until expiration (default from config)
        **kwargs: Additional arguments
            metadata_store: MetadataStore instance (optional)

    Returns:
        Dict with success, session_id, expires_at, or error
    """
    if not user_id:
        return {"success": False, "error": "user_id is required"}

    try:
        metadata_store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {"success": False, "error": str(e), "error_type": "configuration_error"}

    # Get default TTL from config if not provided
    if ttl_hours is None:
        ttl_hours = _get_default_ttl()

    # Validate TTL
    if ttl_hours <= 0:
        return {"success": False, "error": "ttl_hours must be positive"}

    session_id = _generate_session_id()
    now = datetime.now(timezone.utc)
    expires_at = now + timedelta(hours=ttl_hours)

    try:
        # Create session metadata doc
        create_result = metadata_store.set(
            collection=SESSIONS_COLLECTION,
            doc_id=session_id,
            data={
                "session_id": session_id,
                "user_id": user_id,
                "status": "active",
                "created_at": now.isoformat(),
                "expires_at": expires_at.isoformat(),
                "archived_at": None,
                "archive_reason": None,
                "restored_at": None,
            }
        )

        if not create_result.get("success"):
            return {
                "success": False,
                "error": f"Session creation failed: {create_result.get('error')}"
            }

        logger.info(f"Created session {session_id} for user {user_id}, "
                    f"expires in {ttl_hours}h at {expires_at.isoformat()}")

        return {
            "success": True,
            "session_id": session_id,
            "expires_at": expires_at.isoformat(),
            "ttl_hours": ttl_hours,
        }

    except Exception as e:
        logger.error(f"Failed to create session for user {user_id}: {e}")
        return {"success": False, "error": f"Session creation failed: {str(e)}"}


def session_end(
    state: Dict[str, Any],
    session_id: str,
    **kwargs
) -> Dict[str, Any]:
    """
    End session and archive its memory.

    TEA Custom Action: session.end

    AC2: Manually end and archive session:
    - Accepts session_id
    - Moves files from sessions/{id}/* to archived-sessions/{id}/*
    - Updates all agent_memory docs for session with archived status
    - Returns {success: bool, archived_files: int}

    Args:
        state: Current agent state
        session_id: Session to end
        **kwargs: Additional arguments
            metadata_store: MetadataStore instance (optional)
            blob_storage: BlobStorage instance (optional)

    Returns:
        Dict with success, archived_files, or error
    """
    if not session_id:
        return {"success": False, "error": "session_id is required"}

    try:
        metadata_store = _get_metadata_store(state, kwargs)
        blob_storage = _get_blob_storage(state, kwargs)
    except ValueError as e:
        return {"success": False, "error": str(e), "error_type": "configuration_error"}

    return _archive_session_internal(
        metadata_store=metadata_store,
        blob_storage=blob_storage,
        session_id=session_id,
        reason="session_end"
    )


def session_archive(
    state: Dict[str, Any],
    session_id: str,
    reason: str = "manual",
    **kwargs
) -> Dict[str, Any]:
    """
    Archive session with custom reason.

    TEA Custom Action: session.archive

    AC3: Manually archive with custom reason:
    - Accepts session_id, reason (string)
    - Same behavior as session.end but with custom archive_reason
    - Used for admin operations or manual cleanup

    Args:
        state: Current agent state
        session_id: Session to archive
        reason: Archive reason (default "manual")
        **kwargs: Additional arguments
            metadata_store: MetadataStore instance (optional)
            blob_storage: BlobStorage instance (optional)

    Returns:
        Dict with success, archived_files, or error
    """
    if not session_id:
        return {"success": False, "error": "session_id is required"}

    if not reason:
        reason = "manual"

    try:
        metadata_store = _get_metadata_store(state, kwargs)
        blob_storage = _get_blob_storage(state, kwargs)
    except ValueError as e:
        return {"success": False, "error": str(e), "error_type": "configuration_error"}

    return _archive_session_internal(
        metadata_store=metadata_store,
        blob_storage=blob_storage,
        session_id=session_id,
        reason=reason
    )


def session_restore(
    state: Dict[str, Any],
    session_id: str,
    new_ttl_hours: int = None,
    **kwargs
) -> Dict[str, Any]:
    """
    Restore archived session.

    TEA Custom Action: session.restore

    AC7: Restore archived session:
    - Accepts session_id, new_ttl_hours
    - Moves files back from archived-sessions/{id}/* to sessions/{id}/*
    - Sets new expires_at
    - Updates status: "active", clears archive fields
    - Returns {success: bool, restored_files: int}

    Args:
        state: Current agent state
        session_id: Session to restore
        new_ttl_hours: New TTL (default from config)
        **kwargs: Additional arguments
            metadata_store: MetadataStore instance (optional)
            blob_storage: BlobStorage instance (optional)

    Returns:
        Dict with success, restored_files, expires_at, or error
    """
    if not session_id:
        return {"success": False, "error": "session_id is required"}

    try:
        metadata_store = _get_metadata_store(state, kwargs)
        blob_storage = _get_blob_storage(state, kwargs)
    except ValueError as e:
        return {"success": False, "error": str(e), "error_type": "configuration_error"}

    # Get session doc
    get_result = metadata_store.get(
        collection=SESSIONS_COLLECTION,
        doc_id=session_id
    )

    if not get_result.get("success") or not get_result.get("exists"):
        logger.warning(f"Session {session_id} not found for restoration")
        return {"success": False, "error": f"Session {session_id} not found"}

    session_data = get_result.get("data", {})

    if session_data.get("status") != "archived":
        logger.info(f"Session {session_id} is not archived (status: {session_data.get('status')})")
        return {"success": False, "error": f"Session {session_id} is not archived"}

    # Get TTL
    if new_ttl_hours is None:
        new_ttl_hours = _get_default_ttl()

    if new_ttl_hours <= 0:
        return {"success": False, "error": "new_ttl_hours must be positive"}

    now = datetime.now(timezone.utc)
    expires_at = now + timedelta(hours=new_ttl_hours)

    # Move files back from archived-sessions/{id}/* to sessions/{id}/*
    from_prefix = f"archived-sessions/{session_id}"
    to_prefix = f"sessions/{session_id}"

    move_result = _move_session_files(
        metadata_store=metadata_store,
        blob_storage=blob_storage,
        session_id=session_id,
        from_prefix=from_prefix,
        to_prefix=to_prefix,
        restore=True
    )

    # Update session doc
    try:
        update_result = metadata_store.update(
            collection=SESSIONS_COLLECTION,
            doc_id=session_id,
            data={
                "status": "active",
                "expires_at": expires_at.isoformat(),
                "archived_at": None,
                "archive_reason": None,
                "restored_at": now.isoformat(),
            }
        )

        if not update_result.get("success"):
            logger.error(f"Failed to update session doc {session_id}: {update_result.get('error')}")
            return {
                "success": False,
                "error": f"Session metadata update failed: {update_result.get('error')}",
                "restored_files": move_result["moved_count"]
            }

        logger.info(f"Restored session {session_id}, {move_result['moved_count']} files, "
                    f"new expiry in {new_ttl_hours}h")

    except Exception as e:
        logger.error(f"Failed to update session doc {session_id}: {e}")
        return {
            "success": False,
            "error": f"Session metadata update failed: {str(e)}",
            "restored_files": move_result["moved_count"]
        }

    return {
        "success": True,
        "restored_files": move_result["moved_count"],
        "expires_at": expires_at.isoformat(),
        "errors": move_result.get("errors", [])
    }


def session_get(
    state: Dict[str, Any],
    session_id: str,
    **kwargs
) -> Dict[str, Any]:
    """
    Get session metadata.

    TEA Custom Action: session.get

    Helper action to retrieve session state.

    Args:
        state: Current agent state
        session_id: Session ID to retrieve
        **kwargs: Additional arguments
            metadata_store: MetadataStore instance (optional)

    Returns:
        Dict with success, session data, or error
    """
    if not session_id:
        return {"success": False, "error": "session_id is required"}

    try:
        metadata_store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {"success": False, "error": str(e), "error_type": "configuration_error"}

    try:
        get_result = metadata_store.get(
            collection=SESSIONS_COLLECTION,
            doc_id=session_id
        )

        if not get_result.get("success"):
            return {"success": False, "error": get_result.get("error", "Get failed")}

        if not get_result.get("exists"):
            return {"success": False, "error": f"Session {session_id} not found"}

        session_data = get_result.get("data", {})

        return {
            "success": True,
            "session": session_data
        }

    except Exception as e:
        logger.error(f"Failed to get session {session_id}: {e}")
        return {"success": False, "error": f"Session retrieval failed: {str(e)}"}


def session_list(
    state: Dict[str, Any],
    user_id: str = None,
    status: str = None,
    limit: int = 100,
    **kwargs
) -> Dict[str, Any]:
    """
    List sessions with optional filtering.

    TEA Custom Action: session.list

    Helper action to list sessions for a user or all sessions.

    Args:
        state: Current agent state
        user_id: Filter by user ID (optional)
        status: Filter by status ("active" or "archived") (optional)
        limit: Maximum results (default 100)
        **kwargs: Additional arguments
            metadata_store: MetadataStore instance (optional)

    Returns:
        Dict with success, sessions list, or error
    """
    try:
        metadata_store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {"success": False, "error": str(e), "error_type": "configuration_error"}

    try:
        # Build filters
        filters = []
        if user_id:
            filters.append({"field": "user_id", "op": "==", "value": user_id})
        if status:
            filters.append({"field": "status", "op": "==", "value": status})

        query = MetadataQuery(
            collection=SESSIONS_COLLECTION,
            filters=filters if filters else None,
            limit=limit
        )

        query_result = metadata_store.query(query)

        if not query_result.get("success"):
            return {"success": False, "error": query_result.get("error", "Query failed")}

        sessions = query_result.get("results", [])

        return {
            "success": True,
            "sessions": sessions,
            "count": len(sessions)
        }

    except Exception as e:
        logger.error(f"Failed to list sessions: {e}")
        return {"success": False, "error": f"Session list failed: {str(e)}"}


def session_archive_expired(
    state: Dict[str, Any],
    **kwargs
) -> Dict[str, Any]:
    """
    Archive sessions that have exceeded their TTL.

    TEA Custom Action: session.archive_expired

    AC4: Archive expired sessions:
    - Queries sessions where expires_at < now() and status = "active"
    - Archives each with archive_reason: "expired"
    - Returns count of archived sessions

    Args:
        state: Current agent state
        **kwargs: Additional arguments
            metadata_store: MetadataStore instance (optional)
            blob_storage: BlobStorage instance (optional)

    Returns:
        Dict with archived count and error count
    """
    try:
        metadata_store = _get_metadata_store(state, kwargs)
        blob_storage = _get_blob_storage(state, kwargs)
    except ValueError as e:
        return {"success": False, "error": str(e), "error_type": "configuration_error"}

    now = datetime.now(timezone.utc)

    try:
        # Query expired active sessions
        query = MetadataQuery(
            collection=SESSIONS_COLLECTION,
            filters=[
                {"field": "status", "op": "==", "value": "active"},
                {"field": "expires_at", "op": "<", "value": now.isoformat()}
            ]
        )

        query_result = metadata_store.query(query)

        if not query_result.get("success"):
            return {"success": False, "error": query_result.get("error"), "archived": 0, "errors": 1}

        expired_sessions = query_result.get("results", [])

        if not expired_sessions:
            logger.info("No expired sessions to archive")
            return {"success": True, "archived": 0, "errors": 0}

        logger.info(f"Found {len(expired_sessions)} expired sessions to archive")

        archived_count = 0
        error_count = 0

        for session in expired_sessions:
            session_id = session.get("session_id") or session.get("_id") or session.get("id")
            if not session_id:
                continue

            try:
                result = _archive_session_internal(
                    metadata_store=metadata_store,
                    blob_storage=blob_storage,
                    session_id=session_id,
                    reason="expired"
                )
                if result.get("success"):
                    archived_count += 1
                else:
                    error_count += 1
                    logger.error(f"Failed to archive {session_id}: {result.get('error')}")
            except Exception as e:
                error_count += 1
                logger.error(f"Error archiving {session_id}: {e}")

        logger.info(f"Archived {archived_count} sessions, {error_count} errors")

        return {
            "success": True,
            "archived": archived_count,
            "errors": error_count,
        }

    except Exception as e:
        logger.error(f"Scheduled archival failed: {e}")
        return {"success": False, "archived": 0, "errors": 1, "error": str(e)}


# =============================================================================
# ACTION REGISTRATION
# =============================================================================

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register session actions with the TEA YAMLEngine.

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

    registry["session.create"] = wrap_with_backends(session_create)
    registry["session.end"] = wrap_with_backends(session_end)
    registry["session.archive"] = wrap_with_backends(session_archive)
    registry["session.restore"] = wrap_with_backends(session_restore)
    registry["session.get"] = wrap_with_backends(session_get)
    registry["session.list"] = wrap_with_backends(session_list)
    registry["session.archive_expired"] = wrap_with_backends(session_archive_expired)

    logger.info(
        "Session actions registered: session.create, session.end, "
        "session.archive, session.restore, session.get, session.list, "
        "session.archive_expired"
    )


# Module metadata for discovery
__tea_actions__ = {
    "version": "1.0.0",
    "description": "Session lifecycle actions using MetadataStore and BlobStorage abstraction",
    "actions": [
        "session.create",
        "session.end",
        "session.archive",
        "session.restore",
        "session.get",
        "session.list",
        "session.archive_expired"
    ],
    "story": "TEA-BUILTIN-006"
}
