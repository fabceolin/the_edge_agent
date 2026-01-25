"""
Orphan Cleanup for Hierarchical LTM Backend (TEA-LTM-015).

This module provides utilities to clean up orphaned blobs that don't
have corresponding catalog entries.

Orphans occur when:
- Blob write succeeds but catalog update fails
- Process crashes between blob write and catalog update

Example:
    >>> from the_edge_agent.memory.orphan_cleanup import cleanup_orphans
    >>>
    >>> result = cleanup_orphans(
    ...     catalog_url="postgresql://...",
    ...     storage_uri="gs://bucket/ltm/",
    ...     max_age_seconds=3600,
    ...     dry_run=True,
    ... )
    >>> print(result["orphan_count"])
"""

import logging
import time
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Set

logger = logging.getLogger(__name__)

# Check for required dependencies
SQLALCHEMY_AVAILABLE = False
try:
    from sqlalchemy import create_engine, text
    from sqlalchemy.orm import sessionmaker

    SQLALCHEMY_AVAILABLE = True
except ImportError:
    pass

FSSPEC_AVAILABLE = False
try:
    import fsspec

    FSSPEC_AVAILABLE = True
except ImportError:
    pass


def cleanup_orphans(
    catalog_url: str,
    storage_uri: str,
    fs=None,
    max_age_seconds: int = 3600,
    dry_run: bool = False,
    batch_size: int = 1000,
) -> Dict[str, Any]:
    """
    Remove orphaned blobs that don't have catalog entries (AC-18).

    Orphans occur when:
    - Blob write succeeds but catalog update fails
    - Process crashes between blob write and catalog update

    Flow:
    1. List all blobs in storage
    2. For each blob, check if catalog entry exists
    3. If no entry and blob age > max_age_seconds, delete
    4. Log all deletions for audit

    Args:
        catalog_url: SQLAlchemy connection URL for catalog
        storage_uri: Blob storage URI
        fs: Optional fsspec filesystem (created from URI if not provided)
        max_age_seconds: Only delete blobs older than this (default: 1 hour)
        dry_run: If True, only report without deleting
        batch_size: Number of blobs to process per batch

    Returns:
        {
            "success": True,
            "scanned_count": 10000,
            "orphan_count": 5,
            "deleted_count": 5,  # 0 if dry_run
            "deleted_paths": [...]
        }
    """
    if not SQLALCHEMY_AVAILABLE:
        return {
            "success": False,
            "error": "SQLAlchemy not available",
            "error_type": "dependency_missing",
        }

    if not FSSPEC_AVAILABLE:
        return {
            "success": False,
            "error": "fsspec not available",
            "error_type": "dependency_missing",
        }

    # Initialize filesystem if not provided
    if fs is None:
        protocol = _get_protocol(storage_uri)
        fs = fsspec.filesystem(protocol)

    # Initialize database connection
    engine = create_engine(catalog_url)
    Session = sessionmaker(bind=engine)
    session = Session()

    try:
        # Get all catalog blob paths
        catalog_paths = _get_catalog_blob_paths(session)

        # List all blob files in storage
        blob_files = _list_blob_files(fs, storage_uri)

        # Find orphans
        now = time.time()
        scanned_count = 0
        orphan_count = 0
        deleted_count = 0
        deleted_paths = []

        for blob_path in blob_files:
            scanned_count += 1

            # Skip if in catalog
            if blob_path in catalog_paths:
                continue

            # Skip index and metadata files
            filename = blob_path.split("/")[-1]
            if filename.startswith("_index") or filename.startswith("_meta"):
                continue

            # Check age
            try:
                info = fs.info(blob_path)
                mtime = info.get("mtime") or info.get("created")

                if mtime is None:
                    # If no mtime, use a default age check
                    age_seconds = max_age_seconds + 1
                elif isinstance(mtime, datetime):
                    age_seconds = now - mtime.timestamp()
                else:
                    age_seconds = now - mtime

                if age_seconds < max_age_seconds:
                    continue
            except Exception:
                # If we can't get file info, skip it
                continue

            orphan_count += 1

            if not dry_run:
                try:
                    fs.rm(blob_path)
                    deleted_count += 1
                    deleted_paths.append(blob_path)
                    logger.info("Deleted orphaned blob: %s", blob_path)
                except Exception as e:
                    logger.warning("Failed to delete orphan %s: %s", blob_path, e)
            else:
                deleted_paths.append(blob_path)
                logger.info("Would delete orphaned blob: %s", blob_path)

        return {
            "success": True,
            "scanned_count": scanned_count,
            "orphan_count": orphan_count,
            "deleted_count": deleted_count if not dry_run else 0,
            "deleted_paths": deleted_paths,
            "dry_run": dry_run,
        }

    except Exception as e:
        return {
            "success": False,
            "error": f"Cleanup failed: {e}",
            "error_type": "query_error",
        }
    finally:
        session.close()
        engine.dispose()


def _get_protocol(uri: str) -> str:
    """Extract protocol from URI."""
    if "://" in uri:
        return uri.split("://")[0]
    return "file"


def _get_catalog_blob_paths(session) -> Set[str]:
    """Get all blob paths from the catalog."""
    paths = set()

    try:
        result = session.execute(
            text(
                "SELECT blob_path FROM ltm_catalog_entries WHERE blob_path IS NOT NULL"
            )
        )
        for row in result:
            if row[0]:
                paths.add(row[0])
    except Exception as e:
        logger.warning("Failed to query catalog paths: %s", e)

    return paths


def _list_blob_files(fs, storage_uri: str) -> List[str]:
    """List all JSON blob files in storage."""
    files = []

    try:
        # Find all JSON files (entry blobs)
        pattern = f"{storage_uri.rstrip('/')}/**/*.json"
        files = fs.glob(pattern)
    except Exception as e:
        logger.warning("Failed to list blob files: %s", e)

    return files


def schedule_cleanup(
    catalog_url: str,
    storage_uri: str,
    interval_seconds: int = 3600,
    max_age_seconds: int = 3600,
) -> None:
    """
    Schedule periodic cleanup job.

    This is a simple synchronous scheduler for testing.
    In production, use a proper scheduler like APScheduler or Celery.

    Args:
        catalog_url: SQLAlchemy connection URL
        storage_uri: Blob storage URI
        interval_seconds: Run cleanup every N seconds
        max_age_seconds: Only delete blobs older than this
    """
    import threading

    def run_cleanup():
        while True:
            try:
                result = cleanup_orphans(
                    catalog_url=catalog_url,
                    storage_uri=storage_uri,
                    max_age_seconds=max_age_seconds,
                    dry_run=False,
                )
                logger.info(
                    "Orphan cleanup completed: scanned=%d, deleted=%d",
                    result.get("scanned_count", 0),
                    result.get("deleted_count", 0),
                )
            except Exception as e:
                logger.error("Orphan cleanup failed: %s", e)

            time.sleep(interval_seconds)

    thread = threading.Thread(target=run_cleanup, daemon=True)
    thread.start()


__all__ = [
    "cleanup_orphans",
    "schedule_cleanup",
]
