"""
Vector Similarity Search Actions for TEA YAMLEngine.

Migrated from: firebase/functions-agents/actions/vector_search.py
Story: TEA-BUILTIN-006 (Firebase Agent Memory Layer)

This module provides semantic search over agent memory using the pluggable
VectorIndex interface for fast vector similarity queries.

Architecture:
    ┌─────────────────────────────────────────────────────────────┐
    │                     VECTOR SEARCH                            │
    ├─────────────────────────────────────────────────────────────┤
    │                                                              │
    │  memory.vector_search(query, top_k, threshold)              │
    │         │                                                    │
    │         ▼                                                    │
    │  ┌─────────────────┐     ┌─────────────────────┐            │
    │  │ Generate query  │     │ VectorIndex         │            │
    │  │ embedding       │     │ (pluggable backend) │            │
    │  └────────┬────────┘     └─────────┬───────────┘            │
    │           │                        │                         │
    │           └────────┬───────────────┘                         │
    │                    ▼                                         │
    │          ┌─────────────────────────┐                        │
    │          │ search(embedding,       │                        │
    │          │        top_k, threshold)│                        │
    │          └─────────┬───────────────┘                        │
    │                    │                                         │
    │                    ▼                                         │
    │          ┌─────────────────────────┐                        │
    │          │ [{file_path, content,   │                        │
    │          │   similarity_score,     │                        │
    │          │   anchors, ...}, ...]   │                        │
    │          └─────────────────────────┘                        │
    │                                                              │
    └─────────────────────────────────────────────────────────────┘

Provider Abstraction:
    This module uses the VectorIndex ABC for provider-agnostic operations.
    Backend implementations (DuckDB VSS, etc.) are injected via state or kwargs.

Reference: docs/stories/TEA-BUILTIN-006.md
"""

import logging
from typing import Any, Callable, Dict, List, Optional

from ..memory.vector import VectorIndex, VectorSearchConfig

# Configure logging
logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION
# =============================================================================

# Default search parameters
DEFAULT_TOP_K = 10
DEFAULT_THRESHOLD = 0.0  # Minimum similarity score (0.0 = all, 1.0 = exact match)
MAX_TOP_K = 100  # Maximum results to return


# =============================================================================
# DEPENDENCY INJECTION HELPERS
# =============================================================================

def _get_vector_index(state: Dict[str, Any], kwargs: Dict[str, Any]) -> VectorIndex:
    """
    Get VectorIndex from state or kwargs.

    Resolution order:
    1. kwargs['vector_index'] (explicit parameter)
    2. state['_vector_index'] (private convention)
    3. state['vector_index'] (public state)

    Args:
        state: Current agent state
        kwargs: Action keyword arguments

    Returns:
        VectorIndex instance

    Raises:
        ValueError: If no VectorIndex is available
    """
    # Check kwargs first (explicit parameter)
    index = kwargs.get('vector_index')
    if index is not None:
        return index

    # Check private state convention
    index = state.get('_vector_index')
    if index is not None:
        return index

    # Check public state
    index = state.get('vector_index')
    if index is not None:
        return index

    raise ValueError(
        "No VectorIndex available. Provide via kwargs['vector_index'], "
        "state['_vector_index'], or state['vector_index']. "
        "Create with: create_vector_index('duckdb')"
    )


def _get_embedding_function(
    state: Dict[str, Any],
    kwargs: Dict[str, Any]
) -> Optional[Callable[[str], List[float]]]:
    """
    Get embedding function from state or kwargs.

    The embedding function should accept a text string and return
    a list of floats (the embedding vector).

    Resolution order:
    1. kwargs['embedding_fn'] (explicit parameter)
    2. state['_embedding_fn'] (private convention)
    3. state['embedding_fn'] (public state)
    4. Fallback to memory.embed action if available in state

    Args:
        state: Current agent state
        kwargs: Action keyword arguments

    Returns:
        Embedding function or None if not available
    """
    # Check kwargs first
    fn = kwargs.get('embedding_fn')
    if fn is not None:
        return fn

    # Check private state convention
    fn = state.get('_embedding_fn')
    if fn is not None:
        return fn

    # Check public state
    fn = state.get('embedding_fn')
    if fn is not None:
        return fn

    # Try to get from action registry
    action_registry = state.get('_action_registry') or state.get('action_registry')
    if action_registry:
        embed_action = action_registry.get('memory.embed')
        if embed_action:
            # Wrap action to match expected signature
            def embedding_wrapper(text: str) -> List[float]:
                result = embed_action(state, text)
                if result.get('success') and 'embedding' in result:
                    return result['embedding']
                raise ValueError(f"Embedding failed: {result.get('error', 'unknown')}")
            return embedding_wrapper

    return None


# =============================================================================
# TEA CUSTOM ACTIONS
# =============================================================================

def memory_vector_search(
    state: Dict[str, Any],
    query: str,
    top_k: int = DEFAULT_TOP_K,
    threshold: float = DEFAULT_THRESHOLD,
    content_type: Optional[str] = None,
    anchors: Optional[List[str]] = None,
    status: str = "active",
    **kwargs
) -> Dict[str, Any]:
    """
    Semantic search over agent memory using vector similarity.

    TEA Custom Action: memory.vector_search

    This action generates an embedding for the query text and performs
    vector similarity search against the configured VectorIndex.

    Args:
        state: Current agent state (must contain project_id)
        query: Natural language search query
        top_k: Maximum number of results (default 10, max 100)
        threshold: Minimum similarity score 0.0-1.0 (default 0.0)
        content_type: Filter by content type (yaml, json, md)
        anchors: Filter by anchors (any match)
        status: Filter by status (default "active")
        **kwargs: Additional arguments
            vector_index: VectorIndex instance (optional, uses state if not provided)
            embedding_fn: Function to generate embeddings (optional)

    Returns:
        On success:
        {
            "success": True,
            "results": [
                {
                    "file_path": str,
                    "content": str,
                    "similarity_score": float,
                    "anchors": list,
                    "summary": str,
                    ...
                },
                ...
            ],
            "count": int,
            "query_tokens": int
        }

        On error:
        {
            "success": False,
            "error": str,
            "error_type": str
        }
    """
    project_id = state.get("project_id", "rankellix")

    # Validate inputs
    if not query or not isinstance(query, str):
        return {
            "success": False,
            "error": "Query must be a non-empty string",
            "error_type": "invalid_input"
        }

    top_k = min(max(1, top_k), MAX_TOP_K)
    threshold = max(0.0, min(1.0, threshold))

    try:
        # Get the vector index
        vector_index = _get_vector_index(state, kwargs)

        # Get embedding function
        embedding_fn = _get_embedding_function(state, kwargs)
        if embedding_fn is None:
            return {
                "success": False,
                "error": "No embedding function available. Provide via kwargs['embedding_fn'] "
                        "or state['embedding_fn'], or register memory.embed action.",
                "error_type": "embedding_not_configured"
            }

        # Generate query embedding
        try:
            query_embedding = embedding_fn(query)
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to generate query embedding: {e}",
                "error_type": "embedding_failed"
            }

        # Validate embedding dimensions
        expected_dims = vector_index.get_dimensions()
        if len(query_embedding) != expected_dims:
            return {
                "success": False,
                "error": f"Embedding dimensions mismatch: got {len(query_embedding)}, "
                        f"expected {expected_dims}",
                "error_type": "dimension_mismatch"
            }

        # Build search config
        search_config = VectorSearchConfig(
            top_k=top_k,
            threshold=threshold,
            include_metadata=True,
            include_content=True
        )

        # Build metadata filters
        filters = {}
        if project_id:
            filters["project_id"] = project_id
        if status:
            filters["status"] = status
        if content_type:
            filters["content_type"] = content_type
        if anchors:
            filters["anchors"] = anchors

        # Execute search
        results = vector_index.search(
            embedding=query_embedding,
            config=search_config,
            metadata_filter=filters if filters else None
        )

        # Convert SearchResult objects to dicts
        result_dicts = []
        for result in results:
            result_dict = {
                "file_path": result.metadata.get("file_path", ""),
                "file_name": result.metadata.get("file_name", ""),
                "project_id": result.metadata.get("project_id", ""),
                "content": result.content or "",
                "content_type": result.metadata.get("content_type", ""),
                "anchors": result.metadata.get("anchors", []),
                "status": result.metadata.get("status", ""),
                "summary": result.metadata.get("summary", ""),
                "line_count": result.metadata.get("line_count", 0),
                "byte_size": result.metadata.get("byte_size", 0),
                "token_estimate": result.metadata.get("token_estimate", 0),
                "first_heading": result.metadata.get("first_heading", ""),
                "content_hash": result.metadata.get("content_hash", ""),
                "doc_id": result.id,
                "similarity_score": result.score
            }
            result_dicts.append(result_dict)

        # Estimate query tokens (rough approximation)
        query_tokens = len(query.split()) * 2

        return {
            "success": True,
            "results": result_dicts,
            "count": len(result_dicts),
            "query_tokens": query_tokens,
            "top_k": top_k,
            "threshold": threshold
        }

    except ValueError as e:
        # Configuration errors (no index, no embedding function)
        logger.warning(f"Vector search configuration error: {e}")
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    except Exception as e:
        logger.error(f"Vector search failed: {e}")
        return {
            "success": False,
            "error": str(e),
            "error_type": "search_failed"
        }


def memory_vector_search_by_embedding(
    state: Dict[str, Any],
    embedding: List[float],
    top_k: int = DEFAULT_TOP_K,
    threshold: float = DEFAULT_THRESHOLD,
    content_type: Optional[str] = None,
    status: str = "active",
    **kwargs
) -> Dict[str, Any]:
    """
    Search using a pre-computed embedding vector.

    TEA Custom Action: memory.vector_search_by_embedding

    Useful when you already have an embedding (e.g., from a document)
    and want to find similar documents.

    Args:
        state: Current agent state (must contain project_id)
        embedding: Pre-computed embedding vector
        top_k: Maximum number of results
        threshold: Minimum similarity score
        content_type: Filter by content type
        status: Filter by status
        **kwargs: Additional arguments
            vector_index: VectorIndex instance (optional)

    Returns:
        Same format as memory.vector_search
    """
    project_id = state.get("project_id", "rankellix")

    # Validate embedding
    if not embedding or not isinstance(embedding, list):
        return {
            "success": False,
            "error": "Embedding must be a non-empty list of floats",
            "error_type": "invalid_input"
        }

    top_k = min(max(1, top_k), MAX_TOP_K)
    threshold = max(0.0, min(1.0, threshold))

    try:
        # Get the vector index
        vector_index = _get_vector_index(state, kwargs)

        # Validate embedding dimensions
        expected_dims = vector_index.get_dimensions()
        if len(embedding) != expected_dims:
            return {
                "success": False,
                "error": f"Embedding must have {expected_dims} dimensions, "
                        f"got {len(embedding)}",
                "error_type": "invalid_dimensions"
            }

        # Build search config
        search_config = VectorSearchConfig(
            top_k=top_k,
            threshold=threshold,
            include_metadata=True,
            include_content=True
        )

        # Build metadata filters
        filters = {}
        if project_id:
            filters["project_id"] = project_id
        if status:
            filters["status"] = status
        if content_type:
            filters["content_type"] = content_type

        # Execute search
        results = vector_index.search(
            embedding=embedding,
            config=search_config,
            metadata_filter=filters if filters else None
        )

        # Convert SearchResult objects to dicts
        result_dicts = []
        for result in results:
            result_dict = {
                "file_path": result.metadata.get("file_path", ""),
                "file_name": result.metadata.get("file_name", ""),
                "project_id": result.metadata.get("project_id", ""),
                "content": result.content or "",
                "content_type": result.metadata.get("content_type", ""),
                "anchors": result.metadata.get("anchors", []),
                "status": result.metadata.get("status", ""),
                "summary": result.metadata.get("summary", ""),
                "line_count": result.metadata.get("line_count", 0),
                "byte_size": result.metadata.get("byte_size", 0),
                "token_estimate": result.metadata.get("token_estimate", 0),
                "first_heading": result.metadata.get("first_heading", ""),
                "content_hash": result.metadata.get("content_hash", ""),
                "doc_id": result.id,
                "similarity_score": result.score
            }
            result_dicts.append(result_dict)

        return {
            "success": True,
            "results": result_dicts,
            "count": len(result_dicts),
            "top_k": top_k,
            "threshold": threshold
        }

    except ValueError as e:
        # Configuration errors
        logger.warning(f"Vector search configuration error: {e}")
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    except Exception as e:
        logger.error(f"Vector search by embedding failed: {e}")
        return {
            "success": False,
            "error": str(e),
            "error_type": "search_failed"
        }


def memory_vector_load_data(
    state: Dict[str, Any],
    source: str,
    **kwargs
) -> Dict[str, Any]:
    """
    Load vector data from a Parquet file or URL.

    TEA Custom Action: memory.vector_load_data

    Args:
        state: Current agent state
        source: Path or URL to Parquet file
        **kwargs: Additional arguments
            vector_index: VectorIndex instance (optional)

    Returns:
        On success:
        {
            "success": True,
            "loaded": True,
            "source": str,
            "row_count": int
        }

        On error:
        {
            "success": False,
            "error": str,
            "error_type": str
        }
    """
    if not source:
        return {
            "success": False,
            "error": "Source path or URL is required",
            "error_type": "invalid_input"
        }

    try:
        vector_index = _get_vector_index(state, kwargs)

        result = vector_index.load_data(source)

        if result.get("success"):
            return {
                "success": True,
                "loaded": True,
                "source": source,
                "row_count": result.get("row_count", 0)
            }
        else:
            return {
                "success": False,
                "error": result.get("error", "Unknown error loading data"),
                "error_type": "load_failed"
            }

    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    except Exception as e:
        logger.error(f"Failed to load vector data: {e}")
        return {
            "success": False,
            "error": str(e),
            "error_type": "load_failed"
        }


def memory_vector_build_index(
    state: Dict[str, Any],
    **kwargs
) -> Dict[str, Any]:
    """
    Build or rebuild the vector search index.

    TEA Custom Action: memory.vector_build_index

    Args:
        state: Current agent state
        **kwargs: Additional arguments
            vector_index: VectorIndex instance (optional)

    Returns:
        On success:
        {
            "success": True,
            "built": True,
            "stats": dict
        }

        On error:
        {
            "success": False,
            "error": str,
            "error_type": str
        }
    """
    try:
        vector_index = _get_vector_index(state, kwargs)

        result = vector_index.build_index()

        if result.get("success"):
            return {
                "success": True,
                "built": True,
                "stats": vector_index.get_stats()
            }
        else:
            return {
                "success": False,
                "error": result.get("error", "Unknown error building index"),
                "error_type": "build_failed"
            }

    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    except Exception as e:
        logger.error(f"Failed to build vector index: {e}")
        return {
            "success": False,
            "error": str(e),
            "error_type": "build_failed"
        }


def memory_vector_stats(
    state: Dict[str, Any],
    **kwargs
) -> Dict[str, Any]:
    """
    Get statistics about the vector index.

    TEA Custom Action: memory.vector_stats

    Args:
        state: Current agent state
        **kwargs: Additional arguments
            vector_index: VectorIndex instance (optional)

    Returns:
        On success:
        {
            "success": True,
            "stats": {
                "dimensions": int,
                "row_count": int,
                "index_built": bool,
                ...
            }
        }

        On error:
        {
            "success": False,
            "error": str,
            "error_type": str
        }
    """
    try:
        vector_index = _get_vector_index(state, kwargs)

        stats = vector_index.get_stats()
        stats["dimensions"] = vector_index.get_dimensions()

        return {
            "success": True,
            "stats": stats
        }

    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    except Exception as e:
        logger.error(f"Failed to get vector stats: {e}")
        return {
            "success": False,
            "error": str(e),
            "error_type": "stats_failed"
        }


# =============================================================================
# EMBEDDING ACTIONS (migrated from embeddings.py)
# =============================================================================

# OpenAI embedding model configuration
EMBEDDING_MODEL = "text-embedding-3-small"
EMBEDDING_DIMENSIONS = 1536
MAX_INPUT_TOKENS = 8191

# Token counting - lazy import tiktoken
_tiktoken_encoding = None


def _get_tiktoken_encoding():
    """Get or create tiktoken encoding for token counting."""
    global _tiktoken_encoding
    if _tiktoken_encoding is None:
        try:
            import tiktoken
            _tiktoken_encoding = tiktoken.get_encoding("cl100k_base")
        except ImportError:
            logger.warning("tiktoken not available, token counting will use estimate")
            return None
    return _tiktoken_encoding


def count_tokens(text: str) -> int:
    """Count tokens in text using tiktoken or estimate."""
    encoding = _get_tiktoken_encoding()
    if encoding:
        return len(encoding.encode(text))
    else:
        return len(text) // 4  # ~4 chars per token estimate


def memory_embed(
    state: Dict[str, Any],
    content: str,
    model: str = EMBEDDING_MODEL,
    **kwargs
) -> Dict[str, Any]:
    """
    Generate embedding for content.

    TEA Custom Action: memory.embed

    Args:
        state: Current agent state
        content: Text to embed (max ~8000 tokens)
        model: OpenAI embedding model (default: text-embedding-3-small)
        **kwargs: Additional arguments
            embedding_fn: Custom embedding function (optional)

    Returns:
        On success:
        {
            "success": True,
            "embedding": [float, ...],
            "model": str,
            "tokens": int,
            "dimensions": int
        }

        On error:
        {
            "success": False,
            "error": str,
            "error_type": str
        }
    """
    import os

    if not content or not isinstance(content, str):
        return {
            "success": False,
            "error": "Content must be a non-empty string",
            "error_type": "invalid_input"
        }

    token_count = count_tokens(content)
    if token_count > MAX_INPUT_TOKENS:
        return {
            "success": False,
            "error": f"Content too long: {token_count} tokens (max {MAX_INPUT_TOKENS})",
            "error_type": "content_too_long",
            "tokens": token_count
        }

    try:
        # Check for custom embedding function
        embedding_fn = kwargs.get('embedding_fn')
        if embedding_fn:
            result = embedding_fn(content)
            if isinstance(result, list):
                return {
                    "success": True,
                    "embedding": result,
                    "model": model,
                    "tokens": token_count,
                    "dimensions": len(result)
                }
            elif isinstance(result, dict):
                return {
                    "success": result.get("success", True),
                    "embedding": result.get("embedding", []),
                    "model": result.get("model", model),
                    "tokens": result.get("tokens", token_count),
                    "dimensions": result.get("dimensions", len(result.get("embedding", [])))
                }

        # Use OpenAI directly
        import openai

        api_key = os.environ.get("OPENAI_API_KEY")
        if not api_key:
            return {
                "success": False,
                "error": "OPENAI_API_KEY environment variable not set",
                "error_type": "auth_error"
            }

        client = openai.OpenAI(api_key=api_key)
        response = client.embeddings.create(
            model=model,
            input=content,
            encoding_format="float"
        )

        return {
            "success": True,
            "embedding": response.data[0].embedding,
            "model": response.model,
            "tokens": response.usage.total_tokens,
            "dimensions": EMBEDDING_DIMENSIONS
        }

    except Exception as e:
        logger.error(f"Embedding generation failed: {e}")
        error_type = "rate_limit" if "rate" in str(e).lower() else "api_error"
        return {
            "success": False,
            "error": str(e),
            "error_type": error_type
        }


def memory_embed_batch(
    state: Dict[str, Any],
    contents: List[str],
    skip_on_error: bool = True,
    **kwargs
) -> Dict[str, Any]:
    """
    Generate embeddings for multiple content strings.

    TEA Custom Action: memory.embed_batch

    Args:
        state: Current agent state
        contents: List of strings to embed
        skip_on_error: If True, continue on individual errors
        **kwargs: Additional arguments
            embedding_fn: Custom embedding function (optional)

    Returns:
        {
            "success": True,
            "embeddings": [result_dict, ...],
            "total": int,
            "succeeded": int,
            "failed": int,
            "total_tokens": int
        }
    """
    if not contents or not isinstance(contents, list):
        return {
            "success": False,
            "error": "Contents must be a non-empty list of strings",
            "error_type": "invalid_input"
        }

    results = []
    succeeded = 0
    failed = 0
    total_tokens = 0

    for content in contents:
        result = memory_embed(state, content, **kwargs)
        results.append(result)

        if result.get("success"):
            succeeded += 1
            total_tokens += result.get("tokens", 0)
        else:
            failed += 1
            if not skip_on_error:
                break

    return {
        "success": failed == 0 or skip_on_error,
        "embeddings": results,
        "total": len(contents),
        "succeeded": succeeded,
        "failed": failed,
        "total_tokens": total_tokens
    }


def memory_backfill_embeddings(
    state: Dict[str, Any],
    batch_size: int = 10,
    limit: int = 100,
    project_id: Optional[str] = None,
    dry_run: bool = False,
    **kwargs
) -> Dict[str, Any]:
    """
    Backfill embeddings for documents missing them.

    TEA Custom Action: memory.backfill_embeddings

    Uses MetadataStore and BlobStorage backends injected via kwargs.

    Args:
        state: Current agent state
        batch_size: Number of docs to process at once (1-50)
        limit: Maximum total docs to process (1-500)
        project_id: Filter by project_id
        dry_run: If True, report counts but don't generate embeddings
        **kwargs: Additional arguments
            metadata_store: MetadataStore instance (required)
            blob_storage: BlobStorage instance (required)
            embedding_fn: Custom embedding function (optional)

    Returns:
        On success:
        {
            "success": True,
            "processed": int,
            "succeeded": int,
            "failed": int,
            "skipped": int,
            "total_tokens": int,
            "remaining": int
        }

        On error:
        {
            "success": False,
            "error": str,
            "error_type": str
        }
    """
    from ..memory.metadata import MetadataStore
    from ..memory.blob import BlobStorage

    # Get backends
    metadata_store = kwargs.get('metadata_store')
    blob_storage = kwargs.get('blob_storage')

    if metadata_store is None:
        return {
            "success": False,
            "error": "metadata_store is required for backfill_embeddings",
            "error_type": "missing_backend"
        }

    if blob_storage is None:
        return {
            "success": False,
            "error": "blob_storage is required for backfill_embeddings",
            "error_type": "missing_backend"
        }

    project_id = project_id or state.get("project_id", "rankellix")
    batch_size = min(max(1, batch_size), 50)
    limit = min(max(1, limit), 500)

    try:
        # Query for docs without embeddings
        query_result = metadata_store.query(
            collection="agent_memory",
            filters={"project_id": project_id, "embedding": None},
            limit=limit
        )

        if not query_result.get("success"):
            return {
                "success": False,
                "error": query_result.get("error", "Query failed"),
                "error_type": "query_failed"
            }

        docs = query_result.get("documents", [])
        total_to_process = len(docs)

        logger.info(f"Backfill: Found {total_to_process} docs without embeddings")

        if dry_run:
            return {
                "success": True,
                "dry_run": True,
                "docs_without_embeddings": total_to_process,
                "message": f"Would process up to {limit} docs"
            }

        processed = 0
        succeeded = 0
        failed = 0
        skipped = 0
        total_tokens = 0

        for doc in docs:
            doc_id = doc.get("id") or doc.get("doc_id")
            storage_uri = doc.get("storage_uri", "")

            if not storage_uri:
                logger.warning(f"Doc {doc_id} has no storage_uri, skipping")
                skipped += 1
                continue

            try:
                # Download content from blob storage
                download_result = blob_storage.download(storage_uri)
                if not download_result.get("success"):
                    logger.warning(f"Failed to download {storage_uri}: {download_result.get('error')}")
                    skipped += 1
                    continue

                content = download_result.get("content", "")
                if isinstance(content, bytes):
                    content = content.decode("utf-8")

                # Generate embedding
                embed_result = memory_embed(state, content, **kwargs)

                if embed_result.get("success"):
                    # Update metadata with embedding
                    update_result = metadata_store.update(
                        collection="agent_memory",
                        doc_id=doc_id,
                        data={
                            "embedding": embed_result["embedding"],
                            "embedding_model": embed_result["model"],
                            "synced_at": None  # Reset to trigger resync
                        }
                    )

                    if update_result.get("success"):
                        succeeded += 1
                        total_tokens += embed_result.get("tokens", 0)
                        logger.debug(f"Backfilled embedding for {doc_id}")
                    else:
                        failed += 1
                        logger.warning(f"Failed to update {doc_id}: {update_result.get('error')}")
                else:
                    failed += 1
                    logger.warning(f"Failed to generate embedding for {doc_id}: {embed_result.get('error')}")

                processed += 1

            except Exception as e:
                logger.error(f"Error processing doc {doc_id}: {e}")
                failed += 1
                processed += 1

        return {
            "success": True,
            "processed": processed,
            "succeeded": succeeded,
            "failed": failed,
            "skipped": skipped,
            "total_tokens": total_tokens,
            "remaining": total_to_process - processed,
            "message": f"Backfilled {succeeded}/{processed} docs"
        }

    except Exception as e:
        logger.error(f"Backfill failed: {e}")
        return {
            "success": False,
            "error": str(e),
            "error_type": "backfill_failed"
        }


# =============================================================================
# ACTION REGISTRATION
# =============================================================================

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register vector search actions with the TEA YAMLEngine.

    Args:
        registry: Action registry dictionary
        engine: YAMLEngine instance
    """
    # Wrapper to inject engine backends into kwargs
    def wrap_with_backends(fn: Callable) -> Callable:
        """Wrap action to inject engine's vector_index and embedding_fn if not provided."""
        def wrapped(state: Dict[str, Any], *args, **kwargs) -> Dict[str, Any]:
            # Inject vector_index from engine if not already provided
            if 'vector_index' not in kwargs:
                if hasattr(engine, '_vector_index') and engine._vector_index is not None:
                    kwargs['vector_index'] = engine._vector_index
            # Inject embedding_fn from engine if not already provided
            if 'embedding_fn' not in kwargs:
                if hasattr(engine, '_embedding_fn') and engine._embedding_fn is not None:
                    kwargs['embedding_fn'] = engine._embedding_fn
            return fn(state, *args, **kwargs)
        # Preserve function metadata for introspection
        wrapped.__name__ = fn.__name__
        wrapped.__doc__ = fn.__doc__
        return wrapped

    registry["memory.vector_search"] = wrap_with_backends(memory_vector_search)
    registry["memory.vector_search_by_embedding"] = wrap_with_backends(memory_vector_search_by_embedding)
    registry["memory.vector_load_data"] = wrap_with_backends(memory_vector_load_data)
    registry["memory.vector_build_index"] = wrap_with_backends(memory_vector_build_index)
    registry["memory.vector_stats"] = wrap_with_backends(memory_vector_stats)

    # Embedding actions (need metadata_store and blob_storage for backfill)
    def wrap_with_all_backends(fn: Callable) -> Callable:
        """Wrap action to inject all engine backends."""
        def wrapped(state: Dict[str, Any], *args, **kwargs) -> Dict[str, Any]:
            if 'vector_index' not in kwargs:
                if hasattr(engine, '_vector_index') and engine._vector_index is not None:
                    kwargs['vector_index'] = engine._vector_index
            if 'embedding_fn' not in kwargs:
                if hasattr(engine, '_embedding_fn') and engine._embedding_fn is not None:
                    kwargs['embedding_fn'] = engine._embedding_fn
            if 'metadata_store' not in kwargs:
                if hasattr(engine, '_metadata_store') and engine._metadata_store is not None:
                    kwargs['metadata_store'] = engine._metadata_store
            if 'blob_storage' not in kwargs:
                if hasattr(engine, '_blob_storage') and engine._blob_storage is not None:
                    kwargs['blob_storage'] = engine._blob_storage
            return fn(state, *args, **kwargs)
        wrapped.__name__ = fn.__name__
        wrapped.__doc__ = fn.__doc__
        return wrapped

    registry["memory.embed"] = wrap_with_all_backends(memory_embed)
    registry["memory.embed_batch"] = wrap_with_all_backends(memory_embed_batch)
    registry["memory.backfill_embeddings"] = wrap_with_all_backends(memory_backfill_embeddings)

    logger.info(
        "Vector search actions registered: memory.vector_search, "
        "memory.vector_search_by_embedding, memory.vector_load_data, "
        "memory.vector_build_index, memory.vector_stats, "
        "memory.embed, memory.embed_batch, memory.backfill_embeddings"
    )


# Module metadata for discovery
__tea_actions__ = {
    "version": "1.0.0",
    "description": "Vector similarity search and embedding actions using VectorIndex abstraction",
    "actions": [
        "memory.vector_search",
        "memory.vector_search_by_embedding",
        "memory.vector_load_data",
        "memory.vector_build_index",
        "memory.vector_stats",
        "memory.embed",
        "memory.embed_batch",
        "memory.backfill_embeddings"
    ],
    "story": "TEA-BUILTIN-006"
}
