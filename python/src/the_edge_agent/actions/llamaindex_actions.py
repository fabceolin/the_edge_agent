"""
LlamaIndex RAG Actions for YAMLEngine (TEA-AGENT-001.8).

This module provides LlamaIndex integration actions for advanced RAG patterns
including router queries, sub-question decomposition, and index management.

Actions:
    - rag.llamaindex.query: Simple vector query against LlamaIndex index
    - rag.llamaindex.router: Router Query Engine for dynamic retrieval selection
    - rag.llamaindex.subquestion: Sub-Question Query Engine for complex queries
    - rag.llamaindex.create_index: Create new index from documents
    - rag.llamaindex.load_index: Load persisted index
    - rag.llamaindex.add_documents: Add documents to existing index

When LlamaIndex is not available, actions fall back to native rag.* actions
with a warning log.

Example:
    >>> # Simple query
    >>> result = registry['rag.llamaindex.query'](
    ...     state={},
    ...     query="What is machine learning?",
    ...     index_path="./vector_store"
    ... )
    >>> print(result['response'])

    >>> # Router query
    >>> result = registry['rag.llamaindex.router'](
    ...     state={},
    ...     query="Get sales data",
    ...     engines=[
    ...         {"type": "vector", "index_path": "./docs", "description": "Docs"},
    ...         {"type": "sql", "connection": "sqlite:///db.sqlite", "description": "DB"}
    ...     ]
    ... )
    >>> print(result['selected_engine'])

    >>> # Sub-question decomposition
    >>> result = registry['rag.llamaindex.subquestion'](
    ...     state={},
    ...     query="Compare Apple and Google revenue from 2020 to 2024",
    ...     engines=[{"type": "vector", "index_path": "./financials", "description": "Financials"}],
    ...     parallel=True
    ... )
    >>> print(result['sub_questions'])
"""

import logging
from typing import Any, Callable, Dict, List, Optional, Union

from ..rag import LlamaIndexClient, LlamaIndexUnavailableError, LLAMAINDEX_AVAILABLE

logger = logging.getLogger(__name__)


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register LlamaIndex RAG actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """
    # Store LlamaIndex client on engine for reuse
    if not hasattr(engine, "_llamaindex_client"):
        engine._llamaindex_client = None

    def _get_client(state: Dict[str, Any], **kwargs) -> LlamaIndexClient:
        """Get or create LlamaIndex client based on config."""
        # Check engine settings
        settings = {}
        if hasattr(engine, "variables"):
            settings = engine.variables.get("settings", {})

        # Override with kwargs
        llamaindex_settings = settings.get("llamaindex", {})
        if kwargs.get("index_path"):
            llamaindex_settings["index_path"] = kwargs["index_path"]
        if kwargs.get("embedding_model"):
            llamaindex_settings["embedding_model"] = kwargs["embedding_model"]
        if kwargs.get("llm_model"):
            llamaindex_settings["llm_model"] = kwargs["llm_model"]
        if kwargs.get("api_key"):
            llamaindex_settings["api_key"] = kwargs["api_key"]

        # Create or reuse client
        if engine._llamaindex_client is None:
            engine._llamaindex_client = LlamaIndexClient.from_settings(
                {"llamaindex": llamaindex_settings}
            )

        return engine._llamaindex_client

    def _fallback_to_native_query(
        state: Dict[str, Any], query: str, k: int = 5, **kwargs
    ) -> Dict[str, Any]:
        """Fallback to native rag.query when LlamaIndex unavailable."""
        logger.warning("LlamaIndex not available, falling back to native rag.query")
        if "vector.query" in registry:
            return registry["vector.query"](state=state, query=query, k=k, **kwargs)
        return {
            "error": "LlamaIndex not available and no native fallback configured",
            "success": False,
            "fallback_attempted": True,
        }

    # =========================================================================
    # rag.llamaindex.query (AC: 1)
    # =========================================================================

    def llamaindex_query(
        state: Dict[str, Any],
        query: str,
        index_path: Optional[str] = None,
        similarity_top_k: int = 5,
        response_mode: str = "compact",
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Execute a simple vector query against a LlamaIndex index.

        Args:
            state: Current state dictionary
            query: Query text
            index_path: Path to index storage (uses settings default if not provided)
            similarity_top_k: Number of nodes to retrieve (default: 5)
            response_mode: Response synthesis mode (default: compact)
            **kwargs: Additional configuration

        Returns:
            {
                "response": str,
                "nodes": [{"id": str, "text": str, "metadata": dict}],
                "scores": [float],
                "success": True
            }
            Error: {"error": str, "success": False}
        """
        if not LLAMAINDEX_AVAILABLE:
            return _fallback_to_native_query(state, query, k=similarity_top_k, **kwargs)

        try:
            client = _get_client(state, index_path=index_path, **kwargs)
            result = client.query(
                query=query,
                index_path=index_path,
                similarity_top_k=similarity_top_k,
                response_mode=response_mode,
            )
            result["success"] = True
            return result

        except LlamaIndexUnavailableError as e:
            return _fallback_to_native_query(state, query, k=similarity_top_k, **kwargs)
        except FileNotFoundError as e:
            return {
                "error": f"Index not found: {str(e)}",
                "success": False,
            }
        except Exception as e:
            logger.error(f"LlamaIndex query failed: {e}")
            return {
                "error": f"Query failed: {str(e)}",
                "success": False,
            }

    registry["rag.llamaindex.query"] = llamaindex_query
    registry["actions.rag_llamaindex_query"] = llamaindex_query

    # =========================================================================
    # rag.llamaindex.router (AC: 2)
    # =========================================================================

    def llamaindex_router(
        state: Dict[str, Any],
        query: str,
        engines: List[Dict[str, Any]],
        verbose: bool = False,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Execute a router query that selects the best engine for the query.

        The router uses an LLM to analyze the query and select the most
        appropriate engine based on the provided descriptions.

        Args:
            state: Current state dictionary
            query: Query text
            engines: List of engine configurations:
                - type: "vector", "keyword", or "sql"
                - index_path: Path to index (for vector/keyword)
                - connection: DB connection string (for sql)
                - description: Description for LLM selection
                - name: Optional engine name
            verbose: Enable verbose logging
            **kwargs: Additional configuration

        Returns:
            {
                "response": str,
                "selected_engine": dict,
                "nodes": [{"id": str, "text": str, "metadata": dict}],
                "scores": [float],
                "success": True
            }
            Error: {"error": str, "success": False}

        Example:
            >>> result = llamaindex_router(
            ...     state={},
            ...     query="Find recent sales data",
            ...     engines=[
            ...         {
            ...             "type": "vector",
            ...             "index_path": "./docs_index",
            ...             "description": "Semantic search over documentation"
            ...         },
            ...         {
            ...             "type": "sql",
            ...             "connection": "sqlite:///sales.db",
            ...             "description": "Structured sales data queries"
            ...         }
            ...     ]
            ... )
        """
        if not LLAMAINDEX_AVAILABLE:
            # For router, we can't meaningfully fall back
            # Just use the first vector engine if available
            for eng in engines:
                if eng.get("type") == "vector" and eng.get("index_path"):
                    logger.warning(
                        "LlamaIndex not available, using first vector engine"
                    )
                    return _fallback_to_native_query(state, query, **kwargs)
            return {
                "error": "LlamaIndex not available for router queries",
                "success": False,
                "fallback_attempted": False,
            }

        try:
            client = _get_client(state, **kwargs)
            result = client.router_query(
                query=query,
                engines=engines,
                verbose=verbose,
            )
            result["success"] = True
            return result

        except LlamaIndexUnavailableError as e:
            return {
                "error": str(e),
                "success": False,
            }
        except ValueError as e:
            return {
                "error": f"Invalid engine configuration: {str(e)}",
                "success": False,
            }
        except Exception as e:
            logger.error(f"LlamaIndex router query failed: {e}")
            return {
                "error": f"Router query failed: {str(e)}",
                "success": False,
            }

    registry["rag.llamaindex.router"] = llamaindex_router
    registry["actions.rag_llamaindex_router"] = llamaindex_router

    # =========================================================================
    # rag.llamaindex.subquestion (AC: 3)
    # =========================================================================

    def llamaindex_subquestion(
        state: Dict[str, Any],
        query: str,
        engines: Optional[List[Dict[str, Any]]] = None,
        index_path: Optional[str] = None,
        parallel: bool = False,
        synthesis_prompt: Optional[str] = None,
        verbose: bool = False,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Execute a sub-question query that decomposes complex queries.

        The sub-question engine breaks down complex queries into simpler
        sub-questions, executes them, and synthesizes a final answer.

        Args:
            state: Current state dictionary
            query: Complex query text
            engines: List of engine configurations (same format as router)
            index_path: Simple path for single-index queries
            parallel: Enable parallel execution of sub-questions
            synthesis_prompt: Custom prompt for answer synthesis
            verbose: Enable verbose logging
            **kwargs: Additional configuration

        Returns:
            {
                "response": str,
                "sub_questions": [str],
                "sub_answers": [str],
                "success": True
            }
            Error: {"error": str, "success": False}

        Example:
            >>> result = llamaindex_subquestion(
            ...     state={},
            ...     query="Compare the revenue growth of Apple and Google from 2020 to 2024",
            ...     engines=[
            ...         {"type": "vector", "index_path": "./financials", "description": "Financial data"}
            ...     ],
            ...     parallel=True
            ... )
            >>> print(result['sub_questions'])
            ['What was Apple revenue in 2020?', 'What was Apple revenue in 2024?', ...]
        """
        if not LLAMAINDEX_AVAILABLE:
            return {
                "error": "LlamaIndex not available for sub-question queries",
                "success": False,
                "fallback_attempted": False,
            }

        try:
            client = _get_client(state, **kwargs)

            # Build engines list if not provided
            if engines is None:
                if index_path:
                    engines = [
                        {
                            "type": "vector",
                            "index_path": index_path,
                            "description": "Default vector index",
                        }
                    ]
                else:
                    return {
                        "error": "Either engines or index_path must be provided",
                        "success": False,
                    }

            result = client.subquestion_query(
                query=query,
                engines=engines,
                use_async=parallel,
                verbose=verbose,
            )
            result["success"] = True
            return result

        except LlamaIndexUnavailableError as e:
            return {
                "error": str(e),
                "success": False,
            }
        except ValueError as e:
            return {
                "error": f"Invalid configuration: {str(e)}",
                "success": False,
            }
        except Exception as e:
            logger.error(f"LlamaIndex sub-question query failed: {e}")
            return {
                "error": f"Sub-question query failed: {str(e)}",
                "success": False,
            }

    registry["rag.llamaindex.subquestion"] = llamaindex_subquestion
    registry["actions.rag_llamaindex_subquestion"] = llamaindex_subquestion

    # =========================================================================
    # rag.llamaindex.create_index (AC: 4)
    # =========================================================================

    def llamaindex_create_index(
        state: Dict[str, Any],
        documents: Optional[List[Dict[str, Any]]] = None,
        directory: Optional[str] = None,
        persist_path: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Create a new LlamaIndex index from documents or a directory.

        Args:
            state: Current state dictionary
            documents: List of document dicts with 'text' and optional 'metadata'
            directory: Directory path to load documents from
            persist_path: Path to persist the index

        Returns:
            {
                "success": True,
                "persist_path": str,
                "document_count": int
            }
            Error: {"error": str, "success": False}

        Example:
            >>> # From documents
            >>> result = llamaindex_create_index(
            ...     state={},
            ...     documents=[
            ...         {"text": "Doc 1 content", "metadata": {"type": "article"}},
            ...         {"text": "Doc 2 content", "metadata": {"type": "blog"}}
            ...     ],
            ...     persist_path="./my_index"
            ... )

            >>> # From directory
            >>> result = llamaindex_create_index(
            ...     state={},
            ...     directory="./documents/",
            ...     persist_path="./my_index"
            ... )
        """
        if not LLAMAINDEX_AVAILABLE:
            return {
                "error": "LlamaIndex not available for index creation",
                "success": False,
            }

        try:
            client = _get_client(state, **kwargs)
            client.create_index(
                documents=documents,
                directory=directory,
                persist_path=persist_path,
            )

            doc_count = len(documents) if documents else 0

            return {
                "success": True,
                "persist_path": persist_path,
                "document_count": doc_count,
            }

        except LlamaIndexUnavailableError as e:
            return {
                "error": str(e),
                "success": False,
            }
        except ValueError as e:
            return {
                "error": f"Invalid input: {str(e)}",
                "success": False,
            }
        except Exception as e:
            logger.error(f"LlamaIndex index creation failed: {e}")
            return {
                "error": f"Index creation failed: {str(e)}",
                "success": False,
            }

    registry["rag.llamaindex.create_index"] = llamaindex_create_index
    registry["actions.rag_llamaindex_create_index"] = llamaindex_create_index

    # =========================================================================
    # rag.llamaindex.load_index (AC: 4)
    # =========================================================================

    def llamaindex_load_index(
        state: Dict[str, Any], index_path: str, force_reload: bool = False, **kwargs
    ) -> Dict[str, Any]:
        """
        Load a persisted LlamaIndex index.

        Args:
            state: Current state dictionary
            index_path: Path to the index storage
            force_reload: Force reload even if cached

        Returns:
            {
                "success": True,
                "index_path": str,
                "cached": bool
            }
            Error: {"error": str, "success": False}

        Example:
            >>> result = llamaindex_load_index(
            ...     state={},
            ...     index_path="./my_index"
            ... )
            >>> if result['success']:
            ...     print(f"Loaded from: {result['index_path']}")
        """
        if not LLAMAINDEX_AVAILABLE:
            return {
                "error": "LlamaIndex not available for index loading",
                "success": False,
            }

        try:
            client = _get_client(state, **kwargs)

            # Check if already cached
            cache_info = client.get_cache_info()
            was_cached = index_path in cache_info["cached_paths"]

            client.load_index(
                index_path=index_path,
                force_reload=force_reload,
            )

            return {
                "success": True,
                "index_path": index_path,
                "cached": was_cached and not force_reload,
            }

        except LlamaIndexUnavailableError as e:
            return {
                "error": str(e),
                "success": False,
            }
        except FileNotFoundError as e:
            return {
                "error": f"Index not found: {str(e)}",
                "success": False,
            }
        except Exception as e:
            logger.error(f"LlamaIndex index loading failed: {e}")
            return {
                "error": f"Index loading failed: {str(e)}",
                "success": False,
            }

    registry["rag.llamaindex.load_index"] = llamaindex_load_index
    registry["actions.rag_llamaindex_load_index"] = llamaindex_load_index

    # =========================================================================
    # rag.llamaindex.add_documents (AC: 4)
    # =========================================================================

    def llamaindex_add_documents(
        state: Dict[str, Any],
        documents: List[Dict[str, Any]],
        index_path: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Add documents to an existing LlamaIndex index.

        Args:
            state: Current state dictionary
            documents: List of document dicts with 'text' and optional 'metadata'
            index_path: Path to index (uses settings default if not provided)

        Returns:
            {
                "success": True,
                "added": int,
                "index_path": str
            }
            Error: {"error": str, "success": False}

        Example:
            >>> result = llamaindex_add_documents(
            ...     state={},
            ...     documents=[
            ...         {"text": "New document content", "metadata": {"date": "2024-01-15"}}
            ...     ],
            ...     index_path="./my_index"
            ... )
        """
        if not LLAMAINDEX_AVAILABLE:
            return {
                "error": "LlamaIndex not available for adding documents",
                "success": False,
            }

        try:
            client = _get_client(state, index_path=index_path, **kwargs)
            added = client.add_documents(
                documents=documents,
                index_path=index_path,
            )

            return {
                "success": True,
                "added": added,
                "index_path": index_path or client._index_path,
            }

        except LlamaIndexUnavailableError as e:
            return {
                "error": str(e),
                "success": False,
            }
        except FileNotFoundError as e:
            return {
                "error": f"Index not found: {str(e)}",
                "success": False,
            }
        except Exception as e:
            logger.error(f"LlamaIndex add documents failed: {e}")
            return {
                "error": f"Add documents failed: {str(e)}",
                "success": False,
            }

    registry["rag.llamaindex.add_documents"] = llamaindex_add_documents
    registry["actions.rag_llamaindex_add_documents"] = llamaindex_add_documents
