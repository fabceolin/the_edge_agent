"""
Mem0 Client Wrapper for The Edge Agent (TEA-AGENT-001.6).

This module provides a wrapper around the Mem0 library for universal memory
management. It handles:

- Configuration from YAML settings (settings.memory.backend: mem0)
- API key and endpoint management
- Connection testing with graceful fallback
- Scope handling (user_id, session_id, agent_id)
- Graph memory support when enabled

Usage:
    >>> from the_edge_agent.memory.mem0_client import Mem0Client
    >>>
    >>> # Create client from settings
    >>> client = Mem0Client.from_settings({
    ...     "memory": {
    ...         "backend": "mem0",
    ...         "api_key": "${MEM0_API_KEY}",
    ...         "user_id": "user123"
    ...     }
    ... })
    >>>
    >>> # Check availability
    >>> if client.is_available():
    ...     result = client.add(messages, user_id="user123")
"""

import logging
import os
from typing import Any, Dict, List, Optional, Union

logger = logging.getLogger(__name__)

# Track whether mem0 is available
_mem0_available = None
_mem0_client_class = None
_mem0_memory_class = None


def _check_mem0_available() -> bool:
    """Check if mem0ai package is installed and importable."""
    global _mem0_available, _mem0_client_class, _mem0_memory_class

    if _mem0_available is not None:
        return _mem0_available

    try:
        from mem0 import MemoryClient
        from mem0 import Memory

        _mem0_client_class = MemoryClient
        _mem0_memory_class = Memory
        _mem0_available = True
        logger.debug("Mem0 library is available")
    except ImportError:
        _mem0_available = False
        _mem0_client_class = None
        _mem0_memory_class = None
        logger.debug("Mem0 library not installed. Install with: pip install mem0ai")

    return _mem0_available


class Mem0ClientError(Exception):
    """Exception raised for Mem0 client errors."""

    pass


class Mem0Client:
    """
    Wrapper around Mem0 for unified memory management.

    This client provides a consistent interface for Mem0 operations,
    handling configuration, connection testing, and graceful fallback.

    Attributes:
        api_key: Mem0 API key for cloud access
        endpoint: Custom API endpoint (optional)
        default_user_id: Default user scope
        default_session_id: Default session scope
        default_agent_id: Default agent scope
        graph_enabled: Whether graph memory is enabled
        _client: Internal Mem0 MemoryClient instance
        _memory: Internal Mem0 Memory instance (for local mode)
    """

    def __init__(
        self,
        api_key: Optional[str] = None,
        endpoint: Optional[str] = None,
        user_id: Optional[str] = None,
        session_id: Optional[str] = None,
        agent_id: Optional[str] = None,
        graph_enabled: bool = False,
        config: Optional[Dict[str, Any]] = None,
    ):
        """
        Initialize Mem0 client wrapper.

        Args:
            api_key: Mem0 API key (uses MEM0_API_KEY env var if not provided)
            endpoint: Custom API endpoint for self-hosted Mem0
            user_id: Default user_id for all operations
            session_id: Default session_id for all operations
            agent_id: Default agent_id for all operations
            graph_enabled: Enable graph memory features
            config: Additional Mem0 configuration options
        """
        self.api_key = api_key or os.environ.get("MEM0_API_KEY")
        self.endpoint = endpoint or os.environ.get("MEM0_ENDPOINT")
        self.default_user_id = user_id
        self.default_session_id = session_id
        self.default_agent_id = agent_id
        self.graph_enabled = graph_enabled
        self.config = config or {}

        self._client = None
        self._memory = None
        self._initialized = False
        self._error_message = None

    @classmethod
    def from_settings(cls, settings: Dict[str, Any]) -> "Mem0Client":
        """
        Create Mem0Client from YAML engine settings.

        Args:
            settings: Settings dictionary from YAML configuration

        Returns:
            Configured Mem0Client instance

        Example:
            >>> settings = {
            ...     "memory": {
            ...         "backend": "mem0",
            ...         "api_key": "${MEM0_API_KEY}",
            ...         "user_id": "user123",
            ...         "graph": True
            ...     }
            ... }
            >>> client = Mem0Client.from_settings(settings)
        """
        memory_settings = settings.get("memory", {})

        # Extract API key with environment variable expansion
        api_key = memory_settings.get("api_key")
        if api_key and api_key.startswith("${") and api_key.endswith("}"):
            env_var = api_key[2:-1]
            api_key = os.environ.get(env_var)

        # Extract endpoint with environment variable expansion
        endpoint = memory_settings.get("endpoint")
        if endpoint and endpoint.startswith("${") and endpoint.endswith("}"):
            env_var = endpoint[2:-1]
            endpoint = os.environ.get(env_var)

        return cls(
            api_key=api_key,
            endpoint=endpoint,
            user_id=memory_settings.get("user_id"),
            session_id=memory_settings.get("session_id"),
            agent_id=memory_settings.get("agent_id"),
            graph_enabled=memory_settings.get("graph", False),
            config=memory_settings.get("config", {}),
        )

    def is_available(self) -> bool:
        """
        Check if Mem0 is available and properly configured.

        Returns:
            True if Mem0 can be used, False otherwise
        """
        if not _check_mem0_available():
            self._error_message = "Mem0 library not installed"
            return False

        return True

    def _ensure_initialized(self) -> bool:
        """
        Lazily initialize the Mem0 client.

        Returns:
            True if initialization successful, False otherwise
        """
        if self._initialized:
            return self._client is not None or self._memory is not None

        if not self.is_available():
            return False

        self._initialized = True

        try:
            # Use cloud client if API key is provided
            if self.api_key:
                kwargs = {"api_key": self.api_key}
                if self.endpoint:
                    kwargs["host"] = self.endpoint

                self._client = _mem0_client_class(**kwargs)
                logger.info("Initialized Mem0 cloud client")
            else:
                # Use local Memory instance
                config = dict(self.config) if self.config else {}
                if self.graph_enabled:
                    # Enable graph store if not configured
                    if "graph_store" not in config:
                        config["graph_store"] = {"provider": "neo4j"}

                self._memory = (
                    _mem0_memory_class(config=config)
                    if config
                    else _mem0_memory_class()
                )
                logger.info("Initialized Mem0 local memory")

            return True

        except Exception as e:
            self._error_message = f"Failed to initialize Mem0: {str(e)}"
            logger.error(self._error_message)
            return False

    def _get_scope_params(
        self,
        user_id: Optional[str] = None,
        session_id: Optional[str] = None,
        agent_id: Optional[str] = None,
    ) -> Dict[str, str]:
        """
        Build scope parameters from provided and default values.

        Args:
            user_id: User scope (overrides default)
            session_id: Session scope (overrides default)
            agent_id: Agent scope (overrides default)

        Returns:
            Dictionary of scope parameters
        """
        params = {}

        uid = user_id or self.default_user_id
        if uid:
            params["user_id"] = str(uid)

        sid = session_id or self.default_session_id
        if sid:
            # Mem0 uses run_id for session in some versions
            params["session_id"] = str(sid)

        aid = agent_id or self.default_agent_id
        if aid:
            params["agent_id"] = str(aid)

        return params

    def add(
        self,
        messages: Union[str, List[Dict[str, str]], Dict[str, str]],
        user_id: Optional[str] = None,
        session_id: Optional[str] = None,
        agent_id: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Add memories from messages with automatic fact extraction.

        Args:
            messages: Conversation messages (string, dict, or list of dicts)
            user_id: User scope for the memory
            session_id: Session scope for the memory
            agent_id: Agent scope for the memory
            metadata: Additional metadata to store

        Returns:
            Dictionary with:
                - success: bool
                - memory_id: str (if single memory created)
                - memories: list (if multiple memories extracted)
                - error: str (if failed)
        """
        if not self._ensure_initialized():
            return {
                "success": False,
                "error": self._error_message or "Mem0 not available",
            }

        try:
            scope = self._get_scope_params(user_id, session_id, agent_id)

            # Normalize messages to list format
            if isinstance(messages, str):
                messages = [{"role": "user", "content": messages}]
            elif isinstance(messages, dict):
                messages = [messages]

            # Build kwargs
            kwargs = {**scope}
            if metadata:
                kwargs["metadata"] = metadata

            # Call appropriate client
            if self._client:
                result = self._client.add(messages, **kwargs)
            else:
                result = self._memory.add(messages, **kwargs)

            # Normalize response
            if isinstance(result, dict):
                return {
                    "success": True,
                    "memory_id": result.get("id"),
                    "memories": result.get("results", result.get("memories", [])),
                    "raw": result,
                }
            else:
                return {
                    "success": True,
                    "raw": result,
                }

        except Exception as e:
            logger.error(f"Mem0 add failed: {e}")
            return {
                "success": False,
                "error": str(e),
            }

    def search(
        self,
        query: str,
        user_id: Optional[str] = None,
        session_id: Optional[str] = None,
        agent_id: Optional[str] = None,
        limit: int = 5,
        include_relations: bool = False,
    ) -> Dict[str, Any]:
        """
        Search memories by semantic similarity.

        Args:
            query: Search query string
            user_id: Filter by user scope
            session_id: Filter by session scope
            agent_id: Filter by agent scope
            limit: Maximum number of results
            include_relations: Include graph relations in results

        Returns:
            Dictionary with:
                - success: bool
                - results: list of memory objects with scores
                - relations: list (if include_relations and graph enabled)
                - error: str (if failed)
        """
        if not self._ensure_initialized():
            return {
                "success": False,
                "error": self._error_message or "Mem0 not available",
                "results": [],
            }

        try:
            scope = self._get_scope_params(user_id, session_id, agent_id)

            kwargs = {
                **scope,
                "top_k": limit,
            }

            if self._client:
                result = self._client.search(query, **kwargs)
            else:
                result = self._memory.search(query, **kwargs)

            # Normalize response
            results = []
            relations = []

            if isinstance(result, dict):
                results = result.get("results", result.get("memories", []))
                if include_relations and self.graph_enabled:
                    relations = result.get("relations", [])
            elif isinstance(result, list):
                results = result

            return {
                "success": True,
                "results": results,
                "relations": relations if include_relations else None,
            }

        except Exception as e:
            logger.error(f"Mem0 search failed: {e}")
            return {
                "success": False,
                "error": str(e),
                "results": [],
            }

    def get_all(
        self,
        user_id: Optional[str] = None,
        session_id: Optional[str] = None,
        agent_id: Optional[str] = None,
        limit: Optional[int] = None,
        offset: int = 0,
    ) -> Dict[str, Any]:
        """
        Get all memories for a scope.

        Args:
            user_id: Filter by user scope
            session_id: Filter by session scope
            agent_id: Filter by agent scope
            limit: Maximum number of results
            offset: Skip first N results (pagination)

        Returns:
            Dictionary with:
                - success: bool
                - memories: list of memory objects
                - total: int (total count if available)
                - error: str (if failed)
        """
        if not self._ensure_initialized():
            return {
                "success": False,
                "error": self._error_message or "Mem0 not available",
                "memories": [],
            }

        try:
            scope = self._get_scope_params(user_id, session_id, agent_id)

            kwargs = {**scope}
            if limit:
                kwargs["top_k"] = limit
            # Note: Mem0 may use page/page_size instead of offset
            if offset > 0:
                page_size = limit or 10
                kwargs["page"] = (offset // page_size) + 1
                kwargs["page_size"] = page_size

            if self._client:
                result = self._client.get_all(**kwargs)
            else:
                result = self._memory.get_all(**kwargs)

            # Normalize response
            memories = []
            total = None

            if isinstance(result, dict):
                memories = result.get("results", result.get("memories", []))
                total = result.get("total", result.get("count", len(memories)))
            elif isinstance(result, list):
                memories = result
                total = len(memories)

            return {
                "success": True,
                "memories": memories,
                "total": total,
            }

        except Exception as e:
            logger.error(f"Mem0 get_all failed: {e}")
            return {
                "success": False,
                "error": str(e),
                "memories": [],
            }

    def get(self, memory_id: str) -> Dict[str, Any]:
        """
        Get a specific memory by ID.

        Args:
            memory_id: Memory identifier

        Returns:
            Dictionary with:
                - success: bool
                - memory: dict (memory object)
                - error: str (if failed)
        """
        if not self._ensure_initialized():
            return {
                "success": False,
                "error": self._error_message or "Mem0 not available",
            }

        try:
            if self._client:
                result = self._client.get(memory_id)
            else:
                result = self._memory.get(memory_id)

            return {
                "success": True,
                "memory": result,
            }

        except Exception as e:
            logger.error(f"Mem0 get failed: {e}")
            return {
                "success": False,
                "error": str(e),
            }

    def update(
        self,
        memory_id: str,
        text: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Update an existing memory.

        Args:
            memory_id: Memory identifier
            text: New text content (optional)
            metadata: New/updated metadata (optional, merged with existing)

        Returns:
            Dictionary with:
                - success: bool
                - memory: dict (updated memory object)
                - error: str (if failed)
        """
        if not self._ensure_initialized():
            return {
                "success": False,
                "error": self._error_message or "Mem0 not available",
            }

        try:
            kwargs = {}
            if text is not None:
                kwargs["text"] = text
            if metadata is not None:
                kwargs["metadata"] = metadata

            if self._client:
                result = self._client.update(memory_id, **kwargs)
            else:
                result = self._memory.update(memory_id, **kwargs)

            return {
                "success": True,
                "memory": result,
            }

        except Exception as e:
            logger.error(f"Mem0 update failed: {e}")
            return {
                "success": False,
                "error": str(e),
            }

    def delete(
        self,
        memory_id: Optional[str] = None,
        user_id: Optional[str] = None,
        session_id: Optional[str] = None,
        agent_id: Optional[str] = None,
        delete_all: bool = False,
    ) -> Dict[str, Any]:
        """
        Delete memories by ID or scope.

        Args:
            memory_id: Delete specific memory by ID
            user_id: Delete all memories for user (bulk)
            session_id: Delete all memories for session (bulk)
            agent_id: Delete all memories for agent (bulk)
            delete_all: Must be True for bulk delete operations

        Returns:
            Dictionary with:
                - success: bool
                - deleted_count: int (for bulk operations)
                - error: str (if failed)
        """
        if not self._ensure_initialized():
            return {
                "success": False,
                "error": self._error_message or "Mem0 not available",
            }

        try:
            # Single memory deletion
            if memory_id:
                if self._client:
                    result = self._client.delete(memory_id)
                else:
                    result = self._memory.delete(memory_id)

                return {
                    "success": True,
                    "deleted_count": 1,
                    "raw": result,
                }

            # Bulk deletion requires explicit flag
            if not delete_all:
                return {
                    "success": False,
                    "error": "Bulk delete requires delete_all=True",
                }

            scope = self._get_scope_params(user_id, session_id, agent_id)
            if not scope:
                return {
                    "success": False,
                    "error": "Bulk delete requires user_id, session_id, or agent_id",
                }

            if self._client:
                result = self._client.delete_all(**scope)
            else:
                result = self._memory.delete_all(**scope)

            deleted_count = 0
            if isinstance(result, dict):
                deleted_count = result.get("deleted_count", result.get("count", 0))

            return {
                "success": True,
                "deleted_count": deleted_count,
                "raw": result,
            }

        except Exception as e:
            logger.error(f"Mem0 delete failed: {e}")
            return {
                "success": False,
                "error": str(e),
            }

    def test_connection(self) -> Dict[str, Any]:
        """
        Test the connection to Mem0 service.

        Returns:
            Dictionary with:
                - success: bool
                - message: str
                - error: str (if failed)
        """
        if not self.is_available():
            return {
                "success": False,
                "error": "Mem0 library not installed",
                "message": "Install with: pip install mem0ai",
            }

        if not self._ensure_initialized():
            return {
                "success": False,
                "error": self._error_message,
                "message": "Failed to initialize Mem0 client",
            }

        # Try a simple operation to verify connectivity
        try:
            result = self.search("test", limit=1)
            if result.get("success"):
                return {
                    "success": True,
                    "message": "Mem0 connection successful",
                }
            else:
                return {
                    "success": False,
                    "error": result.get("error"),
                    "message": "Mem0 connection test failed",
                }
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "message": "Mem0 connection test failed",
            }


def get_mem0_client(settings: Optional[Dict[str, Any]] = None) -> Optional[Mem0Client]:
    """
    Factory function to get a Mem0 client if available.

    Args:
        settings: Optional settings dictionary

    Returns:
        Mem0Client instance or None if not available/configured
    """
    if not _check_mem0_available():
        return None

    if settings:
        memory_settings = settings.get("memory", {})
        if memory_settings.get("backend") != "mem0":
            return None
        return Mem0Client.from_settings(settings)

    return Mem0Client()
