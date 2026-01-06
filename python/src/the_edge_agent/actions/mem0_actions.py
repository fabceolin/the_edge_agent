"""
Mem0 Memory Actions for YAMLEngine (TEA-AGENT-001.6).

This module provides Mem0 integration actions for universal memory management
in YAMLEngine workflows. Actions support:

- Automatic fact extraction from conversations
- User/session/agent scoped memories
- Semantic search over memories
- Graph-based knowledge storage (when enabled)
- Graceful fallback to native memory when Mem0 unavailable

Actions:
    - memory.mem0.add: Store messages with automatic fact extraction
    - memory.mem0.search: Retrieve relevant memories by semantic similarity
    - memory.mem0.get_all: Get all memories for a scope
    - memory.mem0.get: Get a specific memory by ID
    - memory.mem0.update: Update an existing memory
    - memory.mem0.delete: Delete memories by ID or scope

Example:
    >>> # Store conversation with fact extraction
    >>> result = registry['memory.mem0.add'](
    ...     state={},
    ...     messages=[
    ...         {"role": "user", "content": "I'm allergic to peanuts"},
    ...         {"role": "assistant", "content": "I'll remember that!"}
    ...     ],
    ...     user_id="user123"
    ... )
    >>> print(result['success'])  # True
    >>>
    >>> # Search for relevant memories
    >>> result = registry['memory.mem0.search'](
    ...     state={},
    ...     query="What are my allergies?",
    ...     user_id="user123",
    ...     limit=5
    ... )
    >>> print(result['results'])  # [{"memory": "User is allergic to peanuts", ...}]

Configuration (YAML settings):
    settings:
      memory:
        backend: mem0
        api_key: "${MEM0_API_KEY}"
        user_id: "{{ state.user_id }}"
        graph: true
"""

import logging
from typing import Any, Callable, Dict, List, Optional, Union

logger = logging.getLogger(__name__)

# Lazy import for Mem0 client
_mem0_client = None


def _get_mem0_client(engine: Any) -> Optional[Any]:
    """
    Get or create Mem0 client from engine settings.

    Args:
        engine: YAMLEngine instance

    Returns:
        Mem0Client instance or None if not configured/available
    """
    global _mem0_client

    # Check if already cached
    if hasattr(engine, "_mem0_client"):
        return engine._mem0_client

    # Import lazily to avoid hard dependency
    try:
        from the_edge_agent.memory.mem0_client import Mem0Client, get_mem0_client
    except ImportError:
        logger.warning("Mem0 client module not available")
        engine._mem0_client = None
        return None

    # Get settings from engine
    settings = getattr(engine, "settings", None) or {}
    memory_settings = settings.get("memory", {})

    # Check if mem0 backend is configured
    backend = memory_settings.get("backend", "").lower()
    if backend != "mem0":
        logger.debug(f"Memory backend is '{backend}', not 'mem0'")
        engine._mem0_client = None
        return None

    # Create client from settings
    client = Mem0Client.from_settings(settings)

    # Check availability
    if not client.is_available():
        logger.warning("Mem0 library not installed. Falling back to native memory.")
        engine._mem0_client = None
        return None

    engine._mem0_client = client
    return client


def _fallback_to_native(action_name: str, engine: Any, **kwargs) -> Dict[str, Any]:
    """
    Fallback to native memory actions when Mem0 is unavailable.

    Args:
        action_name: The mem0 action that failed
        engine: YAMLEngine instance
        **kwargs: Original action parameters

    Returns:
        Result from native memory action or error
    """
    logger.warning(f"Mem0 unavailable for {action_name}, falling back to native memory")

    native_actions = getattr(engine, "actions_registry", {})

    # Map mem0 actions to native equivalents
    if action_name == "memory.mem0.add":
        native_action = native_actions.get("memory.store")
        if native_action:
            # Store messages as JSON
            import json

            messages = kwargs.get("messages", [])
            key = f"mem0_fallback_{kwargs.get('user_id', 'default')}"
            return native_action(
                state=kwargs.get("state", {}),
                key=key,
                value={"messages": messages},
                namespace="mem0_fallback",
            )

    elif action_name == "memory.mem0.search":
        # Native memory doesn't support semantic search
        return {
            "success": False,
            "error": "Semantic search requires Mem0. Install with: pip install mem0ai",
            "results": [],
            "fallback": True,
        }

    elif action_name == "memory.mem0.get_all":
        native_action = native_actions.get("memory.retrieve")
        if native_action:
            key = f"mem0_fallback_{kwargs.get('user_id', 'default')}"
            result = native_action(
                state=kwargs.get("state", {}),
                key=key,
                namespace="mem0_fallback",
            )
            return {
                "success": result.get("found", False),
                "memories": [result.get("value", {})] if result.get("found") else [],
                "fallback": True,
            }

    return {
        "success": False,
        "error": f"Mem0 unavailable and no fallback for {action_name}",
        "fallback": True,
    }


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register Mem0 memory actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing settings and memory backend
    """

    def memory_mem0_add(
        state: Dict[str, Any],
        messages: Union[str, List[Dict[str, str]], Dict[str, str]] = None,
        user_id: Optional[str] = None,
        session_id: Optional[str] = None,
        agent_id: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Store messages with automatic fact extraction using Mem0.

        This action stores conversation messages and automatically extracts
        facts and relationships for later retrieval.

        Args:
            state: Current state dictionary
            messages: Conversation messages in one of these formats:
                - String: Single user message
                - Dict: {"role": "user", "content": "..."}
                - List[Dict]: Full conversation history
            user_id: User scope for the memory
            session_id: Session scope for the memory
            agent_id: Agent scope for the memory
            metadata: Additional metadata to store with memories

        Returns:
            {
                "success": True,
                "memory_id": "...",  # If single memory created
                "memories": [...],   # List of extracted memories
            }
            Or {"success": False, "error": "..."} on failure

        Example YAML:
            - name: store_conversation
              action: memory.mem0.add
              with:
                messages:
                  - role: user
                    content: "{{ state.user_input }}"
                  - role: assistant
                    content: "{{ state.response }}"
                user_id: "{{ state.user_id }}"
        """
        client = _get_mem0_client(engine)
        if client is None:
            return _fallback_to_native(
                "memory.mem0.add",
                engine,
                state=state,
                messages=messages,
                user_id=user_id,
                session_id=session_id,
                **kwargs,
            )

        if messages is None:
            return {
                "success": False,
                "error": "messages parameter is required",
            }

        return client.add(
            messages=messages,
            user_id=user_id,
            session_id=session_id,
            agent_id=agent_id,
            metadata=metadata,
        )

    registry["memory.mem0.add"] = memory_mem0_add
    registry["actions.memory_mem0_add"] = memory_mem0_add

    def memory_mem0_search(
        state: Dict[str, Any],
        query: str = None,
        user_id: Optional[str] = None,
        session_id: Optional[str] = None,
        agent_id: Optional[str] = None,
        limit: int = 5,
        include_relations: bool = False,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Search memories by semantic similarity using Mem0.

        Retrieves relevant memories based on the query string using
        vector similarity search.

        Args:
            state: Current state dictionary
            query: Search query string
            user_id: Filter by user scope
            session_id: Filter by session scope
            agent_id: Filter by agent scope
            limit: Maximum number of results (default: 5)
            include_relations: Include graph relations (requires graph: true)

        Returns:
            {
                "success": True,
                "results": [
                    {
                        "id": "...",
                        "memory": "...",
                        "score": 0.95,
                        "metadata": {...}
                    },
                    ...
                ],
                "relations": [...]  # If include_relations=True
            }
            Or {"success": False, "error": "...", "results": []} on failure

        Example YAML:
            - name: recall_context
              action: memory.mem0.search
              with:
                query: "{{ state.user_question }}"
                user_id: "{{ state.user_id }}"
                limit: 5
        """
        client = _get_mem0_client(engine)
        if client is None:
            return _fallback_to_native(
                "memory.mem0.search",
                engine,
                state=state,
                query=query,
                user_id=user_id,
                **kwargs,
            )

        if query is None:
            return {
                "success": False,
                "error": "query parameter is required",
                "results": [],
            }

        return client.search(
            query=query,
            user_id=user_id,
            session_id=session_id,
            agent_id=agent_id,
            limit=limit,
            include_relations=include_relations,
        )

    registry["memory.mem0.search"] = memory_mem0_search
    registry["actions.memory_mem0_search"] = memory_mem0_search

    def memory_mem0_get_all(
        state: Dict[str, Any],
        user_id: Optional[str] = None,
        session_id: Optional[str] = None,
        agent_id: Optional[str] = None,
        limit: Optional[int] = None,
        offset: int = 0,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Get all memories for a specified scope.

        Retrieves all stored memories for the given user, session, or agent
        with optional pagination.

        Args:
            state: Current state dictionary
            user_id: Filter by user scope
            session_id: Filter by session scope
            agent_id: Filter by agent scope
            limit: Maximum number of results (optional)
            offset: Skip first N results for pagination

        Returns:
            {
                "success": True,
                "memories": [...],
                "total": 42
            }
            Or {"success": False, "error": "...", "memories": []} on failure

        Example YAML:
            - name: get_user_memories
              action: memory.mem0.get_all
              with:
                user_id: "{{ state.user_id }}"
                limit: 20
                offset: 0
        """
        client = _get_mem0_client(engine)
        if client is None:
            return _fallback_to_native(
                "memory.mem0.get_all",
                engine,
                state=state,
                user_id=user_id,
                **kwargs,
            )

        return client.get_all(
            user_id=user_id,
            session_id=session_id,
            agent_id=agent_id,
            limit=limit,
            offset=offset,
        )

    registry["memory.mem0.get_all"] = memory_mem0_get_all
    registry["actions.memory_mem0_get_all"] = memory_mem0_get_all

    def memory_mem0_get(
        state: Dict[str, Any],
        memory_id: str = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Get a specific memory by its ID.

        Args:
            state: Current state dictionary
            memory_id: The memory identifier

        Returns:
            {
                "success": True,
                "memory": {...}
            }
            Or {"success": False, "error": "..."} on failure

        Example YAML:
            - name: get_memory
              action: memory.mem0.get
              with:
                memory_id: "{{ state.memory_id }}"
        """
        client = _get_mem0_client(engine)
        if client is None:
            return {
                "success": False,
                "error": "Mem0 not available",
            }

        if memory_id is None:
            return {
                "success": False,
                "error": "memory_id parameter is required",
            }

        return client.get(memory_id)

    registry["memory.mem0.get"] = memory_mem0_get
    registry["actions.memory_mem0_get"] = memory_mem0_get

    def memory_mem0_update(
        state: Dict[str, Any],
        memory_id: str = None,
        text: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Update an existing memory by ID.

        Supports partial updates - only specified fields are modified.
        Metadata is merged with existing metadata.

        Args:
            state: Current state dictionary
            memory_id: The memory identifier to update
            text: New text content (optional)
            metadata: Metadata to update/merge (optional)

        Returns:
            {
                "success": True,
                "memory": {...}  # Updated memory object
            }
            Or {"success": False, "error": "..."} on failure

        Example YAML:
            - name: update_memory
              action: memory.mem0.update
              with:
                memory_id: "{{ state.memory_id }}"
                metadata:
                  verified: true
                  updated_at: "{{ state.timestamp }}"
        """
        client = _get_mem0_client(engine)
        if client is None:
            return {
                "success": False,
                "error": "Mem0 not available",
            }

        if memory_id is None:
            return {
                "success": False,
                "error": "memory_id parameter is required",
            }

        if text is None and metadata is None:
            return {
                "success": False,
                "error": "At least one of text or metadata is required",
            }

        return client.update(
            memory_id=memory_id,
            text=text,
            metadata=metadata,
        )

    registry["memory.mem0.update"] = memory_mem0_update
    registry["actions.memory_mem0_update"] = memory_mem0_update

    def memory_mem0_delete(
        state: Dict[str, Any],
        memory_id: Optional[str] = None,
        user_id: Optional[str] = None,
        session_id: Optional[str] = None,
        agent_id: Optional[str] = None,
        delete_all: bool = False,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Delete memories by ID or scope.

        Can delete a single memory by ID or bulk delete all memories
        for a user, session, or agent.

        Args:
            state: Current state dictionary
            memory_id: Delete specific memory by ID
            user_id: Delete all memories for user (requires delete_all=True)
            session_id: Delete all memories for session (requires delete_all=True)
            agent_id: Delete all memories for agent (requires delete_all=True)
            delete_all: Must be True for bulk delete operations (safety flag)

        Returns:
            {
                "success": True,
                "deleted_count": 1
            }
            Or {"success": False, "error": "..."} on failure

        Example YAML (single delete):
            - name: delete_memory
              action: memory.mem0.delete
              with:
                memory_id: "{{ state.memory_id }}"

        Example YAML (bulk delete):
            - name: delete_user_memories
              action: memory.mem0.delete
              with:
                user_id: "{{ state.user_id }}"
                delete_all: true
        """
        client = _get_mem0_client(engine)
        if client is None:
            return {
                "success": False,
                "error": "Mem0 not available",
            }

        if memory_id is None and not delete_all:
            return {
                "success": False,
                "error": "memory_id required, or use delete_all=True for bulk delete",
            }

        return client.delete(
            memory_id=memory_id,
            user_id=user_id,
            session_id=session_id,
            agent_id=agent_id,
            delete_all=delete_all,
        )

    registry["memory.mem0.delete"] = memory_mem0_delete
    registry["actions.memory_mem0_delete"] = memory_mem0_delete

    def memory_mem0_test(
        state: Dict[str, Any],
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Test Mem0 connection and configuration.

        Useful for verifying setup before using other Mem0 actions.

        Args:
            state: Current state dictionary

        Returns:
            {
                "success": True,
                "message": "Mem0 connection successful"
            }
            Or {"success": False, "error": "...", "message": "..."} on failure

        Example YAML:
            - name: check_mem0
              action: memory.mem0.test
        """
        client = _get_mem0_client(engine)
        if client is None:
            # Check if mem0 is installed but not configured
            try:
                from the_edge_agent.memory.mem0_client import _check_mem0_available

                if not _check_mem0_available():
                    return {
                        "success": False,
                        "error": "Mem0 library not installed",
                        "message": "Install with: pip install mem0ai",
                    }
                else:
                    return {
                        "success": False,
                        "error": "Mem0 not configured",
                        "message": "Set settings.memory.backend: mem0 in YAML",
                    }
            except ImportError:
                return {
                    "success": False,
                    "error": "Mem0 client module not available",
                    "message": "Internal error",
                }

        return client.test_connection()

    registry["memory.mem0.test"] = memory_mem0_test
    registry["actions.memory_mem0_test"] = memory_mem0_test
