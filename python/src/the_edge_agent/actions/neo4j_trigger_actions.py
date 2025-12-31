"""
Neo4j APOC Trigger Actions for YAMLEngine (TEA-BUILTIN-001.7.5).

This module provides YAML-accessible actions for managing APOC database triggers.
Triggers allow reactive programming patterns where graph changes automatically
fire Cypher queries, HTTP webhooks, or state updates.

Actions:
    - neo4j.check_apoc: Check if APOC library is available
    - neo4j.check_triggers: Check if APOC triggers are enabled
    - neo4j.register_trigger: Register a database trigger
    - neo4j.unregister_trigger: Remove a trigger
    - neo4j.list_triggers: List all registered triggers
    - neo4j.pause_trigger: Temporarily disable a trigger
    - neo4j.resume_trigger: Re-enable a paused trigger
    - neo4j.register_callback: Register a webhook trigger
    - neo4j.register_state_update: Register a state update trigger
    - neo4j.cleanup_triggers: Remove triggers by prefix

Requires:
    - Neo4j with APOC plugin installed
    - apoc.trigger.enabled=true in neo4j.conf

Example:
    >>> # Check APOC availability
    >>> result = registry['neo4j.check_apoc'](state={})
    >>> print(result['available'])  # True

    >>> # Register a trigger
    >>> result = registry['neo4j.register_trigger'](
    ...     state={},
    ...     name="log_new_persons",
    ...     query="UNWIND $createdNodes AS n WITH n WHERE n:Person CREATE (:Log {msg: 'New person: ' + n.name})",
    ...     config={"phase": "after"}
    ... )
    >>> print(result['success'])  # True
"""

from typing import Any, Callable, Dict


def _neo4j_not_available_error() -> Dict[str, Any]:
    """Return standard error for missing Neo4j backend."""
    return {
        "success": False,
        "error": "Neo4j backend not configured. Use enable_graph=True with neo4j:// URI",
        "error_type": "configuration_error",
    }


def _not_neo4j_backend_error() -> Dict[str, Any]:
    """Return error when graph backend is not Neo4j."""
    return {
        "success": False,
        "error": "Graph backend is not Neo4j. Trigger actions require Neo4jBackend.",
        "error_type": "configuration_error",
    }


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register Neo4j APOC trigger actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing graph backend
    """

    def _get_neo4j_backend():
        """Get Neo4jBackend from engine or return None."""
        from the_edge_agent.memory.graph import Neo4jBackend

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return None

        if not isinstance(engine._graph_backend, Neo4jBackend):
            return None

        return engine._graph_backend

    # =========================================================================
    # APOC DETECTION (AC 1-4)
    # =========================================================================

    def neo4j_check_apoc(state, **kwargs):
        """
        Check if APOC library is installed and available.

        Args:
            state: Current state dictionary

        Returns:
            {"success": True, "available": bool, "version": str or None}
        """
        backend = _get_neo4j_backend()
        if backend is None:
            return _neo4j_not_available_error()

        return backend.check_apoc_available()

    registry["neo4j.check_apoc"] = neo4j_check_apoc
    registry["actions.neo4j_check_apoc"] = neo4j_check_apoc

    def neo4j_get_apoc_version(state, **kwargs):
        """
        Get the installed APOC library version.

        Args:
            state: Current state dictionary

        Returns:
            {"success": True, "version": str}
            or {"success": False, "error": str, "error_type": str}
        """
        backend = _get_neo4j_backend()
        if backend is None:
            return _neo4j_not_available_error()

        return backend.get_apoc_version()

    registry["neo4j.get_apoc_version"] = neo4j_get_apoc_version
    registry["actions.neo4j_get_apoc_version"] = neo4j_get_apoc_version

    def neo4j_check_triggers(state, **kwargs):
        """
        Check if APOC triggers are enabled in Neo4j configuration.

        Args:
            state: Current state dictionary

        Returns:
            {"success": True, "enabled": bool, "refresh_interval": int or None}
        """
        backend = _get_neo4j_backend()
        if backend is None:
            return _neo4j_not_available_error()

        return backend.check_triggers_enabled()

    registry["neo4j.check_triggers"] = neo4j_check_triggers
    registry["actions.neo4j_check_triggers"] = neo4j_check_triggers

    # =========================================================================
    # TRIGGER REGISTRATION (AC 5-10)
    # =========================================================================

    def neo4j_register_trigger(
        state, name, query, selector=None, config=None, **kwargs
    ):
        """
        Register a database trigger using APOC.

        Args:
            state: Current state dictionary
            name: Unique trigger identifier
            query: Cypher query to execute when trigger fires
            selector: What changes to watch (optional, see docs for options)
            config: Trigger configuration:
                - phase: "before" or "after" (default: "after")
                - params: Additional parameters passed to trigger query

        Supported selectors:
            - createdNodes: New nodes created
            - createdRelationships: New relationships created
            - deletedNodes: Nodes deleted
            - deletedRelationships: Relationships deleted
            - assignedLabels: Labels added to nodes
            - removedLabels: Labels removed from nodes
            - assignedNodeProperties: Node properties set
            - assignedRelationshipProperties: Relationship properties set

        Returns:
            {"success": True, "trigger_name": str, "registered": True}
            or {"success": False, "error": str, "error_type": str}
        """
        backend = _get_neo4j_backend()
        if backend is None:
            return _neo4j_not_available_error()

        if not name:
            return {
                "success": False,
                "error": "Trigger name is required",
                "error_type": "validation_error",
            }

        if not query:
            return {
                "success": False,
                "error": "Trigger query is required",
                "error_type": "validation_error",
            }

        return backend.register_trigger(
            name=str(name), query=str(query), selector=selector, config=config
        )

    registry["neo4j.register_trigger"] = neo4j_register_trigger
    registry["actions.neo4j_register_trigger"] = neo4j_register_trigger

    def neo4j_unregister_trigger(state, name, **kwargs):
        """
        Remove a registered trigger.

        Args:
            state: Current state dictionary
            name: Trigger name to remove

        Returns:
            {"success": True, "trigger_name": str, "removed": True}
            or {"success": False, "error": str, "error_type": str}
        """
        backend = _get_neo4j_backend()
        if backend is None:
            return _neo4j_not_available_error()

        if not name:
            return {
                "success": False,
                "error": "Trigger name is required",
                "error_type": "validation_error",
            }

        return backend.unregister_trigger(str(name))

    registry["neo4j.unregister_trigger"] = neo4j_unregister_trigger
    registry["actions.neo4j_unregister_trigger"] = neo4j_unregister_trigger

    def neo4j_list_triggers(state, **kwargs):
        """
        List all registered triggers.

        Args:
            state: Current state dictionary

        Returns:
            {
                "success": True,
                "triggers": [
                    {
                        "name": str,
                        "query": str,
                        "selector": dict,
                        "params": dict,
                        "installed": bool,
                        "paused": bool
                    },
                    ...
                ],
                "count": int
            }
        """
        backend = _get_neo4j_backend()
        if backend is None:
            return _neo4j_not_available_error()

        return backend.list_triggers()

    registry["neo4j.list_triggers"] = neo4j_list_triggers
    registry["actions.neo4j_list_triggers"] = neo4j_list_triggers

    def neo4j_pause_trigger(state, name, **kwargs):
        """
        Temporarily disable a trigger without removing it.

        Args:
            state: Current state dictionary
            name: Trigger name to pause

        Returns:
            {"success": True, "trigger_name": str, "paused": True}
            or {"success": False, "error": str, "error_type": str}
        """
        backend = _get_neo4j_backend()
        if backend is None:
            return _neo4j_not_available_error()

        if not name:
            return {
                "success": False,
                "error": "Trigger name is required",
                "error_type": "validation_error",
            }

        return backend.pause_trigger(str(name))

    registry["neo4j.pause_trigger"] = neo4j_pause_trigger
    registry["actions.neo4j_pause_trigger"] = neo4j_pause_trigger

    def neo4j_resume_trigger(state, name, **kwargs):
        """
        Re-enable a paused trigger.

        Args:
            state: Current state dictionary
            name: Trigger name to resume

        Returns:
            {"success": True, "trigger_name": str, "paused": False}
            or {"success": False, "error": str, "error_type": str}
        """
        backend = _get_neo4j_backend()
        if backend is None:
            return _neo4j_not_available_error()

        if not name:
            return {
                "success": False,
                "error": "Trigger name is required",
                "error_type": "validation_error",
            }

        return backend.resume_trigger(str(name))

    registry["neo4j.resume_trigger"] = neo4j_resume_trigger
    registry["actions.neo4j_resume_trigger"] = neo4j_resume_trigger

    # =========================================================================
    # CALLBACK MECHANISMS (AC 13-14)
    # =========================================================================

    def neo4j_register_callback(state, name, callback_url, config=None, **kwargs):
        """
        Register a trigger that fires an HTTP webhook on graph changes.

        The trigger will POST to the callback_url when changes matching
        the selector occur.

        Args:
            state: Current state dictionary
            name: Unique trigger identifier
            callback_url: HTTP endpoint to POST to when trigger fires
            config: Trigger configuration:
                - selector: What changes to watch (default: createdNodes)
                - phase: "before" or "after" (default: "after")
                - headers: Optional HTTP headers
                - label_filter: Optional label to filter nodes

        Returns:
            {"success": True, "trigger_name": str, "registered": True, "callback_url": str}
            or {"success": False, "error": str, "error_type": str}
        """
        backend = _get_neo4j_backend()
        if backend is None:
            return _neo4j_not_available_error()

        if not name:
            return {
                "success": False,
                "error": "Trigger name is required",
                "error_type": "validation_error",
            }

        if not callback_url:
            return {
                "success": False,
                "error": "Callback URL is required",
                "error_type": "validation_error",
            }

        return backend.register_trigger_callback(
            name=str(name), callback_url=str(callback_url), config=config
        )

    registry["neo4j.register_callback"] = neo4j_register_callback
    registry["actions.neo4j_register_callback"] = neo4j_register_callback

    def neo4j_register_state_update(
        state, name, state_key, transform=None, config=None, **kwargs
    ):
        """
        Register a trigger that writes to a state node for agent consumption.

        Creates/updates a TriggerStateLog node that agents can query for
        triggered events. This provides an in-database event queue pattern.

        Args:
            state: Current state dictionary
            name: Unique trigger identifier
            state_key: Key to use in the TriggerStateLog node
            transform: Optional Cypher expression to transform the data
            config: Trigger configuration:
                - selector: What changes to watch (default: createdNodes)
                - phase: "before" or "after" (default: "after")
                - label_filter: Optional label to filter nodes

        Returns:
            {"success": True, "trigger_name": str, "registered": True, "state_key": str}
            or {"success": False, "error": str, "error_type": str}
        """
        backend = _get_neo4j_backend()
        if backend is None:
            return _neo4j_not_available_error()

        if not name:
            return {
                "success": False,
                "error": "Trigger name is required",
                "error_type": "validation_error",
            }

        if not state_key:
            return {
                "success": False,
                "error": "State key is required",
                "error_type": "validation_error",
            }

        return backend.register_trigger_state_update(
            name=str(name), state_key=str(state_key), transform=transform, config=config
        )

    registry["neo4j.register_state_update"] = neo4j_register_state_update
    registry["actions.neo4j_register_state_update"] = neo4j_register_state_update

    # =========================================================================
    # LIFECYCLE MANAGEMENT (AC 15-17)
    # =========================================================================

    def neo4j_cleanup_triggers(state, prefix=None, **kwargs):
        """
        Remove triggers by prefix, used for session/agent cleanup.

        Args:
            state: Current state dictionary
            prefix: If provided, only remove triggers with names starting
                   with this prefix. If None, removes all triggers (use with caution).

        Returns:
            {
                "success": True,
                "removed": list of trigger names removed,
                "count": int
            }
        """
        backend = _get_neo4j_backend()
        if backend is None:
            return _neo4j_not_available_error()

        return backend.cleanup_triggers(prefix=prefix)

    registry["neo4j.cleanup_triggers"] = neo4j_cleanup_triggers
    registry["actions.neo4j_cleanup_triggers"] = neo4j_cleanup_triggers
