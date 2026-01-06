"""
A2A (Agent-to-Agent) Communication Actions.

This module provides YAML actions for inter-agent communication:
- a2a.send: Send message to a specific agent
- a2a.receive: Receive messages from agents
- a2a.broadcast: Broadcast to all agents in namespace
- a2a.delegate: Request/response pattern with fallback
- a2a.state.get: Get shared state value
- a2a.state.set: Set shared state value
- a2a.discover: Discover available agents

Story: TEA-AGENT-001.5
"""

import copy
import time
import uuid
from typing import Any, Callable, Dict, List, Optional

from ..a2a.message_queue import (
    Message,
    InMemoryMessageQueue,
    MessageTimeoutError,
    get_global_queue,
)
from ..a2a.shared_state import (
    InMemorySharedState,
    OptimisticLockError,
    get_global_state,
)
from ..a2a.discovery import (
    AgentInfo,
    InMemoryAgentDiscovery,
    DiscoveryMode,
    get_global_discovery,
)


def _parse_timeout(timeout_str: Any) -> float:
    """
    Parse timeout string to seconds.

    Supports formats: "30s", "5m", "1h", or numeric seconds.
    """
    if timeout_str is None:
        return 0.0

    if isinstance(timeout_str, (int, float)):
        return float(timeout_str)

    timeout_str = str(timeout_str).strip().lower()

    if timeout_str.endswith("s"):
        return float(timeout_str[:-1])
    elif timeout_str.endswith("m"):
        return float(timeout_str[:-1]) * 60
    elif timeout_str.endswith("h"):
        return float(timeout_str[:-1]) * 3600
    else:
        return float(timeout_str)


def _get_a2a_config(state: Dict[str, Any]) -> Dict[str, Any]:
    """
    Extract A2A configuration from state.

    Looks for settings.a2a or _a2a in state.
    """
    settings = state.get("settings", {})
    a2a_config = settings.get("a2a", {})
    # Also check for runtime A2A config
    if "_a2a" in state:
        a2a_config = {**a2a_config, **state["_a2a"]}
    return a2a_config


def _get_agent_id(state: Dict[str, Any], a2a_config: Dict[str, Any]) -> str:
    """Get current agent's ID from config or state."""
    return a2a_config.get("agent_id") or state.get("_agent_id") or "default-agent"


def _get_namespace(state: Dict[str, Any], a2a_config: Dict[str, Any]) -> str:
    """Get current namespace from config or state."""
    return a2a_config.get("namespace") or state.get("_namespace") or "default"


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register A2A actions with the engine registry.

    Args:
        registry: Action registry dictionary
        engine: YAMLEngine instance
    """

    def a2a_send(
        state: Dict[str, Any],
        to: str,
        message: Dict[str, Any],
        confirm: bool = False,
        correlation_id: Optional[str] = None,
        ttl: Optional[int] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Send a message to a specific agent.

        Args:
            state: Current workflow state
            to: Recipient agent ID
            message: Message with 'type' and 'payload' fields
            confirm: Wait for delivery confirmation
            correlation_id: Optional correlation ID for request/response
            ttl: Time-to-live in seconds

        Returns:
            Dict with 'a2a_message_sent' and optional 'a2a_message_id'

        Example YAML:
            action: a2a.send
            with:
              to: coordinator
              message:
                type: status_update
                payload:
                  progress: "{{ state.progress }}"
        """
        a2a_config = _get_a2a_config(state)
        agent_id = _get_agent_id(state, a2a_config)
        namespace = _get_namespace(state, a2a_config)

        queue = get_global_queue()

        msg_type = message.get("type", "message")
        payload = message.get("payload", {})

        msg = Message(
            from_agent=agent_id,
            to_agent=to,
            namespace=namespace,
            type=msg_type,
            payload=payload,
            correlation_id=correlation_id,
            ttl=ttl,
        )

        msg_id = queue.send(msg, confirm=confirm)

        result = {"a2a_message_sent": True}
        if confirm and msg_id:
            result["a2a_message_id"] = msg_id

        return result

    def a2a_receive(
        state: Dict[str, Any],
        from_agents: Optional[List[str]] = None,
        type: Optional[str] = None,
        timeout: Any = "0s",
        require_all: bool = False,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Receive messages from agents.

        Args:
            state: Current workflow state
            from_agents: Optional list of sender agent IDs to filter
            type: Optional message type filter
            timeout: Timeout string (e.g., "30s", "5m")
            require_all: Wait for messages from ALL specified agents

        Returns:
            Dict with 'a2a_messages' list

        Example YAML:
            action: a2a.receive
            with:
              from: [worker-1, worker-2]
              type: task_result
              timeout: 30s
              require_all: true
        """
        a2a_config = _get_a2a_config(state)
        agent_id = _get_agent_id(state, a2a_config)
        namespace = _get_namespace(state, a2a_config)

        queue = get_global_queue()
        timeout_seconds = _parse_timeout(timeout)

        # Handle 'from' alias (reserved keyword in Python)
        from_list = from_agents or kwargs.get("from")
        if isinstance(from_list, str):
            from_list = [from_list]

        try:
            messages = queue.receive(
                agent_id=agent_id,
                namespace=namespace,
                from_agents=from_list,
                message_type=type,
                timeout=timeout_seconds,
                require_all=require_all,
            )
            return {
                "a2a_messages": [msg.to_dict() for msg in messages],
            }
        except MessageTimeoutError as e:
            return {
                "a2a_messages": [],
                "a2a_timeout": True,
                "a2a_error": str(e),
            }

    def a2a_broadcast(
        state: Dict[str, Any],
        message: Dict[str, Any],
        agent_type_filter: Optional[str] = None,
        ttl: Optional[int] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Broadcast message to all agents in namespace.

        Args:
            state: Current workflow state
            message: Message with 'type' and 'payload' fields
            agent_type_filter: Optional filter by agent type
            ttl: Time-to-live in seconds

        Returns:
            Dict with 'a2a_broadcast_count'

        Example YAML:
            action: a2a.broadcast
            with:
              message:
                type: announcement
                payload:
                  text: "All hands meeting at 3pm"
              agent_type_filter: worker
        """
        a2a_config = _get_a2a_config(state)
        agent_id = _get_agent_id(state, a2a_config)
        namespace = _get_namespace(state, a2a_config)

        queue = get_global_queue()

        msg_type = message.get("type", "broadcast")
        payload = message.get("payload", {})

        msg = Message(
            from_agent=agent_id,
            to_agent="*",
            namespace=namespace,
            type=msg_type,
            payload=payload,
            ttl=ttl,
        )

        count = queue.broadcast(msg, agent_type_filter=agent_type_filter)

        return {"a2a_broadcast_count": count}

    def a2a_delegate(
        state: Dict[str, Any],
        to: str,
        task: Dict[str, Any],
        timeout: Any = "60s",
        on_timeout: str = "raise",
        fallback: Optional[Dict[str, Any]] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Delegate a task to another agent and wait for response.

        Args:
            state: Current workflow state
            to: Delegate agent ID
            task: Task with 'type' and other fields
            timeout: Timeout string (e.g., "60s")
            on_timeout: Strategy: 'raise', 'fallback_local', 'retry'
            fallback: Fallback action configuration (for fallback_local)

        Returns:
            Dict with 'a2a_delegation_result'

        Example YAML:
            action: a2a.delegate
            with:
              to: search-specialist
              task:
                type: search
                query: "{{ state.query }}"
              timeout: 60s
              on_timeout: fallback_local
              fallback:
                action: web.search
                with:
                  query: "{{ state.query }}"
        """
        a2a_config = _get_a2a_config(state)
        agent_id = _get_agent_id(state, a2a_config)
        namespace = _get_namespace(state, a2a_config)

        queue = get_global_queue()
        timeout_seconds = _parse_timeout(timeout)

        # Generate correlation ID for matching response
        correlation_id = f"req_{uuid.uuid4().hex[:12]}"

        task_type = task.get("type", "task")
        task_payload = {k: v for k, v in task.items() if k != "type"}

        # Send delegation request
        msg = Message(
            from_agent=agent_id,
            to_agent=to,
            namespace=namespace,
            type=task_type,
            payload=task_payload,
            correlation_id=correlation_id,
        )
        queue.send(msg)

        # Wait for response with matching correlation_id
        start_time = time.time()
        while True:
            messages = queue.receive(
                agent_id=agent_id,
                namespace=namespace,
                from_agents=[to],
                timeout=min(0.1, timeout_seconds),
            )

            for msg in messages:
                if msg.correlation_id == correlation_id:
                    return {
                        "a2a_delegation_result": msg.payload,
                        "a2a_delegation_success": True,
                    }

            elapsed = time.time() - start_time
            if elapsed >= timeout_seconds:
                break

        # Timeout handling
        if on_timeout == "raise":
            return {
                "a2a_delegation_result": None,
                "a2a_delegation_success": False,
                "a2a_delegation_timeout": True,
                "a2a_error": f"Delegation to {to} timed out after {timeout_seconds}s",
            }
        elif on_timeout == "fallback_local":
            if fallback and engine:
                # Execute fallback action
                fallback_action = fallback.get("action")
                fallback_params = fallback.get("with", {})
                if fallback_action and fallback_action in registry:
                    fallback_result = registry[fallback_action](
                        state, **fallback_params
                    )
                    return {
                        "a2a_delegation_result": fallback_result,
                        "a2a_delegation_success": False,
                        "a2a_delegation_fallback": True,
                    }
            return {
                "a2a_delegation_result": None,
                "a2a_delegation_success": False,
                "a2a_delegation_timeout": True,
                "a2a_error": "Delegation timed out and no fallback available",
            }
        elif on_timeout == "retry":
            # For retry, we return timeout and let the workflow handle retry
            return {
                "a2a_delegation_result": None,
                "a2a_delegation_success": False,
                "a2a_delegation_timeout": True,
                "a2a_should_retry": True,
            }
        else:
            return {
                "a2a_delegation_result": None,
                "a2a_delegation_success": False,
                "a2a_delegation_timeout": True,
                "a2a_error": f"Unknown on_timeout strategy: {on_timeout}",
            }

    def a2a_state_get(
        state: Dict[str, Any],
        key: str,
        default: Any = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Get a value from shared state.

        Args:
            state: Current workflow state
            key: State key
            default: Default value if key doesn't exist

        Returns:
            Dict with 'a2a_shared_state' and 'a2a_state_version'

        Example YAML:
            action: a2a.state.get
            with:
              key: team_progress
              default: {completed: 0, total: 0}
        """
        a2a_config = _get_a2a_config(state)
        namespace = _get_namespace(state, a2a_config)

        shared_state = get_global_state()
        value, version = shared_state.get(namespace, key, default=default)

        return {
            "a2a_shared_state": value,
            "a2a_state_version": version,
        }

    def a2a_state_set(
        state: Dict[str, Any],
        key: str,
        value: Any,
        ttl: Optional[int] = None,
        expected_version: Optional[int] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Set a value in shared state.

        Args:
            state: Current workflow state
            key: State key
            value: Value to store
            ttl: Time-to-live in seconds
            expected_version: Expected version for optimistic locking

        Returns:
            Dict with 'a2a_state_version'

        Example YAML:
            action: a2a.state.set
            with:
              key: team_progress
              value:
                completed: "{{ state.completed_tasks }}"
                total: "{{ state.total_tasks }}"
              ttl: 3600
        """
        a2a_config = _get_a2a_config(state)
        namespace = _get_namespace(state, a2a_config)

        shared_state = get_global_state()

        try:
            new_version = shared_state.set(
                namespace=namespace,
                key=key,
                value=value,
                ttl=ttl,
                expected_version=expected_version,
            )
            return {
                "a2a_state_version": new_version,
                "a2a_state_updated": True,
            }
        except OptimisticLockError as e:
            return {
                "a2a_state_updated": False,
                "a2a_state_conflict": True,
                "a2a_error": str(e),
                "a2a_state_expected_version": e.expected_version,
                "a2a_state_actual_version": e.actual_version,
            }

    def a2a_discover(
        state: Dict[str, Any],
        capability: Optional[str] = None,
        agent_type: Optional[str] = None,
        status: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Discover available agents in namespace.

        Args:
            state: Current workflow state
            capability: Optional capability filter
            agent_type: Optional type filter
            status: Optional status filter

        Returns:
            Dict with 'a2a_agents' list

        Example YAML:
            action: a2a.discover
            with:
              capability: summarize
        """
        a2a_config = _get_a2a_config(state)
        namespace = _get_namespace(state, a2a_config)

        discovery = get_global_discovery()
        agents = discovery.discover(
            namespace=namespace,
            capability=capability,
            agent_type=agent_type,
            status=status,
        )

        return {
            "a2a_agents": [agent.to_dict() for agent in agents],
        }

    def a2a_register(
        state: Dict[str, Any],
        agent_id: Optional[str] = None,
        capabilities: Optional[List[str]] = None,
        agent_type: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Register current agent for discovery and broadcasts.

        Args:
            state: Current workflow state
            agent_id: Optional override for agent ID
            capabilities: List of capabilities
            agent_type: Agent type for categorization
            metadata: Additional metadata

        Returns:
            Dict with 'a2a_registered' and agent info

        Example YAML:
            action: a2a.register
            with:
              capabilities: [search, summarize]
              agent_type: worker
        """
        a2a_config = _get_a2a_config(state)
        aid = agent_id or _get_agent_id(state, a2a_config)
        namespace = _get_namespace(state, a2a_config)

        # Register with discovery
        discovery = get_global_discovery()
        agent_info = discovery.register(
            agent_id=aid,
            namespace=namespace,
            capabilities=capabilities,
            agent_type=agent_type,
            metadata=metadata,
        )

        # Register with message queue for broadcasts
        queue = get_global_queue()
        queue.register_agent(aid, namespace, agent_type)

        return {
            "a2a_registered": True,
            "a2a_agent_info": agent_info.to_dict(),
            # Update state with agent info
            "_agent_id": aid,
            "_namespace": namespace,
        }

    def a2a_unregister(
        state: Dict[str, Any],
        agent_id: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Unregister current agent.

        Args:
            state: Current workflow state
            agent_id: Optional override for agent ID

        Returns:
            Dict with 'a2a_unregistered'
        """
        a2a_config = _get_a2a_config(state)
        aid = agent_id or _get_agent_id(state, a2a_config)
        namespace = _get_namespace(state, a2a_config)

        # Unregister from discovery
        discovery = get_global_discovery()
        discovery.unregister(aid, namespace)

        # Unregister from message queue
        queue = get_global_queue()
        queue.unregister_agent(aid, namespace)

        return {"a2a_unregistered": True}

    def a2a_heartbeat(
        state: Dict[str, Any],
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Send heartbeat to update last_seen timestamp.

        Returns:
            Dict with 'a2a_heartbeat' timestamp
        """
        a2a_config = _get_a2a_config(state)
        agent_id = _get_agent_id(state, a2a_config)
        namespace = _get_namespace(state, a2a_config)

        discovery = get_global_discovery()
        agent_info = discovery.heartbeat(agent_id, namespace)

        if agent_info:
            return {
                "a2a_heartbeat": agent_info.last_seen,
                "a2a_status": agent_info.status,
            }
        return {
            "a2a_heartbeat": None,
            "a2a_error": "Agent not registered",
        }

    # Register all actions
    registry["a2a.send"] = a2a_send
    registry["a2a.receive"] = a2a_receive
    registry["a2a.broadcast"] = a2a_broadcast
    registry["a2a.delegate"] = a2a_delegate
    registry["a2a.state.get"] = a2a_state_get
    registry["a2a.state.set"] = a2a_state_set
    registry["a2a.discover"] = a2a_discover
    registry["a2a.register"] = a2a_register
    registry["a2a.unregister"] = a2a_unregister
    registry["a2a.heartbeat"] = a2a_heartbeat

    # Also register with underscore aliases
    registry["actions.a2a_send"] = a2a_send
    registry["actions.a2a_receive"] = a2a_receive
    registry["actions.a2a_broadcast"] = a2a_broadcast
    registry["actions.a2a_delegate"] = a2a_delegate
    registry["actions.a2a_state_get"] = a2a_state_get
    registry["actions.a2a_state_set"] = a2a_state_set
    registry["actions.a2a_discover"] = a2a_discover
    registry["actions.a2a_register"] = a2a_register
    registry["actions.a2a_unregister"] = a2a_unregister
    registry["actions.a2a_heartbeat"] = a2a_heartbeat
