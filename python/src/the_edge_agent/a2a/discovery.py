"""
Agent Discovery for A2A Communication.

This module provides agent discovery and capability advertisement
for inter-agent communication within TEA workflows.

Features:
- Agent registration with capabilities
- Discovery modes: broadcast, registry, static
- Capability-based agent filtering
- Namespace isolation

Story: TEA-AGENT-001.5 (AC: 7)
"""

import threading
import time
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Dict, List, Optional, Set


class DiscoveryMode(str, Enum):
    """
    Discovery mode for finding agents.

    - BROADCAST: Discover via broadcast messages (dynamic)
    - REGISTRY: Central registry of agents (managed)
    - STATIC: Pre-configured agent list (fixed)
    """

    BROADCAST = "broadcast"
    REGISTRY = "registry"
    STATIC = "static"


@dataclass
class AgentInfo:
    """
    Information about a registered agent.

    Attributes:
        agent_id: Unique agent identifier within namespace
        namespace: Agent's namespace
        capabilities: List of capability strings
        agent_type: Optional agent type for categorization
        status: Agent status (online, offline, busy)
        metadata: Additional agent metadata
        registered_at: Registration timestamp
        last_seen: Last activity timestamp
    """

    agent_id: str
    namespace: str
    capabilities: List[str] = field(default_factory=list)
    agent_type: Optional[str] = None
    status: str = "online"
    metadata: Dict[str, Any] = field(default_factory=dict)
    registered_at: float = field(default_factory=time.time)
    last_seen: float = field(default_factory=time.time)

    def has_capability(self, capability: str) -> bool:
        """Check if agent has a specific capability."""
        return capability in self.capabilities

    def update_last_seen(self) -> None:
        """Update last seen timestamp to now."""
        self.last_seen = time.time()

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        return {
            "agent_id": self.agent_id,
            "namespace": self.namespace,
            "capabilities": self.capabilities,
            "agent_type": self.agent_type,
            "status": self.status,
            "metadata": self.metadata,
            "registered_at": self.registered_at,
            "last_seen": self.last_seen,
        }


class AgentDiscovery:
    """
    Protocol for agent discovery backends.
    """

    def register(
        self,
        agent_id: str,
        namespace: str,
        capabilities: Optional[List[str]] = None,
        agent_type: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> AgentInfo:
        """
        Register an agent for discovery.

        Args:
            agent_id: Unique agent identifier
            namespace: Agent's namespace
            capabilities: List of capability strings
            agent_type: Optional agent type
            metadata: Additional metadata

        Returns:
            AgentInfo for the registered agent
        """
        ...

    def unregister(self, agent_id: str, namespace: str) -> bool:
        """
        Unregister an agent.

        Args:
            agent_id: Agent identifier
            namespace: Agent's namespace

        Returns:
            True if agent was registered and removed
        """
        ...

    def discover(
        self,
        namespace: str,
        capability: Optional[str] = None,
        agent_type: Optional[str] = None,
        status: Optional[str] = None,
    ) -> List[AgentInfo]:
        """
        Discover agents in namespace.

        Args:
            namespace: Namespace to search
            capability: Optional capability filter
            agent_type: Optional type filter
            status: Optional status filter

        Returns:
            List of matching AgentInfo objects
        """
        ...

    def update_status(
        self, agent_id: str, namespace: str, status: str
    ) -> Optional[AgentInfo]:
        """
        Update an agent's status.

        Args:
            agent_id: Agent identifier
            namespace: Agent's namespace
            status: New status

        Returns:
            Updated AgentInfo or None if not found
        """
        ...

    def heartbeat(self, agent_id: str, namespace: str) -> Optional[AgentInfo]:
        """
        Update agent's last_seen timestamp.

        Args:
            agent_id: Agent identifier
            namespace: Agent's namespace

        Returns:
            Updated AgentInfo or None if not found
        """
        ...


class InMemoryAgentDiscovery:
    """
    In-memory agent discovery implementation.

    Thread-safe implementation for single-process workflows.
    """

    def __init__(self, stale_threshold: float = 60.0):
        """
        Initialize in-memory discovery.

        Args:
            stale_threshold: Seconds after which an agent without
                           heartbeat is considered stale
        """
        self._lock = threading.RLock()
        # Agents stored as: {namespace: {agent_id: AgentInfo}}
        self._agents: Dict[str, Dict[str, AgentInfo]] = {}
        self._stale_threshold = stale_threshold

    def register(
        self,
        agent_id: str,
        namespace: str,
        capabilities: Optional[List[str]] = None,
        agent_type: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> AgentInfo:
        """
        Register an agent for discovery.

        Args:
            agent_id: Unique agent identifier
            namespace: Agent's namespace
            capabilities: List of capability strings
            agent_type: Optional agent type
            metadata: Additional metadata

        Returns:
            AgentInfo for the registered agent
        """
        with self._lock:
            if namespace not in self._agents:
                self._agents[namespace] = {}

            agent = AgentInfo(
                agent_id=agent_id,
                namespace=namespace,
                capabilities=capabilities or [],
                agent_type=agent_type,
                metadata=metadata or {},
            )
            self._agents[namespace][agent_id] = agent
            return agent

    def unregister(self, agent_id: str, namespace: str) -> bool:
        """
        Unregister an agent.

        Args:
            agent_id: Agent identifier
            namespace: Agent's namespace

        Returns:
            True if agent was registered and removed
        """
        with self._lock:
            if namespace in self._agents:
                if agent_id in self._agents[namespace]:
                    del self._agents[namespace][agent_id]
                    return True
            return False

    def discover(
        self,
        namespace: str,
        capability: Optional[str] = None,
        agent_type: Optional[str] = None,
        status: Optional[str] = None,
        include_stale: bool = False,
    ) -> List[AgentInfo]:
        """
        Discover agents in namespace.

        Args:
            namespace: Namespace to search
            capability: Optional capability filter
            agent_type: Optional type filter
            status: Optional status filter
            include_stale: Include agents that haven't sent heartbeat

        Returns:
            List of matching AgentInfo objects
        """
        with self._lock:
            ns_agents = self._agents.get(namespace, {})
            results = []

            current_time = time.time()
            for agent in ns_agents.values():
                # Check stale threshold
                if not include_stale:
                    age = current_time - agent.last_seen
                    if age > self._stale_threshold:
                        continue

                # Filter by capability
                if capability and not agent.has_capability(capability):
                    continue

                # Filter by type
                if agent_type and agent.agent_type != agent_type:
                    continue

                # Filter by status
                if status and agent.status != status:
                    continue

                results.append(agent)

            return results

    def update_status(
        self, agent_id: str, namespace: str, status: str
    ) -> Optional[AgentInfo]:
        """
        Update an agent's status.

        Args:
            agent_id: Agent identifier
            namespace: Agent's namespace
            status: New status

        Returns:
            Updated AgentInfo or None if not found
        """
        with self._lock:
            if namespace in self._agents:
                agent = self._agents[namespace].get(agent_id)
                if agent:
                    agent.status = status
                    agent.update_last_seen()
                    return agent
            return None

    def heartbeat(self, agent_id: str, namespace: str) -> Optional[AgentInfo]:
        """
        Update agent's last_seen timestamp.

        Args:
            agent_id: Agent identifier
            namespace: Agent's namespace

        Returns:
            Updated AgentInfo or None if not found
        """
        with self._lock:
            if namespace in self._agents:
                agent = self._agents[namespace].get(agent_id)
                if agent:
                    agent.update_last_seen()
                    return agent
            return None

    def get_agent(self, agent_id: str, namespace: str) -> Optional[AgentInfo]:
        """
        Get a specific agent's info.

        Args:
            agent_id: Agent identifier
            namespace: Agent's namespace

        Returns:
            AgentInfo or None if not found
        """
        with self._lock:
            if namespace in self._agents:
                return self._agents[namespace].get(agent_id)
            return None

    def clear_namespace(self, namespace: str) -> int:
        """
        Clear all agents in a namespace.

        Args:
            namespace: Namespace to clear

        Returns:
            Number of agents cleared
        """
        with self._lock:
            if namespace in self._agents:
                count = len(self._agents[namespace])
                del self._agents[namespace]
                return count
            return 0

    def cleanup_stale(self, namespace: Optional[str] = None) -> int:
        """
        Remove stale agents that haven't sent heartbeat.

        Args:
            namespace: Optional namespace (None = all)

        Returns:
            Number of agents removed
        """
        count = 0
        current_time = time.time()
        with self._lock:
            namespaces = [namespace] if namespace else list(self._agents.keys())
            for ns in namespaces:
                if ns not in self._agents:
                    continue
                stale_ids = [
                    agent_id
                    for agent_id, agent in self._agents[ns].items()
                    if current_time - agent.last_seen > self._stale_threshold
                ]
                for agent_id in stale_ids:
                    del self._agents[ns][agent_id]
                    count += 1
        return count

    def list_namespaces(self) -> List[str]:
        """
        List all namespaces with registered agents.

        Returns:
            List of namespace names
        """
        with self._lock:
            return list(self._agents.keys())


# Global singleton for in-process discovery
_global_discovery: Optional[InMemoryAgentDiscovery] = None
_global_discovery_lock = threading.Lock()


def get_global_discovery() -> InMemoryAgentDiscovery:
    """
    Get the global in-memory discovery singleton.

    Thread-safe lazy initialization.
    """
    global _global_discovery
    if _global_discovery is None:
        with _global_discovery_lock:
            if _global_discovery is None:
                _global_discovery = InMemoryAgentDiscovery()
    return _global_discovery


def reset_global_discovery() -> None:
    """
    Reset the global discovery.

    Useful for testing to ensure clean state.
    """
    global _global_discovery
    with _global_discovery_lock:
        _global_discovery = None
