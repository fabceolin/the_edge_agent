"""
Message Queue Abstraction for A2A Communication.

This module provides the message queue infrastructure for inter-agent
communication within TEA workflows.

Features:
- Abstract MessageQueue protocol for pluggable backends
- In-memory implementation for single-process workflows
- Message serialization to JSON
- TTL-based message expiration
- Namespace isolation

Story: TEA-AGENT-001.5 (AC: 5)
"""

import json
import threading
import time
import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass, field, asdict
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Protocol, runtime_checkable


class MessageQueueError(Exception):
    """Base exception for message queue errors."""

    pass


class MessageDeliveryError(MessageQueueError):
    """Raised when message delivery fails."""

    pass


class MessageTimeoutError(MessageQueueError):
    """Raised when message receive times out."""

    pass


@dataclass
class Message:
    """
    Message structure for A2A communication.

    Attributes:
        id: Unique message identifier (auto-generated if not provided)
        correlation_id: Optional ID for request/response correlation
        from_agent: Sender agent ID
        to_agent: Recipient agent ID (or "*" for broadcast)
        namespace: Namespace for isolation (prevents cross-workflow interference)
        type: Message type for filtering
        payload: Message payload (any JSON-serializable data)
        timestamp: ISO 8601 timestamp (auto-generated if not provided)
        ttl: Time-to-live in seconds (None = no expiration)
    """

    from_agent: str
    to_agent: str
    namespace: str
    type: str
    payload: Any
    id: str = field(default_factory=lambda: f"msg_{uuid.uuid4().hex[:12]}")
    correlation_id: Optional[str] = None
    timestamp: str = field(
        default_factory=lambda: datetime.now(timezone.utc)
        .isoformat()
        .replace("+00:00", "Z")
    )
    ttl: Optional[int] = None

    def to_dict(self) -> Dict[str, Any]:
        """Convert message to dictionary."""
        return asdict(self)

    def to_json(self) -> str:
        """Serialize message to JSON."""
        return json.dumps(self.to_dict())

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Message":
        """Create message from dictionary."""
        return cls(**data)

    @classmethod
    def from_json(cls, json_str: str) -> "Message":
        """Deserialize message from JSON."""
        return cls.from_dict(json.loads(json_str))

    def is_expired(self) -> bool:
        """Check if message has expired based on TTL."""
        if self.ttl is None:
            return False
        try:
            # Parse timestamp
            ts_str = self.timestamp.rstrip("Z")
            msg_time = datetime.fromisoformat(ts_str)
            now = datetime.now(timezone.utc).replace(tzinfo=None)
            elapsed = (now - msg_time).total_seconds()
            return elapsed > self.ttl
        except (ValueError, TypeError):
            return False


@runtime_checkable
class MessageQueue(Protocol):
    """
    Protocol for message queue backends.

    Implementations must be thread-safe for concurrent access.
    """

    def send(self, message: Message, confirm: bool = False) -> Optional[str]:
        """
        Send a message to the queue.

        Args:
            message: Message to send
            confirm: If True, wait for delivery confirmation

        Returns:
            Message ID if confirm=True, None otherwise

        Raises:
            MessageDeliveryError: If delivery fails (when confirm=True)
        """
        ...

    def receive(
        self,
        agent_id: str,
        namespace: str,
        from_agents: Optional[List[str]] = None,
        message_type: Optional[str] = None,
        timeout: float = 0.0,
        require_all: bool = False,
    ) -> List[Message]:
        """
        Receive messages from the queue.

        Args:
            agent_id: Receiving agent's ID
            namespace: Namespace to receive from
            from_agents: Optional list of sender agent IDs to filter
            message_type: Optional message type to filter
            timeout: Timeout in seconds (0 = no wait)
            require_all: If True with from_agents, wait for messages from ALL agents

        Returns:
            List of received messages (consumed from queue)

        Raises:
            MessageTimeoutError: If timeout expires before receiving required messages
        """
        ...

    def broadcast(
        self,
        message: Message,
        agent_type_filter: Optional[str] = None,
    ) -> int:
        """
        Broadcast message to all agents in namespace.

        Args:
            message: Message to broadcast (to_agent should be "*")
            agent_type_filter: Optional filter by agent type

        Returns:
            Number of agents the message was delivered to
        """
        ...

    def clear_namespace(self, namespace: str) -> int:
        """
        Clear all messages in a namespace.

        Args:
            namespace: Namespace to clear

        Returns:
            Number of messages cleared
        """
        ...


class InMemoryMessageQueue:
    """
    In-memory message queue implementation.

    Thread-safe implementation for single-process workflows.
    Messages are organized by (namespace, to_agent) for efficient retrieval.

    Not suitable for:
    - Multi-process communication
    - Persistent messaging (messages lost on termination)
    """

    def __init__(self):
        """Initialize in-memory queue."""
        self._lock = threading.RLock()
        # Messages stored as: {(namespace, to_agent): [Message, ...]}
        self._queues: Dict[tuple, List[Message]] = {}
        # Registered agents for broadcast: {namespace: {agent_id: agent_type}}
        self._agents: Dict[str, Dict[str, Optional[str]]] = {}
        # Delivery confirmations: {message_id: confirmed}
        self._confirmations: Dict[str, bool] = {}

    def register_agent(
        self, agent_id: str, namespace: str, agent_type: Optional[str] = None
    ) -> None:
        """
        Register an agent for receiving broadcasts.

        Args:
            agent_id: Agent identifier
            namespace: Agent's namespace
            agent_type: Optional type for filtering
        """
        with self._lock:
            if namespace not in self._agents:
                self._agents[namespace] = {}
            self._agents[namespace][agent_id] = agent_type

    def unregister_agent(self, agent_id: str, namespace: str) -> None:
        """
        Unregister an agent.

        Args:
            agent_id: Agent identifier
            namespace: Agent's namespace
        """
        with self._lock:
            if namespace in self._agents:
                self._agents[namespace].pop(agent_id, None)

    def send(self, message: Message, confirm: bool = False) -> Optional[str]:
        """
        Send a message to the queue.

        Args:
            message: Message to send
            confirm: If True, mark as confirmed immediately (in-memory is synchronous)

        Returns:
            Message ID if confirm=True, None otherwise
        """
        with self._lock:
            key = (message.namespace, message.to_agent)
            if key not in self._queues:
                self._queues[key] = []
            self._queues[key].append(message)

            if confirm:
                self._confirmations[message.id] = True
                return message.id

        return None

    def receive(
        self,
        agent_id: str,
        namespace: str,
        from_agents: Optional[List[str]] = None,
        message_type: Optional[str] = None,
        timeout: float = 0.0,
        require_all: bool = False,
    ) -> List[Message]:
        """
        Receive messages from the queue.

        Args:
            agent_id: Receiving agent's ID
            namespace: Namespace to receive from
            from_agents: Optional list of sender agent IDs to filter
            message_type: Optional message type to filter
            timeout: Timeout in seconds (0 = no wait)
            require_all: If True with from_agents, wait for messages from ALL agents

        Returns:
            List of received messages

        Raises:
            MessageTimeoutError: If require_all and timeout expires
        """
        start_time = time.time()
        collected: List[Message] = []
        seen_senders: set = set()
        required_senders = set(from_agents) if from_agents and require_all else None

        while True:
            with self._lock:
                key = (namespace, agent_id)
                if key in self._queues:
                    queue = self._queues[key]
                    remaining = []
                    for msg in queue:
                        # Skip expired messages
                        if msg.is_expired():
                            continue
                        # Filter by sender
                        if from_agents and msg.from_agent not in from_agents:
                            remaining.append(msg)
                            continue
                        # Filter by type
                        if message_type and msg.type != message_type:
                            remaining.append(msg)
                            continue
                        # Collect message
                        collected.append(msg)
                        seen_senders.add(msg.from_agent)
                    self._queues[key] = remaining

                # Also check broadcast queue
                broadcast_key = (namespace, "*")
                if broadcast_key in self._queues:
                    queue = self._queues[broadcast_key]
                    remaining = []
                    for msg in queue:
                        if msg.is_expired():
                            continue
                        if from_agents and msg.from_agent not in from_agents:
                            remaining.append(msg)
                            continue
                        if message_type and msg.type != message_type:
                            remaining.append(msg)
                            continue
                        collected.append(msg)
                        seen_senders.add(msg.from_agent)
                    self._queues[broadcast_key] = remaining

            # Check if we have all required senders
            if required_senders:
                if required_senders <= seen_senders:
                    return collected
            elif collected or timeout <= 0:
                return collected

            # Check timeout
            elapsed = time.time() - start_time
            if elapsed >= timeout:
                if require_all and required_senders:
                    missing = required_senders - seen_senders
                    raise MessageTimeoutError(
                        f"Timeout waiting for messages from agents: {missing}"
                    )
                return collected

            # Brief sleep before retry
            time.sleep(min(0.01, timeout - elapsed))

    def broadcast(
        self,
        message: Message,
        agent_type_filter: Optional[str] = None,
    ) -> int:
        """
        Broadcast message to all agents in namespace.

        Args:
            message: Message to broadcast
            agent_type_filter: Optional filter by agent type

        Returns:
            Number of agents the message was delivered to
        """
        count = 0
        with self._lock:
            namespace = message.namespace
            if namespace not in self._agents:
                return 0

            for agent_id, agent_type in self._agents[namespace].items():
                # Skip sender
                if agent_id == message.from_agent:
                    continue
                # Filter by type
                if agent_type_filter and agent_type != agent_type_filter:
                    continue
                # Create copy for each recipient
                recipient_msg = Message(
                    id=f"msg_{uuid.uuid4().hex[:12]}",
                    correlation_id=message.correlation_id,
                    from_agent=message.from_agent,
                    to_agent=agent_id,
                    namespace=namespace,
                    type=message.type,
                    payload=message.payload,
                    timestamp=message.timestamp,
                    ttl=message.ttl,
                )
                key = (namespace, agent_id)
                if key not in self._queues:
                    self._queues[key] = []
                self._queues[key].append(recipient_msg)
                count += 1

        return count

    def clear_namespace(self, namespace: str) -> int:
        """
        Clear all messages in a namespace.

        Args:
            namespace: Namespace to clear

        Returns:
            Number of messages cleared
        """
        count = 0
        with self._lock:
            keys_to_remove = [key for key in self._queues.keys() if key[0] == namespace]
            for key in keys_to_remove:
                count += len(self._queues[key])
                del self._queues[key]
        return count

    def get_queue_size(self, namespace: str, agent_id: str) -> int:
        """
        Get the number of messages waiting for an agent.

        Args:
            namespace: Namespace
            agent_id: Agent identifier

        Returns:
            Number of messages in queue
        """
        with self._lock:
            key = (namespace, agent_id)
            return len(self._queues.get(key, []))

    def get_all_messages(
        self, namespace: str, agent_id: Optional[str] = None
    ) -> List[Message]:
        """
        Get all messages (for testing/debugging).

        Args:
            namespace: Namespace
            agent_id: Optional agent filter

        Returns:
            List of all messages (not consumed)
        """
        with self._lock:
            messages = []
            for (ns, aid), queue in self._queues.items():
                if ns != namespace:
                    continue
                if agent_id and aid != agent_id:
                    continue
                messages.extend(queue)
            return messages


# Global singleton for in-process communication
_global_queue: Optional[InMemoryMessageQueue] = None
_global_queue_lock = threading.Lock()


def get_global_queue() -> InMemoryMessageQueue:
    """
    Get the global in-memory message queue singleton.

    Thread-safe lazy initialization.
    """
    global _global_queue
    if _global_queue is None:
        with _global_queue_lock:
            if _global_queue is None:
                _global_queue = InMemoryMessageQueue()
    return _global_queue


def reset_global_queue() -> None:
    """
    Reset the global message queue.

    Useful for testing to ensure clean state.
    """
    global _global_queue
    with _global_queue_lock:
        _global_queue = None
