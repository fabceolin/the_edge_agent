"""
A2A (Agent-to-Agent) Communication Infrastructure.

This package provides inter-agent communication primitives for TEA workflows,
enabling message passing, delegation, shared state, and agent discovery.

Components:
- message_queue: Message queue abstraction and in-memory implementation
- shared_state: Namespace-scoped shared state with optimistic locking
- discovery: Agent discovery and capability advertisement

Story: TEA-AGENT-001.5
"""

from .message_queue import (
    Message,
    MessageQueue,
    InMemoryMessageQueue,
    MessageQueueError,
    MessageDeliveryError,
    MessageTimeoutError,
)
from .shared_state import (
    SharedState,
    InMemorySharedState,
    SharedStateError,
    OptimisticLockError,
)
from .discovery import (
    AgentInfo,
    AgentDiscovery,
    InMemoryAgentDiscovery,
    DiscoveryMode,
)

__all__ = [
    # Message Queue
    "Message",
    "MessageQueue",
    "InMemoryMessageQueue",
    "MessageQueueError",
    "MessageDeliveryError",
    "MessageTimeoutError",
    # Shared State
    "SharedState",
    "InMemorySharedState",
    "SharedStateError",
    "OptimisticLockError",
    # Discovery
    "AgentInfo",
    "AgentDiscovery",
    "InMemoryAgentDiscovery",
    "DiscoveryMode",
]
