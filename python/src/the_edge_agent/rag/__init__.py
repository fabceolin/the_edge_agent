"""
RAG (Retrieval-Augmented Generation) module for The Edge Agent.

This module provides abstractions and integrations for advanced RAG patterns,
including LlamaIndex bridge for router queries, sub-question decomposition,
and agentic retrieval.

Components:
    - llamaindex_client: LlamaIndex integration wrapper (TEA-AGENT-001.8)

Usage:
    >>> from the_edge_agent.rag import LlamaIndexClient
    >>> client = LlamaIndexClient()
    >>> results = client.query("What is the capital of France?")
"""

from .llamaindex_client import (
    LlamaIndexClient,
    LlamaIndexUnavailableError,
    LLAMAINDEX_AVAILABLE,
)

__all__ = [
    "LlamaIndexClient",
    "LlamaIndexUnavailableError",
    "LLAMAINDEX_AVAILABLE",
]
