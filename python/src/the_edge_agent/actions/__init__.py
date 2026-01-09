"""
Built-in Actions for YAMLEngine.

This package provides the built-in action registry for YAMLEngine workflows.
Actions are organized by domain:

- llm_actions: LLM calls, streaming, retry, and tool calling (TEA-BUILTIN-001.2)
- core_actions: HTTP, file operations, notifications, checkpoints
- data_actions: JSON/CSV parsing, transformation, validation (TEA-BUILTIN-003.2)
- observability_actions: Tracing spans and event logging (TEA-BUILTIN-001.3)
- memory_actions: Key-value storage with TTL (TEA-BUILTIN-001.1)
- ltm_actions: Long-term persistent key-value storage with FTS5 (TEA-BUILTIN-001.4)
- graph_actions: Graph database with Datalog and HNSW vectors (TEA-BUILTIN-001.4)
- storage_actions: Cloud storage operations via fsspec (S3, GCS, Azure, etc.) (TEA-BUILTIN-004.1)
- web_actions: Web scraping, crawling, and search via Firecrawl/Perplexity (TEA-BUILTIN-002.1)
- rag_actions: Embedding creation, vector storage, and semantic search (TEA-BUILTIN-002.2)
- tools_actions: Bridges to CrewAI, MCP, and LangChain tool ecosystems (TEA-BUILTIN-002.3)
- code_actions: Sandboxed Python code execution (TEA-BUILTIN-003.1)
- schema_actions: Schema merge and manipulation (TEA-BUILTIN-008.3)
- llamaextract_actions: Document extraction via LlamaExtract (TEA-BUILTIN-008.1)
- cache_actions: Cache and memoization with LTM backend (TEA-BUILTIN-010)
- ratelimit_actions: Rate limiting with shared named limiters (TEA-BUILTIN-011)
- secrets_actions: Secrets access via secrets.get and secrets.has (TEA-BUILTIN-012.3)
- validation_actions: Generic extraction validation with Prolog/probes (TEA-YAML-004)
- retry_actions: General-purpose retry loop with correction (TEA-YAML-005)
- academic_actions: Academic research via PubMed and ArXiv APIs (TEA-KIROKU-001)
- text_actions: Text processing including citation insertion (TEA-KIROKU-002)
- neo4j_trigger_actions: Neo4j APOC trigger management (TEA-BUILTIN-001.7.5)
- neo4j_gds_actions: Neo4j GDS graph analytics algorithms (TEA-BUILTIN-001.7.4)
- reasoning_actions: Reasoning technique primitives (CoT, ReAct, self-correct, decompose) (TEA-AGENT-001.4)
- agent_actions: Multi-agent collaboration primitives (dispatch, parallel, sequential, coordinate) (TEA-AGENT-001.1)
- mem0_actions: Mem0 universal memory integration (TEA-AGENT-001.6)
- planning_actions: Planning and decomposition primitives (decompose, execute, replan, status) (TEA-AGENT-001.3)
- session_persistence_actions: Session persistence (load, save, delete, exists) (TEA-BUILTIN-015.1)
- llamaindex_actions: LlamaIndex RAG bridge (query, router, subquestion, index management) (TEA-AGENT-001.8)
- dspy_actions: DSPy prompt optimization (cot, react, compile, optimize) (TEA-AGENT-001.7)
- firestore_actions: Firestore CRUD operations (get, set, query, delete, batch) (TEA-BUILTIN-015.2)
- http_response_actions: HTTP response for early termination (http.respond) (TEA-BUILTIN-015.5)
- input_validation_actions: Input schema validation (validate.input) (TEA-BUILTIN-015.4)
- auth_actions: Authentication verification (auth.verify, auth.get_user) (TEA-BUILTIN-015.3)
- error_actions: Error handling actions (error.is_retryable, error.clear, error.retry) (TEA-BUILTIN-015.6)
- a2a_actions: Inter-agent communication (send, receive, broadcast, delegate, state, discover) (TEA-AGENT-001.5)
- semtools_actions: Semantic search using SemTools CLI (TEA-BUILTIN-014)

Firebase Agent Memory Infrastructure (TEA-BUILTIN-006):
- catalog_actions: Data catalog for tables, files, and snapshots
- cloud_memory_actions: Cloud storage with metadata management
- search_actions: SQL and full-text search via QueryEngine
- vector_actions: Vector similarity search via VectorIndex
- session_actions: Session lifecycle with archive-based expiration
- context_actions: Context assembly with relevance ranking

Usage:
    >>> from the_edge_agent.actions import build_actions_registry
    >>>
    >>> # Build registry for an engine instance
    >>> registry = build_actions_registry(engine)
    >>>
    >>> # Actions available as 'domain.action' and 'actions.domain_action'
    >>> registry['llm.call'](state, model="gpt-4", messages=[...])
    >>> registry['json.parse'](state, text='{"key": "value"}')

Custom Action Registration:
    Each module provides a `register_actions(registry, engine)` function
    that adds actions to the registry. The engine parameter provides access
    to shared resources like memory backends and trace contexts.
"""

from typing import Any, Callable, Dict

from .llm_actions import register_actions as register_llm
from .core_actions import register_actions as register_core
from .data_actions import register_actions as register_data
from .observability_actions import register_actions as register_observability
from .memory_actions import register_actions as register_memory
from .ltm_actions import register_actions as register_ltm
from .graph_actions import register_actions as register_graph
from .storage_actions import register_actions as register_storage
from .web_actions import register_actions as register_web
from .rag_actions import register_actions as register_rag
from .tools_actions import register_actions as register_tools
from .code_actions import register_actions as register_code
from .schema_actions import register_actions as register_schema
from .llamaextract_actions import register_actions as register_llamaextract
from .cache_actions import register_actions as register_cache
from .cache_actions import register_jinja_filters as register_cache_jinja_filters

# TEA-YAML-004: Generic Extraction Validation
from .validation_actions import register_actions as register_validation

# TEA-YAML-005: General-Purpose Retry Loop Action
from .retry_actions import register_actions as register_retry

# TEA-KIROKU-001: Academic Research Actions
from .academic_actions import register_actions as register_academic

# TEA-KIROKU-002: Text Processing Actions (Citation Insertion)
from .text_actions import register_actions as register_text

# TEA-BUILTIN-011: Rate Limiting Actions
from .ratelimit_actions import register_actions as register_ratelimit
from .ratelimit_actions import configure_rate_limiters_from_settings

# TEA-BUILTIN-012.3: Secrets Actions
from .secrets_actions import register_actions as register_secrets

# TEA-BUILTIN-001.7.5: Neo4j APOC Trigger Actions
from .neo4j_trigger_actions import register_actions as register_neo4j_triggers

# TEA-BUILTIN-001.7.4: Neo4j GDS Actions
from .neo4j_gds_actions import register_actions as register_neo4j_gds

# TEA-AGENT-001.2: Reflection Loop Primitive
from .reflection_actions import register_actions as register_reflection
from .reflection_actions import ReflectionFailedError

# TEA-AGENT-001.4: Reasoning Techniques Primitives
from .reasoning_actions import register_actions as register_reasoning

# TEA-AGENT-001.1: Multi-Agent Collaboration Primitives
from .agent_actions import register_actions as register_agent

# TEA-AGENT-001.6: Mem0 Memory Integration
from .mem0_actions import register_actions as register_mem0

# TEA-AGENT-001.3: Planning & Decomposition Primitive
from .planning_actions import register_actions as register_planning

# TEA-BUILTIN-015.1: Session Persistence Actions
from .session_persistence_actions import (
    register_actions as register_session_persistence,
)

# TEA-AGENT-001.8: LlamaIndex RAG Bridge
from .llamaindex_actions import register_actions as register_llamaindex

# TEA-AGENT-001.7: DSPy Prompt Optimization
from .dspy_actions import register_actions as register_dspy

# TEA-BUILTIN-015.2: Firestore CRUD Actions
from .firestore_actions import register_actions as register_firestore

# TEA-BUILTIN-015.5: HTTP Response Actions (Early Termination)
from .http_response_actions import register_actions as register_http_response
from .http_response_actions import HTTPResponse

# TEA-BUILTIN-015.4: Input Validation Schema
from .input_validation_actions import register_actions as register_input_validation

# TEA-BUILTIN-015.3: Authentication Actions
from .auth_actions import register_actions as register_auth

# TEA-BUILTIN-015.6: Error Handling Actions
from .error_actions import register_actions as register_error

# TEA-AGENT-001.5: Inter-Agent Communication
from .a2a_actions import register_actions as register_a2a

# TEA-AGENT-001.9: TextGrad Learning
from .textgrad_actions import register_actions as register_textgrad

# TEA-BUILTIN-014: SemTools Semantic Search
from .semtools_actions import register_actions as register_semtools

# TEA-BUILTIN-006: Firebase Agent Memory Infrastructure
from .catalog_actions import register_actions as register_catalog
from .cloud_memory_actions import register_actions as register_cloud_memory
from .search_actions import register_actions as register_search
from .vector_actions import register_actions as register_vector
from .session_actions import register_actions as register_session
from .context_actions import register_actions as register_context
from .data_tabular_actions import register_actions as register_data_tabular

# TEA-RELEASE-004.5: Local LLM Actions (llama-cpp-python)
from .llm_local_actions import register_actions as register_llm_local


def build_actions_registry(engine: Any) -> Dict[str, Callable]:
    """
    Build the complete actions registry for a YAMLEngine instance.

    This function creates a new registry and populates it with all built-in
    actions. Each action module's register_actions function is called with
    the registry and engine reference, allowing actions to access engine
    internals like memory backends and trace contexts.

    Args:
        engine: YAMLEngine instance that actions may need to access for
                memory backends, trace contexts, and other shared resources.

    Returns:
        Dictionary mapping action names to callable functions.
        Actions are registered under both 'domain.action' and 'actions.domain_action'
        naming conventions for backward compatibility.

    Example:
        >>> from the_edge_agent import YAMLEngine
        >>> engine = YAMLEngine()
        >>> registry = build_actions_registry(engine)
        >>> print(list(registry.keys())[:5])
        ['llm.call', 'actions.llm_call', 'llm.stream', ...]
    """
    registry: Dict[str, Callable] = {}

    # Register all action modules
    register_llm(registry, engine)
    register_core(registry, engine)
    register_data(registry, engine)
    register_observability(registry, engine)
    register_memory(registry, engine)
    register_ltm(registry, engine)
    register_graph(registry, engine)
    register_storage(registry, engine)
    register_web(registry, engine)
    register_rag(registry, engine)
    register_tools(registry, engine)
    register_code(registry, engine)
    register_schema(registry, engine)
    register_llamaextract(registry, engine)
    register_cache(registry, engine)

    # TEA-YAML-004: Generic Extraction Validation
    register_validation(registry, engine)

    # TEA-YAML-005: General-Purpose Retry Loop Action
    register_retry(registry, engine)

    # TEA-KIROKU-001: Academic Research Actions
    register_academic(registry, engine)

    # TEA-KIROKU-002: Text Processing Actions
    register_text(registry, engine)

    # TEA-BUILTIN-011: Rate Limiting Actions
    register_ratelimit(registry, engine)

    # TEA-BUILTIN-012.3: Secrets Actions
    register_secrets(registry, engine)

    # TEA-BUILTIN-001.7.5: Neo4j APOC Trigger Actions
    register_neo4j_triggers(registry, engine)

    # TEA-BUILTIN-001.7.4: Neo4j GDS Actions
    register_neo4j_gds(registry, engine)

    # TEA-AGENT-001.2: Reflection Loop Primitive
    register_reflection(registry, engine)

    # TEA-AGENT-001.4: Reasoning Techniques Primitives
    register_reasoning(registry, engine)

    # TEA-AGENT-001.1: Multi-Agent Collaboration Primitives
    register_agent(registry, engine)

    # TEA-AGENT-001.6: Mem0 Memory Integration
    register_mem0(registry, engine)

    # TEA-AGENT-001.3: Planning & Decomposition Primitive
    register_planning(registry, engine)

    # TEA-BUILTIN-015.1: Session Persistence Actions
    register_session_persistence(registry, engine)

    # TEA-AGENT-001.8: LlamaIndex RAG Bridge
    register_llamaindex(registry, engine)

    # TEA-AGENT-001.7: DSPy Prompt Optimization
    register_dspy(registry, engine)

    # TEA-BUILTIN-015.2: Firestore CRUD Actions
    register_firestore(registry, engine)

    # TEA-BUILTIN-015.5: HTTP Response Actions
    register_http_response(registry, engine)

    # TEA-BUILTIN-015.4: Input Validation Schema
    register_input_validation(registry, engine)

    # TEA-BUILTIN-015.3: Authentication Actions
    register_auth(registry, engine)

    # TEA-BUILTIN-015.6: Error Handling Actions
    register_error(registry, engine)

    # TEA-AGENT-001.5: Inter-Agent Communication
    register_a2a(registry, engine)

    # TEA-AGENT-001.9: TextGrad Learning
    register_textgrad(registry, engine)

    # TEA-BUILTIN-014: SemTools Semantic Search
    register_semtools(registry, engine)

    # TEA-BUILTIN-006: Firebase Agent Memory Infrastructure
    register_catalog(registry, engine)
    register_cloud_memory(registry, engine)
    register_search(registry, engine)
    register_vector(registry, engine)
    register_session(registry, engine)
    register_context(registry, engine)
    register_data_tabular(registry, engine)

    # TEA-RELEASE-004.5: Local LLM Actions (llama-cpp-python)
    register_llm_local(registry, engine)

    return registry


__all__ = [
    "build_actions_registry",
    "register_cache_jinja_filters",
    "configure_rate_limiters_from_settings",
    "ReflectionFailedError",
    "HTTPResponse",
]
