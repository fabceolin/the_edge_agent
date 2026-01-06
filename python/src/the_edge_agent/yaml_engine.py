"""
YAML-based StateGraph engine for declarative agent workflows.

Inspired by GitHub Actions and GitLab CI/CD pipelines.

Supports checkpoint persistence for save/resume of workflow execution:
- config.checkpoint_dir: Enable auto-save at interrupt points
- config.checkpoint: Resume from a saved checkpoint
- checkpoint.save/load actions: Manual checkpoint operations
- {{ checkpoint.dir }}, {{ checkpoint.last }}: Template variables

Example:
    >>> engine = YAMLEngine()
    >>> graph = engine.load_from_file("agent.yaml", checkpoint="./chk/state.pkl")
    >>> for event in graph.invoke():
    ...     print(event)
"""

import yaml
import json
import os
import re
import threading
import time
import importlib
import importlib.util
import logging
from functools import lru_cache
from typing import Any, Callable, Dict, Generator, List, Optional, Set, Union

from jinja2 import Environment, BaseLoader, StrictUndefined, TemplateError

from .stategraph import StateGraph, START, END

logger = logging.getLogger(__name__)

from .memory import (
    MemoryBackend,
    InMemoryBackend,
    LongTermMemoryBackend,
    SQLiteBackend,
    GraphBackend,
    COZO_AVAILABLE,
    KUZU_AVAILABLE,
    DUCKPGQ_AVAILABLE,
    # TEA-BUILTIN-006: Firebase Agent Memory Infrastructure
    MetadataStore,
    create_metadata_store,
    FIRESTORE_AVAILABLE,
    BlobStorage,
    create_blob_storage,
    GCS_AVAILABLE,
    QueryEngine,
    create_query_engine,
    DUCKDB_AVAILABLE,
    VectorIndex,
    create_vector_index,
    DUCKDB_VSS_AVAILABLE,
    # TEA-BUILTIN-001.6.4: LTM Backend Configuration
    expand_env_vars,
    create_ltm_backend,
    parse_backend_config,
)
from .tracing import TraceContext, ConsoleExporter, FileExporter, CallbackExporter
from .observability import ObservabilityContext, EventStream
from .observability import ConsoleHandler, FileHandler, CallbackHandler
from .actions import build_actions_registry, register_cache_jinja_filters
from .yaml_templates import TemplateProcessor, DotDict
from .yaml_nodes import NodeFactory
from .yaml_edges import EdgeFactory
from .yaml_imports import ImportLoader
from .yaml_config import EngineConfig


class YAMLEngine:
    """
    Engine for creating StateGraph instances from YAML configurations.

    Supports:
    - Inline Python code execution
    - Built-in actions (HTTP, LLM, file operations, checkpoint operations)
    - Template variables ({{ state.key }}, {{ checkpoint.dir }}, {{ checkpoint.last }})
    - Conditional expressions
    - Multi-step nodes (GitHub Actions style)
    - Parallel execution with matrix strategy
    - Checkpoint persistence for save/resume workflow execution

    Example:
        >>> engine = YAMLEngine()
        >>> graph = engine.load_from_file("agent_config.yaml")
        >>> result = list(graph.invoke({"query": "AI research"}))

        >>> # Resume from checkpoint
        >>> graph = engine.load_from_file("agent.yaml", checkpoint="./chk/state.pkl")
        >>> for event in graph.invoke():
        ...     print(event)
    """

    def __init__(
        self,
        actions_registry: Optional[Dict[str, Callable]] = None,
        enable_tracing: bool = True,
        trace_exporter: Optional[Union[str, List[Any]]] = None,
        trace_file: Optional[str] = None,
        trace_callback: Optional[Callable[[Dict[str, Any]], None]] = None,
        trace_verbose: bool = False,
        memory_backend: Optional[Any] = None,
        enable_code_execution: bool = False,
        ltm_backend: Optional[Any] = None,
        enable_ltm: bool = True,
        ltm_path: Optional[str] = None,
        ltm_backend_type: str = "sqlite",
        ltm_config: Optional[Dict[str, Any]] = None,
        graph_backend: Optional[Any] = None,
        enable_graph: bool = True,
        graph_path: Optional[str] = None,
        graph_backend_type: Optional[str] = None,
        opik_llm_tracing: bool = False,
        # TEA-BUILTIN-005.3: Opik configuration parameters
        opik_api_key: Optional[str] = None,
        opik_workspace: Optional[str] = None,
        opik_project_name: Optional[str] = None,
        opik_url: Optional[str] = None,
        opik_enabled: Optional[bool] = None,
        opik_trace_export: bool = False,
        # TEA-BUILTIN-006: Firebase Agent Memory Infrastructure
        metadata_store: Optional[Any] = None,
        enable_metadata_store: bool = True,
        blob_storage: Optional[Any] = None,
        enable_blob_storage: bool = True,
        query_engine: Optional[Any] = None,
        enable_query_engine: bool = True,
        vector_index: Optional[Any] = None,
        enable_vector_index: bool = True,
        embedding_fn: Optional[Callable[[str], List[float]]] = None,
        # TEA-LUA.P1: Lua runtime integration
        lua_enabled: bool = False,
        lua_timeout: float = 30.0,
        # TEA-PROLOG: Prolog runtime integration
        prolog_enabled: bool = False,
        prolog_timeout: float = 30.0,
        prolog_sandbox: bool = True,
    ):
        """
        Initialize the YAML engine.

        Args:
            actions_registry: Custom actions to register beyond built-ins
            enable_tracing: Enable trace actions (default: True)
            trace_exporter: Exporter configuration. Can be:
                - "console": Print to stdout
                - "file": Write to trace_file (requires trace_file)
                - "callback": Call trace_callback (requires trace_callback)
                - "opik": Export to Comet Opik (requires pip install opik)
                - List of exporter instances
            trace_file: File path for file exporter (JSON lines format)
            trace_callback: Callback function for callback exporter
            trace_verbose: Enable verbose console output (default: False)
            memory_backend: Optional custom MemoryBackend implementation.
                           If None, uses InMemoryBackend by default.
            enable_code_execution: Enable code.execute and code.sandbox actions.
                                  Default: False (SECURITY: disabled by default).
                                  Only enable for trusted code patterns.
            ltm_backend: Optional custom LongTermMemoryBackend implementation.
                        If None and enable_ltm=True, uses backend based on ltm_backend_type.
            enable_ltm: Enable long-term memory actions (default: True).
            ltm_path: Path to SQLite database for ltm.* actions (SQLite backend only).
                     If None, uses in-memory SQLite.
            ltm_backend_type: Type of LTM backend: "sqlite" (default) or "duckdb".
                             DuckDB backend provides optimized batch operations for
                             serverless environments (TEA-BUILTIN-001.6.3).
            ltm_config: Configuration dict for LTM backend. For DuckDB backend:
                       - catalog_config: Catalog backend configuration
                         - type: "sqlite" | "postgres" | "firestore"
                         - path: Database path (SQLite)
                         - connection_string: Connection string (Postgres)
                         - lazy: Enable lazy initialization
                       - storage_uri: Cloud storage path for large objects
                       - lazy: Enable lazy initialization (default: True for serverless)
            graph_backend: Optional custom GraphBackend implementation.
                          If None and enable_graph=True, uses backend based on graph_backend_type.
            enable_graph: Enable graph database actions (default: True).
            graph_path: Path to graph database for graph.* actions.
                       If None, uses in-memory storage.
            graph_backend_type: Type of graph backend to use. Options:
                              - None: Auto-select (CozoDB if available, else Kuzu)
                              - "cozo": Use CozoDB (Datalog, HNSW vectors)
                              - "kuzu" or "bighorn": Use Kuzu/Bighorn (Cypher, cloud httpfs)
            opik_llm_tracing: Enable native Opik tracing for llm.call and llm.stream.
                             When True, wraps OpenAI clients with track_openai() for
                             rich LLM telemetry (tokens, latency, model params).
                             Default: False (opt-in feature).
            opik_api_key: API key for Opik Cloud (TEA-BUILTIN-005.3).
                         Constructor param has highest priority, then env var.
            opik_workspace: Workspace name for Opik.
            opik_project_name: Project name for grouping traces.
            opik_url: Custom URL for self-hosted Opik instances.
            opik_enabled: Explicitly enable/disable all Opik features.
                         When None, Opik is enabled if trace_exporter="opik".
            opik_trace_export: Export TEA trace spans to Opik (default: False).
            metadata_store: Optional custom MetadataStore implementation for
                           document database operations (e.g., Firestore).
                           If None and enable_metadata_store=True, auto-creates
                           based on available providers.
            enable_metadata_store: Enable metadata store (default: True).
            blob_storage: Optional custom BlobStorage implementation for
                         cloud object storage (e.g., GCS, S3).
                         If None and enable_blob_storage=True, auto-creates
                         based on available providers.
            enable_blob_storage: Enable blob storage (default: True).
            query_engine: Optional custom QueryEngine implementation for
                         SQL queries with resilience patterns.
                         If None and enable_query_engine=True, auto-creates
                         DuckDB-based engine if available.
            enable_query_engine: Enable query engine (default: True).
            vector_index: Optional custom VectorIndex implementation for
                         vector similarity search.
                         If None and enable_vector_index=True, auto-creates
                         DuckDB VSS-based index if available.
            enable_vector_index: Enable vector index (default: True).
            embedding_fn: Optional function to generate embeddings from text.
                         Signature: (text: str) -> List[float].
                         Used by vector_actions for memory.vector_search.
            lua_enabled: Enable Lua runtime for run: blocks (default: False).
                        When True, code blocks starting with '-- lua' or containing
                        Lua-specific syntax (local, end, then) will be executed
                        with the Lua runtime instead of Python exec().
                        Requires pip install 'the_edge_agent[lua]'.
            lua_timeout: Timeout for Lua code execution in seconds (default: 30.0).
            prolog_enabled: Enable Prolog runtime for run: blocks (default: False).
                           When True, code blocks with 'language: prolog' or starting
                           with '% prolog' or containing Prolog-specific syntax (:-,
                           state/2, return/2) will be executed with SWI-Prolog.
                           Requires pip install 'the_edge_agent[prolog]' and
                           SWI-Prolog system installation.
            prolog_timeout: Timeout for Prolog code execution in seconds (default: 30.0).
            prolog_sandbox: Enable sandboxed execution for Prolog (default: True).
                           When True, restricts file I/O, shell access, and network.
        """
        # TEA-BUILTIN-005.3: Store Opik constructor params first (needed for exporter creation)
        self._opik_constructor_params = {
            "api_key": opik_api_key,
            "workspace": opik_workspace,
            "project_name": opik_project_name,
            "url": opik_url,
            "enabled": opik_enabled,
            "llm_tracing": opik_llm_tracing if opik_llm_tracing else None,
            "trace_export": opik_trace_export if opik_trace_export else None,
        }

        # TEA-PY-008.5: Initialize engine config manager (before opik config resolution)
        self._engine_config = EngineConfig(self)

        # Initial config resolution (without YAML settings yet)
        self._opik_config: Dict[str, Any] = self._engine_config.resolve_opik_config()

        # Initialize tracing
        self._enable_tracing = enable_tracing
        self._trace_context: Optional[TraceContext] = None

        if enable_tracing:
            exporters = []

            if isinstance(trace_exporter, list):
                # User provided exporter instances
                exporters = trace_exporter
            elif trace_exporter == "console":
                exporters.append(ConsoleExporter(verbose=trace_verbose))
            elif trace_exporter == "file" and trace_file:
                exporters.append(FileExporter(trace_file))
            elif trace_exporter == "callback" and trace_callback:
                exporters.append(CallbackExporter(trace_callback))
            elif trace_exporter == "opik":
                # Lazy-load OpikExporter with resolved config
                from .exporters import OpikExporter

                exporters.append(
                    OpikExporter(
                        api_key=self._opik_config.get("api_key"),
                        project_name=self._opik_config.get("project_name"),
                        workspace=self._opik_config.get("workspace"),
                        url_override=self._opik_config.get("url"),
                    )
                )
            elif trace_exporter is None:
                # No exporter configured, but tracing is enabled
                # Spans will be collected but not exported
                pass

            self._trace_context = TraceContext(exporters=exporters)

        # Auto-trace flag (can be enabled via YAML settings)
        self._auto_trace = False

        # Initialize memory backend (TEA-BUILTIN-001.1)
        self._memory_backend: Any = (
            memory_backend if memory_backend is not None else InMemoryBackend()
        )

        # Initialize long-term memory backend (TEA-BUILTIN-001.4, TEA-BUILTIN-001.6.3)
        self._ltm_backend: Optional[Any] = None
        self._enable_ltm = enable_ltm
        self._ltm_backend_type = ltm_backend_type
        self._ltm_config = ltm_config or {}
        if enable_ltm:
            if ltm_backend is not None:
                self._ltm_backend = ltm_backend
            elif ltm_backend_type == "duckdb":
                # DuckDB LTM backend with lazy initialization (TEA-BUILTIN-001.6.3)
                try:
                    from .memory.duckdb_ltm import DuckDBLTMBackend

                    # Get configuration with defaults for serverless optimization
                    catalog_config = self._ltm_config.get(
                        "catalog_config",
                        {
                            "type": "sqlite",
                            "path": ltm_path or ":memory:",
                            "lazy": True,
                        },
                    )
                    storage_uri = self._ltm_config.get("storage_uri", "./ltm_data/")
                    lazy = self._ltm_config.get("lazy", True)

                    self._ltm_backend = DuckDBLTMBackend(
                        catalog_config=catalog_config,
                        storage_uri=storage_uri,
                        lazy=lazy,
                        enable_fts=self._ltm_config.get("enable_fts", True),
                    )
                except ImportError:
                    # Fallback to SQLite if DuckDB not available
                    self._ltm_backend = SQLiteBackend(ltm_path or ":memory:")
            else:
                # Default: Use SQLiteBackend with specified path or in-memory
                self._ltm_backend = SQLiteBackend(ltm_path or ":memory:")

        # Initialize graph backend (TEA-BUILTIN-001.4)
        self._graph_backend: Optional[Any] = None
        self._enable_graph = enable_graph
        self._graph_backend_type = graph_backend_type
        if enable_graph:
            if graph_backend is not None:
                self._graph_backend = graph_backend
            elif graph_backend_type in ("kuzu", "bighorn"):
                # Explicitly requested Kuzu/Bighorn backend
                if KUZU_AVAILABLE:
                    from .memory import KuzuBackend

                    try:
                        self._graph_backend = KuzuBackend(graph_path or ":memory:")
                    except Exception:
                        pass
            elif graph_backend_type == "cozo":
                # Explicitly requested CozoDB backend
                if COZO_AVAILABLE:
                    from .memory import CozoBackend

                    try:
                        self._graph_backend = CozoBackend(graph_path or ":memory:")
                    except Exception:
                        pass
            elif graph_backend_type == "duckpgq":
                # Explicitly requested DuckPGQ backend (TEA-BUILTIN-001.8)
                if DUCKPGQ_AVAILABLE:
                    from .memory import DuckPGQBackend

                    try:
                        self._graph_backend = DuckPGQBackend(
                            graph_path or ":memory:", lazy_load_extension=True
                        )
                    except Exception:
                        pass
            else:
                # Auto-select: prefer CozoDB, fallback to Kuzu
                if COZO_AVAILABLE:
                    from .memory import CozoBackend

                    try:
                        self._graph_backend = CozoBackend(graph_path or ":memory:")
                    except Exception:
                        pass
                elif KUZU_AVAILABLE:
                    from .memory import KuzuBackend

                    try:
                        self._graph_backend = KuzuBackend(graph_path or ":memory:")
                    except Exception:
                        pass

        # Code execution flag (TEA-BUILTIN-003.1) - DISABLED by default for security
        self._enable_code_execution = enable_code_execution

        # TEA-BUILTIN-006: Initialize Firebase Agent Memory Infrastructure
        # Metadata Store (document database)
        self._metadata_store: Optional[Any] = None
        self._enable_metadata_store = enable_metadata_store
        if enable_metadata_store:
            if metadata_store is not None:
                self._metadata_store = metadata_store
            elif FIRESTORE_AVAILABLE:
                try:
                    self._metadata_store = create_metadata_store("firestore")
                except Exception as e:
                    logger.warning(f"Failed to create Firestore metadata store: {e}")

        # Blob Storage (object storage)
        self._blob_storage: Optional[Any] = None
        self._enable_blob_storage = enable_blob_storage
        if enable_blob_storage:
            if blob_storage is not None:
                self._blob_storage = blob_storage
            elif GCS_AVAILABLE:
                try:
                    self._blob_storage = create_blob_storage("gcs")
                except Exception as e:
                    logger.warning(f"Failed to create GCS blob storage: {e}")

        # Query Engine (SQL with resilience)
        self._query_engine: Optional[Any] = None
        self._enable_query_engine = enable_query_engine
        if enable_query_engine:
            if query_engine is not None:
                self._query_engine = query_engine
            elif DUCKDB_AVAILABLE:
                try:
                    self._query_engine = create_query_engine("duckdb")
                except Exception as e:
                    logger.warning(f"Failed to create DuckDB query engine: {e}")

        # Vector Index (vector similarity search)
        self._vector_index: Optional[Any] = None
        self._enable_vector_index = enable_vector_index
        if enable_vector_index:
            if vector_index is not None:
                self._vector_index = vector_index
            elif DUCKDB_VSS_AVAILABLE:
                try:
                    self._vector_index = create_vector_index("duckdb")
                except Exception as e:
                    logger.warning(f"Failed to create DuckDB VSS vector index: {e}")

        # Embedding function for vector search
        self._embedding_fn = embedding_fn

        # TEA-LUA.P1: Lua runtime integration
        self._lua_enabled = lua_enabled
        self._lua_timeout = lua_timeout
        self._lua_runtime: Optional[Any] = None  # Lazy-initialized (main thread only)

        # TEA-PROLOG: Prolog runtime integration
        self._prolog_enabled = prolog_enabled
        self._prolog_timeout = prolog_timeout
        self._prolog_sandbox = prolog_sandbox
        self._prolog_runtime: Optional[Any] = None  # Lazy-initialized

        # TEA-BUILTIN-012.1: Secrets backend (initialized from settings.secrets)
        self._secrets_backend: Optional[Any] = None

        # TEA-BUILTIN-015.1: Session backend and settings (initialized from settings.session)
        self._session_backend: Optional[Any] = None
        self._session_settings: Optional[Any] = None

        # TEA-BUILTIN-015.2: Firestore settings (initialized from settings.firestore)
        self._firestore_settings: Optional[Dict[str, Any]] = None

        # TEA-BUILTIN-015.7: Endpoint configuration (initialized from endpoint section)
        self._endpoint_config: Optional[Any] = None

        # Opik LLM tracing flag (TEA-BUILTIN-005.2) - opt-in native Opik instrumentation
        self._opik_llm_tracing = opik_llm_tracing

        self.actions_registry = build_actions_registry(self)
        if actions_registry:
            self.actions_registry.update(actions_registry)

        self.variables: Dict[str, Any] = {}
        self.secrets: Dict[str, Any] = {}
        self.data: Dict[str, Any] = {}
        self.llm_settings: Dict[str, Any] = {}  # Default LLM settings from settings.llm
        self.shell_providers: Dict[str, Any] = (
            {}
        )  # Shell CLI providers from settings.llm.shell_providers

        # Checkpoint tracking
        self._last_checkpoint_path: Optional[str] = None
        self._current_graph: Optional[StateGraph] = None
        self._checkpoint_dir: Optional[str] = None
        self._checkpointer: Optional[Any] = None

        # TEA-PY-008.4: Import loader handles external modules (tracks loaded modules internally)
        self._import_loader = ImportLoader(self)

        # TEA-YAML-001: Initialize Jinja2 environment for template processing
        self._jinja_env = Environment(
            loader=BaseLoader(),
            undefined=StrictUndefined,
            keep_trailing_newline=True,
        )

        # Custom filter: safe fromjson that returns input on parse error
        def safe_fromjson(value):
            """Parse JSON string, returning original value on error."""
            try:
                return json.loads(value)
            except (json.JSONDecodeError, TypeError):
                return value

        # Add custom filters
        self._jinja_env.filters["fromjson"] = safe_fromjson
        # Map legacy filter names to Jinja2 equivalents
        self._jinja_env.filters["json"] = lambda v: json.dumps(v)

        # TEA-BUILTIN-010: Register cache-related Jinja filters (sha256)
        register_cache_jinja_filters(self._jinja_env)

        # TEA-PY-008.1: Initialize template processor with Jinja2 environment
        # Template cache is now managed by TemplateProcessor
        # Pass engine reference for dynamic variable/secret access
        self._template_processor = TemplateProcessor(
            jinja_env=self._jinja_env,
            variables=self.variables,
            secrets=self.secrets,
            engine=self,
        )

        # TEA-PY-008.2: Initialize node factory for node creation
        self._node_factory = NodeFactory(self)

        # TEA-PY-008.3: Initialize edge factory for edge/goto processing
        self._edge_factory = EdgeFactory(self)

        # TEA-OBS-001.1: Observability context (flow-scoped logging)
        # Initialized in load_from_dict() when observability config is present
        self._observability_context: Optional[ObservabilityContext] = None
        self._enable_observability = False

    @property
    def memory_backend(self) -> Any:
        """
        Get the memory backend instance.

        Returns:
            The current memory backend (InMemoryBackend by default, or custom).
        """
        return self._memory_backend

    @property
    def ltm_backend(self) -> Optional[Any]:
        """
        Get the long-term memory backend instance.

        Returns:
            The current LTM backend (SQLiteBackend by default, or custom).
            None if LTM is disabled.
        """
        return self._ltm_backend

    @property
    def graph_backend(self) -> Optional[Any]:
        """
        Get the graph database backend instance.

        Returns:
            The current graph backend (CozoBackend if available, or custom).
            None if graph is disabled or CozoDB is not installed.
        """
        return self._graph_backend

    # TEA-BUILTIN-006: Firebase Agent Memory Infrastructure properties

    @property
    def metadata_store(self) -> Optional[Any]:
        """
        Get the metadata store instance.

        Returns:
            The current MetadataStore (FirestoreMetadataStore if available, or custom).
            None if metadata store is disabled or no provider is available.
        """
        return self._metadata_store

    @property
    def blob_storage(self) -> Optional[Any]:
        """
        Get the blob storage instance.

        Returns:
            The current BlobStorage (GCSBlobStorage if available, or custom).
            None if blob storage is disabled or no provider is available.
        """
        return self._blob_storage

    @property
    def query_engine(self) -> Optional[Any]:
        """
        Get the query engine instance.

        Returns:
            The current QueryEngine (DuckDBQueryEngine if available, or custom).
            None if query engine is disabled or DuckDB is not installed.
        """
        return self._query_engine

    @property
    def vector_index(self) -> Optional[Any]:
        """
        Get the vector index instance.

        Returns:
            The current VectorIndex (DuckDBVSSIndex if available, or custom).
            None if vector index is disabled or DuckDB VSS is not installed.
        """
        return self._vector_index

    @property
    def embedding_fn(self) -> Optional[Callable[[str], List[float]]]:
        """
        Get the embedding function.

        Returns:
            The current embedding function for generating embeddings from text.
            None if no embedding function is configured.
        """
        return self._embedding_fn

    @property
    def opik_llm_tracing(self) -> bool:
        """
        Check if native Opik LLM tracing is enabled.

        When True, llm.call and llm.stream will wrap OpenAI clients with
        track_openai() for rich LLM telemetry.

        Returns:
            True if Opik LLM tracing is enabled, False otherwise.
        """
        return self._opik_llm_tracing

    @property
    def opik_config(self) -> Dict[str, Any]:
        """
        Get the resolved Opik configuration (TEA-BUILTIN-005.3).

        Returns a dictionary with all Opik settings after applying
        the precedence hierarchy:
        1. Constructor parameters (highest priority)
        2. Environment variables
        3. YAML settings
        4. Defaults (lowest priority)

        Returns:
            Dictionary with keys: enabled, api_key, workspace, project_name,
            url, llm_tracing, trace_export.

        Example:
            >>> engine = YAMLEngine(opik_project_name="my-project")
            >>> print(engine.opik_config)
            {'enabled': False, 'api_key': None, 'workspace': None,
             'project_name': 'my-project', 'url': None,
             'llm_tracing': False, 'trace_export': False}
        """
        return self._opik_config.copy()

    @property
    def observability_context(self) -> Optional[ObservabilityContext]:
        """
        Get the observability context instance (TEA-OBS-001.1).

        The observability context provides flow-scoped logging with structured
        events and configurable handlers. It wraps TraceContext to add:
        - Flow-level event aggregation
        - Ring buffer for bounded log retention
        - Structured log schema for cross-runtime parity

        Returns:
            The current ObservabilityContext, or None if observability is disabled.

        Example:
            >>> engine = YAMLEngine()
            >>> graph = engine.load_from_dict({
            ...     'observability': {'enabled': True, 'level': 'info'},
            ...     'nodes': [...], 'edges': [...]
            ... })
            >>> if engine.observability_context:
            ...     flow_log = engine.observability_context.get_flow_log()
            ...     print(f"Events: {flow_log['metrics']['event_count']}")
        """
        return self._observability_context

    @property
    def endpoint_config(self) -> Optional[Any]:
        """
        Get the endpoint configuration (TEA-BUILTIN-015.7).

        The endpoint config defines how this agent should be exposed as an HTTP API,
        including path, method, parameters, and authentication requirements.

        Returns:
            The current EndpointConfig, or None if not configured.

        Example:
            >>> engine = YAMLEngine()
            >>> graph = engine.load_from_dict({
            ...     'name': 'research_agent',
            ...     'endpoint': {'path': '/api/v1/research', 'method': 'POST'},
            ...     'nodes': [...], 'edges': [...]
            ... })
            >>> if engine.endpoint_config:
            ...     print(f"Endpoint: {engine.endpoint_config.method} {engine.endpoint_config.path}")
        """
        return self._endpoint_config

    def _resolve_opik_config(
        self, yaml_settings: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Resolve Opik configuration with proper precedence hierarchy.

        TEA-PY-008.5: Delegates to EngineConfig for modularity.

        Args:
            yaml_settings: Optional YAML settings.opik section.

        Returns:
            Resolved configuration dictionary.
        """
        return self._engine_config.resolve_opik_config(yaml_settings)

    def _add_opik_exporter_from_config(self) -> None:
        """
        Add an OpikExporter to the trace context using resolved config.

        TEA-PY-008.5: Delegates to EngineConfig for modularity.
        """
        self._engine_config.add_opik_exporter_from_config()

    def _configure_memory_infrastructure(self, config: Dict[str, Any]) -> None:
        """
        Configure Firebase Agent Memory Infrastructure from YAML settings.

        TEA-PY-008.5: Delegates to EngineConfig for modularity.

        Args:
            config: Memory infrastructure configuration dict.
        """
        self._engine_config.configure_memory_infrastructure(config)

    def close(self) -> None:
        """
        Close all backends and release resources.

        TEA-PY-008.5: Delegates to EngineConfig for modularity.
        Should be called when the engine is no longer needed.
        Safe to call multiple times.
        """
        self._engine_config.close()

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass

    def get_memory_state(self) -> Dict[str, Any]:
        """
        Get serializable memory state for checkpoint persistence.

        TEA-PY-008.5: Delegates to EngineConfig for modularity.
        """
        return self._engine_config.get_memory_state()

    def restore_memory_state(self, state: Dict[str, Any]) -> None:
        """
        Restore memory state from checkpoint.

        TEA-PY-008.5: Delegates to EngineConfig for modularity.
        """
        self._engine_config.restore_memory_state(state)

    def clear_memory(self, namespace: Optional[str] = None) -> int:
        """
        Clear memory, optionally within a specific namespace.

        Args:
            namespace: If provided, only clear this namespace.
                      If None, clear all namespaces.

        Returns:
            Number of keys cleared.
        """
        return self._memory_backend.clear(namespace)

    def load_from_file(
        self,
        yaml_path: str,
        checkpoint: Optional[str] = None,
        checkpointer: Optional[Any] = None,
    ) -> StateGraph:
        """
        Load a StateGraph from a YAML file.

        Args:
            yaml_path: Path to the YAML configuration file
            checkpoint: Optional path to checkpoint file to resume from.
                If provided, overrides config.checkpoint in YAML.
            checkpointer: Optional checkpointer instance (e.g., MemoryCheckpointer)
                for in-memory checkpoint storage. Required when using interrupts
                without checkpoint_dir.

        Returns:
            Compiled StateGraph instance. If checkpoint is provided,
            the graph is configured to resume from that checkpoint
            when invoke() or stream() is called.

        Example:
            >>> from the_edge_agent import MemoryCheckpointer
            >>> engine = YAMLEngine()
            >>> # With file-based checkpoints
            >>> graph = engine.load_from_file("agent.yaml")
            >>> # With in-memory checkpointer
            >>> graph = engine.load_from_file("agent.yaml", checkpointer=MemoryCheckpointer())
            >>> # Resume from checkpoint
            >>> graph = engine.load_from_file("agent.yaml", checkpoint="./chk/node.pkl")
        """
        with open(yaml_path, "r") as f:
            config = yaml.safe_load(f)

        # Get directory of YAML file for relative path resolution
        yaml_dir = os.path.dirname(os.path.abspath(yaml_path))

        return self.load_from_dict(
            config, checkpoint=checkpoint, checkpointer=checkpointer, yaml_dir=yaml_dir
        )

    def load_from_dict(
        self,
        config: Dict[str, Any],
        checkpoint: Optional[str] = None,
        checkpointer: Optional[Any] = None,
        yaml_dir: Optional[str] = None,
    ) -> StateGraph:
        """
        Load a StateGraph from a configuration dictionary.

        Args:
            config: Configuration dictionary from YAML
            checkpoint: Optional path to checkpoint file to resume from.
                Overrides config['config']['checkpoint'] if provided.
            checkpointer: Optional checkpointer instance (e.g., MemoryCheckpointer)
                for in-memory checkpoint storage. Required when using interrupts
                without checkpoint_dir.
            yaml_dir: Optional directory of the YAML file for relative path
                resolution in imports. If None, uses current working directory.

        Returns:
            Compiled StateGraph instance. If checkpoint is provided (or
            config.checkpoint is set), the graph is configured to resume
            from that checkpoint when invoke() or stream() is called.

        Checkpoint precedence (highest to lowest):
            1. checkpoint parameter
            2. config['config']['checkpoint']
            3. None (normal execution)

        Example:
            >>> from the_edge_agent import MemoryCheckpointer
            >>> engine = YAMLEngine()
            >>> config = {
            ...     'config': {'checkpoint_dir': './checkpoints'},
            ...     'nodes': [...],
            ...     'edges': [...]
            ... }
            >>> graph = engine.load_from_dict(config, checkpointer=MemoryCheckpointer())
        """
        # Extract global variables
        self.variables = config.get("variables", {})

        # Extract data section (reusable prompts, templates, constants)
        self.data = config.get("data", {})

        # Load external action modules from imports section (TEA-BUILTIN: External Imports)
        imports = config.get("imports", [])
        if imports:
            self._load_imports(imports, yaml_dir)

        # Extract settings (YAML-level configuration)
        settings = config.get("settings", {})

        # TEA-KIROKU-005: Store full config for CLI access (interview prompts, etc.)
        self._config = config

        # Extract LLM settings for default injection into llm.call actions
        # Maps settings.llm.deployment -> model, settings.llm.* -> other params
        # Process templates at load time (e.g., {{ env.VAR | default('value') }})
        llm_config = settings.get("llm", {})
        if isinstance(llm_config, dict):
            self.llm_settings = {}
            # Map 'deployment' to 'model' for Azure OpenAI convention
            if "deployment" in llm_config:
                raw_value = llm_config["deployment"]
                # Process template with empty state (env vars available)
                self.llm_settings["model"] = self._process_template(raw_value, {})
            elif "model" in llm_config:
                raw_value = llm_config["model"]
                self.llm_settings["model"] = self._process_template(raw_value, {})
            # Copy other settings (provider, temperature, etc.)
            for key in [
                "provider",
                "temperature",
                "api_base",
                "timeout",
                "max_retries",
            ]:
                if key in llm_config:
                    raw_value = llm_config[key]
                    if isinstance(raw_value, str):
                        self.llm_settings[key] = self._process_template(raw_value, {})
                    else:
                        self.llm_settings[key] = raw_value

            # TEA-LLM-004: Parse shell_providers configuration
            # Allows configuring CLI commands like claude, gemini, qwen
            shell_providers_config = llm_config.get("shell_providers", {})
            if isinstance(shell_providers_config, dict):
                for provider_name, provider_config in shell_providers_config.items():
                    if isinstance(provider_config, dict):
                        processed_config = {}
                        for key, value in provider_config.items():
                            if isinstance(value, str):
                                # Process templates and expand env vars
                                processed_config[key] = self._process_template(
                                    value, {}
                                )
                            elif isinstance(value, list):
                                # Process list items (like args)
                                processed_config[key] = [
                                    (
                                        self._process_template(v, {})
                                        if isinstance(v, str)
                                        else v
                                    )
                                    for v in value
                                ]
                            elif isinstance(value, dict):
                                # Process dict items (like env)
                                processed_config[key] = {
                                    k: (
                                        self._process_template(v, {})
                                        if isinstance(v, str)
                                        else v
                                    )
                                    for k, v in value.items()
                                }
                            else:
                                processed_config[key] = value
                        self.shell_providers[provider_name] = processed_config

        # TEA-BUILTIN-011: Configure rate limiters from YAML settings
        rate_limiters_config = settings.get("rate_limiters", {})
        if isinstance(rate_limiters_config, dict) and rate_limiters_config:
            from .actions import configure_rate_limiters_from_settings

            configure_rate_limiters_from_settings(self, rate_limiters_config)

        # TEA-BUILTIN-005.3: Resolve Opik configuration from YAML settings
        # Settings can be nested under 'opik' key or flat under 'settings'
        opik_yaml_settings = settings.get("opik", {})
        if not isinstance(opik_yaml_settings, dict):
            opik_yaml_settings = {}

        # Re-resolve Opik config with YAML settings applied
        # TEA-PY-008.5: Delegate to EngineConfig for modularity
        self._opik_config = self._engine_config.resolve_opik_config(opik_yaml_settings)

        # Update llm_tracing flag from resolved config
        if self._opik_config.get("llm_tracing", False):
            self._opik_llm_tracing = True
        # Also check flat setting for backwards compatibility
        if settings.get("opik_llm_tracing", False):
            self._opik_llm_tracing = True

        # Handle auto-trace from YAML settings
        if settings.get("auto_trace", False) and self._enable_tracing:
            self._auto_trace = True
            # Configure trace exporter from settings if not already set
            if self._trace_context is not None and not self._trace_context.exporters:
                trace_exporter = settings.get("trace_exporter", "console")
                trace_file = settings.get("trace_file")
                if trace_exporter == "console":
                    self._trace_context.exporters.append(ConsoleExporter(verbose=False))
                elif trace_exporter == "file" and trace_file:
                    self._trace_context.exporters.append(FileExporter(trace_file))
                elif trace_exporter == "opik":
                    self._add_opik_exporter_from_config()
        else:
            self._auto_trace = False

        # TEA-BUILTIN-005.3: Add Opik exporter if trace_export is enabled in config
        if (
            self._opik_config.get("trace_export", False)
            and self._enable_tracing
            and self._trace_context is not None
        ):
            self._add_opik_exporter_from_config()

        # TEA-BUILTIN-006: Configure Firebase Agent Memory Infrastructure from YAML settings
        memory_infra = settings.get("memory_infrastructure", {})
        if memory_infra:
            self._configure_memory_infrastructure(memory_infra)

        # TEA-BUILTIN-001.6.4: Configure LTM backend from YAML settings
        ltm_settings = settings.get("ltm", {})
        if ltm_settings and self._enable_ltm:
            # Expand environment variables in LTM config
            ltm_settings = expand_env_vars(ltm_settings)
            backend_type = ltm_settings.get("backend")
            if backend_type:
                try:
                    # Close existing backend if different type requested
                    if self._ltm_backend is not None:
                        self._ltm_backend.close()
                    # Parse config using standard config parser
                    _, kwargs = parse_backend_config(
                        {"ltm_backend": backend_type, **ltm_settings}
                    )
                    # Create new backend from parsed YAML settings
                    self._ltm_backend = create_ltm_backend(backend_type, **kwargs)
                    self._ltm_backend_type = backend_type
                    logger.debug(f"Configured LTM backend from YAML: {backend_type}")
                except Exception as e:
                    logger.warning(f"Failed to configure LTM backend from YAML: {e}")

        # TEA-BUILTIN-012.3: Configure secrets backend from YAML settings
        secrets_settings = settings.get("secrets", {})
        if secrets_settings:
            secrets_settings = expand_env_vars(secrets_settings)
            backend_type = secrets_settings.get("backend", "env")
            try:
                from .secrets import create_secrets_backend

                # Extract backend-specific kwargs from provider-specific section
                # e.g., settings.secrets.aws -> {region: ..., secret_name: ...}
                kwargs = {}
                if backend_type in secrets_settings and isinstance(
                    secrets_settings[backend_type], dict
                ):
                    kwargs = secrets_settings[backend_type]
                else:
                    # Fallback: use flat kwargs (exclude 'backend' and provider sections)
                    provider_keys = {"backend", "aws", "azure", "gcp", "vault", "env"}
                    kwargs = {
                        k: v
                        for k, v in secrets_settings.items()
                        if k not in provider_keys
                    }
                self._secrets_backend = create_secrets_backend(backend_type, **kwargs)
                # Populate self.secrets from backend for template access
                self.secrets = self._secrets_backend.get_all()
                logger.debug(f"Configured secrets backend from YAML: {backend_type}")
            except Exception as e:
                logger.warning(f"Failed to configure secrets backend from YAML: {e}")

        # TEA-BUILTIN-015.1: Configure session backend from YAML settings
        session_config = settings.get("session", {})
        if session_config:
            try:
                from .session import (
                    SessionSettings,
                    create_session_backend_from_settings,
                )

                # Parse and validate session settings
                self._session_settings = SessionSettings(**session_config)
                # Create session backend based on settings
                self._session_backend = create_session_backend_from_settings(
                    self._session_settings
                )
                logger.debug(
                    f"Configured session backend from YAML: {self._session_settings.backend}"
                )
            except Exception as e:
                logger.warning(f"Failed to configure session backend from YAML: {e}")

        # TEA-BUILTIN-015.2: Configure Firestore settings from YAML
        firestore_settings = settings.get("firestore", {})
        if firestore_settings:
            # Expand environment variables in Firestore config
            firestore_settings = expand_env_vars(firestore_settings)
            # Store settings for firestore actions to access
            self._firestore_settings = {
                "project": firestore_settings.get("project"),
                "emulator_host": firestore_settings.get("emulator_host"),
                "credentials_path": firestore_settings.get("credentials_path"),
            }
            logger.debug(
                f"Configured Firestore settings from YAML: "
                f"project={self._firestore_settings.get('project')}"
            )

        # TEA-BUILTIN-001.8: Configure graph backend from YAML settings
        graph_settings = settings.get("graph", {})
        if graph_settings and self._enable_graph:
            backend_type = graph_settings.get("backend")
            if backend_type and backend_type != self._graph_backend_type:
                lazy = graph_settings.get("lazy", True)
                graph_path = graph_settings.get("path", ":memory:")
                try:
                    # Close existing backend if different type requested
                    if self._graph_backend is not None and hasattr(
                        self._graph_backend, "close"
                    ):
                        self._graph_backend.close()
                    # Create new backend based on type
                    if backend_type == "duckpgq":
                        if DUCKPGQ_AVAILABLE:
                            from .memory import DuckPGQBackend

                            self._graph_backend = DuckPGQBackend(
                                graph_path, lazy_load_extension=lazy
                            )
                            self._graph_backend_type = backend_type
                            logger.debug(
                                f"Configured graph backend from YAML: {backend_type}"
                            )
                        else:
                            logger.warning("DuckPGQ requested but duckdb not installed")
                    elif backend_type in ("kuzu", "bighorn"):
                        if KUZU_AVAILABLE:
                            from .memory import KuzuBackend

                            self._graph_backend = KuzuBackend(graph_path)
                            self._graph_backend_type = backend_type
                            logger.debug(
                                f"Configured graph backend from YAML: {backend_type}"
                            )
                        else:
                            logger.warning(
                                f"{backend_type} requested but kuzu not installed"
                            )
                    elif backend_type == "cozo":
                        if COZO_AVAILABLE:
                            from .memory import CozoBackend

                            self._graph_backend = CozoBackend(graph_path)
                            self._graph_backend_type = backend_type
                            logger.debug(
                                f"Configured graph backend from YAML: {backend_type}"
                            )
                        else:
                            logger.warning("cozo requested but pycozo not installed")
                    else:
                        logger.warning(f"Unknown graph backend type: {backend_type}")
                except Exception as e:
                    logger.warning(f"Failed to configure graph backend from YAML: {e}")

        # TEA-OBS-001.1: Configure ObservabilityContext from YAML settings
        observability_config = config.get("observability", {})
        if observability_config.get("enabled", False):
            self._enable_observability = True
            import uuid

            flow_id = str(uuid.uuid4())
            self._observability_context = ObservabilityContext(
                flow_id=flow_id,
                config=observability_config,
                trace_context=self._trace_context,
            )
        else:
            self._enable_observability = False
            self._observability_context = None

        # TEA-YAML-004: Parse extraction validation configuration
        self._extraction_validation_config = {}
        extraction_schema = config.get("extraction_schema")
        if extraction_schema:
            self._extraction_validation_config["extraction_schema"] = extraction_schema
            # Validate schema at load time (AC: 20)
            try:
                from .extraction_validation import ExtractionSchema

                ExtractionSchema.from_dict(extraction_schema)
            except ValueError as e:
                raise ValueError(f"Invalid extraction_schema: {e}")

        validation_constraints = config.get("validation_constraints")
        if validation_constraints:
            self._extraction_validation_config["validation_constraints"] = (
                validation_constraints
            )
            # Validate Prolog syntax at load time (AC: 21)
            language = validation_constraints.get("language", "prolog")
            if language != "prolog":
                raise ValueError(
                    f"validation_constraints language must be 'prolog', got '{language}'"
                )
            rules = validation_constraints.get("rules", "")
            if rules and self._prolog_enabled:
                # Validate Prolog syntax by attempting to parse
                try:
                    from .prolog_runtime import PrologRuntime, JANUS_AVAILABLE

                    if JANUS_AVAILABLE:
                        # Just check syntax, don't execute
                        pass  # Full validation deferred to runtime
                except Exception as e:
                    raise ValueError(
                        f"Invalid Prolog syntax in validation_constraints: {e}"
                    )

        semantic_probes = config.get("semantic_probes")
        if semantic_probes:
            self._extraction_validation_config["semantic_probes"] = semantic_probes
            # Validate probe templates at load time
            for idx, probe in enumerate(semantic_probes):
                if not probe.get("for_each"):
                    raise ValueError(
                        f"semantic_probes[{idx}] missing required 'for_each' field"
                    )
                if not probe.get("probe"):
                    raise ValueError(
                        f"semantic_probes[{idx}] missing required 'probe' field"
                    )

        validation_logging = config.get("validation_logging")
        if validation_logging:
            self._extraction_validation_config["validation_logging"] = (
                validation_logging
            )

        # TEA-YAML-004: Store extraction_prompt template variable if guide_extraction is enabled
        if extraction_schema and extraction_schema.get("guide_extraction"):
            from .extraction_validation import (
                ExtractionSchema,
                ValidationConstraints,
                generate_extraction_prompt,
            )

            schema_obj = ExtractionSchema.from_dict(extraction_schema)
            constraints_obj = None
            if validation_constraints:
                constraints_obj = ValidationConstraints.from_dict(
                    validation_constraints
                )
            prompt = generate_extraction_prompt(
                schema_obj, constraints_obj, schema_obj.confidence_tracking
            )
            self.variables["extraction_prompt"] = prompt

        # TEA-BUILTIN-015.5: Parse output_schema for response transformation
        output_schema_config = config.get("output_schema")
        self._output_schema = None
        if output_schema_config:
            from .transformation import OutputSchema

            self._output_schema = OutputSchema.from_yaml(output_schema_config)
            logger.debug(
                f"Configured output_schema with {len(self._output_schema.fields)} fields"
            )

        # TEA-BUILTIN-015.4: Parse input_schema for request validation
        input_schema_config = config.get("input_schema")
        self._input_schema = None
        if input_schema_config:
            from .validation import InputSchema

            self._input_schema = InputSchema.from_yaml(input_schema_config)
            logger.debug(
                f"Configured input_schema with {len(self._input_schema.fields)} fields"
            )

        # TEA-BUILTIN-015.7: Parse endpoint configuration for HTTP API
        endpoint_config_yaml = config.get("endpoint")
        self._endpoint_config = None
        if endpoint_config_yaml:
            from .http import EndpointConfig

            self._endpoint_config = EndpointConfig.from_yaml(endpoint_config_yaml)
            logger.debug(
                f"Configured endpoint: {self._endpoint_config.method} {self._endpoint_config.path}"
            )

        # TEA-BUILTIN-015.6: Parse error_handling settings
        error_handling_config = settings.get("error_handling", {})
        self._error_handling_settings = None
        if error_handling_config:
            from .error_handling import ErrorHandlingSettings

            self._error_handling_settings = ErrorHandlingSettings.from_yaml(
                error_handling_config
            )
            logger.debug(
                f"Configured error_handling: mode={self._error_handling_settings.mode.value}, "
                f"max_retries={self._error_handling_settings.max_retries}"
            )
        else:
            # Default settings (raise mode for backward compatibility)
            from .error_handling import ErrorHandlingSettings

            self._error_handling_settings = ErrorHandlingSettings.default()

        # Create graph
        compile_config = config.get("config", {})
        graph = StateGraph(
            state_schema=config.get("state_schema", {}),
            raise_exceptions=compile_config.get("raise_exceptions", False),
        )

        # Store reference for checkpoint actions
        self._current_graph = graph

        # Extract checkpoint configuration
        checkpoint_dir = compile_config.get("checkpoint_dir")
        self._checkpoint_dir = checkpoint_dir

        # Add nodes and collect inline interrupt definitions
        nodes_list = config.get("nodes", [])
        interrupt_before_nodes = []
        interrupt_after_nodes = []
        for node_config in nodes_list:
            self._add_node_from_config(graph, node_config)
            # TEA-KIROKU-005: Collect inline interrupt: before/after from node definitions
            node_name = node_config.get("name")
            interrupt_type = node_config.get("interrupt")
            if interrupt_type == "before" and node_name:
                interrupt_before_nodes.append(node_name)
            elif interrupt_type == "after" and node_name:
                interrupt_after_nodes.append(node_name)

        # TEA-YAML-002: Get edges list to determine which nodes are covered by legacy edges
        edges_list = config.get("edges", [])

        # TEA-YAML-002: Process goto and implicit chaining
        # This implements the precedence: goto > edges > implicit chaining
        # Only add implicit edges for nodes that don't have goto AND don't have edges
        self._process_goto_and_implicit_edges(graph, nodes_list, edges_list)

        # TEA-YAML-002: Add legacy edges (deprecated) with warning
        if edges_list:
            # Emit deprecation warning at INFO level (Phase 1: Soft Deprecation)
            logger.info(
                "DEPRECATION WARNING: The 'edges' section is deprecated and will be "
                "removed in v2.0. Use 'goto' property on nodes or implicit chaining instead. "
                "See: https://github.com/terminusdb-labs/the_edge_agent/docs/migration/edges-to-goto.md"
            )
            for edge_config in edges_list:
                self._add_edge_from_config(graph, edge_config)

        # Store checkpointer reference for resume
        self._checkpointer = checkpointer

        # TEA-KIROKU-005: Merge inline interrupt definitions with config section
        # Inline node-level 'interrupt: before/after' takes precedence by appearing first
        all_interrupt_before = interrupt_before_nodes + compile_config.get(
            "interrupt_before", []
        )
        all_interrupt_after = interrupt_after_nodes + compile_config.get(
            "interrupt_after", []
        )

        # TEA-KIROKU-005: Auto-provide checkpoint directory when interrupts are defined
        # This ensures HITL workflows work out-of-the-box without explicit checkpoint config
        has_interrupts = bool(all_interrupt_before or all_interrupt_after)
        has_checkpointer = bool(checkpoint_dir or checkpointer)
        effective_checkpoint_dir = checkpoint_dir
        if has_interrupts and not has_checkpointer:
            import tempfile

            effective_checkpoint_dir = tempfile.mkdtemp(prefix="tea_checkpoint_")
            self._checkpoint_dir = effective_checkpoint_dir
            logger.info(
                f"Auto-created checkpoint directory for interrupts: {effective_checkpoint_dir}"
            )

        # Compile with checkpoint support
        compiled_graph = graph.compile(
            interrupt_before=all_interrupt_before,
            interrupt_after=all_interrupt_after,
            checkpoint_dir=effective_checkpoint_dir,
            checkpointer=checkpointer,
        )

        # Determine checkpoint path (parameter overrides config)
        checkpoint_path = checkpoint or compile_config.get("checkpoint")

        if checkpoint_path:
            # Store checkpoint path for resume
            compiled_graph._resume_checkpoint_path = checkpoint_path

        # TEA-BUILTIN-015.1: Attach session backend and settings for auto-save
        if self._session_backend is not None:
            compiled_graph._session_backend = self._session_backend
            compiled_graph._session_settings = self._session_settings

        # TEA-BUILTIN-015.5: Attach output_schema and engine reference for auto-application
        if self._output_schema is not None:
            compiled_graph._output_schema = self._output_schema
            compiled_graph._yaml_engine = self

        # TEA-BUILTIN-015.4: Attach input_schema for request validation
        if self._input_schema is not None:
            compiled_graph._input_schema = self._input_schema
            compiled_graph._yaml_engine = self

        # TEA-BUILTIN-015.7: Attach endpoint configuration for HTTP API
        if self._endpoint_config is not None:
            compiled_graph._endpoint_config = self._endpoint_config
            # Store agent name from config for route registration
            compiled_graph._agent_name = config.get("name", "unnamed_agent")

        return compiled_graph

    def resume_from_checkpoint(
        self,
        yaml_path: str,
        checkpoint_path: str,
        config: Optional[Dict[str, Any]] = None,
    ) -> Generator[Dict[str, Any], None, None]:
        """
        Load YAML config, create graph, and resume execution from checkpoint.

        This is a convenience method that:
        1. Loads the YAML configuration to get graph structure
        2. Creates and compiles the StateGraph
        3. Resumes execution from the checkpoint

        Args:
            yaml_path: Path to YAML configuration file (defines graph structure)
            checkpoint_path: Path to checkpoint file to resume from
            config: Optional config overrides to merge with checkpoint config

        Yields:
            Events during execution (same as graph.invoke()):
                - {"type": "interrupt", "node": str, "state": dict}
                - {"type": "error", "node": str, "error": str, "state": dict}
                - {"type": "final", "state": dict}

        Example:
            >>> engine = YAMLEngine()
            >>> for event in engine.resume_from_checkpoint(
            ...     "agent.yaml",
            ...     "./checkpoints/node_a_1733500000.pkl",
            ...     config={"new_param": "value"}
            ... ):
            ...     print(event)
        """
        # Load YAML to get graph structure
        graph = self.load_from_file(yaml_path)

        # Resume from checkpoint
        yield from graph.resume_from_checkpoint(checkpoint_path, config)

    def _add_node_from_config(
        self, graph: StateGraph, node_config: Dict[str, Any]
    ) -> None:
        """
        Add a node to the graph from configuration.

        TEA-PY-008.2: Delegates to NodeFactory for modularity.

        See NodeFactory.add_node_from_config() for full documentation.
        """
        return self._node_factory.add_node_from_config(graph, node_config)

    def _create_run_function(self, node_config: Dict[str, Any]) -> Optional[Callable]:
        """
        Create a run function from node configuration.

        TEA-PY-008.2: Delegates to NodeFactory for modularity.

        See NodeFactory.create_run_function() for full documentation.
        """
        return self._node_factory.create_run_function(node_config)

    def _get_lua_runtime(self):
        """
        Get or create the Lua runtime with parallel isolation.

        TEA-PY-008.2: Delegates to NodeFactory for modularity.
        Kept for backward compatibility with tests that access this directly.

        See NodeFactory._get_lua_runtime() for full documentation.
        """
        return self._node_factory._get_lua_runtime()

    def _get_prolog_runtime(self):
        """
        Get or create the Prolog runtime.

        TEA-PY-008.2: Delegates to NodeFactory for modularity.
        Kept for backward compatibility with tests that access this directly.

        See NodeFactory._get_prolog_runtime() for full documentation.
        """
        return self._node_factory._get_prolog_runtime()

    def _create_action_function(
        self,
        action_name: str,
        params: Dict[str, Any],
        output_key: Optional[str] = None,
    ) -> Callable:
        """
        Create a function that calls a built-in action.

        TEA-PY-008.2: Delegates to NodeFactory for modularity.
        Kept for backward compatibility with tests that access this directly.

        See NodeFactory._create_action_function() for full documentation.
        """
        return self._node_factory._create_action_function(
            action_name, params, output_key
        )

    def _process_goto_and_implicit_edges(
        self,
        graph: StateGraph,
        nodes_list: List[Dict[str, Any]],
        edges_list: List[Dict[str, Any]],
    ) -> None:
        """
        Process goto properties and implicit chaining for nodes.

        TEA-PY-008.3: Delegates to EdgeFactory for modularity.
        TEA-YAML-002: Implements the new implicit graph navigation syntax.

        See EdgeFactory.process_goto_and_implicit_edges() for full documentation.
        """
        self._edge_factory.process_goto_and_implicit_edges(
            graph, nodes_list, edges_list
        )
        # Copy nodes_with_goto for backward compatibility
        self._nodes_with_goto = self._edge_factory.nodes_with_goto

    def _add_edge_from_config(
        self, graph: StateGraph, edge_config: Dict[str, Any]
    ) -> None:
        """
        Add an edge to the graph from configuration.

        TEA-PY-008.3: Delegates to EdgeFactory for modularity.
        TEA-YAML-002: Respects precedence (goto > edges > implicit).

        See EdgeFactory.add_edge_from_config() for full documentation.
        """
        self._edge_factory.add_edge_from_config(graph, edge_config)

    @property
    def _template_cache(self) -> Dict[str, Any]:
        """
        Template cache for backward compatibility.

        TEA-PY-008.1: Exposes TemplateProcessor's cache for code that
        accesses engine._template_cache directly.
        """
        return self._template_processor._template_cache

    def _process_template(
        self,
        text: str,
        state: Dict[str, Any],
        extra_context: Optional[Dict[str, Any]] = None,
    ) -> Any:
        """
        Process template variables in text using Jinja2.

        TEA-PY-008.1: Delegates to TemplateProcessor for modularity.

        See TemplateProcessor.process_template() for full documentation.

        Args:
            text: The template string to process
            state: Current state dictionary
            extra_context: Optional additional context variables (e.g., result from action)
        """
        return self._template_processor.process_template(
            text,
            state,
            checkpoint_dir=self._checkpoint_dir,
            last_checkpoint=self._last_checkpoint_path,
            extra_context=extra_context,
        )

    def _process_params(
        self, params: Dict[str, Any], state: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Recursively process parameters, replacing template variables.

        TEA-PY-008.1: Delegates to TemplateProcessor for modularity.
        """
        return self._template_processor.process_params(
            params,
            state,
            checkpoint_dir=self._checkpoint_dir,
            last_checkpoint=self._last_checkpoint_path,
        )

    def _evaluate_condition(self, expr: str, state: Dict[str, Any]) -> bool:
        """
        Evaluate a condition expression using Jinja2.

        TEA-PY-008.1: Delegates to TemplateProcessor for modularity.

        See TemplateProcessor.evaluate_condition() for full documentation.
        """
        return self._template_processor.evaluate_condition(expr, state)

    def _convert_simple_expression(self, expr: str) -> str:
        """
        Convert simple expression syntax to Python.

        DEPRECATED: Use _evaluate_condition with Jinja2 instead (TEA-YAML-001).
        TEA-PY-008.1: Delegates to TemplateProcessor for modularity.
        """
        return self._template_processor._convert_simple_expression(expr)

    def _load_imports(
        self, imports: List[Dict[str, Any]], yaml_dir: Optional[str] = None
    ) -> None:
        """
        Load external action modules from the imports section.

        TEA-PY-008.4: Delegates to ImportLoader for modularity.

        See ImportLoader.load_imports() for full documentation.
        """
        self._import_loader.load_imports(imports, yaml_dir)

    def _load_from_path(
        self, path: str, namespace: str, yaml_dir: Optional[str] = None
    ) -> None:
        """
        Load actions from a local Python file.

        TEA-PY-008.4: Delegates to ImportLoader for modularity.
        """
        self._import_loader._load_from_path(path, namespace, yaml_dir)

    def _load_from_package(self, package: str, namespace: str) -> None:
        """
        Load actions from an installed Python package.

        TEA-PY-008.4: Delegates to ImportLoader for modularity.
        """
        self._import_loader._load_from_package(package, namespace)

    @property
    def _loaded_modules(self) -> Set[str]:
        """
        Set of loaded module identifiers for backward compatibility.

        TEA-PY-008.4: Exposes ImportLoader's loaded modules set.
        """
        return self._import_loader._loaded_modules

    def _load_subgraph(self, path: str) -> "StateGraph":
        """
        Load a subgraph from a local or remote path using fsspec.

        TEA-YAML-006: Subgraph loading for dynamic_parallel nodes.

        Args:
            path: Path to the subgraph YAML file. Supports:
                  - Local paths: './subgraph.yaml', '/abs/path/subgraph.yaml'
                  - S3: 's3://bucket/path/subgraph.yaml' (requires s3fs)
                  - GCS: 'gs://bucket/path/subgraph.yaml' (requires gcsfs)
                  - Azure: 'az://container/path/subgraph.yaml' (requires adlfs)
                  - HTTP: 'https://example.com/subgraph.yaml' (requires aiohttp)

        Returns:
            Compiled StateGraph instance from the subgraph YAML.

        Raises:
            FileNotFoundError: If the subgraph file does not exist.
            ValueError: If the subgraph YAML is invalid.

        Example:
            >>> engine = YAMLEngine()
            >>> subgraph = engine._load_subgraph('./agents/researcher.yaml')
            >>> # Or from S3:
            >>> subgraph = engine._load_subgraph('s3://my-bucket/agents/researcher.yaml')
        """
        from .actions.core_actions import _get_filesystem

        # Get filesystem and path
        fs, fs_path, err = _get_filesystem(path)
        if err:
            raise FileNotFoundError(
                f"Subgraph not found: {path} - {err.get('error', 'Unknown error')}"
            )

        try:
            # Read YAML content
            with fs.open(fs_path, "r") as f:
                content = f.read()
        except FileNotFoundError:
            raise FileNotFoundError(f"Subgraph not found: {path}")
        except Exception as e:
            raise ValueError(f"Failed to load subgraph '{path}': {e}") from e

        # Parse YAML
        config = yaml.safe_load(content)
        if not config:
            raise ValueError(f"Empty subgraph configuration: {path}")

        # Get directory for relative imports within subgraph
        if path.startswith(("s3://", "gs://", "az://", "http://", "https://")):
            # For remote paths, use parent directory
            subgraph_dir = "/".join(path.rsplit("/", 1)[:-1]) if "/" in path else ""
        else:
            subgraph_dir = os.path.dirname(os.path.abspath(path))

        # Load subgraph using same engine settings
        return self.load_from_dict(config, yaml_dir=subgraph_dir)
