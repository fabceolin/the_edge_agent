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


class DotDict(dict):
    """Dictionary subclass that allows attribute-style access to keys."""

    def __getattr__(self, key):
        try:
            value = self[key]
            if isinstance(value, dict) and not isinstance(value, DotDict):
                return DotDict(value)
            return value
        except KeyError:
            raise AttributeError(
                f"'{type(self).__name__}' object has no attribute '{key}'"
            )

    def __setattr__(self, key, value):
        self[key] = value


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
        # Initial config resolution (without YAML settings yet)
        self._opik_config: Dict[str, Any] = self._resolve_opik_config()

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

        # Opik LLM tracing flag (TEA-BUILTIN-005.2) - opt-in native Opik instrumentation
        self._opik_llm_tracing = opik_llm_tracing

        self.actions_registry = build_actions_registry(self)
        if actions_registry:
            self.actions_registry.update(actions_registry)

        self.variables: Dict[str, Any] = {}
        self.secrets: Dict[str, Any] = {}

        # Checkpoint tracking
        self._last_checkpoint_path: Optional[str] = None
        self._current_graph: Optional[StateGraph] = None
        self._checkpoint_dir: Optional[str] = None
        self._checkpointer: Optional[Any] = None

        # Track loaded external modules to detect circular imports
        self._loaded_modules: Set[str] = set()

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

        # Template cache for performance (AC: 8)
        self._template_cache: Dict[str, Any] = {}

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

    def _resolve_opik_config(
        self, yaml_settings: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Resolve Opik configuration with proper precedence hierarchy.

        Precedence (highest to lowest):
        1. Constructor parameters
        2. Environment variables
        3. YAML settings
        4. Defaults

        Args:
            yaml_settings: Optional YAML settings.opik section.
                          If None, only constructor and env vars are used.

        Returns:
            Resolved configuration dictionary.
        """
        # Start with defaults
        config = {
            "enabled": False,
            "api_key": None,
            "workspace": None,
            "project_name": "the-edge-agent",
            "url": None,
            "llm_tracing": False,
            "trace_export": False,
        }

        # Apply YAML settings (lowest priority after defaults)
        if yaml_settings and isinstance(yaml_settings, dict):
            for key in config:
                if key in yaml_settings and yaml_settings[key] is not None:
                    config[key] = yaml_settings[key]

        # Apply environment variables (higher priority)
        env_mapping = {
            "OPIK_API_KEY": "api_key",
            "OPIK_WORKSPACE": "workspace",
            "OPIK_PROJECT_NAME": "project_name",
            "OPIK_URL_OVERRIDE": "url",
        }
        for env_var, config_key in env_mapping.items():
            env_value = os.getenv(env_var)
            if env_value:
                config[config_key] = env_value

        # Apply constructor parameters (highest priority)
        constructor_params = getattr(self, "_opik_constructor_params", {})
        for key, value in constructor_params.items():
            if value is not None:
                config[key] = value

        # Log effective configuration at debug level
        logger.debug(
            f"Opik configuration resolved: enabled={config['enabled']}, "
            f"project_name={config['project_name']}, "
            f"workspace={config['workspace'] or 'default'}, "
            f"llm_tracing={config['llm_tracing']}, "
            f"trace_export={config['trace_export']}"
        )

        return config

    def _add_opik_exporter_from_config(self) -> None:
        """
        Add an OpikExporter to the trace context using resolved config.

        Uses the current _opik_config to create and add an OpikExporter.
        Checks if an OpikExporter is already present to avoid duplicates.

        This method is called:
        - When trace_exporter="opik" in YAML settings
        - When settings.opik.trace_export=true
        """
        if self._trace_context is None:
            return

        # Check if OpikExporter already added (avoid duplicates)
        from .exporters import OpikExporter

        for exporter in self._trace_context.exporters:
            if isinstance(exporter, OpikExporter):
                return

        # Create exporter with resolved config
        config = self._opik_config
        exporter = OpikExporter(
            api_key=config.get("api_key"),
            project_name=config.get("project_name"),
            workspace=config.get("workspace"),
            url_override=config.get("url"),
        )
        self._trace_context.exporters.append(exporter)
        logger.debug("OpikExporter added from resolved configuration")

    def _configure_memory_infrastructure(self, config: Dict[str, Any]) -> None:
        """
        Configure Firebase Agent Memory Infrastructure from YAML settings.

        TEA-BUILTIN-006: Allows runtime configuration of memory backends via YAML.

        Args:
            config: Memory infrastructure configuration dict with keys:
                - metadata_store: {type: str, ...provider-specific options}
                - blob_storage: {type: str, bucket?: str, ...}
                - query_engine: {type: str, ...}
                - vector_index: {type: str, dimensions?: int, ...}

        Example YAML:
            settings:
              memory_infrastructure:
                metadata_store:
                  type: firestore
                blob_storage:
                  type: gcs
                  bucket: my-bucket
                query_engine:
                  type: duckdb
                  enable_httpfs: true
                vector_index:
                  type: duckdb
                  dimensions: 1536
        """
        # Configure MetadataStore
        metadata_config = config.get("metadata_store", {})
        if metadata_config and self._metadata_store is None:
            store_type = metadata_config.get("type", "firestore")
            try:
                # Remove 'type' from config before passing to factory
                store_kwargs = {k: v for k, v in metadata_config.items() if k != "type"}
                self._metadata_store = create_metadata_store(store_type, **store_kwargs)
                logger.info(f"Configured metadata store: {store_type}")
            except Exception as e:
                logger.warning(
                    f"Failed to configure metadata store '{store_type}': {e}"
                )

        # Configure BlobStorage
        blob_config = config.get("blob_storage", {})
        if blob_config and self._blob_storage is None:
            storage_type = blob_config.get("type", "gcs")
            try:
                storage_kwargs = {k: v for k, v in blob_config.items() if k != "type"}
                self._blob_storage = create_blob_storage(storage_type, **storage_kwargs)
                logger.info(f"Configured blob storage: {storage_type}")
            except Exception as e:
                logger.warning(
                    f"Failed to configure blob storage '{storage_type}': {e}"
                )

        # Configure QueryEngine
        query_config = config.get("query_engine", {})
        if query_config and self._query_engine is None:
            engine_type = query_config.get("type", "duckdb")
            try:
                engine_kwargs = {k: v for k, v in query_config.items() if k != "type"}
                self._query_engine = create_query_engine(engine_type, **engine_kwargs)
                logger.info(f"Configured query engine: {engine_type}")
            except Exception as e:
                logger.warning(f"Failed to configure query engine '{engine_type}': {e}")

        # Configure VectorIndex
        vector_config = config.get("vector_index", {})
        if vector_config and self._vector_index is None:
            index_type = vector_config.get("type", "duckdb")
            try:
                index_kwargs = {k: v for k, v in vector_config.items() if k != "type"}
                self._vector_index = create_vector_index(index_type, **index_kwargs)
                logger.info(f"Configured vector index: {index_type}")
            except Exception as e:
                logger.warning(f"Failed to configure vector index '{index_type}': {e}")

    def close(self) -> None:
        """
        Close all backends and release resources.

        Should be called when the engine is no longer needed.
        Safe to call multiple times.

        Example:
            >>> engine = YAMLEngine(ltm_path="./memory.db")
            >>> # ... use engine ...
            >>> engine.close()  # Release database connections
        """
        if self._ltm_backend is not None:
            try:
                self._ltm_backend.close()
            except Exception:
                pass

        if self._graph_backend is not None:
            try:
                self._graph_backend.close()
            except Exception:
                pass

        # TEA-BUILTIN-006: Close Firebase Agent Memory Infrastructure backends
        if self._query_engine is not None:
            try:
                self._query_engine.close()
            except Exception:
                pass

        if self._vector_index is not None:
            try:
                self._vector_index.close()
            except Exception:
                pass

        # MetadataStore and BlobStorage typically don't need explicit closing
        # but we clear references to allow garbage collection
        self._metadata_store = None
        self._blob_storage = None
        self._query_engine = None
        self._vector_index = None

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass

    def get_memory_state(self) -> Dict[str, Any]:
        """
        Get serializable memory state for checkpoint persistence.

        Returns:
            Dictionary containing all memory data needed for restoration.

        Example:
            >>> engine = YAMLEngine()
            >>> # ... store some values ...
            >>> memory_state = engine.get_memory_state()
            >>> # Save memory_state to checkpoint
        """
        return self._memory_backend.get_state()

    def restore_memory_state(self, state: Dict[str, Any]) -> None:
        """
        Restore memory state from checkpoint.

        Args:
            state: Memory state dictionary from get_memory_state()

        Example:
            >>> engine = YAMLEngine()
            >>> # Load memory_state from checkpoint
            >>> engine.restore_memory_state(memory_state)
        """
        self._memory_backend.restore_state(state)

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

        # Load external action modules from imports section (TEA-BUILTIN: External Imports)
        imports = config.get("imports", [])
        if imports:
            self._load_imports(imports, yaml_dir)

        # Extract settings (YAML-level configuration)
        settings = config.get("settings", {})

        # TEA-BUILTIN-005.3: Resolve Opik configuration from YAML settings
        # Settings can be nested under 'opik' key or flat under 'settings'
        opik_yaml_settings = settings.get("opik", {})
        if not isinstance(opik_yaml_settings, dict):
            opik_yaml_settings = {}

        # Re-resolve Opik config with YAML settings applied
        self._opik_config = self._resolve_opik_config(opik_yaml_settings)

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

        # Add nodes
        nodes_list = config.get("nodes", [])
        for node_config in nodes_list:
            self._add_node_from_config(graph, node_config)

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

        # Compile with checkpoint support
        compiled_graph = graph.compile(
            interrupt_before=compile_config.get("interrupt_before", []),
            interrupt_after=compile_config.get("interrupt_after", []),
            checkpoint_dir=checkpoint_dir,
            checkpointer=checkpointer,
        )

        # Determine checkpoint path (parameter overrides config)
        checkpoint_path = checkpoint or compile_config.get("checkpoint")

        if checkpoint_path:
            # Store checkpoint path for resume
            compiled_graph._resume_checkpoint_path = checkpoint_path

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
        """Add a node to the graph from configuration."""
        node_name = node_config["name"]
        node_type = node_config.get("type")

        # Handle while_loop node type (TEA-PY-003)
        if node_type == "while_loop":
            run_func = self._create_while_loop_function(node_config)
            # Wrap with auto-trace if enabled
            if (
                run_func is not None
                and self._auto_trace
                and self._trace_context is not None
            ):
                run_func = self._wrap_with_auto_trace(run_func, node_name, node_config)
            # TEA-OBS-001.1: Wrap with observability if enabled
            if (
                run_func is not None
                and self._enable_observability
                and self._observability_context is not None
            ):
                run_func = self._wrap_with_observability(
                    run_func, node_name, node_config
                )
            graph.add_node(node_name, run=run_func)
            return

        # Determine if it's a fan-in node
        is_fan_in = node_config.get("fan_in", False)

        # Create the run function based on configuration
        run_func = self._create_run_function(node_config)

        # Wrap with auto-trace if enabled
        if (
            run_func is not None
            and self._auto_trace
            and self._trace_context is not None
        ):
            run_func = self._wrap_with_auto_trace(run_func, node_name, node_config)

        # TEA-OBS-001.1: Wrap with observability if enabled
        if (
            run_func is not None
            and self._enable_observability
            and self._observability_context is not None
        ):
            run_func = self._wrap_with_observability(run_func, node_name, node_config)

        # Add node to graph
        if is_fan_in:
            graph.add_fanin_node(node_name, run=run_func)
        else:
            graph.add_node(node_name, run=run_func)

    def _wrap_with_auto_trace(
        self, func: Callable, node_name: str, node_config: Dict[str, Any]
    ) -> Callable:
        """
        Wrap a node function with automatic tracing.

        Captures:
        - Node execution timing
        - LLM token usage (if present in result)
        - HTTP latency (if http.* action)
        - Errors

        Args:
            func: The original run function
            node_name: Name of the node
            node_config: Node configuration dictionary

        Returns:
            Wrapped function with auto-tracing
        """
        trace_context = self._trace_context

        def traced_func(state, **kwargs):
            # Determine action type for metadata
            action_type = node_config.get("uses", "inline")
            metadata = {"node": node_name, "action_type": action_type}

            # Start span
            trace_context.start_span(name=node_name, metadata=metadata)

            try:
                # Execute the original function
                start_time = time.time()
                result = func(state, **kwargs)
                duration_ms = (time.time() - start_time) * 1000

                # Auto-capture metrics from result
                metrics = {"duration_ms": duration_ms}

                # Capture LLM token usage
                if isinstance(result, dict):
                    if "usage" in result:
                        usage = result["usage"]
                        if isinstance(usage, dict):
                            for key in (
                                "prompt_tokens",
                                "completion_tokens",
                                "total_tokens",
                            ):
                                if key in usage:
                                    metrics[key] = usage[key]

                    # Capture HTTP latency if present
                    if action_type in ("http.get", "http.post"):
                        metrics["http_latency_ms"] = duration_ms

                # Log metrics
                trace_context.log_event(metrics=metrics)

                # End span successfully
                trace_context.end_span(status="ok")

                return result

            except Exception as e:
                # End span with error
                trace_context.end_span(status="error", error=str(e))
                raise

        return traced_func

    def _wrap_with_observability(
        self, func: Callable, node_name: str, node_config: Dict[str, Any]
    ) -> Callable:
        """
        Wrap a node function with observability (TEA-OBS-001.1).

        Emits structured log events:
        - Entry event when node starts
        - Exit event with duration when node completes
        - Error event if node fails

        Also injects flow_id into state under '_observability.flow_id'.

        Args:
            func: The original run function
            node_name: Name of the node
            node_config: Node configuration dictionary

        Returns:
            Wrapped function with observability instrumentation
        """
        obs_context = self._observability_context

        def observed_func(state, **kwargs):
            # Inject flow_id into state for workflow access
            if "_observability" not in state:
                state["_observability"] = {}
            state["_observability"]["flow_id"] = obs_context.flow_id
            state["_observability"]["enabled"] = True

            # Determine action type for metadata
            action_type = node_config.get("uses", "inline")
            metadata = {"node": node_name, "action_type": action_type}

            # Start node span
            obs_context.start_node_span(node_name, metadata=metadata)

            try:
                # Execute the original function
                start_time = time.time()
                result = func(state, **kwargs)
                duration_ms = (time.time() - start_time) * 1000

                # End node span successfully
                obs_context.end_node_span(node_name, status="ok")

                # Log additional metrics if present in result
                if isinstance(result, dict):
                    metrics = {}
                    if "usage" in result:
                        usage = result["usage"]
                        if isinstance(usage, dict):
                            for key in (
                                "prompt_tokens",
                                "completion_tokens",
                                "total_tokens",
                            ):
                                if key in usage:
                                    metrics[key] = usage[key]
                    if metrics:
                        obs_context.log(node_name, "info", "metric", metrics=metrics)

                return result

            except Exception as e:
                # End node span with error
                obs_context.end_node_span(node_name, status="error", error=str(e))
                raise

        return observed_func

    def _create_run_function(self, node_config: Dict[str, Any]) -> Optional[Callable]:
        """
        Create a run function from node configuration.

        Supports:
        - run: inline Python code (string)
        - run: { type: prolog, code: "..." } - explicit Prolog code
        - run: { type: lua, code: "..." } - explicit Lua code
        - language: prolog|lua|python - node-level language override
        - uses: built-in action name
        - steps: list of steps (GitHub Actions style)
        - script: inline Python code (GitLab CI style)
        """
        # Determine language from node config (can be overridden per-node)
        language = node_config.get("language")

        # Option 1: Explicit type in run config (dict with type + code)
        if isinstance(node_config.get("run"), dict):
            run_config = node_config["run"]
            run_type = run_config.get("type")

            # Handle type: prolog
            if run_type == "prolog":
                code = run_config.get("code", "")
                return self._create_inline_function(code, language="prolog")

            # Handle type: lua
            if run_type == "lua":
                code = run_config.get("code", "")
                return self._create_inline_function(code, language="lua")

            # Handle type: expression
            if run_type == "expression":
                return self._create_expression_function(
                    run_config["value"], run_config.get("output_key", "result")
                )

        # Option 2: Inline code (run or script) with optional language
        if "run" in node_config and isinstance(node_config["run"], str):
            return self._create_inline_function(node_config["run"], language=language)

        if "script" in node_config:
            return self._create_inline_function(
                node_config["script"], language=language
            )

        # Option 3: Built-in action
        if "uses" in node_config:
            action_name = node_config["uses"]
            action_params = node_config.get("with", {})
            output_key = node_config.get("output", node_config.get("output_key"))
            return self._create_action_function(action_name, action_params, output_key)

        # Option 4: Multi-step execution
        if "steps" in node_config:
            return self._create_steps_function(node_config["steps"])

        # No run function
        return None

    def _detect_lua_code(self, code: str) -> bool:
        """
        Detect if code block is Lua (vs Python/Jinja2).

        Detection rules:
        1. Explicit marker: code starts with '-- lua' or '--lua'
        2. Heuristic: contains Lua-specific keywords not valid in Python

        Args:
            code: Code string to check

        Returns:
            True if code appears to be Lua
        """
        import re

        # Strip leading whitespace for marker check
        stripped = code.strip()

        # Explicit marker
        if stripped.startswith("-- lua") or stripped.startswith("--lua"):
            return True

        # Heuristic: Lua keywords not valid in Python
        lua_patterns = [
            r"\blocal\b",  # local variable declaration
            r"\bthen\b",  # if-then
            r"\bend\b",  # block terminator
            r"\belseif\b",  # Lua uses elseif, Python uses elif
            r"\.\.+",  # string concatenation operator (.. or more dots)
        ]

        return any(re.search(pattern, code) for pattern in lua_patterns)

    def _detect_prolog_code(self, code: str) -> bool:
        """
        Detect if code block is Prolog (vs Python/Lua/Jinja2).

        Detection rules:
        1. Explicit marker: code starts with '% prolog'
        2. Heuristic: contains Prolog-specific syntax

        Args:
            code: Code string to check

        Returns:
            True if code appears to be Prolog
        """
        from .prolog_runtime import detect_prolog_code, PYSWIP_AVAILABLE

        if not PYSWIP_AVAILABLE:
            return False
        return detect_prolog_code(code)

    def _get_lua_runtime(self):
        """
        Get or create the Lua runtime with parallel isolation.

        TEA-PY-002/TEA-PY-006: Each parallel branch gets its own LuaRuntime
        to prevent cross-branch contamination of globals and functions.

        - Main thread: Uses cached self._lua_runtime (shared for sequential execution)
        - Worker threads: Always creates fresh runtime (Option B fix for TEA-PY-006)

        The main thread detection uses threading.main_thread() rather than
        storing the thread ID during __init__. This prevents race conditions
        when YAMLEngine is created in a worker thread and ThreadPoolExecutor
        later reuses that same thread ID.

        Returns:
            LuaRuntime: Isolated runtime for current execution context
        """
        from .lua_runtime import LuaRuntime, LUPA_AVAILABLE

        if not LUPA_AVAILABLE:
            raise ImportError(
                "Lua runtime requires the 'lupa' package.\n"
                "Install it with: pip install 'the_edge_agent[lua]'\n"
                "Or directly: pip install lupa>=2.0"
            )

        # TEA-PY-006: Use actual Python main thread detection, not stored ID
        # This prevents race conditions when engine is created in worker threads
        is_main_thread = threading.current_thread() is threading.main_thread()

        if is_main_thread:
            # Main thread uses cached instance for sequential execution efficiency
            if self._lua_runtime is None:
                self._lua_runtime = LuaRuntime(timeout=self._lua_timeout)
            return self._lua_runtime

        # Non-main thread: always create fresh runtime for isolation
        # ThreadPoolExecutor reuses threads, so we cannot use thread-local
        # storage (it would leak Lua globals between different parallel branches)
        return LuaRuntime(timeout=self._lua_timeout)

    def _get_prolog_runtime(self):
        """
        Get or create the Prolog runtime.

        TEA-PROLOG: Prolog uses thread-local predicates (state/2, return_value/2)
        for parallel branch isolation rather than separate engines. This is because
        SWI-Prolog engine creation is heavy (~50-100ms) compared to LuaJIT (~1-5ms).

        Returns:
            PrologRuntime: Shared runtime with thread-local state isolation
        """
        from .prolog_runtime import (
            PrologRuntime,
            PYSWIP_AVAILABLE,
            _get_install_instructions,
        )

        if not PYSWIP_AVAILABLE:
            raise ImportError(_get_install_instructions())

        # Lazy initialize the shared Prolog runtime
        # Thread-local predicates handle isolation for parallel branches
        if self._prolog_runtime is None:
            self._prolog_runtime = PrologRuntime(
                timeout=self._prolog_timeout, sandbox=self._prolog_sandbox
            )
        return self._prolog_runtime

    def _create_inline_function(
        self, code: str, language: Optional[str] = None
    ) -> Callable:
        """Create a function that executes inline Python, Lua, or Prolog code."""
        # TEA-PROLOG: Check if this is Prolog code
        is_prolog = language == "prolog" or (
            self._prolog_enabled and language is None and self._detect_prolog_code(code)
        )

        if is_prolog:
            # Create Prolog execution function
            def run_prolog(state, **kwargs):
                prolog_runtime = self._get_prolog_runtime()
                # Convert state to dict if it's a DotDict or other mapping
                state_dict = dict(state) if hasattr(state, "items") else state
                return prolog_runtime.execute_node_code(code, state_dict)

            return run_prolog

        # TEA-LUA.P1: Check if this is Lua code and lua is enabled
        is_lua = language == "lua" or (
            self._lua_enabled and language is None and self._detect_lua_code(code)
        )

        if is_lua:
            # Create Lua execution function
            def run_lua(state, **kwargs):
                lua_runtime = self._get_lua_runtime()
                # Convert state to dict if it's a DotDict or other mapping
                state_dict = dict(state) if hasattr(state, "items") else state
                return lua_runtime.execute_node_code(code, state_dict)

            return run_lua

        # Original Python inline execution
        def run_inline(state, **kwargs):
            # Prepare execution context
            exec_globals = {
                "state": state,
                **kwargs,
                # Common imports
                "json": json,
                "requests": None,  # Will be imported if used
                "datetime": None,
            }

            # Try to import common modules if referenced
            if "requests" in code:
                import requests

                exec_globals["requests"] = requests
            if "datetime" in code:
                import datetime

                exec_globals["datetime"] = datetime
            if "OpenAI" in code or "openai" in code:
                try:
                    from openai import OpenAI

                    exec_globals["OpenAI"] = OpenAI
                except ImportError:
                    pass

            # Replace variable references
            code_processed = self._process_template(code, state)

            # If code contains return statements, wrap in a function
            if "return" in code_processed:
                # Indent the code for function body
                indented_code = "\n".join(
                    "    " + line for line in code_processed.split("\n")
                )
                wrapper_code = (
                    f"def __run_func__():\n{indented_code}\n__result__ = __run_func__()"
                )
                exec_locals = {}
                exec(wrapper_code, exec_globals, exec_locals)
                return exec_locals.get("__result__", {})

            # Execute code directly if no return
            exec_locals = {}
            exec(code_processed, exec_globals, exec_locals)

            # If no explicit return, look for updated values
            return {k: v for k, v in exec_locals.items() if not k.startswith("_")}

        return run_inline

    def _create_action_function(
        self, action_name: str, params: Dict[str, Any], output_key: Optional[str] = None
    ) -> Callable:
        """Create a function that calls a built-in action."""
        # Capture engine reference for closure
        engine_opik_llm_tracing = self._opik_llm_tracing

        def run_action(state, **kwargs):
            # Get action from registry
            if action_name not in self.actions_registry:
                raise ValueError(f"Unknown action: {action_name}")

            action_func = self.actions_registry[action_name]

            # Process parameters (template replacement)
            processed_params = self._process_params(params, state)

            # Inject opik_trace for LLM actions if engine has it enabled (TEA-BUILTIN-005.2)
            # Only inject if not explicitly set in params
            if action_name in (
                "llm.call",
                "llm.stream",
                "actions.llm_call",
                "actions.llm_stream",
            ):
                if "opik_trace" not in processed_params and engine_opik_llm_tracing:
                    processed_params["opik_trace"] = True

            # Call action
            result = action_func(state=state, **processed_params, **kwargs)

            # Return result with appropriate key
            if output_key:
                return {output_key: result}
            elif isinstance(result, dict):
                return result
            else:
                return {"result": result}

        return run_action

    def _create_steps_function(self, steps: List[Dict[str, Any]]) -> Callable:
        """Create a function that executes multiple steps sequentially."""

        def run_steps(state, **kwargs):
            step_results = {}
            current_state = state.copy()

            for step in steps:
                step_name = step.get("name", f"step_{len(step_results)}")

                # Create function for this step
                step_func = self._create_run_function(step)

                if step_func:
                    # Execute step
                    result = step_func(current_state, **kwargs)

                    # Store step result
                    step_results[step_name] = result

                    # Update state
                    if isinstance(result, dict):
                        current_state.update(result)

            # Return combined results
            return current_state

        return run_steps

    def _create_expression_function(self, expression: str, output_key: str) -> Callable:
        """Create a function that evaluates a Python expression."""

        def run_expression(state, **kwargs):
            # Process template variables
            expr_processed = self._process_template(expression, state)

            # Evaluate expression
            try:
                result = eval(expr_processed, {"state": state, **kwargs})
            except Exception as e:
                raise ValueError(f"Error evaluating expression '{expression}': {e}")

            return {output_key: result}

        return run_expression

    def _create_while_loop_function(self, node_config: Dict[str, Any]) -> Callable:
        """
        Create a function that executes a while-loop node.

        TEA-PY-003: While-loop node for autonomous iteration.

        Args:
            node_config: Node configuration with:
                - name: Node name
                - type: 'while_loop'
                - condition: Jinja2 expression (evaluated each iteration)
                - max_iterations: Required safety guard (1-1000)
                - body: List of body node configurations

        Returns:
            Callable that executes the while-loop

        Raises:
            ValueError: If configuration is invalid
        """
        node_name = node_config["name"]
        condition = node_config.get("condition")
        max_iterations = node_config.get("max_iterations")
        body = node_config.get("body", [])

        # Validate required fields (AC-8)
        if not condition:
            raise ValueError(f"while_loop node '{node_name}' requires 'condition'")
        if max_iterations is None:
            raise ValueError(f"while_loop node '{node_name}' requires 'max_iterations'")

        # Validate max_iterations range (AC-9)
        if (
            not isinstance(max_iterations, int)
            or max_iterations < 1
            or max_iterations > 1000
        ):
            raise ValueError(
                f"while_loop node '{node_name}': max_iterations must be integer between 1-1000, "
                f"got {max_iterations}"
            )

        # Validate body exists
        if not body:
            raise ValueError(
                f"while_loop node '{node_name}' requires 'body' with at least one node"
            )

        # Check for nested while-loops (AC-11)
        for body_node in body:
            if body_node.get("type") == "while_loop":
                raise ValueError(
                    f"Nested while-loops not supported: '{node_name}' contains "
                    f"nested while_loop '{body_node.get('name', 'unnamed')}'"
                )

        # Pre-compile body node functions
        body_functions = []
        for body_node_config in body:
            body_node_name = body_node_config.get("name", f"body_{len(body_functions)}")
            body_func = self._create_run_function(body_node_config)
            if body_func:
                body_functions.append((body_node_name, body_func))

        # Capture references for closure
        engine = self
        trace_context = self._trace_context
        enable_tracing = self._enable_tracing

        def run_while_loop(state, **kwargs):
            """Execute the while-loop."""
            current_state = state.copy()
            iteration = 0

            # Emit LoopStart event (AC-12)
            if enable_tracing and trace_context is not None:
                trace_context.log_event(
                    event={
                        "event_type": "LoopStart",
                        "node_name": node_name,
                        "max_iterations": max_iterations,
                    }
                )

            while iteration < max_iterations:
                # Evaluate condition using Jinja2 (AC-2)
                condition_result = engine._evaluate_condition(condition, current_state)

                # Emit LoopIteration event (AC-13)
                if enable_tracing and trace_context is not None:
                    trace_context.log_event(
                        event={
                            "event_type": "LoopIteration",
                            "node_name": node_name,
                            "iteration": iteration,
                            "condition_result": condition_result,
                        }
                    )

                # Exit if condition is false (AC-4)
                if not condition_result:
                    break

                # Execute body nodes sequentially (AC-3)
                for body_node_name, body_func in body_functions:
                    try:
                        # AC-15: Body node events are emitted normally via auto-trace
                        result = body_func(current_state, **kwargs)
                        if isinstance(result, dict):
                            # AC-6: State from each iteration is passed to next
                            current_state.update(result)
                    except Exception as e:
                        # AC-10: If loop body execution fails, error propagates immediately
                        raise RuntimeError(
                            f"Error in while_loop '{node_name}' body node '{body_node_name}' "
                            f"at iteration {iteration}: {e}"
                        ) from e

                iteration += 1

            # Determine exit reason (AC-5)
            if iteration >= max_iterations:
                exit_reason = "max_iterations_reached"
            else:
                exit_reason = "condition_false"

            # Emit LoopEnd event (AC-14)
            if enable_tracing and trace_context is not None:
                trace_context.log_event(
                    event={
                        "event_type": "LoopEnd",
                        "node_name": node_name,
                        "iterations_completed": iteration,
                        "exit_reason": exit_reason,
                    }
                )

            # AC-7: Final state after loop completion is passed to downstream nodes
            return current_state

        return run_while_loop

    def _process_goto_and_implicit_edges(
        self,
        graph: StateGraph,
        nodes_list: List[Dict[str, Any]],
        edges_list: List[Dict[str, Any]],
    ) -> None:
        """
        Process goto properties and implicit chaining for nodes.

        TEA-YAML-002: Implements the new implicit graph navigation syntax.

        This method:
        1. Sets entry point to first node (if no __start__ edge in edges_list)
        2. For each node, checks for goto property:
           - If string: adds unconditional edge to target
           - If list: adds conditional edges based on rules
        3. For nodes without goto AND without legacy edges: adds implicit edge to next node
        4. Sets finish point for last node (if no __end__ edge in edges_list)

        Precedence:
        - goto property on node (highest priority)
        - edges section (legacy, deprecated)
        - implicit chaining (lowest priority)

        Args:
            graph: The StateGraph to add edges to
            nodes_list: List of node configurations from YAML
            edges_list: List of edge configurations (for precedence check)
        """
        if not nodes_list:
            return

        # Build node name to index mapping for validation
        node_names = {node["name"]: idx for idx, node in enumerate(nodes_list)}

        # Collect node names that have edges defined in the edges section
        # These nodes should use legacy edges, NOT implicit chaining
        nodes_with_legacy_edges = set()
        has_start_edge = False
        nodes_with_end_edge = set()

        for edge_config in edges_list:
            from_node = edge_config.get("from")
            to_node = edge_config.get("to")
            if from_node:
                if from_node == START:
                    has_start_edge = True
                else:
                    nodes_with_legacy_edges.add(from_node)
            if to_node == END:
                nodes_with_end_edge.add(from_node)

        # Collect node names that have goto definitions
        # These nodes should NOT get edges from the legacy edges section
        nodes_with_goto = set()

        # Set entry point to first node (implicit __start__ -> first_node)
        # Only if there's no __start__ edge in the legacy edges section
        if not has_start_edge:
            first_node = nodes_list[0]["name"]
            graph.set_entry_point(first_node)

        for idx, node_config in enumerate(nodes_list):
            node_name = node_config["name"]
            goto = node_config.get("goto")

            if goto is not None:
                # Node has goto - process it (highest priority)
                nodes_with_goto.add(node_name)
                self._process_node_goto(
                    graph, node_name, goto, node_names, nodes_list, idx
                )
            elif node_name in nodes_with_legacy_edges:
                # Node has legacy edges - don't add implicit chaining
                # The edges will be added later in _add_edge_from_config
                pass
            else:
                # Implicit chaining: add edge to next node or __end__
                if idx < len(nodes_list) - 1:
                    # Not the last node: chain to next
                    next_node = nodes_list[idx + 1]["name"]
                    graph.add_edge(node_name, next_node)
                else:
                    # Last node: implicit finish (unless it has legacy edge to __end__)
                    if node_name not in nodes_with_end_edge:
                        graph.set_finish_point(node_name)

        # Store nodes with goto for precedence handling in legacy edges
        self._nodes_with_goto = nodes_with_goto

    def _process_node_goto(
        self,
        graph: StateGraph,
        node_name: str,
        goto: Union[str, List[Dict[str, Any]]],
        node_names: Dict[str, int],
        nodes_list: List[Dict[str, Any]],
        current_idx: int,
    ) -> None:
        """
        Process the goto property for a single node.

        TEA-YAML-002: Handles both unconditional (string) and conditional (list) goto.

        Args:
            graph: The StateGraph to add edges to
            node_name: Name of the current node
            goto: The goto value (string or list of rules)
            node_names: Mapping of node names to indices for validation
            nodes_list: Full list of node configurations
            current_idx: Index of current node in nodes_list

        Raises:
            ValueError: If goto target references a non-existent node
        """
        if isinstance(goto, str):
            # Unconditional jump: goto: "target_node"
            target = goto

            # Validate target exists (AC-6: error at validation time)
            if target != END and target not in node_names:
                raise ValueError(
                    f"Node '{node_name}' has goto to non-existent node '{target}'. "
                    f"Available nodes: {list(node_names.keys())}"
                )

            if target == END:
                graph.set_finish_point(node_name)
            else:
                graph.add_edge(node_name, target)

        elif isinstance(goto, list):
            # Conditional goto: list of {if: expr, to: target} rules
            has_fallback = False

            for rule in goto:
                target = rule.get("to")
                condition = rule.get("if")

                if target is None:
                    raise ValueError(
                        f"Node '{node_name}' has goto rule without 'to' field: {rule}"
                    )

                # Validate target exists (AC-6: error at validation time)
                if target != END and target not in node_names:
                    raise ValueError(
                        f"Node '{node_name}' has goto to non-existent node '{target}'. "
                        f"Available nodes: {list(node_names.keys())}"
                    )

                if condition is None:
                    # Fallback rule (no condition = always true)
                    has_fallback = True
                    if target == END:
                        graph.set_finish_point(node_name)
                    else:
                        graph.add_edge(node_name, target)
                else:
                    # Conditional rule: add conditional edge
                    # Create a condition function that evaluates the expression
                    # The condition should have access to 'state' and 'result'
                    def make_goto_condition(expr):
                        def cond_func(state, result=None, **kwargs):
                            # Build evaluation context with state and result
                            return self._evaluate_goto_condition(expr, state, result)

                        return cond_func

                    cond_func = make_goto_condition(condition)

                    if target == END:
                        # Conditional finish
                        graph.add_conditional_edges(node_name, cond_func, {True: END})
                    else:
                        graph.add_conditional_edges(
                            node_name, cond_func, {True: target}
                        )

            # If no fallback rule, don't add implicit chaining
            # When using conditional goto, the user must explicitly specify all branches
            # including a fallback with `{to: next_node}` if they want one
            # This prevents the implicit edge from conflicting with conditional edges
        else:
            raise ValueError(
                f"Node '{node_name}' has invalid goto type: {type(goto)}. "
                f"Expected string or list, got: {goto}"
            )

    def _evaluate_goto_condition(
        self, expr: str, state: Dict[str, Any], result: Optional[Dict[str, Any]] = None
    ) -> bool:
        """
        Evaluate a goto condition expression.

        TEA-YAML-002: Provides access to both 'state' and 'result' in condition expressions.

        Args:
            expr: The condition expression (Jinja2 syntax)
            state: The current agent state
            result: The result returned by the current node's execution (optional)

        Returns:
            Boolean result of evaluating the expression

        Example expressions:
            - "result.status == 'error'"
            - "state.retry_count < 3"
            - "result.confidence < 0.7 and state.require_review"
        """
        # Build evaluation context with state and result
        context = {
            "state": DotDict(state) if state else DotDict({}),
            "result": DotDict(result) if result else DotDict({}),
            "variables": DotDict(self.variables),
            "secrets": DotDict(self.secrets),
        }

        # Wrap expression in Jinja2 syntax if not already
        if not expr.strip().startswith("{{") and not expr.strip().startswith("{%"):
            template_expr = f"{{{{ {expr} }}}}"
        else:
            template_expr = expr

        try:
            # Use existing template processing
            cache_key = f"goto_cond:{template_expr}"
            if cache_key not in self._template_cache:
                self._template_cache[cache_key] = self._jinja_env.from_string(
                    template_expr
                )

            template = self._template_cache[cache_key]
            rendered = template.render(**context).strip().lower()

            # Convert rendered string to boolean
            return rendered in ("true", "1", "yes")

        except Exception as e:
            # Log warning and return False on evaluation errors
            logger.warning(
                f"Failed to evaluate goto condition '{expr}': {e}. Returning False."
            )
            return False

    def _add_edge_from_config(
        self, graph: StateGraph, edge_config: Dict[str, Any]
    ) -> None:
        """Add an edge to the graph from configuration.

        TEA-YAML-002: Now respects precedence - if a node has a goto property,
        edges from that node in the legacy edges section are skipped (logged as warning).
        """
        edge_type = edge_config.get("type", "normal")
        # For entry edges, default from_node to START if not specified
        from_node = edge_config.get("from", START if edge_type == "entry" else None)

        # TEA-YAML-002: Precedence check - skip edges for nodes with goto
        nodes_with_goto = getattr(self, "_nodes_with_goto", set())
        if from_node in nodes_with_goto and from_node != START:
            logger.debug(
                f"Skipping legacy edge from '{from_node}' because node has 'goto' property (goto takes precedence)"
            )
            return
        if from_node is None:
            from_node = edge_config[
                "from"
            ]  # Raise KeyError if missing for non-entry edges
        to_node = edge_config["to"]

        # Handle special edge types
        if from_node == START or edge_type == "entry":
            # Check if this is a conditional entry edge
            if "when" not in edge_config and "condition" not in edge_config:
                graph.set_entry_point(to_node)
                return
            # Fall through to conditional edge handling below

        if to_node == END or edge_type == "finish":
            graph.set_finish_point(from_node)
            return

        # Parallel edge
        if edge_type == "parallel":
            fan_in_node = edge_config["fan_in"]
            graph.add_parallel_edge(from_node, to_node, fan_in_node)
            return

        # Conditional edge
        if "condition" in edge_config:
            cond_config = edge_config["condition"]
            when_value = edge_config.get("when", True)

            # Create condition function using Jinja2 (TEA-YAML-001)
            if isinstance(cond_config, dict):
                if cond_config.get("type") == "expression":
                    expr = cond_config["value"]

                    # Use a factory function to capture expr properly
                    def make_cond(e):
                        return lambda state, **kw: self._evaluate_condition(e, state)

                    cond_func = make_cond(expr)
                else:
                    raise ValueError(
                        f"Unknown condition type: {cond_config.get('type')}"
                    )
            elif isinstance(cond_config, str):
                # Simple expression
                def make_cond(e):
                    return lambda state, **kw: self._evaluate_condition(e, state)

                cond_func = make_cond(cond_config)
            else:
                raise ValueError(f"Invalid condition configuration: {cond_config}")

            # Add conditional edge
            graph.add_conditional_edges(from_node, cond_func, {when_value: to_node})
            return

        # Simple when clause (syntactic sugar for condition)
        if "when" in edge_config:
            when_expr = edge_config["when"]

            # Convert simple boolean strings
            if isinstance(when_expr, str):
                if when_expr.lower() == "true":
                    when_result = True
                elif when_expr.lower() == "false":
                    when_result = False
                else:
                    # Expression like "!escalate", "has_results", or "{{ state.x > 5 }}"
                    # TEA-YAML-001: Use Jinja2 for condition evaluation
                    def make_cond(e):
                        return lambda state, **kw: self._evaluate_condition(e, state)

                    cond_func = make_cond(when_expr)
                    graph.add_conditional_edges(from_node, cond_func, {True: to_node})
                    return
            else:
                when_result = when_expr

            # Simple boolean condition
            graph.add_conditional_edges(
                from_node, lambda **kw: when_result, {True: to_node}
            )
            return

        # Normal unconditional edge
        graph.add_edge(from_node, to_node)

    def _process_template(self, text: str, state: Dict[str, Any]) -> Any:
        """
        Process template variables in text using Jinja2.

        TEA-YAML-001: Refactored to use Jinja2 instead of Python eval().

        Supports:
        - {{ state.key }} - access state values
        - {{ variables.key }} - access global variables
        - {{ secrets.key }} - access secrets
        - {{ checkpoint.dir }} - configured checkpoint directory
        - {{ checkpoint.last }} - most recent auto-saved checkpoint path
        - {{ value | filter }} - Jinja2 filter syntax (tojson, upper, lower, length, etc.)
        - {% if %}...{% endif %} - Jinja2 conditionals
        - {% for %}...{% endfor %} - Jinja2 loops
        - {{ data | fromjson }} - custom filter to parse JSON strings

        When the entire value is a single template expression (e.g., "{{ state.data }}"),
        returns the actual object instead of converting to string. This allows passing
        complex objects between actions (AC: 5).

        Uses StrictUndefined mode for helpful error messages on undefined variables (AC: 7).
        Templates are cached for performance (AC: 8).
        """
        if not isinstance(text, str):
            return text

        # Build render context with checkpoint support (AC: 4)
        context = {
            "state": DotDict(state),
            "variables": DotDict(self.variables),
            "secrets": DotDict(self.secrets),
            "checkpoint": DotDict(
                {
                    "dir": self._checkpoint_dir or "",
                    "last": self._last_checkpoint_path or "",
                }
            ),
        }

        text_stripped = text.strip()

        # Check if the entire string is a single template expression (AC: 5)
        # This allows returning actual objects instead of string representation
        # Pattern matches {{ expr }} without Jinja2 block tags
        single_expr_pattern = r"^\{\{\s*(.+?)\s*\}\}$"
        single_match = re.match(single_expr_pattern, text_stripped)

        if single_match and "{%" not in text_stripped:
            expr = single_match.group(1)

            try:
                # Use compile_expression for single expressions to return native objects
                # Check cache first
                cache_key = f"expr:{expr}"
                if cache_key not in self._template_cache:
                    self._template_cache[cache_key] = (
                        self._jinja_env.compile_expression(expr)
                    )

                compiled = self._template_cache[cache_key]
                return compiled(**context)
            except TemplateError as e:
                # Re-raise with helpful context
                raise ValueError(
                    f"Template error in expression '{{{{ {expr} }}}}': {e}"
                )
            except Exception:
                # Return original text if evaluation fails (backward compat)
                return text

        # Check if this has any template syntax at all
        if "{{" not in text and "{%" not in text and "${" not in text:
            return text

        # Multi-expression or mixed content: render as string (AC: 1, 3)
        try:
            # Check cache for full templates
            cache_key = f"tmpl:{text}"
            if cache_key not in self._template_cache:
                self._template_cache[cache_key] = self._jinja_env.from_string(text)

            template = self._template_cache[cache_key]
            result = template.render(**context)
        except TemplateError as e:
            # Re-raise with helpful context
            raise ValueError(f"Template error: {e}")

        # Also handle ${ } style (GitLab CI) for backward compatibility
        pattern2 = r"\$\{([^}]+)\}"
        result = re.sub(
            pattern2, lambda m: str(self.variables.get(m.group(1), m.group(0))), result
        )

        return result

    def _process_params(
        self, params: Dict[str, Any], state: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Recursively process parameters, replacing template variables."""
        processed = {}

        for key, value in params.items():
            if isinstance(value, str):
                processed[key] = self._process_template(value, state)
            elif isinstance(value, dict):
                processed[key] = self._process_params(value, state)
            elif isinstance(value, list):
                processed[key] = [
                    (
                        self._process_template(item, state)
                        if isinstance(item, str)
                        else (
                            self._process_params(item, state)
                            if isinstance(item, dict)
                            else item
                        )
                    )
                    for item in value
                ]
            else:
                processed[key] = value

        return processed

    def _evaluate_condition(self, expr: str, state: Dict[str, Any]) -> bool:
        """
        Evaluate a condition expression using Jinja2.

        TEA-YAML-001: Unified condition evaluation using Jinja2 templates.

        Supports:
        - Jinja2 template: "{{ state.x > 5 }}" or "{{ 'urgent' in state.tags }}"
        - Simple variable: "has_results" -> state.get('has_results', False)
        - Negation: "!escalate" -> not state.get('escalate', False)
        - Jinja2 expressions without braces: "state.x > 5"
        - Python boolean literals: "True", "False"

        Returns:
            bool: Result of the condition evaluation
        """
        if not isinstance(expr, str):
            return bool(expr)

        expr = expr.strip()

        # Handle Python boolean literals explicitly
        if expr == "True":
            return True
        if expr == "False":
            return False

        # Handle simple negation syntax: "!variable"
        if expr.startswith("!") and not expr.startswith("{{"):
            var_name = expr[1:].strip()
            if var_name.isidentifier():
                return not state.get(var_name, False)

        # Handle simple variable reference: "variable_name"
        if expr.isidentifier():
            return bool(state.get(expr, False))

        # If already a Jinja2 template, process it
        if "{{" in expr or "{%" in expr:
            result = self._process_template(expr, state)
            return bool(result)

        # Otherwise, treat as a Jinja2 expression without braces
        # Wrap it to make a proper template
        template_expr = "{{ " + expr + " }}"
        result = self._process_template(template_expr, state)
        return bool(result)

    def _convert_simple_expression(self, expr: str) -> str:
        """
        Convert simple expression syntax to Python.

        DEPRECATED: Use _evaluate_condition with Jinja2 instead (TEA-YAML-001).

        Examples:
        - "!escalate" -> "not state.get('escalate', False)"
        - "has_results" -> "state.get('has_results', False)"
        """
        expr = expr.strip()

        # Handle negation
        if expr.startswith("!"):
            var_name = expr[1:].strip()
            return f"not state.get('{var_name}', False)"

        # Simple variable reference
        if expr.isidentifier():
            return f"state.get('{expr}', False)"

        # Otherwise return as-is
        return expr

    def _load_imports(
        self, imports: List[Dict[str, Any]], yaml_dir: Optional[str] = None
    ) -> None:
        """
        Load external action modules from the imports section.

        Supports two import types:
        - path: Local Python file relative to YAML file
        - package: Installed Python package

        Args:
            imports: List of import configurations from YAML
            yaml_dir: Directory of the YAML file for relative path resolution

        Raises:
            ValueError: If import configuration is invalid
            FileNotFoundError: If local path does not exist
            ImportError: If package cannot be imported

        Example imports configuration:
            imports:
              - path: ./actions/custom.py
                namespace: custom
              - package: tea_actions_slack
                namespace: slack
        """
        if not imports:
            return

        errors: List[str] = []

        for imp in imports:
            namespace = imp.get("namespace", "")

            try:
                if "path" in imp:
                    self._load_from_path(imp["path"], namespace, yaml_dir)
                elif "package" in imp:
                    self._load_from_package(imp["package"], namespace)
                else:
                    errors.append(
                        f"Invalid import: must specify 'path' or 'package'. Got: {imp}"
                    )
            except Exception as e:
                errors.append(str(e))

        if errors:
            raise ValueError(
                f"Failed to load imports:\n" + "\n".join(f"  - {e}" for e in errors)
            )

    def _load_from_path(
        self, path: str, namespace: str, yaml_dir: Optional[str] = None
    ) -> None:
        """
        Load actions from a local Python file.

        Args:
            path: Path to Python file (relative or absolute)
            namespace: Namespace prefix for registered actions
            yaml_dir: Directory of the YAML file for relative path resolution

        Raises:
            FileNotFoundError: If the file does not exist
            ValueError: If the module doesn't have register_actions function
        """
        # Resolve relative paths from YAML file location
        if yaml_dir and not os.path.isabs(path):
            full_path = os.path.normpath(os.path.join(yaml_dir, path))
        else:
            full_path = os.path.abspath(path)

        # Check for circular imports
        if full_path in self._loaded_modules:
            logger.debug(f"Skipping already loaded module: {full_path}")
            return

        if not os.path.exists(full_path):
            raise FileNotFoundError(
                f"Action module not found: {full_path} (from path: {path})"
            )

        # Create unique module name for dynamic import
        module_name = f"_tea_import_{abs(hash(full_path))}"

        # Load module dynamically
        spec = importlib.util.spec_from_file_location(module_name, full_path)
        if spec is None or spec.loader is None:
            raise ValueError(f"Cannot load module from: {full_path}")

        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)

        # Validate contract: module must have register_actions
        if not hasattr(module, "register_actions"):
            raise ValueError(
                f"Module {path} missing required 'register_actions(registry, engine)' function"
            )

        if not callable(module.register_actions):
            raise ValueError(
                f"Module {path} has 'register_actions' but it is not callable"
            )

        # Log metadata if present
        self._log_module_metadata(module, path, namespace)

        # Register actions with namespace
        local_registry: Dict[str, Callable] = {}
        module.register_actions(local_registry, self)

        # Apply namespace prefix and merge into main registry
        self._merge_registry_with_namespace(local_registry, namespace, path)

        # Mark as loaded
        self._loaded_modules.add(full_path)

    def _load_from_package(self, package: str, namespace: str) -> None:
        """
        Load actions from an installed Python package.

        Args:
            package: Package name (supports dotted names like 'tea_actions.slack')
            namespace: Namespace prefix for registered actions

        Raises:
            ImportError: If package cannot be imported
            ValueError: If package doesn't have register_actions function
        """
        # Check for circular imports
        if package in self._loaded_modules:
            logger.debug(f"Skipping already loaded package: {package}")
            return

        try:
            module = importlib.import_module(package)
        except ImportError as e:
            raise ImportError(
                f"Failed to import package '{package}': {e}. "
                f"Ensure the package is installed: pip install {package}"
            )

        # Validate contract: module must have register_actions
        if not hasattr(module, "register_actions"):
            raise ValueError(
                f"Package {package} missing required 'register_actions(registry, engine)' function"
            )

        if not callable(module.register_actions):
            raise ValueError(
                f"Package {package} has 'register_actions' but it is not callable"
            )

        # Log metadata if present
        self._log_module_metadata(module, package, namespace)

        # Register actions with namespace
        local_registry: Dict[str, Callable] = {}
        module.register_actions(local_registry, self)

        # Apply namespace prefix and merge into main registry
        self._merge_registry_with_namespace(local_registry, namespace, package)

        # Mark as loaded
        self._loaded_modules.add(package)

    def _log_module_metadata(self, module: Any, source: str, namespace: str) -> None:
        """
        Log optional __tea_actions__ metadata from an imported module.

        Args:
            module: The imported module object
            source: Source identifier (path or package name) for logging
            namespace: Namespace being used for this import
        """
        if hasattr(module, "__tea_actions__"):
            metadata = module.__tea_actions__
            if isinstance(metadata, dict):
                version = metadata.get("version", "unknown")
                description = metadata.get("description", "")
                declared_actions = metadata.get("actions", [])

                logger.info(
                    f"Loaded actions from {source} "
                    f"(namespace: {namespace or 'root'}, version: {version})"
                )
                if description:
                    logger.debug(f"  Description: {description}")
                if declared_actions:
                    logger.debug(f"  Declared actions: {declared_actions}")

    def _merge_registry_with_namespace(
        self, local_registry: Dict[str, Callable], namespace: str, source: str
    ) -> None:
        """
        Merge actions from local registry into main registry with namespace prefix.

        Args:
            local_registry: Dictionary of actions registered by the module
            namespace: Namespace prefix to apply
            source: Source identifier for error messages

        Logs a warning if an action would override an existing action.
        """
        for name, func in local_registry.items():
            # Apply namespace prefix
            full_name = f"{namespace}.{name}" if namespace else name

            # Warn about overrides
            if full_name in self.actions_registry:
                logger.warning(
                    f"Action '{full_name}' from {source} overrides existing action"
                )

            self.actions_registry[full_name] = func
