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
import time
import importlib
import importlib.util
import logging
from typing import Any, Callable, Dict, Generator, List, Optional, Set, Union

from .stategraph import StateGraph, START, END

logger = logging.getLogger(__name__)

from .memory import (
    MemoryBackend, InMemoryBackend,
    LongTermMemoryBackend, SQLiteBackend,
    GraphBackend, COZO_AVAILABLE, KUZU_AVAILABLE,
    # TEA-BUILTIN-006: Firebase Agent Memory Infrastructure
    MetadataStore, create_metadata_store, FIRESTORE_AVAILABLE,
    BlobStorage, create_blob_storage, GCS_AVAILABLE,
    QueryEngine, create_query_engine, DUCKDB_AVAILABLE,
    VectorIndex, create_vector_index, DUCKDB_VSS_AVAILABLE,
)
from .tracing import TraceContext, ConsoleExporter, FileExporter, CallbackExporter
from .actions import build_actions_registry


class DotDict(dict):
    """Dictionary subclass that allows attribute-style access to keys."""

    def __getattr__(self, key):
        try:
            value = self[key]
            if isinstance(value, dict) and not isinstance(value, DotDict):
                return DotDict(value)
            return value
        except KeyError:
            raise AttributeError(f"'{type(self).__name__}' object has no attribute '{key}'")

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
                        If None and enable_ltm=True, uses SQLiteBackend.
            enable_ltm: Enable long-term memory actions (default: True).
            ltm_path: Path to SQLite database for ltm.* actions.
                     If None, uses in-memory SQLite.
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
                exporters.append(OpikExporter(
                    api_key=self._opik_config.get("api_key"),
                    project_name=self._opik_config.get("project_name"),
                    workspace=self._opik_config.get("workspace"),
                    url_override=self._opik_config.get("url")
                ))
            elif trace_exporter is None:
                # No exporter configured, but tracing is enabled
                # Spans will be collected but not exported
                pass

            self._trace_context = TraceContext(exporters=exporters)

        # Auto-trace flag (can be enabled via YAML settings)
        self._auto_trace = False

        # Initialize memory backend (TEA-BUILTIN-001.1)
        self._memory_backend: Any = memory_backend if memory_backend is not None else InMemoryBackend()

        # Initialize long-term memory backend (TEA-BUILTIN-001.4)
        self._ltm_backend: Optional[Any] = None
        self._enable_ltm = enable_ltm
        if enable_ltm:
            if ltm_backend is not None:
                self._ltm_backend = ltm_backend
            else:
                # Use SQLiteBackend with specified path or in-memory
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

    def _resolve_opik_config(
        self,
        yaml_settings: Optional[Dict[str, Any]] = None
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
            url_override=config.get("url")
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
        metadata_config = config.get('metadata_store', {})
        if metadata_config and self._metadata_store is None:
            store_type = metadata_config.get('type', 'firestore')
            try:
                # Remove 'type' from config before passing to factory
                store_kwargs = {k: v for k, v in metadata_config.items() if k != 'type'}
                self._metadata_store = create_metadata_store(store_type, **store_kwargs)
                logger.info(f"Configured metadata store: {store_type}")
            except Exception as e:
                logger.warning(f"Failed to configure metadata store '{store_type}': {e}")

        # Configure BlobStorage
        blob_config = config.get('blob_storage', {})
        if blob_config and self._blob_storage is None:
            storage_type = blob_config.get('type', 'gcs')
            try:
                storage_kwargs = {k: v for k, v in blob_config.items() if k != 'type'}
                self._blob_storage = create_blob_storage(storage_type, **storage_kwargs)
                logger.info(f"Configured blob storage: {storage_type}")
            except Exception as e:
                logger.warning(f"Failed to configure blob storage '{storage_type}': {e}")

        # Configure QueryEngine
        query_config = config.get('query_engine', {})
        if query_config and self._query_engine is None:
            engine_type = query_config.get('type', 'duckdb')
            try:
                engine_kwargs = {k: v for k, v in query_config.items() if k != 'type'}
                self._query_engine = create_query_engine(engine_type, **engine_kwargs)
                logger.info(f"Configured query engine: {engine_type}")
            except Exception as e:
                logger.warning(f"Failed to configure query engine '{engine_type}': {e}")

        # Configure VectorIndex
        vector_config = config.get('vector_index', {})
        if vector_config and self._vector_index is None:
            index_type = vector_config.get('type', 'duckdb')
            try:
                index_kwargs = {k: v for k, v in vector_config.items() if k != 'type'}
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
        checkpointer: Optional[Any] = None
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
        with open(yaml_path, 'r') as f:
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
        yaml_dir: Optional[str] = None
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
        self.variables = config.get('variables', {})

        # Load external action modules from imports section (TEA-BUILTIN: External Imports)
        imports = config.get('imports', [])
        if imports:
            self._load_imports(imports, yaml_dir)

        # Extract settings (YAML-level configuration)
        settings = config.get('settings', {})

        # TEA-BUILTIN-005.3: Resolve Opik configuration from YAML settings
        # Settings can be nested under 'opik' key or flat under 'settings'
        opik_yaml_settings = settings.get('opik', {})
        if not isinstance(opik_yaml_settings, dict):
            opik_yaml_settings = {}

        # Re-resolve Opik config with YAML settings applied
        self._opik_config = self._resolve_opik_config(opik_yaml_settings)

        # Update llm_tracing flag from resolved config
        if self._opik_config.get('llm_tracing', False):
            self._opik_llm_tracing = True
        # Also check flat setting for backwards compatibility
        if settings.get('opik_llm_tracing', False):
            self._opik_llm_tracing = True

        # Handle auto-trace from YAML settings
        if settings.get('auto_trace', False) and self._enable_tracing:
            self._auto_trace = True
            # Configure trace exporter from settings if not already set
            if self._trace_context is not None and not self._trace_context.exporters:
                trace_exporter = settings.get('trace_exporter', 'console')
                trace_file = settings.get('trace_file')
                if trace_exporter == 'console':
                    self._trace_context.exporters.append(ConsoleExporter(verbose=False))
                elif trace_exporter == 'file' and trace_file:
                    self._trace_context.exporters.append(FileExporter(trace_file))
                elif trace_exporter == 'opik':
                    self._add_opik_exporter_from_config()
        else:
            self._auto_trace = False

        # TEA-BUILTIN-005.3: Add Opik exporter if trace_export is enabled in config
        if (self._opik_config.get('trace_export', False) and
            self._enable_tracing and
            self._trace_context is not None):
            self._add_opik_exporter_from_config()

        # TEA-BUILTIN-006: Configure Firebase Agent Memory Infrastructure from YAML settings
        memory_infra = settings.get('memory_infrastructure', {})
        if memory_infra:
            self._configure_memory_infrastructure(memory_infra)

        # Create graph
        compile_config = config.get('config', {})
        graph = StateGraph(
            state_schema=config.get('state_schema', {}),
            raise_exceptions=compile_config.get('raise_exceptions', False)
        )

        # Store reference for checkpoint actions
        self._current_graph = graph

        # Extract checkpoint configuration
        checkpoint_dir = compile_config.get('checkpoint_dir')
        self._checkpoint_dir = checkpoint_dir

        # Add nodes
        for node_config in config.get('nodes', []):
            self._add_node_from_config(graph, node_config)

        # Add edges
        for edge_config in config.get('edges', []):
            self._add_edge_from_config(graph, edge_config)

        # Store checkpointer reference for resume
        self._checkpointer = checkpointer

        # Compile with checkpoint support
        compiled_graph = graph.compile(
            interrupt_before=compile_config.get('interrupt_before', []),
            interrupt_after=compile_config.get('interrupt_after', []),
            checkpoint_dir=checkpoint_dir,
            checkpointer=checkpointer
        )

        # Determine checkpoint path (parameter overrides config)
        checkpoint_path = checkpoint or compile_config.get('checkpoint')

        if checkpoint_path:
            # Store checkpoint path for resume
            compiled_graph._resume_checkpoint_path = checkpoint_path

        return compiled_graph

    def resume_from_checkpoint(
        self,
        yaml_path: str,
        checkpoint_path: str,
        config: Optional[Dict[str, Any]] = None
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

    def _add_node_from_config(self, graph: StateGraph, node_config: Dict[str, Any]) -> None:
        """Add a node to the graph from configuration."""
        node_name = node_config['name']

        # Determine if it's a fan-in node
        is_fan_in = node_config.get('fan_in', False)

        # Create the run function based on configuration
        run_func = self._create_run_function(node_config)

        # Wrap with auto-trace if enabled
        if run_func is not None and self._auto_trace and self._trace_context is not None:
            run_func = self._wrap_with_auto_trace(run_func, node_name, node_config)

        # Add node to graph
        if is_fan_in:
            graph.add_fanin_node(node_name, run=run_func)
        else:
            graph.add_node(node_name, run=run_func)

    def _wrap_with_auto_trace(
        self,
        func: Callable,
        node_name: str,
        node_config: Dict[str, Any]
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
            action_type = node_config.get('uses', 'inline')
            metadata = {
                "node": node_name,
                "action_type": action_type
            }

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
                    if 'usage' in result:
                        usage = result['usage']
                        if isinstance(usage, dict):
                            for key in ("prompt_tokens", "completion_tokens", "total_tokens"):
                                if key in usage:
                                    metrics[key] = usage[key]

                    # Capture HTTP latency if present
                    if action_type in ('http.get', 'http.post'):
                        metrics['http_latency_ms'] = duration_ms

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

    def _create_run_function(self, node_config: Dict[str, Any]) -> Optional[Callable]:
        """
        Create a run function from node configuration.

        Supports:
        - run: inline Python code (string)
        - uses: built-in action name
        - steps: list of steps (GitHub Actions style)
        - script: inline Python code (GitLab CI style)
        """
        # Option 1: Inline Python code (run or script)
        if 'run' in node_config and isinstance(node_config['run'], str):
            return self._create_inline_function(node_config['run'])

        if 'script' in node_config:
            return self._create_inline_function(node_config['script'])

        # Option 2: Built-in action
        if 'uses' in node_config:
            action_name = node_config['uses']
            action_params = node_config.get('with', {})
            output_key = node_config.get('output', node_config.get('output_key'))
            return self._create_action_function(action_name, action_params, output_key)

        # Option 3: Multi-step execution
        if 'steps' in node_config:
            return self._create_steps_function(node_config['steps'])

        # Option 4: Expression evaluation
        if isinstance(node_config.get('run'), dict):
            run_config = node_config['run']
            if run_config.get('type') == 'expression':
                return self._create_expression_function(
                    run_config['value'],
                    run_config.get('output_key', 'result')
                )

        # No run function
        return None

    def _create_inline_function(self, code: str) -> Callable:
        """Create a function that executes inline Python code."""
        def run_inline(state, **kwargs):
            # Prepare execution context
            exec_globals = {
                'state': state,
                **kwargs,
                # Common imports
                'json': json,
                'requests': None,  # Will be imported if used
                'datetime': None,
            }

            # Try to import common modules if referenced
            if 'requests' in code:
                import requests
                exec_globals['requests'] = requests
            if 'datetime' in code:
                import datetime
                exec_globals['datetime'] = datetime
            if 'OpenAI' in code or 'openai' in code:
                try:
                    from openai import OpenAI
                    exec_globals['OpenAI'] = OpenAI
                except ImportError:
                    pass

            # Replace variable references
            code_processed = self._process_template(code, state)

            # If code contains return statements, wrap in a function
            if 'return' in code_processed:
                # Indent the code for function body
                indented_code = '\n'.join('    ' + line for line in code_processed.split('\n'))
                wrapper_code = f"def __run_func__():\n{indented_code}\n__result__ = __run_func__()"
                exec_locals = {}
                exec(wrapper_code, exec_globals, exec_locals)
                return exec_locals.get('__result__', {})

            # Execute code directly if no return
            exec_locals = {}
            exec(code_processed, exec_globals, exec_locals)

            # If no explicit return, look for updated values
            return {k: v for k, v in exec_locals.items() if not k.startswith('_')}

        return run_inline

    def _create_action_function(
        self,
        action_name: str,
        params: Dict[str, Any],
        output_key: Optional[str] = None
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
            if action_name in ('llm.call', 'llm.stream', 'actions.llm_call', 'actions.llm_stream'):
                if 'opik_trace' not in processed_params and engine_opik_llm_tracing:
                    processed_params['opik_trace'] = True

            # Call action
            result = action_func(state=state, **processed_params, **kwargs)

            # Return result with appropriate key
            if output_key:
                return {output_key: result}
            elif isinstance(result, dict):
                return result
            else:
                return {'result': result}

        return run_action

    def _create_steps_function(self, steps: List[Dict[str, Any]]) -> Callable:
        """Create a function that executes multiple steps sequentially."""
        def run_steps(state, **kwargs):
            step_results = {}
            current_state = state.copy()

            for step in steps:
                step_name = step.get('name', f'step_{len(step_results)}')

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
                result = eval(expr_processed, {'state': state, **kwargs})
            except Exception as e:
                raise ValueError(f"Error evaluating expression '{expression}': {e}")

            return {output_key: result}

        return run_expression

    def _add_edge_from_config(self, graph: StateGraph, edge_config: Dict[str, Any]) -> None:
        """Add an edge to the graph from configuration."""
        edge_type = edge_config.get('type', 'normal')
        # For entry edges, default from_node to START if not specified
        from_node = edge_config.get('from', START if edge_type == 'entry' else None)
        if from_node is None:
            from_node = edge_config['from']  # Raise KeyError if missing for non-entry edges
        to_node = edge_config['to']

        # Handle special edge types
        if from_node == START or edge_type == 'entry':
            # Check if this is a conditional entry edge
            if 'when' not in edge_config and 'condition' not in edge_config:
                graph.set_entry_point(to_node)
                return
            # Fall through to conditional edge handling below

        if to_node == END or edge_type == 'finish':
            graph.set_finish_point(from_node)
            return

        # Parallel edge
        if edge_type == 'parallel':
            fan_in_node = edge_config['fan_in']
            graph.add_parallel_edge(from_node, to_node, fan_in_node)
            return

        # Conditional edge
        if 'condition' in edge_config:
            cond_config = edge_config['condition']
            when_value = edge_config.get('when', True)

            # Create condition function
            if isinstance(cond_config, dict):
                if cond_config.get('type') == 'expression':
                    expr = cond_config['value']
                    cond_func = lambda state, **kw: eval(
                        self._process_template(expr, state),
                        {'state': state, **kw}
                    )
                else:
                    raise ValueError(f"Unknown condition type: {cond_config.get('type')}")
            elif isinstance(cond_config, str):
                # Simple expression
                cond_func = lambda state, **kw: eval(
                    self._process_template(cond_config, state),
                    {'state': state, **kw}
                )
            else:
                raise ValueError(f"Invalid condition configuration: {cond_config}")

            # Add conditional edge
            graph.add_conditional_edges(
                from_node,
                cond_func,
                {when_value: to_node}
            )
            return

        # Simple when clause (syntactic sugar for condition)
        if 'when' in edge_config:
            when_expr = edge_config['when']

            # Convert simple boolean strings
            if isinstance(when_expr, str):
                if when_expr.lower() == 'true':
                    when_result = True
                elif when_expr.lower() == 'false':
                    when_result = False
                else:
                    # Expression like "!escalate" or "has_results"
                    when_expr_processed = self._convert_simple_expression(when_expr)
                    cond_func = lambda state, **kw: eval(
                        when_expr_processed,
                        {'state': state, **kw}
                    )
                    graph.add_conditional_edges(from_node, cond_func, {True: to_node})
                    return
            else:
                when_result = when_expr

            # Simple boolean condition
            graph.add_conditional_edges(
                from_node,
                lambda **kw: when_result,
                {True: to_node}
            )
            return

        # Normal unconditional edge
        graph.add_edge(from_node, to_node)

    def _process_template(self, text: str, state: Dict[str, Any]) -> Any:
        """
        Process template variables in text.

        Supports:
        - {{ state.key }} - access state values
        - {{ variables.key }} - access global variables
        - {{ secrets.key }} - access secrets
        - {{ checkpoint.dir }} - configured checkpoint directory
        - {{ checkpoint.last }} - most recent auto-saved checkpoint path
        - {{ state.key | json }} - apply filters

        When the entire value is a single template expression (e.g., "{{ state.data }}"),
        returns the actual object instead of converting to string. This allows passing
        complex objects between actions.
        """
        if not isinstance(text, str):
            return text

        # Build evaluation context with checkpoint support
        eval_context = {
            'state': DotDict(state),
            'variables': DotDict(self.variables),
            'secrets': DotDict(self.secrets),
            'checkpoint': DotDict({
                'dir': self._checkpoint_dir or '',
                'last': self._last_checkpoint_path or ''
            })
        }

        # Check if the entire string is a single template expression
        # This allows returning actual objects instead of string representation
        single_expr_pattern = r'^\s*\{\{\s*([^}]+)\s*\}\}\s*$'
        single_match = re.match(single_expr_pattern, text)
        if single_match:
            expr = single_match.group(1).strip()

            # Handle filters (e.g., "state.key | json")
            if '|' in expr:
                parts = expr.split('|')
                expr = parts[0].strip()
                filters = [f.strip() for f in parts[1:]]
            else:
                filters = []

            try:
                value = eval(expr, eval_context)

                # Apply filters
                for filter_name in filters:
                    if filter_name == 'json':
                        value = json.dumps(value)
                    elif filter_name == 'upper':
                        value = str(value).upper()
                    elif filter_name == 'lower':
                        value = str(value).lower()

                # Return actual value (preserves dicts, lists, etc.)
                return value
            except Exception:
                return text  # Return original if evaluation fails

        # Replace {{ state.key }} style templates (multiple or embedded)
        pattern = r'\{\{\s*([^}]+)\s*\}\}'

        def replace_var(match):
            expr = match.group(1).strip()

            # Handle filters (e.g., "state.key | json")
            if '|' in expr:
                parts = expr.split('|')
                expr = parts[0].strip()
                filters = [f.strip() for f in parts[1:]]
            else:
                filters = []

            # Evaluate expression
            try:
                value = eval(expr, eval_context)

                # Apply filters
                for filter_name in filters:
                    if filter_name == 'json':
                        value = json.dumps(value)
                    elif filter_name == 'upper':
                        value = str(value).upper()
                    elif filter_name == 'lower':
                        value = str(value).lower()

                return str(value)
            except Exception:
                return match.group(0)  # Return original if evaluation fails

        result = re.sub(pattern, replace_var, text)

        # Also handle ${ } style (GitLab CI)
        pattern2 = r'\$\{([^}]+)\}'
        result = re.sub(pattern2, lambda m: str(self.variables.get(m.group(1), m.group(0))), result)

        return result

    def _process_params(self, params: Dict[str, Any], state: Dict[str, Any]) -> Dict[str, Any]:
        """Recursively process parameters, replacing template variables."""
        processed = {}

        for key, value in params.items():
            if isinstance(value, str):
                processed[key] = self._process_template(value, state)
            elif isinstance(value, dict):
                processed[key] = self._process_params(value, state)
            elif isinstance(value, list):
                processed[key] = [
                    self._process_template(item, state) if isinstance(item, str)
                    else self._process_params(item, state) if isinstance(item, dict)
                    else item
                    for item in value
                ]
            else:
                processed[key] = value

        return processed

    def _convert_simple_expression(self, expr: str) -> str:
        """
        Convert simple expression syntax to Python.

        Examples:
        - "!escalate" -> "not state.get('escalate', False)"
        - "has_results" -> "state.get('has_results', False)"
        """
        expr = expr.strip()

        # Handle negation
        if expr.startswith('!'):
            var_name = expr[1:].strip()
            return f"not state.get('{var_name}', False)"

        # Simple variable reference
        if expr.isidentifier():
            return f"state.get('{expr}', False)"

        # Otherwise return as-is
        return expr

    def _load_imports(
        self,
        imports: List[Dict[str, Any]],
        yaml_dir: Optional[str] = None
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
            namespace = imp.get('namespace', '')

            try:
                if 'path' in imp:
                    self._load_from_path(imp['path'], namespace, yaml_dir)
                elif 'package' in imp:
                    self._load_from_package(imp['package'], namespace)
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
        self,
        path: str,
        namespace: str,
        yaml_dir: Optional[str] = None
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
        if not hasattr(module, 'register_actions'):
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
        if not hasattr(module, 'register_actions'):
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

    def _log_module_metadata(
        self,
        module: Any,
        source: str,
        namespace: str
    ) -> None:
        """
        Log optional __tea_actions__ metadata from an imported module.

        Args:
            module: The imported module object
            source: Source identifier (path or package name) for logging
            namespace: Namespace being used for this import
        """
        if hasattr(module, '__tea_actions__'):
            metadata = module.__tea_actions__
            if isinstance(metadata, dict):
                version = metadata.get('version', 'unknown')
                description = metadata.get('description', '')
                declared_actions = metadata.get('actions', [])

                logger.info(
                    f"Loaded actions from {source} "
                    f"(namespace: {namespace or 'root'}, version: {version})"
                )
                if description:
                    logger.debug(f"  Description: {description}")
                if declared_actions:
                    logger.debug(f"  Declared actions: {declared_actions}")

    def _merge_registry_with_namespace(
        self,
        local_registry: Dict[str, Callable],
        namespace: str,
        source: str
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
