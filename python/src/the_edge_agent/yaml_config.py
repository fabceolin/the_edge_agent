"""
Configuration and lifecycle management for YAMLEngine.

This module provides the EngineConfig class for managing engine configuration,
backend infrastructure, and lifecycle operations. It supports:

- Opik configuration with proper precedence (constructor > env > YAML > defaults)
- Firebase Agent Memory Infrastructure (Firestore, GCS, DuckDB)
- Resource lifecycle management (close, cleanup)
- Memory state checkpoint persistence

TEA-PY-008.5: Extracted from yaml_engine.py for modularity.

Example usage:
    >>> from the_edge_agent.yaml_config import EngineConfig
    >>> config = EngineConfig(engine)
    >>> opik_settings = config.resolve_opik_config(yaml_settings)
    >>> config.configure_memory_infrastructure(memory_config)
"""

import os
import logging
from typing import Any, Callable, Dict, List, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from .yaml_engine import YAMLEngine
    from .observability import ObservabilityContext

from .memory import (
    create_metadata_store,
    create_blob_storage,
    create_query_engine,
    create_vector_index,
)

logger = logging.getLogger(__name__)


class EngineConfig:
    """
    Configuration and lifecycle management for YAMLEngine.

    This class handles all configuration resolution, backend infrastructure
    setup, and resource lifecycle management for the YAMLEngine.

    Attributes:
        _engine: Reference to YAMLEngine for accessing internal state
    """

    def __init__(self, engine: "YAMLEngine"):
        """
        Initialize the engine config with engine reference.

        Args:
            engine: YAMLEngine instance providing:
                - Backend storage references (_metadata_store, _blob_storage, etc.)
                - Constructor Opik parameters (_opik_constructor_params)
                - Memory backend reference (_memory_backend)
                - Trace context for exporters (_trace_context)
        """
        self._engine = engine

    def resolve_opik_config(
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
        constructor_params = getattr(self._engine, "_opik_constructor_params", {})
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

    def add_opik_exporter_from_config(self) -> None:
        """
        Add an OpikExporter to the trace context using resolved config.

        Uses the current _opik_config to create and add an OpikExporter.
        Checks if an OpikExporter is already present to avoid duplicates.

        This method is called:
        - When trace_exporter="opik" in YAML settings
        - When settings.opik.trace_export=true
        """
        if self._engine._trace_context is None:
            return

        # Check if OpikExporter already added (avoid duplicates)
        from .exporters import OpikExporter

        for exporter in self._engine._trace_context.exporters:
            if isinstance(exporter, OpikExporter):
                return

        # Create exporter with resolved config
        config = self._engine._opik_config
        exporter = OpikExporter(
            api_key=config.get("api_key"),
            project_name=config.get("project_name"),
            workspace=config.get("workspace"),
            url_override=config.get("url"),
        )
        self._engine._trace_context.exporters.append(exporter)
        logger.debug("OpikExporter added from resolved configuration")

    def configure_memory_infrastructure(self, config: Dict[str, Any]) -> None:
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
        if metadata_config and self._engine._metadata_store is None:
            store_type = metadata_config.get("type", "firestore")
            try:
                # Remove 'type' from config before passing to factory
                store_kwargs = {k: v for k, v in metadata_config.items() if k != "type"}
                self._engine._metadata_store = create_metadata_store(
                    store_type, **store_kwargs
                )
                logger.info(f"Configured metadata store: {store_type}")
            except Exception as e:
                logger.warning(
                    f"Failed to configure metadata store '{store_type}': {e}"
                )

        # Configure BlobStorage
        blob_config = config.get("blob_storage", {})
        if blob_config and self._engine._blob_storage is None:
            storage_type = blob_config.get("type", "gcs")
            try:
                storage_kwargs = {k: v for k, v in blob_config.items() if k != "type"}
                self._engine._blob_storage = create_blob_storage(
                    storage_type, **storage_kwargs
                )
                logger.info(f"Configured blob storage: {storage_type}")
            except Exception as e:
                logger.warning(
                    f"Failed to configure blob storage '{storage_type}': {e}"
                )

        # Configure QueryEngine
        query_config = config.get("query_engine", {})
        if query_config and self._engine._query_engine is None:
            engine_type = query_config.get("type", "duckdb")
            try:
                engine_kwargs = {k: v for k, v in query_config.items() if k != "type"}
                self._engine._query_engine = create_query_engine(
                    engine_type, **engine_kwargs
                )
                logger.info(f"Configured query engine: {engine_type}")
            except Exception as e:
                logger.warning(f"Failed to configure query engine '{engine_type}': {e}")

        # Configure VectorIndex
        vector_config = config.get("vector_index", {})
        if vector_config and self._engine._vector_index is None:
            index_type = vector_config.get("type", "duckdb")
            try:
                index_kwargs = {k: v for k, v in vector_config.items() if k != "type"}
                self._engine._vector_index = create_vector_index(
                    index_type, **index_kwargs
                )
                logger.info(f"Configured vector index: {index_type}")
            except Exception as e:
                logger.warning(f"Failed to configure vector index '{index_type}': {e}")

    @property
    def metadata_store(self) -> Optional[Any]:
        """Get the metadata store instance."""
        return self._engine._metadata_store

    @property
    def blob_storage(self) -> Optional[Any]:
        """Get the blob storage instance."""
        return self._engine._blob_storage

    @property
    def query_engine(self) -> Optional[Any]:
        """Get the query engine instance."""
        return self._engine._query_engine

    @property
    def vector_index(self) -> Optional[Any]:
        """Get the vector index instance."""
        return self._engine._vector_index

    @property
    def embedding_fn(self) -> Optional[Callable[[str], List[float]]]:
        """Get the embedding function."""
        return self._engine._embedding_fn

    @property
    def opik_llm_tracing(self) -> bool:
        """Check if native Opik LLM tracing is enabled."""
        return self._engine._opik_llm_tracing

    @property
    def opik_config(self) -> Dict[str, Any]:
        """Get the resolved Opik configuration."""
        return self._engine._opik_config.copy()

    @property
    def observability_context(self) -> Optional["ObservabilityContext"]:
        """Get the observability context instance."""
        return self._engine._observability_context

    def close(self) -> None:
        """
        Close all backends and release resources.

        Should be called when the engine is no longer needed.
        Safe to call multiple times.
        """
        engine = self._engine

        if engine._ltm_backend is not None:
            try:
                engine._ltm_backend.close()
            except Exception:
                pass

        if engine._graph_backend is not None:
            try:
                engine._graph_backend.close()
            except Exception:
                pass

        # TEA-BUILTIN-012.1: Close secrets backend
        if engine._secrets_backend is not None:
            try:
                engine._secrets_backend.close()
            except Exception:
                pass
            engine._secrets_backend = None

        # TEA-BUILTIN-006: Close Firebase Agent Memory Infrastructure backends
        if engine._query_engine is not None:
            try:
                engine._query_engine.close()
            except Exception:
                pass

        if engine._vector_index is not None:
            try:
                engine._vector_index.close()
            except Exception:
                pass

        # MetadataStore and BlobStorage typically don't need explicit closing
        # but we clear references to allow garbage collection
        engine._metadata_store = None
        engine._blob_storage = None
        engine._query_engine = None
        engine._vector_index = None

    def get_memory_state(self) -> Dict[str, Any]:
        """
        Get serializable memory state for checkpoint persistence.

        Returns:
            Dictionary containing all memory data needed for restoration.
        """
        return self._engine._memory_backend.get_state()

    def restore_memory_state(self, state: Dict[str, Any]) -> None:
        """
        Restore memory state from checkpoint.

        Args:
            state: Memory state dictionary from get_memory_state()
        """
        self._engine._memory_backend.restore_state(state)
