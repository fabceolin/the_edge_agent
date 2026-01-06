"""
LlamaIndex Client Wrapper for TEA (TEA-AGENT-001.8).

This module provides a wrapper around LlamaIndex for advanced RAG patterns,
including router queries, sub-question decomposition, and index management.

The wrapper handles:
    - Lazy initialization of LlamaIndex components
    - Configuration from settings
    - Index loading and caching
    - Graceful fallback when LlamaIndex is unavailable

Example:
    >>> from the_edge_agent.rag import LlamaIndexClient
    >>>
    >>> # Create client with settings
    >>> client = LlamaIndexClient(
    ...     index_path="./vector_store",
    ...     embedding_model="text-embedding-3-small"
    ... )
    >>>
    >>> # Simple query
    >>> results = client.query("What is machine learning?", similarity_top_k=5)
    >>>
    >>> # Router query
    >>> results = client.router_query(
    ...     query="Find recent sales data",
    ...     engines=[
    ...         {"type": "vector", "index_path": "./docs", "description": "Documentation"},
    ...         {"type": "sql", "connection": "sqlite:///sales.db", "description": "Sales DB"}
    ...     ]
    ... )
"""

import logging
import os
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

logger = logging.getLogger(__name__)

# Check if LlamaIndex is available
LLAMAINDEX_AVAILABLE = False
_LLAMAINDEX_IMPORT_ERROR = None

try:
    from llama_index.core import (
        VectorStoreIndex,
        SimpleDirectoryReader,
        StorageContext,
        load_index_from_storage,
        Settings,
    )
    from llama_index.core.schema import Document, NodeWithScore, TextNode
    from llama_index.core.query_engine import RouterQueryEngine
    from llama_index.core.selectors import LLMSingleSelector
    from llama_index.core.tools import QueryEngineTool
    from llama_index.core.query_engine import SubQuestionQueryEngine

    LLAMAINDEX_AVAILABLE = True
except ImportError as e:
    _LLAMAINDEX_IMPORT_ERROR = str(e)


class LlamaIndexUnavailableError(Exception):
    """Raised when LlamaIndex is not available."""

    def __init__(self, message: str = None):
        if message is None:
            message = (
                "LlamaIndex is not installed. "
                "Install with: pip install the_edge_agent[llamaindex]"
            )
        super().__init__(message)


class LlamaIndexClient:
    """
    LlamaIndex client wrapper for advanced RAG patterns.

    This client provides a high-level interface to LlamaIndex functionality,
    handling index management, caching, and configuration.

    Attributes:
        index_path: Path to the default index storage
        embedding_model: Model name for embeddings
        llm_model: Model name for LLM (used in routing/synthesis)
        cache: Dictionary of loaded indices

    Example:
        >>> client = LlamaIndexClient(
        ...     index_path="./my_index",
        ...     embedding_model="text-embedding-3-small"
        ... )
        >>> results = client.query("What is Python?")
    """

    def __init__(
        self,
        index_path: Optional[str] = None,
        embedding_model: Optional[str] = None,
        embedding_base_url: Optional[str] = None,
        llm_model: Optional[str] = None,
        llm_base_url: Optional[str] = None,
        api_key: Optional[str] = None,
    ):
        """
        Initialize the LlamaIndex client.

        Args:
            index_path: Default path for index storage
            embedding_model: Model for embeddings (default: text-embedding-3-small)
            embedding_base_url: Base URL for embedding API (for local/compatible APIs)
            llm_model: Model for LLM operations (default: gpt-4o-mini)
            llm_base_url: Base URL for LLM API
            api_key: API key (uses OPENAI_API_KEY env var if not provided)
        """
        self._index_path = index_path
        self._embedding_model = embedding_model or "text-embedding-3-small"
        self._embedding_base_url = embedding_base_url
        self._llm_model = llm_model or "gpt-4o-mini"
        self._llm_base_url = llm_base_url
        self._api_key = api_key or os.environ.get("OPENAI_API_KEY")

        # Index cache: {path: index}
        self._index_cache: Dict[str, Any] = {}

        # Lazy initialization
        self._initialized = False
        self._embed_model = None
        self._llm = None

    def _ensure_available(self) -> None:
        """Ensure LlamaIndex is available, raise if not."""
        if not LLAMAINDEX_AVAILABLE:
            raise LlamaIndexUnavailableError(
                f"LlamaIndex import failed: {_LLAMAINDEX_IMPORT_ERROR}"
            )

    def _initialize(self) -> None:
        """Initialize LlamaIndex settings (lazy)."""
        if self._initialized:
            return

        self._ensure_available()

        # Configure embedding model
        try:
            from llama_index.embeddings.openai import OpenAIEmbedding

            embed_kwargs = {"model": self._embedding_model}
            if self._embedding_base_url:
                embed_kwargs["api_base"] = self._embedding_base_url
            if self._api_key:
                embed_kwargs["api_key"] = self._api_key

            self._embed_model = OpenAIEmbedding(**embed_kwargs)
            Settings.embed_model = self._embed_model
        except ImportError:
            logger.warning("OpenAI embedding not available, using default")

        # Configure LLM
        try:
            from llama_index.llms.openai import OpenAI

            llm_kwargs = {"model": self._llm_model}
            if self._llm_base_url:
                llm_kwargs["api_base"] = self._llm_base_url
            if self._api_key:
                llm_kwargs["api_key"] = self._api_key

            self._llm = OpenAI(**llm_kwargs)
            Settings.llm = self._llm
        except ImportError:
            logger.warning("OpenAI LLM not available, using default")

        self._initialized = True

    @classmethod
    def is_available(cls) -> bool:
        """Check if LlamaIndex is available."""
        return LLAMAINDEX_AVAILABLE

    @classmethod
    def from_settings(cls, settings: Dict[str, Any]) -> "LlamaIndexClient":
        """
        Create client from settings dictionary.

        Args:
            settings: Settings dict with 'llamaindex' key

        Returns:
            Configured LlamaIndexClient instance

        Example:
            >>> settings = {
            ...     "llamaindex": {
            ...         "index_path": "./vector_store",
            ...         "embedding_model": "text-embedding-3-small",
            ...         "llm_model": "gpt-4o-mini"
            ...     }
            ... }
            >>> client = LlamaIndexClient.from_settings(settings)
        """
        llamaindex_settings = settings.get("llamaindex", {})
        return cls(
            index_path=llamaindex_settings.get("index_path"),
            embedding_model=llamaindex_settings.get("embedding_model"),
            embedding_base_url=llamaindex_settings.get("embedding_base_url"),
            llm_model=llamaindex_settings.get("llm_model"),
            llm_base_url=llamaindex_settings.get("llm_base_url"),
            api_key=llamaindex_settings.get("api_key"),
        )

    def load_index(
        self,
        index_path: Optional[str] = None,
        force_reload: bool = False,
    ) -> Any:
        """
        Load an index from storage.

        Args:
            index_path: Path to index storage (uses default if not provided)
            force_reload: Force reload even if cached

        Returns:
            VectorStoreIndex instance

        Raises:
            LlamaIndexUnavailableError: If LlamaIndex is not installed
            ValueError: If no index path provided and no default set
            FileNotFoundError: If index path doesn't exist
        """
        self._initialize()

        path = index_path or self._index_path
        if not path:
            raise ValueError("No index path provided and no default set")

        # Check cache
        if not force_reload and path in self._index_cache:
            return self._index_cache[path]

        # Load from storage
        path_obj = Path(path)
        if not path_obj.exists():
            raise FileNotFoundError(f"Index not found at: {path}")

        storage_context = StorageContext.from_defaults(persist_dir=str(path_obj))
        index = load_index_from_storage(storage_context)

        # Cache it
        self._index_cache[path] = index

        return index

    def create_index(
        self,
        documents: Optional[List[Dict[str, Any]]] = None,
        directory: Optional[str] = None,
        persist_path: Optional[str] = None,
    ) -> Any:
        """
        Create a new index from documents or a directory.

        Args:
            documents: List of document dicts with 'text' and optional 'metadata'
            directory: Directory path to load documents from
            persist_path: Path to persist the index

        Returns:
            VectorStoreIndex instance

        Raises:
            LlamaIndexUnavailableError: If LlamaIndex is not installed
            ValueError: If neither documents nor directory provided
        """
        self._initialize()

        if documents:
            # Create from document dicts
            llama_docs = []
            for doc in documents:
                text = doc.get("text", doc.get("content", ""))
                metadata = doc.get("metadata", {})
                llama_docs.append(Document(text=text, metadata=metadata))
            index = VectorStoreIndex.from_documents(llama_docs)
        elif directory:
            # Load from directory
            reader = SimpleDirectoryReader(directory)
            docs = reader.load_data()
            index = VectorStoreIndex.from_documents(docs)
        else:
            raise ValueError("Either documents or directory must be provided")

        # Persist if path provided
        if persist_path:
            index.storage_context.persist(persist_dir=persist_path)
            self._index_cache[persist_path] = index

        return index

    def add_documents(
        self,
        documents: List[Dict[str, Any]],
        index_path: Optional[str] = None,
    ) -> int:
        """
        Add documents to an existing index.

        Args:
            documents: List of document dicts with 'text' and optional 'metadata'
            index_path: Path to index (uses default if not provided)

        Returns:
            Number of documents added

        Raises:
            LlamaIndexUnavailableError: If LlamaIndex is not installed
        """
        self._initialize()

        index = self.load_index(index_path)

        # Convert to LlamaIndex documents
        llama_docs = []
        for doc in documents:
            text = doc.get("text", doc.get("content", ""))
            metadata = doc.get("metadata", {})
            llama_docs.append(Document(text=text, metadata=metadata))

        # Insert into index
        for doc in llama_docs:
            index.insert(doc)

        # Persist changes
        path = index_path or self._index_path
        if path:
            index.storage_context.persist(persist_dir=path)

        return len(llama_docs)

    def query(
        self,
        query: str,
        index_path: Optional[str] = None,
        similarity_top_k: int = 5,
        response_mode: str = "compact",
    ) -> Dict[str, Any]:
        """
        Execute a simple vector query against an index.

        Args:
            query: Query text
            index_path: Path to index (uses default if not provided)
            similarity_top_k: Number of nodes to retrieve
            response_mode: Response synthesis mode

        Returns:
            Dict with 'response', 'nodes', and 'scores'

        Raises:
            LlamaIndexUnavailableError: If LlamaIndex is not installed
        """
        self._initialize()

        index = self.load_index(index_path)

        # Create query engine
        query_engine = index.as_query_engine(
            similarity_top_k=similarity_top_k,
            response_mode=response_mode,
        )

        # Execute query
        response = query_engine.query(query)

        # Format results
        nodes = []
        scores = []
        for node in response.source_nodes:
            nodes.append(
                {
                    "id": node.node_id,
                    "text": node.get_content(),
                    "metadata": node.metadata,
                }
            )
            scores.append(node.score if node.score else 0.0)

        return {
            "response": str(response),
            "nodes": nodes,
            "scores": scores,
        }

    def router_query(
        self,
        query: str,
        engines: List[Dict[str, Any]],
        verbose: bool = False,
    ) -> Dict[str, Any]:
        """
        Execute a router query that selects the best engine for the query.

        Args:
            query: Query text
            engines: List of engine configurations with 'type', 'description', etc.
            verbose: Enable verbose logging

        Returns:
            Dict with 'response', 'selected_engine', and 'nodes'

        Raises:
            LlamaIndexUnavailableError: If LlamaIndex is not installed
        """
        self._initialize()

        # Build query engine tools
        tools = []
        engine_map = {}

        for i, engine_config in enumerate(engines):
            engine_type = engine_config.get("type", "vector")
            description = engine_config.get("description", f"Engine {i}")
            name = engine_config.get("name", f"engine_{i}")

            if engine_type == "vector":
                index_path = engine_config.get("index_path")
                if not index_path:
                    raise ValueError(
                        f"Vector engine requires index_path: {engine_config}"
                    )
                index = self.load_index(index_path)
                engine = index.as_query_engine()
            elif engine_type == "keyword":
                # Use BM25 retriever if available
                index_path = engine_config.get("index_path")
                if not index_path:
                    raise ValueError(
                        f"Keyword engine requires index_path: {engine_config}"
                    )
                index = self.load_index(index_path)
                # Use standard engine (LlamaIndex handles this)
                engine = index.as_query_engine(retriever_mode="keyword")
            elif engine_type == "sql":
                # SQL engine requires additional setup
                connection = engine_config.get("connection")
                if not connection:
                    raise ValueError(f"SQL engine requires connection: {engine_config}")
                try:
                    from llama_index.core import SQLDatabase
                    from llama_index.core.query_engine import NLSQLTableQueryEngine
                    from sqlalchemy import create_engine

                    sql_engine = create_engine(connection)
                    sql_db = SQLDatabase(sql_engine)
                    engine = NLSQLTableQueryEngine(sql_database=sql_db)
                except ImportError:
                    raise LlamaIndexUnavailableError(
                        "SQL engine requires sqlalchemy: pip install sqlalchemy"
                    )
            else:
                raise ValueError(f"Unknown engine type: {engine_type}")

            tool = QueryEngineTool.from_defaults(
                query_engine=engine,
                name=name,
                description=description,
            )
            tools.append(tool)
            engine_map[name] = engine_type

        # Create router query engine
        router_engine = RouterQueryEngine(
            selector=LLMSingleSelector.from_defaults(),
            query_engine_tools=tools,
            verbose=verbose,
        )

        # Execute query
        response = router_engine.query(query)

        # Get selected engine info
        selected = None
        if hasattr(response, "metadata") and response.metadata:
            selected = response.metadata.get("selector_result", {})

        # Format results
        nodes = []
        scores = []
        if hasattr(response, "source_nodes"):
            for node in response.source_nodes:
                nodes.append(
                    {
                        "id": node.node_id,
                        "text": node.get_content(),
                        "metadata": node.metadata,
                    }
                )
                scores.append(node.score if node.score else 0.0)

        return {
            "response": str(response),
            "selected_engine": selected,
            "nodes": nodes,
            "scores": scores,
        }

    def subquestion_query(
        self,
        query: str,
        engines: List[Dict[str, Any]],
        use_async: bool = False,
        verbose: bool = False,
    ) -> Dict[str, Any]:
        """
        Execute a sub-question query that decomposes complex queries.

        Args:
            query: Complex query text
            engines: List of engine configurations (same as router_query)
            use_async: Enable parallel execution of sub-questions
            verbose: Enable verbose logging

        Returns:
            Dict with 'response', 'sub_questions', and 'sub_answers'

        Raises:
            LlamaIndexUnavailableError: If LlamaIndex is not installed
        """
        self._initialize()

        # Build query engine tools
        tools = []

        for i, engine_config in enumerate(engines):
            engine_type = engine_config.get("type", "vector")
            description = engine_config.get("description", f"Engine {i}")
            name = engine_config.get("name", f"engine_{i}")

            if engine_type == "vector":
                index_path = engine_config.get("index_path")
                if not index_path:
                    raise ValueError(
                        f"Vector engine requires index_path: {engine_config}"
                    )
                index = self.load_index(index_path)
                engine = index.as_query_engine()
            else:
                # For subquestion, we primarily use vector engines
                logger.warning(
                    f"Non-vector engine type {engine_type} may not work well with subquestion"
                )
                index_path = engine_config.get("index_path")
                if index_path:
                    index = self.load_index(index_path)
                    engine = index.as_query_engine()
                else:
                    continue

            tool = QueryEngineTool.from_defaults(
                query_engine=engine,
                name=name,
                description=description,
            )
            tools.append(tool)

        if not tools:
            raise ValueError("No valid tools could be created from engines")

        # Create sub-question query engine
        subq_engine = SubQuestionQueryEngine.from_defaults(
            query_engine_tools=tools,
            use_async=use_async,
            verbose=verbose,
        )

        # Execute query
        response = subq_engine.query(query)

        # Extract sub-questions and answers
        sub_questions = []
        sub_answers = []

        if hasattr(response, "metadata") and response.metadata:
            sub_qa = response.metadata.get("sub_qa", [])
            for item in sub_qa:
                sub_questions.append(item.get("sub_question", ""))
                sub_answers.append(item.get("answer", ""))

        return {
            "response": str(response),
            "sub_questions": sub_questions,
            "sub_answers": sub_answers,
        }

    def clear_cache(self) -> None:
        """Clear the index cache."""
        self._index_cache.clear()

    def get_cache_info(self) -> Dict[str, Any]:
        """Get information about cached indices."""
        return {
            "cached_paths": list(self._index_cache.keys()),
            "count": len(self._index_cache),
        }
