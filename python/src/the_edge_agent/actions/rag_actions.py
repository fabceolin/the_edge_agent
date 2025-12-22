"""
RAG Actions for YAMLEngine (TEA-BUILTIN-002.2).

This module provides Retrieval-Augmented Generation (RAG) actions for
YAMLEngine workflows. Actions support embedding creation, vector storage,
and semantic similarity search.

Embedding Providers:
    - OpenAI (local/remote compatible API): text-embedding-3-small, text-embedding-3-large, ada-002
    - Ollama (local): nomic-embed-text, mxbai-embed-large, all-minilm, bge-m3

Vector Stores:
    - InMemoryVectorStore: Pure Python, zero dependencies
    - ChromaVectorStore: Optional Chroma integration (requires chromadb)

Actions:
    - embedding.create: Generate embeddings from text
    - vector.store: Store documents with embeddings
    - vector.query: Semantic similarity search

Example:
    >>> # Create embedding
    >>> result = registry['embedding.create'](
    ...     state={},
    ...     text="Hello world"
    ... )
    >>> print(f"Embedding dims: {result['dimensions']}")

    >>> # Store documents
    >>> result = registry['vector.store'](
    ...     state={},
    ...     texts=["Doc 1", "Doc 2"],
    ...     metadata=[{"type": "article"}, {"type": "blog"}]
    ... )
    >>> print(f"Stored {result['stored']} documents")

    >>> # Query
    >>> result = registry['vector.query'](
    ...     state={},
    ...     query="hello",
    ...     k=5
    ... )
    >>> for r in result['results']:
    ...     print(f"{r['text']}: {r['score']:.3f}")
"""

import json
import math
import uuid
from abc import ABC, abstractmethod
from typing import Any, Callable, Dict, List, Optional, Protocol, Union


# =============================================================================
# Embedding Provider Abstraction (AC: 4)
# =============================================================================

class EmbeddingProvider(Protocol):
    """Protocol for embedding providers (AC: 4)."""

    def embed(self, texts: Union[str, List[str]]) -> List[List[float]]:
        """
        Generate embeddings for one or more texts.

        Args:
            texts: Single text string or list of text strings

        Returns:
            List of embedding vectors (one per input text)
        """
        ...

    @property
    def dimensions(self) -> int:
        """Return the dimensionality of embeddings from this provider."""
        ...

    @property
    def model_name(self) -> str:
        """Return the model name being used."""
        ...


class OpenAIEmbeddingProvider:
    """
    OpenAI-compatible embedding provider (AC: 4).

    Supports:
    - OpenAI API (default)
    - LocalAI, vLLM, and other OpenAI-compatible APIs via base_url

    Models:
    - text-embedding-3-small: 1536 dimensions (default)
    - text-embedding-3-large: 3072 dimensions
    - text-embedding-ada-002: 1536 dimensions
    """

    # Default dimensions by model
    MODEL_DIMENSIONS = {
        "text-embedding-3-small": 1536,
        "text-embedding-3-large": 3072,
        "text-embedding-ada-002": 1536,
    }

    def __init__(
        self,
        model: str = "text-embedding-3-small",
        base_url: Optional[str] = None,
        api_key: Optional[str] = None,
        dimensions: Optional[int] = None
    ):
        """
        Initialize OpenAI embedding provider.

        Args:
            model: Model name (default: text-embedding-3-small)
            base_url: Custom API base URL for local/compatible APIs
            api_key: API key (uses OPENAI_API_KEY env var if not provided)
            dimensions: Custom dimensions (only for text-embedding-3-* models)
        """
        self._model = model
        self._base_url = base_url
        self._api_key = api_key

        # Determine dimensions
        if dimensions is not None:
            self._dimensions = dimensions
        else:
            self._dimensions = self.MODEL_DIMENSIONS.get(model, 1536)

        self._client = None

    def _get_client(self):
        """Lazily create OpenAI client."""
        if self._client is None:
            try:
                from openai import OpenAI
            except ImportError:
                raise ImportError(
                    "OpenAI library not installed. Install with: pip install openai>=1.0.0"
                )

            kwargs = {}
            if self._base_url:
                kwargs["base_url"] = self._base_url
            if self._api_key:
                kwargs["api_key"] = self._api_key

            self._client = OpenAI(**kwargs)

        return self._client

    def embed(self, texts: Union[str, List[str]]) -> List[List[float]]:
        """Generate embeddings using OpenAI API."""
        client = self._get_client()

        # Normalize to list
        if isinstance(texts, str):
            texts = [texts]

        # Build request params
        params: Dict[str, Any] = {
            "model": self._model,
            "input": texts
        }

        # Add dimensions for text-embedding-3-* models
        if self._model.startswith("text-embedding-3-") and self._dimensions:
            params["dimensions"] = self._dimensions

        response = client.embeddings.create(**params)

        # Extract embeddings in order
        embeddings = [None] * len(texts)
        for item in response.data:
            embeddings[item.index] = item.embedding

        return embeddings

    @property
    def dimensions(self) -> int:
        return self._dimensions

    @property
    def model_name(self) -> str:
        return self._model


class OllamaEmbeddingProvider:
    """
    Ollama local embedding provider (AC: 4).

    Models:
    - nomic-embed-text: 768 dims, 8K context (default)
    - mxbai-embed-large: 1024 dims
    - all-minilm: 384 dims (lightweight)
    - bge-m3: 1024 dims (highest accuracy)

    Requires Ollama running locally or at custom base_url.
    """

    # Model dimensions
    MODEL_DIMENSIONS = {
        "nomic-embed-text": 768,
        "mxbai-embed-large": 1024,
        "all-minilm": 384,
        "bge-m3": 1024,
    }

    def __init__(
        self,
        model: str = "nomic-embed-text",
        base_url: str = "http://localhost:11434",
        timeout: float = 60.0
    ):
        """
        Initialize Ollama embedding provider.

        Args:
            model: Model name (default: nomic-embed-text)
            base_url: Ollama server URL (default: http://localhost:11434)
            timeout: Request timeout in seconds (default: 60.0)
        """
        self._model = model
        self._base_url = base_url.rstrip("/")
        self._timeout = timeout
        self._dimensions = self.MODEL_DIMENSIONS.get(model, 768)

    def embed(self, texts: Union[str, List[str]]) -> List[List[float]]:
        """Generate embeddings using Ollama API."""
        import requests

        # Normalize to list
        if isinstance(texts, str):
            texts = [texts]

        embeddings = []
        for text in texts:
            response = requests.post(
                f"{self._base_url}/api/embeddings",
                json={"model": self._model, "prompt": text},
                timeout=self._timeout
            )

            if response.status_code == 404:
                raise ValueError(
                    f"Ollama model '{self._model}' not found. "
                    f"Pull it with: ollama pull {self._model}"
                )

            response.raise_for_status()
            data = response.json()
            embeddings.append(data["embedding"])

        return embeddings

    @property
    def dimensions(self) -> int:
        return self._dimensions

    @property
    def model_name(self) -> str:
        return self._model


# =============================================================================
# Vector Store Abstraction (AC: 5)
# =============================================================================

class VectorStore(ABC):
    """Abstract base class for vector stores (AC: 5)."""

    @abstractmethod
    def add(
        self,
        ids: List[str],
        texts: List[str],
        embeddings: List[List[float]],
        metadatas: Optional[List[Dict[str, Any]]] = None,
        collection: str = "default"
    ) -> int:
        """
        Add documents to the vector store.

        Args:
            ids: List of document IDs
            texts: List of document texts
            embeddings: List of embedding vectors
            metadatas: Optional list of metadata dicts
            collection: Collection name (default: "default")

        Returns:
            Number of documents added
        """
        ...

    @abstractmethod
    def query(
        self,
        embedding: List[float],
        k: int = 5,
        collection: str = "default",
        filter: Optional[Dict[str, Any]] = None,
        include_embeddings: bool = False
    ) -> List[Dict[str, Any]]:
        """
        Query the vector store for similar documents.

        Args:
            embedding: Query embedding vector
            k: Number of results to return
            collection: Collection name
            filter: Metadata filter conditions
            include_embeddings: Include embeddings in results

        Returns:
            List of result dicts with id, text, score, metadata
        """
        ...

    @abstractmethod
    def get_state(self) -> Dict[str, Any]:
        """Get serializable state for checkpoints."""
        ...

    @abstractmethod
    def restore_state(self, state: Dict[str, Any]) -> None:
        """Restore state from checkpoint."""
        ...


def cosine_similarity(a: List[float], b: List[float]) -> float:
    """
    Calculate cosine similarity between two vectors.

    Pure Python implementation (AC: 5 - no dependencies).

    Args:
        a: First vector
        b: Second vector

    Returns:
        Cosine similarity score between -1 and 1
    """
    dot = sum(x * y for x, y in zip(a, b))
    norm_a = math.sqrt(sum(x * x for x in a))
    norm_b = math.sqrt(sum(x * x for x in b))
    if norm_a == 0 or norm_b == 0:
        return 0.0
    return dot / (norm_a * norm_b)


# Try to use numpy for efficient similarity calculation
try:
    import numpy as np
    _HAS_NUMPY = True

    def cosine_similarity_batch(query: List[float], embeddings: List[List[float]]) -> List[float]:
        """Batch cosine similarity using numpy."""
        query_arr = np.array(query)
        emb_arr = np.array(embeddings)

        # Normalize
        query_norm = query_arr / (np.linalg.norm(query_arr) + 1e-10)
        emb_norms = emb_arr / (np.linalg.norm(emb_arr, axis=1, keepdims=True) + 1e-10)

        # Dot product
        similarities = np.dot(emb_norms, query_norm)
        return similarities.tolist()

except ImportError:
    _HAS_NUMPY = False

    def cosine_similarity_batch(query: List[float], embeddings: List[List[float]]) -> List[float]:
        """Batch cosine similarity using pure Python."""
        return [cosine_similarity(query, emb) for emb in embeddings]


class InMemoryVectorStore(VectorStore):
    """
    In-memory vector store implementation (AC: 5).

    Zero external dependencies. Uses pure Python or numpy if available.
    Supports:
    - Multiple collections
    - Metadata filtering
    - Pickle serialization for checkpoints
    """

    def __init__(self):
        # Structure: {collection_name: {id: {text, embedding, metadata}}}
        self._collections: Dict[str, Dict[str, Dict[str, Any]]] = {}

    def add(
        self,
        ids: List[str],
        texts: List[str],
        embeddings: List[List[float]],
        metadatas: Optional[List[Dict[str, Any]]] = None,
        collection: str = "default"
    ) -> int:
        """Add documents to collection."""
        if collection not in self._collections:
            self._collections[collection] = {}

        # Validate dimension consistency
        if embeddings:
            dim = len(embeddings[0])
            for i, emb in enumerate(embeddings):
                if len(emb) != dim:
                    raise ValueError(
                        f"Embedding dimension mismatch at index {i}: "
                        f"expected {dim}, got {len(emb)}"
                    )

            # Check against existing documents in collection
            if self._collections[collection]:
                existing_doc = next(iter(self._collections[collection].values()))
                existing_dim = len(existing_doc["embedding"])
                if dim != existing_dim:
                    raise ValueError(
                        f"Embedding dimension mismatch: new embeddings have {dim} dims, "
                        f"collection '{collection}' has {existing_dim} dims"
                    )

        metadatas = metadatas or [{}] * len(ids)
        added = 0

        for i, doc_id in enumerate(ids):
            self._collections[collection][doc_id] = {
                "text": texts[i],
                "embedding": embeddings[i],
                "metadata": metadatas[i] if i < len(metadatas) else {}
            }
            added += 1

        return added

    def query(
        self,
        embedding: List[float],
        k: int = 5,
        collection: str = "default",
        filter: Optional[Dict[str, Any]] = None,
        include_embeddings: bool = False
    ) -> List[Dict[str, Any]]:
        """Query for similar documents."""
        if collection not in self._collections:
            return []

        docs = self._collections[collection]
        if not docs:
            return []

        # Filter documents
        filtered_docs = []
        for doc_id, doc in docs.items():
            if filter and not self._match_filter(doc["metadata"], filter):
                continue
            filtered_docs.append((doc_id, doc))

        if not filtered_docs:
            return []

        # Calculate similarities
        embeddings_list = [doc["embedding"] for _, doc in filtered_docs]
        similarities = cosine_similarity_batch(embedding, embeddings_list)

        # Sort by similarity (descending) and take top k
        scored = list(zip(filtered_docs, similarities))
        scored.sort(key=lambda x: x[1], reverse=True)
        top_k = scored[:k]

        # Build results
        results = []
        for (doc_id, doc), score in top_k:
            result = {
                "id": doc_id,
                "text": doc["text"],
                "score": score,
                "metadata": doc["metadata"]
            }
            if include_embeddings:
                result["embedding"] = doc["embedding"]
            results.append(result)

        return results

    def _match_filter(self, metadata: Dict[str, Any], filter: Dict[str, Any]) -> bool:
        """
        Check if metadata matches filter conditions.

        Supports:
        - Exact match: {"type": "article"}
        - Greater than or equal: {"date_gte": "2024-01-01"}
        - Less than or equal: {"date_lte": "2024-12-31"}
        - Greater than: {"count_gt": 10}
        - Less than: {"count_lt": 100}
        - Not equal: {"status_ne": "deleted"}
        - In list: {"category_in": ["tech", "science"]}
        """
        for key, value in filter.items():
            # Parse operator suffix
            if key.endswith("_gte"):
                field = key[:-4]
                if field not in metadata or metadata[field] < value:
                    return False
            elif key.endswith("_lte"):
                field = key[:-4]
                if field not in metadata or metadata[field] > value:
                    return False
            elif key.endswith("_gt"):
                field = key[:-3]
                if field not in metadata or metadata[field] <= value:
                    return False
            elif key.endswith("_lt"):
                field = key[:-3]
                if field not in metadata or metadata[field] >= value:
                    return False
            elif key.endswith("_ne"):
                field = key[:-3]
                if metadata.get(field) == value:
                    return False
            elif key.endswith("_in"):
                field = key[:-3]
                if field not in metadata or metadata[field] not in value:
                    return False
            else:
                # Exact match
                if metadata.get(key) != value:
                    return False

        return True

    def get_state(self) -> Dict[str, Any]:
        """Get serializable state."""
        return {"collections": self._collections}

    def restore_state(self, state: Dict[str, Any]) -> None:
        """Restore from state."""
        self._collections = state.get("collections", {})


class ChromaVectorStore(VectorStore):
    """
    Chroma vector store wrapper (AC: 5).

    Requires: pip install chromadb

    Provides persistent storage with efficient similarity search.
    """

    def __init__(self, persist_directory: Optional[str] = None):
        """
        Initialize Chroma vector store.

        Args:
            persist_directory: Path for persistent storage (None for in-memory)
        """
        try:
            import chromadb
        except ImportError:
            raise ImportError(
                "Chroma not installed. Install with: pip install chromadb"
            )

        if persist_directory:
            self._client = chromadb.PersistentClient(path=persist_directory)
        else:
            self._client = chromadb.Client()

        self._collections: Dict[str, Any] = {}

    def _get_collection(self, name: str):
        """Get or create a Chroma collection."""
        if name not in self._collections:
            self._collections[name] = self._client.get_or_create_collection(
                name=name,
                metadata={"hnsw:space": "cosine"}
            )
        return self._collections[name]

    def add(
        self,
        ids: List[str],
        texts: List[str],
        embeddings: List[List[float]],
        metadatas: Optional[List[Dict[str, Any]]] = None,
        collection: str = "default"
    ) -> int:
        """Add documents to Chroma collection."""
        coll = self._get_collection(collection)

        # Chroma doesn't accept empty dicts, so filter them out or pass None
        if metadatas:
            # Filter out empty dicts - Chroma requires non-empty metadata
            filtered_metadatas = [m if m else None for m in metadatas]
            # If all are None, don't pass metadatas
            if all(m is None for m in filtered_metadatas):
                filtered_metadatas = None
        else:
            filtered_metadatas = None

        add_kwargs = {
            "ids": ids,
            "documents": texts,
            "embeddings": embeddings,
        }
        if filtered_metadatas is not None:
            add_kwargs["metadatas"] = filtered_metadatas

        coll.add(**add_kwargs)

        return len(ids)

    def query(
        self,
        embedding: List[float],
        k: int = 5,
        collection: str = "default",
        filter: Optional[Dict[str, Any]] = None,
        include_embeddings: bool = False
    ) -> List[Dict[str, Any]]:
        """Query Chroma collection."""
        coll = self._get_collection(collection)

        # Convert filter to Chroma format
        where = None
        if filter:
            where = self._convert_filter(filter)

        include = ["documents", "metadatas", "distances"]
        if include_embeddings:
            include.append("embeddings")

        results = coll.query(
            query_embeddings=[embedding],
            n_results=k,
            where=where,
            include=include
        )

        # Format results
        output = []
        if results["ids"] and results["ids"][0]:
            for i, doc_id in enumerate(results["ids"][0]):
                result = {
                    "id": doc_id,
                    "text": results["documents"][0][i] if results.get("documents") else "",
                    "score": 1 - results["distances"][0][i],  # Convert distance to similarity
                    "metadata": results["metadatas"][0][i] if results.get("metadatas") else {}
                }
                if include_embeddings and results.get("embeddings"):
                    result["embedding"] = results["embeddings"][0][i]
                output.append(result)

        return output

    def _convert_filter(self, filter: Dict[str, Any]) -> Dict[str, Any]:
        """Convert our filter syntax to Chroma's where clause."""
        conditions = []

        for key, value in filter.items():
            if key.endswith("_gte"):
                field = key[:-4]
                conditions.append({field: {"$gte": value}})
            elif key.endswith("_lte"):
                field = key[:-4]
                conditions.append({field: {"$lte": value}})
            elif key.endswith("_gt"):
                field = key[:-3]
                conditions.append({field: {"$gt": value}})
            elif key.endswith("_lt"):
                field = key[:-3]
                conditions.append({field: {"$lt": value}})
            elif key.endswith("_ne"):
                field = key[:-3]
                conditions.append({field: {"$ne": value}})
            elif key.endswith("_in"):
                field = key[:-3]
                conditions.append({field: {"$in": value}})
            else:
                conditions.append({key: {"$eq": value}})

        if len(conditions) == 0:
            return None
        elif len(conditions) == 1:
            return conditions[0]
        else:
            return {"$and": conditions}

    def get_state(self) -> Dict[str, Any]:
        """Get state (Chroma handles persistence)."""
        return {"type": "chroma", "collections": list(self._collections.keys())}

    def restore_state(self, state: Dict[str, Any]) -> None:
        """Restore state (no-op for Chroma, it persists automatically)."""
        pass


# =============================================================================
# Provider/Store Factory
# =============================================================================

def create_embedding_provider(
    provider: str = "openai",
    model: Optional[str] = None,
    base_url: Optional[str] = None,
    api_key: Optional[str] = None,
    dimensions: Optional[int] = None,
    timeout: float = 60.0
) -> EmbeddingProvider:
    """
    Factory function to create embedding providers.

    Args:
        provider: Provider type - "openai" or "ollama"
        model: Model name (provider-specific defaults)
        base_url: Custom API base URL
        api_key: API key (for OpenAI)
        dimensions: Custom dimensions (for OpenAI text-embedding-3-* models)
        timeout: Request timeout (for Ollama)

    Returns:
        EmbeddingProvider instance
    """
    if provider == "openai":
        return OpenAIEmbeddingProvider(
            model=model or "text-embedding-3-small",
            base_url=base_url,
            api_key=api_key,
            dimensions=dimensions
        )
    elif provider == "ollama":
        return OllamaEmbeddingProvider(
            model=model or "nomic-embed-text",
            base_url=base_url or "http://localhost:11434",
            timeout=timeout
        )
    else:
        raise ValueError(f"Unknown embedding provider: {provider}")


def create_vector_store(
    store_type: str = "memory",
    chroma_path: Optional[str] = None
) -> VectorStore:
    """
    Factory function to create vector stores.

    Args:
        store_type: Store type - "memory" or "chroma"
        chroma_path: Path for Chroma persistence

    Returns:
        VectorStore instance
    """
    if store_type == "memory":
        return InMemoryVectorStore()
    elif store_type == "chroma":
        return ChromaVectorStore(persist_directory=chroma_path)
    else:
        raise ValueError(f"Unknown vector store type: {store_type}")


# =============================================================================
# Action Registration (AC: 7, 8)
# =============================================================================

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register RAG actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    # Store provider and vector store instances on engine
    if not hasattr(engine, '_rag_provider'):
        engine._rag_provider = None
    if not hasattr(engine, '_rag_vector_store'):
        engine._rag_vector_store = None

    def _get_provider(state: Dict[str, Any], **kwargs) -> EmbeddingProvider:
        """Get or create embedding provider based on config."""
        # Check for explicit provider in kwargs
        provider_type = kwargs.get("provider", "openai")
        model = kwargs.get("model")
        base_url = kwargs.get("base_url")
        api_key = kwargs.get("api_key")
        dimensions = kwargs.get("dimensions")
        timeout = kwargs.get("timeout", 60.0)

        # Check engine settings (from YAML)
        if hasattr(engine, 'variables'):
            settings = engine.variables.get("settings", {})
            rag_settings = settings.get("rag", {})
            provider_type = rag_settings.get("embedding_provider", provider_type)
            model = model or rag_settings.get("embedding_model")
            base_url = base_url or rag_settings.get("openai_base_url") or rag_settings.get("ollama_base_url")
            timeout = rag_settings.get("ollama_timeout", timeout)

        # Create or reuse provider
        if engine._rag_provider is None:
            engine._rag_provider = create_embedding_provider(
                provider=provider_type,
                model=model,
                base_url=base_url,
                api_key=api_key,
                dimensions=dimensions,
                timeout=timeout
            )

        return engine._rag_provider

    def _get_vector_store(**kwargs) -> VectorStore:
        """Get or create vector store based on config."""
        store_type = kwargs.get("store_type", "memory")
        chroma_path = kwargs.get("chroma_path")

        # Check engine settings
        if hasattr(engine, 'variables'):
            settings = engine.variables.get("settings", {})
            rag_settings = settings.get("rag", {})
            store_type = rag_settings.get("vector_store", store_type)
            chroma_path = chroma_path or rag_settings.get("chroma_path")

        # Create or reuse store
        if engine._rag_vector_store is None:
            engine._rag_vector_store = create_vector_store(
                store_type=store_type,
                chroma_path=chroma_path
            )

        return engine._rag_vector_store

    # =========================================================================
    # embedding.create (AC: 1, 6, 7, 8)
    # =========================================================================

    def embedding_create(
        state: Dict[str, Any],
        text: Union[str, List[str]],
        model: Optional[str] = None,
        batch: bool = False,
        **kwargs
    ) -> Dict[str, Any]:
        """
        Create embeddings from text.

        Args:
            state: Current state dictionary
            text: Text or list of texts to embed
            model: Model name (optional, uses config default)
            batch: If True, always return batch format
            **kwargs: Additional provider configuration

        Returns:
            Single text: {"embedding": List[float], "model": str, "dimensions": int}
            Batch: {"embeddings": List[List[float]], "model": str, "count": int, "dimensions": int}
            Error: {"error": str, "success": False}
        """
        try:
            if model:
                kwargs["model"] = model
            provider = _get_provider(state, **kwargs)

            # Normalize input
            is_batch = isinstance(text, list) or batch
            texts = text if isinstance(text, list) else [text]

            # Generate embeddings
            embeddings = provider.embed(texts)

            if is_batch or len(texts) > 1:
                return {
                    "embeddings": embeddings,
                    "model": provider.model_name,
                    "count": len(embeddings),
                    "dimensions": provider.dimensions
                }
            else:
                return {
                    "embedding": embeddings[0],
                    "model": provider.model_name,
                    "dimensions": provider.dimensions
                }

        except Exception as e:
            return {
                "error": f"Embedding creation failed: {str(e)}",
                "success": False
            }

    registry['embedding.create'] = embedding_create
    registry['actions.embedding_create'] = embedding_create

    # =========================================================================
    # vector.store (AC: 2, 6, 7, 8)
    # =========================================================================

    def vector_store(
        state: Dict[str, Any],
        texts: Union[str, List[str]],
        embeddings: Optional[List[List[float]]] = None,
        ids: Optional[List[str]] = None,
        metadata: Optional[Union[Dict[str, Any], List[Dict[str, Any]]]] = None,
        collection: str = "default",
        **kwargs
    ) -> Dict[str, Any]:
        """
        Store documents with embeddings in vector store.

        Args:
            state: Current state dictionary
            texts: Text or list of texts to store
            embeddings: Pre-computed embeddings (auto-generated if not provided)
            ids: Document IDs (auto-generated UUIDs if not provided)
            metadata: Metadata dict or list of dicts
            collection: Collection name (default: "default")
            **kwargs: Additional configuration

        Returns:
            {"stored": int, "collection": str, "ids": List[str]}
            Error: {"error": str, "success": False}
        """
        try:
            # Normalize texts to list
            if isinstance(texts, str):
                texts = [texts]

            # Auto-generate IDs if not provided
            if ids is None:
                ids = [str(uuid.uuid4()) for _ in texts]
            elif len(ids) != len(texts):
                return {
                    "error": f"Number of IDs ({len(ids)}) doesn't match texts ({len(texts)})",
                    "success": False
                }

            # Auto-generate embeddings if not provided
            if embeddings is None:
                provider = _get_provider(state, **kwargs)
                embeddings = provider.embed(texts)
            elif len(embeddings) != len(texts):
                return {
                    "error": f"Number of embeddings ({len(embeddings)}) doesn't match texts ({len(texts)})",
                    "success": False
                }

            # Normalize metadata
            if metadata is None:
                metadatas = [{}] * len(texts)
            elif isinstance(metadata, dict):
                # Same metadata for all
                metadatas = [metadata] * len(texts)
            else:
                metadatas = metadata

            # Get vector store and add documents
            store = _get_vector_store(**kwargs)
            stored = store.add(
                ids=ids,
                texts=texts,
                embeddings=embeddings,
                metadatas=metadatas,
                collection=collection
            )

            return {
                "stored": stored,
                "collection": collection,
                "ids": ids
            }

        except Exception as e:
            return {
                "error": f"Vector store failed: {str(e)}",
                "success": False
            }

    registry['vector.store'] = vector_store
    registry['actions.vector_store'] = vector_store

    # =========================================================================
    # vector.query (AC: 3, 7, 8)
    # =========================================================================

    def vector_query(
        state: Dict[str, Any],
        query: str,
        k: int = 5,
        collection: str = "default",
        filter: Optional[Dict[str, Any]] = None,
        include_embeddings: bool = False,
        **kwargs
    ) -> Dict[str, Any]:
        """
        Query vector store for similar documents.

        Args:
            state: Current state dictionary
            query: Query text
            k: Number of results to return (default: 5)
            collection: Collection to query (default: "default")
            filter: Metadata filter conditions
            include_embeddings: Include embeddings in results
            **kwargs: Additional configuration

        Returns:
            {
                "results": [{"id": str, "text": str, "score": float, "metadata": dict}],
                "query": str,
                "collection": str,
                "k": int
            }
            Error: {"error": str, "success": False}
        """
        try:
            # Validate filter syntax
            if filter is not None:
                if not isinstance(filter, dict):
                    return {
                        "error": "Filter must be a dictionary",
                        "success": False
                    }

            # Generate query embedding
            provider = _get_provider(state, **kwargs)
            query_embedding = provider.embed(query)[0]

            # Query vector store
            store = _get_vector_store(**kwargs)
            results = store.query(
                embedding=query_embedding,
                k=k,
                collection=collection,
                filter=filter,
                include_embeddings=include_embeddings
            )

            return {
                "results": results,
                "query": query,
                "collection": collection,
                "k": k
            }

        except Exception as e:
            return {
                "error": f"Vector query failed: {str(e)}",
                "success": False
            }

    registry['vector.query'] = vector_query
    registry['actions.vector_query'] = vector_query

    # =========================================================================
    # Utility functions for checkpoint integration
    # =========================================================================

    def get_rag_state() -> Dict[str, Any]:
        """Get RAG state for checkpointing."""
        state = {}
        if engine._rag_vector_store:
            state["vector_store"] = engine._rag_vector_store.get_state()
        return state

    def restore_rag_state(state: Dict[str, Any]) -> None:
        """Restore RAG state from checkpoint."""
        if "vector_store" in state and engine._rag_vector_store:
            engine._rag_vector_store.restore_state(state["vector_store"])

    # Attach to engine for checkpoint access
    engine.get_rag_state = get_rag_state
    engine.restore_rag_state = restore_rag_state
