"""
RAG Actions for YAMLEngine (TEA-BUILTIN-002.2, TEA-BUILTIN-002.3).

This module provides Retrieval-Augmented Generation (RAG) actions for
YAMLEngine workflows. Actions support embedding creation, vector storage,
semantic similarity search, and file indexing.

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
    - vector.index_files: Index files/directories into vector store (TEA-BUILTIN-002.3)

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
import os
import time
import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Protocol, Tuple, Union

# Optional fsspec import for cloud URI support
try:
    import fsspec

    FSSPEC_AVAILABLE = True
except ImportError:
    fsspec = None  # type: ignore
    FSSPEC_AVAILABLE = False


# =============================================================================
# File Indexing Data Structures (TEA-BUILTIN-002.3)
# =============================================================================


class ChunkStrategy(Enum):
    """Chunking strategies for file content (AC: 4)."""

    LINE = "line"
    PARAGRAPH = "paragraph"
    DOCUMENT = "document"


@dataclass
class Chunk:
    """Represents a chunk of file content with line information (AC: 7)."""

    text: str
    start_line: int  # 0-based
    end_line: int  # Exclusive
    chunk_type: str


@dataclass
class FileState:
    """Tracks file state for incremental updates (AC: 8)."""

    path: str
    size: int
    mtime: float
    indexed_at: float


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
        dimensions: Optional[int] = None,
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
        params: Dict[str, Any] = {"model": self._model, "input": texts}

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


class Model2VecEmbeddingProvider:
    """
    Local embedding provider using model2vec (TEA-BUILTIN-002.4).

    Uses minishlab/potion-multilingual-128M model (128 dimensions).
    Model is lazy-loaded and cached at module level.

    Features:
    - No API calls or external services required
    - ~500MB model downloaded from HuggingFace on first use
    - Cached in ~/.cache/huggingface/
    - Efficient batch encoding

    Requires: pip install model2vec
    """

    MODEL_NAME = "minishlab/potion-multilingual-128M"
    DIMENSIONS = 128

    _model = None  # Module-level cache

    def __init__(self, model: Optional[str] = None):
        """
        Initialize model2vec provider.

        Args:
            model: Model name (default: minishlab/potion-multilingual-128M)
        """
        self._model_name = model or self.MODEL_NAME

    def _get_model(self):
        """Lazy load model (cached at class level)."""
        if Model2VecEmbeddingProvider._model is None:
            try:
                from model2vec import StaticModel
            except ImportError:
                raise ImportError(
                    "model2vec not installed. Install with: pip install model2vec"
                )

            Model2VecEmbeddingProvider._model = StaticModel.from_pretrained(
                self._model_name
            )

        return Model2VecEmbeddingProvider._model

    def embed(self, texts: Union[str, List[str]]) -> List[List[float]]:
        """Generate embeddings using model2vec."""
        model = self._get_model()

        # Normalize to list
        if isinstance(texts, str):
            texts = [texts]

        # Encode batch
        embeddings = model.encode(texts)

        # Convert numpy to list if needed
        try:
            import numpy as np

            if isinstance(embeddings, np.ndarray):
                return embeddings.tolist()
        except ImportError:
            pass

        return embeddings

    @property
    def dimensions(self) -> int:
        return self.DIMENSIONS

    @property
    def model_name(self) -> str:
        return self._model_name


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
        timeout: float = 60.0,
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
                timeout=self._timeout,
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
        collection: str = "default",
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
        include_embeddings: bool = False,
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

    def cosine_similarity_batch(
        query: List[float], embeddings: List[List[float]]
    ) -> List[float]:
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

    def cosine_similarity_batch(
        query: List[float], embeddings: List[List[float]]
    ) -> List[float]:
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
        collection: str = "default",
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
                "metadata": metadatas[i] if i < len(metadatas) else {},
            }
            added += 1

        return added

    def query(
        self,
        embedding: List[float],
        k: int = 5,
        collection: str = "default",
        filter: Optional[Dict[str, Any]] = None,
        include_embeddings: bool = False,
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
                "metadata": doc["metadata"],
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


class LanceDBVectorStore(VectorStore):
    """
    LanceDB vector store implementation (TEA-BUILTIN-002.5).

    Provides persistent vector storage with:
    - Automatic vector indexing at 256+ rows
    - Efficient upsert operations (delete + insert)
    - Path and metadata filtering
    - Cloud storage support via fsspec (s3://, gs://, az://)

    Requires: pip install lancedb pyarrow
    """

    INDEX_THRESHOLD = 256  # Create index when table has this many rows

    def __init__(self, path: str = "~/.tea/vectors/"):
        """
        Initialize LanceDB vector store.

        Args:
            path: Database path (local or fsspec URI like s3://)
        """
        try:
            import lancedb
        except ImportError:
            raise ImportError(
                "lancedb not installed. Install with: pip install lancedb pyarrow"
            )

        self._path = os.path.expanduser(path)
        self._db = lancedb.connect(self._path)
        self._tables: Dict[str, Any] = {}
        self._indexed: Dict[str, bool] = {}

    def _get_table(self, collection: str):
        """Get or create table for collection."""
        if collection not in self._tables:
            try:
                self._tables[collection] = self._db.open_table(collection)
            except Exception:
                # Table doesn't exist yet, will create on first add
                self._tables[collection] = None
        return self._tables[collection]

    def add(
        self,
        ids: List[str],
        texts: List[str],
        embeddings: List[List[float]],
        metadatas: Optional[List[Dict[str, Any]]] = None,
        collection: str = "default",
    ) -> int:
        """Add documents with upsert semantics (delete existing, then insert)."""
        metadatas = metadatas or [{}] * len(ids)

        # Build records
        records = [
            {
                "id": ids[i],
                "text": texts[i],
                "vector": embeddings[i],
                "metadata": json.dumps(metadatas[i]),
            }
            for i in range(len(ids))
        ]

        table = self._get_table(collection)

        if table is None:
            # Create new table
            self._tables[collection] = self._db.create_table(collection, records)
        else:
            # Upsert: delete existing, then add
            for doc_id in ids:
                try:
                    table.delete(f"id = '{doc_id}'")
                except Exception:
                    pass  # No existing row to delete
            table.add(records)

        # Check if we should create index
        self._maybe_create_index(collection)

        return len(ids)

    def query(
        self,
        embedding: List[float],
        k: int = 5,
        collection: str = "default",
        filter: Optional[Dict[str, Any]] = None,
        include_embeddings: bool = False,
    ) -> List[Dict[str, Any]]:
        """Query for similar documents."""
        table = self._get_table(collection)
        if table is None:
            return []

        # Build query
        query = table.search(embedding).metric("cosine").limit(k)

        # Apply filters
        if filter:
            where_clauses = []
            for key, value in filter.items():
                # Handle filter operators
                if key.endswith("_gte"):
                    field = key[:-4]
                    where_clauses.append(
                        f"json_extract(metadata, '$.{field}') >= {value}"
                    )
                elif key.endswith("_lte"):
                    field = key[:-4]
                    where_clauses.append(
                        f"json_extract(metadata, '$.{field}') <= {value}"
                    )
                elif key.endswith("_gt"):
                    field = key[:-3]
                    where_clauses.append(
                        f"json_extract(metadata, '$.{field}') > {value}"
                    )
                elif key.endswith("_lt"):
                    field = key[:-3]
                    where_clauses.append(
                        f"json_extract(metadata, '$.{field}') < {value}"
                    )
                elif key.endswith("_ne"):
                    field = key[:-3]
                    if isinstance(value, str):
                        where_clauses.append(
                            f"json_extract(metadata, '$.{field}') != '{value}'"
                        )
                    else:
                        where_clauses.append(
                            f"json_extract(metadata, '$.{field}') != {value}"
                        )
                else:
                    # Exact match
                    if isinstance(value, str):
                        where_clauses.append(
                            f"json_extract(metadata, '$.{key}') = '{value}'"
                        )
                    else:
                        where_clauses.append(
                            f"json_extract(metadata, '$.{key}') = {value}"
                        )
            if where_clauses:
                query = query.where(" AND ".join(where_clauses))

        results = query.to_list()

        # Format results
        output = []
        for row in results:
            result = {
                "id": row["id"],
                "text": row["text"],
                "score": 1 - row["_distance"],  # Convert distance to similarity
                "metadata": json.loads(row["metadata"]) if row.get("metadata") else {},
            }
            if include_embeddings:
                result["embedding"] = row["vector"]
            output.append(result)

        return output

    def _maybe_create_index(self, collection: str):
        """Create vector index if threshold reached."""
        if self._indexed.get(collection):
            return

        table = self._get_table(collection)
        if table and table.count_rows() >= self.INDEX_THRESHOLD:
            try:
                table.create_index(metric="cosine")
                self._indexed[collection] = True
            except Exception:
                pass  # Index might already exist

    def get_state(self) -> Dict[str, Any]:
        """Get state for checkpointing."""
        return {
            "type": "lancedb",
            "path": self._path,
            "collections": list(self._tables.keys()),
        }

    def restore_state(self, state: Dict[str, Any]) -> None:
        """Restore from checkpoint (no-op, LanceDB is persistent)."""
        pass


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
                name=name, metadata={"hnsw:space": "cosine"}
            )
        return self._collections[name]

    def add(
        self,
        ids: List[str],
        texts: List[str],
        embeddings: List[List[float]],
        metadatas: Optional[List[Dict[str, Any]]] = None,
        collection: str = "default",
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
        include_embeddings: bool = False,
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
            query_embeddings=[embedding], n_results=k, where=where, include=include
        )

        # Format results
        output = []
        if results["ids"] and results["ids"][0]:
            for i, doc_id in enumerate(results["ids"][0]):
                result = {
                    "id": doc_id,
                    "text": (
                        results["documents"][0][i] if results.get("documents") else ""
                    ),
                    "score": 1
                    - results["distances"][0][i],  # Convert distance to similarity
                    "metadata": (
                        results["metadatas"][0][i] if results.get("metadatas") else {}
                    ),
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
    timeout: float = 60.0,
) -> EmbeddingProvider:
    """
    Factory function to create embedding providers.

    Args:
        provider: Provider type - "openai", "ollama", or "model2vec"
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
            dimensions=dimensions,
        )
    elif provider == "ollama":
        return OllamaEmbeddingProvider(
            model=model or "nomic-embed-text",
            base_url=base_url or "http://localhost:11434",
            timeout=timeout,
        )
    elif provider == "model2vec":
        return Model2VecEmbeddingProvider(
            model=model  # Uses default MODEL_NAME if None
        )
    else:
        raise ValueError(f"Unknown embedding provider: {provider}")


def create_vector_store(
    store_type: str = "memory",
    chroma_path: Optional[str] = None,
    lancedb_path: Optional[str] = None,
) -> VectorStore:
    """
    Factory function to create vector stores.

    Args:
        store_type: Store type - "memory", "chroma", or "lancedb"
        chroma_path: Path for Chroma persistence
        lancedb_path: Path for LanceDB persistence (default: ~/.tea/vectors/)

    Returns:
        VectorStore instance
    """
    if store_type == "memory":
        return InMemoryVectorStore()
    elif store_type == "chroma":
        return ChromaVectorStore(persist_directory=chroma_path)
    elif store_type == "lancedb":
        return LanceDBVectorStore(path=lancedb_path or "~/.tea/vectors/")
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
    if not hasattr(engine, "_rag_provider"):
        engine._rag_provider = None
    if not hasattr(engine, "_rag_vector_store"):
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
        if hasattr(engine, "variables"):
            settings = engine.variables.get("settings", {})
            rag_settings = settings.get("rag", {})
            provider_type = rag_settings.get("embedding_provider", provider_type)
            model = model or rag_settings.get("embedding_model")
            base_url = (
                base_url
                or rag_settings.get("openai_base_url")
                or rag_settings.get("ollama_base_url")
            )
            timeout = rag_settings.get("ollama_timeout", timeout)

        # Create or reuse provider
        if engine._rag_provider is None:
            engine._rag_provider = create_embedding_provider(
                provider=provider_type,
                model=model,
                base_url=base_url,
                api_key=api_key,
                dimensions=dimensions,
                timeout=timeout,
            )

        return engine._rag_provider

    def _get_vector_store(**kwargs) -> VectorStore:
        """Get or create vector store based on config."""
        store_type = kwargs.get("store_type", "memory")
        chroma_path = kwargs.get("chroma_path")
        lancedb_path = kwargs.get("lancedb_path")

        # Check engine settings
        if hasattr(engine, "variables"):
            settings = engine.variables.get("settings", {})
            rag_settings = settings.get("rag", {})
            store_type = rag_settings.get("vector_store", store_type)
            chroma_path = chroma_path or rag_settings.get("chroma_path")
            lancedb_path = lancedb_path or rag_settings.get("lancedb_path")

        # Create or reuse store
        if engine._rag_vector_store is None:
            engine._rag_vector_store = create_vector_store(
                store_type=store_type,
                chroma_path=chroma_path,
                lancedb_path=lancedb_path,
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
        **kwargs,
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
                    "dimensions": provider.dimensions,
                }
            else:
                return {
                    "embedding": embeddings[0],
                    "model": provider.model_name,
                    "dimensions": provider.dimensions,
                }

        except Exception as e:
            return {"error": f"Embedding creation failed: {str(e)}", "success": False}

    registry["embedding.create"] = embedding_create
    registry["actions.embedding_create"] = embedding_create

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
        **kwargs,
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
                    "success": False,
                }

            # Auto-generate embeddings if not provided
            if embeddings is None:
                provider = _get_provider(state, **kwargs)
                embeddings = provider.embed(texts)
            elif len(embeddings) != len(texts):
                return {
                    "error": f"Number of embeddings ({len(embeddings)}) doesn't match texts ({len(texts)})",
                    "success": False,
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
                collection=collection,
            )

            return {"stored": stored, "collection": collection, "ids": ids}

        except Exception as e:
            return {"error": f"Vector store failed: {str(e)}", "success": False}

    registry["vector.store"] = vector_store
    registry["actions.vector_store"] = vector_store

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
        **kwargs,
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
                    return {"error": "Filter must be a dictionary", "success": False}

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
                include_embeddings=include_embeddings,
            )

            return {
                "results": results,
                "query": query,
                "collection": collection,
                "k": k,
            }

        except Exception as e:
            return {"error": f"Vector query failed: {str(e)}", "success": False}

    registry["vector.query"] = vector_query
    registry["actions.vector_query"] = vector_query

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

    # =========================================================================
    # File Indexing Helpers (TEA-BUILTIN-002.3)
    # =========================================================================

    def _list_files(
        paths: List[str],
        pattern: str = "**/*",
        recursive: bool = True,
        extensions: Optional[List[str]] = None,
    ) -> List[str]:
        """
        List files matching pattern and extensions (AC: 2, 3, 5, 6).

        Args:
            paths: List of file/directory paths (local or fsspec URIs)
            pattern: Glob pattern (default: "**/*")
            recursive: Whether to traverse directories recursively
            extensions: List of file extensions to filter (e.g., [".py", ".md"])

        Returns:
            List of file paths matching criteria
        """
        files = []
        extensions_lower = [e.lower() for e in (extensions or [])]

        for path in paths:
            # Check if it's a URI (contains ://) or local path
            is_uri = "://" in path

            if is_uri:
                if not FSSPEC_AVAILABLE:
                    raise ImportError(
                        "fsspec not installed. Install with: pip install fsspec "
                        "(and s3fs for S3, gcsfs for GCS)"
                    )
                fs, resolved_path = fsspec.core.url_to_fs(path)
            else:
                fs = None
                resolved_path = os.path.abspath(os.path.expanduser(path))

            # Check if path is a file or directory
            if fs:
                is_file = fs.isfile(resolved_path)
                is_dir = fs.isdir(resolved_path)
            else:
                is_file = os.path.isfile(resolved_path)
                is_dir = os.path.isdir(resolved_path)

            if is_file:
                # Single file
                if extensions_lower:
                    _, ext = os.path.splitext(resolved_path)
                    if ext.lower() not in extensions_lower:
                        continue
                files.append(path)
            elif is_dir:
                # Directory - apply glob pattern
                if fs:
                    # fsspec glob
                    full_pattern = f"{resolved_path.rstrip('/')}/{pattern}"
                    matches = fs.glob(full_pattern)
                    for match in matches:
                        if fs.isfile(match):
                            if extensions_lower:
                                _, ext = os.path.splitext(match)
                                if ext.lower() not in extensions_lower:
                                    continue
                            # Reconstruct URI
                            proto = path.split("://")[0]
                            files.append(f"{proto}://{match}")
                else:
                    # Local glob
                    import glob as glob_module

                    if recursive:
                        full_pattern = os.path.join(resolved_path, pattern)
                    else:
                        # Non-recursive: only top-level
                        full_pattern = os.path.join(resolved_path, "*")
                    matches = glob_module.glob(full_pattern, recursive=recursive)
                    for match in matches:
                        if os.path.isfile(match):
                            if extensions_lower:
                                _, ext = os.path.splitext(match)
                                if ext.lower() not in extensions_lower:
                                    continue
                            files.append(match)

        return files

    def _chunk_file(content: str, strategy: ChunkStrategy) -> List[Chunk]:
        """
        Chunk file content based on strategy (AC: 4, 7).

        Args:
            content: File content as string
            strategy: Chunking strategy

        Returns:
            List of Chunk objects with line information
        """
        chunks = []
        lines = content.split("\n")

        if strategy == ChunkStrategy.LINE:
            # Line-by-line chunking
            for i, line in enumerate(lines):
                if line.strip():  # Skip empty lines
                    chunks.append(
                        Chunk(
                            text=line, start_line=i, end_line=i + 1, chunk_type="line"
                        )
                    )

        elif strategy == ChunkStrategy.PARAGRAPH:
            # Paragraph chunking (split on double newlines)
            paragraphs = []
            current_para = []
            current_start = 0
            line_idx = 0

            for i, line in enumerate(lines):
                if not line.strip():  # Empty line
                    if current_para:
                        paragraphs.append((current_para, current_start, i))
                        current_para = []
                else:
                    if not current_para:
                        current_start = i
                    current_para.append(line)
                line_idx = i

            # Handle last paragraph
            if current_para:
                paragraphs.append((current_para, current_start, line_idx + 1))

            for para_lines, start, end in paragraphs:
                text = "\n".join(para_lines)
                if text.strip():
                    chunks.append(
                        Chunk(
                            text=text,
                            start_line=start,
                            end_line=end,
                            chunk_type="paragraph",
                        )
                    )

        elif strategy == ChunkStrategy.DOCUMENT:
            # Entire document as one chunk
            if content.strip():
                chunks.append(
                    Chunk(
                        text=content,
                        start_line=0,
                        end_line=len(lines),
                        chunk_type="document",
                    )
                )

        return chunks

    def _get_file_state(path: str) -> Optional[FileState]:
        """
        Get file state for change detection (AC: 8).

        Args:
            path: File path (local or URI)

        Returns:
            FileState or None if file doesn't exist
        """
        try:
            is_uri = "://" in path

            if is_uri:
                if not FSSPEC_AVAILABLE:
                    return None
                fs, resolved_path = fsspec.core.url_to_fs(path)
                info = fs.info(resolved_path)
                return FileState(
                    path=path,
                    size=info.get("size", 0),
                    mtime=info.get("mtime", 0) or info.get("LastModified", 0) or 0,
                    indexed_at=0,
                )
            else:
                stat = os.stat(path)
                return FileState(
                    path=path, size=stat.st_size, mtime=stat.st_mtime, indexed_at=0
                )
        except Exception:
            return None

    def _read_file(path: str) -> Optional[str]:
        """
        Read file content.

        Args:
            path: File path (local or URI)

        Returns:
            File content as string, or None if read fails
        """
        try:
            is_uri = "://" in path

            if is_uri:
                if not FSSPEC_AVAILABLE:
                    return None
                with fsspec.open(path, "r", encoding="utf-8", errors="ignore") as f:
                    return f.read()
            else:
                with open(path, "r", encoding="utf-8", errors="ignore") as f:
                    return f.read()
        except Exception:
            return None

    def _is_binary_file(path: str) -> bool:
        """
        Check if a file is binary (AC: 16).

        Args:
            path: File path

        Returns:
            True if file appears to be binary
        """
        try:
            is_uri = "://" in path

            if is_uri:
                if not FSSPEC_AVAILABLE:
                    return True
                with fsspec.open(path, "rb") as f:
                    chunk = f.read(8192)
            else:
                with open(path, "rb") as f:
                    chunk = f.read(8192)

            # Check for null bytes (common in binary files)
            if b"\x00" in chunk:
                return True

            # Check for high ratio of non-text characters
            text_chars = bytearray({7, 8, 9, 10, 12, 13, 27} | set(range(0x20, 0x100)))
            non_text = sum(1 for b in chunk if b not in text_chars)
            if len(chunk) > 0 and non_text / len(chunk) > 0.3:
                return True

            return False
        except Exception:
            return True

    # =========================================================================
    # vector.index_files (AC: 1-17, TEA-BUILTIN-002.3)
    # =========================================================================

    def vector_index_files(
        state: Dict[str, Any],
        paths: Union[str, List[str]],
        pattern: str = "**/*",
        chunk_by: str = "line",
        collection: str = "default",
        recursive: bool = True,
        extensions: Optional[List[str]] = None,
        incremental: bool = True,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Index files/directories into vector store (AC: 1-13).

        Reads files from local or cloud storage, chunks them based on
        strategy, generates embeddings, and stores in vector store.

        Args:
            state: Current state dictionary
            paths: File/directory paths (local or fsspec URIs like s3://, gs://)
            pattern: Glob pattern for file filtering (default: "**/*")
            chunk_by: Chunking strategy - "line", "paragraph", or "document"
            collection: Vector store collection name
            recursive: Whether to traverse directories recursively
            extensions: File extensions to filter (e.g., [".py", ".md"])
            incremental: Skip unchanged files (default: True)
            **kwargs: Additional embedding/store configuration

        Returns:
            {
                "success": True,
                "indexed": int,
                "skipped": int,
                "errors": List[str],
                "collection": str,
                "files": int
            }
            Error: {"success": False, "error": str}

        Example YAML:
            - name: index_codebase
              uses: vector.index_files
              with:
                paths:
                  - src/
                  - docs/
                pattern: "**/*.py"
                chunk_by: line
                collection: codebase
                extensions: [".py", ".md"]
              output: index_result
        """
        try:
            # Normalize paths to list
            if isinstance(paths, str):
                paths = [paths]

            # Parse chunk strategy
            try:
                strategy = ChunkStrategy(chunk_by.lower())
            except ValueError:
                return {
                    "success": False,
                    "error": f"Invalid chunk_by value: {chunk_by}. "
                    f"Must be one of: line, paragraph, document",
                }

            # Get provider and store
            provider = _get_provider(state, **kwargs)
            store = _get_vector_store(**kwargs)

            # List files
            try:
                files = _list_files(
                    paths=paths,
                    pattern=pattern,
                    recursive=recursive,
                    extensions=extensions,
                )
            except ImportError as e:
                return {"success": False, "error": str(e)}
            except Exception as e:
                return {"success": False, "error": f"File listing failed: {str(e)}"}

            # Get file states collection name for tracking
            state_collection = f"_file_states_{collection}"

            indexed = 0
            skipped = 0
            errors = []

            for file_path in files:
                try:
                    # Check if binary
                    if _is_binary_file(file_path):
                        skipped += 1
                        continue

                    # Get file state
                    file_state = _get_file_state(file_path)
                    if file_state is None:
                        errors.append(f"Cannot access: {file_path}")
                        continue

                    # Check for incremental update
                    if incremental:
                        # Query for existing file state
                        existing = store.query(
                            embedding=[0.0] * provider.dimensions,  # Dummy embedding
                            k=1,
                            collection=state_collection,
                            filter={"path": file_path},
                        )
                        if existing:
                            stored_state = existing[0].get("metadata", {})
                            if (
                                stored_state.get("size") == file_state.size
                                and stored_state.get("mtime") == file_state.mtime
                            ):
                                skipped += 1
                                continue

                    # Read file
                    content = _read_file(file_path)
                    if content is None:
                        errors.append(f"Cannot read: {file_path}")
                        continue

                    # Chunk content
                    chunks = _chunk_file(content, strategy)
                    if not chunks:
                        skipped += 1
                        continue

                    # Prepare texts and metadata
                    texts = [c.text for c in chunks]
                    ids = [f"{file_path}:{c.start_line}" for c in chunks]
                    metadatas = [
                        {
                            "file": file_path,
                            "line": c.start_line,
                            "start_line": c.start_line,
                            "end_line": c.end_line,
                            "chunk_type": c.chunk_type,
                        }
                        for c in chunks
                    ]

                    # Generate embeddings
                    embeddings = provider.embed(texts)

                    # Store chunks
                    store.add(
                        ids=ids,
                        texts=texts,
                        embeddings=embeddings,
                        metadatas=metadatas,
                        collection=collection,
                    )

                    indexed += len(chunks)

                    # Store file state for incremental updates
                    if incremental:
                        state_id = f"state:{file_path}"
                        state_embedding = [0.0] * provider.dimensions  # Dummy
                        store.add(
                            ids=[state_id],
                            texts=[file_path],
                            embeddings=[state_embedding],
                            metadatas=[
                                {
                                    "path": file_path,
                                    "size": file_state.size,
                                    "mtime": file_state.mtime,
                                    "indexed_at": time.time(),
                                }
                            ],
                            collection=state_collection,
                        )

                except Exception as e:
                    errors.append(f"Error indexing {file_path}: {str(e)}")

            return {
                "success": True,
                "indexed": indexed,
                "skipped": skipped,
                "errors": errors,
                "collection": collection,
                "files": len(files),
            }

        except Exception as e:
            return {"success": False, "error": f"File indexing failed: {str(e)}"}

    registry["vector.index_files"] = vector_index_files
    registry["actions.vector_index_files"] = vector_index_files
