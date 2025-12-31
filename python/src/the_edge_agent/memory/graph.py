"""
Graph Database Backends (TEA-BUILTIN-001.4).

This module provides graph database backends for entity-relationship storage
with Datalog/Cypher queries and optional vector search capabilities.

Available Backends:
    - CozoBackend: CozoDB with Datalog queries and HNSW vector search
    - KuzuBackend: Kuzu (Bighorn) with Cypher queries and cloud storage

Example:
    >>> from the_edge_agent.memory import CozoBackend
    >>>
    >>> backend = CozoBackend("./agent_graph.db")
    >>> result = backend.store_entity("person_1", "Person", {"name": "Alice"})
    >>> print(result['success'])  # True
    >>> result = backend.store_relation("person_1", "person_2", "KNOWS")
    >>> print(result['success'])  # True
    >>> backend.close()
"""

import json
import os
import threading
from typing import Any, Callable, Dict, List, Optional, Protocol


class GraphBackend(Protocol):
    """
    Protocol for graph database backends (TEA-BUILTIN-001.4).

    Graph backends provide entity-relationship storage with Datalog queries
    and optional vector search capabilities. Designed for knowledge graphs
    and entity memory in AI agents.

    All methods return dictionaries with consistent error format:
        Success: {"success": True, ...additional fields...}
        Failure: {"success": False, "error": str, "error_type": str}

    Error types:
        - validation_error: Invalid input parameters
        - connection_error: Database connection issues
        - query_error: Query/Datalog execution failure
        - dependency_missing: Required library not installed
    """

    def store_entity(
        self,
        entity_id: str,
        entity_type: str,
        properties: Optional[Dict[str, Any]] = None,
        embedding: Optional[List[float]] = None,
    ) -> Dict[str, Any]:
        """
        Store an entity (node) in the graph.

        Args:
            entity_id: Unique identifier for the entity
            entity_type: Type/label of the entity (e.g., "Person", "Document")
            properties: Optional properties dict (will be JSON serialized)
            embedding: Optional vector embedding for semantic search

        Returns:
            {"success": True, "entity_id": str, "type": str, "created": bool, "has_embedding": bool}

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def store_relation(
        self,
        from_entity: str,
        to_entity: str,
        relation_type: str,
        properties: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Store a relation (edge) between two entities.

        Args:
            from_entity: Source entity ID
            to_entity: Target entity ID
            relation_type: Type of the relationship (e.g., "KNOWS", "MENTIONS")
            properties: Optional properties dict

        Returns:
            {"success": True, "from": str, "to": str, "type": str, "created": bool}

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def query(
        self,
        datalog: Optional[str] = None,
        pattern: Optional[Dict[str, Any]] = None,
        params: Optional[Dict[str, Any]] = None,
        limit: int = 100,
        timeout: Optional[float] = None,
    ) -> Dict[str, Any]:
        """
        Execute a Datalog query or pattern match.

        Args:
            datalog: Raw Datalog query string
            pattern: Simplified pattern dict (alternative to raw Datalog)
            params: Query parameters (substituted into query)
            limit: Maximum results to return
            timeout: Query timeout in seconds (None for no timeout)

        Returns:
            {
                "success": True,
                "results": list,
                "count": int,
                "query": str (the executed query)
            }

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def retrieve_context(
        self,
        query: Optional[str] = None,
        embedding: Optional[List[float]] = None,
        entity_id: Optional[str] = None,
        hops: int = 2,
        limit: int = 20,
    ) -> Dict[str, Any]:
        """
        Retrieve relevant subgraph context.

        Can be called with:
        - query: Text query (converted to embedding for HNSW search)
        - embedding: Direct embedding vector for HNSW search
        - entity_id: Start from entity and expand N hops

        Args:
            query: Text query for semantic search
            embedding: Direct embedding vector
            entity_id: Entity ID to start neighborhood expansion from
            hops: Number of relationship hops to traverse (default: 2)
            limit: Maximum entities to return

        Returns:
            {
                "success": True,
                "entities": list,
                "relations": list,
                "context_summary": str
            }

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def close(self) -> None:
        """
        Close the backend and release resources.

        Should be called when the backend is no longer needed.
        Safe to call multiple times.
        """
        ...


# =============================================================================
# COZO AVAILABILITY CHECK
# =============================================================================


def _check_cozo_available() -> bool:
    """Check if CozoDB is available."""
    try:
        from pycozo import Client  # noqa: F401

        return True
    except ImportError:
        return False


COZO_AVAILABLE = _check_cozo_available()


# =============================================================================
# KUZU AVAILABILITY CHECK
# =============================================================================


def _check_kuzu_available() -> bool:
    """Check if Kuzu (Bighorn) is available."""
    try:
        import kuzu  # noqa: F401

        return True
    except ImportError:
        return False


KUZU_AVAILABLE = _check_kuzu_available()


# =============================================================================
# NEO4J AVAILABILITY CHECK
# =============================================================================


def _check_neo4j_available() -> bool:
    """Check if Neo4j is available."""
    try:
        import neo4j  # noqa: F401

        return True
    except ImportError:
        return False


NEO4J_AVAILABLE = _check_neo4j_available()


# =============================================================================
# COZO BACKEND IMPLEMENTATION
# =============================================================================


class CozoBackend:
    """
    CozoDB implementation of GraphBackend.

    Provides graph database functionality with Datalog queries and
    optional HNSW vector search for semantic retrieval. Uses SQLite
    as the storage engine for maximum compatibility.

    Requires: pip install "pycozo[embedded]"

    Example:
        >>> backend = CozoBackend("./agent_graph.db")
        >>> result = backend.store_entity("person_1", "Person", {"name": "Alice"})
        >>> print(result['success'])  # True
        >>> result = backend.store_relation("person_1", "person_2", "KNOWS")
        >>> print(result['success'])  # True
        >>> result = backend.query(datalog="?[id, type] := entity[id, type, _, _, _]")
        >>> print(result['results'])  # [{'id': 'person_1', 'type': 'Person'}, ...]
        >>> backend.close()
    """

    # Default embedding dimensions (OpenAI text-embedding-3-small)
    DEFAULT_EMBEDDING_DIM = 1536

    def __init__(self, db_path: str = ":memory:", embedding_dim: int = None):
        """
        Initialize CozoDB backend.

        Args:
            db_path: Path to SQLite database file, or ":memory:" for in-memory
            embedding_dim: Embedding dimensions for HNSW index (default: 1536)

        Raises:
            ImportError: If pycozo is not installed
        """
        if not COZO_AVAILABLE:
            raise ImportError(
                "CozoDB not installed. Install with: pip install 'pycozo[embedded]'"
            )

        from pycozo import Client

        self.db_path = db_path
        self.embedding_dim = embedding_dim or self.DEFAULT_EMBEDDING_DIM
        self._lock = threading.Lock()
        self._closed = False
        self._hnsw_created = False

        # Initialize CozoDB with SQLite storage
        if db_path == ":memory:":
            self._client = Client("mem")
        else:
            self._client = Client("sqlite", db_path)

        self._init_schema()

    def _init_schema(self) -> None:
        """Initialize graph schema with entity and relation tables."""
        # Create entity table
        try:
            self._client.run(
                """
                :create entity {
                    id: String =>
                    type: String,
                    properties: String,
                    embedding: <F32; 1536>?,
                    created_at: Float default now()
                }
            """
            )
        except Exception:
            # Table might already exist
            pass

        # Create relation table
        try:
            self._client.run(
                """
                :create relation {
                    from_id: String,
                    to_id: String,
                    rel_type: String =>
                    properties: String?,
                    created_at: Float default now()
                }
            """
            )
        except Exception:
            # Table might already exist
            pass

    def _ensure_hnsw_index(self) -> bool:
        """
        Create HNSW index if not exists.

        Returns:
            True if index exists/created, False on failure
        """
        if self._hnsw_created:
            return True

        try:
            self._client.run(
                f"""
                ::hnsw create entity:semantic_idx {{
                    dim: {self.embedding_dim},
                    m: 50,
                    ef_construction: 200,
                    fields: [embedding],
                    distance: Cosine
                }}
            """
            )
            self._hnsw_created = True
            return True
        except Exception:
            # Index might already exist or other error
            self._hnsw_created = True  # Assume it exists
            return True

    def _parse_result(self, result: Any) -> List[List]:
        """
        Parse pycozo result into a list of rows.

        pycozo returns pandas DataFrames in newer versions.
        Legacy format was: {'headers': [...], 'rows': [...], 'next': None}
        """
        # Handle pandas DataFrame (current pycozo format)
        if hasattr(result, "values") and hasattr(result, "columns"):
            return result.values.tolist()
        # Handle dict with rows key (legacy format)
        elif isinstance(result, dict) and "rows" in result:
            return result["rows"]
        elif hasattr(result, "rows"):
            return result.rows
        elif isinstance(result, (list, tuple)):
            return list(result)
        return []

    def store_entity(
        self,
        entity_id: str,
        entity_type: str,
        properties: Optional[Dict[str, Any]] = None,
        embedding: Optional[List[float]] = None,
    ) -> Dict[str, Any]:
        """Store an entity (node) in the graph."""
        if not entity_id or not entity_type:
            return {
                "success": False,
                "error": "entity_id and entity_type are required",
                "error_type": "validation_error",
            }

        try:
            entity_id_str = str(entity_id)
            entity_type_str = str(entity_type)
            props_json = json.dumps(properties or {})

            with self._lock:
                # Check if entity exists
                result = self._client.run(
                    "?[id] := *entity[id, _, _, _, _], id = $id", {"id": entity_id_str}
                )
                rows = self._parse_result(result)
                exists = len(rows) > 0

                if embedding:
                    # Ensure HNSW index exists
                    self._ensure_hnsw_index()

                    # Store with embedding
                    self._client.run(
                        """
                        ?[id, type, properties, embedding, created_at] <- [[$id, $type, $props, $emb, now()]]
                        :put entity {id => type, properties, embedding, created_at}
                    """,
                        {
                            "id": entity_id_str,
                            "type": entity_type_str,
                            "props": props_json,
                            "emb": embedding,
                        },
                    )
                else:
                    # Store without embedding
                    self._client.run(
                        """
                        ?[id, type, properties, embedding, created_at] <- [[$id, $type, $props, null, now()]]
                        :put entity {id => type, properties, embedding, created_at}
                    """,
                        {
                            "id": entity_id_str,
                            "type": entity_type_str,
                            "props": props_json,
                        },
                    )

                return {
                    "success": True,
                    "entity_id": entity_id_str,
                    "type": entity_type_str,
                    "created": not exists,
                    "has_embedding": embedding is not None,
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store entity: {str(e)}",
                "error_type": "query_error",
            }

    def store_relation(
        self,
        from_entity: str,
        to_entity: str,
        relation_type: str,
        properties: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """Store a relation (edge) between two entities."""
        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error",
            }

        try:
            from_id = str(from_entity)
            to_id = str(to_entity)
            rel_type = str(relation_type)
            props_json = json.dumps(properties) if properties else None

            with self._lock:
                # Check if relation exists
                result = self._client.run(
                    "?[from_id, to_id, rel_type] := *relation[from_id, to_id, rel_type, _, _], from_id = $from, to_id = $to, rel_type = $rel",
                    {"from": from_id, "to": to_id, "rel": rel_type},
                )
                rows = self._parse_result(result)
                exists = len(rows) > 0

                # Store relation
                self._client.run(
                    """
                    ?[from_id, to_id, rel_type, properties, created_at] <- [[$from, $to, $rel, $props, now()]]
                    :put relation {from_id, to_id, rel_type => properties, created_at}
                """,
                    {
                        "from": from_id,
                        "to": to_id,
                        "rel": rel_type,
                        "props": props_json,
                    },
                )

                return {
                    "success": True,
                    "from": from_id,
                    "to": to_id,
                    "type": rel_type,
                    "created": not exists,
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store relation: {str(e)}",
                "error_type": "query_error",
            }

    def query(
        self,
        datalog: Optional[str] = None,
        cypher: Optional[str] = None,
        pattern: Optional[Dict[str, Any]] = None,
        params: Optional[Dict[str, Any]] = None,
        limit: int = 100,
        timeout: Optional[float] = None,
    ) -> Dict[str, Any]:
        """Execute a Datalog query or pattern match."""
        # CozoDB uses Datalog, not Cypher
        if cypher and not datalog:
            return {
                "success": False,
                "error": "CozoBackend uses Datalog, not Cypher. Use 'datalog' parameter instead, "
                "or use KuzuBackend for Cypher support.",
                "error_type": "validation_error",
            }

        if not datalog and not pattern:
            return {
                "success": False,
                "error": "Either datalog or pattern is required",
                "error_type": "validation_error",
            }

        try:
            query_str = datalog

            if pattern and not datalog:
                # Convert pattern to Datalog
                query_str = self._pattern_to_datalog(pattern, limit)

            # Add limit if not already in query
            if query_str and ":limit" not in query_str.lower():
                query_str = f"{query_str}\n:limit {limit}"

            with self._lock:
                result = self._client.run(query_str, params or {})

                # Convert result to list of dicts
                results = []
                rows = self._parse_result(result)

                # Extract headers from DataFrame or dict
                if hasattr(result, "columns"):
                    headers = list(result.columns)
                elif isinstance(result, dict):
                    headers = result.get("headers", [])
                else:
                    headers = []

                for row in rows:
                    if headers:
                        results.append(dict(zip(headers, row)))
                    else:
                        results.append(list(row))

                return {
                    "success": True,
                    "results": results,
                    "count": len(results),
                    "query": query_str,
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Query failed: {str(e)}",
                "error_type": "query_error",
            }

    def _pattern_to_datalog(self, pattern: Dict[str, Any], limit: int) -> str:
        """Convert a simple pattern dict to Datalog query."""
        # Simple pattern: {entity_type: "Person"} -> find all Person entities
        if "entity_type" in pattern:
            return f"""
                ?[id, type, properties] :=
                    *entity[id, type, properties, _, _],
                    type = "{pattern['entity_type']}"
                :limit {limit}
            """

        # Simple pattern: {from_entity: "X"} -> find relations from X
        if "from_entity" in pattern:
            return f"""
                ?[to_id, rel_type, properties] :=
                    *relation[from_id, to_id, rel_type, properties, _],
                    from_id = "{pattern['from_entity']}"
                :limit {limit}
            """

        # Default: return all entities
        return f"""
            ?[id, type, properties] := *entity[id, type, properties, _, _]
            :limit {limit}
        """

    def retrieve_context(
        self,
        query: Optional[str] = None,
        embedding: Optional[List[float]] = None,
        entity_id: Optional[str] = None,
        hops: int = 2,
        limit: int = 20,
    ) -> Dict[str, Any]:
        """Retrieve relevant subgraph context."""
        if not query and not embedding and not entity_id:
            return {
                "success": False,
                "error": "One of query, embedding, or entity_id is required",
                "error_type": "validation_error",
            }

        try:
            entities = []
            relations = []

            with self._lock:
                if entity_id:
                    # Expand from entity via N-hop traversal
                    entities, relations = self._expand_from_entity(
                        entity_id, hops, limit
                    )

                elif embedding:
                    # HNSW vector search
                    self._ensure_hnsw_index()
                    entities = self._hnsw_search(embedding, limit)

                    # Get relations between found entities
                    if entities:
                        entity_ids = [e["id"] for e in entities]
                        relations = self._get_relations_between(entity_ids)

                # Build context summary
                context_summary = self._build_context_summary(entities, relations)

                return {
                    "success": True,
                    "entities": entities,
                    "relations": relations,
                    "context_summary": context_summary,
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to retrieve context: {str(e)}",
                "error_type": "query_error",
            }

    def _expand_from_entity(self, entity_id: str, hops: int, limit: int) -> tuple:
        """Expand N hops from an entity."""
        entities = []
        relations = []

        # Get starting entity
        result = self._client.run(
            "?[id, type, properties] := *entity[id, type, properties, _, _], id = $id",
            {"id": entity_id},
        )

        rows = self._parse_result(result)
        for row in rows:
            entities.append(
                {
                    "id": row[0],
                    "type": row[1],
                    "properties": json.loads(row[2]) if row[2] else {},
                }
            )

        # Build recursive query for N hops
        if hops >= 1:
            hop_query = f"""
                hop1[id] := *relation["{entity_id}", id, _, _, _]
                hop1[id] := *relation[id, "{entity_id}", _, _, _]
            """

            for h in range(2, hops + 1):
                hop_query += f"""
                hop{h}[id] := hop{h-1}[mid], *relation[mid, id, _, _, _]
                hop{h}[id] := hop{h-1}[mid], *relation[id, mid, _, _, _]
                """

            # Combine all hops
            all_hops = " ".join(
                [f"all_connected[id] := hop{i}[id]" for i in range(1, hops + 1)]
            )

            query = f"""
                {hop_query}
                {all_hops}
                ?[id, type, properties] :=
                    all_connected[id],
                    *entity[id, type, properties, _, _]
                :limit {limit}
            """

            result = self._client.run(query)
            rows = self._parse_result(result)
            for row in rows:
                if row[0] != entity_id:  # Don't duplicate start entity
                    entities.append(
                        {
                            "id": row[0],
                            "type": row[1],
                            "properties": json.loads(row[2]) if row[2] else {},
                        }
                    )

        # Get relations
        entity_ids = [e["id"] for e in entities]
        if entity_ids:
            relations = self._get_relations_between(entity_ids)

        return entities, relations

    def _hnsw_search(self, embedding: List[float], limit: int) -> List[Dict]:
        """Perform HNSW vector similarity search."""
        entities = []

        try:
            result = self._client.run(
                f"""
                ?[id, type, properties, score] :=
                    ~entity:semantic_idx {{id, type, properties |
                        query: $query_embedding,
                        k: {limit},
                        ef: 50,
                        bind_distance: score
                    }}
            """,
                {"query_embedding": embedding},
            )

            rows = self._parse_result(result)
            for row in rows:
                entities.append(
                    {
                        "id": row[0],
                        "type": row[1],
                        "properties": json.loads(row[2]) if row[2] else {},
                        "score": float(row[3]),
                    }
                )
        except Exception:
            # HNSW might not be available or no embeddings stored
            pass

        return entities

    def _get_relations_between(self, entity_ids: List[str]) -> List[Dict]:
        """Get all relations between a set of entities."""
        relations = []

        if not entity_ids:
            return relations

        # Build query to find relations within the set
        ids_str = ", ".join([f'"{id}"' for id in entity_ids])
        query = f"""
            ?[from_id, to_id, rel_type, properties] :=
                *relation[from_id, to_id, rel_type, properties, _],
                from_id in [{ids_str}],
                to_id in [{ids_str}]
        """

        try:
            result = self._client.run(query)
            rows = self._parse_result(result)
            for row in rows:
                relations.append(
                    {
                        "from": row[0],
                        "to": row[1],
                        "type": row[2],
                        "properties": json.loads(row[3]) if row[3] else {},
                    }
                )
        except Exception:
            pass

        return relations

    def _build_context_summary(
        self, entities: List[Dict], relations: List[Dict]
    ) -> str:
        """Build a text summary of the context."""
        if not entities:
            return "No relevant entities found."

        summary_parts = []

        # Summarize entities by type
        type_counts = {}
        for e in entities:
            t = e.get("type", "Unknown")
            type_counts[t] = type_counts.get(t, 0) + 1

        entity_summary = ", ".join([f"{count} {t}" for t, count in type_counts.items()])
        summary_parts.append(f"Found {len(entities)} entities: {entity_summary}.")

        # Summarize relations
        if relations:
            rel_types = set(r.get("type", "Unknown") for r in relations)
            summary_parts.append(
                f"Connected by {len(relations)} relations ({', '.join(rel_types)})."
            )

        return " ".join(summary_parts)

    def close(self) -> None:
        """Close the backend and release resources."""
        self._closed = True

        if hasattr(self, "_client") and self._client is not None:
            try:
                # CozoDB doesn't have explicit close, but we can clear reference
                self._client = None
            except Exception:
                pass

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass


# =============================================================================
# KUZU BACKEND IMPLEMENTATION (BIGHORN - CLOUD-NATIVE GRAPH)
# =============================================================================


class KuzuBackend:
    """
    Kuzu (Bighorn) implementation of GraphBackend.

    Provides graph database functionality with Cypher queries and
    cloud storage support via httpfs extension. Designed for serverless
    deployments with direct S3/GCS/Azure I/O.

    Requires: pip install kuzu

    Supported URI schemes:
        - file:///path/to/graph.kuzu or ./graph.kuzu - Local filesystem
        - s3://bucket/graph/ - AWS S3 (requires httpfs + credentials)
        - gs://bucket/graph/ - Google Cloud Storage (via HMAC)
        - az://container/graph/ - Azure Blob (read-only)

    Example:
        >>> backend = KuzuBackend("./agent_graph.kuzu")
        >>> result = backend.store_entity("person_1", "Person", {"name": "Alice"})
        >>> print(result['success'])  # True
        >>> result = backend.store_relation("person_1", "person_2", "KNOWS")
        >>> print(result['success'])  # True
        >>> result = backend.query(cypher="MATCH (p:Entity) RETURN p.id, p.type")
        >>> print(result['results'])  # [{'id': 'person_1', 'type': 'Person'}, ...]
        >>> backend.close()
    """

    # Default embedding dimensions
    DEFAULT_EMBEDDING_DIM = 1536

    def __init__(
        self,
        db_path: str = ":memory:",
        embedding_dim: int = None,
        enable_httpfs: bool = True,
        extension_repo: str = None,
        s3_access_key: str = None,
        s3_secret_key: str = None,
        s3_region: str = None,
        gcs_access_key: str = None,
        gcs_secret_key: str = None,
    ):
        """
        Initialize Kuzu (Bighorn) backend.

        Args:
            db_path: Path to Kuzu database directory, or cloud URI
            embedding_dim: Embedding dimensions (default: 1536)
            enable_httpfs: Try to load httpfs extension for cloud access
            extension_repo: URL to extension server
            s3_access_key: AWS access key ID (optional, uses env if not set)
            s3_secret_key: AWS secret access key (optional, uses env if not set)
            s3_region: AWS region (optional, defaults to us-east-1)
            gcs_access_key: GCS HMAC access key (optional)
            gcs_secret_key: GCS HMAC secret key (optional)

        Raises:
            ImportError: If kuzu is not installed
        """
        if not KUZU_AVAILABLE:
            raise ImportError(
                "Kuzu (Bighorn) not installed. Install with: pip install kuzu"
            )

        import kuzu
        import tempfile

        self.embedding_dim = embedding_dim or self.DEFAULT_EMBEDDING_DIM
        self._lock = threading.Lock()
        self._closed = False
        self._httpfs_available = False
        self._schema_initialized = False
        self._extension_repo = extension_repo or os.environ.get("KUZU_EXTENSION_REPO")

        # Parse storage URI
        self._storage_uri = db_path
        self._is_cloud = self._is_cloud_uri(db_path)

        # Determine local path for database
        if db_path == ":memory:":
            self._temp_dir = tempfile.mkdtemp(prefix="kuzu_mem_")
            self._db_path = os.path.join(self._temp_dir, "kuzu_db")
            self._owns_temp = True
        elif self._is_cloud:
            self._temp_dir = tempfile.mkdtemp(prefix="kuzu_cloud_")
            self._db_path = os.path.join(self._temp_dir, "kuzu_db")
            self._owns_temp = True
        else:
            self._db_path = os.path.abspath(db_path)
            self._temp_dir = None
            self._owns_temp = False
            parent_dir = os.path.dirname(self._db_path)
            if parent_dir and not os.path.exists(parent_dir):
                os.makedirs(parent_dir, exist_ok=True)

        # Initialize Kuzu database
        self._db = kuzu.Database(self._db_path)
        self._conn = kuzu.Connection(self._db)

        # Try to load httpfs extension
        if enable_httpfs:
            self._try_load_httpfs()
            if self._httpfs_available:
                self._configure_cloud_credentials(
                    s3_access_key,
                    s3_secret_key,
                    s3_region,
                    gcs_access_key,
                    gcs_secret_key,
                )

        # Initialize schema
        self._init_schema()

    def _is_cloud_uri(self, path: str) -> bool:
        """Check if path is a cloud storage URI."""
        cloud_prefixes = ("s3://", "gs://", "az://", "http://", "https://")
        return path.lower().startswith(cloud_prefixes)

    def _try_load_httpfs(self) -> None:
        """Attempt to install and load the httpfs extension."""
        try:
            try:
                self._conn.execute("LOAD EXTENSION httpfs")
                self._httpfs_available = True
                return
            except Exception:
                pass

            if self._extension_repo:
                self._conn.execute(f"INSTALL httpfs FROM '{self._extension_repo}'")
            else:
                self._conn.execute("INSTALL httpfs")

            self._conn.execute("LOAD EXTENSION httpfs")
            self._httpfs_available = True
        except Exception:
            self._httpfs_available = False

    def _configure_cloud_credentials(
        self,
        s3_access_key: str = None,
        s3_secret_key: str = None,
        s3_region: str = None,
        gcs_access_key: str = None,
        gcs_secret_key: str = None,
    ) -> None:
        """Configure cloud storage credentials."""
        s3_key = s3_access_key or os.environ.get("AWS_ACCESS_KEY_ID")
        s3_secret = s3_secret_key or os.environ.get("AWS_SECRET_ACCESS_KEY")
        region = s3_region or os.environ.get("AWS_REGION", "us-east-1")

        if s3_key and s3_secret:
            try:
                self._conn.execute(
                    f"CALL httpfs_set_option('s3_access_key_id', '{s3_key}')"
                )
                self._conn.execute(
                    f"CALL httpfs_set_option('s3_secret_access_key', '{s3_secret}')"
                )
                self._conn.execute(f"CALL httpfs_set_option('s3_region', '{region}')")
            except Exception:
                pass

        gcs_key = gcs_access_key or os.environ.get("GCS_ACCESS_KEY_ID")
        gcs_secret = gcs_secret_key or os.environ.get("GCS_SECRET_ACCESS_KEY")

        if gcs_key and gcs_secret:
            try:
                self._conn.execute(
                    f"CALL httpfs_set_option('s3_access_key_id', '{gcs_key}')"
                )
                self._conn.execute(
                    f"CALL httpfs_set_option('s3_secret_access_key', '{gcs_secret}')"
                )
                self._conn.execute(
                    "CALL httpfs_set_option('s3_endpoint', 'storage.googleapis.com')"
                )
            except Exception:
                pass

    def _init_schema(self) -> None:
        """Initialize graph schema with Entity and Relation tables."""
        if self._schema_initialized:
            return

        try:
            try:
                self._conn.execute("MATCH (e:Entity) RETURN e.id LIMIT 1")
                self._schema_initialized = True
                return
            except Exception:
                pass

            self._conn.execute(
                """
                CREATE NODE TABLE Entity (
                    id STRING PRIMARY KEY,
                    type STRING,
                    properties STRING,
                    created_at TIMESTAMP DEFAULT current_timestamp()
                )
            """
            )

            self._conn.execute(
                """
                CREATE REL TABLE RELATES_TO (
                    FROM Entity TO Entity,
                    rel_type STRING,
                    properties STRING,
                    created_at TIMESTAMP DEFAULT current_timestamp()
                )
            """
            )

            self._schema_initialized = True

        except Exception:
            self._schema_initialized = True

    def _execute_query(self, cypher: str, params: dict = None) -> list:
        """Execute a Cypher query and return results as list of dicts."""
        result = self._conn.execute(cypher)
        columns = result.get_column_names()
        rows = []

        while result.has_next():
            row = result.get_next()
            row_dict = {}
            for i, col in enumerate(columns):
                clean_col = col.split(".")[-1] if "." in col else col
                row_dict[clean_col] = row[i]
            rows.append(row_dict)

        return rows

    def store_entity(
        self,
        entity_id: str,
        entity_type: str,
        properties: Optional[Dict[str, Any]] = None,
        embedding: Optional[List[float]] = None,
    ) -> Dict[str, Any]:
        """Store an entity (node) in the graph."""
        if not entity_id or not entity_type:
            return {
                "success": False,
                "error": "entity_id and entity_type are required",
                "error_type": "validation_error",
            }

        try:
            entity_id_str = str(entity_id)
            entity_type_str = str(entity_type)
            props_json = json.dumps(properties or {})

            if embedding:
                props_dict = properties or {}
                props_dict["_embedding"] = embedding
                props_json = json.dumps(props_dict)

            with self._lock:
                try:
                    results = self._execute_query(
                        f"MATCH (e:Entity {{id: '{entity_id_str}'}}) RETURN e.id"
                    )
                    exists = len(results) > 0
                except Exception:
                    exists = False

                props_escaped = props_json.replace("'", "''")

                if exists:
                    self._conn.execute(
                        f"""
                        MATCH (e:Entity {{id: '{entity_id_str}'}})
                        SET e.type = '{entity_type_str}',
                            e.properties = '{props_escaped}'
                    """
                    )
                else:
                    self._conn.execute(
                        f"""
                        CREATE (e:Entity {{
                            id: '{entity_id_str}',
                            type: '{entity_type_str}',
                            properties: '{props_escaped}'
                        }})
                    """
                    )

                return {
                    "success": True,
                    "entity_id": entity_id_str,
                    "type": entity_type_str,
                    "created": not exists,
                    "has_embedding": embedding is not None,
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store entity: {str(e)}",
                "error_type": "query_error",
            }

    def store_relation(
        self,
        from_entity: str,
        to_entity: str,
        relation_type: str,
        properties: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """Store a relation (edge) between two entities."""
        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error",
            }

        try:
            from_id = str(from_entity)
            to_id = str(to_entity)
            rel_type = str(relation_type)
            props_json = json.dumps(properties) if properties else "{}"
            props_escaped = props_json.replace("'", "''")

            with self._lock:
                try:
                    results = self._execute_query(
                        f"""
                        MATCH (a:Entity {{id: '{from_id}'}})-[r:RELATES_TO]->(b:Entity {{id: '{to_id}'}})
                        WHERE r.rel_type = '{rel_type}'
                        RETURN a.id
                    """
                    )
                    exists = len(results) > 0
                except Exception:
                    exists = False

                if exists:
                    self._conn.execute(
                        f"""
                        MATCH (a:Entity {{id: '{from_id}'}})-[r:RELATES_TO]->(b:Entity {{id: '{to_id}'}})
                        WHERE r.rel_type = '{rel_type}'
                        SET r.properties = '{props_escaped}'
                    """
                    )
                else:
                    self._conn.execute(
                        f"""
                        MATCH (a:Entity {{id: '{from_id}'}}), (b:Entity {{id: '{to_id}'}})
                        CREATE (a)-[:RELATES_TO {{rel_type: '{rel_type}', properties: '{props_escaped}'}}]->(b)
                    """
                    )

                return {
                    "success": True,
                    "from": from_id,
                    "to": to_id,
                    "type": rel_type,
                    "created": not exists,
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store relation: {str(e)}",
                "error_type": "query_error",
            }

    def query(
        self,
        cypher: Optional[str] = None,
        datalog: Optional[str] = None,
        pattern: Optional[Dict[str, Any]] = None,
        params: Optional[Dict[str, Any]] = None,
        limit: int = 100,
        timeout: Optional[float] = None,
    ) -> Dict[str, Any]:
        """Execute a Cypher query or pattern match."""
        if datalog and not cypher:
            return {
                "success": False,
                "error": "KuzuBackend uses Cypher, not Datalog. Use 'cypher' parameter instead. "
                "Example: MATCH (e:Entity) RETURN e.id, e.type LIMIT 100",
                "error_type": "validation_error",
            }

        if not cypher and not pattern:
            return {
                "success": False,
                "error": "Either cypher or pattern is required",
                "error_type": "validation_error",
            }

        try:
            query_str = cypher

            if pattern and not cypher:
                query_str = self._pattern_to_cypher(pattern, limit)

            if query_str and "LIMIT" not in query_str.upper():
                query_str = f"{query_str} LIMIT {limit}"

            with self._lock:
                results = self._execute_query(query_str)

                return {
                    "success": True,
                    "results": results,
                    "count": len(results),
                    "query": query_str,
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Query failed: {str(e)}",
                "error_type": "query_error",
            }

    def _pattern_to_cypher(self, pattern: Dict[str, Any], limit: int) -> str:
        """Convert a simple pattern dict to Cypher query."""
        if "entity_type" in pattern:
            return f"""
                MATCH (e:Entity)
                WHERE e.type = '{pattern['entity_type']}'
                RETURN e.id, e.type, e.properties
                LIMIT {limit}
            """

        if "from_entity" in pattern:
            return f"""
                MATCH (a:Entity {{id: '{pattern['from_entity']}'}})-[r:RELATES_TO]->(b:Entity)
                RETURN b.id, r.rel_type, r.properties
                LIMIT {limit}
            """

        return f"""
            MATCH (e:Entity)
            RETURN e.id, e.type, e.properties
            LIMIT {limit}
        """

    def retrieve_context(
        self,
        query: Optional[str] = None,
        embedding: Optional[List[float]] = None,
        entity_id: Optional[str] = None,
        hops: int = 2,
        limit: int = 20,
    ) -> Dict[str, Any]:
        """Retrieve relevant subgraph context."""
        if not query and not embedding and not entity_id:
            return {
                "success": False,
                "error": "One of query, embedding, or entity_id is required",
                "error_type": "validation_error",
            }

        try:
            entities = []
            relations = []

            with self._lock:
                if entity_id:
                    entities, relations = self._expand_from_entity(
                        entity_id, hops, limit
                    )
                elif embedding:
                    return {
                        "success": True,
                        "entities": [],
                        "relations": [],
                        "context_summary": "Vector search not natively supported in KuzuBackend. "
                        "Use entity_id for graph traversal, or consider CozoBackend for HNSW search.",
                    }

            context_summary = self._build_context_summary(entities, relations)

            return {
                "success": True,
                "entities": entities,
                "relations": relations,
                "context_summary": context_summary,
            }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to retrieve context: {str(e)}",
                "error_type": "query_error",
            }

    def _expand_from_entity(self, entity_id: str, hops: int, limit: int) -> tuple:
        """Expand N hops from an entity using Cypher."""
        entities = []
        relations = []

        try:
            results = self._execute_query(
                f"""
                MATCH (e:Entity {{id: '{entity_id}'}})
                RETURN e.id, e.type, e.properties
            """
            )

            for row in results:
                props = row.get("properties", "{}")
                entities.append(
                    {
                        "id": row["id"],
                        "type": row["type"],
                        "properties": json.loads(props) if props else {},
                    }
                )
        except Exception:
            pass

        if hops >= 1:
            try:
                results = self._execute_query(
                    f"""
                    MATCH (start:Entity {{id: '{entity_id}'}})-[*1..{hops}]-(connected:Entity)
                    RETURN DISTINCT connected.id, connected.type, connected.properties
                    LIMIT {limit}
                """
                )

                for row in results:
                    if row["id"] != entity_id:
                        props = row.get("properties", "{}")
                        entities.append(
                            {
                                "id": row["id"],
                                "type": row["type"],
                                "properties": json.loads(props) if props else {},
                            }
                        )
            except Exception:
                pass

        entity_ids = [e["id"] for e in entities]
        if entity_ids:
            relations = self._get_relations_between(entity_ids)

        return entities, relations

    def _get_relations_between(self, entity_ids: List[str]) -> List[Dict]:
        """Get all relations between a set of entities."""
        relations = []

        if not entity_ids:
            return relations

        ids_list = ", ".join([f"'{id}'" for id in entity_ids])

        try:
            results = self._execute_query(
                f"""
                MATCH (a:Entity)-[r:RELATES_TO]->(b:Entity)
                WHERE a.id IN [{ids_list}] AND b.id IN [{ids_list}]
                RETURN a.id AS from_id, r.rel_type, r.properties, b.id AS to_id
            """
            )

            for row in results:
                props = row.get("properties", "{}")
                relations.append(
                    {
                        "from": row["from_id"],
                        "to": row["to_id"],
                        "type": row["rel_type"],
                        "properties": json.loads(props) if props else {},
                    }
                )
        except Exception:
            pass

        return relations

    def _build_context_summary(
        self, entities: List[Dict], relations: List[Dict]
    ) -> str:
        """Build a text summary of the context."""
        if not entities:
            return "No relevant entities found."

        summary_parts = []

        type_counts = {}
        for e in entities:
            t = e.get("type", "Unknown")
            type_counts[t] = type_counts.get(t, 0) + 1

        entity_summary = ", ".join([f"{count} {t}" for t, count in type_counts.items()])
        summary_parts.append(f"Found {len(entities)} entities: {entity_summary}.")

        if relations:
            rel_types = set(r.get("type", "Unknown") for r in relations)
            summary_parts.append(
                f"Connected by {len(relations)} relations ({', '.join(rel_types)})."
            )

        return " ".join(summary_parts)

    def load_from_cloud(self, uri: str, table_name: str = "Entity") -> Dict[str, Any]:
        """Load data from cloud storage into the graph."""
        if not self._httpfs_available:
            return {
                "success": False,
                "error": "httpfs extension not available. Cloud storage operations require httpfs.",
                "error_type": "dependency_missing",
            }

        try:
            with self._lock:
                if uri.endswith(".parquet"):
                    self._conn.execute(f"COPY {table_name} FROM '{uri}'")
                elif uri.endswith(".csv"):
                    self._conn.execute(f"COPY {table_name} FROM '{uri}' (HEADER=true)")
                else:
                    return {
                        "success": False,
                        "error": "Unsupported file format. Use .parquet or .csv",
                        "error_type": "validation_error",
                    }

                return {"success": True, "loaded": True, "uri": uri}

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to load from cloud: {str(e)}",
                "error_type": "query_error",
            }

    def save_to_cloud(
        self, uri: str, query: str = "MATCH (e:Entity) RETURN e.*"
    ) -> Dict[str, Any]:
        """Export query results to cloud storage."""
        if not self._httpfs_available:
            return {
                "success": False,
                "error": "httpfs extension not available. Cloud storage operations require httpfs.",
                "error_type": "dependency_missing",
            }

        if uri.startswith("az://"):
            return {
                "success": False,
                "error": "Azure Blob Storage is read-only. Writing requires alternative methods.",
                "error_type": "validation_error",
            }

        try:
            with self._lock:
                self._conn.execute(f"COPY ({query}) TO '{uri}'")

                return {"success": True, "saved": True, "uri": uri}

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to save to cloud: {str(e)}",
                "error_type": "query_error",
            }

    def close(self) -> None:
        """Close the backend and release resources."""
        if self._closed:
            return

        self._closed = True

        try:
            if hasattr(self, "_conn") and self._conn is not None:
                self._conn.close()
                self._conn = None
        except Exception:
            pass

        try:
            if hasattr(self, "_db") and self._db is not None:
                self._db.close()
                self._db = None
        except Exception:
            pass

        if self._owns_temp and self._temp_dir:
            try:
                import shutil

                shutil.rmtree(self._temp_dir, ignore_errors=True)
            except Exception:
                pass

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass


# Alias for story compatibility
BighornBackend = KuzuBackend


# =============================================================================
# NEO4J BACKEND IMPLEMENTATION (TEA-BUILTIN-001.7.1)
# =============================================================================


class Neo4jBackend:
    """
    Neo4j implementation of GraphBackend (TEA-BUILTIN-001.7.1).

    Provides graph database functionality with Cypher queries using the
    official Neo4j Python driver. Supports all Neo4j URI schemes and
    both basic and bearer token authentication.

    Requires: pip install neo4j

    Supported URI schemes:
        - bolt://host:port - Unencrypted Bolt protocol
        - bolt+s://host:port - Bolt with TLS (certificate verification)
        - bolt+ssc://host:port - Bolt with TLS (self-signed, skip verification)
        - neo4j://host:port - Neo4j scheme with routing support
        - neo4j+s://host:port - Neo4j with TLS
        - neo4j+ssc://host:port - Neo4j with TLS (self-signed)

    Example:
        >>> backend = Neo4jBackend(
        ...     uri="bolt://localhost:7687",
        ...     username="neo4j",
        ...     password="password"
        ... )
        >>> result = backend.store_entity("person_1", "Person", {"name": "Alice"})
        >>> print(result['success'])  # True
        >>> result = backend.store_relation("person_1", "person_2", "KNOWS")
        >>> print(result['success'])  # True
        >>> result = backend.query(cypher="MATCH (p:Entity) RETURN p.id, p.type")
        >>> print(result['results'])
        >>> backend.close()
    """

    # Default embedding dimensions
    DEFAULT_EMBEDDING_DIM = 1536

    def __init__(
        self,
        uri: str = "bolt://localhost:7687",
        username: str = None,
        password: str = None,
        bearer_token: str = None,
        database: str = "neo4j",
        max_connection_pool_size: int = 50,
        max_connection_lifetime: int = 3600,
        connection_acquisition_timeout: int = 60,
        embedding_dim: int = None,
    ):
        """
        Initialize Neo4j backend.

        Args:
            uri: Neo4j connection URI (supports all Neo4j URI schemes)
            username: Username for basic authentication
            password: Password for basic authentication
            bearer_token: Bearer token for token authentication
            database: Database name (default: "neo4j")
            max_connection_pool_size: Maximum connections in pool (default: 50)
            max_connection_lifetime: Max lifetime of connection in seconds (default: 3600)
            connection_acquisition_timeout: Timeout for acquiring connection in seconds (default: 60)
            embedding_dim: Embedding dimensions (default: 1536)

        Raises:
            ImportError: If neo4j driver is not installed
        """
        if not NEO4J_AVAILABLE:
            raise ImportError(
                "Neo4j driver not installed. Install with: pip install neo4j"
            )

        self._uri = uri
        self._database = database
        self._max_connection_pool_size = max_connection_pool_size
        self._max_connection_lifetime = max_connection_lifetime
        self._connection_acquisition_timeout = connection_acquisition_timeout
        self.embedding_dim = embedding_dim or self.DEFAULT_EMBEDDING_DIM
        self._lock = threading.Lock()
        self._closed = False
        self._driver = None

        # Expand environment variables in credentials
        self._username = self._expand_env_var(username)
        self._password = self._expand_env_var(password)
        self._bearer_token = self._expand_env_var(bearer_token)

        # Initialize driver
        self._init_driver()

    def _expand_env_var(self, value: str) -> Optional[str]:
        """
        Expand environment variable references in value.

        Supports ${VAR} syntax for environment variable expansion.

        Args:
            value: String potentially containing ${VAR} references

        Returns:
            Expanded string or None if value is None
        """
        if value is None:
            return None

        import re

        def replace_env_var(match):
            var_name = match.group(1)
            return os.environ.get(var_name, "")

        return re.sub(r"\$\{([^}]+)\}", replace_env_var, str(value))

    def _init_driver(self) -> None:
        """
        Initialize Neo4j driver with appropriate authentication.

        Supports all Neo4j URI schemes:
        - bolt:// - Unencrypted Bolt protocol
        - bolt+s:// - Bolt with TLS (certificate verification)
        - bolt+ssc:// - Bolt with TLS (self-signed, skip verification)
        - neo4j:// - Neo4j scheme with routing support
        - neo4j+s:// - Neo4j with TLS
        - neo4j+ssc:// - Neo4j with TLS (self-signed)

        Raises:
            ValueError: If authentication parameters are invalid
            ConnectionError: If unable to connect to Neo4j
        """
        from neo4j import GraphDatabase, basic_auth, bearer_auth
        from neo4j.exceptions import ServiceUnavailable, AuthError

        # Validate URI scheme
        valid_schemes = [
            "bolt://",
            "bolt+s://",
            "bolt+ssc://",
            "neo4j://",
            "neo4j+s://",
            "neo4j+ssc://",
        ]
        if not any(self._uri.startswith(scheme) for scheme in valid_schemes):
            raise ValueError(
                f"Invalid URI scheme. Supported schemes: {', '.join(valid_schemes)}"
            )

        # Build authentication
        auth = None
        if self._bearer_token:
            auth = bearer_auth(self._bearer_token)
        elif self._username and self._password:
            auth = basic_auth(self._username, self._password)
        elif self._username or self._password:
            raise ValueError(
                "Both username and password are required for basic authentication"
            )

        # Driver configuration
        driver_config = {
            "auth": auth,
            "max_connection_pool_size": self._max_connection_pool_size,
            "max_connection_lifetime": self._max_connection_lifetime,
            "connection_acquisition_timeout": self._connection_acquisition_timeout,
        }

        try:
            self._driver = GraphDatabase.driver(self._uri, **driver_config)
            # Verify connectivity
            self._driver.verify_connectivity()
        except AuthError as e:
            self._driver = None
            raise ValueError(f"Authentication failed: Invalid credentials") from e
        except ServiceUnavailable as e:
            self._driver = None
            raise ConnectionError(
                f"Unable to connect to Neo4j: Server unavailable"
            ) from e
        except Exception as e:
            self._driver = None
            raise ConnectionError(
                f"Unable to connect to Neo4j: {type(e).__name__}"
            ) from e

    def _get_session(self):
        """
        Get a Neo4j session with automatic reconnection on transient failures.

        Returns:
            Neo4j session for the configured database

        Raises:
            ConnectionError: If unable to get session after reconnection attempt
        """
        from neo4j.exceptions import ServiceUnavailable

        if self._closed:
            raise ConnectionError("Backend is closed")

        if self._driver is None:
            try:
                self._init_driver()
            except Exception as e:
                raise ConnectionError(f"Unable to reconnect to Neo4j: {e}")

        try:
            return self._driver.session(database=self._database)
        except ServiceUnavailable:
            # Attempt reconnection once
            try:
                self._init_driver()
                return self._driver.session(database=self._database)
            except Exception as e:
                raise ConnectionError(f"Unable to reconnect to Neo4j: {e}")

    def _execute_with_retry(self, work_func: Callable, max_retries: int = 3) -> Any:
        """
        Execute a work function with automatic retry on transient failures.

        Args:
            work_func: Function to execute within a session
            max_retries: Maximum number of retry attempts

        Returns:
            Result of work_func

        Raises:
            ConnectionError: If unable to execute after all retries
        """
        from neo4j.exceptions import ServiceUnavailable, TransientError, SessionExpired

        last_error = None
        for attempt in range(max_retries):
            try:
                with self._get_session() as session:
                    return work_func(session)
            except (ServiceUnavailable, TransientError, SessionExpired) as e:
                last_error = e
                if attempt < max_retries - 1:
                    # Wait before retry with exponential backoff
                    import time

                    time.sleep(0.5 * (2**attempt))
                    # Force driver reconnection
                    if self._driver:
                        try:
                            self._driver.close()
                        except Exception:
                            pass
                        self._driver = None

        raise ConnectionError(
            f"Failed after {max_retries} attempts: {type(last_error).__name__}"
        )

    def store_entity(
        self,
        entity_id: str,
        entity_type: str,
        properties: Optional[Dict[str, Any]] = None,
        embedding: Optional[List[float]] = None,
    ) -> Dict[str, Any]:
        """
        Store an entity (node) in the graph.

        Uses MERGE for upsert semantics - creates if not exists, updates if exists.
        Node label is derived from entity_type parameter.

        Args:
            entity_id: Unique identifier for the entity
            entity_type: Type/label of the entity (e.g., "Person", "Document")
            properties: Optional properties dict (will be JSON serialized)
            embedding: Optional vector embedding (stored in _embedding property)

        Returns:
            {"success": True, "entity_id": str, "type": str, "created": bool, "has_embedding": bool}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not entity_id or not entity_type:
            return {
                "success": False,
                "error": "entity_id and entity_type are required",
                "error_type": "validation_error",
            }

        # Sanitize entity_type to be a valid Neo4j label (alphanumeric + underscore)
        import re

        safe_label = re.sub(r"[^a-zA-Z0-9_]", "_", str(entity_type))
        if not safe_label or not safe_label[0].isalpha():
            safe_label = "Entity_" + safe_label

        try:
            entity_id_str = str(entity_id)
            props_json = json.dumps(properties or {})

            def work(session):
                # Check if entity exists first
                check_query = """
                    MATCH (e {id: $entity_id})
                    RETURN e.id
                """
                check_result = session.run(check_query, entity_id=entity_id_str)
                exists = check_result.single() is not None

                # Build the MERGE query with dynamic label
                if embedding:
                    query = f"""
                        MERGE (e:{safe_label} {{id: $entity_id}})
                        SET e.type = $entity_type,
                            e.properties = $properties,
                            e._embedding = $embedding,
                            e.updated_at = datetime()
                        RETURN e.id, e.type
                    """
                    session.run(
                        query,
                        entity_id=entity_id_str,
                        entity_type=str(entity_type),
                        properties=props_json,
                        embedding=embedding,
                    )
                else:
                    query = f"""
                        MERGE (e:{safe_label} {{id: $entity_id}})
                        SET e.type = $entity_type,
                            e.properties = $properties,
                            e.updated_at = datetime()
                        RETURN e.id, e.type
                    """
                    session.run(
                        query,
                        entity_id=entity_id_str,
                        entity_type=str(entity_type),
                        properties=props_json,
                    )

                return not exists

            with self._lock:
                created = self._execute_with_retry(work)

            return {
                "success": True,
                "entity_id": entity_id_str,
                "type": str(entity_type),
                "created": created,
                "has_embedding": embedding is not None,
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store entity: {type(e).__name__}",
                "error_type": "query_error",
            }

    def store_relation(
        self,
        from_entity: str,
        to_entity: str,
        relation_type: str,
        properties: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Store a relation (edge) between two entities.

        Uses MERGE for upsert semantics - creates if not exists, updates if exists.
        Relationship type is derived from relation_type parameter.

        Args:
            from_entity: Source entity ID
            to_entity: Target entity ID
            relation_type: Type of the relationship (e.g., "KNOWS", "MENTIONS")
            properties: Optional properties dict

        Returns:
            {"success": True, "from": str, "to": str, "type": str, "created": bool}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error",
            }

        # Sanitize relation_type to be a valid Neo4j relationship type
        import re

        safe_rel_type = re.sub(r"[^a-zA-Z0-9_]", "_", str(relation_type)).upper()
        if not safe_rel_type or not safe_rel_type[0].isalpha():
            safe_rel_type = "REL_" + safe_rel_type

        try:
            from_id = str(from_entity)
            to_id = str(to_entity)
            props_json = json.dumps(properties) if properties else "{}"

            def work(session):
                # Check if relationship exists first
                check_query = f"""
                    MATCH (a {{id: $from_id}})-[r:{safe_rel_type}]->(b {{id: $to_id}})
                    RETURN r
                """
                check_result = session.run(check_query, from_id=from_id, to_id=to_id)
                exists = check_result.single() is not None

                # Build the MERGE query
                query = f"""
                    MATCH (a {{id: $from_id}}), (b {{id: $to_id}})
                    MERGE (a)-[r:{safe_rel_type}]->(b)
                    SET r.properties = $properties,
                        r.rel_type = $rel_type,
                        r.updated_at = datetime()
                    RETURN a.id, b.id
                """
                session.run(
                    query,
                    from_id=from_id,
                    to_id=to_id,
                    properties=props_json,
                    rel_type=str(relation_type),
                )

                return not exists

            with self._lock:
                created = self._execute_with_retry(work)

            return {
                "success": True,
                "from": from_id,
                "to": to_id,
                "type": str(relation_type),
                "created": created,
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store relation: {type(e).__name__}",
                "error_type": "query_error",
            }

    def query(
        self,
        cypher: Optional[str] = None,
        datalog: Optional[str] = None,
        pattern: Optional[Dict[str, Any]] = None,
        params: Optional[Dict[str, Any]] = None,
        limit: int = 100,
        timeout: Optional[float] = None,
    ) -> Dict[str, Any]:
        """
        Execute a Cypher query or pattern match.

        Args:
            cypher: Raw Cypher query string
            datalog: Datalog query (returns error - Neo4j doesn't support Datalog)
            pattern: Simplified pattern dict (converted to Cypher)
            params: Query parameters (passed to Cypher query)
            limit: Maximum results to return (appended if not in query)
            timeout: Query timeout in seconds (None for no timeout)

        Returns:
            {
                "success": True,
                "results": list,
                "count": int,
                "query": str (the executed query)
            }
            or {"success": False, "error": str, "error_type": str} on failure
        """
        # Neo4j uses Cypher, not Datalog
        if datalog and not cypher:
            return {
                "success": False,
                "error": "Neo4jBackend uses Cypher, not Datalog. Use 'cypher' parameter instead. "
                "Example: MATCH (e:Entity) RETURN e.id, e.type LIMIT 100",
                "error_type": "validation_error",
            }

        if not cypher and not pattern:
            return {
                "success": False,
                "error": "Either cypher or pattern is required",
                "error_type": "validation_error",
            }

        try:
            query_str = cypher

            if pattern and not cypher:
                query_str = self._pattern_to_cypher(pattern, limit)

            # Add LIMIT if not already in query
            if query_str and "LIMIT" not in query_str.upper():
                query_str = f"{query_str} LIMIT {limit}"

            def work(session):
                result = session.run(query_str, **(params or {}))
                records = []
                for record in result:
                    records.append(dict(record))
                return records

            with self._lock:
                results = self._execute_with_retry(work)

            return {
                "success": True,
                "results": results,
                "count": len(results),
                "query": query_str,
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Query failed: {type(e).__name__}",
                "error_type": "query_error",
            }

    def _pattern_to_cypher(self, pattern: Dict[str, Any], limit: int) -> str:
        """
        Convert a simple pattern dict to Cypher query.

        Supported patterns:
            - {entity_type: "Person"} -> Find all entities of type Person
            - {from_entity: "X"} -> Find all relations from entity X
            - {} -> Find all entities

        Args:
            pattern: Pattern dictionary
            limit: Maximum results

        Returns:
            Cypher query string
        """
        if "entity_type" in pattern:
            entity_type = pattern["entity_type"]
            return f"""
                MATCH (e)
                WHERE e.type = '{entity_type}'
                RETURN e.id, e.type, e.properties
                LIMIT {limit}
            """

        if "from_entity" in pattern:
            from_entity = pattern["from_entity"]
            return f"""
                MATCH (a {{id: '{from_entity}'}})-[r]->(b)
                RETURN b.id, type(r) AS rel_type, r.properties
                LIMIT {limit}
            """

        # Default: return all entities
        return f"""
            MATCH (e)
            WHERE e.id IS NOT NULL
            RETURN e.id, e.type, e.properties
            LIMIT {limit}
        """

    def retrieve_context(
        self,
        query: Optional[str] = None,
        embedding: Optional[List[float]] = None,
        entity_id: Optional[str] = None,
        hops: int = 2,
        limit: int = 20,
    ) -> Dict[str, Any]:
        """
        Retrieve relevant subgraph context.

        Can be called with:
        - entity_id: Start from entity and expand N hops
        - embedding: Not natively supported in Neo4j (use entity_id instead)
        - query: Text query (not currently supported)

        Args:
            query: Text query for semantic search (not supported)
            embedding: Direct embedding vector (not natively supported)
            entity_id: Entity ID to start neighborhood expansion from
            hops: Number of relationship hops to traverse (default: 2)
            limit: Maximum entities to return

        Returns:
            {
                "success": True,
                "entities": list,
                "relations": list,
                "context_summary": str
            }
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not query and not embedding and not entity_id:
            return {
                "success": False,
                "error": "One of query, embedding, or entity_id is required",
                "error_type": "validation_error",
            }

        try:
            entities = []
            relations = []

            if entity_id:
                entities, relations = self._expand_from_entity(entity_id, hops, limit)
            elif embedding:
                # Neo4j doesn't have native vector search without additional plugins
                return {
                    "success": True,
                    "entities": [],
                    "relations": [],
                    "context_summary": "Vector search not natively supported in Neo4jBackend. "
                    "Use entity_id for graph traversal, or consider adding "
                    "Neo4j Vector Index plugin for embedding search.",
                }
            elif query:
                return {
                    "success": True,
                    "entities": [],
                    "relations": [],
                    "context_summary": "Text query search not currently supported. "
                    "Use entity_id for graph traversal.",
                }

            context_summary = self._build_context_summary(entities, relations)

            return {
                "success": True,
                "entities": entities,
                "relations": relations,
                "context_summary": context_summary,
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to retrieve context: {type(e).__name__}",
                "error_type": "query_error",
            }

    def _expand_from_entity(self, entity_id: str, hops: int, limit: int) -> tuple:
        """
        Expand N hops from an entity using Cypher.

        Args:
            entity_id: Starting entity ID
            hops: Number of hops to expand
            limit: Maximum entities to return

        Returns:
            Tuple of (entities list, relations list)
        """
        entities = []
        relations = []

        def work(session):
            # Get starting entity
            start_query = """
                MATCH (e {id: $entity_id})
                RETURN e.id AS id, e.type AS type, e.properties AS properties
            """
            start_result = session.run(start_query, entity_id=str(entity_id))

            start_entities = []
            for record in start_result:
                props = record.get("properties", "{}")
                start_entities.append(
                    {
                        "id": record["id"],
                        "type": record.get("type"),
                        "properties": json.loads(props) if props else {},
                    }
                )

            # Get connected entities within N hops
            connected_entities = []
            if hops >= 1:
                hop_query = f"""
                    MATCH (start {{id: $entity_id}})-[*1..{hops}]-(connected)
                    RETURN DISTINCT
                        connected.id AS id,
                        connected.type AS type,
                        connected.properties AS properties
                    LIMIT {limit}
                """
                hop_result = session.run(hop_query, entity_id=str(entity_id))

                for record in hop_result:
                    if record["id"] != entity_id:
                        props = record.get("properties", "{}")
                        connected_entities.append(
                            {
                                "id": record["id"],
                                "type": record.get("type"),
                                "properties": json.loads(props) if props else {},
                            }
                        )

            all_entities = start_entities + connected_entities
            entity_ids = [e["id"] for e in all_entities]

            # Get relations between these entities
            rels = []
            if entity_ids:
                rel_query = """
                    MATCH (a)-[r]->(b)
                    WHERE a.id IN $entity_ids AND b.id IN $entity_ids
                    RETURN
                        a.id AS from_id,
                        type(r) AS rel_type,
                        r.properties AS properties,
                        b.id AS to_id
                """
                rel_result = session.run(rel_query, entity_ids=entity_ids)

                for record in rel_result:
                    props = record.get("properties", "{}")
                    rels.append(
                        {
                            "from": record["from_id"],
                            "to": record["to_id"],
                            "type": record["rel_type"],
                            "properties": json.loads(props) if props else {},
                        }
                    )

            return all_entities, rels

        with self._lock:
            entities, relations = self._execute_with_retry(work)

        return entities, relations

    def _build_context_summary(
        self, entities: List[Dict], relations: List[Dict]
    ) -> str:
        """
        Build a text summary of the context.

        Args:
            entities: List of entity dictionaries
            relations: List of relation dictionaries

        Returns:
            Human-readable summary string
        """
        if not entities:
            return "No relevant entities found."

        summary_parts = []

        # Summarize entities by type
        type_counts = {}
        for e in entities:
            t = e.get("type", "Unknown")
            type_counts[t] = type_counts.get(t, 0) + 1

        entity_summary = ", ".join([f"{count} {t}" for t, count in type_counts.items()])
        summary_parts.append(f"Found {len(entities)} entities: {entity_summary}.")

        # Summarize relations
        if relations:
            rel_types = set(r.get("type", "Unknown") for r in relations)
            summary_parts.append(
                f"Connected by {len(relations)} relations ({', '.join(rel_types)})."
            )

        return " ".join(summary_parts)

    def close(self) -> None:
        """Close the backend and release resources."""
        if self._closed:
            return

        self._closed = True

        if hasattr(self, "_driver") and self._driver is not None:
            try:
                self._driver.close()
                self._driver = None
            except Exception:
                pass

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass

    def __repr__(self) -> str:
        """Return string representation (without credentials)."""
        return f"Neo4jBackend(uri='{self._uri}', database='{self._database}')"

    def __str__(self) -> str:
        """Return string representation (without credentials)."""
        return self.__repr__()

    # =========================================================================
    # EXTENDED CRUD OPERATIONS (TEA-BUILTIN-001.7.2)
    # =========================================================================

    def delete_entity(self, entity_id: str, detach: bool = True) -> Dict[str, Any]:
        """
        Delete an entity (node) from the graph.

        Args:
            entity_id: The unique identifier of the entity to delete
            detach: If True (default), also delete all relationships connected
                   to this entity. If False, fail if relationships exist.

        Returns:
            {"success": True, "deleted": True, "entity_id": str}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not entity_id:
            return {
                "success": False,
                "error": "entity_id is required",
                "error_type": "validation_error",
            }

        try:
            entity_id_str = str(entity_id)

            def work(session):
                # Check if entity exists
                check_query = """
                    MATCH (e {id: $entity_id})
                    RETURN e.id
                """
                check_result = session.run(check_query, entity_id=entity_id_str)
                if check_result.single() is None:
                    return {"exists": False, "deleted": False}

                # Check for relationships if detach=False
                if not detach:
                    rel_check = """
                        MATCH (e {id: $entity_id})-[r]-()
                        RETURN count(r) AS rel_count
                    """
                    rel_result = session.run(rel_check, entity_id=entity_id_str)
                    rel_record = rel_result.single()
                    if rel_record and rel_record["rel_count"] > 0:
                        return {
                            "exists": True,
                            "deleted": False,
                            "has_relationships": True,
                            "rel_count": rel_record["rel_count"],
                        }

                # Delete the entity
                if detach:
                    delete_query = """
                        MATCH (e {id: $entity_id})
                        DETACH DELETE e
                        RETURN count(e) AS deleted
                    """
                else:
                    delete_query = """
                        MATCH (e {id: $entity_id})
                        DELETE e
                        RETURN count(e) AS deleted
                    """
                session.run(delete_query, entity_id=entity_id_str)
                return {"exists": True, "deleted": True}

            with self._lock:
                result = self._execute_with_retry(work)

            if not result.get("exists"):
                return {
                    "success": False,
                    "error": f"Entity '{entity_id_str}' not found",
                    "error_type": "not_found",
                }

            if result.get("has_relationships"):
                return {
                    "success": False,
                    "error": f"Entity '{entity_id_str}' has {result['rel_count']} relationships. "
                    "Use detach=True to delete with relationships.",
                    "error_type": "constraint_error",
                }

            return {"success": True, "deleted": True, "entity_id": entity_id_str}

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to delete entity: {type(e).__name__}",
                "error_type": "query_error",
            }

    def delete_relation(
        self, from_entity: str, to_entity: str, relation_type: str
    ) -> Dict[str, Any]:
        """
        Delete a specific relation (edge) between two entities.

        Args:
            from_entity: Source entity ID
            to_entity: Target entity ID
            relation_type: Type of the relationship to delete

        Returns:
            {"success": True, "deleted": True}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error",
            }

        # Sanitize relation_type to be a valid Neo4j relationship type
        import re

        safe_rel_type = re.sub(r"[^a-zA-Z0-9_]", "_", str(relation_type)).upper()
        if not safe_rel_type or not safe_rel_type[0].isalpha():
            safe_rel_type = "REL_" + safe_rel_type

        try:
            from_id = str(from_entity)
            to_id = str(to_entity)

            def work(session):
                # Check if relationship exists first
                check_query = f"""
                    MATCH (a {{id: $from_id}})-[r:{safe_rel_type}]->(b {{id: $to_id}})
                    RETURN r
                """
                check_result = session.run(check_query, from_id=from_id, to_id=to_id)
                if check_result.single() is None:
                    return {"exists": False, "deleted": False}

                # Delete the relationship
                delete_query = f"""
                    MATCH (a {{id: $from_id}})-[r:{safe_rel_type}]->(b {{id: $to_id}})
                    DELETE r
                    RETURN count(r) AS deleted
                """
                session.run(delete_query, from_id=from_id, to_id=to_id)
                return {"exists": True, "deleted": True}

            with self._lock:
                result = self._execute_with_retry(work)

            if not result.get("exists"):
                return {
                    "success": False,
                    "error": f"Relation '{relation_type}' from '{from_id}' to '{to_id}' not found",
                    "error_type": "not_found",
                }

            return {"success": True, "deleted": True}

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to delete relation: {type(e).__name__}",
                "error_type": "query_error",
            }

    def delete_entities_batch(
        self, entity_ids: List[str], detach: bool = True
    ) -> Dict[str, Any]:
        """
        Delete multiple entities in a single transaction.

        Args:
            entity_ids: List of entity IDs to delete
            detach: If True (default), also delete all relationships connected
                   to these entities. If False, fail if any relationships exist.

        Returns:
            {"success": True, "deleted_count": int, "entity_ids": list}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not entity_ids:
            return {
                "success": False,
                "error": "entity_ids list is required",
                "error_type": "validation_error",
            }

        if not isinstance(entity_ids, (list, tuple)):
            return {
                "success": False,
                "error": "entity_ids must be a list",
                "error_type": "validation_error",
            }

        try:
            entity_ids_str = [str(eid) for eid in entity_ids]

            def work(session):
                # Check for relationships if detach=False
                if not detach:
                    rel_check = """
                        MATCH (e)-[r]-()
                        WHERE e.id IN $entity_ids
                        RETURN count(r) AS rel_count
                    """
                    rel_result = session.run(rel_check, entity_ids=entity_ids_str)
                    rel_record = rel_result.single()
                    if rel_record and rel_record["rel_count"] > 0:
                        return {
                            "deleted": False,
                            "has_relationships": True,
                            "rel_count": rel_record["rel_count"],
                        }

                # Delete entities using UNWIND for efficiency
                if detach:
                    delete_query = """
                        UNWIND $entity_ids AS eid
                        MATCH (e {id: eid})
                        DETACH DELETE e
                        RETURN count(e) AS deleted_count
                    """
                else:
                    delete_query = """
                        UNWIND $entity_ids AS eid
                        MATCH (e {id: eid})
                        DELETE e
                        RETURN count(e) AS deleted_count
                    """
                result = session.run(delete_query, entity_ids=entity_ids_str)
                record = result.single()
                deleted_count = record["deleted_count"] if record else 0
                return {"deleted": True, "deleted_count": deleted_count}

            with self._lock:
                result = self._execute_with_retry(work)

            if result.get("has_relationships"):
                return {
                    "success": False,
                    "error": f"Entities have {result['rel_count']} relationships. "
                    "Use detach=True to delete with relationships.",
                    "error_type": "constraint_error",
                }

            return {
                "success": True,
                "deleted_count": result.get("deleted_count", 0),
                "entity_ids": entity_ids_str,
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to delete entities: {type(e).__name__}",
                "error_type": "query_error",
            }

    # =========================================================================
    # UPDATE OPERATIONS (TEA-BUILTIN-001.7.2 AC-4 to AC-7)
    # =========================================================================

    def update_entity_properties(
        self, entity_id: str, properties: Dict[str, Any], merge: bool = True
    ) -> Dict[str, Any]:
        """
        Update properties of an entity (node).

        Args:
            entity_id: The unique identifier of the entity to update
            properties: Properties to set/merge
            merge: If True (default), merge with existing properties.
                   If False, replace all properties.

        Returns:
            {"success": True, "entity_id": str, "properties": dict}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not entity_id:
            return {
                "success": False,
                "error": "entity_id is required",
                "error_type": "validation_error",
            }

        if properties is None:
            return {
                "success": False,
                "error": "properties is required",
                "error_type": "validation_error",
            }

        try:
            entity_id_str = str(entity_id)
            props_json = json.dumps(properties)

            def work(session):
                # Check if entity exists
                check_query = """
                    MATCH (e {id: $entity_id})
                    RETURN e.id, e.properties
                """
                check_result = session.run(check_query, entity_id=entity_id_str)
                record = check_result.single()
                if record is None:
                    return {"exists": False}

                existing_props_str = record.get("properties", "{}")
                existing_props = (
                    json.loads(existing_props_str) if existing_props_str else {}
                )

                if merge:
                    # Merge with existing properties
                    new_props = {**existing_props, **properties}
                else:
                    # Replace all properties
                    new_props = properties

                new_props_json = json.dumps(new_props)
                props_escaped = new_props_json.replace("'", "''")

                update_query = f"""
                    MATCH (e {{id: $entity_id}})
                    SET e.properties = '{props_escaped}',
                        e.updated_at = datetime()
                    RETURN e.properties
                """
                session.run(update_query, entity_id=entity_id_str)
                return {"exists": True, "properties": new_props}

            with self._lock:
                result = self._execute_with_retry(work)

            if not result.get("exists"):
                return {
                    "success": False,
                    "error": f"Entity '{entity_id_str}' not found",
                    "error_type": "not_found",
                }

            return {
                "success": True,
                "entity_id": entity_id_str,
                "properties": result["properties"],
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to update entity properties: {type(e).__name__}",
                "error_type": "query_error",
            }

    def update_relation_properties(
        self,
        from_entity: str,
        to_entity: str,
        relation_type: str,
        properties: Dict[str, Any],
        merge: bool = True,
    ) -> Dict[str, Any]:
        """
        Update properties of a relation (edge).

        Args:
            from_entity: Source entity ID
            to_entity: Target entity ID
            relation_type: Type of the relationship
            properties: Properties to set/merge
            merge: If True (default), merge with existing properties.
                   If False, replace all properties.

        Returns:
            {"success": True, "properties": dict}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error",
            }

        if properties is None:
            return {
                "success": False,
                "error": "properties is required",
                "error_type": "validation_error",
            }

        # Sanitize relation_type
        import re

        safe_rel_type = re.sub(r"[^a-zA-Z0-9_]", "_", str(relation_type)).upper()
        if not safe_rel_type or not safe_rel_type[0].isalpha():
            safe_rel_type = "REL_" + safe_rel_type

        try:
            from_id = str(from_entity)
            to_id = str(to_entity)

            def work(session):
                # Check if relationship exists
                check_query = f"""
                    MATCH (a {{id: $from_id}})-[r:{safe_rel_type}]->(b {{id: $to_id}})
                    RETURN r.properties
                """
                check_result = session.run(check_query, from_id=from_id, to_id=to_id)
                record = check_result.single()
                if record is None:
                    return {"exists": False}

                existing_props_str = (
                    record.get("r.properties") or record.get("properties") or "{}"
                )
                existing_props = (
                    json.loads(existing_props_str) if existing_props_str else {}
                )

                if merge:
                    new_props = {**existing_props, **properties}
                else:
                    new_props = properties

                new_props_json = json.dumps(new_props)
                props_escaped = new_props_json.replace("'", "''")

                update_query = f"""
                    MATCH (a {{id: $from_id}})-[r:{safe_rel_type}]->(b {{id: $to_id}})
                    SET r.properties = '{props_escaped}',
                        r.updated_at = datetime()
                    RETURN r.properties
                """
                session.run(update_query, from_id=from_id, to_id=to_id)
                return {"exists": True, "properties": new_props}

            with self._lock:
                result = self._execute_with_retry(work)

            if not result.get("exists"):
                return {
                    "success": False,
                    "error": f"Relation '{relation_type}' from '{from_id}' to '{to_id}' not found",
                    "error_type": "not_found",
                }

            return {"success": True, "properties": result["properties"]}

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to update relation properties: {type(e).__name__}",
                "error_type": "query_error",
            }

    def add_labels(self, entity_id: str, labels: List[str]) -> Dict[str, Any]:
        """
        Add labels to an entity (node).

        Args:
            entity_id: The unique identifier of the entity
            labels: List of labels to add

        Returns:
            {"success": True, "entity_id": str, "labels_added": list}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not entity_id:
            return {
                "success": False,
                "error": "entity_id is required",
                "error_type": "validation_error",
            }

        if not labels or not isinstance(labels, (list, tuple)):
            return {
                "success": False,
                "error": "labels must be a non-empty list of strings",
                "error_type": "validation_error",
            }

        # Sanitize labels
        import re

        safe_labels = []
        for label in labels:
            safe_label = re.sub(r"[^a-zA-Z0-9_]", "_", str(label))
            if safe_label and safe_label[0].isalpha():
                safe_labels.append(safe_label)
            elif safe_label:
                safe_labels.append("Label_" + safe_label)

        if not safe_labels:
            return {
                "success": False,
                "error": "No valid labels provided",
                "error_type": "validation_error",
            }

        try:
            entity_id_str = str(entity_id)
            labels_str = ":".join(safe_labels)

            def work(session):
                # Check if entity exists
                check_query = """
                    MATCH (e {id: $entity_id})
                    RETURN e.id
                """
                check_result = session.run(check_query, entity_id=entity_id_str)
                if check_result.single() is None:
                    return {"exists": False}

                # Add labels
                add_query = f"""
                    MATCH (e {{id: $entity_id}})
                    SET e:{labels_str}
                    RETURN labels(e) AS new_labels
                """
                result = session.run(add_query, entity_id=entity_id_str)
                record = result.single()
                new_labels = record["new_labels"] if record else []
                return {"exists": True, "labels": new_labels}

            with self._lock:
                result = self._execute_with_retry(work)

            if not result.get("exists"):
                return {
                    "success": False,
                    "error": f"Entity '{entity_id_str}' not found",
                    "error_type": "not_found",
                }

            return {
                "success": True,
                "entity_id": entity_id_str,
                "labels_added": safe_labels,
                "current_labels": result.get("labels", []),
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to add labels: {type(e).__name__}",
                "error_type": "query_error",
            }

    def remove_labels(self, entity_id: str, labels: List[str]) -> Dict[str, Any]:
        """
        Remove labels from an entity (node).

        Args:
            entity_id: The unique identifier of the entity
            labels: List of labels to remove

        Returns:
            {"success": True, "entity_id": str, "labels_removed": list}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not entity_id:
            return {
                "success": False,
                "error": "entity_id is required",
                "error_type": "validation_error",
            }

        if not labels or not isinstance(labels, (list, tuple)):
            return {
                "success": False,
                "error": "labels must be a non-empty list of strings",
                "error_type": "validation_error",
            }

        # Sanitize labels
        import re

        safe_labels = []
        for label in labels:
            safe_label = re.sub(r"[^a-zA-Z0-9_]", "_", str(label))
            if safe_label and safe_label[0].isalpha():
                safe_labels.append(safe_label)
            elif safe_label:
                safe_labels.append("Label_" + safe_label)

        if not safe_labels:
            return {
                "success": False,
                "error": "No valid labels provided",
                "error_type": "validation_error",
            }

        try:
            entity_id_str = str(entity_id)
            labels_str = ":".join(safe_labels)

            def work(session):
                # Check if entity exists
                check_query = """
                    MATCH (e {id: $entity_id})
                    RETURN e.id
                """
                check_result = session.run(check_query, entity_id=entity_id_str)
                if check_result.single() is None:
                    return {"exists": False}

                # Remove labels
                remove_query = f"""
                    MATCH (e {{id: $entity_id}})
                    REMOVE e:{labels_str}
                    RETURN labels(e) AS remaining_labels
                """
                result = session.run(remove_query, entity_id=entity_id_str)
                record = result.single()
                remaining_labels = record["remaining_labels"] if record else []
                return {"exists": True, "labels": remaining_labels}

            with self._lock:
                result = self._execute_with_retry(work)

            if not result.get("exists"):
                return {
                    "success": False,
                    "error": f"Entity '{entity_id_str}' not found",
                    "error_type": "not_found",
                }

            return {
                "success": True,
                "entity_id": entity_id_str,
                "labels_removed": safe_labels,
                "current_labels": result.get("labels", []),
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to remove labels: {type(e).__name__}",
                "error_type": "query_error",
            }

    # =========================================================================
    # BATCH OPERATIONS (TEA-BUILTIN-001.7.2 AC-8 to AC-10)
    # =========================================================================

    def store_entities_batch(self, entities: List[Dict[str, Any]]) -> Dict[str, Any]:
        """
        Bulk insert/update multiple entities in a single transaction.

        Uses UNWIND for efficient batch processing. All operations are
        performed in a single transaction (AC-10).

        Args:
            entities: List of entity dictionaries, each with:
                - entity_id: str (required)
                - entity_type: str (required)
                - properties: dict (optional)
                - embedding: list[float] (optional)

        Returns:
            {"success": True, "processed_count": int, "created": int, "updated": int}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not entities:
            return {
                "success": False,
                "error": "entities list is required",
                "error_type": "validation_error",
            }

        if not isinstance(entities, (list, tuple)):
            return {
                "success": False,
                "error": "entities must be a list",
                "error_type": "validation_error",
            }

        # Validate each entity
        for i, entity in enumerate(entities):
            if not isinstance(entity, dict):
                return {
                    "success": False,
                    "error": f"Entity at index {i} must be a dictionary",
                    "error_type": "validation_error",
                }
            if not entity.get("entity_id"):
                return {
                    "success": False,
                    "error": f"Entity at index {i} missing required 'entity_id'",
                    "error_type": "validation_error",
                }
            if not entity.get("entity_type"):
                return {
                    "success": False,
                    "error": f"Entity at index {i} missing required 'entity_type'",
                    "error_type": "validation_error",
                }

        try:
            # Prepare entities for UNWIND
            import re

            entity_data = []
            for entity in entities:
                entity_id = str(entity["entity_id"])
                entity_type = str(entity["entity_type"])
                props = entity.get("properties", {})
                embedding = entity.get("embedding")

                # Sanitize type for label
                safe_label = re.sub(r"[^a-zA-Z0-9_]", "_", entity_type)
                if not safe_label or not safe_label[0].isalpha():
                    safe_label = "Entity_" + safe_label

                entity_data.append(
                    {
                        "id": entity_id,
                        "type": entity_type,
                        "label": safe_label,
                        "properties": json.dumps(props),
                        "embedding": embedding,
                    }
                )

            def work(session):
                # First, check which entities already exist
                entity_ids = [e["id"] for e in entity_data]
                check_query = """
                    UNWIND $entity_ids AS eid
                    OPTIONAL MATCH (e {id: eid})
                    RETURN eid, CASE WHEN e IS NOT NULL THEN true ELSE false END AS exists
                """
                check_result = session.run(check_query, entity_ids=entity_ids)
                existing = set()
                for record in check_result:
                    if record["exists"]:
                        existing.add(record["eid"])

                # Process each entity (can't use dynamic labels in pure UNWIND)
                created_count = 0
                updated_count = 0

                for entity in entity_data:
                    props_escaped = entity["properties"].replace("'", "''")
                    label = entity["label"]

                    if entity["embedding"]:
                        query = f"""
                            MERGE (e:{label} {{id: $entity_id}})
                            SET e.type = $entity_type,
                                e.properties = '{props_escaped}',
                                e._embedding = $embedding,
                                e.updated_at = datetime()
                            RETURN e.id
                        """
                        session.run(
                            query,
                            entity_id=entity["id"],
                            entity_type=entity["type"],
                            embedding=entity["embedding"],
                        )
                    else:
                        query = f"""
                            MERGE (e:{label} {{id: $entity_id}})
                            SET e.type = $entity_type,
                                e.properties = '{props_escaped}',
                                e.updated_at = datetime()
                            RETURN e.id
                        """
                        session.run(
                            query, entity_id=entity["id"], entity_type=entity["type"]
                        )

                    if entity["id"] in existing:
                        updated_count += 1
                    else:
                        created_count += 1

                return {
                    "processed_count": len(entity_data),
                    "created": created_count,
                    "updated": updated_count,
                }

            with self._lock:
                result = self._execute_with_retry(work)

            return {
                "success": True,
                "processed_count": result["processed_count"],
                "created": result["created"],
                "updated": result["updated"],
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store entities batch: {type(e).__name__}",
                "error_type": "query_error",
            }

    def store_relations_batch(self, relations: List[Dict[str, Any]]) -> Dict[str, Any]:
        """
        Bulk create/update multiple relations in a single transaction.

        Uses UNWIND for efficient batch processing. All operations are
        performed in a single transaction (AC-10).

        Args:
            relations: List of relation dictionaries, each with:
                - from_entity: str (required)
                - to_entity: str (required)
                - relation_type: str (required)
                - properties: dict (optional)

        Returns:
            {"success": True, "processed_count": int, "created": int, "updated": int}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not relations:
            return {
                "success": False,
                "error": "relations list is required",
                "error_type": "validation_error",
            }

        if not isinstance(relations, (list, tuple)):
            return {
                "success": False,
                "error": "relations must be a list",
                "error_type": "validation_error",
            }

        # Validate each relation
        for i, relation in enumerate(relations):
            if not isinstance(relation, dict):
                return {
                    "success": False,
                    "error": f"Relation at index {i} must be a dictionary",
                    "error_type": "validation_error",
                }
            if not relation.get("from_entity"):
                return {
                    "success": False,
                    "error": f"Relation at index {i} missing required 'from_entity'",
                    "error_type": "validation_error",
                }
            if not relation.get("to_entity"):
                return {
                    "success": False,
                    "error": f"Relation at index {i} missing required 'to_entity'",
                    "error_type": "validation_error",
                }
            if not relation.get("relation_type"):
                return {
                    "success": False,
                    "error": f"Relation at index {i} missing required 'relation_type'",
                    "error_type": "validation_error",
                }

        try:
            # Group relations by type (Neo4j requires static relationship types in Cypher)
            import re
            from collections import defaultdict

            relations_by_type = defaultdict(list)
            for relation in relations:
                from_id = str(relation["from_entity"])
                to_id = str(relation["to_entity"])
                rel_type = str(relation["relation_type"])
                props = relation.get("properties", {})

                # Sanitize relation type
                safe_rel_type = re.sub(r"[^a-zA-Z0-9_]", "_", rel_type).upper()
                if not safe_rel_type or not safe_rel_type[0].isalpha():
                    safe_rel_type = "REL_" + safe_rel_type

                relations_by_type[safe_rel_type].append(
                    {
                        "from_id": from_id,
                        "to_id": to_id,
                        "rel_type": rel_type,
                        "properties": json.dumps(props),
                    }
                )

            def work(session):
                created_count = 0
                updated_count = 0

                for safe_rel_type, rels in relations_by_type.items():
                    for rel in rels:
                        # Check if exists
                        check_query = f"""
                            MATCH (a {{id: $from_id}})-[r:{safe_rel_type}]->(b {{id: $to_id}})
                            RETURN r
                        """
                        check_result = session.run(
                            check_query, from_id=rel["from_id"], to_id=rel["to_id"]
                        )
                        exists = check_result.single() is not None

                        props_escaped = rel["properties"].replace("'", "''")

                        query = f"""
                            MATCH (a {{id: $from_id}}), (b {{id: $to_id}})
                            MERGE (a)-[r:{safe_rel_type}]->(b)
                            SET r.properties = '{props_escaped}',
                                r.rel_type = $rel_type,
                                r.updated_at = datetime()
                            RETURN r
                        """
                        session.run(
                            query,
                            from_id=rel["from_id"],
                            to_id=rel["to_id"],
                            rel_type=rel["rel_type"],
                        )

                        if exists:
                            updated_count += 1
                        else:
                            created_count += 1

                return {
                    "processed_count": created_count + updated_count,
                    "created": created_count,
                    "updated": updated_count,
                }

            with self._lock:
                result = self._execute_with_retry(work)

            return {
                "success": True,
                "processed_count": result["processed_count"],
                "created": result["created"],
                "updated": result["updated"],
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store relations batch: {type(e).__name__}",
                "error_type": "query_error",
            }

    # =========================================================================
    # APOC TRIGGER SUPPORT (TEA-BUILTIN-001.7.5)
    # =========================================================================

    def check_apoc_available(self) -> Dict[str, Any]:
        """
        Check if APOC library is installed and available (AC-1).

        Returns:
            {
                "success": True,
                "available": bool,
                "version": str or None
            }
        """
        try:

            def work(session):
                result = session.run("RETURN apoc.version() AS version")
                record = result.single()
                if record:
                    return {"available": True, "version": record["version"]}
                return {"available": False, "version": None}

            with self._lock:
                result = self._execute_with_retry(work)
                return {
                    "success": True,
                    "available": result["available"],
                    "version": result["version"],
                }

        except Exception:
            return {"success": True, "available": False, "version": None}

    def get_apoc_version(self) -> Dict[str, Any]:
        """
        Get the installed APOC library version (AC-2).

        Returns:
            {"success": True, "version": str}
            or {"success": False, "error": str, "error_type": str}
        """
        result = self.check_apoc_available()
        if result.get("available"):
            return {"success": True, "version": result["version"]}
        return {
            "success": False,
            "error": "APOC library not available. Install APOC plugin for Neo4j.",
            "error_type": "dependency_missing",
        }

    def check_triggers_enabled(self) -> Dict[str, Any]:
        """
        Check if APOC triggers are enabled in Neo4j configuration (AC-3).

        Checks apoc.trigger.enabled configuration setting.

        Returns:
            {
                "success": True,
                "enabled": bool,
                "refresh_interval": int or None
            }
        """
        # First check if APOC is available
        apoc_check = self.check_apoc_available()
        if not apoc_check.get("available"):
            return {
                "success": True,
                "enabled": False,
                "refresh_interval": None,
                "error": "APOC library not available",
            }

        try:

            def work(session):
                enabled = False
                refresh_interval = None

                # Check trigger.enabled setting
                try:
                    result = session.run(
                        "CALL dbms.listConfig('apoc.trigger.enabled') YIELD value RETURN value"
                    )
                    record = result.single()
                    if record:
                        enabled = str(record["value"]).lower() == "true"
                except Exception:
                    # Try alternative method for newer Neo4j versions
                    try:
                        result = session.run(
                            "CALL apoc.trigger.list() YIELD name RETURN count(*) AS count"
                        )
                        # If this works without error, triggers are enabled
                        enabled = True
                    except Exception:
                        enabled = False

                # Try to get refresh interval
                try:
                    result = session.run(
                        "CALL dbms.listConfig('apoc.trigger.refresh') YIELD value RETURN value"
                    )
                    record = result.single()
                    if record:
                        refresh_interval = int(record["value"])
                except Exception:
                    pass

                return {"enabled": enabled, "refresh_interval": refresh_interval}

            with self._lock:
                result = self._execute_with_retry(work)
                return {
                    "success": True,
                    "enabled": result["enabled"],
                    "refresh_interval": result["refresh_interval"],
                }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": True,
                "enabled": False,
                "refresh_interval": None,
                "error": f"Unable to check trigger configuration: {type(e).__name__}",
            }

    @property
    def APOC_AVAILABLE(self) -> bool:
        """Property flag indicating if APOC library is available (AC-4)."""
        result = self.check_apoc_available()
        return result.get("available", False)

    @property
    def TRIGGERS_ENABLED(self) -> bool:
        """Property flag indicating if APOC triggers are enabled (AC-4)."""
        result = self.check_triggers_enabled()
        return result.get("enabled", False)

    # =========================================================================
    # TRIGGER REGISTRATION (TEA-BUILTIN-001.7.5 AC-5 to AC-10)
    # =========================================================================

    # Supported selector types (AC-6)
    TRIGGER_SELECTORS = [
        "createdNodes",
        "createdRelationships",
        "deletedNodes",
        "deletedRelationships",
        "assignedLabels",
        "removedLabels",
        "assignedNodeProperties",
        "assignedRelationshipProperties",
    ]

    def register_trigger(
        self,
        name: str,
        query: str,
        selector: Optional[Dict[str, Any]] = None,
        config: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Register a database trigger using APOC (AC-5).

        Args:
            name: Unique trigger identifier
            query: Cypher query to execute when trigger fires
            selector: What changes to watch (see TRIGGER_SELECTORS for options)
            config: Trigger configuration:
                - phase: "before" or "after" (default: "after")
                - params: Additional parameters passed to trigger query

        Supported selectors (AC-6):
            - createdNodes: New nodes created
            - createdRelationships: New relationships created
            - deletedNodes: Nodes deleted
            - deletedRelationships: Relationships deleted
            - assignedLabels: Labels added to nodes
            - removedLabels: Labels removed from nodes
            - assignedNodeProperties: Node properties set
            - assignedRelationshipProperties: Relationship properties set

        Transaction context variables (AC-12):
            - $createdNodes, $deletedNodes lists
            - $createdRelationships, $deletedRelationships lists
            - $assignedLabels, $removedLabels maps
            - $assignedNodeProperties, $assignedRelationshipProperties maps

        Returns:
            {"success": True, "trigger_name": str, "registered": True}
            or {"success": False, "error": str, "error_type": str}
        """
        if not name:
            return {
                "success": False,
                "error": "Trigger name is required",
                "error_type": "validation_error",
            }

        if not query:
            return {
                "success": False,
                "error": "Trigger query is required",
                "error_type": "validation_error",
            }

        # Check APOC availability (AC-20)
        if not self.APOC_AVAILABLE:
            return {
                "success": False,
                "error": "APOC library not available. Install APOC plugin for Neo4j to use triggers.",
                "error_type": "dependency_missing",
            }

        # Check triggers enabled (AC-21)
        if not self.TRIGGERS_ENABLED:
            return {
                "success": False,
                "error": "APOC triggers not enabled. Set apoc.trigger.enabled=true in neo4j.conf",
                "error_type": "configuration_error",
            }

        try:
            config = config or {}
            phase = config.get("phase", "after")
            params = config.get("params", {})

            # Build trigger options
            trigger_config = {"phase": phase}
            if params:
                trigger_config["params"] = params

            def work(session):
                # Register trigger using apoc.trigger.add
                cypher = """
                    CALL apoc.trigger.add($name, $query, $config)
                    YIELD name, query, selector, params, installed, paused
                    RETURN name, installed
                """
                result = session.run(
                    cypher, name=name, query=query, config=trigger_config
                )
                record = result.single()
                if record:
                    return {"name": record["name"], "installed": record["installed"]}
                return None

            with self._lock:
                result = self._execute_with_retry(work)

            if result:
                return {
                    "success": True,
                    "trigger_name": result["name"],
                    "registered": result["installed"],
                }
            else:
                return {
                    "success": False,
                    "error": "Failed to register trigger - no result returned",
                    "error_type": "query_error",
                }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            # AC-22: Warning when trigger registration fails
            return {
                "success": False,
                "error": f"Failed to register trigger: {type(e).__name__}",
                "error_type": "query_error",
            }

    def unregister_trigger(self, name: str) -> Dict[str, Any]:
        """
        Remove a registered trigger (AC-7).

        Args:
            name: Trigger name to remove

        Returns:
            {"success": True, "trigger_name": str, "removed": True}
            or {"success": False, "error": str, "error_type": str}
        """
        if not name:
            return {
                "success": False,
                "error": "Trigger name is required",
                "error_type": "validation_error",
            }

        if not self.APOC_AVAILABLE:
            return {
                "success": False,
                "error": "APOC library not available",
                "error_type": "dependency_missing",
            }

        try:

            def work(session):
                result = session.run(
                    "CALL apoc.trigger.remove($name) YIELD name RETURN name", name=name
                )
                record = result.single()
                return record["name"] if record else None

            with self._lock:
                removed_name = self._execute_with_retry(work)

            if removed_name:
                return {"success": True, "trigger_name": removed_name, "removed": True}
            else:
                return {
                    "success": False,
                    "error": f"Trigger '{name}' not found",
                    "error_type": "not_found",
                }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to remove trigger: {type(e).__name__}",
                "error_type": "query_error",
            }

    def list_triggers(self) -> Dict[str, Any]:
        """
        List all registered triggers (AC-8).

        Returns:
            {
                "success": True,
                "triggers": [
                    {
                        "name": str,
                        "query": str,
                        "selector": dict,
                        "params": dict,
                        "installed": bool,
                        "paused": bool
                    },
                    ...
                ],
                "count": int
            }
        """
        if not self.APOC_AVAILABLE:
            return {
                "success": False,
                "error": "APOC library not available",
                "error_type": "dependency_missing",
            }

        try:

            def work(session):
                result = session.run(
                    """
                    CALL apoc.trigger.list()
                    YIELD name, query, selector, params, installed, paused
                    RETURN name, query, selector, params, installed, paused
                """
                )
                triggers = []
                for record in result:
                    triggers.append(
                        {
                            "name": record["name"],
                            "query": record["query"],
                            "selector": record["selector"],
                            "params": record["params"],
                            "installed": record["installed"],
                            "paused": record["paused"],
                        }
                    )
                return triggers

            with self._lock:
                triggers = self._execute_with_retry(work)

            return {"success": True, "triggers": triggers, "count": len(triggers)}

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to list triggers: {type(e).__name__}",
                "error_type": "query_error",
            }

    def pause_trigger(self, name: str) -> Dict[str, Any]:
        """
        Temporarily disable a trigger without removing it (AC-9).

        Args:
            name: Trigger name to pause

        Returns:
            {"success": True, "trigger_name": str, "paused": True}
            or {"success": False, "error": str, "error_type": str}
        """
        if not name:
            return {
                "success": False,
                "error": "Trigger name is required",
                "error_type": "validation_error",
            }

        if not self.APOC_AVAILABLE:
            return {
                "success": False,
                "error": "APOC library not available",
                "error_type": "dependency_missing",
            }

        try:

            def work(session):
                result = session.run(
                    "CALL apoc.trigger.pause($name) YIELD name, paused RETURN name, paused",
                    name=name,
                )
                record = result.single()
                if record:
                    return {"name": record["name"], "paused": record["paused"]}
                return None

            with self._lock:
                result = self._execute_with_retry(work)

            if result:
                return {
                    "success": True,
                    "trigger_name": result["name"],
                    "paused": result["paused"],
                }
            else:
                return {
                    "success": False,
                    "error": f"Trigger '{name}' not found",
                    "error_type": "not_found",
                }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to pause trigger: {type(e).__name__}",
                "error_type": "query_error",
            }

    def resume_trigger(self, name: str) -> Dict[str, Any]:
        """
        Re-enable a paused trigger (AC-10).

        Args:
            name: Trigger name to resume

        Returns:
            {"success": True, "trigger_name": str, "paused": False}
            or {"success": False, "error": str, "error_type": str}
        """
        if not name:
            return {
                "success": False,
                "error": "Trigger name is required",
                "error_type": "validation_error",
            }

        if not self.APOC_AVAILABLE:
            return {
                "success": False,
                "error": "APOC library not available",
                "error_type": "dependency_missing",
            }

        try:

            def work(session):
                result = session.run(
                    "CALL apoc.trigger.resume($name) YIELD name, paused RETURN name, paused",
                    name=name,
                )
                record = result.single()
                if record:
                    return {"name": record["name"], "paused": record["paused"]}
                return None

            with self._lock:
                result = self._execute_with_retry(work)

            if result:
                return {
                    "success": True,
                    "trigger_name": result["name"],
                    "paused": result["paused"],
                }
            else:
                return {
                    "success": False,
                    "error": f"Trigger '{name}' not found",
                    "error_type": "not_found",
                }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to resume trigger: {type(e).__name__}",
                "error_type": "query_error",
            }

    # =========================================================================
    # CALLBACK MECHANISMS (TEA-BUILTIN-001.7.5 AC-13 to AC-14)
    # =========================================================================

    def register_trigger_callback(
        self, name: str, callback_url: str, config: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Register a trigger that fires an HTTP webhook on graph changes (AC-13).

        The trigger will POST to the callback_url when changes matching
        the selector occur. Uses apoc.load.jsonParams for HTTP calls.

        Args:
            name: Unique trigger identifier
            callback_url: HTTP endpoint to POST to when trigger fires
            config: Trigger configuration:
                - selector: What changes to watch (default: createdNodes)
                - phase: "before" or "after" (default: "after")
                - headers: Optional HTTP headers
                - label_filter: Optional label to filter nodes

        Returns:
            {"success": True, "trigger_name": str, "registered": True, "callback_url": str}
            or {"success": False, "error": str, "error_type": str}
        """
        if not name:
            return {
                "success": False,
                "error": "Trigger name is required",
                "error_type": "validation_error",
            }

        if not callback_url:
            return {
                "success": False,
                "error": "Callback URL is required",
                "error_type": "validation_error",
            }

        config = config or {}
        selector = config.get("selector", "createdNodes")
        phase = config.get("phase", "after")
        label_filter = config.get("label_filter")

        # Build the webhook query
        if label_filter:
            query = f"""
                UNWIND ${selector} AS n
                WITH n WHERE n:{label_filter}
                CALL apoc.load.jsonParams(
                    $callback_url,
                    {{method: 'POST', `Content-Type`: 'application/json'}},
                    {{event: '{selector}', data: properties(n), id: id(n)}}
                ) YIELD value
                RETURN value
            """
        else:
            query = f"""
                UNWIND ${selector} AS n
                CALL apoc.load.jsonParams(
                    $callback_url,
                    {{method: 'POST', `Content-Type`: 'application/json'}},
                    {{event: '{selector}', data: properties(n), id: id(n)}}
                ) YIELD value
                RETURN value
            """

        trigger_config = {"phase": phase, "params": {"callback_url": callback_url}}

        result = self.register_trigger(name=name, query=query, config=trigger_config)

        if result.get("success"):
            result["callback_url"] = callback_url

        return result

    def register_trigger_state_update(
        self,
        name: str,
        state_key: str,
        transform: Optional[str] = None,
        config: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Register a trigger that writes to a state node for agent consumption (AC-14).

        Creates/updates a TriggerStateLog node that agents can query for
        triggered events. This provides an in-database event queue pattern.

        Args:
            name: Unique trigger identifier
            state_key: Key to use in the TriggerStateLog node
            transform: Optional Cypher expression to transform the data
            config: Trigger configuration:
                - selector: What changes to watch (default: createdNodes)
                - phase: "before" or "after" (default: "after")
                - label_filter: Optional label to filter nodes

        Returns:
            {"success": True, "trigger_name": str, "registered": True, "state_key": str}
            or {"success": False, "error": str, "error_type": str}
        """
        if not name:
            return {
                "success": False,
                "error": "Trigger name is required",
                "error_type": "validation_error",
            }

        if not state_key:
            return {
                "success": False,
                "error": "State key is required",
                "error_type": "validation_error",
            }

        config = config or {}
        selector = config.get("selector", "createdNodes")
        phase = config.get("phase", "after")
        label_filter = config.get("label_filter")

        # Build the data expression
        data_expr = transform if transform else "properties(n)"

        # Build the state update query
        if label_filter:
            query = f"""
                UNWIND ${selector} AS n
                WITH n WHERE n:{label_filter}
                CREATE (log:TriggerStateLog {{
                    state_key: $state_key,
                    event_type: '{selector}',
                    data: {data_expr},
                    entity_id: id(n),
                    timestamp: datetime(),
                    trigger_name: $trigger_name
                }})
                RETURN log
            """
        else:
            query = f"""
                UNWIND ${selector} AS n
                CREATE (log:TriggerStateLog {{
                    state_key: $state_key,
                    event_type: '{selector}',
                    data: {data_expr},
                    entity_id: id(n),
                    timestamp: datetime(),
                    trigger_name: $trigger_name
                }})
                RETURN log
            """

        trigger_config = {
            "phase": phase,
            "params": {"state_key": state_key, "trigger_name": name},
        }

        result = self.register_trigger(name=name, query=query, config=trigger_config)

        if result.get("success"):
            result["state_key"] = state_key

        return result

    # =========================================================================
    # LIFECYCLE MANAGEMENT (TEA-BUILTIN-001.7.5 AC-15 to AC-17)
    # =========================================================================

    def cleanup_triggers(self, prefix: Optional[str] = None) -> Dict[str, Any]:
        """
        Remove triggers by prefix, used for session/agent cleanup (AC-15).

        Args:
            prefix: If provided, only remove triggers with names starting with this prefix.
                   If None, removes all triggers (use with caution).

        Returns:
            {
                "success": True,
                "removed": list of trigger names removed,
                "count": int
            }
        """
        if not self.APOC_AVAILABLE:
            return {
                "success": False,
                "error": "APOC library not available",
                "error_type": "dependency_missing",
            }

        try:
            # First list all triggers
            list_result = self.list_triggers()
            if not list_result.get("success"):
                return list_result

            triggers = list_result.get("triggers", [])
            removed = []

            for trigger in triggers:
                trigger_name = trigger.get("name", "")
                # Check prefix filter
                if prefix is None or trigger_name.startswith(prefix):
                    remove_result = self.unregister_trigger(trigger_name)
                    if remove_result.get("success"):
                        removed.append(trigger_name)

            return {"success": True, "removed": removed, "count": len(removed)}

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to cleanup triggers: {type(e).__name__}",
                "error_type": "query_error",
            }

    # =========================================================================
    # TRANSACTION SUPPORT (TEA-BUILTIN-001.7.2 AC-11 to AC-14)
    # =========================================================================

    def begin_transaction(self):
        """
        Start an explicit transaction and return a transaction context.

        Returns a Neo4jTransaction object that can be used to execute
        queries within the transaction.

        Returns:
            Neo4jTransaction context manager

        Example:
            with backend.begin_transaction() as tx:
                tx.store_entity("person_1", "Person", {"name": "Alice"})
                tx.store_relation("person_1", "person_2", "KNOWS")
            # Auto-commit on success, rollback on exception
        """
        return Neo4jTransaction(self)

    def transaction(self):
        """
        Return a transaction context manager (convenience alias for begin_transaction).

        Returns:
            Neo4jTransaction context manager
        """
        return self.begin_transaction()

    # =========================================================================
    # MERGE OPERATIONS (TEA-BUILTIN-001.7.2 AC-15 to AC-16)
    # =========================================================================

    def merge_entity(
        self,
        entity_id: str,
        entity_type: str,
        on_create: Optional[Dict[str, Any]] = None,
        on_match: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Conditional upsert of an entity with ON CREATE / ON MATCH semantics.

        Args:
            entity_id: Unique identifier for the entity
            entity_type: Type/label of the entity
            on_create: Properties to set only when creating (new entity)
            on_match: Properties to set only when updating (existing entity)

        Returns:
            {"success": True, "entity_id": str, "created": bool, "properties": dict}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not entity_id or not entity_type:
            return {
                "success": False,
                "error": "entity_id and entity_type are required",
                "error_type": "validation_error",
            }

        # Sanitize entity_type
        import re

        safe_label = re.sub(r"[^a-zA-Z0-9_]", "_", str(entity_type))
        if not safe_label or not safe_label[0].isalpha():
            safe_label = "Entity_" + safe_label

        try:
            entity_id_str = str(entity_id)
            on_create_props = on_create or {}
            on_match_props = on_match or {}

            def work(session):
                # Check if entity exists first
                check_query = """
                    MATCH (e {id: $entity_id})
                    RETURN e.id, e.properties
                """
                check_result = session.run(check_query, entity_id=entity_id_str)
                record = check_result.single()
                exists = record is not None

                # Build the MERGE query with ON CREATE / ON MATCH
                on_create_json = json.dumps(on_create_props).replace("'", "''")
                on_match_json = json.dumps(on_match_props).replace("'", "''")

                if exists:
                    # Merge with existing properties for ON MATCH
                    existing_props_str = record.get("properties", "{}")
                    existing_props = (
                        json.loads(existing_props_str) if existing_props_str else {}
                    )
                    final_props = {**existing_props, **on_match_props}
                else:
                    # Only ON CREATE properties for new entity
                    final_props = on_create_props

                final_props_json = json.dumps(final_props).replace("'", "''")

                query = f"""
                    MERGE (e:{safe_label} {{id: $entity_id}})
                    ON CREATE SET e.type = $entity_type,
                                  e.properties = '{on_create_json}',
                                  e.created_at = datetime()
                    ON MATCH SET e.properties = '{final_props_json}',
                                 e.updated_at = datetime()
                    RETURN e.id, e.properties
                """
                session.run(
                    query, entity_id=entity_id_str, entity_type=str(entity_type)
                )

                return {"exists": exists, "properties": final_props}

            with self._lock:
                result = self._execute_with_retry(work)

            return {
                "success": True,
                "entity_id": entity_id_str,
                "created": not result["exists"],
                "properties": result["properties"],
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to merge entity: {type(e).__name__}",
                "error_type": "query_error",
            }

    def merge_relation(
        self,
        from_entity: str,
        to_entity: str,
        relation_type: str,
        on_create: Optional[Dict[str, Any]] = None,
        on_match: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Conditional upsert of a relation with ON CREATE / ON MATCH semantics.

        Args:
            from_entity: Source entity ID
            to_entity: Target entity ID
            relation_type: Type of the relationship
            on_create: Properties to set only when creating (new relation)
            on_match: Properties to set only when updating (existing relation)

        Returns:
            {"success": True, "created": bool, "properties": dict}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error",
            }

        # Sanitize relation_type
        import re

        safe_rel_type = re.sub(r"[^a-zA-Z0-9_]", "_", str(relation_type)).upper()
        if not safe_rel_type or not safe_rel_type[0].isalpha():
            safe_rel_type = "REL_" + safe_rel_type

        try:
            from_id = str(from_entity)
            to_id = str(to_entity)
            on_create_props = on_create or {}
            on_match_props = on_match or {}

            def work(session):
                # Check if relationship exists
                check_query = f"""
                    MATCH (a {{id: $from_id}})-[r:{safe_rel_type}]->(b {{id: $to_id}})
                    RETURN r.properties
                """
                check_result = session.run(check_query, from_id=from_id, to_id=to_id)
                record = check_result.single()
                exists = record is not None

                on_create_json = json.dumps(on_create_props).replace("'", "''")
                on_match_json = json.dumps(on_match_props).replace("'", "''")

                if exists:
                    existing_props_str = (
                        record.get("r.properties") or record.get("properties") or "{}"
                    )
                    existing_props = (
                        json.loads(existing_props_str) if existing_props_str else {}
                    )
                    final_props = {**existing_props, **on_match_props}
                else:
                    final_props = on_create_props

                final_props_json = json.dumps(final_props).replace("'", "''")

                query = f"""
                    MATCH (a {{id: $from_id}}), (b {{id: $to_id}})
                    MERGE (a)-[r:{safe_rel_type}]->(b)
                    ON CREATE SET r.rel_type = $rel_type,
                                  r.properties = '{on_create_json}',
                                  r.created_at = datetime()
                    ON MATCH SET r.properties = '{final_props_json}',
                                 r.updated_at = datetime()
                    RETURN r.properties
                """
                session.run(
                    query, from_id=from_id, to_id=to_id, rel_type=str(relation_type)
                )

                return {"exists": exists, "properties": final_props}

            with self._lock:
                result = self._execute_with_retry(work)

            return {
                "success": True,
                "created": not result["exists"],
                "properties": result["properties"],
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to merge relation: {type(e).__name__}",
                "error_type": "query_error",
            }

    # =========================================================================
    # GDS (GRAPH DATA SCIENCE) SUPPORT (TEA-BUILTIN-001.7.4)
    # =========================================================================

    @property
    def GDS_AVAILABLE(self) -> bool:
        """
        Check if Neo4j GDS library is available.

        Returns:
            True if GDS library is installed and accessible, False otherwise.
        """
        return self.check_gds_available()

    def check_gds_available(self) -> bool:
        """
        Check if Neo4j Graph Data Science (GDS) library is available.

        Queries the database to check if GDS procedures are installed.
        GDS is an Enterprise-only feature that requires the GDS plugin.

        Returns:
            True if GDS library is available, False otherwise.
        """
        if self._closed:
            return False

        try:

            def work(session):
                result = session.run("RETURN gds.version() AS version")
                record = result.single()
                return record is not None

            with self._lock:
                return self._execute_with_retry(work)
        except Exception:
            return False

    def get_gds_version(self) -> Dict[str, Any]:
        """
        Get the installed Neo4j GDS library version.

        Returns:
            {"success": True, "version": str} if GDS is available
            {"success": False, "error": str, "error_type": str} if not available
        """
        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error",
            }

        try:

            def work(session):
                result = session.run("RETURN gds.version() AS version")
                record = result.single()
                if record:
                    return record["version"]
                return None

            with self._lock:
                version = self._execute_with_retry(work)

            if version:
                return {"success": True, "version": version}
            else:
                return {
                    "success": False,
                    "error": "GDS library not available. Requires Neo4j Enterprise with GDS plugin.",
                    "error_type": "dependency_missing",
                }
        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"GDS library not available. Requires Neo4j Enterprise with GDS plugin. ({type(e).__name__})",
                "error_type": "dependency_missing",
            }

    def _gds_not_available_error(self) -> Dict[str, Any]:
        """Return standard error for GDS not being available."""
        return {
            "success": False,
            "error": "GDS library not available. Requires Neo4j Enterprise with GDS plugin.",
            "error_type": "dependency_missing",
        }

    # =========================================================================
    # GDS GRAPH PROJECTION (TEA-BUILTIN-001.7.4)
    # =========================================================================

    def gds_project_graph(
        self,
        graph_name: str,
        node_projection: Any,
        relationship_projection: Any,
        config: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Create an in-memory graph projection for GDS algorithms.

        Args:
            graph_name: Name for the projected graph
            node_projection: Node labels and properties to include.
                Can be a string (single label), list of strings, or dict with config.
                Example: "Person" or ["Person", "Company"] or
                {"Person": {"properties": ["age", "name"]}}
            relationship_projection: Relationship types and properties to include.
                Can be a string, list, or dict with config.
                Example: "KNOWS" or {"KNOWS": {"orientation": "UNDIRECTED"}}
            config: Optional additional configuration dict
                - nodeProperties: List of node properties to include
                - relationshipProperties: List of relationship properties

        Returns:
            {
                "success": True,
                "graph_name": str,
                "node_count": int,
                "relationship_count": int
            }
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not self.check_gds_available():
            return self._gds_not_available_error()

        if not graph_name:
            return {
                "success": False,
                "error": "graph_name is required",
                "error_type": "validation_error",
            }

        try:
            config = config or {}

            def work(session):
                # Build the projection query
                query = """
                    CALL gds.graph.project(
                        $graph_name,
                        $node_projection,
                        $relationship_projection,
                        $config
                    )
                    YIELD graphName, nodeCount, relationshipCount
                    RETURN graphName, nodeCount, relationshipCount
                """
                result = session.run(
                    query,
                    graph_name=graph_name,
                    node_projection=node_projection,
                    relationship_projection=relationship_projection,
                    config=config,
                )
                record = result.single()
                if record:
                    return {
                        "graph_name": record["graphName"],
                        "node_count": record["nodeCount"],
                        "relationship_count": record["relationshipCount"],
                    }
                return None

            with self._lock:
                result = self._execute_with_retry(work)

            if result:
                return {
                    "success": True,
                    "graph_name": result["graph_name"],
                    "node_count": result["node_count"],
                    "relationship_count": result["relationship_count"],
                }
            else:
                return {
                    "success": False,
                    "error": "Failed to create graph projection",
                    "error_type": "query_error",
                }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to create graph projection: {type(e).__name__}: {str(e)}",
                "error_type": "query_error",
            }

    def gds_drop_graph(self, graph_name: str) -> Dict[str, Any]:
        """
        Drop (remove) an in-memory graph projection.

        Args:
            graph_name: Name of the projected graph to drop

        Returns:
            {"success": True, "graph_name": str, "dropped": True}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not self.check_gds_available():
            return self._gds_not_available_error()

        if not graph_name:
            return {
                "success": False,
                "error": "graph_name is required",
                "error_type": "validation_error",
            }

        try:

            def work(session):
                query = """
                    CALL gds.graph.drop($graph_name)
                    YIELD graphName
                    RETURN graphName
                """
                result = session.run(query, graph_name=graph_name)
                record = result.single()
                return record["graphName"] if record else None

            with self._lock:
                dropped_name = self._execute_with_retry(work)

            if dropped_name:
                return {"success": True, "graph_name": dropped_name, "dropped": True}
            else:
                return {
                    "success": False,
                    "error": f"Graph '{graph_name}' not found",
                    "error_type": "query_error",
                }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to drop graph: {type(e).__name__}: {str(e)}",
                "error_type": "query_error",
            }

    def gds_list_graphs(self) -> Dict[str, Any]:
        """
        List all active in-memory graph projections.

        Returns:
            {
                "success": True,
                "graphs": [
                    {
                        "graph_name": str,
                        "node_count": int,
                        "relationship_count": int,
                        "memory_usage": str
                    },
                    ...
                ],
                "count": int
            }
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not self.check_gds_available():
            return self._gds_not_available_error()

        try:

            def work(session):
                query = """
                    CALL gds.graph.list()
                    YIELD graphName, nodeCount, relationshipCount, memoryUsage
                    RETURN graphName, nodeCount, relationshipCount, memoryUsage
                """
                result = session.run(query)
                graphs = []
                for record in result:
                    graphs.append(
                        {
                            "graph_name": record["graphName"],
                            "node_count": record["nodeCount"],
                            "relationship_count": record["relationshipCount"],
                            "memory_usage": record["memoryUsage"],
                        }
                    )
                return graphs

            with self._lock:
                graphs = self._execute_with_retry(work)

            return {"success": True, "graphs": graphs, "count": len(graphs)}

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to list graphs: {type(e).__name__}: {str(e)}",
                "error_type": "query_error",
            }

    def gds_estimate_memory(
        self, algorithm: str, graph_name: str, config: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Estimate memory requirements for running a GDS algorithm.

        Args:
            algorithm: GDS algorithm name (e.g., "pageRank", "louvain", "dijkstra")
            graph_name: Name of the projected graph
            config: Optional algorithm configuration

        Returns:
            {
                "success": True,
                "algorithm": str,
                "graph_name": str,
                "required_memory": str,
                "treeview": str
            }
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not self.check_gds_available():
            return self._gds_not_available_error()

        if not algorithm or not graph_name:
            return {
                "success": False,
                "error": "algorithm and graph_name are required",
                "error_type": "validation_error",
            }

        try:
            config = config or {}

            def work(session):
                # Map algorithm names to GDS procedure names
                algo_map = {
                    "pageRank": "gds.pageRank",
                    "page_rank": "gds.pageRank",
                    "betweenness": "gds.betweenness",
                    "betweenness_centrality": "gds.betweenness",
                    "degree": "gds.degree",
                    "degree_centrality": "gds.degree",
                    "closeness": "gds.closeness",
                    "closeness_centrality": "gds.closeness",
                    "louvain": "gds.louvain",
                    "label_propagation": "gds.labelPropagation",
                    "labelPropagation": "gds.labelPropagation",
                    "wcc": "gds.wcc",
                    "dijkstra": "gds.shortestPath.dijkstra",
                    "astar": "gds.shortestPath.astar",
                    "allShortestPaths": "gds.allShortestPaths.dijkstra",
                    "all_shortest_paths": "gds.allShortestPaths.dijkstra",
                    "nodeSimilarity": "gds.nodeSimilarity",
                    "node_similarity": "gds.nodeSimilarity",
                    "knn": "gds.knn",
                }
                algo_proc = algo_map.get(algorithm, f"gds.{algorithm}")

                query = f"""
                    CALL {algo_proc}.estimate($graph_name, $config)
                    YIELD requiredMemory, treeView
                    RETURN requiredMemory, treeView
                """
                result = session.run(query, graph_name=graph_name, config=config)
                record = result.single()
                if record:
                    return {
                        "required_memory": record["requiredMemory"],
                        "treeview": record["treeView"],
                    }
                return None

            with self._lock:
                result = self._execute_with_retry(work)

            if result:
                return {
                    "success": True,
                    "algorithm": algorithm,
                    "graph_name": graph_name,
                    "required_memory": result["required_memory"],
                    "treeview": result["treeview"],
                }
            else:
                return {
                    "success": False,
                    "error": "Failed to estimate memory",
                    "error_type": "query_error",
                }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to estimate memory: {type(e).__name__}: {str(e)}",
                "error_type": "query_error",
            }

    # =========================================================================
    # GDS CENTRALITY ALGORITHMS (TEA-BUILTIN-001.7.4)
    # =========================================================================

    def _run_gds_algorithm(
        self,
        algorithm_proc: str,
        graph_name: str,
        config: Optional[Dict[str, Any]] = None,
        mode: str = "stream",
    ) -> Dict[str, Any]:
        """
        Generic method to run a GDS algorithm.

        Args:
            algorithm_proc: Full GDS procedure name (e.g., "gds.pageRank")
            graph_name: Name of the projected graph
            config: Algorithm configuration
            mode: Execution mode ("stream", "write", "mutate")

        Returns:
            Algorithm results or error dict
        """
        if not self.check_gds_available():
            return self._gds_not_available_error()

        if not graph_name:
            return {
                "success": False,
                "error": "graph_name is required",
                "error_type": "validation_error",
            }

        valid_modes = ["stream", "write", "mutate", "stats"]
        if mode not in valid_modes:
            return {
                "success": False,
                "error": f"Invalid mode. Must be one of: {valid_modes}",
                "error_type": "validation_error",
            }

        try:
            config = config or {}

            def work(session):
                # Build the procedure call based on mode
                proc_name = f"{algorithm_proc}.{mode}"

                if mode == "stream":
                    query = f"""
                        CALL {proc_name}($graph_name, $config)
                        YIELD nodeId, score
                        RETURN gds.util.asNode(nodeId).id AS entity_id, score
                        ORDER BY score DESC
                    """
                elif mode == "stats":
                    query = f"""
                        CALL {proc_name}($graph_name, $config)
                        YIELD preProcessingMillis, computeMillis, postProcessingMillis,
                              nodesConsidered, ranIterations
                        RETURN preProcessingMillis, computeMillis, postProcessingMillis,
                               nodesConsidered, ranIterations
                    """
                elif mode == "write":
                    write_property = config.pop("writeProperty", "gds_result")
                    config["writeProperty"] = write_property
                    query = f"""
                        CALL {proc_name}($graph_name, $config)
                        YIELD nodePropertiesWritten, preProcessingMillis, computeMillis,
                              postProcessingMillis
                        RETURN nodePropertiesWritten, preProcessingMillis, computeMillis,
                               postProcessingMillis
                    """
                else:  # mutate
                    mutate_property = config.pop("mutateProperty", "gds_result")
                    config["mutateProperty"] = mutate_property
                    query = f"""
                        CALL {proc_name}($graph_name, $config)
                        YIELD nodePropertiesWritten, preProcessingMillis, computeMillis,
                              postProcessingMillis
                        RETURN nodePropertiesWritten, preProcessingMillis, computeMillis,
                               postProcessingMillis
                    """

                result = session.run(query, graph_name=graph_name, config=config)

                if mode == "stream":
                    results = []
                    for record in result:
                        results.append(
                            {"entity_id": record["entity_id"], "score": record["score"]}
                        )
                    return {"results": results, "count": len(results)}
                else:
                    record = result.single()
                    if record:
                        return dict(record)
                    return {}

            with self._lock:
                result = self._execute_with_retry(work)

            return {
                "success": True,
                "algorithm": algorithm_proc,
                "graph_name": graph_name,
                "mode": mode,
                **result,
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Algorithm execution failed: {type(e).__name__}: {str(e)}",
                "error_type": "query_error",
            }

    def gds_page_rank(
        self, graph_name: str, config: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Run PageRank algorithm on a projected graph.

        PageRank measures the importance of nodes based on link structure.
        Nodes with more incoming links from important nodes get higher scores.

        Args:
            graph_name: Name of the projected graph
            config: Algorithm configuration
                - maxIterations: Max iterations (default: 20)
                - dampingFactor: Damping factor (default: 0.85)
                - tolerance: Convergence tolerance (default: 0.0000001)
                - mode: Execution mode ("stream", "write", "mutate", "stats")
                - writeProperty: Property name for write mode
                - mutateProperty: Property name for mutate mode

        Returns:
            {
                "success": True,
                "algorithm": "gds.pageRank",
                "graph_name": str,
                "mode": str,
                "results": [{"entity_id": str, "score": float}, ...] (stream mode)
            }
            or {"success": False, "error": str, "error_type": str} on failure
        """
        config = config or {}
        mode = config.pop("mode", "stream")
        return self._run_gds_algorithm("gds.pageRank", graph_name, config, mode)

    def gds_betweenness_centrality(
        self, graph_name: str, config: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Run Betweenness Centrality algorithm on a projected graph.

        Betweenness centrality measures how often a node lies on the shortest
        path between other nodes. Nodes with high betweenness are "bridges".

        Args:
            graph_name: Name of the projected graph
            config: Algorithm configuration
                - samplingSize: Number of nodes to sample (optional)
                - samplingSeed: Random seed for sampling (optional)
                - mode: Execution mode ("stream", "write", "mutate", "stats")

        Returns:
            {
                "success": True,
                "algorithm": "gds.betweenness",
                "graph_name": str,
                "mode": str,
                "results": [{"entity_id": str, "score": float}, ...] (stream mode)
            }
        """
        config = config or {}
        mode = config.pop("mode", "stream")
        return self._run_gds_algorithm("gds.betweenness", graph_name, config, mode)

    def gds_degree_centrality(
        self, graph_name: str, config: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Run Degree Centrality algorithm on a projected graph.

        Degree centrality counts the number of relationships a node has.
        Higher degree indicates more connections.

        Args:
            graph_name: Name of the projected graph
            config: Algorithm configuration
                - orientation: "NATURAL", "REVERSE", or "UNDIRECTED"
                - mode: Execution mode ("stream", "write", "mutate", "stats")

        Returns:
            {
                "success": True,
                "algorithm": "gds.degree",
                "graph_name": str,
                "mode": str,
                "results": [{"entity_id": str, "score": float}, ...] (stream mode)
            }
        """
        config = config or {}
        mode = config.pop("mode", "stream")
        return self._run_gds_algorithm("gds.degree", graph_name, config, mode)

    def gds_closeness_centrality(
        self, graph_name: str, config: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Run Closeness Centrality algorithm on a projected graph.

        Closeness centrality measures how close a node is to all other nodes.
        Nodes with high closeness can reach others quickly.

        Args:
            graph_name: Name of the projected graph
            config: Algorithm configuration
                - useWassermanFaust: Use Wasserman-Faust improvement (default: False)
                - mode: Execution mode ("stream", "write", "mutate", "stats")

        Returns:
            {
                "success": True,
                "algorithm": "gds.closeness",
                "graph_name": str,
                "mode": str,
                "results": [{"entity_id": str, "score": float}, ...] (stream mode)
            }
        """
        config = config or {}
        mode = config.pop("mode", "stream")
        return self._run_gds_algorithm("gds.closeness", graph_name, config, mode)

    # =========================================================================
    # GDS COMMUNITY DETECTION ALGORITHMS (TEA-BUILTIN-001.7.4)
    # =========================================================================

    def _run_gds_community_algorithm(
        self,
        algorithm_proc: str,
        graph_name: str,
        config: Optional[Dict[str, Any]] = None,
        mode: str = "stream",
    ) -> Dict[str, Any]:
        """
        Generic method to run a GDS community detection algorithm.

        Community algorithms return community IDs instead of scores.
        """
        if not self.check_gds_available():
            return self._gds_not_available_error()

        if not graph_name:
            return {
                "success": False,
                "error": "graph_name is required",
                "error_type": "validation_error",
            }

        valid_modes = ["stream", "write", "mutate", "stats"]
        if mode not in valid_modes:
            return {
                "success": False,
                "error": f"Invalid mode. Must be one of: {valid_modes}",
                "error_type": "validation_error",
            }

        try:
            config = config or {}

            def work(session):
                proc_name = f"{algorithm_proc}.{mode}"

                if mode == "stream":
                    query = f"""
                        CALL {proc_name}($graph_name, $config)
                        YIELD nodeId, communityId
                        RETURN gds.util.asNode(nodeId).id AS entity_id, communityId AS community_id
                    """
                elif mode == "stats":
                    query = f"""
                        CALL {proc_name}($graph_name, $config)
                        YIELD preProcessingMillis, computeMillis, postProcessingMillis,
                              communityCount
                        RETURN preProcessingMillis, computeMillis, postProcessingMillis,
                               communityCount
                    """
                elif mode == "write":
                    write_property = config.pop("writeProperty", "community")
                    config["writeProperty"] = write_property
                    query = f"""
                        CALL {proc_name}($graph_name, $config)
                        YIELD nodePropertiesWritten, communityCount, preProcessingMillis,
                              computeMillis, postProcessingMillis
                        RETURN nodePropertiesWritten, communityCount, preProcessingMillis,
                               computeMillis, postProcessingMillis
                    """
                else:  # mutate
                    mutate_property = config.pop("mutateProperty", "community")
                    config["mutateProperty"] = mutate_property
                    query = f"""
                        CALL {proc_name}($graph_name, $config)
                        YIELD nodePropertiesWritten, communityCount, preProcessingMillis,
                              computeMillis, postProcessingMillis
                        RETURN nodePropertiesWritten, communityCount, preProcessingMillis,
                               computeMillis, postProcessingMillis
                    """

                result = session.run(query, graph_name=graph_name, config=config)

                if mode == "stream":
                    results = []
                    for record in result:
                        results.append(
                            {
                                "entity_id": record["entity_id"],
                                "community_id": record["community_id"],
                            }
                        )
                    return {"results": results, "count": len(results)}
                else:
                    record = result.single()
                    if record:
                        return dict(record)
                    return {}

            with self._lock:
                result = self._execute_with_retry(work)

            return {
                "success": True,
                "algorithm": algorithm_proc,
                "graph_name": graph_name,
                "mode": mode,
                **result,
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Algorithm execution failed: {type(e).__name__}: {str(e)}",
                "error_type": "query_error",
            }

    def gds_louvain(
        self, graph_name: str, config: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Run Louvain community detection algorithm on a projected graph.

        Louvain is a hierarchical clustering algorithm that maximizes modularity.
        It identifies densely connected communities.

        Args:
            graph_name: Name of the projected graph
            config: Algorithm configuration
                - maxLevels: Maximum hierarchy levels (default: 10)
                - maxIterations: Max iterations per level (default: 10)
                - tolerance: Convergence tolerance (default: 0.0001)
                - includeIntermediateCommunities: Include hierarchy (default: False)
                - mode: Execution mode ("stream", "write", "mutate", "stats")

        Returns:
            {
                "success": True,
                "algorithm": "gds.louvain",
                "graph_name": str,
                "mode": str,
                "results": [{"entity_id": str, "community_id": int}, ...] (stream mode)
            }
        """
        config = config or {}
        mode = config.pop("mode", "stream")
        return self._run_gds_community_algorithm(
            "gds.louvain", graph_name, config, mode
        )

    def gds_label_propagation(
        self, graph_name: str, config: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Run Label Propagation community detection algorithm on a projected graph.

        Label Propagation is a fast semi-supervised algorithm where nodes adopt
        the label that most of their neighbors have.

        Args:
            graph_name: Name of the projected graph
            config: Algorithm configuration
                - maxIterations: Max iterations (default: 10)
                - nodeWeightProperty: Property for node weights (optional)
                - relationshipWeightProperty: Property for edge weights (optional)
                - mode: Execution mode ("stream", "write", "mutate", "stats")

        Returns:
            {
                "success": True,
                "algorithm": "gds.labelPropagation",
                "graph_name": str,
                "mode": str,
                "results": [{"entity_id": str, "community_id": int}, ...] (stream mode)
            }
        """
        config = config or {}
        mode = config.pop("mode", "stream")
        return self._run_gds_community_algorithm(
            "gds.labelPropagation", graph_name, config, mode
        )

    def gds_wcc(
        self, graph_name: str, config: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Run Weakly Connected Components (WCC) algorithm on a projected graph.

        WCC finds sets of nodes where each node is reachable from every other
        node in the same set, ignoring relationship direction.

        Args:
            graph_name: Name of the projected graph
            config: Algorithm configuration
                - relationshipWeightProperty: Property for edge weights (optional)
                - threshold: Weight threshold for connectivity (optional)
                - mode: Execution mode ("stream", "write", "mutate", "stats")

        Returns:
            {
                "success": True,
                "algorithm": "gds.wcc",
                "graph_name": str,
                "mode": str,
                "results": [{"entity_id": str, "community_id": int}, ...] (stream mode)
            }
        """
        config = config or {}
        mode = config.pop("mode", "stream")
        return self._run_gds_community_algorithm("gds.wcc", graph_name, config, mode)

    # =========================================================================
    # GDS PATH FINDING ALGORITHMS (TEA-BUILTIN-001.7.4)
    # =========================================================================

    def gds_dijkstra(
        self,
        graph_name: str,
        source_id: str,
        target_id: str,
        config: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Find the shortest weighted path between two nodes using Dijkstra's algorithm.

        Args:
            graph_name: Name of the projected graph
            source_id: Entity ID of the source node
            target_id: Entity ID of the target node
            config: Algorithm configuration
                - relationshipWeightProperty: Property to use as weight (optional)
                - mode: Execution mode ("stream" only for single path)

        Returns:
            {
                "success": True,
                "algorithm": "gds.shortestPath.dijkstra",
                "graph_name": str,
                "source_id": str,
                "target_id": str,
                "path": [str, ...],
                "total_cost": float,
                "node_count": int
            }
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not self.check_gds_available():
            return self._gds_not_available_error()

        if not graph_name or not source_id or not target_id:
            return {
                "success": False,
                "error": "graph_name, source_id, and target_id are required",
                "error_type": "validation_error",
            }

        try:
            config = config or {}

            def work(session):
                # First, find the internal node IDs
                query = """
                    MATCH (source {id: $source_id}), (target {id: $target_id})
                    CALL gds.shortestPath.dijkstra.stream($graph_name, {
                        sourceNode: source,
                        targetNode: target,
                        relationshipWeightProperty: $weight_property
                    })
                    YIELD index, sourceNode, targetNode, totalCost, nodeIds, costs, path
                    RETURN totalCost, nodeIds, costs,
                           [nodeId IN nodeIds | gds.util.asNode(nodeId).id] AS path_ids
                """
                weight_property = config.get("relationshipWeightProperty")
                result = session.run(
                    query,
                    graph_name=graph_name,
                    source_id=source_id,
                    target_id=target_id,
                    weight_property=weight_property,
                )
                record = result.single()
                if record:
                    return {
                        "path": record["path_ids"],
                        "total_cost": record["totalCost"],
                        "node_count": len(record["nodeIds"]),
                        "costs": list(record["costs"]),
                    }
                return None

            with self._lock:
                result = self._execute_with_retry(work)

            if result:
                return {
                    "success": True,
                    "algorithm": "gds.shortestPath.dijkstra",
                    "graph_name": graph_name,
                    "source_id": source_id,
                    "target_id": target_id,
                    **result,
                }
            else:
                return {
                    "success": False,
                    "error": f"No path found between '{source_id}' and '{target_id}'",
                    "error_type": "query_error",
                }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Path finding failed: {type(e).__name__}: {str(e)}",
                "error_type": "query_error",
            }

    def gds_astar(
        self,
        graph_name: str,
        source_id: str,
        target_id: str,
        config: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Find the shortest path using A* algorithm with heuristic.

        A* uses a heuristic function to guide the search, which can be more
        efficient than Dijkstra for large graphs when geographic coordinates
        are available.

        Args:
            graph_name: Name of the projected graph
            source_id: Entity ID of the source node
            target_id: Entity ID of the target node
            config: Algorithm configuration
                - latitudeProperty: Property containing latitude (required for A*)
                - longitudeProperty: Property containing longitude (required for A*)
                - relationshipWeightProperty: Property to use as weight (optional)

        Returns:
            {
                "success": True,
                "algorithm": "gds.shortestPath.astar",
                "graph_name": str,
                "source_id": str,
                "target_id": str,
                "path": [str, ...],
                "total_cost": float,
                "node_count": int
            }
        """
        if not self.check_gds_available():
            return self._gds_not_available_error()

        if not graph_name or not source_id or not target_id:
            return {
                "success": False,
                "error": "graph_name, source_id, and target_id are required",
                "error_type": "validation_error",
            }

        try:
            config = config or {}

            def work(session):
                query = """
                    MATCH (source {id: $source_id}), (target {id: $target_id})
                    CALL gds.shortestPath.astar.stream($graph_name, {
                        sourceNode: source,
                        targetNode: target,
                        latitudeProperty: $latitude_property,
                        longitudeProperty: $longitude_property,
                        relationshipWeightProperty: $weight_property
                    })
                    YIELD index, sourceNode, targetNode, totalCost, nodeIds, costs, path
                    RETURN totalCost, nodeIds, costs,
                           [nodeId IN nodeIds | gds.util.asNode(nodeId).id] AS path_ids
                """
                result = session.run(
                    query,
                    graph_name=graph_name,
                    source_id=source_id,
                    target_id=target_id,
                    latitude_property=config.get("latitudeProperty", "latitude"),
                    longitude_property=config.get("longitudeProperty", "longitude"),
                    weight_property=config.get("relationshipWeightProperty"),
                )
                record = result.single()
                if record:
                    return {
                        "path": record["path_ids"],
                        "total_cost": record["totalCost"],
                        "node_count": len(record["nodeIds"]),
                        "costs": list(record["costs"]),
                    }
                return None

            with self._lock:
                result = self._execute_with_retry(work)

            if result:
                return {
                    "success": True,
                    "algorithm": "gds.shortestPath.astar",
                    "graph_name": graph_name,
                    "source_id": source_id,
                    "target_id": target_id,
                    **result,
                }
            else:
                return {
                    "success": False,
                    "error": f"No path found between '{source_id}' and '{target_id}'",
                    "error_type": "query_error",
                }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Path finding failed: {type(e).__name__}: {str(e)}",
                "error_type": "query_error",
            }

    def gds_all_shortest_paths(
        self, graph_name: str, source_id: str, config: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Find shortest paths from a source node to all other nodes.

        Uses Dijkstra's algorithm to compute single-source shortest paths.

        Args:
            graph_name: Name of the projected graph
            source_id: Entity ID of the source node
            config: Algorithm configuration
                - relationshipWeightProperty: Property to use as weight (optional)
                - delta: Delta stepping parameter (optional)

        Returns:
            {
                "success": True,
                "algorithm": "gds.allShortestPaths.dijkstra",
                "graph_name": str,
                "source_id": str,
                "paths": [
                    {"target_id": str, "total_cost": float, "path": [str, ...]},
                    ...
                ],
                "count": int
            }
        """
        if not self.check_gds_available():
            return self._gds_not_available_error()

        if not graph_name or not source_id:
            return {
                "success": False,
                "error": "graph_name and source_id are required",
                "error_type": "validation_error",
            }

        try:
            config = config or {}

            def work(session):
                query = """
                    MATCH (source {id: $source_id})
                    CALL gds.allShortestPaths.dijkstra.stream($graph_name, {
                        sourceNode: source,
                        relationshipWeightProperty: $weight_property
                    })
                    YIELD index, sourceNode, targetNode, totalCost, nodeIds, costs
                    RETURN gds.util.asNode(targetNode).id AS target_id,
                           totalCost,
                           [nodeId IN nodeIds | gds.util.asNode(nodeId).id] AS path_ids
                """
                result = session.run(
                    query,
                    graph_name=graph_name,
                    source_id=source_id,
                    weight_property=config.get("relationshipWeightProperty"),
                )
                paths = []
                for record in result:
                    paths.append(
                        {
                            "target_id": record["target_id"],
                            "total_cost": record["totalCost"],
                            "path": record["path_ids"],
                        }
                    )
                return paths

            with self._lock:
                paths = self._execute_with_retry(work)

            return {
                "success": True,
                "algorithm": "gds.allShortestPaths.dijkstra",
                "graph_name": graph_name,
                "source_id": source_id,
                "paths": paths,
                "count": len(paths),
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Path finding failed: {type(e).__name__}: {str(e)}",
                "error_type": "query_error",
            }

    # =========================================================================
    # GDS NODE SIMILARITY ALGORITHMS (TEA-BUILTIN-001.7.4)
    # =========================================================================

    def gds_node_similarity(
        self, graph_name: str, config: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Compute Jaccard similarity between nodes based on shared neighbors.

        Node Similarity compares nodes based on the overlap of their neighbor sets.
        Useful for finding similar nodes in bipartite-like graphs.

        Args:
            graph_name: Name of the projected graph
            config: Algorithm configuration
                - similarityCutoff: Minimum similarity threshold (default: 0.0)
                - topK: Number of similar pairs per node (default: 10)
                - topN: Total number of pairs to return (optional)
                - mode: Execution mode ("stream", "write", "mutate", "stats")

        Returns:
            {
                "success": True,
                "algorithm": "gds.nodeSimilarity",
                "graph_name": str,
                "mode": str,
                "results": [
                    {"entity1_id": str, "entity2_id": str, "similarity": float},
                    ...
                ] (stream mode)
            }
        """
        if not self.check_gds_available():
            return self._gds_not_available_error()

        if not graph_name:
            return {
                "success": False,
                "error": "graph_name is required",
                "error_type": "validation_error",
            }

        try:
            config = config or {}
            mode = config.pop("mode", "stream")

            def work(session):
                proc_name = f"gds.nodeSimilarity.{mode}"

                if mode == "stream":
                    query = f"""
                        CALL {proc_name}($graph_name, $config)
                        YIELD node1, node2, similarity
                        RETURN gds.util.asNode(node1).id AS entity1_id,
                               gds.util.asNode(node2).id AS entity2_id,
                               similarity
                        ORDER BY similarity DESC
                    """
                else:
                    query = f"""
                        CALL {proc_name}($graph_name, $config)
                        YIELD preProcessingMillis, computeMillis, postProcessingMillis,
                              nodesCompared, relationshipsWritten
                        RETURN preProcessingMillis, computeMillis, postProcessingMillis,
                               nodesCompared, relationshipsWritten
                    """

                result = session.run(query, graph_name=graph_name, config=config)

                if mode == "stream":
                    results = []
                    for record in result:
                        results.append(
                            {
                                "entity1_id": record["entity1_id"],
                                "entity2_id": record["entity2_id"],
                                "similarity": record["similarity"],
                            }
                        )
                    return {"results": results, "count": len(results)}
                else:
                    record = result.single()
                    if record:
                        return dict(record)
                    return {}

            with self._lock:
                result = self._execute_with_retry(work)

            return {
                "success": True,
                "algorithm": "gds.nodeSimilarity",
                "graph_name": graph_name,
                "mode": mode,
                **result,
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Algorithm execution failed: {type(e).__name__}: {str(e)}",
                "error_type": "query_error",
            }

    def gds_knn(
        self, graph_name: str, config: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Run K-Nearest Neighbors algorithm on node properties.

        KNN finds similar nodes based on node property values rather than
        graph structure. Useful when nodes have numerical feature vectors.

        Args:
            graph_name: Name of the projected graph
            config: Algorithm configuration
                - nodeProperties: List of properties to compare (required)
                - topK: Number of neighbors per node (default: 10)
                - similarityCutoff: Minimum similarity threshold (default: 0.0)
                - sampleRate: Sampling rate (default: 0.5)
                - deltaThreshold: Convergence threshold (default: 0.001)
                - mode: Execution mode ("stream", "write", "mutate", "stats")

        Returns:
            {
                "success": True,
                "algorithm": "gds.knn",
                "graph_name": str,
                "mode": str,
                "results": [
                    {"entity1_id": str, "entity2_id": str, "similarity": float},
                    ...
                ] (stream mode)
            }
        """
        if not self.check_gds_available():
            return self._gds_not_available_error()

        if not graph_name:
            return {
                "success": False,
                "error": "graph_name is required",
                "error_type": "validation_error",
            }

        try:
            config = config or {}
            mode = config.pop("mode", "stream")

            if "nodeProperties" not in config:
                return {
                    "success": False,
                    "error": "nodeProperties is required for KNN algorithm",
                    "error_type": "validation_error",
                }

            def work(session):
                proc_name = f"gds.knn.{mode}"

                if mode == "stream":
                    query = f"""
                        CALL {proc_name}($graph_name, $config)
                        YIELD node1, node2, similarity
                        RETURN gds.util.asNode(node1).id AS entity1_id,
                               gds.util.asNode(node2).id AS entity2_id,
                               similarity
                        ORDER BY similarity DESC
                    """
                else:
                    query = f"""
                        CALL {proc_name}($graph_name, $config)
                        YIELD preProcessingMillis, computeMillis, postProcessingMillis,
                              nodesCompared, relationshipsWritten
                        RETURN preProcessingMillis, computeMillis, postProcessingMillis,
                               nodesCompared, relationshipsWritten
                    """

                result = session.run(query, graph_name=graph_name, config=config)

                if mode == "stream":
                    results = []
                    for record in result:
                        results.append(
                            {
                                "entity1_id": record["entity1_id"],
                                "entity2_id": record["entity2_id"],
                                "similarity": record["similarity"],
                            }
                        )
                    return {"results": results, "count": len(results)}
                else:
                    record = result.single()
                    if record:
                        return dict(record)
                    return {}

            with self._lock:
                result = self._execute_with_retry(work)

            return {
                "success": True,
                "algorithm": "gds.knn",
                "graph_name": graph_name,
                "mode": mode,
                **result,
            }

        except ConnectionError as e:
            return {"success": False, "error": str(e), "error_type": "connection_error"}
        except Exception as e:
            return {
                "success": False,
                "error": f"Algorithm execution failed: {type(e).__name__}: {str(e)}",
                "error_type": "query_error",
            }


class Neo4jTransaction:
    """
    Transaction context manager for Neo4j operations.

    Provides transactional guarantees - all operations succeed or all fail.
    Auto-commits on success, auto-rollbacks on exception.

    Example:
        with backend.transaction() as tx:
            tx.store_entity("person_1", "Person", {"name": "Alice"})
            tx.store_relation("person_1", "person_2", "KNOWS")
        # Auto-commit on success

        try:
            with backend.transaction() as tx:
                tx.store_entity("person_1", "Person", {"name": "Alice"})
                raise ValueError("Something went wrong")
        except ValueError:
            pass  # Transaction was auto-rolled back
    """

    def __init__(self, backend: "Neo4jBackend"):
        """Initialize transaction context."""
        self._backend = backend
        self._session = None
        self._tx = None
        self._committed = False
        self._rolled_back = False

    def __enter__(self):
        """Enter transaction context - start a new transaction."""
        if self._backend._closed:
            raise ConnectionError("Backend is closed")

        self._session = self._backend._driver.session(database=self._backend._database)
        self._tx = self._session.begin_transaction()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Exit transaction context - commit or rollback."""
        try:
            if exc_type is not None:
                # Exception occurred - rollback
                if self._tx and not self._committed and not self._rolled_back:
                    try:
                        self._tx.rollback()
                        self._rolled_back = True
                    except Exception:
                        pass
            else:
                # No exception - commit
                if self._tx and not self._committed and not self._rolled_back:
                    self._tx.commit()
                    self._committed = True
        finally:
            # Clean up session
            if self._session:
                try:
                    self._session.close()
                except Exception:
                    pass
                self._session = None
            self._tx = None

        return False  # Don't suppress exceptions

    def commit(self) -> Dict[str, Any]:
        """
        Explicitly commit the transaction.

        Returns:
            {"success": True} or {"success": False, "error": str}
        """
        if self._committed:
            return {
                "success": False,
                "error": "Transaction already committed",
                "error_type": "transaction_error",
            }
        if self._rolled_back:
            return {
                "success": False,
                "error": "Transaction already rolled back",
                "error_type": "transaction_error",
            }
        if not self._tx:
            return {
                "success": False,
                "error": "No active transaction",
                "error_type": "transaction_error",
            }

        try:
            self._tx.commit()
            self._committed = True
            return {"success": True}
        except Exception as e:
            return {
                "success": False,
                "error": f"Commit failed: {type(e).__name__}",
                "error_type": "transaction_error",
            }

    def rollback(self) -> Dict[str, Any]:
        """
        Explicitly rollback the transaction.

        Returns:
            {"success": True} or {"success": False, "error": str}
        """
        if self._committed:
            return {
                "success": False,
                "error": "Transaction already committed",
                "error_type": "transaction_error",
            }
        if self._rolled_back:
            return {
                "success": False,
                "error": "Transaction already rolled back",
                "error_type": "transaction_error",
            }
        if not self._tx:
            return {
                "success": False,
                "error": "No active transaction",
                "error_type": "transaction_error",
            }

        try:
            self._tx.rollback()
            self._rolled_back = True
            return {"success": True}
        except Exception as e:
            return {
                "success": False,
                "error": f"Rollback failed: {type(e).__name__}",
                "error_type": "transaction_error",
            }

    def store_entity(
        self,
        entity_id: str,
        entity_type: str,
        properties: Optional[Dict[str, Any]] = None,
        embedding: Optional[List[float]] = None,
    ) -> Dict[str, Any]:
        """Store an entity within this transaction."""
        if not self._tx:
            return {
                "success": False,
                "error": "No active transaction",
                "error_type": "transaction_error",
            }

        if not entity_id or not entity_type:
            return {
                "success": False,
                "error": "entity_id and entity_type are required",
                "error_type": "validation_error",
            }

        import re

        safe_label = re.sub(r"[^a-zA-Z0-9_]", "_", str(entity_type))
        if not safe_label or not safe_label[0].isalpha():
            safe_label = "Entity_" + safe_label

        try:
            entity_id_str = str(entity_id)
            props_json = json.dumps(properties or {})
            props_escaped = props_json.replace("'", "''")

            # Check if exists
            check_query = """
                MATCH (e {id: $entity_id})
                RETURN e.id
            """
            check_result = self._tx.run(check_query, entity_id=entity_id_str)
            exists = check_result.single() is not None

            if embedding:
                query = f"""
                    MERGE (e:{safe_label} {{id: $entity_id}})
                    SET e.type = $entity_type,
                        e.properties = '{props_escaped}',
                        e._embedding = $embedding,
                        e.updated_at = datetime()
                    RETURN e.id
                """
                self._tx.run(
                    query,
                    entity_id=entity_id_str,
                    entity_type=str(entity_type),
                    embedding=embedding,
                )
            else:
                query = f"""
                    MERGE (e:{safe_label} {{id: $entity_id}})
                    SET e.type = $entity_type,
                        e.properties = '{props_escaped}',
                        e.updated_at = datetime()
                    RETURN e.id
                """
                self._tx.run(
                    query, entity_id=entity_id_str, entity_type=str(entity_type)
                )

            return {
                "success": True,
                "entity_id": entity_id_str,
                "type": str(entity_type),
                "created": not exists,
                "has_embedding": embedding is not None,
            }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store entity: {type(e).__name__}",
                "error_type": "query_error",
            }

    def store_relation(
        self,
        from_entity: str,
        to_entity: str,
        relation_type: str,
        properties: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """Store a relation within this transaction."""
        if not self._tx:
            return {
                "success": False,
                "error": "No active transaction",
                "error_type": "transaction_error",
            }

        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error",
            }

        import re

        safe_rel_type = re.sub(r"[^a-zA-Z0-9_]", "_", str(relation_type)).upper()
        if not safe_rel_type or not safe_rel_type[0].isalpha():
            safe_rel_type = "REL_" + safe_rel_type

        try:
            from_id = str(from_entity)
            to_id = str(to_entity)
            props_json = json.dumps(properties) if properties else "{}"
            props_escaped = props_json.replace("'", "''")

            # Check if exists
            check_query = f"""
                MATCH (a {{id: $from_id}})-[r:{safe_rel_type}]->(b {{id: $to_id}})
                RETURN r
            """
            check_result = self._tx.run(check_query, from_id=from_id, to_id=to_id)
            exists = check_result.single() is not None

            query = f"""
                MATCH (a {{id: $from_id}}), (b {{id: $to_id}})
                MERGE (a)-[r:{safe_rel_type}]->(b)
                SET r.properties = '{props_escaped}',
                    r.rel_type = $rel_type,
                    r.updated_at = datetime()
                RETURN r
            """
            self._tx.run(
                query, from_id=from_id, to_id=to_id, rel_type=str(relation_type)
            )

            return {
                "success": True,
                "from": from_id,
                "to": to_id,
                "type": str(relation_type),
                "created": not exists,
            }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store relation: {type(e).__name__}",
                "error_type": "query_error",
            }

    def run(self, cypher: str, **params) -> Any:
        """
        Execute a raw Cypher query within this transaction.

        Args:
            cypher: Cypher query string
            **params: Query parameters

        Returns:
            Query result or error dict
        """
        if not self._tx:
            return {
                "success": False,
                "error": "No active transaction",
                "error_type": "transaction_error",
            }

        try:
            return self._tx.run(cypher, **params)
        except Exception as e:
            return {
                "success": False,
                "error": f"Query failed: {type(e).__name__}",
                "error_type": "query_error",
            }
