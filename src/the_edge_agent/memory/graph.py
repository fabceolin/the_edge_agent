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
        embedding: Optional[List[float]] = None
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
        properties: Optional[Dict[str, Any]] = None
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
        timeout: Optional[float] = None
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
        limit: int = 20
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
            self._client = Client('mem')
        else:
            self._client = Client('sqlite', db_path)

        self._init_schema()

    def _init_schema(self) -> None:
        """Initialize graph schema with entity and relation tables."""
        # Create entity table
        try:
            self._client.run("""
                :create entity {
                    id: String =>
                    type: String,
                    properties: String,
                    embedding: <F32; 1536>?,
                    created_at: Float default now()
                }
            """)
        except Exception:
            # Table might already exist
            pass

        # Create relation table
        try:
            self._client.run("""
                :create relation {
                    from_id: String,
                    to_id: String,
                    rel_type: String =>
                    properties: String?,
                    created_at: Float default now()
                }
            """)
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
            self._client.run(f"""
                ::hnsw create entity:semantic_idx {{
                    dim: {self.embedding_dim},
                    m: 50,
                    ef_construction: 200,
                    fields: [embedding],
                    distance: Cosine
                }}
            """)
            self._hnsw_created = True
            return True
        except Exception:
            # Index might already exist or other error
            self._hnsw_created = True  # Assume it exists
            return True

    def _parse_result(self, result: Any) -> List[List]:
        """
        Parse pycozo result into a list of rows.

        pycozo returns: {'headers': [...], 'rows': [...], 'next': None}
        """
        if isinstance(result, dict) and 'rows' in result:
            return result['rows']
        elif hasattr(result, 'rows'):
            return result.rows
        elif isinstance(result, (list, tuple)):
            return list(result)
        return []

    def store_entity(
        self,
        entity_id: str,
        entity_type: str,
        properties: Optional[Dict[str, Any]] = None,
        embedding: Optional[List[float]] = None
    ) -> Dict[str, Any]:
        """Store an entity (node) in the graph."""
        if not entity_id or not entity_type:
            return {
                "success": False,
                "error": "entity_id and entity_type are required",
                "error_type": "validation_error"
            }

        try:
            entity_id_str = str(entity_id)
            entity_type_str = str(entity_type)
            props_json = json.dumps(properties or {})

            with self._lock:
                # Check if entity exists
                result = self._client.run(
                    "?[id] := *entity[id, _, _, _, _], id = $id",
                    {"id": entity_id_str}
                )
                rows = self._parse_result(result)
                exists = len(rows) > 0

                if embedding:
                    # Ensure HNSW index exists
                    self._ensure_hnsw_index()

                    # Store with embedding
                    self._client.run("""
                        ?[id, type, properties, embedding, created_at] <- [[$id, $type, $props, $emb, now()]]
                        :put entity {id => type, properties, embedding, created_at}
                    """, {
                        'id': entity_id_str,
                        'type': entity_type_str,
                        'props': props_json,
                        'emb': embedding
                    })
                else:
                    # Store without embedding
                    self._client.run("""
                        ?[id, type, properties, embedding, created_at] <- [[$id, $type, $props, null, now()]]
                        :put entity {id => type, properties, embedding, created_at}
                    """, {
                        'id': entity_id_str,
                        'type': entity_type_str,
                        'props': props_json
                    })

                return {
                    "success": True,
                    "entity_id": entity_id_str,
                    "type": entity_type_str,
                    "created": not exists,
                    "has_embedding": embedding is not None
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store entity: {str(e)}",
                "error_type": "query_error"
            }

    def store_relation(
        self,
        from_entity: str,
        to_entity: str,
        relation_type: str,
        properties: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """Store a relation (edge) between two entities."""
        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error"
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
                    {"from": from_id, "to": to_id, "rel": rel_type}
                )
                rows = self._parse_result(result)
                exists = len(rows) > 0

                # Store relation
                self._client.run("""
                    ?[from_id, to_id, rel_type, properties, created_at] <- [[$from, $to, $rel, $props, now()]]
                    :put relation {from_id, to_id, rel_type => properties, created_at}
                """, {
                    'from': from_id,
                    'to': to_id,
                    'rel': rel_type,
                    'props': props_json
                })

                return {
                    "success": True,
                    "from": from_id,
                    "to": to_id,
                    "type": rel_type,
                    "created": not exists
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store relation: {str(e)}",
                "error_type": "query_error"
            }

    def query(
        self,
        datalog: Optional[str] = None,
        cypher: Optional[str] = None,
        pattern: Optional[Dict[str, Any]] = None,
        params: Optional[Dict[str, Any]] = None,
        limit: int = 100,
        timeout: Optional[float] = None
    ) -> Dict[str, Any]:
        """Execute a Datalog query or pattern match."""
        # CozoDB uses Datalog, not Cypher
        if cypher and not datalog:
            return {
                "success": False,
                "error": "CozoBackend uses Datalog, not Cypher. Use 'datalog' parameter instead, "
                         "or use KuzuBackend for Cypher support.",
                "error_type": "validation_error"
            }

        if not datalog and not pattern:
            return {
                "success": False,
                "error": "Either datalog or pattern is required",
                "error_type": "validation_error"
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
                headers = result.get('headers', []) if isinstance(result, dict) else []

                for row in rows:
                    if headers:
                        results.append(dict(zip(headers, row)))
                    else:
                        results.append(list(row))

                return {
                    "success": True,
                    "results": results,
                    "count": len(results),
                    "query": query_str
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Query failed: {str(e)}",
                "error_type": "query_error"
            }

    def _pattern_to_datalog(self, pattern: Dict[str, Any], limit: int) -> str:
        """Convert a simple pattern dict to Datalog query."""
        # Simple pattern: {entity_type: "Person"} -> find all Person entities
        if 'entity_type' in pattern:
            return f"""
                ?[id, type, properties] :=
                    *entity[id, type, properties, _, _],
                    type = "{pattern['entity_type']}"
                :limit {limit}
            """

        # Simple pattern: {from_entity: "X"} -> find relations from X
        if 'from_entity' in pattern:
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
        limit: int = 20
    ) -> Dict[str, Any]:
        """Retrieve relevant subgraph context."""
        if not query and not embedding and not entity_id:
            return {
                "success": False,
                "error": "One of query, embedding, or entity_id is required",
                "error_type": "validation_error"
            }

        try:
            entities = []
            relations = []

            with self._lock:
                if entity_id:
                    # Expand from entity via N-hop traversal
                    entities, relations = self._expand_from_entity(entity_id, hops, limit)

                elif embedding:
                    # HNSW vector search
                    self._ensure_hnsw_index()
                    entities = self._hnsw_search(embedding, limit)

                    # Get relations between found entities
                    if entities:
                        entity_ids = [e['id'] for e in entities]
                        relations = self._get_relations_between(entity_ids)

                # Build context summary
                context_summary = self._build_context_summary(entities, relations)

                return {
                    "success": True,
                    "entities": entities,
                    "relations": relations,
                    "context_summary": context_summary
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to retrieve context: {str(e)}",
                "error_type": "query_error"
            }

    def _expand_from_entity(self, entity_id: str, hops: int, limit: int) -> tuple:
        """Expand N hops from an entity."""
        entities = []
        relations = []

        # Get starting entity
        result = self._client.run(
            "?[id, type, properties] := *entity[id, type, properties, _, _], id = $id",
            {"id": entity_id}
        )

        rows = self._parse_result(result)
        for row in rows:
            entities.append({
                "id": row[0],
                "type": row[1],
                "properties": json.loads(row[2]) if row[2] else {}
            })

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
            all_hops = " ".join([f"all_connected[id] := hop{i}[id]" for i in range(1, hops + 1)])

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
                    entities.append({
                        "id": row[0],
                        "type": row[1],
                        "properties": json.loads(row[2]) if row[2] else {}
                    })

        # Get relations
        entity_ids = [e['id'] for e in entities]
        if entity_ids:
            relations = self._get_relations_between(entity_ids)

        return entities, relations

    def _hnsw_search(self, embedding: List[float], limit: int) -> List[Dict]:
        """Perform HNSW vector similarity search."""
        entities = []

        try:
            result = self._client.run(f"""
                ?[id, type, properties, score] :=
                    ~entity:semantic_idx {{id, type, properties |
                        query: $query_embedding,
                        k: {limit},
                        ef: 50,
                        bind_distance: score
                    }}
            """, {'query_embedding': embedding})

            rows = self._parse_result(result)
            for row in rows:
                entities.append({
                    "id": row[0],
                    "type": row[1],
                    "properties": json.loads(row[2]) if row[2] else {},
                    "score": float(row[3])
                })
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
                relations.append({
                    "from": row[0],
                    "to": row[1],
                    "type": row[2],
                    "properties": json.loads(row[3]) if row[3] else {}
                })
        except Exception:
            pass

        return relations

    def _build_context_summary(self, entities: List[Dict], relations: List[Dict]) -> str:
        """Build a text summary of the context."""
        if not entities:
            return "No relevant entities found."

        summary_parts = []

        # Summarize entities by type
        type_counts = {}
        for e in entities:
            t = e.get('type', 'Unknown')
            type_counts[t] = type_counts.get(t, 0) + 1

        entity_summary = ", ".join([f"{count} {t}" for t, count in type_counts.items()])
        summary_parts.append(f"Found {len(entities)} entities: {entity_summary}.")

        # Summarize relations
        if relations:
            rel_types = set(r.get('type', 'Unknown') for r in relations)
            summary_parts.append(f"Connected by {len(relations)} relations ({', '.join(rel_types)}).")

        return " ".join(summary_parts)

    def close(self) -> None:
        """Close the backend and release resources."""
        self._closed = True

        if hasattr(self, '_client') and self._client is not None:
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
        self._extension_repo = extension_repo or os.environ.get('KUZU_EXTENSION_REPO')

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
                    s3_access_key, s3_secret_key, s3_region,
                    gcs_access_key, gcs_secret_key
                )

        # Initialize schema
        self._init_schema()

    def _is_cloud_uri(self, path: str) -> bool:
        """Check if path is a cloud storage URI."""
        cloud_prefixes = ('s3://', 'gs://', 'az://', 'http://', 'https://')
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
        s3_key = s3_access_key or os.environ.get('AWS_ACCESS_KEY_ID')
        s3_secret = s3_secret_key or os.environ.get('AWS_SECRET_ACCESS_KEY')
        region = s3_region or os.environ.get('AWS_REGION', 'us-east-1')

        if s3_key and s3_secret:
            try:
                self._conn.execute(f"CALL httpfs_set_option('s3_access_key_id', '{s3_key}')")
                self._conn.execute(f"CALL httpfs_set_option('s3_secret_access_key', '{s3_secret}')")
                self._conn.execute(f"CALL httpfs_set_option('s3_region', '{region}')")
            except Exception:
                pass

        gcs_key = gcs_access_key or os.environ.get('GCS_ACCESS_KEY_ID')
        gcs_secret = gcs_secret_key or os.environ.get('GCS_SECRET_ACCESS_KEY')

        if gcs_key and gcs_secret:
            try:
                self._conn.execute(f"CALL httpfs_set_option('s3_access_key_id', '{gcs_key}')")
                self._conn.execute(f"CALL httpfs_set_option('s3_secret_access_key', '{gcs_secret}')")
                self._conn.execute("CALL httpfs_set_option('s3_endpoint', 'storage.googleapis.com')")
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

            self._conn.execute("""
                CREATE NODE TABLE Entity (
                    id STRING PRIMARY KEY,
                    type STRING,
                    properties STRING,
                    created_at TIMESTAMP DEFAULT current_timestamp()
                )
            """)

            self._conn.execute("""
                CREATE REL TABLE RELATES_TO (
                    FROM Entity TO Entity,
                    rel_type STRING,
                    properties STRING,
                    created_at TIMESTAMP DEFAULT current_timestamp()
                )
            """)

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
                clean_col = col.split('.')[-1] if '.' in col else col
                row_dict[clean_col] = row[i]
            rows.append(row_dict)

        return rows

    def store_entity(
        self,
        entity_id: str,
        entity_type: str,
        properties: Optional[Dict[str, Any]] = None,
        embedding: Optional[List[float]] = None
    ) -> Dict[str, Any]:
        """Store an entity (node) in the graph."""
        if not entity_id or not entity_type:
            return {
                "success": False,
                "error": "entity_id and entity_type are required",
                "error_type": "validation_error"
            }

        try:
            entity_id_str = str(entity_id)
            entity_type_str = str(entity_type)
            props_json = json.dumps(properties or {})

            if embedding:
                props_dict = properties or {}
                props_dict['_embedding'] = embedding
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
                    self._conn.execute(f"""
                        MATCH (e:Entity {{id: '{entity_id_str}'}})
                        SET e.type = '{entity_type_str}',
                            e.properties = '{props_escaped}'
                    """)
                else:
                    self._conn.execute(f"""
                        CREATE (e:Entity {{
                            id: '{entity_id_str}',
                            type: '{entity_type_str}',
                            properties: '{props_escaped}'
                        }})
                    """)

                return {
                    "success": True,
                    "entity_id": entity_id_str,
                    "type": entity_type_str,
                    "created": not exists,
                    "has_embedding": embedding is not None
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store entity: {str(e)}",
                "error_type": "query_error"
            }

    def store_relation(
        self,
        from_entity: str,
        to_entity: str,
        relation_type: str,
        properties: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """Store a relation (edge) between two entities."""
        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error"
            }

        try:
            from_id = str(from_entity)
            to_id = str(to_entity)
            rel_type = str(relation_type)
            props_json = json.dumps(properties) if properties else '{}'
            props_escaped = props_json.replace("'", "''")

            with self._lock:
                try:
                    results = self._execute_query(f"""
                        MATCH (a:Entity {{id: '{from_id}'}})-[r:RELATES_TO]->(b:Entity {{id: '{to_id}'}})
                        WHERE r.rel_type = '{rel_type}'
                        RETURN a.id
                    """)
                    exists = len(results) > 0
                except Exception:
                    exists = False

                if exists:
                    self._conn.execute(f"""
                        MATCH (a:Entity {{id: '{from_id}'}})-[r:RELATES_TO]->(b:Entity {{id: '{to_id}'}})
                        WHERE r.rel_type = '{rel_type}'
                        SET r.properties = '{props_escaped}'
                    """)
                else:
                    self._conn.execute(f"""
                        MATCH (a:Entity {{id: '{from_id}'}}), (b:Entity {{id: '{to_id}'}})
                        CREATE (a)-[:RELATES_TO {{rel_type: '{rel_type}', properties: '{props_escaped}'}}]->(b)
                    """)

                return {
                    "success": True,
                    "from": from_id,
                    "to": to_id,
                    "type": rel_type,
                    "created": not exists
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store relation: {str(e)}",
                "error_type": "query_error"
            }

    def query(
        self,
        cypher: Optional[str] = None,
        datalog: Optional[str] = None,
        pattern: Optional[Dict[str, Any]] = None,
        params: Optional[Dict[str, Any]] = None,
        limit: int = 100,
        timeout: Optional[float] = None
    ) -> Dict[str, Any]:
        """Execute a Cypher query or pattern match."""
        if datalog and not cypher:
            return {
                "success": False,
                "error": "KuzuBackend uses Cypher, not Datalog. Use 'cypher' parameter instead. "
                         "Example: MATCH (e:Entity) RETURN e.id, e.type LIMIT 100",
                "error_type": "validation_error"
            }

        if not cypher and not pattern:
            return {
                "success": False,
                "error": "Either cypher or pattern is required",
                "error_type": "validation_error"
            }

        try:
            query_str = cypher

            if pattern and not cypher:
                query_str = self._pattern_to_cypher(pattern, limit)

            if query_str and 'LIMIT' not in query_str.upper():
                query_str = f"{query_str} LIMIT {limit}"

            with self._lock:
                results = self._execute_query(query_str)

                return {
                    "success": True,
                    "results": results,
                    "count": len(results),
                    "query": query_str
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Query failed: {str(e)}",
                "error_type": "query_error"
            }

    def _pattern_to_cypher(self, pattern: Dict[str, Any], limit: int) -> str:
        """Convert a simple pattern dict to Cypher query."""
        if 'entity_type' in pattern:
            return f"""
                MATCH (e:Entity)
                WHERE e.type = '{pattern['entity_type']}'
                RETURN e.id, e.type, e.properties
                LIMIT {limit}
            """

        if 'from_entity' in pattern:
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
        limit: int = 20
    ) -> Dict[str, Any]:
        """Retrieve relevant subgraph context."""
        if not query and not embedding and not entity_id:
            return {
                "success": False,
                "error": "One of query, embedding, or entity_id is required",
                "error_type": "validation_error"
            }

        try:
            entities = []
            relations = []

            with self._lock:
                if entity_id:
                    entities, relations = self._expand_from_entity(entity_id, hops, limit)
                elif embedding:
                    return {
                        "success": True,
                        "entities": [],
                        "relations": [],
                        "context_summary": "Vector search not natively supported in KuzuBackend. "
                                          "Use entity_id for graph traversal, or consider CozoBackend for HNSW search."
                    }

            context_summary = self._build_context_summary(entities, relations)

            return {
                "success": True,
                "entities": entities,
                "relations": relations,
                "context_summary": context_summary
            }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to retrieve context: {str(e)}",
                "error_type": "query_error"
            }

    def _expand_from_entity(self, entity_id: str, hops: int, limit: int) -> tuple:
        """Expand N hops from an entity using Cypher."""
        entities = []
        relations = []

        try:
            results = self._execute_query(f"""
                MATCH (e:Entity {{id: '{entity_id}'}})
                RETURN e.id, e.type, e.properties
            """)

            for row in results:
                props = row.get('properties', '{}')
                entities.append({
                    "id": row['id'],
                    "type": row['type'],
                    "properties": json.loads(props) if props else {}
                })
        except Exception:
            pass

        if hops >= 1:
            try:
                results = self._execute_query(f"""
                    MATCH (start:Entity {{id: '{entity_id}'}})-[*1..{hops}]-(connected:Entity)
                    RETURN DISTINCT connected.id, connected.type, connected.properties
                    LIMIT {limit}
                """)

                for row in results:
                    if row['id'] != entity_id:
                        props = row.get('properties', '{}')
                        entities.append({
                            "id": row['id'],
                            "type": row['type'],
                            "properties": json.loads(props) if props else {}
                        })
            except Exception:
                pass

        entity_ids = [e['id'] for e in entities]
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
            results = self._execute_query(f"""
                MATCH (a:Entity)-[r:RELATES_TO]->(b:Entity)
                WHERE a.id IN [{ids_list}] AND b.id IN [{ids_list}]
                RETURN a.id AS from_id, r.rel_type, r.properties, b.id AS to_id
            """)

            for row in results:
                props = row.get('properties', '{}')
                relations.append({
                    "from": row['from_id'],
                    "to": row['to_id'],
                    "type": row['rel_type'],
                    "properties": json.loads(props) if props else {}
                })
        except Exception:
            pass

        return relations

    def _build_context_summary(self, entities: List[Dict], relations: List[Dict]) -> str:
        """Build a text summary of the context."""
        if not entities:
            return "No relevant entities found."

        summary_parts = []

        type_counts = {}
        for e in entities:
            t = e.get('type', 'Unknown')
            type_counts[t] = type_counts.get(t, 0) + 1

        entity_summary = ", ".join([f"{count} {t}" for t, count in type_counts.items()])
        summary_parts.append(f"Found {len(entities)} entities: {entity_summary}.")

        if relations:
            rel_types = set(r.get('type', 'Unknown') for r in relations)
            summary_parts.append(f"Connected by {len(relations)} relations ({', '.join(rel_types)}).")

        return " ".join(summary_parts)

    def load_from_cloud(
        self,
        uri: str,
        table_name: str = "Entity"
    ) -> Dict[str, Any]:
        """Load data from cloud storage into the graph."""
        if not self._httpfs_available:
            return {
                "success": False,
                "error": "httpfs extension not available. Cloud storage operations require httpfs.",
                "error_type": "dependency_missing"
            }

        try:
            with self._lock:
                if uri.endswith('.parquet'):
                    self._conn.execute(f"COPY {table_name} FROM '{uri}'")
                elif uri.endswith('.csv'):
                    self._conn.execute(f"COPY {table_name} FROM '{uri}' (HEADER=true)")
                else:
                    return {
                        "success": False,
                        "error": "Unsupported file format. Use .parquet or .csv",
                        "error_type": "validation_error"
                    }

                return {
                    "success": True,
                    "loaded": True,
                    "uri": uri
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to load from cloud: {str(e)}",
                "error_type": "query_error"
            }

    def save_to_cloud(
        self,
        uri: str,
        query: str = "MATCH (e:Entity) RETURN e.*"
    ) -> Dict[str, Any]:
        """Export query results to cloud storage."""
        if not self._httpfs_available:
            return {
                "success": False,
                "error": "httpfs extension not available. Cloud storage operations require httpfs.",
                "error_type": "dependency_missing"
            }

        if uri.startswith('az://'):
            return {
                "success": False,
                "error": "Azure Blob Storage is read-only. Writing requires alternative methods.",
                "error_type": "validation_error"
            }

        try:
            with self._lock:
                self._conn.execute(f"COPY ({query}) TO '{uri}'")

                return {
                    "success": True,
                    "saved": True,
                    "uri": uri
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to save to cloud: {str(e)}",
                "error_type": "query_error"
            }

    def close(self) -> None:
        """Close the backend and release resources."""
        if self._closed:
            return

        self._closed = True

        try:
            if hasattr(self, '_conn') and self._conn is not None:
                self._conn.close()
                self._conn = None
        except Exception:
            pass

        try:
            if hasattr(self, '_db') and self._db is not None:
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
