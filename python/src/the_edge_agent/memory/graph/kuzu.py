"""
Kuzu (Bighorn) Graph Database Backend.

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
    >>> from the_edge_agent.memory.graph import KuzuBackend
    >>>
    >>> backend = KuzuBackend("./agent_graph.kuzu")
    >>> result = backend.store_entity("person_1", "Person", {"name": "Alice"})
    >>> print(result['success'])  # True
    >>> result = backend.store_relation("person_1", "person_2", "KNOWS")
    >>> print(result['success'])  # True
    >>> result = backend.query(cypher="MATCH (p:Entity) RETURN p.id, p.type")
    >>> print(result['results'])  # [{'id': 'person_1', 'type': 'Person'}, ...]
    >>> backend.close()
"""

import json
import os
import threading
from typing import Any, Dict, List, Optional

from .protocol import KUZU_AVAILABLE


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


__all__ = ["KuzuBackend", "BighornBackend"]
