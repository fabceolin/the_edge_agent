"""
DuckPGQ Graph Database Backend (TEA-BUILTIN-001.8).

Provides SQL/PGQ (ISO SQL:2023) graph query capabilities via the DuckPGQ
extension integrated into DuckDB. Supports pattern matching, path finding,
and graph algorithms (PageRank, clustering, connected components).

Requires: pip install duckdb

Extension: DuckPGQ community extension (auto-installed on first use)
    INSTALL duckpgq FROM community;
    LOAD duckpgq;

Supported features:
    - CREATE/DROP PROPERTY GRAPH
    - MATCH patterns with SQL/PGQ syntax
    - ANY SHORTEST path queries
    - Graph algorithms: pagerank, weakly_connected_component, local_clustering_coefficient
    - Parquet loading from local or cloud storage (S3/GCS/Azure via httpfs)

Example:
    >>> from the_edge_agent.memory.graph import DuckPGQBackend
    >>>
    >>> backend = DuckPGQBackend()
    >>> result = backend.create_property_graph(
    ...     name="knowledge_graph",
    ...     vertex_tables=[{"name": "entities", "source": "entities.parquet", "key": "id"}],
    ...     edge_tables=[{"name": "relations", "source": "relations.parquet",
    ...                   "source_key": "from_id", "destination_key": "to_id"}]
    ... )
    >>> print(result['success'])  # True
    >>>
    >>> result = backend.query(pgq='''
    ...     FROM GRAPH_TABLE (knowledge_graph
    ...       MATCH (a:entities)-[r:relations]->(b:entities)
    ...       COLUMNS (a.id AS source, b.id AS target)
    ...     ) LIMIT 10
    ... ''')
    >>> print(result['results'])
    >>> backend.close()
"""

import json
import logging
import os
import threading
from typing import Any, Dict, List, Optional

from .protocol import DUCKPGQ_AVAILABLE

logger = logging.getLogger(__name__)


class DuckPGQBackend:
    """
    DuckPGQ implementation of GraphBackend using SQL/PGQ.

    Provides graph database functionality using DuckDB's DuckPGQ extension
    with SQL/PGQ (ISO SQL:2023) syntax. Designed for analytical graph queries
    on Parquet data with cloud storage support.

    Requires: pip install duckdb

    Features:
        - SQL/PGQ pattern matching (MATCH ... COLUMNS)
        - Shortest path queries (ANY SHORTEST)
        - Graph algorithms (PageRank, clustering, connected components)
        - Parquet loading from local or cloud storage
        - Lazy extension loading (cold start optimization)

    Example:
        >>> backend = DuckPGQBackend()
        >>> backend.create_property_graph(
        ...     name="my_graph",
        ...     vertex_tables=[{"name": "nodes", "source": "nodes.parquet", "key": "id"}],
        ...     edge_tables=[{"name": "edges", "source": "edges.parquet",
        ...                   "source_key": "src", "destination_key": "dst"}]
        ... )
        >>> result = backend.query(pgq="FROM GRAPH_TABLE (my_graph MATCH (n) COLUMNS (n.id))")
        >>> backend.close()
    """

    def __init__(
        self,
        db_path: str = ":memory:",
        enable_httpfs: bool = True,
        lazy_load_extension: bool = True,
        s3_access_key: Optional[str] = None,
        s3_secret_key: Optional[str] = None,
        s3_region: Optional[str] = None,
        s3_endpoint: Optional[str] = None,
        gcs_access_key: Optional[str] = None,
        gcs_secret_key: Optional[str] = None,
    ):
        """
        Initialize DuckPGQ backend.

        Args:
            db_path: Path to DuckDB database file, or ":memory:" for in-memory
            enable_httpfs: Enable httpfs extension for cloud storage access
            lazy_load_extension: Only load duckpgq on first graph query (cold start optimization)
            s3_access_key: AWS access key ID (optional, uses env if not set)
            s3_secret_key: AWS secret access key (optional, uses env if not set)
            s3_region: AWS region (optional, defaults to us-east-1)
            s3_endpoint: S3-compatible endpoint (e.g., storage.googleapis.com for GCS)
            gcs_access_key: GCS HMAC access key (optional)
            gcs_secret_key: GCS HMAC secret key (optional)

        Raises:
            ImportError: If duckdb is not installed
        """
        if not DUCKPGQ_AVAILABLE:
            raise ImportError("DuckDB not installed. Install with: pip install duckdb")

        import duckdb

        self._lock = threading.Lock()
        self._closed = False
        self._extension_loaded = False
        self._httpfs_loaded = False
        self._lazy_load = lazy_load_extension
        self._enable_httpfs = enable_httpfs
        self._property_graphs: Dict[str, Dict[str, Any]] = {}

        # Store credentials for later configuration
        self._s3_access_key = s3_access_key
        self._s3_secret_key = s3_secret_key
        self._s3_region = s3_region
        self._s3_endpoint = s3_endpoint
        self._gcs_access_key = gcs_access_key
        self._gcs_secret_key = gcs_secret_key

        # Initialize DuckDB connection
        self._conn = duckdb.connect(db_path)

        # Load httpfs if enabled (needed before duckpgq for cloud table loading)
        if enable_httpfs:
            self._load_httpfs()

        # Load duckpgq extension immediately if not lazy loading
        if not lazy_load_extension:
            self._load_duckpgq_extension()

    def _load_httpfs(self) -> bool:
        """Load and configure httpfs extension for cloud storage."""
        if self._httpfs_loaded:
            return True

        try:
            try:
                self._conn.execute("LOAD httpfs;")
            except Exception:
                self._conn.execute("INSTALL httpfs; LOAD httpfs;")

            # Configure S3/GCS credentials
            self._configure_cloud_credentials()
            self._httpfs_loaded = True
            logger.debug("httpfs extension loaded")
            return True

        except Exception as e:
            logger.warning(f"Failed to load httpfs extension: {e}")
            return False

    def _configure_cloud_credentials(self) -> None:
        """Configure cloud storage credentials for httpfs."""
        # AWS S3 credentials
        s3_key = self._s3_access_key or os.environ.get("AWS_ACCESS_KEY_ID")
        s3_secret = self._s3_secret_key or os.environ.get("AWS_SECRET_ACCESS_KEY")
        s3_region = self._s3_region or os.environ.get("AWS_REGION", "us-east-1")
        s3_endpoint = self._s3_endpoint or os.environ.get("S3_ENDPOINT")

        if s3_key and s3_secret:
            try:
                self._conn.execute(f"SET s3_access_key_id='{s3_key}';")
                self._conn.execute(f"SET s3_secret_access_key='{s3_secret}';")
                self._conn.execute(f"SET s3_region='{s3_region}';")
                if s3_endpoint:
                    self._conn.execute(f"SET s3_endpoint='{s3_endpoint}';")
                    self._conn.execute("SET s3_url_style='path';")
                logger.debug("S3 credentials configured")
            except Exception as e:
                logger.warning(f"Failed to configure S3 credentials: {e}")

        # GCS credentials (via S3-compatible API)
        gcs_key = self._gcs_access_key or os.environ.get("GCS_ACCESS_KEY_ID")
        gcs_secret = self._gcs_secret_key or os.environ.get("GCS_SECRET_ACCESS_KEY")

        if gcs_key and gcs_secret and not s3_key:
            try:
                self._conn.execute(f"SET s3_access_key_id='{gcs_key}';")
                self._conn.execute(f"SET s3_secret_access_key='{gcs_secret}';")
                self._conn.execute("SET s3_endpoint='storage.googleapis.com';")
                self._conn.execute("SET s3_url_style='path';")
                logger.debug("GCS credentials configured via S3 compatibility")
            except Exception as e:
                logger.warning(f"Failed to configure GCS credentials: {e}")

    def _load_duckpgq_extension(self) -> Dict[str, Any]:
        """
        Load the DuckPGQ extension.

        Returns:
            {"success": True} or {"success": False, "error": str, "error_type": str}
        """
        if self._extension_loaded:
            return {"success": True, "already_loaded": True}

        try:
            try:
                self._conn.execute("LOAD duckpgq;")
            except Exception:
                # Try installing from community repository
                self._conn.execute("INSTALL duckpgq FROM community; LOAD duckpgq;")

            self._extension_loaded = True
            logger.info("DuckPGQ extension loaded")
            return {"success": True, "already_loaded": False}

        except Exception as e:
            error_msg = str(e)
            logger.error(f"Failed to load DuckPGQ extension: {error_msg}")
            return {
                "success": False,
                "error": f"DuckPGQ extension not available: {error_msg}. "
                "Ensure DuckDB version supports community extensions. "
                "Try: pip install duckdb --upgrade",
                "error_type": "dependency_missing",
            }

    def _ensure_extension_loaded(self) -> Dict[str, Any]:
        """Ensure DuckPGQ extension is loaded (lazy loading)."""
        if self._extension_loaded:
            return {"success": True}
        return self._load_duckpgq_extension()

    # =========================================================================
    # PROPERTY GRAPH MANAGEMENT (AC-5, AC-6, AC-7, AC-8)
    # =========================================================================

    def create_property_graph(
        self,
        name: str,
        vertex_tables: List[Dict[str, Any]],
        edge_tables: List[Dict[str, Any]],
    ) -> Dict[str, Any]:
        """
        Create a property graph from vertex and edge tables.

        Args:
            name: Name of the property graph
            vertex_tables: List of vertex table definitions:
                [{"name": "entities", "source": "path/to/entities.parquet", "key": "id"}]
            edge_tables: List of edge table definitions:
                [{"name": "relations", "source": "path/to/relations.parquet",
                  "source_key": "from_id", "destination_key": "to_id",
                  "references": "entities"}]

        Returns:
            {"success": True, "graph": str} or error dict
        """
        if not name:
            return {
                "success": False,
                "error": "Graph name is required",
                "error_type": "validation_error",
            }

        if not vertex_tables:
            return {
                "success": False,
                "error": "At least one vertex table is required",
                "error_type": "validation_error",
            }

        # Ensure extension is loaded
        ext_result = self._ensure_extension_loaded()
        if not ext_result.get("success"):
            return ext_result

        try:
            with self._lock:
                # Drop existing graph if exists
                try:
                    self._conn.execute(f"DROP PROPERTY GRAPH IF EXISTS {name};")
                except Exception:
                    pass

                # Create tables from Parquet sources
                for vtable in vertex_tables:
                    table_name = vtable["name"]
                    source = vtable["source"]
                    self._conn.execute(
                        f"CREATE OR REPLACE TABLE {table_name} AS "
                        f"SELECT * FROM read_parquet('{source}');"
                    )

                for etable in edge_tables:
                    table_name = etable["name"]
                    source = etable["source"]
                    self._conn.execute(
                        f"CREATE OR REPLACE TABLE {table_name} AS "
                        f"SELECT * FROM read_parquet('{source}');"
                    )

                # Build CREATE PROPERTY GRAPH statement
                sql = self._build_create_property_graph_sql(
                    name, vertex_tables, edge_tables
                )
                self._conn.execute(sql)

                # Store graph metadata
                self._property_graphs[name] = {
                    "vertex_tables": vertex_tables,
                    "edge_tables": edge_tables,
                }

                logger.info(f"Created property graph: {name}")
                return {"success": True, "graph": name}

        except Exception as e:
            logger.error(f"Failed to create property graph: {e}")
            return {
                "success": False,
                "error": f"Failed to create property graph: {str(e)}",
                "error_type": "query_error",
            }

    def _build_create_property_graph_sql(
        self,
        name: str,
        vertex_tables: List[Dict[str, Any]],
        edge_tables: List[Dict[str, Any]],
    ) -> str:
        """Build CREATE PROPERTY GRAPH SQL statement."""
        vertex_defs = []
        for vtable in vertex_tables:
            table_name = vtable["name"]
            key = vtable.get("key", "id")
            # Label defaults to table name
            label = vtable.get("label", table_name)
            vertex_defs.append(f"{table_name} PROPERTIES ARE ALL COLUMNS LABEL {label}")

        edge_defs = []
        for etable in edge_tables:
            table_name = etable["name"]
            source_key = etable.get("source_key", "source_id")
            dest_key = etable.get("destination_key", "target_id")
            # References can be a single table or source/destination pair
            references = etable.get("references")
            source_ref = etable.get("source_references", references)
            dest_ref = etable.get("destination_references", references or source_ref)
            label = etable.get("label", table_name)

            edge_defs.append(
                f"{table_name} "
                f"SOURCE KEY ({source_key}) REFERENCES {source_ref} "
                f"DESTINATION KEY ({dest_key}) REFERENCES {dest_ref} "
                f"PROPERTIES ARE ALL COLUMNS LABEL {label}"
            )

        sql = f"""
            CREATE PROPERTY GRAPH {name}
            VERTEX TABLES (
                {', '.join(vertex_defs)}
            )
            EDGE TABLES (
                {', '.join(edge_defs)}
            );
        """
        return sql

    def drop_property_graph(self, name: str) -> Dict[str, Any]:
        """
        Drop a property graph.

        Args:
            name: Name of the property graph to drop

        Returns:
            {"success": True} or error dict
        """
        if not name:
            return {
                "success": False,
                "error": "Graph name is required",
                "error_type": "validation_error",
            }

        try:
            with self._lock:
                self._conn.execute(f"DROP PROPERTY GRAPH IF EXISTS {name};")
                self._property_graphs.pop(name, None)
                logger.info(f"Dropped property graph: {name}")
                return {"success": True, "graph": name}

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to drop property graph: {str(e)}",
                "error_type": "query_error",
            }

    def list_property_graphs(self) -> Dict[str, Any]:
        """
        List all created property graphs.

        Returns:
            {"success": True, "graphs": list}
        """
        return {
            "success": True,
            "graphs": list(self._property_graphs.keys()),
        }

    # =========================================================================
    # SQL/PGQ QUERY EXECUTION (AC-9, AC-10, AC-11, AC-12, AC-13)
    # =========================================================================

    def query(
        self,
        pgq: Optional[str] = None,
        datalog: Optional[str] = None,
        pattern: Optional[Dict[str, Any]] = None,
        params: Optional[Dict[str, Any]] = None,
        limit: int = 100,
        timeout: Optional[float] = None,
    ) -> Dict[str, Any]:
        """
        Execute a SQL/PGQ query.

        Args:
            pgq: SQL/PGQ query string (uses FROM GRAPH_TABLE syntax)
            datalog: Ignored (DuckPGQ uses SQL/PGQ, not Datalog)
            pattern: Simple pattern dict (alternative to raw PGQ)
            params: Query parameters for Jinja2 template substitution
            limit: Maximum results to return
            timeout: Query timeout in seconds (None for no timeout)

        Returns:
            {"success": True, "results": list, "count": int, "query": str}
            or error dict

        Example PGQ query:
            FROM GRAPH_TABLE (knowledge_graph
              MATCH (a:entities)-[r:relations]->(b:entities)
              COLUMNS (a.id AS source, r.type AS relation, b.id AS target)
            )
            ORDER BY source
            LIMIT 10
        """
        if datalog and not pgq:
            return {
                "success": False,
                "error": "DuckPGQBackend uses SQL/PGQ, not Datalog. "
                "Use 'pgq' parameter with FROM GRAPH_TABLE syntax.",
                "error_type": "validation_error",
            }

        if not pgq and not pattern:
            return {
                "success": False,
                "error": "Either pgq or pattern is required",
                "error_type": "validation_error",
            }

        # Ensure extension is loaded
        ext_result = self._ensure_extension_loaded()
        if not ext_result.get("success"):
            return ext_result

        try:
            query_str = pgq

            if pattern and not pgq:
                query_str = self._pattern_to_pgq(pattern, limit)

            # Apply Jinja2 template substitution if params provided
            if params:
                query_str = self._render_template(query_str, params)

            # Add LIMIT if not present
            if query_str and "LIMIT" not in query_str.upper():
                query_str = f"{query_str} LIMIT {limit}"

            with self._lock:
                result = self._conn.execute(query_str)
                columns = (
                    [desc[0] for desc in result.description]
                    if result.description
                    else []
                )
                rows = result.fetchall()

                # Convert to list of dicts
                results = [dict(zip(columns, row)) for row in rows]

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

    def _pattern_to_pgq(self, pattern: Dict[str, Any], limit: int) -> str:
        """Convert a simple pattern dict to SQL/PGQ query."""
        graph = pattern.get("graph", "default_graph")

        if "entity_type" in pattern:
            entity_type = pattern["entity_type"]
            return f"""
                FROM GRAPH_TABLE ({graph}
                  MATCH (e:{entity_type})
                  COLUMNS (e.*)
                )
                LIMIT {limit}
            """

        if "from_entity" in pattern:
            from_id = pattern["from_entity"]
            return f"""
                FROM GRAPH_TABLE ({graph}
                  MATCH (a)-[r]->(b)
                  WHERE a.id = '{from_id}'
                  COLUMNS (b.*, r.*)
                )
                LIMIT {limit}
            """

        # Default: return all nodes
        return f"""
            FROM GRAPH_TABLE ({graph}
              MATCH (e)
              COLUMNS (e.*)
            )
            LIMIT {limit}
        """

    def _render_template(self, template: str, params: Dict[str, Any]) -> str:
        """Render Jinja2 template with parameters."""
        try:
            from jinja2 import Template

            t = Template(template)
            return t.render(state=params, **params)
        except ImportError:
            # Fallback to simple string replacement
            result = template
            for key, value in params.items():
                placeholder = "{{ " + key + " }}"
                result = result.replace(placeholder, str(value))
                placeholder = "{{" + key + "}}"
                result = result.replace(placeholder, str(value))
                # Also support state.key format
                placeholder = "{{ state." + key + " }}"
                result = result.replace(placeholder, str(value))
                placeholder = "{{state." + key + "}}"
                result = result.replace(placeholder, str(value))
            return result

    # =========================================================================
    # GRAPH ALGORITHMS (AC-14, AC-15, AC-16, AC-17)
    # =========================================================================

    def run_algorithm(
        self,
        algorithm: str,
        graph: str,
        table: str,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Run a graph algorithm.

        Args:
            algorithm: Algorithm name (pagerank, weakly_connected_component,
                      local_clustering_coefficient)
            graph: Property graph name
            table: Vertex table name
            **kwargs: Additional algorithm parameters (e.g., limit, iterations)

        Returns:
            {"success": True, "results": list, "algorithm": str}
            or error dict
        """
        # Ensure extension is loaded
        ext_result = self._ensure_extension_loaded()
        if not ext_result.get("success"):
            return ext_result

        algorithm = algorithm.lower()
        limit = kwargs.get("limit", 100)

        try:
            if algorithm == "pagerank":
                sql = f"""
                    SELECT id, pagerank({graph}, {table}) as score
                    FROM {table}
                    ORDER BY score DESC
                    LIMIT {limit}
                """
            elif algorithm in (
                "weakly_connected_component",
                "wcc",
                "connected_components",
            ):
                sql = f"""
                    SELECT id, weakly_connected_component({graph}, {table}) as component
                    FROM {table}
                    LIMIT {limit}
                """
            elif algorithm in ("local_clustering_coefficient", "clustering", "lcc"):
                sql = f"""
                    SELECT id, local_clustering_coefficient({graph}, {table}) as coefficient
                    FROM {table}
                    ORDER BY coefficient DESC
                    LIMIT {limit}
                """
            else:
                return {
                    "success": False,
                    "error": f"Unknown algorithm: {algorithm}. "
                    "Supported: pagerank, weakly_connected_component, "
                    "local_clustering_coefficient",
                    "error_type": "validation_error",
                }

            with self._lock:
                result = self._conn.execute(sql)
                columns = (
                    [desc[0] for desc in result.description]
                    if result.description
                    else []
                )
                rows = result.fetchall()
                results = [dict(zip(columns, row)) for row in rows]

                return {
                    "success": True,
                    "results": results,
                    "count": len(results),
                    "algorithm": algorithm,
                    "query": sql,
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Algorithm execution failed: {str(e)}",
                "error_type": "query_error",
            }

    def shortest_path(
        self,
        graph: str,
        from_id: str,
        to_id: str,
        edge_table: str = "edges",
        vertex_table: str = "vertices",
        max_hops: int = 10,
    ) -> Dict[str, Any]:
        """
        Find shortest path between two entities.

        Args:
            graph: Property graph name
            from_id: Source entity ID
            to_id: Target entity ID
            edge_table: Edge table name/label
            vertex_table: Vertex table name/label
            max_hops: Maximum path length

        Returns:
            {"success": True, "path": list, "hops": int}
            or error dict
        """
        # Ensure extension is loaded
        ext_result = self._ensure_extension_loaded()
        if not ext_result.get("success"):
            return ext_result

        try:
            sql = f"""
                FROM GRAPH_TABLE ({graph}
                  MATCH path = ANY SHORTEST
                    (a:{vertex_table} WHERE a.id = '{from_id}')
                    -[r:{edge_table}]->{{1,{max_hops}}}
                    (b:{vertex_table} WHERE b.id = '{to_id}')
                  COLUMNS (path_length(path) as hops, vertices(path) as nodes)
                )
            """

            with self._lock:
                result = self._conn.execute(sql)
                rows = result.fetchall()

                if not rows:
                    return {
                        "success": True,
                        "path": [],
                        "hops": -1,
                        "message": "No path found",
                    }

                row = rows[0]
                return {
                    "success": True,
                    "hops": row[0],
                    "path": row[1] if len(row) > 1 else [],
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Shortest path query failed: {str(e)}",
                "error_type": "query_error",
            }

    # =========================================================================
    # GRAPHBACKEND PROTOCOL METHODS
    # =========================================================================

    def store_entity(
        self,
        entity_id: str,
        entity_type: str,
        properties: Optional[Dict[str, Any]] = None,
        embedding: Optional[List[float]] = None,
    ) -> Dict[str, Any]:
        """
        Store an entity (node) in the default entities table.

        Note: For SQL/PGQ workflows, prefer creating property graphs from
        Parquet files. This method provides compatibility with GraphBackend protocol.
        """
        if not entity_id or not entity_type:
            return {
                "success": False,
                "error": "entity_id and entity_type are required",
                "error_type": "validation_error",
            }

        try:
            props = properties or {}
            if embedding:
                props["_embedding"] = embedding
            props_json = json.dumps(props)

            with self._lock:
                # Ensure entities table exists
                self._conn.execute(
                    """
                    CREATE TABLE IF NOT EXISTS entities (
                        id VARCHAR PRIMARY KEY,
                        type VARCHAR,
                        properties JSON,
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                    );
                """
                )

                # Upsert entity
                self._conn.execute(
                    """
                    INSERT OR REPLACE INTO entities (id, type, properties)
                    VALUES (?, ?, ?);
                    """,
                    [entity_id, entity_type, props_json],
                )

                return {
                    "success": True,
                    "entity_id": entity_id,
                    "type": entity_type,
                    "created": True,
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
        """
        Store a relation (edge) in the default relations table.

        Note: For SQL/PGQ workflows, prefer creating property graphs from
        Parquet files. This method provides compatibility with GraphBackend protocol.
        """
        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error",
            }

        try:
            props_json = json.dumps(properties) if properties else "{}"

            with self._lock:
                # Ensure relations table exists
                self._conn.execute(
                    """
                    CREATE TABLE IF NOT EXISTS relations (
                        from_id VARCHAR,
                        to_id VARCHAR,
                        type VARCHAR,
                        properties JSON,
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        PRIMARY KEY (from_id, to_id, type)
                    );
                """
                )

                # Upsert relation
                self._conn.execute(
                    """
                    INSERT OR REPLACE INTO relations (from_id, to_id, type, properties)
                    VALUES (?, ?, ?, ?);
                    """,
                    [from_entity, to_entity, relation_type, props_json],
                )

                return {
                    "success": True,
                    "from": from_entity,
                    "to": to_entity,
                    "type": relation_type,
                    "created": True,
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store relation: {str(e)}",
                "error_type": "query_error",
            }

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

        Note: Vector search not natively supported. Use entity_id for graph traversal.
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

            with self._lock:
                if entity_id:
                    # Get the entity
                    result = self._conn.execute(
                        "SELECT id, type, properties FROM entities WHERE id = ?",
                        [entity_id],
                    )
                    for row in result.fetchall():
                        entities.append(
                            {
                                "id": row[0],
                                "type": row[1],
                                "properties": json.loads(row[2]) if row[2] else {},
                            }
                        )

                    # Get connected entities within N hops
                    for hop in range(1, hops + 1):
                        # This is a simplified traversal; full implementation would use
                        # recursive CTEs or property graph traversal
                        result = self._conn.execute(
                            """
                            SELECT DISTINCT e.id, e.type, e.properties
                            FROM entities e
                            JOIN relations r ON (e.id = r.to_id OR e.id = r.from_id)
                            WHERE (r.from_id = ? OR r.to_id = ?)
                            AND e.id != ?
                            LIMIT ?
                            """,
                            [entity_id, entity_id, entity_id, limit],
                        )
                        for row in result.fetchall():
                            if row[0] not in [e["id"] for e in entities]:
                                entities.append(
                                    {
                                        "id": row[0],
                                        "type": row[1],
                                        "properties": (
                                            json.loads(row[2]) if row[2] else {}
                                        ),
                                    }
                                )

                    # Get relations between found entities
                    entity_ids = [e["id"] for e in entities]
                    if entity_ids:
                        placeholders = ",".join(["?" for _ in entity_ids])
                        result = self._conn.execute(
                            f"""
                            SELECT from_id, to_id, type, properties
                            FROM relations
                            WHERE from_id IN ({placeholders})
                            AND to_id IN ({placeholders})
                            """,
                            entity_ids + entity_ids,
                        )
                        for row in result.fetchall():
                            relations.append(
                                {
                                    "from": row[0],
                                    "to": row[1],
                                    "type": row[2],
                                    "properties": json.loads(row[3]) if row[3] else {},
                                }
                            )

                elif embedding:
                    return {
                        "success": True,
                        "entities": [],
                        "relations": [],
                        "context_summary": "Vector search not supported in DuckPGQBackend. "
                        "Use entity_id for graph traversal.",
                    }

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

    def _build_context_summary(
        self, entities: List[Dict], relations: List[Dict]
    ) -> str:
        """Build a text summary of the context."""
        if not entities:
            return "No relevant entities found."

        type_counts: Dict[str, int] = {}
        for e in entities:
            t = e.get("type", "Unknown")
            type_counts[t] = type_counts.get(t, 0) + 1

        entity_summary = ", ".join([f"{count} {t}" for t, count in type_counts.items()])
        parts = [f"Found {len(entities)} entities: {entity_summary}."]

        if relations:
            rel_types = set(r.get("type", "Unknown") for r in relations)
            parts.append(
                f"Connected by {len(relations)} relations ({', '.join(rel_types)})."
            )

        return " ".join(parts)

    # =========================================================================
    # EXTENSION STATUS
    # =========================================================================

    def is_extension_loaded(self) -> bool:
        """Check if DuckPGQ extension is loaded."""
        return self._extension_loaded

    def get_status(self) -> Dict[str, Any]:
        """Get backend status information."""
        return {
            "success": True,
            "backend": "duckpgq",
            "extension_loaded": self._extension_loaded,
            "httpfs_loaded": self._httpfs_loaded,
            "property_graphs": list(self._property_graphs.keys()),
            "closed": self._closed,
        }

    # =========================================================================
    # LIFECYCLE
    # =========================================================================

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

        self._property_graphs.clear()
        logger.debug("DuckPGQBackend closed")

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass


__all__ = ["DuckPGQBackend"]
