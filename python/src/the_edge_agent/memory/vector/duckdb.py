"""
DuckDBVSSIndex Implementation (TEA-BUILTIN-006).

Implements VectorIndex ABC using DuckDB's VSS extension.
Migrated from firebase/functions-agents/actions/vector_search.py.

Requirements:
    pip install duckdb

Features:
    - Cosine similarity search using array_cosine_similarity
    - Parquet file loading (local and remote via httpfs)
    - Metadata filtering
    - In-memory index for fast queries

Usage:
    >>> from the_edge_agent.memory.vector import DuckDBVSSIndex
    >>>
    >>> index = DuckDBVSSIndex()
    >>> index.load_data("path/to/embeddings.parquet")
    >>> results = index.search([0.1, 0.2, ...], top_k=10)
"""

import os
import logging
import tempfile
import threading
from typing import Any, Dict, List, Optional

from .base import VectorIndex, VectorSearchConfig

try:
    import duckdb
    DUCKDB_AVAILABLE = True
except ImportError:
    DUCKDB_AVAILABLE = False
    duckdb = None  # type: ignore


logger = logging.getLogger(__name__)


# =============================================================================
# DEFAULT CONFIGURATION
# =============================================================================

DEFAULT_EMBEDDING_DIMENSIONS = 1536
DEFAULT_TOP_K = 10
DEFAULT_THRESHOLD = 0.0
MAX_TOP_K = 100


class DuckDBVSSIndex(VectorIndex):
    """
    DuckDB VSS implementation of VectorIndex.

    Uses DuckDB's VSS extension for fast vector similarity search.
    Supports both local and remote Parquet files.

    Attributes:
        dimensions: Expected embedding dimensions
        _conn: DuckDB connection
        _data_loaded: Whether data has been loaded
        _index_built: Whether VSS index is built
        _table_name: Name of the data table
    """

    def __init__(
        self,
        dimensions: int = DEFAULT_EMBEDDING_DIMENSIONS,
        database: str = ":memory:",
        enable_httpfs: bool = True
    ):
        """
        Initialize DuckDB VSS index.

        Args:
            dimensions: Expected embedding dimensions
            database: DuckDB database path (":memory:" for in-memory)
            enable_httpfs: Whether to load httpfs extension

        Raises:
            ImportError: If duckdb is not installed
        """
        if not DUCKDB_AVAILABLE:
            raise ImportError(
                "duckdb is required for DuckDBVSSIndex. "
                "Install with: pip install duckdb"
            )

        self.dimensions = dimensions
        self._database = database
        self._enable_httpfs = enable_httpfs
        self._conn = None
        self._lock = threading.Lock()
        self._data_loaded = False
        self._index_built = False
        self._table_name = "vector_data"
        self._vector_count = 0
        self._metric = "cosine"

        # Column mappings
        self._embedding_column = "embedding"
        self._content_column = "content"
        self._id_column = "id"

    def _get_connection(self) -> "duckdb.DuckDBPyConnection":
        """Get or create DuckDB connection."""
        if self._conn is None:
            self._conn = duckdb.connect(self._database)
            self._initialize_extensions()
        return self._conn

    def _initialize_extensions(self) -> None:
        """Initialize DuckDB extensions."""
        conn = self._conn

        # Install and load VSS extension
        try:
            conn.execute("INSTALL vss; LOAD vss;")
            logger.debug("VSS extension loaded")
        except Exception as e:
            logger.warning(f"VSS install: {e}")
            try:
                conn.execute("LOAD vss;")
            except Exception:
                logger.warning("VSS extension not available")

        # Install and load httpfs extension if enabled
        if self._enable_httpfs:
            try:
                conn.execute("INSTALL httpfs; LOAD httpfs;")
                self._configure_httpfs()
                logger.debug("httpfs extension loaded")
            except Exception as e:
                logger.warning(f"httpfs not available: {e}")

    def _configure_httpfs(self) -> None:
        """Configure httpfs for cloud storage access."""
        conn = self._conn

        # Configure GCS access via HMAC keys
        gcs_key = os.environ.get("GCS_ACCESS_KEY_ID")
        gcs_secret = os.environ.get("GCS_SECRET_ACCESS_KEY")

        if gcs_key and gcs_secret:
            try:
                conn.execute(f"SET s3_access_key_id='{gcs_key}'")
                conn.execute(f"SET s3_secret_access_key='{gcs_secret}'")
                conn.execute("SET s3_endpoint='storage.googleapis.com'")
                conn.execute("SET s3_url_style='path'")
                logger.debug("GCS httpfs configured")
            except Exception as e:
                logger.warning(f"GCS httpfs config failed: {e}")

        # Configure S3 access if AWS credentials exist
        aws_key = os.environ.get("AWS_ACCESS_KEY_ID")
        aws_secret = os.environ.get("AWS_SECRET_ACCESS_KEY")
        aws_region = os.environ.get("AWS_REGION", "us-east-1")

        if aws_key and aws_secret and not gcs_key:
            try:
                conn.execute(f"SET s3_access_key_id='{aws_key}'")
                conn.execute(f"SET s3_secret_access_key='{aws_secret}'")
                conn.execute(f"SET s3_region='{aws_region}'")
                logger.debug("AWS S3 httpfs configured")
            except Exception as e:
                logger.warning(f"AWS S3 httpfs config failed: {e}")

    # =========================================================================
    # SEARCH OPERATIONS
    # =========================================================================

    def search(
        self,
        embedding: List[float],
        top_k: int = DEFAULT_TOP_K,
        threshold: float = DEFAULT_THRESHOLD,
        filters: Optional[Dict[str, Any]] = None,
        config: Optional[VectorSearchConfig] = None
    ) -> Dict[str, Any]:
        """Search for similar vectors using cosine similarity."""
        # Validate embedding
        validation = self.validate_embedding(embedding)
        if not validation.get("valid"):
            return {
                "success": False,
                "error": validation.get("error", "Invalid embedding"),
                "error_type": "invalid_input"
            }

        # Check data loaded
        if not self._data_loaded:
            return {
                "success": False,
                "error": "No data loaded. Call load_data() first.",
                "error_type": "index_not_built"
            }

        # Apply config
        if config:
            top_k = config.top_k
            threshold = config.threshold

        # Clamp values
        top_k = min(max(1, top_k), MAX_TOP_K)
        threshold = max(0.0, min(1.0, threshold))

        try:
            with self._lock:
                conn = self._get_connection()
                results = self._execute_search(
                    conn=conn,
                    embedding=embedding,
                    top_k=top_k,
                    threshold=threshold,
                    filters=filters
                )

            return {
                "success": True,
                "results": results,
                "count": len(results),
                "top_k": top_k,
                "threshold": threshold
            }

        except Exception as e:
            logger.error(f"Vector search failed: {e}")
            return {
                "success": False,
                "error": str(e),
                "error_type": "search_failed"
            }

    def _execute_search(
        self,
        conn: "duckdb.DuckDBPyConnection",
        embedding: List[float],
        top_k: int,
        threshold: float,
        filters: Optional[Dict[str, Any]] = None
    ) -> List[Dict[str, Any]]:
        """Execute vector similarity search query."""
        # Build filter conditions
        conditions = [f"{self._embedding_column} IS NOT NULL"]

        if filters:
            for key, value in filters.items():
                if isinstance(value, str):
                    conditions.append(f"{key} = '{value}'")
                elif isinstance(value, (int, float)):
                    conditions.append(f"{key} = {value}")
                elif isinstance(value, bool):
                    conditions.append(f"{key} = {str(value).lower()}")
                elif isinstance(value, list):
                    # IN clause for lists
                    values_str = ", ".join(
                        f"'{v}'" if isinstance(v, str) else str(v)
                        for v in value
                    )
                    conditions.append(f"{key} IN ({values_str})")

        where_clause = " AND ".join(conditions)

        # Build the query
        sql = f"""
            WITH ranked AS (
                SELECT
                    {self._id_column} as id,
                    {self._content_column} as content,
                    * EXCLUDE ({self._id_column}, {self._content_column}, {self._embedding_column}),
                    array_cosine_similarity(
                        {self._embedding_column},
                        ?::FLOAT[{self.dimensions}]
                    ) as score
                FROM {self._table_name}
                WHERE {where_clause}
            )
            SELECT *
            FROM ranked
            WHERE score >= {threshold}
            ORDER BY score DESC
            LIMIT {top_k}
        """

        result = conn.execute(sql, [embedding])
        columns = [desc[0] for desc in result.description] if result.description else []
        rows = result.fetchall()

        # Convert to list of dicts
        results = []
        for row in rows:
            record = dict(zip(columns, row))

            # Extract core fields
            search_result = {
                "id": record.pop("id", ""),
                "score": record.pop("score", 0.0),
                "content": record.pop("content", ""),
                "metadata": record  # Remaining fields are metadata
            }
            results.append(search_result)

        return results

    # =========================================================================
    # INDEX MANAGEMENT
    # =========================================================================

    def load_data(
        self,
        source: str,
        embedding_column: str = "embedding",
        content_column: str = "content",
        id_column: str = "id"
    ) -> Dict[str, Any]:
        """Load data from Parquet file."""
        self._embedding_column = embedding_column
        self._content_column = content_column
        self._id_column = id_column

        try:
            with self._lock:
                conn = self._get_connection()

                # Try direct load first (works for local files and httpfs)
                try:
                    conn.execute(f"""
                        CREATE OR REPLACE TABLE {self._table_name} AS
                        SELECT * FROM read_parquet('{source}')
                    """)
                except Exception as e:
                    # If httpfs fails, try downloading first
                    logger.warning(f"Direct load failed, trying download: {e}")
                    local_path = self._download_file(source)
                    if local_path:
                        conn.execute(f"""
                            CREATE OR REPLACE TABLE {self._table_name} AS
                            SELECT * FROM read_parquet('{local_path}')
                        """)
                        # Cleanup temp file
                        try:
                            os.unlink(local_path)
                        except Exception:
                            pass
                    else:
                        raise

                # Get row count
                result = conn.execute(
                    f"SELECT COUNT(*) FROM {self._table_name}"
                )
                self._vector_count = result.fetchone()[0]
                self._data_loaded = True

                logger.info(f"Loaded {self._vector_count} vectors from {source}")

                return {
                    "success": True,
                    "loaded": self._vector_count,
                    "source": source
                }

        except Exception as e:
            logger.error(f"Failed to load data from {source}: {e}")
            return {
                "success": False,
                "error": str(e),
                "error_type": "parquet_not_found"
            }

    def _download_file(self, uri: str) -> Optional[str]:
        """
        Download file from cloud storage to temp location.

        Supports gs:// URIs via google-cloud-storage.
        """
        if not uri.startswith("gs://"):
            return None

        try:
            from google.cloud import storage

            # Parse URI
            parts = uri.replace("gs://", "").split("/", 1)
            if len(parts) != 2:
                return None

            bucket_name, blob_path = parts

            # Download
            client = storage.Client()
            bucket = client.bucket(bucket_name)
            blob = bucket.blob(blob_path)

            with tempfile.NamedTemporaryFile(suffix='.parquet', delete=False) as tmp:
                blob.download_to_filename(tmp.name)
                logger.debug(f"Downloaded {uri} to {tmp.name}")
                return tmp.name

        except ImportError:
            logger.warning("google-cloud-storage not installed for gs:// URIs")
            return None
        except Exception as e:
            logger.error(f"Failed to download {uri}: {e}")
            return None

    def build_index(
        self,
        metric: str = "cosine"
    ) -> Dict[str, Any]:
        """
        Build HNSW index for faster search.

        Note: DuckDB VSS uses array_cosine_similarity directly.
        This method creates an HNSW index for better performance.
        """
        if not self._data_loaded:
            return {
                "success": False,
                "error": "No data loaded. Call load_data() first.",
                "error_type": "index_not_built"
            }

        self._metric = metric

        try:
            with self._lock:
                conn = self._get_connection()

                # Create HNSW index if available
                try:
                    conn.execute(f"""
                        CREATE INDEX IF NOT EXISTS vector_idx
                        ON {self._table_name}
                        USING HNSW ({self._embedding_column})
                        WITH (metric = '{metric}')
                    """)
                    self._index_built = True
                    logger.info(f"Built HNSW index with {metric} metric")
                except Exception as e:
                    # HNSW index creation is optional
                    logger.warning(f"HNSW index not created: {e}")
                    self._index_built = False

            return {
                "success": True,
                "indexed": self._vector_count,
                "metric": metric
            }

        except Exception as e:
            logger.error(f"Failed to build index: {e}")
            return {
                "success": False,
                "error": str(e),
                "error_type": "search_failed"
            }

    # =========================================================================
    # CONFIGURATION
    # =========================================================================

    def get_dimensions(self) -> int:
        """Get expected embedding dimensions."""
        return self.dimensions

    def get_stats(self) -> Dict[str, Any]:
        """Get index statistics."""
        return {
            "success": True,
            "vector_count": self._vector_count,
            "dimensions": self.dimensions,
            "metric": self._metric,
            "index_built": self._index_built,
            "data_loaded": self._data_loaded,
            "table_name": self._table_name
        }

    # =========================================================================
    # LIFECYCLE
    # =========================================================================

    def close(self) -> None:
        """Close DuckDB connection."""
        with self._lock:
            if self._conn is not None:
                try:
                    self._conn.close()
                except Exception:
                    pass
                self._conn = None
                self._data_loaded = False
                self._index_built = False
        logger.debug("DuckDBVSSIndex closed")
