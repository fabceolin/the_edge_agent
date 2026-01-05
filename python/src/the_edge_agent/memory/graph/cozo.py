"""
CozoDB Graph Database Backend.

Provides graph database functionality with Datalog queries and
optional HNSW vector search for semantic retrieval. Uses SQLite
as the storage engine for maximum compatibility.

Requires: pip install "pycozo[embedded]"

Example:
    >>> from the_edge_agent.memory.graph import CozoBackend
    >>>
    >>> backend = CozoBackend("./agent_graph.db")
    >>> result = backend.store_entity("person_1", "Person", {"name": "Alice"})
    >>> print(result['success'])  # True
    >>> result = backend.store_relation("person_1", "person_2", "KNOWS")
    >>> print(result['success'])  # True
    >>> result = backend.query(datalog="?[id, type] := entity[id, type, _, _, _]")
    >>> print(result['results'])  # [{'id': 'person_1', 'type': 'Person'}, ...]
    >>> backend.close()
"""

import json
import threading
from typing import Any, Dict, List, Optional

from .protocol import COZO_AVAILABLE


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


__all__ = ["CozoBackend"]
