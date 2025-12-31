# Epic TEA-BUILTIN-001.7: Neo4j Remote Graph Backend

## Status

**Ready for Development**

> All substories have passed QA validation with comprehensive test designs. Development should proceed in order: 001.7.1 → 001.7.2 → (001.7.3, 001.7.4, 001.7.5 can parallelize).

## Substories

| Substory | Status | Test Scenarios | Description |
|----------|--------|----------------|-------------|
| [TEA-BUILTIN-001.7.1](TEA-BUILTIN-001.7.1-neo4j-core-connection.md) | ✅ Ready | 67 (P0:24) | Core Connection & Authentication |
| [TEA-BUILTIN-001.7.2](TEA-BUILTIN-001.7.2-neo4j-extended-crud.md) | ✅ Ready | 52 (P0:18) | Extended CRUD Operations |
| [TEA-BUILTIN-001.7.3](TEA-BUILTIN-001.7.3-neo4j-vector-index.md) | ✅ Ready | 55 (P0:18) | Vector Index Integration |
| [TEA-BUILTIN-001.7.4](TEA-BUILTIN-001.7.4-neo4j-gds.md) | ✅ Ready (Optional) | 78 (P0:26) | Graph Data Science Integration |
| [TEA-BUILTIN-001.7.5](TEA-BUILTIN-001.7.5-neo4j-triggers.md) | ✅ Ready (Optional) | 47 (P0:18) | APOC Triggers Support |

**Total Test Coverage:** 299 scenarios (P0: 104 critical path tests)

## Epic Goal

Provide a production-ready Neo4j remote graph backend for The Edge Agent, enabling agents to leverage enterprise graph database capabilities including CRUD operations, vector similarity search, Graph Data Science algorithms, and event-driven triggers for building sophisticated knowledge graphs and AI-powered applications.

## Story

**As a** YAML agent developer building knowledge-intensive applications,
**I want** a Neo4j remote graph backend with full CRUD operations, vector search, GDS algorithms, and event triggers,
**so that** I can leverage enterprise-grade graph database capabilities for complex entity relationships, semantic search, graph analytics, and reactive agent behaviors without managing local database infrastructure.

## Epic Context

### Existing System Integration

- **Integrates with**: `GraphBackend` protocol (TEA-BUILTIN-001.4)
- **Technology**: Neo4j 5.4+ with official Python driver, GDS Enterprise, APOC
- **Follows pattern**: Existing `KuzuBackend` (Cypher) and `CozoBackend` (Datalog)
- **Touch points**: `memory/graph.py`, `memory/__init__.py`, `actions/graph_actions.py`

### Technology Stack

| Component | Technology | Version |
|-----------|------------|---------|
| Database | Neo4j Enterprise/Community | 5.4+ |
| Driver | neo4j (official) | 5.x |
| Vector Index | Neo4j Native Vector Index | 5.11+ |
| Graph Algorithms | Neo4j GDS | 2.x |
| Triggers | APOC | 5.x |

### Connection Protocols

All Neo4j connection schemes must be supported:

| Scheme | Description | Port |
|--------|-------------|------|
| `bolt://` | Unencrypted Bolt | 7687 |
| `bolt+s://` | Bolt with TLS (verify certificate) | 7687 |
| `bolt+ssc://` | Bolt with TLS (self-signed, skip verify) | 7687 |
| `neo4j://` | Neo4j URI scheme (routing for clusters) | 7687 |
| `neo4j+s://` | Neo4j with TLS (verify certificate) | 7687 |
| `neo4j+ssc://` | Neo4j with TLS (self-signed) | 7687 |

### Authentication Methods

| Method | Configuration | Use Case |
|--------|--------------|----------|
| Basic Auth | `username` + `password` | Standard deployments |
| Bearer Token | `bearer_token` | OAuth/JWT, cloud-managed |

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                      NEO4J REMOTE GRAPH ARCHITECTURE                        │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                         Neo4jBackend                                 │   │
│  │                    implements GraphBackend                           │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                    │                                        │
│          ┌─────────────────────────┼─────────────────────────┐             │
│          │                         │                         │             │
│          ▼                         ▼                         ▼             │
│  ┌───────────────┐        ┌───────────────┐        ┌───────────────┐      │
│  │  Connection   │        │   Extended    │        │   Advanced    │      │
│  │   & Auth      │        │    CRUD       │        │   Features    │      │
│  │               │        │               │        │               │      │
│  │ - bolt://     │        │ - delete_*    │        │ - Vector Idx  │      │
│  │ - neo4j://    │        │ - update_*    │        │ - GDS Algos   │      │
│  │ - TLS (+s)    │        │ - batch_*     │        │ - Triggers    │      │
│  │ - Basic Auth  │        │ - transactions│        │               │      │
│  │ - Bearer      │        │               │        │               │      │
│  └───────────────┘        └───────────────┘        └───────────────┘      │
│                                                                             │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                        Neo4j Server 5.4+                             │   │
│  │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐   │   │
│  │  │  Core   │  │ Vector  │  │   GDS   │  │  APOC   │  │ Cluster │   │   │
│  │  │  Graph  │  │  Index  │  │ Library │  │Triggers │  │ Routing │   │   │
│  │  └─────────┘  └─────────┘  └─────────┘  └─────────┘  └─────────┘   │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Acceptance Criteria (Epic Level)

### Core Functionality (001.7.1)

1. **AC-1**: `Neo4jBackend` implements `GraphBackend` protocol
2. **AC-2**: Supports all connection schemes (bolt, neo4j, +s, +ssc)
3. **AC-3**: Supports Basic Auth (username/password)
4. **AC-4**: Supports Bearer Token authentication
5. **AC-5**: Connection pooling with configurable pool size
6. **AC-6**: Graceful connection handling and reconnection
7. **AC-7**: `store_entity` persists nodes to Neo4j
8. **AC-8**: `store_relation` creates relationships between nodes
9. **AC-9**: `query` executes Cypher queries with parameters
10. **AC-10**: `retrieve_context` performs N-hop graph traversal

### Extended CRUD (001.7.2)

11. **AC-11**: `delete_entity` removes nodes and their relationships
12. **AC-12**: `delete_relation` removes specific relationships
13. **AC-13**: `update_entity_properties` modifies node properties
14. **AC-14**: `update_relation_properties` modifies relationship properties
15. **AC-15**: `store_entities_batch` bulk inserts multiple nodes
16. **AC-16**: `store_relations_batch` bulk creates relationships
17. **AC-17**: Transaction support with explicit begin/commit/rollback
18. **AC-18**: Merge operations (upsert semantics)

### Vector Index (001.7.3)

19. **AC-19**: Detect Neo4j Vector Index availability (5.11+)
20. **AC-20**: `create_vector_index` on entity properties
21. **AC-21**: `store_entity` with `embedding` parameter stores vectors
22. **AC-22**: `vector_search` performs similarity search
23. **AC-23**: `retrieve_context` supports embedding-based retrieval
24. **AC-24**: Configurable similarity metrics (cosine, euclidean)
25. **AC-25**: Configurable embedding dimensions (default 1536)

### Graph Data Science (001.7.4)

26. **AC-26**: Detect GDS library availability
27. **AC-27**: `gds.project_graph` creates in-memory graph projection
28. **AC-28**: `gds.centrality` runs centrality algorithms (PageRank, Betweenness)
29. **AC-29**: `gds.community` runs community detection (Louvain, Label Propagation)
30. **AC-30**: `gds.pathfinding` runs path algorithms (Dijkstra, A*)
31. **AC-31**: `gds.similarity` runs node similarity algorithms
32. **AC-32**: `gds.drop_graph` cleans up projections

### APOC Triggers (001.7.5)

33. **AC-33**: Detect APOC availability
34. **AC-34**: `register_trigger` creates database triggers
35. **AC-35**: `unregister_trigger` removes triggers
36. **AC-36**: `list_triggers` shows active triggers
37. **AC-37**: Trigger callbacks via webhook or state update
38. **AC-38**: Automatic trigger cleanup on backend close

### Integration & Quality

39. **AC-39**: Registered as `neo4j` in graph backend factory
40. **AC-40**: YAML configuration support in `settings.graph`
41. **AC-41**: Environment variable expansion for credentials
42. **AC-42**: Graceful degradation when Neo4j unavailable
43. **AC-43**: Comprehensive error handling with typed errors
44. **AC-44**: Thread-safe operations with connection pooling
45. **AC-45**: Unit tests for all operations (mocked driver)
46. **AC-46**: Integration tests with Neo4j testcontainers
47. **AC-47**: Documentation in YAML_REFERENCE.md

## Dependencies

### Blocked By

- TEA-BUILTIN-001.4 (Long-Term Memory) - establishes GraphBackend protocol ✅ DONE

### Blocks

- Future multi-database agent architectures
- Knowledge graph reasoning agents with GDS
- Event-driven reactive agents

### External Dependencies

- `neo4j` Python driver >= 5.0
- Neo4j Server 5.4+ (5.11+ for Vector Index)
- Neo4j GDS 2.x (Enterprise only, optional)
- APOC 5.x (optional)

## User Prerequisites

- [ ] **Required**: Neo4j Server 5.4+ accessible
- [ ] **Required**: Install driver: `pip install neo4j>=5.0`
- [ ] **Optional for Vector**: Neo4j 5.11+ with Vector Index enabled
- [ ] **Optional for GDS**: Neo4j Enterprise + GDS plugin
- [ ] **Optional for Triggers**: APOC plugin installed

## Local Development Setup

A Docker Compose configuration is provided for local testing:

```bash
# Start Neo4j Enterprise (with APOC, supports GDS)
docker compose --profile neo4j up -d

# Or start Neo4j Community (lighter, no GDS support)
docker compose --profile neo4j-community up -d

# Check health
docker compose ps

# View logs
docker compose logs -f neo4j_enterprise
```

**Environment Variables** (see `.env.example`):

```bash
export NEO4J_URI="bolt://localhost:7687"
export NEO4J_USERNAME="neo4j"
export NEO4J_PASSWORD="tea-test-password"
```

**Run Integration Tests**:

```bash
cd python
pytest tests/test_neo4j_backend.py -v
```

## YAML Configuration Example

```yaml
settings:
  graph:
    backend: neo4j
    uri: "${NEO4J_URI:-bolt://localhost:7687}"
    database: "${NEO4J_DATABASE:-neo4j}"
    auth:
      type: basic  # or "bearer"
      username: "${NEO4J_USERNAME}"
      password: "${NEO4J_PASSWORD}"
      # For bearer auth:
      # type: bearer
      # token: "${NEO4J_BEARER_TOKEN}"
    pool_size: 50
    max_connection_lifetime: 3600
    connection_timeout: 30

    # Vector Index configuration (optional)
    vector:
      enabled: true
      dimension: 1536
      similarity: cosine  # cosine, euclidean
      index_name: entity_embeddings

    # GDS configuration (optional)
    gds:
      enabled: true
      memory_estimation: true

    # Triggers configuration (optional)
    triggers:
      enabled: true
      cleanup_on_close: true
```

## Risk Assessment

### Primary Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| Network latency vs local backends | Performance | Connection pooling, query optimization, batching |
| Neo4j version incompatibility | Functionality | Version detection, graceful feature degradation |
| GDS Enterprise-only features | Scope | Clear documentation, feature flags |
| APOC plugin availability | Functionality | Optional dependency, graceful fallback |
| Credentials security | Security | Env vars, no logging of secrets |

### Rollback Plan

1. Neo4j backend is additive - existing backends unaffected
2. Factory registration allows easy enable/disable
3. No schema changes to existing memory infrastructure
4. Feature flags for GDS/APOC allow partial rollback

## Compatibility Requirements

- [x] Existing `GraphBackend` protocol unchanged
- [x] Existing actions (`graph.store_entity`, etc.) work transparently
- [x] CozoBackend and KuzuBackend continue to function
- [x] YAML configuration is backwards compatible
- [x] No breaking changes to public API

## Definition of Done

- [ ] All 5 substories completed with acceptance criteria met
- [ ] All existing graph tests pass
- [ ] Neo4j-specific unit tests pass (mocked)
- [ ] Integration tests pass with testcontainers
- [ ] Documentation updated (YAML_REFERENCE.md, actions-reference.md)
- [ ] Example YAML agents demonstrate Neo4j usage
- [ ] Performance benchmarks documented
- [ ] Security review completed (credential handling)

## Story Manager Handoff

Please develop detailed user stories for this brownfield epic. Key considerations:

- This is an extension to the existing `GraphBackend` protocol in `memory/graph.py`
- Integration points: `graph.py`, `__init__.py`, `graph_actions.py`, `yaml_engine.py`
- Existing patterns to follow: `KuzuBackend` class structure and error handling
- Critical compatibility requirements: Must implement `GraphBackend` protocol exactly
- Each story must include verification that existing functionality remains intact
- Feature detection pattern for optional capabilities (GDS, APOC, Vector Index)

The epic should maintain system integrity while delivering enterprise Neo4j graph capabilities.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-30 | 0.3 | Status updated to Ready for Development; added Docker Compose setup; marked GDS/Triggers as optional | PO (Sarah) |
| 2025-12-30 | 0.2 | All substories QA validated with test designs (299 total scenarios) | Quinn (QA) |
| 2024-12-30 | 0.1 | Initial epic creation | PO (Sarah) |
