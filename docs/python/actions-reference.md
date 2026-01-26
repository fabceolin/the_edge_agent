# Python Actions Reference

> **Auto-generated summary** | Last updated: 2026-01-25 | Git: `8eb000e`
>
> **Total modules:** 51 | **Total actions:** 276

This document lists all built-in actions available in the Python implementation of The Edge Agent.

For complete action documentation including parameters and examples, see the [YAML Reference](../shared/YAML_REFERENCE.md#built-in-actions).

## Quick Reference

| Module | Actions | Description |
|--------|---------|-------------|
| [cache.*](#cache) | 4 | Caching/memoization with LTM backend |
| [core.*](#core) | 7 | HTTP, file operations, notifications, checkpoints |
| [data.*](#data) | 16 | JSON/CSV parsing, transformation, validation |
| [llm.*](#llm) | 4 | LLM calls, streaming, retry, and tool calling |
| [ltm.*](#ltm) | 4 | Long-term persistent key-value storage with FTS5 |
| [memory.*](#memory) | 3 | Key-value storage with TTL |
| [a2a.*](#a2a) | 10 | Inter-agent communication (send, receive, broadcast, delegate) |
| [agent.*](#agent) | 5 | Multi-agent collaboration (dispatch, parallel, sequential) |
| [firestore.*](#firestore) | 5 | Firestore CRUD operations |
| [graph.*](#graph) | 25 | Graph database with Datalog and HNSW vectors |
| [neo4j_gds.*](#neo4j-gds) | 18 | Neo4j GDS graph analytics algorithms |
| [neo4j_trigger.*](#neo4j-trigger) | 11 | Neo4j APOC trigger management |
| [error.*](#error) | 7 | Error handling actions (is_retryable, clear, retry) |
| [planning.*](#planning) | 4 | Planning/decomposition primitives |
| [ratelimit.*](#ratelimit) | 1 | Rate limiting with shared named limiters |
| [reasoning.*](#reasoning) | 7 | Reasoning techniques (CoT, ReAct, self-correct, decompose) |
| [reflection.*](#reflection) | 3 | Self-reflection loop primitive |
| [retry.*](#retry) | 1 | General-purpose retry loop with correction |
| [validation.*](#validation) | 2 | Generic extraction validation with Prolog/probes |
| [academic.*](#academic) | 3 | Academic research via PubMed, ArXiv, CrossRef APIs |
| [auth.*](#auth) | 2 | Authentication verification (verify, get_user) |
| [bmad.*](#bmad) | 2 | BMad story task extraction |
| [catalog.*](#catalog) | 10 | Data catalog for tables, files, and snapshots |
| [cloud_memory.*](#cloud-memory) | 5 | Cloud storage with metadata management |
| [code.*](#code) | 2 | Sandboxed Python code execution |
| [context.*](#context) | 1 | Context assembly with relevance ranking |
| [data_tabular.*](#data-tabular) | 6 | Tabular data operations |
| [dspy.*](#dspy) | 7 | DSPy prompt optimization (cot, react, compile) |
| [git.*](#git) | 6 | Git worktree actions (execution modes) |
| [github.*](#github) | 4 | GitHub Issues integration |
| [http_response.*](#http-response) | 1 | HTTP response for early termination |
| [input_validation.*](#input-validation) | 2 | Input schema validation |
| [llamaextract.*](#llamaextract) | 8 | Document extraction via LlamaExtract |
| [llamaindex.*](#llamaindex) | 6 | LlamaIndex RAG bridge (query, router, subquestion) |
| [llm_local.*](#llm-local) | 6 | Local LLM inference via llama-cpp-python |
| [markdown.*](#markdown) | 2 | Markdown parsing with sections, variables, checklists |
| [mem0.*](#mem0) | 7 | Mem0 universal memory integration |
| [observability.*](#observability) | 7 | Tracing spans and event logging |
| [rag.*](#rag) | 4 | Embedding creation, vector storage, semantic search |
| [schema.*](#schema) | 1 | Schema merge and manipulation |
| [search.*](#search) | 3 | SQL and full-text search via QueryEngine |
| [secrets.*](#secrets) | 2 | Secrets access via secrets.get and secrets.has |
| [semtools.*](#semtools) | 1 | Semantic search using SemTools CLI |
| [session.*](#session) | 7 | Session lifecycle with archive-based expiration |
| [session_persistence.*](#session-persistence) | 4 | Session persistence (load, save, delete, exists) |
| [storage.*](#storage) | 7 | Cloud storage operations via fsspec (S3, GCS, Azure) |
| [text.*](#text) | 1 | Text processing including citation insertion |
| [textgrad.*](#textgrad) | 8 | TextGrad learning actions |
| [tools.*](#tools) | 5 | Bridges to CrewAI, MCP, and LangChain tools |
| [vector.*](#vector) | 8 | Vector similarity search via VectorIndex |
| [web.*](#web) | 4 | Web scraping, crawling, search via Firecrawl/Perplexity |

## Table of Contents

- [Quick Reference](#quick-reference)
- [Core Actions (P0)](#core-actions-p0)
- [Integration Actions (P1)](#integration-actions-p1)
- [Reasoning Actions (P2)](#reasoning-actions-p2)
- [Utility Actions (P3)](#utility-actions-p3)
- [Deprecated Actions](#deprecated-actions)
- [Custom Actions](#custom-actions)

---

## Core Actions (P0)

Core actions provide essential functionality for most workflows.


### cache.*

**Module:** `cache_actions.py`

| Action | Description |
|--------|-------------|
| `cache.wrap` | Wrap any action with automatic caching. |
| `cache.get` | Retrieve cached value by key without executing any action. |
| `cache.invalidate` | Invalidate (delete) cached entries by exact key or pattern. |
| `storage.hash` | Compute hash of file content from any URI. |

### core.*

**Module:** `core_actions.py`

| Action | Description |
|--------|-------------|
| `http.get` | Make HTTP GET request. |
| `http.post` | Make HTTP POST request. |
| `file.write` | Write content to a file (local or remote via fsspec). |
| `file.read` | Read content from a file (local or remote via fsspec). |
| `notify` | Send a notification. |
| `checkpoint.save` | Save checkpoint to specified path. |
| `checkpoint.load` | Load checkpoint from specified path. |

### data.*

**Module:** `data_actions.py`

| Action | Description |
|--------|-------------|
| `json.parse` | Parse a JSON string into a Python object. |
| `json_parse` | Parse a JSON string into a Python object. |
| `json.transform` | Transform data using JMESPath or JSONPath expressions. |
| `json_transform` | Transform data using JMESPath or JSONPath expressions. |
| `json.stringify` | Convert a Python object to a JSON string. |
| `json_stringify` | Convert a Python object to a JSON string. |
| `csv.parse` | Parse CSV data from text or file. |
| `csv_parse` | Parse CSV data from text or file. |
| `csv.stringify` | Convert a list of dicts or list of lists to a CSV string. |
| `csv_stringify` | Convert a list of dicts or list of lists to a CSV string. |
| `data.validate` | Validate data against a JSON Schema. |
| `data_validate` | Validate data against a JSON Schema. |
| `data.merge` | Merge multiple dictionaries/objects. |
| `data_merge` | Merge multiple dictionaries/objects. |
| `data.filter` | Filter list items using predicate expressions. |
| `data_filter` | Filter list items using predicate expressions. |

### llm.*

**Module:** `llm_actions.py`

| Action | Description |
|--------|-------------|
| `llm.call` | Call a language model (supports OpenAI, Azure OpenAI, Ollama, LiteLLM, and Shell CLI) |
| `llm.stream` | Stream LLM responses token-by-token |
| `llm.retry` | DEPRECATED: Use llm.call with max_retries parameter instead |
| `llm.tools` | LLM call with tool/function calling support |

#### LLM Provider Configuration

The LLM actions support multiple providers: **OpenAI**, **Azure OpenAI**, **Ollama**, **LiteLLM**, and **Shell CLI**.

**Provider Detection Priority:**
1. Explicit `provider` parameter (highest priority)
2. Environment variable detection:
   - `OLLAMA_API_BASE` → Ollama
   - `AZURE_OPENAI_API_KEY` + `AZURE_OPENAI_ENDPOINT` → Azure OpenAI
3. Default → OpenAI

**Environment Variables:**

| Variable | Provider | Description |
|----------|----------|-------------|
| `OPENAI_API_KEY` | OpenAI | OpenAI API key |
| `AZURE_OPENAI_API_KEY` | Azure | Azure OpenAI API key |
| `AZURE_OPENAI_ENDPOINT` | Azure | Azure endpoint URL |
| `AZURE_OPENAI_DEPLOYMENT` | Azure | Deployment name (optional) |
| `OLLAMA_API_BASE` | Ollama | Ollama API URL (default: `http://localhost:11434/v1`) |

**Ollama Example:**

```yaml
- name: ask_local_llm
  uses: llm.call
  with:
    provider: ollama
    model: llama3.2
    api_base: http://localhost:11434/v1
    messages:
      - role: user
        content: "{{ state.question }}"
```

**LiteLLM Example:**

```yaml
- name: ask_claude
  uses: llm.call
  with:
    provider: litellm
    model: anthropic/claude-3-opus-20240229
    messages:
      - role: user
        content: "{{ state.question }}"
```

See [YAML Reference](../shared/YAML_REFERENCE.md) for complete provider documentation.

### ltm.*

**Module:** `ltm_actions.py`

| Action | Description |
|--------|-------------|
| `ltm.store` | Store a key-value pair persistently with optional metadata. |
| `ltm.retrieve` | Retrieve a value from long-term memory by key. |
| `ltm.delete` | Delete a value from long-term memory by key. |
| `ltm.search` | Search across long-term memory using FTS5 and/or metadata filtering. |

### memory.*

**Module:** `memory_actions.py`

| Action | Description |
|--------|-------------|
| `memory.store` | Store a key-value pair in memory with optional TTL. |
| `memory.retrieve` | Retrieve a value from memory by key. |
| `memory.summarize` | Summarize conversation history using LLM to fit token windows. |

---

## Integration Actions (P1)

Integration actions connect TEA with external systems and enable multi-agent workflows.


### a2a.*

**Module:** `a2a_actions.py`

| Action | Description |
|--------|-------------|
| `a2a.send` | Send a message to a specific agent. |
| `a2a.receive` | Receive messages from agents. |
| `a2a.broadcast` | Broadcast message to all agents in namespace. |
| `a2a.delegate` | Delegate a task to another agent and wait for response. |
| `a2a.state.get` | Get a value from shared state. |
| `a2a.state.set` | Set a value in shared state. |
| `a2a.discover` | Discover available agents in namespace. |
| `a2a.register` | Register current agent for discovery and broadcasts. |
| `a2a.unregister` | Unregister current agent. |
| `a2a.heartbeat` | Send heartbeat to update last_seen timestamp. |

### agent.*

**Module:** `agent_actions.py`

| Action | Description |
|--------|-------------|
| `agent.dispatch` | Dispatch a task to a single named agent. |
| `agent.parallel` | Dispatch same task to multiple agents in parallel. |
| `agent.sequential` | Chain multiple agents where output feeds into next agent's input. |
| `agent.coordinate` | Coordinator pattern with leader agent dispatching to workers. |
| `agent.crewai_delegate` | Delegate to CrewAI for complex multi-agent workflows. |

### firestore.*

**Module:** `firestore_actions.py`

| Action | Description |
|--------|-------------|
| `firestore.get` |  |
| `firestore.set` |  |
| `firestore.query` |  |
| `firestore.delete` |  |
| `firestore.batch` |  |

### graph.*

**Module:** `graph_actions.py`

| Action | Description |
|--------|-------------|
| `graph.store_entity` | Store an entity (node) in the graph database. |
| `graph.store_relation` | Store a relation (edge) between two entities. |
| `graph.query` | Execute a Cypher/Datalog/SQL-PGQ query or pattern match. |
| `graph.retrieve_context` | Retrieve relevant subgraph context. |
| `graph.delete_entity` | Delete an entity (node) from the graph database. |
| `graph.delete_relation` | Delete a relation (edge) from the graph database. |
| `graph.update_entity` | Update properties of an entity (node) in the graph database. |
| `graph.update_relation` | Update properties of a relation (edge) in the graph database. |
| `graph.add_labels` | Add labels to an entity (node) in the graph database. |
| `graph.remove_labels` | Remove labels from an entity (node) in the graph database. |
| `graph.store_entities_batch` | Bulk insert/update multiple entities in a single transaction. |
| `graph.store_relations_batch` | Bulk create/update multiple relations in a single transaction. |
| `graph.delete_entities_batch` | Delete multiple entities in a single transaction. |
| `graph.merge_entity` | Conditional upsert with ON CREATE / ON MATCH semantics. |
| `graph.merge_relation` | Conditional upsert of a relation with ON CREATE / ON MATCH semantics. |
| `graph.create` | Create a property graph from vertex and edge tables (DuckPGQ). |
| `graph.drop` | Drop a property graph (DuckPGQ). |
| `graph.algorithm` | Run a graph algorithm (DuckPGQ). |
| `graph.shortest_path` | Find shortest path between two entities (DuckPGQ). |
| `graph.list_graphs` | List all created property graphs (DuckPGQ). |
| `graph.vector_search` | Perform vector similarity search using Neo4j Vector Index. |
| `graph.create_vector_index` | Create a vector index in Neo4j for similarity search. |
| `graph.drop_vector_index` | Drop a vector index from Neo4j. |
| `graph.list_vector_indexes` | List all vector indexes in Neo4j. |
| `graph.check_vector_support` | Check if the Neo4j instance supports vector indexes. |

### neo4j_gds.*

**Module:** `neo4j_gds_actions.py`

| Action | Description |
|--------|-------------|
| `neo4j.gds_check_available` | Check if Neo4j GDS library is available. |
| `neo4j.gds_version` | Get the installed Neo4j GDS library version. |
| `neo4j.gds_project_graph` | Create an in-memory graph projection for GDS algorithms. |
| `neo4j.gds_drop_graph` | Drop (remove) an in-memory graph projection. |
| `neo4j.gds_list_graphs` | List all active in-memory graph projections. |
| `neo4j.gds_estimate_memory` | Estimate memory requirements for a GDS algorithm. |
| `neo4j.gds_page_rank` | Run PageRank algorithm on a projected graph. |
| `neo4j.gds_betweenness` | Run Betweenness Centrality algorithm. |
| `neo4j.gds_degree` | Run Degree Centrality algorithm. |
| `neo4j.gds_closeness` | Run Closeness Centrality algorithm. |
| `neo4j.gds_louvain` | Run Louvain community detection algorithm. |
| `neo4j.gds_label_propagation` | Run Label Propagation community detection algorithm. |
| `neo4j.gds_wcc` | Run Weakly Connected Components algorithm. |
| `neo4j.gds_dijkstra` | Find shortest weighted path using Dijkstra's algorithm. |
| `neo4j.gds_astar` | Find shortest path using A* algorithm with heuristic. |
| `neo4j.gds_all_shortest_paths` | Find shortest paths from source to all other nodes. |
| `neo4j.gds_node_similarity` | Compute Jaccard similarity between nodes based on shared neighbors. |
| `neo4j.gds_knn` | Run K-Nearest Neighbors algorithm on node properties. |

### neo4j_trigger.*

**Module:** `neo4j_trigger_actions.py`

| Action | Description |
|--------|-------------|
| `neo4j.check_apoc` | Check if APOC library is installed and available. |
| `neo4j.get_apoc_version` | Get the installed APOC library version. |
| `neo4j.check_triggers` | Check if APOC triggers are enabled in Neo4j configuration. |
| `neo4j.register_trigger` | Register a database trigger using APOC. |
| `neo4j.unregister_trigger` | Remove a registered trigger. |
| `neo4j.list_triggers` | List all registered triggers. |
| `neo4j.pause_trigger` | Temporarily disable a trigger without removing it. |
| `neo4j.resume_trigger` | Re-enable a paused trigger. |
| `neo4j.register_callback` | Register a trigger that fires an HTTP webhook on graph changes. |
| `neo4j.register_state_update` | Register a trigger that writes to a state node for agent consumption. |
| `neo4j.cleanup_triggers` | Remove triggers by prefix, used for session/agent cleanup. |

---

## Reasoning Actions (P2)

Reasoning actions provide advanced AI capabilities for planning, reflection, and error handling.


### error.*

**Module:** `error_actions.py`

| Action | Description |
|--------|-------------|
| `error.is_retryable` |  |
| `error.clear` |  |
| `error.get` |  |
| `error.has` |  |
| `error.type` |  |
| `error.retry` |  |
| `error.respond` |  |

### planning.*

**Module:** `planning_actions.py`

| Action | Description |
|--------|-------------|
| `plan.decompose` | Decompose a goal into subtasks using LLM. |
| `plan.execute` | Execute plan subtasks respecting dependency order. |
| `plan.replan` | Re-plan from current state, preserving completed subtasks. |
| `plan.status` | Get current plan execution status. |

### ratelimit.*

**Module:** `ratelimit_actions.py`

| Action | Description |
|--------|-------------|
| `ratelimit.wrap` | Wrap any action with rate limiting. |

### reasoning.*

**Module:** `reasoning_actions.py`

| Action | Description |
|--------|-------------|
| `reason.cot` | Chain-of-Thought reasoning action. |
| `reason.react` | ReAct (Reason-Act) reasoning action. |
| `reason.self_correct` | Self-correction reasoning action. |
| `reason.decompose` | Problem decomposition reasoning action. |
| `reason.dspy.cot` | Chain-of-Thought using DSPy ChainOfThought module. |
| `reason.dspy.react` | ReAct using DSPy ReAct module with tool bridge. |
| `reason.dspy.compile` | Compile DSPy module with teleprompter for optimized prompts. |

### reflection.*

**Module:** `reflection_actions.py`

| Action | Description |
|--------|-------------|
| `reflection.loop` | Execute a generate→evaluate→correct loop (AC: 1, 5, 6). |
| `reflection.evaluate` | Standalone evaluation action (AC: 7). |
| `reflection.correct` | Standalone correction action (AC: 8). |

### retry.*

**Module:** `retry_actions.py`

| Action | Description |
|--------|-------------|
| `retry.loop` | Execute validation with retry loop (TEA-YAML-005). |

### validation.*

**Module:** `validation_actions.py`

| Action | Description |
|--------|-------------|
| `validate.extraction` | Validate extracted entities and relationships (AC: 16-18). |
| `validate.generate_prompt` | Generate a schema-guided extraction prompt (AC: 23-27). |

---

## Utility Actions (P3)

Utility actions provide specialized functionality for specific use cases.


### academic.*

**Module:** `academic_actions.py`

| Action | Description |
|--------|-------------|
| `academic.pubmed` | Search PubMed database for scientific articles via NCBI E-utilities |
| `academic.arxiv` | Search ArXiv preprint server for papers |
| `academic.crossref` | Query CrossRef API for DOI metadata or search by query string |

#### academic.pubmed

Search the PubMed database for scientific articles using NCBI E-utilities API.

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `query` | string | required | Search query (PubMed query syntax) |
| `max_results` | int | 5 | Maximum results to return |
| `sort_by` | string | "relevance" | Sort order: "relevance" or "date" |
| `timeout` | int | 30 | Request timeout in seconds |

**Rate Limiting:** 3 requests/second (10 req/s with `NCBI_API_KEY`)

#### academic.arxiv

Search the ArXiv preprint server for research papers.

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `query` | string | optional | Search query string |
| `arxiv_id` | string | optional | Direct paper lookup by ID |
| `max_results` | int | 5 | Maximum results to return |
| `sort_by` | string | "relevance" | Sort order: "relevance" or "date" |

**Rate Limiting:** 1 request per 3 seconds (per ArXiv terms of service)

#### academic.crossref

Query the CrossRef API for DOI metadata or search by query string.

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `doi` | string | optional | DOI for direct lookup |
| `query` | string | optional | Search query string |
| `max_results` | int | 5 | Maximum results to return |
| `mailto` | string | optional | Email for polite pool access (50 req/s) |

### auth.*

**Module:** `auth_actions.py`

| Action | Description |
|--------|-------------|
| `auth.verify` | Verify an authentication token |
| `auth.get_user` | Get full user profile by UID |

#### auth.verify

Verify an authentication token. Extracts token from headers if not provided directly.

```yaml
- name: verify_token
  uses: auth.verify
  with:
    token: "{{ state.custom_token }}"  # Optional
    headers: "{{ state.request_headers }}"  # Optional
  output: auth_result
```

#### auth.get_user

Get full user profile by UID from Firebase Authentication.

```yaml
- name: get_profile
  uses: auth.get_user
  with:
    uid: "{{ state.__user__.uid }}"
  output: full_profile
```

### bmad.*

**Module:** `bmad_actions.py`

| Action | Description |
|--------|-------------|
| `bmad.parse_story` | Parse a BMad story file into structured data. |
| `bmad_parse_story` | Parse a BMad story file into structured data. |

### catalog.*

**Module:** `catalog_actions.py`

| Action | Description |
|--------|-------------|
| `catalog.register_table` | Register a new table in the DuckLake catalog. |
| `catalog.get_table` | Get table metadata from the catalog. |
| `catalog.list_tables` | List tables in the catalog with optional filtering. |
| `catalog.track_file` | Track a Parquet or delta file in the catalog. |
| `catalog.get_file` | Get file metadata from the catalog. |
| `catalog.list_files` | List files for a table with optional filtering. |
| `catalog.create_snapshot` | Create a point-in-time snapshot for a table. |
| `catalog.get_latest_snapshot` | Get the most recent snapshot for a table. |
| `catalog.list_snapshots` | List snapshots for a table. |
| `catalog.get_changed_files` | Get files that changed since a snapshot. |

### cloud_memory.*

**Module:** `cloud_memory_actions.py`

| Action | Description |
|--------|-------------|
| `memory.cloud_store` | Store an artifact in cloud storage with metadata and embedding. |
| `memory.cloud_retrieve` | Retrieve an artifact from cloud storage. |
| `memory.cloud_list` | List artifacts with filtering. |
| `memory.manifest_update` | Update metadata only (not file content). |
| `memory.manifest_search` | Search documents by anchors. |

### code.*

**Module:** `code_actions.py`

| Action | Description |
|--------|-------------|
| `code.execute` | Execute Python code in a RestrictedPython sandbox. |
| `code.sandbox` | Manage persistent sandbox sessions for multi-step code execution. |

### context.*

**Module:** `context_actions.py`

| Action | Description |
|--------|-------------|
| `context.assemble` | Assemble context from configured layers with relevance ranking. |

### data_tabular.*

**Module:** `data_tabular_actions.py`

| Action | Description |
|--------|-------------|
| `data.create_table` | Register a new tabular table in the catalog. |
| `data.insert` | Insert rows into a tabular table. |
| `data.update` | Update rows matching WHERE clause. |
| `data.delete` | Delete rows matching WHERE clause. |
| `data.query` | Query tabular data with SQL. |
| `data.consolidate` | Full compaction: merge N Parquet files + inlined -> 1 Parquet file. |

### dspy.*

**Module:** `dspy_actions.py`

| Action | Description |
|--------|-------------|
| `reason.dspy.cot` | Chain-of-Thought reasoning using DSPy ChainOfThought module. |
| `reason.dspy.react` | ReAct reasoning using DSPy ReAct module. |
| `reason.dspy.compile` | Compile a DSPy module with teleprompter for optimized prompts. |
| `reason.dspy.optimize` | Run optimization against a validation set. |
| `reason.dspy.list_compiled` | List all compiled DSPy modules. |
| `reason.dspy.export` | Export all compiled DSPy prompts for checkpoint persistence. |
| `reason.dspy.import` | Import compiled DSPy prompts from checkpoint persistence. |

### git.*

**Module:** `git_actions.py`

| Action | Description |
|--------|-------------|
| `git.worktree_create` |  |
| `git.worktree_remove` |  |
| `git.worktree_merge` |  |
| `git.worktree_list` |  |
| `git.current_branch` |  |
| `git.status` |  |

### github.*

**Module:** `github_actions.py`

| Action | Description |
|--------|-------------|
| `github.list_issues` | List issues from a GitHub repository. |
| `github.create_issue` | Create a new GitHub issue. |
| `github.update_issue` | Update an existing GitHub issue. |
| `github.search_issues` | Search GitHub issues using GitHub search syntax. |

### http_response.*

**Module:** `http_response_actions.py`

| Action | Description |
|--------|-------------|
| `http.respond` | Synchronous version of http.respond. |

### input_validation.*

**Module:** `input_validation_actions.py`

| Action | Description |
|--------|-------------|
| `validate.input` | Validate input data against a schema (AC10). |
| `validate.schema` | Create a reusable schema validator. |

### llamaextract.*

**Module:** `llamaextract_actions.py`

| Action | Description |
|--------|-------------|
| `llamaextract.extract` | Extract structured data from a document using LlamaExtract. |
| `llamaextract.upload_agent` | Create or update an extraction agent. |
| `llamaextract.list_agents` | List available extraction agents. |
| `llamaextract.get_agent` | Get extraction agent details. |
| `llamaextract.delete_agent` | Delete an extraction agent. |
| `llamaextract.submit_job` | Submit async extraction job to LlamaExtract. |
| `llamaextract.poll_status` | Poll job status from LlamaExtract. |
| `llamaextract.get_result` | Get extraction result for a completed job. |

### llamaindex.*

**Module:** `llamaindex_actions.py`

| Action | Description |
|--------|-------------|
| `rag.llamaindex.query` | Execute a simple vector query against a LlamaIndex index. |
| `rag.llamaindex.router` | Execute a router query that selects the best engine for the query. |
| `rag.llamaindex.subquestion` | Execute a sub-question query that decomposes complex queries. |
| `rag.llamaindex.create_index` | Create a new LlamaIndex index from documents or a directory. |
| `rag.llamaindex.load_index` | Load a persisted LlamaIndex index. |
| `rag.llamaindex.add_documents` | Add documents to an existing LlamaIndex index. |

### llm_local.*

**Module:** `llm_local_actions.py`

| Action | Description |
|--------|-------------|
| `llm.local.call` | LLM completion using local or API backend. |
| `llm.local.chat` | Chat completion using OpenAI-compatible format. |
| `llm.local.stream` | Streaming LLM generation with token-by-token output. |
| `llm.local.embed` | Generate text embeddings using local or API backend. |
| `llm.chat` | Chat completion using OpenAI-compatible format. |
| `llm.embed` | Generate text embeddings using local or API backend. |

### markdown.*

**Module:** `markdown_actions.py`

| Action | Description |
|--------|-------------|
| `markdown.parse` | Parse Markdown content into a structured document. |
| `markdown_parse` | Parse Markdown content into a structured document. |

### mem0.*

**Module:** `mem0_actions.py`

| Action | Description |
|--------|-------------|
| `memory.mem0.add` | Store messages with automatic fact extraction using Mem0. |
| `memory.mem0.search` | Search memories by semantic similarity using Mem0. |
| `memory.mem0.get_all` | Get all memories for a specified scope. |
| `memory.mem0.get` | Get a specific memory by its ID. |
| `memory.mem0.update` | Update an existing memory by ID. |
| `memory.mem0.delete` | Delete memories by ID or scope. |
| `memory.mem0.test` | Test Mem0 connection and configuration. |

### observability.*

**Module:** `observability_actions.py`

| Action | Description |
|--------|-------------|
| `trace.start` | Start a new trace span. |
| `trace.log` | Log an event, metrics, or state snapshot to the current span. |
| `trace.end` | End the current trace span. |
| `opik.healthcheck` | Validate Opik connectivity and authentication (TEA-BUILTIN-005.3). |
| `obs.get_flow_log` | Get the complete flow log from ObservabilityContext (TEA-OBS-001.1). |
| `obs.log_event` | Log a custom event to the observability stream (TEA-OBS-001.1). |
| `obs.query_events` | Query events from the observability stream (TEA-OBS-001.1). |

### rag.*

**Module:** `rag_actions.py`

| Action | Description |
|--------|-------------|
| `embedding.create` | Create embeddings from text. |
| `vector.store` | Store documents with embeddings in vector store. |
| `vector.query` | Query vector store for similar documents. |
| `vector.index_files` | Index files/directories into vector store (AC: 1-13). |

### schema.*

**Module:** `schema_actions.py`

| Action | Description |
|--------|-------------|
| `schema.merge` | Deep merge multiple JSON Schemas with kubectl-style semantics. |

### search.*

**Module:** `search_actions.py`

| Action | Description |
|--------|-------------|
| `memory.grep` | Execute grep-like search across agent memory. |
| `memory.sql_query` | Execute SQL query against agent_memory table with safety controls. |
| `memory.search_content` | Search for files by structured content field values. |

### secrets.*

**Module:** `secrets_actions.py`

| Action | Description |
|--------|-------------|
| `secrets.get` | Get a secret value by key. |
| `secrets.has` | Check if a secret exists. |

### semtools.*

**Module:** `semtools_actions.py`

| Action | Description |
|--------|-------------|
| `semtools.search` | Semantic search using SemTools CLI. |

### session.*

**Module:** `session_actions.py`

| Action | Description |
|--------|-------------|
| `session.create` | Create a new session with expiration. |
| `session.end` | End session and archive its memory. |
| `session.archive` | Archive session with custom reason. |
| `session.restore` | Restore archived session. |
| `session.get` | Get session metadata. |
| `session.list` | List sessions with optional filtering. |
| `session.archive_expired` | Archive sessions that have exceeded their TTL. |

### session_persistence.*

**Module:** `session_persistence_actions.py`

| Action | Description |
|--------|-------------|
| `session.load` | Load session data from the configured session backend. |
| `session.save` | Save current state to the session backend. |
| `session.delete` | Delete a session from the backend. |
| `session.exists` | Check if a session exists. |

### storage.*

**Module:** `storage_actions.py`

| Action | Description |
|--------|-------------|
| `storage.list` | List files/objects at the given path. |
| `storage.exists` | Check if a file/object exists. |
| `storage.delete` | Delete a file/object or directory. |
| `storage.copy` | Copy a file/object to another location. |
| `storage.info` | Get metadata/info about a file/object. |
| `storage.mkdir` | Create a directory/prefix. |
| `storage.native` | Execute a native filesystem operation not exposed by standard fsspec API. |

### text.*

**Module:** `text_actions.py`

| Action | Description |
|--------|-------------|
| `text.insert_citations` | Insert citation markers using semantic embedding matching |

#### text.insert_citations

Insert citation markers into text using semantic embedding matching. Uses OpenAI embeddings to compute similarity between sentences and references, placing citations at the most semantically relevant positions.

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `text` | string | required | Markdown text to process |
| `references` | list[str] | required | List of reference strings |
| `model` | string | "text-embedding-3-large" | OpenAI embedding model |
| `api_key` | string | None | OpenAI API key (uses env var if not provided) |

**Returns:**

```json
{
  "cited_text": "Text with [1] citation markers inserted.",
  "references_section": "## References\n\n1. Author. Title. 2020.",
  "citation_map": {"1": "Author. Title. 2020."},
  "text": "Full text with citations and References section"
}
```

**Features:**
- Semantic matching via embeddings (not just keyword matching)
- Citations placed at most relevant sentences
- Conclusions and Abstract sections excluded from citation
- References reordered by first occurrence
- Markdown formatting preserved

### textgrad.*

**Module:** `textgrad_actions.py`

| Action | Description |
|--------|-------------|
| `learn.textgrad.variable` | Define an optimizable prompt variable (learn.textgrad.variable action). |
| `learn.textgrad.feedback` | Compute textual gradients from output evaluation (learn.textgrad.feedback action... |
| `learn.textgrad.optimize_prompt` | Optimize a prompt variable using TextGrad (learn.textgrad.optimize_prompt action... |
| `learn.textgrad.reflection_corrector` | Corrector for reflection.loop that uses TextGrad for prompt optimization (AC: 4)... |
| `textgrad.variable` | Define an optimizable prompt variable (learn.textgrad.variable action). |
| `textgrad.feedback` | Compute textual gradients from output evaluation (learn.textgrad.feedback action... |
| `textgrad.optimize_prompt` | Optimize a prompt variable using TextGrad (learn.textgrad.optimize_prompt action... |
| `textgrad.reflection_corrector` | Corrector for reflection.loop that uses TextGrad for prompt optimization (AC: 4)... |

### tools.*

**Module:** `tools_actions.py`

| Action | Description |
|--------|-------------|
| `tools.crewai` | Execute a CrewAI tool. |
| `tools.mcp` | Execute a tool from an MCP server. |
| `tools.langchain` | Execute a LangChain tool. |
| `tools.discover` | Discover available tools from specified sources. |
| `tools.clear_cache` | Clear the tool discovery cache. |

### vector.*

**Module:** `vector_actions.py`

| Action | Description |
|--------|-------------|
| `memory.vector_search` | Semantic search over agent memory using vector similarity. |
| `memory.vector_search_by_embedding` | Search using a pre-computed embedding vector. |
| `memory.vector_load_data` | Load vector data from a Parquet file or URL. |
| `memory.vector_build_index` | Build or rebuild the vector search index. |
| `memory.vector_stats` | Get statistics about the vector index. |
| `memory.embed` | Generate embedding for content. |
| `memory.embed_batch` | Generate embeddings for multiple content strings. |
| `memory.backfill_embeddings` | Backfill embeddings for documents missing them. |

### web.*

**Module:** `web_actions.py`

| Action | Description |
|--------|-------------|
| `web.scrape` | Scrape a URL and extract LLM-ready content via Firecrawl API. |
| `web.crawl` | Crawl a website recursively via Firecrawl API. |
| `web.search` | Perform web search via Perplexity API. |
| `web.ai_scrape` | Extract structured data from a URL using ScrapeGraphAI. |

---

## Deprecated Actions

The following actions have preferred alternatives:

| Deprecated | Use Instead | Notes |
|------------|-------------|-------|
| `llm.retry` | `llm.call` with `max_retries` | Built-in retry support |

---

## Custom Actions

Register custom actions via the `imports:` section in YAML:

```yaml
imports:
  - path: ./my_actions.py
    actions:
      - my_custom_action
```

Your module must implement `register_actions()`:

```python
def register_actions(registry, engine):
    registry['custom.my_action'] = my_action_function
```

---

## Source Location

All action modules are in:

```
python/src/the_edge_agent/actions/
```

---

*This document was auto-generated from the codebase. Run `python scripts/extract_action_signatures.py` to update.*
