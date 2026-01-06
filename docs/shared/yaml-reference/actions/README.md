# Built-in Actions Reference

> **Parent document:** [YAML Reference](../../YAML_REFERENCE.md)
> **Related:** [Node Specification](../nodes.md) | [Template Syntax](../templates.md)
> **Epic:** [DOC-002](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/DOC-002-yaml-reference-modularization.md)

## Overview

The Edge Agent provides **100+ built-in actions** organized into 12 categories. Actions are the building blocks for YAML agent workflows, providing capabilities from LLM calls to file I/O, data processing, memory persistence, reasoning patterns, multi-agent coordination, and cloud production deployment.

---

## Quick Reference

| Action | Description | Document |
|--------|-------------|----------|
| `llm.call` | Call LLM API | [LLM](./llm.md#llmcall) |
| `llm.stream` | Stream LLM response | [LLM](./llm.md#llmstream) |
| `llm.tools` | Function/tool calling | [LLM](./llm.md#llmtools) |
| `http.get` | HTTP GET request | [I/O](./io.md#httpget) |
| `http.post` | HTTP POST request | [I/O](./io.md#httppost) |
| `file.read` | Read file | [I/O](./io.md#fileread) |
| `file.write` | Write file | [I/O](./io.md#filewrite) |
| `json.parse` | Parse JSON string | [Data](./data.md#jsonparse) |
| `json.transform` | JMESPath/JSONPath transform | [Data](./data.md#jsontransform) |
| `memory.store` | Store in session memory | [Memory](./memory.md#memorystore) |
| `ltm.store` | Long-term memory store | [Memory](./memory.md#ltmstore) |
| `vector.query` | Semantic search | [Integrations](./integrations.md#vectorquery) |
| `web.scrape` | Web scraping | [Integrations](./integrations.md#webscrape) |
| `retry.loop` | Retry with correction | [Specialized](./specialized.md#retryloop) |
| `ratelimit.wrap` | Rate limit any action | [Specialized](./specialized.md#ratelimitwrap) |
| `secrets.get` | Get secret value | [Specialized](./specialized.md#secretsget) |
| `secrets.has` | Check secret exists | [Specialized](./specialized.md#secretshas) |
| `reason.cot` | Chain-of-Thought reasoning | [Reasoning](./reasoning.md#reasoncot) |
| `reason.react` | ReAct reasoning loop | [Reasoning](./reasoning.md#reasonreact) |
| `reason.self_correct` | Generate-critique-improve | [Reasoning](./reasoning.md#reasonself_correct) |
| `reason.decompose` | Problem decomposition | [Reasoning](./reasoning.md#reasondecompose) |
| `agent.dispatch` | Single agent task dispatch | [Agent](./agent.md#agentdispatch) |
| `agent.parallel` | Parallel multi-agent execution | [Agent](./agent.md#agentparallel) |
| `agent.sequential` | Sequential agent chaining | [Agent](./agent.md#agentsequential) |
| `agent.coordinate` | Leader-worker coordination | [Agent](./agent.md#agentcoordinate) |
| `agent.crewai_delegate` | CrewAI integration | [Agent](./agent.md#agentcrewai_delegate) |
| `a2a.send` | Send message to agent | [A2A](./a2a.md#a2asend) |
| `a2a.receive` | Receive messages | [A2A](./a2a.md#a2areceive) |
| `a2a.broadcast` | Broadcast to namespace | [A2A](./a2a.md#a2abroadcast) |
| `a2a.delegate` | Request/response delegation | [A2A](./a2a.md#a2adelegate) |
| `a2a.state.get` | Get shared state | [A2A](./a2a.md#a2astateget) |
| `a2a.state.set` | Set shared state | [A2A](./a2a.md#a2astateset) |
| `a2a.discover` | Discover agents | [A2A](./a2a.md#a2adiscover) |
| `reflection.loop` | Generate-evaluate-correct loop | [Reflection](./reflection.md#reflectionloop) |
| `reflection.evaluate` | Standalone evaluation | [Reflection](./reflection.md#reflectionevaluate) |
| `reflection.correct` | Standalone correction | [Reflection](./reflection.md#reflectioncorrect) |
| `session.load` | Load session data | [Cloud Production](./cloud-production.md#sessionload) |
| `session.save` | Save session data | [Cloud Production](./cloud-production.md#sessionsave) |
| `firestore.get` | Get Firestore document | [Cloud Production](./cloud-production.md#firestoreget) |
| `firestore.set` | Set Firestore document | [Cloud Production](./cloud-production.md#firestoreset) |
| `firestore.query` | Query Firestore | [Cloud Production](./cloud-production.md#firestorequery) |
| `auth.verify` | Verify auth token | [Cloud Production](./cloud-production.md#authverify) |
| `validate.input` | Validate input data | [Cloud Production](./cloud-production.md#validateinput) |
| `http.respond` | Custom HTTP response | [Cloud Production](./cloud-production.md#httprespond) |
| `error.is_retryable` | Check if error is retryable | [Cloud Production](./cloud-production.md#erroris_retryable) |

---

## Action Categories

### [LLM Actions](./llm.md) (4 actions)

Language model integration with multiple providers.

| Action | Description |
|--------|-------------|
| `llm.call` | Call OpenAI-compatible LLM API |
| `llm.stream` | Stream LLM responses with chunk aggregation |
| `llm.retry` | LLM calls with exponential backoff retry |
| `llm.tools` | Function/tool calling with action dispatch |

**Providers:** OpenAI, Azure OpenAI, Ollama, LiteLLM (100+ models)

---

### [I/O Actions](./io.md) (10 actions)

HTTP requests, file operations, and cloud storage.

| Action | Description |
|--------|-------------|
| `http.get` | HTTP GET request |
| `http.post` | HTTP POST request |
| `file.read` | Read local or remote files |
| `file.write` | Write local or remote files |
| `storage.list` | List files in directory |
| `storage.exists` | Check if file exists |
| `storage.info` | Get file metadata |
| `storage.copy` | Copy files (cross-cloud) |
| `storage.delete` | Delete files |
| `storage.mkdir` | Create directories |

**Cloud Support:** S3, GCS, Azure Blob via fsspec

---

### [Data Processing Actions](./data.md) (15 actions)

JSON/CSV parsing, validation, transformation, and code execution.

| Action | Description |
|--------|-------------|
| `json.parse` | Parse JSON string to object |
| `json.transform` | Transform with JMESPath/JSONPath |
| `json.stringify` | Convert object to JSON string |
| `csv.parse` | Parse CSV to list of dicts |
| `csv.stringify` | Convert list to CSV |
| `data.validate` | Validate against JSON Schema |
| `data.merge` | Deep merge dictionaries |
| `data.filter` | Filter list with predicates |
| `code.execute` | Execute sandboxed Python |
| `code.sandbox` | Manage persistent sandbox sessions |
| `data.create_table` | Create tabular data table |
| `data.insert` | Insert rows into table |
| `data.update` | Update rows by primary key |
| `data.delete` | Delete rows (tombstone) |
| `data.query` | SQL query with LWW merge |

---

### [Memory Actions](./memory.md) (20 actions)

Session memory, long-term memory, caching, and graph storage.

| Action | Description |
|--------|-------------|
| `memory.store` | Store in session memory |
| `memory.retrieve` | Retrieve from session |
| `memory.summarize` | Summarize with LLM |
| `ltm.store` | Store in long-term memory |
| `ltm.retrieve` | Retrieve from LTM |
| `ltm.delete` | Delete from LTM |
| `ltm.search` | Search LTM by pattern |
| `cache.wrap` | Cache action results |
| `cache.get` | Get cached value |
| `cache.invalidate` | Invalidate cache entries |
| `memory.cloud_store` | Firebase Agent Memory store |
| `memory.cloud_retrieve` | Firebase retrieve |
| `memory.cloud_list` | Firebase list memories |
| `memory.grep` | Text search with ranking |
| `memory.sql_query` | SQL query via DuckDB |
| `memory.embed` | Generate embeddings |
| `memory.vector_search` | Vector similarity search |
| `graph.store_entity` | Store graph entity |
| `graph.store_relation` | Store graph relationship |
| `graph.query` | Query graph with Cypher/Datalog |

**Backends:** SQLite, DuckDB, Firebase, CozoDB, Kuzu

---

### [Integration Actions](./integrations.md) (15 actions)

External services, web scraping, RAG, and observability.

| Action | Description |
|--------|-------------|
| `trace.start` | Start trace span |
| `trace.log` | Log events/metrics |
| `trace.end` | End trace span |
| `opik.healthcheck` | Validate Opik connection |
| `web.scrape` | Scrape single page (Firecrawl) |
| `web.crawl` | Crawl multiple pages |
| `web.search` | Web search (Perplexity) |
| `web.ai_scrape` | AI-powered extraction (ScrapeGraphAI) |
| `embedding.create` | Generate embeddings |
| `vector.store` | Store with embeddings |
| `vector.query` | Semantic similarity search |
| `tools.crewai` | Execute CrewAI tools |
| `tools.mcp` | Execute MCP server tools |
| `tools.langchain` | Execute LangChain tools |
| `tools.discover` | Discover available tools |

**External APIs:** Firecrawl, Perplexity, ScrapeGraphAI, Opik

---

### [Reasoning Actions](./reasoning.md) (7 actions)

AI reasoning patterns: Chain-of-Thought, ReAct, self-correction, and decomposition.

| Action | Description |
|--------|-------------|
| `reason.cot` | Chain-of-Thought structured reasoning |
| `reason.react` | ReAct thought-action-observation loop |
| `reason.self_correct` | Generate-critique-improve cycle |
| `reason.decompose` | Problem decomposition and synthesis |
| `reason.dspy.cot` | DSPy ChainOfThought wrapper |
| `reason.dspy.react` | DSPy ReAct wrapper |
| `reason.dspy.compile` | Compile DSPy module |

**Features:** Multiple thinking formats, tool integration, multi-model support, Opik-compatible traces

---

### [Specialized Actions](./specialized.md) (15 actions)

Checkpoints, schema manipulation, document extraction, validation, rate limiting, and secrets management.

| Action | Description |
|--------|-------------|
| `checkpoint.save` | Save workflow checkpoint |
| `checkpoint.load` | Load checkpoint |
| `schema.merge` | Deep merge JSON Schemas |
| `llamaextract.extract` | Extract structured data from documents |
| `llamaextract.upload_agent` | Create LlamaExtract agent |
| `llamaextract.list_agents` | List extraction agents |
| `llamaextract.get_agent` | Get agent details |
| `llamaextract.delete_agent` | Delete extraction agent |
| `validate.extraction` | 3-layer extraction validation |
| `validate.generate_prompt` | Generate extraction prompt |
| `retry.loop` | Retry with correction node |
| `ratelimit.wrap` | Rate limit any action (RPM/RPS) |
| `secrets.get` | Get secret from cloud provider |
| `secrets.has` | Check if secret exists |
| Custom Actions | Register Python functions |

**Features:** Prolog constraints, semantic probes, checkpoint persistence, rate limiting, cloud secrets (AWS, Azure, GCP)

---

### [Multi-Agent Actions](./agent.md) (5 actions)

Multi-agent collaboration primitives for orchestrating multiple AI agents.

| Action | Description |
|--------|-------------|
| `agent.dispatch` | Dispatch task to single named agent |
| `agent.parallel` | Parallel dispatch with aggregation strategies |
| `agent.sequential` | Chain agents where output feeds next |
| `agent.coordinate` | Leader-worker coordination pattern |
| `agent.crewai_delegate` | CrewAI integration with native fallback |

**Features:** Agent registry, consensus/voting aggregation, sequential chaining, leader-worker coordination, CrewAI integration

---

### [Planning Actions](./planning.md) (4 actions)

Planning and decomposition primitives for complex task management.

| Action | Description |
|--------|-------------|
| `plan.decompose` | Decompose goal into subtasks with dependencies |
| `plan.execute` | Execute plan respecting dependency order |
| `plan.replan` | Re-plan from current state preserving completed work |
| `plan.status` | Get plan execution status |

**Features:** DAG-based dependencies, parallel/sequential execution, failure handling strategies (replan/retry/skip/abort), checkpoint integration

---

### [A2A Communication Actions](./a2a.md) (10 actions)

Inter-agent communication primitives for distributed agent systems.

| Action | Description |
|--------|-------------|
| `a2a.send` | Send message to specific agent |
| `a2a.receive` | Receive messages with filtering |
| `a2a.broadcast` | Broadcast to all agents in namespace |
| `a2a.delegate` | Request/response with fallback |
| `a2a.state.get` | Get shared state value |
| `a2a.state.set` | Set shared state with optimistic locking |
| `a2a.discover` | Discover available agents |
| `a2a.register` | Register agent for discovery |
| `a2a.unregister` | Unregister agent |
| `a2a.heartbeat` | Update agent last_seen timestamp |

**Features:** Namespace isolation, optimistic locking, capability-based discovery, timeout handling, delegation fallbacks

---

### [Reflection Actions](./reflection.md) (3 actions)

Self-correcting agent patterns with generate-evaluate-correct loops.

| Action | Description |
|--------|-------------|
| `reflection.loop` | Execute generate→evaluate→correct cycle |
| `reflection.evaluate` | Standalone evaluation (schema, LLM, custom) |
| `reflection.correct` | Standalone correction action |

**Features:** JSON Schema validation, LLM-as-judge evaluation, custom Python/Lua/Prolog evaluators, multiple failure strategies, iteration history tracking

---

### [Cloud Production Actions](./cloud-production.md) (20 actions)

Enterprise-grade capabilities for production deployment.

| Action | Description |
|--------|-------------|
| `session.load` | Load session data by ID |
| `session.save` | Persist state to session backend |
| `session.delete` | Delete session |
| `session.exists` | Check if session exists |
| `firestore.get` | Get Firestore document |
| `firestore.set` | Create/update document |
| `firestore.query` | Query with filters |
| `firestore.delete` | Delete document |
| `firestore.batch` | Atomic batch operations |
| `auth.verify` | Verify authentication token |
| `auth.get_user` | Get user profile by UID |
| `validate.input` | Validate data against schema |
| `validate.schema` | Create reusable validator |
| `http.respond` | Custom HTTP response with early termination |
| `error.is_retryable` | Check if error is retryable |
| `error.clear` | Clear error from state |
| `error.get` | Get error info |
| `error.has` | Check if error exists |
| `error.type` | Get error type |
| `error.retry` | Retry last failed action |

**Features:** Session persistence, Firestore CRUD, Firebase/JWT auth, input validation, HTTP response transformation, structured error handling

---

## Dual Namespace Convention

All actions support two naming conventions:

```yaml
# Dotted namespace (recommended)
- uses: llm.call
- uses: file.read
- uses: memory.store

# Underscore namespace (alternative)
- uses: actions.llm_call
- uses: actions.file_read
- uses: actions.memory_store
```

Both forms are functionally identical. The dotted namespace is preferred for readability.

---

## Action Anatomy

Every action follows a consistent structure:

```yaml
- name: step_name           # Unique identifier for this step
  uses: category.action     # Action to execute
  with:                     # Parameters (action-specific)
    param1: value1
    param2: "{{ state.dynamic }}"
  output: result_key        # Where to store results in state
  condition: "{{ state.should_run }}"  # Optional: skip if false
```

**Common Return Fields:**

| Field | Type | Description |
|-------|------|-------------|
| `success` | bool | Whether action succeeded |
| `error` | string | Error message (on failure) |
| `error_type` | string | Error category for handling |

---

## Error Handling

Actions return structured errors for conditional handling:

```yaml
- name: risky_operation
  uses: http.get
  with:
    url: "{{ state.url }}"
  output: result

- name: handle_error
  condition: "{{ not result.success }}"
  uses: llm.call
  with:
    model: gpt-4
    messages:
      - role: user
        content: "Handle error: {{ result.error }}"
```

---

## See Also

- [Node Specification](../nodes.md) - Complete node structure
- [Template Syntax](../templates.md) - Jinja2 expressions
- [Advanced Runtimes](../advanced-runtimes.md) - Lua and Prolog nodes
- [YAML Reference](../../YAML_REFERENCE.md) - Complete reference
