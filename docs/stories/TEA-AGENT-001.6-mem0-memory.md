# Story TEA-AGENT-001.6: Mem0 Memory Integration

## Status

**Draft**

## Story

**As a** YAML agent developer,
**I want** Mem0 integration for universal memory management,
**so that** I can build agents with persistent user/session/agent memory and graph-based knowledge storage without implementing custom memory backends.

## Background

The Edge Agent currently provides basic `memory.*` actions for state persistence, but lacks:

1. Universal memory layer that persists across sessions
2. User-level, session-level, and agent-level memory scopes
3. Graph-based knowledge storage (Mem0g) for multi-hop reasoning
4. Automatic fact extraction from conversations
5. Semantic search over memories

Mem0 (https://github.com/mem0ai/mem0) is the industry-standard library for agent memory. This story integrates Mem0 as an optional backend for TEA's memory system.

## Acceptance Criteria

### AC1: `memory.mem0.add` Action
1. Stores messages with automatic fact extraction
2. Supports user_id, session_id, agent_id scopes
3. Accepts conversation messages format
4. Returns memory ID for reference
5. Configurable metadata extraction

### AC2: `memory.mem0.search` Action
1. Retrieves relevant memories by semantic similarity
2. Supports query string input
3. Configurable limit on results
4. Filter by user_id, session_id, agent_id
5. Returns list of memory objects with scores

### AC3: `memory.mem0.get_all` Action
1. Returns all memories for specified scope
2. Filter by user_id, session_id, or agent_id
3. Pagination support (limit, offset)
4. Optional include metadata

### AC4: `memory.mem0.update` Action
1. Modifies existing memory entries by ID
2. Partial update support (merge metadata)
3. Returns updated memory object

### AC5: `memory.mem0.delete` Action
1. Removes memories by ID
2. Bulk delete by scope (user_id, session_id)
3. Returns deletion confirmation

### AC6: Graph Memory Support
1. Enable graph memory with `graph: true` in settings
2. Entity and relation extraction from memories
3. Multi-hop query support via graph traversal
4. Compatible with Mem0g API

### AC7: Settings Configuration
1. Configure via `settings.memory.backend: mem0`
2. Support Mem0 configuration options (API key, endpoint)
3. Default scope (user_id, session_id) from settings
4. Graceful fallback to native memory when Mem0 unavailable

### AC8: Python Implementation
1. New module: `python/src/the_edge_agent/actions/mem0_actions.py`
2. All actions registered in `build_actions_registry()`
3. Test coverage >90%
4. Requires `mem0` optional dependency

## Tasks / Subtasks

- [ ] **Task 1: Mem0 Client Wrapper** (AC: 7)
  - [ ] Create `Mem0Client` wrapper class
  - [ ] Configuration from settings.memory
  - [ ] API key and endpoint management
  - [ ] Connection testing and fallback
  - [ ] Unit tests

- [ ] **Task 2: `memory.mem0.add` Action** (AC: 1)
  - [ ] Implement add action
  - [ ] Message format parsing
  - [ ] Scope handling (user_id, session_id, agent_id)
  - [ ] Metadata configuration
  - [ ] Unit tests

- [ ] **Task 3: `memory.mem0.search` Action** (AC: 2)
  - [ ] Implement search action
  - [ ] Query and limit handling
  - [ ] Scope filtering
  - [ ] Score normalization
  - [ ] Unit tests

- [ ] **Task 4: `memory.mem0.get_all` Action** (AC: 3)
  - [ ] Implement get_all action
  - [ ] Pagination support
  - [ ] Scope filtering
  - [ ] Unit tests

- [ ] **Task 5: `memory.mem0.update` Action** (AC: 4)
  - [ ] Implement update action
  - [ ] Partial update logic
  - [ ] Unit tests

- [ ] **Task 6: `memory.mem0.delete` Action** (AC: 5)
  - [ ] Implement delete action
  - [ ] Bulk delete support
  - [ ] Unit tests

- [ ] **Task 7: Graph Memory** (AC: 6)
  - [ ] Enable Mem0g integration
  - [ ] Entity/relation extraction
  - [ ] Graph query support
  - [ ] Integration tests

- [ ] **Task 8: Documentation & Examples**
  - [ ] Update YAML_REFERENCE.md
  - [ ] Create example: mem0-conversation-memory.yaml
  - [ ] Create example: mem0-graph-memory.yaml
  - [ ] Create example: mem0-user-personalization.yaml

## Dev Notes

### Source Tree Context

**Python:**
```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py           # Add mem0_actions
│   ├── memory_actions.py     # Reference: native memory
│   ├── mem0_actions.py       # NEW: Mem0 actions
│   └── ...
└── memory/
    └── mem0_client.py        # NEW: Mem0 client wrapper
```

### YAML Syntax Reference

#### Basic Memory Operations
```yaml
settings:
  memory:
    backend: mem0
    user_id: "{{ state.user_id }}"
    api_key: "${MEM0_API_KEY}"

nodes:
  - name: recall_context
    action: memory.mem0.search
    with:
      query: "{{ state.user_question }}"
      limit: 5

  - name: store_conversation
    action: memory.mem0.add
    with:
      messages:
        - role: user
          content: "{{ state.user_input }}"
        - role: assistant
          content: "{{ state.response }}"
```

#### Graph Memory
```yaml
settings:
  memory:
    backend: mem0
    graph: true
    user_id: "{{ state.user_id }}"

nodes:
  - name: recall_with_relations
    action: memory.mem0.search
    with:
      query: "What does the user prefer?"
      include_relations: true
```

### Dependencies

```
pip install the_edge_agent[mem0]
# or
pip install mem0ai>=0.1.0
```

### Integration with Native Memory

When Mem0 is unavailable (not installed or API error), actions gracefully fallback to native `memory.*` actions with a warning log.

## Constraints

- Mem0 is an optional dependency
- Graph memory requires Mem0 v0.1.0+
- API key required for cloud Mem0 (local mode available)

## References

- [Mem0 GitHub](https://github.com/mem0ai/mem0)
- [Mem0 Documentation](https://docs.mem0.ai/)
- [Agentic Design Patterns - Chapter 8: Memory Management](https://github.com/sarwarbeing-ai/Agentic_Design_Patterns)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-05 | 0.1 | Initial story from Sprint Change Proposal | Sarah (PO) |
