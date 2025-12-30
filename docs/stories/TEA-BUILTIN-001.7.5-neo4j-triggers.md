# Story TEA-BUILTIN-001.7.5: Neo4j APOC Triggers Support

## Status

**Ready for Development**

*Updated: 2024-12-30 | Checklist validation passed - All criteria met*

## Story

**As a** YAML agent developer building reactive applications,
**I want** to register database triggers that fire on graph changes,
**so that** I can build event-driven agent behaviors that respond to node/relationship creation, updates, and deletions in real-time.

## Story Context

**Existing System Integration:**

- Integrates with: `Neo4jBackend` from TEA-BUILTIN-001.7.1
- Technology: APOC Core/Extended triggers (`apoc.trigger.*`)
- Follows pattern: Optional feature with graceful degradation
- Touch points: `memory/graph.py`, `actions/graph_actions.py`

**Prerequisites:**
- APOC Core 5.x or APOC Extended installed
- Trigger configuration enabled in Neo4j settings

## Acceptance Criteria

### APOC Detection

1. **AC-1**: `check_apoc_available()` detects APOC library presence
2. **AC-2**: `get_apoc_version()` returns installed APOC version
3. **AC-3**: `check_triggers_enabled()` verifies trigger support is enabled
4. **AC-4**: `APOC_AVAILABLE` and `TRIGGERS_ENABLED` properties on Neo4jBackend

### Trigger Registration

5. **AC-5**: `register_trigger(name, query, selector, config)` creates trigger
   - `name`: Unique trigger identifier
   - `query`: Cypher to execute on trigger fire
   - `selector`: What changes to watch (created nodes, deleted rels, etc.)
   - `config`: Phase (before/after), parameters
   - Returns `{"success": True, "trigger_name": str, "registered": True}`

6. **AC-6**: Supported selectors:
   - `createdNodes` - New nodes
   - `createdRelationships` - New relationships
   - `deletedNodes` - Deleted nodes
   - `deletedRelationships` - Deleted relationships
   - `assignedLabels` - Labels added
   - `removedLabels` - Labels removed
   - `assignedNodeProperties` - Node properties set
   - `assignedRelationshipProperties` - Relationship properties set

7. **AC-7**: `unregister_trigger(name)` removes trigger
8. **AC-8**: `list_triggers()` returns all registered triggers
9. **AC-9**: `pause_trigger(name)` temporarily disables trigger
10. **AC-10**: `resume_trigger(name)` re-enables paused trigger

### Trigger Execution

11. **AC-11**: Triggers can execute Cypher to:
    - Update other nodes/relationships
    - Write to log nodes
    - Call external procedures
12. **AC-12**: Triggers receive transaction context:
    - `$createdNodes`, `$deletedNodes` lists
    - `$createdRelationships`, `$deletedRelationships` lists
    - `$assignedLabels`, `$removedLabels` maps
    - `$assignedNodeProperties`, `$assignedRelationshipProperties` maps

### Callback Mechanisms

13. **AC-13**: `register_trigger_callback(name, callback_url, config)` registers HTTP callback
    - Trigger fires webhook POST to `callback_url`
    - Payload includes triggered data
14. **AC-14**: `register_trigger_state_update(name, state_key, transform)` updates agent state
    - Trigger writes to specified state key
    - Optional transform function for data

### Lifecycle Management

15. **AC-15**: `cleanup_triggers(prefix)` removes triggers by prefix
    - Used for session/agent cleanup
16. **AC-16**: Auto-cleanup option on backend close
17. **AC-17**: Trigger names include agent/session prefix for isolation

### Configuration

18. **AC-18**: YAML configuration for triggers:
    ```yaml
    settings:
      graph:
        backend: neo4j
        triggers:
          enabled: true
          prefix: "tea_agent_"
          cleanup_on_close: true
          default_phase: "after"
    ```

### Action Registration

19. **AC-19**: Register trigger actions:
    - `graph.register_trigger`
    - `graph.unregister_trigger`
    - `graph.list_triggers`
    - `graph.pause_trigger`
    - `graph.resume_trigger`

### Graceful Degradation

20. **AC-20**: Clear error when APOC not available
21. **AC-21**: Clear error when triggers not enabled in Neo4j config
22. **AC-22**: Warning when trigger registration fails

## Tasks / Subtasks

- [ ] **Task 1: Implement APOC detection** (AC: 1-4)
  - [ ] Add `check_apoc_available()` procedure check
  - [ ] Add `get_apoc_version()`
  - [ ] Add `check_triggers_enabled()` config check
  - [ ] Add property flags

- [ ] **Task 2: Implement trigger registration** (AC: 5-10)
  - [ ] Add `register_trigger()` using `apoc.trigger.add`
  - [ ] Support all selector types
  - [ ] Add `unregister_trigger()` using `apoc.trigger.remove`
  - [ ] Add `list_triggers()`, `pause_trigger()`, `resume_trigger()`

- [ ] **Task 3: Implement trigger execution** (AC: 11-12)
  - [ ] Document Cypher patterns for triggers
  - [ ] Test transaction context variables

- [ ] **Task 4: Implement callback mechanisms** (AC: 13-14)
  - [ ] Add HTTP callback support (via apoc.load.jsonParams)
  - [ ] Add state update mechanism

- [ ] **Task 5: Implement lifecycle management** (AC: 15-17)
  - [ ] Add `cleanup_triggers()` with prefix filter
  - [ ] Add auto-cleanup on close
  - [ ] Implement prefix naming convention

- [ ] **Task 6: Add configuration** (AC: 18)
  - [ ] Parse trigger settings from YAML
  - [ ] Apply defaults

- [ ] **Task 7: Register actions** (AC: 19)
  - [ ] Add trigger actions to `graph_actions.py`

- [ ] **Task 8: Add unit tests**
  - [ ] Test APOC detection
  - [ ] Test trigger CRUD
  - [ ] Test cleanup
  - [ ] Test graceful degradation

## Dev Notes

### APOC Trigger Cypher

```cypher
// Check APOC availability
RETURN apoc.version() AS version

// Check if triggers are enabled
CALL dbms.listConfig('apoc.trigger.enabled')
YIELD value
RETURN value = 'true' AS enabled

// Register trigger
CALL apoc.trigger.add(
  'tea_agent_on_entity_create',
  'UNWIND $createdNodes AS n
   WITH n WHERE n:Entity
   CREATE (log:TriggerLog {
     event: "entity_created",
     entity_id: n.id,
     timestamp: datetime()
   })',
  {phase: 'after'}
)

// List triggers
CALL apoc.trigger.list()
YIELD name, query, selector, params, installed, paused

// Remove trigger
CALL apoc.trigger.remove('tea_agent_on_entity_create')

// Pause/Resume
CALL apoc.trigger.pause('tea_agent_on_entity_create')
CALL apoc.trigger.resume('tea_agent_on_entity_create')
```

### Webhook Callback Pattern

```cypher
// Trigger that calls webhook
CALL apoc.trigger.add(
  'tea_agent_webhook',
  'UNWIND $createdNodes AS n
   WITH n WHERE n:Entity
   CALL apoc.load.jsonParams(
     $callback_url,
     {method: "POST"},
     {event: "created", entity_id: n.id}
   ) YIELD value
   RETURN value',
  {phase: 'after', params: {callback_url: 'http://localhost:8080/webhook'}}
)
```

### Neo4j Configuration Required

```properties
# neo4j.conf
apoc.trigger.enabled=true
apoc.trigger.refresh=60000
```

### Testing

- Test file: `python/tests/test_neo4j_triggers.py`
- Mock APOC procedures
- Test trigger lifecycle
- Test cleanup on close

## Definition of Done

- [ ] APOC detection working
- [ ] Trigger CRUD operations working
- [ ] All selector types supported
- [ ] Cleanup on close working
- [ ] Actions registered and documented
- [ ] Graceful degradation when APOC unavailable
- [ ] Unit tests with >90% coverage

---

## QA Notes

**Test Design Review:** 2024-12-30 | **Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Scenarios | 47 |
| Unit Tests | 23 (49%) |
| Integration Tests | 18 (38%) |
| E2E Tests | 6 (13%) |
| P0 (Critical) | 18 |
| P1 (High) | 16 |
| P2 (Medium) | 10 |
| P3 (Low) | 3 |

### Risk Areas Identified

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| APOC library not installed | High | Medium | Graceful degradation tests (1.7.5-UNIT-002, 1.7.5-UNIT-035, 1.7.5-INT-022) |
| Trigger config disabled in Neo4j | High | Medium | Pre-flight config checks (1.7.5-UNIT-006, 1.7.5-UNIT-036) |
| Transaction context corruption | Critical | Low | Isolation tests (1.7.5-INT-014, 1.7.5-INT-015) |
| Trigger cascade loops | Critical | Low | Guard/timeout patterns recommended |
| Cleanup failure on backend close | Medium | Medium | Lifecycle tests (1.7.5-INT-019, 1.7.5-INT-020, 1.7.5-E2E-006) |
| Trigger prefix collision | Medium | Low | Naming convention tests (1.7.5-UNIT-025, 1.7.5-INT-021) |

### Recommended Test Scenarios

**P0 Critical Path (18 scenarios):**
- APOC detection logic (UNIT-001, 002, 005, 006)
- Core trigger registration (UNIT-009, 010, 017)
- Trigger unregistration (UNIT-020, 024)
- Graceful degradation errors (UNIT-035, 036)
- Integration: Live APOC detection (INT-001)
- Integration: Trigger CRUD (INT-003, 004, 008)
- Integration: Transaction context (INT-014, 015)
- Integration: Webhook callback (INT-018)
- Integration: Lifecycle cleanup (INT-019, 020)
- Integration: Graceful degradation (INT-022)
- E2E: Full reactive workflow (E2E-001)
- E2E: Webhook delivery (E2E-003)
- E2E: Agent shutdown cleanup (E2E-006)

**Test Environment Requirements:**
- Neo4j 5.x with APOC Core/Extended plugin
- Docker container: `neo4j:5.x` with APOC
- Config: `apoc.trigger.enabled=true`, `apoc.trigger.refresh=1000`
- Mock webhook server for callback tests

### Concerns / Blockers

1. **Trigger cascade prevention:** No explicit AC for preventing infinite trigger loops. Recommend adding timeout/guard documentation.
2. **APOC version compatibility:** Story references APOC 5.x; ensure backward compatibility tested if supporting older versions.
3. **Webhook reliability:** No retry/dead-letter queue mechanism specified for failed HTTP callbacks.
4. **Test isolation:** Integration tests require unique trigger prefixes to avoid cross-test interference.

### QA Recommendation

**Status:** Ready for Development

All 22 acceptance criteria have test coverage. P0 scenarios address critical graceful degradation and lifecycle management risks. Recommend addressing trigger cascade prevention documentation before implementation.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-30 | 0.2 | Added QA Notes section with test coverage and risk analysis | Quinn (Test Architect) |
| 2024-12-30 | 0.1 | Initial story creation | PO (Sarah) |
