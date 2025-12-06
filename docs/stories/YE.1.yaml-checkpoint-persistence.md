# Story YE.1: YAML Engine Checkpoint Persistence

## Status
Draft

## Story
**As a** developer using YAML-based agent configurations,
**I want** to save and resume workflow execution from checkpoints,
**so that** I can recover from failures, pause/resume long-running workflows, and debug execution at specific points without writing Python code.

## Context
This story extends the YAML Engine (`src/the_edge_agent/yaml_engine.py`) to expose the checkpoint persistence features implemented in TD.10. The core StateGraph already supports `save_checkpoint()`, `load_checkpoint()`, `resume_from_checkpoint()`, and auto-save at interrupts via `checkpoint_dir`. This story makes these features accessible through YAML configuration and provides built-in actions for manual checkpoint operations within workflows.

**Depends on:**
- TD.10: Checkpoint Persistence (completed)

**Integrates with:**
- `YAMLEngine.load_from_file()` and `YAMLEngine.load_from_dict()`
- Core StateGraph checkpoint methods
- Existing YAML `config` section
- Built-in actions registry

**Follows patterns:**
- YAML configuration structure from `docs/YAML_AGENTS.md`
- Built-in action pattern (`uses: action.name` with `with:` parameters)
- Template variable pattern (`{{ variable }}`)

## Acceptance Criteria

### Configuration Support
1. YAML `config.checkpoint_dir` parameter enables auto-save at interrupt points
2. Auto-saved checkpoints use format: `{checkpoint_dir}/{node}_{timestamp}.pkl`
3. YAML `config.checkpoint` parameter allows starting from a saved checkpoint
4. When `checkpoint` is provided, execution resumes from saved state/node

### YAMLEngine API Extensions
5. `YAMLEngine.load_from_file(path, checkpoint=None)` accepts optional checkpoint path
6. `YAMLEngine.load_from_dict(config, checkpoint=None)` accepts optional checkpoint path
7. `YAMLEngine.resume_from_checkpoint(yaml_path, checkpoint_path, config=None)` method added
8. Resume methods return a compiled graph that starts from checkpoint state/node

### Built-in Actions for Manual Checkpoints
9. `checkpoint.save` action saves current state to specified path
10. `checkpoint.load` action loads checkpoint and returns state dict
11. Actions support template variables for dynamic paths: `{{ state.checkpoint_path }}`
12. `checkpoint.save` returns `{"checkpoint_path": str, "saved": bool}`
13. `checkpoint.load` returns `{"checkpoint_state": dict, "checkpoint_node": str}`

### Template Variables
14. `{{ checkpoint.dir }}` resolves to configured checkpoint_dir
15. `{{ checkpoint.last }}` resolves to most recent auto-saved checkpoint path (if any)

### Integration Requirements
16. Existing YAML workflows work unchanged when checkpoint features not used
17. Checkpoint features work with parallel flows (captures main thread state)
18. Checkpoint features work with conditional edges
19. All existing YAMLEngine tests pass without modification

### Quality Requirements
20. Clear error messages for: invalid checkpoint path, corrupt checkpoint, missing checkpoint_dir
21. New functionality covered by unit tests
22. Docstrings document new parameters and methods
23. YAML_AGENTS.md updated with checkpoint documentation

## Tasks / Subtasks

- [ ] **Task 1: Add checkpoint_dir to YAML config** (AC: 1, 2, 16)
  - [ ] Parse `config.checkpoint_dir` in `load_from_dict()`
  - [ ] Pass `checkpoint_dir` to `graph.compile()`
  - [ ] Verify auto-save works with YAML-defined workflows
  - [ ] Test that existing workflows without checkpoint_dir work unchanged

- [ ] **Task 2: Add checkpoint parameter to load methods** (AC: 3, 4, 5, 6)
  - [ ] Add `checkpoint` parameter to `load_from_file(yaml_path, checkpoint=None)`
  - [ ] Add `checkpoint` parameter to `load_from_dict(config, checkpoint=None)`
  - [ ] When checkpoint provided, call graph methods to resume from checkpoint
  - [ ] Return compiled graph positioned at checkpoint state/node

- [ ] **Task 3: Add resume_from_checkpoint method** (AC: 7, 8)
  - [ ] Add `resume_from_checkpoint(yaml_path, checkpoint_path, config=None)` method
  - [ ] Load YAML config, create graph, then resume from checkpoint
  - [ ] Support config override (merge YAML config with provided config)
  - [ ] Return generator that yields events from resume point

- [ ] **Task 4: Implement checkpoint.save built-in action** (AC: 9, 11, 12)
  - [ ] Add `checkpoint.save` to actions registry
  - [ ] Parameters: `path` (required), supports template variables
  - [ ] Action calls `graph.save_checkpoint()` with current state
  - [ ] Returns `{"checkpoint_path": str, "saved": True}`
  - [ ] Handle errors gracefully with `{"saved": False, "error": str}`

- [ ] **Task 5: Implement checkpoint.load built-in action** (AC: 10, 11, 13)
  - [ ] Add `checkpoint.load` to actions registry
  - [ ] Parameters: `path` (required), supports template variables
  - [ ] Action calls `StateGraph.load_checkpoint()` class method
  - [ ] Returns `{"checkpoint_state": dict, "checkpoint_node": str}`
  - [ ] Handle errors gracefully

- [ ] **Task 6: Add checkpoint template variables** (AC: 14, 15)
  - [ ] Add `checkpoint` namespace to template context
  - [ ] `{{ checkpoint.dir }}` resolves to checkpoint_dir config value
  - [ ] `{{ checkpoint.last }}` resolves to most recent auto-saved path
  - [ ] Track last auto-saved path in YAMLEngine instance

- [ ] **Task 7: Verify integration scenarios** (AC: 17, 18, 19)
  - [ ] Test checkpoint with parallel flows in YAML
  - [ ] Test checkpoint with conditional edges in YAML
  - [ ] Verify all existing YAMLEngine tests pass
  - [ ] Test round-trip: save checkpoint, modify state, resume

- [ ] **Task 8: Add tests** (AC: 19, 21)
  - [ ] Test checkpoint_dir in YAML config enables auto-save
  - [ ] Test load_from_file with checkpoint parameter
  - [ ] Test load_from_dict with checkpoint parameter
  - [ ] Test resume_from_checkpoint method
  - [ ] Test checkpoint.save action
  - [ ] Test checkpoint.load action
  - [ ] Test template variables {{ checkpoint.dir }} and {{ checkpoint.last }}
  - [ ] Test error handling for invalid/missing checkpoints
  - [ ] Verify existing tests still pass

- [ ] **Task 9: Update documentation** (AC: 22, 23)
  - [ ] Add docstrings to new methods and parameters
  - [ ] Update docs/YAML_AGENTS.md with Checkpoint section
  - [ ] Add YAML checkpoint examples
  - [ ] Update CLAUDE.md if needed

## Dev Notes

### File Locations
- `src/the_edge_agent/yaml_engine.py` - main implementation
- `tests/test_yaml_engine.py` - new test file (or add to existing)
- `docs/YAML_AGENTS.md` - documentation updates

### YAML Configuration Example
```yaml
name: checkpoint-enabled-workflow
description: Workflow with checkpoint persistence

config:
  checkpoint_dir: ./checkpoints  # Auto-save at interrupts
  checkpoint: ./checkpoints/resume_point.pkl  # Resume from this checkpoint
  interrupt_before: [critical_node]
  interrupt_after: [validation_node]

nodes:
  - name: process_data
    run: |
      # Process something
      return {"processed": True}

  - name: save_progress
    uses: checkpoint.save
    with:
      path: "./checkpoints/{{ state.step_name }}.pkl"
    output: save_result

  - name: load_previous
    uses: checkpoint.load
    with:
      path: "{{ variables.checkpoint_path }}"
    output: loaded_checkpoint

edges:
  - from: __start__
    to: process_data
  - from: process_data
    to: save_progress
  - from: save_progress
    to: __end__
```

### Built-in Action Signatures
```python
def checkpoint_save(state, path, graph=None, node=None, config=None, **kwargs):
    """Save checkpoint to specified path."""
    # graph and node injected by engine
    graph.save_checkpoint(path, state, node, config or {})
    return {"checkpoint_path": path, "saved": True}

def checkpoint_load(state, path, **kwargs):
    """Load checkpoint from specified path."""
    checkpoint = StateGraph.load_checkpoint(path)
    return {
        "checkpoint_state": checkpoint["state"],
        "checkpoint_node": checkpoint["node"],
        "checkpoint_config": checkpoint.get("config", {})
    }
```

### Implementation Considerations
1. **Action context injection**: checkpoint.save needs access to `graph`, `node`, `config` - extend action calling to inject these
2. **Template context**: Add `checkpoint` dict to template evaluation context alongside `state`, `variables`, `secrets`
3. **Last checkpoint tracking**: Store `_last_checkpoint_path` on YAMLEngine instance, update on auto-save

### Testing Strategy
- Use `tempfile.TemporaryDirectory()` for checkpoint files
- Create simple test YAML configs inline (not from files)
- Test both success and error paths
- Verify checkpoint data integrity after round-trip

## Definition of Done
- [ ] All acceptance criteria met
- [ ] All tasks completed
- [ ] Existing tests pass (`pytest tests/`)
- [ ] New tests pass
- [ ] No regressions in existing functionality
- [ ] Code follows existing patterns
- [ ] Documentation updated

## Risk Assessment
- **Primary Risk:** Action context injection complexity - checkpoint.save needs graph/node context
- **Mitigation:** Extend `_create_action_function` to optionally inject graph context
- **Rollback:** Feature is additive; can be removed without breaking existing YAML workflows

## Change Log
| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
