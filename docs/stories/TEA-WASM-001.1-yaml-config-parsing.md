# Story TEA-WASM-001.1: YAML Config Parsing

## Status
Ready for Review

## Epic
[TEA-WASM-001: WASM YAML Engine Expansion](./TEA-WASM-001-yaml-engine-expansion.md)

## Story

**As a** browser-based agent developer,
**I want** tea-wasm-llm to parse standard TEA YAML workflow files,
**so that** I can use the same YAML format across Python, Rust, and WASM runtimes without modification.

## Acceptance Criteria

1. Parse standard TEA YAML format with `state_schema`, `variables`, `nodes`, `edges`, `settings` sections
2. `WasmYamlConfig` struct matches Python/Rust schema for interoperability
3. Required field validation with descriptive error messages
4. Invalid YAML returns error with line number context
5. Successfully parse all YAML files in `examples/` directory
6. Config struct accessible for engine execution

## Tasks / Subtasks

- [x] Define `WasmYamlConfig` struct (AC: 2)
  - [x] Add `name: String` field
  - [x] Add `description: Option<String>` field
  - [x] Add `state_schema: Option<HashMap<String, SchemaField>>` field
  - [x] Add `variables: HashMap<String, JsonValue>` field
  - [x] Add `nodes: Vec<WasmNodeConfig>` field
  - [x] Add `edges: Vec<WasmEdgeConfig>` field
  - [x] Add `settings: Option<WasmSettings>` field

- [x] Define `WasmNodeConfig` struct (AC: 2)
  - [x] Add `name: String` field
  - [x] Add `action: Option<String>` field
  - [x] Add `with: Option<HashMap<String, JsonValue>>` (params) field
  - [x] Add `output: Option<String>` field
  - [x] Add `run: Option<String>` for inline code
  - [x] Add `language: Option<String>` (lua/prolog)
  - [x] Add `goto: Option<GotoConfig>` for navigation

- [x] Define `WasmEdgeConfig` struct (AC: 2)
  - [x] Add `from: String` field
  - [x] Add `to: String` field
  - [x] Add `when: Option<String>` condition field

- [x] Implement YAML parsing function (AC: 1, 4)
  - [x] Create `parse_yaml_config(yaml: &str) -> Result<WasmYamlConfig, WasmError>`
  - [x] Use serde_yaml for deserialization
  - [x] Wrap parse errors with line number context

- [x] Implement validation (AC: 3)
  - [x] Validate `name` field is present and non-empty
  - [x] Validate `nodes` array is non-empty
  - [x] Validate node names are unique
  - [x] Validate edge references exist in nodes

- [x] Add WASM bindings (AC: 6)
  - [x] Export `parse_yaml` function via wasm-bindgen
  - [x] Return JSON-serialized config or error

- [x] Test with example files (AC: 5)
  - [x] Test parsing `examples/qa-neurosymbolic-wasm.yaml`
  - [x] Test parsing `examples/prolog/` YAML files
  - [x] Test parsing `examples/parallel_strategies_demo.yaml`

## Dev Notes

### Relevant Source Tree
```
rust/tea-wasm-llm/
├── src/
│   ├── lib.rs          # Main entry, add parse_yaml_config here
│   ├── config.rs       # NEW: WasmYamlConfig structs
│   └── error.rs        # Extend WasmError for parse errors
└── Cargo.toml          # serde_yaml already included
```

### Current Implementation Reference
The existing `LlmYamlConfig` in `lib.rs` (lines ~100-130) provides a starting point but is too limited:
```rust
pub struct LlmYamlConfig {
    pub name: String,
    pub description: Option<String>,
    pub variables: HashMap<String, JsonValue>,
    pub nodes: Vec<LlmNodeConfig>,
    pub edges: Vec<LlmEdgeConfig>,
}
```

### Python Schema Reference
Reference `python/src/the_edge_agent/yaml_engine.py` for the complete schema:
- `state_schema` with type definitions
- `settings.llm` for LLM configuration
- `settings.ltm` for long-term memory
- Complex `goto` structures

### Key Dependencies
- `serde_yaml = "0.9"` - Already in Cargo.toml
- `serde = { version = "1.0", features = ["derive"] }` - Already included

### Error Handling Pattern
```rust
#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("YAML parse error at line {line}: {message}")]
    YamlError { line: usize, message: String },

    #[error("Validation error: {0}")]
    ValidationError(String),
}
```

## Testing

### Test Location
`rust/tea-wasm-llm/tests/test_config_parsing.rs`

### Test Standards
- Use `#[wasm_bindgen_test]` for WASM-specific tests
- Use standard `#[test]` for non-WASM logic
- Test both success and error cases
- Include edge cases (empty nodes, duplicate names, invalid refs)

### Test Cases
```rust
#[test]
fn test_parse_minimal_yaml() { ... }

#[test]
fn test_parse_full_yaml() { ... }

#[test]
fn test_parse_invalid_yaml_returns_error() { ... }

#[test]
fn test_validation_rejects_empty_nodes() { ... }

#[test]
fn test_validation_rejects_duplicate_node_names() { ... }
```

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-17 | 0.1 | Initial story creation | Sarah (PO) |
| 2026-01-17 | 0.2 | Implementation complete - all tasks done | James (Dev) |

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5

### Debug Log References
None - implementation completed without blocking issues

### Completion Notes List
- Created comprehensive `config.rs` module with all structs matching Python/Rust schema
- Implemented `WasmYamlConfig`, `WasmNodeConfig`, `WasmEdgeConfig` with full field support
- Added `SchemaField`, `GotoConfig`, `GotoBranch`, `RunConfig`, `StepConfig` for complex types
- Added `LlmSettings`, `LtmSettings`, `OpikSettings`, `WasmSettings` for settings section
- Implemented `parse_yaml_config()` with line-number error context
- Implemented validation for name, nodes, duplicates, and edge references
- Exported `parse_yaml()` and `validate_yaml()` WASM bindings
- Created 16 unit tests covering all validation scenarios
- Created 10 integration tests parsing real example YAML files

### File List
| File | Action | Description |
|------|--------|-------------|
| `rust/tea-wasm-llm/src/config.rs` | Created | YAML config structs and parsing |
| `rust/tea-wasm-llm/src/lib.rs` | Modified | Added config module and exports |
| `rust/tea-wasm-llm/tests/test_config_parsing.rs` | Created | Integration tests for YAML parsing |

## QA Results

### QA Review Date
2026-01-17

### Reviewer
Quinn (Test Architect)

### Test Design Summary
| Metric | Count |
|--------|-------|
| Total Scenarios | 15 |
| Unit Tests | 11 |
| Integration Tests | 4 |
| P0 (Critical) | 5 |
| P1 (Important) | 6 |
| P2 (Edge cases) | 4 |

### Risk Assessment
| Risk ID | Score | Description |
|---------|-------|-------------|
| TECH-004 | 6 (High) | YAML parsing divergence from Python/Rust - could break portability promise |
| TECH-008 | 2 (Low) | serde_yaml version compatibility |

### Key Test Scenarios
- `1.1-UNIT-001`: Parse minimal valid YAML (P0)
- `1.1-UNIT-003`: WasmYamlConfig fields match expected types (P0)
- `1.1-INT-001`: Parse `examples/simple-agent.yaml` (P0)
- `1.1-INT-004`: Parsed config feeds into executor (P0)

### Recommendations
1. Create canonical test YAML files that must pass on Python, Rust, AND WASM
2. Add property-based testing for schema validation
3. Consider schema version field for future compatibility

### Gate Status
**PASS** - Story is ready for implementation. Risk TECH-004 mitigated by cross-runtime test requirement.

### Reference
Test design: `docs/qa/assessments/TEA-WASM-001-test-design-20260117.md`
