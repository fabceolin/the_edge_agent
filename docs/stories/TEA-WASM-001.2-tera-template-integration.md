# Story TEA-WASM-001.2: Tera Template Integration

## Status
Draft

## Epic
[TEA-WASM-001: WASM YAML Engine Expansion](./TEA-WASM-001-yaml-engine-expansion.md)

## Story

**As a** browser-based agent developer,
**I want** tea-wasm-llm to use Tera templates instead of regex substitution,
**so that** I can use Jinja2-like syntax with filters, conditionals, and loops in my YAML workflows.

## Acceptance Criteria

### Functional Requirements
1. Tera template engine integrated and functional in WASM
2. `{{ state.key.nested }}` renders nested state access correctly
3. `{{ data | tojson }}` filter serializes objects to JSON
4. `{% if state.active %}...{% endif %}` conditionals evaluate correctly
5. `{% for item in state.items %}...{% endfor %}` loops iterate correctly
6. Single expressions return native objects (object passthrough)
7. Template errors include source context and helpful messages
8. Template caching implemented for performance

### Security Requirements (Critical - SEC-001)
9. Template execution timeout enforced (default: 1000ms, configurable)
10. Loop iteration limit enforced (default: 10,000 iterations max)
11. Template recursion depth limited (default: 64 levels max)
12. Only allowlisted filters available (no arbitrary function calls)
13. State values sanitized before context injection (no prototype pollution)
14. Template compilation errors do not leak sensitive path information

## Tasks / Subtasks

- [ ] Add Tera dependency (AC: 1)
  - [ ] Add `tera = "1.19"` to Cargo.toml
  - [ ] Verify WASM compilation succeeds
  - [ ] Check bundle size impact

- [ ] Create template engine module (AC: 1)
  - [ ] Create `src/templates.rs` module
  - [ ] Initialize Tera instance with custom configuration
  - [ ] Disable autoescaping (not needed for non-HTML)

- [ ] Register custom filters (AC: 3)
  - [ ] Implement `tojson` filter (serialize to JSON string)
  - [ ] Implement `fromjson` filter (parse JSON string)
  - [ ] Register filters with Tera instance

- [ ] Implement template rendering (AC: 2, 4, 5)
  - [ ] Create `render_template(template: &str, context: &Context) -> Result<String>`
  - [ ] Build context from state, variables, secrets
  - [ ] Support nested key access via Tera's dot notation

- [ ] Implement object passthrough (AC: 6)
  - [ ] Detect single expression templates `{{ expr }}`
  - [ ] Return native JsonValue instead of string for single expressions
  - [ ] Fall back to string for multi-expression templates

- [ ] Implement error handling (AC: 7)
  - [ ] Wrap Tera errors with template source context
  - [ ] Include variable name in undefined variable errors
  - [ ] Provide helpful suggestions for common mistakes

- [ ] Implement template caching (AC: 8)
  - [ ] Cache compiled templates by template string hash
  - [ ] Use `once_cell::Lazy` or similar for thread-safe cache
  - [ ] Benchmark cache hit performance

- [ ] Migrate existing template usage
  - [ ] Replace `process_template()` regex implementation
  - [ ] Update action parameter processing
  - [ ] Update condition evaluation

- [ ] **Implement template sandboxing (AC: 9, 10, 11, 12) - SECURITY CRITICAL**
  - [ ] Create `TemplateConfig` struct with security limits
  - [ ] Implement execution timeout via WASM-compatible timer
  - [ ] Add loop iteration counter with configurable max (10,000 default)
  - [ ] Configure Tera recursion depth limit (64 default)
  - [ ] Create explicit filter allowlist (tojson, fromjson, upper, lower, trim, default, length, first, last, join, split, replace)
  - [ ] Disable Tera's `include`, `import`, and `extends` features
  - [ ] Document security model in code comments

- [ ] **Implement state sanitization (AC: 13, 14) - SECURITY CRITICAL**
  - [ ] Create `sanitize_context_value()` function
  - [ ] Strip `__proto__`, `constructor`, `prototype` keys from objects
  - [ ] Validate all keys are valid identifiers (alphanumeric + underscore)
  - [ ] Truncate deeply nested objects (max depth: 32)
  - [ ] Sanitize error messages to remove filesystem paths
  - [ ] Add unit tests for sanitization edge cases

- [ ] **Add security test suite**
  - [ ] Test timeout enforcement with infinite loop template
  - [ ] Test iteration limit with large loop
  - [ ] Test recursion limit with nested includes
  - [ ] Test filter allowlist (blocked filters should error)
  - [ ] Test prototype pollution prevention
  - [ ] Fuzz testing with malformed templates

## Dev Notes

### Relevant Source Tree
```
rust/tea-wasm-llm/
├── src/
│   ├── lib.rs          # Update to use new template module
│   ├── templates.rs    # NEW: Tera template engine
│   └── config.rs       # From Story 1
└── Cargo.toml          # Add tera dependency
```

### Current Implementation
The existing regex-based template in `lib.rs` (around line 200):
```rust
fn process_template(template: &str, state: &JsonValue, variables: &HashMap<String, JsonValue>) -> String {
    // Simple regex: {{ state.key }} or {{ variables.key }}
    let re = Regex::new(r"\{\{\s*(state|variables)\.(\w+(?:\.\w+)*)\s*\}\}").unwrap();
    // ...
}
```

This needs full replacement with Tera.

### Tera Integration Pattern
```rust
use tera::{Tera, Context, Value};
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::sync::RwLock;

static TEMPLATE_CACHE: Lazy<RwLock<HashMap<u64, tera::Template>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

pub fn render_template(
    template_str: &str,
    state: &JsonValue,
    variables: &HashMap<String, JsonValue>,
) -> Result<JsonValue, TemplateError> {
    let mut tera = Tera::default();

    // Register custom filters
    tera.register_filter("tojson", tojson_filter);
    tera.register_filter("fromjson", fromjson_filter);

    // Build context
    let mut context = Context::new();
    context.insert("state", state);
    context.insert("variables", variables);

    // Check for single expression (object passthrough)
    if is_single_expression(template_str) {
        return evaluate_expression(template_str, &context);
    }

    // Render as string
    let result = tera.render_str(template_str, &context)?;
    Ok(JsonValue::String(result))
}
```

### Filter Implementations
```rust
fn tojson_filter(value: &Value, _args: &HashMap<String, Value>) -> tera::Result<Value> {
    Ok(Value::String(serde_json::to_string(value)?))
}

fn fromjson_filter(value: &Value, _args: &HashMap<String, Value>) -> tera::Result<Value> {
    match value {
        Value::String(s) => Ok(serde_json::from_str(s)?),
        _ => Ok(value.clone()),
    }
}
```

### Bundle Size Consideration
Tera adds ~50-80KB to WASM bundle. This is acceptable per epic requirements (< 100KB increase).

### Security Architecture (SEC-001 Mitigation)

**Threat Model:**
- Untrusted YAML workflows may contain malicious templates
- State values from external sources (LLM responses, user input) may contain injection payloads
- Template errors could leak sensitive information

**Defense Layers:**

```rust
/// Security configuration for template rendering
#[derive(Debug, Clone)]
pub struct TemplateSecurityConfig {
    /// Maximum template execution time in milliseconds
    pub timeout_ms: u64,           // Default: 1000
    /// Maximum loop iterations across all loops
    pub max_iterations: usize,     // Default: 10_000
    /// Maximum recursion depth for nested templates
    pub max_recursion: usize,      // Default: 64
    /// Maximum depth for nested state objects
    pub max_state_depth: usize,    // Default: 32
    /// Allowlisted filter names
    pub allowed_filters: HashSet<String>,
}

impl Default for TemplateSecurityConfig {
    fn default() -> Self {
        Self {
            timeout_ms: 1000,
            max_iterations: 10_000,
            max_recursion: 64,
            max_state_depth: 32,
            allowed_filters: [
                "tojson", "fromjson", "upper", "lower", "trim",
                "default", "length", "first", "last", "join",
                "split", "replace", "escape", "safe",
            ].into_iter().map(String::from).collect(),
        }
    }
}
```

**Iteration Tracking Pattern:**
```rust
use std::cell::RefCell;

thread_local! {
    static ITERATION_COUNT: RefCell<usize> = RefCell::new(0);
}

fn track_iteration(max: usize) -> tera::Result<()> {
    ITERATION_COUNT.with(|count| {
        let mut c = count.borrow_mut();
        *c += 1;
        if *c > max {
            return Err(tera::Error::msg(format!(
                "Template iteration limit exceeded ({} iterations)",
                max
            )));
        }
        Ok(())
    })
}

// Register as Tera function called in loops
tera.register_function("__track_iter", |_args| {
    track_iteration(10_000)?;
    Ok(Value::Null)
});
```

**State Sanitization:**
```rust
const DANGEROUS_KEYS: &[&str] = &["__proto__", "constructor", "prototype"];

fn sanitize_value(value: JsonValue, depth: usize, max_depth: usize) -> Result<JsonValue, SecurityError> {
    if depth > max_depth {
        return Err(SecurityError::MaxDepthExceeded(max_depth));
    }

    match value {
        JsonValue::Object(map) => {
            let mut sanitized = serde_json::Map::new();
            for (key, val) in map {
                // Block dangerous keys
                if DANGEROUS_KEYS.contains(&key.as_str()) {
                    continue; // Silently drop
                }
                // Validate key format
                if !is_valid_identifier(&key) {
                    return Err(SecurityError::InvalidKey(key));
                }
                sanitized.insert(key, sanitize_value(val, depth + 1, max_depth)?);
            }
            Ok(JsonValue::Object(sanitized))
        }
        JsonValue::Array(arr) => {
            let sanitized: Result<Vec<_>, _> = arr
                .into_iter()
                .map(|v| sanitize_value(v, depth + 1, max_depth))
                .collect();
            Ok(JsonValue::Array(sanitized?))
        }
        other => Ok(other), // Primitives pass through
    }
}

fn is_valid_identifier(s: &str) -> bool {
    !s.is_empty()
        && s.chars().next().map(|c| c.is_alphabetic() || c == '_').unwrap_or(false)
        && s.chars().all(|c| c.is_alphanumeric() || c == '_')
}
```

**Error Sanitization:**
```rust
fn sanitize_error(err: tera::Error) -> TemplateError {
    let msg = err.to_string();
    // Remove filesystem paths
    let sanitized = regex::Regex::new(r"(/[^\s]+)+")
        .unwrap()
        .replace_all(&msg, "[path-redacted]");
    TemplateError::Render(sanitized.to_string())
}
```

## Testing

### Test Location
`rust/tea-wasm-llm/tests/test_templates.rs`

### Test Standards
- Test all Jinja2-compatible features used in TEA
- Test error cases with helpful messages
- Benchmark template caching effectiveness

### Test Cases
```rust
#[test]
fn test_simple_variable_access() {
    let state = json!({"name": "Alice"});
    let result = render_template("Hello {{ state.name }}", &state, &HashMap::new());
    assert_eq!(result.unwrap(), "Hello Alice");
}

#[test]
fn test_nested_access() {
    let state = json!({"user": {"profile": {"name": "Bob"}}});
    let result = render_template("{{ state.user.profile.name }}", &state, &HashMap::new());
    assert_eq!(result.unwrap(), "Bob");
}

#[test]
fn test_tojson_filter() {
    let state = json!({"data": {"key": "value"}});
    let result = render_template("{{ state.data | tojson }}", &state, &HashMap::new());
    assert_eq!(result.unwrap(), r#"{"key":"value"}"#);
}

#[test]
fn test_conditional() {
    let state = json!({"active": true});
    let result = render_template(
        "{% if state.active %}Yes{% else %}No{% endif %}",
        &state, &HashMap::new()
    );
    assert_eq!(result.unwrap(), "Yes");
}

#[test]
fn test_loop() {
    let state = json!({"items": [1, 2, 3]});
    let result = render_template(
        "{% for i in state.items %}{{ i }},{% endfor %}",
        &state, &HashMap::new()
    );
    assert_eq!(result.unwrap(), "1,2,3,");
}

#[test]
fn test_object_passthrough() {
    let state = json!({"data": {"key": "value"}});
    let result = render_template("{{ state.data }}", &state, &HashMap::new());
    assert_eq!(result.unwrap(), json!({"key": "value"}));
}

#[test]
fn test_undefined_variable_error() {
    let state = json!({});
    let result = render_template("{{ state.missing }}", &state, &HashMap::new());
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("missing"));
}
```

### Security Test Cases (SEC-001)
```rust
#[test]
fn test_iteration_limit_enforced() {
    let state = json!({"items": (0..100_000).collect::<Vec<_>>()});
    let result = render_template(
        "{% for i in state.items %}{{ i }}{% endfor %}",
        &state,
        &HashMap::new()
    );
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("iteration limit"));
}

#[test]
fn test_infinite_loop_timeout() {
    // Tera doesn't have while loops, but recursive macros could loop
    // This tests the timeout mechanism
    let state = json!({});
    let template = r#"
        {% macro infinite(n) %}
            {{ self::infinite(n=n+1) }}
        {% endmacro %}
        {{ self::infinite(n=0) }}
    "#;
    let result = render_template_with_config(
        template,
        &state,
        &HashMap::new(),
        TemplateSecurityConfig { timeout_ms: 100, ..Default::default() }
    );
    assert!(result.is_err());
}

#[test]
fn test_recursion_depth_limit() {
    let state = json!({});
    let template = r#"
        {% macro recurse(depth) %}
            {% if depth < 1000 %}
                {{ self::recurse(depth=depth+1) }}
            {% endif %}
        {% endmacro %}
        {{ self::recurse(depth=0) }}
    "#;
    let result = render_template(template, &state, &HashMap::new());
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("recursion"));
}

#[test]
fn test_blocked_filter_rejected() {
    let state = json!({"cmd": "ls"});
    // Hypothetical dangerous filter that shouldn't exist
    let result = render_template("{{ state.cmd | shell }}", &state, &HashMap::new());
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("filter"));
}

#[test]
fn test_include_disabled() {
    let state = json!({});
    let result = render_template(
        "{% include \"../../../etc/passwd\" %}",
        &state,
        &HashMap::new()
    );
    assert!(result.is_err());
}

#[test]
fn test_prototype_pollution_blocked() {
    let state = json!({
        "__proto__": {"polluted": true},
        "constructor": {"polluted": true},
        "normal": "value"
    });
    let sanitized = sanitize_context_value(&state).unwrap();
    assert!(!sanitized.as_object().unwrap().contains_key("__proto__"));
    assert!(!sanitized.as_object().unwrap().contains_key("constructor"));
    assert!(sanitized.as_object().unwrap().contains_key("normal"));
}

#[test]
fn test_deep_nesting_blocked() {
    // Create deeply nested object (depth > 32)
    let mut deep = json!("leaf");
    for _ in 0..50 {
        deep = json!({"nested": deep});
    }
    let state = json!({"data": deep});
    let result = sanitize_context_value(&state);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("depth"));
}

#[test]
fn test_error_path_sanitization() {
    let state = json!({});
    // Trigger an error that might include paths
    let result = render_template("{% include \"/home/user/secret.txt\" %}", &state, &HashMap::new());
    assert!(result.is_err());
    let err_msg = result.unwrap_err().to_string();
    assert!(!err_msg.contains("/home/"));
    assert!(err_msg.contains("[path-redacted]") || !err_msg.contains("/"));
}

#[test]
fn test_invalid_key_rejected() {
    let state = json!({
        "valid_key": "ok",
        "also-valid": "ok",  // Note: hyphens may be allowed depending on policy
        "123invalid": "bad", // Starts with number
    });
    // Depending on implementation, either sanitize or reject
    let result = sanitize_context_value(&state);
    // At minimum, the key starting with number should be handled
    if let Ok(sanitized) = result {
        assert!(!sanitized.as_object().unwrap().contains_key("123invalid"));
    }
}

#[test]
fn test_allowlisted_filters_work() {
    let state = json!({"name": "alice", "data": {"key": "value"}});

    // All these should work
    assert!(render_template("{{ state.name | upper }}", &state, &HashMap::new()).is_ok());
    assert!(render_template("{{ state.name | lower }}", &state, &HashMap::new()).is_ok());
    assert!(render_template("{{ state.name | trim }}", &state, &HashMap::new()).is_ok());
    assert!(render_template("{{ state.data | tojson }}", &state, &HashMap::new()).is_ok());
    assert!(render_template("{{ state.name | length }}", &state, &HashMap::new()).is_ok());
    assert!(render_template("{{ state.name | default(value='none') }}", &state, &HashMap::new()).is_ok());
}
```

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-17 | 0.1 | Initial story creation | Sarah (PO) |
| 2026-01-17 | 0.2 | Added security requirements (AC 9-14), sandboxing tasks, security architecture, and security test suite per SEC-001 risk assessment | Quinn (QA) |

## Dev Agent Record

### Agent Model Used
_To be filled during implementation_

### Debug Log References
_To be filled during implementation_

### Completion Notes List
_To be filled during implementation_

### File List
_To be filled during implementation_

## QA Results

### QA Review Date
2026-01-17

### Reviewer
Quinn (Test Architect)

### Test Design Summary
| Metric | Count |
|--------|-------|
| Total Scenarios | 24 |
| Unit Tests | 20 |
| Integration Tests | 3 |
| E2E Tests | 1 |
| P0 (Critical) | 13 |
| P1 (Important) | 7 |
| P2 (Edge cases) | 4 |

### Risk Assessment
| Risk ID | Score | Description | Status |
|---------|-------|-------------|--------|
| **SEC-001** | **9 (Critical)** | Template injection via Tera expressions | **MITIGATED** - AC 9-14 added |
| TECH-005 | 4 (Medium) | Tera filter compatibility with Python Jinja2 |
| PERF-002 | 2 (Low) | Template compilation overhead |

### Critical Risk Mitigation (SEC-001)
**Status: ADDRESSED** ✅

Security requirements added (AC 9-14):
- Template execution timeout (1000ms default)
- Loop iteration limit (10,000 max)
- Recursion depth limit (64 levels)
- Filter allowlist (14 safe filters)
- State sanitization (`__proto__`, `constructor` blocked)
- Error message sanitization (no path leakage)

### Key Test Scenarios
- `1.2-UNIT-013`: Iteration limit stops large loop (P0, SEC-001)
- `1.2-UNIT-015`: Blocked filter rejected (P0, SEC-001)
- `1.2-UNIT-017`: `__proto__` key stripped (P0, SEC-001)
- `1.2-UNIT-019`: Deep nesting rejected (P0, SEC-001)
- `1.2-UNIT-020`: Error message doesn't contain paths (P0, SEC-001)

### Recommendations
1. Implement security test suite BEFORE functional features
2. Add fuzzing tests for template injection
3. Consider security audit before production use

### Gate Status
**PASS** - Story is ready for implementation. Critical risk SEC-001 has been addressed with comprehensive security requirements and 9 dedicated security test scenarios.

### Reference
Test design: `docs/qa/assessments/TEA-WASM-001-test-design-20260117.md`
