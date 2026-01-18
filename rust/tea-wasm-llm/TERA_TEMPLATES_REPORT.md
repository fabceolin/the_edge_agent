# Full Tera Templates Implementation Report

**Date:** 2026-01-17
**Issue:** WebAssembly.Table.grow(): failed to grow table by 4
**Status:** Under Investigation

## Summary

Attempted to enable full Tera template engine support in the WASM demo's `execute_return` action. The change results in a WebAssembly table growth error during initialization.

---

## Changes Made

### 1. `rust/tea-wasm-llm/src/lib.rs` (line 1126-1128)

**Location:** `execute_return` function

**Before:**
```rust
fn execute_return(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    if let Some(params) = &node.params {
        if let Some(value) = params.get("value") {
            let processed = process_template_value(value, &state, variables);
            // ...
        }
    }
    Ok(state)
}
```

**After:**
```rust
fn execute_return(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    if let Some(params) = &node.params {
        if let Some(value) = params.get("value") {
            // Use the full Tera template engine from templates module
            let processed = templates::process_template_value(value, &state, variables)
                .map_err(|e| JsValue::from_str(&format!("Template error: {}", e)))?;
            // ...
        }
    }
    Ok(state)
}
```

**Difference:**
- `process_template_value` (local) → `templates::process_template_value` (full Tera)

---

### 2. Local vs Templates Module Implementation

#### Local `process_template_value` (lib.rs ~line 1160)

Uses simple regex-based template processing:
```rust
fn process_template_value(
    value: &JsonValue,
    state: &JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> JsonValue {
    match value {
        JsonValue::String(s) => {
            let processed = process_template(s, state, variables);
            JsonValue::String(processed)
        }
        // recursive handling for objects/arrays...
    }
}

fn process_template(
    template: &str,
    state: &JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> String {
    // Simple regex: {{ state.key }} or {{ variables.key }}
    let re = regex::Regex::new(r"\{\{\s*(\w+)\.(\w+(?:\.\w+)*)\s*\}\}").unwrap();
    // ...
}
```

**Capabilities:**
- `{{ state.key }}` - basic variable access
- `{{ state.nested.key }}` - nested access
- `{{ variables.name }}` - variable access

**Limitations:**
- No filters (`| upper`, `| lower`, etc.)
- No conditionals (`{% if %}`)
- No loops (`{% for %}`)

#### Templates Module `process_template_value` (templates.rs ~line 346)

Uses full Tera template engine:
```rust
pub fn process_template_value(
    value: &serde_json::Value,
    state: &serde_json::Value,
    variables: &std::collections::HashMap<String, serde_json::Value>,
) -> TemplateResult<serde_json::Value> {
    match value {
        serde_json::Value::String(s) if is_template(s) => {
            let mut tera = Tera::default();
            // Register custom filters...
            // Render template...
        }
        // ...
    }
}
```

**Capabilities:**
- All Jinja2/Tera syntax
- Filters: `| upper`, `| lower`, `| tojson`, `| fromjson`, `| default`, `| length`
- Conditionals: `{% if %}`, `{% else %}`, `{% endif %}`
- Loops: `{% for item in items %}`, `{% endfor %}`
- Object passthrough for single expressions

---

### 3. New `initYamlEngine` Function

**File:** `rust/tea-wasm-llm/js/index.ts` (line 482-494)

```typescript
/**
 * Initialize just the YAML workflow engine without requiring an LLM handler.
 */
export async function initYamlEngine(
  config: { verbose?: boolean } = {}
): Promise<void> {
  if (!initialized) {
    await init();
    initialized = true;

    if (config.verbose) {
      console.log('[TEA-WASM-LLM] WASM module initialized (YAML engine only)');
      console.log('[TEA-WASM-LLM] Version:', version());
    }
  }
}
```

**Purpose:** Allow YAML workflows to run without requiring LLM initialization.

---

### 4. App.js Initialization Restructure

**File:** `docs/extra/wasm-demo/app.js` (line 1362-1430)

**Before:** Single try-catch, LLM failure blocks everything

**After:** Separate try-catch blocks:
1. Initialize YAML engine first (enables button immediately)
2. Try to load LLM (may fail, but YAML workflows still work)

---

## Build Information

### WASM Sizes

| Build | Size |
|-------|------|
| Original (git) | ~4.4 MB |
| With Tera change | 4407.4 KB |
| Dev build (unoptimized) | 18528.8 KB |

**Note:** The WASM size is essentially unchanged, suggesting the Tera code was already compiled in.

### Build Command
```bash
cd rust/tea-wasm-llm
wasm-pack build --target web --release
node scripts/bundle-for-demo.cjs
```

### Build Output
```
Finished `release` profile [optimized] target(s) in 7.07s
[INFO]: Optimizing wasm binaries with `wasm-opt`...
[INFO]: Done in 21.10s
```

---

## Error Details

### Error Message
```
Error: WebAssembly.Table.grow(): failed to grow table by 4
```

### When It Occurs
- During WASM module initialization (`init()` call)
- Before any template processing happens

### Observations
1. The error occurs during `initYamlEngine()` or `initTeaLlm()` call
2. Node.js tests pass successfully (`npm test` - 54 tests pass)
3. The error is browser-specific
4. The error persists across page reloads in the same browser session
5. Navigating to `about:blank` and back doesn't clear the issue

---

## Templates Module Already Exported

The templates module is already exported in lib.rs:
```rust
pub use templates::{
    render_template, render_template_wasm, render_template_with_config,
    sanitize_context_value, TemplateError, TemplateResult, TemplateSecurityConfig,
};
```

This means the Tera engine code is already compiled into the WASM binary, even before this change.

---

## Files Modified

```
rust/tea-wasm-llm/src/lib.rs          # Changed execute_return to use templates::
rust/tea-wasm-llm/js/index.ts         # Added initYamlEngine function
docs/extra/wasm-demo/app.js           # Restructured initialization
docs/extra/wasm-demo/pkg/*            # Rebuilt WASM bundle
scripts/copy-wllama-assets.sh         # Fixed wllama package path
```

---

## Investigation Questions

1. **Why does calling `templates::process_template_value` cause table growth issues?**
   - The code is already compiled into the binary
   - Only the call site changed, not the compiled code

2. **Is this a wasm-bindgen issue?**
   - The function signature includes `Result<T, E>` which requires error handling
   - The `.map_err()` call might be creating new closures

3. **Is there a conflict with wllama?**
   - Both modules use WebAssembly tables
   - The index.js bundles wllama code inline

4. **Browser-specific behavior:**
   - Works in Node.js
   - Fails in Chrome (via DevTools MCP)
   - Might be memory pressure from debugging session

---

## Test Commands

```bash
# Run Node.js tests (should pass)
cd rust/tea-wasm-llm
npm test

# Build WASM
wasm-pack build --target web --release

# Bundle for demo
node scripts/bundle-for-demo.cjs

# Start test server
cd docs/extra/wasm-demo
python3 -m http.server 8082
```

---

## Demo URL

http://localhost:8082/

---

## Git Status

```bash
# Current changes
git diff src/lib.rs
git diff js/index.ts
git diff ../../docs/extra/wasm-demo/app.js

# Restore original pkg (for comparison)
cd docs/extra/wasm-demo && git checkout pkg/
```
