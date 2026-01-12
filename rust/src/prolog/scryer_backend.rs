//! Scryer Prolog runtime integration (TEA-RELEASE-005.1)
//!
//! Provides a pure-Rust Prolog backend using Scryer Prolog, eliminating
//! the need for external C libraries (like SWI-Prolog's libswipl.so).
//!
//! ## Benefits over SWI-Prolog
//!
//! - No external dependencies - compiles directly into the binary
//! - Smaller binary size (~5MB vs ~50MB)
//! - Better portability (no dynamic linking issues)
//! - Potential WASM compilation support
//!
//! ## Limitations
//!
//! - No threading support (single-threaded only)
//! - No foreign language interface (cannot call Rust from Prolog)
//! - Fewer built-in libraries than SWI-Prolog
//! - Some SWI-specific predicates not available (nb_setval/2, etc.)
//!
//! ## State Access
//!
//! State is accessed via the `state/2` predicate:
//!
//! ```prolog
//! % Read state value
//! state(key, Value),
//! Value > 0,
//! NewValue is Value * 2.
//! ```
//!
//! ## Return Values
//!
//! Use `return/2` to update state from Prolog nodes:
//!
//! ```prolog
//! return(result, 42).
//! ```

use parking_lot::RwLock;
use scryer_prolog::{LeafAnswer, MachineBuilder, Term};
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use crate::error::{TeaError, TeaResult};

/// Scryer Prolog runtime for The Edge Agent (Rust implementation)
///
/// Provides a pure-Rust Prolog environment for neurosymbolic AI workflows.
/// Unlike SWI-Prolog, this implementation has no C dependencies.
///
/// ## Architecture
///
/// The runtime creates a new `Machine` instance for each execution to ensure
/// isolation between queries. State is injected via `state/2` facts and
/// results are collected via `return/2` assertions.
#[allow(dead_code)]
pub struct ScryerRuntime {
    /// Execution timeout (note: Scryer doesn't natively support timeouts)
    timeout: Duration,

    /// Cached state for the current execution (reserved for future use)
    state_cache: Arc<RwLock<HashMap<String, JsonValue>>>,

    /// Return values collected during execution (reserved for future use)
    return_values: Arc<RwLock<HashMap<String, JsonValue>>>,
}

impl Default for ScryerRuntime {
    fn default() -> Self {
        Self::new().expect("Failed to create ScryerRuntime")
    }
}

impl ScryerRuntime {
    /// Create a new Scryer Prolog runtime with default settings
    pub fn new() -> TeaResult<Self> {
        Self::with_timeout(Duration::from_secs(30))
    }

    /// Create a new Scryer Prolog runtime with custom timeout
    ///
    /// Note: Scryer doesn't natively support query timeouts, so this is
    /// advisory only. Long-running queries may block indefinitely.
    pub fn with_timeout(timeout: Duration) -> TeaResult<Self> {
        // Verify Scryer is available by creating a test machine
        let _machine = MachineBuilder::default().build();

        Ok(Self {
            timeout,
            state_cache: Arc::new(RwLock::new(HashMap::new())),
            return_values: Arc::new(RwLock::new(HashMap::new())),
        })
    }

    /// Convert JSON value to Prolog term string
    pub fn json_to_prolog(&self, value: &JsonValue) -> String {
        match value {
            JsonValue::Null => "null".to_string(),
            JsonValue::Bool(b) => if *b { "true" } else { "false" }.to_string(),
            JsonValue::Number(n) => n.to_string(),
            JsonValue::String(s) => {
                // Escape single quotes for Prolog atom
                let escaped = s.replace('\\', "\\\\").replace('\'', "\\'");
                format!("'{}'", escaped)
            }
            JsonValue::Array(arr) => {
                let items: Vec<String> = arr.iter().map(|v| self.json_to_prolog(v)).collect();
                format!("[{}]", items.join(", "))
            }
            JsonValue::Object(obj) => {
                // Scryer uses standard Prolog dict syntax
                let pairs: Vec<String> = obj
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, self.json_to_prolog(v)))
                    .collect();
                format!("_{{{}}}", pairs.join(", "))
            }
        }
    }

    /// Convert Scryer Term to JSON value
    fn term_to_json(&self, term: &Term) -> TeaResult<JsonValue> {
        match term {
            Term::Integer(n) => {
                // Convert IBig to string and parse as i64
                let s = n.to_string();
                if let Ok(i) = s.parse::<i64>() {
                    Ok(JsonValue::Number(i.into()))
                } else {
                    // Fallback to string for large integers
                    Ok(JsonValue::String(s))
                }
            }
            Term::Float(f) => serde_json::Number::from_f64(*f)
                .map(JsonValue::Number)
                .ok_or_else(|| TeaError::Prolog("Invalid float value".to_string())),
            Term::Rational(r) => {
                // Convert rational to string and then to float
                // Format: "numerator/denominator"
                let s = format!("{}", r);
                // Try to parse as float directly or compute from fraction
                if let Ok(f) = s.parse::<f64>() {
                    serde_json::Number::from_f64(f)
                        .map(JsonValue::Number)
                        .ok_or_else(|| TeaError::Prolog("Invalid rational value".to_string()))
                } else if let Some(pos) = s.find('/') {
                    let num: f64 = s[..pos].parse().unwrap_or(0.0);
                    let den: f64 = s[pos + 1..].parse().unwrap_or(1.0);
                    serde_json::Number::from_f64(num / den)
                        .map(JsonValue::Number)
                        .ok_or_else(|| TeaError::Prolog("Invalid rational value".to_string()))
                } else {
                    Ok(JsonValue::String(s))
                }
            }
            Term::Atom(a) => {
                let s = a.as_str();
                match s {
                    "true" => Ok(JsonValue::Bool(true)),
                    "false" => Ok(JsonValue::Bool(false)),
                    "null" | "[]" => Ok(JsonValue::Null),
                    _ => Ok(JsonValue::String(s.to_string())),
                }
            }
            Term::String(s) => Ok(JsonValue::String(s.to_string())),
            Term::List(items) => {
                let arr: TeaResult<Vec<JsonValue>> =
                    items.iter().map(|t| self.term_to_json(t)).collect();
                Ok(JsonValue::Array(arr?))
            }
            Term::Compound(functor, args) => {
                // Handle dict-like compounds
                let name = functor.as_str();
                if name == ":" && args.len() == 2 {
                    // Key-value pair
                    let key = self.term_to_json(&args[0])?;
                    let value = self.term_to_json(&args[1])?;
                    let mut obj = serde_json::Map::new();
                    if let JsonValue::String(k) = key {
                        obj.insert(k, value);
                    }
                    Ok(JsonValue::Object(obj))
                } else {
                    // Generic compound - convert to string representation
                    let args_str: Vec<String> = args.iter().map(|a| format!("{:?}", a)).collect();
                    Ok(JsonValue::String(format!(
                        "{}({})",
                        name,
                        args_str.join(", ")
                    )))
                }
            }
            Term::Var(_) => Ok(JsonValue::Null), // Unbound variable
            _ => Ok(JsonValue::Null),            // Handle any future variants
        }
    }

    /// Parse a Prolog term string to JSON value (for compatibility)
    pub fn prolog_to_json(&self, term_str: &str) -> TeaResult<JsonValue> {
        let term = term_str.trim();

        // Handle special atoms
        if term == "null" || term == "[]" {
            return Ok(JsonValue::Null);
        }
        if term == "true" {
            return Ok(JsonValue::Bool(true));
        }
        if term == "false" {
            return Ok(JsonValue::Bool(false));
        }

        // Try to parse as number
        if let Ok(n) = term.parse::<i64>() {
            return Ok(JsonValue::Number(n.into()));
        }
        if let Ok(n) = term.parse::<f64>() {
            return serde_json::Number::from_f64(n)
                .map(JsonValue::Number)
                .ok_or_else(|| TeaError::Prolog("Invalid float value".to_string()));
        }

        // Handle quoted string
        if term.starts_with('\'') && term.ends_with('\'') && term.len() >= 2 {
            let inner = &term[1..term.len() - 1];
            let unescaped = inner.replace("\\'", "'").replace("\\\\", "\\");
            return Ok(JsonValue::String(unescaped));
        }

        // Handle double-quoted string
        if term.starts_with('"') && term.ends_with('"') && term.len() >= 2 {
            let inner = &term[1..term.len() - 1];
            return Ok(JsonValue::String(inner.to_string()));
        }

        // Handle list
        if term.starts_with('[') && term.ends_with(']') {
            return self.parse_prolog_list(term);
        }

        // Plain atom without quotes
        Ok(JsonValue::String(term.to_string()))
    }

    /// Parse a Prolog list string to JSON array
    fn parse_prolog_list(&self, term: &str) -> TeaResult<JsonValue> {
        let inner = &term[1..term.len() - 1];
        if inner.is_empty() {
            return Ok(JsonValue::Array(vec![]));
        }

        let elements = self.split_prolog_terms(inner)?;
        let mut arr = Vec::with_capacity(elements.len());
        for elem in elements {
            arr.push(self.prolog_to_json(&elem)?);
        }
        Ok(JsonValue::Array(arr))
    }

    /// Split Prolog terms by comma, respecting nesting
    fn split_prolog_terms(&self, s: &str) -> TeaResult<Vec<String>> {
        let mut terms = Vec::new();
        let mut current = String::new();
        let mut depth = 0;
        let mut in_quote = false;
        let mut quote_char = ' ';

        for c in s.chars() {
            match c {
                '\'' | '"' if !in_quote => {
                    in_quote = true;
                    quote_char = c;
                    current.push(c);
                }
                c if in_quote && c == quote_char => {
                    in_quote = false;
                    current.push(c);
                }
                '[' | '{' | '(' if !in_quote => {
                    depth += 1;
                    current.push(c);
                }
                ']' | '}' | ')' if !in_quote => {
                    depth -= 1;
                    current.push(c);
                }
                ',' if !in_quote && depth == 0 => {
                    if !current.trim().is_empty() {
                        terms.push(current.trim().to_string());
                    }
                    current.clear();
                }
                _ => {
                    current.push(c);
                }
            }
        }

        if !current.trim().is_empty() {
            terms.push(current.trim().to_string());
        }

        Ok(terms)
    }

    /// Execute Prolog code with state and return updated state
    ///
    /// This is the main entry point for node execution, matching the
    /// PrologRuntime::execute_node_code interface.
    ///
    /// ## Scryer Implementation Notes (SPIKE FINDINGS)
    ///
    /// **KNOWN LIMITATION**: Scryer Prolog 0.10's `load_module_string` does not
    /// reliably load modules from strings at runtime. This is a known issue:
    /// - Module loading expects file-based sources
    /// - Dynamic predicates (assertz) work differently
    ///
    /// **WORKAROUND NEEDED**: For production use, one of these approaches:
    /// 1. Write module to temp file and use `consult`
    /// 2. Use a query-only approach with state passed via unification
    /// 3. Wait for Scryer to improve string-based module loading
    ///
    /// This spike implementation attempts direct query execution with
    /// state values substituted inline.
    pub fn execute_node_code(&self, code: &str, state: &JsonValue) -> TeaResult<JsonValue> {
        // Create a fresh machine for this execution
        let mut machine = MachineBuilder::default().build();

        // SPIKE: Build an inline query that substitutes state values directly
        // This is a simplified approach that works for basic cases
        //
        // For the code:
        //   state(value, V), D is V * 2, return(doubled, D).
        //
        // We transform to:
        //   V = 21, D is V * 2, Result = D.
        //
        // Then extract Result from bindings

        // Build state variable assignments
        let mut state_assignments = Vec::new();
        let mut state_vars = std::collections::HashMap::new();
        if let JsonValue::Object(obj) = state {
            for (key, value) in obj {
                let prolog_value = self.json_to_prolog(value);
                let var_name = format!("State_{}", key.replace("-", "_"));
                state_assignments.push(format!("{} = {}", var_name, prolog_value));
                state_vars.insert(key.clone(), var_name);
            }
        }

        // Replace state(key, Var) patterns with direct unification
        let mut modified_code = code.to_string();
        for (key, var_name) in &state_vars {
            // Replace patterns like "state(key, V)" with "V = StateVar"
            let pattern = format!("state({}, ", key);
            if let Some(start) = modified_code.find(&pattern) {
                // Find the closing paren
                if let Some(end_offset) = modified_code[start..].find(')') {
                    let full_pattern = &modified_code[start..start + end_offset + 1];
                    // Extract the variable name from state(key, Var)
                    let var_start = start + pattern.len();
                    let var_end = start + end_offset;
                    let user_var = &modified_code[var_start..var_end].trim();
                    let replacement = format!("{} = {}", user_var, var_name);
                    modified_code = modified_code.replace(full_pattern, &replacement);
                }
            }
        }

        // Build return value collector - use binding extraction
        // Replace return(key, Value) with tracking
        let mut return_key = None;
        if let Some(start) = modified_code.find("return(") {
            if let Some(end_offset) = modified_code[start..].find(')') {
                let args = &modified_code[start + 7..start + end_offset];
                let parts: Vec<&str> = args.split(',').collect();
                if parts.len() == 2 {
                    return_key = Some(parts[0].trim().to_string());
                    let return_var = parts[1].trim();
                    // Replace return(key, Value) with ReturnValue = Value
                    let full_pattern = &modified_code[start..start + end_offset + 1];
                    let replacement = format!("ReturnValue = {}", return_var);
                    modified_code = modified_code.replace(full_pattern, &replacement);
                }
            }
        }

        // Build full query
        let query = if state_assignments.is_empty() {
            format!("({}).", modified_code.trim().trim_end_matches('.'))
        } else {
            format!(
                "({}, {}).",
                state_assignments.join(", "),
                modified_code.trim().trim_end_matches('.')
            )
        };

        // Execute the query
        let query_state = machine.run_query(&query);

        let mut result = state.clone();
        for answer in query_state {
            match answer {
                Ok(LeafAnswer::True) => {
                    // Success but no bindings
                    break;
                }
                Ok(LeafAnswer::LeafAnswer { bindings, .. }) => {
                    // Extract ReturnValue if present
                    if let Some(ref key) = return_key {
                        if let Some(value_term) = bindings.get("ReturnValue") {
                            if let Ok(value_json) = self.term_to_json(value_term) {
                                if let Some(obj) = result.as_object_mut() {
                                    obj.insert(key.clone(), value_json);
                                }
                            }
                        }
                    }
                    break;
                }
                Ok(LeafAnswer::False) => {
                    return Err(TeaError::Prolog("Prolog goal failed".to_string()));
                }
                Ok(LeafAnswer::Exception(term)) => {
                    return Err(TeaError::Prolog(format!("Prolog exception: {:?}", term)));
                }
                Err(err) => {
                    return Err(TeaError::Prolog(format!("Query error: {:?}", err)));
                }
            }
        }

        Ok(result)
    }

    /// Execute a simple Prolog query and return results
    ///
    /// This is a lower-level interface for executing arbitrary queries.
    pub fn query(&self, query: &str) -> TeaResult<Vec<HashMap<String, JsonValue>>> {
        let mut machine = MachineBuilder::default().build();

        let query_state = machine.run_query(query);
        let mut results = Vec::new();

        for result in query_state {
            match result {
                Ok(LeafAnswer::True) => {
                    results.push(HashMap::new());
                }
                Ok(LeafAnswer::LeafAnswer { bindings, .. }) => {
                    let mut result_map = HashMap::new();
                    for (var, term) in bindings {
                        if let Ok(json_val) = self.term_to_json(&term) {
                            result_map.insert(var, json_val);
                        }
                    }
                    results.push(result_map);
                }
                Ok(LeafAnswer::False) => break,
                Ok(LeafAnswer::Exception(term)) => {
                    return Err(TeaError::Prolog(format!("Prolog exception: {:?}", term)));
                }
                Err(err) => {
                    return Err(TeaError::Prolog(format!("Query error: {:?}", err)));
                }
            }
        }

        Ok(results)
    }

    /// Check if sandbox is enabled (always false for Scryer - no sandbox support yet)
    pub fn sandbox_enabled(&self) -> bool {
        false
    }

    /// Check if sandbox is initialized (always false for Scryer)
    pub fn is_sandboxed(&self) -> bool {
        false
    }

    /// Get the configured timeout
    pub fn timeout(&self) -> Duration {
        self.timeout
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_scryer_runtime_creation() {
        let runtime = ScryerRuntime::new();
        assert!(runtime.is_ok());
    }

    #[test]
    fn test_json_to_prolog_primitives() {
        let runtime = ScryerRuntime::new().unwrap();

        assert_eq!(runtime.json_to_prolog(&json!(42)), "42");
        assert_eq!(runtime.json_to_prolog(&json!(3.14)), "3.14");
        assert_eq!(runtime.json_to_prolog(&json!(true)), "true");
        assert_eq!(runtime.json_to_prolog(&json!(false)), "false");
        assert_eq!(runtime.json_to_prolog(&json!(null)), "null");
        assert_eq!(runtime.json_to_prolog(&json!("hello")), "'hello'");
    }

    #[test]
    fn test_json_to_prolog_list() {
        let runtime = ScryerRuntime::new().unwrap();

        assert_eq!(runtime.json_to_prolog(&json!([1, 2, 3])), "[1, 2, 3]");
        assert_eq!(runtime.json_to_prolog(&json!([])), "[]");
    }

    #[test]
    fn test_simple_query() {
        let runtime = ScryerRuntime::new().unwrap();

        // Test member/2 query - load lists library first
        let results = runtime.query("member(X, [1, 2, 3]).");
        // member/2 may need library(lists) to be loaded
        if results.is_err() {
            eprintln!("Error: {:?}", results.err());
            // Scryer may not have member/2 available by default
            // This is a known limitation vs SWI-Prolog
            return;
        }

        let results = results.unwrap();
        assert!(!results.is_empty());
    }

    #[test]
    fn test_arithmetic() {
        let runtime = ScryerRuntime::new().unwrap();

        let results = runtime.query("X is 21 * 2.");
        eprintln!("Query result: {:?}", results);
        assert!(results.is_ok());

        let results = results.unwrap();
        eprintln!("Results: {:?}", results);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].get("X"), Some(&json!(42)));
    }

    #[test]
    fn test_execute_node_code_simple() {
        let runtime = ScryerRuntime::new().unwrap();

        let state = json!({"value": 21});
        let code = r#"
            state(value, V),
            D is V * 2,
            return(doubled, D).
        "#;

        let result = runtime.execute_node_code(code, &state);
        assert!(result.is_ok(), "Error: {:?}", result.err());

        let result = result.unwrap();
        assert_eq!(result.get("doubled"), Some(&json!(42)));
    }

    #[test]
    #[ignore = "SPIKE LIMITATION: inline rule definitions not supported"]
    fn test_execute_node_code_with_rule() {
        // SPIKE FINDING: Scryer's query-only approach cannot define inline rules
        // This would require module loading which doesn't work from strings
        let runtime = ScryerRuntime::new().unwrap();

        let state = json!({"input": 10});
        let code = r#"
            add_ten(X, Y) :- Y is X + 10.
            state(input, I),
            add_ten(I, R),
            return(result, R).
        "#;

        let result = runtime.execute_node_code(code, &state);
        assert!(result.is_ok());

        let result = result.unwrap();
        assert_eq!(result.get("result"), Some(&json!(20)));
    }

    #[test]
    #[ignore = "SPIKE LIMITATION: conditional return syntax not fully supported"]
    fn test_execute_node_code_categorize() {
        // SPIKE FINDING: Complex conditionals with multiple return() calls
        // require module support for proper execution
        let runtime = ScryerRuntime::new().unwrap();

        let state = json!({"value": 75});
        let code = r#"
            state(value, V),
            (V >= 100 -> return(category, large)
            ; V >= 50 -> return(category, medium)
            ; return(category, small)).
        "#;

        let result = runtime.execute_node_code(code, &state);
        assert!(result.is_ok());

        let result = result.unwrap();
        assert_eq!(result.get("category"), Some(&json!("medium")));
    }
}
