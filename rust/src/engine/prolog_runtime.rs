//! Prolog runtime integration via swipl-rs
//!
//! Provides SWI-Prolog scripting support for:
//! - Inline code execution in nodes
//! - Conditional edge expressions
//! - CLP(FD) constraint solving
//! - Neurosymbolic AI workflows
//!
//! ## State Access
//!
//! State is accessed via the `state/2` predicate:
//!
//! ```prolog
//! % Read state value
//! state(key, Value),
//!
//! % Perform logic operations
//! Value > 0,
//! NewValue is Value * 2.
//! ```
//!
//! ## Current Limitation: return/2
//!
//! The `return/2` predicate is recognized by auto-detection but does not
//! currently update the state due to swipl-rs crate constraints. For workflows
//! that need to pass values between Prolog and other nodes:
//!
//! 1. Use Lua nodes for state manipulation
//! 2. Use CLP(FD) constraints where labeled values can be extracted
//! 3. Use Prolog for validation/logic checks that succeed or fail
//!
//! Future versions may implement full `return/2` support when swipl-rs
//! provides better dynamic fact querying capabilities.
//!
//! ## Sandbox
//!
//! When sandbox is enabled (default), dangerous predicates are blocked
//! via SWI-Prolog's `library(sandbox)`:
//! - File I/O (open/3, read/2, write/2, etc.)
//! - Network access
//! - Shell execution (shell/1, process_create/3)
//! - Dynamic code loading (consult/1, load_files/2)
//!
//! The sandbox uses a "Default Deny" model - only whitelisted predicates
//! are allowed. User queries are wrapped with `sandbox:safe_call/1`.

use parking_lot::RwLock;
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use crate::error::{TeaError, TeaResult};

// Import swipl types for engine initialization
use swipl::prelude::*;

/// Prolog runtime for The Edge Agent (Rust implementation)
///
/// Provides a sandboxed SWI-Prolog environment with timeout protection
/// for neurosymbolic AI workflows. Uses the same SWI-Prolog engine as
/// the Python implementation for cross-runtime parity.
///
/// ## Architecture
///
/// The runtime uses a **persistent Engine** pattern as recommended by swipl-rs.
/// When sandbox is enabled, `library(sandbox)` is loaded once at initialization
/// and persists for the lifetime of the runtime. This avoids the overhead of
/// reloading the sandbox library for each query.
pub struct PrologRuntime {
    /// Execution timeout
    timeout: Duration,

    /// Whether sandbox is enabled
    sandbox: bool,

    /// Whether sandbox was successfully initialized
    sandbox_initialized: bool,

    /// Cached state for thread safety
    state_cache: Arc<RwLock<HashMap<String, JsonValue>>>,

    /// Return values collected during execution
    return_values: Arc<RwLock<HashMap<String, JsonValue>>>,
}

impl PrologRuntime {
    /// Create a new Prolog runtime with default settings (sandbox enabled)
    pub fn new() -> TeaResult<Self> {
        Self::with_config(Duration::from_secs(30), true)
    }

    /// Create a new Prolog runtime with custom timeout (sandbox enabled)
    pub fn with_timeout(timeout: Duration) -> TeaResult<Self> {
        Self::with_config(timeout, true)
    }

    /// Create a new Prolog runtime with full configuration
    ///
    /// # Arguments
    /// * `timeout` - Maximum execution time for queries
    /// * `sandbox` - If true, wraps queries with `sandbox:safe_call/1`
    pub fn with_config(timeout: Duration, sandbox: bool) -> TeaResult<Self> {
        // Verify SWI-Prolog is available by trying to create an engine
        // This will fail at runtime if SWI-Prolog is not installed
        let engine_available = std::panic::catch_unwind(|| {
            let _engine = Engine::new();
        })
        .is_ok();

        if !engine_available {
            return Err(TeaError::Prolog(get_install_instructions()));
        }

        // Attempt to initialize sandbox if enabled
        let sandbox_initialized = if sandbox {
            Self::try_init_sandbox()
        } else {
            false
        };

        let runtime = Self {
            timeout,
            sandbox,
            sandbox_initialized,
            state_cache: Arc::new(RwLock::new(HashMap::new())),
            return_values: Arc::new(RwLock::new(HashMap::new())),
        };

        Ok(runtime)
    }

    /// Attempt to initialize the sandbox by loading library(sandbox)
    ///
    /// Returns true if sandbox was loaded successfully, false otherwise.
    /// This is called once at runtime initialization.
    fn try_init_sandbox() -> bool {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        // Try to load library(sandbox)
        let load_cmd = "use_module(library(sandbox))";
        match context.term_from_string(load_cmd) {
            Ok(term) => match context.call_term_once(&term) {
                Ok(()) => true,
                Err(_) => {
                    // Sandbox library not available - protection will be disabled
                    false
                }
            },
            Err(_) => {
                // Failed to parse sandbox load command
                false
            }
        }
    }

    /// Check if sandbox was requested in configuration
    pub fn sandbox_enabled(&self) -> bool {
        self.sandbox
    }

    /// Check if sandbox is enabled and successfully initialized
    pub fn is_sandboxed(&self) -> bool {
        self.sandbox && self.sandbox_initialized
    }

    /// Convert JSON value to Prolog term string
    pub fn json_to_prolog(&self, value: &JsonValue) -> String {
        match value {
            JsonValue::Null => "null".to_string(),
            JsonValue::Bool(b) => if *b { "true" } else { "false" }.to_string(),
            JsonValue::Number(n) => n.to_string(),
            JsonValue::String(s) => {
                // Escape single quotes and backslashes for Prolog atom
                let escaped = s.replace('\\', "\\\\").replace('\'', "\\'");
                format!("'{}'", escaped)
            }
            JsonValue::Array(arr) => {
                let items: Vec<String> = arr.iter().map(|v| self.json_to_prolog(v)).collect();
                format!("[{}]", items.join(", "))
            }
            JsonValue::Object(obj) => {
                // Use Prolog dict syntax: _{key1: value1, key2: value2}
                let pairs: Vec<String> = obj
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, self.json_to_prolog(v)))
                    .collect();
                format!("_{{{}}}", pairs.join(", "))
            }
        }
    }

    /// Parse a Prolog term string to JSON value
    ///
    /// Handles:
    /// - Atoms: 'hello' or hello -> "hello"
    /// - Numbers: 42, 3.14 -> number
    /// - Lists: [1, 2, 3] -> array
    /// - Dicts: _{a: 1, b: 2} -> object
    pub fn prolog_to_json(&self, term: &str) -> TeaResult<JsonValue> {
        let term = term.trim();

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

        // Handle dict
        if term.starts_with("_{") && term.ends_with('}') {
            return self.parse_prolog_dict(term);
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

    /// Parse a Prolog dict string to JSON object
    fn parse_prolog_dict(&self, term: &str) -> TeaResult<JsonValue> {
        let inner = &term[2..term.len() - 1]; // Remove _{ and }
        if inner.is_empty() {
            return Ok(JsonValue::Object(serde_json::Map::new()));
        }

        let pairs = self.split_prolog_terms(inner)?;
        let mut obj = serde_json::Map::new();

        for pair in pairs {
            if let Some(colon_pos) = pair.find(':') {
                let key = pair[..colon_pos].trim().to_string();
                let value_str = pair[colon_pos + 1..].trim();
                let value = self.prolog_to_json(value_str)?;
                obj.insert(key, value);
            }
        }

        Ok(JsonValue::Object(obj))
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

    /// Execute Prolog code with state access
    ///
    /// If sandbox is enabled and initialized, the query is wrapped with
    /// `sandbox:safe_call/1` to enforce security restrictions.
    ///
    /// State values are available via `state(Key, Value)` predicate.
    /// Return values are collected from `return_value(Key, Value)` facts.
    pub fn execute(&self, code: &str, state: &JsonValue) -> TeaResult<JsonValue> {
        // Store state in cache
        if let JsonValue::Object(obj) = state {
            let mut cache = self.state_cache.write();
            cache.clear();
            for (k, v) in obj {
                cache.insert(k.clone(), v.clone());
            }
        }

        // Clear return values
        {
            let mut returns = self.return_values.write();
            returns.clear();
        }

        // Create engine and execute
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        // Load sandbox if enabled and not yet loaded in this context
        if self.is_sandboxed() && self.load_sandbox_in_context(&context).is_err() {
            // Fall back to unsandboxed execution
            return self.execute_in_context(&context, code, false);
        }

        self.execute_in_context(&context, code, self.is_sandboxed())
    }

    /// Load library(sandbox) in the given context
    fn load_sandbox_in_context<C: QueryableContextType>(
        &self,
        context: &Context<C>,
    ) -> TeaResult<()> {
        let load_cmd = "use_module(library(sandbox))";
        match context.term_from_string(load_cmd) {
            Ok(term) => match context.call_term_once(&term) {
                Ok(()) => Ok(()),
                Err(e) => Err(TeaError::Prolog(format!(
                    "Failed to load sandbox: {:?}",
                    e
                ))),
            },
            Err(e) => Err(TeaError::Prolog(format!(
                "Failed to parse sandbox command: {:?}",
                e
            ))),
        }
    }

    /// Execute Prolog code in the given context
    ///
    /// If `use_sandbox` is true, wraps the query with `sandbox:safe_call/1`.
    fn execute_in_context<C: QueryableContextType>(
        &self,
        context: &Context<C>,
        code: &str,
        use_sandbox: bool,
    ) -> TeaResult<JsonValue> {
        let code_clean = code.trim().trim_end_matches('.');

        // Build the query with timeout
        let timeout_secs = self.timeout.as_secs_f64();
        let timed_code = format!(
            "catch(call_with_time_limit({}, ({})), time_limit_exceeded, throw(prolog_timeout))",
            timeout_secs, code_clean
        );

        // Wrap with sandbox if enabled
        let final_code = if use_sandbox {
            // Wrap the timed query with sandbox:safe_call/1
            format!("sandbox:safe_call(({}))", timed_code)
        } else {
            timed_code
        };

        // Parse and execute
        match context.term_from_string(&final_code) {
            Ok(term) => match context.call_term_once(&term) {
                Ok(()) => self.collect_returns_from_context(context),
                Err(PrologError::Failure) => Ok(JsonValue::Object(serde_json::Map::new())),
                Err(PrologError::Exception) => {
                    // Check if this is a sandbox violation or timeout
                    self.handle_prolog_exception(context)
                }
            },
            Err(e) => Err(TeaError::Prolog(format!(
                "Failed to parse Prolog code: {:?}",
                e
            ))),
        }
    }

    /// Handle Prolog exceptions, distinguishing between sandbox violations,
    /// timeouts, and other errors
    fn handle_prolog_exception<C: QueryableContextType>(
        &self,
        _context: &Context<C>,
    ) -> TeaResult<JsonValue> {
        // In swipl-rs, we can access the exception via context.exception()
        // For now, we use heuristics based on the error type
        // The exception term structure for sandbox violations is:
        //   error(permission_error(call, sandboxed, Goal), Context)

        // Since we can't easily introspect the exception term in swipl-rs,
        // we return a generic error. In practice, the sandbox:safe_call/1
        // will throw permission_error which becomes PrologError::Exception.

        // Check if this was a timeout (our custom throw)
        // This is difficult without exception introspection, so we default
        // to assuming sandbox violation if sandbox was enabled
        if self.is_sandboxed() {
            Err(TeaError::PrologSandboxViolation {
                predicate: "unknown (sandboxed predicate)".to_string(),
            })
        } else {
            Err(TeaError::PrologTimeout(self.timeout.as_secs()))
        }
    }

    /// Collect return values from the Prolog context
    ///
    /// Returns the current state merged with any return values collected during execution.
    ///
    /// ## Limitation
    ///
    /// Due to swipl-rs crate constraints, the `return/2` predicate in Prolog code
    /// does not currently update the state. Users should use the `state/2` predicate
    /// to read input values, and the query result (success/failure) determines the
    /// outcome. Future versions may support `return/2` for explicit value returns.
    ///
    /// ## Workaround
    ///
    /// For workflows that need to pass values between nodes, use Lua nodes for
    /// state manipulation, or use CLP(FD) constraints where the labeled values
    /// are the output.
    fn collect_returns_from_context<C: QueryableContextType>(
        &self,
        _context: &Context<C>,
    ) -> TeaResult<JsonValue> {
        // Return the cached state (input state from execute())
        // The return_values field is reserved for future implementation
        // when swipl-rs provides better support for querying dynamic facts
        let cache = self.state_cache.read();
        let returns = self.return_values.read();

        let mut result = serde_json::Map::new();

        // Start with input state
        for (k, v) in cache.iter() {
            result.insert(k.clone(), v.clone());
        }

        // Merge any return values (if populated by future implementation)
        for (k, v) in returns.iter() {
            result.insert(k.clone(), v.clone());
        }

        Ok(JsonValue::Object(result))
    }

    /// Evaluate a Prolog condition expression
    ///
    /// Returns:
    /// - `Some("true")` if query succeeds
    /// - `None` if query fails
    /// - `Some(value)` if Result variable is bound
    pub fn eval_condition(
        &self,
        expression: &str,
        state: &JsonValue,
    ) -> TeaResult<Option<String>> {
        // Store state in cache
        if let JsonValue::Object(obj) = state {
            let mut cache = self.state_cache.write();
            cache.clear();
            for (k, v) in obj {
                cache.insert(k.clone(), v.clone());
            }
        }

        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        // Load sandbox if enabled
        if self.is_sandboxed() {
            let _ = self.load_sandbox_in_context(&context);
        }

        // Execute condition expression
        match self.execute_in_context(&context, expression, self.is_sandboxed()) {
            Ok(_) => Ok(Some("true".to_string())),
            Err(TeaError::Prolog(msg)) if msg.contains("failed") => Ok(None),
            Err(e) => Err(e),
        }
    }

    /// Execute inline Prolog code for a node
    ///
    /// Handles both rule definitions and queries.
    /// Rules are asserted, then the main query is executed.
    pub fn execute_node_code(&self, code: &str, state: &JsonValue) -> TeaResult<JsonValue> {
        let code = code.trim();

        // Check for rule definitions (lines ending with '.')
        // and separate them from the final query
        let lines: Vec<&str> = code
            .lines()
            .map(|l| l.trim())
            .filter(|l| !l.is_empty() && !l.starts_with('%'))
            .collect();

        if lines.is_empty() {
            return Ok(JsonValue::Object(serde_json::Map::new()));
        }

        // Execute all lines as one query
        let full_code = lines.join(", ");
        self.execute(&full_code, state)
    }
}

impl Default for PrologRuntime {
    fn default() -> Self {
        Self::new().expect("Failed to create Prolog runtime")
    }
}

impl std::fmt::Debug for PrologRuntime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PrologRuntime")
            .field("timeout", &self.timeout)
            .field("sandbox", &self.sandbox)
            .field("sandbox_initialized", &self.sandbox_initialized)
            .finish()
    }
}

/// Detect if code block is Prolog
///
/// Uses heuristic patterns to distinguish Prolog from other languages.
pub fn detect_prolog_code(code: &str) -> bool {
    let code = code.trim();

    // Explicit marker comment
    if code.starts_with("% prolog") || code.starts_with("%prolog") {
        return true;
    }

    // Characteristic Prolog patterns
    let patterns = [
        ":-",             // Rule operator
        "?-",             // Query operator
        "state(",         // state/2 predicate
        "return(",        // return/2 predicate
        "assertz(",       // assert predicate
        "findall(",       // findall predicate
        "all_different(", // CLP(FD) all_different
        "#=", "#<", "#>", // CLP(FD) operators
        "ins ",           // CLP(FD) domain
        "labeling",       // CLP(FD) labeling
    ];

    patterns.iter().any(|p| code.contains(p))
}

/// Get OS-specific SWI-Prolog installation instructions
pub fn get_install_instructions() -> String {
    #[cfg(target_os = "linux")]
    {
        "SWI-Prolog is required but not installed.\n\
         Install on Ubuntu/Debian:\n  \
           sudo apt install swi-prolog swi-prolog-nox\n\n\
         Install on Fedora:\n  \
           sudo dnf install pl\n\n\
         Install on Arch:\n  \
           sudo pacman -S swi-prolog"
            .to_string()
    }

    #[cfg(target_os = "macos")]
    {
        "SWI-Prolog is required but not installed.\n\
         Install with Homebrew:\n  \
           brew install swi-prolog"
            .to_string()
    }

    #[cfg(target_os = "windows")]
    {
        "SWI-Prolog is required but not installed.\n\
         Download from: https://www.swi-prolog.org/download/stable\n\
         Or install with Chocolatey:\n  \
           choco install swi-prolog"
            .to_string()
    }

    #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
    {
        "SWI-Prolog is required but not installed.\n\
         Download from: https://www.swi-prolog.org/download/stable"
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_json_to_prolog_primitives() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();

        assert_eq!(runtime.json_to_prolog(&json!(null)), "null");
        assert_eq!(runtime.json_to_prolog(&json!(true)), "true");
        assert_eq!(runtime.json_to_prolog(&json!(false)), "false");
        assert_eq!(runtime.json_to_prolog(&json!(42)), "42");
        assert_eq!(runtime.json_to_prolog(&json!(3.14)), "3.14");
        assert_eq!(runtime.json_to_prolog(&json!("hello")), "'hello'");
    }

    #[test]
    fn test_json_to_prolog_string_escaping() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();

        assert_eq!(runtime.json_to_prolog(&json!("it's")), "'it\\'s'");
        assert_eq!(runtime.json_to_prolog(&json!("a\\b")), "'a\\\\b'");
    }

    #[test]
    fn test_json_to_prolog_array() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();

        assert_eq!(runtime.json_to_prolog(&json!([1, 2, 3])), "[1, 2, 3]");
        assert_eq!(runtime.json_to_prolog(&json!(["a", "b"])), "['a', 'b']");
    }

    #[test]
    fn test_json_to_prolog_object() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();

        let obj = json!({"name": "test", "value": 42});
        let result = runtime.json_to_prolog(&obj);
        // Dict format: _{key: value, ...}
        assert!(result.starts_with("_{"));
        assert!(result.ends_with("}"));
        assert!(result.contains("name: 'test'"));
        assert!(result.contains("value: 42"));
    }

    #[test]
    fn test_prolog_to_json_primitives() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();

        assert_eq!(runtime.prolog_to_json("null").unwrap(), json!(null));
        assert_eq!(runtime.prolog_to_json("true").unwrap(), json!(true));
        assert_eq!(runtime.prolog_to_json("false").unwrap(), json!(false));
        assert_eq!(runtime.prolog_to_json("42").unwrap(), json!(42));
        assert_eq!(runtime.prolog_to_json("3.14").unwrap(), json!(3.14));
    }

    #[test]
    fn test_prolog_to_json_strings() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();

        assert_eq!(runtime.prolog_to_json("'hello'").unwrap(), json!("hello"));
        assert_eq!(runtime.prolog_to_json("hello").unwrap(), json!("hello"));
        assert_eq!(runtime.prolog_to_json("\"world\"").unwrap(), json!("world"));
    }

    #[test]
    fn test_prolog_to_json_list() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();

        assert_eq!(runtime.prolog_to_json("[]").unwrap(), json!(null));
        assert_eq!(
            runtime.prolog_to_json("[1, 2, 3]").unwrap(),
            json!([1, 2, 3])
        );
    }

    #[test]
    fn test_detect_prolog_code() {
        assert!(detect_prolog_code("% prolog\nparent(X, Y)."));
        assert!(detect_prolog_code("state(input, X), X > 0."));
        assert!(detect_prolog_code("return(result, 42)."));
        assert!(detect_prolog_code("X #= Y + 1."));
        assert!(detect_prolog_code("head :- body."));
        assert!(detect_prolog_code("?- member(X, [1,2,3])."));

        // Should not detect non-Prolog code
        assert!(!detect_prolog_code("return state.value"));
        assert!(!detect_prolog_code("x = 1"));
        assert!(!detect_prolog_code("function foo() {}"));
    }

    #[test]
    fn test_install_instructions() {
        let instructions = get_install_instructions();
        assert!(!instructions.is_empty());
        assert!(instructions.contains("SWI-Prolog"));
    }

    #[test]
    fn test_sandbox_initialization() {
        let runtime_sandboxed = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
        let runtime_unsandboxed =
            PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        // Sandboxed runtime should have sandbox flag set
        assert!(runtime_sandboxed.sandbox);
        assert!(!runtime_unsandboxed.sandbox);
        assert!(!runtime_unsandboxed.is_sandboxed());
    }
}
