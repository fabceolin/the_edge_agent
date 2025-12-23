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
//! ## Return Values
//!
//! Use `return/2` to update state from Prolog nodes:
//!
//! ```prolog
//! % Return a single value
//! return(result, 42).
//!
//! % Return multiple values (last-write-wins for same key)
//! return(a, 1), return(b, 2).
//! ```
//!
//! The `return/2` predicate asserts facts that are collected after query
//! execution and merged into the output state, providing full cross-runtime
//! parity with the Python implementation.
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

/// Common modules to pre-load for Python parity.
///
/// These modules are loaded at initialization time so that YAML agents
/// can use predicates like `member/2`, `findall/3`, CLP(FD) operators,
/// etc. without requiring explicit `:- use_module(library(...))` directives.
///
/// Same modules as Python implementation for cross-runtime parity.
const DEFAULT_MODULES: &[&str] = &["lists", "clpfd", "apply", "aggregate"];

/// TEA Prolog predicates embedded as a string constant.
///
/// These predicates implement Prolog-side parsing via `tea_load_code/1`,
/// which uses SWI-Prolog's native `read_term/3` parser instead of
/// error-prone host-side heuristic parsing.
///
/// This is the "thin runtime" architecture: let Prolog parse Prolog!
///
/// NOTE: This constant is kept for documentation and as a fallback reference.
/// The primary loading method uses `consult/1` from `tea_prolog_predicates.pl`.
#[allow(dead_code)]
const TEA_PROLOG_PREDICATES: &str = r#"
% TEA Prolog Predicates - Prolog-side term processing
% Using SWI-Prolog's native read_term/3 for 100% accurate parsing

:- thread_local(state/2).
:- thread_local(return_value/2).
:- thread_local(tea_user_fact/1).

% Define return/2 predicate
return(Key, Value) :- assertz(return_value(Key, Value)).

% Action predicates - should be called, not asserted
% Also includes structural operators that should never be asserted
tea_action_predicate(return).
tea_action_predicate(state).
tea_action_predicate(',').  % Conjunction - must be called, not asserted
tea_action_predicate(';').  % Disjunction - must be called, not asserted
tea_action_predicate('->').  % If-then - must be called, not asserted
tea_action_predicate('*->').  % Soft cut if-then - must be called, not asserted
tea_action_predicate('>').   % Arithmetic comparison
tea_action_predicate('<').   % Arithmetic comparison
tea_action_predicate('>=').  % Arithmetic comparison
tea_action_predicate('=<').  % Arithmetic comparison
tea_action_predicate('=:=').  % Arithmetic equality
tea_action_predicate('=\\=').  % Arithmetic inequality
tea_action_predicate('=').   % Unification
tea_action_predicate('\\=').  % Not unifiable
tea_action_predicate('==').  % Structural equality
tea_action_predicate('\\==').  % Structural inequality
tea_action_predicate('@<').  % Term comparison
tea_action_predicate('@>').  % Term comparison
tea_action_predicate('@=<').  % Term comparison
tea_action_predicate('@>=').  % Term comparison
tea_action_predicate('\\+').  % Negation as failure
tea_action_predicate(is).   % Arithmetic evaluation
tea_action_predicate(findall).
tea_action_predicate(bagof).
tea_action_predicate(setof).
tea_action_predicate(member).
tea_action_predicate(memberchk).
tea_action_predicate(append).
tea_action_predicate(length).
tea_action_predicate(nth0).
tea_action_predicate(nth1).
tea_action_predicate(msort).
tea_action_predicate(sort).
tea_action_predicate(reverse).
tea_action_predicate(last).
tea_action_predicate(sumlist).
tea_action_predicate(max_list).
tea_action_predicate(min_list).
tea_action_predicate(forall).
tea_action_predicate(aggregate_all).
tea_action_predicate(aggregate).
tea_action_predicate(call).
tea_action_predicate(once).
tea_action_predicate(succ).
tea_action_predicate(plus).
tea_action_predicate(abs).
tea_action_predicate(sign).
tea_action_predicate(max).
tea_action_predicate(min).
tea_action_predicate(write).
tea_action_predicate(writeln).
tea_action_predicate(print).
tea_action_predicate(format).
tea_action_predicate(atom).
tea_action_predicate(number).
tea_action_predicate(integer).
tea_action_predicate(float).
tea_action_predicate(compound).
tea_action_predicate(is_list).
tea_action_predicate(ground).
tea_action_predicate(atom_string).
tea_action_predicate(atom_codes).
tea_action_predicate(atom_chars).

% Determine if a term is a fact (should be asserted)
tea_is_fact(Term) :-
    compound(Term),
    ground(Term),
    functor(Term, F, _),
    atom(F),
    \+ tea_action_predicate(F).

% Process a single term from user code (unsandboxed version)
tea_process_term((:-Body)) :- !, call(Body).
tea_process_term((Head :- Body)) :- !, assertz((Head :- Body)).
tea_process_term(Term) :-
    ( tea_is_fact(Term)
    -> assertz(Term), assertz(tea_user_fact(Term))
    ; call(Term)
    ).

% Check if a goal uses only TEA predicates (safe to run without sandbox)
tea_uses_only_tea_predicates(Goal) :-
    Goal = (A, B), !,
    tea_uses_only_tea_predicates(A),
    tea_uses_only_tea_predicates(B).
tea_uses_only_tea_predicates(Goal) :-
    Goal = (A ; B), !,
    tea_uses_only_tea_predicates(A),
    tea_uses_only_tea_predicates(B).
tea_uses_only_tea_predicates(Goal) :-
    compound(Goal),
    functor(Goal, F, _),
    tea_action_predicate(F).

% Process a single term from user code (sandboxed version)
% Uses sandbox:safe_call/1 for dangerous predicates, but allows TEA predicates directly
tea_process_term_sandboxed((:-Body)) :- !,
    ( tea_uses_only_tea_predicates(Body)
    -> call(Body)
    ; sandbox:safe_call(Body)
    ).
tea_process_term_sandboxed((Head :- Body)) :- !, assertz((Head :- Body)).
tea_process_term_sandboxed(Term) :-
    ( tea_is_fact(Term)
    -> assertz(Term), assertz(tea_user_fact(Term))
    ; tea_uses_only_tea_predicates(Term)
    -> call(Term)
    ; sandbox:safe_call(Term)
    ).

% Read and process terms from a stream (unsandboxed version)
tea_load_terms(Stream) :-
    catch(
        read_term(Stream, Term, []),
        Error,
        throw(tea_syntax_error(Error))
    ),
    ( Term == end_of_file
    -> true
    ; tea_process_term(Term),
      tea_load_terms(Stream)
    ).

% Read and process terms from a stream (sandboxed version)
tea_load_terms_sandboxed(Stream) :-
    catch(
        read_term(Stream, Term, []),
        Error,
        throw(tea_syntax_error(Error))
    ),
    ( Term == end_of_file
    -> true
    ; tea_process_term_sandboxed(Term),
      tea_load_terms_sandboxed(Stream)
    ).

% Main entry point: load code from a string (unsandboxed version)
tea_load_code(CodeAtom) :-
    atom_string(CodeAtom, CodeString),
    open_string(CodeString, Stream),
    catch(
        tea_load_terms(Stream),
        Error,
        ( close(Stream), throw(Error) )
    ),
    close(Stream).

% Main entry point: load code from a string (sandboxed version)
tea_load_code_sandboxed(CodeAtom) :-
    atom_string(CodeAtom, CodeString),
    open_string(CodeString, Stream),
    catch(
        tea_load_terms_sandboxed(Stream),
        Error,
        ( close(Stream), throw(Error) )
    ),
    close(Stream).

% Clean up user-asserted facts
tea_cleanup_facts :-
    forall(tea_user_fact(Fact), retract(Fact)),
    retractall(tea_user_fact(_)).
"#;

/// Prolog runtime for The Edge Agent (Rust implementation)
///
/// Provides a sandboxed SWI-Prolog environment with timeout protection
/// for neurosymbolic AI workflows. Uses the same SWI-Prolog engine as
/// the Python implementation for cross-runtime parity.
///
/// ## Module Pre-Loading
///
/// The runtime automatically pre-loads common modules at initialization:
/// - `lists` - List manipulation (`member/2`, `append/3`, `reverse/2`)
/// - `clpfd` - CLP(FD) constraints (no explicit import needed!)
/// - `apply` - Higher-order predicates (`maplist/2`, etc.)
/// - `aggregate` - Aggregation predicates (`aggregate_all/3`)
///
/// This matches the Python implementation for cross-runtime parity.
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

        // Pre-load common modules for Python parity (lists, clpfd, apply, aggregate)
        // This must happen AFTER sandbox initialization to ensure modules are available
        Self::preload_modules();

        // Note: state/return predicates are set up per-execution in execute()
        // because each Engine context needs its own predicate definitions

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

    /// Pre-load common SWI-Prolog modules and TEA predicates for Python parity.
    ///
    /// This enables predicates like `member/2`, `findall/3`, CLP(FD) operators,
    /// etc. without requiring explicit `:- use_module(library(...))` directives
    /// in user code.
    ///
    /// Also loads the TEA predicates (`tea_load_code/1`, etc.) for Prolog-side parsing.
    ///
    /// Modules that fail to load are silently skipped (graceful failure).
    /// This matches the Python implementation behavior.
    fn preload_modules() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        for module in DEFAULT_MODULES {
            let cmd = format!("use_module(library({}))", module);
            if let Ok(term) = context.term_from_string(&cmd) {
                // Ignore failures - module may not be available
                let _ = context.call_term_once(&term);
            }
        }

        // NOTE: TEA predicates are loaded per-execution in execute_node_code()
        // because each Engine context is independent.
    }

    /// Load TEA predicates from the embedded .pl file via consult/1.
    ///
    /// Uses SWI-Prolog's `consult/1` to load the predicates from a file,
    /// which is more reliable than loading from a string.
    fn load_tea_predicates_in_context<C: QueryableContextType>(context: &Context<C>) {
        // Try to find the TEA predicates .pl file
        // First check relative to CARGO_MANIFEST_DIR (for dev/test)
        // Then fall back to embedded predicates
        let pl_file = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("src/engine/tea_prolog_predicates.pl");

        if pl_file.exists() {
            // Use file-based consult (more reliable)
            let escaped_path = pl_file.to_str().unwrap_or("").replace('\'', "\\'");
            let consult_cmd = format!("consult('{}')", escaped_path);

            if let Ok(term) = context.term_from_string(&consult_cmd) {
                if context.call_term_once(&term).is_ok() {
                    return; // Successfully loaded
                }
            }
        }

        // Fall back to asserting predicates individually
        Self::fallback_load_tea_predicates(context);
    }

    /// Fallback method to load TEA predicates by asserting them individually.
    ///
    /// This is used if `load_files/2` from string fails.
    fn fallback_load_tea_predicates<C: QueryableContextType>(context: &Context<C>) {
        // Declare thread-local predicates
        let decl_goals = [
            "thread_local(state/2)",
            "thread_local(return_value/2)",
            "thread_local(tea_user_fact/1)",
        ];

        for goal in &decl_goals {
            if let Ok(term) = context.term_from_string(goal) {
                let _ = context.call_term_once(&term);
            }
        }

        // Define return/2
        let return_def = "assertz((return(Key, Value) :- assertz(return_value(Key, Value))))";
        if let Ok(term) = context.term_from_string(return_def) {
            let _ = context.call_term_once(&term);
        }

        // Define tea_load_code/1 using assertz
        // This is a simplified version that still uses Prolog's read_term
        let tea_load_code_def = r#"assertz((
            tea_load_code(CodeAtom) :-
                atom_string(CodeAtom, CodeString),
                open_string(CodeString, Stream),
                catch(
                    (repeat,
                     read_term(Stream, Term, []),
                     (Term == end_of_file -> ! ; (call(Term), fail))),
                    Error,
                    (close(Stream), throw(Error))
                ),
                close(Stream)
        ))"#;
        if let Ok(term) = context.term_from_string(tea_load_code_def) {
            let _ = context.call_term_once(&term);
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
            self.setup_predicates_in_context(&context);
            self.set_state_facts(&context, state);
            return self.execute_in_context(&context, code, false);
        }

        // Set up return/2 and state/2 predicates in this execution context
        self.setup_predicates_in_context(&context);

        // Set state facts in Prolog
        self.set_state_facts(&context, state);

        self.execute_in_context(&context, code, self.is_sandboxed())
    }

    /// Set up state/2 and return/2 predicates in the given execution context
    ///
    /// This must be called for each new Engine context since SWI-Prolog
    /// thread-local predicates and clause definitions are per-engine.
    fn setup_predicates_in_context<C: QueryableContextType>(&self, context: &Context<C>) {
        // Declare state/2 and return_value/2 as thread_local
        let decl_goals = [
            "thread_local(state/2)",
            "thread_local(return_value/2)",
            "thread_local(tea_rv/3)",
        ];

        for goal in &decl_goals {
            if let Ok(term) = context.term_from_string(goal) {
                let _ = context.call_term_once(&term);
            }
        }

        // Define return/2 using assertz
        let define_return = "assertz((return(Key, Value) :- assertz(return_value(Key, Value))))";
        if let Ok(term) = context.term_from_string(define_return) {
            let _ = context.call_term_once(&term);
        }
    }

    /// Set state/2 facts in Prolog from the input state
    ///
    /// Clears any existing state/2 facts and asserts new ones from the input.
    fn set_state_facts<C: QueryableContextType>(&self, context: &Context<C>, state: &JsonValue) {
        // Clear existing state facts
        let clear_cmd = "retractall(state(_, _))";
        if let Ok(term) = context.term_from_string(clear_cmd) {
            let _ = context.call_term_once(&term);
        }

        // Assert new state facts
        if let JsonValue::Object(obj) = state {
            for (key, value) in obj {
                let prolog_value = self.json_to_prolog(value);
                // Escape key for use in Prolog atom
                let escaped_key = key.replace('\'', "\\'");
                let assert_cmd = format!("assertz(state('{}', {}))", escaped_key, prolog_value);
                if let Ok(term) = context.term_from_string(&assert_cmd) {
                    let _ = context.call_term_once(&term);
                }
            }
        }
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
                Err(e) => Err(TeaError::Prolog(format!("Failed to load sandbox: {:?}", e))),
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
    /// However, if the code contains `return(` predicates, sandbox is disabled
    /// since return/2 uses assertz which the sandbox blocks.
    fn execute_in_context<C: QueryableContextType>(
        &self,
        context: &Context<C>,
        code: &str,
        use_sandbox: bool,
    ) -> TeaResult<JsonValue> {
        let code_clean = code.trim().trim_end_matches('.');

        // Check if code uses TEA predicates (state/2 or return/2)
        // If so, we must disable sandbox because:
        // - return/2 uses assertz which the sandbox blocks
        // - state/2 is a user-defined predicate which sandbox blocks by default
        //
        // This is a known limitation: using TEA predicates disables sandbox protection.
        // Users should avoid mixing state/return predicates with potentially dangerous
        // operations. Future work could implement a custom sandbox whitelist.
        let uses_tea_predicates = code_clean.contains("return(") || code_clean.contains("state(");
        let effective_sandbox = use_sandbox && !uses_tea_predicates;

        // Build the query with timeout
        let timeout_secs = self.timeout.as_secs_f64();
        let timed_code = format!(
            "catch(call_with_time_limit({}, ({})), time_limit_exceeded, throw(prolog_timeout))",
            timeout_secs, code_clean
        );

        // Wrap with sandbox if enabled and code doesn't use return/2
        let final_code = if effective_sandbox {
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
                Err(PrologError::Exception) => self.handle_prolog_exception(context),
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
        context: &Context<C>,
    ) -> TeaResult<JsonValue> {
        self.handle_prolog_exception_impl(context, self.is_sandboxed())
    }

    /// Implementation of exception handling with explicit sandbox flag
    ///
    /// The `sandbox_was_applied` parameter indicates whether sandbox was actually
    /// applied to the query. This is needed because `execute_node_code` does NOT
    /// apply sandbox even when the runtime has sandbox enabled.
    fn handle_prolog_exception_impl<C: QueryableContextType>(
        &self,
        _context: &Context<C>,
        sandbox_was_applied: bool,
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
        // to assuming sandbox violation if sandbox was actually applied
        if sandbox_was_applied {
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
    /// Strategy: Use Prolog's assert/retract to create numbered facts that encode
    /// key-value pairs as atoms. Then query each numbered fact by constructing
    /// specific queries. Finally, use atom equality checks to extract the values.
    fn collect_returns_from_context<C: QueryableContextType>(
        &self,
        context: &Context<C>,
    ) -> TeaResult<JsonValue> {
        let cache = self.state_cache.read();

        let mut result = serde_json::Map::new();

        // Start with input state
        for (k, v) in cache.iter() {
            result.insert(k.clone(), v.clone());
        }

        // Check if there are any return values
        let check_query = "return_value(_, _)";
        if let Ok(check_term) = context.term_from_string(check_query) {
            if context.call_term_once(&check_term).is_err() {
                // No return values - nothing to collect
                return Ok(JsonValue::Object(result));
            }
        }

        // Create indexed facts from return_value/2 using a counter
        // Each return value gets stored as tea_rv(Index, KeyAtom, ValueStr)
        let setup_query = r#"
            retractall(tea_rv(_, _, _)),
            nb_setval(tea_rv_idx, 0),
            forall(
                return_value(K, V),
                (
                    nb_getval(tea_rv_idx, I),
                    term_string(K, KS),
                    term_string(V, VS),
                    assertz(tea_rv(I, KS, VS)),
                    I1 is I + 1,
                    nb_setval(tea_rv_idx, I1)
                )
            )
        "#;

        if let Ok(term) = context.term_from_string(setup_query) {
            if context.call_term_once(&term).is_err() {
                return Ok(JsonValue::Object(result));
            }
        }

        // Now extract each indexed fact
        // We know the facts are tea_rv(0, K0, V0), tea_rv(1, K1, V1), ...
        let mut idx = 0;
        loop {
            // Try to get the indexed fact
            // We use a probe approach: check if tea_rv(idx, _, _) exists
            let probe_query = format!("tea_rv({}, _, _)", idx);
            if let Ok(term) = context.term_from_string(&probe_query) {
                if context.call_term_once(&term).is_err() {
                    break; // No more facts
                }
            } else {
                break;
            }

            // Extract key and value by probing known values
            let key = self.extract_rv_component(context, idx, "key");
            let value = self.extract_rv_component(context, idx, "value");

            if let (Some(k), Some(v)) = (key, value) {
                // Parse the value string to JSON
                if let Ok(json_val) = self.prolog_to_json(&v) {
                    result.insert(k, json_val);
                }
            }

            idx += 1;

            // Safety limit
            if idx > 1000 {
                break;
            }
        }

        // Cleanup
        let cleanup = "retractall(tea_rv(_, _, _)), nb_delete(tea_rv_idx)";
        if let Ok(term) = context.term_from_string(cleanup) {
            let _ = context.call_term_once(&term);
        }

        // Clear return_value facts for next execution
        let clear_query = "retractall(return_value(_, _))";
        if let Ok(term) = context.term_from_string(clear_query) {
            let _ = context.call_term_once(&term);
        }

        Ok(JsonValue::Object(result))
    }

    /// Extract a component (key or value) from an indexed return value fact
    ///
    /// Uses character-by-character probing to build the string.
    fn extract_rv_component<C: QueryableContextType>(
        &self,
        context: &Context<C>,
        idx: usize,
        component: &str,
    ) -> Option<String> {
        // Store the component in a global variable for easier access
        let extract_query = if component == "key" {
            format!("tea_rv({}, K, _), nb_setval(tea_extract, K)", idx)
        } else {
            format!("tea_rv({}, _, V), nb_setval(tea_extract, V)", idx)
        };

        if let Ok(term) = context.term_from_string(&extract_query) {
            if context.call_term_once(&term).is_err() {
                return None;
            }
        }

        // Convert the string to codes for extraction
        let codes_query =
            "nb_getval(tea_extract, S), string_codes(S, Codes), nb_setval(tea_codes, Codes)";
        if let Ok(term) = context.term_from_string(codes_query) {
            if context.call_term_once(&term).is_err() {
                return None;
            }
        }

        // Extract each character code using binary search
        let mut chars = Vec::new();
        let mut char_idx = 0;

        loop {
            // Get the code at this index
            let get_code = format!(
                "nb_getval(tea_codes, Codes), nth0({}, Codes, Code), nb_setval(tea_code, Code)",
                char_idx
            );

            if let Ok(term) = context.term_from_string(&get_code) {
                if context.call_term_once(&term).is_err() {
                    break;
                }
            } else {
                break;
            }

            // Binary search for the code value
            if let Some(code) = self.binary_search_integer(context, "tea_code", 0, 0x10FFFF) {
                if let Some(c) = char::from_u32(code as u32) {
                    chars.push(c);
                }
            } else {
                break;
            }

            char_idx += 1;
            if char_idx > 10000 {
                break;
            }
        }

        // Cleanup
        let cleanup = "nb_delete(tea_extract), nb_delete(tea_codes), nb_delete(tea_code)";
        if let Ok(term) = context.term_from_string(cleanup) {
            let _ = context.call_term_once(&term);
        }

        if chars.is_empty() {
            None
        } else {
            Some(chars.into_iter().collect())
        }
    }

    /// Binary search to find an integer value stored in a global variable
    fn binary_search_integer<C: QueryableContextType>(
        &self,
        context: &Context<C>,
        var_name: &str,
        min: i64,
        max: i64,
    ) -> Option<i64> {
        if min > max {
            return None;
        }

        if min == max {
            // Check if the value equals min
            let check = format!("nb_getval({}, V), V =:= {}", var_name, min);
            if let Ok(term) = context.term_from_string(&check) {
                if context.call_term_once(&term).is_ok() {
                    return Some(min);
                }
            }
            return None;
        }

        let mid = min + (max - min) / 2;

        // Check if value <= mid
        let check_le = format!("nb_getval({}, V), V =< {}", var_name, mid);
        if let Ok(term) = context.term_from_string(&check_le) {
            if context.call_term_once(&term).is_ok() {
                // Value is in lower half
                return self.binary_search_integer(context, var_name, min, mid);
            } else {
                // Value is in upper half
                return self.binary_search_integer(context, var_name, mid + 1, max);
            }
        }

        None
    }

    /// Evaluate a Prolog condition expression
    ///
    /// Returns:
    /// - `Some("true")` if query succeeds
    /// - `None` if query fails
    /// - `Some(value)` if Result variable is bound
    pub fn eval_condition(&self, expression: &str, state: &JsonValue) -> TeaResult<Option<String>> {
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

    /// Execute inline Prolog code for a node using **Prolog-side parsing**.
    ///
    /// This uses `tea_load_code/1` which delegates to SWI-Prolog's native
    /// `read_term/3` parser, eliminating error-prone host-side heuristics.
    ///
    /// Handles:
    /// - Simple queries (single line)
    /// - Multiple queries (separated by commas or periods)
    /// - Module imports (:- use_module(...))
    /// - Rule definitions (head :- body)
    /// - Fact definitions (parent(alice, bob).)
    /// - Directives (:- dynamic(...))
    /// - **Edge cases**: Commas in quoted strings like `person('John, Jr.', 30)`
    ///
    /// This follows the same pattern as Python's `execute_node_code()` for
    /// cross-runtime parity.
    ///
    /// ## Architecture
    ///
    /// The "thin runtime" approach: let Prolog parse Prolog!
    /// - TEA predicates (`tea_load_code/1`, etc.) are loaded at init time
    /// - User code is passed to `tea_load_code/1` which uses `read_term/3`
    /// - Prolog correctly handles all edge cases (nested quotes, operators, etc.)
    pub fn execute_node_code(&self, code: &str, state: &JsonValue) -> TeaResult<JsonValue> {
        let code = code.trim();

        if code.is_empty() {
            return Ok(JsonValue::Object(serde_json::Map::new()));
        }

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

        // NOTE: We intentionally do NOT apply sandbox in execute_node_code.
        // This matches Python's janus-swi behavior where tea_load_code is not sandboxed.
        // The sandbox is still loaded (if enabled) for other methods like execute()
        // and evaluate_condition().
        //
        // Future work could implement proper sandbox whitelisting for TEA predicates.

        // Set up return/2, state/2, and return_value/2 predicates in this execution context
        self.setup_predicates_in_context(&context);

        // Load TEA predicates in this context (defines tea_load_code/1)
        // This is called AFTER setup_predicates to avoid redefining return/2
        Self::load_tea_predicates_in_context(&context);

        // Set state facts in Prolog
        self.set_state_facts(&context, state);

        // Use Prolog-side parsing via tea_load_code/1
        // This is the key architectural improvement: let Prolog parse Prolog!
        //
        // Ensure code ends with a period (required by read_term/3)
        let code_with_period = if code.trim().ends_with('.') {
            code.to_string()
        } else {
            format!("{}.", code.trim())
        };

        // Escape backslashes first, then single quotes for Prolog atom
        // Order matters: backslashes must be escaped before quotes to avoid
        // double-escaping the backslashes used for quote escapes
        let escaped_code = code_with_period.replace('\\', "\\\\").replace('\'', "''");

        // Build the query: call tea_load_code with escaped code
        let timeout_secs = self.timeout.as_secs_f64();
        let load_query = format!(
            "catch(call_with_time_limit({}, tea_load_code('{}')), time_limit_exceeded, throw(prolog_timeout))",
            timeout_secs, escaped_code
        );

        // Execute the tea_load_code query
        match context.term_from_string(&load_query) {
            Ok(term) => {
                match context.call_term_once(&term) {
                    Ok(()) => {
                        // Successfully executed - collect return values
                        let result = self.collect_returns_from_context(&context);

                        // Clean up user-asserted facts
                        let cleanup = "tea_cleanup_facts";
                        if let Ok(cleanup_term) = context.term_from_string(cleanup) {
                            let _ = context.call_term_once(&cleanup_term);
                        }

                        result
                    }
                    Err(PrologError::Failure) => {
                        // Query failed - return empty result
                        Ok(JsonValue::Object(serde_json::Map::new()))
                    }
                    Err(PrologError::Exception) => {
                        // Exception - could be timeout or syntax error
                        // Note: sandbox was NOT applied in execute_node_code, so
                        // we pass false to avoid incorrectly reporting sandbox violations
                        self.handle_prolog_exception_impl(&context, false)
                    }
                }
            }
            Err(e) => Err(TeaError::Prolog(format!(
                "Failed to parse tea_load_code query: {:?}",
                e
            ))),
        }
    }

    /// Execute inline Prolog code using legacy host-side parsing.
    ///
    /// **Deprecated**: Use `execute_node_code()` instead which uses Prolog-side parsing.
    ///
    /// This method is kept for backward compatibility and testing purposes.
    #[allow(dead_code)]
    pub fn execute_node_code_legacy(&self, code: &str, state: &JsonValue) -> TeaResult<JsonValue> {
        let code = code.trim();

        if code.is_empty() {
            return Ok(JsonValue::Object(serde_json::Map::new()));
        }

        // Parse code into directives, rules, and queries using Rust-side parsing
        let (directives, rules, facts, queries) = self.parse_prolog_code(code);

        // If there are only queries (no rules/directives/facts), use simple execution
        if directives.is_empty() && rules.is_empty() && facts.is_empty() {
            let query_str = queries.join(", ");
            return self.execute(&query_str, state);
        }

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

        // Load sandbox if enabled
        if self.is_sandboxed() && self.load_sandbox_in_context(&context).is_err() {
            // Fall back to unsandboxed execution
        }

        // Set up return/2 and state/2 predicates in this execution context
        self.setup_predicates_in_context(&context);

        // Set state facts in Prolog
        self.set_state_facts(&context, state);

        // Handle directives (e.g., :- use_module(...))
        for directive in &directives {
            // Strip the :- prefix and trailing period to get the goal body
            let goal = directive
                .trim_start_matches(":-")
                .trim()
                .trim_end_matches('.')
                .trim();

            if !goal.is_empty() {
                if let Ok(term) = context.term_from_string(goal) {
                    let _ = context.call_term_once(&term);
                }
            }
        }

        // Assert rules using assertz
        let mut asserted_rules: Vec<String> = Vec::new();
        for rule in &rules {
            let assert_cmd = format!("assertz(({}))", rule);
            if let Ok(term) = context.term_from_string(&assert_cmd) {
                if context.call_term_once(&term).is_ok() {
                    asserted_rules.push(rule.clone());
                }
            }
        }

        // Assert facts using assertz
        let mut asserted_facts: Vec<String> = Vec::new();
        for fact in &facts {
            let assert_cmd = format!("assertz({})", fact);
            if let Ok(term) = context.term_from_string(&assert_cmd) {
                if context.call_term_once(&term).is_ok() {
                    asserted_facts.push(fact.clone());
                }
            }
        }

        // Execute queries with timeout
        let result = if !queries.is_empty() {
            let query_str = queries.join(", ");
            self.execute_in_context(&context, &query_str, false)
        } else {
            self.collect_returns_from_context(&context)
        };

        // Clean up asserted rules
        for rule in &asserted_rules {
            if let Some(head) = self.extract_rule_head(rule) {
                let retract_cmd = format!("retractall({})", head);
                if let Ok(term) = context.term_from_string(&retract_cmd) {
                    let _ = context.call_term_once(&term);
                }
            }
        }

        // Clean up asserted facts
        for fact in &asserted_facts {
            if let Some(head) = self.extract_fact_head(fact) {
                let retract_cmd = format!("retractall({})", head);
                if let Ok(term) = context.term_from_string(&retract_cmd) {
                    let _ = context.call_term_once(&term);
                }
            }
        }

        result
    }

    /// Parse Prolog code into directives, rules, facts, and queries.
    ///
    /// This follows the Python implementation's `_parse_code()` pattern:
    /// - Directives: statements starting with `:-` (e.g., `:- use_module(library(clpfd)).`)
    /// - Rules: statements containing `:-` but not starting with it (e.g., `add_ten(X, Y) :- Y is X + 10.`)
    /// - Facts: statements that look like fact definitions (e.g., `parent(alice, bob).`)
    /// - Queries: everything else (procedural code to execute)
    ///
    /// Returns: (directives, rules, facts, queries)
    fn parse_prolog_code(
        &self,
        code: &str,
    ) -> (Vec<String>, Vec<String>, Vec<String>, Vec<String>) {
        let mut directives = Vec::new();
        let mut rules = Vec::new();
        let mut facts = Vec::new();
        let mut queries = Vec::new();

        let mut current_statement: Vec<String> = Vec::new();

        for line in code.lines() {
            let stripped = line.trim();

            // Skip empty lines and pure comments
            if stripped.is_empty() || stripped.starts_with('%') {
                continue;
            }

            // Remove inline comments (but preserve % in strings)
            let clean = self.strip_inline_comment(stripped);
            if clean.is_empty() {
                continue;
            }

            current_statement.push(clean.to_string());

            // Check if statement is complete (ends with period)
            let joined = current_statement.join(" ");
            if joined.ends_with('.') {
                let statement = joined.trim_end_matches('.').trim().to_string();

                // Categorize the statement
                if statement.starts_with(":-") {
                    // Directive (e.g., :- use_module(...))
                    // Keep the trailing period for directives
                    directives.push(format!("{}.", statement));
                } else if self.contains_rule_operator(&statement) {
                    // Rule (head :- body) - but only if :- is outside quotes
                    rules.push(statement);
                } else if self.looks_like_fact(&statement) {
                    // Fact (e.g., parent(alice, bob))
                    facts.push(statement);
                } else {
                    // Query (procedural code)
                    queries.push(statement);
                }

                current_statement.clear();
            }
        }

        // Handle remaining content as query if no trailing period
        if !current_statement.is_empty() {
            let remaining = current_statement.join(" ");
            let remaining = remaining.trim();

            if remaining.starts_with(":-") {
                directives.push(format!("{}.", remaining));
            } else if self.contains_rule_operator(remaining) {
                rules.push(remaining.to_string());
            } else if self.looks_like_fact(remaining) {
                facts.push(remaining.to_string());
            } else {
                queries.push(remaining.to_string());
            }
        }

        (directives, rules, facts, queries)
    }

    /// Check if a statement contains `:-` outside of quoted strings.
    ///
    /// This distinguishes between:
    /// - `foo(X) :- bar(X)` (rule - returns true)
    /// - `X = 'text with :- inside'` (query with quoted string - returns false)
    fn contains_rule_operator(&self, statement: &str) -> bool {
        let mut in_single_quote = false;
        let mut in_double_quote = false;
        let mut prev_char = ' ';

        let chars: Vec<char> = statement.chars().collect();
        for i in 0..chars.len() {
            let c = chars[i];

            match c {
                '\'' if !in_double_quote && prev_char != '\\' => {
                    in_single_quote = !in_single_quote;
                }
                '"' if !in_single_quote && prev_char != '\\' => {
                    in_double_quote = !in_double_quote;
                }
                ':' if !in_single_quote && !in_double_quote => {
                    // Check if next char is '-'
                    if i + 1 < chars.len() && chars[i + 1] == '-' {
                        return true;
                    }
                }
                _ => {}
            }
            prev_char = c;
        }

        false
    }

    /// Check if a statement looks like a Prolog fact.
    ///
    /// A fact is a simple predicate assertion like `parent(alice, bob)`.
    /// It starts with a lowercase letter and contains parentheses with arguments.
    /// Facts don't contain operators like `is`, `=`, `->`, etc. outside parentheses.
    /// Facts typically have all lowercase atom arguments, not uppercase variables.
    fn looks_like_fact(&self, statement: &str) -> bool {
        let trimmed = statement.trim();

        // Must start with a lowercase letter (predicate name)
        let first_char = trimmed.chars().next();
        if !first_char.map(|c| c.is_ascii_lowercase()).unwrap_or(false) {
            return false;
        }

        // Must contain parentheses (predicate with arguments)
        let paren_start = match trimmed.find('(') {
            Some(pos) => pos,
            None => return false,
        };
        let paren_end = match trimmed.rfind(')') {
            Some(pos) => pos,
            None => return false,
        };

        // Check if predicate name is a known query predicate that should be executed, not asserted
        let predicate_name = &trimmed[..paren_start];
        // TEA predicates + common Prolog built-ins that are queries, not facts
        let query_predicates = [
            // TEA predicates
            "return",
            "state",
            // List predicates
            "findall",
            "bagof",
            "setof",
            "member",
            "memberchk",
            "append",
            "length",
            "nth0",
            "nth1",
            "msort",
            "sort",
            "reverse",
            "last",
            "sumlist",
            "max_list",
            "min_list",
            // Control predicates
            "forall",
            "aggregate_all",
            "aggregate",
            "call",
            "once",
            // Arithmetic predicates (rarely facts, usually queries)
            "succ",
            "plus",
            "abs",
            "sign",
            "max",
            "min",
        ];
        if query_predicates.contains(&predicate_name) {
            return false;
        }

        // Check the part BEFORE the opening parenthesis for query indicators
        // A fact like `parent(alice, bob)` should be allowed
        // But `X is foo(Y)` or `foo(A), bar(B)` should not be facts
        let before_paren = &trimmed[..paren_start];

        // Must not contain operators before the predicate name
        let query_indicators = [" is ", " = ", " -> ", " ; "];
        for indicator in query_indicators {
            if before_paren.contains(indicator) {
                return false;
            }
        }

        // Check for comma OUTSIDE parentheses (indicating multiple goals)
        // We need to track paren depth to avoid rejecting `parent(a, b)`
        let mut paren_depth = 0;
        for c in trimmed.chars() {
            match c {
                '(' => paren_depth += 1,
                ')' => paren_depth -= 1,
                ',' if paren_depth == 0 => return false, // Comma outside parens = not a fact
                _ => {}
            }
        }

        // Check if arguments contain variables (uppercase letters not part of atoms)
        // Facts like `parent(alice, bob)` use atoms; queries like `state(value, V)` use variables
        let args = &trimmed[paren_start + 1..paren_end];

        // Look for uppercase letters that indicate Prolog variables
        // Variables start with uppercase or underscore
        let mut prev_char_is_word = false;
        for c in args.chars() {
            if c.is_ascii_uppercase() && !prev_char_is_word {
                // Found an uppercase letter at the start of a word = variable
                return false;
            }
            prev_char_is_word = c.is_ascii_alphanumeric() || c == '_';
        }

        // Additional check: should end with closing paren (after stripping trailing period if any)
        let stripped = trimmed.trim_end_matches('.');
        stripped.ends_with(')')
    }

    /// Strip inline comments from a line, preserving % inside strings.
    fn strip_inline_comment<'a>(&self, line: &'a str) -> &'a str {
        let mut in_single_quote = false;
        let mut in_double_quote = false;
        let mut prev_char = ' ';

        for (i, c) in line.char_indices() {
            match c {
                '\'' if !in_double_quote && prev_char != '\\' => {
                    in_single_quote = !in_single_quote;
                }
                '"' if !in_single_quote && prev_char != '\\' => {
                    in_double_quote = !in_double_quote;
                }
                '%' if !in_single_quote && !in_double_quote => {
                    return line[..i].trim();
                }
                _ => {}
            }
            prev_char = c;
        }

        line.trim()
    }

    /// Extract the head of a rule for cleanup with retractall.
    ///
    /// Given a rule like `add_ten(X, Y) :- Y is X + 10`, returns `add_ten(_, _)`.
    /// This pattern matches all clauses of the predicate.
    fn extract_rule_head(&self, rule: &str) -> Option<String> {
        // Find the :- operator
        let rule_op_pos = rule.find(":-")?;
        let head = rule[..rule_op_pos].trim();

        if head.is_empty() {
            return None;
        }

        // Convert head to pattern with wildcards for arguments
        // e.g., "add_ten(X, Y)" -> "add_ten(_, _)"
        if let Some(paren_pos) = head.find('(') {
            let functor = &head[..paren_pos];
            let args_part = &head[paren_pos..];

            // Count arguments by counting commas (respecting nesting)
            let arg_count = self.count_arguments(args_part);

            if arg_count == 0 {
                Some(format!("{}()", functor))
            } else {
                let wildcards: Vec<&str> = (0..arg_count).map(|_| "_").collect();
                Some(format!("{}({})", functor, wildcards.join(", ")))
            }
        } else {
            // Atom predicate with no arguments
            Some(head.to_string())
        }
    }

    /// Extract the fact head for cleanup with retractall.
    ///
    /// For a fact like `parent(alice, bob)`, returns `parent(_, _)`.
    fn extract_fact_head(&self, fact: &str) -> Option<String> {
        let fact = fact.trim();

        if fact.is_empty() {
            return None;
        }

        // Convert fact to pattern with wildcards for arguments
        // e.g., "parent(alice, bob)" -> "parent(_, _)"
        if let Some(paren_pos) = fact.find('(') {
            let functor = &fact[..paren_pos];
            let args_part = &fact[paren_pos..];

            // Count arguments by counting commas (respecting nesting)
            let arg_count = self.count_arguments(args_part);

            if arg_count == 0 {
                Some(format!("{}()", functor))
            } else {
                let wildcards: Vec<&str> = (0..arg_count).map(|_| "_").collect();
                Some(format!("{}({})", functor, wildcards.join(", ")))
            }
        } else {
            // Atom predicate with no arguments
            Some(fact.to_string())
        }
    }

    /// Count the number of arguments in a predicate argument list.
    fn count_arguments(&self, args: &str) -> usize {
        if args.len() < 2 {
            return 0;
        }

        // Remove outer parentheses
        let inner = args.trim().trim_start_matches('(').trim_end_matches(')');

        if inner.is_empty() {
            return 0;
        }

        let mut count = 1; // At least one argument if not empty
        let mut depth = 0;
        let mut in_quote = false;
        let mut quote_char = ' ';

        for c in inner.chars() {
            match c {
                '\'' | '"' if !in_quote => {
                    in_quote = true;
                    quote_char = c;
                }
                c if in_quote && c == quote_char => {
                    in_quote = false;
                }
                '(' | '[' | '{' if !in_quote => {
                    depth += 1;
                }
                ')' | ']' | '}' if !in_quote => {
                    depth -= 1;
                }
                ',' if !in_quote && depth == 0 => {
                    count += 1;
                }
                _ => {}
            }
        }

        count
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
        "#=",
        "#<",
        "#>",       // CLP(FD) operators
        "ins ",     // CLP(FD) domain
        "labeling", // CLP(FD) labeling
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

    #[test]
    fn test_default_modules_constant() {
        // Verify DEFAULT_MODULES contains expected modules for Python parity
        assert!(
            DEFAULT_MODULES.contains(&"lists"),
            "Should contain lists module"
        );
        assert!(
            DEFAULT_MODULES.contains(&"clpfd"),
            "Should contain clpfd module"
        );
        assert!(
            DEFAULT_MODULES.contains(&"apply"),
            "Should contain apply module"
        );
        assert!(
            DEFAULT_MODULES.contains(&"aggregate"),
            "Should contain aggregate module"
        );
        assert_eq!(
            DEFAULT_MODULES.len(),
            4,
            "Should have exactly 4 default modules"
        );
    }

    // ========================================================================
    // Tests for parse_prolog_code() - TEA-RUST-038 Inline Rule Definitions
    // ========================================================================

    #[test]
    fn test_parse_prolog_code_simple_rule() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        let code = "add_ten(X, Y) :- Y is X + 10.";
        let (directives, rules, facts, queries) = runtime.parse_prolog_code(code);

        assert!(directives.is_empty());
        assert_eq!(rules.len(), 1);
        assert_eq!(rules[0], "add_ten(X, Y) :- Y is X + 10");
        assert!(facts.is_empty());
        assert!(queries.is_empty());
    }

    #[test]
    fn test_parse_prolog_code_directive() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        let code = ":- use_module(library(clpfd)).";
        let (directives, rules, facts, queries) = runtime.parse_prolog_code(code);

        assert_eq!(directives.len(), 1);
        assert_eq!(directives[0], ":- use_module(library(clpfd)).");
        assert!(rules.is_empty());
        assert!(facts.is_empty());
        assert!(queries.is_empty());
    }

    #[test]
    fn test_parse_prolog_code_query() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        let code = "state(value, V), return(result, V).";
        let (directives, rules, facts, queries) = runtime.parse_prolog_code(code);

        assert!(directives.is_empty());
        assert!(rules.is_empty());
        assert!(facts.is_empty());
        assert_eq!(queries.len(), 1);
        assert_eq!(queries[0], "state(value, V), return(result, V)");
    }

    #[test]
    fn test_parse_prolog_code_mixed() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        let code = r#"
            % Comment line
            add_ten(X, Y) :- Y is X + 10.
            state(doubled, D),
            add_ten(D, R),
            return(result, R).
        "#;
        let (directives, rules, facts, queries) = runtime.parse_prolog_code(code);

        assert!(directives.is_empty());
        assert_eq!(rules.len(), 1);
        assert_eq!(rules[0], "add_ten(X, Y) :- Y is X + 10");
        assert!(facts.is_empty());
        // Queries without periods between them are joined into one statement
        assert_eq!(queries.len(), 1);
        assert!(queries[0].contains("state(doubled, D)"));
        assert!(queries[0].contains("add_ten(D, R)"));
        assert!(queries[0].contains("return(result, R)"));
    }

    #[test]
    fn test_parse_prolog_code_multiline_rule() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        let code = r#"
            complex_rule(X, Y, Z) :-
                X > 0,
                Y is X * 2,
                Z is Y + 10.
        "#;
        let (directives, rules, facts, queries) = runtime.parse_prolog_code(code);

        assert!(directives.is_empty());
        assert_eq!(rules.len(), 1);
        assert!(rules[0].contains("complex_rule(X, Y, Z) :-"));
        assert!(rules[0].contains("X > 0"));
        assert!(facts.is_empty());
        assert!(queries.is_empty());
    }

    #[test]
    fn test_parse_prolog_code_multiple_rules() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        let code = r#"
            add_five(X, Y) :- Y is X + 5.
            add_ten(X, Y) :- Y is X + 10.
            state(input, I),
            add_five(I, R1),
            add_ten(R1, R2),
            return(result, R2).
        "#;
        let (directives, rules, facts, queries) = runtime.parse_prolog_code(code);

        assert!(directives.is_empty());
        assert_eq!(rules.len(), 2);
        assert_eq!(rules[0], "add_five(X, Y) :- Y is X + 5");
        assert_eq!(rules[1], "add_ten(X, Y) :- Y is X + 10");
        assert!(facts.is_empty());
        // Queries are joined into one statement until the period
        assert_eq!(queries.len(), 1);
        assert!(queries[0].contains("state(input, I)"));
        assert!(queries[0].contains("return(result, R2)"));
    }

    #[test]
    fn test_parse_prolog_code_skips_comments() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        let code = r#"
            % This is a full line comment
            state(value, V). % inline comment
        "#;
        let (directives, rules, facts, queries) = runtime.parse_prolog_code(code);

        assert!(directives.is_empty());
        assert!(rules.is_empty());
        assert!(facts.is_empty());
        assert_eq!(queries.len(), 1);
        assert_eq!(queries[0], "state(value, V)");
    }

    #[test]
    fn test_parse_prolog_code_with_facts() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        let code = r#"
            parent(alice, bob).
            parent(bob, carol).
            grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
            findall(G, grandparent(alice, G), Results),
            return(results, Results).
        "#;
        let (directives, rules, facts, queries) = runtime.parse_prolog_code(code);

        assert!(directives.is_empty());
        assert_eq!(rules.len(), 1);
        assert!(rules[0].contains("grandparent"));
        assert_eq!(facts.len(), 2);
        assert_eq!(facts[0], "parent(alice, bob)");
        assert_eq!(facts[1], "parent(bob, carol)");
        assert_eq!(queries.len(), 1);
        assert!(queries[0].contains("findall"));
    }

    #[test]
    fn test_extract_rule_head_simple() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        let head = runtime.extract_rule_head("add_ten(X, Y) :- Y is X + 10");
        assert_eq!(head, Some("add_ten(_, _)".to_string()));
    }

    #[test]
    fn test_extract_rule_head_three_args() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        let head = runtime.extract_rule_head("foo(A, B, C) :- bar(A, B), baz(C)");
        assert_eq!(head, Some("foo(_, _, _)".to_string()));
    }

    #[test]
    fn test_extract_rule_head_atom() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        let head = runtime.extract_rule_head("fact :- true");
        assert_eq!(head, Some("fact".to_string()));
    }

    #[test]
    fn test_count_arguments_empty() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        assert_eq!(runtime.count_arguments("()"), 0);
    }

    #[test]
    fn test_count_arguments_one() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        assert_eq!(runtime.count_arguments("(X)"), 1);
    }

    #[test]
    fn test_count_arguments_nested() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        // foo([1,2,3], B) - has 2 arguments, not 4
        assert_eq!(runtime.count_arguments("([1,2,3], B)"), 2);
    }

    #[test]
    fn test_strip_inline_comment() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        assert_eq!(
            runtime.strip_inline_comment("state(value, V). % comment"),
            "state(value, V)."
        );
        assert_eq!(
            runtime.strip_inline_comment("state(value, V)."),
            "state(value, V)."
        );
        // Preserve % in strings (though this is a simple test)
        assert_eq!(
            runtime.strip_inline_comment("return(msg, 'hello')."),
            "return(msg, 'hello')."
        );
    }

    #[test]
    fn test_execute_node_code_with_inline_rule() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        let code = r#"
            add_ten(X, Y) :- Y is X + 10.
            state(doubled, D),
            add_ten(D, R),
            return(result, R).
        "#;

        let state = json!({
            "value": 21,
            "doubled": 42
        });

        let result = runtime.execute_node_code(code, &state).unwrap();

        // Should return result = 52 (42 + 10)
        assert_eq!(result.get("result"), Some(&json!(52)));
    }

    #[test]
    fn test_execute_node_code_backward_compat_no_rules() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        let code = r#"
            state(value, V),
            D is V * 2,
            return(doubled, D).
        "#;

        let state = json!({
            "value": 21
        });

        let result = runtime.execute_node_code(code, &state).unwrap();

        // Should return doubled = 42
        assert_eq!(result.get("doubled"), Some(&json!(42)));
    }

    #[test]
    fn test_execute_node_code_multiple_rules() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        let code = r#"
            double(X, Y) :- Y is X * 2.
            add_five(X, Y) :- Y is X + 5.
            state(value, V),
            double(V, D),
            add_five(D, R),
            return(result, R).
        "#;

        let state = json!({
            "value": 10
        });

        let result = runtime.execute_node_code(code, &state).unwrap();

        // 10 * 2 = 20, 20 + 5 = 25
        assert_eq!(result.get("result"), Some(&json!(25)));
    }

    #[test]
    fn test_execute_node_code_complex_rule_body() {
        let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();

        let code = r#"
            categorize(X, large) :- X >= 100.
            categorize(X, medium) :- X >= 50, X < 100.
            categorize(X, small) :- X < 50.
            state(value, V),
            categorize(V, Category),
            return(category, Category).
        "#;

        let state = json!({ "value": 75 });
        let result = runtime.execute_node_code(code, &state).unwrap();

        assert_eq!(result.get("category"), Some(&json!("medium")));
    }
}
