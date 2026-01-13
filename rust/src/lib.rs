//! The Edge Agent (tea) - Lightweight State Graph Workflow Engine
//!
//! A Rust implementation of a state graph workflow engine inspired by LangGraph,
//! designed for edge computing environments with minimal resource footprint.
//!
//! # Features
//!
//! - Static graph execution from YAML configuration
//! - Lua 5.4 integration for custom logic and conditional expressions
//! - Parallel fan-out/fan-in execution via rayon
//! - Checkpoint persistence for save/resume capability
//! - Template variable substitution with Tera
//! - Built-in actions for LLM, HTTP, file operations, and more
//!
//! # Quick Start
//!
//! Load and execute a workflow from YAML:
//!
//! ```rust
//! use the_edge_agent::prelude::*;
//!
//! let yaml = r#"
//! name: greeting
//! nodes:
//!   - name: greet
//!     run: |
//!       return { greeting = "Hello, " .. (state.name or "World") }
//! edges:
//!   - from: __start__
//!     to: greet
//!   - from: greet
//!     to: __end__
//! "#;
//!
//! // Load from YAML string
//! let graph = StateGraph::from_yaml(yaml)?;
//! let compiled = graph.compile()?;
//!
//! // Execute
//! let executor = Executor::new(compiled)?;
//! let result = executor.invoke(serde_json::json!({"name": "World"}))?;
//!
//! assert_eq!(result["greeting"], "Hello, World");
//! # Ok::<(), the_edge_agent::TeaError>(())
//! ```
//!
//! # Custom Actions
//!
//! Register custom Rust functions as actions callable from YAML:
//!
//! ```rust
//! use the_edge_agent::prelude::*;
//! use std::sync::Arc;
//!
//! // Create action registry with custom action
//! let registry = Arc::new(ActionRegistry::new());
//! registry.register("custom.greet", |state, params| {
//!     let name = params.get("name")
//!         .and_then(|v| v.as_str())
//!         .unwrap_or("World");
//!     let mut result = state.clone();
//!     result["greeting"] = serde_json::json!(format!("Hello, {}!", name));
//!     Ok(result)
//! });
//!
//! // Verify action is registered
//! assert!(registry.has("custom.greet"));
//! ```
//!
//! # Programmatic Graph Building
//!
//! Build graphs programmatically without YAML:
//!
//! ```rust
//! use the_edge_agent::prelude::*;
//!
//! let mut graph = StateGraph::new();
//!
//! // Add a node with a run function
//! graph.add_node(Node::new("process").with_run(|state| {
//!     let mut new_state = state.clone();
//!     new_state["processed"] = serde_json::json!(true);
//!     Ok(new_state)
//! }));
//!
//! graph.set_entry_point("process")?;
//! graph.set_finish_point("process")?;
//!
//! let compiled = graph.compile()?;
//! let executor = Executor::new(compiled)?;
//! let result = executor.invoke(serde_json::json!({"input": "test"}))?;
//!
//! assert_eq!(result["processed"], true);
//! # Ok::<(), the_edge_agent::TeaError>(())
//! ```
//!
//! # API Stability
//!
//! This crate is pre-1.0. The API may change between minor versions.
//! Core types (`StateGraph`, `Executor`, `ActionRegistry`) are considered stable.

pub mod actions;
pub mod engine;
mod error;
#[cfg(feature = "scryer")]
pub mod prolog;
pub mod report;

// Re-exports
pub use engine::checkpoint::{Checkpoint, Checkpointer, FileCheckpointer};
pub use engine::executor::{
    ActionRegistry, EventType, ExecutionEvent, ExecutionOptions, Executor, LoopExitReason,
    StreamIterator,
};
pub use engine::graph::{CompiledGraph, Edge, EdgeType, Node, NodeType, StateGraph};
pub use engine::yaml::{RateLimiterConfig, SettingsConfig, YamlConfig, YamlEngine};
pub use error::{TeaError, TeaResult};

// Scryer Prolog runtime (TEA-RELEASE-005.1)
#[cfg(feature = "scryer")]
pub use prolog::ScryerRuntime;

/// Special constant for the END node
pub const END: &str = "__end__";

/// Special constant for the START node
pub const START: &str = "__start__";

/// Prelude module for convenient imports
///
/// Import all commonly-used types with a single statement:
///
/// ```rust
/// use the_edge_agent::prelude::*;
/// ```
pub mod prelude {
    pub use crate::actions::register_defaults;
    pub use crate::{
        ActionRegistry, Checkpoint, Checkpointer, CompiledGraph, Edge, EdgeType, ExecutionEvent,
        ExecutionOptions, Executor, FileCheckpointer, LoopExitReason, Node, NodeType, StateGraph,
        TeaError, TeaResult, YamlConfig, YamlEngine, END, START,
    };
}
