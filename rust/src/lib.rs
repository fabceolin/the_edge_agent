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
//! ```rust,no_run
//! use the_edge_agent::{StateGraph, YamlEngine};
//! use the_edge_agent::engine::executor::Executor;
//!
//! // Load from YAML
//! let engine = YamlEngine::new();
//! let graph = engine.load_from_file("workflow.yaml")?;
//! let compiled = graph.compile()?;
//!
//! // Execute
//! let executor = Executor::new(compiled)?;
//! let initial_state = serde_json::json!({"input": "hello"});
//! for event in executor.stream(initial_state)? {
//!     println!("{:?}", event);
//! }
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```

pub mod actions;
pub mod engine;
mod error;

// Re-exports
pub use engine::checkpoint::{Checkpoint, Checkpointer, FileCheckpointer};
pub use engine::executor::{ExecutionEvent, ExecutionOptions};
pub use engine::graph::{CompiledGraph, Edge, EdgeType, Node, StateGraph};
pub use engine::yaml::{YamlConfig, YamlEngine};
pub use error::{TeaError, TeaResult};

/// Special constant for the END node
pub const END: &str = "__end__";

/// Special constant for the START node
pub const START: &str = "__start__";

/// Prelude module for convenient imports
pub mod prelude {
    pub use crate::{
        Checkpoint, Checkpointer, CompiledGraph, Edge, EdgeType, ExecutionEvent, ExecutionOptions,
        FileCheckpointer, Node, StateGraph, TeaError, TeaResult, YamlConfig, YamlEngine, END,
        START,
    };
}
