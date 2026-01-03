//! Built-in actions for The Edge Agent
//!
//! Actions are composable units of functionality that can be used in workflow nodes.

pub mod data;
pub mod file;
pub mod http;
pub mod llm;
pub mod memory;
pub mod ratelimit;

use crate::engine::executor::ActionRegistry;

/// Register all default actions with the registry
pub fn register_defaults(registry: &ActionRegistry) {
    http::register(registry);
    file::register(registry);
    memory::register(registry);
    data::register(registry);
    ratelimit::register(registry);

    #[cfg(feature = "llm")]
    llm::register(registry);
}

/// Action result helper
pub type ActionResult = crate::error::TeaResult<serde_json::Value>;
