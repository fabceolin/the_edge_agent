//! Built-in actions for The Edge Agent
//!
//! Actions are composable units of functionality that can be used in workflow nodes.

#[cfg(feature = "a2a")]
pub mod a2a;
#[cfg(feature = "agent")]
pub mod agent;
pub mod data;
pub mod file;
pub mod http;
#[cfg(feature = "llm")]
pub mod llm;
#[cfg(feature = "llm")]
pub mod llm_backend;
#[cfg(feature = "llm-local")]
pub mod llm_local;
pub mod memory;
#[cfg(feature = "planning")]
pub mod planning;
pub mod ratelimit;
#[cfg(feature = "reasoning")]
pub mod reasoning;
#[cfg(feature = "reflection")]
pub mod reflection;

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

    #[cfg(feature = "reasoning")]
    reasoning::register(registry);

    #[cfg(feature = "reflection")]
    reflection::register(registry);

    #[cfg(feature = "agent")]
    agent::register(registry);

    #[cfg(feature = "planning")]
    planning::register(registry);

    #[cfg(feature = "a2a")]
    a2a::register(registry);
}

/// Action result helper
pub type ActionResult = crate::error::TeaResult<serde_json::Value>;
