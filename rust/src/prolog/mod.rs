//! Prolog runtime backends for The Edge Agent
//!
//! This module provides multiple Prolog backend implementations:
//!
//! - **SWI-Prolog** (`prolog` feature): Traditional C-based SWI-Prolog via swipl-rs
//! - **Scryer Prolog** (`scryer` feature): Pure Rust implementation (TEA-RELEASE-005.1)
//!
//! ## Backend Selection
//!
//! Use Cargo feature flags to select the backend:
//!
//! ```toml
//! # SWI-Prolog (requires libswipl.so)
//! the_edge_agent = { features = ["prolog"] }
//!
//! # Scryer Prolog (pure Rust, no external dependencies)
//! the_edge_agent = { features = ["scryer"] }
//! ```
//!
//! ## API Parity
//!
//! Both backends implement the same interface for state access and return values:
//!
//! ```prolog
//! % Read state value
//! state(key, Value).
//!
//! % Return a value
//! return(key, Value).
//! ```
//!
//! ## Known Differences
//!
//! See `docs/shared/scryer-vs-swi.md` for syntax differences between backends.

#[cfg(feature = "scryer")]
pub mod scryer_backend;

#[cfg(feature = "scryer")]
pub use scryer_backend::ScryerRuntime;
