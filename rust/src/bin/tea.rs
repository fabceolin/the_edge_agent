//! The Edge Agent CLI
//!
//! Usage:
//!   tea run workflow.yaml --input '{"key": "value"}'
//!   tea run workflow.yaml --secrets '{"api_key": "sk-123"}' --input '{"prompt": "hello"}'
//!   tea run workflow.yaml --secrets @secrets.json --input '{"prompt": "hello"}'
//!   tea run workflow.yaml --secrets-env TEA_SECRET_ --input '{"prompt": "hello"}'
//!   tea resume checkpoint.bin --input '{"update": "value"}'
//!   tea validate workflow.yaml
//!   tea inspect workflow.yaml
//!   tea --impl
//!   tea --version --impl

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use the_edge_agent::actions;
use the_edge_agent::engine::checkpoint::{Checkpointer, FileCheckpointer};
use the_edge_agent::engine::executor::{ExecutionOptions, Executor};
use the_edge_agent::engine::yaml::YamlEngine;
use the_edge_agent::{TeaError, YamlConfig};

/// Implementation identifier for CLI parity (TEA-CLI-004)
const IMPLEMENTATION: &str = "rust";

/// The Edge Agent - Lightweight State Graph Workflow Engine
#[derive(Parser, Debug)]
#[command(name = "tea")]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Increase verbosity (-v, -vv, -vvv)
    #[arg(short, long, action = clap::ArgAction::Count, global = true)]
    verbose: u8,

    /// Suppress non-error output
    #[arg(short, long, global = true)]
    quiet: bool,

    /// Show implementation (python/rust)
    #[arg(long, global = true)]
    r#impl: bool,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Execute a workflow
    Run {
        /// Path to workflow YAML file
        file: PathBuf,

        /// Initial state as JSON string or @file.json
        #[arg(short, long)]
        input: Option<String>,

        /// Secrets as JSON string or @file.json
        /// Use {{ secrets.key }} in templates to reference
        #[arg(long)]
        secrets: Option<String>,

        /// Load secrets from environment variables with this prefix
        /// e.g., --secrets-env TEA_SECRET_ loads TEA_SECRET_API_KEY as secrets.api_key
        #[arg(long)]
        secrets_env: Option<String>,

        /// Output events as NDJSON stream
        #[arg(short, long)]
        stream: bool,

        /// Directory for checkpoint files
        #[arg(short = 'c', long)]
        checkpoint_dir: Option<PathBuf>,

        /// Config overrides as JSON
        #[arg(short = 'C', long)]
        config: Option<String>,

        /// Nodes to interrupt before (comma-separated)
        #[arg(long)]
        interrupt_before: Option<String>,

        /// Nodes to interrupt after (comma-separated)
        #[arg(long)]
        interrupt_after: Option<String>,

        /// Skip interactive prompts at interrupts
        #[arg(long)]
        auto_continue: bool,

        /// [NOT IMPLEMENTED] Python module for actions (Python only)
        #[arg(long, value_name = "MODULE")]
        actions_module: Option<Vec<String>>,

        /// [NOT IMPLEMENTED] Python file for actions (Python only)
        #[arg(long, value_name = "FILE")]
        actions_file: Option<Vec<String>>,
    },

    /// Resume execution from a checkpoint
    Resume {
        /// Path to checkpoint file
        checkpoint: PathBuf,

        /// Path to the original workflow YAML file
        #[arg(short, long)]
        workflow: PathBuf,

        /// State updates as JSON string or @file.json
        #[arg(short, long)]
        input: Option<String>,

        /// Secrets as JSON string or @file.json
        /// Use {{ secrets.key }} in templates to reference
        #[arg(long)]
        secrets: Option<String>,

        /// Load secrets from environment variables with this prefix
        /// e.g., --secrets-env TEA_SECRET_ loads TEA_SECRET_API_KEY as secrets.api_key
        #[arg(long)]
        secrets_env: Option<String>,

        /// Output events as NDJSON stream
        #[arg(short, long)]
        stream: bool,

        /// Directory for checkpoint files (default: same as checkpoint)
        #[arg(short = 'c', long)]
        checkpoint_dir: Option<PathBuf>,

        /// Skip interactive prompts at interrupts
        #[arg(long)]
        auto_continue: bool,
    },

    /// Validate a workflow without execution
    Validate {
        /// Path to workflow YAML file
        file: PathBuf,

        /// Show detailed validation info
        #[arg(long)]
        detailed: bool,
    },

    /// Inspect workflow structure
    Inspect {
        /// Path to workflow YAML file
        file: PathBuf,

        /// Output format (text, json, dot)
        #[arg(short, long, default_value = "text")]
        format: String,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Handle --impl flag (TEA-CLI-004)
    if cli.r#impl {
        println!("{}", IMPLEMENTATION);
        return Ok(());
    }

    // If no subcommand is provided, show help
    let command = match cli.command {
        Some(cmd) => cmd,
        None => {
            // Print help and exit
            use clap::CommandFactory;
            Cli::command().print_help()?;
            println!();
            return Ok(());
        }
    };

    // Setup logging
    let log_level = match (cli.quiet, cli.verbose) {
        (true, _) => tracing::Level::ERROR,
        (_, 0) => tracing::Level::WARN,
        (_, 1) => tracing::Level::INFO,
        (_, 2) => tracing::Level::DEBUG,
        (_, _) => tracing::Level::TRACE,
    };

    tracing_subscriber::fmt()
        .with_max_level(log_level)
        .with_target(false)
        .init();

    match command {
        Commands::Run {
            file,
            input,
            secrets,
            secrets_env,
            stream,
            checkpoint_dir,
            config,
            interrupt_before,
            interrupt_after,
            auto_continue,
            actions_module,
            actions_file,
        } => {
            // Check for not-implemented flags (TEA-CLI-004 AC-29, AC-30)
            if actions_module.is_some() {
                eprintln!("Error: --actions-module is not implemented (Python only)");
                eprintln!("Hint: Use the Python implementation for action plugins");
                std::process::exit(1);
            }
            if actions_file.is_some() {
                eprintln!("Error: --actions-file is not implemented (Python only)");
                eprintln!("Hint: Use the Python implementation for action plugins");
                std::process::exit(1);
            }

            run_workflow(
                file,
                input,
                secrets,
                secrets_env,
                stream,
                checkpoint_dir,
                config,
                interrupt_before,
                interrupt_after,
                auto_continue,
            )
        }

        Commands::Resume {
            checkpoint,
            workflow,
            input,
            secrets,
            secrets_env,
            stream,
            checkpoint_dir,
            auto_continue,
        } => resume_workflow(
            checkpoint,
            workflow,
            input,
            secrets,
            secrets_env,
            stream,
            checkpoint_dir,
            auto_continue,
        ),

        Commands::Validate { file, detailed } => validate_workflow(file, detailed),

        Commands::Inspect { file, format } => inspect_workflow(file, format),
    }
}

/// Run a workflow
#[allow(clippy::too_many_arguments)]
fn run_workflow(
    file: PathBuf,
    input: Option<String>,
    secrets: Option<String>,
    secrets_env: Option<String>,
    stream: bool,
    checkpoint_dir: Option<PathBuf>,
    _config: Option<String>,
    interrupt_before: Option<String>,
    interrupt_after: Option<String>,
    _auto_continue: bool,
) -> Result<()> {
    // Load workflow
    let mut engine = YamlEngine::new();

    // Load secrets from file/string and/or environment
    let secrets_map = parse_secrets(secrets, secrets_env)?;
    if !secrets_map.is_empty() {
        engine.set_secrets(secrets_map);
        tracing::debug!("Loaded {} secrets", engine.secrets().len());
    }

    let graph = engine
        .load_from_file(&file)
        .context(format!("Failed to load workflow from {:?}", file))?;

    // Compile graph with interrupts
    let mut compiled = graph.compile()?;

    // Merge initial state: YAML initial_state first, CLI input overlays (CLI takes precedence)
    let cli_input = parse_input(input)?;
    let initial_state = if let Some(yaml_state) = compiled.initial_state() {
        let mut merged = yaml_state.clone();
        if let (Some(merged_obj), Some(cli_obj)) = (merged.as_object_mut(), cli_input.as_object()) {
            for (key, value) in cli_obj {
                merged_obj.insert(key.clone(), value.clone());
            }
        }
        merged
    } else {
        cli_input
    };

    if let Some(nodes) = interrupt_before {
        compiled = compiled
            .with_interrupt_before(nodes.split(',').map(|s| s.trim().to_string()).collect());
    }

    if let Some(nodes) = interrupt_after {
        compiled =
            compiled.with_interrupt_after(nodes.split(',').map(|s| s.trim().to_string()).collect());
    }

    // Setup executor with actions and observability (TEA-OBS-001.2)
    let registry = Arc::new(the_edge_agent::engine::executor::ActionRegistry::new());
    actions::register_defaults(&registry);

    let executor = if let Some(obs_config) = engine.observability_config() {
        Executor::with_actions_and_observability(compiled, registry, obs_config)?
    } else {
        Executor::with_actions(compiled, registry)?
    };

    // Setup checkpointer
    let checkpointer: Option<Arc<dyn Checkpointer>> = checkpoint_dir.map(|dir| {
        Arc::new(FileCheckpointer::new(dir).expect("Failed to create checkpointer"))
            as Arc<dyn Checkpointer>
    });

    let options = ExecutionOptions {
        stream,
        checkpointer,
        resume_from: None,
        ..Default::default()
    };

    // Execute
    match executor.execute(initial_state, &options) {
        Ok(events) => {
            if stream {
                for event in events {
                    println!("{}", serde_json::to_string(&event)?);
                }
            } else {
                // Output final state
                if let Some(last) = events.last() {
                    println!("{}", serde_json::to_string_pretty(&last.state)?);
                }
            }
            Ok(())
        }
        Err(TeaError::Interrupt(node)) => {
            eprintln!("Execution interrupted at node: {}", node);
            eprintln!("Use 'tea resume <checkpoint>' to continue");
            std::process::exit(130); // SIGINT exit code
        }
        Err(e) => Err(e.into()),
    }
}

/// Resume from checkpoint
#[allow(clippy::too_many_arguments)]
fn resume_workflow(
    checkpoint_path: PathBuf,
    workflow_path: PathBuf,
    input: Option<String>,
    secrets: Option<String>,
    secrets_env: Option<String>,
    stream: bool,
    checkpoint_dir: Option<PathBuf>,
    _auto_continue: bool,
) -> Result<()> {
    // Determine checkpoint directory
    let cp_dir = checkpoint_dir.unwrap_or_else(|| {
        checkpoint_path
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| PathBuf::from("."))
    });

    let checkpointer = FileCheckpointer::new(&cp_dir)?;

    // Load checkpoint
    let mut checkpoint = checkpointer
        .load(checkpoint_path.to_str().unwrap())?
        .context("Checkpoint not found")?;

    tracing::info!(
        "Resuming from checkpoint {} at node {}",
        checkpoint.id,
        checkpoint.current_node
    );

    // Parse state updates and merge into checkpoint state
    let updates = parse_input(input)?;
    checkpoint.merge_state(&updates);

    // Load the workflow with secrets
    let mut engine = YamlEngine::new();

    // Load secrets from file/string and/or environment
    let secrets_map = parse_secrets(secrets, secrets_env)?;
    if !secrets_map.is_empty() {
        engine.set_secrets(secrets_map);
        tracing::debug!("Loaded {} secrets", engine.secrets().len());
    }

    let graph = engine
        .load_from_file(&workflow_path)
        .context(format!("Failed to load workflow from {:?}", workflow_path))?;

    // Compile graph
    let compiled = graph.compile()?;

    // Setup executor with actions and observability (TEA-OBS-001.2)
    let registry = Arc::new(the_edge_agent::engine::executor::ActionRegistry::new());
    actions::register_defaults(&registry);

    let executor = if let Some(obs_config) = engine.observability_config() {
        Executor::with_actions_and_observability(compiled, registry, obs_config)?
    } else {
        Executor::with_actions(compiled, registry)?
    };

    // Setup checkpointer for continued execution
    let checkpointer_arc: Arc<dyn Checkpointer> = Arc::new(checkpointer);

    let options = ExecutionOptions {
        stream,
        checkpointer: Some(checkpointer_arc),
        resume_from: Some(checkpoint_path.to_string_lossy().to_string()),
        ..Default::default()
    };

    // Execute from checkpoint state
    match executor.execute(checkpoint.state, &options) {
        Ok(events) => {
            if stream {
                for event in events {
                    println!("{}", serde_json::to_string(&event)?);
                }
            } else {
                // Output final state
                if let Some(last) = events.last() {
                    println!("{}", serde_json::to_string_pretty(&last.state)?);
                }
            }
            Ok(())
        }
        Err(TeaError::Interrupt(node)) => {
            eprintln!("Execution interrupted at node: {}", node);
            eprintln!("Use 'tea resume <checkpoint> --workflow <file>' to continue");
            std::process::exit(130); // SIGINT exit code
        }
        Err(e) => Err(e.into()),
    }
}

/// Validate workflow
fn validate_workflow(file: PathBuf, detailed: bool) -> Result<()> {
    let content = fs::read_to_string(&file).context(format!("Failed to read {:?}", file))?;

    // Parse YAML
    let config: YamlConfig = serde_yaml::from_str(&content).context("Failed to parse YAML")?;

    if detailed {
        println!("Workflow: {}", config.name);
        if let Some(desc) = &config.description {
            println!("Description: {}", desc);
        }
        println!("\nNodes: {}", config.nodes.len());
        for node in &config.nodes {
            print!("  - {}", node.name);
            if let Some(action) = node.uses.as_ref().or(node.action.as_ref()) {
                print!(" (uses: {})", action);
            }
            println!();
        }
        println!("\nEdges: {}", config.edges.len());
        for edge in &config.edges {
            if let Some(to) = &edge.to {
                println!("  - {} -> {}", edge.from, to);
            }
            if let Some(targets) = &edge.targets {
                for (result, target) in targets {
                    println!("  - {} --[{}]--> {}", edge.from, result, target);
                }
            }
        }
    }

    // Build and validate graph
    let engine = YamlEngine::new();
    let graph = engine.load_from_string(&content)?;
    let _compiled = graph.compile()?;

    println!("\n{} is valid", file.display());
    Ok(())
}

/// Inspect workflow
fn inspect_workflow(file: PathBuf, format: String) -> Result<()> {
    let content = fs::read_to_string(&file).context(format!("Failed to read {:?}", file))?;
    let config: YamlConfig = serde_yaml::from_str(&content)?;

    match format.as_str() {
        "json" => {
            println!("{}", serde_json::to_string_pretty(&config)?);
        }
        "dot" => {
            // Output Graphviz DOT format
            println!("digraph {} {{", config.name.replace('-', "_"));
            println!("  rankdir=TB;");
            println!("  node [shape=box];");
            println!();

            // Nodes
            for node in &config.nodes {
                let label = if let Some(action) = node.uses.as_ref().or(node.action.as_ref()) {
                    format!("{}\\n[{}]", node.name, action)
                } else {
                    node.name.clone()
                };
                println!("  \"{}\" [label=\"{}\"];", node.name, label);
            }

            println!();

            // Edges
            for edge in &config.edges {
                if let Some(to) = &edge.to {
                    if let Some(cond) = &edge.condition {
                        println!(
                            "  \"{}\" -> \"{}\" [label=\"{}\"];",
                            edge.from,
                            to,
                            cond.replace('"', "\\\"")
                        );
                    } else {
                        println!("  \"{}\" -> \"{}\";", edge.from, to);
                    }
                }
                if let Some(targets) = &edge.targets {
                    for (result, target) in targets {
                        println!(
                            "  \"{}\" -> \"{}\" [label=\"{}\"];",
                            edge.from, target, result
                        );
                    }
                }
            }

            println!("}}");
        }
        _ => {
            // Text format
            println!("Workflow: {}", config.name);
            if let Some(desc) = &config.description {
                println!("Description: {}", desc);
            }
            println!();

            println!("Nodes ({}):", config.nodes.len());
            for node in &config.nodes {
                print!("  {} ", node.name);
                if let Some(action) = node.uses.as_ref().or(node.action.as_ref()) {
                    print!("[{}]", action);
                }
                if node.run.is_some() {
                    print!(" (lua)");
                }
                if node.retry.is_some() {
                    print!(" (retry)");
                }
                if node.fallback.is_some() {
                    print!(" (fallback: {})", node.fallback.as_ref().unwrap());
                }
                println!();
            }

            println!();
            println!("Edges ({}):", config.edges.len());
            for edge in &config.edges {
                if let Some(to) = &edge.to {
                    print!("  {} -> {}", edge.from, to);
                    if let Some(cond) = &edge.condition {
                        print!(" [when: {}]", cond);
                    }
                    println!();
                }
                if let Some(targets) = &edge.targets {
                    for (result, target) in targets {
                        println!("  {} --[{}]--> {}", edge.from, result, target);
                    }
                }
                if let Some(branches) = &edge.parallel {
                    println!("  {} => [{}]", edge.from, branches.join(", "));
                }
            }

            if !config.variables.is_empty() {
                println!();
                println!("Variables:");
                for (key, value) in &config.variables {
                    println!("  {}: {}", key, value);
                }
            }
        }
    }

    Ok(())
}

/// Parse input from string or file
fn parse_input(input: Option<String>) -> Result<JsonValue> {
    match input {
        None => Ok(serde_json::json!({})),
        Some(s) if s.starts_with('@') => {
            let path = &s[1..];
            let content = fs::read_to_string(path).context(format!("Failed to read {}", path))?;
            serde_json::from_str(&content).context("Failed to parse input JSON file")
        }
        Some(s) => serde_json::from_str(&s).context("Failed to parse input JSON"),
    }
}

/// Parse secrets from JSON string/file and/or environment variables
///
/// Secrets can be provided via:
/// - `--secrets '{"key": "value"}'` - inline JSON
/// - `--secrets @secrets.json` - JSON file
/// - `--secrets-env PREFIX_` - environment variables with prefix (converted to lowercase)
///
/// When both are provided, they are merged (env vars take precedence).
fn parse_secrets(
    secrets: Option<String>,
    secrets_env: Option<String>,
) -> Result<HashMap<String, JsonValue>> {
    let mut result = HashMap::new();

    // Load from JSON string/file
    if let Some(s) = secrets {
        let json_value: JsonValue = if let Some(path) = s.strip_prefix('@') {
            let content = fs::read_to_string(path)
                .context(format!("Failed to read secrets file {}", path))?;
            serde_json::from_str(&content).context("Failed to parse secrets JSON file")?
        } else {
            serde_json::from_str(&s).context("Failed to parse secrets JSON")?
        };

        // Convert JSON object to HashMap
        if let JsonValue::Object(obj) = json_value {
            for (key, value) in obj {
                result.insert(key, value);
            }
        } else {
            anyhow::bail!("Secrets must be a JSON object");
        }
    }

    // Load from environment variables with prefix
    if let Some(prefix) = secrets_env {
        for (key, value) in std::env::vars() {
            if key.starts_with(&prefix) {
                // Remove prefix and convert to lowercase
                let secret_key = key[prefix.len()..].to_lowercase();
                if !secret_key.is_empty() {
                    // Environment vars override file-based secrets
                    result.insert(secret_key, JsonValue::String(value));
                }
            }
        }
    }

    Ok(result)
}
