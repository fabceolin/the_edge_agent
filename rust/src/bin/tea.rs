//! The Edge Agent CLI
//!
//! Usage:
//!   tea run workflow.yaml --input '{"key": "value"}'
//!   tea resume checkpoint.bin --input '{"update": "value"}'
//!   tea validate workflow.yaml
//!   tea inspect workflow.yaml

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use serde_json::Value as JsonValue;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use the_edge_agent::actions;
use the_edge_agent::engine::checkpoint::{Checkpoint, Checkpointer, FileCheckpointer};
use the_edge_agent::engine::executor::{ExecutionOptions, Executor};
use the_edge_agent::engine::yaml::YamlEngine;
use the_edge_agent::{TeaError, YamlConfig};

/// The Edge Agent - Lightweight State Graph Workflow Engine
#[derive(Parser, Debug)]
#[command(name = "tea")]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Increase verbosity (-v, -vv, -vvv)
    #[arg(short, long, action = clap::ArgAction::Count, global = true)]
    verbose: u8,

    /// Suppress non-error output
    #[arg(short, long, global = true)]
    quiet: bool,
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
    },

    /// Resume execution from a checkpoint
    Resume {
        /// Path to checkpoint file
        checkpoint: PathBuf,

        /// State updates as JSON string or @file.json
        #[arg(short, long)]
        input: Option<String>,

        /// Output events as NDJSON stream
        #[arg(short, long)]
        stream: bool,

        /// Directory for checkpoint files (default: same as checkpoint)
        #[arg(short = 'c', long)]
        checkpoint_dir: Option<PathBuf>,
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

    match cli.command {
        Commands::Run {
            file,
            input,
            stream,
            checkpoint_dir,
            config,
            interrupt_before,
            interrupt_after,
        } => run_workflow(
            file,
            input,
            stream,
            checkpoint_dir,
            config,
            interrupt_before,
            interrupt_after,
        ),

        Commands::Resume {
            checkpoint,
            input,
            stream,
            checkpoint_dir,
        } => resume_workflow(checkpoint, input, stream, checkpoint_dir),

        Commands::Validate { file, detailed } => validate_workflow(file, detailed),

        Commands::Inspect { file, format } => inspect_workflow(file, format),
    }
}

/// Run a workflow
fn run_workflow(
    file: PathBuf,
    input: Option<String>,
    stream: bool,
    checkpoint_dir: Option<PathBuf>,
    _config: Option<String>,
    interrupt_before: Option<String>,
    interrupt_after: Option<String>,
) -> Result<()> {
    // Load workflow
    let engine = YamlEngine::new();
    let mut graph = engine
        .load_from_file(&file)
        .context(format!("Failed to load workflow from {:?}", file))?;

    // Parse initial state
    let initial_state = parse_input(input)?;

    // Compile graph with interrupts
    let mut compiled = graph.compile()?;

    if let Some(nodes) = interrupt_before {
        compiled = compiled
            .with_interrupt_before(nodes.split(',').map(|s| s.trim().to_string()).collect());
    }

    if let Some(nodes) = interrupt_after {
        compiled =
            compiled.with_interrupt_after(nodes.split(',').map(|s| s.trim().to_string()).collect());
    }

    // Setup executor with actions
    let registry = Arc::new(the_edge_agent::engine::executor::ActionRegistry::new());
    actions::register_defaults(&registry);

    let executor = Executor::with_actions(compiled, registry)?;

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
fn resume_workflow(
    checkpoint_path: PathBuf,
    input: Option<String>,
    stream: bool,
    checkpoint_dir: Option<PathBuf>,
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
    let checkpoint = checkpointer
        .load(checkpoint_path.to_str().unwrap())?
        .context("Checkpoint not found")?;

    tracing::info!(
        "Resuming from checkpoint {} at node {}",
        checkpoint.id,
        checkpoint.current_node
    );

    // Parse state updates
    let updates = parse_input(input)?;

    // Merge updates into checkpoint state
    let mut state = checkpoint.state;
    if let (Some(base), Some(upd)) = (state.as_object_mut(), updates.as_object()) {
        for (k, v) in upd {
            base.insert(k.clone(), v.clone());
        }
    }

    // We need the original workflow to resume
    // The checkpoint should store the graph name or we need it as an argument
    eprintln!("Note: Resume requires the original workflow file.");
    eprintln!(
        "Checkpoint state: {}",
        serde_json::to_string_pretty(&state)?
    );

    // For now, just output the merged state
    println!("{}", serde_json::to_string_pretty(&state)?);

    Ok(())
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
