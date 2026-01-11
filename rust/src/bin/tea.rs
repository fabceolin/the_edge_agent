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
use std::io::{self, BufRead, Write};
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
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
#[allow(clippy::large_enum_variant)]
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

        /// Enable interactive human-in-the-loop mode
        #[arg(short = 'I', long, conflicts_with = "stream")]
        interactive: bool,

        /// State key(s) to extract question from (comma-separated)
        #[arg(long, default_value = "question,prompt,message,ask,next_question")]
        question_key: String,

        /// State key to inject user response into
        #[arg(long, default_value = "response")]
        response_key: String,

        /// State key(s) that signal completion (comma-separated)
        #[arg(long, default_value = "complete,done,finished")]
        complete_key: String,

        /// Response to use when user types 'skip'
        #[arg(long, default_value = "I don't have information about this.")]
        skip_response: String,

        /// State key(s) to display to user (comma-separated, default: question only)
        #[arg(long)]
        display_key: Option<String>,

        /// Display format: pretty, json, raw (default: pretty)
        #[arg(long, default_value = "pretty")]
        display_format: String,

        /// Input timeout in seconds (for automated testing)
        #[arg(long)]
        input_timeout: Option<u64>,

        /// Allow cycles in the graph (for feedback loops like QA retry patterns)
        /// Overrides YAML settings.allow_cycles
        #[arg(long)]
        allow_cycles: bool,

        /// Maximum iterations for cyclic graphs (prevents infinite loops)
        /// Overrides YAML settings.max_iterations
        #[arg(long)]
        max_iterations: Option<usize>,

        /// YE.8: Overlay YAML file(s) to merge with base. Applied in order (last wins).
        #[arg(short = 'f', long = "overlay")]
        overlay: Option<Vec<PathBuf>>,

        /// YE.8: Output merged YAML to stdout without executing
        #[arg(long)]
        dump_merged: bool,

        /// TEA-CLI-001: Path to GGUF model file for local LLM inference.
        /// Overrides TEA_MODEL_PATH and YAML settings.llm.model_path.
        #[arg(long)]
        gguf: Option<PathBuf>,

        /// TEA-CLI-001: LLM backend selection: local, api, or auto.
        /// Overrides YAML settings.llm.backend.
        #[arg(long)]
        backend: Option<String>,
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

    /// Convert from other formats to YAML workflow
    From {
        #[command(subcommand)]
        source: FromSource,
    },
}

#[derive(Subcommand, Debug)]
enum FromSource {
    /// Convert DOT (Graphviz) file to YAML workflow
    Dot {
        /// Path to DOT file
        file: PathBuf,

        /// Use per-node commands from command="" attributes
        #[arg(long)]
        use_node_commands: bool,

        /// Output file path (default: stdout)
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Override tea executable name in commands (e.g., tea-python, tea-rust)
        /// Replaces "tea" at start of command with the specified executable
        #[arg(long)]
        tea_executable: Option<String>,

        /// Custom workflow name (default: from DOT graph name)
        #[arg(short = 'n', long)]
        name: Option<String>,

        /// Maximum concurrency for parallel execution (default: 3)
        #[arg(short = 'm', long, default_value = "3")]
        max_workers: usize,

        /// Validate generated YAML before writing
        #[arg(long)]
        validate: bool,
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
            interactive,
            question_key,
            response_key,
            complete_key,
            skip_response,
            display_key,
            display_format,
            input_timeout,
            allow_cycles,
            max_iterations,
            overlay,
            dump_merged,
            gguf,
            backend,
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

            // TEA-CLI-001: Validate and process --backend parameter
            let cli_backend: Option<String> = if let Some(ref b) = backend {
                let b_lower = b.to_lowercase();
                if !["local", "api", "auto"].contains(&b_lower.as_str()) {
                    eprintln!(
                        "Error: --backend must be one of: local, api, auto. Got: {}",
                        b
                    );
                    std::process::exit(1);
                }
                Some(b_lower)
            } else {
                None
            };

            // TEA-CLI-001: Validate and expand --gguf path
            let cli_model_path: Option<PathBuf> = if let Some(ref gguf_path) = gguf {
                // Expand ~ and environment variables using shellexpand
                let path_str = gguf_path.to_string_lossy();
                let expanded = shellexpand::full(&path_str)
                    .map(|s| PathBuf::from(s.into_owned()))
                    .unwrap_or_else(|_| gguf_path.clone());

                // Validate file exists
                if !expanded.exists() {
                    eprintln!("Error: GGUF file not found: {}", expanded.display());
                    std::process::exit(1);
                }

                Some(expanded)
            } else {
                None
            };

            // AC-5: --gguf implies --backend local unless explicitly specified
            let effective_backend = if cli_model_path.is_some() && cli_backend.is_none() {
                Some("local".to_string())
            } else {
                cli_backend
            };

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
                interactive,
                question_key,
                response_key,
                complete_key,
                skip_response,
                display_key,
                display_format,
                input_timeout,
                allow_cycles,
                max_iterations,
                overlay,
                dump_merged,
                cli_model_path,
                effective_backend,
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

        Commands::From { source } => match source {
            FromSource::Dot {
                file,
                use_node_commands,
                output,
                tea_executable,
                name,
                max_workers,
                validate,
            } => from_dot(
                file,
                use_node_commands,
                output,
                tea_executable,
                name,
                max_workers,
                validate,
            ),
        },
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
    interactive: bool,
    question_key: String,
    response_key: String,
    complete_key: String,
    skip_response: String,
    display_key: Option<String>,
    display_format: String,
    input_timeout: Option<u64>,
    allow_cycles: bool,
    max_iterations: Option<usize>,
    overlay: Option<Vec<PathBuf>>,
    dump_merged: bool,
    // TEA-CLI-001: CLI overrides for LLM model
    _cli_model_path: Option<PathBuf>,
    _cli_backend: Option<String>,
) -> Result<()> {
    use the_edge_agent::engine::deep_merge::merge_all;

    // YE.8: Handle overlay merging and --dump-merged
    // Compute the YAML content to use (merged or original file)
    let yaml_content: Option<String> = if overlay.is_some() || dump_merged {
        // Load base YAML
        let base_content =
            fs::read_to_string(&file).context(format!("Failed to read base file {:?}", file))?;
        let base_value: serde_yaml::Value = serde_yaml::from_str(&base_content)
            .context(format!("Invalid YAML in base file {:?}", file))?;

        // Collect configs to merge
        let mut configs = vec![base_value];

        // Load overlay files
        if let Some(overlay_paths) = &overlay {
            for overlay_path in overlay_paths {
                if !overlay_path.exists() {
                    eprintln!(
                        "Error: Overlay file not found: {:?}",
                        overlay_path.canonicalize().unwrap_or(overlay_path.clone())
                    );
                    std::process::exit(1);
                }
                let overlay_content = fs::read_to_string(overlay_path)
                    .context(format!("Failed to read overlay file {:?}", overlay_path))?;
                let overlay_value: serde_yaml::Value = serde_yaml::from_str(&overlay_content)
                    .context(format!("Invalid YAML in overlay file {:?}", overlay_path))?;

                // Handle empty YAML files (null) as empty mapping
                let overlay_value = if overlay_value.is_null() {
                    serde_yaml::Value::Mapping(serde_yaml::Mapping::new())
                } else {
                    overlay_value
                };

                configs.push(overlay_value);
            }
        }

        // Merge all configs
        let merged = merge_all(configs);

        // Handle --dump-merged: output and exit
        if dump_merged {
            let yaml_output =
                serde_yaml::to_string(&merged).context("Failed to serialize merged YAML")?;
            print!("{}", yaml_output);
            return Ok(());
        }

        // Return merged YAML string
        Some(serde_yaml::to_string(&merged)?)
    } else {
        None
    };

    // Load workflow
    let mut engine = YamlEngine::new();

    // Load secrets from file/string and/or environment
    let secrets_map = parse_secrets(secrets, secrets_env)?;
    if !secrets_map.is_empty() {
        engine.set_secrets(secrets_map);
        tracing::debug!("Loaded {} secrets", engine.secrets().len());
    }

    // Load graph from merged content or file
    let mut graph = if let Some(content) = yaml_content {
        engine
            .load_from_string(&content)
            .context("Failed to load merged workflow")?
    } else {
        engine
            .load_from_file(&file)
            .context(format!("Failed to load workflow from {:?}", file))?
    };

    // TEA-RUST-044: Apply CLI cycle overrides (CLI takes precedence over YAML settings)
    if allow_cycles {
        graph = graph.allow_cycles();
    }
    if let Some(max_iter) = max_iterations {
        graph = graph.with_max_iterations(max_iter);
    }

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

    // Get workflow name before executor takes ownership (for interactive mode banner)
    let workflow_name_from_graph = compiled.name().to_string();

    // Setup executor with actions and observability (TEA-OBS-001.2)
    let registry = Arc::new(the_edge_agent::engine::executor::ActionRegistry::new());
    actions::register_defaults(&registry);

    let executor = if let Some(obs_config) = engine.observability_config() {
        Executor::with_actions_and_observability(compiled, registry, obs_config)?
    } else {
        Executor::with_actions(compiled, registry)?
    };

    // Interactive mode requires checkpointing (AC-14)
    let checkpoint_dir = if interactive {
        Some(checkpoint_dir.unwrap_or_else(|| PathBuf::from("/tmp/tea_checkpoints")))
    } else {
        checkpoint_dir
    };

    // Setup checkpointer
    let checkpointer: Option<Arc<dyn Checkpointer>> = checkpoint_dir.clone().map(|dir| {
        fs::create_dir_all(&dir).expect("Failed to create checkpoint directory");
        Arc::new(FileCheckpointer::new(dir).expect("Failed to create checkpointer"))
            as Arc<dyn Checkpointer>
    });

    let options = ExecutionOptions {
        stream,
        checkpointer: checkpointer.clone(),
        resume_from: None,
        ..Default::default()
    };

    // Branch to interactive mode if requested
    if interactive {
        // Get workflow name for banner
        let workflow_name = if workflow_name_from_graph.is_empty() {
            file.file_stem()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_else(|| "Workflow".to_string())
        } else {
            workflow_name_from_graph
        };

        return run_interactive(
            executor,
            initial_state,
            options,
            checkpoint_dir.unwrap(),
            workflow_name,
            question_key,
            response_key,
            complete_key,
            skip_response,
            display_key,
            display_format,
            input_timeout,
        );
    }

    // Execute (non-interactive mode)
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

/// Run workflow in interactive mode (TEA-CLI-005a, TEA-CLI-005b)
/// Loops on interrupts, prompting user for input until completion or quit.
#[allow(clippy::too_many_arguments)]
fn run_interactive(
    executor: Executor,
    initial_state: JsonValue,
    options: ExecutionOptions,
    checkpoint_dir: PathBuf,
    workflow_name: String,
    question_key: String,
    response_key: String,
    complete_key: String,
    skip_response: String,
    display_key: Option<String>,
    display_format: String,
    input_timeout: Option<u64>,
) -> Result<()> {
    // Setup Ctrl+C signal handler (AC-10)
    let interrupted = Arc::new(AtomicBool::new(false));
    let interrupted_clone = interrupted.clone();
    let checkpoint_dir_clone = checkpoint_dir.clone();

    ctrlc::set_handler(move || {
        interrupted_clone.store(true, Ordering::SeqCst);
        eprintln!();
        eprintln!("------------------------------------------------------------");
        eprintln!(" Interrupted! Saving checkpoint...");
        eprintln!("------------------------------------------------------------");
        eprintln!();
        eprintln!("Checkpoint directory: {:?}", checkpoint_dir_clone);
        eprintln!("Resume with: tea resume <checkpoint> --workflow <file>");
    })
    .expect("Error setting Ctrl-C handler");

    // Parse key lists
    let question_keys: Vec<&str> = question_key.split(',').map(|s| s.trim()).collect();
    let complete_keys: Vec<&str> = complete_key.split(',').map(|s| s.trim()).collect();
    let display_keys: Option<Vec<&str>> = display_key
        .as_ref()
        .map(|k| k.split(',').map(|s| s.trim()).collect());

    let mut current_state = initial_state;
    let mut iteration = 0;

    // Display welcome banner (AC-1, AC-2)
    display_welcome(&workflow_name);

    loop {
        // Check if interrupted by Ctrl+C (AC-10)
        if interrupted.load(Ordering::SeqCst) {
            // Find latest checkpoint to report
            if let Ok(Some(checkpoint_path)) = find_latest_checkpoint(&checkpoint_dir) {
                eprintln!("Checkpoint saved: {:?}", checkpoint_path);
            }
            return Ok(());
        }

        iteration += 1;

        // Execute workflow
        match executor.execute(current_state.clone(), &options) {
            Ok(events) => {
                // Workflow completed normally (reached __end__) - AC-17
                eprintln!();
                eprintln!("============================================================");
                eprintln!(" Workflow complete!");
                eprintln!("============================================================");
                if let Some(last) = events.last() {
                    // Display final state respecting filters (AC-9)
                    display_final_state(&last.state, display_keys.as_deref(), &display_format);
                }
                return Ok(());
            }
            Err(TeaError::Interrupt(node)) => {
                tracing::debug!("Interrupted at node: {}", node);

                // Find latest checkpoint - AC-16
                let checkpoint_path = match find_latest_checkpoint(&checkpoint_dir)? {
                    Some(path) => path,
                    None => {
                        // AC-13: Helpful checkpoint error message
                        eprintln!();
                        eprintln!("============================================================");
                        eprintln!(" Error: Checkpoint Not Found");
                        eprintln!("============================================================");
                        eprintln!();
                        eprintln!("An interrupt occurred but no checkpoint file was found.");
                        eprintln!();
                        eprintln!("Possible causes:");
                        eprintln!("  - The checkpoint directory is not writable");
                        eprintln!("  - The checkpoint was not created before the interrupt");
                        eprintln!("  - Permissions issue on the directory");
                        eprintln!();
                        eprintln!("Checkpoint directory: {:?}", checkpoint_dir);
                        eprintln!();
                        eprintln!("Try:");
                        eprintln!("  1. Ensure the directory exists and is writable");
                        eprintln!("  2. Check disk space availability");
                        eprintln!("  3. Run with -v for verbose logging");
                        std::process::exit(1);
                    }
                };

                // Load checkpoint state
                let checkpointer = FileCheckpointer::new(&checkpoint_dir)?;
                let checkpoint = match checkpointer.load(checkpoint_path.to_str().unwrap())? {
                    Some(cp) => cp,
                    None => {
                        // AC-13: Helpful checkpoint load error
                        eprintln!();
                        eprintln!("============================================================");
                        eprintln!(" Error: Failed to Load Checkpoint");
                        eprintln!("============================================================");
                        eprintln!();
                        eprintln!("The checkpoint file exists but could not be loaded.");
                        eprintln!("File: {:?}", checkpoint_path);
                        eprintln!();
                        eprintln!("Possible causes:");
                        eprintln!("  - Checkpoint file is corrupted");
                        eprintln!("  - File was created by incompatible version");
                        eprintln!("  - Partial write due to previous crash");
                        eprintln!();
                        eprintln!("Try:");
                        eprintln!("  1. Delete the checkpoint and restart");
                        eprintln!("  2. Check for other checkpoint files in directory");
                        std::process::exit(1);
                    }
                };

                let checkpoint_state = checkpoint.state;

                // AC-14: Handle empty state gracefully
                if checkpoint_state
                    .as_object()
                    .map(|m| m.is_empty())
                    .unwrap_or(true)
                {
                    eprintln!();
                    eprintln!("------------------------------------------------------------");
                    eprintln!(" Warning: Empty State");
                    eprintln!("------------------------------------------------------------");
                    eprintln!();
                    eprintln!("The workflow state is empty at this interrupt point.");
                    eprintln!("This may indicate the workflow hasn't produced output yet.");
                    eprintln!("Continuing to next iteration...");
                    eprintln!();
                    current_state = checkpoint_state;
                    continue;
                }

                // Check for completion via complete keys - AC-5, AC-9
                if is_complete(&checkpoint_state, &complete_keys) {
                    eprintln!();
                    eprintln!("============================================================");
                    eprintln!(" Complete!");
                    eprintln!("============================================================");
                    display_final_state(
                        &checkpoint_state,
                        display_keys.as_deref(),
                        &display_format,
                    );
                    return Ok(());
                }

                // Extract question
                let question = extract_question(&checkpoint_state, &question_keys);

                // Display question with formatting (AC-3, AC-4, AC-7, AC-8)
                display_question(
                    question.as_deref(),
                    &checkpoint_state,
                    iteration,
                    display_keys.as_deref(),
                    &display_format,
                    &question_keys,
                );

                // Prompt for input
                eprint!("Your answer: ");
                io::stderr().flush()?;

                // Read user input with optional timeout (AC-11)
                let input_result = if let Some(timeout_secs) = input_timeout {
                    read_multiline_input_with_timeout(std::time::Duration::from_secs(timeout_secs))
                } else {
                    read_multiline_input()
                };

                match input_result {
                    Ok(InteractiveCommand::Quit) => {
                        // quit/exit ends session
                        eprintln!();
                        eprintln!("------------------------------------------------------------");
                        eprintln!(" Session ended by user.");
                        eprintln!("------------------------------------------------------------");
                        eprintln!();
                        eprintln!("Checkpoint saved: {:?}", checkpoint_path);
                        eprintln!(
                            "Resume with: tea resume {:?} --workflow <file>",
                            checkpoint_path
                        );
                        return Ok(());
                    }
                    Ok(InteractiveCommand::Skip) => {
                        // skip sends default skip response
                        eprintln!();
                        eprintln!(" (Skipping with default response)");
                        let mut new_state = checkpoint_state.clone();
                        inject_response(&mut new_state, &response_key, &skip_response);
                        current_state = new_state;
                    }
                    Ok(InteractiveCommand::Response(response)) => {
                        // Inject response into state
                        let mut new_state = checkpoint_state.clone();
                        inject_response(&mut new_state, &response_key, &response);
                        current_state = new_state;
                    }
                    Ok(InteractiveCommand::Timeout) => {
                        // Input timeout occurred
                        eprintln!();
                        eprintln!("------------------------------------------------------------");
                        eprintln!(" Input timeout");
                        eprintln!("------------------------------------------------------------");
                        eprintln!();
                        eprintln!(
                            "No input received within {} seconds.",
                            input_timeout.unwrap()
                        );
                        eprintln!("Checkpoint saved: {:?}", checkpoint_path);
                        return Ok(());
                    }
                    Err(e) => {
                        // Input error
                        eprintln!();
                        eprintln!("Input error: {}", e);
                        return Err(e);
                    }
                }

                // Update options to resume from checkpoint
                // Note: The next execute call will use the updated current_state
            }
            Err(e) => {
                // Other errors propagate up
                return Err(e.into());
            }
        }
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

/// Convert DOT file to YAML workflow
#[allow(clippy::too_many_arguments)]
fn from_dot(
    file: PathBuf,
    use_node_commands: bool,
    output: Option<PathBuf>,
    tea_executable: Option<String>,
    custom_name: Option<String>,
    max_workers: usize,
    validate: bool,
) -> Result<()> {
    if !use_node_commands {
        anyhow::bail!(
            "--use-node-commands is required\n\
            Hint: tea from dot workflow.dot --use-node-commands -o workflow.yaml"
        );
    }

    eprintln!("Error: tea from dot is not yet implemented");
    eprintln!();
    eprintln!("This feature will:");
    eprintln!("  1. Parse DOT (Graphviz) files");
    eprintln!("  2. Extract node commands and dependencies");
    eprintln!("  3. Generate executable YAML workflows");
    eprintln!();
    eprintln!("Arguments you provided:");
    eprintln!("  File: {:?}", file);
    eprintln!("  Use node commands: {}", use_node_commands);
    if let Some(name) = &custom_name {
        eprintln!("  Workflow name: {}", name);
    }
    if let Some(tea_exe) = &tea_executable {
        eprintln!(
            "  Tea executable: {} (will replace 'tea' in commands)",
            tea_exe
        );
    }
    eprintln!("  Max workers: {}", max_workers);
    eprintln!("  Validate: {}", validate);
    if let Some(out) = &output {
        eprintln!("  Output: {:?}", out);
    } else {
        eprintln!("  Output: stdout");
    }
    eprintln!();
    eprintln!("Workaround: Use the Python implementation for DOT conversion");
    eprintln!("  # Generate YAML from DOT");
    eprintln!("  python -m the_edge_agent.tools.dot_to_yaml workflow.dot -o workflow.yaml");
    eprintln!();
    eprintln!("See: docs/shared/DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md");

    std::process::exit(1);
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

// ============================================================================
// Interactive Mode Helpers (TEA-CLI-005a, TEA-CLI-005b)
// ============================================================================

/// User command during interactive mode
#[derive(Debug, PartialEq)]
enum InteractiveCommand {
    /// User provided response text
    Response(String),
    /// User wants to quit/exit
    Quit,
    /// User wants to skip this question
    Skip,
    /// Input timeout occurred (TEA-CLI-005b)
    Timeout,
}

// ----------------------------------------------------------------------------
// Display Functions (TEA-CLI-005b)
// ----------------------------------------------------------------------------

/// Display welcome banner with workflow name (AC-1, AC-2)
fn display_welcome(workflow_name: &str) {
    eprintln!();
    eprintln!("============================================================");
    eprintln!("   TEA - {} (Interactive Mode)", workflow_name);
    eprintln!("============================================================");
    eprintln!();
    eprintln!("Commands:");
    eprintln!("  - Type your answer and press Enter twice to send");
    eprintln!("  - 'quit' or 'exit' to end session");
    eprintln!("  - 'skip' to skip current question");
    eprintln!();
    eprintln!("------------------------------------------------------------");
}

/// Display question with iteration counter and optional state keys (AC-3, AC-4, AC-7, AC-8, AC-12)
fn display_question(
    question: Option<&str>,
    state: &JsonValue,
    iteration: usize,
    display_keys: Option<&[&str]>,
    format: &str,
    expected_keys: &[&str],
) {
    eprintln!();
    eprintln!("------------------------------------------------------------");
    eprintln!(" Question ({})", iteration);
    eprintln!("------------------------------------------------------------");
    eprintln!();

    match question {
        Some(q) => {
            // AC-15: Handle very long questions (wrap at 80 chars)
            let wrapped = wrap_text(q, 76);
            eprintln!("{}", wrapped);
        }
        None => {
            // AC-12: Warning when question key not found with available keys
            eprintln!("Warning: No question found in state.");
            eprintln!();
            eprintln!("Available keys: {:?}", state_keys(state));
            eprintln!("Expected one of: {:?}", expected_keys);
        }
    }

    // Display additional state keys if specified (AC-5, AC-8)
    if let Some(keys) = display_keys {
        eprintln!();
        for key in keys {
            if let Some(value) = state.get(*key) {
                match format {
                    "json" => eprintln!(
                        "{}: {}",
                        key,
                        serde_json::to_string(value).unwrap_or_default()
                    ),
                    "raw" => eprintln!("{}: {:?}", key, value),
                    _ => eprintln!("{}: {}", key, format_value_pretty(value)),
                }
            }
        }
    }

    eprintln!();
    eprintln!("------------------------------------------------------------");
    eprintln!("Your answer (Enter twice to send):");
}

/// Display final state respecting display filters (AC-9)
fn display_final_state(state: &JsonValue, display_keys: Option<&[&str]>, format: &str) {
    eprintln!();

    match display_keys {
        Some(keys) if !keys.is_empty() => {
            // Display only specified keys
            for key in keys {
                if let Some(value) = state.get(*key) {
                    match format {
                        "json" => println!(
                            "{}: {}",
                            key,
                            serde_json::to_string(value).unwrap_or_default()
                        ),
                        "raw" => println!("{}: {:?}", key, value),
                        _ => println!("{}: {}", key, format_value_pretty(value)),
                    }
                }
            }
        }
        _ => {
            // Display full state
            match format {
                "json" => println!("{}", serde_json::to_string(state).unwrap_or_default()),
                "raw" => println!("{:?}", state),
                _ => println!(
                    "{}",
                    serde_json::to_string_pretty(state).unwrap_or_default()
                ),
            }
        }
    }
}

/// Format a JSON value in a human-readable way (AC-6)
fn format_value_pretty(value: &JsonValue) -> String {
    match value {
        JsonValue::String(s) => {
            // AC-16: Handle binary/non-UTF8 by checking for replacement chars
            if s.contains('\u{FFFD}') {
                format!("<binary data, {} bytes>", s.len())
            } else {
                s.clone()
            }
        }
        JsonValue::Array(arr) => {
            // Format arrays nicely
            let items: Vec<String> = arr
                .iter()
                .map(|v| match v {
                    JsonValue::String(s) => s.clone(),
                    _ => v.to_string(),
                })
                .collect();
            items.join(", ")
        }
        JsonValue::Object(_) => serde_json::to_string_pretty(value).unwrap_or_default(),
        JsonValue::Bool(b) => b.to_string(),
        JsonValue::Number(n) => n.to_string(),
        JsonValue::Null => "null".to_string(),
    }
}

/// Wrap text at specified width (AC-15)
fn wrap_text(text: &str, width: usize) -> String {
    let mut result = String::new();
    for line in text.lines() {
        if line.len() <= width {
            result.push_str(line);
            result.push('\n');
        } else {
            let mut current_line = String::new();
            for word in line.split_whitespace() {
                if current_line.is_empty() {
                    current_line = word.to_string();
                } else if current_line.len() + 1 + word.len() <= width {
                    current_line.push(' ');
                    current_line.push_str(word);
                } else {
                    result.push_str(&current_line);
                    result.push('\n');
                    current_line = word.to_string();
                }
            }
            if !current_line.is_empty() {
                result.push_str(&current_line);
                result.push('\n');
            }
        }
    }
    result.trim_end().to_string()
}

/// Read multiline input from stdin, double-enter (empty line) to send.
/// Returns InteractiveCommand indicating user intent.
fn read_multiline_input() -> Result<InteractiveCommand> {
    let stdin = io::stdin();
    let mut lines = Vec::new();

    for line_result in stdin.lock().lines() {
        let line = line_result.context("Failed to read input line")?;

        // Check for quit/exit commands (only on first line if empty so far)
        if lines.is_empty() {
            let trimmed = line.trim().to_lowercase();
            if trimmed == "quit" || trimmed == "exit" {
                return Ok(InteractiveCommand::Quit);
            }
            if trimmed == "skip" {
                return Ok(InteractiveCommand::Skip);
            }
        }

        // Check for double-enter (empty line after content or another empty line)
        if line.is_empty() {
            if lines.is_empty() {
                // First line is empty - wait for another empty to send nothing
                lines.push(String::new());
            } else if lines.last().map(|l| l.is_empty()).unwrap_or(false) {
                // Double empty line - done
                lines.pop(); // Remove the first empty line
                break;
            } else {
                // Single empty line after content - mark it
                lines.push(String::new());
            }
        } else {
            lines.push(line);
        }
    }

    // Join lines and trim
    let response = lines.join("\n").trim().to_string();
    Ok(InteractiveCommand::Response(response))
}

/// Read multiline input with timeout (AC-11)
/// Uses a separate thread to read input and a channel with timeout.
fn read_multiline_input_with_timeout(timeout: std::time::Duration) -> Result<InteractiveCommand> {
    use std::sync::mpsc;
    use std::thread;

    let (tx, rx) = mpsc::channel();

    // Spawn thread to read input
    thread::spawn(move || {
        let result = read_multiline_input();
        let _ = tx.send(result);
    });

    // Wait for result with timeout
    match rx.recv_timeout(timeout) {
        Ok(result) => result,
        Err(mpsc::RecvTimeoutError::Timeout) => Ok(InteractiveCommand::Timeout),
        Err(mpsc::RecvTimeoutError::Disconnected) => {
            // Thread died unexpectedly
            anyhow::bail!("Input thread disconnected unexpectedly")
        }
    }
}

/// Extract question from state using configurable keys (tries each in order)
fn extract_question(state: &JsonValue, keys: &[&str]) -> Option<String> {
    for key in keys {
        if let Some(value) = state.get(*key) {
            if let Some(s) = value.as_str() {
                if !s.is_empty() {
                    return Some(s.to_string());
                }
            }
            // Also handle arrays of strings (multiple questions)
            if let Some(arr) = value.as_array() {
                let texts: Vec<&str> = arr
                    .iter()
                    .filter_map(|v| v.as_str())
                    .filter(|s| !s.is_empty())
                    .collect();
                if !texts.is_empty() {
                    return Some(texts.join("\n"));
                }
            }
        }
    }
    None
}

/// Check if workflow is complete using configurable keys
fn is_complete(state: &JsonValue, keys: &[&str]) -> bool {
    for key in keys {
        if let Some(value) = state.get(*key) {
            if value.as_bool().unwrap_or(false) {
                return true;
            }
        }
    }
    false
}

/// Inject response into state under the specified key
fn inject_response(state: &mut JsonValue, key: &str, response: &str) {
    if let JsonValue::Object(ref mut map) = state {
        map.insert(key.to_string(), JsonValue::String(response.to_string()));
    }
}

/// Get list of all keys in state (for debugging when question not found)
fn state_keys(state: &JsonValue) -> Vec<String> {
    match state {
        JsonValue::Object(map) => map.keys().cloned().collect(),
        _ => vec![],
    }
}

/// Find the most recent checkpoint file in directory
fn find_latest_checkpoint(dir: &PathBuf) -> Result<Option<PathBuf>> {
    if !dir.exists() {
        return Ok(None);
    }

    let mut latest: Option<(PathBuf, std::time::SystemTime)> = None;

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        // Only consider .bin files (checkpoint files)
        if path.extension().map(|e| e == "bin").unwrap_or(false) {
            if let Ok(metadata) = entry.metadata() {
                if let Ok(modified) = metadata.modified() {
                    match &latest {
                        None => latest = Some((path, modified)),
                        Some((_, prev_time)) if modified > *prev_time => {
                            latest = Some((path, modified));
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    Ok(latest.map(|(path, _)| path))
}
