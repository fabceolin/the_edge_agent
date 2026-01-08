# Stream Channels

> **Version**: 0.9.0+
> **Platforms**: Linux, macOS (Windows not supported)
> **Requires**: `parallel_strategy: process`

Stream channels enable Unix-style pipe streaming between nodes, allowing
real-time data flow alongside traditional state passing.

## Overview

The Edge Agent supports two data passing models:

| Model | Transfer | Serialization | Checkpoint | Use Case |
|-------|----------|---------------|------------|----------|
| **State** | Discrete batches | JSON/Pickle | Yes | Structured data, metadata |
| **Stream** | Continuous flow | Raw bytes | No | Large files, logs, events |

### When to Use Streams

Use streams when:
- Processing large files (GB+) that don't fit in memory
- Building real-time pipelines (logs, events, metrics)
- Connecting to external tools (grep, awk, jq)
- Avoiding serialization overhead for bulk data

Use state when:
- Passing structured configuration or metadata
- Need checkpoint/resume capability
- Data fits comfortably in memory
- Interoperability with non-Unix environments

## Enabling Streams

Streams are opt-in and require process-based parallel execution:

```yaml
settings:
  parallel:
    strategy: process    # Required - threads cannot use pipes
    streams:
      enabled: true      # Enable stream support
      buffer_size: 65536 # Pipe buffer in bytes (default: 64KB)
      timeout: 300       # Stream timeout in seconds (optional)
```

### Settings Reference

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `enabled` | bool | `false` | Enable stream channel support |
| `buffer_size` | int | 65536 | Pipe buffer size in bytes |
| `timeout` | int | null | Stream timeout in seconds |

## Node Configuration

### Producer Node (stdout)

A producer writes data to stdout, which is captured by a named stream:

```yaml
nodes:
  - name: producer
    run: |
      import sys
      for item in items:
        print(item, file=sys.stdout, flush=True)
      return {"count": len(items)}
    streams:
      stdout: my_stream  # Named stream channel
```

### Consumer Node (stdin)

A consumer reads from stdin, connected to a named stream:

```yaml
nodes:
  - name: consumer
    run: |
      import sys
      for line in sys.stdin:
        process(line)
      return {"status": "done"}
    streams:
      stdin: my_stream  # Must match producer's channel name
```

### Transform Node (stdin + stdout)

A node can both consume and produce streams:

```yaml
nodes:
  - name: transformer
    run: |
      import sys
      for line in sys.stdin:
        transformed = line.upper()
        print(transformed, file=sys.stdout, flush=True)
      return {"status": "transformed"}
    streams:
      stdin: input_stream
      stdout: output_stream
```

### Error Stream (stderr)

Capture error output to a separate stream:

```yaml
nodes:
  - name: processor
    run: |
      import sys
      for line in sys.stdin:
        try:
          process(line)
        except Exception as e:
          print(f"Error: {e}", file=sys.stderr)
      return {}
    streams:
      stdin: data_stream
      stderr: error_stream
```

## Broadcasting

To send the same stream to multiple consumers, use `stream_mode: broadcast`:

```yaml
edges:
  - from: producer
    to: [consumer_a, consumer_b, consumer_c]
    parallel: true
    parallel_strategy: process
    stream_mode: broadcast  # All consumers get same data
    fan_in: merger
```

### Stream Modes

| Mode | Description | Targets |
|------|-------------|---------|
| `direct` (default) | Single consumer | 1 |
| `broadcast` | Duplicate to all consumers | N |

### Broadcast Implementation

Broadcasting uses Unix named pipes (FIFOs) and the `tee` command:

```
Producer stdout -> tee -> FIFO_1 -> Consumer A stdin
                     |-> FIFO_2 -> Consumer B stdin
                     |-> FIFO_3 -> Consumer C stdin
```

When `tee` is not available, a Python fallback handles duplication.

## Complete Example

```yaml
name: log-processor
description: Process logs with parallel analysis

settings:
  parallel:
    strategy: process
    streams:
      enabled: true
      buffer_size: 131072  # 128KB for log throughput

state_schema:
  input_file: str
  total_lines: int
  errors: int
  warnings: int

nodes:
  - name: reader
    run: |
      import sys
      with open(state["input_file"]) as f:
        for line in f:
          print(line, end="", file=sys.stdout, flush=True)
      return {}
    streams:
      stdout: log_stream

  - name: error_counter
    run: |
      import sys
      count = 0
      for line in sys.stdin:
        if "ERROR" in line:
          count += 1
      return {"errors": count}
    streams:
      stdin: log_stream

  - name: warning_counter
    run: |
      import sys
      count = 0
      for line in sys.stdin:
        if "WARNING" in line:
          count += 1
      return {"warnings": count}
    streams:
      stdin: log_stream

  - name: aggregator
    run: |
      return {
        "errors": parallel_results[0].get("errors", 0),
        "warnings": parallel_results[1].get("warnings", 0)
      }

edges:
  - from: __start__
    to: reader
  - from: reader
    to: [error_counter, warning_counter]
    parallel: true
    parallel_strategy: process
    stream_mode: broadcast
    fan_in: aggregator
  - from: aggregator
    to: __end__
```

## Limitations

### 1. No Checkpointing

Stream nodes cannot use `interrupt_before` or `interrupt_after`. Stream
position cannot be restored after a checkpoint.

```yaml
# INVALID - will raise error
nodes:
  - name: streamer
    interrupt_before: true  # Error!
    streams:
      stdout: data
```

### 2. Process Strategy Only

Streams require `parallel_strategy: process`. Thread-based execution
cannot use pipes between threads.

```yaml
# INVALID
settings:
  parallel:
    strategy: thread  # Error if streams enabled
    streams:
      enabled: true
```

### 3. Unix Only

Windows does not support named pipes (mkfifo). Streams are only
available on Linux and macOS.

### 4. Single Producer

Each stream channel can only have one producer. Multiple nodes
cannot write to the same stream.

```yaml
# INVALID - multiple producers
nodes:
  - name: producer_a
    streams:
      stdout: shared_stream  # Error!
  - name: producer_b
    streams:
      stdout: shared_stream  # Same stream!
```

### 5. No Nested Subgraphs

Stream channels do not cross subgraph boundaries. Each subgraph
has its own stream namespace.

## Troubleshooting

| Error | Cause | Fix |
|-------|-------|-----|
| "Streams require parallel_strategy: process" | Using thread strategy | Set `settings.parallel.strategy: process` |
| "Stream 'X' has no producer" | Consumer references undefined stream | Add producer with `streams.stdout: X` |
| "Stream 'X' has multiple producers" | Multiple nodes output to same stream | Use unique stream names per producer |
| "Stream nodes cannot be checkpointed" | `interrupt_*` on stream node | Move checkpoint to non-streaming node |
| "Platform not supported" | Running on Windows | Use Linux or macOS |
| "direct requires single target" | `stream_mode: direct` with multiple targets | Use `stream_mode: broadcast` |

### Debugging Tips

1. **Check buffer size**: Small buffers can cause frequent context switches.
   Try `buffer_size: 131072` (128KB) for high-throughput streams.

2. **Flush output**: Always use `flush=True` when writing to stdout:
   ```python
   print(data, file=sys.stdout, flush=True)
   ```

3. **Handle EOF**: Consumers should handle stdin EOF gracefully:
   ```python
   for line in sys.stdin:  # Stops at EOF
       process(line)
   ```

4. **Check tee availability**: Run `which tee` to verify tee is installed.
   The Python fallback is slower but works when tee is missing.

## Performance Considerations

### Buffer Sizing

| Use Case | Recommended `buffer_size` |
|----------|---------------------------|
| Small records (<1KB) | 65536 (64KB) |
| Medium records (1-10KB) | 131072 (128KB) |
| Large records (>10KB) | 262144 (256KB) |
| Maximum throughput | 1048576 (1MB) |

### Throughput Expectations

On modern hardware with default settings:
- **Small data (<1KB records)**: 100-500 MB/s
- **Bulk data (large chunks)**: 500+ MB/s
- **Broadcast (3 consumers)**: ~80% of direct throughput

### Memory Usage

Streams use minimal memory compared to state passing:
- **Producer**: buffer_size bytes per stream
- **Consumer**: buffer_size bytes per stream
- **Broadcast**: buffer_size * N consumers + FIFO overhead

## See Also

- [YAML Reference](../YAML_REFERENCE.md) - Complete YAML syntax
- [Parallel Execution](./parallel.md) - Process and thread strategies
- [Examples](../../../examples/yaml/) - Working stream examples
