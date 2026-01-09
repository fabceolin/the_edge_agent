# Local LLM Examples

This directory contains examples for using local LLM inference with The Edge Agent.

## Examples

| File | Description | AC |
|------|-------------|----|
| `local-chat.yaml` | Simple chat with local model | AC-6 |
| `local-embed.yaml` | Generate text embeddings | AC-7 |
| `local-rag.yaml` | RAG workflow with local embeddings | AC-8 |

## Requirements

### AppImage (Recommended)

The easiest way to run these examples is with an LLM-bundled AppImage:

```bash
# Download AppImage
curl -L https://github.com/fabceolin/the_edge_agent/releases/download/v0.9.5/tea-rust-llm-gemma-0.9.5-x86_64.AppImage -o tea-llm.AppImage
chmod +x tea-llm.AppImage

# Run examples
./tea-llm.AppImage run examples/llm/local-chat.yaml --input '{"question": "Hello!"}'
./tea-llm.AppImage run examples/llm/local-embed.yaml --input '{"text": "Sample text"}'
./tea-llm.AppImage run examples/llm/local-rag.yaml --input '{"query": "What is TEA?"}'
```

### From Source

To run from source with local LLM support:

**Rust:**
```bash
cd rust
cargo build --release --features llm-local
./target/release/tea run ../examples/llm/local-chat.yaml
```

**Python:**
```bash
cd python
pip install -e .[llm-local]
python -m the_edge_agent.cli run ../examples/llm/local-chat.yaml
```

## Model Path Configuration

Models are auto-detected in AppImages. For manual configuration:

```bash
# Environment variable
export TEA_MODEL_PATH=/path/to/model.gguf
tea run examples/llm/local-chat.yaml

# Or in YAML settings
# settings:
#   llm:
#     model_path: /path/to/model.gguf
```

See [Installation Guide](../../docs/installation.md#model-path-configuration) for complete path resolution order.

## Example Outputs

### local-chat.yaml

```json
{
  "question": "What is 2+2?",
  "answer": "4",
  "backend": "local",
  "model": "gemma-3n-E4B"
}
```

### local-embed.yaml

```json
{
  "text": "Hello world",
  "embedding": [0.123, -0.456, ...],
  "dimensions": 768
}
```

### local-rag.yaml

```json
{
  "query": "What is TEA?",
  "answer": "TEA (The Edge Agent) is a lightweight state graph library...",
  "sources": ["docs/overview.md", "README.md"]
}
```

## Related Documentation

- [Installation Guide](../../docs/installation.md) - AppImage downloads
- [WASM Deployment](../../docs/wasm/llm-deployment.md) - Browser deployment
- [Actions Reference](../../docs/python/actions-reference.md) - LLM action parameters
- [YAML Reference](../../docs/shared/YAML_REFERENCE.md) - Complete syntax
