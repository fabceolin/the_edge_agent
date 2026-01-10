# Page snapshot

```yaml
- generic [active] [ref=e1]:
  - heading "TEA WASM LLM E2E Test Page" [level=1] [ref=e2]
  - generic [ref=e3]: "WASM loaded! Version: 0.1.0, SharedArrayBuffer: true"
  - generic [ref=e4]: LLM handler initialized (mock mode)
  - heading "Output" [level=2] [ref=e6]
  - generic [ref=e7]: "Waiting for initialization...[01:42:53.401] Initializing WASM module... [01:42:53.407] WASM module loaded successfully [01:42:53.407] Version: 0.1.0 [01:42:53.407] SharedArrayBuffer: true [01:42:53.407] Ready for testing [01:42:53.430] LLM handler registered [01:42:53.434] Executing YAML workflow... [01:42:53.440] Mock LLM called with prompt: \"Process this: hello world...\" [01:42:53.441] Result: { \"process\": { \"content\": \"Mock response to: Process this: hello world\", \"model\": \"mock-model\", \"usage\": { \"completion_tokens\": 5, \"prompt_tokens\": 10, \"total_tokens\": 15 } }, \"user_input\": \"hello world\" }"
```