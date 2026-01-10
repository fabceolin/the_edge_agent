# Page snapshot

```yaml
- generic [active] [ref=e1]:
  - heading "TEA WASM LLM E2E Test Page" [level=1] [ref=e2]
  - generic [ref=e3]: "WASM loaded! Version: 0.1.0, SharedArrayBuffer: true"
  - generic [ref=e4]: LLM handler initialized (mock mode)
  - heading "Output" [level=2] [ref=e6]
  - generic [ref=e7]: "Waiting for initialization...[01:42:52.709] Initializing WASM module... [01:42:52.715] WASM module loaded successfully [01:42:52.715] Version: 0.1.0 [01:42:52.716] SharedArrayBuffer: true [01:42:52.716] Ready for testing [01:42:52.727] LLM handler registered [01:42:52.730] Executing YAML workflow... [01:42:52.738] Mock LLM called with prompt: \"Say hello...\" [01:42:52.739] Result: { \"gen\": { \"content\": \"Mock response to: Say hello\", \"model\": \"mock-model\", \"usage\": { \"completion_tokens\": 5, \"prompt_tokens\": 10, \"total_tokens\": 15 } }, \"input\": \"test\" }"
```