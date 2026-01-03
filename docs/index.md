# The Edge Agent

> **Small LLMs hallucinate. TEA fixes that with Prolog.**

One binary. No cloud. Neurosymbolic AI that actually reasons.

TEA combines LLMs with symbolic reasoning (Prolog) to create AI agents that can prove their conclusions, not just generate plausible-sounding text. Perfect for small/local models (Llama, Mistral, Phi, Ollama) where symbolic reasoning compensates for limited model capacity.

## 30-Second Example

```yaml
# LLM extracts facts, Prolog derives relationships via temporal reasoning
name: hero-family-reasoning

nodes:
  - name: extract
    uses: llm.call              # LLM extracts: mother(alice, bob). affair(alice, dave, 1980, 1990).
    with:
      model: "gemma3n:e4b"
      messages:
        - role: user
          content: "Extract family relationships as Prolog facts from: {{ state.text }}"
    output: llm_response

  - name: reason
    language: prolog            # Prolog derives: child_of_affair, half_sibling
    run: |
      child_of_affair(Child, Partner) :-
          mother(Mother, Child), birth_year(Child, Year),
          affair(Mother, Partner, Start, End), Year >= Start, Year =< End.

      half_sibling(X, Y) :-
          mother(M, X), mother(M, Y), X \= Y,
          \+ (father(F, X), father(F, Y)).

      state(facts, Facts), tea_load_code(Facts),
      findall(H, half_sibling(bob, H), Results),
      return(half_siblings, Results).
```

**Run it:**
```bash
tea run examples/prolog/neurosymbolic/hero-family-reasoning.yaml \
  --input '{"text": "Alice had two children: Bob and Carol. Alice had an affair with Dave from 1980 to 1990. Bob was born in 1985. Carol was born in 1975.", "person": "bob"}'
# Output: {"answer": "bob's half-siblings: Carol"}
```

**What happens:** LLM extracts facts → Prolog *proves* Bob is Carol's half-sibling (born during affair = different father).

## Why TEA?

| Challenge | TEA Solution |
|-----------|--------------|
| **Small LLMs make reasoning errors** | Prolog handles logic, math, and constraints while LLM handles language |
| **LLMs hallucinate facts** | Knowledge graphs with verifiable inference chains |
| **Complex agent frameworks** | Simple YAML syntax, learn in minutes |
| **Need for external services** | Single binary, zero dependencies, runs offline |
| **Cloud vendor lock-in** | Portable agents run on any platform |
| **Building everything from scratch** | 20+ built-in actions for LLM, RAG, memory, and storage |
| **No visibility into agent behavior** | Built-in observability with distributed tracing |

## Quick Install

```bash
curl -L https://github.com/fabceolin/the_edge_agent/releases/latest/download/tea-rust-linux-x86_64 -o tea && chmod +x tea
```

For Prolog support, use the [AppImage](installation.md#appimage-installation) (self-contained) or install SWI-Prolog.

See [Installation Guide](installation.md) for all platforms and options.

## vs Alternatives

| Feature | TEA | LangGraph | AutoGen |
|---------|-----|-----------|---------|
| **Symbolic reasoning (Prolog)** | Yes | No | No |
| **Single binary** | Yes | No (Python) | No (Python) |
| **Offline operation** | Yes | Limited | No |
| **YAML-first** | Yes | Code-first | Code-first |
| **Neurosymbolic** | Yes | No | No |
| **Local LLM optimized** | Yes | Partial | No |
| **Edge/embedded ready** | Yes | No | No |

## Implementations

| Implementation | Status | Best For |
|----------------|--------|----------|
| **[Python](python/getting-started.md)** | Production-ready | Online edge, full features, 20+ actions |
| **[Rust](rust/getting-started.md)** | Active development | Embedded, offline, resource-constrained |

Both share the same YAML syntax. Write once, run anywhere.

## Featured Article

!!! info "Why LLMs Can't Count"
    Learn how neurosymbolic AI solves the fundamental limitations of LLMs with mathematical precision.

    **[Read the article →](articles/medium-neurosymbolic-ai.md)**

## License

MIT License. See [LICENSE](https://github.com/fabceolin/the_edge_agent/blob/main/LICENSE) for details.
