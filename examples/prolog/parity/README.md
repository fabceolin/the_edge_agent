# Prolog Parity Tests

Cross-runtime parity tests for Python and Rust TEA implementations.

## Purpose

Both Python and Rust TEA use **SWI-Prolog** as their underlying logic engine:
- Python: `janus-swi` bindings
- Rust: `swipl-rs` crate

These parity tests verify that the same Prolog YAML agents produce identical results in both runtimes.

## Directory Structure

```
examples/prolog/
├── parity/                    # Test fixture YAML files
│   ├── README.md              # This file
│   ├── basic-state-access.yaml
│   ├── clpfd-deterministic.yaml
│   ├── clpfd-multiple-solutions.yaml
│   ├── error-syntax.yaml
│   ├── error-timeout.yaml
│   ├── error-sandbox.yaml
│   ├── parallel-isolation.yaml
│   ├── unicode-strings.yaml
│   ├── nested-objects.yaml
│   └── empty-collections.yaml
│
└── parity-expected/           # Expected JSON outputs
    ├── basic-state-access.json
    ├── clpfd-deterministic.json
    └── ...
```

## Running Parity Tests

### Python

```bash
cd python
pytest tests/test_prolog_parity.py -v
```

### Rust

```bash
cd rust
cargo test --features prolog parity
```

### Cross-Runtime Harness

```bash
./scripts/parity-test.sh examples/prolog/parity/basic-state-access.yaml
```

## Test Categories

| Category | Fixtures | Priority |
|----------|----------|----------|
| State Access | basic-state-access.yaml | P0 |
| CLP(FD) | clpfd-deterministic.yaml, clpfd-multiple-solutions.yaml | P0 |
| Error Handling | error-syntax.yaml, error-timeout.yaml, error-sandbox.yaml | P0 |
| Parallel Execution | parallel-isolation.yaml | P0 |
| Edge Cases | unicode-strings.yaml, nested-objects.yaml, empty-collections.yaml | P1 |

## Known Parity Differences

| Area | Python Behavior | Rust Behavior | Resolution |
|------|-----------------|---------------|------------|
| `return/2` predicate | Full support - values returned to state | **Not supported** - swipl-rs limitation | Use query success/failure for validation, CLP(FD) labeling for values |
| Empty list `[]` | Preserved as `[]` | Converted to `null` | Document as known difference |

## JSON Normalization

When comparing outputs:
1. Sort object keys alphabetically
2. Normalize float precision to 14 significant digits
3. Handle `null` vs `[]` consistently

## Design Principles

1. **Determinism**: All tests use `first_only=True` mode
2. **CLP(FD) uniqueness**: Constraints have unique first solutions
3. **Timeout tolerance**: Allow ±10% variance for timing tests
4. **Sandbox parity**: Both runtimes use SWI-Prolog's `library(sandbox)`
