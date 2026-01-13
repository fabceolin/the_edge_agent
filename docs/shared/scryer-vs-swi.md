# Scryer Prolog vs SWI-Prolog: Syntax and Feature Differences

TEA-RELEASE-005.1: Scryer Prolog Spike

## Overview

TEA supports two Prolog backends:

| Backend | Feature Flag | Dependencies | Status |
|---------|--------------|--------------|--------|
| SWI-Prolog | `--features prolog` | `libswipl.so` (50MB+) | Production |
| Scryer Prolog | `--features scryer` | Pure Rust (5MB) | Experimental |

## Key Differences

### 1. Module Loading

**SWI-Prolog**: Supports loading modules from strings via `consult/1` and `load_files/2`.

```prolog
% SWI: Works
:- dynamic(my_fact/1).
my_fact(hello).
```

**Scryer**: Module loading from strings is limited. The `load_module_string` method
doesn't reliably make predicates available for subsequent queries.

```prolog
% Scryer: Limited support
% Dynamic predicates and rules must be defined differently
```

### 2. State Access Pattern

**SWI-Prolog**: Uses `state/2` predicate with dynamic assertion.

```prolog
% SWI: TEA injects state as dynamic facts
state(value, 42).
state(name, 'Alice').

% User code
state(value, V), Result is V * 2.
```

**Scryer (Spike Implementation)**: Uses query-time variable substitution.

```prolog
% Scryer: State values are substituted inline
% state(value, V) becomes V = 42
V = 42, Result is V * 2.
```

### 3. Return Values

**SWI-Prolog**: Uses `return/2` predicate with `assertz`.

```prolog
% SWI: Returns via assertion
return(result, 42).
% TEA collects return_value/2 facts after execution
```

**Scryer (Spike Implementation)**: Uses binding extraction.

```prolog
% Scryer: return(key, Value) becomes ReturnValue = Value
% TEA extracts ReturnValue from query bindings
```

### 4. Inline Rule Definitions

**SWI-Prolog**: Supports inline rule definitions in node code.

```prolog
% SWI: Works
double(X, Y) :- Y is X * 2.
state(value, V),
double(V, R),
return(result, R).
```

**Scryer**: NOT supported in spike. Requires module loading.

```prolog
% Scryer: This will fail
% Workaround: Use inline arithmetic only
state(value, V),
R is V * 2,
return(result, R).
```

### 5. Conditional Returns

**SWI-Prolog**: Supports multiple `return/2` calls in conditionals.

```prolog
% SWI: Works
state(value, V),
(V >= 100 -> return(category, large)
; V >= 50 -> return(category, medium)
; return(category, small)).
```

**Scryer**: NOT fully supported in spike. Single return only.

```prolog
% Scryer: Use single return with computed value
state(value, V),
(V >= 100 -> Cat = large
; V >= 50 -> Cat = medium
; Cat = small),
return(category, Cat).
```

### 6. Library Availability

**SWI-Prolog**: Rich standard library.

- `library(lists)` - List predicates (auto-loaded)
- `library(clpfd)` - Constraint logic programming
- `library(aggregate)` - Aggregation predicates

**Scryer**: Growing library support.

- `library(lists)` - Available
- `library(clpz)` - CLP(Z) instead of CLP(FD)
- Some predicates may have different names

### 7. Dict Syntax

**SWI-Prolog**: Native dict syntax `_{key: value}`.

```prolog
% SWI: Dict syntax
D = _{name: 'Alice', age: 30}.
```

**Scryer**: Standard Prolog structures.

```prolog
% Scryer: Use compound terms
D = person('Alice', 30).
```

## Feature Compatibility Matrix

| Feature | SWI-Prolog | Scryer |
|---------|------------|--------|
| Basic arithmetic | ✅ | ✅ |
| List operations | ✅ | ✅ |
| state/2 access | ✅ | ✅ (via substitution) |
| return/2 single | ✅ | ✅ |
| return/2 multiple | ✅ | ❌ |
| Inline rules | ✅ | ❌ |
| Conditionals | ✅ | Partial |
| CLP(FD/Z) | ✅ | ✅ (different API) |
| Sandbox | ✅ | ❌ |
| Threading | ✅ | ❌ |

## When to Use Each Backend

### Use SWI-Prolog when:
- You need full Prolog expressiveness
- Inline rule definitions are required
- Complex conditionals with multiple returns
- Production deployment with trusted code
- CLP(FD) constraint solving

### Use Scryer Prolog when:
- Minimizing dependencies is critical
- Building for edge devices or WASM
- Simple arithmetic and logic operations
- Small binary size is important
- No external C library installation desired

## Migration Guide

### From SWI to Scryer

1. **Replace inline rules with arithmetic**:
   ```prolog
   % SWI
   add_ten(X, Y) :- Y is X + 10.
   add_ten(Input, Result).

   % Scryer
   Result is Input + 10.
   ```

2. **Flatten conditional returns**:
   ```prolog
   % SWI
   (Cond -> return(key, val1) ; return(key, val2)).

   % Scryer
   (Cond -> R = val1 ; R = val2), return(key, R).
   ```

3. **Check library availability**:
   - `clpfd` → `clpz`
   - Some predicates may have different names

## Performance Benchmarks

Run with `cargo bench --features scryer "Scryer"`:

| Benchmark | Time | Notes |
|-----------|------|-------|
| Runtime creation | ~160ms | Machine instantiation overhead |
| Simple arithmetic | ~152ms | `X is 21 * 2` |
| List length | ~151ms | `length([1,2,3,4,5], N)` |
| Node execution | ~150ms | Full `execute_node_code` |

### Analysis

The primary bottleneck is **Machine creation** (~160ms). Each query currently
creates a fresh Machine instance for isolation. For production use:

1. **Machine pooling**: Reuse Machine instances across queries
2. **Lazy initialization**: Defer Machine creation until first query
3. **Batch queries**: Combine multiple queries into one Machine session

### Comparison with SWI-Prolog

| Operation | Scryer | SWI-Prolog |
|-----------|--------|------------|
| Engine creation | ~160ms | ~50ms |
| Simple query | ~5ms* | ~1ms |
| Module loading | Limited | Full support |

*After Machine is created; the 150ms includes creation.

## Spike Findings Summary

The TEA-RELEASE-005.1 spike discovered:

1. **Basic Scryer integration works**: Simple queries execute correctly
2. **Module loading is problematic**: `load_module_string` doesn't work reliably
3. **Workaround exists**: Query-time substitution enables basic functionality
4. **Production gap**: Full parity with SWI-Prolog requires more work
5. **Performance bottleneck**: Machine creation is expensive (~160ms)

### Recommended Next Steps

1. File issue with Scryer project re: string-based module loading
2. Implement temp-file workaround for module loading
3. Add runtime backend switching (not compile-time only)
4. Implement Machine pooling for better performance
5. Expand Scryer test coverage for edge cases
