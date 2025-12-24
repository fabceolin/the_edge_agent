# Knowledge Graph Reasoning

This example demonstrates **Prolog-based inference over knowledge graphs**. Knowledge graphs are powerful for representing structured relationships, and Prolog's logic programming paradigm makes it ideal for querying and reasoning over them.

## The Knowledge Graph

The example encodes a family tree as Prolog facts:

```
           alice
          /     \
        bob    carol
       /   \      \
    david  eve   frank
      |
    grace
```

Base facts (explicit relationships):
```prolog
parent(alice, bob).
parent(alice, carol).
parent(bob, david).
parent(bob, eve).
parent(carol, frank).
parent(david, grace).
```

## Inference Rules

Prolog rules derive new relationships from base facts:

### Grandparent

```prolog
grandparent(GP, GC) :-
    parent(GP, P),    % GP is parent of P
    parent(P, GC).    % P is parent of GC
```

### Sibling

```prolog
sibling(X, Y) :-
    parent(P, X),     % P is parent of X
    parent(P, Y),     % P is also parent of Y
    X \= Y.           % but X and Y are different
```

### Ancestor (Transitive)

```prolog
ancestor(A, D) :- parent(A, D).              % Base case
ancestor(A, D) :- parent(A, X), ancestor(X, D).  % Recursive case
```

## The Neurosymbolic Pattern

```
┌─────────────────────┐     ┌─────────────────────┐     ┌─────────────────────┐
│   Knowledge Graph   │     │   Inference Rules   │     │   Derived Facts     │
│   (Prolog facts)    │ ──► │   (Prolog rules)    │ ──► │   (query results)   │
└─────────────────────┘     └─────────────────────┘     └─────────────────────┘
         │                           │                           │
         ▼                           ▼                           ▼
   parent(alice, bob)         grandparent(X,Z):-         grandchildren of
   parent(bob, david)         parent(X,Y), parent(Y,Z)   alice = [david,eve,frank]
```

## Running the Example

**Python:**
```bash
cd python
python -m the_edge_agent run ../examples/prolog/neurosymbolic/knowledge-graph.yaml
```

**With custom query:**
```python
from the_edge_agent import YAMLEngine

engine = YAMLEngine()
graph = engine.load_from_file("examples/prolog/neurosymbolic/knowledge-graph.yaml")
compiled = graph.compile()

for event in compiled.invoke({
    "query_person": "grace",
    "relationship_type": "ancestors"
}):
    print(event)
# Results: [david, bob, alice]
```

## Query Types

| Query Type | Description | Example Result |
|------------|-------------|----------------|
| `children` | Direct children | alice → [bob, carol] |
| `grandchildren` | Children of children | alice → [david, eve, frank] |
| `siblings` | Same parent | david → [eve] |
| `ancestors` | All parents recursively | grace → [david, bob, alice] |
| `descendants` | All children recursively | alice → [bob, carol, david, eve, frank, grace] |

## Why Prolog for Knowledge Graphs?

### 1. Natural Representation

Knowledge graphs naturally map to Prolog facts:
```prolog
% Entity relationships
parent(alice, bob).
works_at(bob, acme_corp).
located_in(acme_corp, new_york).
```

### 2. Built-in Unification

Prolog's pattern matching finds relationships automatically:
```prolog
% Find all people who work in New York
?- works_at(Person, Company), located_in(Company, new_york).
```

### 3. Transitive Closure

Recursive rules handle unlimited depth:
```prolog
% Multi-hop relationship
connected(X, Y) :- link(X, Y).
connected(X, Y) :- link(X, Z), connected(Z, Y).
```

### 4. Negation as Failure

Query for absence of relationships:
```prolog
% Find people without children
childless(X) :- person(X), \+ parent(X, _).
```

## Extending the Example

### Add More Relationship Types

```prolog
% Uncle/Aunt
uncle_aunt(UA, NN) :-
    parent(P, NN),
    sibling(P, UA).

% Cousin
cousin(X, Y) :-
    parent(PX, X),
    parent(PY, Y),
    sibling(PX, PY).
```

### Add Attributes

```prolog
% Facts with attributes
person(alice, female, 1950).
person(bob, male, 1975).

% Query with constraints
born_after(Person, Year) :-
    person(Person, _, BirthYear),
    BirthYear > Year.
```

### Multiple Relationship Types

```prolog
% Different edge types
parent(alice, bob).
works_for(bob, alice).
married(bob, carol).

% Path finding across types
related(X, Y) :- parent(X, Y).
related(X, Y) :- works_for(X, Y).
related(X, Y) :- married(X, Y).
```

## Use Cases

### 1. Organizational Hierarchy

```prolog
reports_to(alice, bob).
reports_to(bob, carol).
reports_to(carol, ceo).

% Find all managers above someone
chain_of_command(Employee, Manager) :-
    reports_to(Employee, Manager).
chain_of_command(Employee, Manager) :-
    reports_to(Employee, Middle),
    chain_of_command(Middle, Manager).
```

### 2. Product Taxonomy

```prolog
category(laptop, electronics).
category(electronics, products).
category(apple_macbook, laptop).

% Find all products in a category (including subcategories)
in_category(Item, Cat) :-
    category(Item, Cat).
in_category(Item, Cat) :-
    category(Item, SubCat),
    in_category(SubCat, Cat).
```

### 3. Access Control

```prolog
role(alice, admin).
role(bob, editor).
permission(admin, read).
permission(admin, write).
permission(admin, delete).
permission(editor, read).
permission(editor, write).

% Check if user has permission
can_do(User, Action) :-
    role(User, Role),
    permission(Role, Action).
```

## Performance Considerations

| Graph Size | Recommendation |
|------------|----------------|
| < 1000 nodes | Direct Prolog queries |
| 1000-10000 | Use indexing, limit recursion depth |
| > 10000 | Consider tabling (memoization) |

### Using Tabling

```prolog
:- table ancestor/2.  % Memoize ancestor computations

ancestor(A, D) :- parent(A, D).
ancestor(A, D) :- parent(A, X), ancestor(X, D).
```

## Related Examples

- [LLM + Prolog Family Reasoning](llm-prolog-family-reasoning.md) - LLM extraction + Prolog reasoning
- [Classifier Rules](classifier-rules.md) - Decision making
- [Multi-Step Reasoning](reasoning-chain.md) - Chained inference
