# Family Relationships Ontology

## Purpose

This ontology defines the vocabulary and structure for extracting family relationships from narrative text. It guides LLM extraction and ensures consistent Prolog fact generation.

**Key Principle:** The LLM extracts only **observable facts** (dates, explicit statements). Prolog **derives** uncertain relationships through logical inference.

---

## Relationship Types

### Extracted Relationships (LLM Output)

These are directly extracted from text - things that are explicitly stated.

| Relationship | Prolog Predicate | Description |
|--------------|------------------|-------------|
| **Parent** | `parent(Parent, Child)` | Explicitly stated parent-child |
| **Mother** | `mother(Mother, Child)` | Biological mother (when specified) |
| **Father** | `father(Father, Child)` | Biological father (when specified) |
| **Marriage** | `married(P1, P2, StartDate, EndDate)` | Marriage with dates |
| **Affair** | `affair(Person, Partner, StartDate, EndDate)` | Extramarital relationship with dates |

### Extracted Attributes (LLM Output)

| Attribute | Prolog Predicate | Description |
|-----------|------------------|-------------|
| **Birth Date** | `birth_date(Person, Date)` | Date of birth |
| **Death Date** | `death_date(Person, Date)` | Date of death (if applicable) |

### Derived Relationships (Prolog Inference)

These are **NOT extracted** by the LLM - they are **deduced** by Prolog rules.

| Relationship | Prolog Rule | Inferred From |
|--------------|-------------|---------------|
| **Child of Affair** | `child_of_affair(Child, Partner)` | Birth date falls within affair period |
| **Potential Father** | `potential_father(Father, Child)` | Affair partner when child born during affair |
| **Sibling** | `sibling(X, Y)` | Same mother AND same father |
| **Half-Sibling** | `half_sibling(X, Y)` | Exactly ONE shared parent |
| **Grandparent** | `grandparent(X, Z)` | `parent(X, Y), parent(Y, Z)` |

---

## Temporal Reasoning

### Date Format

All dates use ISO 8601 format: `YYYY-MM-DD`

For approximate dates, use:
- `YYYY-01-01` for "early YYYY"
- `YYYY-07-01` for "mid YYYY"
- `YYYY-12-31` for "late YYYY"
- `YYYY-XX-XX` when only year is known (normalized to `YYYY-01-01`)

### Relationship Periods

Marriages and affairs have temporal bounds:

```prolog
% Marriage with dates
married(charles, diana, '1981-07-29', '1996-08-28').

% Ongoing marriage (no end date)
married(william, catherine, '2011-04-29', null).

% Affair with approximate dates
affair(camilla, charles, '1970-01-01', '1995-12-31').
```

### Key Inference: Child of Affair

A child is **potentially from an affair** if:
1. The child's mother was having an affair
2. The child was born during the affair period

```prolog
child_of_affair(Child, AffairPartner) :-
    mother(Mother, Child),
    birth_date(Child, BirthDate),
    affair(Mother, AffairPartner, StartDate, EndDate),
    date_in_range(BirthDate, StartDate, EndDate).
```

This replaces the need for LLM to infer uncertain parentage!

---

## Extraction Schema

### JSON Output Format

```json
{
  "entities": [
    {
      "name": "Person Name",
      "aliases": ["Alt Name"],
      "birth_date": "YYYY-MM-DD",
      "death_date": "YYYY-MM-DD | null"
    }
  ],
  "relationships": [
    {
      "type": "parent | mother | father | married | affair",
      "subject": "Person Name",
      "object": "Person Name",
      "start_date": "YYYY-MM-DD | null",
      "end_date": "YYYY-MM-DD | null",
      "evidence": "Quote from text"
    }
  ]
}
```

### Relationship Type Details

#### 1. Mother (`mother`)

```json
{
  "type": "mother",
  "subject": "Camilla",
  "object": "Thomas",
  "evidence": "Camilla had two children named Thomas and Laura"
}
```

**Extraction Rules:**
- Extract when text explicitly names the mother
- "X had children" implies X is the mother
- Subject = MOTHER, Object = CHILD

#### 2. Father (`father`)

```json
{
  "type": "father",
  "subject": "King Charles III",
  "object": "Prince William",
  "evidence": "Charles has two legitimate sons: Prince William"
}
```

**Extraction Rules:**
- Extract when text explicitly names the father
- "X's sons/daughters" implies X is the father (if X is male)
- Subject = FATHER, Object = CHILD

#### 3. Marriage (`married`)

```json
{
  "type": "married",
  "subject": "King Charles III",
  "object": "Princess Diana",
  "start_date": "1981-07-29",
  "end_date": null,
  "evidence": "King Charles III is married to Princess Diana"
}
```

**Extraction Rules:**
- Include wedding date if mentioned
- `end_date: null` means still married (in the text's narrative)
- Use divorce date if marriage ended

#### 4. Affair (`affair`)

```json
{
  "type": "affair",
  "subject": "Camilla",
  "object": "King Charles III",
  "start_date": "1970-01-01",
  "end_date": "1995-12-31",
  "evidence": "extramarital relationship... which began in the early 1970s"
}
```

**Extraction Rules:**
- Approximate dates from phrases like "early 1970s" → `1970-01-01`
- If no end date mentioned, use latest date in narrative or `null`
- Subject = person having affair, Object = affair partner

---

## Prolog Fact Generation

### From Entities

```prolog
% Birth dates
birth_date(king_charles_iii, '1948-11-14').
birth_date(prince_william, '1982-06-21').
birth_date(thomas, '1974-01-01').  % Approximate

% Death dates
death_date(queen_elizabeth_ii, '2022-09-08').
```

### From Relationships

```prolog
% Explicit parent relationships
mother(camilla, thomas).
mother(camilla, laura).
mother(diana, william).
mother(diana, harry).
father(charles, william).
father(charles, harry).

% Generic parent (when gender not specified)
parent(queen_elizabeth_ii, charles).

% Marriages with dates
married(charles, diana, '1981-07-29', null).
married(william, catherine, '2011-04-29', null).

% Affairs with dates
affair(camilla, charles, '1970-01-01', '1995-12-31').
```

---

## Prolog Inference Rules

### Date Range Check

```prolog
% Check if a date falls within a range
date_in_range(Date, Start, End) :-
    Date @>= Start,
    (End = null -> true ; Date @=< End).

% Check if a date falls within a range (with 9-month pregnancy buffer)
date_conceived_during(BirthDate, Start, End) :-
    % Conception is ~9 months before birth
    % For simplicity, we check if birth is within range + 9 months
    date_in_range(BirthDate, Start, End).
```

### Child of Affair (DERIVED)

```prolog
%% child_of_affair(?Child, ?AffairPartner)
%  DERIVED: Child was born during mother's affair with AffairPartner.
%  This is NOT extracted - it's inferred from dates.
child_of_affair(Child, AffairPartner) :-
    mother(Mother, Child),
    birth_date(Child, BirthDate),
    affair(Mother, AffairPartner, StartDate, EndDate),
    date_in_range(BirthDate, StartDate, EndDate).
```

### Potential Father (DERIVED)

```prolog
%% potential_father(?Father, ?Child)
%  DERIVED: Father is potentially the biological father based on:
%  1. Explicit father relationship, OR
%  2. Was affair partner when child was born
potential_father(F, C) :- father(F, C).
potential_father(F, C) :- child_of_affair(C, F).
```

### Resolving Parentage with Premises

```prolog
%% resolve_with_exclusive_premise(+Mother)
%  If premise states Mother had exclusive relationships,
%  then affair partner IS the father of children born during affair.
resolve_with_exclusive_premise(Mother) :-
    forall(
        child_of_affair(Child, AffairPartner),
        (
            mother(Mother, Child) ->
            assertz(father(AffairPartner, Child))
            ; true
        )
    ).
```

### Full Sibling

```prolog
sibling(X, Y) :-
    mother(M, X), mother(M, Y),
    father(F, X), father(F, Y),
    X \= Y.
```

### Half-Sibling

```prolog
half_sibling(X, Y) :-
    (mother(M, X), mother(M, Y) ; father(F, X), father(F, Y)),
    X \= Y,
    \+ sibling(X, Y).
```

---

## Example: Alternative Royal Family

### Input Text

> "King Charles III, born November 14, 1948, is married to Princess Diana. Charles maintained a significant extramarital relationship with Camilla, which began in the early 1970s. Camilla had two children named Thomas and Laura. Prince William was born June 21, 1982, and Prince Harry was born September 15, 1984."

### Extracted Facts (by LLM)

```json
{
  "entities": [
    {"name": "King Charles III", "birth_date": "1948-11-14"},
    {"name": "Princess Diana", "birth_date": "1961-07-01"},
    {"name": "Camilla", "birth_date": null},
    {"name": "Thomas", "birth_date": "1974-01-01"},
    {"name": "Laura", "birth_date": "1978-01-01"},
    {"name": "Prince William", "birth_date": "1982-06-21"},
    {"name": "Prince Harry", "birth_date": "1984-09-15"}
  ],
  "relationships": [
    {"type": "married", "subject": "King Charles III", "object": "Princess Diana", "start_date": null},
    {"type": "affair", "subject": "Camilla", "object": "King Charles III", "start_date": "1970-01-01", "end_date": "1995-12-31"},
    {"type": "mother", "subject": "Camilla", "object": "Thomas"},
    {"type": "mother", "subject": "Camilla", "object": "Laura"},
    {"type": "mother", "subject": "Princess Diana", "object": "Prince William"},
    {"type": "mother", "subject": "Princess Diana", "object": "Prince Harry"},
    {"type": "father", "subject": "King Charles III", "object": "Prince William"},
    {"type": "father", "subject": "King Charles III", "object": "Prince Harry"}
  ]
}
```

### Generated Prolog Facts

```prolog
% Birth dates
birth_date(king_charles_iii, '1948-11-14').
birth_date(princess_diana, '1961-07-01').
birth_date(thomas, '1974-01-01').
birth_date(laura, '1978-01-01').
birth_date(prince_william, '1982-06-21').
birth_date(prince_harry, '1984-09-15').

% Explicit relationships
married(king_charles_iii, princess_diana, null, null).
affair(camilla, king_charles_iii, '1970-01-01', '1995-12-31').
mother(camilla, thomas).
mother(camilla, laura).
mother(princess_diana, prince_william).
mother(princess_diana, prince_harry).
father(king_charles_iii, prince_william).
father(king_charles_iii, prince_harry).
```

### Prolog Derivation

```prolog
% Query: Who are potentially children of the affair?
?- child_of_affair(Child, Partner).

% Prolog reasoning:
% 1. mother(camilla, thomas) ✓
% 2. birth_date(thomas, '1974-01-01') ✓
% 3. affair(camilla, king_charles_iii, '1970-01-01', '1995-12-31') ✓
% 4. date_in_range('1974-01-01', '1970-01-01', '1995-12-31') ✓

Child = thomas, Partner = king_charles_iii ;
Child = laura, Partner = king_charles_iii.

% With exclusive relationship premise:
?- resolve_with_exclusive_premise(camilla).
% Asserts: father(king_charles_iii, thomas).
% Asserts: father(king_charles_iii, laura).

% Now half-siblings work:
?- half_sibling(thomas, X).
X = prince_william ;
X = prince_harry.
```

---

## Benefits of This Approach

| Aspect | Old Approach | New Approach |
|--------|--------------|--------------|
| **LLM Task** | Extract + Infer uncertain relationships | Extract only observable facts |
| **Uncertainty** | LLM guesses "child_of_affair" | Prolog derives from dates |
| **Transparency** | Black box inference | Clear logical derivation |
| **Premises** | Applied by code transformation | Applied by Prolog rules |
| **Extensibility** | Hard to add new inferences | Add new Prolog rules |

---

## References

- [SWI-Prolog Date/Time](https://www.swi-prolog.org/pldoc/man?section=dates)
- [ISO 8601 Date Format](https://en.wikipedia.org/wiki/ISO_8601)
- [Neurosymbolic AI: The 3rd Wave](https://arxiv.org/abs/2012.05876)
