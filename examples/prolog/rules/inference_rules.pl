% ============================================================================
% Inference Rules Library
% ============================================================================
%
% Common inference patterns for neurosymbolic AI applications.
% These rules provide reusable logic for knowledge graph reasoning,
% multi-step inference, and explanation generation.
%
% Usage in TEA YAML agents:
%   settings:
%     prolog:
%       consult:
%         - rules/inference_rules.pl
%
% ============================================================================

% ----------------------------------------------------------------------------
% Dynamic predicate declarations
% ----------------------------------------------------------------------------

:- dynamic fact/3.                  % fact(Subject, Predicate, Object)
:- dynamic rule_applies/2.          % rule_applies(RuleName, Context)
:- dynamic inference/4.             % inference(Conclusion, Premise, Rule, Confidence)
:- dynamic explanation/2.           % explanation(Conclusion, Steps)

% Thread-local for TEA state interface
:- thread_local state/2.
:- thread_local return_value/2.

% ----------------------------------------------------------------------------
% GRAPH TRAVERSAL
% ----------------------------------------------------------------------------

%% connected(+Node1, +Node2)
%  True if Node1 is directly connected to Node2.
connected(N1, N2) :-
    fact(N1, _, N2).
connected(N1, N2) :-
    fact(N2, _, N1).

%% path(+Start, +End, -Path)
%  Find a path from Start to End.
path(Start, End, [Start, End]) :-
    connected(Start, End).

path(Start, End, [Start|Rest]) :-
    connected(Start, Next),
    Next \= End,
    path(Next, End, Rest),
    \+ member(Start, Rest).  % Prevent cycles

%% shortest_path(+Start, +End, -Path)
%  Find shortest path from Start to End.
shortest_path(Start, End, Path) :-
    findall(P, path(Start, End, P), Paths),
    Paths \= [],
    sort_by_length(Paths, Sorted),
    Sorted = [Path|_].

sort_by_length(Paths, Sorted) :-
    map_list_to_pairs(length, Paths, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).

% ----------------------------------------------------------------------------
% TRANSITIVE CLOSURE
% ----------------------------------------------------------------------------

%% transitive(+Relation, ?X, ?Y)
%  Transitive closure of a binary relation.
transitive(Relation, X, Y) :-
    fact(X, Relation, Y).

transitive(Relation, X, Z) :-
    fact(X, Relation, Y),
    transitive(Relation, Y, Z).

%% transitive_path(+Relation, +Start, -End, -Path)
%  Find transitive path with intermediate nodes.
transitive_path(Relation, Start, End, [Start, End]) :-
    fact(Start, Relation, End).

transitive_path(Relation, Start, End, [Start|Rest]) :-
    fact(Start, Relation, Next),
    transitive_path(Relation, Next, End, Rest).

% ----------------------------------------------------------------------------
% SYMMETRIC RELATIONS
% ----------------------------------------------------------------------------

%% symmetric_fact(+Relation, ?X, ?Y)
%  Treat a relation as symmetric.
symmetric_fact(Relation, X, Y) :-
    fact(X, Relation, Y).
symmetric_fact(Relation, X, Y) :-
    fact(Y, Relation, X).

% ----------------------------------------------------------------------------
% INFERENCE CHAIN
% ----------------------------------------------------------------------------

%% infer(+Goal, -Explanation)
%  Attempt to prove Goal and explain how.
infer(fact(S, P, O), direct(S, P, O)) :-
    fact(S, P, O).

infer(transitive(Rel, X, Z), chain(X, Path, Z)) :-
    transitive_path(Rel, X, Z, Path).

infer(and(G1, G2), and(E1, E2)) :-
    infer(G1, E1),
    infer(G2, E2).

infer(or(G1, _), or_left(E1)) :-
    infer(G1, E1).

infer(or(_, G2), or_right(E2)) :-
    infer(G2, E2).

%% record_inference(+Conclusion, +Premises, +RuleName, +Confidence)
%  Record an inference for explanation.
record_inference(Conclusion, Premises, Rule, Confidence) :-
    assertz(inference(Conclusion, Premises, Rule, Confidence)).

%% get_inference_chain(+Conclusion, -Chain)
%  Get the chain of inferences leading to a conclusion.
get_inference_chain(Conclusion, Chain) :-
    findall(step(Premise, Rule, Conf),
        inference(Conclusion, Premise, Rule, Conf),
        Chain).

% ----------------------------------------------------------------------------
% RULE-BASED INFERENCE
% ----------------------------------------------------------------------------

%% apply_inference_rule(+RuleName, +Input, -Output)
%  Apply a named inference rule.

% Modus ponens: if P and P->Q, then Q
apply_inference_rule(modus_ponens, [P, implies(P, Q)], Q).

% Modus tollens: if not Q and P->Q, then not P
apply_inference_rule(modus_tollens, [not(Q), implies(P, Q)], not(P)).

% Transitivity: if R(A,B) and R(B,C), then R(A,C)
apply_inference_rule(transitivity, [rel(R, A, B), rel(R, B, C)], rel(R, A, C)).

% Symmetry: if symmetric(R) and R(A,B), then R(B,A)
apply_inference_rule(symmetry, [symmetric(R), rel(R, A, B)], rel(R, B, A)).

%% chain_rules(+Rules, +Input, -FinalOutput, -Steps)
%  Apply a chain of rules.
chain_rules([], Output, Output, []).
chain_rules([Rule|Rest], Input, FinalOutput, [step(Rule, Intermediate)|Steps]) :-
    apply_inference_rule(Rule, Input, Intermediate),
    chain_rules(Rest, [Intermediate|Input], FinalOutput, Steps).

% ----------------------------------------------------------------------------
% CONFIDENCE PROPAGATION
% ----------------------------------------------------------------------------

%% combine_confidence(+Conf1, +Conf2, +Method, -Combined)
%  Combine two confidence scores.
combine_confidence(C1, C2, min, C) :- C is min(C1, C2).
combine_confidence(C1, C2, max, C) :- C is max(C1, C2).
combine_confidence(C1, C2, product, C) :- C is C1 * C2.
combine_confidence(C1, C2, average, C) :- C is (C1 + C2) / 2.

%% propagate_confidence(+Facts, +Method, -MinConfidence)
%  Get minimum confidence across a set of facts.
propagate_confidence(Facts, Method, Result) :-
    findall(C, (member(F, Facts), fact_confidence(F, C)), Confs),
    aggregate_confidence(Confs, Method, Result).

aggregate_confidence([C], _, C).
aggregate_confidence([C1, C2|Rest], Method, Result) :-
    combine_confidence(C1, C2, Method, C3),
    aggregate_confidence([C3|Rest], Method, Result).

fact_confidence(F, C) :-
    (inference(F, _, _, C) -> true ; C = 1.0).

% ----------------------------------------------------------------------------
% EXPLANATION GENERATION
% ----------------------------------------------------------------------------

%% explain(+Conclusion, -Explanation)
%  Generate human-readable explanation.
explain(Conclusion, Explanation) :-
    findall(Step, explain_step(Conclusion, Step), Steps),
    atomic_list_concat(Steps, '\n', Explanation).

explain_step(Conclusion, Step) :-
    inference(Conclusion, Premise, Rule, Conf),
    format(atom(Step), 'Applied ~w to ~w with confidence ~2f', [Rule, Premise, Conf]).

%% build_proof_tree(+Goal, -Tree)
%  Build a proof tree for a goal.
build_proof_tree(Goal, leaf(Goal)) :-
    fact(_, _, _),
    Goal = fact(_, _, _),
    call(Goal).

build_proof_tree(Goal, node(Goal, SubTrees)) :-
    inference(Goal, Premises, _, _),
    findall(T, (member(P, Premises), build_proof_tree(P, T)), SubTrees).

% ----------------------------------------------------------------------------
% KNOWLEDGE GRAPH QUERIES
% ----------------------------------------------------------------------------

%% find_relations(+Entity, -Relations)
%  Find all relations involving an entity.
find_relations(Entity, Relations) :-
    findall(relation(Pred, Obj), fact(Entity, Pred, Obj), Outgoing),
    findall(relation(Pred, Subj), fact(Subj, Pred, Entity), Incoming),
    append(Outgoing, Incoming, Relations).

%% find_by_type(+Type, -Entities)
%  Find entities by their type.
find_by_type(Type, Entities) :-
    findall(E, fact(E, type, Type), Entities).

%% find_by_property(+Property, +Value, -Entities)
%  Find entities with a specific property value.
find_by_property(Property, Value, Entities) :-
    findall(E, fact(E, Property, Value), Entities).

%% neighbors(+Entity, -Neighbors)
%  Find immediate neighbors of an entity.
neighbors(Entity, Neighbors) :-
    findall(N, (fact(Entity, _, N) ; fact(N, _, Entity)), Ns),
    list_to_set(Ns, Neighbors).

%% common_neighbors(+E1, +E2, -Common)
%  Find common neighbors of two entities.
common_neighbors(E1, E2, Common) :-
    neighbors(E1, N1),
    neighbors(E2, N2),
    intersection(N1, N2, Common).

% ----------------------------------------------------------------------------
% PATTERN MATCHING
% ----------------------------------------------------------------------------

%% match_pattern(+Pattern, -Bindings)
%  Match a pattern against the knowledge graph.
match_pattern([], []).
match_pattern([fact(S, P, O)|Rest], Bindings) :-
    fact(S, P, O),
    match_pattern(Rest, RestBindings),
    Bindings = [bind(S, P, O)|RestBindings].

%% find_pattern(+Pattern, -AllBindings)
%  Find all matches for a pattern.
find_pattern(Pattern, AllBindings) :-
    findall(B, match_pattern(Pattern, B), AllBindings).

% ----------------------------------------------------------------------------
% TEA STATE INTERFACE
% ----------------------------------------------------------------------------

%% return(+Key, +Value)
%  Helper for TEA - asserts return_value/2 fact.
return(Key, Value) :-
    assertz(return_value(Key, Value)).

%% load_facts_from_state
%  Load facts from TEA state (expects list of fact(S,P,O) terms).
load_facts_from_state :-
    (state(facts, Facts) ->
        load_facts(Facts)
    ; true).

load_facts([]).
load_facts([fact(S, P, O)|Rest]) :-
    assertz(fact(S, P, O)),
    load_facts(Rest).

% ============================================================================
% EXAMPLE USAGE
% ============================================================================
%
% % Load knowledge graph facts
% assertz(fact(alice, parent_of, bob)).
% assertz(fact(bob, parent_of, carol)).
% assertz(fact(carol, parent_of, david)).
%
% % Query transitive relationships
% ?- transitive(parent_of, alice, X).
% X = bob ;
% X = carol ;
% X = david.
%
% % Find path between nodes
% ?- transitive_path(parent_of, alice, david, Path).
% Path = [alice, bob, carol, david].
%
% % Find neighbors
% ?- neighbors(bob, N).
% N = [alice, carol].
%
% % Pattern matching
% ?- find_pattern([fact(X, parent_of, Y), fact(Y, parent_of, Z)], Bindings).
% ============================================================================
