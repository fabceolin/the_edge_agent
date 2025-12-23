% TEA Prolog Predicates
% Prolog-side term processing for The Edge Agent
% These predicates enable 100% accurate fact/query detection
% using Prolog's native read_term/3 parser.
%
% NOTE: state/2, return_value/2, and return/2 are defined in the Rust runtime
% via setup_predicates_in_context(). This file only defines tea_load_code/1
% and supporting predicates.

% Thread-local predicate for tracking user-asserted facts (for cleanup)
:- thread_local(tea_user_fact/1).

% Action predicates - should be called, not asserted
% Also includes structural operators that should never be asserted
tea_action_predicate(return).
tea_action_predicate(state).
tea_action_predicate(',').  % Conjunction - must be called, not asserted
tea_action_predicate(';').  % Disjunction - must be called, not asserted
tea_action_predicate('->').  % If-then - must be called, not asserted
tea_action_predicate('*->').  % Soft cut if-then - must be called, not asserted
tea_action_predicate('>').   % Arithmetic comparison
tea_action_predicate('<').   % Arithmetic comparison
tea_action_predicate('>=').  % Arithmetic comparison
tea_action_predicate('=<').  % Arithmetic comparison
tea_action_predicate('=:=').  % Arithmetic equality
tea_action_predicate('=\\=').  % Arithmetic inequality
tea_action_predicate('=').   % Unification
tea_action_predicate('\\=').  % Not unifiable
tea_action_predicate('==').  % Structural equality
tea_action_predicate('\\==').  % Structural inequality
tea_action_predicate('@<').  % Term comparison
tea_action_predicate('@>').  % Term comparison
tea_action_predicate('@=<').  % Term comparison
tea_action_predicate('@>=').  % Term comparison
tea_action_predicate('\\+').  % Negation as failure
tea_action_predicate(is).   % Arithmetic evaluation
tea_action_predicate(findall).
tea_action_predicate(bagof).
tea_action_predicate(setof).
tea_action_predicate(member).
tea_action_predicate(memberchk).
tea_action_predicate(append).
tea_action_predicate(length).
tea_action_predicate(nth0).
tea_action_predicate(nth1).
tea_action_predicate(msort).
tea_action_predicate(sort).
tea_action_predicate(reverse).
tea_action_predicate(last).
tea_action_predicate(sumlist).
tea_action_predicate(max_list).
tea_action_predicate(min_list).
tea_action_predicate(forall).
tea_action_predicate(aggregate_all).
tea_action_predicate(aggregate).
tea_action_predicate(call).
tea_action_predicate(once).
tea_action_predicate(succ).
tea_action_predicate(plus).
tea_action_predicate(abs).
tea_action_predicate(sign).
tea_action_predicate(max).
tea_action_predicate(min).
tea_action_predicate(write).
tea_action_predicate(writeln).
tea_action_predicate(print).
tea_action_predicate(format).
tea_action_predicate(atom).
tea_action_predicate(number).
tea_action_predicate(integer).
tea_action_predicate(float).
tea_action_predicate(compound).
tea_action_predicate(is_list).
tea_action_predicate(ground).
tea_action_predicate(atom_string).
tea_action_predicate(atom_codes).
tea_action_predicate(atom_chars).

% Determine if a term is a fact (should be asserted)
tea_is_fact(Term) :-
    compound(Term),
    ground(Term),
    functor(Term, F, _),
    atom(F),
    \+ tea_action_predicate(F).

% Process a single term from user code (unsandboxed version)
tea_process_term((:-Body)) :- !, call(Body).
tea_process_term((Head :- Body)) :- !, assertz((Head :- Body)).
tea_process_term(Term) :-
    ( tea_is_fact(Term)
    -> assertz(Term), assertz(tea_user_fact(Term))
    ; call(Term)
    ).

% Check if a goal uses only TEA predicates (safe to run without sandbox)
tea_uses_only_tea_predicates(Goal) :-
    Goal = (A, B), !,
    tea_uses_only_tea_predicates(A),
    tea_uses_only_tea_predicates(B).
tea_uses_only_tea_predicates(Goal) :-
    Goal = (A ; B), !,
    tea_uses_only_tea_predicates(A),
    tea_uses_only_tea_predicates(B).
tea_uses_only_tea_predicates(Goal) :-
    compound(Goal),
    functor(Goal, F, _),
    tea_action_predicate(F).

% Process a single term from user code (sandboxed version)
% Uses sandbox:safe_call/1 for dangerous predicates, but allows TEA predicates directly
tea_process_term_sandboxed((:-Body)) :- !,
    ( tea_uses_only_tea_predicates(Body)
    -> call(Body)
    ; sandbox:safe_call(Body)
    ).
tea_process_term_sandboxed((Head :- Body)) :- !, assertz((Head :- Body)).
tea_process_term_sandboxed(Term) :-
    ( tea_is_fact(Term)
    -> assertz(Term), assertz(tea_user_fact(Term))
    ; tea_uses_only_tea_predicates(Term)
    -> call(Term)
    ; sandbox:safe_call(Term)
    ).

% Read and process terms from a stream (unsandboxed version)
tea_load_terms(Stream) :-
    catch(
        read_term(Stream, Term, []),
        Error,
        throw(tea_syntax_error(Error))
    ),
    ( Term == end_of_file
    -> true
    ; tea_process_term(Term),
      tea_load_terms(Stream)
    ).

% Read and process terms from a stream (sandboxed version)
tea_load_terms_sandboxed(Stream) :-
    catch(
        read_term(Stream, Term, []),
        Error,
        throw(tea_syntax_error(Error))
    ),
    ( Term == end_of_file
    -> true
    ; tea_process_term_sandboxed(Term),
      tea_load_terms_sandboxed(Stream)
    ).

% Main entry point: load code from a string (unsandboxed version)
tea_load_code(CodeAtom) :-
    atom_string(CodeAtom, CodeString),
    open_string(CodeString, Stream),
    catch(
        tea_load_terms(Stream),
        Error,
        ( close(Stream), throw(Error) )
    ),
    close(Stream).

% Main entry point: load code from a string (sandboxed version)
% Uses sandbox:safe_call/1 to enforce security restrictions on user code
tea_load_code_sandboxed(CodeAtom) :-
    atom_string(CodeAtom, CodeString),
    open_string(CodeString, Stream),
    catch(
        tea_load_terms_sandboxed(Stream),
        Error,
        ( close(Stream), throw(Error) )
    ),
    close(Stream).

% Clean up user-asserted facts
tea_cleanup_facts :-
    forall(tea_user_fact(Fact), retract(Fact)),
    retractall(tea_user_fact(_)).
