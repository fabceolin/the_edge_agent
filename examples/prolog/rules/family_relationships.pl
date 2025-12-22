% ============================================================================
% Family Relationships Rules Library (with Temporal Reasoning)
% ============================================================================
%
% This file contains Prolog rules for inferring family relationships from
% a set of base facts including temporal information (dates).
%
% KEY PRINCIPLE: LLM extracts observable facts. Prolog derives inferences.
%
% Usage in TEA YAML agents:
%   settings:
%     prolog:
%       consult:
%         - rules/family_relationships.pl
%
% ============================================================================

% ----------------------------------------------------------------------------
% Dynamic predicate declarations
% ----------------------------------------------------------------------------

% Core relationships (extracted by LLM)
:- dynamic mother/2.           % mother(Mother, Child)
:- dynamic father/2.           % father(Father, Child)
:- dynamic parent/2.           % parent(Parent, Child) - generic
:- dynamic married/4.          % married(P1, P2, StartDate, EndDate)
:- dynamic affair/4.           % affair(Person, Partner, StartDate, EndDate)

% Temporal attributes (extracted by LLM)
:- dynamic birth_date/2.       % birth_date(Person, Date)
:- dynamic death_date/2.       % death_date(Person, Date)

% Thread-local for TEA state interface
:- thread_local state/2.
:- thread_local return_value/2.

% ----------------------------------------------------------------------------
% DATE UTILITIES
% ----------------------------------------------------------------------------

%% date_in_range(+Date, +StartDate, +EndDate)
%  True if Date falls within [StartDate, EndDate].
%  Handles null EndDate as "ongoing".
date_in_range(Date, Start, End) :-
    Date @>= Start,
    (End = null -> true ; Date @=< End).

%% date_before(+Date1, +Date2)
%  True if Date1 is before Date2.
date_before(D1, D2) :-
    D1 @< D2.

%% date_after(+Date1, +Date2)
%  True if Date1 is after Date2.
date_after(D1, D2) :-
    D1 @> D2.

%% years_between(+Date1, +Date2, -Years)
%  Approximate years between two dates (simplified).
%  For precise calculation, use SWI-Prolog's date library.
years_between(Date1, Date2, Years) :-
    atom_codes(Date1, C1),
    atom_codes(Date2, C2),
    % Extract year (first 4 characters)
    append(Y1Codes, _, C1), length(Y1Codes, 4),
    append(Y2Codes, _, C2), length(Y2Codes, 4),
    number_codes(Year1, Y1Codes),
    number_codes(Year2, Y2Codes),
    Years is abs(Year2 - Year1).

% ----------------------------------------------------------------------------
% DERIVED RELATIONSHIPS (Inferred by Prolog)
% ----------------------------------------------------------------------------

%% child_of_affair(?Child, ?AffairPartner)
%  DERIVED: Child was born during mother's affair with AffairPartner.
%
%  This is the KEY inference - LLM does NOT extract this directly.
%  It's derived from:
%    1. mother(Mother, Child) - extracted
%    2. birth_date(Child, BirthDate) - extracted
%    3. affair(Mother, AffairPartner, Start, End) - extracted
%    4. BirthDate is within [Start, End] - INFERRED
child_of_affair(Child, AffairPartner) :-
    mother(Mother, Child),
    birth_date(Child, BirthDate),
    affair(Mother, AffairPartner, StartDate, EndDate),
    date_in_range(BirthDate, StartDate, EndDate).

%% potential_father(?Father, ?Child)
%  DERIVED: Father is potentially the biological father.
%  Either:
%    1. Explicitly stated as father, OR
%    2. Was affair partner when child was born
potential_father(F, C) :- father(F, C).
potential_father(F, C) :- child_of_affair(C, F).

%% child_born_during_marriage(?Child, ?Spouse1, ?Spouse2)
%  DERIVED: Child was born during a marriage.
child_born_during_marriage(Child, Spouse1, Spouse2) :-
    birth_date(Child, BirthDate),
    married(Spouse1, Spouse2, StartDate, EndDate),
    date_in_range(BirthDate, StartDate, EndDate).

% ----------------------------------------------------------------------------
% PREMISE APPLICATION
% ----------------------------------------------------------------------------

%% resolve_exclusive_relationship_premise(+Mother)
%  Apply "exclusive relationship" premise to resolve uncertain parentage.
%
%  If we assume Mother only had one partner at a time, then the affair
%  partner IS the biological father of children born during the affair.
resolve_exclusive_relationship_premise(Mother) :-
    forall(
        (child_of_affair(Child, AffairPartner), mother(Mother, Child)),
        (
            % Only assert if not already known
            (father(AffairPartner, Child) -> true ; assertz(father(AffairPartner, Child)))
        )
    ).

%% resolve_all_exclusive_premises
%  Apply exclusive relationship premise to all mothers with affairs.
resolve_all_exclusive_premises :-
    forall(
        affair(Mother, _, _, _),
        resolve_exclusive_relationship_premise(Mother)
    ).

% ----------------------------------------------------------------------------
% SIBLING RELATIONSHIPS
% ----------------------------------------------------------------------------

%% sibling(?X, ?Y)
%  Full siblings: share BOTH mother AND father.
sibling(X, Y) :-
    mother(M, X), mother(M, Y),
    father(F, X), father(F, Y),
    X \= Y.

%% half_sibling(?X, ?Y)
%  Half siblings: share exactly ONE parent (mother or father, not both).
half_sibling(X, Y) :-
    X \= Y,
    (
        (mother(M, X), mother(M, Y)) ;
        (father(F, X), father(F, Y))
    ),
    \+ sibling(X, Y).

%% maternal_half_sibling(?X, ?Y)
%  Half siblings through mother only.
maternal_half_sibling(X, Y) :-
    mother(M, X), mother(M, Y),
    X \= Y,
    \+ sibling(X, Y).

%% paternal_half_sibling(?X, ?Y)
%  Half siblings through father only.
paternal_half_sibling(X, Y) :-
    father(F, X), father(F, Y),
    X \= Y,
    \+ sibling(X, Y).

%% any_sibling(?X, ?Y)
%  Any type of sibling (full or half).
any_sibling(X, Y) :- sibling(X, Y).
any_sibling(X, Y) :- half_sibling(X, Y).

% ----------------------------------------------------------------------------
% EXTENDED FAMILY
% ----------------------------------------------------------------------------

%% grandparent(?Grandparent, ?Grandchild)
grandparent(GP, GC) :-
    parent_of(GP, P),
    parent_of(P, GC).

%% grandchild(?Grandchild, ?Grandparent)
grandchild(GC, GP) :- grandparent(GP, GC).

%% ancestor(?Ancestor, ?Descendant)
%  Transitive closure of parent relationship.
ancestor(A, D) :- parent_of(A, D).
ancestor(A, D) :- parent_of(A, X), ancestor(X, D).

%% descendant(?Descendant, ?Ancestor)
descendant(D, A) :- ancestor(A, D).

%% uncle_aunt(?UncleAunt, ?NephewNiece)
uncle_aunt(UA, NN) :-
    parent_of(P, NN),
    any_sibling(P, UA).

%% cousin(?X, ?Y)
%  First cousins: their parents are siblings.
cousin(X, Y) :-
    parent_of(PX, X),
    parent_of(PY, Y),
    any_sibling(PX, PY),
    X \= Y.

% ----------------------------------------------------------------------------
% UNIFIED PARENT ACCESSOR
% ----------------------------------------------------------------------------

%% parent_of(?Parent, ?Child)
%  Unified accessor for any parent relationship.
parent_of(P, C) :- mother(P, C).
parent_of(P, C) :- father(P, C).
parent_of(P, C) :- parent(P, C).

%% child_of(?Child, ?Parent)
child_of(C, P) :- parent_of(P, C).

% ----------------------------------------------------------------------------
% QUERY HELPERS
% ----------------------------------------------------------------------------

%% find_siblings(+Person, -Siblings)
find_siblings(Person, Siblings) :-
    findall(S, sibling(Person, S), Siblings).

%% find_half_siblings(+Person, -HalfSiblings)
find_half_siblings(Person, HalfSiblings) :-
    findall(H, half_sibling(Person, H), HalfSiblings).

%% find_all_siblings(+Person, -AllSiblings)
find_all_siblings(Person, AllSiblings) :-
    find_siblings(Person, Full),
    find_half_siblings(Person, Half),
    append(Full, Half, AllSiblings).

%% find_parents(+Person, -Parents)
find_parents(Person, Parents) :-
    findall(P, parent_of(P, Person), Parents).

%% find_children(+Person, -Children)
find_children(Person, Children) :-
    findall(C, parent_of(Person, C), Children).

%% find_potential_fathers(+Child, -Fathers)
find_potential_fathers(Child, Fathers) :-
    findall(F, potential_father(F, Child), Fathers).

%% find_children_of_affair(+AffairPartner, -Children)
find_children_of_affair(AffairPartner, Children) :-
    findall(C, child_of_affair(C, AffairPartner), Children).

% ----------------------------------------------------------------------------
% AGE AND TEMPORAL QUERIES
% ----------------------------------------------------------------------------

%% born_before(+Person1, +Person2)
%  True if Person1 was born before Person2.
born_before(P1, P2) :-
    birth_date(P1, D1),
    birth_date(P2, D2),
    date_before(D1, D2).

%% born_after(+Person1, +Person2)
born_after(P1, P2) :- born_before(P2, P1).

%% older_sibling(?Older, ?Younger)
%  True if Older is an older sibling of Younger.
older_sibling(Older, Younger) :-
    any_sibling(Older, Younger),
    born_before(Older, Younger).

%% younger_sibling(?Younger, ?Older)
younger_sibling(Y, O) :- older_sibling(O, Y).

%% alive(?Person)
%  True if Person has no death_date.
alive(Person) :-
    birth_date(Person, _),
    \+ death_date(Person, _).

%% deceased(?Person)
deceased(Person) :- death_date(Person, _).

% ----------------------------------------------------------------------------
% TEA STATE INTERFACE
% ----------------------------------------------------------------------------

%% return(+Key, +Value)
%  Helper for TEA - asserts return_value/2 fact.
return(Key, Value) :-
    assertz(return_value(Key, Value)).

% ----------------------------------------------------------------------------
% EXAMPLE USAGE
% ----------------------------------------------------------------------------
%
% % Load facts (extracted by LLM)
% assertz(birth_date(charles, '1948-11-14')).
% assertz(birth_date(william, '1982-06-21')).
% assertz(birth_date(harry, '1984-09-15')).
% assertz(birth_date(thomas, '1974-01-01')).
% assertz(birth_date(laura, '1978-01-01')).
%
% assertz(mother(diana, william)).
% assertz(mother(diana, harry)).
% assertz(mother(camilla, thomas)).
% assertz(mother(camilla, laura)).
% assertz(father(charles, william)).
% assertz(father(charles, harry)).
%
% assertz(affair(camilla, charles, '1970-01-01', '1995-12-31')).
%
% % Derived query - who are children of the affair?
% ?- child_of_affair(Child, Partner).
% Child = thomas, Partner = charles ;
% Child = laura, Partner = charles.
%
% % Apply premise
% ?- resolve_exclusive_relationship_premise(camilla).
% % Now father(charles, thomas) and father(charles, laura) are asserted
%
% % Query half-siblings
% ?- find_half_siblings(thomas, H).
% H = [william, harry].
%
% % Query full siblings
% ?- find_siblings(thomas, S).
% S = [laura].
% ============================================================================
