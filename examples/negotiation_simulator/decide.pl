% decide.pl
%
% Story 4 — Camada Simbólica de Decisão (Inferência Matemática).
% Pure Prolog / CLP(FD) rules: given a scenario's financial constraints
% (from Story 1) and a buyer's offer extracted by the LLM (Story 3),
% produce a deterministic decision: accept, reject, or counter(TargetPrice).
%
% Section 12 of the article extends the decision tree with three
% concession-discipline rules:
%
%   * Karrass 50% rule — each new counter concedes ≤ ratio% of the previous
%   * Concession budget — after N counters, lock to floor (anchor)
%   * Stall detector  — buyer repeats face value StallN times → reject
%
% All monetary values are expressed in *cents* (integers) so CLP(FD) can
% reason over them without fractional arithmetic.
%
% Public entry point:
%   decide(+Offer, +Constraints, +DisciplineState, -Decision)
%
% Offer       = offer{ unit_cents:Int, term_days:Int, demands:ListOfAtoms }
%
% Constraints = cons{ unit_cost_cents, list_price_cents, min_margin_pct,
%                     max_discount_pct, shipping_cost_pct,
%                     interest_by_term (list of Days-Rate pairs),
%                     batna_cents,
%                     concession_budget, karrass_ratio_pct,
%                     stall_repeat_threshold }
%
% DisciplineState = disc{ concession_count:Int,
%                         last_target_cents:Int,
%                         prev_concession_delta_cents:Int,
%                         buyer_face_history:List,    % newest-first
%                         quote_expired:Bool }
%
% Decision    = accept(final_cents, floor_cents, ceiling_cents)
%             | reject(below_break_even | stalled | quote_expired,
%                      details)
%             | counter(target_cents, floor_cents, ceiling_cents,
%                       buyer_effective_cents, floor_locked, mode)
%
% The article embeds this file's logic verbatim in the `decide` node of
% agent.yaml.  Keep them in sync.

:- use_module(library(clpfd)).
:- use_module(library(lists)).

% ---------------------------------------------------------------------------
% Effective offer: subtract the cost of buyer demands (e.g. free shipping)
% from what they are nominally offering.
% ---------------------------------------------------------------------------

effective_offer_cents(OfferCents, _ShippingPct, [], OfferCents) :- !.
effective_offer_cents(OfferCents, ShippingPct, Demands, Eff) :-
    ( member(free_shipping, Demands) ->
        ShipCost is (OfferCents * ShippingPct) // 100,
        Eff0 is OfferCents - ShipCost
    ;   Eff0 = OfferCents
    ),
    Eff = Eff0.

% ---------------------------------------------------------------------------
% Term interest: longer payment terms reduce the present-value of the offer.
% InterestMap is a list of Days-Rate pairs where Rate is a fractional number
% (e.g. 30-0.0, 60-0.02, 90-0.04).  If the requested term is not in the map
% we conservatively treat it as the largest available rate.
% ---------------------------------------------------------------------------

term_discount_rate(TermDays, InterestMap, Rate) :-
    ( member(TermDays-R, InterestMap) ->
        Rate = R
    ; % Fallback: use the highest rate (most conservative for the seller)
        findall(R0, member(_-R0, InterestMap), Rates),
        ( Rates = [] -> Rate = 0.0 ; max_list(Rates, Rate) )
    ).

apply_term_interest(EffCents, TermDays, InterestMap, Present) :-
    term_discount_rate(TermDays, InterestMap, Rate),
    Penalty is round(EffCents * Rate),
    Present is EffCents - Penalty.

% ---------------------------------------------------------------------------
% Floor: minimum unit price we will accept (in cents).
% Protects both the minimum margin and the BATNA — whichever binds tighter.
% ---------------------------------------------------------------------------

floor_cents(UnitCostCents, MinMarginPct, BatnaCents, Floor) :-
    MarginFloor is UnitCostCents * (100 + MinMarginPct) // 100,
    Floor is max(MarginFloor, BatnaCents).

% ---------------------------------------------------------------------------
% Ceiling: maximum discount we are authorised to offer (in cents).
% Anchors the counter-proposal so we never volunteer below it.
% ---------------------------------------------------------------------------

ceiling_cents(ListPriceCents, MaxDiscountPct, Ceiling) :-
    Ceiling is ListPriceCents * (100 - MaxDiscountPct) // 100.

% ---------------------------------------------------------------------------
% Counter-proposal search (CLP(FD)):
%   Choose V in [LowerBound, Ceiling] such that
%     - V is strictly above the buyer's effective offer (we must *move*),
%     - V is a whole-Real number (V mod 100 = 0, i.e. no cents),
%     - V minimises |V - midpoint(Effective, Ceiling)|
%       (i.e. we meet the buyer roughly halfway between their offer and our
%       authorised ceiling — classic concession split).
%
% If no feasible V exists, we return LowerBound itself as a take-it-or-
% leave-it counter.
% ---------------------------------------------------------------------------

counter_target(EffectiveCents, LowerBoundIn, Ceiling, Target) :-
    EffectiveCents < Ceiling,
    LowerBound is max(LowerBoundIn, EffectiveCents + 1),
    LowerBound =< Ceiling,
    Midpoint is (EffectiveCents + Ceiling) // 2,
    V in LowerBound..Ceiling,
    V mod 100 #= 0,
    Delta #= abs(V - Midpoint),
    labeling([min(Delta)], [V]),
    !,
    Target = V.
counter_target(_, LowerBoundIn, _, LowerBoundIn).        % fallback: hold the line

% ---------------------------------------------------------------------------
% § 12 — Concession discipline.
% ---------------------------------------------------------------------------

% take(N, List, Prefix) — first N items.
take(0, _, []) :- !.
take(_, [], []) :- !.
take(N, [H|T], [H|R]) :- N1 is N-1, take(N1, T, R).

% Stall detector: are the most recent StallN buyer face values all equal?
% History is newest-first.
stall_detected(_History, StallN, false) :- StallN =< 1, !.
stall_detected(History, StallN, Flag) :-
    length(History, L),
    ( L < StallN ->
        Flag = false
    ;   take(StallN, History, Last),
        Last = [H|_],
        ( forall(member(X, Last), X =:= H) -> Flag = true ; Flag = false )
    ).

% disciplined_counter: applies Karrass + budget on top of counter_target.
%
% Three branches:
%
%   1. Concession budget exhausted (Count >= Budget, Budget > 0)
%       → lock to max(Floor, LastTarget); FloorLocked = true; Mode = budget_exhausted.
%
%   2. First counter (LastTarget = 0)
%       → no Karrass constraint applies yet; search in [Floor, Ceil];
%         FloorLocked = false; Mode = first_counter.
%
%   3. Subsequent counter
%       → KarrassLB = max(Floor, LastTarget - PrevDelta * KarrassPct / 100);
%         search in [KarrassLB, Ceil]; FloorLocked = false; Mode = karrass.

disciplined_counter(_Eff, Floor, _Ceil,
                    LastTarget, _PrevDelta, Count, Budget, _KPct,
                    Target, true, budget_exhausted) :-
    Budget > 0,
    Count >= Budget,
    !,
    Target is max(Floor, LastTarget).

disciplined_counter(Eff, Floor, Ceil,
                    0, _PrevDelta, _Count, _Budget, _KPct,
                    Target, false, first_counter) :-
    !,
    counter_target(Eff, Floor, Ceil, Target).

disciplined_counter(Eff, Floor, Ceil,
                    LastTarget, PrevDelta, _Count, _Budget, KPct,
                    Target, false, karrass) :-
    AllowedStep is PrevDelta * KPct // 100,
    KLB0 is LastTarget - AllowedStep,
    KLB  is max(KLB0, Floor),
    counter_target(Eff, KLB, Ceil, Target).

% ---------------------------------------------------------------------------
% Main decision predicate.
% ---------------------------------------------------------------------------

decide(Offer, Constraints, Discipline, Decision) :-
    % Unpack offer
    get_dict(unit_cents, Offer, OfferCents),
    get_dict(term_days, Offer, TermDays),
    get_dict(demands,   Offer, Demands),

    % Unpack constraints
    get_dict(unit_cost_cents,         Constraints, UnitCost),
    get_dict(list_price_cents,        Constraints, ListPrice),
    get_dict(min_margin_pct,          Constraints, MinMargin),
    get_dict(max_discount_pct,        Constraints, MaxDiscount),
    get_dict(shipping_cost_pct,       Constraints, ShippingPct),
    get_dict(interest_by_term,        Constraints, InterestMap),
    get_dict(batna_cents,             Constraints, Batna),
    get_dict(concession_budget,       Constraints, Budget),
    get_dict(karrass_ratio_pct,       Constraints, KarrassPct),
    get_dict(stall_repeat_threshold,  Constraints, StallN),

    % Unpack discipline state
    get_dict(concession_count,             Discipline, CCount),
    get_dict(last_target_cents,            Discipline, LastTarget),
    get_dict(prev_concession_delta_cents,  Discipline, PrevDelta),
    get_dict(buyer_face_history,           Discipline, BHist),
    get_dict(quote_expired,                Discipline, QExpired),

    % Derive the buyer's effective, present-value offer
    effective_offer_cents(OfferCents, ShippingPct, Demands, AfterShipping),
    apply_term_interest(AfterShipping, TermDays, InterestMap, Effective),

    % Derive bounds
    floor_cents(UnitCost, MinMargin, Batna, Floor),
    ceiling_cents(ListPrice, MaxDiscount, Ceiling),

    % Stall detection (operates on face values)
    stall_detected(BHist, StallN, StallFlag),

    % Classify
    ( StallFlag == true ->
        Decision = reject{ reason: stalled,
                           threshold: StallN,
                           buyer_face_history: BHist }
    ; QExpired == true ->
        Decision = reject{ reason: quote_expired,
                           buyer_effective_cents: Effective }
    ; Effective >= Floor ->
        Decision = accept{ final_cents: Effective,
                           floor_cents: Floor,
                           ceiling_cents: Ceiling }
    ; Effective < UnitCost ->
        Decision = reject{ reason: below_break_even,
                           unit_cost_cents: UnitCost,
                           buyer_effective_cents: Effective }
    ; % Otherwise produce a discipline-constrained counter
        disciplined_counter(Effective, Floor, Ceiling,
                            LastTarget, PrevDelta, CCount, Budget, KarrassPct,
                            Target, FloorLocked, Mode),
        Decision = counter{ target_cents: Target,
                            floor_cents: Floor,
                            ceiling_cents: Ceiling,
                            buyer_effective_cents: Effective,
                            floor_locked: FloorLocked,
                            mode: Mode }
    ).

% ---------------------------------------------------------------------------
% Success evaluation (used at the end of the negotiation).
% Given the final agreed price and the scenario constraints, did the
% symbolic side "win" (price >= floor) or not?
% ---------------------------------------------------------------------------

won_negotiation(FinalCents, Constraints) :-
    get_dict(unit_cost_cents,  Constraints, UnitCost),
    get_dict(min_margin_pct,   Constraints, MinMargin),
    get_dict(batna_cents,      Constraints, Batna),
    floor_cents(UnitCost, MinMargin, Batna, Floor),
    FinalCents >= Floor.
