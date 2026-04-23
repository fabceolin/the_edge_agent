% decide.pl
%
% Story 4 — Camada Simbólica de Decisão (Inferência Matemática).
% Pure Prolog / CLP(FD) rules: given a scenario's financial constraints
% (from Story 1) and a buyer's offer extracted by the LLM (Story 3),
% produce a deterministic decision: accept, reject, or counter(TargetPrice).
%
% All monetary values are expressed in *cents* (integers) so CLP(FD) can
% reason over them without fractional arithmetic.
%
% Public entry point:
%   decide(+Offer, +Constraints, -Decision)
%
% Offer       = offer{ unit_cents:Int, term_days:Int, demands:ListOfAtoms }
% Constraints = cons{ unit_cost_cents, list_price_cents, min_margin_pct,
%                     max_discount_pct, shipping_cost_pct,
%                     interest_by_term (list of Days-Rate pairs),
%                     batna_cents }
%
% Decision    = accept(final_cents, floor_cents, ceiling_cents)
%             | reject(below_break_even, unit_cost_cents)
%             | counter(target_cents, floor_cents, ceiling_cents, buyer_effective_cents)
%
% The article embeds this file verbatim in the `decide` node of agent.yaml.

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
%   Choose V in [Floor, Ceiling] such that
%     - V is strictly above the buyer's effective offer (we must *move*),
%     - V is a whole-Real number (V mod 100 = 0, i.e. no cents),
%     - V minimises |V - midpoint(Effective, Ceiling)|
%       (i.e. we meet the buyer roughly halfway between their offer and our
%       authorised ceiling — classic concession split).
%
% If no feasible V exists (e.g. buyer already offers the ceiling), we return
% the Floor itself as a take-it-or-leave-it counter.
% ---------------------------------------------------------------------------

counter_target(EffectiveCents, Floor, Ceiling, Target) :-
    EffectiveCents < Ceiling,
    LowerBound is max(Floor, EffectiveCents + 1),
    LowerBound =< Ceiling,
    Midpoint is (EffectiveCents + Ceiling) // 2,
    V in LowerBound..Ceiling,
    V mod 100 #= 0,                        % whole dollars
    Delta #= abs(V - Midpoint),
    labeling([min(Delta)], [V]),
    !,
    Target = V.
counter_target(_, Floor, _, Floor).        % fallback: hold the line at floor

% ---------------------------------------------------------------------------
% Main decision predicate.
% ---------------------------------------------------------------------------

decide(Offer, Constraints, Decision) :-
    % Unpack offer
    get_dict(unit_cents, Offer, OfferCents),
    get_dict(term_days, Offer, TermDays),
    get_dict(demands,   Offer, Demands),

    % Unpack constraints
    get_dict(unit_cost_cents,    Constraints, UnitCost),
    get_dict(list_price_cents,   Constraints, ListPrice),
    get_dict(min_margin_pct,     Constraints, MinMargin),
    get_dict(max_discount_pct,   Constraints, MaxDiscount),
    get_dict(shipping_cost_pct,  Constraints, ShippingPct),
    get_dict(interest_by_term,   Constraints, InterestMap),
    get_dict(batna_cents,        Constraints, Batna),

    % Derive the buyer's effective, present-value offer
    effective_offer_cents(OfferCents, ShippingPct, Demands, AfterShipping),
    apply_term_interest(AfterShipping, TermDays, InterestMap, Effective),

    % Derive bounds
    floor_cents(UnitCost, MinMargin, Batna, Floor),
    ceiling_cents(ListPrice, MaxDiscount, Ceiling),

    % Classify
    ( Effective >= Floor ->
        Decision = accept{ final_cents: Effective,
                           floor_cents: Floor,
                           ceiling_cents: Ceiling }
    ; Effective < UnitCost ->
        Decision = reject{ reason: below_break_even,
                           unit_cost_cents: UnitCost,
                           buyer_effective_cents: Effective }
    ; % Otherwise produce a counter-proposal
        counter_target(Effective, Floor, Ceiling, Target),
        Decision = counter{ target_cents: Target,
                            floor_cents: Floor,
                            ceiling_cents: Ceiling,
                            buyer_effective_cents: Effective }
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
