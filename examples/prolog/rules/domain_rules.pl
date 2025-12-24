% ============================================================================
% Domain Rules Library
% ============================================================================
%
% Generic domain logic rules for business/application decision making.
% These rules demonstrate common patterns for neurosymbolic AI applications.
%
% Usage in TEA YAML agents:
%   settings:
%     prolog:
%       consult:
%         - rules/domain_rules.pl
%
% ============================================================================

% ----------------------------------------------------------------------------
% Dynamic predicate declarations
% ----------------------------------------------------------------------------

:- dynamic classification/2.        % classification(Item, Class)
:- dynamic confidence/2.            % confidence(Item, Score)
:- dynamic priority/2.              % priority(Item, Level)
:- dynamic category/2.              % category(Item, Category)
:- dynamic tag/2.                   % tag(Item, Tag)
:- dynamic threshold/2.             % threshold(Name, Value)

% Thread-local for TEA state interface
:- thread_local state/2.
:- thread_local return_value/2.

% ----------------------------------------------------------------------------
% THRESHOLD RULES
% ----------------------------------------------------------------------------

%% high_confidence(+Item)
%  True if Item has confidence above high threshold.
high_confidence(Item) :-
    confidence(Item, Score),
    threshold(high_confidence, Threshold),
    Score >= Threshold.

high_confidence(Item) :-
    confidence(Item, Score),
    \+ threshold(high_confidence, _),
    Score >= 0.8.  % Default threshold

%% medium_confidence(+Item)
%  True if Item has confidence in medium range.
medium_confidence(Item) :-
    confidence(Item, Score),
    Score >= 0.5,
    Score < 0.8.

%% low_confidence(+Item)
%  True if Item has confidence below threshold.
low_confidence(Item) :-
    confidence(Item, Score),
    Score < 0.5.

% ----------------------------------------------------------------------------
% PRIORITY CLASSIFICATION
% ----------------------------------------------------------------------------

%% requires_escalation(+Item, -Reason)
%  Determines if an item requires escalation and why.
requires_escalation(Item, 'High priority with high confidence') :-
    priority(Item, high),
    high_confidence(Item).

requires_escalation(Item, 'Critical classification') :-
    classification(Item, critical).

requires_escalation(Item, 'Multiple high-priority tags') :-
    findall(T, (tag(Item, T), high_priority_tag(T)), Tags),
    length(Tags, N),
    N >= 2.

%% high_priority_tag(+Tag)
%  Tags that indicate high priority.
high_priority_tag(urgent).
high_priority_tag(critical).
high_priority_tag(emergency).
high_priority_tag(security).
high_priority_tag(outage).

%% requires_human_review(+Item, -Reason)
%  Determines if an item requires human review.
requires_human_review(Item, 'Low confidence score') :-
    low_confidence(Item).

requires_human_review(Item, 'Ambiguous classification') :-
    findall(C, classification(Item, C), Classes),
    length(Classes, N),
    N > 1.

requires_human_review(Item, 'Sensitive category') :-
    category(Item, Cat),
    sensitive_category(Cat).

%% sensitive_category(+Category)
%  Categories that require human oversight.
sensitive_category(legal).
sensitive_category(financial).
sensitive_category(medical).
sensitive_category(personal_data).

% ----------------------------------------------------------------------------
% ROUTING RULES
% ----------------------------------------------------------------------------

%% route_to(+Item, -Destination, -Reason)
%  Determines where to route an item and why.
route_to(Item, escalation_queue, Reason) :-
    requires_escalation(Item, Reason), !.

route_to(Item, human_review, Reason) :-
    requires_human_review(Item, Reason), !.

route_to(Item, support_queue, 'Support request identified') :-
    classification(Item, support_request), !.

route_to(Item, sales_queue, 'Sales inquiry identified') :-
    classification(Item, sales_inquiry), !.

route_to(Item, general_queue, 'Standard processing') :-
    true.

% ----------------------------------------------------------------------------
% SLA RULES
% ----------------------------------------------------------------------------

%% sla_target(+Item, -Hours)
%  Determines SLA target in hours.
sla_target(Item, 1) :-
    priority(Item, critical), !.

sla_target(Item, 4) :-
    priority(Item, high), !.

sla_target(Item, 24) :-
    priority(Item, medium), !.

sla_target(Item, 72) :-
    priority(Item, low), !.

sla_target(_, 48) :-
    true.  % Default SLA

% ----------------------------------------------------------------------------
% AGGREGATION RULES
% ----------------------------------------------------------------------------

%% count_by_classification(-Counts)
%  Count items by classification.
count_by_classification(Counts) :-
    findall(Class-Count,
        (
            setof(Item, classification(Item, Class), Items),
            length(Items, Count)
        ),
        Counts).

%% count_by_priority(-Counts)
%  Count items by priority.
count_by_priority(Counts) :-
    findall(Level-Count,
        (
            setof(Item, priority(Item, Level), Items),
            length(Items, Count)
        ),
        Counts).

% ----------------------------------------------------------------------------
% DECISION HELPERS
% ----------------------------------------------------------------------------

%% make_decision(+Item, -Decision)
%  Make a routing decision for an item.
make_decision(Item, decision(Destination, Reason, SLA)) :-
    route_to(Item, Destination, Reason),
    sla_target(Item, SLA).

%% apply_rule(+RuleName, +Item, -Result)
%  Apply a named rule to an item.
apply_rule(escalation, Item, Result) :-
    (requires_escalation(Item, Reason)
    -> Result = required(Reason)
    ; Result = not_required).

apply_rule(human_review, Item, Result) :-
    (requires_human_review(Item, Reason)
    -> Result = required(Reason)
    ; Result = not_required).

apply_rule(routing, Item, Result) :-
    route_to(Item, Dest, Reason),
    Result = route(Dest, Reason).

% ----------------------------------------------------------------------------
% TEA STATE INTERFACE
% ----------------------------------------------------------------------------

%% return(+Key, +Value)
%  Helper for TEA - asserts return_value/2 fact.
return(Key, Value) :-
    assertz(return_value(Key, Value)).

%% load_thresholds_from_state
%  Load threshold configuration from TEA state.
load_thresholds_from_state :-
    (state(thresholds, Thresholds) ->
        load_thresholds(Thresholds)
    ; true).

load_thresholds([]).
load_thresholds([Name-Value|Rest]) :-
    assertz(threshold(Name, Value)),
    load_thresholds(Rest).

% ============================================================================
% EXAMPLE USAGE
% ============================================================================
%
% % Load item data
% assertz(classification(ticket_123, support_request)).
% assertz(confidence(ticket_123, 0.92)).
% assertz(priority(ticket_123, high)).
%
% % Query routing
% ?- route_to(ticket_123, Dest, Reason).
% Dest = support_queue,
% Reason = 'Support request identified'.
%
% % Check escalation
% ?- requires_escalation(ticket_123, Why).
% Why = 'High priority with high confidence'.
%
% % Get full decision
% ?- make_decision(ticket_123, D).
% D = decision(escalation_queue, 'High priority with high confidence', 4).
% ============================================================================
