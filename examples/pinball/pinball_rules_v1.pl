%% =============================================================================
%% Video Pinball AI Rules - Version 1
%% =============================================================================
%%
%% This file contains the initial symbolic rules for playing Video Pinball.
%% These rules encode a "worldview" - our understanding of pinball physics
%% and strategy. The LLM will analyze game performance and improve these rules.
%%
%% WORLDVIEW COMPONENTS:
%% 1. Physics: How the ball moves (gravity, bouncing, trajectories)
%% 2. Geometry: Where things are (flipper zones, bumper positions)
%% 3. Timing: When to act (flipper activation windows)
%% 4. Strategy: What to prioritize (targets, combos, survival)
%%
%% =============================================================================

%% -----------------------------------------------------------------------------
%% PHYSICAL CONSTANTS
%% These model the pinball physics in the Atari game
%% -----------------------------------------------------------------------------

% Screen dimensions (Atari 2600 resolution)
screen_width(160).
screen_height(210).

% Gravity effect (pixels per frame squared, approximately)
gravity(0.5).

% Flipper zones - where flippers can affect the ball
flipper_zone(left, 20, 60, 180, 200).   % x_min, x_max, y_min, y_max
flipper_zone(right, 100, 140, 180, 200).

% Flipper response time (frames)
flipper_activation_frames(3).

% Ball speed thresholds
slow_ball_speed(3).
medium_ball_speed(6).
fast_ball_speed(10).

%% -----------------------------------------------------------------------------
%% GEOMETRIC PREDICATES
%% Spatial reasoning about game objects
%% -----------------------------------------------------------------------------

% Check if ball is in a flipper's zone
ball_in_flipper_zone(Side) :-
    ball_position(BX, BY),
    flipper_zone(Side, XMin, XMax, YMin, YMax),
    BX >= XMin, BX =< XMax,
    BY >= YMin, BY =< YMax.

% Check if ball is approaching flipper (moving downward)
ball_approaching_flipper(Side) :-
    ball_in_flipper_zone(Side),
    ball_velocity(_, VY),
    VY > 0.  % Positive VY means moving down in screen coords

% Ball is in upper playfield (opportunity for aiming)
ball_in_upper_playfield :-
    ball_position(_, BY),
    BY < 100.

% Ball is in danger zone (close to drain)
ball_in_danger_zone :-
    ball_position(_, BY),
    BY > 195.

% Ball moving toward left side
ball_moving_left :-
    ball_velocity(VX, _),
    VX < -1.

% Ball moving toward right side
ball_moving_right :-
    ball_velocity(VX, _),
    VX > 1.

%% -----------------------------------------------------------------------------
%% TIMING PREDICATES
%% When to activate flippers for optimal contact
%% -----------------------------------------------------------------------------

% Estimate frames until ball reaches flipper height
frames_to_flipper(Frames) :-
    ball_position(_, BY),
    ball_velocity(_, VY),
    flipper_zone(_, _, _, YTarget, _),
    Distance is YTarget - BY,
    VY > 0,
    Frames is Distance / VY.

% Ball is at optimal flip timing
optimal_flip_timing :-
    frames_to_flipper(F),
    flipper_activation_frames(Activation),
    F =< Activation.

% Ball is too early to flip (would miss)
too_early_to_flip :-
    frames_to_flipper(F),
    F > 10.

% Ball is too late to flip (already past)
too_late_to_flip :-
    ball_position(_, BY),
    BY > 200.

%% -----------------------------------------------------------------------------
%% VELOCITY ANALYSIS
%% Understanding ball speed for strategy adjustment
%% -----------------------------------------------------------------------------

ball_speed(Speed) :-
    ball_velocity(VX, VY),
    Speed is sqrt(VX * VX + VY * VY).

ball_is_slow :-
    ball_speed(S),
    slow_ball_speed(Threshold),
    S =< Threshold.

ball_is_fast :-
    ball_speed(S),
    fast_ball_speed(Threshold),
    S >= Threshold.

% Fast balls need earlier timing
timing_adjustment(Early) :-
    ball_is_fast,
    Early is 2.  % Flip 2 frames earlier

timing_adjustment(0) :-
    \+ ball_is_fast.

%% -----------------------------------------------------------------------------
%% ACTION RULES
%% The core decision logic - which action to take
%% -----------------------------------------------------------------------------

% Rule 1: Flip left when ball approaches left flipper at right time
optimal_action(flip_left) :-
    ball_approaching_flipper(left),
    optimal_flip_timing,
    !.

% Rule 2: Flip right when ball approaches right flipper at right time
optimal_action(flip_right) :-
    ball_approaching_flipper(right),
    optimal_flip_timing,
    !.

% Rule 3: Emergency flip - ball in danger zone, try both
optimal_action(flip_both) :-
    ball_in_danger_zone,
    !.

% Rule 4: Launch ball when not visible (start of game/ball)
optimal_action(launch) :-
    ball_not_visible,
    !.

% Rule 5: Default - wait
optimal_action(wait) :-
    true.

%% -----------------------------------------------------------------------------
%% STRATEGY RULES (for future enhancement)
%% Higher-level decision making
%% -----------------------------------------------------------------------------

% Identify high-value targets
% (To be learned from gameplay analysis)
high_value_target(bumper, 0).

% Should we aim for a specific target?
should_aim_for_target(Target) :-
    high_value_target(Target, _),
    ball_in_upper_playfield,
    can_reach_target(Target).

% Placeholder - trajectory calculation
can_reach_target(_) :-
    ball_in_upper_playfield.

%% -----------------------------------------------------------------------------
%% SAFETY RULES
%% Avoid losing the ball
%% -----------------------------------------------------------------------------

% Never flip too early (wastes the opportunity)
safe_to_flip :-
    \+ too_early_to_flip.

% Consider nudging when ball is stuck
consider_nudge :-
    ball_velocity(VX, VY),
    abs(VX) < 0.5,
    abs(VY) < 0.5,
    ball_position(_, BY),
    BY > 50.  % Not at plunger

%% -----------------------------------------------------------------------------
%% META-RULES
%% Rules about rules - for self-improvement
%% -----------------------------------------------------------------------------

% Track which rules fired for analysis
:- dynamic rule_trace/3.  % rule_trace(Frame, Rule, Action)

record_rule(Rule, Action) :-
    frame(F),
    assertz(rule_trace(F, Rule, Action)).

% Get all traces for export
get_traces(Traces) :-
    findall(trace(F, R, A), rule_trace(F, R, A), Traces).

%% -----------------------------------------------------------------------------
%% VERSION HISTORY
%% -----------------------------------------------------------------------------
%%
%% v1.0 - Initial rules
%%   - Basic flipper activation based on ball position
%%   - Simple timing heuristics
%%   - Emergency flip for danger zone
%%
%% Future improvements (to be learned):
%%   - [ ] Velocity-adjusted timing
%%   - [ ] Target prioritization
%%   - [ ] Combo detection
%%   - [ ] Multiball handling
%%
%% =============================================================================
