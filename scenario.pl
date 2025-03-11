:- module(scenario_runner, [run_scenario/2]).

:- op(900,fx,user:not). % same as \+
:- use_module('traffic_rules-prolog', [example/2, can/2]).
:- use_module(library(apply_macros)).

% :- set_prolog_flag(unknown,warning).

:- dynamic is_a/2.
:- dynamic has_light/2.
:- dynamic has_a_sign/2.
:- dynamic is_approaching/2.
:- dynamic is_in_junction/2.
:- dynamic has_of/3.
:- dynamic is_in_the_junction/1.
:- dynamic the_allowed_excess_is/1.
:- dynamic the_speed_limit_is/1.

:-style_check(-singleton).
:-style_check(-discontiguous).

% can(A, B) :-
%     'traffic_rules-prolog':can(A, B).

run_scenario(Scenario, Query) :-
    example(Scenario, [scenario(Facts, true)]),
    forall(member(X, Facts), assert(X)),
    once(can(_, Query)).
