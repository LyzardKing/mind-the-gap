:- op(900,fx,user:not). % same as \+
:- use_module('traffic_rules-prolog').
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

can(A, B) :-
    'traffic_rules-prolog':can(A, B).

is_a(456, car).
has_light(456, green).
is_a(123, ambulance).
is_approaching(123, 456).

% ?- once(can(123, 'enter the junction')).
% ?- can(456, 'enter the junction').