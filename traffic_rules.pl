:- module(traffic_rules, [
        % is_a/2,
        % the_distance_between_and_is/3,
        % has_of/3,
        % is_approaching/2,
        % has_a_sign/2,
        % has_light/2,
        % is_in_the_junction/1,
        not_enter_junction_out/2,
        must_not/2,
        behaviour/3,
        distance/3,
        speed/3,
        has_light/3,
        has_sign/3,
        is_of_type/3,
        has_neighbours/3,
        is_in_junction/3,
        violates/2
    ]).

:- style_check(-singleton).
:- style_check(-discontiguous).
:- op(900,fx,user:not). % same as \+

:- use_module('utils/utils.pl').

:- if(current_module(wasm)).
:- use_module('/traffic_rules-prolog.pl', [violates/2, must_not/2]).
:- else.
:- use_module('traffic_rules-prolog', [violates/2, must_not/2]).
:- endif.

:- dynamic behaviour/3.
:- dynamic distance/3.
:- dynamic speed/3.
:- dynamic has_light/3.
:- dynamic has_sign/3.
:- dynamic is_of_type/3.
:- dynamic has_neighbours/3.
:- dynamic is_in_junction/3.

has_neighbour(Self, Neighbour, Time) :-
    has_neighbours(Self, Ns, Time),
    member(Neighbour, Ns).

% The neighbour is outside the if condition since it could exist more than once
% For the previous facts we just need to unify with the first one (semidet)
% Possibly just an issue with the single prolog instance.
% There is no more if condition because everything happens in a snapshot.
% It should(TM) be more efficient
not_enter_junction_out(Self, Time) :-
    must_not(Self, 'enter the junction').

% violates(A, B) :-
% 'traffic_rules-prolog':violates(A, B) :-
%     violates(A, B).
 
% Temporary glue
'traffic_rules-prolog':is_a(A, B) :-
    is_of_type(A, B, _).

'traffic_rules-prolog':has_of(A, behaviour, B) :-
    behaviour(A, B, _).

'traffic_rules-prolog':the_distance_between_and_is(A, 'the sign', B) :-
    distance(A, B, _).

'traffic_rules-prolog':has_of(A, speed, B) :-
    speed(A, B, _).
    
'traffic_rules-prolog':is_approaching(Neighbour, Self) :-
    has_neighbour(Self, Neighbour, _).

'traffic_rules-prolog':has_a_sign(A, B) :-
    has_sign(A, [B], _).

'traffic_rules-prolog':has_light(Self, B) :-
    has_light(Self, [Light], _),
    netlogo:color(Light, B).

'traffic_rules-prolog':is_in_the_junction(A) :-
    is_in_junction(A, InIntersection, _),
    InIntersection is 1.

'traffic_rules-prolog':is_past_the_stop_line(A) :-
    'traffic_rules-prolog':is_in_the_junction(A).