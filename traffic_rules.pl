:-style_check(-singleton).
:-style_check(-discontiguous).
:- op(900,fx,user:not). % same as \+

:- include('netlogo_glue.pl').
% :- include('traffic_rules-prolog.pl').
:- use_module('traffic_rules-prolog.pl').

:- dynamic behaviour/2.
:- dynamic distance/2.
:- dynamic speed/2.
:- dynamic has_light/2. 
:- dynamic has_sign/2.
:- dynamic is_of_type/2.
:- dynamic has_neighbours/2.
:- dynamic is_in_junction/2.

% Add \+ give_way to enter_junction.
% Keep all parameters in arguments or assert facts?
% Logo deals with the "sensor" part of the simulation,
% such as avoiding other cars.
% The Prolog rules determine which actions can be executed
% in "normal" scenarios (no accidents involved).

% Neighbours are all the agents in a 3 block range from self
% who are moving in a different direction.
% the coordinates in give_way are used to filter out
% the agents behind self.

% Possibiity or prohibition to enter the intersection.
% One represented as the opposite of the other, but
% logically the possibility is given by the sensors:
%   - there is no impediment on the road
% the prohibition is given by the norms:
%   - there is something that prohibits this action, even
%     though it is still possible.
% In the second case there is a violation.

has_neighbour(Self, Neighbour, Time) :-
    has_neighbours(Self, Ns, Time),
    member(Neighbour, Ns).

% The neighbour is outside the if condition since it could exist more than once
% For the previous facts we just need to unify with the first one (semidet)
% Possibly just an issue with the single prolog instance.
% There is no more if condition because everything happens in a snapshot.
% It should(TM) be more efficient
not_enter_junction_out(Self, Time) :-
    % must_not_enter_the_junction(Self).
    'traffic_rules-prolog':must_not(Self, 'enter the junction') .

violates(A, B) :-
    'traffic_rules-prolog':violates(A, B) .

% Temporary glue
is_a(A, B) :-
    is_of_type(A, B, _).

has_of(A, behaviour, B) :-
    behaviour(A, B, _).

the_distance_between_and_is(A, 'the sign', B) :-
    distance(A, B, _).

has_of(A, speed, B) :-
    speed(A, B, _).
    
is_approaching(Neighbour, Self) :-
    has_neighbour(Self, Neighbour, _).

has_a_sign(A, B) :-
    has_sign(A, [B], _).

has_light(A, B) :-
    has_light(Self, [Light], _),
    color(Light, B).

is_in_the_junction(A) :-
    is_in_junction(A, InIntersection, _),
    InIntersection is 1.