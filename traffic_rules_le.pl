:-style_check(-singleton).
:-style_check(-discontiguous).

:- include('netlogo_glue.pl').

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
    % Static vehicle properties
    % Could be defined once on vehicle spawn
    is_of_type(Self, CarType, Time),
    behaviour(Self, Behaviour, Time),
    % Dynamic state of the world
    % This section contains the facts to assert in the snapshot
    distance(Self, Distance, Time),
    speed(Self, Speed, Time),
    has_light(Self, LightNum, Time),
    has_sign(Self, Sign, Time),
    is_in_junction(Self, InIntersection, Time),
    has_neighbour(Self, Neighbours, Time),
    must_not_enter_the_junction(Self).
    % -> cleanup(a); cleanup(a),false.

% Temporary glue
is_a(A, B) :-
    is_of_type(A, B, _).

has_of(A, behaviour, B) :-
    behaviour(A, B, _).

the_distance_between_and_is(A, 'the sign', B) :-
    distance(A, B, _).

has_of(A, speed, B) :-
    speed(A, B).
    
is_approaching(Self, Neighbour) :-
    has_neighbour(Self, Neighbour, _).

has_a_sign(A, B) :-
    has_sign(A, B, _).

has_light(A, B) :-
    has_light(Self, [Light], _),
    color(Light, B).

is_in_the_junction(A) :-
    is_in_junction(A, InIntersection, _),
    InIntersection is 1.

% LE generated

must_not_enter_the_junction(A) :-
    has_of(A, behaviour, good),
    \+ can_enter_the_junction(A).

must_give_way_to(A, B) :-
    is_approaching(B, A),
    is_a(B, ambulance).

must_give_way_to(A, B) :-
    is_approaching(B, A),
    is_a(B, pedestrian).

must_give_way_to(A, B) :-
    has_a_sign(A, stop),
    is_approaching(B, A).

can_enter_the_junction(A) :-
    is_a(A, ambulance),
    \+ must_give_way_to(A, _).

can_enter_the_junction(A) :-
    has_light(A, green),
    \+ must_give_way_to(A, _).

can_enter_the_junction(A) :-
    has_light(A, yellow),
    is_in_the_junction(A),
    \+ must_give_way_to(A, _).

is_approaching(A, B) :-
    has_of(A, direction, C),
    has_of(B, direction, D),
    C\=D.

% A must stop :-
%     has_a_sign(A, stop),
%     the_distance_between_and_is(A, 'the sign', B),
%     B<1,
%     \+ is_stopped(A).

must_give_way_to(A, _) :-
    has_a_sign(A, 'give way').

is_stopped(A) :-
    has_of(A, speed, 0).