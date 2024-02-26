:-module('av_rules-prolog', []).
source_lang(en).
local_dict([the_distance_between_and_is, A, B, C], [vehicle-vehicle, sign-sign, distance-distance], [the, distance, between, A, and, B, is, C]).
local_dict([the_speed_of_is, A, B], [vehicle-vehicle, amount-amount], [the, speed, of, A, is, B]).
local_dict([must_give_way_to, A, B], [vehicle-vehicle, other_vehicle-other_vehicle], [A, must, give, way, to, B]).
local_dict([is_in_the_junction, A], [vehicle-vehicle], [A, is, in, the, junction]).
local_dict([has_a_sign, A, B], [vehicle-vehicle, sign-sign], [A, has, a, B, sign]).
local_dict([can_enter_the_junction, A], [vehicle-vehicle], [A, can, enter, the, junction]).
local_dict([is_a, A, B], [vehicle-vehicle, type-type], [A, is, a, B]).
local_dict([has_light, A, B], [vehicle-vehicle, color-color], [A, has, B, light]).
local_dict([must, A, B], [vehicle-vehicle, action-action], [A, must, B]).
local_dict([is_stopped, A], [vehicle-vehicle], [A, is, stopped]).
local_meta_dict([],[],[]).
prolog_le(verified).
can_enter_the_junction(A) :-
    is_a(A, ambulance),
    not must_give_way_to(A, _).
can_enter_the_junction(A) :-
    has_light(A, green),
    not must_give_way_to(A, _).
can_enter_the_junction(A) :-
    has_light(A, yellow),
    is_in_the_junction(A),
    not must_give_way_to(A, _).
can_enter_the_junction(A) :-
    has_a_sign(A, stop),
    not must_give_way_to(A, _).
A must stop :-
    has_a_sign(A, stop),
    the_distance_between_and_is(A, 'the sign', B),
    B<1,
    not is_stopped(A).
A must 'give way' :-
    has_a_sign(A, 'give way').
must_give_way_to(_, A) :-
    is_a(A, ambulance).
must_give_way_to(_, A) :-
    is_a(A, pedestrian).
must_give_way_to(A, B) :-
    has_a_sign(A, stop),
    B=approaching.
is_stopped(A) :-
    the_speed_of_is(A, 0).
example(null, []).
example(ambulance, [scenario([(is_a(123, ambulance):-true)], true)]).
example(car, [scenario([(is_a(456, car):-true), (has_light(456, green):-true), (is_a(123, ambulance):-true)], true)]).
example(car_stop, [scenario([(is_a(234, car):-true), (has_a_sign(234, stop):-true)], true)]).
query(null, true).
query(enter, can_enter_the_junction(_)).
