:-module('traffic_rules-prolog', []).
source_lang(en).
local_dict([the_distance_between_and_is, A, B, C], [vehicle-vehicle, sign-sign, distance-distance], [the, distance, between, A, and, B, is, C]).
local_dict([must_give_way_to, A, B], [vehicle-vehicle, vehicle-vehicle], [A, must, give, way, to, B]).
local_dict([the_speed_limit_is, A], [limit-limit], [the, speed, limit, is, A]).
local_dict([the_allowed_excess_is, A], [value-value], [the, allowed, excess, is, A]).
local_dict([is_in_the_junction, A], [vehicle-vehicle], [A, is, in, the, junction]).
local_dict([has_a_sign, A, B], [vehicle-vehicle, sign-sign], [A, has, a, B, sign]).
local_dict([has_of, A, B, C], [vehicle-vehicle, property-property, value-value], [A, has, B, of, C]).
local_dict([must_not, A, B], [vehicle-vehicle, action-action], [A, must, not, B]).
local_dict([is_approaching, A, B], [vehicle-vehicle, vehicle-vehicle], [A, is, approaching, B]).
local_dict([has_light, A, B], [vehicle-vehicle, color-color], [A, has, B, light]).
local_dict([violates, A, B], [vehicle-vehicle, rule-rule], [A, violates, B]).
local_dict([must, A, B], [vehicle-vehicle, action-action], [A, must, B]).
local_dict([is_stopped, A], [vehicle-vehicle], [A, is, stopped]).
local_dict([can, A, B], [vehicle-vehicle, action-action], [A, can, B]).
local_meta_dict([],[],[]).
prolog_le(verified).
must_not(A, 'enter the junction') :-
    has_of(A, behaviour, good),
    not can(A, 'enter the junction').
can(A, 'enter the junction') :-
    is_a(A, ambulance),
    not must_give_way_to(A, _).
can(A, 'enter the junction') :-
    has_light(A, green),
    not must_give_way_to(A, _).
can(A, 'enter the junction') :-
    has_light(A, yellow),
    is_in_the_junction(A),
    not must_give_way_to(A, _).
can(A, 'enter the junction') :-
    has_a_sign(A, stop),
    is_stopped(A),
    not must_give_way_to(A, _).
must_give_way_to(A, B) :-
    is_approaching(B, A),
    is_a(B, ambulance).
must_give_way_to(A, B) :-
    is_approaching(B, A),
    is_a(B, pedestrian).
must_give_way_to(A, B) :-
    has_a_sign(A, stop),
    is_approaching(B, A).
is_stopped(A) :-
    has_of(A, speed, 0).
violates(A, 'entering the junction') :-
    has_light(A, red),
    not is_a(A, ambulance),
    not is_stopped(A).
violates(A, speeding) :-
    has_of(A, speed, B),
    the_speed_limit_is(C),
    the_allowed_excess_is(D),
    B>C+D,
    not is_a(A, ambulance).
example(null, []).
example(ambulance, [scenario([(is_a(123, ambulance):-true)], true)]).
example(car, [scenario([(is_a(456, car):-true), (has_light(456, green):-true), (is_a(123, ambulance):-true), (is_approaching(123, 456):-true)], true)]).
example(car_stop, [scenario([(is_a(234, car):-true), (has_a_sign(234, stop):-true)], true)]).
example(monitor, [scenario([(is_a(456, car):-true), (has_light(456, red):-true)], true)]).
example(monitor_2, [scenario([(is_a(456, ambulance):-true), (has_light(456, red):-true)], true)]).
example(speeding, [scenario([(is_a(123, car):-true), (has_of(123, speed, 15):-true), (the_speed_limit_is(10):-true), (the_allowed_excess_is(3):-true)], true)]).
query(null, true).
query(enter, can(_, 'enter the junction')).
query(violation, violates(_, _)).
