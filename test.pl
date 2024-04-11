
:- use_module(library(plunit)).

% :- use_module(traffic_rules).
:- use_module(library(le_answer)).

:- set_test_options([silent(false), format(log), cleanup(true)]).

:- begin_tests(scenarios).

% Re-import modules in the test environment
:- use_module(traffic_rules).
% :- use_module(library(le_answer)).

setup :-
    read_file_to_string('traffic_rules.le', String, []),
    parse_and_query_and_explanation('traffic_rules', en(String), null, with(null), R).

fact(is_a(A, B)) :- 'traffic_rules-prolog':is_a(A, B).
fact(has_of(A, B, C)) :- 'traffic_rules-prolog':has_of(A, B, C).
fact(the_distance_between_and_is(A, B, C)) :- 'traffic_rules-prolog':the_distance_between_and_is(A, B, C).
fact(is_approaching(A, B)) :- 'traffic_rules-prolog':is_approaching(A, B).
fact(has_a_sign(A, B)) :- 'traffic_rules-prolog':has_a_sign(A, B).
fact(has_light(A, B)) :- 'traffic_rules-prolog':has_light(A, B).
fact(is_in_the_junction(A)) :- 'traffic_rules-prolog':is_in_the_junction(A).

% LE Methods

facts_to_string(String) :-
    fact(A),
    pl_to_string(A, String),
    writeln(String).

pl_to_string(A, String) :-
    le_answer:translate_goal_into_LE(A,List),
    atomics_to_string(List, ' ', String).

le_input:dictionary(A, B, C) :-
    'traffic_rules-prolog':local_dict(A, B, C).

not_enter_junction_out(A, B, true) :-
    traffic_rules:not_enter_junction_out(a, 0).
not_enter_junction_out(A, B, fail) :-
    \+ traffic_rules:not_enter_junction_out(a, 0).

test(stop, [
    R == true,
    Scenario == ["a has behaviour of good", "a has speed of 0", "the distance between a and the sign is 20.32", "a is approaching a", "b is approaching a", "a has a stop sign"]
    ]) :-
    snapshot((
    asserta(behaviour(a, good, 0)),
    asserta(distance(a, 20.32, 0)),
    asserta(speed(a, 0, 0)),
    asserta(has_light(a, [], 0)),
    asserta(has_sign(a, [stop], 0)),
    asserta(is_of_type(a, car, 0)),
    asserta(has_neighbours(a, [a,b], 0)),
    asserta(is_in_junction(a, 0, 0)),
    findall(A, facts_to_string(A), Scenario),
    not_enter_junction_out(a, 0, R)
    % assertion(traffic_rules:not_enter_junction_out(a, 0))
    )).

test(light, [
    R == true,
    Scenario == ["a has behaviour of good", "a has speed of 0", "the distance between a and the sign is 20.32", "a has red light"]
    ]) :-
    snapshot((
    asserta(behaviour(a, good, 0)),
    asserta(distance(a, 20.32, 0)),
    asserta(speed(a, 0, 0)),
    asserta(has_light(a, [15], 0)),
    asserta(has_sign(a, [], 0)),
    asserta(is_of_type(a, car, 0)),
    asserta(has_neighbours(a, [], 0)),
    asserta(is_in_junction(a, 0, 0)),
    findall(A, facts_to_string(A), Scenario),
    not_enter_junction_out(a, 0, R)
    % assertion(traffic_rules:not_enter_junction_out(a, 0))
    )).

test(light_green, [
    R == fail,
    Scenario == ["a has behaviour of good", "a has speed of 0", "the distance between a and the sign is 20.32", "a has green light"]
    ]) :-
    snapshot((
    asserta(behaviour(a, good, 0)),
    asserta(distance(a, 20.32, 0)),
    asserta(speed(a, 0, 0)),
    asserta(has_light(a, [55], 0)),
    asserta(has_sign(a, [], 0)),
    asserta(is_of_type(a, car, 0)),
    asserta(has_neighbours(a, [], 0)),
    asserta(is_in_junction(a, 0, 0)),
    findall(A, facts_to_string(A), Scenario),
    not_enter_junction_out(a, 0, R)
    )).

:- end_tests(scenarios).
