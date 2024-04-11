:- use_module(library(le_answer)).
:- use_module(traffic_rules).

% :- include('traffic_rules.pl').

fact(is_a(A, B)) :- is_a(A, B).
fact(has_of(A, B, C)) :- has_of(A, B, C).
fact(the_distance_between_and_is(A, B, C)) :- the_distance_between_and_is(A, B, C).
fact(is_approaching(A, B)) :- is_approaching(A, B).
fact(has_a_sign(A, B)) :- has_a_sign(A, B).
fact(has_light(A, B)) :- has_light(A, B).
fact(is_in_the_junction(A)) :- is_in_the_junction(A).

% LE Methods

facts_to_string(String) :-
    fact(A),
    pl_to_string(A, String).

pl_to_string(A, String) :-
    le_answer:translate_goal_into_LE(A,List),
    atomics_to_string(List, ' ', String).

le_input:dictionary(A, B, C) :-
    'traffic_rules-prolog':local_dict(A, B, C).

test_scenario_1 :-
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
        assertion(Scenario == [
            "a has behaviour of good", 
            "a has speed of 0", 
            "the distance between a and the sign is 20.32", 
            "a is approaching a", 
            "b is approaching a", 
            "a has a stop sign"
            ]),
        assertion(not_enter_junction_out(a, 0))
    )).

run_tests :-
    test_scenario_1.
