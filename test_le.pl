
:- use_module(library(plunit)).
% Re-import modules in the test environment
% :- use_module(library(le_answer)).
:- use_module('logicalenglish/prolog/le_answer.pl').
:- set_test_options([silent(false), format(log)]).

:- begin_tests(le).

test(le) :-
    snapshot((
        read_file_to_string('traffic_rules.le', String, []),
        parse_and_query_and_explanation('traffic_rules', en(String), violation, with(monitor), R)
    )).

:- end_tests(le).
