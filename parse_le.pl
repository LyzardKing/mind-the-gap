:- use_module(library(le_answer)).

:- initialization(main, main).

main :-
    read_file_to_string('traffic_rules.le', S, []),
    parse_and_query_and_explanation('traffic_rules', en(S), enter, with(car), _).
