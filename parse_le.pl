:- use_module(library(le_answer)).

% :- initialization(main, main).

main :-
    read_file_to_string('traffic_rules.le', String, []),
    % string_lines(String, L),
    % find('scenario 'S, L),
    % find('query ', Q, L),
    parse_and_query_and_explanation('traffic_rules', en(String), null, with(null), _).

find(Text, S, L) :-
    member(X, L),
    atom_concat(Text, Y, X),
    atom_concat(Test, "is:", Y),
    !.