% :- use_module(library(le_answer)).

:- initialization(main, main).

main :-
    consult('logicalenglish/prolog/le_answer.pl'),
    read_file_to_string('traffic_rules.le', String, []),
    once(parse_and_query('traffic_rules', en(String), enter, with(car), _)).

% find(Text, S, L) :-
%     member(X, L),
%     atom_concat(Text, Y, X),
%     atom_concat(Test, "is:", Y),
%     !.