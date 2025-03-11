:- use_module('logicalenglish/prolog/le_answer.pl').
% :- use_module('scenario.pl').
:- set_prolog_flag(color_term, false).

:- initialization(main, main).

main :-
    protocol('log.txt'),
    read_file_to_string('traffic_rules_llama.le', String, []),
    parse_and_query('traffic_rules_llama', en(String), null, with(null), _).
    % run_scenario(obligation, stop).

% % run_scenarios
% %     consult('scenario.pl'),
% %     findall(S, run_scenarios(S), R).

% % find_all(Keyword, L) :-
% %     find(Keyword, S, L).

% find(Text, R, L) :-
%     member(X, L),
%     atom_concat(Text, Y, X),
%     atom_concat(Test, "is:", Y),
%     split_string(Test, "", "\s\t\n", [R]).