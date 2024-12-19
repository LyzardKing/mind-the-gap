:- module(le, []).

% LE Methods

pl_to_le([], []).
pl_to_le([Fact|Rest], [LEFact|LERest]) :-
    le_answer:translate_goal_into_LE(A,List),
    atomics_to_string(List, ' ', LEFact).

query_log(ID, Facts, Query) :-
    pl_to_le(Facts, LEFacts).