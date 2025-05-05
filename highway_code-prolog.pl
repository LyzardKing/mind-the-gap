:-module('highway_code-prolog', []).
source_lang(en).
local_dict([should_to, A, B, C], [agent-agent, action-action, agent-agent], [A, should, B, to, C]).
local_dict([must_to, A, B, C], [agent-agent, action-action, agent-agent], [A, must, B, to, C]).
local_dict([is_in, A, B], [agent-agent, place-place], [A, is, in, B]).
local_dict([is_at, A, B], [agent-agent, place-place], [A, is, at, B]).
local_dict([should, A, B], [agent-agent, action-action], [A, should, B]).
local_dict([sees, A, B], [agent-agent, item-item], [A, sees, B]).
local_dict([must, A, B], [agent-agent, action-action], [A, must, B]).
local_dict([cannot, A, B], [agent-agent, action-action], [A, cannot, B]).
local_dict([can, A, B], [agent-agent, action-action], [A, can, B]).
local_dict(['_is', A, B], [agent-agent, place-place], [A, '_is', B]).
local_meta_dict([],[],[]).
prolog_le(verified).
A must B :-
    must_to(A, B, _).
should(A, B) :-
    should_to(A, B, _).
should_to(A, 'give way', B) :-
    is_at(A, _),
    (   '_is'(B, crossing)
    ;   '_is'(B, 'waiting to cross')
    ).
A must 'stop behind the line at a junction' :-
    is_at(A, 'the junction'),
    sees(A, 'stop sign'),
    sees(A, 'solid white line across the road').
must_to(A, 'give way', traffic) :-
    (   sees(A, 'give way sign')
    ;   sees(A, 'triangle marked on the road')
    ;   sees(A, 'broken white lines across the road')
    ),
    sees(A, 'traffic oncoming').
can(A, 'enter a junction') :-
    sees(A, 'stop line'),
    sees(A, 'green light').
can(A, 'enter a junction') :-
    sees(A, 'stop line'),
    sees(A, 'amber light'),
    (   is_in(A, 'the junction')
    ;   cannot(A, 'stop safely')
    ).
example(null, []).
example(give_way, [scenario([(is_at(car1, junction1):-true), (sees(car1, 'give way sign'):-true), ('_is'(person, crossing):-true), (is_at(car2, junction1):-true), (sees(car2, 'give way sign'):-true)], true)]).
example(stop_behind_line, [scenario([(is_at(car1, junction1):-true), (sees(car1, 'stop sign'):-true), (sees(car1, 'solid white line across the road'):-true)], true)]).
query(null, true).
query(must_stop, (_ must 'stop behind the line at which junction')).
query(must, (_ must _)).
query(should, should(_, _)).
