:-module('traffic_rules-prolog', []).
source_lang(en).
local_dict([must_at, A, B, C], [agent-agent, action-action, location-location], [A, must, B, at, C]).
local_dict([might, A, B], [thing-thing, action-action], [A, might, B]).
local_dict([have, A, B], [agent-agent, action-action], [A, have, B]).
local_dict([has, A, B], [thing-thing, property-property], [A, has, B]).
local_dict([can, A, B], [agent-agent, action-action], [A, can, B]).
local_meta_dict([],[],[]).
prolog_le(verified).

:- dynamic must_at/3.
:- dynamic can_at/3.
:- dynamic has/2.
:- dynamic might/2.
:- dynamic can/2.
:- dynamic must/3.

can_break_rule(A, B) :-
    maybe(0.2),
    must(A, B),
    rule_penalty(B, Penalty),
    random_between(0, 5, Tokens),
    % has_tokens(A, Tokens),
    Tokens >= Penalty.

rule_penalty('stop', 2).

must(A, B) :- must_at(A, B, _, _).
must_at(ego, stop, 'behind the line', A) :-
    not has(A, light, green).
can_at(ego, 'go on', A) :-
    has(A, light, amber),
    has(ego, 'crossed the stop line').
can_at(ego, 'go on', A) :-
    has(A, light, amber),
    might('to stop', 'cause a collision').
% example(null, []).
% example(car, [scenario([(has(junction, 'red light'):-true)], true)]).
% query(null, true).
% query(must, must_at(ego, _, _)).
