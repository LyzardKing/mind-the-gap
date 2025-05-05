% :-module('highway_code-prolog', []).

% check_dialect(Dialect) :-
%     catch(current_prolog_flag(dialect, Dialect), _, fail).

% :- if(check_dialect(swi)).
:- use_module(library(apply_macros)).
:- use_module(library(random)).
% %:- endif.

:- style_check(-discontiguous).
% :- set_prolog_flag(unknown, fail).

% remove annoying ellipsis behavior
:- set_prolog_flag(answer_write_options,
		   [	quoted(true),
			portray(true),
			attributes(portray)
		   ]).
:- set_prolog_flag(debugger_write_options,
		   [	quoted(true),
			portray(true),
			attributes(portray)
		   ]).
% :- endif.

% Facts

% TODO: add possibility of not being detected. What happens to the behaviour?
% Rules
% Add minimum safety conditions
% can_break_rule(A, _) :-
%     type(A, emergency),
%     status(A, emergency), !.
can_break_rule(A, B) :-
    has_behaviour(A, Behaviour),
    maybe(Behaviour),
    must(A, B),
    rule_penalty(B, Penalty),
    has_tokens(A, Tokens),
    Tokens >= Penalty,
    % Here I decided to risk it since I have enough tokens
    NewTokens is Tokens - Penalty,
    set_tokens(A, NewTokens),
    NewBehaviour is Behaviour - (Penalty/Tokens) * Behaviour,
    set_behaviour(A, NewBehaviour).

% Set the behaviour of the vehicle based on the current status
set_behaviour(A) :-
    has_behaviour(A, Value),
    % type(A, Type),
    (status(A, emergency) ->
        NewValue is Value + 0.5
    ;   NewValue is Value
    ),
    set_behaviour(ego, NewValue).

has_behaviour(ego, Value) :-
    % b_getval(behaviour, Value).
    behaviour(ego, Value).

set_behaviour(A, Value) :-
    % b_setval(behaviour, Value).
    (call(behaviour(A, _)) ->
    retract(behaviour(A, _))),
    asserta(behaviour(A, Value)).

has_tokens(ego, Value) :-
    % b_getval(tokens, Value).
    tokens(ego, Value).

set_tokens(A, Value) :-
    % b_setval(tokens, Value).
    (call(tokens(A, _)) ->
    retract(tokens(A, _))),
    asserta(tokens(A, Value)).

rule_penalty('stop behind the line at a junction', 2).
rule_penalty('give way', 1).

must(A, B) :-
    must_to(A, B, _).
should(A, B) :-
    should_to(A, B, _).
should_to(A, 'give way', B) :-
    is_at(A, _),
    (   '_is'(B, crossing)
    ;   '_is'(B, 'waiting to cross')
    ).
must(A, 'stop behind the line at a junction') :-
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


run_check(Goal, Beliefs) :-
    forall(member(X, Beliefs), asserta(X)),
    (
        can_break_rule(ego, Goal) -> 
            % b_getval(tokens, NewTokens),
            % b_getval(behaviour, NewBehaviour),
            has_tokens(ego, NewTokens),
            has_behaviour(ego, NewBehaviour),
            format('Rule broken: ~w.~nRemaining tokens: ~w.~nNew behaviour: ~w~n', [Goal, NewTokens, NewBehaviour])
            ;
            writeln('No rule broken')
    ),
    forall(member(X, Beliefs), retract(X)), nl.

run_event(Goal, Beliefs) :-
    forall(member(X, Beliefs), asserta(X)),
    can_break_rule(ego, Goal),
    forall(member(X, Beliefs), retract(X)).

% Add the detection possibilty of a rule being broken. The vehicle can believe that the action is fully or partially mitigated.
% This has an impact on the decision making, and is not tied to the actual detection. Spawn another program that detects the violation and decides on a possible penalty.

events(Events, Results) :-
    % b_setval(tokens, 10),
    % b_setval(behaviour, 0.5),
    events_acc(Events, [], Results).

events_acc([], Acc, Acc) :- !.
events_acc([H|T], Acc, Results) :-
    run_event(_, H),
    % b_getval(tokens, Tokens),
    % b_getval(behaviour, Behaviour),
    has_tokens(ego, Tokens),
    has_behaviour(ego, Behaviour),
    % events_acc(T, [[H, Tokens, Behaviour] | Acc], Results), !.
    append(Acc, [[H, Tokens, Behaviour]], NewAcc),
    events_acc(T, NewAcc, Results), !.
events_acc([_|T], Acc, Results) :-
    (bonus_points(Acc); true),
    % events_acc(T, [[] | Acc], Results).
    append(Acc, [[]], NewAcc),
    events_acc(T, NewAcc, Results).

bonus_points(Acc) :-
    % last(Acc, [A,B]),
    % A = [],
    % B = [],
    reverse(Acc, [[],[]|_]),
    % b_getval(tokens, Tokens),
    has_tokens(ego, Tokens),
    Tokens < 10,
    NewTokens is Tokens + 1,
    % b_setval(tokens, NewTokens).
    set_tokens(ego, NewTokens).
    
run_events(R) :-
    run_events(R, _, _).

run_events(R, Type, Status) :-
    % b_setval(tokens, 10),
    % b_setval(behaviour, 0.2),
    asserta(tokens(ego, 10)),
    asserta(behaviour(ego, 0.2)),
    % Declare vehicle type (car, emergency)
    (var(Type) -> 
        (maybe(0.2) -> asserta(type(ego, emergency)); asserta(type(ego, car)))
        ;   asserta(type(ego, Type))),
    % Declare vehicle status (normal, emergency)
    (var(Status) -> 
        (maybe(0.2) -> asserta(status(ego, emergency)); asserta(status(ego, normal)))
        ;   asserta(status(ego, Status))),
    % (maybe(0.2) -> asserta(status(ego, emergency)); asserta(status(ego, normal))),
    set_behaviour(ego),
    type(ego, Type),
    status(ego, Status),
    format('Vehicle type: ~w.~nVehicle status: ~w.~n', [Type, Status]),
    events([
        [
            is_at(ego, 'the junction'),
            sees(ego, 'stop sign'),
            sees(ego, 'solid white line across the road')
        ],
        [
            sees(ego, 'give way sign'),
            sees(ego, 'traffic oncoming')
        ],    % Repeat occurrencies
        [
            is_at(ego, 'the junction'),
            sees(ego, 'stop sign'),
            sees(ego, 'solid white line across the road')
        ], 
        [
            sees(ego, 'give way sign'),
            sees(ego, 'traffic oncoming')
        ],
        [
            is_at(ego, 'the junction'),
            sees(ego, 'stop sign'),
            sees(ego, 'solid white line across the road')
        ],
        [
            sees(ego, 'give way sign'),
            sees(ego, 'traffic oncoming')
        ],
        [
            is_at(ego, 'the junction'),
            sees(ego, 'stop sign'),
            sees(ego, 'solid white line across the road')
        ],
        [
            sees(ego, 'give way sign'),
            sees(ego, 'traffic oncoming')
        ],
        [
            is_at(ego, 'the junction'),
            sees(ego, 'stop sign'),
            sees(ego, 'solid white line across the road')
        ],
        [
            sees(ego, 'give way sign'),
            sees(ego, 'traffic oncoming')
        ]], R),
        % print_term(R, []),
        retractall(type(ego, _)),
        retractall(status(ego, _)),
        retractall(tokens(ego, _)),
        retractall(behaviour(ego, _)).
