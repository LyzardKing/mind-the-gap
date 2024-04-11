:- module(utils, []).

:- use_module(le).
:- use_module(netlogo).

%% Rules to bridge prolog and Logo

:- dynamic violation/4.

:- prolog_load_context(directory, Dir),
   asserta(user:log_path(Dir)).

log(crossing, Self, CarType, LightNum) :-
    (
        (
            color(LightNum, red),
            swritef(Message, '%w %w crossed with %w light.', [CarType, Self, red]),
            print_message(warning, Message)
        )
        ;   true
    ).

log(give_way, Self, CarType, X, LightNum) :-
    color(LightNum, Light),
    swritef(Message, '%w %w has given way to %w at a %w light.', [CarType, Self, X, Light]),
    print_message(warning, Message).

log(stop, Self, CarType, X) :-
    swritef(Message, '%w %w has given way to %w at a stop sign.', [CarType, Self, X]),
    print_message(warning, Message).

log(yellow, Self, CarType, LightNum, X, Y) :-
    color(LightNum, Light),
    swritef(Message, '%w %w crossed with a %w light seen at position %w %w.', [CarType, Self, Light, X, Y]),
    print_message(warning, Message).


add_violation(Vehicle, CarType, Violation, Context) :-
    % Context is anonymous to avoid repeating the same violation.
    (   violation(Vehicle, CarType, Violation, _)
    ->  true
    ;   assertz(violation(Vehicle, CarType, Violation, Context))
    ).

log_violations :-
    log_path(Path),
    string_concat(Path, '/violations.pl', PathFull),
    atom_string(PathAtom, PathFull),
    % prolog_load_context(directory, Path),
    % print_message(warning, Path),
    tell(PathAtom), write(':- dynamic violation/4.'), nl, listing(violation/4), told.