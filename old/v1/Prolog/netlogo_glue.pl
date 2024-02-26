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

color(55, green).
color(45, yellow).
color(15, red).
color(9.9, white).

emergency_vehicle(Vehicle) :-
    % Set one of the colours to be the emergency vehicle
    % 85 is colour cyan.
    % Change to emergency_vehicle in the NetLogo and Prolog code.
    %member('truck', Vehicle).
    Vehicle == 'ambulance'.

emergency_vehicles(Vehicle) :-
    member(ambulance, Vehicle).

pedestrian(Vehicle) :-
    % Identify pedestrian by type
    member(person, Vehicle).

in_intersection(X, Y) :-
    X > -5,
    Y > -5.

% in_intersection(X, Y) :-
%     X > -5,
%     Y > -5.

add_violation(Vehicle, CarType, Violation, Context) :-
    % Context is anonymous to avoid repeating the same violation.
    (   violation(Vehicle, CarType, Violation, _)
    ->  true
    ;   assertz(violation(Vehicle, CarType, Violation, Context))
    ).

log_violations() :-
    log_path(Path),
    string_concat(Path, '/violations.pl', PathFull),
    atom_string(PathAtom, PathFull),
    % prolog_load_context(directory, Path),
    % print_message(warning, Path),
    tell(PathAtom), write(':- dynamic violation/4.'), nl, listing(violation/4), told.
