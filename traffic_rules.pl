:-style_check(-singleton).
:-style_check(-discontiguous).

:- include('netlogo_glue.pl').

% Add not give_way to enter_junction.
% Keep all parameters in arguments or assert facts?
% Logo deals with the "sensor" part of the simulation,
% such as avoiding other cars.
% The Prolog rules determine which actions can be executed
% in "normal" scenarios (no accidents involved).

% Neighbours are all the agents in a 3 block range from self
% who are moving in a different direction.
% the coordinates in give_way are used to filter out
% the agents behind self.

% Possibiity or prohibition to enter the intersection.
% One represented as the opposite of the other, but
% logically the possibility is given by the sensors:
%   - there is no impediment on the road
% the prohibition is given by the norms:
%   - there is something that prohibits this action, even
%     though it is still possible.
% In the second case there is a violation.

% Simple negation rule
not_enter_junction(Self, Behaviour, Distance, Speed, [LightNum], Sign, CarType, Neighbours, X, Y) :-
    \+ enter_junction(Self, LightNum, CarType, Neighbours, X, Y),
    (
        Behaviour = bad -> 
            (
                color(LightNum, Light),
                add_violation(Self, CarType, ignored_junction_rules, [Self, Behaviour, Light, CarType, Neighbours, X, Y]),
                false
            )
    ;   true
    ).
    % Behaviour == good.

not_enter_junction(Self, Behaviour, Distance, Speed, [], [Sign], CarType, Neighbours, X, Y) :-
    (
        (
            must_stop(Sign, Speed, Distance),
            not(emergency_vehicle(CarType))
        )
    ;   \+ enter_junction(Self, LightNum, CarType, Neighbours, X, Y)
    ),
    (
        Behaviour = bad -> 
            (
                add_violation(Self, CarType, ignored_junction_rules, [Self, Behaviour, stop_sign, CarType, Neighbours, X, Y]),
                false
            )
    ;   true
    ).

% Emergency vehicles can enter the intersection even if the light is red.
enter_junction(Self, Light, CarType, Neighbours, X, Y) :-
    emergency_vehicle(CarType),
    \+ give_way(Self, CarType, Neighbours, Light),
    % add_violation(Self, CarType, crossed_with_red_light).
    log(crossing, Self, CarType, Light).

% Normal cars can enter the intersection if they do not have to give way to other agents.
enter_junction(Self, Light, CarType, Neighbours, X, Y) :-
    color(Light, green),
    \+ give_way(Self, CarType, Neighbours, Light).

enter_junction(Self, Light, CarType, Neighbours, X, Y) :-
    color(Light, yellow),
    in_intersection(X, Y),
    \+ give_way(Self, CarType, Neighbours, Light),
    log(yellow, Self, CarType, Light, X, Y).
    % add_violation(Self, CarType, crossed_with_red_light).

% Rule 171
% You MUST stop behind the line at a junction with a 'Stop' sign and a solid
% white line across the road. Wait for a safe gap in the traffic before you move off.
% TODO: Add self to log what the vehicles do
must_stop(Sign, Speed, Distance) :-
    stop_sign(Sign),
    Speed > 0,
    Distance > 1.

stop_sign(Sign) :-
    Sign = 'stop'.

stop_sign(Sign) :-
    Sign = 'white_line'.

% The safe distance is calculated by the sensors
% and is strictly tied with the is-blocked status
% TODO: port this to prolog by expressing the whole world as input, or
% Use prolog to give the rules that the actuators should use.
% safe distance(...) :-
%   ...

% Cars have to give way to emergency vehicles at an intersection
% and pedestrians (at the moment when they are about to cross)
% The vehicle's sensors only see things in a cone in front, so it is
% not necessary to check for the direction.

% Is this rule 219?
give_way(Self, CarType, Neighbours, Light) :-
    emergency_vehicle(Neighbours),
    log(give_way, Self, CarType, emergency_vehicle, Light).

% Rule 170.2
% give way to pedestrians crossing or waiting to cross a road into which or
% from which you are turning. If they have started to cross they have priority,
% so give way (see Rule H2)
give_way(Self, CarType, Neighbours, Light) :-
    pedestrian(Neighbours),
    log(give_way, Self, CarType, pedestrian, Light).

