:-style_check(-singleton).
:-style_check(-discontiguous).

:- include('netlogo_glue.pl').

:- dynamic behaviour/2.
:- dynamic distance/2.
:- dynamic speed/2.
:- dynamic has_light/2. 
:- dynamic has_sign/2.
:- dynamic is_of_type/2.
:- dynamic has_neighbours/2.
:- dynamic is_in_junction/2.

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

has_neighbour(Self, Neighbour, Time) :-
    has_neighbours(Self, Ns, Time),
    member(Neighbour, Ns).

% The neighbour is outside the if condition since it could exist more than once
% For the previous facts we just need to unify with the first one (semidet)
% Possibly just an issue with the single prolog instance.
% There is no more if condition because everything happens in a snapshot.
% It should(TM) be more efficient
not_enter_junction_out(Self, Time) :-
    % Static vehicle properties
    % Could be defined once on vehicle spawn
    is_of_type(Self, CarType, Time),
    behaviour(Self, Behaviour, Time),
    % Dynamic state of the world
    % This section contains the facts to assert in the snapshot
    distance(Self, Distance, Time),
    speed(Self, Speed, Time),
    has_light(Self, LightNum, Time),
    has_sign(Self, Sign, Time),
    is_in_junction(Self, InIntersection, Time),
    has_neighbour(Self, Neighbours, Time),
    not_enter_junction(Self, Behaviour, Distance, Speed, LightNum, Sign, CarType, Neighbours, InIntersection).
    % -> cleanup(a); cleanup(a),false.

% Deprecated since the evaluation happens in a snapshot/1.
% cleanup(Self) :-
%     retractall(behaviour(Self, _)),
%     retractall(distance(Self, _)),
%     retractall(speed(Self, _)),
%     retractall(has_light(Self, _)),
%     retractall(has_sign(Self, _)),
%     retractall(is_of_type(Self, _)),
%     retractall(has_neighbours(Self, _)),
%     retractall(is_in_junction(Self, _)). 

not_enter_junction(Self, Behaviour, Distance, Speed, [LightNum], Sign, CarType, Neighbours, InIntersection) :-
    ground(LightNum),
    \+ enter_junction(Self, LightNum, CarType, Neighbours, InIntersection),
    (
        Behaviour = bad -> 
            (
                color(LightNum, Light),
                add_violation(Self, CarType, ignored_junction_rules, [Self, Behaviour, Light, CarType, Neighbours, InIntersection]),
                false
            )
    ;   true
    ).
    % Behaviour == good.

not_enter_junction(Self, Behaviour, Distance, Speed, [], [Sign], CarType, Neighbours, InIntersection) :-
    (
        (
            must_stop(Sign, Speed, Distance),
            not(emergency_vehicle(CarType))
        )
    ;   \+ enter_junction(Self, LightNum, CarType, Neighbours, InIntersection)
    ),
    (
        Behaviour = bad -> 
            (
                add_violation(Self, CarType, ignored_junction_rules, [Self, Behaviour, stop_sign, CarType, Neighbours, InIntersection]),
                false
            )
    ;   true
    ).

% Emergency vehicles can enter the intersection even if the light is red.
enter_junction(Self, Light, CarType, Neighbours, InIntersection) :-
    emergency_vehicle(CarType),
    \+ give_way(Self, CarType, Neighbours, Light).
    % add_violation(Self, CarType, crossed_with_red_light).
    % log(crossing, Self, CarType, Light).

% Normal cars can enter the intersection if they do not have to give way to other agents.
enter_junction(Self, Light, CarType, Neighbours, InIntersection) :-
    ground(Light),
    color(Light, green),
    \+ give_way(Self, CarType, Neighbours, Light).

enter_junction(Self, Light, CarType, Neighbours, InIntersection) :-
    ground(Light),
    color(Light, yellow),
    in_intersection(InIntersection),
    \+ give_way(Self, CarType, Neighbours, Light).
    % print_message(warning, 'Yellow light').
    % log(yellow, Self, CarType, Light, X, Y).
    % add_violation(Self, CarType, crossed_with_red_light).

enter_junction(Self, Light, CarType, Neighbours, InIntersection) :-
    member(x, Neighbours),
    \+ give_way(Self, stop, CarType, Neighbours).

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

% Rule 172
% The approach to a junction may have a 'Give Way' sign or a triangle marked on the road.
% You MUST give way to traffic on the main road when emerging from a junction with broken white lines across the road.
must_give_way(Sign) :-
    give_way_sign(Sign).    

give_way_sign(Sign) :-
    Sign = 'give_way'.


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
    emergency_vehicle(Neighbours).
    % log(give_way, Self, CarType, emergency_vehicle, Light).

% Rule 170.2
% give way to pedestrians crossing or waiting to cross a road into which or
% from which you are turning. If they have started to cross they have priority,
% so give way (see Rule H2)
give_way(Self, CarType, Neighbours, Light) :-
    pedestrian(Neighbours).
    % log(give_way, Self, CarType, pedestrian, Light).

give_way(Self, stop, CarType, Neighbours) :-
    length(Neighbours, Y),
    % log(stop, Self, CarType, Neighbours),
    Y > 1.

% Add the possibility to give way only to vehicles coming from the right.
% The autonomous vehicle should also be careful not to fall into the situation where nobody moves
% For now use the neighbours to determine if there are any from the right side
% TODO: add overtaking rule
