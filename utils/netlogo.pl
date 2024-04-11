:- module(netlogo, []).

color(55, green).
color(45, yellow).
color(15, red).
color(9.9, white).

emergency_vehicle(Vehicle) :-
    % Set one of the colours to be the emergency vehicle
    % 85 is colour cyan.
    % Change to emergency_vehicle in the NetLogo and Prolog code.
    % member('truck', Vehicle).
    Vehicle == 'ambulance'.

emergency_vehicles(Vehicle) :-
    member(ambulance, Vehicle).

pedestrian(Vehicle) :-
    % Identify pedestrian by type
    member(person, Vehicle).

in_intersection(InIntersection) :-
    % Check if the vehicle is in the intersection
    % InIntersection is true if set to 1
    InIntersection == 1.

% in_intersection(X, Y) :-
%     X > -5,
%     Y > -5.

% in_intersection(X, Y) :-
%     X > -5,
%     Y > -5.
