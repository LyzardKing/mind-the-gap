type(a, car).
neighbour(a, ambulance).
neighbour(a, pedestrian).

stop(A) :-
    type(A, car) ->
    neighbour(A, B),
    give_way(A, B).

give_way(A, B) :-
    B = ambulance.

give_way(A, B) :-
    B = pedestrian.