extensions[netprologo]

globals [
  ticks-at-last-change  ; value of the tick counter the last time a light changed
  total_accidents
  time_mul
  halt
]

breed [ lights light ]

breed [ accidents accident ]
accidents-own [
  clear-in              ; how many ticks before an accident is cleared
]

breed [ cars car ]
cars-own [
  speed                 ; how many patches per tick the car moves
  behaviour
  direction            ; Direction in which the car will go at the intersection.
  turned               ; True/False set IF the car has turned once already.
  autonomous           ; True/False set IF the car is an autonomous vehicle.
]

breed [ pedestrians pedestrian ]
pedestrians-own [
  speed
]

breed [ ambulances ambulance ]
ambulances-own [
  speed
]

breed [ signs sign ]
signs-own [
  spec
]

to startup
  consult-prolog
end

to consult-prolog
  if not netprologo:run-query "consult('traffic_rules_le.pl')"
  [
    user-message "Error loading prolog file"
  ]
end

;;;;;;;;;;;;;;;;;;;;
;;SETUP PROCEDURES;;
;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  retract-all
  consult-prolog
  set time_mul 100
  set halt false
  set-default-shape lights "square"
  set-default-shape accidents "fire"
  set-default-shape cars "car"
  set-default-shape pedestrians "person"
  set-default-shape signs "x"
  ask patches [
    set pcolor green - 1
  ]
  ; add box road around the screen
  ; now make the cars use them and not spawn/disappear
;  add_road "h" max-pycor - 1 1
;  add_road "h" min-pycor + 1 1
;  add_road "v" max-pxcor - 1 1
;  add_road "v" min-pxcor + 1 1

  add_road "h" 0 1
  add_road "v" -8 1
  add_road "v" 8 1

  ; add_intersection_color 8 max-pycor - 1
  ; add_intersection_color -8 max-pycor - 1

  add_intersection_lights -8 0
  add_intersection_stop 8 0 "v"
  reset-ticks
end

to add_road [dir center width]
  ask patches [
    ifelse dir = "h" [
      if (pycor >= center - width and pycor <= center + width) [
        ifelse pcolor = black [
          set pcolor grey
        ] [
          set pcolor black
        ]
      ]
    ] [
      if (pxcor >= center - width and pxcor <= center + width) [
        ifelse pcolor = black [
          set pcolor grey
        ] [
          set pcolor black
        ]
      ]
    ]
  ]
end

to add_intersection_lights [x y]
  ; add lights
  ask patch (x + 1) (y - 4) [ sprout-lights 1 [ set color green ] ]
  ask patch (x - 4) (y - 1) [ sprout-lights 1 [ set color red ] ]
  ask patch (x - 1) (y + 4) [ sprout-lights 1 [ set color green ] ]
  ask patch (x + 4) (y + 1) [ sprout-lights 1 [ set color red ] ]
  ; add_intersection_color x y
  ; add_pedestrian_crossing
end

to add_intersection_stop [x y dir]
  ; add stop signs
  let y1 y
  let x1 x
  if dir = "h" [
    set x1 x - 3
    set y1 y + 1
    set x x + 3
    set y y - 1
  ]
  ask patch (x1 + 1) (y1 - 2) [ sprout-signs  1 [ set color red set spec "stop" ] ]
  ask patch (x - 1) (y + 2) [ sprout-signs  1 [ set color red set spec "stop" ] ]
  ; add_intersection_color x y
end

to add_intersection_color [x y]
  ask patches [
  if (pxcor >= x - 1  and pxcor <= x + 1 and pycor >= y - 1 and pycor <= y + 1) [
      set pcolor grey
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;
;;RUNTIME PROCEDURES;;
;;;;;;;;;;;;;;;;;;;;;;

to go
  if halt [
    stop
  ]
  ask cars [ move ]
  ask pedestrians [ move ]
  ask ambulances [ move ]
  check-for-collisions

  ; Insert Monitor here!
  ; ...
  ; ask monitors [ monitor ]

  ; if the number of cars is less than X then
  if count cars < 15 [
    make-new-car freq-north / time_mul -7 min-pycor 0
    make-new-car freq-north / time_mul -9 max-pycor 180

    make-new-car freq-east / time_mul min-pxcor -1 90
    make-new-car freq-east / time_mul max-pxcor 1 -90

    make-new-car freq-north / time_mul 9 min-pycor 0
    make-new-car freq-north / time_mul 7 max-pycor 180
  ]
  ; make pedestrians
  make-new-pedestrian freq-pedestrian / time_mul -5 min-pycor 0
  make-new-pedestrian freq-pedestrian / time_mul min-pxcor -3 90
  ; if we are in "auto" mode and a light has been
  ; green for long enough, we turn it yellow
  if auto? and elapsed? (green-length * 100) [
    change-to-yellow
  ]
  ; if a light has been yellow for long enough,
  ; we turn it red and turn the other one green
  if any? lights with [ color = yellow ] and elapsed? (yellow-length * 100) [
    change-to-red
  ]
  tick
end

; to monitor
  ; Sees light
  ; if the light is green then report nothing to prolog
  ; if the light is yellow then report nothing to prolog
  ; if the light is red and there is a vehicle and the vehicle is moving
  ; then report to prolog the vehicle violates the red light. violates(vehicle, action).
; end

to make-new-pedestrian [freq x y h]
  ;let min-speed max (list (speed - sbrake) 0)
  ; Randomize crossing (before or after intersection)
  ; Enable only if prolog rule has position of self and neighbors
;  if (random 100 < 50) [
;    ifelse x = -3 [
;      set x 3
;    ] [
;      set y 3
;    ]
;  ]
  if (random-float 100 < 0.1 * freq) and not any? turtles-on patch x y [
    create-pedestrians 1 [
     ; to use the random function the rules have to be expanded.
     ; at the moment the "give_way" prolog method is called only
     ; in proximity of a junction.
     setxy x y
     ; setxy random-pxcor random-pycor
     set size 0.5
     set heading h
     set color one-of base-colors
     set speed 1
    ]
  ]
end

to make-new-car [ freq x y h ]
  let d "ahead"
  ifelse random 100 < 20 [
        set d "right"
      ] [
        if random 100 < 20 [
          set d "left"
        ]
      ]
  if (random-float 100 < freq) and not any? turtles-on patch x y [
    ifelse random 100 < 5 [
      create-cars 1 [
        setxy x y
        set heading h
        ; set color white
        ; There is no 3D model for the ambulance, so use only with the 2D simulation
        ; (it also does not point in the right direction).
        set shape "ambulance"
        ; set label "emergency"
        set label who
        set direction d
        set turned false
        adjust-speed
      ]
    ] [
      let b "good"
      if random 100 < freq-bad-cars [
        set b "bad"
      ]
      let a true
      let c blue
      if random 100 < freq-human-drivers [
        set a false
        set c red
      ]
      create-cars 1 [
        setxy x y
        set heading h
        set color c
        set label who
        set behaviour b
        set direction d
        set turned false
        set autonomous a
        adjust-speed
      ]
    ]
  ]
end

to move ; turtle procedure
  ; Some cars will turn at the intersection.
  ; The system should not break in this case.
;  if in-intersection? [
;    ; we are in the intersection.
;    ; let's figure out the possible directions
;    let directions []
;    if patch-ahead 2 != nobody and [pcolor] of patch-ahead 2 = black [
;      set directions lput "ahead" directions
;    ]
;    if patch-at-heading-and-distance 90 2 != nobody and [pcolor] of patch-at-heading-and-distance 90 2 = black [
;      set directions lput "right" directions
;    ]
;    if patch-at-heading-and-distance -90 2 != nobody and [pcolor] of patch-at-heading-and-distance -90 2 = black [
;      set directions lput "left" directions
;    ]
;  ]
;  let d one-of directions
;  show d
;  if shape = "ambulance" [
;    change-lane
;  ]
  if (xcor > min-pxcor + 1 and ycor > min-pycor + 1 and xcor < max-pxcor - 1 and ycor < max-pycor - 1) [
    let front_color [pcolor] of patch-at-heading-and-distance heading 1
    let past_color [pcolor] of patch-at-heading-and-distance heading -1
    if  (past_color != front_color) and in-intersection? and not turned [
      if (front_color = grey and direction = "right") [
        set heading heading + 90
      ]
      if (front_color = black and direction = "left") [
        set heading heading - 90
      ]
      set turned true
    ]
  ]

  adjust-speed
  repeat speed [ ; move ahead the correct amount
    fd 1 / 100
     if not can-move? 1 [ die ] ; die when I reach the end of the world
    ; or turn in a random direction
;    if shape != "person" and (not can-move? 3) [
;       ifelse random 100 < 50 [
;        set direction "right"
;      ] [
;        set direction "left"
;      ]
;      set turned false
;    ]
    if any? accidents-here [
      ; if I hit an accident, I cause another one
      ask accidents-here [ set clear-in 5 ]
      die
    ]
  ]
end

;to change-lane
;  ; The target lane to overtake is usually the one on the left
;  set heading ifelse-value target-lane < ycor [ 180 ] [ 0 ]
;  let blocking-cars other turtles in-cone (1 + abs (ycor - target-lane)) 180 with [ x-distance <= 1 ]
;  let blocking-car min-one-of blocking-cars [ distance myself ]
;  ifelse blocking-car = nobody [
;    forward 0.2
;    set ycor precision ycor 1 ; to avoid floating point errors
;  ] [
;    ; slow down if the car blocking us is behind, otherwise speed up
;    ifelse towards blocking-car <= 180 [ slow-down-car ] [ speed-up-car ]
;  ]
;end

to adjust-speed
  ; calculate the minimum and maximum possible speed I could go

  ; If I am a human, I will think I can brake as an AV, at least to determine the speed I can go to
  ; I will (randomly) decide to go faster than the max speed
  let limit speed-limit
  let accel max-accel
  if shape = "car" and not autonomous [
    if random 100 < 50 [
      set limit speed-limit * 1.2
    ]
    if speed = 0 and random 100 < 20 [
      set accel 0
    ]
  ]
  let min-speed max (list (speed - max-brake) 0)
  let max-speed min (list (speed + accel) limit)

  let target-speed max-speed ; aim to go as fast as possible
  if shape = "person" [
    set target-speed 1 ; / 10
  ]
  ;let target-speed min-speed + (random-float (max-speed - min-speed))

  let blocked-patch next-blocked-patch
  if blocked-patch != nobody [
    ; if there is an obstacle ahead, reduce my speed
    ; until I'm sure I won't hit it on the next tick
    let space-ahead (distance blocked-patch - 1)
    while [
      breaking-distance-at target-speed > space-ahead and
      target-speed > min-speed
    ] [
      set target-speed (target-speed - 1)
      if target-speed < 0 [
        set target-speed 0
      ]
    ]
  ]
  ; Consider moving the prolog rule here.
  set speed target-speed
end

to-report breaking-distance-at [ speed-at-this-tick ] ; car reporter
  ; If I was to break as hard as I can on the next tick,
  ; how much distance would I have travelled assuming I'm
  ; currently going at `speed-this-tick`?

  ; If I am a human I will have a longer max-brake
  ; Also I will not be consistent
  let brake max-brake
  let r (random 75) / 100
  if shape = "car" and not autonomous [
    set brake max-brake * r
  ]
  let min-speed-at-next-tick max (list (speed-at-this-tick - brake) 0)
  report speed-at-this-tick + min-speed-at-next-tick
end

to-report next-blocked-patch ; turtle procedure
  ; check all patches ahead until I find a blocked
  ; patch or I reach the end of the world
  let patch-to-check patch-here
  while [ patch-to-check != nobody and not is-blocked? patch-to-check ] [
    set patch-to-check patch-ahead ((distance patch-to-check) + 1)
  ]
  ; report the blocked patch or nobody if I didn't find any
  report patch-to-check
end

to-report is-blocked? [ target-patch ] ; turtle reporter
  let target_patch_x [pxcor] of target-patch
  let target_patch_y [pycor] of target-patch
  let self_shape [shape] of turtle who
  let self_heading [heading] of turtle who
  let other_turtles (other turtles in-cone 6 120) with [heading != self_heading]
  ; let self_behaviour ([behaviour] of turtle who)
  let self_distance (distance target-patch)
  let self_speed ([speed] of turtle who)
  ; let light_color ([color] of lights in-radius 0.5)
  let light_color ([color] of lights in-radius 0.5)
  let sign_spec ([spec] of signs in-radius 0.5)
  let in_intersection 0

  if in-intersection? [
    set in_intersection 1
  ]
  report
  any? other cars-on target-patch or (self_shape != "person" and any? other pedestrians-on target-patch) or
    any? accidents-on target-patch or
    ; replaced ; in-radius 6 with in-cone 6 180
  (any? (lights in-radius 0.5) and self_shape != "person" and
    ; ([behaviour] of turtle who = "good") and
    ; netprologo:run-query(netprologo:build-prolog-call "not_enter_junction(?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)" who ([behaviour] of turtle who) self_distance self_speed light_color sign_spec self_shape ([shape] of other_turtles) in_intersection)
    netprologo:run-query(netprologo:build-prolog-call ( word
      "snapshot(("
      "asserta(behaviour(?1, ?2, ?10)),"
      "asserta(distance(?1, ?3, ?10)),"
      "asserta(speed(?1, ?4, ?10)),"
      "asserta(has_light(?1, ?5, ?10)),"
      "asserta(has_sign(?1, ?6, ?10)),"
      "asserta(is_of_type(?1, ?7, ?10)),"
      "asserta(has_neighbours(?1, ?8, ?10)),"
      "asserta(is_in_junction(?1, ?9, ?10)),"
      "not_enter_junction_out(?1, ?10)"
      "))") who ([behaviour] of turtle who) self_distance self_speed light_color sign_spec self_shape ([shape] of other_turtles) in_intersection ticks)
  ) or
  (any? (signs in-radius 0.5) and self_shape != "person" and speed > 0 and
    ; netprologo:run-query(netprologo:build-prolog-call "not_enter_junction(?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)" who ([behaviour] of turtle who) self_distance self_speed light_color sign_spec self_shape ([shape] of other_turtles) in_intersection)
    ; Add a timer for the stop sign? or make the condition to restart more complex
        netprologo:run-query(netprologo:build-prolog-call ( word
      "snapshot(("
      "asserta(behaviour(?1, ?2, ?10)),"
      "asserta(distance(?1, ?3, ?10)),"
      "asserta(speed(?1, ?4, ?10)),"
      "asserta(has_light(?1, ?5, ?10)),"
      "asserta(has_sign(?1, ?6, ?10)),"
      "asserta(is_of_type(?1, ?7, ?10)),"
      "asserta(has_neighbours(?1, ?8, ?10)),"
      "asserta(is_in_junction(?1, ?9, ?10)),"
      "not_enter_junction_out(?1, ?10)"
      "))") who ([behaviour] of turtle who) self_distance self_speed light_color sign_spec self_shape ([shape] of other_turtles) in_intersection ticks)
  )
  or
  (
    any? (signs in-radius 0.5) and any? other cars in-cone 5 120 with [heading != self_heading and speed != 0]
  )
  ;( self_shape != "person" and
  ;  netprologo:run-query(netprologo:build-prolog-call "give_way(?1, ?2, ?3, ?4)" who ([behaviour] of turtle who) ([shape] of other_turtles) "_")
  ;)
end

; TODO: write with prolog link
; to-report can-overtake

to-report make-way?
  let self_heading [heading] of turtle who
  ; let self_back self_heading - 180
  ; let ambulance_behind count (ambulances-on patch-at-heading-and-distance self_back 1)
  let ambulance_behind count (cars-on patch-at-heading-and-distance self_heading -1) with [shape = "ambulance"]
  report ambulance_behind > 0
end

to-report in-intersection?
;  set ycor precision ycor 1 ; to avoid floating point errors
;  set xcor precision xcor 1 ; to avoid floating point errors
;  report pcolor = grey
  report pcolor = grey and xcor mod 1 < 0.01 and ycor mod 1 < 0.01 ; Determine if in intersection by looking at the patch color
end

to check-for-collisions
  ask accidents [
    set clear-in clear-in - 1
    if clear-in = 0 [ die ]
  ]
  ask patches with [ count turtles-here with [ shape != "square" and shape != "x" ] > 1 and count turtles-here with [ shape != "square" and shape != "x" ] > count turtles-here with [ shape = "person" ] ] [
    sprout-accidents 1 [
      set size 1.5
      set color yellow
      set clear-in 5
    ]
    ; stop
    ask cars-here [ die ]
    ask pedestrians-here [ die ]
    set total_accidents total_accidents + 1
;    set halt true
  ]
end

to change-to-yellow
  ask lights with [ color = green ] [
    set color yellow
    set ticks-at-last-change ticks
  ]
end

to change-to-red
  let red_lights lights with [ color = red ]
  ask lights with [ color = yellow ] [
    set color red
    ;ask other lights with [ color = red ] [ set color green ]
    set ticks-at-last-change ticks
  ]
  ask red_lights [
    set color green
    set ticks-at-last-change ticks
  ]
end

; reports `true` if `time-length` ticks
; has elapsed since the last light change
to-report elapsed? [ time-length ]
  report (ticks - ticks-at-last-change) > time-length
end

to log-violations
  if not netprologo:run-query "log_violations()" [
    show "Cannot log violations"
  ]
end

to retract-all
  if not netprologo:run-query "retractall(violation(_, _, _, _))" [
    show "Cannot reset violations"
  ]
end

; Copyright 1998 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
235
10
1263
719
-1
-1
20.0
1
10
1
1
1
0
0
0
1
-25
25
-17
17
0
0
1
ticks
30.0

BUTTON
8
10
98
43
NIL
setup
NIL
1
T
OBSERVER
NIL
R
NIL
NIL
1

BUTTON
105
10
170
43
NIL
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
0

BUTTON
105
85
170
119
switch
change-to-yellow
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
7
319
168
352
green-length
green-length
1
50
25.0
1
1
NIL
HORIZONTAL

SLIDER
7
125
168
158
speed-limit
speed-limit
1
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
7
168
168
201
max-accel
max-accel
1
10
2.0
1
1
NIL
HORIZONTAL

SLIDER
7
201
168
234
max-brake
max-brake
1
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
7
243
168
276
freq-north
freq-north
0
100
25.0
5
1
%
HORIZONTAL

SLIDER
7
276
168
309
freq-east
freq-east
0
100
25.0
5
1
%
HORIZONTAL

SWITCH
8
85
98
118
auto?
auto?
0
1
-1000

SLIDER
7
352
168
385
yellow-length
yellow-length
0
10
10.0
1
1
NIL
HORIZONTAL

BUTTON
105
45
170
78
go once
go
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
0

BUTTON
5
645
122
678
NIL
log-violations
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
20
690
112
723
NIL
retract-all
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
10
520
112
565
NIL
total_accidents
0
1
11

SLIDER
5
390
170
423
freq-bad-cars
freq-bad-cars
0
100
20.0
1
1
NIL
HORIZONTAL

SLIDER
0
435
177
468
freq-human-drivers
freq-human-drivers
0
100
28.0
1
1
NIL
HORIZONTAL

MONITOR
15
585
77
630
fast cars
count cars with [speed > 4]
0
1
11

SLIDER
0
475
172
508
freq-pedestrian
freq-pedestrian
0
100
0.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

In this model the turtles are cars traveling through an intersection.  The user has the ability to control the frequency of cars coming from each direction, the speed of the cars, and the timing of the light at the traffic intersection.  Once the frequency and speed of cars is selected, the user should run the simulation and adjust the timing of the traffic light so as to minimize the amount of waiting time of cars traveling through the intersection.

## HOW IT WORKS

The rules for each car are:

- I can only go in the direction I started in, or stop.

- I stop for cars in front of me and red lights, and I stop for a yellow light if I'm not already on it.

- If I am moving quickly and I see that I will have to stop soon, I try to slow down enough to make sure I can stop in time, up to MAX-BRAKE.

- If I see that I have free space in front of me, I speed up towards the SPEED-LIMIT, up to MAX-ACCEL.

- If I am on the same space as another car, we crash and die.

## HOW TO USE IT

WAIT-TIME-OVERALL shows how many cars are waiting during the given clock tick.

WAIT-TIME-EASTBOUND shows how many eastbound cars are waiting during the given clock tick.

WAIT-TIME-NORTHBOUND shows how many northbound cars are waiting during the given clock tick.

CLOCK shows how many ticks have elapsed.

Use the FREQ-EAST slider to select how often new eastbound cars travel on the road.

Use the FREQ-NORTH slider to select how often new northbound cars travel on the road.

Use the SPEED-LIMIT slider to select how fast the cars will travel.

Use the MAX-ACCEL slider to determine how fast the cars can accelerate.

Use the MAX-BRAKE slider to determine how fast the cars can decelerate.

Use the GREEN-LENGTH slider to set how long the light will remain green.

Use the YELLOW-LENGTH slider to set how long the light will remain yellow.

Press GO ONCE to make the cars move once.

Press GO to make the cars move continuously.

To stop the cars, press the GO button again.

## THINGS TO NOTICE

Cars start out evenly spaced but over time, they form bunches. What kinds of patterns appear in the traffic flow?

Under what conditions do the cars appear to be moving backwards?

Gridlock happens when cars are unable to move because cars from the other direction are in their path.  What settings cause gridlock in this model?  What settings can be changed to end the gridlock?

## THINGS TO TRY

Try to answer the following questions before running the simulations.

Record your predictions.

Compare your predicted results with the actual results.

- What reasoning led you to correct predictions?

- What assumptions that you made need to be revised?

Try different numbers of eastbound cars while keeping all other slider values the same.

Try different numbers of northbound cars while keeping all other slider values the same.

Try different values of SPEED-LIMIT while keeping all other slider values the same.

Try different values of MAX-ACCEL while keeping all other slider values the same.

Try different values of GREEN-LENGTH and YELLOW-LENGTH while keeping all other slider values the same.

For all of the above cases, consider the following:

- What happens to the waiting time of eastbound cars?

- What happens to the waiting time of northbound cars?

- What happens to the overall waiting time?

- What generalizations can you make about the impact of each variable on the waiting time of cars?

- What kind of relationship exists between the number of cars and the waiting time they experience?

- What kind of relationship exists between the speed of cars and the waiting time they experience?

- What kind of relationship exists between the number of ticks of green light and the waiting time cars experience?

Use your answers to the above questions to come up with a strategy for minimizing the waiting time of cars.

What factor (or combination of factors) has the most influence over the waiting time experienced by the cars?

## EXTENDING THE MODEL

Find a realistic way to eliminate all crashes by only changing car behavior.

Allow different light lengths for each direction in order to control wait time better.

Is there a better way to measure the efficiency of an intersection than the current number of stopped cars?

## RELATED MODELS

- "Traffic Basic": a simple model of the movement of cars on a highway.

- "Traffic Basic Utility": a version of "Traffic Basic" including a utility function for the cars.

- "Traffic Basic Adaptive": a version of "Traffic Basic" where cars adapt their acceleration to try and maintain a smooth flow of traffic.

- "Traffic Basic Adaptive Individuals": a version of "Traffic Basic Adaptive" where each car adapts individually, instead of all cars adapting in unison.

- "Traffic 2 Lanes": a more sophisticated two-lane version of the "Traffic Basic" model.

- "Traffic Grid": a model of traffic moving in a city grid, with stoplights at the intersections.

- "Traffic Grid Goal": a version of "Traffic Grid" where the cars have goals, namely to drive to and from work.

- "Gridlock HubNet": a version of "Traffic Grid" where students control traffic lights in real-time.

- "Gridlock Alternate HubNet": a version of "Gridlock HubNet" where students can enter NetLogo code to plot custom metrics.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U. (1998).  NetLogo Traffic Intersection model.  http://ccl.northwestern.edu/netlogo/models/TrafficIntersection.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 1998 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

This model was created as part of the project: CONNECTED MATHEMATICS: MAKING SENSE OF COMPLEX PHENOMENA THROUGH BUILDING OBJECT-BASED PARALLEL MODELS (OBPML).  The project gratefully acknowledges the support of the National Science Foundation (Applications of Advanced Technologies Program) -- grant numbers RED #9552950 and REC #9632612.

This model was converted to NetLogo as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227. Converted from StarLogoT to NetLogo, 2002.

<!-- 1998 2002 -->
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

ambulance
true
0
Rectangle -7500403 true true 90 90 195 270
Polygon -7500403 true true 190 4 150 4 134 41 104 56 105 90 190 90
Rectangle -1 true false 60 105 105 105
Polygon -16777216 true false 112 62 141 48 141 81 112 82
Circle -16777216 true false 174 24 42
Circle -16777216 true false 174 189 42
Rectangle -1 true false 158 3 173 12
Rectangle -1184463 true false 180 2 172 11
Rectangle -2674135 true false 151 2 158 271
Line -16777216 false 90 90 195 90
Rectangle -16777216 true false 116 172 133 217
Rectangle -16777216 true false 111 124 134 147
Line -7500403 true 105 135 135 135
Rectangle -7500403 true true 186 267 195 286
Line -13345367 false 135 255 120 225
Line -13345367 false 135 225 120 255
Line -13345367 false 112 240 142 240

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
true
0
Polygon -7500403 true true 180 15 164 21 144 39 135 60 132 74 106 87 84 97 63 115 50 141 50 165 60 225 150 285 165 285 225 285 225 15 180 15
Circle -16777216 true false 180 30 90
Circle -16777216 true false 180 180 90
Polygon -16777216 true false 80 138 78 168 135 166 135 91 105 106 96 111 89 120
Circle -7500403 true true 195 195 58
Circle -7500403 true true 195 47 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fire
false
0
Polygon -7500403 true true 151 286 134 282 103 282 59 248 40 210 32 157 37 108 68 146 71 109 83 72 111 27 127 55 148 11 167 41 180 112 195 57 217 91 226 126 227 203 256 156 256 201 238 263 213 278 183 281
Polygon -955883 true false 126 284 91 251 85 212 91 168 103 132 118 153 125 181 135 141 151 96 185 161 195 203 193 253 164 286
Polygon -2674135 true false 155 284 172 268 172 243 162 224 148 201 130 233 131 260 135 282

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
