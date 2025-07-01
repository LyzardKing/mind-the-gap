# Syntax error checking

Fix the syntax error. Be sure that the rules follow the templates and the general rules presented in the prompt.

## Sentence to analyse
The approach to a junction may have a 'Give Way' sign or a triangle marked on the road. You MUST give way to traffic on the main road when emerging from a junction with broken white lines across the road.

## Generated LE
the templates are:
  *a thing* has *a property*.
  *an agent* must *an action* at *a location*.

the knowledge base example includes:
ego must give way to traffic on the main road
if the junction has broken white lines across the road.


## Error generated
ERROR: _6592:9: Syntax error: LE error found around this expression: : ego must give way to traffic on the main road