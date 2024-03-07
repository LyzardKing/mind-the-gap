# Mind the Gap

## Requirements

NetLogo: [https://github.com/NetLogo/NetLogo/](https://github.com/NetLogo/NetLogo/)

NetProLogo extension: [https://github.com/LyzardKing/NetProLogo](https://github.com/LyzardKing/NetProLogo)

## Usage

Load the file `Traffic Intersection.nlogo` in NetLogo.
It should pick up the Prolog file declared at the beginning.

The current version uses `traffic_rules.pl`.

The rules are automatically translated (using `parse_le.pl`) from Logical English. A working version is always contained in `traffic_rules-prolog.pl`, and should not be modified manually.

The glue for Netlogo is contained in `netlogo_glue.pl`, and mostly deals with data translation and logging functions.