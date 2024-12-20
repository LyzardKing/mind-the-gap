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

## Docker image

### Syntax

Run a docker image to compile the LE code and return errors if any.

Build the image via:
`docker build -f Syntax.dockerfile -t avsyntax .`

PWD is the current directory.

`docker run --volume ${PWD}:/app avsyntax`

### Netlogo

Build an image with the dockerfile to run NetLogo.
Can be used redirecting output to the display, or headless, by running the following command:

`docker run -it --volume .:/app netlogo /opt/netlogo/netlogo-headless.sh --model /app/Traffic\ Intersection.nlogo --experiment default --table /app/output.csv`

output.csv will contain the log of the simulation (default 1000 ticks), with the last two elements being the number of accidents and violations.