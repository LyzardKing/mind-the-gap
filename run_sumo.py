import os, sys

SUMO_HOME = "/var/lib/flatpak/app/org.eclipse.sumo/current/active/files/share/sumo"
sys.path.append(os.path.join(SUMO_HOME, "tools"))
import traci
sumoBin = "/var/lib/flatpak/exports/bin/org.eclipse.sumo"
sumoCmd = [sumoBin, "-c", "/home/galileo/Downloads/Bologna_small-0.29.0/acosta/run.sumo.cfg"]
traci.start(sumoCmd)
