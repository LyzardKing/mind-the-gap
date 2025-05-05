# import traci
import libsumo as traci
import sumolib
import time
import random
import janus_swi as janus
from contextlib import suppress
import xml.etree.ElementTree as ET
import sys

distance = 50
janus.consult("traffic_rules.pl")

def get_vehicle_ids(file_path):
    tree = ET.parse(file_path)
    root = tree.getroot()
    
    vehicle_ids = []
    for vehicle in root.findall('vehicle'):
        if vehicle.get('type') != "autonomous":
            continue
        vehicle_id = vehicle.get('id')
        if vehicle_id:
            vehicle_ids.append(vehicle_id)
    
    return vehicle_ids

def get_traffic_light_junctions():
    """Find all junctions with traffic lights in the SUMO network."""
    return traci.trafficlight.getIDList()

def get_random_route():
    """Select a random route from the available routes in the simulation."""
    routes = traci.route.getIDList()
    print(routes)
    return random.choice(routes) if routes else None

def get_lane_light_color(traffic_light_id, lane_id):
    """Retrieve the traffic light color for a specific lane."""
    # Use the TraCI command "0xa2" to get the traffic light state for a specific lane
    tl_state = traci.trafficlight.getRedYellowGreenState(traffic_light_id)
    lane_color = None
    
    # Iterate through the phases for this traffic light and find the lane state
    for i, state in enumerate(tl_state):
        if traci.trafficlight.getControlledLanes(traffic_light_id)[i] == lane_id:
            if state == 'r':
                lane_color = "red"
            elif state == 'y':
                lane_color = "yellow"
            # Ignore the priority mode used by SUMO. To be replaced with Prolog.
            elif state == 'g' or state == 'G':
                lane_color = "green"
            break
    
    return lane_color

def get_lane_light_color(state):
    """Retrieve the traffic light color for a specific lane."""
    # Iterate through the phases for this traffic light and find the lane state
    if state == 'r':
        lane_color = "red"
    elif state == 'y':
        lane_color = "amber"
    elif state == 'g' or state == 'G':
        lane_color = "green"
    return lane_color

def get_next_traffic_light_state(vehicle_id):
    next_traffic_lights = traci.vehicle.getNextTLS(vehicle_id)
    traffic_light_color, traffic_light_distance = None, None
    if next_traffic_lights:
        traffic_light = next_traffic_lights[0]
        traffic_light_color = get_lane_light_color(traffic_light[3])
        traffic_light_distance = traffic_light[2]
    return traffic_light_color, traffic_light_distance

def get_junction_foes(vehicle_id):
    vehicle_lane = traci.vehicle.getLaneID(vehicle_id)
    lane_links = traci.lane.getLinks(vehicle_lane)
    foe_lanes = []
    for lane_link in lane_links:
        if not lane_link[3]:
            continue
        if lane_link[5] != "G":
            continue
        foe_lanes.append(lane_link)
    traci.vehicle.subscribeContext(vehicle_id, 0, 10)
    with suppress(traci.exceptions.TraCIException):
        foe_id = traci.vehicle.getContextSubscriptionResults(vehicle_id)
        print(foe_id)
    if foe_lanes:
        # return foe_lanes
        next_link = traci.vehicle.getNextLinks(vehicle_id)[0]
        print(vehicle_id, next_link, foe_lanes)

def set_autopilot(vehicle_id):
    # Custom autopilot behavior
    if vehicle_id in traci.vehicle.getIDList():
        vehicle_edge = traci.vehicle.getLaneID(vehicle_id)
        traffic_light_color, traffic_light_distance = get_next_traffic_light_state(vehicle_id)
        # Add here the other signals
        # junction_foes = traci.vehicle.getJunctionFoes(vehicle_id)
        # junction_foes = get_junction_foes(vehicle_id)
        result = True
        try:
            # must_stop = janus.query_once(f"snapshot((asserta(has(junction, light, {traffic_light_color})),'traffic_rules-prolog':must_at(ego, stop, _, junction)))")["truth"]
            must_stop = janus.query_once(f"snapshot((asserta(has(junction, light, {traffic_light_color})),'traffic_rules-prolog':must(ego, stop)))")
            stop_truth = must_stop["truth"]
            stop_message = janus.query_once(f"snapshot((asserta(has(junction, light, {traffic_light_color})),'traffic_rules-prolog':can_break_rule(ego, stop)))")["truth"]
            if stop_truth:
                result = False
            if stop_message:
                print("Rule broken.")
                result = True
            else:
                # Add position in junction
                can_go = janus.query_once(f"snapshot((asserta(has(junction, light, {traffic_light_color})),'traffic_rules-prolog':can_at(ego, 'go on', junction)))")["truth"]
                if can_go:
                    result = True
                # else:
                #     print("No rules found. Default to permission.")
        except UnboundLocalError as e:
            print(e)
            print(vehicle_id)
        # if len(junction_foes) > 0:
        #     print(vehicle_id, junction_foes)
        # result = traffic_light_color == "red"
        # print(f"Result: {result}")
        if not result and traffic_light_distance < distance:
            stop_position = traci.lane.getLength(vehicle_edge)
            try:
                traci.vehicle.setStop(vehicle_id, traci.vehicle.getRoadID(vehicle_id), pos=stop_position)
            except traci.exceptions.TraCIException as e:
                print(vehicle_id, e)
        else:
            # if traci.vehicle.isStopped(vehicle_id):
            if traci.vehicle.getStops(vehicle_id):
                traci.vehicle.replaceStop(vehicle_id, 0, "")
                # traci.vehicle.resume(vehicle_id)
            # traci.vehicle.resume(vehicle_id)

def load_map(sumo_binary, map_name):
    if map_name == "swansea":
        traci.start([sumo_binary, '-c', 'swansea/run.sumocfg'])
        # Apply custom autopilot behavior to all vehicles
        vehicle_ids = get_vehicle_ids("swansea/routes.rou.xml")
    elif map_name == "bologna":
        traci.start([sumo_binary, '-c', 'bologna/acosta/run.sumocfg'])
        # Apply custom autopilot behavior to all vehicles
        vehicle_ids = get_vehicle_ids("bologna/acosta/acosta.rou.xml")
    return vehicle_ids



def main(map="bologna"):
    # Start SUMO simulation
    sumo_binary = sumolib.checkBinary('sumo-gui')  # Use 'sumo' for command line
    vehicle_ids = load_map(sumo_binary, map)
    stop = False
    try:
        while traci.simulation.getMinExpectedNumber() > 0:
            for vehicle_id in vehicle_ids:
                if vehicle_id not in traci.vehicle.getIDList():
                    continue

                set_autopilot(vehicle_id)
            if not stop:
                traci.simulationStep()

    except KeyboardInterrupt:
        print("Stopping simulation.")
    finally:
        traci.close()
        print("Simulation closed.")

if __name__ == "__main__":
    if len(sys.argv) > 1:
        main(sys.argv[1])
    else:
        main()
