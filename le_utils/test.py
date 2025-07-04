import re
import janus_swi as janus
import json
import sys

"""LE Test Module
This module provides functions to test Logical English (LE) files against a knowledge base and scenarios.
If no log file is provided, it will only check the LE file.

To run the tests via the cli, use the following command:

python -m le_utils.test <le_file> (<log_file>)

To run the tests programmatically, you can use the `scenario_test` function with the LE file and the log file:

from le_utils.test import scenario_test, load_files
le_src, content = load_files("path/to/le_file.le", "path/to/log_file.log or None")
result = scenario_test(le_src=le_src, content=content)
"""

templates_regex = r"the templates are:\n(.+)\nthe knowledge base"
kb_regex = r"the knowledge base.+?\n(.+?)scenario .+? is"
# scenario_regex = r"Scenario:\*\*\n```le\n(scenario (.+?) [^`]+)```\n.+?Query:\*\*\n```le\n(query (.+?) [^`]+)```\n.+?Results:\*\*\n```le\n([^`]+)```"
scenario_regex = r"Templates created:\*\* ?(\n```le\n(.+? [^`]+)```|None)\n.+?Scenario:\*\*\n```le\n(scenario (.+?) [^`]+)```\n.+?Query:\*\*\n```le\n(query (.+?) [^`]+)```\n.+?Results:\*\*\n```le\n([^`]+)```"

builtin_templates = [
    ".+ is .+"
]

le_src, content = None, None

# Load the LE source file and the log file
def load_files_argv():
    # Load the log file and the LE source file
    kb_file = sys.argv[1]

    if len(sys.argv) == 3:
        log_file = sys.argv[2]

        with open(log_file, "r") as file:
            content = file.read()
    else:
        content = None

    with open(kb_file, "r") as file:
        le_src = file.read()
    return le_src, content

def load_files(kb_file, log_file=None):
    if log_file:
        with open(log_file, "r") as file:
            content = file.read()
    else:
        content = None
    with open(kb_file, "r") as file:
        le_src = file.read()
    return le_src, content

def scenario_test(le_src=le_src, content=None):
    output = {
        "status": "success",
        "message": "",
        "errors": {
            "kb": [],
            "scenario": []
        }
    }

    if content:
        matches = re.findall(scenario_regex, content, re.DOTALL)
        match = matches[0]
        templates_parse, templates, scenario, scenario_id, query, query_id, _ = match

        if templates_parse:
            # print("Templates found in log file, parsing...")
            le_src = le_src.replace("the knowledge base", templates + "\n\nthe knowledge base")

        # print("Testing new scenario and query...")
        le_src += f"""
\n{scenario}
\n{query}
"""
    else:
        scenario = ""
        
    janus.consult("""logicalenglish/prolog/le_answer.pl""")

    kb_errors = check_templates(le_src=le_src)
    scenario_errors = check_scenario(le_src=le_src, scenario=scenario)

    # if kb_errors:
    #     print("\nErrors in LE lines:")
    #     for error in kb_errors:
    #         print(error)
    
    # if scenario_errors:
    #     print("\nErrors in scenario lines:")
    #     for error in scenario_errors:
    #         print(error)

    if kb_errors or scenario_errors:
        # print("Errors found in LE lines or scenario lines. Cannot proceed with query.")
        output["status"] = "Syntax Error"
        output["errors"] = {
            "kb": kb_errors,
            "scenario": scenario_errors
        }
    elif content:
        # print("No errors found in LE lines or scenario lines. Proceeding with query...")
        # add_templates(le_src, )
        # print(le_src)
        print(f"Querying: {query_id} with scenario: {scenario_id}")
        result = (janus.query_once(f"""parse_and_query_and_explanation_text('le_rules', en("{le_src}"), {query_id}, with({scenario_id}), R, E)""")["R"])
        query_str = query.splitlines()[-1].strip().strip(".")  # Remove the first line which is the query name
        if json.loads(result)[0] != query_str:
            output["status"] = "Scenario Error"
        output["message"] = result
        #TODO add output error
    else:
        output["message"] = "No log file provided, skipping query execution."
    return output


def parse_templates(le_src=le_src):
    parsed_templates = set()
    templates = re.search(templates_regex, le_src, re.DOTALL).group(1).strip().splitlines()
    for i in templates:
        parsed_templates.add(re.sub(r"\*.+?\*", ".+", i).rstrip("."))
    parsed_templates = sorted(parsed_templates, key=len, reverse=True)  # Sort by length, longest first
    parsed_templates = builtin_templates + parsed_templates
    return parsed_templates

def parse_le_lines(le_src=le_src):
    parsed_le_lines = set()
    kb = re.search(kb_regex, le_src, re.DOTALL).group(1).strip().splitlines()
    for i in kb:
        if i.startswith("%"):
            continue
        parsed_le_lines.add(i.strip().removeprefix("and ").removeprefix("or ").removeprefix("if "))
    return parsed_le_lines

# templates()

def check_templates(le_src=le_src):
    print("Checking templates against Knowledge Base (Rules) lines...")
    errors = []
    le_lines = parse_le_lines(le_src)
    templates = parse_templates(le_src)

    for i in le_lines:
        if not i:
            continue
        if i.startswith("%"):
            continue
        for j in templates:
            if re.match(j, i):
                # print(f"Match found: {i} matches {j}")
                break
        else:
            errors.append(f"No template found for: {i}")
    return errors

def check_scenario(le_src, scenario):
    print("Checking tempates against scenario lines...")
    errors = []
    templates = parse_templates(le_src)
    scenario = [x.strip() for x in scenario.splitlines()][1:]

    for i in scenario:
        if not i:
            continue
        if i.startswith("%"):
            continue
        for j in templates:
            if re.match(j, i):
                # print(f"Match found: {i} matches {j}")
                break
        else:
            errors.append(f"No template found for: {i}")
    return errors

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python test/test.py <le_file> <log_file>")
        sys.exit(1)
    le_src, content = load_files_argv()
    print(scenario_test(le_src=le_src, content=content))

def test_scenario():
    le_src, content = load_files("highway_code.le")
    print(scenario_test(le_src=le_src, content=content))