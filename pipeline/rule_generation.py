#
# Rule Generation
# 
import re

from llm import api_call
from dotenv import load_dotenv
from collections import OrderedDict

import logging
from datetime import datetime
from pathlib import Path

timestamp = datetime.now().strftime("%Y-%m-%d_%H.%M.%S")
script_name = Path(__file__).stem
log_filename = f"logs/{script_name}_{timestamp}.log"

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    handlers=[logging.FileHandler(log_filename), logging.StreamHandler()],
    force=True  # This will remove and re-add handlers, ensuring your config is applied.
)
logging.info("Logging setup complete.")


def parse_markdown_rules(markdown_text: str) -> dict:
    # Use OrderedDict to maintain the original order of sections and rules
    parsed_data = OrderedDict()
    current_section_key = None
    current_rule_key = None
    current_content = []

    lines = markdown_text.strip().split('\n')

    for i, line in enumerate(lines):
        stripped_line = line.strip()

        # Identify a new section (H2, underlined with '---' or '===')
        # A section is a line of text followed by a line of --- or ===
        is_h2 = (i + 1 < len(lines)) and re.match(r'^-{3,}$|^={3,}$', lines[i+1].strip())
        
        if is_h2:
            # If we were processing a rule, save it before starting a new section
            if current_section_key and current_rule_key:
                parsed_data[current_section_key][current_rule_key] = "\n".join(current_content).strip()

            # Start a new section
            current_section_key = stripped_line
            # Use an OrderedDict for the inner dictionary as well
            parsed_data[current_section_key] = OrderedDict()
            current_rule_key = None
            current_content = []
            continue

        # Skip the separator lines ('---' or '===')
        if re.match(r'^-{3,}$|^={3,}$', stripped_line):
            continue

        # Identify a new rule (H3)
        if stripped_line.startswith('### '):
            # If we were already processing a rule, save its content first
            if current_section_key and current_rule_key:
                parsed_data[current_section_key][current_rule_key] = "\n".join(current_content).strip()

            # Start the new rule
            current_rule_key = stripped_line.replace('###', '').strip()
            current_content = []
            continue

        # Append content lines to the current rule
        if current_section_key and current_rule_key:
            current_content.append(line)

    # Save the last rule after the loop has finished
    if current_section_key and current_rule_key:
        parsed_data[current_section_key][current_rule_key] = "\n".join(current_content).strip()

    # Filter out any top-level sections that were captured but contain no rules
    # (e.g., the main document title)
    return OrderedDict((k, v) for k, v in parsed_data.items() if v)


def translate_to_le(source_text:str, previous_le:str) -> str:

    #1 Fill the user prompt with the source text and previous LE
    system_prompt_generator = open("prompts/system_step-by-step_pipeline.md").read().strip()
    user_prompt_generator = open("prompts/user_prompt-conversion.md").read().strip()

    user_prompt_generator = user_prompt_generator.replace("@SOURCE_TEXT", source_text)
    user_prompt_generator = user_prompt_generator.replace("@PREVIOUS_TEMPLATES", previous_le)

    #2 Send to the API
    response = api_call(
        system_prompt=system_prompt_generator,
        user_prompt=user_prompt_generator
    )

    logging.info(f"Response: \n\n====\n{response}\n=======")

    #3 Get the generate LE.
    

    #4 While not validated:
    #4.1 Send to LE compiler
    #4.2 If error, get the error description
    #4.2.1 Fill the user propmt for the syntax check    
    #4.3 Generate again.
   

def process_existing_le(target):
    """
    Processes a Logical English string to separate templates and a knowledge base.

    Returns two strings:
    1.  The templates section, starting with its original heading.
    2.  The knowledge base section, starting with its original heading and
        followed only by the rules that were flagged with '% LLM'.
    """

    def extract_rules_from_le_for_llm(le_string: str) -> list[str]:
        """
        Helper function to extract only the blocks of text marked with '% LLM'.
        """
        results = []
        blocks = re.split(r'\n\s*\n', le_string.strip())
        for block in blocks:
            if '% LLM' in block:
                cleaned_block = re.sub(r'^\s*% LLM\s*\n?', '', block, flags=re.MULTILINE).strip()
                if cleaned_block:
                    results.append(cleaned_block)
        return results

    # --- Main function logic ---

    relevant_portion = target.split("prolog.")[-1]

    # 1. Isolate the relevant part of the input string
    relevant_portion = relevant_portion.split("% LLM_Finish")[0].strip()
    
    # 2. Use a regex to find the actual knowledge base heading
    # This looks for a line like "the knowledge base <name> is:", with an optional '%'
    kb_heading_pattern = re.compile(r"^\s*(?:% *)?the knowledge base.*includes:", re.IGNORECASE | re.MULTILINE)
    match = kb_heading_pattern.search(relevant_portion)

    if match:
        # If the heading is found, get its start position
        kb_start_index = match.start()
        
        # Slice the document at the correct position
        existing_templates = relevant_portion[:kb_start_index].replace("% Knowledge Base", "").strip()
        knowledge_base = relevant_portion[kb_start_index:].strip()
    else:
        # If no KB heading is found, assume everything is templates
        existing_templates = relevant_portion
        knowledge_base = ""
    
    # 3. Process the knowledge base to construct the final string
    if knowledge_base:
        # Capture the heading from the first line of the correctly sliced KB
        first_newline_pos = knowledge_base.find('\n')
        kb_heading = knowledge_base[:first_newline_pos].strip() if first_newline_pos != -1 else knowledge_base.strip()
        
        # Extract the LLM rules
        llm_rules_list = extract_rules_from_le_for_llm(knowledge_base)
        rules_text = "\n\n".join(llm_rules_list)
        
        # Combine the true heading with the processed rules
        knowledge_base_for_llm = f"{kb_heading}\n\n{rules_text}"
    else:
        knowledge_base_for_llm = ""

    existing_templates = existing_templates.split("% Templates")[-1].strip()
    knowledge_base_for_llm = knowledge_base_for_llm.replace("% LLM", "").strip()

    return existing_templates, knowledge_base_for_llm

if __name__ == "__main__":

    rules_to_ignore = [
        "Rule 170",
        "Rule 171",
        "Rule 172",
        "Rule 175"
    ]
    load_dotenv()

    generator_llm_options = {
        "model": "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free",
        "temperature": 0.7,
        "top_p": 0.0,
        "top_k": 20,
        "max_new_tokens": 5000,
        "stream": False,
    }

    markdown_dataset = open("../Using the road (159 to 203).md").read().strip()

    existing_logical_english = open("../highway_code.le").read().strip()
    existing_templates, existing_kb = process_existing_le(existing_logical_english)
    
    existing_le= "\n\n".join([existing_templates, existing_kb])
    logging.info(f"-----\n{existing_templates}\n=====\n{existing_kb}")

    parsed_source_text = parse_markdown_rules(markdown_dataset)
    junction_rules = parsed_source_text["Road junctions (rules 170 to 183)"]    

    for rule, source_text in junction_rules.items():        
        if len(source_text.strip()) == 0 or rule.strip() in rules_to_ignore:
            continue
        
        logging.info("-"*50)
        logging.info(f"Processing: {rule}")
        logging.info("-"*50)
        
        content = translate_to_le(
            source_text=source_text,
            previous_le=existing_logical_english
        )

        input("\n\nPress a key to continue...")

    """
    translate_to_le(
        source_text=source_text_sample,
        previous_le=previous_logical_english
    )
    """