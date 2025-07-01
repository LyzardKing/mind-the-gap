from together import Together


def api_call(system_prompt=None, user_prompt="", options=None):
    if options is None:
        options = {}

    # Initialize client
    client = Together()
    
    # Build message list
    messages = []
    if system_prompt:
        messages.append({"role": "system", "content": system_prompt})
    messages.append({"role": "user", "content": user_prompt})
    
    try:
        # Send API call with dynamic options
        response = client.chat.completions.create(
            model=options.get("model", "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free"),
            messages=messages,
            **{k: v for k, v in options.items() if k != "model"}  # Pass all other options
        )
        return response.choices[0].message.content
    except Exception as e:
        return f"Error: {str(e)}"


def read_file(path_to_file, encoding="utf-8"):
    try:
        with open(path_to_file, "r", encoding=encoding) as file:
            content = file.read().strip()
        return content
    except FileNotFoundError:
        return f"Error: File not found at {path_to_file}"
    except PermissionError:
        return f"Error: Permission denied for file at {path_to_file}"
    except UnicodeDecodeError:
        return f"Error: Unable to decode file using {encoding}"
    except Exception as e:
        return f"Error: {str(e)}"
