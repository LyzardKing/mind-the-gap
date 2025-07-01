#
# Rule Generation
# 

def translate_to_le(source_text:str, previous_le:str) -> str:

    #1 Fill the user prompt with the source text and previous LE
    #2 Send to the API
    #3 Get the generate LE.
    #4 While not validated:
    #4.1 Send to LE compiler
    #4.2 If error, get the error description
    #4.2.1 Fill the user propmt for the syntax check
    #4.3 Generate again.

    pass




if __name__ == "__main__":
    source_text_sample = open("source_text/Rule176.txt").read().strip()
    previous_logical_english= "??"


    translate_to_le(
        source_text=source_text_sample,
        previous_le=previous_logical_english
    )