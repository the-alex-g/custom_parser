# BRAND CORE v.1.0
from math import floor
import os
FUNCTIONS_REQUIRING_EXTRA_PARAMETERS = {}
AUTOMATIC_VARIABLES = ["str", "con", "dex", "spd", "lor", "ins", "cha", "det"]
NEWLINE = "\\\\"
LINEBREAK = "\\bigskip"


# returns in the form of "result (dice + bonus)"
def roll(num, size, *bonuses):
    total_bonus = 0
    for bonus in bonuses:
        total_bonus += bonus
    total = floor(num * (size / 2 + 0.5)) + total_bonus
    string = str(total) + " (" + str(num) + "d" + str(size)
    if total_bonus != 0:
        if total_bonus > 0:
            string += " + "
        else:
            string += " - "
        string += str(abs(total_bonus))
    return string + ")"


# adds all the inputs and returns them as a string
def sum(*numbers):
    total = 0
    for number in numbers:
        total += number
    return str(total)


# returns the name with the proper indefinite article prefixed to it
def articulate(capitalized, *name):
    name = _separate(name)
    string = "a"
    if name[0].lower() in ["a", "e", "i", "o", "u"]:
        string += "n"
    if capitalized:
        string = string.title()
    return string + " " + name


# returns spellname in italics
def spell(*spellname):
    return "\\textit{" + _separate(spellname) + "}"


# returns monster name in bold
def monster(*monstername):
    return "\\textbf{" + _separate(monstername) + "}"


# bolds input
def bold(*stuff):
    return "\\textbf{" + _separate(stuff) + "}"


# italicizes input
def italics(*stuff):
    return "\\textit{" + _separate(stuff) + "}"


# makes input bold and italic
def bolditalics(*stuff):
    return "\\textbf{\\textit{" + _separate(stuff) + "}}"


# collects all parameters as a brand-recognized string group
def bind(*stuff):
    return "<" + _separate(stuff) + ">"


# returns a table mapping the roll of a die to ampersand-separated entries
def dicetable(diesize, title, *entries):
    final_entries = ["1d" + str(diesize), "&", title, "&", 1, "&"]
    die_index = 1
    for item in entries:
        final_entries.append(item)
        if item == "&":
            die_index += 1
            final_entries.append(die_index)
            final_entries.append("&")
    return table("cX", final_entries)


# creates a table based on given columns and ampersand-separated entries
def table(cols, *entries):
    if type(entries[0]) == list:
        entries = entries[0]
    tablestring = NEWLINE + "\\bigskip\\begin{tabular"
    if "X" in cols:
        tablestring += "x}{0.8\\columnwidth"
    tablestring += "}{|"
    for char in cols:
        tablestring += char + "|"
    tablestring += "}\\hline "
    line_count = 0
    for item in entries:
        if item == "&":
            line_count += 1
            if line_count < len(cols):
                tablestring += "&"
            else:
                tablestring += NEWLINE + "[2pt]\\hline "
                line_count = 0
        else:
             tablestring += str(item) + " "
    tablestring += NEWLINE + "\\hline\\end{tabular"
    if "X" in cols:
        tablestring += "x"
    return tablestring + "}"


# returns a bulleted list of items
def bulletlist(*items):
    return NEWLINE + "\\begin{itemize}" + _get_list_body(items) + "\\end{itemize}"


# returns a numbered list of items 
def numberlist(*items):
    return NEWLINE + "\\begin{enumerate}" + _get_list_body(items) + "\\end{enumerate}"


# internal function that takes a list and returns it as a list for LaTeX
def _get_list_body(items):
    list_body = ""
    entry = ""
    for item in items:
        if item == "&":
            list_body += "\\item " + entry
            entry = ""
        else:
            entry += str(item) + " "
    return list_body + "\\item " + entry


# Return array as string with each item separated by a given sequence
def _separate(array, spacer=" "):
    string = ""
    for item in array:
        if string != "":
            string += spacer
        string += str(item)
    return string


# Returns a number as a string with proper sign indication
def format_bonus(bonus):
    if bonus >= 0:
        return "+" + str(bonus)
    else:
        return str(bonus)


# returns string of number with proper ordinary suffix
def format_index(index):
    index = str(index)
    if index[-1] == 1:
        return "1st"
    elif index[-1] == 2:
        return "2nd"
    elif index[-1] == 3:
        return "3rd"
    else:
        return str(index) + "th"


# returns string with proper possessive suffix
def possessive(*name):
    name = _separate(name)
    if name[-1] == "s":
        return name + "'"
    else:
        return name + "'s"


# builds and executes function from bracketed command string
def _format_and_execute(field, params):
    formatted_field = ""
    in_function_body = False
    in_string_block = False
    arg_text = ""
    function_name = ""
    # the extra space at the end of field causes the last argument to be processed
    for char in field + " ":
        if char == "<":
            in_string_block = True
        elif char == ">":
            in_string_block = False
        elif char == " " and not in_string_block:
            if in_function_body:
                if formatted_field[-1] != "(":
                    formatted_field += ", "
                if arg_text in AUTOMATIC_VARIABLES:
                    arg_text = str(params[arg_text])
                elif not arg_text.isdigit() and not arg_text == "True" and not arg_text == "False":
                    arg_text = "\"" + arg_text + "\""
                formatted_field += arg_text
                arg_text = ""
            elif formatted_field != field:
                formatted_field += "("
                in_function_body = True
        else:
            if in_function_body:
                arg_text += char
            else:
                formatted_field += char
                function_name += char
    if function_name in FUNCTIONS_REQUIRING_EXTRA_PARAMETERS:
        for param_name in FUNCTIONS_REQUIRING_EXTRA_PARAMETERS[function_name]:
            if formatted_field[-1] != "(":
                formatted_field += ", "
            formatted_field += str(params[param_name])
    return eval(formatted_field + ")")


# processes a string to extract and execute all bracketed command sequences
def _resolve_functions(string, params):
    updated_string = ""
    field = ""
    field_started = False
    indentation_level = 0
    nested = False
    for char in string:
        if char == "[":
            if field_started:
                indentation_level += 1
                nested = True
                field += "["
            else:
                field_started = True
        elif char == "]":
            if indentation_level > 0:
                indentation_level -= 1
                field += "]"
            else:
                field_started = False
                if nested:
                    field = _resolve_functions(field, params)
                updated_string += _format_and_execute(field, params)
                field = ""
                nested = False
        elif field_started:
            field += char
        else:
            updated_string += char
    return updated_string


# processes all bracketed commands in the given string
def eval_string(string, params):
    for key in params:
        string = string.replace("[" + key + "]", str(params[key]))
    return _resolve_functions(string, params)

##################################################################################