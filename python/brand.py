# BRAND CORE v.1.1.0
from math import floor
import os
FUNCTIONS_REQUIRING_EXTRA_PARAMETERS = {"attack":["str"], "weapon":["str"]}
AUTOMATIC_VARIABLES = []
RANGES = {
    "bow":"12/50",
    "thrown":"5/10",
    "boulder":"10/20"
}
WEAPONS = {
    "bow":{
        "die":6,
        "dtype":"piercing"
    },
    "dagger":{
        "die":4,
        "dtype":"piercing"
    },
    "longsword":{
        "die":8,
        "dtype":"slashing"
    },
    "shortsword":{
        "die":6,
        "dtype":"slashing"
    },
    "waraxe":{
        "die":10,
        "dtype":"slashing"
    }
}
NEWLINE = "\\\\"
LINEBREAK = "\\bigskip"
include_functions = {}


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


def index_plural(num, *name):
    name = _separate(name)
    string = str(num) + " " + name
    if num != 1:
        if name[-2:] in ["sh", "ch", "x"]:
            return string + "es"
        elif name[-1] in ["s", "z"]:
            if name[-2] == name[-1]:
                return string + "es"
            return string + name[-1] + "es"
        return string + "s"
    return string


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
        tablestring += "x}{0.9\\columnwidth"
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
                tablestring += "\\\\[2pt]\\hline "
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
    if type(bonus) == str:
        bonus = int(bonus)
    if bonus >= 0:
        return "+" + str(bonus)
    else:
        return str(bonus)


# returns string of number with proper ordinary suffix
def format_index(index):
    index = str(index)
    if not (len(index) == 2 and index[0] == "1"):
        if index[-1] == "1":
          return index + "st"
        elif index[-1] == "2":
          return index + "nd"
        elif index[-1] == "3":
            return index + "rd"
    return index + "th"


# returns string with proper possessive suffix
def possessive(*name):
    name = _separate(name)
    if name[-1] == "s":
        return name + "'"
    else:
        return name + "'s"


def include(include_type, quote, *filename):
    global include_functions

    filename = filename[0] if len(filename) > 0 else ""

    content = include_functions[include_type](filename)
    if quote:
        string = "\\begin{quote}" + content + "\\end{quote}"
    else:
        string = NEWLINE + LINEBREAK + content
    return string


def percent(*value):
    return sum(*value) + "\\%"


def newline(*skip_size):
    if len(skip_size) > 0:
        return NEWLINE + skip(skip_size[0])
    return NEWLINE


def skip(size):
    return f"\\{size}skip"


# builds and executes function from bracketed command string
def _format_and_execute(field, params):
    if field in params:
        return eval_string(str(params[field]), params)

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
        elif (char == " " or char == "\n") and not in_string_block:
            if in_function_body:
                if formatted_field[-1] != "(":
                    formatted_field += ", "
                if arg_text in AUTOMATIC_VARIABLES:
                    arg_text = eval_string(str(params[arg_text]), params)
                elif arg_text in ["t", "T", "true", "True"]:
                    arg_text = "True"
                elif arg_text in ["f", "F", "false", "False"]:
                    arg_text = "False"
                elif not arg_text.isdigit():
                    arg_text = '"' + arg_text + '"'
                formatted_field += arg_text
                arg_text = ""
            else:
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
            formatted_field += eval_string(str(params[param_name]), params)
    return eval(formatted_field + ")")


# processes a string to extract and execute all bracketed command sequences
def eval_string(string, params):
    updated_string = ""
    field = ""
    field_started = False
    indentation_level = 0
    nested = False
    in_comment = False
    for char in string:
        if char == "#":
            in_comment = not in_comment
        elif not in_comment:
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
                        field = eval_string(field, params)
                    updated_string += _format_and_execute(field, params)
                    field = ""
                    nested = False
            elif field_started:
                field += char
            else:
                updated_string += char
    return updated_string

#-------------------------------------------------------------------

def attack(diesize, *bonuses):
    threshold = diesize
    if bonuses[0] > 1:
        threshold -= bonuses[0] - 1
    return f"d{diesize}/{min(diesize, max(threshold, 5))}"


def weapon(weapon_name, *bonuses):
    data = WEAPONS[weapon_name]
    string = f"{attack(data["die"], *bonuses)} {data["dtype"]} ({weapon_name}"
    if weapon_name in RANGES:
        string += f", {range(weapon_name)}"
    return f"{string})"


def check(stat, *bonuses):
    total_difficulty = 0
    for i in bonuses:
        total_difficulty += _get_difficulty(i)
    return stat.title() + " " + str(total_difficulty) + " check"


def _get_difficulty(difficulty):
    if type(difficulty) == str:
        if difficulty == "easy":
            return 10
        elif difficulty == "med":
            return 14
        elif difficulty == "hard":
            return 18
    else:
        return difficulty


def difficulty(diff):
    return str(_get_difficulty(diff))


def range(weapon_type):
    return "range " + RANGES[weapon_type]


def recharge(value, *config):
    string = f"recharge {value}"
    if "c" in config:
        string = string.capitalize()
    if "p" in config:
        string = f"({string})"
    return string


def illuminates(bright, *dim):
    string = "sheds light (" + str(bright)
    if len(dim) > 0:
        if dim[0] == 0:
            return string + ")"
        return string + "/" + str(dim[0]) + ")"
    return string + "/" + str(bright * 2) + ")"


def section(subs, *content):
    return f"\\unnumbered{"sub" * subs}section" + "{" + _separate(content) + "}"


def example(*content):
    return NEWLINE + "\\textit{Example:} " + _separate(content)