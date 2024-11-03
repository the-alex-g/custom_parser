# BRAND CORE v.1.3.0
# additional module: wander v.1.0.0

from math import floor
import os

FUNCTIONS_REQUIRING_EXTRA_PARAMETERS = {"attack":["str"], "weapon":["str"]}
AUTOMATIC_VARIABLES = []
NEWLINE = "\\\\"
LINEBREAK = "\\bigskip"
TABLEWIDTH = 0.9
include_functions = {}
unquoted_include_functions = {}
DICE = {
    "big":[4, 6, 8, 8, 10, 10, 12, 12],
    "med":[6, 4, 6, 6, 8, 8, 10, 10],
    "sm1":[8, 10, 4, 4, 6, 6, 8, 8],
    "sm2":[8, 10, 10, 12, 6, 6, 8, 8]
}
RANGES = {
    "bow":"12/50",
    "thrown":"4/10",
    "boulder":"10/20",
    "crossbow":"15/50",
    "warbow":"25/100",
    "sling":"10/80",
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
MAJOR_DEITIES = {
    "LG":{
        "aspect":"the sun",
        "title":"Luminant",
        "name":"S'dathon"
    },
    "NG":{
        "aspect":"the moon",
        "title":"Warden",
        "name":"Auleria"
    },
    "CG":{
        "aspect":"travelers",
        "title":"Guide",
        "name":"Hvyga"
    },
    "LN":{
        "aspect":"shadow",
        "title":"Whisper in the Dark",
        "name":"Thonatoth"
    },
    "N":{
        "aspect":"knowledge",
        "title":"Librarian",
        "name":"Iolaith"
    },
    "CN":{
        "aspect":"nature",
        "title":"Wild King",
        "name":"Vethagor"
    },
    "LE":{
        "aspect":"deception",
        "title":"Weaver of Lies",
        "name":"Serath"
    },
    "NE":{
        "aspect":"decay",
        "title":"Endless Rot",
        "name":"Gaqur"
    },
    "CE":{
        "aspect":"destruction",
        "title":"Devourer",
        "name":"Sha'Immoleth"
    }
}


def plural(*name):
    string = _separate(name)
    if string[-2:] in ("sh", "ch", "x"):
        return string + "es"
    elif string[-1] in ("s", "z"):
        if string[-2] == string[-1]:
            return string + "es"
        return string + string[-1] + "es"
    return string + "s"


def index_plural(num, *name):
    if num != 1:
        return f"{num} {plural(*name)}"
    return f"{num} {_separate(name)}"


# adds all the inputs and returns them as a string
def sum(*numbers):
    total = 0
    for number in numbers:
        if type(number) == str:
            if _isdigit(number):
                total += int(number)
        elif type(number) == int:
            total += number
    return str(total)


# returns the name with the proper indefinite article prefixed to it
def articulate(capitalized, *name):
    name = _separate(name)
    string = "a"
    if name[0].lower() in ("a", "e", "i", "o", "u"):
        string += "n"
    if capitalized:
        string = string.title()
    return f"{string} {name}"


# returns spellname in italics
def spell(*spellname):
    return "\\textit{" + _separate(spellname) + "}"


# returns monster name in bold
def monster(*monstername):
    return "\\textbf{" + _separate(monstername) + "}"


def text(faces, *stuff):
    content = _separate(stuff)
    if "b" in faces:
        content = "\\textbf{" + content + "}"
    if "i" in faces:
        content = "\\textit{" + content + "}"
    if "sc" in faces:
        content = "\\textsc{" + content + "}"
    return content


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
    return f"<{_separate(stuff)}>"


# returns a table mapping the roll of a die to ampersand-separated entries
def dicetable(dice, title, *entries):
    if "d" in dice:
        dienum = int(dice[0:dice.find("d")])
        diesize = int(dice[dice.find("d") + 1:])
    else:
        diesize = dice
        dienum = 1
    if entries[0] == "h":
        print("horizontal table")
        final_entries = [f"{dienum}d{diesize}", "&"]
        x = dienum
        while x <= dienum * diesize:
            final_entries.append(x)
            final_entries.append("&")
            x += 1
        final_entries.append(title)
        final_entries.append("&")
        for item in entries[1:]:
            final_entries.append(item)
        return table("l" + "c" * (dienum * (diesize - 1) + 1), *final_entries)
    else:
        final_entries = [f"{dienum}d{diesize}", "&", title, "&", dienum, "&"]
        die_index = dienum
        for item in entries:
            final_entries.append(item)
            if item == "&":
                die_index += 1
                final_entries.append(die_index)
                final_entries.append("&")
        return table("cX", *final_entries)

        


# creates a table based on given columns and ampersand-separated entries
def table(cols, *entries):
    tablestring = NEWLINE + "\\bigskip\\begin{tabular"
    if "X" in cols:
        tablestring += "x}{" + f"{TABLEWIDTH}\\columnwidth"
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
    return tablestring + "}" + NEWLINE + LINEBREAK


# returns a bulleted list of items
def bulletlist(*items):
    return "\\begin{itemize}" + _get_list_body(items) + "\\end{itemize}"


# returns a numbered list of items 
def numberlist(*items):
    return "\\begin{enumerate}" + _get_list_body(items) + "\\end{enumerate}"


# internal function that takes a list and returns it as a list for LaTeX
def _get_list_body(items):
    list_body = "\\setlength\\itemsep{0em}"
    entry = ""
    for item in items:
        if item == "&":
            list_body += "\\item " + entry
            entry = ""
        else:
            entry += f"{item} "
    return f"{list_body}\\item {entry}"


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
        return f"+{bonus}"
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


def add_include_function(name, function):
    unquoted_include_functions[name] = function


def include(include_type, filename, *options):
    global include_functions, unquoted_include_functions

    quote = False

    if include_type in include_functions:
        content = include_functions[include_type](filename)
        quote = True
    elif include_type in unquoted_include_functions:
        content = unquoted_include_functions[include_type](filename)
    
    if "q" in options or quote:
        content = "\\begin{quote}" + content + "\\end{quote}"
    elif "n" in options:
        content = NEWLINE + LINEBREAK + content
    
    return content


def percent(*values):
    if len(values) > 0:
        return f"{sum(*values)}\\%"
    return "\\%"


def newline(*skip_size):
    if len(skip_size) > 0:
        return NEWLINE + skip(skip_size[0])
    return NEWLINE


def skip(size):
    return f"\\{size}skip "


# same as .isdigit, but accounts for negative numbers
def _isdigit(string):
    if string != "":
        if string.startswith("-"):
            return string[1:].isdigit()
        return string.isdigit()
    return False


# splits string at first space or linebreak, returns [head, rest]
def _split(string):
    first_space = string.find(" ")
    first_newline = string.find("\n")
    if first_space == -1 and first_newline == -1:
        return [string, ""]
    elif first_space == -1:
        first_break = first_newline
    elif first_newline == -1:
        first_break = first_space
    else:
        first_break = min(first_newline, first_space)
    return [string[0:first_break], string[first_break + 1:]]


def _get_string_value(string):
    if string.lower() in ("t", "true"):
        string = "True"
    elif string.lower() in ("f", "false"):
        string = "False"
    elif not _isdigit(string):
        string = f'"{string}"'
    return string


# builds and executes function from bracketed command string
def _format_and_execute(field, params):
    if field in params:
        return eval_string(str(params[field]), params)

    arg_list = []
    in_string_block = False
    arg_text = ""
    (function_name, arg_string) = _split(field)
    escaping = False
    # the extra space at the end of field causes the last argument to be processed
    for char in arg_string + " ":
        if escaping: # add the next string, regardless of what it is
            arg_text += char
            escaping = False
        elif char == "\\": # escape next character
            escaping = True
        elif char == '"':
            arg_text += '\\"'
        elif char == "<": # entering string
            in_string_block = True
        elif char == ">": # exiting string
            in_string_block = False
        elif char in (" ", "\n") and not in_string_block: # the argument is finished
            if arg_text != "": # ignore empty arguments
                if arg_text in AUTOMATIC_VARIABLES:
                    arg_text = eval_string(str(params[arg_text]), params)
                else:
                    arg_text = _get_string_value(arg_text)
                arg_list.append(arg_text)
                arg_text = ""
        else: # it's just a character
            arg_text += char
    if function_name in FUNCTIONS_REQUIRING_EXTRA_PARAMETERS:
        for param_name in FUNCTIONS_REQUIRING_EXTRA_PARAMETERS[function_name]:
            arg_list.append(eval_string(str(params[param_name]), params))

    try: # try to run custom function
        output = eval(f"{function_name}({_separate(arg_list, spacer=", ")})")
    except NameError: # if the function doesn't exist
        # the eval is necessary to un-stringify the content
        if len(arg_list) > 0:
            output = f"\\{function_name}" + "{" + eval(_separate(arg_list, spacer="}{")) + "}"
        else:
            output = f"\\{function_name}"
    return output


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


def _clamp(val, mn, mx):
    return min(mx, max(val, mn))
    

def attack(diesize, *bonuses):
    if type(diesize) == str:
        diesize = DICE[diesize][_clamp(bonuses[0], 1, 8) - 1]
    threshold = _clamp(diesize - bonuses[0] + 1, 5, diesize)
    return f"d{diesize}/{threshold}"


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


def range(weapon_type, *num_only):
    if len(num_only) == 0:
        return f"range {RANGES[weapon_type]}"
    else:
        return str(RANGES[weapon_type])


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
    if subs == 0:
        return "\\clearpage\\unnumberedsection{" + _separate(content) + "}"
    else:
        return f"\\unnumbered{"sub" * subs}section" + "{" + _separate(content) + "}"


def example(*content):
    return NEWLINE + "\\textit{Example:} " + _separate(content)


def extra_attacks(first_level, second_level):
    return f"""You get extra attacks based on your Spd bonus. If your Spd bonus
    is {format_bonus(first_level)} or greater, on every round that you don't make
    an attack, gain a boon that allows you to make a second melee attack when you
    make a melee attack. If your Spd bonus is {format_bonus(second_level)} or greater,
    you may make a second melee attack after the first time that you make a melee
    attack on your turn."""


def deity(field, alignment):
    return MAJOR_DEITIES[alignment][field]

# MOVE THIS TO CORE
def qmark(*what):
    return f'"{_separate(what)}"'
