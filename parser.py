import yaml
import parser_utility as pp
import os

SCORE_TO_BONUS = {
    1:-4,
    2:-3,
    3:-2,
    4:-2,
    5:-1,
    6:-1,
    7:0,
    8:0,
    9:0,
    10:0,
    11:1,
    12:1,
    13:2,
    14:2,
    15:3,
    16:4
}
SIZE_NUMBERS = {
    "v":16,
    "h":8,
    "l":4,
    "m":2,
    "s":1,
    "t":0.5
}
SIZE_SPELLOUTS = {
    "v":"vast",
    "h":"huge",
    "l":"large",
    "m":"medium",
    "s":"small",
    "t":"tiny"
}
ALIGNMENT_SPELLOUTS = {
    "ce":"chaotic evil",
    "ne":"neutral evil",
    "u":"unaligned",
}
EVD_SIZE_MOD = {
    "v":-4,
    "h":-2,
    "l":-1,
    "m":0,
    "s":1,
    "t":2
}
STR_SIZE_MOD = {
    "v":4,
    "h":2,
    "l":1,
    "m":0,
    "s":-1,
    "t":-2
}
ABILITIES = ["str", "dex", "con", "lor", "ins", "cha"]
ATTACK_LIST_OPERATORS = ["AND", "OR", "and", "or"]
NEWLINE = "\\\\"
LINEDIVIDE = "\\linedivide"
LINEBREAK = "\\bigskip"
PREAMBLE = """\\documentclass[letterpaper, 12pt, twocolumn]{book}
\\usepackage{ragged2e}
\\usepackage[left=0.5in, right=0.5in, top=1in, bottom=1in]{geometry}
\\usepackage{graphicx}
\\def\\halfline{\\makebox[\\columnwidth]{\\rule{3.7in}{0.4pt}}\\\\}
\\def\\linedivide{, }
\\def\\sectionheader#1{\\textbf{#1: }}
\\begin{document}\\RaggedRight"""
CONCLUSION = "\\end{document}"


def calculate_health(size, con, hardness):
    return str((6 + con + hardness) * SIZE_NUMBERS[size])


def bold(string):
    return "\\textbf{" + string + "}"


def sectionheader(string):
    return "\\sectionheader{" + string + "}"


def create_title_line(monster):
    name = pp.headername(monster).upper()
    size = SIZE_SPELLOUTS[monster["size"]]
    monster_type = monster["type"]
    alignment = ALIGNMENT_SPELLOUTS[pp.get_key_if_exists(monster, "alignment", "u")]
    if "tags" in monster:
        monster_type += " (" + pp.comma_separate(monster["tags"]) + ")"
    return bold(name) + "---\\key{" + size + " " + monster_type + ", " + alignment + "}"


def create_stat_block(bonuses, size, hardness):
    string = ""
    linelength = 0
    if STR_SIZE_MOD[size] != 0 and not "str" in bonuses:
        bonuses["str"] = 0
    if hardness != 0 and not "con" in bonuses:
        bonuses["con"] = 0
    for ability in ABILITIES:
        if ability in bonuses:
            if len(string) > 100 + linelength:
                string += NEWLINE
                linelength = len(string)
            elif string != "":
                string += LINEDIVIDE
            string += sectionheader(ability.title())
            special_bonus = 0
            if ability == "str" and STR_SIZE_MOD[size] != 0:
                special_bonus = STR_SIZE_MOD[size]
            elif ability == "con" and hardness != 0:
                special_bonus = hardness
            string += pp.format_bonus(bonuses[ability])
            if special_bonus != 0:
                string += " (" + pp.format_bonus(bonuses[ability] + special_bonus) + ")"
    return string
            


def create_attack_list(attacks):
    string = ""
    operator = ""
    for attack in attacks:
        if string != "":
            string += " " + operator + " "
        if attack in ATTACK_LIST_OPERATORS:
            operator = attack
        else:
            string += attack
    return string


def create_evd(dex_bonus, evd_bonus, size):
    string = ""
    evd_bonus += EVD_SIZE_MOD[size]
    if evd_bonus != 0:
        string += pp.format_bonus(evd_bonus) + " ("
    string += str(10 + dex_bonus + evd_bonus)
    if evd_bonus != 0:
        string += ")"
    return string


def get_dmg_attributes(dmg_attributes):
    return pp.comma_separate(sorted(dmg_attributes))


def create_special_block(specials):
    string = ""
    for special in specials:
        if string != "":
            string += NEWLINE
        string += special
    return string


def create_key_list(keys):
    return "\\key{" + pp.comma_separate(keys, linelength=100, offset=5) + "}"


def create_monster(monster):
    bonuses = monster["bonuses"]
    size = monster["size"]
    hardness = pp.get_key_if_exists(monster, "hardness", 0)
    string = create_title_line(monster) + NEWLINE
    string += create_stat_block(bonuses, size, hardness) + NEWLINE
    string += sectionheader("Health") + calculate_health(
        size,
        pp.get_key_if_exists(bonuses, "con", 0),
        hardness) + LINEDIVIDE
    string += sectionheader("Arm") + str(pp.get_key_if_exists(monster, "armor", 0) + hardness) + LINEDIVIDE
    string += sectionheader("Evd") + create_evd(pp.get_key_if_exists(bonuses, "dex", 0), pp.get_key_if_exists(bonuses, "evd", 0), size) + LINEDIVIDE
    string += sectionheader("Spd") + str(monster["speed"]) + NEWLINE

    if "immune" in monster:
        string += sectionheader("Immune") + get_dmg_attributes(monster["immune"]) + NEWLINE

    if "resist" in monster:
        string += sectionheader("Resist") + get_dmg_attributes(monster["resist"]) + NEWLINE

    if "vulnerable" in monster:
        string += sectionheader("Vulnerable") + get_dmg_attributes(monster["vulnerable"]) + NEWLINE

    if "attack" in monster:
        string += sectionheader("Attack") + create_attack_list(monster["attack"]) + NEWLINE

    if "keys" in monster:
        string += sectionheader("Keys") + create_key_list(monster["keys"]) + NEWLINE

    if "special" in monster:
        string += sectionheader("Special") + create_special_block(monster["special"]) + NEWLINE
    return string + LINEBREAK


def create_doc():
    latexfile = open("monsters.tex", "w")
    latexfile.write(get_latex_file_contents("tex/preamble.tex"))
    
    monster_name_dict= {}
    for monster in get_yaml_from_directory("monsters"):
        monster_name_dict[monster["name"]] = monster
    for monster_name in sorted(monster_name_dict):
        latexfile.write(create_monster(monster_name_dict[monster_name]))
    
    latexfile.write(get_latex_file_contents("tex/conclusion.tex"))
    latexfile.close()


def open_yaml(filepath):
    print("loading", filepath)
    with open(filepath) as stream:
        try:
            return yaml.safe_load(stream)
        except yaml.YAMLError as err:
            print(err)


def get_yaml_from_directory(dirname):
    yaml_list = []
    if os.path.isdir(dirname):
        for item in os.listdir(dirname):
            path = os.path.join(dirname, item)
            if os.path.isfile(path):
                yaml_list.append(open_yaml(path))
            elif os.path.isdir(path):
                for yaml_dict in get_yaml_from_directory(path):
                    yaml_list.append(yaml_dict)
    else:
        print("ERROR:", dirname, "is not a directory!")
    return yaml_list


def get_latex_file_contents(filename):
    return pp.separate(open(filename).readlines())


create_doc()
