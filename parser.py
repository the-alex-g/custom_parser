import yaml
import parser_utility as pp

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
ATTACK_LIST_OPERATORS = ["AND", "OR", "and", "or"]
NEWLINE = "\\\\"
PREAMBLE = """\\documentclass[letterpaper, 12pt, twocolumn]{book}
\\usepackage{ragged2e}
\\usepackage[left=0.5in, right=0.5in, top=1in, bottom=1in]{geometry}
\\usepackage{graphicx}
\\def\\halfline{\\makebox[\\columnwidth]{\\rule{3.7in}{0.4pt}}\\\\}
\\def\protlinesep{, }
\\def\sectionheader#1{\\textsc{#1: }}
\\begin{document}\\RaggedRight"""
CONCLUSION = "\\end{document}"


def calculate_health(size, con):
    return str((6 + con) * SIZE_NUMBERS[size])


def bold(string):
    return "\\textbf{" + string + "}"


def get_bonuses(scores):
    bonuses = {}
    for stat in scores:
        bonuses[stat] = SCORE_TO_BONUS[scores[stat]]
    return bonuses


def create_title_line(monster):
    name = pp.headername(monster).upper()
    size = SIZE_SPELLOUTS[monster["size"]]
    monster_type = monster["type"]
    alignment = ALIGNMENT_SPELLOUTS[pp.get_key_if_exists(monster, "alignment", "u")]
    if "tags" in monster:
        monster_type += " (" + pp.comma_separate(monster["tags"]) + ")"
    return bold(name) + "---" + size + " " + monster_type + ", " + alignment


def format_score(stat, score, size):
    string = stat.upper()
    special = 0
    if stat == "str":
        special = STR_SIZE_MOD[size]
    string += " " + str(score)
    if special != 0:
        string += pp.format_bonus(special)
    string += " (" + pp.format_bonus(SCORE_TO_BONUS[score] + special) + ")"
    return string


def create_stat_block(scores, size):
    string = "\\begin{footnotesize}\\begin{tabular}{@{}lll}"
    for stat in scores:
        string += format_score(stat, scores[stat], size)
        if stat == "con":
            string += NEWLINE
        elif stat != "cha":
            string += "&"
    return string + "\\end{tabular}\\end{footnotesize}"


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


def create_evd(dex_bonus, size):
    string = ""
    if EVD_SIZE_MOD[size] != 0:
        string += pp.format_bonus(EVD_SIZE_MOD[size]) + " ("
    string += str(10 + dex_bonus + EVD_SIZE_MOD[size])
    if EVD_SIZE_MOD[size] != 0:
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


def create_monster(monster):
    scores = monster["scores"]
    bonuses = get_bonuses(scores)
    size = monster["size"]
    string = create_title_line(monster) + NEWLINE
    string += create_stat_block(scores, size) + NEWLINE
    string += "\\sectionheader{Health}" + calculate_health(size, bonuses["con"]) + "\\protlinesep"
    string += "\\sectionheader{Arm}" + str(pp.get_key_if_exists(monster, "armor", 0)) + "\\protlinesep"
    string += "\\sectionheader{Evd}" + create_evd(bonuses["dex"], size) + "\\protlinesep"
    string += "\\sectionheader{Spd}" + str(monster["speed"]) + NEWLINE

    if "immune" in monster:
        string += "\\sectionheader{Immune}" + get_dmg_attributes(monster["immune"]) + NEWLINE

    if "resist" in monster:
        string += "\\sectionheader{Resist}" + get_dmg_attributes(monster["resist"]) + NEWLINE

    if "vulnerable" in monster:
        string += "\\sectionheader{Vulnerable}" + get_dmg_attributes(monster["vulnerable"]) + NEWLINE

    if "attack" in monster:
        string += "\\sectionheader{Attack}" + create_attack_list(monster["attack"]) + NEWLINE

    if "special" in monster:
        string += "\\sectionheader{Special}" + create_special_block(monster["special"]) + NEWLINE
    return string


def create_doc(filecontents):
    latexfile = open("monsters.tex", "w")
    #latexfile.write(PREAMBLE)
    latexfile.write(create_monster(filecontents))
    #latexfile.write(CONCLUSION)


with open("test.yaml") as stream:
    try:
        create_doc(yaml.safe_load(stream))
    except yaml.YAMLError as err:
        print(err)
