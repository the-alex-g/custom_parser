import yaml
import os
import python.parser_utility as pp
import python.brand as brand

ABILITIES = ["str", "con", "dex", "spd", "lor", "ins", "cha", "det"]
ARMOR_PERCENTS = [0.83, 0.75, 0.67, 0.50, 0.33, 0.25, 0.17]
NEWLINE = "\\\\"
LINEBREAK = "\\bigskip"
PAGEBREAK = "\n\\clearpage\n"
DEFAULT_MONSTER_TRAITS = pp.open_yaml("config_yaml/monster_type_traits.yaml")
BASE_HEALTH = 4


def get_size_as_number(size):
    if type(size) == str:
        part_a = ""
        part_b = ""
        in_first_part = True
        for char in size:
            if char == "/":
                in_first_part = False
            elif in_first_part:
                part_a += char
            else:
                part_b += char
        return int(part_a) / int(part_b)
    else:
        return size



def get_ability_list(bonuses):
    bonuses = bonuses.copy()
    string = ""
    for ability in bonuses.copy():
        if bonuses[ability] == 0:
            del bonuses[ability]
    i = 0
    for ability in ABILITIES:
        if ability in bonuses:
            if string != "" and i != 4:
                string += ", "
            string += "\\textbf{" + ability.title() + "} " + brand.format_bonus(bonuses[ability])
            i += 1
            if i == 4 and len(bonuses) > 4:
                string += NEWLINE
    return string


def get_size_bonus(size, scale):
    bonus = 0
    match size:
        case 0:
            bonus = -4
        case "1/4":
            bonus = -2
        case "1/2":
            bonus = -1
        case 1:
            bonus = 0
        case _:
            bonus = pow(2, size - 2)
    return bonus * scale


def calculate_evade(bonuses, size, dodge):
    evd_base = 0
    evd_bonus = 0
    if dodge:
        evd_base = pp.get_key_if_exists(bonuses, "dex", 0)
        evd_bonus = pp.get_key_if_exists(bonuses, "spd", 0)
    else:
        evd_base += pp.get_key_if_exists(bonuses, "spd", 0)
        evd_bonus = pp.get_key_if_exists(bonuses, "dex", 0)
    evade = evd_base + get_size_bonus(size, -1)
    if evd_bonus > 1:
        evade += 1
    elif evd_bonus < -1:
        evade -= 1
    return evade


def calculate_health(size, bonus, armor):
    return int((BASE_HEALTH + bonus) * size / ARMOR_PERCENTS[armor])


def create_monster(monster):
    if monster["type"] in DEFAULT_MONSTER_TRAITS:
        monster = pp.combine_dictionaries(monster, DEFAULT_MONSTER_TRAITS[monster["type"]], {})

    name = monster["name"]

    print("compiling", name)

    headername = pp.headername(monster)
    size = monster["size"]
    size_number = get_size_as_number(size)
    alignment = pp.get_key_if_exists(monster, "alignment", "").upper()
    bonus_dict = pp.get_key_if_exists(monster, "bonuses", {})
    health_bonus = 0

    if monster["type"] == "construct":
        hardness = pp.get_key_if_exists(monster, "hardness", 0)
        pp.increment_key(bonus_dict, "det", hardness)
        pp.increment_key(monster, "armor", hardness)
        health_bonus = bonus_dict["det"]
    elif monster["type"] == "undead":
        health_bonus = pp.get_key_if_exists(bonus_dict, "det", 0)
    else:
        health_bonus = pp.get_key_if_exists(bonus_dict, "con", 0)
    pp.increment_key(bonus_dict, "str", get_size_bonus(size, 1))

    params = {"name":name.lower()}
    for ability in ABILITIES:
        params[ability] = pp.get_key_if_exists(bonus_dict, ability, 0)
    
    
    string = "\\section*{" + headername + "}\\textit{" + pp.get_key_if_exists(monster, "flavor", "") + "}" + NEWLINE + "\\medskip"
    string += "\\textsc{" + alignment
    if string[-1] != "{":
        string += " "

    armor = pp.get_key_if_exists(monster, "armor", 1)

    string += "size " + str(size) + " " + monster["type"] + "}" + NEWLINE
    string += get_ability_list(bonus_dict) + NEWLINE
    string += "\\textbf{Health} " + str(calculate_health(size_number, health_bonus, armor))
    string += ", \\textbf{Arm} " + str(armor)
    string += ", \\textbf{Evd} " + str(calculate_evade(bonus_dict, size, "dodge" in monster))
    string += ", \\textbf{Mv} " + str(6 + pp.get_key_if_exists(bonus_dict, "spd", 0) * 2)

    if "movement_modes" in monster:
        string += ", " + pp.comma_separate(monster["movement_modes"])
    string += NEWLINE

    if "immune" in monster:
        string += "\\textbf{Immune} " + pp.comma_separate(monster["immune"]) + NEWLINE

    if "resist" in monster:
        string += "\\textbf{Resist} " + pp.comma_separate(monster["resist"]) + NEWLINE

    if "vulnerable" in monster:
        string += "\\textbf{Vulnerable} " + pp.comma_separate(monster["vulnerable"]) + NEWLINE
    
    if "attack" in monster:
        string += "\\textbf{Attack }" + monster["attack"][0] + ": " + pp.comma_separate(monster["attack"][1:]) + NEWLINE

    if "traits" in monster:
        string += "\\textbf{Traits} " + pp.comma_separate(sorted(monster["traits"])) + NEWLINE
    
    if "languages" in monster:
        string += "\\textbf{Languages} " + pp.comma_separate(sorted(monster["languages"])) + NEWLINE
    
    if "special" in monster:
        ability_name_dict = pp.get_dict_by_name(monster["special"])
        for ability_name in sorted(ability_name_dict):
            ability = ability_name_dict[ability_name]
            string += LINEBREAK + "\\textbf{" + ability["name"] + "}. " + ability["text"] + NEWLINE
    
    if "variants" in monster:
        string += LINEBREAK + "\\textbf{Variants}" + NEWLINE + "\\halfline"
        variant_name_dict = pp.get_dict_by_name(monster["variants"])
        for variant_name in sorted(variant_name_dict):
            string += "\\textbf{" + variant_name + "}. " + variant_name_dict[variant_name]["text"] + NEWLINE + LINEBREAK

    return brand.eval_string(string, params)


def create_monster_block():
    string = ""
    monster_name_dict = pp.get_dict_by_name(pp.get_yaml_from_directory("monsters"))
    for monster_name in sorted(monster_name_dict):
        string += create_monster(monster_name_dict[monster_name])
    return string


def create_doc():
    latex_file = open("monsters.tex", "w")
    for line in open("tex/framework.tex").readlines():
        if line == "%[monsters]\n":
            latex_file.write(create_monster_block())
        else:
            latex_file.write(line)
    latex_file.close()



create_doc()