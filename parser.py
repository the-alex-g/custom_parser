import yaml
import os
import python.parser_utility as pp
import python.brand as brand
from math import floor, ceil

SOURCE_FOLDER = "monsters"

ABILITIES = ["str", "con", "dex", "spd", "lor", "ins", "cha", "det"]
ARMOR_PERCENTS = [0.83, 0.75, 0.67, 0.50, 0.33, 0.25, 0.17]
HARDNESSES = {"leather":1, "wood":2, "stone":3, "bronze":4, "iron":5, "gemstone":6}
ARMOR_NAMES = {"none":0, "leather":1, "hide":2, "brigandine":3, "chain":4, "scale":5, "plate":6}
NEWLINE = "\\\\"
LINEBREAK = "\\bigskip"
PAGEBREAK = "\n\\clearpage\n"
DEFAULT_MONSTER_TRAITS = pp.open_yaml("config_yaml/monster_type_traits.yaml")
SPECIAL_MONSTER_TRAITS = pp.open_yaml("config_yaml/monster_special_traits.yaml")
BASE_HEALTH = 4

monster_count = 0


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
            string += f"[bold {ability.title()}] [format_bonus {bonuses[ability]}]"
            i += 1
            if i == 4 and len(bonuses) > 4:
                string += NEWLINE
    return string


def get_size_bonus(size):
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
    return bonus


def calculate_evade(bonuses, size, dodge):
    evd_base = 0
    evd_bonus = 0
    if dodge:
        evd_base = pp.get_key_if_exists(bonuses, "dex", 0)
        evd_bonus = pp.get_key_if_exists(bonuses, "spd", 0)
    else:
        evd_base += pp.get_key_if_exists(bonuses, "spd", 0)
        evd_bonus = pp.get_key_if_exists(bonuses, "dex", 0)
    evade = evd_base - get_size_bonus(size)
    if evd_bonus > 1:
        evade += 1
    elif evd_bonus < -1:
        evade -= 1
    return evade


def calculate_health(size, bonus, armor):
    health = (BASE_HEALTH + bonus) * size / (2 * ARMOR_PERCENTS[armor])
    if health < 3 and armor > 0:
        return max(0, ceil(health))
    return max(0, floor(health))


def get_monster_array_field(name, monster):
    return f"[bold {name}] {pp.comma_separate(sorted(monster[name.lower()]))}[newline]"


def create_monster(monster):
    global monster_count

    if monster["type"] in DEFAULT_MONSTER_TRAITS:
        monster = pp.combine_dictionaries(monster, DEFAULT_MONSTER_TRAITS[monster["type"]], {})

    name = monster["name"]

    print("compiling", name)

    for trait in pp.get_key_if_exists(monster, "traits", []):
        if "special" in monster:
            monster["special"][trait] = SPECIAL_MONSTER_TRAITS[trait]
        else:
            monster["special"] = {trait:SPECIAL_MONSTER_TRAITS[trait]}

    headername = pp.headername(monster)
    size = monster["size"]
    size_number = get_size_as_number(size)
    alignment = pp.get_key_if_exists(monster, "alignment", "").upper()
    bonus_dict = pp.get_key_if_exists(monster, "bonuses", {})
    health_bonus = 0

    armor = pp.get_key_if_exists(monster, "armor", 0)
    if type(armor) == str:
        armor = ARMOR_NAMES[armor]

    if monster["type"] == "construct":
        hardness = pp.get_key_if_exists(monster, "hardness", 0)
        if type(hardness) == str:
            hardness = HARDNESSES[hardness]
        pp.increment_key(bonus_dict, "det", hardness)
        armor += hardness
        health_bonus = bonus_dict["det"]
    elif monster["type"] == "undead":
        health_bonus = pp.get_key_if_exists(bonus_dict, "det", 0)
    else:
        health_bonus = pp.get_key_if_exists(bonus_dict, "con", 0)
    pp.increment_key(bonus_dict, "str", get_size_bonus(size))

    params = {"name":name.lower()}
    for ability in ABILITIES:
        params[ability] = pp.get_key_if_exists(bonus_dict, ability, 0)
    
    string = "\\section*{" + headername + "}\\textit{" + pp.get_key_if_exists(monster, "flavor", "", if_exists=NEWLINE) + "}\\medskip"
    string += "\\textsc{" + alignment
    if string[-1] != "{":
        string += " "

    string += "size " + str(size) + " " + monster["type"] + "}"
    if "tags" in monster:
        string += f" ({pp.comma_separate(sorted(monster["tags"]))})"
    string += NEWLINE + get_ability_list(bonus_dict) + NEWLINE
    string += f"[bold Health] {calculate_health(size_number, health_bonus, armor)}"
    string += f", [bold Arm] {armor}"
    string += f", [bold Evd] {calculate_evade(bonus_dict, size, "dodge" in monster)}"
    string += f", [bold Mv] {6 + pp.get_key_if_exists(bonus_dict, "spd", 0) * 2}"

    if "movement_modes" in monster:
        string += f", {pp.comma_separate(monster["movement_modes"])}"
    string += NEWLINE

    if "immune" in monster:
        string += get_monster_array_field("Immune", monster)

    if "resist" in monster:
        string += get_monster_array_field("Resist", monster)

    if "vulnerable" in monster:
        string += get_monster_array_field("Vulnerable", monster)

    if "spell resist" in monster:
        string += f"[bold Spell Resist] {monster["spell resist"]}[newline]"
    
    if "attack" in monster:
        string += f"[bold Attack] {monster["attack"][0]}: {pp.comma_separate(monster["attack"][1:])}[newline]"

    if "languages" in monster:
        string += get_monster_array_field("Languages", monster)
    
    if "senses" in monster:
        string += get_monster_array_field("Senses", monster)
    
    if "special" in monster:
        ability_name_dict = pp.get_dict_by_name(monster["special"])
        for ability_name in ability_name_dict:
            ability = ability_name_dict[ability_name]
            string += LINEBREAK + f"[bold {ability_name}]. {ability}[newline]"
    
    if "variants" in monster:
        string += LINEBREAK + f"[bold Variants][newline]\\halfline"
        variant_name_dict = pp.get_dict_by_name(monster["variants"])
        for variant_name in variant_name_dict:
            string += f"[bold {variant_name}]. {variant_name_dict[variant_name]["text"]}[newline]" + LINEBREAK

    monster_count += 1

    return brand.eval_string(string, params)


def create_theme(theme):
    return brand.eval_string(
        f"""[bold {theme["name"]}].[newline]
        This song affects all {theme["targets"]} within six fathoms that can hear you. {theme["effect"]}[newline]
        [italics Climax:] {theme["climax"]}[newline big]""",
        {}
    )


def create_circle(circle):
    string = "\\unnumberedsubsection{" + circle["name"] + "}"
    spells = pp.sort_dictionary(circle["spells"])
    for spell_name in spells:
        spell = spells[spell_name]
        string += brand.eval_string(
            f"""[bold {spell_name} ({spell["cost"]})][newline]
            [italics Duration: {spell["duration"]}][newline]
            {spell["text"]}[newline big]""",
            {}
        )
    return string


def create_block(source, item_creation_function):
    string = ""
    name_dict = pp.get_dict_by_name(pp.get_yaml_from_directory(source))
    for name in name_dict:
        string += item_creation_function(name_dict[name])
    return string


def create_doc():
    global monster_count

    latex_file = open("monsters.tex", "w")
    for line in open("tex/framework.tex").readlines():
        if line == "%[monsters]\n":
            latex_file.write(create_block(SOURCE_FOLDER, create_monster))
        elif line == "%[themes]\n":
            latex_file.write(create_block("bard_songs", create_theme))
        elif line == "%[spells]\n":
            latex_file.write(create_block("spell_circles", create_circle))
        else:
            latex_file.write(line)
    latex_file.close()

    print(monster_count, "monsters total")


create_doc()