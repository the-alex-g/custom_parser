import yaml
import os
import python.parser_utility as pp
import python.brand as brand
from math import floor, ceil

SOURCE_FOLDER = "monsters"

ABILITIES = ["str", "con", "dex", "spd", "lor", "ins", "cha", "det"]
ARMOR_HEALTH_MODS = [6/5, 4/3, 3/2, 2, 3, 4, 6]
HARDNESSES = {"leather":1, "wood":2, "stone":3, "bronze":4, "iron":5, "gemstone":6}
ARMOR_NAMES = {"none":0, "leather":1, "hide":2, "brigandine":3, "chain":4, "scale":5, "plate":6}
NEWLINE = "\\\\"
LINEBREAK = "\\bigskip"
PAGEBREAK = "\n\\clearpage\n"
DEFAULT_MONSTER_TRAITS = pp.open_yaml("config_yaml/monster_type_traits.yaml")
SPECIAL_MONSTER_TRAITS = pp.open_yaml("config_yaml/monster_special_traits.yaml")
BASE_HEALTH = 4

monster_count = 0
appendicies = {}
spell_data = {}


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
    dex = pp.get_key_if_exists(bonuses, "dex", 0)
    spd = pp.get_key_if_exists(bonuses, "spd", 0)
    evade = 10 + spd - get_size_bonus(size)
    if dodge:
        evade += dex
    elif dex > 1:
        evade += 1
    elif dex < -1:
        evade -= 1
    return evade


def calculate_health(size, bonus, armor):
    health = (BASE_HEALTH + bonus) * size * ARMOR_HEALTH_MODS[armor] / 2
    if health < 3 and armor > 0:
        return max(0, ceil(health))
    return max(0, floor(health))


def get_monster_array_field(name, monster):
    return f"[bold {name}] {pp.comma_separate(sorted(monster[name.lower()]))}[newline]"


def add_to_appendix(appendix, item_name, categorizer):
    global appendicies

    if appendix in appendicies:
        pp.append_to_key(appendicies[appendix], categorizer, item_name)
    else:
        appendicies[appendix] = {categorizer:[item_name]}


def modify_health(health, monster):
    damage_types = ["bludgeoning", "piercing", "slashing"]
    mods = {
        "resist":1/3,
        "immune":1,
        "vulnerable":-1/3
    }
    multiplier = 1
    for mod in mods:
        if mod in monster:
            if "non-magic" in monster[mod]:
                multiplier += mods[mod] * 3
            else:
                for damage in damage_types:
                    if damage in monster[mod]:
                        multiplier += mods[mod]
    return health * max(1/3, multiplier)


def get_level(health, evasion, monster, bonuses):
    number = round(
        modify_health(health, monster) *
        (evasion / 10) *
        calculate_dpr(monster, bonuses)
    )
    if number <= 3:
        return "I"
    elif number <= 10:
        return "II"
    elif number <= 15:
        return "III"
    elif number <= 25:
        return "IV"
    elif number <= 50:
        return "V"
    elif number <= 100:
        return "VI"
    elif number <= 150:
        return "VII"
    return "VIII"


def calculate_dpr(monster, bonuses):
    attacks = monster["attack"]
    attack_count = attacks[0]
    attacks_skipped = 0
    if type(attack_count) == str:
        attack_count = int(attack_count[0])
    damage = 0
    for attack in attacks[1:]:
        multiplier = 1

        # if the attack is a 2x, 3x, etc.
        if attack[0].isdigit():
            multiplier = int(attack[0])
            attack = attack[3:]
        
        attack = brand.eval_string(attack, bonuses)
        die = attack[attack.find("d") + 1:attack.find("/")]
        if die.isdigit():
            die = int(die)
            threshold = die - int(attack[attack.find("/") + 1:attack.find(" ")]) + 1
            damage += (1 + threshold / die + 3 * pow(threshold / die, 2)) * multiplier
        else:
            attacks_skipped += 1
    damage *= attack_count / max(1, len(attacks[1:]) - attacks_skipped)

    extra_damage = pp.get_key_if_exists(monster, "extra_damage", {})
    actions = 0
    for i in pp.get_key_if_exists(extra_damage, "per", []): # 'persistent'
        damage += i
    for i in pp.get_key_if_exists(extra_damage, "act", []): # 'action'
        damage += i
        actions += 1
    damage /= 1 + actions
    return damage


def get_monster_spells(spellcasting):
    circles = {}
    spells = spellcasting["spells"]
    for spell in spells:
        if " - " in spell:
            spell_name = spell[0:spell.find(" - ")]
            circle = spell_data[spell_name]["circle"]
            spell = spell.replace("-", f"({spell_data[spell_name]["cost"]})")
        else:
            circle = spell_data[spell]["circle"]
            spell += f" ({spell_data[spell]["cost"]})"
        pp.append_to_key(circles, circle, spell)
    
    string = f"[bold Spells ({spellcasting["type"].title()})][newline]"
    for circle in sorted(circles):
        string += f"[bold {circle}] [italics {pp.comma_separate(sorted(circles[circle]))}][newline]"
    return string


def create_monster(monster):
    global monster_count

    if monster["type"] in DEFAULT_MONSTER_TRAITS:
        monster = pp.combine_dictionaries(monster, DEFAULT_MONSTER_TRAITS[monster["type"]], {})

    name = monster["name"]

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
    health = calculate_health(size_number, health_bonus, armor)
    evasion = calculate_evade(bonus_dict, size, "dodge" in monster)
    
    string = "\\section*{" + headername + "}\\textit{" + pp.get_key_if_exists(monster, "flavor", "", if_exists=NEWLINE) + "}\\medskip"
    string += "\\label{" + headername + "}"
    string += "\\textsc{" + alignment
    if string[-1] != "{":
        string += " "

    string += "size " + str(size) + " " + monster["type"] + "}"
    if "tags" in monster:
        string += f" ({pp.comma_separate(sorted(monster["tags"]))})"
    string += NEWLINE + get_ability_list(bonus_dict) + NEWLINE
    string += f"[bold Health] {health}"
    string += f", [bold Arm] {armor}"
    string += f", [bold Evd] {evasion}"
    string += f", [bold Mv] {max(1, 6 + pp.get_key_if_exists(bonus_dict, "spd", 0) * 2)}"

    if "movement_modes" in monster:
        string += f", {pp.comma_separate(monster["movement_modes"])}"
    string += NEWLINE

    for field in ("immune", "resist", "vulnerable"):
        if field in monster:
            string += get_monster_array_field(field.title(), monster)

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
    
    if "spellcasting" in monster:
        string += LINEBREAK + get_monster_spells(monster["spellcasting"])
    
    if "variants" in monster:
        string += LINEBREAK + f"[bold Variants][newline]\\halfline"
        variant_name_dict = pp.get_dict_by_name(monster["variants"])
        for variant_name in variant_name_dict:
            string += f"[bold {variant_name}]. {variant_name_dict[variant_name]["text"]}[newline]" + LINEBREAK

    monster_count += 1

    level = get_level(health, evasion, monster, bonus_dict)

    add_to_appendix("Monsters by Rating", headername, level)
    add_to_appendix("Monsters by Type", headername, monster["type"].title())

    print("compiled", name, f"({level})")

    return brand.eval_string(string, params)


def create_theme(theme):
    return brand.eval_string(
        f"""[bold {theme["name"]}].[newline]
        This song affects all {theme["targets"]} within six fathoms that can hear you. {theme["effect"]}[newline]
        [italics Climax:] {theme["climax"]}[newline big]""",
        {}
    )


def create_circle(circle, title=True):
    global spell_data

    if title:
        string = f"[section 1 {circle["name"]}]"
    else:
        string = f"[bold {circle["name"]}][newline big]"
    spells = pp.sort_dictionary(circle["spells"])
    for spell_name in spells:
        spell = spells[spell_name]

        spell_data[spell_name] = {"circle":circle["name"], "cost":spell["cost"]}

        string += f"""[bold {spell_name} ({spell["cost"]})][newline][italics Duration: {spell["duration"]}]
[newline]{spell["text"]}[newline big]"""
    return brand.eval_string(string, {})


def create_block(source, item_creation_function):
    string = ""
    name_dict = pp.get_dict_by_name(pp.get_yaml_from_directory(source))
    for name in name_dict:
        string += item_creation_function(name_dict[name])
    return string


def create_appendices():
    global appendicies
    string = PAGEBREAK + "\\unnumberedsection{Appendicies}"

    for appendix in sorted(appendicies):
        string += "\\unnumberedsubsection{" + appendix + "}"
        for categorizer in sorted(appendicies[appendix]):
            string += "\\subsubsection*{" + categorizer + "}"
            for item in sorted(appendicies[appendix][categorizer]):
                string += item + "\\hfill\\pageref{" + item + "}" + NEWLINE
        string += "\\newpage"
    
    return string


def create_doc():
    global monster_count

    latex_file = open("document/document.tex", "w")
    for line in open("document/preamble.tex").readlines():
        latex_file.write(line)
    latex_file.write(brand.eval_string(open("document/brand_document.txt").read(), {}))
    latex_file.write("\\end{document}")
    latex_file.close()

    print(monster_count, "monsters total")


brand.add_include_function("spells", lambda filename: create_block(filename, create_circle))
brand.add_include_function("themes", lambda filename: create_block(filename, create_theme))
brand.add_include_function("monsters", lambda filename: create_block(filename, create_monster))
brand.add_include_function("appendicies", lambda f: create_appendices())
brand.add_include_function("circle", lambda filename: create_circle(pp.open_yaml(filename), title=False))

create_doc()