import yaml
import os
import python.parser_utility as pp
import python.brand as brand
import python.level_calculation as level_calculator
from math import floor, ceil

SOURCE_DOC = "document/shortform.brand"

ABILITIES = ["str", "con", "dex", "spd", "int", "per", "cha", "det"]
ARMOR_HEALTH_MODS = [6/5, 4/3, 3/2, 2, 3, 4, 6]
HARDNESSES = {"leather":1, "wood":2, "stone":3, "bronze":4, "iron":5, "gemstone":6}
ARMOR_NAMES = {"none":0, "leather":1, "hide":2, "brigandine":3, "chain":4, "scale":5, "plate":6}
ARMOR_SPD_PENALTY = {4:-1, 5:-2, 6:-2}
ARMOR_DEX_PENALTY = {6:-1}
SHIELD_EVD = {"targe":1, "tower":1}
SHIELD_EXTRA_BONUS = {
    "buckler":("Buckler", "The [name] has +1 Evd against melee attacks."),
    "tower":("Tower Shield", "The [name] has +1 Evd against ranged attacks.")
}
NEWLINE = "\\\\"
LINEBREAK = NEWLINE # "\\bigskip "
PAGEBREAK = "\n\\clearpage\n"
DEFAULT_MONSTER_TRAITS = pp.open_yaml("config_yaml/monster_type_traits.yaml")
SPECIAL_MONSTER_TRAITS = pp.open_yaml("config_yaml/monster_special_traits.yaml")
BASE_HEALTH = 2
MONSTER_TYPE_DICE = {
    "":{"str":6, "dex":6, "con":6, "spd":6, "int":6, "per":6, "det":6, "cha":6},
    "ooze":{"str":6, "dex":12, "con":6, "spd":4, "int":4, "per":4, "det":6, "cha":4},
    "plant":{"str":8, "dex":6, "con":8, "spd":6, "int":8, "per":8, "det":10, "cha":6},
    "animal":{"str":8, "dex":8, "con":8, "spd":8, "int":6, "per":10, "det":6, "cha":6},

    "goblin":{"str":4, "dex":10, "con":4, "spd":8, "int":6, "per":8, "det":4, "cha":6},
    "boar":{"str":8, "dex":6, "con":10, "spd":8, "per":6, "det":10, "cha":4, "int":4},
    "spider":{"str":8, "dex":8, "con":8, "spd":6, "per":8, "det":6, "cha":8, "int":4},
}

monster_count = 0
appendicies = {}
spell_data = {}


def get_ability_list(bonuses):
    string = ""
    foo = {}
    bar = {}

    for ability in ABILITIES:
        if not ability in bonuses:
            bonuses[ability] = 0
    
    for value in bonuses.values():
        if value > -4:
            pp.increment_key(foo, value, 1)

    for value in foo:
        bar[foo[value]] = value

    base = bar[sorted(bar)[len(bar) - 1]]

    for ability in ABILITIES:
        if ability in bonuses:
            if abs(bonuses[ability] - base) <= 1 or bonuses[ability] <= -4:
                del(bonuses[ability])

    i = 0
    if base != 0:
        if len(bonuses) == 0:
            bonuses["all stats"] = base
        else:
            bonuses["all others"] = base
    for ability in bonuses:
        if i == 4:
            string += NEWLINE
        elif string != "":
            string += ", "
        string += f"[bold {ability.title()}] [format_bonus {bonuses[ability]}]"
        i += 1
    return string


def calculate_evade(bonuses, dice, size, dodge, shield):
    dex = pp.get_key_if_exists(bonuses, "dex", 0)
    spd = pp.get_key_if_exists(bonuses, "spd", 0)
    dex_die = dice["dex"]
    spd_die = dice["spd"]
    evade = dex + floor(spd_die / 2)
    spd_base_evd = spd + floor(dex_die / 2)
    if dodge and spd_base_evd > evade:
        evade = spd_base_evd
    return evade + shield - size


def get_base_health(size, bonus):
    return BASE_HEALTH + bonus + size


def calculate_health(base, armor):
    health = base * ARMOR_HEALTH_MODS[armor]
    return max(1, floor(health))


def get_monster_array_field(name, monster):
    return f"[bold {name}] {pp.comma_separate(sorted(monster[name.lower()]))} "


def add_to_appendix(appendix, item_name, categorizer):
    global appendicies

    if appendix in appendicies:
        pp.append_to_key(appendicies[appendix], categorizer, item_name)
    else:
        appendicies[appendix] = {categorizer:[item_name]}


def get_monster_spells(spellcasting, bonuses):
    return ""
    
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

    if "type" in spellcasting:
        spell_type = spellcasting["type"]
        if spell_type == "wizard":
            ability = "int"
            mana = 8 + 2 * pp.get_key_if_exists(bonuses, "mana", 0)
        mana += pp.get_key_if_exists(spellcasting, "mana", 0)
    else:
        ability = spellcasting["ability"].title()
        mana = spellcasting["mana"]
    string = f"[bold Spells ({ability}, {mana} mp)][newline]"
    for circle in sorted(circles):
        string += f"[bold {circle}] [italics {pp.comma_separate(sorted(circles[circle]))}][newline]"
    return string


def get_movement(spd, modes, size):
    names = {"b":"burrow", "t":"tunnel", "s":"swim", "f":"fly", "h":"hover"}
    base = max(1, 6 + spd * 2)
    string = str(base)
    for mode in sorted(modes):
        if "=" in mode:
            string += f", {names[mode[0]]} {mode[mode.find("=") + 1:]}"
        elif mode == "f":
            string += f", {names[mode]} {5 * ceil(1.2 * (base + size))}" # equals 6 * (base + size), rounded up to a multiple of 5
        elif mode in "bt":
            string += f", {names[mode]} {ceil(0.5 * base)}"
        else:
            string += f", {names[mode]} {base}"
    return string


def get_bonus_dict(monster):
    if "bonuses" in monster:
        bonus_dict = pp.get_key_if_exists(monster, "bonuses", {})
    else:
        bonus_dict = {}
        for ability in ABILITIES:
            if ability in monster:
                bonus_dict[ability] = monster[ability]
    return bonus_dict


def get_monster_dice(size, type):
    if type in MONSTER_TYPE_DICE:
        dice = MONSTER_TYPE_DICE[type]
    else:
        dice = MONSTER_TYPE_DICE[""]

    if size > 1:
        if dice["str"] < 12:
            dice["str"] += 2
        if dice["dex"] > 4:
            dice["dex"] -= 2
    elif size < -1:
        if dice["str"] > 4:
            dice["str"] -= 2
        if dice["dex"] < 12:
            dice["dex"] += 2

    return dice


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
    alignment = pp.get_key_if_exists(monster, "alignment", "").upper()
    bonus_dict = get_bonus_dict(monster)
    health_bonus = 0
    movement = get_movement(pp.get_key_if_exists(bonus_dict, "spd", 0), pp.get_key_if_exists(monster, "movement", []), size)

    dice = get_monster_dice(size, pp.get_key_if_exists(monster, "dice", monster["type"]))

    armor = pp.get_key_if_exists(monster, "armor", 0)
    if type(armor) == str:
        armor = ARMOR_NAMES[armor]
    if pp.get_key_if_exists(monster, "real-armor", False):
        pp.increment_key(
            bonus_dict,
            "spd",
            pp.get_key_if_exists(ARMOR_SPD_PENALTY, armor, 0)
        )
        pp.increment_key(
            bonus_dict,
            "dex",
            pp.get_key_if_exists(ARMOR_DEX_PENALTY, armor, 0)
        )

    if monster["type"] == "construct":
        hardness = pp.get_key_if_exists(monster, "hardness", 0)
        if type(hardness) == str:
            hardness = HARDNESSES[hardness]
        pp.increment_key(bonus_dict, "det", hardness)
        armor += hardness
        # constructs use det instead of con to determine health
        health_bonus = bonus_dict["det"]
    elif monster["type"] == "undead":
        # undead use det instead of con to determine health
        health_bonus = pp.get_key_if_exists(bonus_dict, "det", 0)
    else:
        health_bonus = pp.get_key_if_exists(bonus_dict, "con", 0)
    pp.increment_key(bonus_dict, "str", size)

    params = {"name":name.lower(), "size":size}
    for ability in ABILITIES:
        params[ability] = pp.get_key_if_exists(bonus_dict, ability, 0)
    base_health = get_base_health(size, health_bonus)
    health = calculate_health(base_health, armor)
    shield = pp.get_key_if_exists(monster, "shield", "").lower()
    shield_evd = 0
    if shield in SHIELD_EVD:
        shield_evd = SHIELD_EVD[shield]
    if shield in SHIELD_EXTRA_BONUS:
        shield_bonus = SHIELD_EXTRA_BONUS[shield]
        if "special" in monster:
            monster["special"][shield_bonus[0]] = shield_bonus[1]
        else:
            monster["special"] = {
                shield_bonus[0]:shield_bonus[1]
            }
    evasion = calculate_evade(
        bonus_dict, dice, size, "dodge" in monster,
        shield_evd
    ) + pp.get_key_if_exists(monster, "evd_bonus", 0)
    level = level_calculator.get_level(health, evasion, monster, bonus_dict)

    print(f"compiling {name} ({level})")
    
    string = "\\section*{" + headername + "}"
    if "flavor" in monster:
        string += f"[text i {pp.get_key_if_exists(monster, "flavor", "")}][newline med]"
    string += f"[label <{headername}>][text sc {monster["type"]}]"

    if "tags" in monster:
        string += f" ({pp.comma_separate(sorted(monster["tags"]))})"

    string += NEWLINE
    ability_string = get_ability_list(bonus_dict)
    if ability_string != "":
        string += ability_string + NEWLINE
    string += f"[bold Size] {size} [bold Health] {health} "
    #if armor == 0:
    #    string += f"{health} "
    #elif base_health <= 0:
    #    string += f"{health} "
    #else:
    #    string += f"{base_health}/{health - base_health} "
    if armor > 0:
        string += f"[bold Arm] {armor} "
    string += f"[bold Evd] {evasion} "
    string += f"[bold Mv] {movement} "

    for field in ("immune", "resist", "vulnerable"):
        if field in monster:
            string += get_monster_array_field(field.title(), monster)

    if "languages" in monster:
        string += get_monster_array_field("Languages", monster)
    
    if "senses" in monster:
        string += get_monster_array_field("Senses", monster)

    if "spell resist" in monster:
        string += f"[bold Spell Resist] {monster["spell resist"]}"
    
    if "attack" in monster:
        string += f"[newline][bold Attack] {monster["attack"][0]}/round[newline][quad] {pp.separate(monster["attack"][1:], spacer="[newline] [quad] ")}"
    
    if "special" in monster:
        ability_name_dict = pp.get_dict_by_name(monster["special"])
        for ability_name in ability_name_dict:
            ability = ability_name_dict[ability_name]
            string += NEWLINE
            if ability_name.endswith(" @"):
                string += LINEBREAK + "\\includegraphics@[scale=0.45@]{actionicon} "
                string += f"[bold {ability_name[0:-2]}]. {ability}"
            else:
                string += LINEBREAK + f"[bold {ability_name}]. {ability}"
    
    if "spellcasting" in monster:
        string += LINEBREAK + get_monster_spells(monster["spellcasting"], bonus_dict)
    
    if "variants" in monster:
        string += LINEBREAK + f"[bold Variants][newline][halfline]"
        for variant_name in monster["variants"]:
            string += f"[bold {variant_name}]. {monster["variants"][variant_name]}[newline]"
            string += LINEBREAK

    if "text" in monster:
        string += NEWLINE + LINEBREAK + monster["text"]

    monster_count += 1

    #add_to_appendix("Monsters by Rating", headername, level)
    add_to_appendix("Monsters by Type", headername, monster["type"].title())

    return brand.eval_string(string, params)


def create_theme(theme):
    if "effect" in theme:
        string = f"[text b {theme["name"]}][newline]This song affects all {theme["targets"]} that are within six fathoms and can hear you. {theme["effect"]}[newline]"
    else:
        string = f"[text b {theme["name"]}][newline]This song has no ongoing effect.[newline]"
    string += f"[text i Climax:] {theme["climax"]}[newline big]"
    return brand.eval_string(string, {"targets":theme["targets"]})


def create_deity(deity):
    alignment = deity["alignment"]
    name = f"[deity name {alignment}]"
    private_circle_name = deity["circle name"]
    # format private circle so that create_circle() can understand it
    private_circle = {"name":private_circle_name, "spells":deity["private circle"]}

    circles = ["Circle of Prayers", private_circle_name]
    if "circles" in deity:
        for circle_name in deity["circles"]:
            circles.append(f"Circle of {circle_name.title()}")

    return brand.eval_string(f"""[section 1 {name}][text i The [deity title {alignment}]][newline big]
    {deity["text"]}
    
    Clerics of {name} can only cast spells from the following circles: [spell {pp.comma_separate(sorted(circles))}]
    {create_circle(private_circle, title=False)}""", {})


def load_spell_data():
    global spell_data
    raw_data = pp.get_yaml_from_directory("spells")
    for file_data in raw_data:
        for spell_name in file_data:
            spell_data[spell_name] = file_data[spell_name]["rank"]

        
def create_spells(spells):
    string = ""
    
    for spell_name in sorted(spells):
        spell = spells[spell_name]
        string += f"[bold {spell_name} "
        if spell["rank"] != 0:
            string += f"({spell["rank"]})"
    string += f"""][newline]
[italics Duration: {spell["duration"]}]
[newline]
{spell["text"]}
[bigskip]"""
    return brand.eval_string(string, {})


def create_deity_block():
    string = ""
    # load deities into a list of the form {name:deity}
    name_dict = {}
    for deity in pp.get_yaml_from_directory("deities"):
        # use brand to get deity name
        name_dict[brand.deity("name", deity["alignment"])] = deity
    # create alphebetized deity list
    for name in sorted(name_dict):
        string += create_deity(name_dict[name])
    return string


def create_block(source, item_creation_function):
    string = ""
    name_dict = pp.get_dict_by_name(pp.get_yaml_from_directory(source))
    for name in name_dict:
        string += item_creation_function(name_dict[name])
    return string


def create_appendices():
    global appendicies
    string = PAGEBREAK + "[section 0 Appendicies]"

    for appendix in sorted(appendicies):
        string += f"[section 1 {appendix}]"
        for categorizer in sorted(appendicies[appendix]):
            string += "\\subsubsection*{" + categorizer + "}"
            for item in sorted(appendicies[appendix][categorizer]):
                string += f"{item}[hfill][pageref <{item}>][newline]"
        string += "[newpage]"
    
    return brand.eval_string(string, {})


def create_doc():
    global monster_count

    load_spell_data()

    latex_file = open("document/document.tex", "w")
    for line in open("document/preamble.tex").readlines():
        latex_file.write(line)
    latex_file.write(brand.eval_string(open(SOURCE_DOC).read(), {}))
    latex_file.write("\\end{document}")
    latex_file.close()

    print(monster_count, "monsters total")


brand.add_include_function("spells", lambda filename: create_block(filename, create_spells))
brand.add_include_function("themes", lambda filename: create_block(filename, create_theme))
brand.add_include_function("monsters", lambda filename: create_block(filename, create_monster))
brand.add_include_function("appendicies", lambda: create_appendices())
brand.add_include_function("circle", lambda filename: create_circle(pp.open_yaml(filename), title=False))
brand.add_include_function("deities", lambda filename: create_deity_block())

if __name__ == "__main__":
    create_doc()
