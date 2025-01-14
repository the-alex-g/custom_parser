import yaml
import os
import python.parser_utility as pp
import python.brand as brand
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
LINEBREAK = "\\bigskip "
PAGEBREAK = "\n\\clearpage\n"
DEFAULT_MONSTER_TRAITS = pp.open_yaml("config_yaml/monster_type_traits.yaml")
SPECIAL_MONSTER_TRAITS = pp.open_yaml("config_yaml/monster_special_traits.yaml")
BASE_HEALTH = 2

monster_count = 0
appendicies = {}
spell_data = {}


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


def calculate_evade(bonuses, size, dodge, shield):
    dex = pp.get_key_if_exists(bonuses, "dex", 0)
    spd = pp.get_key_if_exists(bonuses, "spd", 0)
    evade = 10 + spd - size
    if dodge:
        evade += dex
    elif dex > 1:
        evade += 1
    elif dex < -1:
        evade -= 1
    return evade + shield


def calculate_health(size, bonus, armor):
    health = (BASE_HEALTH + bonus + size) * ARMOR_HEALTH_MODS[armor]
    return max(1, floor(health))


def get_monster_array_field(name, monster):
    return f"[bold {name}] {pp.comma_separate(sorted(monster[name.lower()]))}[newline]"


def add_to_appendix(appendix, item_name, categorizer):
    global appendicies

    if appendix in appendicies:
        pp.append_to_key(appendicies[appendix], categorizer, item_name)
    else:
        appendicies[appendix] = {categorizer:[item_name]}


def modify_health(health, monster):
    damage_types = ("bludgeoning", "piercing", "slashing")
    mods = {
        "resist":1/3,
        "immune":1,
        "vulnerable":-1/3
    }
    multiplier = 1
    for mod in mods:
        if mod in monster:
            for damage in monster[mod]:
                if damage == "non-magic":
                    multiplier += mods[mod] * 3
                elif damage == "non-magic physical":
                    multiplier += mods[mod] * 2
                else:
                    if damage in damage_types:
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


def get_damage(string):
    die = string[string.find("d") + 1:string.find("/")]
    if die.isdigit():
        die = int(die)
        threshold = die - int(
            string[string.find("/") + 1:]
        )
        damage = 1 + threshold / die
        damage += 3 * pow(threshold / die, 2)
    else:
        damage = 0
    return damage


def calculate_dpr(monster, bonuses):
    damage = 0
    if "attack" in monster:
        attacks = monster["attack"]
        attack_count = attacks[0]
        attacks_skipped = 0
        if type(attack_count) == str:
            attack_count = int(attack_count[0])
        for attack in attacks[1:]:
            multiplier = 1

            # if the attack is a 2x, 3x, etc.
            if attack[0].isdigit():
                multiplier = int(attack[0])
                attack = attack[3:]
            
            attack = brand.eval_string(attack, bonuses)
            damage_string = attack[0:attack.find(" ")]
            if damage_string.isdigit():
                if "unblockable" in attack:
                    attack_damage = int(damage_string) * 2
                else:
                    attack_damage = int(damage_string)
            elif "d" in damage_string:
                attack_damage = get_damage(damage_string)
            else:
                attack_damage = 0
            if attack_damage > 0:
                damage += attack_damage * multiplier
            else:
                attacks_skipped += 1
        damage *= attack_count / max(1, len(attacks[1:]) - attacks_skipped)

    extra_damage = pp.get_key_if_exists(monster, "extra_damage", {})
    actions = 0
    for i in pp.get_key_if_exists(extra_damage, "per", []): # 'persistent'
        if type(i) == str:
            damage += get_damage(i)
        else:
            damage += i
    for i in pp.get_key_if_exists(extra_damage, "act", []): # 'action'
        if type(i) == str:
            damage += get_damage(i)
        else:
            damage += i
        actions += 1
    damage /= 1 + actions
    return damage


def get_monster_spells(spellcasting, bonuses):
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
    base = max(1, 6 + spd * 2)
    string = str(base)
    for mode in sorted(modes):
        if "=" in mode:
            string += f", {mode[0]} {mode[mode.find("=") + 1:]}"
        elif mode == "f":
            string += f", f {5 * ceil(1.2 * (base + size))}" # equals 6 * (base + size), rounded up to a multiple of 5
        elif mode in "bt":
            string += f", {mode} {ceil(0.5 * base)}"
        else:
            string += f", {mode} {base}"
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
    health = calculate_health(size, health_bonus, armor)
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
        bonus_dict, size, "dodge" in monster,
        shield_evd
    )
    level = get_level(health, evasion, monster, bonus_dict)

    print("compiling", name, f"({level})")
    
    string = "\\section*{" + headername + "}" + f"[text i {pp.get_key_if_exists(monster, "flavor", "")}][newline med][label <{headername}>]"
    string += f"[text sc {monster["type"]} ({level})]"

    if "tags" in monster:
        string += f" ({pp.comma_separate(sorted(monster["tags"]))})"

    string += NEWLINE + get_ability_list(bonus_dict) + NEWLINE
    string += f"[bold Size] {size}, [bold Health] {health}, "
    string += f"[bold Arm] {armor}, [bold Evd] {evasion}" + NEWLINE
    string += f"[bold Mv] {movement}" + NEWLINE

    for field in ("immune", "resist", "vulnerable"):
        if field in monster:
            string += get_monster_array_field(field.title(), monster)

    if "spell resist" in monster:
        string += f"[bold Spell Resist] {monster["spell resist"]}[newline]"
    
    if "attack" in monster:
        string += f"[bold Attack] {monster["attack"][0]}/round[newline][quad] {pp.separate(monster["attack"][1:], spacer="[newline] [quad] ")}[newline]"

    if "languages" in monster:
        string += get_monster_array_field("Languages", monster)
    
    if "senses" in monster:
        string += get_monster_array_field("Senses", monster)
    
    if "special" in monster:
        ability_name_dict = pp.get_dict_by_name(monster["special"])
        for ability_name in ability_name_dict:
            ability = ability_name_dict[ability_name]
            if ability_name.endswith(" @"):
                string += LINEBREAK + "\\includegraphics@[scale=0.45@]{actionicon} "
                string += f"[bold {ability_name[0:-2]}]. {ability}[newline]"
            else:
                string += LINEBREAK + f"[bold {ability_name}]. {ability}[newline]"
    
    if "spellcasting" in monster:
        string += LINEBREAK + get_monster_spells(monster["spellcasting"], bonus_dict)
    
    if "variants" in monster:
        string += LINEBREAK + f"[bold Variants][newline][halfline]"
        for variant_name in monster["variants"]:
            string += f"[bold {variant_name}]. {monster["variants"][variant_name]}[newline]"
            string += LINEBREAK

    if "text" in monster:
        string += LINEBREAK + monster["text"]

    monster_count += 1

    add_to_appendix("Monsters by Rating", headername, level)
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
    raw_data = pp.get_yaml_from_directory("spell_circles")
    for circle in raw_data:
        for spell_name in circle["spells"]:
            spell_data[spell_name] = {"circle":circle["name"], "cost":circle["spells"][spell_name]["cost"]}


def create_circle(circle, title=True):
    if title:
        string = f"[section 1 {circle["name"]}]"
    else:
        string = f"[section 2 {circle["name"]}]"
    spells = pp.sort_dictionary(circle["spells"])
    for spell_name in spells:
        spell = spells[spell_name]

        string += f"""[bold {spell_name} ({spell["cost"]})][newline][italics Duration: {spell["duration"]}]
[newline]{spell["text"]}[newline big]"""
    return brand.eval_string(string, {})


def create_spell(spell):
    return brand.eval_string(
        f"""[bold {spell["name"]} ({spell["cost"]})]
[newline]
[italics {spell["circle_name"]}[newline] Duration: {spell["duration"]}]
[newline]
{spell["text"]}
[newline big]""",
        {}
    )


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


brand.add_include_function("spells", lambda filename: create_block(filename, create_circle))
brand.add_include_function("themes", lambda filename: create_block(filename, create_theme))
brand.add_include_function("monsters", lambda filename: create_block(filename, create_monster))
brand.add_include_function("appendicies", lambda: create_appendices())
brand.add_include_function("circle", lambda filename: create_circle(pp.open_yaml(filename), title=False))
brand.add_include_function("deities", lambda filename: create_deity_block())

if __name__ == "__main__":
    create_doc()
