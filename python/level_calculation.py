import python.parser_utility as pp
import python.brand as brand

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
