from math import floor

# Return array as string with each item separated by a given sequence
def separate(array, spacer=" "):
    string = ""
    for item in array:
        if string != "":
            string += spacer
        string += str(item)
    return string


# Returns array as string with each item separated by a comma
def comma_separate(array):
    return separate(array, spacer=", ")


# Returns a number as a string with proper sign indication
def format_bonus(bonus):
    if bonus >= 0:
        return "+" + str(bonus)
    else:
        return str(bonus)


# calculates proficiency bonus from cr
def cr_to_prof(cr):
    if type(cr) == str:
        return 2
    else:
        return max(2, floor((cr - 1) / 4) + 2)


# returns decimal form of given cr
def cr_to_digit(cr):
    if type(cr) == int:
        return cr
    else:
        return 1 / int(cr[-1])


# returns fractional from of given cr
def cr_to_string(cr):
    if type(cr) == int:
        return str(cr)
    elif type(cr) == float:
        return {0.125:"1/8", 0.25:"1/4", 0.5:"1/2"}[cr]
    else:
        return cr


# calculates bonus from ability score
def score_to_bonus(score):
    return floor(score / 2) - 5


# returns dictionary with bonuses mapped to abilities
def ability_scores_to_bonuses(stats):
    bonusdict = {}
    for ability in stats:
        bonusdict[ability] = score_to_bonus(stats[ability])
    return bonusdict


# returns string of number with proper ordinary suffix
def format_index(index):
    if index == 1:
        return "1st"
    elif index == 2:
        return "2nd"
    elif index == 3:
        return "3rd"
    else:
        return str(index) + "th"


# returns string with proper possessive suffix
def possessive(*name):
    name = separate(name, " ")
    if name[-1] == "s":
        return name + "'"
    else:
        return name + "'s"


# returns headername field if it exists, or name field if it doesn't
def headername(dictionary):
    if "headername" in dictionary:
        return dictionary["headername"]
    else:
        return dictionary["name"]


# returns shortname field if it exists, or lowercased name field if it doesn't
def shortname(dictionary):
    if "shortname" in dictionary:
        return dictionary["shortname"]
    else:
        return dictionary["name"].lower()


# returns a dictionary with the name and a 0 profbonus
def blank_params(name):
    return {"name":name, "profbonus":0}


# returns the given key from the dictionary if it exists, or returns the default if the key does not exist
def get_key_if_exists(dictionary, key, default):
    if key in dictionary:
        return dictionary[key]
    else:
        return default