import yaml
import os
from math import floor

_nameless_dict_tracker = 0

# returns headername field if it exists, or name field if it doesn't
def headername(dictionary):
    global _nameless_dict_tracker
    if "headername" in dictionary:
        return dictionary["headername"]
    elif "name" in dictionary:
        return dictionary["name"]
    else:
        _nameless_dict_tracker += 1
        return _nameless_dict_tracker - 1


# returns shortname field if it exists, or lowercased name field if it doesn't
def shortname(dictionary):
    if "shortname" in dictionary:
        return dictionary["shortname"]
    else:
        return dictionary["name"].lower()


# returns the given key from the dictionary if it exists, or returns the default if the key does not exist
def get_key_if_exists(dictionary, key, default, if_exists=""):
    if key in dictionary:
        value = dictionary[key]
        if type(value) == str:
            return value + if_exists
        return value
    else:
        return default


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
def _score_to_bonus(score):
    return floor(score / 2) - 5


# returns dictionary with bonuses mapped to abilities
def ability_scores_to_bonuses(stats):
    bonusdict = {}
    for ability in stats:
        bonusdict[ability] = _score_to_bonus(stats[ability])
    return bonusdict


def separate(array, spacer=" "):
    string = ""
    for item in array:
        if string != "":
            string += spacer
        string += str(item)
    return string


def comma_separate(array):
    return separate(array, spacer=", ")


def combine_dictionaries(a, b, exclude):
    for field in b:
        if not field in exclude:
            if type(b[field]) == list:
                for item in b[field]:
                    if field in a:
                        if type(item) == dict:
                            if "n/" + item["name"] in a[field]:
                                a[field].remove("n/" + item["name"])
                                continue
                        elif type(item) == str:
                            if "n/" + item in a[field]:
                                a[field].remove("n/" + item)
                                continue
                        a[field].append(item)
                    else:
                        a[field] = [item]

            elif type(b[field]) == str:
                if not field in a:
                    a[field] = b[field]
    return a


def open_yaml(filepath, show=True):
    if show:
        print("loading", filepath)
    with open(filepath) as stream:
        try:
            return yaml.safe_load(stream)
        except yaml.YAMLError as exc:
            print(exc, " in file ", filepath)


def get_yaml_from_directory(dirname):
    yaml_list = []
    if os.path.isdir(dirname):
        for item in os.listdir(dirname):
            path = os.path.join(dirname, item)
            if os.path.isfile(path) and path.endswith(".yaml"):
                yaml_list.append(open_yaml(path))
            elif os.path.isdir(path):
                for yaml_dir in get_yaml_from_directory(path):
                    yaml_list.append(yaml_dir)
    return yaml_list


def get_dict_by_name(data):
    if type(data) == list:
        name_dict = {}
        for item in data:
            print(item)
            name_dict[headername(item)] = item
        return sort_dictionary(name_dict)
    elif type(data) == dict:
        return sort_dictionary(data)


def sort_dictionary(dictionary):
    sorted_dictionary = {}
    for key in sorted(dictionary):
        sorted_dictionary[key] = dictionary[key]
    return sorted_dictionary
    


def increment_key(dictionary, key, value):
    if key in dictionary:
        dictionary[key] += value
    else:
        dictionary[key] = value


def append_to_key(dictionary, key, value):
    if key in dictionary:
        dictionary[key].append(value)
    else:
        dictionary[key] = [value]


def get_file_contents(filename):
    return separate(open(filename).readlines(), spacer="")
