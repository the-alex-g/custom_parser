import yaml
import python.parser_utility as pp

master = pp.get_dict_by_name(pp.get_yaml_from_directory("temp"))


def snake_case(string):
    return string.lower().replace(" ", "_").replace(",", "")


def squish_size(size):
    if size >= 3:
        size = 4
    elif size == 2:
        size = 3
    elif size >= 0:
        size = 2
    elif size >= -2:
        size = 1
    else:
        size = 0
    return size


def convert(dictionary):
    if not pp.get_key_if_exists(dictionary, "flag_reformated", False):
        if "size" in dictionary:
            dictionary["size"] = squish_size(dictionary["size"])
        dictionary["flag_reformated"] = True
    return dictionary


if __name__ == "__main__":
    for monster_name in master:
        with open(f"monsters/{snake_case(monster_name)}.yaml", "w") as file:
            print(monster_name)
            yaml.dump(convert(master[monster_name]), file)
        
