import random

def get_random(array):
    return array[random.randrange(0, len(array))]


def run():
    consonants = "bcdfghjklmnprstvwxyz"
    letter_pairs = ["th", "sh", "ch", "qu"]
    vowels = "aeiou"

    charset = input("Enter the desired charset: ")
    pair = ""
    for char in charset:
        if char == "(":
            pair += "-"
        elif char == ")":
            letter_pairs.append(pair[1:])
            pair = ""
        elif pair != "":
            pair += char
        elif char in vowels:
            vowels += char
        else:
            consonants += char

    running = True
    while running:
        for i in range(int(input("How many? "))):
            wordlength = random.randrange(2, 5) + random.randrange(1, 4)
            word = ""
            cons = random.randrange(0, 3)
            for x in range(0, wordlength):
                if cons > 2 or random.randrange(0, 3) == 2:
                    cons = 0
                    word += get_random(vowels)
                else:
                    cons += 1
                    if random.randrange(
                        0,
                        len(consonants) + len(letter_pairs)
                    ) < len(consonants):
                        word += get_random(consonants)
                    else:
                        word += get_random(letter_pairs)
            print(word.title())
        
        if input("Generate? (Y/n) ") == "n":
            running = False

run()
