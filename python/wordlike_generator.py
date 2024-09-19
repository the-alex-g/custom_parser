import random

CHARSETS = {
    "dwarven":"khuzdul*th*ch*chggkkm-jx*qu",
    "human":"*dh*dh*bjrhfstddnnmygg*dr*dr-jx*qucpwz",
}


def get_random(array):
    return array[random.randrange(0, len(array))]


def run():
    running = True
    while running:
        consonants = []

        # build the consonants array
        for char in "bcdfghjklmnprstvwxyz":
            consonants.append(char)
        for pair in ("th", "sh", "ch", "qu"):
            consonants.append(pair)
        
        vowels = ["a", "e", "i", "o", "u"]
                
        charset = input("Enter the desired charset: ")
        if charset in CHARSETS:
            # load the named charset
            charset = CHARSETS[charset]

        # modify consonants and vowels based on charset content
        mode = "+"
        savemode = "+"
        pair = ""
        for char in charset:
            if char in "+-*":
                savemode = mode
                mode = char
            elif mode == "+": # add characters
                if char in "aeiou":
                    vowels.append(char)
                else:
                    consonants.append(char)
            elif mode == "-": # remove characters
                if char in vowels:
                    vowels.remove(char)
                elif char in consonants:
                    consonants.remove(char)
            elif mode == "*": # group the next two characters together
                pair += char
                if len(pair) == 2:
                    mode = savemode
                    if mode == "+":
                        consonants.append(pair)
                    elif mode == "-":
                        if pair in consonants:
                            consonants.remove(pair)
                    pair = ""

        for i in range(10):
            wordstring = ""
            for i in range(10):
                wordlength = random.randrange(2, 5) + random.randrange(1, 4)
                word = ""
                # number of consecutive consonants in the word
                cons = random.randrange(0, 3)
                for x in range(0, wordlength):
                    if cons > 2 or random.randrange(0, 3) == 2:
                        cons = 0
                        word += get_random(vowels)
                    else:
                        cons += 1
                        word += get_random(consonants)

                # if the word is too long to fit in the box, trim it
                if len(word) > 11:
                    word = word[0:11]

                # add the word, plus enough spacing to make columns
                wordstring += word.title() + " " * (12 - len(word))
            
            print(wordstring)
    
        if input("Again? (n/Y) ") == "n":
            running = False

run()



"""
Dvdeu, Dvla, Kthov, Tuim, Uvlhoz, Uhthk, Eokz, Udgrn

Ovend, Drrut, Igd, Ogrev

"""
