import os

TEMPLATE_CONTENTS = """
size: 
type: 
flavor: 
alignment: 
bonuses:
 str: 
 con: 
 dex: 
 spd: 
 lor: 
 ins: 
 cha: 
 det: 
armor: 
attack:
- 
- 
traits:
- 
languages:
- 
"""


def create_templates():
    destination = input("Enter the destination folder: " + os.getcwd() + "/")
    if destination == "":
      destination = os.getcwd()
    template_name = input("Enter a template name: ")
    while template_name != "":
        template_file = open(destination + "/" + template_name.replace(" ", "_").lower() + ".yaml", "w")
        template_file.write("name: " + template_name)
        template_file.write(TEMPLATE_CONTENTS)
        template_file.close()
        template_name = input("Enter a template name: ")


def scrub_templates():
    if input("Are you sure? (y/N) ") == "y":
        for file in os.listdir():
            if file.endswith(".yaml"):
                os.remove(file)


task = int(input("Create templates: 1\nRemove templates: 2\n"))
if task == 1:
    create_templates()
elif task == 2:
    scrub_templates()