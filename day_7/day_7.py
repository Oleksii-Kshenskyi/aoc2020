from dataclasses import dataclass
import sys
import os

@dataclass
class Rule:
    container: str
    children: list

def file_to_lines(filename):
    file_string = ""
    with open(filename, "r") as filed:
        file_string = filed.read()
    return file_string.split("\n")

def line_to_rule(line):
    rule = Rule("", [])
    contain_split = line.split(" contain ")
    rule.container = " ".join(contain_split[0].split(" ")[:-1])

    children_unparsed = contain_split[1][:-1].split(", ")

    if children_unparsed == ["no other bags"]:
        return rule

    children = []
    for child_u in children_unparsed:
        child_split = child_u.split(" ")
        count = int(child_split[0])
        bag_kind = " ".join(child_split[1:3])
        children += [(count, bag_kind)]

    rule.children = children
    return rule

def lines_to_rules(lines):
    return list(map(lambda line: line_to_rule(line), lines))

def who_contains_targets(rules, list_of_names):
    target_containers = []
    for rule in rules:
        for name in list_of_names:
            children_names = list(child[1] for child in rule.children)
            if name in children_names:
                target_containers += [rule.container]
    if target_containers != []:
        target_containers += who_contains_targets(rules, target_containers)
    return list(set(target_containers))

def main():
    if len(sys.argv) <= 1:
        print("ERROR: need the name of the file with puzzle input!")
        return
    elif not os.path.isfile(sys.argv[1]):
        print("ERROR: first argument to the script has to be the name of the file with puzzle input.")
        return
    lines = list(file_to_lines(sys.argv[1]))
    print("Number of bags containing 'shiny gold':", len(who_contains_targets(lines_to_rules(lines), ["shiny gold"])))


if __name__ == "__main__":
    main()