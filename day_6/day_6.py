import sys
import os

def file_to_group_answers(filename):
    file_string = ""
    with open(filename, "r") as filed:
        file_string = filed.read()
    return map(lambda line: line.split("\n"), file_string.split("\n\n"))

def count_any_in_group_answered_yes(answers):
    group_answers = map(lambda group: "".join(group), answers)
    list_of_unique_counts = map(lambda line: len(set(line)), group_answers)
    return sum(list_of_unique_counts)

def main():
    if len(sys.argv) <= 1:
        print("ERROR: need the name of the file with puzzle input!")
        return
    elif not os.path.isfile(sys.argv[1]):
        print("ERROR: first argument to the script has to be the name of the file with puzzle input.")
        return
    print(count_any_in_group_answered_yes(list(file_to_group_answers(sys.argv[1]))))

if __name__ == "__main__":
    main()