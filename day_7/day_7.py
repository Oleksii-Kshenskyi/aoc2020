import sys
import os

def file_to_group_answers(filename):
    file_string = ""
    with open(filename, "r") as filed:
        file_string = filed.read()
    return file_string.split("\n")

def main():
    if len(sys.argv) <= 1:
        print("ERROR: need the name of the file with puzzle input!")
        return
    elif not os.path.isfile(sys.argv[1]):
        print("ERROR: first argument to the script has to be the name of the file with puzzle input.")
        return
    file_list = list(file_to_group_answers(sys.argv[1]))
    print("Puzzle Input: ", file_list)

if __name__ == "__main__":
    main()