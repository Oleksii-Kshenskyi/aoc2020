import sys

def filename_to_matrix(filename):
    with open(filename, 'r') as f:
        return f.read().split()

def main():
    filename = sys.argv[1]
    matrix = filename_to_matrix(filename)
    print("File:", matrix)


if __name__ == "__main__":
    main()