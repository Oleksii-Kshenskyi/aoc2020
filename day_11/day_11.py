import sys

class SeatMap:
    def __init__(self, filename):
        self.matrix = self._filename_to_matrix(filename)
        self.next = self.matrix.copy()

    def at(self, row, column):
        return self.matrix[row][column]
    def is_empty(self, pos):
        return pos == 'L'
    def empty(self):
        return 'L'
    def occupied(self):
        return '#'
    def is_occupied(self, pos):
        return pos == '#'
    def count_adjacent(self, row, column):
        prev_row_exists = row != 0
        next_row_exists = row < (len(self.matrix) - 1)
        prev_col_exists = column != 0
        next_col_exists = column < (len(self.matrix[0]) - 1)
        adjacents = ""
        if prev_row_exists and prev_col_exists:
            adjacents += self.at(row - 1, column - 1)
        if prev_row_exists:
            adjacents += self.at(row - 1, column)
        if prev_row_exists and next_col_exists:
            adjacents += self.at(row - 1, column + 1)
        if prev_col_exists:
            adjacents += self.at(row, column - 1)
        if next_col_exists:
            adjacents += self.at(row, column + 1)
        if next_row_exists and prev_col_exists:
            adjacents += self.at(row + 1, column - 1)
        if next_row_exists:
            adjacents += self.at(row + 1, column)
        if next_row_exists and next_col_exists:
            adjacents += self.at(row + 1, column + 1)
        return adjacents.count('#')
    def change_next_at(self, row, column, what):
        the_list = list(self.next[row])
        the_list[column] = what
        self.next[row] = "".join(the_list)
    def count_occupied(self):
        occupied_count = 0
        for row in range(0, len(self.matrix)):
            for column in range(0, len(self.matrix[0])):
                if self.at(row, column) == self.occupied():
                    occupied_count += 1
        
        return occupied_count
    def hashes_differ(self):
        return hash(tuple(self.matrix)) != hash(tuple(self.next))


    def simulate_step(self):
        for row in range(0, len(self.matrix)):
            for column in range(0, len(self.matrix[0])):
                current_pos = self.at(row, column)
                if self.is_empty(current_pos) and self.count_adjacent(row,column) == 0:
                    self.change_next_at(row, column, self.occupied())
                elif self.is_occupied(current_pos) and self.count_adjacent(row,column) >= 4:
                    self.change_next_at(row, column, self.empty())


    def simulate_until_it_stops(self):
        self.simulate_step()
        while(self.hashes_differ()):
            self.matrix = self.next.copy()
            self.simulate_step()
        
        return self.count_occupied()



    def _filename_to_matrix(self, filename):
        with open(filename, 'r') as f:
            return f.read().split()

def main():
    seatmap = SeatMap(sys.argv[1])
    occupied_after_simulation_stops = seatmap.simulate_until_it_stops()

    print("Part 1: occupied count after simulation stops is:", occupied_after_simulation_stops)


if __name__ == "__main__":
    main()