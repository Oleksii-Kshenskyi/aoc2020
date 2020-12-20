import sys

class SeatMap:
    def __init__(self, filename):
        self.matrix = self._filename_to_matrix(filename)
        self.next = self.matrix.copy()

    def _filename_to_matrix(self, filename):
        with open(filename, 'r') as f:
            return f.read().split()
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
        for row in range(0, len(self.next)):
            for column in range(0, len(self.next[0])):
                if self.at(row, column) == self.occupied():
                    occupied_count += 1
        
        return occupied_count
    def hashes_differ(self):
        return hash(tuple(self.matrix)) != hash(tuple(self.next))


    # part 1 simulation
    def simulate_step(self):
        for row in range(0, len(self.matrix)):
            for column in range(0, len(self.matrix[0])):
                current_pos = self.at(row, column)
                if self.is_empty(current_pos) and self.count_adjacent(row,column) == 0:
                    self.change_next_at(row, column, self.occupied())
                elif self.is_occupied(current_pos) and self.count_adjacent(row,column) >= 4:
                    self.change_next_at(row, column, self.empty())



    def count_ul(self, row, column):
        scan_row = row - 1
        scan_col = column - 1
        while(scan_row >= 0 and scan_col >= 0):
            if(self.at(scan_row, scan_col) == self.occupied()):
                return 1
            elif(self.at(scan_row, scan_col) == self.empty()):
                break
            scan_row -= 1
            scan_col -= 1
        return 0
    def count_u(self, row, column):
        scan_row = row - 1
        while(scan_row >= 0):
            if(self.at(scan_row, column) == self.occupied()):
                return 1
            elif(self.at(scan_row, column) == self.empty()):
                break
            scan_row -= 1
        return 0
    def count_ur(self, row, column):
        scan_row = row - 1
        scan_col = column + 1
        while(scan_row >= 0 and scan_col <= (len(self.matrix[0]) - 1)):
            if(self.at(scan_row, scan_col) == self.occupied()):
                return 1
            elif(self.at(scan_row, scan_col) == self.empty()):
                break
            scan_row -= 1
            scan_col += 1
        return 0
    def count_l(self, row, column):
        scan_col = column - 1
        while(scan_col >= 0):
            if(self.at(row, scan_col) == self.occupied()):
                return 1
            elif(self.at(row, scan_col) == self.empty()):
                break
            scan_col -= 1
        return 0
    def count_r(self, row, column):
        scan_col = column + 1
        while(scan_col <= len(self.matrix[0]) - 1):
            if(self.at(row, scan_col) == self.occupied()):
                return 1
            elif(self.at(row, scan_col) == self.empty()):
                break
            scan_col += 1
        return 0
    def count_dl(self, row, column):
        scan_row = row + 1
        scan_col = column - 1
        while(scan_row <= (len(self.matrix) - 1) and scan_col >= 0):
            if(self.at(scan_row, scan_col) == self.occupied()):
                return 1
            elif(self.at(scan_row, scan_col) == self.empty()):
                break
            scan_row += 1
            scan_col -= 1
        return 0
    def count_d(self, row, column):
        scan_row = row + 1
        while(scan_row <= (len(self.matrix) - 1)):
            if(self.at(scan_row, column) == self.occupied()):
                return 1
            elif(self.at(scan_row, column) == self.empty()):
                break
            scan_row += 1
        return 0
    def count_dr(self, row, column):
        scan_row = row + 1
        scan_col = column + 1
        while(scan_row <= (len(self.matrix) - 1) and scan_col <= len(self.matrix[0]) - 1):
            if(self.at(scan_row, scan_col) == self.occupied()):
                return 1
            elif(self.at(scan_row, scan_col) == self.empty()):
                break
            scan_row += 1
            scan_col += 1
        return 0


    def count_stretch(self, row, column):
        seeing_seats = 0

        seeing_seats += self.count_ul(row, column)
        seeing_seats += self.count_u(row, column)
        seeing_seats += self.count_ur(row, column)
        seeing_seats += self.count_l(row, column)
        seeing_seats += self.count_r(row, column)
        seeing_seats += self.count_dl(row, column)
        seeing_seats += self.count_d(row, column)
        seeing_seats += self.count_dr(row, column)

        return seeing_seats
    # part 2 simulation
    def simulate_step_2(self):
        for row in range(0, len(self.matrix)):
            for column in range(0, len(self.matrix[0])):
                current_pos = self.at(row, column)
                if self.is_empty(current_pos) and self.count_stretch(row,column) == 0:
                    self.change_next_at(row, column, self.occupied())
                elif self.is_occupied(current_pos) and self.count_stretch(row,column) >= 5:
                    self.change_next_at(row, column, self.empty())


    def simulate_until_it_stops(self, simulation_method):
        simulation_method()
        while(self.hashes_differ()):
            self.matrix = self.next.copy()
            simulation_method()
        
        return self.count_occupied()

def main():
    seatmap = SeatMap(sys.argv[1])

    print("Part 1: occupied count after simulation stops is:", seatmap.simulate_until_it_stops(seatmap.simulate_step))

    seatmap = SeatMap(sys.argv[1])

    print("Part 2: occupied count after simulation stops is:", seatmap.simulate_until_it_stops(seatmap.simulate_step_2))


if __name__ == "__main__":
    main()