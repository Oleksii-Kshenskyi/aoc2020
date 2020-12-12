use std::collections::HashMap;
use std::env::args;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CellKind {
    OpenSquare,
    Tree,
}

pub fn input_to_map(input: String) -> (HashMap<(i32, i32), CellKind>, i32) {
    let (mut column, mut row) = (0 as i32, 0 as i32);
    let mut result_map: HashMap<(i32, i32), CellKind> = HashMap::new();
    for chr in input.chars() {
        let cell_kind: Option<CellKind>;
        match chr {
            '.' => {
                cell_kind = Some(CellKind::OpenSquare);
            }
            '#' => {
                cell_kind = Some(CellKind::Tree);
            }
            _ => {
                cell_kind = None;
            }
        }

        if let Some(the_kind) = cell_kind {
            result_map.insert((row, column), the_kind);
        }

        match chr {
            '\n' => {
                row += 1;
                column = 0;
            }
            _ => {
                column += 1;
            }
        }
    }

    (result_map, column)
}

pub fn single_step_map(
    current_pos: (i32, i32),
    columns_count: i32,
    slope: (i32, i32),
) -> (i32, i32) {
    let (slope_down, slope_right) = slope;
    (
        current_pos.0 + slope_down,
        (current_pos.1 + slope_right) % columns_count,
    )
}

pub fn traverse_counting_trees(
    input_map: &HashMap<(i32, i32), CellKind>,
    column_count: i32,
    slope: (i32, i32),
) -> i32 {
    let mut current_pos = (0 as i32, 0 as i32);
    let mut tree_count = 0 as i32;
    while input_map.contains_key(&current_pos) {
        let current_cell = input_map.get(&current_pos);

        if current_cell.is_some() && *current_cell.unwrap() == CellKind::Tree {
            tree_count += 1;
        }

        current_pos = single_step_map(current_pos, column_count, slope);
    }

    tree_count
}

pub fn check_for_slope(
    input_map: &HashMap<(i32, i32), CellKind>,
    column_count: i32,
    slope: (i32, i32),
) -> i64 {
    let (down, right) = slope;
    let trees = traverse_counting_trees(&input_map, column_count, slope);
    println!(
        "The 'right {}, down {}' tree count is: {}",
        right, down, trees
    );

    trees as i64
}

fn main() {
    let args = args().collect::<Vec<String>>();
    let filename = args
        .get(1)
        .expect("ERROR: no filename for puzzle input specified");

    let line = std::fs::read_to_string(filename).unwrap();
    let (the_input_map, columns) = input_to_map(line);

    let first = check_for_slope(&the_input_map, columns, (1, 1));
    let second = check_for_slope(&the_input_map, columns, (1, 3));
    let third = check_for_slope(&the_input_map, columns, (1, 5));
    let fourth = check_for_slope(&the_input_map, columns, (1, 7));
    let fifth = check_for_slope(&the_input_map, columns, (2, 1));

    println!(
        "The product of tree counts is {}.",
        (first * second * third * fourth * fifth) as i64
    );
}
