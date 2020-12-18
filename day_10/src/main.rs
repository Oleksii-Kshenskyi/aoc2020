use std::path::Path;

fn file_to_input_vec(filename: &str) -> Vec<i32> {
    std::fs::read_to_string(filename)
        .unwrap()
        .lines()
        .map(|one_str| one_str.parse::<i32>().expect("ERROR: can't convert inputs to ints!"))
        .collect::<Vec<i32>>()
}

fn calculate_prod_of_13_diffs(input: &mut Vec<i32>) -> usize {
    input.sort_unstable();

    let mut one_diffs: usize = 0;
    let mut three_diffs: usize = 0;
    for i in 0..input.len() {
        let curr_elem = input[i];
        if i != input.len() - 1 {
            let next_elem = input[i + 1];
            if next_elem == curr_elem + 1 {
                one_diffs += 1;
            } else if next_elem == curr_elem + 3 {
                three_diffs += 1;
            }
        }
    }
    one_diffs += 1; three_diffs += 1;

    one_diffs * three_diffs
}

fn main() {
    let filename = std::env::args().nth(1).expect("ERROR: name of file with puzzle input not provided!");
    if !Path::new(&filename).is_file() {
        panic!("ERROR: The path provided in the argument is not a file name!");
    }
    let mut file_contents = file_to_input_vec(&filename);

    let prod_of_diffs = calculate_prod_of_13_diffs(&mut file_contents);
    println!("Part 1, multiply 1-jolt diffs by 3-jolt diffs: {}", prod_of_diffs);
}
