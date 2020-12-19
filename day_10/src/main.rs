use std::collections::HashMap;
use std::path::Path;

fn file_to_input_vec(filename: &str) -> Vec<i32> {
    std::fs::read_to_string(filename)
        .unwrap()
        .lines()
        .map(|one_str| {
            one_str
                .parse::<i32>()
                .expect("ERROR: can't convert inputs to ints!")
        })
        .collect::<Vec<i32>>()
}

// part 1
fn calculate_prod_of_13_diffs(input: &Vec<i32>) -> usize {
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

    one_diffs * three_diffs
}

//part 2
fn get_map_of_size_to_permcount() -> HashMap<u8, u8> {
    let mut size_to_permcount: HashMap<u8, u8> = HashMap::new();

    size_to_permcount.insert(3, 2);
    size_to_permcount.insert(4, 4);
    size_to_permcount.insert(5, 7);

    size_to_permcount
}
fn get_permcounts_for(input: &[i32]) -> Vec<i32> {
    let permcounts = get_map_of_size_to_permcount();
    let mut sequences: Vec<i32> = vec![];

    let mut curr_seq_size: u8 = 0;
    for counter in 0..(input.len() - 1) {
        if counter < (input.len() - 1) && input[counter] == input[counter + 1] - 1 {
            curr_seq_size += 1;
        } else if counter > 0 && input[counter] == input[counter - 1] + 1 {
            curr_seq_size += 1;
            if curr_seq_size >= 3 && curr_seq_size <= 5 {
                sequences.push(permcounts.get(&curr_seq_size).unwrap().clone().into());
            }
            curr_seq_size = 0;
        }
    }

    sequences
}
fn get_arrangement_count_for_input(input: &[i32]) -> u64 {
    let permcounts = get_permcounts_for(input);

    permcounts
        .iter()
        .fold(1u64, |prod, elem| prod * (*elem as u64))
}

fn main() {
    let filename = std::env::args()
        .nth(1)
        .expect("ERROR: name of file with puzzle input not provided!");
    if !Path::new(&filename).is_file() {
        panic!("ERROR: The path provided in the argument is not a file name!");
    }
    let mut file_contents = file_to_input_vec(&filename);
    file_contents.push(0);
    file_contents.sort_unstable();
    file_contents.push(file_contents[file_contents.len() - 1] + 3);

    let prod_of_diffs = calculate_prod_of_13_diffs(&file_contents);
    println!(
        "Part 1, multiply 1-jolt diffs by 3-jolt diffs: {}",
        prod_of_diffs
    );

    let arrangement_count = get_arrangement_count_for_input(&file_contents);
    println!(
        "Part 2, count of possible arrangement permutations for the input: {}",
        arrangement_count
    );
}
