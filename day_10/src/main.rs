use std::path::Path;

fn file_to_input_vec(filename: &str) -> Vec<i32> {
    std::fs::read_to_string(filename)
        .unwrap()
        .lines()
        .map(|one_str| one_str.parse::<i32>().expect("ERROR: can't convert inputs to ints!"))
        .collect::<Vec<i32>>()
}

fn main() {
    let filename = std::env::args().nth(1).expect("ERROR: name of file with puzzle input not provided!");
    if !Path::new(&filename).is_file() {
        panic!("ERROR: The path provided in the argument is not a file name!");
    }
    let file_contents = file_to_input_vec(&filename);
    println!("Hello, args: {:?}", file_contents);
}
