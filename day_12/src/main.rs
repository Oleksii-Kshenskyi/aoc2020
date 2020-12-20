use std::path::Path;

fn file_to_instructions(filename: &str) -> Vec<String> {
    if !Path::new(filename).is_file() {
        panic!("ERROR: provided name is not a valid file path!");
    }
    std::fs::read_to_string(filename)
        .unwrap()
        .split("\n")
        .map(|one_str| one_str.to_owned())
        .collect()
}

fn main() {
    let filename: &str = &std::env::args().nth(1).expect("ERROR: no filename given!");
    let instructions = file_to_instructions(filename);
    println!("FILE: {:?}", instructions);
}
