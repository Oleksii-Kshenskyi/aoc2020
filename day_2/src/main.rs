use std::env::args;
use regex::Regex;

#[derive(Debug)]
pub struct Rule {
    pub lower_bound: u32,
    pub upper_bound: u32,
    pub character: char,
}

#[derive(Debug)]
pub struct Password {
    pub password: String
}

#[derive(Debug)]
struct RulesAndPasses {
    rules_and_passes: Vec<(Rule, Password)>
}

impl RulesAndPasses {
    pub fn from_lines(lines: Vec<String>) -> Self {
        let mut rules_and_passes: Vec<(Rule, Password)> = vec![];
        let regex = Regex::new(r"(\d+)-(\d+) (.): (.+)").unwrap();

        for line in lines {
            let captures = regex.captures(&line).unwrap();
            let lower = captures.get(1).unwrap().as_str().to_owned().parse::<u32>().unwrap();
            let upper = captures.get(2).unwrap().as_str().to_owned().parse::<u32>().unwrap();
            let chr = captures.get(3).unwrap().as_str().chars().nth(0).unwrap();
            let pass = captures.get(4).unwrap().as_str();
            rules_and_passes.push(
                (Rule {
                    lower_bound: lower,
                    upper_bound: upper,
                    character: chr
                }, Password { password: pass.to_owned() })
            )
        }

        RulesAndPasses { rules_and_passes: rules_and_passes }
    }

    pub fn does_pass_conform_to_1st_rules(tuple: &(Rule, Password)) -> bool {
        let (rule, pass) = tuple;
        let count_of_matches = pass.password.matches(rule.character).count();

        count_of_matches >= rule.lower_bound as usize && count_of_matches <= rule.upper_bound as usize
    }

    pub fn does_pass_conform_to_2nd_rules(tuple: &(Rule, Password)) -> bool {
        let (rule, pass) = tuple;
        let mut matches: u32 = 0;
        if pass.password.chars().nth((rule.lower_bound - 1) as usize).unwrap() == rule.character {
            matches += 1;
        }
        if pass.password.chars().nth((rule.upper_bound - 1) as usize).unwrap() == rule.character {
            matches += 1;
        }
        
        matches == 1
    }

    pub fn count_conformities(&self) -> (usize, usize) {
        let mut conformities_1: usize = 0;
        let mut conformities_2: usize = 0;
        for tuple in &self.rules_and_passes {
            if RulesAndPasses::does_pass_conform_to_1st_rules(tuple) {
                conformities_1 += 1;
            }
            if RulesAndPasses::does_pass_conform_to_2nd_rules(tuple) {
                conformities_2 += 1;
            }
        }

        (conformities_1, conformities_2)
    }
}

fn main() {
    let args = args().collect::<Vec<String>>();
    let filename = args.get(1).expect("ERROR: no filename for puzzle input specified");
    
    let lines: Vec<String> = std::fs::read_to_string(filename).unwrap().lines().map(|elem| elem.to_owned()).collect::<Vec<String>>();
    let rap = RulesAndPasses::from_lines(lines);
    
    let (part_1, part_2) = rap.count_conformities();

    println!("{} passes confirm to rules according to the interpretation from part 1.", part_1);
    println!("{} passes confirm to rules according to the interpretation from part 2.", part_2);
}
