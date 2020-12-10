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

    pub fn does_pass_conform_to_rule(tuple: &(Rule, Password)) -> bool {
        let (rule, pass) = tuple;
        let count_of_matches = pass.password.matches(rule.character).count();

        count_of_matches >= rule.lower_bound as usize && count_of_matches <= rule.upper_bound as usize
    }

    pub fn count_conformities(&self) -> usize {
        let mut conformities: usize = 0;
        for tuple in &self.rules_and_passes {
            if RulesAndPasses::does_pass_conform_to_rule(tuple) {
                conformities += 1;
            }
        }

        conformities
    }
}

fn main() {
    let args = args().collect::<Vec<String>>();
    let filename = args.get(1).expect("ERROR: no filename for puzzle input specified");
    
    let lines: Vec<String> = std::fs::read_to_string(filename).unwrap().lines().map(|elem| elem.to_owned()).collect::<Vec<String>>();
    let rap = RulesAndPasses::from_lines(lines);
    
    println!("In the current puzzle input, {} passes confirm to rules.", rap.count_conformities());
}
