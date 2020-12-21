use std::path::Path;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum InstrType {
    N,
    S,
    W,
    E,
    F,
    L,
    R
}

#[derive(Debug, Clone)]
struct Instruction {
    itype: InstrType,
    value: i32,
}
impl Instruction {
    pub fn new(init_val: &str) -> Self {
        let itype: InstrType = match &init_val[0..1] {
            "N" => InstrType::N,
            "S" => InstrType::S,
            "W" => InstrType::W,
            "E" => InstrType::E,
            "F" => InstrType::F,
            "R" => InstrType::R,
            "L" => InstrType::L,
            _ => unreachable!("Instruction::new(): this should have never happened!")
        };
        let ival: i32 = init_val[1..].parse().expect("ERROR: couldn't convert numeric part of instruction");
        Self {
            itype: itype,
            value: ival
        }
    }
}

struct Waypoint {
    north_south_pos: i32,
    east_west_pos: i32,
}
impl Waypoint {
    pub fn new() -> Self {
        Self {
            north_south_pos: 1,
            east_west_pos: 10,
        }
    }
    pub fn move_to(&mut self, dir: &InstrType, val: i32) {
        match dir {
            InstrType::N => self.north_south_pos += val,
            InstrType::E => self.east_west_pos += val,
            InstrType::S => self.north_south_pos -= val,
            InstrType::W => self.east_west_pos -= val,
            _ => unreachable!("Waypoint::move_to(): This should have never happened")
        }
    }
    pub fn turn(&mut self, side: InstrType, degrees: i32) {
        let turn_val: i8 = (degrees / 90) as i8;
        match side {
            InstrType::R => self.transpose_turning(turn_val),
            InstrType::L => self.transpose_turning(-turn_val),
            _ => unreachable!("Waypoint::turn(): This should have never happened...")
        }
    }
    fn transpose_turning(&mut self, turn_val: i8) {
        match turn_val {
            1 => self.north_south_pos = -self.north_south_pos,
            2 => { 
                self.north_south_pos = -self.north_south_pos;
                self.east_west_pos = -self.east_west_pos;
            },
            3 => self.east_west_pos = -self.east_west_pos,
            -1 => self.east_west_pos = -self.east_west_pos,
            -2 => { 
                self.north_south_pos = -self.north_south_pos;
                self.east_west_pos = -self.east_west_pos;
            },
            -3 => self.north_south_pos = -self.north_south_pos,
            _ => unreachable!("Waypoint::transpose_turning(): This should have never happened...")
        }
    }
}

struct Ship {
    instructions: Vec<Instruction>,
    north_south_pos: i32,
    east_west_pos: i32,
    facing: InstrType,
    waypoint: Waypoint,
}
impl Ship {
    pub fn new(instrs: Vec<Instruction>) -> Self {
        Self {
            instructions: instrs,
            north_south_pos: 0,
            east_west_pos: 0,
            facing: InstrType::E,
            waypoint: Waypoint::new(),
        }
    }

    pub fn follow_set(&mut self) -> &mut Self {
        let instrs = self.instructions.clone();
        instrs.iter().for_each(
            |instr| {
                self.follow_single(instr.clone());
            }
        );

        self
    }

    pub fn follow_set_waypoint(&mut self) -> &mut Self {
        let instrs = self.instructions.clone();
        instrs.iter().for_each(
            |instr| {
                self.follow_single_waypoint(instr.clone());
            }
        );

        self
    }

    pub fn manhattan(&self) -> i32 {
        self.north_south_pos.abs() + self.east_west_pos.abs()
    }

    pub fn reboot(&mut self) -> &mut Self {
        self.north_south_pos = 0;
        self.east_west_pos = 0;
        self.facing = InstrType::E;
        self.waypoint.east_west_pos = 10;
        self.waypoint.north_south_pos = 1;

        self
    }

    fn follow_single(&mut self, instr: Instruction) {
        match instr.itype {
            InstrType::N => self.go(&InstrType::N, instr.value),
            InstrType::S => self.go(&InstrType::S, instr.value),
            InstrType::E => self.go(&InstrType::E, instr.value),
            InstrType::W => self.go(&InstrType::W, instr.value),
            InstrType::L => self.turn(InstrType::L, instr.value),
            InstrType::R => self.turn(InstrType::R, instr.value),
            InstrType::F => self.go(&self.facing.clone(), instr.value),
        }
    }

    fn follow_single_waypoint(&mut self, instr: Instruction) {
        match instr.itype {
            InstrType::N => self.waypoint.move_to(&InstrType::N, instr.value),
            InstrType::S => self.waypoint.move_to(&InstrType::S, instr.value),
            InstrType::E => self.waypoint.move_to(&InstrType::E, instr.value),
            InstrType::W => self.waypoint.move_to(&InstrType::W, instr.value),
            InstrType::L => self.waypoint.turn(InstrType::L, instr.value),
            InstrType::R => self.waypoint.turn(InstrType::R, instr.value),
            InstrType::F => self.follow_waypoint_times(instr.value),
        }
    }

    fn follow_waypoint_times(&mut self, times: i32) {
        self.north_south_pos += self.waypoint.north_south_pos * times;
        self.east_west_pos += self.waypoint.east_west_pos * times;
    }

    fn turn(&mut self, side: InstrType, degrees: i32) {
        let turn_val: i8 = (degrees / 90) as i8;
        match side {
            InstrType::R => self.facing = Ship::i8_to_dir((Ship::dir_to_i8(&self.facing) + turn_val) % 4),
            InstrType::L => self.facing = Ship::i8_to_dir((Ship::dir_to_i8(&self.facing) - turn_val) % 4),
            _ => unreachable!("turn(): This should have never happened...")
        }
    }
    pub fn dir_to_i8(dir: &InstrType) -> i8 {
        match dir {
            InstrType::N => 0,
            InstrType::E => 1,
            InstrType::S => 2,
            InstrType::W => 3,
            _ => unreachable!("dir_to_i8(): This should have never happened...")
        }
    }
    pub fn i8_to_dir(i8_val: i8) -> InstrType {
        match i8_val {
            0 => InstrType::N,
            1 => InstrType::E,
            2 => InstrType::S,
            3 => InstrType::W,
            -1 => InstrType::W,
            -2 => InstrType::S,
            -3 => InstrType::E,
            _ => unreachable!("i8_to_dir(): This should have never happened..."),
        }
    }
    fn go(&mut self, dir: &InstrType, val: i32) {
        match dir {
            InstrType::N => self.north_south_pos += val,
            InstrType::E => self.east_west_pos += val,
            InstrType::S => self.north_south_pos -= val,
            InstrType::W => self.east_west_pos -= val,
            _ => unreachable!("go(): This should have never happened")
        }
    }
}

fn file_to_lines(filename: &str) -> Vec<String> {
    if !Path::new(filename).is_file() {
        panic!("ERROR: provided name is not a valid file path!");
    }
    std::fs::read_to_string(filename)
        .unwrap()
        .split("\n")
        .map(|one_str| one_str.to_owned())
        .collect()
}
fn lines_to_instructions(lines :Vec<String>) -> Vec<Instruction> {
    let mut instrs: Vec<Instruction> = vec![];
    lines.iter().for_each(|line| instrs.push(Instruction::new(line)));

    instrs
}

fn main() {
    let filename: &str = &std::env::args().nth(1).expect("ERROR: no filename given!");
    let mut ship = Ship::new(lines_to_instructions(file_to_lines(filename)));
    let manhattan = ship
        .follow_set()
        .manhattan();
    println!("Part 1: manhattan distance after instructions: {}", manhattan);

    let manhattan = ship.reboot().follow_set_waypoint().manhattan();
    println!("Part 2: manhattan distance after following waypoint: {}", manhattan);
}
