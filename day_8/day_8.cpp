#include <iostream>
#include <fstream>
#include <cstring>
#include <iterator>
#include <algorithm>
#include <cstdlib>

static constexpr char PUZZLE_INPUT[] = "puzzle_input.txt";


// Instruction parsing
enum class InstructionKind {
    Nop,
    Jmp,
    Acc
};

class Instruction {
    public:
        Instruction(const std::vector<std::string>& input_vec) {
            this->kind = this->parse_instruction_kind(input_vec[0]);
            this->arg = this->parse_arg(input_vec[1]);
            this->times_executed = 0;
        }

        InstructionKind kind;
        size_t times_executed;
        int32_t arg;

    private:
        InstructionKind parse_instruction_kind(const std::string& input) {
            if(input == "jmp")
                return InstructionKind::Jmp;
            else if(input == "nop")
                return InstructionKind::Nop;
            else if(input == "acc")
                return InstructionKind::Acc;
            else return InstructionKind::Nop;
        }
        int32_t parse_arg(const std::string& input) {
            char sign = input[0];
            int32_t retval = 0;
            std::string nosign_input = input.substr(1);
            try {
                retval = std::stoi(nosign_input);
            }
            catch(...) {
                std::cout << "ERROR: conversion to int failed on " << nosign_input << "!" << std::endl;
                exit(1);
            }
            return (sign == '-') ? 0 - retval : retval;
        }
};


// Utility funcs
void print_vec_of_strings(const std::vector<std::string>& vec) {
    std::cout << "{" << std::endl;
    for(const auto& str : vec) {
        std::cout << "\t\"" << str << "\"," << std::endl;
    }
    std::cout << "}" << std::endl;
}
std::string instr_kind_to_str(InstructionKind kind)  {
    if(kind == InstructionKind::Jmp)
        return "Jmp";
    else if(kind == InstructionKind::Nop)
        return "Nop";
    else if(kind == InstructionKind::Acc)
        return "Acc";
    else return "Nop";
}
void print_vec_of_instructions(const std::vector<Instruction>& vec) {
    std::cout << "Vec {" << std::endl;
    for(const Instruction& instr : vec) {
        std::cout << "\tInstruction {" << std::endl;
        std::cout << "\t\tKind: " << instr_kind_to_str(instr.kind) << "," << std::endl;
        std::cout << "\t\tArg: " << instr.arg << "," << std::endl;
        std::cout << "\t}," << std::endl;
    }
    std::cout << "}" << std::endl;
}
std::vector<std::string> file_to_vector(std::string filename) {
    std::ifstream input { PUZZLE_INPUT };
    std::vector<std::string> file_lines {};

    std::copy(std::istream_iterator<std::string>(input),
              std::istream_iterator<std::string>(),
              std::back_inserter(file_lines));

    return file_lines;
}
std::vector<Instruction> file_vec_to_instructions(const std::vector<std::string>& file_vec) {
    std::vector<Instruction> instructions {};
    for(size_t index = 0; index < file_vec.size(); index = index + 2) {
        std::vector<std::string> single_instruction_unparsed { file_vec.begin() + index, file_vec.begin() + index + 2};
        instructions.emplace_back(Instruction(single_instruction_unparsed));
    }

    return instructions;
}


// Day 8 task logic
class Interpreter {
    public:
        Interpreter(std::vector<Instruction> instructions): instructions(instructions), acc(0), instruction_pointer(0) {}

        int32_t execute(bool test_mode = false) {
            this->restart();

            while(instruction_pointer < instructions.size()) {
                Instruction& current_instr = this->instructions[instruction_pointer];
                if(current_instr.times_executed != 0) {
                    return (test_mode) ? -1 : this->acc;
                }
                else current_instr.times_executed++;

                if(current_instr.kind == InstructionKind::Acc)
                    this->acc += current_instr.arg;
                else if(current_instr.kind == InstructionKind::Jmp)
                    this->instruction_pointer += current_instr.arg;

                if(current_instr.kind != InstructionKind::Jmp)
                    this->instruction_pointer++;
            }

            return this->acc;
        }

        int32_t repair_and_execute() {
            this->restart();

            int32_t repaired_acc = -2;
            size_t nop_or_jmp = 0;
            repaired_acc = Interpreter(this->instructions).execute(true);
            while(repaired_acc < 0) {
                std::vector<Instruction> local_instrs { this->instructions };

                int32_t supposed_nop_or_jmp = Interpreter::next_nop_or_jmp(local_instrs, nop_or_jmp);
                if(supposed_nop_or_jmp != -1)
                    nop_or_jmp = supposed_nop_or_jmp;
                else break;

                Interpreter::swap_nop_and_jmp_at(local_instrs, nop_or_jmp);
                nop_or_jmp++;

                Interpreter local_interp { local_instrs };
                repaired_acc = local_interp.execute(true);
            }

            return repaired_acc;
        }
    
    private:
        void restart() {
            this->acc = 0;
            this->instruction_pointer = 0;
            for(auto& instr : instructions) {
                instr.times_executed = 0;
            }
        }

        static int32_t next_nop_or_jmp(const std::vector<Instruction>& instrs, size_t offset) {
            for(size_t counter = offset; counter < instrs.size(); counter++) {
                if(instrs[counter].kind == InstructionKind::Jmp || 
                   instrs[counter].kind == InstructionKind::Nop) {
                    return counter;
                }
            }

            return -1;
        }

        static void swap_nop_and_jmp_at(std::vector<Instruction>& local_instrs, size_t nop_or_jmp) {
            if(local_instrs[nop_or_jmp].kind == InstructionKind::Nop)
                local_instrs[nop_or_jmp].kind = InstructionKind::Jmp;
            else if(local_instrs[nop_or_jmp].kind == InstructionKind::Jmp)
                local_instrs[nop_or_jmp].kind = InstructionKind::Nop;
            else
                throw "swap_nop_and_jmp_at(): ERROR: expected Nop or Jmp!\n";
        }

        std::vector<Instruction> instructions;
        int32_t acc;
        size_t instruction_pointer;
};


int main() {
    Interpreter interpreter { file_vec_to_instructions(file_to_vector(PUZZLE_INPUT)) };

    std::cout << "Exit when an instruction is repeated: " << interpreter.execute() << std::endl;
    std::cout << "Repair corrupted instruction and execute: " << interpreter.repair_and_execute() << std::endl;
    
    return 0;
}