let fs = require("fs");

function range(low, high) {
    return [...Array(high).keys()].slice(low);
}
function get_slice_before(slice_of_this, before_index, slice_size) {
    let slice_low_bound = before_index - slice_size;
    let slice_high_bound = before_index;
    return slice_of_this.slice(slice_low_bound, slice_high_bound);
}


function get_numbers_from_file(filename) {
    return fs.readFileSync(filename).toString().split("\n").map(str => parseInt(str));
}

function is_valid_for_slice(slice, number) {
    for (first of range(0, slice.length)) {
        for(second of range(0, slice.length)) {
            if(slice[second] + slice[first] == number && slice[second] != slice[first]) {
                return true;
            }
        }
    }
    return false;
}
function get_first_invalid(numbers, preambleSize) {
    for(counter of range(preambleSize, numbers.length + 1)) {
        let slice_before_me = get_slice_before(numbers, counter, preambleSize);
        if(!is_valid_for_slice(slice_before_me, numbers[counter])) {
            return numbers[counter];
        }
    }

    return null;
}


function get_slice_that_sums_to(slice_of, sums_to) {
    for(index1 of range(0, slice_of.length)) {
        for(index2 of range(0, slice_of.length)) {
            if(index1 != index2) {
                let [low, high] = [index1, index2].sort((a, b) => a - b);
                let current_slice = slice_of.slice(low, high);
                if(current_slice.reduce((a, b) => a + b, 0) == sums_to) {
                    return current_slice;
                }
            }
        }
    }

    return [];
}
function sum_min_and_max_for(slice) {
    return Math.min.apply(Math, slice) + Math.max.apply(Math, slice);
}


function process_file(filename, preamble_size) {
    let numbers = get_numbers_from_file(filename);

    let first_not_valid = get_first_invalid(numbers, parseInt(preamble_size));
    console.log(`First number that is not valid in the sequence: ${first_not_valid}.`);

    let slice_sums_to_invalid = get_slice_that_sums_to(numbers, first_not_valid);
    let sum_of_min_max_of_slice = sum_min_and_max_for(slice_sums_to_invalid);
    console.log(`Sum of min and max for slice that sums to ${first_not_valid} is ${sum_of_min_max_of_slice}.`);
}


function main() {
    let [filename, preamble_size] = process.argv.slice(2, 4);
    process_file(filename, preamble_size);
}

main()