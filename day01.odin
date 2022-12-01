package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"

main :: proc() {
    // read input

	filepath := "./inputs/day01.txt"

	data, ok := os.read_entire_file(filepath, context.allocator)
	if !ok {
		fmt.println("err reading file")
		return
	}
    defer delete(data, context.allocator)

	elves: [dynamic][dynamic]i64
	elf: [dynamic]i64

    it := string(data)
	for line in strings.split_lines_iterator(&it) {
		if line == "" {
			continue
		}

		i, ok := strconv.parse_i64_of_base(line, 10)
		if !ok {
			fmt.println("could not parse")
			return
		}

		append(&caloriesArr, i)
	}

	// find max

	max: i64 = 0

	for calories in caloriesArr {
		if calories > max {
			max = calories
		}
	}

	// print

	fmt.println(max)
}
