package main

import "core:fmt"
import "core:os"
import "core:strings"

slurp_file :: proc(filepath: string) -> (content: string, success: bool) {
	data, ok := os.read_entire_file(filepath, context.allocator)

	// Could not read file
	if !ok {
		return "", false
	}
	defer delete(data, context.allocator)

	return string(data), true
}


solve :: proc(data: string) -> (int) {
  instructions := map[rune]int {
    cast(rune) '(' = 1,
    cast(rune) ')' = -1
  }

  floor := 0
  for char in data {
    floor = floor + instructions[char]
  }
  return floor
}

main :: proc() {
	input, ok := slurp_file("../input.txt")
  floor := solve(input)
  fmt.println("Santa is now on floor:", floor)
}
