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

OPEN_PAREN : rune : '('
CLOSED_PAREN : rune : ')'

solve :: proc(data: string) -> (int) {
  instructions : map[rune]int = {
    OPEN_PAREN = 1,
    CLOSED_PAREN = -1
  }

  floor := 0
  for char, index in data {
    floor = floor + instructions[char]

    if (floor == -1) {
      return index + 1
    }
  }
  return -1
}

main :: proc() {
	input, ok := slurp_file("../input.txt")
  step := solve(input)
  fmt.println("Santa entered the basement on step:", step)
}
