package main

import "core:fmt"
import "core:os"

EXIT_FAILURE :: 1

Point :: struct {
  x: int,
  y: int
}

slurp_file :: proc(filepath: string) -> (string) {
  contents, ok := os.read_entire_file_from_filename(filepath)
  if !ok {
    fmt.println("Could not read", filepath)
    os.exit(EXIT_FAILURE)
  }

  return string(contents)
}

main :: proc() {
  instructions := slurp_file("../input.txt")
  location := Point{0, 0}

  visited := make(map[Point]int)
  defer delete(visited)

  for dir in instructions {
    switch dir {
      case '^':
        location.y += 1
      case 'v':
        location.y -= 1
      case '>':
        location.x += 1
      case '<':
        location.x -= 1
    }

    // NOTE: This works due to Odin's use of "default to zero"
    // values for all variables.
    visited[location] += 1
  }

  fmt.println("Ending location:", location)
  fmt.println("Visited locations:", len(visited))
}
