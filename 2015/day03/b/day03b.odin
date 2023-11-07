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
  santa_loc := Point{0, 0}
  robot_loc := Point{0, 0}

  visited := map[Point]int {
    {0, 0} = 1
  }
  defer delete(visited)

  for dir, i in instructions {
    // If we are making a Santa move, set loc to point
    // to santa_loc, otherwise to robot_loc.
    loc := &santa_loc if (i % 2) == 0 else &robot_loc

    switch dir {
      case '^':
        loc^.y += 1
      case 'v':
        loc^.y -= 1
      case '>':
        loc^.x += 1
      case '<':
        loc^.x -= 1
    }

    // NOTE: This works due to Odin's use of "default to zero"
    // values for all variables.
    visited[loc^] += 1
  }

  fmt.println("Visited locations:", len(visited))
}
