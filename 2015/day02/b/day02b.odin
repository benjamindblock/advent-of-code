package main

import "core:fmt"
import "core:os"
import "core:slice"
import "core:strconv"
import "core:strings"

Box :: struct {
  length: int,
  width: int,
  depth: int
}

slurp_file :: proc(filepath: string) -> (string, bool) {
  contents, ok := os.read_entire_file(filepath, context.allocator)
  if !ok {
    return "", false
  }
  defer delete(contents, context.allocator)

  return string(contents), true
}

to_box :: proc(raw: string) -> (Box) {
  dimensions := strings.split(raw, "x")

  length, length_ok := strconv.parse_int(dimensions[0])
  if !length_ok {
    fmt.println("Could not parse length to integer:", dimensions[0])
  }

  width, width_ok := strconv.parse_int(dimensions[1])
  if !width_ok {
    fmt.println("Could not parse width to integer:", dimensions[1])
  }

  depth, depth_ok := strconv.parse_int(dimensions[2])
  if !depth_ok {
    fmt.println("Could not parse depth to integer:", dimensions[2])
  }

  return Box{length, width, depth}
}

smallest_side_of_box :: proc(box: Box) -> (int, int) {
  sides := []int{box.length, box.width, box.depth}
  slice.sort(sides)

  return sides[0], sides[1]
}

perimeter :: proc(x: int, y: int) -> int {
  return x + x + y + y
}

main :: proc() {
  raw, _ := slurp_file("../input.txt")
  total_ribbon := 0

	for line in strings.split_lines_iterator(&raw) {
    box := to_box(line)
    x, y := smallest_side_of_box(box)
    ribbon := perimeter(x, y)
    bow := box.width * box.length * box.depth

    total_ribbon = total_ribbon + ribbon + bow
	}

  fmt.println("Total feet of ribbon:", total_ribbon)
}
