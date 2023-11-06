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

surface_area :: proc(box: Box) -> (int) {
  return (2 * box.length * box.width) +
    (2 * box.width * box.depth) +
    (2 * box.length * box.depth)
}

area :: proc(x: int, y: int) -> (int) {
  return x * y
}

main :: proc() {
  raw, _ := slurp_file("../input.txt")
  total_paper := 0

  // How to read line-by-line?
  // Option 1: Split and iterate
  // dimensions := strings.split(raw, "\n")
  // for box in dimensions {
  //   if (box == "") {
  //     continue
  //   }
  // }

  // Option 2: Iterate with an iterator
	for line in strings.split_lines_iterator(&raw) {
    box := to_box(line)
    core_paper := surface_area(box)

    x, y := smallest_side_of_box(box)
    extra := area(x, y)

    box_total := core_paper + extra
    total_paper = total_paper + box_total
	}

  fmt.println("Total sq. feet of wrapping paper:", total_paper)
}
