package main

import "core:fmt"
import "core:mem"
import "core:os"
import "core:strconv"
import "core:strings"

FAILURE_CODE :: 1
LIGHT_OFF :: 0
LIGHT_ON :: 1

// Light grid global var.
grid: [1000][1000]int

// Enumeration.
Command :: enum {
  On,
  Off,
  Toggle
}

// Enumerated array.
Command_Mapping :: [Command]string {
  .On = "turn on ",
  .Off = "turn off ",
  .Toggle = "toggle "
}

Point :: struct {
  x: int,
  y: int
}

Instruction :: struct {
  point1: Point,
  point2: Point,
  cmd: Command
}

// turn on 226,196 through 599,390
// turn off 199,133 through 461,193
// toggle 537,781 through 687,941
parse :: proc(_inst: string) -> (Instruction) {
  // Make a copy so that we can modify the instruction.
  inst := _inst

  // Determine the command
  cmd: Command
  if strings.has_prefix(inst, Command_Mapping[.On]) {
    cmd = Command.On
  } else if strings.has_prefix(inst, Command_Mapping[.Off]) {
    cmd = Command.Off
  } else if strings.has_prefix(inst, Command_Mapping[.Toggle]) {
    cmd = Command.Toggle
  }

  // Making a copy of Command_Mapping so that we can access it by
  // index dynamically.
  mappings := Command_Mapping
  to_remove := mappings[cmd]

  // Remove the appropriate command string from the instruction.
  // "turn on 226,196 through 599,390" => "226,196 through 599,390"
  inst, _ = strings.remove(inst, to_remove, 1)
  defer delete(inst)

  // Retrieve the two coordinate strings.
  // head => "226,196"
  // tail => "599,390"
  head, _, tail := strings.partition(inst, " through ")

  // Point 1
  // "226,196" => ["226", "196"]
  point1_arr := strings.split(head, ",")
  defer delete(point1_arr)
  x, _ := strconv.parse_int(point1_arr[0])
  y, _ := strconv.parse_int(point1_arr[1])
  point1 := Point{x, y}

  // Point 2
  // "599,390" => ["599", 390"]
  point2_arr := strings.split(tail, ",")
  defer delete(point2_arr)
  x, _ = strconv.parse_int(point2_arr[0])
  y, _ = strconv.parse_int(point2_arr[1])
  point2 := Point{x, y}

  return Instruction{point1, point2, cmd}
}

// Updates the grid in a void by altering values in the
// global 2D array we defined.
execute :: proc(inst: Instruction) {
  for x := inst.point1.x; x <= inst.point2.x; x += 1 {
    for y := inst.point1.y; y <= inst.point2.y; y += 1 {
      switch inst.cmd {
        case Command.On:
          grid[x][y] = LIGHT_ON
        case Command.Off:
          grid[x][y] = LIGHT_OFF
        case Command.Toggle:
          cur := grid[x][y]
          new := LIGHT_OFF if cur == LIGHT_ON else LIGHT_ON
          grid[x][y] = new
      }
    }
  }
}

slurp_file :: proc(filename: string) -> (string) {
  contents, ok := os.read_entire_file_from_filename(filename)

  if !ok {
    fmt.println("Could not open file:", filename)
    os.exit(FAILURE_CODE)
  }

  return string(contents)
}

solve :: proc() {
  contents := slurp_file("../input.txt")
  defer delete(contents)

  input := strings.split_lines(contents)
  defer delete(input)

  // Trim off the last newline at the end of the instructions.
  input = input[0:(len(input)-1)]

  for instruction in input {
    inst := parse(instruction)
    execute(inst)
  }

  total := 0
  for row in grid {
    for cell in row {
      if cell == LIGHT_ON {
        total += 1
      }
    }
  }

  fmt.println("Lights lit:", total)
}

main :: proc() {
  when ODIN_DEBUG {
    track: mem.Tracking_Allocator
    mem.tracking_allocator_init(&track, context.allocator)
    context.allocator = mem.tracking_allocator(&track)
    defer {
      if len(track.allocation_map) > 0 {
        fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
        for _, entry in track.allocation_map {
          fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
        }
      }
      if len(track.bad_free_array) > 0 {
        fmt.eprintf("=== %v incorrect frees: ===\n", len(track.bad_free_array))
        for entry in track.bad_free_array {
          fmt.eprintf("- %p @ %v\n", entry.memory, entry.location)
        }
      }
      mem.tracking_allocator_destroy(&track)
    }
  }

  solve()
}
