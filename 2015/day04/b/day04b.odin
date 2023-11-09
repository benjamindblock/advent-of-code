package main

import "core:crypto/md5"
import "core:fmt"
import "core:encoding/hex"
import "core:mem"
import "core:os"
import "core:strconv"
import "core:strings"

FAILURE_CODE :: 1

// Part 2 only requires changing our prefix to search for.
// Now with six zeroes instead of just five.
PREFIX :: "000000"

// Global variable to track our number
cursor := 0

slurp_file :: proc(filename: string) -> (string) {
  contents, ok := os.read_entire_file_from_filename(filename)
  if !ok {
    fmt.println("Could not read", filename)
    os.exit(FAILURE_CODE)
  }

  return string(contents)
}

next_input :: proc(input: string) -> (string) {
  // Convert the cursor to string, and then increment by one.
  buffer: [16]byte
  cursor_str := strconv.itoa(buffer[:], cursor)
  cursor += 1

  // Concat the string and integer to create the next input.
  next := strings.concatenate([]string{input, cursor_str})
  return next
}

// Given a string, find its MD5 hash and then
// print its hex representation.
md5_in_hex_str :: proc(input: string) -> (string) {
  md5 := md5.hash_string(input)
  md5_in_hex := fmt.aprintf("%02x", string(md5[:]))
  return md5_in_hex
}

solve :: proc() {
  contents := slurp_file("../input.txt") 
  defer delete(contents)

  split := strings.split_lines(contents)
  defer delete(split)

  // Get the first line of the file (there is a newline at
  // end which results in a blank line).
  input := split[0]

  solved := false
  for !solved {
    potential := next_input(input)
    defer delete(potential)

    md5 := md5_in_hex_str(potential)
    defer delete(md5)

    if strings.has_prefix(md5, PREFIX) {
      solved = true
      fmt.println("Solution:", potential)
      fmt.println("MD5 hash:", md5)
    }
  }
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
