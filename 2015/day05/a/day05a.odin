package main

import "core:fmt"
import "core:mem"
import "core:os"
import "core:strings"
import "core:unicode/utf8"

FAILURE_CODE :: 0
VOWEL_COUNT :: 3
VOWELS: [5]string: {"a", "e", "i", "o", "u"}
BAD_PAIRS: [4]string: {"ab", "cd", "pq", "xy"}

slurp_file :: proc(filename: string) -> (string) {
  contents, ok := os.read_entire_file_from_filename(filename)
  if !ok {
    fmt.println("Could not read file:", filename)
    os.exit(FAILURE_CODE)
  }

  return string(contents)
}

contains_bad_char_pairs :: proc(str: string) -> (bool) {
  for pair in BAD_PAIRS {
    if strings.index(str, pair) != -1 {
      return true
    }
  }
  return false
}

contains_required_num_vowels :: proc(str: string) -> (bool) {
  count := 0
  for vowel in VOWELS {
    count += strings.count(str, vowel)
  }
  return count >= VOWEL_COUNT
}

contains_sequential_chars :: proc(str: string) -> (bool) {
  in_runes := utf8.string_to_runes(str)
  defer delete(in_runes)

  length := len(in_runes)
  for char, i in str {
    if i + 1 < length && char == in_runes[i + 1] {
      return true
    }
  }

  return false
}

solve :: proc() {
  contents := slurp_file("../input.txt")
  defer delete(contents)

  by_line := strings.split_lines(contents)
  defer delete(by_line)

  // Remove the last blank line from the slice.
  num_lines := len(by_line)
  by_line = by_line[0:(num_lines - 1)]

  count := 0
  for line, i in by_line {
    // Put the third rule first as we can rule out bad
    // candidates quickly.
    if contains_bad_char_pairs(line) do continue
    if !contains_required_num_vowels(line) do continue
    if !contains_sequential_chars(line) do continue
    count += 1
  }

  fmt.println("Nice count:", count)
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
