package utils

import "core:fmt"
import "core:os"
import "core:strings"

slurp_file :: proc(filename: string) -> (string) {
  content, ok := os.read_entire_file_from_filename(
    filename,
    context.temp_allocator
  )

  if !ok {
    fmt.println("Could not open", filename)
    os.exit(1)
  }

  return string(content)
}

slurp_file_into_lines :: proc(filename: string) -> ([]string) {
  content := slurp_file(filename)
  as_lines := strings.split_lines(content)

  // Remove the last blank line.
  length := len(as_lines)
  as_lines = as_lines[0:length - 1]

  return as_lines
}
