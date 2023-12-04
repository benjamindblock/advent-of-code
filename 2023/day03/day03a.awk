# Locations to check (clockwise) from 12 o'clock.
# (x, y+1)
# (x+1, y+1)
# (x+1, y)
# (x+1, y-1)
# (x, y-1)
# (x-1, y-1)
# (x-1, y),
# (x-1, y+1)
function point_has_adj_sym(grid, x, y) {
  sym = grid[x,y+1]
  if (match(sym, /[0-9.]/) == 0 && sym != "") { return 1 }

  sym = grid[x+1,y+1]
  if (match(sym, /[0-9.]/) == 0 && sym != "") { return 1 }

  sym = grid[x+1,y]
  if (match(sym, /[0-9.]/) == 0 && sym != "") { return 1 }

  sym = grid[x+1,y-1]
  if (match(sym, /[0-9.]/) == 0 && sym != "") { return 1 }

  sym = grid[x,y-1]
  if (match(sym, /[0-9.]/) == 0 && sym != "") { return 1 }

  sym = grid[x-1,y-1]
  if (match(sym, /[0-9.]/) == 0 && sym != "") { return 1 }

  sym = grid[x-1,y]
  if (match(sym, /[0-9.]/) == 0 && sym != "") { return 1 }

  sym = grid[x-1,y+1]
  if (match(sym, /[0-9.]/) == 0 && sym != "") { return 1 }

  return 0
}

function num_is_adj_to_sym(grid, x1, x2, y,    i) {
  for (i = x1; i <= x2; i++) {
    if (point_has_adj_sym(grid, i, y)) {
      return 1
    }
  }

  return 0
}

BEGIN {
  print "()()()() Day 3, Part 1 ()()()()"
  print "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n"
}

{
  x = split($0, cells, "")
  y = y + 1

  for (i = 1; i <= x; i++) {
    v = cells[i]
    grid[i,y] = v
  }
}

END {
  for (i = 1; i <= y; i++) {
    num = ""
    num_start = -1

    for (j = 1; j <= x; j++) {
      v = grid[j,i]

      if (match(v, /[0-9]/) > 0) {
        if (num_start == -1) { num_start = j }
        num = num v 
      } else {
        if (num_start != -1 && num_is_adj_to_sym(grid, num_start, j-1, i)) {
          sum = sum + num
        }
        num = ""
        num_start = -1
      }
    }

    # Check at the end of the row to make sure we don't
    # miss a number at the end.
    if (num_start != -1 && num_is_adj_to_sym(grid, num_start, j-1, i)) {
      sum = sum + num
    }
  }

  print "Answer:", sum
}
