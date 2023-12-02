BEGIN {
  print "()()()() Day 2, Part 1 ()()()()"
  print "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n"
}

{
  max_r = 0
  max_g = 0
  max_b = 0

  # Split each game into the sets after removing the game no. prefix.
  $1 = ""
  $2 = ""
  n = split($0, sets, ";")

  for (i = 1; i <= n; i++) {
    # Split each set into the individual cube pulls.
    m = split(sets[i], cubes, ",")

    for (j = 1; j <= m; j++) {
      # Split each cube pull into its color and num. of cubes.
      cube = cubes[j]
      gsub("^[ ]*", "", cube)
      split(cube, num_and_color, " ")
      num = num_and_color[1]
      color = num_and_color[2]

      # Find the max cubes revealed for any given color.
      if (color == "red" && num > max_r) { max_r = num }
      if (color == "green" && num > max_g) { max_g = num }
      if (color == "blue" && num > max_b) { max_b = num }
    }
  }

  if (max_r && max_g && max_b) {
    sum = sum + (max_r * max_g * max_b)
  }
}

END {
  print "Answer:", sum
}
