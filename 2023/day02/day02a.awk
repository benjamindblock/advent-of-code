BEGIN {
  print "()()()() Day 2, Part 1 ()()()()"
  print "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n"

  TARGET_RED = 12
  TARGET_GREEN = 13
  TARGET_BLUE = 14
}

{
  valid_r = 1
  valid_g = 1
  valid_b = 1

  # Cache the game number and then clear out the first two fields
  # so we can operate on the remaining as a single string.
  game_seq = substr($2, 0, length($2)-1)
  $1 = ""
  $2 = ""

  # Split each game into the sets.
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

      # If any number of cubes exceeds the target, then we can mark
      # this whole game as invalid as we now know that the cubes in
      # the bag exceed the target.
      if (color == "red" && num > TARGET_RED) { valid_r = 0 }
      if (color == "green" && num > TARGET_GREEN) { valid_g = 0 }
      if (color == "blue" && num > TARGET_BLUE) { valid_b = 0 }
    }
  }

  if (valid_r && valid_g && valid_b) {
    sum = sum + game_seq
  }
}

END {
  print "Answer:", sum
}
