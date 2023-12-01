BEGIN {
  print "()()()() Day 1, Part 1 ()()()()"
  print "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n"
  regex = "[0-9]"
  sum = 0
}

{
  forward = $0
  match(forward, regex)
  digit1 = substr(forward, RSTART, RLENGTH)

  backward = ""
  for(i = length($0); i > 0; i--) {
    char = substr($0, i, 1)
    backward = backward "" char
  }
  match(backward, regex)
  digit2 = substr(backward, RSTART, RLENGTH)

  number = digit1 "" digit2
  sum = sum + number
}

END {
  print "Answer: " sum
}
