# Reverses a given string.
function reverse(s,    r) {
  r = ""
  for(i = length(s); i > 0; i--) {
    char = substr(s, i, 1)
    r = r "" char
  }
  return r
}

function first_digit(s,    digit) {
  match(s, regex)
  digit = substr(s, RSTART, RLENGTH)
  start = RSTART

  for (n in str_to_i) {
    match(s, n)
    if (RSTART > 0 && RSTART < start) {
      start = RSTART
      digit = str_to_i[n]
    }
  }

  return digit
}

function last_digit(s,    digit) {
  s = reverse(s)
  match(s, regex)
  digit = substr(s, RSTART, RLENGTH)
  start = RSTART

  for (n in str_to_i) {
    match(s, reverse(n))
    if (RSTART > 0 && RSTART < start) {
      start = RSTART
      digit = str_to_i[n]
    }
  }

  return digit
}

BEGIN {
  print "()()()() Day 1, Part 2 ()()()()"
  print "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n"

  regex = "[0-9]"
  str_to_i["zero"] = 0
  str_to_i["one"] = 1
  str_to_i["two"] = 2
  str_to_i["three"] = 3
  str_to_i["four"] = 4
  str_to_i["five"] = 5
  str_to_i["six"] = 6
  str_to_i["seven"] = 7
  str_to_i["eight"] = 8
  str_to_i["nine"] = 9
}

{
  digit1 = first_digit($0)
  digit2 = last_digit($0)

  number = digit1 "" digit2
  sum = sum + number
}

END {
  print "Answer: " sum
}
