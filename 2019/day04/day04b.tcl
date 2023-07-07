#############################################################
# Helper functions.
#############################################################

# Converts a list of elements into a new list that tracks
# how many times an element appears next to another one that
# is the same.
#
# Examples:
#   {1 1 2 2 2 3} => {2 3 1}
#   {1 2 3 4 5 5} => {1 1 1 1 2}
proc listToFreqList {input} {
  set count 1
  set freqList [list]

  for {set i 0} {$i < [llength $input]} {incr i} {
    set cur [lindex $input $i]
    set next [lindex $input [expr {$i + 1}]]

    if {$cur == $next} {
      incr count
    } else {
      lappend freqList $count
      set count 1
    }
  }

  return $freqList
}

# Rule 1: It is a six-digit number
# Rule 2: It is within the range of the input
# Rule 3: At least two adjacent digits are the same AND are
#         not in a group of more than two.
# Rule 4: From left to right, numbers only increase
proc validatePassword {int} {
  return [
    expr {[increaseRule $int] && [adjacentRule $int]}
  ]
}

# Implementing Rule 3.
proc adjacentRule {int} {
  set freqList [listToFreqList [split $int ""]]
  return [
    expr {[lsearch $freqList 2] > -1}
  ]
}

# Implementing Rule 4.
proc increaseRule {int} {
  foreach {digit} [split $int ""] {
    if {![info exists curMax]} {
      set curMax $digit
    } elseif {$digit < $curMax} {
      return false
    } else {
      set curMax $digit
    }
  }

  return true
}

#############################################################
# Script.
#############################################################

# Read input from file into an array.
set reader [open "data.txt" r]
set range [split [read $reader] "-"]
close $reader

set min [lindex $range 0]
set max [lindex $range 1]

puts "Finding valid passwords between $min and $max"

set validCount 0
for {set i $min} {$i <= $max} {incr i} {
  if {[validatePassword $i]} {
    incr validCount
  }
}

puts "Number of valid passwords: $validCount"
