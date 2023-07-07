#############################################################
# Helper functions.
#############################################################

# Rule 1: It is a six-digit number
# Rule 2: It is within the range of the input
# Rule 3: At least two adjacent digits are the same
# Rule 4: From left to right, numbers only increase
proc validatePassword {int} {
  return [
    expr {[increaseRule $int] && [adjacentRule $int]}
  ]
}

# Implementing Rule 3.
proc adjacentRule {int} {
  foreach {digit} [split $int ""] {
    if {![info exists curAdj]} {
      set curAdj $digit
    } elseif {$digit == $curAdj} {
      return true
    }
    set curAdj $digit
  }
  return false
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
