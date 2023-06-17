# Determines the amount of fueld required for
# a rocket, by its mass.
proc determine_fuel {mass} {
  set divided_by_three [expr {$mass / 3.0}]
  set rounded [expr {floor($divided_by_three)}]
  set minus_two [expr {$rounded - 2}]

  return $minus_two
}

# Read input from file
set reader [open "day1.txt" r]
set data [read $reader]
close $reader

# Setup initial vars
set requirement 0
set data [split $data "\n"]

# Loop over each module, determine its fuel requirement,
# and add that to the total required fuel.
foreach line $data {
  if {$line ne ""} {
    set fuel [determine_fuel $line]
    set requirement [expr {$requirement + $fuel}]
  }
}

puts "Solution: $requirement"
