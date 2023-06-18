# Determines the amount of fuel required for a module, by its mass.
proc fuel_for_mass {mass} {
  set divided_by_three [expr {$mass / 3.0}]
  set rounded [expr {floor($divided_by_three)}]
  set minus_two [expr {$rounded - 2}]

  return $minus_two
}

# Determines the amount of fuel required for a module (including
# the amount of fuel # required for the fuel itself...)
proc determine_fuel {mass {total 0}} {
  set fuel [fuel_for_mass $mass]

  if {$fuel <= 0} {
    return $total
  }

  set total [expr {$total + $fuel}]
  return [determine_fuel $fuel $total]
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
