global orbits

# Helper function to count the number of objects
# that a planet is orbiting.
proc countOrbits {startPlanet {endPlanet "COM"}} {
  global orbits
  set curPlanet $startPlanet
  set count 0

  while {([info exists orbits($curPlanet)]) && ($curPlanet ne $endPlanet)} {
    incr count
    set curPlanet [set orbits($curPlanet)]
  }

  return $count
}
# proc countOrbits {planet} {
#   global orbits
#   set curPlanet $planet
#   set count 0

#   while {[info exists orbits($curPlanet)]} {
#     incr count
#     set curPlanet [set orbits($curPlanet)]
#   }

#   return $count
# }

# Set program data.
set reader [open "data.txt" r]
set program [split [read $reader] "\n"]
close $reader

# Build an associative array that links each orbiting
# body to the body it is orbiting.
foreach {orbit} $program {
  if {$orbit eq ""} continue
  lassign [split $orbit ")"] orbitee orbiter
  set orbits($orbiter) $orbitee
}

# Iterate over each orbiting planet and count the
# number of objects it is orbiting (directly and
# indirectly).
foreach {planet} [array names orbits] {
  incr total [countOrbits $planet]
}

puts "Total orbits: $total"
