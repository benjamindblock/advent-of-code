global orbits you santa

# Set program data.
set reader [open "data.txt" r]
set program [split [read $reader] "\n"]
close $reader

# Helper function to count the number of objects
# that a planet is orbiting from the $startPlanet
# to the $endPlanet. By default we will trace the
# entire path to COM if no $endPlanet is provided.
proc numTransfers {startPlanet {endPlanet "COM"}} {
  global orbits
  set curPlanet $startPlanet
  set count 0

  while {([info exists orbits($curPlanet)]) && ($curPlanet ne $endPlanet)} {
    incr count
    set curPlanet [set orbits($curPlanet)]
  }

  return $count
}

# Retrieve the total path from the $planet to the
# COM and return the list.
proc orbitPath {planet} {
  global orbits
  set path [list]
  set curPlanet $planet

  while {[info exists orbits($curPlanet)]} {
    lappend path $curPlanet
    set curPlanet [set orbits($curPlanet)]
  }

  return $path
}

# Finds the lowest common denominator between two
# ordered lists.
proc lcd {list1 list2} {
  foreach {el} $list1 {
    if {[lsearch $list2 $el] != -1} {
      return $el
    }
  }

  return -1
}

# Build an associative array that links each orbiting
# body to the body it is orbiting.
foreach {orbit} $program {
  if {$orbit eq ""} continue
  lassign [split $orbit ")"] orbitee orbiter

  if {$orbiter eq "YOU"} {
    set you $orbitee
  } elseif {$orbiter eq "SAN"} {
    set santa $orbitee
  } else {
    set orbits($orbiter) $orbitee
  }
}

# Get the full path for me.
set youPath [orbitPath $you]

# Get Santa's full path.
set santaPath [orbitPath $santa]

# Find our lowest common planet.
set commonPlanet [lcd $youPath $santaPath]

# Find how many steps it will take each of us to reach
# the common planet.
set youCount [numTransfers $you $commonPlanet]
set santaCount [numTransfers $santa $commonPlanet]

puts "Total transfers: [expr {$youCount + $santaCount}]"
