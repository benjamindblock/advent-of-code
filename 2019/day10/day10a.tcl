package require struct::set

# Finds the greatest common denominator of two numbers.
proc gcd {a b} {
  if {$b == 0} {
    return $a
  }
  gcd $b [expr {$a % $b}]
}


# Reduces a fraction, given a numerator and denominator.
# NOTE: Retains the original polarity of the inputs.
# Eg. reduceFraction 10 -2 => 5/-1
proc reduceFraction {n d} {
  if {$n == 0 && $d == 0} {
    set numerator 0
    set denominator 0
  } elseif {$d == 0} {
    set numerator 1
    set denominator 0
  } elseif {$n == 0} {
    set numerator 0
    set denominator 1
  } else {
    set gcd [gcd $n $d]
    set numerator [expr {abs($n / $gcd)}]
    set denominator [expr {abs($d / $gcd)}]
  }

  if {$n < 0} {
    set numerator [expr {-$numerator}]
  }

  if {$d < 0} {
    set denominator [expr {-$denominator}]
  }

  return "$numerator/$denominator" 
}

# Print the map of asteroids.
proc printMap {map} {
  foreach row $map {
    foreach point $row {
      puts -nonewline "$point "
    }
    puts ""
  }
}

# Given a map (2D array), find the position of all the asteroids.
proc asteroidPositions {map {marker "#"}} {
  set asteroids [list]

  for {set y 0} {$y < [llength $map]} {incr y} {
    for {set x 0} {$x < [llength [lindex $map end]]} {incr x} {
      set point [lindex [lindex $map $y] $x]
      if {$point eq $marker} {
        lappend asteroids [list $x $y]
      }
    }
  }

  return $asteroids
}

# Set program.
set reader [open "data.txt" r]
set map [split [read $reader] "\n"]
close $reader

# Create a 2D array from the input map.
set map [lrange $map 0 end-1]
set map [
  lmap {row} $map {
    split $row ""
  }
]

# Find asteroids
set asteroidLocations [asteroidPositions $map]

# Finds all the points on the map, not including the origin,
# from an origin point to a target point.
proc find {originX originY targetX targetY asteroidLocations} {
  set asteroids [list]

  set diffX [expr {$targetX - $originX}]
  set diffY [expr {$targetY - $originY}]
  set slope [reduceFraction $diffY $diffX]
  lassign [split $slope "/"] stepY stepX

  while {$originX != $targetX || $originY != $targetY} {
    incr originX $stepX
    incr originY $stepY

    if {[lsearch $asteroidLocations [list $originX $originY]] != -1} {
      lappend asteroids [list $originX $originY]
    }
  }

  return $asteroids
}

set bestAsteroid [list]
set bestCount 0

foreach {asteroid} $asteroidLocations {
  lassign $asteroid asteroidX asteroidY

  set visible [list]
  foreach {searchLoc} $asteroidLocations {
    lassign $searchLoc targetX targetY

    # Find all asteroids from the potential station location
    # and the target asteroid.
    set found [find $asteroidX $asteroidY $targetX $targetY $asteroidLocations]

    # Only keep the closest asteroid that we found, all others
    # behind it are "invisible" from the monitoring station.
    set closest [lindex $found 0]

    if {[llength $closest] > 0} {
      ::struct::set include visible $closest 
    }
  }

  if {[llength $visible] > $bestCount} {
    set bestAsteroid [list $asteroidX $asteroidY]
    set bestCount [llength $visible]
  }
}

# Print out the winner.
printMap $map
puts "Asteroid ([join $bestAsteroid ", "]) has $bestCount"
