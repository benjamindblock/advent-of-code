# NOTE: Need to fix memory bug...
# The data.txt input fails.

package require struct::set

proc printMap {map} {
  foreach {row} $map {
    foreach {point} $row {
      puts -nonewline "$point "
    }
    puts ""
  }
}

# Given a map (2D array), find all the asteroids.
proc findAsteroids {map {marker "#"}} {
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

proc perimeterPoints {map} {
  set perimeter [list]
  set minX 0
  set minY 0
  set maxX [llength [lindex $map end]]
  set maxY [llength $map]

  for {set y 0} {$y < $maxY} {incr y} {
    for {set x 0} {$x < $maxX} {incr x} {
      if {$y > $minY && $y < ($maxY - 1)} {
        if {$x == $minX || $x == ($maxX - 1)} {
          lappend perimeter [list $x $y]
        }
      } else {
        lappend perimeter [list $x $y]
      }
    }
  }

  return $perimeter
}

proc mapPoints {map} {
  set points [list]
  set minX 0
  set minY 0
  set maxX [llength [lindex $map end]]
  set maxY [llength $map]

  for {set y 0} {$y < $maxY} {incr y} {
    for {set x 0} {$x < $maxX} {incr x} {
      lappend points [list $x $y]
    }
  }

  return $points
}

# Set program.
set reader [open "test3.txt" r]
set map [split [read $reader] "\n"]
close $reader

# Create a 2D array from the input map.
set map [lrange $map 0 end-1]
set map [
  lmap {row} $map {
    split $row ""
  }
]

# All searchable locations will be the perimeter of the map.
# set toSearch [perimeterPoints $map]

# All searchable locations is every point on the map.
set toSearch [mapPoints $map]

# Find asteroids
set asteroidLocations [findAsteroids $map]

proc dbl2frac {dbl {eps 0.001}} {
  for {set den 1} {$den < 1024} {incr den} {
    set num [expr {round($dbl * $den)}]
    if {abs(double($num)/$den - $dbl) < $eps} {
      break
    }
  }

  return [list $num $den]
}

proc steps {x1 y1 x2 y2} {
  set diffY [expr {$y2 - $y1}]
  set diffX [expr {$x2 - $x1}]

  if {$diffX == 0 && $diffY == 0} {
    set stepX 0
    set stepY 0
  } elseif {$diffX == 0} {
    set stepX 0
    set stepY 1
  } elseif {$diffY == 0} {
    set stepX 1
    set stepY 0
  } else {
    set slope [expr {$diffY / double($diffX)}]
    lassign [dbl2frac $slope] stepY stepX
  }

  if {$diffX < 0} {
    set stepX [expr {-abs($stepX)}]
  } else {
    set stepX [expr {abs($stepX)}]
  }

  if {$diffY < 0} {
    set stepY [expr {-abs($stepY)}]
  } else {
    set stepY [expr {abs($stepY)}]
  }

  return [list [expr {int($stepX)}] [expr {int($stepY)}]]
}

# Finds all the points on the map, not including the origin,
# from an origin point to a target point.
proc pointsInBetween {originX originY targetX targetY map} {
  set points [list]

  lassign [steps $originX $originY $targetX $targetY] stepX stepY

  set totalPoints 0
  while {$originX != $targetX || $originY != $targetY} {
    incr originX $stepX
    incr originY $stepY
    lappend points [list $originX $originY]
  }

  return $points
}

set best [list]
set bestCount 0

foreach {asteroid} $asteroidLocations {
  foreach {asteroidX asteroidY} $asteroid {
    set visible [list]

    puts "Search for ($asteroidX, $asteroidY)"
    foreach {search} $toSearch {
      foreach {targetX targetY} $search {
        # puts [pointsInBetween $asteroidX $asteroidY $targetX $targetY $map]
        set points [pointsInBetween $asteroidX $asteroidY $targetX $targetY $map]
        set closest [lindex [::struct::set intersect $points $asteroidLocations] 0]

        if {[llength $closest] > 0} {
          ::struct::set include visible $closest 
        }
      }
    }

    if {[llength $visible] > $bestCount} {
      set best [list $asteroidX $asteroidY]
      set bestCount [llength $visible]
    }
  }
}

puts "Best: ($best) with $bestCount"
