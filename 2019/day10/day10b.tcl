package require math::geometry
package require struct::record

::struct::record define vaporizePath {
  origin
  target
  line
  angle
  distance
}

# Set program.
set reader [open "test1.txt" r]
set map [split [read $reader] "\n"]
close $reader

# Create a 2D array from the input map.
set map [lrange $map 0 end-1]
set map [
  lmap {row} $map {
    split $row ""
  }
]

# TODO: Fill in with the results of Part 1.
set originX 3
set originY 4
set origin [::math::geometry::p $originX $originY]

# The asteroids list contains all the asteroids except
# the origin (AKA. the monitoring station). These are
# the asteroids that we will be vaporizing in due time.
set asteroids [list]
for {set y 0} {$y < [llength $map]} {incr y} {
  for {set x 0} {$x < [llength [lindex $map end]]} {incr x} {
    set point [lindex [lindex $map $y] $x]
    if {$point eq "#" && !($x == $originX && $y == $originY)} {
      lappend asteroids [::math::geometry::p $x $y]
    }
  }
}

proc definePaths {origin asteroids} {
  set paths [list]

  foreach asteroid $asteroids {
    set line [concat $origin $asteroid]
    set angle [::math::geometry::angle $line]
    set distance [::math::geometry::distance $origin $asteroid]


    # Defining a new vaporizePath
    set name "$origin\_$asteroid\_path"
    vaporizePath $name
    $name configure \
      -origin $origin \
      -target $asteroid \
      -line [concat $origin $asteroid] \
      -angle $angle \
      -distance $distance \

    # Testing
    lappend paths $name
  }

  return $paths
}

# TODO: Make the path vs. line language consistent.
# Finds other path along the same angle as the first
# path passed in.
proc sameAngle {path otherPaths} {
  set sourceLine [$path cget -line]

  foreach otherPath $otherPaths {
    set otherLine [$otherPath cget -line]
    if {$sourceLine ne $otherLine} {
      if {[$path cget -angle] == [$otherPath cget -angle]} {
        puts "same path from $sourceLine to $otherLine"
      }
    }
  }
}

set names [definePaths $origin $asteroids]
set name [lindex $names 0]
# puts [$name cget -angle]

sameAngle [lindex $names 0] $names
