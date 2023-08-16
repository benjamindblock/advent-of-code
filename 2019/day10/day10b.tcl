package require math::geometry
package require struct::record

# Define a struct that holds information about a path from
# the origin station to another asteroid.
::struct::record define vaporizePath {
  origin
  target
  line
  angle
  distance
}

proc definePaths {origin asteroids} {
  set paths [list]

  foreach asteroid $asteroids {
    set line [concat $origin $asteroid]

    # We need 0 degress to be point upwards, actually.
    # Ref: https://wiki.tcl-lang.org/page/fmod
    set angle [::math::geometry::angle $line]
    set angle [::tcl::mathfunc::fmod [expr {$angle + 90}] 360]

    set distance [::math::geometry::distance $origin $asteroid]

    # Defining a new vaporizePath
    set path [
      vaporizePath #auto \
        -origin $origin \
        -target $asteroid \
        -line [concat $origin $asteroid] \
        -angle $angle \
        -distance $distance \
    ]

    # Testing
    lappend paths $path
  }

  return $paths
}

# Finds all paths that along the same angle as the path passed in.
proc sameAngle {path otherPaths} {
  set share [list $path]

  set sourceLine [$path cget -line]
  foreach otherPath $otherPaths {
    set otherLine [$otherPath cget -line]
    if {$sourceLine ne $otherLine} {
      if {[$path cget -angle] == [$otherPath cget -angle]} {
        lappend share $otherPath
      }
    }
  }

  return $share
}

# Sorting command on two vaporizePath objects to
# compare an attribute such as -angle or -distance.
proc lsort_paths {attr a b} {
  set a [$a cget $attr]
  set b [$b cget $attr]
  if { $a < $b } {
     return -1
   } elseif { $a > $b} {
     return 1
   } else {
     return 0
   }
}

# Given a path, find the next path in a clockwise direction.
proc nextPath {path otherPaths} {
  if {[llength $otherPaths] == 1} {
    return
  }

  set curAngle [$path cget -angle] 
  set sorted [lsort -command "lsort_paths -angle" $otherPaths]

  foreach sPath $sorted {
    if {[$sPath cget -angle] > $curAngle} {
      return $sPath
    }
  }

  # If we did not return a path that has a higher angle than
  # the current one, that means we are at the end of the list
  # at the path with the max angle. In that case, return the
  # first path so we "loop back around."
  return [lindex $sorted 0]
}

# Given a path, and all the paths we have, find the path to destroy.
# This will be:
# 1. A path on the current angle (defined by "path")
# 2. The closest path to the origin point (defined by the "-distance")
proc toDestroy {path paths} {
  set destroyPossibles [sameAngle $path $paths]
  set sortedByDistance [lsort -command "lsort_paths -distance" $destroyPossibles]

  return [lindex $sortedByDistance 0]
}

# Removes a path from the list of paths.
proc destroyPath {toDestroy paths} {
  set pathIndex [lsearch $paths $toDestroy]
  lreplace $paths $pathIndex $pathIndex
}

### Implementation.
# Set program.
set reader [open "data.txt" r]
set map [split [read $reader] "\n"]
close $reader

# Create a 2D array from the input map.
set map [lrange $map 0 end-1]
set map [lmap {row} $map {split $row ""}]

# NOTE: Harcoding the origin here so we don't have to copy
# and paste all the procedural code from Part 1 here.
set originX 26
set originY 29
set origin [::math::geometry::p $originX $originY]

# Set the asteroids list with all the asteroids except for the origin point.
set asteroids [list]
for {set y 0} {$y < [llength $map]} {incr y} {
  for {set x 0} {$x < [llength [lindex $map end]]} {incr x} {
    set point [lindex [lindex $map $y] $x]
    if {$point eq "#" && !($x == $originX && $y == $originY)} {
      lappend asteroids [::math::geometry::p $x $y]
    }
  }
}

# Calculate all the paths from the origin point to all the
# other asteroids in the system.
set paths [definePaths $origin $asteroids]

# Find our starting path.
set path [lindex [lsort -command "lsort_paths -angle" $paths] 0]

for {set i 0} {$i < 200} {incr i} {
  set toDestroy [toDestroy $path $paths]
  set path [nextPath $toDestroy $paths]
  set paths [destroyPath $toDestroy $paths]
}

puts "Last path to be destroyed was: [::struct::record show value $toDestroy]"
