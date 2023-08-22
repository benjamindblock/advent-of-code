package require struct::record
package require math::numtheory
namespace import struct::record

proc updateVelocity {moon moons dimension} {
  set position [$moon cget -position.$dimension]
  set velocity [$moon cget -velocity.$dimension]

  # Remove $moon from $moons.
  # Ref: https://www.tcl.tk/man/tcl8.4/TclCmd/lsearch.html#M12
  set moons [lsearch -all -inline -not $moons $moon]

  foreach otherMoon $moons {
    set otherPosition [$otherMoon cget -position.$dimension]
    if {$position > $otherPosition} {
      incr velocity -1
    } elseif {$position < $otherPosition} {
      incr velocity
    }
  }

  $moon configure -velocity.$dimension $velocity
}

# Update the position of a moon by adding its velocity
# to the position of the moon.
proc updatePosition {moon dimension} {
  set velocity [$moon cget -velocity.$dimension]
  set position [$moon cget -position.$dimension]

  $moon configure -position.$dimension [expr {$velocity + $position}]
}

# Get the total energy for a moon by multiplying its
# potential energy against its kinetic energy.
proc moonEnergy {moon} {
  set velocityX [$moon cget -velocity.x]
  set velocityY [$moon cget -velocity.y]
  set velocityZ [$moon cget -velocity.z]
  set kin [expr {abs($velocityX) + abs($velocityY) + abs($velocityZ)}]

  set positionX [$moon cget -position.x]
  set positionY [$moon cget -position.y]
  set positionZ [$moon cget -position.z]
  set pot [expr {abs($positionX) + abs($positionY) + abs($positionZ)}]

  return [expr {$pot * $kin}]
}

record define position {
  x
  y
  z
}

record define velocity {
  {x 0}
  {y 0}
  {z 0}
}

record define moon {
  {record position position}
  {record velocity velocity}
}

# Holds each of the 4 moons
set moons [list]

# Moon 1
set position [position #auto -x -7 -y 17 -z -11]
set velocity [velocity #auto]
lappend moons [moon #auto -position [$position cget] -velocity [$velocity cget]]

# Moon 2
set position [position #auto -x 9 -y 12 -z 5]
lappend moons [moon #auto -position [$position cget] -velocity [$velocity cget]]

# Moon 3
set position [position #auto -x -9 -y 0 -z -4]
lappend moons [moon #auto -position [$position cget] -velocity [$velocity cget]]

# Moon 4
set position [position #auto -x 4 -y 6 -z 0]
lappend moons [moon #auto -position [$position cget] -velocity [$velocity cget]]

proc positionAndVelocity {moons dimension} {
  set pos [lmap moon $moons {$moon cget -position.$dimension}]
  set vel [lmap moon $moons {$moon cget -velocity.$dimension}]
  return [list {*}$pos {*}$vel]
}

# Takes in the moons and a dimension, and finds the first
# two steps in the movement of the moons that repeat a 
# position exactly. The returned array will have two values,
# the position of the first and second time the position and
# velocity combination occurred.
proc solveDimension {moons dimension} {
  set step 0
  set moonStatus [positionAndVelocity $moons $dimension]
  set data [dict create $moonStatus [list $step]]

  while {[llength [dict get $data $moonStatus]] <= 1} {
    incr step

    # Get new status of all the moons along the dimension
    foreach moon $moons { updateVelocity $moon $moons $dimension }
    foreach moon $moons { updatePosition $moon $dimension }
    set moonStatus [positionAndVelocity $moons $dimension]

    if {[dict exists $data $moonStatus]} {
      set indexes [dict get $data $moonStatus]
      dict set data $moonStatus [lappend indexes $step]
    } else {
      dict set data $moonStatus [list $step]
    }
  }

  return [dict get $data $moonStatus]
}

puts "--------------------------------"
puts "Solving for X..."
set xSolved [solveDimension $moons x]
puts "Solving for Y..."
set ySolved [solveDimension $moons y]
puts "Solving for Z..."
set zSolved [solveDimension $moons z]
puts "--------------------------------"
set x [lindex $xSolved end]
set y [lindex $ySolved end]
set z [lindex $zSolved end]
set lcm1 [::math::numtheory::lcm $x $y]
set lcm2 [::math::numtheory::lcm $lcm1 $z]
puts "Number of steps to repeat: $lcm2"
puts "--------------------------------"
