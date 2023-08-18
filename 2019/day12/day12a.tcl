package require struct::record
namespace import struct::record

# Finds all the unique combinations of list elements
# in groups of a certain size.
proc combinations {list {size 2}} {
  if { $size == 0 } {
    return [list [list]]
  }
  set retVal {}
  for { set i 0 } { ($i + $size) <= [llength $list] } { incr i } {
    set firstElement [lindex $list $i]
    set remainingElements [lrange $list [expr { $i + 1 }] end]
    foreach subset [combinations $remainingElements [expr { $size - 1 }]] {
        lappend retVal [linsert $subset 0 $firstElement]
    }
  }
   return $retVal
}

# Update velocity of a pair of moons by applying the
# force of gravity.
proc updateVelocity {moon1 moon2 dimension} {
  set position1 [$moon1 cget -position.$dimension]
  set position2 [$moon2 cget -position.$dimension]
  set delta [expr {$position1 - $position2}]

  set velocity1 [$moon1 cget -velocity.$dimension]
  set velocity2 [$moon2 cget -velocity.$dimension]

  if {$position1 > $position2} {
    incr velocity1 -1
    incr velocity2
  } elseif {$position1 < $position2} {
    incr velocity1
    incr velocity2 -1
  }

  $moon1 configure -velocity.$dimension $velocity1
  $moon2 configure -velocity.$dimension $velocity2
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

set steps 1000
for {set i 0} {$i < $steps} {incr i} {
  set moonPairs [combinations $moons]

  foreach pair $moonPairs {
    set moon1 [lindex $pair 0]
    set moon2 [lindex $pair end]

    updateVelocity $moon1 $moon2 x
    updateVelocity $moon1 $moon2 y
    updateVelocity $moon1 $moon2 z
  }

  foreach moon $moons {
    updatePosition $moon x
    updatePosition $moon y
    updatePosition $moon z
  }
}

# The total energy is the sum of each moon's energy.
set totalEnergy 0
foreach moon $moons {
  incr totalEnergy [moonEnergy $moon]
}


puts "--------------------------------"
puts "Final moon data:"
foreach moon $moons {
  puts [$moon cget]
}
puts "--------------------------------"
puts "Total energy: $totalEnergy"
puts "--------------------------------"
