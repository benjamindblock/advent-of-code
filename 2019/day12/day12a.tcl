package require struct::record
namespace import struct::record

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

# set moon [moon #auto -position [$position cget] -velocity [$velocity cget]]
# puts [$moon cget]
# # How to access a nested attribute
# puts [$moon cget -position.x]
# puts [$moon cget -velocity.x]
