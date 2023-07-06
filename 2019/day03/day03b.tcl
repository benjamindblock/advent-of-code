# Read input from file into an array.
set reader [open "data.txt" r]
set wirePaths [split [read $reader] "\n"]
close $reader

# Set the first and second wire paths as lists of instructions.
set firstWirePath [split [lindex $wirePaths 0] ","]
set secondWirePath [split [lindex $wirePaths 1] ","]

proc handleInstruction {instruction stepsName incrName dirName} {
  upvar $stepsName _steps
  upvar $incrName _incr
  upvar $dirName _dir

  switch -regexp -matchvar count -- $instruction {
    {U([0-9]*)} {
      set _steps [lindex $count 1]
      set _incr 1
      set _dir y
    }
    {D([0-9]*)} {
      set _steps [lindex $count 1]
      set _incr -1
      set _dir y
    }
    {L([0-9]*)} {
      set _steps [lindex $count 1]
      set _incr -1
      set _dir x
    }
    {R([0-9]*)} {
      set _steps [lindex $count 1]
      set _incr 1
      set _dir x
    }
  }
}

# Start path one at the origin.
set x 0
set y 0
set firstWireSteps 0
global cache

# Step through each point in the first path
# and record its position.
foreach {inst} $firstWirePath {
  handleInstruction $inst steps increment direction

  switch $direction {
    x {
      for {set i 0} {$i < $steps} {incr i} {
        incr x $increment
        incr firstWireSteps
        dict set cache [list $x $y] $firstWireSteps
      }
    }
    y {
      for {set i 0} {$i < $steps} {incr i} {
        incr y $increment
        incr firstWireSteps
        dict set cache [list $x $y] $firstWireSteps
      }
    }
  }
}


set x 0
set y 0
set secondWireSteps 0
set intersect [list]

# Step through each point on the second path and, for each point, check if
# that was traversed during the first path. If so, add it to the list of
# intersection points.
foreach {inst} $secondWirePath {
  handleInstruction $inst steps increment direction

  switch $direction {
    x {
      for {set i 0} {$i < $steps} {incr i} {
        incr x $increment
        incr secondWireSteps

        set point [list $x $y]
        if {[dict exists $cache $point]} {
          lappend intersect [expr {[dict get $cache $point] + $secondWireSteps}]
        }
      }
    }
    y {
      for {set i 0} {$i < $steps} {incr i} {
        incr y $increment
        incr secondWireSteps

        set point [list $x $y]
        if {[dict exists $cache $point]} {
          lappend intersect [expr {[dict get $cache $point] + $secondWireSteps}]
        }
      }
    }
  }
}

set absoluted [lmap {steps} $intersect {expr {
  abs($steps)
}}]

set leastSteps [lindex [lsort -integer $absoluted] 0]
puts "Least amount of steps to an intersection is: $leastSteps"
