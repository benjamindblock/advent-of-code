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
global cache

foreach {inst} $firstWirePath {
  handleInstruction $inst steps increment direction

  switch $direction {
    x {
      for {set i 0} {$i < $steps} {incr i} {
        incr x $increment
        dict set cache [list $x $y] 1
      }
    }
    y {
      for {set i 0} {$i < $steps} {incr i} {
        incr y $increment
        dict set cache [list $x $y] 1
      }
    }
  }
}


set x 0
set y 0
set intersect [list]

foreach {inst} $secondWirePath {
  handleInstruction $inst steps increment direction

  switch $direction {
    x {
      for {set i 0} {$i < $steps} {incr i} {
        incr x $increment
        set point [list $x $y]
        if {[dict exists $cache $point]} {
          lappend intersect $point
        }
      }
    }
    y {
      for {set i 0} {$i < $steps} {incr i} {
        incr y $increment
        set point [list $x $y]
        if {[dict exists $cache $point]} {
          lappend intersect $point
        }
      }
    }
  }
}

set closest 0
foreach {point} $intersect {
  set x [lindex $point 0]
  set y [lindex $point 1]
  set x [expr abs($x)]
  set y [expr abs($y)]
  set sum [expr {$x + $y}]

  if {$sum < $closest || $closest == 0} {
    set closest $sum
  }
}

puts "Distance at the closest intersection: $closest"
