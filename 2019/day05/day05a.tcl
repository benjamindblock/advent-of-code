# Create reader.
set reader [open "data.txt" r]

# Set global program.
set program [split [read $reader] ","]
close $reader

# Set pointer.
set pointer 0

proc retrieveVal {program param mode} {
  switch $mode {
    0 {
      return [lindex $program $param]
    }
    1 {
      return $param
    }
  }
}

while {$pointer < [llength $program]} {
  set inst [
    format %05s [
      string trim [lindex $program $pointer]
    ]
  ]
  set instLen [string length $inst]

  # Extract the opcode and convert to a proper integer.
  # If we have a leading zero (eg., 02) we strip that away
  # to prevent accidental interpretation as an octal.
  set opcode [string range $inst [expr {$instLen - 2}] end]
  regexp {^0*(\d+)} $opcode _ opcode

  # Get the modes for each parameter, in the correct order.
  set paramModes [
    lreverse [
      split [string range $inst 0 [expr {$instLen - 3}]] ""
    ]
  ]
  lassign $paramModes mode1 mode2 mode3

  switch $opcode {
    1 {
      set param1Loc [expr {$pointer + 1}]
      set param2Loc [expr {$pointer + 2}]
      set param3Loc [expr {$pointer + 3}]
      lassign [lrange $program $param1Loc $param3Loc] param1 param2 param3

      set val1 [retrieveVal $program $param1 $mode1]
      set val2 [retrieveVal $program $param2 $mode2]
      set dest $param3

      set result [expr {$val1 + $val2}]
      set program [lreplace $program $dest $dest $result]

      incr pointer 4
    }
    2 {
      set param1Loc [expr {$pointer + 1}]
      set param2Loc [expr {$pointer + 2}]
      set param3Loc [expr {$pointer + 3}]
      lassign [lrange $program $param1Loc $param3Loc] param1 param2 param3

      set val1 [retrieveVal $program $param1 $mode1]
      set val2 [retrieveVal $program $param2 $mode2]
      set dest $param3

      set result [expr {$val1 * $val2}]
      set program [lreplace $program $dest $dest $result]

      incr pointer 4
    }
    3 {
      # Find the location in memory to store the value.
      set dest [lindex $program [expr {$pointer + 1}]]

      # Get value from stdin.
      puts -nonewline "Store $dest: "
      flush stdout
      gets stdin input

      # Store the value in memory at that location.
      set program [lreplace $program $dest $dest $input]

      incr pointer 2
    }
    4 {
      set location [expr {$pointer + 1}]
      switch $mode1 {
        0 {
          set src [lindex $program $location]
          set val [lindex $program $src]
        }
        1 {
          set val [lindex $program $location]
        }
      }
      puts "Value at $location: $val"
      incr pointer 2
    }
    99 {
      puts "Halting."
      return
    }
  }
}
