# Set global program.
set reader [open "data.txt" r]
global program
set program [split [read $reader] ","]
close $reader

# Set pointer.
global pointer
set pointer 0

proc getParam {offset mode} {
  global program pointer

  # Retrieve the value at the correct position.
  set pos [expr {$pointer + $offset}]
  set param [lindex $program $pos]

  # If in position mode, find the value at the position indicated by
  # the parameter. If in immediate mode, use the parameter's value.
  switch $mode {
    0 {
      return [lindex $program $param]
    }
    1 {
      return $param
    }
  }
}

# TODO: Finish by adding the opcode,
#       modes, and params to the dict.
proc parseInstruction {structName} {
  global program pointer
  upvar $structName localStruct

  # Get the instruction from the program.
  # 101,2,3,4,99 => 101
  set inst [lindex $program $pointer]

  # Left pad to five digits to avoid implicit logic around
  # the leading zeroes.
  # 101 => 00101
  set inst [format %05s [string trim $inst]]

  # Get the final opcode integer.
  # 00101 => 1
  regexp {^0*(\d+)} [string range $inst 3 end] _ opcode

  # Retrieve the parameter modes.
  # 00101 => {0} {0} {1}
  lassign [lreverse [split [string range $inst 1 2] ""]] \
    mode1 mode2

  set localStruct(opcode) $opcode
  set localStruct(mode1) $mode1
  set localStruct(mode2) $mode2
}

while {$pointer < [llength $program]} {
  parseInstruction opStruct

  switch $opStruct(opcode) {
    1 {
      set param1 [getParam 1 $opStruct(mode1)]
      set param2 [getParam 2 $opStruct(mode2)]
      set result [expr {$param1 + $param2}]

      set dest [getParam 3 1]
      set program [lreplace $program $dest $dest $result]

      incr pointer 4
    }
    2 {
      set param1 [getParam 1 $opStruct(mode1)]
      set param2 [getParam 2 $opStruct(mode2)]
      set result [expr {$param1 * $param2}]

      set dest [getParam 3 1]
      set program [lreplace $program $dest $dest $result]

      incr pointer 4
    }
    3 {
      puts -nonewline "Input: "
      flush stdout
      gets stdin input

      set dest [getParam 1 1]
      set program [lreplace $program $dest $dest $input]

      incr pointer 2
    }
    4 {
      set ret [getParam 1 $opStruct(mode1)]
      puts "Value: $ret"
      incr pointer 2
    }
    99 {
      puts "Halting."
      return
    }
  }
}
