global program pointer posMode immdtMode

# Set program.
set reader [open "data.txt" r]
set program [split [read $reader] ","]
close $reader

# Set pointer.
set pointer 0

set posMode 0
set immdtMode 1

# Fetches a value from the program at a given offset from
# the current pointer. If the mode is {0}, then we return
# the value at the POSITION specified by the value at the
# offset. If the mode is {1}, return the value at the offset
# DIRECTLY.
proc fetch {offset mode} {
  global program pointer posMode immdtMode

  # Retrieve the value at the correct position.
  set pos [expr {$pointer + $offset}]
  set param [lindex $program $pos]

  # NOTE: Using the single-line version here becuase the multi-line
  # switch statement results in a body that is inside {} braces,
  # meaning that Tcl will NOT interpret the variables.
  #
  # If in position mode, find the value at the position indicated by
  # the parameter. If in immediate mode, use the parameter's value.
  switch $mode $posMode { return [lindex $program $param] } \
    $immdtMode { return $param }
}

# Parses an instruction from the program. Extracts:
# 1. opcode
# 2. mode for the first parameter
# 3. mode for the second parameter
proc parse {structName} {
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
  parse opStruct

  # opcodes perform the following actions:
  # 1. Addition
  # 2. Multiplication
  # 3. Read input
  # 4. Send output
  # 99. Halt.
  switch $opStruct(opcode) {
    1 {
      set param1 [fetch 1 $opStruct(mode1)]
      set param2 [fetch 2 $opStruct(mode2)]
      set result [expr {$param1 + $param2}]

      set dest [fetch 3 $immdtMode]
      set program [lreplace $program $dest $dest $result]

      incr pointer 4
    }
    2 {
      set param1 [fetch 1 $opStruct(mode1)]
      set param2 [fetch 2 $opStruct(mode2)]
      set result [expr {$param1 * $param2}]

      set dest [fetch 3 $immdtMode]
      set program [lreplace $program $dest $dest $result]

      incr pointer 4
    }
    3 {
      puts -nonewline "Input: "
      flush stdout
      gets stdin input

      set dest [fetch 1 $immdtMode]
      set program [lreplace $program $dest $dest $input]

      incr pointer 2
    }
    4 {
      set ret [fetch 1 $opStruct(mode1)]
      puts "Value: $ret"
      incr pointer 2
    }
    99 {
      puts "Halting."
      return
    }
  }
}
