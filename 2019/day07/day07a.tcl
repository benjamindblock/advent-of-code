global program programCache pointer posMode immdtMode mainOutput mainInputs

# Get global constants that represent the two modes
# of parameters: positional, immediate.
set posMode 0
set immdtMode 1

# Set programCache. This is an static version of the
# original program that can be reused by the computer.
set reader [open "data.txt" r]
set programCache [split [read $reader] ","]
close $reader

# Finds all permutations of a given list.
proc permutations {input} {
  # Base case, if there is no more input,
  # return a blank list.
  if {[llength $input] == 0} {
    return [list [list]]
  }

  # Split list into head and remaining.
  set remaining [lassign $input head]

  # Get new permutations from the remaining list.
  set permsFromRemaining [permutations $remaining]

  # Insert the head into each location, in each of
  # the new permutations.
  set newPerms [list]
  foreach {perm} $permsFromRemaining {
    set permLen [llength $perm]
    
    for {set i $permLen} {$i >= 0} {incr i -1} {
      # NOTE: This could be factored out to do some sort
      # "proc insertAtPos {list}" function.
      set b [lrange $perm 0 "end-$i"]
      set m [list $head]
      set e [lrange $perm [expr {$permLen - $i}] $permLen]
      lappend newPerms [list {*}$b {*}$m {*}$e]
    }
  }
  return $newPerms
}

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

# Performs an operation using the opStruct that is parsed
# inside `proc main`
proc perform {structName} {
  global pointer program immdtMode posMode mainInputs mainOutput
  upvar $structName opStruct

  # opcodes perform the following actions with (# params):
  # 1. Addition (3)
  # 2. Multiplication (3)
  # 3. Take input from $mainInputs (1)
  # 4. Store output into $mainOutput (1)
  # 5. Jump-if-true (2)
  # 6. Jump-if-false (2)
  # 7. Less than (3)
  # 8. Equals (3)
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
      # Split the list and take the head as the input for this
      # command, and store the remaining back as mainInputs to
      # be used in subsequent commands.
      set mainInputs [lassign $mainInputs input]

      set dest [fetch 1 $immdtMode]
      set program [lreplace $program $dest $dest $input]

      incr pointer 2
    }
    4 {
      set ret [fetch 1 $opStruct(mode1)]

      # Set the global mainOutput variable to hold the
      # output of the command as well, so we can acess it
      # in other places.
      set mainOutput $ret

      incr pointer 2
    }
    5 {
      set param1 [fetch 1 $opStruct(mode1)]
      set param2 [fetch 2 $opStruct(mode2)]

      if {$param1 != 0} {
        set pointer $param2
      } else {
        incr pointer 3
      }
    }
    6 {
      set param1 [fetch 1 $opStruct(mode1)]
      set param2 [fetch 2 $opStruct(mode2)]

      if {$param1 == 0} {
        set pointer $param2
      } else {
        incr pointer 3
      }
    }
    7 {
      set param1 [fetch 1 $opStruct(mode1)]
      set param2 [fetch 2 $opStruct(mode2)]
      set dest [fetch 3 $immdtMode]

      set res [expr {$param1 < $param2}]
      set program [lreplace $program $dest $dest $res]

      incr pointer 4
    }
    8 {
      set param1 [fetch 1 $opStruct(mode1)]
      set param2 [fetch 2 $opStruct(mode2)]
      set dest [fetch 3 $immdtMode]

      set res [expr {$param1 == $param2}]
      set program [lreplace $program $dest $dest $res]

      incr pointer 4
    }
    99 {
      # Return and halt the program by advancing the pointer
      # to the end of the program.
      set pointer [llength $program]
      return
    }
  }
}

proc main {phaseSetting inputSignal} {
  global pointer program programCache mainInputs mainOutput

  set program $programCache

  # Setup the two inputs that will be read from the
  # mainInputs variable, rather than stdin.
  set mainInputs [list $phaseSetting $inputSignal]

  # Reset pointer to zero for this run of the computer.
  set pointer 0

  while {$pointer < [llength $program]} {
    parse opStruct
    perform opStruct
  }

  return $mainOutput
}

# Calculate all possible input combinations of the list {0 1 2 3 4}
set phaseSettingSequences [permutations {0 1 2 3 4}]

# When setting up Ampilifier A, provide 0 as the input signal.
# For the remaining Amplifiers, the input signal will be the
# output signal of the previous amplifier.
set outputSignal 0

# Final output is the maxThrusterSignal
set maxThrusterSignal 0

foreach {phaseSettingSequence} $phaseSettingSequences {
  foreach {phaseSetting} $phaseSettingSequence {
    set outputSignal [main $phaseSetting $outputSignal]
  }

  if {$outputSignal > $maxThrusterSignal} {
    set maxThrusterSignal $outputSignal
  }

  # Reset the output signal back to zero for the next
  # amplifier sequence.
  set outputSignal 0
}

puts "Maximum thrust signal: $maxThrusterSignal"
