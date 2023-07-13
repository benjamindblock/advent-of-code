# Class to create a computer with a program and run
# it with a set of inputs.
oo::class create computer {
  variable program pointer posMode immdtMode \
    inputs output

  constructor {programArg inputsArg} {
    set program $programArg
    set inputs $inputsArg
    set pointer 0

    # Parameter modes
    set posMode 0
    set immdtMode 1
  }

  method printProgram {} {
    puts $program
  }

  # Run the computer program.
  method main {} {
    set pointer 0

    while {$pointer < [llength $program]} {
      my parse opStruct
      my perform opStruct
    }

    return $output
  }

  # Retrieve the value at the correct position.
  method fetch {offset mode} {
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
  method parse {structName} {
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
  method perform {structName} {
    upvar $structName opStruct

    # opcodes perform the following actions with (# params):
    # 1. Addition (3)
    # 2. Multiplication (3)
    # 3. Take input from $inputs (1)
    # 4. Store output into $output (1)
    # 5. Jump-if-true (2)
    # 6. Jump-if-false (2)
    # 7. Less than (3)
    # 8. Equals (3)
    # 99. Halt.
    switch $opStruct(opcode) {
      1 {
        set param1 [my fetch 1 $opStruct(mode1)]
        set param2 [my fetch 2 $opStruct(mode2)]
        set result [expr {$param1 + $param2}]

        set dest [my fetch 3 $immdtMode]
        set program [lreplace $program $dest $dest $result]

        incr pointer 4
      }
      2 {
        set param1 [my fetch 1 $opStruct(mode1)]
        set param2 [my fetch 2 $opStruct(mode2)]
        set result [expr {$param1 * $param2}]

        set dest [my fetch 3 $immdtMode]
        set program [lreplace $program $dest $dest $result]

        incr pointer 4
      }
      3 {
        # Split the list and take the head as the input for this
        # command, and store the remaining back as mainInputs to
        # be used in subsequent commands.
        set inputs [lassign $inputs input]

        set dest [my fetch 1 $immdtMode]
        set program [lreplace $program $dest $dest $input]

        incr pointer 2
      }
      4 {
        set ret [my fetch 1 $opStruct(mode1)]

        # Set the global output variable to hold the
        # output of the command as well, so we can acess it
        # in other places.
        set output $ret

        incr pointer 2
      }
      5 {
        set param1 [my fetch 1 $opStruct(mode1)]
        set param2 [my fetch 2 $opStruct(mode2)]

        if {$param1 != 0} {
          set pointer $param2
        } else {
          incr pointer 3
        }
      }
      6 {
        set param1 [my fetch 1 $opStruct(mode1)]
        set param2 [my fetch 2 $opStruct(mode2)]

        if {$param1 == 0} {
          set pointer $param2
        } else {
          incr pointer 3
        }
      }
      7 {
        set param1 [my fetch 1 $opStruct(mode1)]
        set param2 [my fetch 2 $opStruct(mode2)]
        set dest [my fetch 3 $immdtMode]

        set res [expr {$param1 < $param2}]
        set program [lreplace $program $dest $dest $res]

        incr pointer 4
      }
      8 {
        set param1 [my fetch 1 $opStruct(mode1)]
        set param2 [my fetch 2 $opStruct(mode2)]
        set dest [my fetch 3 $immdtMode]

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
}

# Set programCache. This is an static version of the
# original program that can be reused by the computer.
set reader [open "data.txt" r]
set program [split [read $reader] ","]
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
