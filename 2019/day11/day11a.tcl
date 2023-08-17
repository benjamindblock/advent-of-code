package require struct::record
package require struct::set
package require math::geometry

# Define a struct that holds information about each panel
# that we have covered on the ship.
::struct::record define panel {
  coordinates
  color
}

# Class to create a computer with a program and run it.
# Set to have a memory bank of 10,000 digits.
oo::class create computer {
  variable program memory pointer relBase \
    posMode immdtMode relMode \
    output opCode \
    input direction curLoc panelLog \
    instructions

  # Takes the full program as well as the initial input
  # value (0 or 1).
  constructor {programArg {inputArg 0}} {
    set memory [lrepeat 10000 0]
    set program $programArg
    set input $inputArg

    for {set i 0} {$i < [llength $program]} {incr i} {
      set memory [lreplace $memory $i $i [lindex $program $i]]
    }

    # Current pointer to our position in the program.
    set pointer 0
    set relBase 0

    # Parameter modes
    set posMode 0
    set immdtMode 1
    set relMode 2

    # Direction starts as up.
    set direction up

    # Current position is (0, 0)
    set curLoc [::math::geometry::p 0 0]
    set panelLog [dict create]

    # First instruction will be paint color:
    #   0 - black
    #   1 - white
    # Second will be the direction to turn:
    #   0 - left 90 degrees
    #   1 - right 90 degrees
    set instructions [list]

    set debugCount 0
  }

  method printProgram {} {
    puts $program
  }

  method printMemory {} {
    puts $memory
  }

  method getOpCode {} {
    return $opCode
  }

  method getPointer {} {
    return $pointer
  }

  method getOutput {} {
    return $output
  }

  # Run the computer program.
  method main {} {
    while {$pointer < [llength $program]} {
      my parse opStruct
      my perform opStruct

      # False positive warning.
      ##nagelfar ignore Unknown variable
      set opcode $opStruct(opcode)

      # Return here and pause execution. We will wait until
      # the next set of instructions arrive.
      if {$opcode == 99} {
        puts "Breaking"
        puts "Number of unique painted panels: [dict size $panelLog]"
        break
      }
    }
  }

  # Retrieve the value at the correct position.
  # Position Mode (0): Find the value at the position indicated by
  # Immediate Mode (1): Use the parameter's value directly
  # Relative Mode (2): Find the value at the position of the relative base,
  # modified by the parameter.
  method fetch {offset mode} {
    set loc [expr {$pointer + $offset}]
    set pos [lindex $memory $loc]

    switch $mode \
      $posMode { return [lindex $memory $pos] } \
      $immdtMode { return $pos } \
      $relMode { return [lindex $memory [expr {$relBase + $pos}]] }
  }

  # Writes a value at a given position. "Parameters that an instruction
  # writes to will never be in immediate mode"
  method write {offset mode value} {
    set loc [expr {$pointer + $offset}]
    set pos [lindex $memory $loc]

    switch $mode \
      $posMode { set dest $pos } \
      $immdtMode { set dest $pos } \
      $relMode { set dest [expr {$relBase + $pos}] }

    set memory [lreplace $memory $dest $dest $value]
  }

  # Parses an instruction from the program. Extracts:
  # 1. opcode
  # 2. mode for the first parameter
  # 3. mode for the second parameter
  method parse {structName} {
    upvar $structName localStruct

    # Get the instruction from the program.
    # 101,2,3,4,99 => 101
    set inst [lindex $memory $pointer]

    # Left pad to five digits to avoid implicit logic around
    # the leading zeroes.
    # 101 => 00101
    set inst [format %05s [string trim $inst]]

    # Get the final opcode integer.
    # 00101 => 1
    regexp {^0*(\d+)} [string range $inst 3 end] _ opcode

    # Retrieve the parameter modes.
    # 00101 => {1} {0} {0}
    lassign [lreverse [split [string range $inst 0 2] ""]] \
      mode1 mode2 mode3

    set localStruct(opcode) $opcode
    set localStruct(mode1) $mode1
    set localStruct(mode2) $mode2
    set localStruct(mode3) $mode3
  }

  # Performs an operation using the opStruct that is parsed
  # inside `proc main`
  method perform {structName} {
    upvar $structName opStruct

    # Store the current opCode
    set opCode $opStruct(opcode)

    # opcodes perform the following actions with (# params):
    # 1. Addition (3)
    # 2. Multiplication (3)
    # 3. Take input from stdin (1)
    # 4. Store output into $output (1)
    # 5. Jump-if-true (2)
    # 6. Jump-if-false (2)
    # 7. Less than (3)
    # 8. Equals (3)
    # 9. Adjust relative base (1)
    # 99. Halt.
    switch $opStruct(opcode) {
      1 {
        set param1 [my fetch 1 $opStruct(mode1)]
        set param2 [my fetch 2 $opStruct(mode2)]
        set result [expr {$param1 + $param2}]

        my write 3 $opStruct(mode3) $result

        incr pointer 4
      }
      2 {
        set param1 [my fetch 1 $opStruct(mode1)]
        set param2 [my fetch 2 $opStruct(mode2)]
        set result [expr {$param1 * $param2}]
        my write 3 $opStruct(mode3) $result

        incr pointer 4
      }
      3 {
        my write 1 $opStruct(mode1) $input

        # Reset the instructions after providing new input.
        set instructions [list]

        incr pointer 2
      }
      4 {
        set ret [my fetch 1 $opStruct(mode1)]
        lappend instructions $ret

        # When we have gathered two instructions, then we
        # activate the robot again to paint the current
        # panel, move in a new direction, and set the input
        # based on the new panel's color.
        if {[llength $instructions] == 2} {
          my paintPanel [lindex $instructions 0]
          my setNewDirection [lindex $instructions end]
          my move
          my setInput
        }

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
        set result [expr {$param1 < $param2}]
        my write 3 $opStruct(mode3) $result

        incr pointer 4
      }
      8 {
        set param1 [my fetch 1 $opStruct(mode1)]
        set param2 [my fetch 2 $opStruct(mode2)]
        set result [expr {$param1 == $param2}]
        my write 3 $opStruct(mode3) $result

        incr pointer 4
      }
      9 {
        set param1 [my fetch 1 $opStruct(mode1)]
        incr relBase $param1

        incr pointer 2
      }
      99 {
        # Return and halt the program by advancing the pointer
        # to the end of the program.
        set pointer [llength $program]
        return
      }
    }
  }

  # Given a value of [0, 1], find the new direction that the
  # robot will move in. "0 means it should turn left 90 degrees,
  # and 1 means it should turn right 90 degress."
  method setNewDirection {turnCode} {
    set direction [
      switch -- $direction\_$turnCode {
        up_0 {
          string cat left
        }
        up_1 {
          string cat right
        }
        down_0 {
          string cat right
        }
        down_1 {
          string cat left
        }
        right_0 {
          string cat up
        }
        right_1 {
          string cat down
        }
        left_0 {
          string cat down
        }
        left_1 {
          string cat up
        }
      }
    ]
  }

  # Moves the machine by applying the movement delta to the
  # current coordinates. Whenever we move, add the new location
  # to our location log so we know where we've been.
  method move {} {
    set delta [
      switch $direction {
        up {
          ::math::geometry::p 0 1    
        }
        down {
          ::math::geometry::p 0 -1    
        }
        right {
          ::math::geometry::p 1 0
        }
        left {
          ::math::geometry::p -1 0
        }
      }
    ]

    set curLoc [::math::geometry::+ $curLoc $delta]
  }

  # If the new panel the robot has moved to has been seen already,
  # determine the input from its color. Otherwise, use a black input
  # value of 0 as the default.
  method setInput {} {
    if {[dict exists $panelLog $curLoc]} {
      set panel [dict get $panelLog $curLoc]
      set input [$panel cget -color]
    } else {
      set input 0
    }
  }

  # "Paints" a panel by setting the struct's -color field to the
  # provided color value.
  method paintPanel {color} {
    if {[dict exists $panelLog $curLoc]} {
      set panel [dict get $panelLog $curLoc]
      $panel configure -color $color
    } else {
      set panel [panel #auto -coordinates $curLoc -color $color]
    }

    dict set panelLog $curLoc $panel
  }
}

# Set program.
set reader [open "data.txt" r]
set program [split [read $reader] ","]
close $reader

set computer [computer new $program]
set output [$computer main]
