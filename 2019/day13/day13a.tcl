package require struct::record
package require struct::set
package require math::geometry

# Define a struct that holds information about the
# game screen.
::struct::record define screen {
  {maxX 0}
  {maxY 0}
  {panels}
}

# Class to create a computer with a program and run it.
# Set to have a memory bank of 10,000 digits.
oo::class create computer {
  variable program memory pointer relBase \
    posMode immdtMode relMode output opCode \
    instructions gameScreen

  # Takes the full program. 
  constructor {programArg} {
    set memory [lrepeat 10000 0]
    set program $programArg

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

    # First instruction will be the X position.
    # Second will be the Y position.
    # Third is the tile ID
    # 0 - Empty tile
    # 1 - Wall tile
    # 2 - Block tile
    # 3 - Horizontal paddle
    # 4 - Ball tile
    set instructions [list]

    # Initialize the dictionary that tracks all
    # panels (key is the x_y, value is tileId).
    set gameScreen [screen #auto -panels [dict create]]
  }

  method printProgram {} {
    puts $program
  }

  method printMemory {} {
    puts $memory
  }

  method printScreen {} {
    set blockCount 0

    for {set y [$gameScreen cget -maxY]} {$y >= 0} {incr y -1} {
      for {set x 0} {$x < [$gameScreen cget -maxX]} {incr x} {
        set tileId [dict get [$gameScreen cget -panels] $x\_$y]

        if {$tileId == 2} {
          incr blockCount
        }

        set token [
          switch $tileId {
            0 { string cat "" }
            1 { string cat "|" }
            2 { string cat "x" }
            3 { string cat "-" }
            4 { string cat "o" }
          }
        ]

        puts -nonewline "$token "
      }
      puts ""
    }

    puts "Final block tile count: $blockCount"
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
        puts "Breaking...\n"
        my printScreen
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
        puts -nonewline "Input: "
        flush stdout
        gets stdin input
        my write 1 $opStruct(mode1) $input

        incr pointer 2
      }
      4 {
        set ret [my fetch 1 $opStruct(mode1)]
        lappend instructions $ret

        # When we have gathered three instructions, then we
        # draw a panel. After that, reset instructions.
        if {[llength $instructions] == 3} {
          my draw \
            [lindex $instructions 0] \
            [lindex $instructions 1] \
            [lindex $instructions 2]
          set instructions [list]
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

  # Moves the machine by applying the movement delta to the
  # current coordinates. Whenever we draw, add the new location
  # to our location log so we know where we've been.
  method draw {x y tileId} {
    set panels [$gameScreen cget -panels]
    dict set panels $x\_$y $tileId
    $gameScreen configure -panels $panels

    if {$x > [$gameScreen cget -maxX]} {
      $gameScreen configure -maxX $x
    }

    if {$x > [$gameScreen cget -maxY]} {
      $gameScreen configure -maxY $y
    }
  }
}

# Set program.
set reader [open "data.txt" r]
set program [split [read $reader] ","]
close $reader

set computer [computer new $program]
set output [$computer main]
