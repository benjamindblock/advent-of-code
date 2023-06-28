proc setparams {val1 val2} {
  upvar mem loc_mem
  set loc_mem [lreplace $loc_mem 1 2 $val1 $val2]
}

# Initializes a sequence of numbers
proc initseq {length} {
  set seq [list]

  for {set x 0} {$x < $length} {incr x} {
    set seq [lappend seq $x]
  }

  return $seq
}

# Sets the two values needed for an operation.
#
# Uses upvar to bind local variables to the variables inside the
# foreach loop. This way, when the local var is updated, the
# variable in the scope above is also updated.
proc setvals {var_name1 address1 var_name2 address2} {
  upvar mem loc_mem $var_name1 loc_var1 $var_name2 loc_var2
  set loc_var1 [lindex $loc_mem $address1]
  set loc_var2 [lindex $loc_mem $address2]
}

set x [initseq 100]
set y [initseq 100]
set total 0

foreach {val1} $x {
  foreach {val2} $y {
    # Read input from file into an array.
    set reader [open "data.txt" r]
    set mem [split [read $reader] ","]
    close $reader
    setparams $val1 $val2

    # Iterates over chunks of four integers, which comprises a single
    # instruction for the opcode computer.
    #
    # opcode - The operation to take place
    # pos1 - position in $mem with the first val for the operation
    # pos2 - position in $mem with the second val for the operation
    # dest - position in $mem to store the results of the operation
    foreach {opcode pos1 src2 dest} $mem {
      switch $opcode {
        1 {
          setvals noun $pos1 verb $src2
          set res [expr {$noun + $verb}]
          set mem [lreplace $mem $dest $dest $res]
        }
        2 {
          setvals noun $pos1 verb $src2
          set res [expr {$noun * $verb}]
          set mem [lreplace $mem $dest $dest $res]
        }
        99 {
          set total [lindex $mem 0]
          if {$total == 19690720} {
            puts "$total for \[$val1, $val2\]"
            puts "Answer is: [expr {[expr {100 * $val1}] + $val2}]"
            return
          }
        }
      }
    }
  }
}
