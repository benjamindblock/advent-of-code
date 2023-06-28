# Read input from file into an array.
set reader [open "data.txt" r]
set mem [split [read $reader] ","]
close $reader

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
      puts [lindex $mem 0]
      return
    }
  }
}
