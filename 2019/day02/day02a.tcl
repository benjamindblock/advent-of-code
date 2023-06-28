# Read input from file into an array.
set reader [open "data.txt" r]
set data [split [read $reader] ","]
close $reader

# Sets the two values needed for an operation.
#
# Uses upvar to bind local variables to the variables inside the
# foreach loop. This way, when the local var is updated, the
# variable in the scope above is also updated.
proc setvals {var1 pos1 var2 pos2} {
  upvar data locdata $var1 locvar1 $var2 locvar2
  set locvar1 [lindex $locdata $pos1]
  set locvar2 [lindex $locdata $pos2]
}

# Iterates over chunks of four integers, which comprises a single
# instruction for the opcode computer.
#
# opcode - The operation to take place
# pos1 - position in $data with the first val for the operation
# pos2 - position in $data with the second val for the operation
# dest - position in $data to store the results of the operation
foreach {opcode pos1 src2 dest} $data {
  switch $opcode {
    1 {
      setvals val1 $pos1 val2 $src2
      set res [expr {$val1 + $val2}]
      set data [lreplace $data $dest $dest $res]
    }
    2 {
      setvals val1 $pos1 val2 $src2
      set res [expr {$val1 * $val2}]
      set data [lreplace $data $dest $dest $res]
    }
    99 {
      puts [lindex $data 0]
      return
    }
  }
}
