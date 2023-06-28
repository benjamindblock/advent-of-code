# Sets the values at positions 1 and 2 in memory
# with val1 and val2, respectively.
proc prepareMemory {val1 val2} {
  upvar tempMem _tempMem
  set _tempMem [lreplace $_tempMem 1 2 $val1 $val2]
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
proc setNounVerb {nounName nounAddrName verbName verbAddrName} {
  upvar memory _memory $nounName _noun $verbName _verb
  set _noun [lindex $_memory $nounAddrName]
  set _verb [lindex $_memory $verbAddrName]
}

# Iterates over the memory in chunks of four integers, which
# comprises a single instruction for the intcode computer.
#
# opcode - The operation to take place
# ptr1 - position in $memory with the first val for the operation
# ptr2 - position in $memory with the second val for the operation
# dest - position in $memory to store the results of the operation
proc operate {memory} {
  foreach {opcode ptr1 ptr2 dest} $memory {
    switch $opcode {
      1 {
        setNounVerb noun $ptr1 verb $ptr2
        set res [expr {$noun + $verb}]
        set memory [lreplace $memory $dest $dest $res]
      }
      2 {
        setNounVerb noun $ptr1 verb $ptr2
        set res [expr {$noun * $verb}]
        set memory [lreplace $memory $dest $dest $res]
      }
      99 {
        return [lindex $memory 0]
      }
    }
  }
}

# Read input from file into an array.
set reader [open "data.txt" r]
set staticMem [split [read $reader] ","]
close $reader

# Initialize two arrays from [0..99] to
# loop over to find the correct val.
set x [initseq 100]
set y [initseq 100]

foreach {val1} $x {
  foreach {val2} $y {
    set tempMem $staticMem
    prepareMemory $val1 $val2
    set total [operate $tempMem]

    if {$total == 19690720} {
      puts "\[$val1, $val2\]: $total"
      puts "Answer: [expr {[expr {100 * $val1}] + $val2}]"
    }
  }
}
