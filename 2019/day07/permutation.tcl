proc permutations {input} {
  # Base case, if there is no more input,
  # return a blank list.
  if {[llength $input] == 0} {
    return [list [list]]
  }

  set remaining [lassign $input head]
  set permutationsFromRemaining [permutations $remaining]
  set newPermutations [list]


  foreach {permutation} $permutationsFromRemaining {
    set permLen [llength $permutation]
    
    for {set i $permLen} {$i >= 0} {incr i -1} {
      set beginning [lrange $permutation 0 "end-$i"]
      set middle [list $head]
      set end [lrange $permutation [expr {$permLen - $i}] $permLen]
      set new [list {*}$beginning {*}$middle {*}$end]
      lappend newPermutations $new
    }
  }

  return $newPermutations
}


set input {1 2 3 4}
set output [permutations $input]

puts "Output: $output"
