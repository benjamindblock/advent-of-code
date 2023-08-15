proc gcd {a b} {
  if {$b == 0} {
    return $a
  }
  gcd $b [expr {$a % $b}]
}

proc reduce {n d} {
  if {$n == 0 && $d == 0} {
    set numerator 0
    set denominator 0
  } elseif {$d == 0} {
    set numerator 1
    set denominator 0
  } elseif {$n == 0} {
    set numerator 0
    set denominator 1
  } else {
    set gcd [gcd $n $d]
    set numerator [expr {abs($n / $gcd)}]
    set denominator [expr {abs($d / $gcd)}]
  }

  if {$n < 0} {
    set numerator [expr {-$numerator}]
  }

  if {$d < 0} {
    set denominator [expr {-$denominator}]
  }

  return "$numerator/$denominator" 
}

proc ? {cmd expected} {
  catch {uplevel 1 $cmd} res
  if {$res ne $expected} {puts "$cmd -> $res, expected $expected"}
}

# ? {reduce 4 -6} 1/-2
? {reduce 2 -1} 2/-1
? {reduce 2 4} 1/2
? {steps 0 1 1 0} {1 -1}
? {steps 0 1 0 2} {0 1}
