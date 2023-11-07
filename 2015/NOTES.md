# Odin
## Variables
### Default values
Odin uses default values for all variables ([see here](https://odin-lang.org/docs/overview/#default-values)). This makes it easy to setup a map that counts occurrences, for example, because the `int` values will always default to zero.
```odin
occurrences := make(map[string]int)
occurrences["foo"] += 1
occurrences["bar"] += 1
occurrences["foo"] += 1

// map[bar=1, foo=2]
fmt.println(occurrences)
```


## Pointers
### Dynamically reference a variable
Pointers can be used to dynamically set a reference to a variable, based on some condition. For example:
```odin
x := 0
y := 1

set_x := true
var := &x if set_x else &y

// The pointer (^) operator on the ride side of the variable ill
// dereference the underlying value.
var^ += 1

// "x is: 1"
fmt.println("x is:", var^)
```
