# Odin
## Memory management
### Checking for unfreed allocations
Add the following snippet inside `main :: proc()`, and then make sure to use the `-debug` flag with either `odin build` or `odin run`.

```odin
main :: proc() {
  when ODIN_DEBUG {
    track: mem.Tracking_Allocator
    mem.tracking_allocator_init(&track, context.allocator)
    context.allocator = mem.tracking_allocator(&track)
    defer {
      if len(track.allocation_map) > 0 {
        fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
        for _, entry in track.allocation_map {
          fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
        }
      }
      if len(track.bad_free_array) > 0 {
        fmt.eprintf("=== %v incorrect frees: ===\n", len(track.bad_free_array))
        for entry in track.bad_free_array {
          fmt.eprintf("- %p @ %v\n", entry.memory, entry.location)
        }
      }
      mem.tracking_allocator_destroy(&track)
    }
  }

  // Rest of code
  ...
}
```

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