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

## Conversions between types
### `string` to `[]rune`
These pieces of code do the same thing:
```odin
import "core:fmt"

str := "Hello, world!"
for char in str {
  fmt.println(char)
}
```

```odin
import "core:fmt"
import "core:unicode/ut8"

str := "Hello, world!"
in_runes := utf8.string_to_runes(str)
defer delete(in_runes)
for char in in_runes {
  fmt.println(char)
}
```

### `[]rune` to `string`
To turn a list of runes into a string
```odin
import "core:fmt"
import "core:unicode/ut8"

chars := []rune{'a', 'b', 'c'}
str := utf8.runes_to_string(chars)
defer delete(str)

// => "abc"
fmt.println(str)
```

### `int` to `sting`
Use `strconv.itoa`, which takes in a slice buffer of bytes and the integer in question.

NOTE: The size of the buffer is the maximum length of the returned string. If an integer has 100 digits in it, create a buffer of `buffer: [100]byte`.

Usage:
```odin
import "core:strconv"

buffer: [16]byte
number := 105
str := strconv.itoa(buffer[:], number)
```

## Printing & String Formatting
### `fmt`
#### Knowing what to use
- `fmt.print*`: Prints to the console
- `fmt.wprint*`: Prints to an io.Writer (which could be implemented to do a multitude of things when written to)
- `fmt.sbprint*`: Prints to a strings.Builder
- `fmt.aprint*`, `fmt.tprint*`: Don't print to any external thing (they just return the string you "printed")

#### Args
`%[char]` is the "verb", and any values before the verb are arguments. View all flags [in the docs](https://pkg.odin-lang.org/core/fmt/).

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

## Enumerations
### Mapping from enum value to string
```odin
Command :: enum {On, Off}
Command_Str :: [Command]string {
  .On = "turn on",
  .Off = "turn off"
}

fmt.println(Command_Str[Command.On])
```

NOTE: This cannot be done:
```odin
cmd := Command.On
// Raises "Error: Cannot index a constant 'Command_Mapping'"
fmt.println(Command_Str[cmd])
```

One must do this instead:
```odin
str := Command_Str
cmd := Command.On
fmt.println(str[cmd])
```

This is because constants (eg., `Command_Str`) are only known at compile time, not runtime. There is a lot of discussion about this and the confusion that it can cause. Not very intuitive (maybe write a blog post about it?).


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

## Miscellania
- `[]u8` is the same as `[]byte`
