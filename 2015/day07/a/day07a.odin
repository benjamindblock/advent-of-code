package main

import "core:container/queue"
import "core:fmt"
import "core:mem"
import "core:slice"
import "core:strconv"
import "core:strings"
import "../../utils"

Wire :: struct {
  id: string,
  signal: int
}

Gate_Op :: enum {
  And,
  Or,
  Not,
  L_Shift,
  R_Shift,
  Nop
}

Gate_Str :: [Gate_Op]string {
  .And = " AND ",
  .Or = " OR ",
  .Not = "NOT ",
  .L_Shift = " LSHIFT ",
  .R_Shift = " RSHIFT ",
  .Nop = " -> "
}

Gate_Input :: union {
  string,
  int
}

Gate :: struct {
  op: Gate_Op,
  input1: Gate_Input,
  // input2 can be nil (eg., a NOT gate: "NOT x -> y")
  input2: Gate_Input,
  output: string
}

determine_op :: proc(inst: string) -> (Gate_Op) {
  for str, gate in Gate_Str {
    if strings.contains(inst, str) {
      return gate
    }
  }
  return Gate_Op.Nop
}

find_or_create_wire :: proc(id: string, signal: int) -> (Wire) {
  wire, ok := wires[id]
  if !ok {
    wire = Wire{id, signal}
    wires[id] = wire
  }

  return wire
}

create_gate :: proc(op: Gate_Op, input1: string, input2: string, output: string) -> (Gate) {
  gate_input1: Gate_Input
  gate_input2: Gate_Input

  int1, int1_ok := strconv.parse_int(input1)
  if !int1_ok {
    gate_input1 = input1
  } else {
    gate_input1 = int1
  }

  // Not all gates have two inputs. In the case of a .Not gate
  // (and maybe a .Nop???) there is just one.
  if input2 != "" {
    int2, int2_ok := strconv.parse_int(input2)
    if !int2_ok {
      gate_input2 = input2
    } else {
      gate_input2 = int2
    }
  }

  return Gate{op, gate_input1, gate_input2, output}
}

process_two_input_gate :: proc(inst: string, op: Gate_Op) {
  gate_str := Gate_Str
  input1, _, tail := strings.partition(inst, gate_str[op])
  input2, _, output := strings.partition(tail, Gate_Str[.Nop])
  gate := create_gate(op, input1, input2, output)

  // Add destination Gate structs to the adjacency list
  // if (and only if) the input is a wire (and not a raw
  // signal).
  adjacent: [dynamic]Gate
  #partial switch type in gate.input1 {
    case string:
      adjacent = circuit_board[input1]
      append(&adjacent, gate)
      circuit_board[input1] = adjacent
  }

  // Add destination Gate structs to the adjacency list
  // if (and only if) the input is a wire (and not a raw
  // signal).
  #partial switch type in gate.input2 {
    case string:
      adjacent = circuit_board[input2]
      append(&adjacent, gate)
      circuit_board[input2] = adjacent
  }
}

process_one_input_gate :: proc(inst: string, op: Gate_Op) {
  gate_str := Gate_Str
  _, _, parse := strings.partition(inst, gate_str[op])
  input, _, output := strings.partition(parse, Gate_Str[.Nop])
  gate := create_gate(op, input, "", output)

  // Add destination Gate structs to the adjacency list
  // if (and only if) the input is a wire (and not a raw
  // signal).
  adjacent: [dynamic]Gate
  #partial switch type in gate.input1 {
    case string:
      adjacent = circuit_board[input]
      append(&adjacent, gate)
      circuit_board[input] = adjacent
  }
}

gate_can_execute :: proc(gate: Gate) -> (bool) {
  not_visited := !slice.contains(visited[:], gate)

  input1 := gate.input1
  valid1: bool
  switch v in input1 {
    case string:
      valid1 = v in wires
    case int:
      valid1 = true
    case:
      valid1 = false
  }
  

  input2 := gate.input2
  valid2: bool
  switch v in input2 {
    case string:
      valid2 = v in wires
    case int:
      valid2 = true
    case:
      valid2 = true
  }

  return not_visited && valid1 && valid2
}

gate_inputs :: proc(gate: Gate) -> (input1: int, input2: int) {
  switch v in gate.input1 {
    case string:
      input1 = wires[v].signal
    case int:
      input1 = v
  }

  switch v in gate.input2 {
    case string:
      input2 = wires[v].signal
    case int:
      input2 = v
    case: 
      input2 = 0
  }

  return input1, input2
}

execute_gate :: proc(gate: Gate) {
  input1, input2 := gate_inputs(gate)
  output: int

  switch gate.op {
    case .And:
      output = input1 & input2
    case .Or:
      output = input1 | input2
    case .Not:
      signal_bin := fmt.aprintf("%16b", input1)
      defer delete(signal_bin)

      // Flipping the bits (eg., bitwise NOT)
      buf: [16]string
      for c, i in signal_bin {
        if c == '0' {
          buf[i] = "1"
        } else {
          buf[i] = "0"
        }
      }

      signal_not := strings.join(buf[:], "")
      defer delete(signal_not)

      signal_base10, _ := strconv.parse_int(signal_not, 2)
      output = signal_base10
    case .L_Shift:
      output = input1 << uint(input2)
    case .R_Shift:
      output = input1 >> uint(input2)
    case .Nop:
      output = input1
  }

  wire_output := wires[gate.output]
  wire_output.id = gate.output
  wire_output.signal = output
  wires[gate.output] = wire_output
}

descend :: proc(wire_id: string) {
  gates := circuit_board[wire_id]
  q: queue.Queue(Gate, context.temp_allocator)
  defer delete(q.data)

  for gate in gates {
    if gate_can_execute(gate) {
      queue.push_back(&q, gate)
    }
  }

  for queue.len(q) > 0 {
    to_process := queue.pop_front(&q)
    execute_gate(to_process)
    append(&visited, to_process)

    adjacent := circuit_board[to_process.output]
    for new_gate in adjacent {
      if gate_can_execute(new_gate) {
        queue.push_back(&q, new_gate)
      }
    }
  }
}

// Maps a wire, to all of its adjacent Gates
circuit_board := make(map[string][dynamic]Gate)
inputs: [dynamic]Wire
visited: [dynamic]Gate
wires := make(map[string]Wire)

// Iterate the instructions and initialize Wire structs.
// When the "123 -> x" pattern is seen, init the Wire
// or update with the correct signal value.
solve :: proc() {
  lines := utils.slurp_file_into_lines("../input.txt")
  defer delete(lines)
  defer delete(inputs)
  defer delete(visited)
  defer for wire, gate_arr in circuit_board {
    delete(gate_arr)
  }
  defer free_all(context.temp_allocator)

  for line in lines {
    gate_op := determine_op(line) 
    switch gate_op {
      case .And:
        process_two_input_gate(line, Gate_Op.And)
      case .Or:
        process_two_input_gate(line, Gate_Op.Or)
      case .Not:
        process_one_input_gate(line, Gate_Op.Not)
      case .L_Shift:
        process_two_input_gate(line, Gate_Op.L_Shift)
      case .R_Shift:
        process_two_input_gate(line, Gate_Op.R_Shift)
      case .Nop:
        head, _, tail := strings.partition(line, Gate_Str[.Nop])
        input, ok := strconv.parse_int(head)
        // If input is an integer, then feed the wire and set
        // it as an input wire. These will be the starting places
        // in the DAG for our DFS that executes gates and instructions.
        if ok {
          wire := find_or_create_wire(tail, input)
          append(&inputs, wire)
        } else {
          // input_wire := find_or_create_wire(head, 0)
          // output_wire := find_or_create_wire(tail, 0)
          gate := Gate{Gate_Op.Nop, head, nil, tail}

          adjacent := circuit_board[head]
          append(&adjacent, gate)
          circuit_board[head] = adjacent
        }
    }
  }

  // Descend the tree, hitting gates and setting them as
  // visited.
  for wire in inputs {
    descend(wire.id)
  }
  fmt.println("Signal on wire a is", wires["a"].signal)
}

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

  solve()
}
