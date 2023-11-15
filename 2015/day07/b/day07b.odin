package main

import "core:container/queue"
import "core:fmt"
import "core:mem"
import "core:slice"
import "core:strconv"
import "core:strings"
import "../../utils"

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

// Not all gates have two inputs. In the case of .Not and
// .Nop gates there is just one.
create_gate :: proc(op: Gate_Op, input1: string, input2: string, output: string) -> (Gate) {
  gate_input1: Gate_Input
  gate_input2: Gate_Input

  int1, int1_ok := strconv.parse_int(input1)
  if !int1_ok {
    gate_input1 = input1
  } else {
    gate_input1 = int1
  }

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
      input1 = wires[v]
    case int:
      input1 = v
  }

  switch v in gate.input2 {
    case string:
      input2 = wires[v]
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

  wires[gate.output] = output
}

// Takes a starting point (a wire) and navigates through
// the circuit board using BFS. If a gate can be executed,
// we add it to the queue.
bfs :: proc(wire_id: string) {
  q: queue.Queue(Gate)
  queue.push_back_elems(&q, ..circuit_board[wire_id][:])
  defer delete(q.data)

  for queue.len(q) > 0 {
    to_process := queue.pop_front(&q)
    execute_gate(to_process)
    append(&visited, to_process)

    adjacent_gates := circuit_board[to_process.output]
    for gate in adjacent_gates {
      if gate_can_execute(gate) {
        queue.push_back(&q, gate)
      }
    }
  }
}

// Maps a wire, to all of its adjacent Gates
circuit_board := make(map[string][dynamic]Gate)
inputs: [dynamic]string
visited: [dynamic]Gate

// A wire is just an ID (eg., "a") with a signal.
wires := make(map[string]int)

// Iterate the instructions and initialize wires when the
// "123 -> x" pattern is seen.
solve :: proc() {
  // NOTE: Input 2 has been modified with the instructions
  // Part 2 (put the signal from wire a in Part 1 as the
  // input to wire b and re-run).
  lines := utils.slurp_file_into_lines("../input2.txt")

  defer delete(lines)
  defer delete(inputs)
  defer delete(visited)
  defer for wire, gate_arr in circuit_board {
    delete(gate_arr)
  }
  defer free_all(context.temp_allocator)

  // Initialize all of our Gate structs.
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
        input_wire, _, wire_id := strings.partition(line, Gate_Str[.Nop])
        signal, ok := strconv.parse_int(input_wire)

        // If input is a signal (eg., integer), then feed the wire and
        // set it as an input wire. These will be the starting places
        // in the DAG for our DFS that executes gates and instructions.
        if ok {
          wires[wire_id] = signal
          append(&inputs, wire_id)
        } else {
          gate := Gate{Gate_Op.Nop, input_wire, nil, wire_id}
          adjacent := circuit_board[input_wire]
          append(&adjacent, gate)
          circuit_board[input_wire] = adjacent
        }
    }
  }

  // Descend the tree using BFS, hitting gates and setting them as
  // visited if they are able to be executed.
  for wire_id in inputs {
    bfs(wire_id)
  }
  fmt.println("Signal on wire a is", wires["a"])
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
