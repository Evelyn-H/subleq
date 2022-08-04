module Interpreter

// core types
type Instruction = int * int * int
type Program = Instruction list
type MachineCode = int list // essentially a flattened version of `Program`

// memory-mapped io abstraction
type IOHandler = int -> int -> unit

// default io handler
let io location value   = 
    match location with
    | 1 -> printf "%c" (char value)
    | _ -> ()

// an instance of a running program
// using this much mutability is a poor example of functional programming,
// but it does showcase how easy it is to write imperative code with mutable state
// and in this case it's implemented this way for performance reasons
type Machine = {
    mutable memory: int array
    mutable instruction: int
    mutable halted: bool
    io: IOHandler
}

module Machine =
    // constructor
    let initialize io (code: MachineCode) = { memory = Array.ofList code; instruction = 0; halted = false; io = io }

    // io operations
    let inline read sys location = if location >= 0 then sys.memory[location] else 0
    let inline read3 sys location = read sys location, read sys (location + 1), read sys (location + 2)
    let inline write sys location value = Array.set sys.memory location value

    // run one instruction
    let step sys = 
        if sys.instruction < 0 then sys.halted <- true
        else
            let a, b, c = read3 sys sys.instruction

            // write new value (or print if b < 0)
            if b < 0
            then sys.io -b (read sys a)
            else write sys b (read sys b - read sys a)

            // jump if <= 0 else go to next instruction
            sys.instruction <- if read sys b <= 0 then c else sys.instruction + 3

    // run machine until it halts
    let rec run sys =
        while not sys.halted do
            step sys

let compile (program: Program) : MachineCode = List.collect (fun (a, b, c) -> [ a; b; c ]) program

let run = Machine.initialize io >> Machine.run
