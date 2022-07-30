module Interpreter

    // core types
    type Instruction = int * int * int
    type Program = Instruction list
    type MachineCode = int list // essentially a flattened version of `Program`

    // memory-mapped io abstraction
    type IOHandler = int -> int -> unit

    // an instance of a running program
    type Machine = {
        memory: int array
        instruction: int
        halted: bool
        io: IOHandler
    }

    let io location value   = 
        match location with
        | 1 -> printf "%c" (char value)
        | _ -> ()


    module Machine =
        // constructor
        let initialize io (code: MachineCode) = { memory = Array.ofList code; instruction = 0; halted = false; io = io }

        // io operations
        let inline read sys location = if location >= 0 then sys.memory[location] else 0
        let inline read3 sys location = read sys location, read sys (location + 1), read sys (location + 2)
        let inline write sys location value = Array.set sys.memory location value

        // run one instruction
        let step sys = 
            if sys.instruction < 0 then { sys with halted = true } 
            else
                let a, b, c = read3 sys sys.instruction
                let _a = read sys a // value stored at memory[a]

                // write new value (or print if b < 0)
                if b < 0
                then sys.io -b _a
                else write sys b (read sys b - _a)

                // jump if <= 0 else go to next instruction
                if read sys b <= 0 
                then { sys with instruction = c }
                else { sys with instruction = sys.instruction + 3 }

        // run machine until it halts
        let rec run sys =
            if sys.halted then ()
            else 
                step sys
                |> run


    let flatten ((a, b, c): Instruction) : MachineCode = [ a; b; c ]

    let compile (program: Program) : MachineCode = List.collect flatten program

    let run (program: Program) =
        // printfn "Loading machine..."
        let code = compile program
        let sys = Machine.initialize io code
        
        // printfn "Running code...\n"
        Machine.run sys
