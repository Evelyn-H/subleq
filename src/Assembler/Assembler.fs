module Assembler
    open Types

    module List =
        let foldBack f s input = List.foldBack f input s
        let maybeMap mapping = List.map (fun o -> Option.defaultValue o (mapping o))

    let tee f x = f x; x


    // when implementing constants: remember to add a check to disallow constants to be written to

    let rec absolutifyOperand current op =
        match op with
            | Current -> Address current
            | Offset o -> Address (current + o)
            | Next -> Address (current + 1)
            | Named (l, op) -> Named (l, (absolutifyOperand current op))
            // do nothing for the ones that are already absolute
            | Address a -> Address a
            | Reference l -> Reference l

    // i need a better name...
    let absolutify out op = absolutifyOperand (List.length out) op :: out
        

    // replace all labels with concrete addresses
    let concretise instructions =
        // find all labels
        let labels = 
            instructions 
            |> List.indexed
            // turn all Named operands into name * address tuples
            |> List.choose (fun (i, op) -> match op with | Named (l, op) -> Some (l, i) | _ -> None)
            |> Map.ofList
            |> tee (printfn "%A")

        instructions
        // get rid of labels in code
        |> List.maybeMap (fun op -> 
            match op with
            | Named (l, op) -> Some op
            | _ -> None)
        // resolve references
        |> List.maybeMap (fun op -> 
            match op with
            | Reference l -> Some (Address (Map.find l labels))
            | _ -> None)


    let getOpcode op = 
        match op with
        | Address a -> a
        | _ -> invalidArg "operand" $"can't convert {string op} to a raw opcode"

    let flatten op out = 
        match op with
        | Subleq (a, b, c) -> a :: b :: c :: out
        | _ -> invalidArg "operand" $"Can't flatten this instruction: {string op}"

    let assemble (instructions: Instruction list) = 
        instructions
        // unpack instructions into a flat list of operands
        |> List.foldBack flatten []
        // replace all relative addresses with absolute addresses
        |> List.fold absolutify [] |> List.rev
        |> concretise
        // |> List.map (tee (string >> printfn "%s"))
        |> List.map getOpcode
        // |> tee (printfn "%A")

    type AssemblerError = 
        | ParseError of Farkle.FarkleError

    let assembleFile path = 
        match Parser.parseFile path with
        | Error e -> Error (ParseError e)
        | Ok instructions -> Ok (assemble instructions)
    