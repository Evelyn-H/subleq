module Assembler
    open Types

    // little helper to make it easier to conditionally transform things
    let maybeReplace mapping original = 
        match mapping original with
        | Some s -> s
        | None -> original

    module List =
        let foldBack f s input = List.foldBack f input s
        let maybeMap mapping = List.map (maybeReplace mapping)

    let tee f x = f x; x

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
    let rec absolutify out op = absolutifyOperand (List.length out) op :: out
        

    // replace all labels with concrete addresses
    let concretise instructions =
        let add labels (i, op) =
            match op with 
                | Named (l, op) -> Map.add l i labels
                | _ -> labels 

        let labels = 
            instructions 
            // make (address, operand) tuples
            |> List.mapi (fun i op -> (i, op))
            // turn all Named operands into name * address tuples
            |> List.choose (fun (i, op) -> match op with | Named (l, op) -> Some (l, i) | _ -> None)
            |> Map.ofList
            |> tee (printfn "%A")

        instructions
        // get rid of labels
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

    let assemble (instructions: Instruction list) = 
        instructions
        |> List.foldBack (fun (a, b, c) out -> (a :: b :: c :: out)) []
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
    