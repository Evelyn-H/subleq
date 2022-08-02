module Assembler
    open Types

    module List =
        let foldBack f s input = List.foldBack f input s

    let tee f x = f x; x

    // i need a better name...
    let absolutify out op = 
        match op with
            | Current -> Address (List.length out)
            | Offset o -> Address ((List.length out) + o)
            | Next -> Address ((List.length out) + 1)
            // do nothing for the ones that are already absolute
            | Address a -> Address a
            | NamedAddress (l, a) -> NamedAddress (l, a)
            | Reference l -> Reference l
        :: out

    // replace all labels with concrete addresses
    let concretise instructions =
        instructions 

    let assemble (instructions: Instruction list) = 
        instructions
        |> List.foldBack (fun (a, b, c) out -> (a :: b :: c :: out)) []
        |> List.fold absolutify [] |> List.rev
        |> List.map (tee (string >> printfn "%s"))

    type AssemblerError = 
        | ParseError of Farkle.FarkleError

    let assembleFile path = 
        match Parser.parseFile path with
        | Error e -> Error (ParseError e)
        | Ok instructions -> Ok (assemble instructions)
    