open Argu

let program = [
    (12, 12, 3)
    (36, 37, 6)
    (37, 12, 9)
    (37, 37, 12)
    (0, -1, 15)
    (38, 36, 18)
    (12, 12, 21)
    (53, 37, 24)
    (37, 12, 27)
    (37, 37, 30)
    (36, 12, -1)
    (37, 37, 0)
    (39, 0, -1)
    (72, 101, 108)
    (108, 111, 44)
    (32, 87, 111)
    (114, 108, 100)
    (33, 10, 53)
]

let hello () = 
    program
    |> Interpreter.compile
    |> Interpreter.run

let run (path: string) =
    ()

let compile (path: string) =
    ()

let assemble (path: string) =
    match Assembler.assembleFile path with
    | Ok instructions -> printfn "Final code: %A" instructions
    | Error e -> printfn "Error:\n%s" (string e)


type Args = 
    | [<Unique; First; CliPrefix(CliPrefix.None); AltCommandLine("-r")>] Run of path: string
    | [<Unique; First; CliPrefix(CliPrefix.None); AltCommandLine("-c")>] Compile of path: string
    | [<Unique; First; CliPrefix(CliPrefix.None); AltCommandLine("-a")>] Assemble of path: string
    | [<Unique; First>] Hello

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Run _ -> "specify a file"
            | Compile _ -> "specify a file"
            | Assemble _ -> "specify a file"
            | Hello -> "run a hello world program"


[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Args>(programName = "subleq")

    let results = 
        try 
            (parser.ParseCommandLine args).GetAllResults()
        with e ->
            printfn "%s" e.Message
            []

    let handle arg = 
        match arg with
        | Run path -> run path
        | Compile path -> compile path
        | Assemble path -> printfn "%s" (string (assemble path))
        | Hello -> hello ()

    match results with
    | [] -> printfn "%s" (parser.PrintUsage())
    | args -> List.iter handle args

    0
