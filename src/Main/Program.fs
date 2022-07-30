open Argu
open Interpreter

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
    |> compile
    |> run

let run (path: string) =
    ()

let compile (path: string) =
    ()

type Args = 
    | [<Unique; CliPrefix(CliPrefix.None)>] Run of path: string
    | [<Unique; First; CliPrefix(CliPrefix.None)>] Compile of path: string
    | [<Unique>] Hello

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Run _ -> "specify a file"
            | Compile _ -> "specify a file"
            | Hello -> "run a hello world program"


[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Args>(programName = "subleq")
    
    try
        let results = (parser.ParseCommandLine args).GetAllResults()
        if results.Length = 0 
        then printfn "%s" (parser.PrintUsage())
        else
            results
            |> List.iter (fun r ->
                match r with
                | Run path -> run path
                | Compile path -> compile path
                | Hello -> hello ())
    with e ->
        printfn "%s" e.Message

    0
