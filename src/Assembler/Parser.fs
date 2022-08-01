module Parser
    open Farkle
    open Farkle.Builder

    // aka: curry and uncurry
    let pack f a b = f (a,b)
    let unpack f (a, b) = f a b

    type Operand = 
        // absolute addresses
        | Address of int
        | NamedAddress of string * int
        | Reference of string
        // relative addresses (relative to the current instruction)
        | Offset of int
        | Current
        | Next

    type Instruction = 
        | Subleq of Operand * Operand * Operand

    module Grammar =
        let number = Terminals.int "number"

        let label = 
            Regex.regexString @"( \p{AllLetters} | _ ) ( \p{AllLetters} | \d |_ )*" 
            |> terminal "label" (T(fun _ x -> x.ToString()))

        let operand = "operand" ||= [
            !@ number                    => Address
            !@ label .>> ":" .>>. number => pack NamedAddress
            !@ label                     => Reference
            !& "?+" .>>. number          => Offset
            !& "?-" .>>. number          => ((~-) >> Offset)
            !& "?"                       =% Current
            !& ">"                       =% Next
        ]

        let subleq = "subleq" ||= [
            !@ operand .>>. operand .>>. operand => (fun a b c -> Subleq(a, b, c))
            !@ operand .>>. operand              => (fun a b -> Subleq(a, b, Next))
            !@ operand                           => (fun a -> Subleq(a, a, Next))
        ]

        let instruction = "instruction" ||= [
            !@ subleq |> asIs
        ]

        let separator = "separator" ||= [
            !& ";" =% ()
            !% newline =% ()
        ]

        let statement = "statement" ||= [
            !@ instruction .>> (many separator) |> asIs
        ]
        
        let program = 
            "program" ||= [ !% (many separator) .>>. (many statement) |> asIs ]
            |> DesigntimeFarkle.addBlockComment"/*" "*/"
            |> DesigntimeFarkle.addLineComment "//"
            |> DesigntimeFarkle.autoWhitespace true
            |> DesigntimeFarkle.caseSensitive false

    let _farkle = RuntimeFarkle.build Grammar.program

    let parseString = RuntimeFarkle.parseString _farkle
    let parseFile = RuntimeFarkle.parseFile _farkle
