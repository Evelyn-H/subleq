module Parser
    open Farkle
    open Farkle.Builder
    open Types
    

    module Grammar =
        // little helper to make the syntax a lot prettier
        let productions (nonterminal: Nonterminal<'a>) productions =
            let p = Array.ofList productions
            nonterminal.SetProductions(p[0], p[1..])


        let number = Terminals.int "number"

        let label = 
            Regex.regexString @"( \p{AllLetters} | _ ) ( \p{AllLetters} | \d |_ )*" 
            |> terminal "label" (T(fun _ x -> x.ToString()))

        let separator = "separator" ||= [
            !& ";" =% ()
            !% newline =% ()
        ]

        let operand = nonterminal "operand" 
        let instruction = nonterminal "instruction"
        let statement = nonterminal "statement" 

        productions operand [
            !@ number           => Address
            !@ label .>> ":" .>> (opt separator) .>>. operand 
                                => (fun l op -> Named (l, op))
            !@ label            => Reference
            !& "#" .>>. number  => Constant
            !& "?+" .>>. number => Offset
            !& "?-" .>>. number => ((~-) >> Offset)
            !& "?"              =% Current
            !& ">"              =% Next
        ]

        let subleq = "subleq" ||= [
            !@ operand .>>. operand .>>. operand => (fun a b c -> Subleq (a, b, c))
            !@ operand .>>. operand              => (fun a b -> Subleq (a, b, Next))
            !@ operand                           => (fun a -> Subleq (a, a, Next))
        ]

        productions instruction [
            !@ subleq |> asIs
            // data
            !& ".!" .>>. (many operand) => Data
            !& ".!" .>>. Terminals.stringEx "abfnrtv" true true ''' "datastring"  => DataString
            // macro call
            !& "." .>>. label .>>. (many operand) 
                => (fun label args -> (MacroCall (label, args)))
            // macro definition
            !& ".:" .>>. label .>>. (many label) .>> "[" .>> (opt separator) .>>. (many statement) .>> "]" 
                => (fun label args ops -> (MacroDefinition { label = label; args = args; ops = ops }))
        ]

        productions statement [
            !@ instruction .>> (many separator) |> asIs
        ]
        
        let program = 
            "program" ||= [ !% (many separator) .>>. (many statement) |> asIs ]
            |> DesigntimeFarkle.addBlockComment"--[" "]--"
            |> DesigntimeFarkle.addLineComment "--"
            |> DesigntimeFarkle.autoWhitespace true
            |> DesigntimeFarkle.caseSensitive false

    let _farkle = RuntimeFarkle.build Grammar.program

    let parseString = RuntimeFarkle.parseString _farkle
    let parseFile = RuntimeFarkle.parseFile _farkle
