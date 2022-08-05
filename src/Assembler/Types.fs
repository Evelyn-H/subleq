module Types


    type Operand = 
        // absolute
        | Address of int
        | Named of string * Operand
        | Reference of string
        | Constant of int
        
        // relative
        | Offset of int
        | Current
        | Next


    type Instruction = 
        | Subleq of Operand * Operand * Operand
        | MacroDefinition of MacroDefinition
        | MacroCall of string * Operand list
        | Data of Operand list
        | DataString of string

    and MacroDefinition = {
        label: string
        args: string list
        ops: Instruction list
    }
