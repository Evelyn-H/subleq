module Types

    type Operand = 
        // absolute
        | Address of int
        | Named of string * Operand
        | Reference of string
        
        // relative
        | Offset of int
        | Current
        | Next

    type Instruction = Operand * Operand * Operand
