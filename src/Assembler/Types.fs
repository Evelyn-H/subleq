module Types

    type Operand = 
        // absolute
        | Address of int
        | NamedAddress of string * int
        | Reference of string
        // relative
        | Offset of int
        | Current
        | Next

    type Instruction = Operand * Operand * Operand
