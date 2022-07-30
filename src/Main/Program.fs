﻿open Interpreter

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

program
|> compile
|> run
