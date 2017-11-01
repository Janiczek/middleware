module Main exposing (main)

import ExampleProgram
import Middleware.History
import Middleware.ResetByMsg
import Program


main =
    Program.compose3
        Middleware.History.middleware
        Middleware.ResetByMsg.middleware
        ExampleProgram.program
