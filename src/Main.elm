module Main exposing (main)

import ExampleProgram
import Middleware.Noop
import Middleware.ResetByMsg
import Program


main =
    Program.compose3
        Middleware.Noop.middleware
        Middleware.ResetByMsg.middleware
        ExampleProgram.program
