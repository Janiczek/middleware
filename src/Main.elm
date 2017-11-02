module Main exposing (main)

import ExampleProgram
import Middleware.History
import Middleware.Navigation
import Program


main =
    Program.compose3
        Middleware.Navigation.middleware
        Middleware.History.middleware
        ExampleProgram.program
