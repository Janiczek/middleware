module Main exposing (main)

import ExampleProgram
import Middleware.History
import Middleware.SubsTest
import Program


main =
    --compose2 noopMiddleware businessLogicProgram
    --compose2 msgCountingMiddleware businessLogicProgram
    Program.compose3
        Middleware.History.middleware
        Middleware.SubsTest.middleware
        ExampleProgram.program
