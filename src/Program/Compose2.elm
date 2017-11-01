module Program.Compose2 exposing (init, subscriptions, update, view)

import Html exposing (Html)
import Program.Types exposing (Middleware, ProgramRecord, HasInnerModel)


init :
    Middleware modelProgram modelMiddleware msgProgram msgMiddleware programMsgs msgProgram
    -> ProgramRecord modelProgram msgProgram programMsgs
    -> ( HasInnerModel modelMiddleware modelProgram, Cmd msgMiddleware )
init middleware program =
    program.init
        |> middleware.init


view :
    Middleware modelProgram modelMiddleware msgProgram msgMiddleware programMsgs msgProgram
    -> ProgramRecord modelProgram msgProgram programMsgs
    -> HasInnerModel modelMiddleware modelProgram
    -> Html msgMiddleware
view middleware program model =
    middleware.view model
        (program.view model.innerModel
            |> Html.map middleware.wrapMsg
        )


update :
    Middleware modelProgram modelMiddleware msgProgram msgMiddleware programMsgs msgProgram
    -> ProgramRecord modelProgram msgProgram programMsgs
    -> msgMiddleware
    -> HasInnerModel modelMiddleware modelProgram
    -> ( HasInnerModel modelMiddleware modelProgram, Cmd msgMiddleware )
update middleware program msgMiddleware modelMiddleware =
    let
        updateProgramMsg msgProgram modelMiddleware cmdMiddleware =
            let
                ( modelProgramAfterMsg, cmdProgramAfterMsg ) =
                    program.update msgProgram modelMiddleware.innerModel
            in
                ( { modelMiddleware | innerModel = modelProgramAfterMsg }
                , Cmd.batch
                    [ cmdMiddleware
                    , cmdProgramAfterMsg
                        |> Cmd.map middleware.wrapMsg
                    ]
                )

        ( modelMiddlewareAfterMiddleware, cmdAfterMiddleware, maybeCrossMsg ) =
            middleware.update msgMiddleware modelMiddleware program.programMsgs

        ( modelMiddlewareAfterCrossMsg, cmdAfterCrossMsg ) =
            maybeCrossMsg
                |> Maybe.map (\msg -> updateProgramMsg msg modelMiddlewareAfterMiddleware cmdAfterMiddleware)
                |> Maybe.withDefault ( modelMiddlewareAfterMiddleware, cmdAfterMiddleware )

        ( modelMiddlewareAfterProgram, cmdAfterProgram ) =
            msgMiddleware
                |> middleware.unwrapMsg
                |> Maybe.map (\msg -> updateProgramMsg msg modelMiddlewareAfterCrossMsg cmdAfterCrossMsg)
                |> Maybe.withDefault ( modelMiddlewareAfterCrossMsg, cmdAfterCrossMsg )
    in
        ( modelMiddlewareAfterProgram
        , cmdAfterProgram
        )


subscriptions :
    Middleware modelProgram modelMiddleware msgProgram msgMiddleware programMsgs msgProgram
    -> ProgramRecord modelProgram msgProgram programMsgs
    -> HasInnerModel modelMiddleware modelProgram
    -> Sub msgMiddleware
subscriptions middleware program model =
    let
        programSubs =
            program.subscriptions model.innerModel

        middlewareSubs =
            middleware.subscriptions model
    in
        Sub.batch
            [ programSubs
                |> Sub.map middleware.wrapMsg
            , middlewareSubs
            ]
