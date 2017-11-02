module Program.Compose2 exposing (init, subscriptions, update, view)

import Html exposing (Html)
import Program.Types exposing (Middleware, ProgramRecord, HasInnerModel)
import Task


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
    -- TODO optimize: if one middleware/program already reacted to the Msg, don't let others try to react to it
    let
        scheduleMsg msgProgram cmdMiddleware =
            Cmd.batch
                [ cmdMiddleware
                , Task.perform identity (Task.succeed msgProgram)
                    |> Cmd.map middleware.wrapMsg
                ]

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

        ( modelMiddlewareAfterMiddleware, cmdAfterMiddleware, maybeProgramMsg ) =
            middleware.update msgMiddleware modelMiddleware program.programMsgs

        cmdAfterProgramMsg =
            maybeProgramMsg
                |> Maybe.map (\msg -> scheduleMsg msg cmdAfterMiddleware)
                |> Maybe.withDefault cmdAfterMiddleware

        ( modelMiddlewareAfterProgram, cmdAfterProgram ) =
            msgMiddleware
                |> middleware.unwrapMsg
                |> Maybe.map (\msg -> updateProgramMsg msg modelMiddlewareAfterMiddleware cmdAfterProgramMsg)
                |> Maybe.withDefault ( modelMiddlewareAfterMiddleware, cmdAfterProgramMsg )
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

        ( middlewareSubs, middlewareSubsForProgram ) =
            middleware.subscriptions model program.programMsgs
    in
        Sub.batch
            [ programSubs
                |> Sub.map middleware.wrapMsg
            , middlewareSubs
            , middlewareSubsForProgram
                |> Sub.map middleware.wrapMsg
            ]
