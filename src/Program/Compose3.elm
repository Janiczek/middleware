module Program.Compose3 exposing (init, subscriptions, update, view)

import Html exposing (Html)
import Program.Types exposing (Middleware, ProgramRecord, HasInnerModel)
import Task


init :
    Middleware (HasInnerModel modelIn modelProgram) modelOut msgIn msgOut programMsgs msgProgram
    -> Middleware modelProgram modelIn msgProgram msgIn programMsgs msgProgram
    -> ProgramRecord modelProgram msgProgram programMsgs
    -> ( HasInnerModel modelOut (HasInnerModel modelIn modelProgram), Cmd msgOut )
init middlewareOut middlewareIn program =
    program.init
        |> middlewareIn.init
        |> middlewareOut.init


subscriptions :
    Middleware (HasInnerModel modelIn modelProgram) modelOut msgIn msgOut programMsgs msgProgram
    -> Middleware modelProgram modelIn msgProgram msgIn programMsgs msgProgram
    -> ProgramRecord modelProgram msgProgram programMsgs
    -> HasInnerModel modelOut (HasInnerModel modelIn modelProgram)
    -> Sub msgOut
subscriptions middlewareOut middlewareIn program model =
    let
        programSubs =
            program.subscriptions model.innerModel.innerModel

        middlewareInSubs =
            middlewareIn.subscriptions model.innerModel

        middlewareOutSubs =
            middlewareOut.subscriptions model
    in
        Sub.batch
            [ programSubs
                |> Sub.map middlewareIn.wrapMsg
                |> Sub.map middlewareOut.wrapMsg
            , middlewareInSubs
                |> Sub.map middlewareOut.wrapMsg
            , middlewareOutSubs
            ]


update :
    Middleware (HasInnerModel modelIn modelProgram) modelOut msgIn msgOut programMsgs msgProgram
    -> Middleware modelProgram modelIn msgProgram msgIn programMsgs msgProgram
    -> ProgramRecord modelProgram msgProgram programMsgs
    -> msgOut
    -> HasInnerModel modelOut (HasInnerModel modelIn modelProgram)
    -> ( HasInnerModel modelOut (HasInnerModel modelIn modelProgram), Cmd msgOut )
update middlewareOut middlewareIn program msgOut modelOut =
    -- TODO optimize: if one middleware/program already reacted to the Msg, don't let others try to react to it
    let
        scheduleMsg msgProgram cmdMiddlewareOut =
            Cmd.batch
                [ cmdMiddlewareOut
                , Task.perform identity (Task.succeed msgProgram)
                    |> Cmd.map middlewareIn.wrapMsg
                    |> Cmd.map middlewareOut.wrapMsg
                ]

        updateProgramMsg msgProgram modelOut cmdOut =
            let
                modelIn =
                    modelOut.innerModel

                ( modelProgramAfterMsg, cmdProgramAfterMsg ) =
                    program.update msgProgram modelOut.innerModel.innerModel
            in
                ( { modelOut | innerModel = { modelIn | innerModel = modelProgramAfterMsg } }
                , Cmd.batch
                    [ cmdOut
                    , cmdProgramAfterMsg
                        |> Cmd.map middlewareIn.wrapMsg
                        |> Cmd.map middlewareOut.wrapMsg
                    ]
                )

        ( modelOutAfterOut, cmdAfterOut, maybeProgramMsgOut ) =
            middlewareOut.update msgOut modelOut program.programMsgs

        cmdAfterProgramMsgOut =
            maybeProgramMsgOut
                |> Maybe.map (\msg -> scheduleMsg msg cmdAfterOut)
                |> Maybe.withDefault cmdAfterOut

        maybeMsgIn =
            msgOut
                |> middlewareOut.unwrapMsg

        ( modelOutAfterIn, cmdAfterIn, maybeProgramMsgIn ) =
            maybeMsgIn
                |> Maybe.map (\msgIn -> middlewareIn.update msgIn modelOutAfterOut.innerModel program.programMsgs)
                |> Maybe.map
                    (\( modelIn, cmdIn, maybeProgramMsgIn ) ->
                        ( { modelOutAfterOut | innerModel = modelIn }
                        , Cmd.batch
                            [ cmdAfterProgramMsgOut
                            , cmdIn
                                |> Cmd.map middlewareOut.wrapMsg
                            ]
                        , maybeProgramMsgIn
                        )
                    )
                |> Maybe.withDefault ( modelOutAfterOut, cmdAfterProgramMsgOut, Nothing )

        cmdAfterProgramMsgIn =
            maybeProgramMsgIn
                |> Maybe.map (\msg -> scheduleMsg msg cmdAfterIn)
                |> Maybe.withDefault cmdAfterIn

        maybeMsgProgram =
            maybeMsgIn
                |> Maybe.andThen middlewareIn.unwrapMsg

        ( modelOutAfterProgram, cmdAfterProgram ) =
            maybeMsgProgram
                |> Maybe.map (\msg -> updateProgramMsg msg modelOutAfterIn cmdAfterProgramMsgIn)
                |> Maybe.withDefault ( modelOutAfterIn, cmdAfterProgramMsgIn )
    in
        ( modelOutAfterProgram
        , cmdAfterProgram
        )


view :
    Middleware (HasInnerModel modelIn modelProgram) modelOut msgIn msgOut programMsgs msgProgram
    -> Middleware modelProgram modelIn msgProgram msgIn programMsgs msgProgram
    -> ProgramRecord modelProgram msgProgram programMsgs
    -> HasInnerModel modelOut (HasInnerModel modelIn modelProgram)
    -> Html msgOut
view middlewareOut middlewareIn program model =
    let
        programView =
            program.view model.innerModel.innerModel
                |> Html.map middlewareIn.wrapMsg

        middlewareInView =
            middlewareIn.view model.innerModel programView
                |> Html.map middlewareOut.wrapMsg
    in
        middlewareOut.view model middlewareInView
