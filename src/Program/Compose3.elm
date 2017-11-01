module Program.Compose3 exposing (init, subscriptions, update, view)

import Html exposing (Html)
import Program.Types exposing (Middleware, ProgramRecord)


init :
    Middleware { modelIn | innerModel : modelProgram } modelOut msgIn msgOut programMsgs msgProgram
    -> Middleware modelProgram modelIn msgProgram msgIn programMsgs msgProgram
    -> ProgramRecord modelProgram msgProgram programMsgs
    -> ( { modelOut | innerModel : { modelIn | innerModel : modelProgram } }, Cmd msgOut )
init middlewareOut middlewareIn program =
    program.init
        |> middlewareIn.init
        |> middlewareOut.init


subscriptions :
    Middleware { modelIn | innerModel : modelProgram } modelOut msgIn msgOut programMsgs msgProgram
    -> Middleware modelProgram modelIn msgProgram msgIn programMsgs msgProgram
    -> ProgramRecord modelProgram msgProgram programMsgs
    -> { modelOut | innerModel : { modelIn | innerModel : modelProgram } }
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
    Middleware { modelIn | innerModel : modelProgram } modelOut msgIn msgOut programMsgs msgProgram
    -> Middleware modelProgram modelIn msgProgram msgIn programMsgs msgProgram
    -> ProgramRecord modelProgram msgProgram programMsgs
    -> msgOut
    -> { modelOut | innerModel : { modelIn | innerModel : modelProgram } }
    -> ( { modelOut | innerModel : { modelIn | innerModel : modelProgram } }, Cmd msgOut )
update middlewareOut middlewareIn program msgOut modelOut =
    let
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

        ( modelOutAfterOut, cmdAfterOut, maybeprogramMsgOut ) =
            middlewareOut.update msgOut modelOut program.programMsgs

        ( modelOutAfterprogramMsgOut, cmdAfterprogramMsgOut ) =
            maybeprogramMsgOut
                |> Maybe.map (\msg -> updateProgramMsg msg modelOutAfterOut cmdAfterOut)
                |> Maybe.withDefault ( modelOutAfterOut, cmdAfterOut )

        maybeMsgIn =
            msgOut
                |> middlewareOut.unwrapMsg

        ( modelOutAfterIn, cmdAfterIn, maybeprogramMsgIn ) =
            maybeMsgIn
                |> Maybe.map
                    (\msgIn ->
                        middlewareIn.update msgIn
                            modelOutAfterprogramMsgOut.innerModel
                            program.programMsgs
                    )
                |> Maybe.map
                    (\( modelIn, cmdIn, maybeprogramMsgIn ) ->
                        ( { modelOutAfterprogramMsgOut | innerModel = modelIn }
                        , Cmd.batch
                            [ cmdAfterprogramMsgOut
                            , cmdIn
                                |> Cmd.map middlewareOut.wrapMsg
                            ]
                        , maybeprogramMsgIn
                        )
                    )
                |> Maybe.withDefault ( modelOutAfterprogramMsgOut, cmdAfterprogramMsgOut, Nothing )

        ( modelOutAfterprogramMsgIn, cmdAfterprogramMsgIn ) =
            maybeprogramMsgIn
                |> Maybe.map (\msg -> updateProgramMsg msg modelOutAfterIn cmdAfterIn)
                |> Maybe.withDefault ( modelOutAfterIn, cmdAfterIn )

        maybeMsgprogram =
            maybeMsgIn
                |> Maybe.andThen middlewareIn.unwrapMsg

        ( modelOutAfterProgram, cmdAfterProgram ) =
            maybeMsgprogram
                |> Maybe.map (\msg -> updateProgramMsg msg modelOutAfterprogramMsgIn cmdAfterprogramMsgIn)
                |> Maybe.withDefault ( modelOutAfterprogramMsgIn, cmdAfterprogramMsgIn )
    in
        ( modelOutAfterProgram
        , cmdAfterProgram
        )


view :
    Middleware { modelIn | innerModel : modelProgram } modelOut msgIn msgOut programMsgs msgProgram
    -> Middleware modelProgram modelIn msgProgram msgIn programMsgs msgProgram
    -> ProgramRecord modelProgram msgProgram programMsgs
    -> { modelOut | innerModel : { modelIn | innerModel : modelProgram } }
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
