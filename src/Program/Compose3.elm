module Program.Compose3 exposing (init, subscriptions, update, view)

import Html


init middleware1 middleware2 program =
    program.init
        |> middleware2.init
        |> middleware1.init


subscriptions middleware1 middleware2 program model =
    let
        programSubs =
            program.subscriptions model.nextModel.nextModel

        middleware2Subs =
            middleware2.subscriptions model.nextModel

        middleware1Subs =
            middleware1.subscriptions model
    in
        Sub.batch
            [ programSubs
                |> Sub.map middleware2.wrapMsg
                |> Sub.map middleware1.wrapMsg
            , middleware2Subs
                |> Sub.map middleware1.wrapMsg
            , middleware1Subs
            ]


update middleware1 middleware2 program msg1 model1 =
    let
        ( model1After1, cmdAfter1 ) =
            middleware1.update msg1 model1

        ( model1After2, cmdAfter2 ) =
            case middleware1.unwrapMsg msg1 of
                Nothing ->
                    ( model1After1, cmdAfter1 )

                Just msg2 ->
                    let
                        ( model2, cmd2 ) =
                            middleware2.update msg2 model1After1.nextModel
                    in
                        ( { model1After1 | nextModel = model2 }
                        , Cmd.batch
                            [ cmdAfter1
                            , cmd2
                                |> Cmd.map middleware1.wrapMsg
                            ]
                        )

        ( model1AfterProgram, cmdAfterProgram ) =
            case
                msg1
                    |> middleware1.unwrapMsg
                    |> Maybe.andThen middleware2.unwrapMsg
            of
                Nothing ->
                    ( model1After2, cmdAfter2 )

                Just msgProgram ->
                    let
                        model2After2 =
                            model1After2.nextModel

                        ( modelProgram, cmdProgram ) =
                            program.update msgProgram model2After2.nextModel
                    in
                        ( { model1After2 | nextModel = { model2After2 | nextModel = modelProgram } }
                        , Cmd.batch
                            [ cmdAfter2
                            , cmdProgram
                                |> Cmd.map middleware2.wrapMsg
                                |> Cmd.map middleware1.wrapMsg
                            ]
                        )
    in
        ( model1AfterProgram
        , cmdAfterProgram
        )


view middleware1 middleware2 program model =
    let
        programView =
            program.view model.nextModel.nextModel
                |> Html.map middleware2.wrapMsg

        middleware2View =
            middleware2.view model.nextModel programView
                |> Html.map middleware1.wrapMsg
    in
        middleware1.view model middleware2View
