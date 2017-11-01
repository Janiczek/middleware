module Program.Compose2 exposing (init, subscriptions, update, view)

import Html


init middleware program =
    program.init
        |> middleware.init


view middleware program model =
    middleware.view model
        (program.view model.nextModel
            |> Html.map middleware.wrapMsg
        )


update middleware program msg1 model1 =
    let
        ( model1After1, cmdAfter1 ) =
            middleware.update msg1 model1

        ( model1AfterProgram, cmdAfterProgram ) =
            case middleware.unwrapMsg msg1 of
                Nothing ->
                    ( model1After1, cmdAfter1 )

                Just msgProgram ->
                    let
                        ( modelProgram, cmdProgram ) =
                            program.update msgProgram model1After1.nextModel
                    in
                        ( { model1After1 | nextModel = modelProgram }
                        , Cmd.batch
                            [ cmdAfter1
                            , cmdProgram
                                |> Cmd.map middleware.wrapMsg
                            ]
                        )
    in
        ( model1AfterProgram
        , cmdAfterProgram
        )


subscriptions middleware program model =
    let
        programSubs =
            program.subscriptions model.nextModel

        middlewareSubs =
            middleware.subscriptions model
    in
        Sub.batch
            [ programSubs
                |> Sub.map middleware.wrapMsg
            , middlewareSubs
            ]
