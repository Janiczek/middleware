module Middleware.History exposing (middleware)

import Html exposing (Html)
import Html.Attributes
import Program
import Program.Types exposing (Middleware)


type Msg innerMsg
    = Other innerMsg


type alias Model innerMsg =
    { history : List innerMsg }


middleware : Middleware model (Model innerMsg) innerMsg (Msg innerMsg) programMsgs innerMsg
middleware =
    { init = init
    , update = update
    , subscriptions = Program.subscriptionsNoop
    , wrapMsg = Other
    , unwrapMsg = unwrapMsg
    , view = view
    }


init : (innerModel, Cmd innerMsg ) -> ( { Model innerMsg | innerModel : innerModel }, Cmd (Msg innerMsg) )
init ( innerModel, innerCmd ) =
    ( { innerModel = innerModel, history = [] }
    , Cmd.map Other innerCmd
    )


update :
    Msg innerMsg
    -> { Model innerMsg | innerModel : innerModel }
    -> programMsgs
    -> ( { Model innerMsg | innerModel : innerModel }, Cmd (Msg innerMsg), Maybe innerMsg )
update msg model programMsgs =
    ( { model
        | history =
            msg
                |> unwrapMsg
                |> Maybe.map (\m -> model.history ++ [ m ])
                |> Maybe.withDefault model.history
      }
    , Cmd.none
    , Nothing
    )


unwrapMsg : Msg innerMsg -> Maybe innerMsg
unwrapMsg msg =
    case msg of
        Other innerMsg ->
            Just innerMsg


view : { Model innerMsg | innerModel : innerModel } -> Html (Msg innerMsg)
     -> Html (Msg innerMsg)
view model innerView =
    Html.div [ Html.Attributes.class "history" ]
        [ Html.text <| "history middleware here!"
        , viewHistory model.history
        , innerView
        ]


viewHistory : List innerMsg -> Html (Msg innerMsg)
viewHistory history =
    history
        |> List.map (\msg -> Html.li [] [ Html.text (toString msg) ])
        |> Html.ul []
