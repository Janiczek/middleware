module Middleware.History exposing (middleware)

import Html exposing (Html)
import Html.Attributes
import Program exposing (Middleware, HasNextModel)


type Msg nextMsg
    = Other nextMsg


type alias Model msg =
    { history : List msg }


middleware : Middleware model (Model msg) msg (Msg msg)
middleware =
    { init = init
    , update = update
    , subscriptions = Program.subscriptionsNoop
    , wrapMsg = Other
    , unwrapMsg = unwrapMsg
    , view = view
    }


init : ( oldModel, Cmd msg ) -> ( HasNextModel (Model msg) oldModel, Cmd (Msg msg) )
init ( oldModel, oldCmd ) =
    ( { nextModel = oldModel, history = [] }
    , Cmd.map Other oldCmd
    )


update : Msg msg -> HasNextModel (Model msg) model -> ( HasNextModel (Model msg) model, Cmd (Msg msg) )
update msg model =
    ( { model
        | history =
            msg
                |> unwrapMsg
                |> Maybe.map (\m -> model.history ++ [ m ])
                |> Maybe.withDefault model.history
      }
    , Cmd.none
    )


unwrapMsg : Msg msg -> Maybe msg
unwrapMsg msg =
    case msg of
        Other innerMsg ->
            Just innerMsg


view : HasNextModel (Model msg) model -> Html (Msg msg) -> Html (Msg msg)
view model innerView =
    Html.div [ Html.Attributes.class "msg-counting" ]
        [ Html.text <| "history middleware here!"
        , viewHistory model.history
        , innerView
        ]


viewHistory : List msg -> Html (Msg msg)
viewHistory history =
    history
        |> List.map (\msg -> Html.li [] [ Html.text (toString msg) ])
        |> Html.ul []
