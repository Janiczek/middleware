module Middleware.MsgCounting exposing (middleware)

import Html exposing (Html)
import Html.Attributes
import Program exposing (Middleware, HasNextModel)


type Msg nextMsg
    = Other nextMsg


type alias Model =
    { msgCounter : Int }


middleware : Middleware model Model msg (Msg msg)
middleware =
    { init = init
    , update = update
    , subscriptions = Program.subscriptionsNoop
    , wrapMsg = Other
    , unwrapMsg = unwrapMsg
    , view = view
    }


init : ( oldModel, Cmd msg ) -> ( HasNextModel Model oldModel, Cmd (Msg msg) )
init ( oldModel, oldCmd ) =
    ( { nextModel = oldModel, msgCounter = 0 }
    , Cmd.map Other oldCmd
    )


update : Msg msg -> HasNextModel Model model -> ( HasNextModel Model model, Cmd (Msg msg) )
update msg model =
    let
        newMsgCounter =
            (model.msgCounter + 1)
                |> Debug.log "MW2 (msg counting)"
    in
        ( { model | msgCounter = newMsgCounter }
        , Cmd.none
        )


unwrapMsg : Msg msg -> Maybe msg
unwrapMsg msg =
    case msg of
        Other innerMsg ->
            Just innerMsg


view : HasNextModel Model model -> Html (Msg msg) -> Html (Msg msg)
view model innerView =
    Html.div [ Html.Attributes.class "msg-counting" ]
        [ Html.text <| "msg counting middleware here! current msg count: " ++ toString model.msgCounter
        , innerView
        ]
