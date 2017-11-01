module Middleware.MsgCounting exposing (middleware)

import Html exposing (Html)
import Html.Attributes
import Program
import Program.Types exposing (Middleware)


type Msg innerMsg
    = Other innerMsg


type alias Model =
    { msgCounter : Int }


middleware : Middleware innerModel Model innerMsg (Msg innerMsg) programMsgs programMsg
middleware =
    { init = init
    , update = update
    , subscriptions = Program.subscriptionsNoop
    , wrapMsg = Other
    , unwrapMsg = unwrapMsg
    , view = view
    }


init : ( innerModel, Cmd innerMsg ) -> ( { Model | innerModel : innerModel }, Cmd (Msg innerMsg) )
init ( innerModel, innerCmd ) =
    ( { innerModel = innerModel, msgCounter = 0 }
    , Cmd.map Other innerCmd
    )


update :
    Msg innerMsg
    -> { Model | innerModel : innerModel }
    -> programMsgs
    -> ( { Model | innerModel : innerModel }, Cmd (Msg innerMsg), Maybe programMsg )
update msg model programMsgs =
        ( { model | msgCounter = model.msgCounter + 1 }
        , Cmd.none
        , Nothing
        )


unwrapMsg : Msg innerMsg -> Maybe innerMsg
unwrapMsg msg =
    case msg of
        Other innerMsg ->
            Just innerMsg


view : { Model | innerModel : innerModel } -> Html (Msg innerMsg) -> Html (Msg innerMsg)
view model innerView =
    Html.div [ Html.Attributes.class "msg-counting" ]
        [ Html.text <| "msg counting middleware here! current msg count: " ++ toString model.msgCounter
        , innerView
        ]
